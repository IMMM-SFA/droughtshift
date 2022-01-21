#' create_droughtshift_object
#'
#' @description takes annual time totals and converts to droughtshift object for plotting and robustness analysis
#' @param droughtshift_input A tibble with headers: water_year, precip, flow, status. The status column must contain columns "pre_drought", "drought" and can also contain "termination" and "post-drought". See example by running generate_example_droughtshift_input(). This column will be overwritten if drought_years and termination_year are specified.
#' @param drought_years A vector of water years (integer)
#' @param termination_year Single integer. Year of drought termination.
#' @return a tibble of storage target levels by week
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter pull case_when select left_join
#' @importFrom parsnip linear_reg set_engine fit
#' @importFrom purrr map_dfr map_dbl
#' @export
#'
create_droughtshift_object <- function(droughtshift_input, drought_years, termination_year){

  # ensure correct input headers
  stopifnot("water_year" %in% names(droughtshift_input))
  stopifnot("precip" %in% names(droughtshift_input))
  stopifnot("flow" %in% names(droughtshift_input))
  if(!("status" %in% names(droughtshift_input))){
    stopifnot(
      !missing(drought_years) | !missing(termination_year)
    )
  }

  # perform boxcox transform on flow
  droughtshift_input %>%
    mutate(flow_trans = transform_flow(flow)) ->
    ds_trans

  linear_reg() %>%
    set_engine("lm") %>%
    fit(flow_trans ~ precip, data = ds_trans) ->
    lm_all_years

  # set a slope
  lm_all_years$fit$coefficients[[2]] -> fixed_slope

  # find the intercepts for each status
  pull(ds_trans, status) %>% unique() %>% .[!grepl("termination", .)] %>%
    map_dfr(function(wy_status){
      linear_reg() %>%
        set_engine("lm") %>%
        fit(flow_trans - fixed_slope * precip ~ 1,
            data = ds_trans %>%
              filter(status == !!wy_status) %>%
              mutate(fixed_slope = !!fixed_slope)) %>%
        .[["fit"]] %>% .[["coefficients"]] %>% .[[1]] ->
        intercept

      tibble(status = !!wy_status,
             slope = !!fixed_slope,
             intercept = !!intercept)
    }) -> fitted_rr_lines

  ds_trans %>%
    left_join(fitted_rr_lines, by = "status") %>%
    mutate(flow_trans_prd = precip * slope + intercept) %>%
    select(water_year, precip, flow, status, flow_trans, flow_trans_prd) ->
    ds_flow_and_predictions


  # bootstrap the intercepts
  ds_trans %>% pull(status) %>% unique() %>%
    .[!grepl("termination", .)] %>%
    map_dfr(function(period){
      ds_flow_and_predictions %>%
        filter(status == !!period) ->
        period_data

      # create samples
      nrow(period_data) -> sample_size

      # create 1000 samples
      1:1000 %>%
        map_dbl(function(x){
          period_data[sample(1:sample_size, replace = TRUE),] ->
            data_resample

          return(

            lm(flow_trans - fixed_slope * precip ~ 1,
               data = data_resample %>%
                 mutate(fixed_slope = !!fixed_slope)) %>%
              .[["coefficients"]] %>% .[[1]]
          )

        }) -> resample

      tibble(resample = resample, status = !!period)
    })  ->
    resamples

  return(
    list(
      ds_flow_and_predictions,
      resamples
    )
  )

}


#' transform_flow
#'
transform_flow <- function(x, lamda = 0.3){
    (x^lamda - 1) / lamda
}


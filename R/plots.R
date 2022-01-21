#' create_droughtshift_object
#'
#' @description plots a droughtshift object
#' @param droughtshift_object output from `create_droughtshift_object()`
#' @return a ggplot object
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom cowplot plot_grid
#' @importFrom dplyr filter
#' @export
#'
plot_droughtshift_object <- function(droughtshift_object){

  droughtshift_object[[1]] %>%
    ggplot(aes(precip, flow_trans, col = status)) +
    geom_point() +
    geom_line(aes(y = flow_trans_prd), show.legend = FALSE) +
    scale_color_manual(values = ds_pal) +
    geom_text_repel(data = droughtshift_object[[1]] %>%
                      filter(status %in% c("drought", "termination")),
                      aes(label = water_year),
                      show.legend = FALSE) +
    theme_classic() +
    theme(legend.position = "top",
          legend.text = element_text(size = 12),
          axis.title = element_text(size = 11),
          axis.text = element_text(size = 11)) +
    guides(colour = guide_legend(override.aes = list(size=3))) +
    labs(color = NULL, x = "Water year precipitation",
         y = "Water year flow total, (box-cox transformed)") +
    NULL -> main_plot

  droughtshift_object[[2]] %>%
    ggplot(aes(resample, fill = status, alpha = status)) +
    geom_density(show.legend = FALSE) +
    scale_fill_manual(values = ds_pal[1:3]) +
    scale_alpha_manual(values = c(0.7, 0, 0.7)) +
    theme_void() ->
    robustness_plot_1

  droughtshift_object[[2]] %>%
    ggplot(aes(resample, fill = status, alpha = status)) +
    geom_density(show.legend = FALSE) +
    scale_fill_manual(values = ds_pal[1:3]) +
    scale_alpha_manual(values = c(0.0, 0.7, 0.7)) +
    theme_void() ->
    robustness_plot_2

  plot_grid(main_plot,
            plot_grid(
              NA,
              robustness_plot_1,
              robustness_plot_2,
              NA, ncol =1, rel_heights = c(0.1, 0.4, 0.4, 0.1)
              ), rel_widths = c(0.7, 0.3)
            )
}

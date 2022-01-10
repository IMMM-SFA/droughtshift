#' generate_example_droughtshift_input
#'
#' @description this function generates an example input dataset for droughtshift
#' @return an example input dataset for droughtshift
#' @importFrom tibble tribble
#' @export
#'
generate_example_droughtshift_input <- function(){
  tribble(
    ~water_year, ~precip, ~flow, ~status,
    1997L, 1061, 53.8, "pre-drought",
    1998L, 801, 17.5, "pre-drought",
    1999L, 716, 22.4, "pre-drought",
    2000L, 539, 2.93, "pre-drought",
    2001L, 905, 340., "pre-drought",
    2002L, 911, 29.2, "pre-drought",
    2003L, 766, 1.89, "pre-drought",
    2004L, 872, 14.9, "pre-drought",
    2005L, 758, 36.9, "pre-drought",
    2006L, 615, 9.41, "pre-drought",
    2007L, 1430, 128., "pre-drought",
    2008L, 371, 22.8, "pre-drought",
    2009L, 688, 9.06, "drought",
    2010L, 967, 38.2, "drought",
    2011L, 184, 0.0425, "drought",
    2012L, 797, 4.54, "drought",
    2013L, 505, 0.253, "drought",
    2014L, 600, 2.51, "drought",
    2015L, 701, 15.8, "drought",
    2016L, 1051, 21.4, "termination",
    2017L, 845, 20.2, "post-drought",
    2018L, 602, 2.53, "post-drought",
    2019L, 846, 57.9, "post-drought",
    2020L, 762, 13.7, "post-drought",
    2021L, 719, 10.8, "post-drought"
  )
}

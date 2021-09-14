#' compute distances and groups from algorithm based on raw distributions
#'
#' @param .data
#' @param gran1
#' @param gran2
#' @param response
#' @param quantile_prob_val
#'
#' @return
#' @export scaled_dist_gran_iqr
#'
#' @examples
#' library(gravitas)
#' library(tidyverse)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
#' gran1 = "hour_day"
#' gran2 = NULL
#' response = "general_supply_kwh"
#' v2 <- suppressWarnings(scaled_dist_gran_iqr(sm, "hour_day"))
#' v2
#' v3 <- suppressWarnings(scaled_dist_gran_iqr(sm, "hour_day"))
#' v3

scaled_dist_gran_iqr <-  function(.data,
                               gran1 = NULL,
                               gran2 = NULL,
                               response = NULL,
                               quantile_prob_val = seq(0.1, 0.9, 0.1),
                               kopt = NULL){

  .data = robust_scale_data(.data, gran1, gran2, response)


}

#' Title
#'
#' @param .data
#' @param gran1
#' @param gran2
#' @param response
#' @param quantile_prob_val
#' @param kopt
#'
#' @return
#' @export
#'
#' @examples
#' library(gravitas)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10006704", "10017936", "10018064"))
#' gran1 <- "hour_day"
#' gran2 <- NULL
#' suppressWarnings(den_plot(sm, "hour_day", kopt = 3))
den_plot <- function(.data,
                     gran1 = NULL,
                     gran2 = NULL,
                     response = NULL,
                     quantile_prob_val = seq(0.1, 0.9, 0.1),
                     kopt = NULL) {
  dist_mat <- dist_clust(.data, gran1, gran2, response, quantile_prob_val)

  d <- stats::as.dist(dist_mat[, -1])
  hc <- stats::hclust(d, method = "complete")
  # plot(hc)


  nmaxclust <- nrow(dist_mat)

  # plot(hc, cex = 0.6)
  groups <- cutree(hc, k = kopt)

  group_ref <- tibble(customer_id = names(groups), groups %>% as_tibble()) %>%
    rename("group" = "value")

  group_ref
  # rect.hclust(hc, k =  kopt)
}

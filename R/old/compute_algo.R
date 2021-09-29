#' Title
#'
#' @param .data
#' @param gran1
#' @param gran2
#' @param harmony_tbl
#' @param nperm
#' @param nsamp
#' @param method
#' @param response
#' @param quantile_prob_val
#'
#' @return
#' @export
#'
#' @examples
compute_wpd <- function(.data = NULL,
                        gran1 = NULL, # one-gran case for algorithm 1
                        gran2 = NULL, ## two-gran case for algorithm 1
                        harmony_tbl = NULL, # algorithm 2
                        nperm = 2, # number of permutations for algo 2
                        nsamp = 2, # number of samples for threshold computation for algo 2
                        method = "dist_clust",
                        response = NULL,
                        quantile_prob_val = seq(0.1, 0.9, 0.1)) # could be one of dist_clust, scaled_dist_clust or wpd_clust
{
  if (is.null(gran1) & is.null(gran2) & is.null(harmony_tbl)) {
    stop("Provide atleast one of gran1, gran2 or harmony_tbl")
  }

  if (method == "dist_clust") {
    groups <- dist_clust(
      .data, gran1, gran2, response,
      quantile_prob_val
    )
  }

  if (method == "scaled_dist_clust") {
    groups <- scaled_dist_clust(
      .data, gran1, gran2, response,
      quantile_prob_val
    )
  }

  if (method == "wpd_clust") {
    groups <- wpd_clust(.data, harmony_tbl, nperm, nsamp)
  }
}

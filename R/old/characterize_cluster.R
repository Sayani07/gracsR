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
#' library(gravitas)
#' library(tidyverse)
#' library(parallel)
#' library(tsibble)
#' library(rlang)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
#' gran1 = "hour_day"
#' gran2 = NULL
#'   harmonies <- sm %>%
#'   harmony(
#'   ugran = "month",
#'   filter_in = "wknd_wday",
#'   filter_out = c("hhour", "fortnight")
#'   )
#' v = suppressWarnings(characterize_cluster(sm, gran1 = "hour_day", harmony_tbl = harmonies, method = "scaled_dist_clust"))
#' v
#' v1 = suppressWarnings(characterize_cluster(sm, gran1 = "hour_day", harmony_tbl = harmonies, method = "wpd_clust"))
#' v1
#' v2 = suppressWarnings(characterize_cluster(sm, gran1 = "hour_day", harmony_tbl = harmonies, method = "dist_clust", kopt = 2))
#' v2
characterize_cluster <- function(.data = NULL,
                        gran1 = NULL, # one-gran case for algorithm 1
                        gran2 = NULL, ## two-gran case for algorithm 1
                        harmony_tbl = NULL, # algorithm 2
                        nperm = 2, # number of permutations for algo 2
                        nsamp = 2,  # number of samples for threshold computation for algo 2
                        method = "dist_clust",
                        response = NULL,
                        quantile_prob_val = c(0.1, 0.25, 0.5, 0.75, 0.9),
                        kopt = NULL,
                        key = NULL) # could be one of dist_clust, scaled_dist_clust or wpd_clust
{

  if(is.null(gran1) & is.null(gran2) & is.null(harmony_tbl))
    stop("Provide atleast one of gran1, gran2 or harmony_tbl")

  if(method == "dist_clust"){
    groups = dist_clust(.data,
                        gran1,
                        gran2,
                        response,
                        quantile_prob_val = seq(0.1, 0.9, 0.1),
                        key)
  }

  if(method == "scaled_dist_clust"){
    groups = scaled_dist_clust(.data,
                               gran1,
                               gran2,
                               response,
                               quantile_prob_val = seq(0.1, 0.9, 0.1))
  }

  if(method == "wpd_clust"){
    groups = wpd_clust(.data,
                       harmony_tbl,
                       filter_comb,
                       nperm,
                       nsamp,
                       kopt)
  }

  groups$customer_id <- as.character(groups$customer_id)

  data_group <- .data %>% left_join(groups)
  data_group

  # quantiles to show

  data_list <- cust_by_cat(data_group,
              gran1,
              gran2,
              response) %>%
    left_join(groups)

  ncol_sm <- seq_len(ncol(data_list %>% select(-customer_id, -group)))
  nrow_sm <- unique(data_list$group)


  data_quantiles <- map(nrow_sm, function(x){
    map(ncol_sm, function(y){
      cell <- data_list %>%
        dplyr::filter(group == x) %>%
        select(-customer_id, -group) %>%
        magrittr::extract(y) %>%
        unlist()
      quantile(cell, prob = quantile_prob_val)
    })  %>% bind_rows(.id = "categories_serial_id")
  }) %>% bind_rows(.id = "group_id") %>%
    mutate(group_id = as.integer(group_id),
           categories_serial_id = as.integer(categories_serial_id))

  categories_ref_cat <- names(data_list %>% select(-customer_id, -group)) %>% as_tibble() %>%
    mutate(categories_serial_id = row_number())

  data_heatmap <- data_quantiles %>%
    pivot_longer(-c(1, 2), names_to = "quantile_prob", values_to = "quantile_val") %>%
    left_join(categories_ref_cat) %>%
    rename("category" = "value") %>%
    select(-categories_serial_id) %>%
    mutate(category = as.numeric(category))

  data_heatmap  %>%
    pivot_wider( names_from = quantile_prob,
                 values_from = quantile_val) %>%
    ggplot(aes(x = category)) +
    geom_ribbon(aes(ymin = `25%`,
                    ymax = `75%`), fill = "#D55E00", alpha = 0.8) +
    geom_ribbon(aes(ymin = `10%`,
                    ymax = `90%`), fill = "#56B4E9", alpha = 0.4) +
    geom_line(aes(y = `50%`)) +
    facet_wrap(~group_id, ncol = 1,
               scales = "free_y",
               strip.position = "right") +
    theme_bw() +
    theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0)))

}



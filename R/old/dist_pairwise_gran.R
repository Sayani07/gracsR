#' compute distances and groups from algorithm based on raw distributions
#'
#' @param .data
#' @param gran1
#' @param gran2
#' @param response
#' @param quantile_prob_val
#'
#' @return
#' @export scaled_dist_clust
#'
#' @examples
#' library(gravitas)
#' library(tidyverse)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10006704", "10017936", "10006414", "10018250"))
#' gran1 <- "day_month"
#' gran2 <- NULL
#' response <- "general_supply_kwh"
#' v2 <- suppressWarnings(dist_pairwise_gran(sm, "hour_day", quantile_prob_val = c(0.25, 0.5, 0.75)))
#' v2
dist_pairwise_gran <- function(.data,
                               gran1 = NULL,
                               gran2 = NULL,
                               response = NULL,
                               quantile_prob_val = seq(0.1, 0.9, 0.1),
                               kopt = NULL) {
  key <- tsibble::key(.data)
  key <- key[1] %>% as.character()


  if (is.null(response)) {
    response <- tsibble::measured_vars(.data)
    response <- response[1]
  }

  # scaling response is already done by compute_quantiles
  # # scale the response
  #
  # .data <- .data %>%
  #   group_by(!!sym(key)) %>%
  #   dplyr::mutate(m = stats::qqnorm(!!sym(response), plot.it=FALSE)$x) %>%
  #   dplyr::mutate(!!response := m) %>%
  #   select(-m) %>%
  #   ungroup()

  # create_gran data

  if (is.null(gran2)) {
    sm_gran <- .data %>%
      create_gran(gran1) %>%
      as_tibble() %>%
      select(
        key,
        response,
        {{ gran1 }}
      )
  }

  if (!is.null(gran2)) {
    sm_gran <- .data %>%
      create_gran(gran1) %>%
      create_gran(gran2) %>%
      as_tibble() %>%
      select(
        key,
        response,
        {{ gran1 }},
        {{ gran2 }}
      )
  }

  data <- unite(sm_gran, category, -c(1, 2), sep = "-") %>%
    rename("id_x" = "category") %>%
    mutate(id_facet = 1) %>%
    rename("sim_data" = response)

  data_split <- data %>% group_split(!!sym(key))

  df3 <- map(seq_len(length(data_split)), function(x) {
    data <- data_split %>% magrittr::extract2(x)
    data %>%
      hakear::compute_quantiles(quantile_prob = quantile_prob_val) %>%
      hakear::distance_all_pairwise(quantile_prob = quantile_prob_val)
  }) %>%
    bind_rows(.id = "customer_serial_id") %>%
    mutate(customer_serial_id = as.integer(customer_serial_id))


  # customer reference


  uni_cust <- unique(data[, 1])
  customer_ref <- tibble(
    customer_serial_id = seq(nrow(uni_cust)),
    uni_cust
  )




  data_return <- df3 %>%
    arrange(customer_serial_id, id_x_1, id_x_2) %>%
    mutate(cat_comb = paste(id_x_1, id_x_2, sep = "-")) %>%
    select(customer_serial_id, cat_comb, value) %>%
    rename("dist" = "value") %>%
    left_join(customer_ref)


  data_return %>%
    pivot_wider(names_from = cat_comb, values_from = dist) %>%
    select(-1)
}

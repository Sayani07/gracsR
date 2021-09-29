#' compute distances for each categories for a specified granularity from algorithm based on raw distributions
#'
#' @param quantile_prob_val
#'
#' @param .data a tsibble
#' @param gran1 one granularity e.g. hour_day, day_week, wknd_wday
#' @param gran2 one granularity distinct from gran1
#' @param response measured variable
#' @param quantile_prob_val values of probability for which distances between quantiles would be computed
#'
#' @return
#'
#' @examples
#' library(gravitas)
#' library(tidyverse)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10006704", "10017936", "10006414", "10018250"))
#' gran1 <- "hour_day"
#' gran2 <- NULL
#' response <- "general_supply_kwh"
#' dist_gran(sm, "hour_day")
#' dist_gran(sm, "month_year")
#' sm %>%
#'   scale_gran(method = "robust") %>%
#'   dist_gran_cat("hour_day")
#' @export
dist_gran_cat <- function(.data,
                      gran1 = NULL,
                      gran2 = NULL,
                      response = NULL,
                      quantile_prob_val = seq(0.1, 0.9, 0.1)) {
  key <- tsibble::key(.data)
  key <- key[1] %>% as.character()


  if (is.null(response)) {
    response <- tsibble::measured_vars(.data)
    response <- response[1]
  }

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

  data <- unite(sm_gran, category, -c(1, 2), sep = "-")

  # category reference
  uni_cat <- unique(data$category)
  category_ref <- tibble(
    category_id = seq(uni_cat),
    category = uni_cat
  )


  # Compute list across categories
  sm_list <- data %>%
    select(key, category, response) %>%
    pivot_wider(
      names_from = category,
      values_from = response, values_fn = list
    )

  # customer reference


  uni_cust <- unique(sm_list[, 1])
  customer_ref <- tibble(
    customer_serial_id = seq(nrow(uni_cust)),
    uni_cust
  )

  # Compute quantiles across categories

  ncol_sm <- seq_len(ncol(sm_list))[-ncol(sm_list)]
  nrow_sm <- seq_len(nrow(sm_list))

  sm_quantiles <- parallel::mclapply(
    nrow_sm,
    function(x) {
      k <- parallel::mclapply(
        ncol_sm,
        function(y) {
          cell <- sm_list[-1] %>%
            magrittr::extract(x, y) %>%
            unlist()
          quantile(cell, prob = quantile_prob_val, na.rm = TRUE)
        }
      ) %>% bind_rows(.id = "category_id")
    }
  ) %>% bind_rows(.id = "customer_serial_id")


  # Distance between quantiles for each category sm_dist_data

  sm_dist_data <- sm_quantiles %>%
    pivot_longer(-c(1, 2),
                 names_to = "quantile_prob",
                 values_to = "quantile_val"
    ) %>%
    pivot_wider(-3,
                names_from = category_id,
                values_from = quantile_val, values_fn = list
    )

  nrow_data <- nrow(sm_dist_data)
  ncol_data <- ncol(sm_dist_data[-1])

  dist_data <- parallel::mclapply(seq_len(nrow_data), function(x) { # first data
    parallel::mclapply(seq_len(nrow_data), function(y) { # 2nd data
      parallel::mclapply(seq_len(ncol_data), function(z) { # number of combinations nx*nfacet
        JS(
          prob = quantile_prob_val,
          unlist(sm_dist_data[-1] %>% magrittr::extract(x, z)),
          unlist(sm_dist_data[-1] %>% magrittr::extract(y, z))
        ) %>% as_tibble()
      }) %>% bind_rows(.id = "category_id")
    }) %>% bind_rows(.id = "customer_serial_id")
  }) %>% bind_rows(.id = "customer_serial_id1")

  dist_mat <- dist_data %>%
    mutate(customer_serial_id = as.numeric(customer_serial_id), customer_serial_id1 = as.numeric(customer_serial_id1)) %>%
    left_join(customer_ref) %>%
    rename("customer_from" = "customer_id") %>%
    left_join(customer_ref, by = c("customer_serial_id1" = "customer_serial_id")) %>%
    rename("customer_to" = "customer_id") %>%
    ungroup() %>%
    mutate(category_id = as.integer(category_id)) %>%
    left_join(category_ref, by = "category_id") %>%
    select(customer_from, customer_to, category, value) %>%
    rename("distance" = "value")

  dist_mat
}

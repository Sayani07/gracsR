#' compute distances and groups from algorithm based on raw distributions
#'
#' @param .data
#' @param gran1
#' @param gran2
#' @param response
#' @param quantile_prob_val
#'
#' @return
#' @export
#'
#' @examples
#' library(gravitas)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10006704", "10017936", "10006414", "10018250"))
#' gran1 <- "hour_day"
#' gran2 <- NULL
#' v1 <- suppressWarnings(dist_clust(sm, "hour_day"))
#' v1
dist_clust <- function(.data,
                       gran1 = NULL,
                       gran2 = NULL,
                       response = NULL,
                       quantile_prob_val = seq(0.1, 0.9, 0.1),
                       kopt = NULL,
                       key = NULL) {
  if (is.null(key)) {
    key <- tsibble::key(.data)
    key <- key[1] %>% as.character()
  }

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
      values_from = response
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

  sm_quantiles <- map(nrow_sm, function(x) {
    map(ncol_sm, function(y) {
      cell <- sm_list[-1] %>%
        magrittr::extract(x, y) %>%
        unlist()
      quantile(cell, prob = quantile_prob_val, na.rm = TRUE)
    }) %>% bind_rows(.id = "category_id")
  }) %>% bind_rows(.id = "customer_serial_id")


  # Distance between quantiles for each category sm_dist_data

  sm_dist_data <- sm_quantiles %>%
    pivot_longer(-c(1, 2),
      names_to = "quantile_prob",
      values_to = "quantile_val"
    ) %>%
    pivot_wider(-3,
      names_from = category_id,
      values_from = quantile_val
    )

  nrow_data <- nrow(sm_dist_data)
  ncol_data <- ncol(sm_dist_data[-1])

  dist_data <- map(seq_len(nrow_data), function(x) { # first data
    map(seq_len(nrow_data), function(y) { # 2nd data
      map(seq_len(ncol_data), function(z) { # number of combinations nx*nfacet
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
    group_by(customer_from, customer_to) %>%
    summarise(dist = sum(value)) %>%
    pivot_wider(
      names_from = customer_to,
      values_from = dist
    ) %>%
    mutate(customer_from = as.integer(customer_from)) %>%
    rename("customer_id" = "customer_from")

  d <- stats::as.dist(dist_mat[, -1])
  hc <- stats::hclust(d, method = "complete")

  nmaxclust <- nrow(dist_mat)

  if (is.null(kopt)) {
    koptimal <- fpc::nselectboot(d,
      B = 50,
      method = "complete",
      clustermethod = fpc::disthclustCBI,
      classification = "averagedist",
      krange = 2:nmaxclust
    )

    kopt <- koptimal$kopt
  }

  groups <- tibble(group = cutree(hc, k = kopt), customer_id = dist_mat$customer_id)
  groups


  # Hierarchical clustering hc
  # return(Groups) groups
}

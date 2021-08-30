distance_all_pairwise <- function(sim_panel_quantiles,
                                  quantile_prob = seq(0.01, 0.99, 0.01),
                                  dist_ordered = TRUE,
                                  lambda = 0.67)
  # dist_rel = function(x){1-x}
  # relative distance
  # additive inverse
  # weights = function(x){1/x} multiplicative inverse)

{
  row_number <- id_facet.x <- id_facet.y <- id_x.x <- remove_row <- id_facet_1 <-  id_x_1 <- id_facet_2 <-  id_x_2 <- value <- id_x.y <-  NULL

  dist_type <- NULL
  # ncoly <- sim_panel_quantiles %>%
  #   distinct(id_facet) %>%
  #   nrow()
  # nrowy <- sim_panel_quantiles %>%
  #   distinct(id_x) %>%
  #   nrow()

  # range of i, j and k are defined in this way since some cyclic granularities start from 0 and others from 1 -  it was creating a problem while filtering in m1 and m2, where m2 was leading to a tibble of 0 rows and JS function was failing
  if (any((class(sim_panel_quantiles$id_x) %in% c("character", "integer")))) {
    data$id_x <- as.numeric(data$id_x) %>% factor()
  }
  if (any((class(data$id_facet) %in% c("character", "integer")))) {
    data$id_facet <- as.numeric(data$id_facet) %>% factor()
  }

  vm <- sim_panel_quantiles %>% dplyr::mutate(row_number = row_number())

  # differences of all combination of row taking two a time need to be computed
  allcomb <- utils::combn(vm$row_number, 2) %>%
    t() %>%
    tibble::as_tibble()

  # define within-facet and between-facet distances
  all_data <- allcomb %>%
    dplyr::left_join(vm, by = c("V1" = "row_number")) %>%
    dplyr::left_join(vm, by = c("V2" = "row_number")) %>%
    dplyr::mutate(dist_type = dplyr::if_else(id_facet.x == id_facet.y,
                                             "within-facet",
                                             dplyr::if_else(id_x.x == id_x.y, "between-facet", "uncategorised")
    )) %>%
    dplyr::filter(dist_type != "uncategorised")

  # remove un-ordered within-facet distances if categories are ordered

  if (dist_ordered) {
    all_data <- all_data %>%
      dplyr::mutate(
        remove_row =
          dplyr::if_else((dist_type == "within-facet" &
                            (as.numeric(id_x.y) - as.numeric(id_x.x)) != 1), 1, 0)
      ) %>%
      dplyr::filter(remove_row == 0)
  }

  all_dist <- lapply(
    seq_len(nrow(all_data)),
    function(x) {
      distance <- JS(
        prob = quantile_prob,
        unlist(all_data[x, ]$sim_data_quantile.x),
        unlist(all_data[x, ]$sim_data_quantile.y)
      )
    }
  ) %>%
    unlist() %>%
    tibble::as_tibble()


  return_data <- all_data %>%
    dplyr::rename(
      "id_facet_1" = "id_facet.x",
      "id_facet_2" = "id_facet.y",
      "id_x_1" = "id_x.x",
      "id_x_2" = "id_x.y"
    ) %>%
    dplyr::select(
      id_facet_1,
      id_x_1,
      id_facet_2,
      id_x_2,
      dist_type
    ) %>%
    dplyr::bind_cols(all_dist) %>%
    dplyr::mutate(trans_value = dplyr::if_else(dist_type == "within-facet",
                                               lambda * value,
                                               (1 - lambda) * value
    ))

  return_data
}

JS <- function(prob, q, p) {
  # Compute approximate densities
  x <- seq(min(q, p), max(q, p), l = 201)
  qpmf <- pmf(x, prob, q)
  ppmf <- pmf(x, prob, p)
  m <- 0.5 * (ppmf + qpmf)
  JS <- suppressWarnings(0.5 * (sum(stats::na.omit(ppmf * log(ppmf / m, base = 2))) +
                                  sum(stats::na.omit(qpmf * log(qpmf / m, base = 2)))))
  return(JS)
}

# Compute approximate discretized density (like a probability mass function)
# at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q) {
  qcdf <- stats::approx(q, p, xout = x, yleft = 0, yright = 1, ties = max, na.rm = TRUE)$y
  qpmf <- c(0, diff(qcdf) / (x[2] - x[1]))
  return(qpmf / sum(qpmf))
}

#' Title Compute distances based on wpd
#' Computes distances between subjects based on wpd across different granularities
#' @param .data a tsibble
#' @param harmony_tbl a harmony table
#' @param response measured variable
#' @param nperm number of permutations for normalization

#' @return  returns an object of class "dist"
#'
#' @examples
#' library(gravitas)
#' library(tidyverse)
#' library(parallel)
#' library(tsibble)
#' library(rlang)
#' sm <- smart_meter10 %>%
#'   filter(customer_id %in% c("10006704", "10017936", "10006414", "10018250"))
#' gran1 <- "hour_day"
#' gran2 <- NULL
#' harmonies <- sm %>%
#'   harmony(
#'     ugran = "year",
#'     filter_in = "wknd_wday",
#'     filter_out = c("hhour", "fortnight", "quarter", "semester")
#'   )
#' harmonies1 <- harmonies %>% mutate(facet_variable = NA)
#'
#' h <- harmonies1 %>%
#'   select(-facet_levels) %>%
#'   distinct() %>%
#'   mutate(facet_levels = NA) %>%
#'   filter(x_variable %in% c("month_year", "hour_day", "wknd_wday"))
#'
#' v <- dist_wpd(sm, harmony_tbl = h)
#' v
#' @export
dist_wpd <- function(.data,
                     harmony_tbl = NULL,
                     # filter_comb = NULL,
                     response = NULL,
                     nperm = 100) {

  key <- tsibble::key(.data)
  key <- key[1] %>% as.character()

  index <- tsibble::index(.data) %>% as.character()

  if (is.null(response)) {
    response <- tsibble::measured_vars(.data)
    response <- response[1]
  }

  if (is.null(harmony_tbl)) {
    stop("harmony table must be provided")
  }

  harmonies <- harmony_tbl %>%
    mutate(comb = paste(facet_variable,
      x_variable,
      sep = "-"
    )) %>%
    select(-comb)

  # %>%  filter(comb %in% c("hour_day-wknd_wday",
  # "day_month-hour_day",
  # "wknd_wday-hour_day",
  # "hour_day-day_week",
  # "day_week-hour_day"))

  uni_cust <- unique(.data %>% pull(!!sym(key)))
  customer_ref <- tibble(
    customer_serial_id = as.character(seq(length(uni_cust))),
    customer_id = uni_cust
  )


  elec_split <- .data %>% group_split(!!sym(key))

  elec_select_harmony <- parallel::mclapply(seq_len(length(elec_split)), function(x) {
    data_id <- elec_split %>%
      magrittr::extract2(x) %>%
      as_tsibble(index = index)


    k <- hakear::wpd(data_id,
      harmony_tbl = harmonies,
      response = {{ response }},
      nperm = nperm
    ) %>% arrange(-wpd)
  }, mc.cores = parallel::detectCores() - 1, mc.preschedule = FALSE, mc.set.seed = FALSE) %>%
    dplyr::bind_rows(.id = "customer_serial_id") %>%
    # dplyr::mutate(!!key := m) %>%
    # dplyr::select(-m) %>%
    dplyr::left_join(customer_ref) %>%
    dplyr::select(-customer_serial_id)

  # write_rds(elec_select_harmony, "data/elec_select_harmony.rds")

  mydist <- elec_select_harmony %>%
    mutate(comb = paste(facet_variable, x_variable, sep = "-")) %>%
    select(comb, customer_id, wpd) %>%
    pivot_wider(names_from = comb, values_from = wpd) %>%
    dplyr::rename(key1 := !!key)

  mydist <- column_to_rownames(mydist,
    var = "key1"
  ) %>%
    dist()

  mydist
}

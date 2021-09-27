#' compute distances and groups from algorithm based on raw distributions
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
#' filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
#' gran1 = "hour_day"
#' gran2 = NULL
#' response = "general_supply_kwh"
#' dist_gran(sm, "hour_day")
#' dist_gran(sm, "month_year")
#' sm %>% quantile_gran(gran1 = "hour_day")
#' @export
quantile_gran <-  function(.data,
                       gran1 = NULL,
                       gran2 = NULL,
                       response = NULL,
                       quantile_prob_val = seq(0.1, 0.9, 0.1)){

  key =  tsibble::key(.data)
  key = key[1] %>% as.character()


  if(is.null(response)){
    response =  tsibble::measured_vars(.data)
    response = response[1]
  }

  # create_gran data

  if(is.null(gran2)){
    sm_gran <- .data %>%
      create_gran(gran1) %>%
      as_tibble() %>%
      select(key,
             response,
             {{gran1}})

  }

  if(!is.null(gran2)){
    sm_gran <- .data %>%
      create_gran(gran1) %>%
      create_gran(gran2) %>%
      as_tibble() %>%
      select(key,
             response,
             {{gran1}},
             {{gran2}})
  }

  data <- unite(sm_gran, category, -c(1, 2), sep = "-")

  # category reference
  uni_cat <- unique(data$category)
  category_ref <-tibble(category_id = seq(uni_cat),
                        category = uni_cat)


  # Compute list across categories
  sm_list <- data %>%
    select(key, category, response) %>%
    pivot_wider(names_from = category,
                values_from = response, values_fn = list)

  # customer reference


  uni_cust <- unique(sm_list[,1])
  customer_ref <- tibble(customer_serial_id = seq(nrow(uni_cust)),
                         uni_cust)

  # Compute quantiles across categories

  ncol_sm <- seq_len(ncol(sm_list))[-ncol(sm_list)]
  nrow_sm <- seq_len(nrow(sm_list))

  sm_quantiles <- parallel::mclapply(nrow_sm,
                                     function(x){
                                       k <- parallel::mclapply(ncol_sm,
                                                               function(y){

                                                                 cell <- sm_list[-1] %>%
                                                                   magrittr::extract(x, y) %>% unlist()
                                                                 quantile(cell, prob = quantile_prob_val, na.rm =TRUE)

                                                               })  %>% bind_rows(.id = "category_id")
                                     }) %>% bind_rows(.id = "customer_serial_id") %>%
    mutate(category_id = as.integer(category_id),
           customer_serial_id = as.integer(customer_serial_id)) %>% left_join(customer_ref,  by = "customer_serial_id") %>%
    left_join(category_ref, by = "category_id") %>%
    select(-customer_serial_id, -category_id)



  sm_quantiles %>%
    pivot_longer(-c(ncol(sm_quantiles), (ncol(sm_quantiles) - 1)), names_to = "quantiles", values_to = "quantiles_values") %>%
    mutate(gran = gran1)
}

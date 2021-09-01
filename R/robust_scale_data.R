#' robust_scaling of data marginal on categories of different granularities
#'
#' @param .data
#' @param gran1
#' @param gran2
#' @param response
#' @param method
#'
#' @return robust_scale_data
#' @export
#'
#' @examples
#' library(gravitas)
#' library(tidyverse)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
#' gran1 = "hour_day"
#' gran2 = NULL
#' response = "general_supply_kwh"
#' v2 <- suppressWarnings(robust_scale_data(sm, "hour_day"))
#' v2
#' #todo : can add NQT marginal on each category?
robust_scale_data = function(.data,
                      gran1 = c, # can't be kept NULL
                      gran2 = NULL,
                      response = NULL,
                      method = "robust"){

  key =  tsibble::key(.data)
  key = key[1] %>% as.character()

  index =  tsibble::index(.data) %>% as.character()

  if(is.null(response)){
    response =  tsibble::measured_vars(.data)
    response = response[1]
  }

  # create_gran data

  if(is.null(gran2)){
    sm_gran <- .data %>%
      create_gran(gran1) %>%
      #as_tibble() %>%
      select(all_of(key),
             response,
             {{gran1}})

  }

  if(!is.null(gran2)){
    sm_gran <- .data %>%
      create_gran(gran1) %>%
      create_gran(gran2) %>%
      #as_tibble() %>%
      select(key,
             response,
             {{gran1}},
             {{gran2}})
  }

  # scale the response (raw - marginal median/marginal IQR)

  data <- unite(sm_gran, category, -c(all_of(key), all_of(response), all_of(index)), sep = "-")

  sm_gran_quant <- data %>%
    group_by(!!sym(key), category) %>%
    summarise(q2 = stats::median(!!sym(response), na.rm = TRUE),
              iqr = stats::IQR(!!sym(response), na.rm = TRUE),  .groups = 'drop')

  data %>%
    left_join(sm_gran_quant, by = c(key, "category")) %>%
    dplyr::mutate(scaled_response = (!!sym(response) - q2)/iqr) %>%
    select(-q2, -iqr) %>%
    ungroup() %>%
    as_tsibble(index = index, key = key) %>%
    select(all_of(key), all_of(index), category, all_of(response), scaled_response)

}

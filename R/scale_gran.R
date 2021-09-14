#' robust_scaling of data marginal on categories of different granularities
#'
#' @param .data a tsibble
#' @param gran1 one granularity e.g. hour_day, day_week, wknd_wday
#' @param gran2 one granularity distinct from gran1
#' @param response measured variable
#' @param method scaling method like "robust" (Robust sscaling), "nqt" (Normal-quantile transform)
#'
#' @return scale_gran
#'
#' @examples
#' library(gravitas)
#' library(tidyverse)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
#' gran1 = "hour_day"
#' gran2 = NULL
#' response = "general_supply_kwh"
#'scale_gran(sm, response, method = "none")
#'scale_gran(sm, response, method = "robust")
#'scale_gran(sm, response, method = "nqt")

#' #todo : can add NQT marginal on each category?
#' @export
scale_gran = function(.data,
                             response = NULL,
                             method = "none"){ #, c("none", robust", "nqt")



  key =  tsibble::key(.data)
  key = key[1] %>% as.character()

  index =  tsibble::index(.data) %>% as.character()

  if(is.null(response)){
    response =  tsibble::measured_vars(.data)
    response = response[1]
  }

if(method == "robust"){
  summarise_data <- .data %>%
    as_tibble() %>%
    group_by(!!sym(key)) %>%
    summarise(q2 = stats::median(!!sym(response), na.rm = TRUE),
              iqr = stats::IQR(!!sym(response), na.rm = TRUE),  .groups = 'drop')

  data <- .data %>%
    left_join(summarise_data, by = c(key)) %>%
    dplyr::mutate(m = (!!sym(response) - q2)/iqr) %>%
    select(-q2, -iqr) %>%
    dplyr::mutate(!!response := m) %>%
    select(-m) %>%
    ungroup() %>%
    as_tsibble(index = index, key = key)

  # %>%  select(all_of(key), all_of(index), all_of(response), scaled_response)
}

  if(method == "nqt"){
    data <- .data %>%
      as_tibble() %>%
      group_by(!!sym(key)) %>%
      dplyr::mutate(
        m = stats::qqnorm(!!sym(response), plot.it=FALSE)$x) %>%
      dplyr::mutate(!!response := m) %>%
      select(-m) %>%
      ungroup()%>%
      as_tsibble(index = index, key = key)
  }

  if(method == "none")
  {
  data <- .data
  }

  return(data)
}


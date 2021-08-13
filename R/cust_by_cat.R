#' Title
#'
#' @param .data
#' @param gran1
#' @param gran2
#' @param response
#'
#' @return
#' @export
#'
#' @examples
#' library(gravitas)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
#' gran1 = "hour_day"
#' gran2 = NULL
#' v1 = cust_by_cat(sm, "hour_day")
#' v1
cust_by_cat <-  function(.data,
                        gran1 = NULL,
                        gran2 = NULL,
                        response = NULL){

if(is.null(key)){
  key =  tsibble::key(.data)
  key = key[1] %>% as.character()
}

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
              values_from = response)
}

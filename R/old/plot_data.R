#' robust_scaling of data marginal on categories of different granularities
#'
#' @param .data
#' @param gran1
#' @param gran2
#' @param response
#' @param method
#'
#' @return plot_data
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
#' v2 <- suppressWarnings(plot_data(sm, "hour_day"))
#' v2
#'sm <- robust_scale_data(sm, "hour_day")
#'v3 <- suppressWarnings(plot_data(sm, "hour_day", response = "scaled_response"))
#' #todo : can add NQT marginal on each category?
plot_data = function(.data,
                      gran1 = c, # can't be kept NULL
                      gran2 = NULL,
                      response = NULL,
                      quantile_prob_val = c(0.25, 0.5, 0.75),
                      nfacet = NULL,
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

# made category with data
sm_category <- unite(sm_gran, category, -c(all_of(key), all_of(response), all_of(index)), sep = "-")

# dataframe with keys as rows and catgories as column with each cell as a list
sm_category_list <- sm_category %>%
    as_tibble() %>%
    select(customer_id, category, all_of(response)) %>%
    pivot_wider(names_from = category,
                values_from = all_of(response))

ncol_sm <- seq_len(ncol(sm_category_list[-c(1)]))
nrow_sm <- seq_len(nrow(sm_category_list))



tab <- expand.grid(x = nrow_sm, y = ncol_sm)

sm_hod_quantiles_cat <- purrr::pmap(tab,
                                  function(x, y){

                                      sm_category_list[-1] %>%
                                               magrittr::extract2(x, y) %>% unlist() %>% quantile(prob = quantile_prob_val, na.rm =TRUE)
})  %>% bind_rows() %>% bind_cols(tab) %>%
  rename("customer_serial_id" = "x",
         "category_id" = "y")

ref_cat <- names(sm_category_list)[-c(1)] %>% as_tibble() %>% set_names("category") %>%
  mutate(category_id = row_number())

ref_cust <-sm_category_list %>%
  pull(all_of(key)) %>%
  as_tibble() %>%
  set_names(key) %>%
  mutate(customer_serial_id = row_number())


sm_quantiles_ref <- sm_hod_quantiles_cat %>%
  mutate(customer_serial_id = as.integer(customer_serial_id),
         category_id = as.integer(category_id)) %>%
  left_join(ref_cat, by = "category_id") %>%
  left_join(ref_cust, by = "customer_serial_id") %>%
  select(customer_serial_id, category_id, category, key, everything())

#data_heatmap <- sm_quantiles_ref %>% distinct(!!sym(key))

sm_quantiles_ref %>%
  ggplot(aes(x = as.integer(category))) +
  geom_ribbon(aes(ymin = `25%`,
                  ymax = `75%`), fill = "lightblue") +
  geom_line(aes(y = `50%`), size = 0.7) +
  facet_wrap(~customer_id,
             scales = "free_y",
             labeller = "label_value",
             ncol = nfacet) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + xlab("hour-of-day") + ylab("demand (in Kwh)")

}

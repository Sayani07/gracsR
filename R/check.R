library(gravitas)
library(tidyverse)

sm <- smart_meter10 %>%
filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
gran1 = "hour_day"
gran2 = NULL
response = "general_supply_kwh"
v2 <- suppressWarnings(robust_scale_data(sm, "hour_day"))




quantile_q2 <-  function(x){
  y = quantile(x, probs = c(0.5))
  #c(y[1], y[2]) %>% as_tibble() %>% bind_cols(names(y)) %>% set_names(c("quant_value", "quantile"))
}


quantile_q1 <-  function(x){
  y = quantile(x, probs = c(0.25))
  #c(y[1], y[2]) %>% as_tibble() %>% bind_cols(names(y)) %>% set_names(c("quant_value", "quantile"))
}


quantile_q3 <-  function(x){
  y = quantile(x, probs = c(0.75))
  #c(y[1], y[2]) %>% as_tibble() %>% bind_cols(names(y)) %>% set_names(c("quant_value", "quantile"))
}


v2 %>%
dplyr::filter(customer_id %in% c("10006414")) %>%
  pivot_longer(c("general_supply_kwh", "scaled_response"),
               names_to = "kwh_type",
               values_to = "kwh_value") %>%
ggplot(aes(x = kwh_value, y = category), fill = "#999999") +
  ggridges::geom_density_ridges(alpha = 0.7) +
  facet_grid(kwh_type~customer_id, scales = "free") +
  coord_flip() +
  stat_summary(
    fun = quantile_q2,
    geom = 'line',
    aes(group = 1), size = 0.7, color = "blue") +
  theme(legend.position = "bottom") +
  stat_summary(
    fun = quantile_q1,
    geom = 'line',
    aes(group = 1), size = 0.7, color = "#D55E00") +
  theme(legend.position = "bottom") +
  stat_summary(
    fun = quantile_q3,
    geom = 'line',
    aes(group = 1), size = 0.7, color = "#D55E00") +
  #coord_flip() +
  theme(legend.position = "bottom") +
  theme_bw()

# library(tidyverse)
#
# # for loop
#
# for (x in 1:2){
#   for(y in 1:2){
#     for(z in 1:5){
#       dist_data[x, y] = x*y + y*z
#     }
#   }
#
# }
#
#
#
# tab <- expand.grid(x = 1:2, y = 1:2, z =1:5)
# tab
#
# # Using map
#
#
#
# # using pmap
#
# dist_data <- purrr::pmap(tab,
#                                   function(x, y, z){
#                                     value3 =
#                                       x*y + y*z
# }) %>%
#   unlist () %>%
#   as_tibble() %>%
#   bind_cols(tab)

sm <- smart_meter10 %>%
filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
gran1 = "hour_day"
gran2 = NULL


key =  tsibble::key(.data)
key = key[1] %>% as.character()

index =  tsibble::index(.data) %>% as.character()

if(is.null(response)){
  response =  tsibble::measured_vars(.data)
  response = response[1]
}

data <- .data %>%
  as_tibble() %>%
  group_by(!!sym(key)) %>%
  dplyr::mutate(scaled_response = stats::qqnorm(!!sym(response), plot.it=FALSE)$x) %>%
  select(-general_supply_kwh) %>%
  pivot_wider(-c(3), names_from = "customer_id", values_from = scaled_response)




# example plot

set.seed(123)
x1 = rgamma(10, shape =1 ) %>% as_tibble()
x2 = rcauchy(10) %>% as_tibble()
x3= rnorm(10) %>% as_tibble()

tbl <- bind_rows(x1, x2, x3, .id = "dist") %>%
  #group_by(dist) %>%
  mutate(serial = row_number()) %>%
  ungroup()

tbls <- as_tsibble(tbl, index=serial, key = dist)

tbls_summarize <- tbls %>% group_by(dist) %>%
  dplyr::mutate(scaled_response = stats::qqnorm(value, plot.it=FALSE)$x)


tbls_summarize %>%
  as_tibble %>%
  select(-c(value, serial)) %>%
  pivot_wider(names_from = dist,
              values_from = scaled_response) %>% unnest() %>% View()

tbls_summarize%>% ggplot() + geom_density(aes(x=scaled_response, color = dist))

# real-data plot


sm_summarize <- sm %>%
  as_tibble() %>%
  group_by(!!sym(key)) %>%
  dplyr::mutate(scaled_response = stats::qqnorm(!!sym(response), plot.it=FALSE)$x)

sm_summarize %>% filter(customer_id== 10006414)
sm_summarize %>% filter(customer_id== 10006704)
sm_summarize %>% filter(customer_id== 10017936)
sm_summarize %>% filter(customer_id== 10018250)

sm_summarize %>%
  as_tibble %>%
  select(-c(general_supply_kwh, reading_datetime)) %>%
  pivot_wider(names_from = customer_id,
              values_from = scaled_response)

sm_summarize%>% ggplot() + ggridges::geom_den(aes(x=scaled_response, y = customer_id))


cust = sm$customer_id %>% unique

data_check <- map(1:4, function(i){
  sm %>%
    filter(customer_id == cust[i]) %>%
    dplyr::mutate(scaled_response = stats::qqnorm(!!sym(response), plot.it=FALSE)$x)
}) %>% bind_rows()

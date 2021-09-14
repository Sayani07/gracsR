#' compute distances and groups from algorithm based on raw distributions
#'
#' @param .data
#' @param gran1
#' @param gran2
#' @param response
#' @param quantile_prob_val
#'
#' @return
#' @export clust_gran
#'
#' @examples
#' library(gravitas)
#' library(tidyverse)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
#' gran1 = "hour_day"
#' gran2 = NULL
#' response = "general_supply_kwh"
#' clust_gran(v, "hour_day")
#' clust_gran(v, "month_year")
#' sm %>% scale_gran(method = "robust") %>% clust_gran("hour_day")

clust_gran <-  function(dist,
                        kopt = NULL,
                        method = "ward.D"){


  hc = stats::hclust(dist,method="ward.D")

  nmaxclust <- dist %>% broom::tidy() %>% nrow()

  if(is.null(kopt)){
    koptimal = fpc::nselectboot(d,
                                B = 50,
                                method = "complete",
                                clustermethod = fpc::disthclustCBI,
                                classification = "averagedist",
                                krange = 2:nmaxclust)

    kopt = koptimal$kopt


    groups <- tibble(group = cutree(hc, k=kopt))
    groups
  }
}


#' compute distances and groups from algorithm based on raw distributions
#'
#' @param dist a dissimilarity structure as produced by dist.
#' @param kopt the number of clusters
#' @param method method in stats::hclust()
#'
#' @return
#' @export clust_gran
#'
#' @examples
#' library(gravitas)
#' library(tidyverse)
#' library(tsibble)
#' sm <- smart_meter10 %>%
#' filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
#' gran1 = "hour_day"
#' gran2 = NULL
#' response = "general_supply_kwh"
#' sm %>% scale_gran(method = "robust") %>% dist_gran(gran1 = "hour_day")%>%
#' clust_gran("hour_day")
#' sm %>% scale_gran(method = "nqt") %>% dist_gran(gran1 = "hour_day")%>%
#' clust_gran()
#' dist_wpd(sm, harmony_tbl = h)) %>% clust_gran()

clust_gran <-  function(dist,
                        kopt = NULL,
                        method = "ward.D"){


  hc = stats::hclust(dist,method=method)

  nmaxclust <- c(dist %>% broom::tidy() %>% pull(item1),dist %>% broom::tidy() %>% pull(item2)) %>% unique() %>% length()

  if(is.null(kopt)){
    koptimal = fpc::nselectboot(dist,
                                B = 50,
                                method = "complete",
                                clustermethod = fpc::disthclustCBI,
                                classification = "averagedist",
                                krange = 2:nmaxclust)
}
    kopt = koptimal$kopt


    groups <- tibble(group = cutree(hc, k=kopt))
    groups
}


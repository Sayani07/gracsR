plot_clust <-  function(.data = NULL,
                        response = NULL,
                        groups = NULL,
                        quantile_prob_val = c(0.25, 0.5, 0.75),
                        nfacet = NULL)
 {
  .data %>% scale_gran(method = c( “robust”, “nqt”)) %>% dist_gran() %>% clust_gran()
  .data %>% scale_gran(method=“none”) %>% dist_gran() %>% clust_gran()


 }



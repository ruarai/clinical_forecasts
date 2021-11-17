
make_quants <- function(tbl) {
  data_matrix <- tbl %>%
    select(starts_with("sim_")) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with("sim_"))
  
  medians <- data_matrix %>%
    matrixStats::rowMedians() %>%
    tibble(median = .)
  
  probs <- c(0.5, 0.75, 0.9, 0.95, 0.99)
  
  quant_probs <- c(rev(1 - probs) / 2, 0.5 + probs / 2)
  quant_names <- c(str_c("lower_", rev(probs) * 100), str_c("upper_", probs * 100))
  
  quants <- data_matrix %>%
    matrixStats::rowQuantiles(probs = quant_probs) %>%
    `colnames<-`(quant_names) %>%
    as_tibble() %>%
    bind_cols(id_tbl, .) %>%
    pivot_longer(cols = -all_of(colnames(id_tbl)),
                 names_to = c("type", "quant"),
                 names_sep = "_") %>%
    pivot_wider(names_from = "type",
                values_from = "value") %>%
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}

runp <- function(){
  shiny::runApp("inst/shiny")
}

get_nmb <- function(threshold, x){
  threshold * x$m_eff - x$m_cost
}

get_nmb_tbl <- function(x, min, max, by){
  thresholds <- seq.int(min, max, by)
  res <- map_df(thresholds , function(threshold){
    line <- map_df(x, function(y){
      get_nmb(threshold, y)
    }) %>% apply(1, which.max) %>%
      table() %>%
      prop.table() %>%
      as.vector()
    matrix(line, nrow = 1) %>%
      as_tibble()
  })
  names(res) <- names(x)
  res %>%s
    mutate(threshold = thresholds) %>%
    gather("strategy", "proportion", -threshold)
}

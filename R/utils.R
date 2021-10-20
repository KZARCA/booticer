runp <- function(){
  shiny::runApp("inst/shiny")
}

get_nmb <- function(threshold, x){
  threshold * x$m_eff - x$m_cost
}

get_nmb_tbl <- function(x, min, max, by){
  thresholds <- seq.int(min, max, by)
  res <- map_df(thresholds , function(threshold){
    lines <- purrr:::map_dfc(x, function(y){
      get_nmb(threshold, y)
    }) %>%
      apply(1, which.max) %>%
      table() %>%
      prop.table()
    matrix(lines, nrow = 1, dimnames = list(NULL, names(lines))) %>%
      as_tibble() -> p
    purrr::map_dfc(names(p), function(z) dplyr::bind_cols(max(0,p[[z]])))
  })
  names(res) <- names(x)
  res
   res %>%
     mutate(threshold = thresholds) %>%
     gather("strategy", "proportion", -threshold)
}

#' @export
force_range <- function(x, mini, maxi){
  max(min(x, maxi), mini)
}

#' @export
pourcent <- function(nb, symbol = TRUE, round = NULL){
  map_chr(nb, function(x){
    if (is.nan(x) | is.na(x)) return("-")
    if (is.null(round)) round <- 2
    val <- x * 100
    val <- if (val > 1E-3) {
      base::format(val, digits = round, nsmall = round - 2)
    } else 0

    if (symbol) paste0(val, "%") else val
  })
}


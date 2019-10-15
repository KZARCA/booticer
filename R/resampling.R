#' Create non-parametric resamples with replacement of a data.frame
#'
#' @param tab A data frame with colnames containing "cost", "effect", and "strategy"
#' @param n Number of resamples
#' @param seed Parameter to reproduce the results
#'
#' @return A list with a length equal to the number of strategies containing a list of n
#' data frames
#' @export
#'
#' @examples
#'
get_resamples <- function(tab, n = 1000, seed = NULL) {
  set.seed(seed)
  if(!inherits(tab, "data.frame")){
    stop("tab must be a data.frame")
  }
  if (!"cost" %in% names(tab) | !"effect" %in% names(tab) | !"strategy" %in% names(tab)){
    stop("Your column names must include 'cost', 'effect' and 'strategy' (lower case)")
  }
  tab %>%
    select(cost, effect, strategy) %>%
    split(.$strategy)%>%
    map(function(x){
      map(seq_len(n), ~ sample_n(x, size = nrow(x), replace = TRUE))
    })
}

#' Get the mean cost and effect in each strategy
#'
#' @param res A list created with get_resamples
#'
#' @return A list with a length equal to the number of strategies containing a
#' data.frame with one line for each resample
#' @export
#'
#' @examples
#'
get_means <- function(res){
  map(res, function(x) {
    map_df(x, function(y) {
      summarise(y, m_cost = mean(cost), m_eff = mean(effect), strategy = unique(strategy))
    })
})
}

#' Get cost and effects difference in each strategy
#'
#' @param x A list created with get_means
#' @param reference A atomic character vector naming the reference strategy
#'
#' @return A data.frame with one line for each resample
#' @export
#'
#' @examples
#'
get_differences <- function(x, reference){
  if (!reference %in% names(x)){
    stop("The reference strategy is not specified in any strategy")
  }
  ref <- x[[reference]] # !data.frame
  #noref <- x[which(reference != names(x))] # !list
  imap_dfr(x, function(y, name){
    tibble(
      d_cost = y$m_cost - ref$m_cost,
      d_eff = y$m_eff - ref$m_eff,
      strategy = name
    )
  })

}

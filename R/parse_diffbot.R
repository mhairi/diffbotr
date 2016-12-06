#' Parse a 'diffbot_api' object into a dataframe.
#'
#' Take the results of get_diffbot and parse into dataframe.
#' Optionally include tag data or image data in final frame.
#'
#' @param diffbot_output A object of class diffbot_api.
#' @param include NULL for just the page data, 'images' to include
#' image data or 'tags' to include tags data.
#' @return A dataframe. If just the page data this will be a one row dataframe.
#' Otherwise will either have a row for each image or each tag.
#' @export
#' @examples
#' \dontrun{
#' url <- 'http://techcrunch.com/2012/05/31/diffbot-raises-2-million-seed-round-for-web-content-extraction-technology/'
#' api_key <- '12345'
#'
#' t <- get_diffbot(url, api_key)
#'
#' parse_diffbot(t)
#' parse_diffbot(t, 'tags')
#' }
#'
#' @examples
parse_diffbot <- function(diffbot_output, include = NULL){

  if (!inherits(diffbot_output, 'diffbot_api')) stop('Can only parse objects of class diffbot_api')

  include_options <- c('images', 'tags')
  if (!is.null(include)) if (!is.element(include, include_options)) stop('include must be in: ', include_options)

  df <- as.data.frame(purrr::discard(diffbot_output$content$objects[[1]], is.list), stringsAsFactors = FALSE)

  if (is.null(include)) return(df)

  if (include == 'tags'){
    df$tags <- suppressWarnings(list(purrr::map_df(diffbot_output$content$objects[[1]]$tags, data.frame)[, 1:4]))
    return(tidyr::unnest(df))
  }

  if (include == 'images'){
    df$images <- suppressWarnings(list(purrr::map_df(diffbot_output$content$objects[[1]]$images, data.frame)[, -3]))
    return(tidyr::unnest(df))
  }

}

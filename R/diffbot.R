library(httr)

#' Get data about a URL from Diffbot, using the analyse API
#'
#' Get data for a URL from the analyse API.
#' For more information see \href{https://www.diffbot.com/dev/docs/}{Diffbot's documenation}.
#'
#' @param url The URL to analyse.
#' @param api_key Your API key (also called token in the diffbot documenation).
#' @param mode Setting to NULL uses the analyse API, where Diffbot tries to
#'  dermine the page type and analyse accordingly. Other options are: 'article',
#'   'discussion', 'image', 'product' or 'video'.
#' @param fallback If analyse API can't find the correct page type it will use this as a default. Options
#'  are: 'article', 'discussion', 'image', 'product' or 'video'.
#' @param fields Depends on the API used. See each APIs documatation page for details.
#' @param discussion Set to FALSE to not extract comments for articles and products.
#' @param timeout How long to wait before timing out request for URL's content.
#' @param callback Used for jsonp requests. See documentation.
#'
#' @return An object of class 'diffbot_api'; a list of information returned from Diffbot.
#'
#' @examples
#' \dontrun{
#' api_key <- '12345'
#' url <- 'http://techcrunch.com/2012/05/31/diffbot-raises-2-million-seed-round-for-web-content-extraction-technology/'
#'
#' get_diffbot(url, api_key)
#' get_diffbot(url, api_key, fallback = 'article', discussion = FALSE)
#' }
#' @export
get_analyse <- function(url,
                        api_key,
                        mode = NULL,
                        fallback = NULL,
                        fields = NULL,
                        discussion = TRUE,
                        timeout = NULL,
                        callback = NULL) {


  modes <- c('article', 'discussion', 'image', 'product', 'video')

  if (!is.null(mode))       if(!(mode %in% modes))      stop('mode must be one of: ', paste(modes, collapse = ', '), '.')
  if (!is.null(fallback))   if(!(fallback %in% modes))  stop('fallback must be one of: ', paste(modes, collapse = ', '), '.')
  if (!is.null(discussion)) if(!is.logical(discussion)) stop('discussion must be TRUE or FALSE (or NULL)')

  ### Checking correct fields and converting to API style list ###

  accepted_fields <- list(
    'article'    = c('sentiment', 'links', 'meta', 'querystring'),
    'discussion' = c('sentiment', 'links', 'meta', 'querystring', 'breadcrumb'),
    'image'      = c('links', 'meta', 'querystring', 'breadcrumb', 'displayWidth', 'displayHeight'),
    'product'    = c('links', 'meta', 'querystring', 'breadcrumb'),
    'video'      = c('links', 'meta', 'querystring', 'breadcrumb')
  )

  if (!is.null(fields)){

    if (!is.null(mode)){

       wrong_fields <- setdiff(fields, accepted_fields[[mode]])

       if (length(wrong_fields) != 0) stop('Fields: ', paste(wrong_fields, collapse = ', '), 'not avalible for mode ', mode, '.', .call = FALSE)

    } else { #using analyse

      accepted_fields <- Reduce(union, accepted_fields)

      wrong_fields <- setdiff(fields, accepted_fields)

      if (length(wrong_fields) != 0) stop('Fields: ', paste(wrong_fields, sep = ', '), 'not avalible.')

    }

    fields <- paste(fields, collapse = ',')

  }

  if (!is.null(discussion)) discussion <- tolower(as.character(discussion))

  parameters <- list(
    'url'        = url,
    'token'      = api_key,
    'mode'       = mode,
    'fallback'   = fallback,
    'fields'     = fields,
    'discussion' = discussion,
    'timeout'    = timeout,
    'callback'   = callback
  )

  # Remove null parameters
  parameters <- Filter(function(x) !is.null(x), parameters)

  url <- httr::modify_url('http://api.diffbot.com/v3/analyze', query = parameters)

  response <- httr::GET(url)

  httr::stop_for_status(response)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  content <- httr::content(response, "text")
  parsed  <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  if(!is.null(parsed$errorCode)) if(parsed$errorCode != '200') stop('Error: ', parsed$errorCode, '. ', parsed$error)

  structure(
    list(
      request_type= 'analyse',
      content     = parsed,
      response    = response,
      raw_content = content
    ),
    class = "diffbot_api"
  )

}

#' Get data about a URL from Diffbot, using the image API
#'
#' Get data for a URL from the image API.
#' For more information see \href{https://www.diffbot.com/dev/docs/}{Diffbot's documenation}.
#'
#' @param url The URL to analyse.
#' @param api_key Your API key (also called token in the diffbot documenation).
#' @param fields Extra fields, as a vector. Options are: links, meta, querystring, breadcrumb, displayWidth, displayHeight.
#' @param timeout How long to wait before timing out request for URL's content.
#' @param callback Used for jsonp requests. See documentation.
#'
#' @return An object of class 'diffbot_api'; a list of information returned from Diffbot.
#'
#' @examples
#' \dontrun{
#' api_key <- '12345'
#' url <- 'http://techcrunch.com/2012/05/31/diffbot-raises-2-million-seed-round-for-web-content-extraction-technology/'
#'
#' get_diffbot(url, api_key)
#' get_diffbot(url, api_key, fallback = 'article', discussion = FALSE)
#' }
#' @export
get_image <- function(url,
                      api_key,
                      fields = NULL,
                      timeout = NULL,
                      callback = NULL) {


    ### Checking correct fields and converting to API style list ###

  accepted_fields <- c('links', 'meta', 'querystring', 'breadcrumb', 'displayWidth', 'displayHeight')

  if (!is.null(fields)){

      wrong_fields <- setdiff(fields, accepted_fields[[mode]])

      if (length(wrong_fields) != 0) stop('Fields: ', paste(wrong_fields, collapse = ', '), 'not avalible for Image API.', .call = FALSE)

      fields <- paste(fields, collapse = ',')
  }

  parameters <- list(
    'url'        = url,
    'token'      = api_key,
    'fields'     = fields,
    'timeout'    = timeout,
    'callback'   = callback
  )

  # Remove null parameters
  parameters <- Filter(function(x) !is.null(x), parameters)

  url <- httr::modify_url('http://api.diffbot.com/v3/image', query = parameters)

  response <- httr::GET(url)

  httr::stop_for_status(response)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  content <- httr::content(response, "text")
  parsed  <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  if(!is.null(parsed$errorCode)) if(parsed$errorCode != '200') stop('Error: ', parsed$errorCode, '. ', parsed$error)

  structure(
    list(
      request_type= 'image',
      content     = parsed,
      response    = response,
      raw_content = content
    ),
    class = "diffbot_api"
  )

}

#' Print a diffbot API object
#'
#' @param diffbot_return An object of class 'diffbot_api'; a list of information returned from Diffbot.
#' @return
#'
#' @examples
#' \dontrun{
#' api_key <- '12345'
#' url <- 'http://techcrunch.com/2012/05/31/diffbot-raises-2-million-seed-round-for-web-content-extraction-technology/'
#'
#' get_diffbot(url, api_key)
#' }
#' @export
print.diffbot_api <- function(diffbot_return){
  cat('Diffbot response for: ', diffbot_return$content$request$pageUrl, '\n',
      'Status code: ', diffbot_return$response$status_code, '\n',
      'Requested API: ', diffbot_return$request_type, '\n',
      'Actual API: ', diffbot_return$content$request$api, '\n',
      'Objects returned: ', length(diffbot_return$content$objects))
}





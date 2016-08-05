library(httr)

get_diffbot <- function(url,
                        api_key,
                        mode = NULL,
                        fallback = NULL,
                        fields = NULL,
                        discussion = NULL,
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

  url <- modify_url('http://api.diffbot.com/v3/analyze', query = parameters)

  print(url)

  response <- GET(url)

  stop_for_status(response)

  if (http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(content(response, "text"), simplifyVector = FALSE)

  print(parsed)

  if(!is.null(parsed$errorCode)) if(parsed$errorCode != '200') stop('Error: ', parsed$errorCode, '. ', parsed$error)

  structure(
    list(
      content  = parsed,
      response = response
    ),
    class = "diffbot_api"
  )

}

print.diffbot_api <- function(test){
  cat('Diffbot response for: ', test$content$request$pageUrl, '\n',
      'Status code: ', test$response$status_code, '\n',
      'API: ', test$content$request$api, '\n',
      'Objects returned: ', length(test$content$objects[[1]]))
}







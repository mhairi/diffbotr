context('Do we have the expected response for the image API')

test_that('Basic call works', {
  url <- 'http://techcrunch.com/2012/05/31/diffbot-raises-2-million-seed-round-for-web-content-extraction-technology/'
  api_key <- '51cc26f6d2be06a53e51fa8a1e64d567'

  t <- get_image(url, api_key)

  expect_equal(t$content$request$pageUrl, url)
  expect_equal(t$response$status_code, 200)
  expect_equal(t$content$request$api, 'image')
  expect_equal(length(t$content$objects), 5)
})

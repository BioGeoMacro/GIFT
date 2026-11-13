# Tests for internal check functions ----

test_that("check_api() fails gracefully when the server is unreachable", {
  # 127.0.0.1:9 refuses the connection immediately; before the fix the
  # graceful message was followed by an uncaught httr2 error
  expect_null(suppressMessages(check_api("http://127.0.0.1:9/")))
  expect_message(check_api("http://127.0.0.1:9/"))
})

test_that("check_query() fails gracefully when the server is unreachable", {
  expect_null(suppressMessages(check_query("http://127.0.0.1:9/")))
  expect_message(check_query("http://127.0.0.1:9/"))
})

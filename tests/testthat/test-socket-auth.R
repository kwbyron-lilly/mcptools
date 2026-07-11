# mac_seal() / mac_open() -----------------------------------------------

test_that("mac_open() reverses mac_seal()", {
  local_socket_secret()
  payload <- serialize(list(a = 1, b = "two"), NULL)

  expect_identical(mac_open(mac_seal(payload)), payload)
})

test_that("mac_open() rejects a tampered MAC or payload", {
  local_socket_secret()
  wire <- mac_seal(serialize(list(a = 1), NULL))

  flip <- function(wire, i) {
    wire[i] <- as.raw(bitwXor(as.integer(wire[i]), 1L))
    wire
  }

  expect_null(mac_open(flip(wire, 1L)))
  expect_null(mac_open(flip(wire, length(wire))))
})

test_that("mac_open() rejects a wrong secret and malformed input", {
  local_socket_secret("secret-a")
  wire <- mac_seal(serialize(list(a = 1), NULL))

  the$socket_secret <- "secret-b"
  expect_null(mac_open(wire))

  expect_null(mac_open(as.raw(1:10)))
  expect_null(mac_open("not raw"))
})

# socket_secret() -------------------------------------------------------

test_that("socket_secret() creates a stable secret under the socket dir", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  withr::local_envvar(MCPTOOLS_SOCKET_DIR = dir)
  old_url <- the$socket_url
  withr::defer(the$socket_url <- old_url)
  the$socket_url <- socket_url()
  local_socket_secret(NULL)

  first <- socket_secret()
  expect_true(file.exists(file.path(dir, "secret")))

  the$socket_secret <- NULL
  expect_identical(socket_secret(), first)
})

# validate_session_message() --------------------------------------------

test_that("validate_session_message() accepts well-formed requests", {
  expect_null(validate_session_message(list(id = 1, method = "tools/list")))
  expect_null(validate_session_message(list(
    id = 1,
    method = "tools/call",
    tool = function() 1,
    params = list(arguments = list())
  )))
})

test_that("validate_session_message() rejects malformed requests", {
  expect_equal(validate_session_message(42)$error$code, -32600)
  expect_equal(validate_session_message(list(id = 1))$error$code, -32600)
  expect_equal(
    validate_session_message(list(
      id = 1,
      method = "tools/call",
      tool = "system",
      params = list()
    ))$error$code,
    -32600
  )
})

# handle_message_from_server() ------------------------------------------

test_that("handle_message_from_server() drops an unauthenticated tools/call", {
  local_socket_secret()
  executed <- FALSE
  local_mocked_bindings(
    schedule_handle_message_from_server = function() invisible(),
    execute_tool_call = function(...) executed <<- TRUE
  )
  testthat::local_mocked_bindings(pipe_id = function(...) 1L, .package = "nanonext")

  attack <- serialize(
    list(
      method = "tools/call",
      tool = base::system,
      params = list(arguments = list("id"))
    ),
    NULL
  )
  forged <- c(as.raw(openssl::sha256(attack, key = "wrong-secret")), attack)

  expect_invisible(handle_message_from_server(forged))
  expect_false(executed)
})

test_that("handle_message_from_server() dispatches an authenticated call", {
  local_socket_secret()
  seen <- NULL
  local_mocked_bindings(
    schedule_handle_message_from_server = function() invisible(),
    execute_tool_call = function(data) {
      seen <<- data
      jsonrpc_response(data$id, result = list())
    },
    session_send = function(text, pipe) text
  )
  testthat::local_mocked_bindings(pipe_id = function(...) 1L, .package = "nanonext")

  msg <- list(
    id = 5L,
    method = "tools/call",
    tool = function() 1,
    params = list(arguments = list())
  )
  handle_message_from_server(mac_seal(serialize(msg, NULL)))

  expect_equal(seen$id, 5L)
})

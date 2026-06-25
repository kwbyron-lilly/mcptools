skip_if(is_fedora())

local_inproc_url <- function() {
  paste0(
    "inproc://mcptools-",
    paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = "")
  )
}

test_that("roundtrip mcp_server and mcp_tools (stdio)", {
  previous_server_processes <- names(the$server_processes)

  # example-config configures `Rscript -e "mcptools::mcp_server()"`
  example_config <- readLines(system.file(
    "example-config.json",
    package = "mcptools"
  ))
  example_config <- gsub("Rscript", rscript_binary(), example_config)
  tmp_file <- withr::local_tempfile(fileext = ".json")
  writeLines(example_config, tmp_file)

  tools <- mcp_tools(tmp_file)
  withr::defer(
    the$server_processes[[
      setdiff(names(the$server_processes), previous_server_processes)
    ]]$kill()
  )
  tool_names <- c()
  for (tool in tools) {
    tool_names <- c(tool_names, tool@name)
  }
  expect_true(
    all(c("list_r_sessions", "select_r_session") %in% tool_names)
  )
  list_r_sessions_ <- tools[[which(tool_names == "list_r_sessions")]]
  expect_equal(list_r_sessions_tool@description, list_r_sessions_@description)
})

test_that("roundtrip mcp_server and mcp_tools (http)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(nzchar(Sys.which("npx")), "npx not available")

  http_server <- processx::process$new(
    command = rscript_binary(),
    args = c(
      "-e",
      "mcptools::mcp_server(type = 'http', port = 8080)"
    ),
    stdout = "|",
    stderr = "|"
  )
  withr::defer(http_server$kill())

  Sys.sleep(2)

  if (!http_server$is_alive()) {
    stop("HTTP server failed to start")
  }

  tools <- mcp_tools(system.file(
    "example-config-remote.json",
    package = "mcptools"
  ))

  tool_names <- c()
  for (tool in tools) {
    tool_names <- c(tool_names, tool@name)
  }
  expect_true(
    all(c("list_r_sessions", "select_r_session") %in% tool_names)
  )
  list_r_sessions_ <- tools[[which(tool_names == "list_r_sessions")]]
  expect_equal(list_r_sessions_tool@description, list_r_sessions_@description)
})

test_that("check_not_interactive errors informatively", {
  testthat::local_mocked_bindings(interactive = function(...) TRUE)

  expect_snapshot(error = TRUE, mcp_server())
})

test_that("forward_request returns append_tool_fn errors", {
  old_server_tools <- the$server_tools
  withr::defer(the$server_tools <- old_server_tools)

  set_server_tools(NULL)

  res <- forward_request(list(
    id = 1,
    method = "tools/call",
    params = list(name = "missing_tool", arguments = list())
  ))

  expect_s3_class(res, "jsonrpc_error")
  expect_equal(res$error$message, "Method not found")
})

test_that("forward_request times out when session does not respond", {
  old_socket_url <- the$socket_url
  old_server_socket <- the$server_socket
  old_server_tools <- the$server_tools
  withr::defer({
    nanonext::reap(the$server_socket)
    the$socket_url <- old_socket_url
    the$server_socket <- old_server_socket
    the$server_tools <- old_server_tools
  })

  test_tool <- ellmer::tool(function() "ok", "Test tool", name = "test_tool")
  set_server_tools(list(test_tool), session_tools = FALSE)
  testthat::local_mocked_bindings(session_response_timeout = function() 10L)

  the$socket_url <- local_inproc_url()
  session_socket <- nanonext::socket("poly")
  withr::defer(nanonext::reap(session_socket))
  session_url <- sprintf("%s%d", the$socket_url, 1L)
  expect_identical(nanonext::listen(session_socket, url = session_url), 0L)

  the$server_socket <- nanonext::socket("poly")
  expect_identical(
    nanonext::dial(the$server_socket, url = session_url, autostart = NA),
    0L
  )

  res <- forward_request(list(
    id = 1,
    method = "tools/call",
    params = list(name = "test_tool", arguments = list())
  ))

  expect_equal(res$error$code, -32603)
  expect_match(res$error$message, "Timed out waiting")
})

test_that("forward_request ignores stale responses", {
  old_socket_url <- the$socket_url
  old_server_socket <- the$server_socket
  old_server_tools <- the$server_tools
  withr::defer({
    nanonext::reap(the$server_socket)
    the$socket_url <- old_socket_url
    the$server_socket <- old_server_socket
    the$server_tools <- old_server_tools
  })

  test_tool <- ellmer::tool(function() "ok", "Test tool", name = "test_tool")
  set_server_tools(list(test_tool), session_tools = FALSE)
  testthat::local_mocked_bindings(session_response_timeout = function() 20L)

  the$socket_url <- local_inproc_url()
  session_socket <- nanonext::socket("poly")
  withr::defer(nanonext::reap(session_socket))
  session_url <- sprintf("%s%d", the$socket_url, 1L)
  expect_identical(nanonext::listen(session_socket, url = session_url), 0L)

  the$server_socket <- nanonext::socket("poly")
  expect_identical(
    nanonext::dial(the$server_socket, url = session_url, autostart = NA),
    0L
  )

  expect_identical(
    nanonext::send(
      session_socket,
      to_json(jsonrpc_response(99, result = list(ok = TRUE))),
      mode = "raw"
    ),
    0L
  )

  res <- forward_request(list(
    id = 1,
    method = "tools/call",
    params = list(name = "test_tool", arguments = list())
  ))

  expect_equal(res$error$code, -32603)
  expect_match(res$error$message, "Timed out waiting")
})

test_that("receive_forwarded_response errors for non-object JSON", {
  old_socket_url <- the$socket_url
  old_server_socket <- the$server_socket
  old_server_tools <- the$server_tools
  withr::defer({
    nanonext::reap(the$server_socket)
    the$socket_url <- old_socket_url
    the$server_socket <- old_server_socket
    the$server_tools <- old_server_tools
  })

  the$socket_url <- local_inproc_url()
  session_socket <- nanonext::socket("poly")
  withr::defer(nanonext::reap(session_socket))
  session_url <- sprintf("%s%d", the$socket_url, 1L)
  expect_identical(nanonext::listen(session_socket, url = session_url), 0L)

  the$server_socket <- nanonext::socket("poly")
  expect_identical(
    nanonext::dial(the$server_socket, url = session_url, autostart = NA),
    0L
  )

  expect_identical(nanonext::send(session_socket, "\"oops\"", mode = "raw"), 0L)

  res <- receive_forwarded_response(1, 10L)

  expect_equal(res$error$code, -32603)
  expect_match(res$error$message, "invalid response")
})

test_that("session_response_timeout is configurable", {
  withr::local_options(mcptools.session_response_timeout_seconds = NULL)
  withr::local_envvar(MCPTOOLS_SESSION_RESPONSE_TIMEOUT_SECONDS = "2")
  expect_equal(session_response_timeout(), 2000L)

  withr::local_options(mcptools.session_response_timeout_seconds = 1)
  expect_equal(session_response_timeout(), 1000L)

  withr::local_options(mcptools.session_response_timeout_seconds = -1)
  expect_equal(session_response_timeout(), 120000L)
})

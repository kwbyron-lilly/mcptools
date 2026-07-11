# Any local process can dial the session socket, so each message is framed as
# c(mac, payload) with an HMAC keyed by a per-user secret and verified before
# the payload is unserialized or dispatched; only the paired peer, which shares
# the secret under the 0700 socket dir, is acted on.

mac_length <- 32L

mac_seal <- function(payload) {
  mac <- as.raw(openssl::sha256(payload, key = socket_secret()))
  c(mac, payload)
}

# Returns the payload, or NULL if the MAC is absent or wrong.
mac_open <- function(wire) {
  if (!is.raw(wire) || length(wire) <= mac_length) {
    return(NULL)
  }

  mac <- wire[seq_len(mac_length)]
  payload <- wire[-seq_len(mac_length)]
  expected <- as.raw(openssl::sha256(payload, key = socket_secret()))

  if (!constant_time_equal(rawToHex(mac), rawToHex(expected))) {
    return(NULL)
  }

  payload
}

rawToHex <- function(x) {
  paste(format(x), collapse = "")
}

# Shared by every same-user session and server; cached after the first read.
socket_secret <- function() {
  if (!is.null(the$socket_secret)) {
    return(the$socket_secret)
  }

  the$socket_secret <- read_or_create_secret(secret_file())
  the$socket_secret
}

secret_file <- function() {
  dir <- socket_dir_in_use()
  if (is.null(dir)) {
    # Windows named pipes have no protected directory; this is best-effort and
    # not a security boundary (see ?mcp_session).
    dir <- file.path(
      Sys.getenv("TEMP", unset = tempdir()),
      paste0("mcptools-", Sys.info()[["user"]])
    )
  }
  file.path(dir, "secret")
}

# file.link() is an atomic exclusive create: concurrent creators race to link
# and losers read the winner's file, so all peers converge on one secret.
read_or_create_secret <- function(path) {
  if (file.exists(path)) {
    return(readLines(path, n = 1L, warn = FALSE))
  }

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE, mode = "0700")
  secret <- nanonext::random(32L)
  tmp <- tempfile(tmpdir = dirname(path))
  writeLines(secret, tmp)
  Sys.chmod(tmp, mode = "0600")

  linked <- suppressWarnings(file.link(tmp, path))
  unlink(tmp)
  if (!linked) {
    return(readLines(path, n = 1L, warn = FALSE))
  }

  secret
}

# Reject a malformed message before it reaches do.call(); returns a jsonrpc
# error to send back, or NULL when the message is well-formed.
validate_session_message <- function(data) {
  if (!is.list(data)) {
    return(jsonrpc_response(
      NULL,
      error = list(code = -32600, message = "Invalid request")
    ))
  }

  well_formed <- is_string(data$method) &&
    (!identical(data$method, "tools/call") ||
      (is.function(data$tool) && is.list(data$params)))

  if (!well_formed) {
    return(jsonrpc_response(
      data$id,
      error = list(code = -32600, message = "Invalid request")
    ))
  }

  NULL
}

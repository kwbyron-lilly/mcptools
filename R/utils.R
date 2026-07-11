jsonrpc_response <- function(id, result = NULL, error = NULL) {
  if (!xor(is.null(result), is.null(error))) {
    warning("Either `result` or `error` must be provided, but not both.")
  }

  drop_nulls(list(
    jsonrpc = "2.0",
    id = id,
    result = result,
    error = error
  ))
}

# Create a named list, ensuring that it's a named list, even if empty.
named_list <- function(...) {
  res <- list(...)
  if (length(res) == 0) {
    # A way of creating an empty named list
    res <- list(a = 1)[0]
  }
  res
}

to_json <- function(x, ...) {
  jsonlite::toJSON(x, ..., auto_unbox = TRUE, null = "null")
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

is_absolute_path <- function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
}

split_envvar <- function(x) {
  if (!nzchar(x)) {
    return(character())
  }

  trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}

first_nonempty_string <- function(...) {
  for (x in list(...)) {
    if (is_string(x) && nzchar(x)) {
      return(x)
    }
  }

  ""
}

url_origin <- function(x) {
  parsed <- url_parse_or_null(x)
  if (is.null(parsed$scheme) || is.null(parsed$hostname)) {
    return(NULL)
  }

  host <- parsed$hostname
  if (!is.null(parsed$port)) {
    host <- paste0(host, ":", parsed$port)
  }

  paste0(parsed$scheme, "://", host)
}

url_parse_or_null <- function(x) {
  tryCatch(
    httr2::url_parse(x),
    error = function(err) NULL
  )
}

# SSRF guard for server-side fetches of tool-supplied URLs: TRUE when `host` is
# an IP literal in a loopback, private, or link-local range, or resolves to
# loopback by name. curl normalizes octal/hex/integer IPv4 forms to
# dotted-decimal before we see the parsed host, so only dotted-quad and
# bracketed IPv6 literals need handling here. This is a literal-only block, not
# a DNS-rebinding defense: a hostname that resolves to a private address is not
# caught.
is_private_host_literal <- function(host) {
  host <- tolower(host %||% "")
  if (!nzchar(host)) {
    return(FALSE)
  }

  if (identical(host, "localhost") || endsWith(host, ".localhost")) {
    return(TRUE)
  }

  if (startsWith(host, "[") && endsWith(host, "]")) {
    host <- substr(host, 2L, nchar(host) - 1L)
  }

  if (grepl(":", host, fixed = TRUE)) {
    return(ipv6_is_private_or_loopback(host))
  }

  octets <- ipv4_literal_octets(host)
  if (!is.null(octets)) {
    return(ipv4_is_private_or_loopback(octets))
  }

  FALSE
}

ipv4_literal_octets <- function(host) {
  if (!grepl("^[0-9]{1,3}(\\.[0-9]{1,3}){3}$", host)) {
    return(NULL)
  }

  octets <- as.integer(strsplit(host, ".", fixed = TRUE)[[1]])
  if (any(octets > 255L)) {
    return(NULL)
  }

  octets
}

ipv4_is_private_or_loopback <- function(octets) {
  a <- octets[[1]]
  b <- octets[[2]]

  a == 0L || # 0.0.0.0/8 "this host"
    a == 10L || # 10.0.0.0/8 private
    a == 127L || # 127.0.0.0/8 loopback
    (a == 169L && b == 254L) || # 169.254.0.0/16 link-local
    (a == 172L && b >= 16L && b <= 31L) || # 172.16.0.0/12 private
    (a == 192L && b == 168L) # 192.168.0.0/16 private
}

ipv6_is_private_or_loopback <- function(host) {
  if (host %in% c("::1", "0:0:0:0:0:0:0:1", "::")) {
    return(TRUE)
  }

  # IPv4-mapped/-embedded addresses, e.g. ::ffff:127.0.0.1.
  embedded <- ipv4_literal_octets(sub("^.*:", "", host))
  if (!is.null(embedded)) {
    return(ipv4_is_private_or_loopback(embedded))
  }

  first <- strtoi(sub(":.*$", "", host), base = 16L)
  if (is.na(first)) {
    return(FALSE)
  }

  (first >= 0xfe80L && first <= 0xfebfL) || # fe80::/10 link-local
    (first >= 0xfc00L && first <= 0xfdffL) # fc00::/7 unique local
}

constant_time_equal <- function(x, y) {
  if (!is_string(x) || !is_string(y)) {
    return(FALSE)
  }

  x <- as.integer(charToRaw(enc2utf8(x)))
  y <- as.integer(charToRaw(enc2utf8(y)))
  n <- max(length(x), length(y))
  diff <- bitwXor(length(x), length(y))

  for (i in seq_len(n)) {
    x_i <- if (i <= length(x)) x[[i]] else 0L
    y_i <- if (i <= length(y)) y[[i]] else 0L
    diff <- bitwOr(diff, bitwXor(x_i, y_i))
  }

  identical(diff, 0L)
}

interactive <- NULL

mcptools_server_log <- function() {
  Sys.getenv("MCPTOOLS_SERVER_LOG", tempfile(fileext = ".txt"))
}

mcptools_client_log <- function() {
  Sys.getenv("MCPTOOLS_CLIENT_LOG", tempfile(fileext = ".txt"))
}

# from rstudio/reticulate
is_unix <- function() {
  identical(.Platform$OS.type, "unix")
}

is_fedora <- function() {
  if (is_unix() && file.exists("/etc/os-release")) {
    os_info <- readLines("/etc/os-release")
    any(grepl("Fedora", os_info))
  } else {
    FALSE
  }
}

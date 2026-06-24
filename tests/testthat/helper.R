on_windows <- function() {
  isTRUE(Sys.info()[['sysname']] == "Windows")
}

rscript_binary <- function() {
  if (on_windows()) {
    return(file.path(R.home("bin"), "Rscript.exe"))
  }

  file.path(R.home("bin"), "Rscript")
}

local_protocol_version <- function(
  protocol_version = the$protocol_version,
  env = parent.frame()
) {
  old_protocol_version <- the$protocol_version
  withr::defer(the$protocol_version <- old_protocol_version, envir = env)

  the$protocol_version <- protocol_version
  invisible(protocol_version)
}

# Environment interno para connection pooling
# La conexion se crea lazy (al primer uso), no al cargar el paquete
.ciecl_env <- new.env(parent = emptyenv())
.ciecl_env$con <- NULL
.ciecl_env$db_path <- NULL

.onUnload <- function(libpath) {
  if (!is.null(.ciecl_env$con)) {
    try(DBI::dbDisconnect(.ciecl_env$con), silent = TRUE)
    .ciecl_env$con <- NULL
    .ciecl_env$db_path <- NULL
  }
}

#'@export
use <- function(module, ...){
  m <- import_(module, ...)
  existings_vars <- ls(.GlobalEnv)
  for(v in ls(m)) {
    if(v %in% existings_vars) warning(sprintf("overriding %s ", v))
    .GlobalEnv[[v]] = m[[v]]
  }
}

is_module <- function() {
    rstudioapi::getActiveDocumentContext()$contents %>% 
        grep(pattern="#module") %>%
        any
}

evalInModule <- function() {
    code <- rstudioapi::primary_selection(rstudioapi::getActiveDocumentContext())$text
    
    if(!is_module()) {
        rstudioapi::sendToConsole(code)
    } else {
        module_path <- normalizePath(rstudioapi::getActiveDocumentContext()$path)
        path_parts <- strsplit(module_path, "/")[[1]]
        r_dir_idx <- which(path_parts == "R")
        relative_path <- paste(path_parts[r_dir_idx:length(path_parts)],
                               collapse="/")
        module <- substr(relative_path, 1, nchar(relative_path)-2) # remove extension
        rstudioapi::sendToConsole(paste0(code, "\n"),
                                  execute = FALSE,
                                  focus = FALSE)
        do_import(module_path=module_path,
                  module=module,
                  doc=FALSE,
                  custom_code=code)
    }
}

in_module <- function(module_name){
    list(name = module_name,
         path = find_module(module_name))
}

which_module <- function() {
    module_cmd <- rstudioapi::getActiveDocumentContext()$contents %>%
    stringr::str_extract(pattern = "^\\s*in_module\\(.*\\)") %>% 
        purrr::keep(~!is.na(.))
    if (identical(module_cmd, character(0))){
        return(NULL)
    }
    return(parse(text = module_cmd[[1]]) %>% 
               eval())
}

evalInModule <- function() {
    m <- which_module()
    code <- rstudioapi::primary_selection(rstudioapi::getActiveDocumentContext())$text
    if(is.null(m)) {
        rstudioapi::sendToConsole(code)
    } else {
        rstudioapi::sendToConsole(paste0("with_module(\"",m$name,"\", ",code,")"))
    }
}


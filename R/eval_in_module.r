in_module <- function(module_name){
    list(name = module_name,
         path = find_module(module_name))
}

which_module <- function() {
    module_cmd <- rstudioapi::getSourceEditorContext()$contents %>%
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
    code <- rstudioapi::primary_selection(rstudioapi::getSourceEditorContext())$text
    if(is.null(m)) {
        rstudioapi::sendToConsole(code)
    } else {
        exprs <- parse(text = code)
        for (i in seq_along(exprs)){
            rstudioapi::sendToConsole(paste0("with_module(\"",m$name,"\", ",deparse(exprs[[i]]),")"), 
                                      focus = FALSE)
        }
    }
}


#' @export
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
    s <- rstudioapi::primary_selection(rstudioapi::getSourceEditorContext());
    code <- s$text
    #TODO: parse with source files and use line information to jump to next
    #code start
    if (code == ""){
        current_row <- s$range[[1]][[1]]
        lines <- rstudioapi::getSourceEditorContext()$contents
        code_text = lines[[current_row]]
        for (i in 1:20){
            code <- tryCatch(parse(text = code_text),
                            error = function(e){e})  
            if (is(code, "error")){
                if (stringr::str_detect(code$message, "unexpected end of input") ){
                    code_text = paste(lines[current_row:(current_row+i)], collapse = "\n")
                } else {
                    break
                }
            } else {
                break
            }
        }
        code = code_text
        rstudioapi::setCursorPosition(position = rstudioapi::document_position(current_row+i,1))
    }
    
    if(is.null(m)) {
        rstudioapi::sendToConsole(code)
    } else {
        exprs <- parse(text = code)
        for (i in seq_along(exprs)){
            rstudioapi::sendToConsole(paste0("with_module(\"",m$name,"\", ",
                                             paste0(deparse(exprs[[i]]),
                                                    collapse = "\n")
                                             ,")"), 
                                      focus = FALSE)
        }
    }
}


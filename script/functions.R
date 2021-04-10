install_lib <- function(lib) {
    if (lib %in% rownames(installed.packages()) == FALSE) {
        install.packages(lib, dependencies=TRUE)
    }
    message(lib)
    library(eval(lib))
    
    return(TRUE)
}
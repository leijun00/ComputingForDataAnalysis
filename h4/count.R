count <- function(cause = NULL) {
    # Check that "cause" is non-NULL; else throw error
    if(is.null(cause)) {
        stop("Need to enter a valid cause")
    }
    # Check that specific "cause" is allowed; else throw error
    causes <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
    if(!cause %in% causes){
        stop(cause)
    }
    
    # Read "homicides.txt" data file
    homicides <- readLines("homicides.txt")

    # Extract causes of death
    # Return integer containing count of homicides for that cause
    if(cause == "asphyxiation"){
        count <- length(grep("[Aa]sphyxiation", homicides))
    } else if(cause == "blunt force"){
        count <- length(grep("[Cc]ause: [Bb]lunt [Ff]orce", homicides))
    } else if(cause == "shooting"){
        count <- length(grep("[Cc]ause: [Ss]hooting", homicides))
    } else if(cause == "stabbing"){
        count <- length(grep("[Cc]ause: [Ss]tabbing", homicides))
    } else if(cause == "other"){
        count <- length(grep("[Cc]ause: [Oo]ther", homicides))
    } else { # cause == "unknown"
        count <- length(grep("[Cc]ause: [Uu]nknown", homicides))
    }
    count
}
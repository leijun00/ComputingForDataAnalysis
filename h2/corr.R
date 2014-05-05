corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    data <- lapply(1:332, function(x) getmonitor(x, directory))
    completedata <- lapply(data, function(x) na.omit(x))
    dims <- lapply(completedata, function(x) dim(x)[1])
    index <- 1
    filterdata <- list()
    for(i in 1:length(dims)) {
        if(dims[[i]] > threshold){
    		filterdata[[index]] <- completedata[[i]]
        	index <- index + 1
        }
    }
    
    if(length(filterdata)>0){
    	cr <- sapply(filterdata, function(x) cor(x$nitrate,x$sulfate))
    }
    else{
    	cr <- as.numeric(c())
    }
    cr
}
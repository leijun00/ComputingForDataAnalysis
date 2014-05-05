complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    data <- lapply(id, function(x) getmonitor(x, directory))
    completedata <- lapply(data, function(x) na.omit(x))
    dims <- lapply(completedata , function(x) dim(x)[1])
    df <- data.frame(id=id, nobs=sapply(dims, function(x) x))
    print(df)
}
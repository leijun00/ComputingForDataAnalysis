getmonitor <- function(id, directory, summarize = FALSE) {
    ## 'id' is a vector of length 1 indicating the monitor ID
    ## number. The user can specify 'id' as either an integer, a
    ## character, or a numeric.
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'summarize' is a logical indicating whether a summary of
    ## the data should be printed to the console; the default is
    ## FALSE
    
    ## Your code here
    if(is.character(id)){
        name <- paste(id, ".csv", sep="")
    }
    if(is.numeric(id)){
    name <- sprintf("%3.3d",id)
    name <- paste(name, ".csv", sep="")
    }
    filename <- paste(directory, name, sep="/")
    data <- read.csv(filename)
    if(summarize){
    print(summary(data))
    }
    data
}
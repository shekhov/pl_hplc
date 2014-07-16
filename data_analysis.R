# File for data proccessing 

apply_statistic <- function (measurments, expand_raws=FALSE, length_to_expand=0){
    # Returns new matrix with first column as mean and second column as standard deviation
    # If we will need to expand the vector (so it will fit the chromatogram dimensions) the expand_raws 
    # should be set on TRUE and the total number of rows should be determine
    average <- apply (X=measurments, MARGIN=1, FUN=function(x) { mean (x, na.rm=TRUE)})
    std_dev <- apply (X=measurments, MARGIN=1, FUN=function(x) { sd (x, na.rm=TRUE)})
    
    error <- qnorm (0.975) * std_dev/sqrt(length(std_dev)) # Normal Distribution
    
    new_data <- cbind (average, std_dev, error)
    ncol <- dim (new_data)[2]
    
    if (expand_raws) {
        expand_matrix <- matrix (data=0, nrow=length_to_expand - dim(new_data)[1], ncol=ncol)
        new_data <- rbind (new_data, expand_matrix)
    }
    return (new_data)
}

summary_measurments <- function (meas_names = '') {
    #' Analyze data and return all measurments, which are more then Mean
    
    output <- list ()
    for (id in 1:length (meas_names)) {
        name <- meas_names[id]
        meas <- get (name, pos=-1)[,1]
        mean_meas <- mean (meas) 
        ID <- which (meas>=mean_meas, arr.ind=TRUE)
        value <- meas[meas>=mean_meas]
        answer <- cbind (ID, value)
        output[[name]] <- answer
    }

    return (output)
}
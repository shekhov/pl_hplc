# Script contains all plotting functions in the project. 

draw_related_standards <- function (combined_data, names='', xlab=x_label, ylab=y_label) {
    # For all data.frames in the combined data we will plot a line. 
    # combined data is a list with elements we want to draw. Each 
    # element has two rows (one for time, another for measurment)
    
    # Getting percentage and real meas data
    mx <- combined_data[[1]]
    real_y <- combined_data[[2]] * (combined_data[[2]] > 0) # Only positive numbers
    my <- real_y / max (real_y)
    
    ## Settings for left axis
    # Look at the range of the y numbers
    ry <- range(real_y, na.rm=TRUE)
    lyspan <- diff(ry)
    rylim <- range (my, na.rm=TRUE)
    ryspan <- diff (rylim)
    ymult <- diff(ry) / diff(rylim)
    axat <- pretty(rylim) 
    y_labels <- round (axat * ymult, digits=-1)
    
    plot (mx, my, col=my_colors[1], type='l', axes=FALSE, xlab=xlab, ylab=ylab)
    axis (side=1)
    axis (side=2, at=axat, labels=y_labels, las=1)
    
    len <- length(combined_data)
    color_id <- 2
    for (i in seq (3, len, 2)){
        x <- combined_data[[i]]
        y <- combined_data[[i+1]] * (combined_data[[i+1]] > 0)
        y <- y / max (y)
        lines (x, y, col=my_colors[color_id])
        color_id <- color_id + 1
    }
    
    legend ("topright", title="Standards", 
            legend=names, cex=1,
            fill=my_colors[1:(len+1)])
}

draw_chromatogram_with_ACN_gradient <- function (hplcX, hplcY, gradX, gradY, 
                                                 title='', xlab='', ylab='',
                                                 ...) {
    # If specify, then we will draw another line on the plot and add
    # scale on the right side. 
    # This method is actually kind of twoord.plot, but for my needs
    
    ## Settings for left axis
    # Look at the range of the y numbers
    lylim <- range(hplcY, na.rm=TRUE)
    lyspan <- diff(lylim)
    if(lyspan == 0) lyspan <- lylim[1]    
    # Increase the range a little bit to make sure its fit
    lylim[2]<- lylim[2] + lyspan*0.04
    if(lylim[1] != 0) lylim[1] <- lylim[1]-lyspan*0.04
    # Make labels them look pretty :)
    lytickpos <- pretty(hplcY)
    
    ## Settings for right axis
    rylim <- range (gradY, na.rm=TRUE)
    ryspan <- diff (rylim)
    ymult <- diff(lylim) / diff(rylim)
    # offset for the "right" y values
    yoff <- lylim[1] - rylim[1] * ymult
    #make labels looked good
    rylabels <- pretty(rylim)    
    axat <- rylabels * ymult + yoff
    # Prepare ry to be resized
    ry <- gradY * ymult + yoff
    # Place of the label
    rylab.at <- ymult + yoff
    
    
    # First, draw the hplc itself
    plot (hplcX, hplcY, type='l', main=title, xlab=xlab, ylab=ylab, 
          col='red', ylim=lylim, axes=FALSE, ...)
    axis (side=2, at=lytickpos, cex.axis=1, las=1)
    if (!is.na(xlab)) axis (side=1)
    
    # Then add gradiend 
    lines (gradX, ry, type='l', col='grey')
    axis (side=4, at=axat, labels=rylabels, cex.axis=1,  line=-1.5, las=1)
    mtext ("Acetonitrile, %", side=4, line=0.5, cex=0.8)
}

draw_chromatogram <- function (x, y, title='', xlab='', ylab='', color='black', ...){
    # Draw a chromatogram based on dataFrame (time, signal)
    # Draw only positive signals
    plot (x, y,
          type='l',
          main = title,
          col=color,        
          #axes=FALSE,
          ylab=ylab,
          xlab=xlab, 
          ...)  
}

create_gap <- function (dm) {
    # Search for the biggest differences between numbers and suggest them
    # to be cutted. If the maximum in the data set is not big enough it returns
    # False
    result <- NA
    this_mean <- mean (dm)
    this_max <- max (dm)
    this_max_id <- which.max (dm)
    maxes <- this_max
    new_vector <- dm[-this_max_id]
    gaps <- 0
    
    # Creating vectors with maximums and gaps values
    while (this_max > this_mean*5) {
        next_max <- max (new_vector)
        next_max_id <- which.max (new_vector)
        
        gaps <- append (gaps, this_max - next_max)
        maxes <- append (maxes, next_max)
        
        new_vector <- new_vector[-next_max_id]
        
        this_max <- next_max
    }
    
    max_gap <- max(gaps)
    
    too_low <- max(dm) < minimum_gap_start
    too_small <- max_gap < max(dm)*0.1
    near_minimum <- max(dm) < minimum_gap_start + minimum_gap_start*0.5
    
    if (too_low | too_small | near_minimum) return (FALSE) # We don't need gaps with bars lower then 200
    
    max_id <- which.max (gaps)
    
    # Check the first gap number. Should be more 200 (depends on variable), wo we will have mostly
    # the same scale for bottom
    if (maxes[max_id]>minimum_gap_start) {
        min_gap <- maxes[max_id]
    }
    else {
        min_gap <- minimum_gap_start
    }
    c (min_gap+10, maxes[max_id-1]-40)
}

create_y_tics <- function (maxN, gaps) {
    # Select which tics is better to draw
    yt <- c(0, 0)
    if (maxN - gaps[2] > 100) {
        yt[2] <- round (gaps[2]+30, -1)
    }
    
    if (gaps[1] - 200 > 100) yt[1] <- round (gaps[1]-50, -1)
    return (c(yt, round(maxN, -1)))
}

draw_bars <- function (dataMatrix, with_errors=FALSE, width_error_bars=0.5,
                       title='', xlabel='', ylabel='', color='green') {
    
    colors <- rep (color, length (dataMatrix[,1]))
    
    maxM <- max (dataMatrix[,1]) #+ max (dataMatrix[,3])
    gaps <- create_gap (dataMatrix[,1])
    
    if (gaps[1]){  
        yt <- create_y_tics(maxM, gaps)
        gap.barplot (y=dataMatrix[,1], gap=gaps,
                     col=colors, xaxt='n',
                     ytics=c(0, 100, 200, yt), las=1, ylab=ylabel)
        if (with_errors) {
            dispersion (x=1:this_method_time, y=dataMatrix[,1], 
                        ulim=dataMatrix[,1] + dataMatrix[,3])
        }
    }
    else {
        ylim <- c(0, maxM)
        if (ylim[2] < 200) ylim[2] = 200
        barplot (dataMatrix[,1], 
                 col=colors, xaxt='n', ylab=ylabel, #arrow.cap=width_error_bars,
                 ylim=ylim, las=1)   
    }
}

combine_data_and_plot <- function (sampleData, standardDataName, 
                                   dataMatrix, title = "", ...) {
    # Will first plot the data into one image
    # Args:
    #   - sampleData: The data frame with chromatogram data (time, value)
    #   - standardData: The data frame with standard chromatogram for the sample
    #   - dataMatrix: The n:2 matrix, where first column is a means of measurmens and 
    #       second column is standard deviation of this measurmens
    #   - title, the characters will be on the top of the plot
    layout(matrix(1:3, ncol=1), heights=c(5,2,3))
    
    # The main chromatogram
    par (mar=c(0, 4, 2, 2))
    if (use_gradient) {
        draw_chromatogram_with_ACN_gradient(sampleData[[1]], 
                                            sampleData[[2]] * (sampleData[[2]]>0),
                                            hplc_method[[1]], hplc_method[[2]],
                                            title=title, ylab=y_label, xlab=NA)
    }
    else {
        draw_chromatogram (sampleData[[1]], sampleData[[2]] * (sampleData[[2]]>0), 
                           title=title,
                           axes=FALSE,
                           xlab=x_label, 
                           ylab=y_label, 
                           color='red')
        axis (side=2)
    }
    
    # The measurments bars
    par (mar=c(0, 4, 0, 2))
    #plot (dpm_data[[2]], type='h', col='green', axes=FALSE)
    draw_bars (dataMatrix, 
               ylabel='Activity, DPM', 
               with_errors = with_error_bars,  # Some errors occures while calculating std dev
               width_error_bars=0.2, # Still does not working 
               color='green')
    #axis (side=2)
    
    # The chromatograms to compare with.
    par (mar=c(5, 4, 0.5, 2))
    standardData <- get (standardDataName, pos=1)
    if (use_related_std) {
        # What names will be used for this set
        std_name <- standardDataName#deparse(substitute(standardData, env=.GlobalEnv))
        names <- c (std_name, related_std[[std_name]])
        #print (std_name)
        # Creating dataset to draw
        data_pass <- c (standardData)
        for (i in 1:length (related_std[[std_name]])) {
            this_name <- related_std[[std_name]][[i]]
            #print (this_name)
            data_pass <- c(data_pass, get (this_name, pos=1))
        }
        
        # Cut of std_ from names
        names <- sapply (names, FUN=function (x) {strsplit(x, split='_')[[1]][-1]}, 
                         simplify=TRUE)
        
        
        
        draw_related_standards (data_pass, names)
    }
    else {
        draw_chromatogram (standardData[[1]], standardData[[2]] * (standardData[[2]]>0), 
                           #xlabel='Time, min',
                           ylab=y_label, 
                           color='blue',
                           axes=FALSE,
                           xlab='Time, min')
        axis (side=2, las=1)
        axis (side=1)
    }
    
    # Reset layout options to default
    layout (matrix (1:1, ncol=1))
    par (mar=c(5, 4, 3, 2)) 
}

turn_on_device <- function (title, fileType) {
    path_to_file <- paste (path_to_save, title, '.', fileType, sep='')
    if (fileType == 'png') {
        png (path_to_file, width=7, height=7, units='in', res=400)   
        return (TRUE)
    }
    else if (fileType == 'pdf') {
        pdf (path_to_file)
        return (TRUE)
    }
    else return (FALSE)
}

proceed_single_plot <- function (title, sampleData, fileType='screen'){
    
    path_to_file <- paste (path_to_save, title, '.', fileType, sep='')
    device_on <- turn_on_device (title, fileType)
    
    if (use_gradient) {
        draw_chromatogram_with_ACN_gradient(sampleData[[1]], 
                                            sampleData[[2]] * (sampleData[[2]]>0),
                                            hplc_method[[1]], hplc_method[[2]],
                                            title=title, ylab=y_label, xlab=x_label)
        
    }
    else {
        draw_chromatogram (sampleData[[1]], sampleData[[2]] * (sampleData[[2]]>0),
                           title=title, 
                           ylab=y_label, xlab=x_label, col='blue') 
    }
    
    if (device_on) dev.off()
}

# for each set of data will create two files, pdf and png
proceed_plots <- function (title, sampleData, standardDataName, dataMatrix, fileType='screen'){
    # Create file with a name and extension was given. Draw into this file graphs
    # using function combine_data_and_plot
    device_on <- turn_on_device (title, fileType)  
    #print (deparse(substitute(standardData)))
    combine_data_and_plot (sampleData, standardDataName, dataMatrix, title)   
    if (device_on) dev.off()
}

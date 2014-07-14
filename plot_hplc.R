# This script is writing to help procceed data from isotop lab (HPLC-UV and 
# scintifilation counter measurements. NEvertheless, it can be used for
# combining UV spectrums with some measurments. 
# The first goal is to plot combined graph with 3 parts on it - chromatogram
# of sample, measurments and chromatogram of standard. 
# The next goal is to make simple to draw and combine them manually

# To start work first set working directory path_to_data, where all files
# you want to analyze are located. Note, that names of the files should be 
# looked like:
# experimentName_typeOfData_sampleType_sampleIDorNAME.extension
# where typeOfData can be std, hplc, or variable measurment (can be set any)
# extension is only csv right now
# EXAMPLE: sh25_DPM_sinalbin_231.csv

# TODO: DONE! to use plotrix package. Install if has not yet and load if did not used
# TODO: DONE! Figure out how to combine standards and lines
# TODO: Separate modules for diferent files. Draw, data analyzis and settings at least should be
# separated


# Set path to data. Replace all "\" to "/" before running!
path_to_data <- "Z://LAB DATA/GLS/SH29. CF7/raw_data/"
path_to_save <- "Z://LAB DATA/GLS/SH29. CF7/plots/"
experiment <- 'sh29'
measurment_name <- 'DPM'
this_method_time <- 69 # Set this to adjust hplc and measurments on graph
output_format <- 'screen'
minimum_gap_start <- 200 # This value means from what height gap should appear on the bars
my_colors <- c('red', 'green', 'blue', 'grey', 'black')

# This method for my samples. Replace by calling set_hplc_method function
method_dvlong8 <- data.frame (time=c(0, 9, 17, 40, 54, 58, 58.10, 61.9, 62, 69), 
                       acn=c(0.5, 0.5, 6, 10, 50, 70, 100, 100, 0.5, 0.5))
hplc_method <- method_dvlong8
use_gradient = TRUE # Set it to false and usual plots will be drown instead

# Will use this variables, And if you want to change all graphs, just change properties here
y_label <- 'A, 229 nm' 
x_label <- 'time, min'

# If we have standards that should be plot together, then we will use this list
use_related_std = FALSE
related_std <- list()
related_std$std_4msob <- c('std_4mtb')
related_std$std_sinalbin <- c('std_P-OH-Benzyle-cyanide')
related_std$std_sinigrin <- c('std_allyl-ITC', 'std_allyl-cyanide')

# Flag for plotting error on the bars
with_error_bars = FALSE

# Load and install packages
# Plotrix for graphs
plot_package <- find.package ("plotrix", quiet = TRUE)
if (length(plot_package) == 0) install.packages ("plotrix")
require ('plotrix')

analyze_working_folder <- function () {
    # Create all logic vectors for function load_data
    file_list <<- list.files (path=path_to_data)
    file_table <<- strsplit (file_list, split='[_.]')
    
    is_experiment <<- sapply (X=file_table, FUN=function(x) {x[1]==experiment})
    is_std <<- sapply (X=file_table, FUN=function(x) {x[2]=='std'})
    is_hplc <<- sapply (X=file_table, FUN=function(x) {x[2]=='hplc'})
    is_meas <<- sapply (X=file_table, FUN=function(x) {x[2]== measurment_name})
    is_csv <<- sapply (X=file_table, FUN=function(x) {x[length(x)]=='csv'})
    
    if (!exists ('standard_names', where=1)) standard_names <<- vector()
    if (!exists ('hplc_names', where=1)) hplc_names <<- vector()
    if (!exists ('hplc_std', where=1)) hplc_std <<- vector()
    if (!exists ('meas_names', where=1)) meas_names <<- vector()  
    if (!exists ('single_plots', where=1)) single_plots <<- vector() 
}

start <- function () {
    # This function will start to work interactively with user
    # NOT COMPLETED
    print ('Welcome! We will do all by ourself')
    analyze_working_folder()
    load_data()
    draw_data ()
}

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
  
  # Check the first gap number. Should be more 200, wo we will have mostly
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


# Load all data from folder to environment
load_data <- function (to_workspace=TRUE) {
  # Scan folder for the file, and extract data to the workplace if they 
  # coresponds to the filters.
  
  standard_names <<- vector()
  hplc_names <<- vector()
  hplc_std <<- vector()
  meas_names <<- vector()
  single_plots <<- vector()
  
  
  for (i in 1:length(file_table)){
    if (is_csv[i]) {
      len_name <- length (file_table[[i]])
      sep_this <- '' # for chromatogram is ',', for measurments is ';'
      
      if (is_experiment[i]){
        this_name <- paste (file_table[[i]][2:(len_name-1)], collapse='_')
        
        if (is_std[i]) {
          standard_names <<- append (standard_names, file_table[[i]][3])
          this_name <- paste ('std', file_table[[i]][3], sep='_')      
          sep_this <- ','      
        }
        
        else if (is_hplc[i]) {
          hplc_names <<- append (hplc_names, this_name)
          hplc_std <<- append (hplc_std, paste ('std', file_table[[i]][4], sep='_'))
          sep_this <- ','
        }
        
        else if (is_meas[i]) {
          meas_names <<- append (meas_names, this_name)
          sep_this <- ';'
        }
        
            
        else next # Skip this undefiend sample
      }
      
      
      else {
        this_name <- paste (file_table[[i]][1:(len_name-1)], collapse='_')
        sep_this <- ','
        single_plots <<- append (single_plots, this_name)
      }
          
      if (to_workspace) {
        # Load csv file into variable
        assign (this_name, pos=1, 
                value=read.csv (file=paste (path_to_data, '/', file_list[[i]][1], sep=''),
                                             header=FALSE, sep=sep_this, dec='.'))
        
        # For measurments merge data to mean and std columns
        if (is_meas[i]){
          assign (this_name, pos=1,
                  apply_statistic (get(this_name, pos=1), expand_raws=TRUE, 
                                             length_to_expand=this_method_time))
        }
      }
    }
  }
  to_find <<- gsub ("hplc", 'DPM', hplc_names)
  hplc_and_meas <<- match (to_find, meas_names)
}

draw_data <- function (to_format=output_format){  
  # Call this function if you sure that load_data worked ok and load all 
  # variables and tables from the folder
  # Otherwise, you have to make sure that all data filled correctly
    if (length(hplc_and_meas > 0)) {
      for (i in 1:length(hplc_and_meas)){
        title <- paste (strsplit(hplc_names[i], split='_')[[1]][-1], collapse=' ')
        
        if (!is.na(hplc_and_meas[i])) {
           # if (!is.na (std_together[hplc_std[i]])
          proceed_plots (title=title, 
                        get(hplc_names[i], pos=1), # Sample
                        #get(hplc_std[i], pos=1), # Standard
                        hplc_std[i],
                        get(meas_names[hplc_and_meas[i]], pos=1), # Measurments
                        fileType=to_format)
        }
        else {
          # It is single plot, has experiment ID, but no other details were specified.
          proceed_single_plot (title=title, get(hplc_names[i], pos=1), fileType=to_format)
        }  
        
        }
    }
    if (length (single_plots) > 0) {
      for (i in 1:length(single_plots)){
        title <- paste (strsplit(single_plots[i], split='_')[[1]], collapse=' ')
        proceed_single_plot (title=title, get(single_plots[i], pos=1), fileType=to_format)
      }
    }
}

set_data_path <- function (path) {
    # Replace the default path.
    # Note that path should contane '/' not '\'
    path_to_data <<- path
}

set_save_path <- function (path) {
    path_to_save <<- path
}

set_experiment_name <- function (new_name) {
  # Replace default experiment name.
  # Script will extract only those files, whos names starts with it
  experiment <<- new_name
}

set_measurment_name <- function (new_name) {
  # Replace default measurment name.
  # All files with measurment should have it on the second place
  meas_names <<- new_name
}

set_output_format <- function (new_format) {
  # Set the way data will be saved
  # 'pdf', 'png', 'screen'
  output_format <- new_format
}

set_hplc_method <- function (time, gradient) {
    # If the hplc method is set, then all chromatogramm will be drawn with
    # gradient
    hplc_method <- data.frame (time, gradient)
}

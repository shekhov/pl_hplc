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

# TODO: to use plotrix package. Install if has not yet and load if did not used
# TODO: Figure out how to combine standards and lines


# Set path to data. Replace all "\" to "/" before running!
path_to_data <- "//Groups/mol-grp/Anton/Data/LC-UV from isotop"
path_to_save <- "Z://LAB DATA/GLS/SH25. Beetles/plots/"
experiment <- 'sh25'
measurment_name <- 'DPM'
output_format <- 'screen'

# Load and install packages
# Plotrix for graphs
plot_package <- find.package ("plotrix", quiet = TRUE)
if (nchar(plot_package) == 0) install.packages ("plotrix")
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
    load_data(FALSE)
    draw_data (output_format)
}


draw_chromatogram <- function (x, y, title='', xlabel='', ylabel='', color='black', ...){
  # Draw a chromatogram based on dataFrame (time, signal)
  # Draw only positive signals
  plot (x, y,
        type='l',
        main = title,
        col=color,
        #axes=FALSE,
        ylab=ylabel,
        xlab=xlabel, ...)  
}

create_gap <- function (dm) {
  # Search for the biggest differences between numbers and suggest them
  # to be cutted.
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
  if (max(dm) < 200) return (FALSE) # We don't need gaps with bars lower then 200
  
  max_id <- which.max (gaps)
  
  # Check the first gap number. Should be more 200, wo we will have mostly
  # the same scale for bottom
  if (maxes[max_id]>200) {
    min_gap <- maxes[max_id]
  }
  else {
    min_gap <-   200
  }
  #print (maxes)
  #print (gaps)
  c (min_gap+10, maxes[max_id-1]-40)
}

create_y_tics <- function (maxN, gaps) {
  # Select which tics is better to draw
  yt <- c(0, 0)
  if (maxN - gaps[2] > 100) {
    yt[2] <- round (gaps[2]+10, -1)
  }
  
  if (gaps[1] - 200 > 100) yt[1] <- round (gaps[1]-50, -1)
  return (c(yt, round(maxN, -1)))
}

draw_bars <- function (dataMatrix, with_errors=FALSE, width_error_bars=1.0,
                       title='', xlabel='', ylabel='', color='green') {
  
  colors <- rep (color, length (dataMatrix[,1]))
  
  maxM <- max (dataMatrix[,1])
  gaps <- create_gap (dataMatrix[,1])
 
    if (gaps[1]){  
        yt <- create_y_tics(maxM, gaps)
        gap.barplot (y=dataMatrix[,1], gap=gaps,
                     col=colors, xaxt='n',
                     ytics=c(0, 100, 200, yt), las=1)
    }
    else {
        barplot (dataMatrix[,1], 
                     col=colors, xaxt='n',
                     ylim=c(0,200), las=1)   
    }
}

combine_data_and_plot <- function (sampleData, standardData, 
                                   dataMatrix, title = "", ...) {
  # Will first plot the data into one image
  # Args:
  #   - sampleData: The data frame with chromatogram data (time, value)
  #   - standardData: The data frame with standard chromatogram for the sample
  #   - dataMatrix: The n:2 matrix, where first column is a means of measurmens and 
  #       second column is standard deviation of this measurmens
  #   - title, the characters will be on the top of the plot
  layout(matrix(1:3, ncol=1), heights=c(5,2,2))
  
  # The main chromatogram
  par (mar=c(0, 4, 2, 0))
  draw_chromatogram (sampleData[[1]], sampleData[[2]] * (sampleData[[2]]>0), 
                     title=title,
                     axes=FALSE,
                     xlabel='Time, min', 
                     ylabel='A, 229nm', 
                     color='red')
  axis (side=2)
  
  # The measurments bars
  par (mar=c(0, 4, 0, 0))
  #plot (dpm_data[[2]], type='h', col='green', axes=FALSE)
  draw_bars (dataMatrix, 
             ylabel='Activity, DPM', 
             with_errors = FALSE,  # Some errors occures while calculating std dev
             width_error_bars=0.2, # Still does not working 
             color='green')
  #axis (side=2)

  # The chromatograms to compare with.
  par (mar=c(5, 4, 0.5, 0))
  draw_chromatogram (standardData[[1]], standardData[[2]] * (standardData[[2]]>0), 
                     #xlabel='Time, min',
                     ylabel='A, 229nm', 
                     color='blue',
                     axes=FALSE,
                     xlab='Time, min')
  axis (side=2)
  axis (side=1)
  
  # Reset layout options to default
  layout (matrix (1:1, ncol=1))
  par (mar=c(5, 4, 3, 1)) 
}

proceed_single_plot <- function (title, sampleData, fileType='screen'){
    
    path_to_file <- paste (path_to_save, title, '.', fileType, sep='')
    
    if (fileType == 'png') {
        png (file=paste (title, '.png', sep=''), width=6, height=4, units='in', res=400)   
        draw_chromatogram (sampleData[[1]], sampleData[[2]] * (sampleData[[2]]>0),
                           title=title, 
                           xlabel='time, min', ylabel='A, 229nm', col='blue') 
        dev.off()
        
      }
    else if (fileType == 'pdf') {
        pdf (file=paste (title, '.pdf', sep=''))
        draw_chromatogram (sampleData[[1]], sampleData[[2]] * (sampleData[[2]]>0),
                           title=title, 
                           xlabel='time, min', ylabel='A, 229nm', col='blue') 
        dev.off()
    
    }
    
    else {
        draw_chromatogram (sampleData[[1]], sampleData[[2]] * (sampleData[[2]]>0),
                           title=title, 
                           xlabel='time, min', ylabel='A, 229nm', col='blue')     
    }
}

# for each set of data will create two files, pdf and png
proceed_plots <- function (title, sampleData, standardData, dataMatrix, fileType='screen'){
  # Create file with a name and extension was given. Draw into this file graphs
  # using function combine_data_and_plot
    
  path_to_file <- paste (path_to_save, title, '.', fileType, sep='')
  
  if (fileType == 'png') {
    png (file=path_to_file, width=6, height=4, units='in', res=400)
    combine_data_and_plot (sampleData, standardData, dataMatrix, title)   
    dev.off()
  }
  else if (fileType == 'pdf') {
    pdf (file=path_to_file)
    combine_data_and_plot (sampleData, standardData, dataMatrix, title)   
    dev.off()
  }
  else {
      #print ("Combine to", standardData)
    combine_data_and_plot (sampleData, standardData, dataMatrix, title)   
  }
}

get_mean_and_sd_as_matrix <- function (measurments, expand_raws=FALSE, length_to_expand=0){
  # Returns new matrix with first column as mean and second column as standard deviation
  # If we will need to expand the vector (so it will fit the chromatogram dimensions) the expand_raws 
  # should be set on TRUE and the total number of rows should be determine
  average <- apply (X=measurments, MARGIN=1, FUN=function(x) { mean (x, na.rm=TRUE)})
  std_dev <- apply (X=measurments, MARGIN=1, FUN=function(x) { sd (x, na.rm=TRUE)})
  new_data <- cbind (average, std_dev)
  if (expand_raws) {
    expand_matrix <- matrix (data=0, nrow=length_to_expand - dim(new_data)[1], ncol=2)
    new_data <- rbind (new_data, expand_matrix)
  }
  return (new_data)
}


# Load all data to environment
load_data <- function (is_done = TRUE) {
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
          
      if (!is_done) {
        # Load csv file into variable
        assign (this_name, pos=1, 
                value=read.csv (file=paste (path_to_data, '/', file_list[[i]][1], sep=''),
                                             header=FALSE, sep=sep_this, dec='.'))
        
        # For HPLC also create percentage for each measurment (UV)
        # so it can be drawn together. We treat all samples (with csv) that not measurments
        # like hplc (because is_hplc works only for those with names hplc)
        if (!is_meas[i] & is_csv[i]){
            temp <- get (this_name, pos=1)
            temp$perc <- temp[,2] / max(temp[,2])
            assign (this_name, pos=1, temp)        
        }
        
        # For measurments merge data to mean and std columns
        if (is_meas[i]){
          assign (this_name, pos=1,
                  get_mean_and_sd_as_matrix (get(this_name), expand_raws=TRUE, length_to_expand=69))
        }
      }
    }
  }
  to_find <<- gsub ("hplc", 'DPM', hplc_names)
  hplc_and_meas <<- match (to_find, meas_names)
}

#load_data (TRUE)
draw_data <- function (to_format){  
  # Call this function if you sure that load_data worked ok and load all 
  # variables and tables from the folder
  # Otherwise, you have to make sure that all data filled correctly
  for (i in 1:length(hplc_and_meas)){
    title <- paste (strsplit(hplc_names[i], split='_')[[1]][-1], collapse=' ')
    #print (title)
    if (!is.na(hplc_and_meas[i])) {
      #print (paste ("Draw standard ", hplc_std[i], sep=' '))
      proceed_plots(title=title, 
                    get(hplc_names[i], pos=1), get(hplc_std[i], pos=1),
                   get(meas_names[hplc_and_meas[i]], pos=1), fileType=to_format)
    }
    else {
      #print (paste ("Draw single ", hplc_names[i], sep=' '))
      # It is single plot
      proceed_single_plot (title=title, get(hplc_names[i], pos=1), fileType=to_format)
    }  
   
  }
  for (i in 1:length(single_plots)){
    title <- paste (strsplit(single_plots[i], split='_')[[1]], collapse=' ')
    proceed_single_plot (title=title, get(single_plots[i], pos=1), fileType=to_format)
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



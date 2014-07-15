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

# Combine the project files together
source('~/pl_hplc/settings.R')
source('~/pl_hplc/tui.R')
source('~/pl_hplc/plot_functions.R')

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


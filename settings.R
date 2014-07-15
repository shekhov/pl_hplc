# There are all settings that are used in main script and function to control them.

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

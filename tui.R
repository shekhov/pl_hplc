# This file is text user interface that allow user by following instructions
# analyze data, create and combine graphs and so on.
# This file works only with plot_hplc.R script, because tui uses its variables
source ('~/pl_hplc/messages.R')
source ('~/pl_hplc/help_msg.R')

init <- function () {
    cat (welcomeMSG)
}

start <- function () {
    # This function will start to work interactively with user
    # NOT COMPLETED
    print ('Welcome! We will do all by ourself')
    analyze_working_folder()
    load_data()
    draw_data ()
}


# Function to print descriptions about all functions in the script
help_me <- function (f_name = '') {
    if (f_name == 'help_me') cat (helpMSG)
    else if (f_name == '') cat (aboutMSG)
}
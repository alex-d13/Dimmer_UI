
# -------- DEPENDENCIES -------- 

source('dependencies.R')
source('run_dimmer.R')


# -------- VARIABLES -------- 

# path do dimmer jar
dimmer_path <- '/bin/dimmer.jar'

#directory, where annotation file and methylation files will be stored
global_dir <- tempdir(check = TRUE)

annotation_checked <- FALSE
met_files_processed <- FALSE

overlay_color <- "rgb(51, 62, 72, .5)"

# -------- TEXT -------- 

source('texts.R')


# ------ MODALS ---------

errorModal <- function(error_message=NULL){
  modalDialog(
    title = 'Something went wrong.',
    p(error_message,style="color:red;"),
    easyClose = T,
    modalButton("Cancel")
  )
}

infoModal <- function(info_message=NULL){
  modalDialog(
    title = 'Success!',
    p(info_message,style="color:green;"),
    easyClose = T,
    modalButton("Close")
  )
}

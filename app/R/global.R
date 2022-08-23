
# -------- DEPENDENCIES -------- 

source('dependencies.R')
source('run_dimmer.R')


# -------- VARIABLES -------- 

# path do dimmer jar
dimmer_path <- 'src/dimmer.jar'

annotation_checked <- FALSE

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
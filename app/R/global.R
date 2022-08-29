
# -------- DEPENDENCIES -------- 

source('dependencies.R')
source('run_dimmer.R')


# -------- VARIABLES -------- 

# path do dimmer jar
dimmer_path <- '/bin/dimmer.jar'
#dimmer_path <- '~/NetfLID/Dimmer_UI/src/dimmer.jar'

#directory, where annotation file and methylation files will be stored
global_dir <- tempdir(check = TRUE)
output_dir <- paste0(tempdir(check = TRUE),'/output')

overlay_color <- "rgb(51, 62, 72, .5)"

# -------- TEXT -------- 

source('texts.R')


jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"


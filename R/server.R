server <- function(input, output, session) {
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  
  shinyFiles::shinyDirChoose(input = input, 
                             id = "dimmer_output_path",
                             roots = volumes, 
                             session = session, 
                             restrictions = system.file(package = "base"), 
                             allowDirCreate = FALSE)
  
  # print selected output directory
  output$dimmer_output_dir_text <- renderPrint({
    if (is.integer(input$dimmer_output_path)) {
      cat("No output directory has been selected.")
    } else {
      parseDirPath(volumes, input$dimmer_output_path)
    }
  })
  
  # read annotation file as soon as it is uploaded and update select input
  observe({
    annotation_file <- input$dimmer_annotation_path
    if(is.null(annotation_file)){return(NULL)}
    
    annotation_data <- read.csv(annotation_file$datapath)
    updateSelectInput(session, 'dimmer_variable', choices = colnames(annotation_data))
  })
  
  
  # write config file to disc
  output$dimmer_create_config <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), '_dimmer_config.txt')
    },
    content = function(file){
      
      if(input$dimmer_output_path[1] == 0){
        showModal(errorModal('You need to specify the output directory!'))
        return(NULL)
      }
      if(is.null(input$dimmer_annotation_path)){
        showModal(errorModal('You need to upload a sample annotation file!'))
        return(NULL)
      }
      
      config_text <- create_config(volumes, input)
      write(unlist(config_text), file = file, sep = '\t')
      showModal(infoModal('The config file has successfully been downloaded by your browser. Please check your downloads folder.'))
    }
  )
}
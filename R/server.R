server <- function(input, output, session) {
  
  # print selected output directory
  output$dimmer_output_dir_text <- renderPrint({
    if (!dir.exists(input$dimmer_output_path)) {
      cat("Selected output directory does not exist or cannot be found.")
    } else {
      cat(input$dimmer_output_path)
    }
  })
  
  annotation_data <- reactive({
    req(input$dimmer_annotation_path) # require that input is available, prevents error when no data uploaded
    inFile <- input$dimmer_annotation_path
    tryCatch({
      df <- read.csv(inFile$datapath, check.names = FALSE)
      if(is.null(colnames(df))){return(NULL)}
      #TODO: check annotation file 
      return(df)
    }, error = function(e){
      shinyCatch(stop("Error while reading annotation file."), blocking_level = "error", position = 'bottom-left',shiny = T)
      print(e$message)
      return(NULL)
    })
  })
  
  # read annotation file as soon as it is uploaded and update select input
  observe({
    if(!is.null(annotation_data())){
      updateSelectInput(session, 'dimmer_variable', choices = colnames(annotation_data()))
      updateSelectInput(session, 'dimmer_confounding_variables', choices = colnames(annotation_data()))
    }
  })

  observe({
    if(input$dimmer_cell_composition){
      shinyjs::show('dimmer_cell_types',anim = T)
    }else{
      shinyjs::hide('dimmer_cell_types',anim = T)
    }
})
  
  
  # write config file to disc
  output$dimmer_create_config <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), '_dimmer_config.txt')
    },
    content = function(file){
      
      if(is.null(input$dimmer_output_path)){
        shinyCatch(stop('You need to specify the output directory!'), blocking_level = "error", position = 'bottom-left',shiny = T)
        return(NULL)
      }
      if(is.null(input$dimmer_annotation_path)){
        shinyCatch(stop('You need to upload a sample annotation file!'), blocking_level = "error", position = 'bottom-left',shiny = T)
        return(NULL)
      }
      
      dimmer_variable_type <- check_variable(input$dimmer_variable, annotation_data())
      if(input$dimmer_variable == 'Regression' && !dimmer_variable_type$numeric){
        shinyCatch(stop('Selected variable needs to be numeric when using Regression model!'), blocking_level = "error", position = 'bottom-left',shiny = T)
        return(NULL)
      }
      if(input$dimmer_variable == 'Regression' && !dimmer_variable_type$binary){
        shinyCatch(stop('Selected variable needs to be binary when using T-test model!'), blocking_level = "error", position = 'bottom-left',shiny = T)
        return(NULL)
      }
      
      config_text <- create_config(input)
      write(unlist(config_text), file = file, sep = '\t')
      showModal(infoModal('The config file has successfully been downloaded by your browser. Please check your downloads folder.'))
    }
  )
}
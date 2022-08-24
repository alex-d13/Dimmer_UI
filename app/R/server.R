server <- function(input, output, session) {
  waiter_hide()
  # Upload max of 10Gb
  options(shiny.maxRequestSize = 10000 * 1024^2)
  
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
    #move file to shared directory
    inFileNew <- paste0(global_dir,'/',basename(inFile$datapath))
    file_move(path = inFile$datapath, new_path = inFileNew)
    tryCatch({
      df <- read.csv(inFileNew, check.names = FALSE)
      if(is.null(colnames(df))){return(NULL)}
      #TODO: check annotation file
      annotation_checked <- TRUE
      return(list(df=df, anno_file=inFileNew))
    }, error = function(e){
      shinyCatch(stop("Error while reading annotation file."), blocking_level = "error", position = 'bottom-left',shiny = T)
      print(e$message)
      return(NULL)
    })
  })
  
  methyl_data <- reactive({
    req(input$dimmer_upload_methylation)
    inFile <- input$dimmer_upload_methylation
    tryCatch({
      met_files <- decompress(inFile$datapath, global_dir)
      met_files_processed <- TRUE
      return(met_files)
    }, error = function(e){
      print(e$message)
    })
    return(NULL)
  })
  
  output$dimmer_upload_ready <- renderInfoBox({
    if(met_files_processed){
      infoBox("Files ready", icon = icon("thumbs-up", lib = "glyphicon"), color="green", width=4, fill = TRUE)
    }else{
      infoBox("Files not ready", icon = icon("thumbs-down", lib = "glyphicon"), color="red", width=4, fill=TRUE)
    }
  })
  
  # read annotation file as soon as it is uploaded and update select input
  observe({
    if(!is.null(annotation_data()$df)){
      updateSelectInput(session, 'dimmer_variable', choices = colnames(annotation_data()$df))
      updateSelectInput(session, 'dimmer_confounding_variables', choices = colnames(annotation_data()$df), selected = c(''))
      shinyjs::enable('dimmer_upload_methylation')
    }
  })

  observe({
    if(input$dimmer_cell_composition){
      shinyjs::show('dimmer_cell_types',anim = T)
    }else{
      shinyjs::hide('dimmer_cell_types',anim = T)
    }
})
  
  # write config to temporary file and start DiMmer
  observeEvent(input$dimmer_start, {
    waiter::waiter_show(html = tagList(spin_rotating_plane(),"Running DiMmer ..." ),color=overlay_color)
    
    if(is.null(annotation_data()$df)){
      waiter::waiter_hide()
      shinyCatch(stop("No annotation file uploaded."), blocking_level = "error", position = 'bottom-left',shiny = T)
      return(NULL)
    }
    if(is.null(methyl_data())){
      waiter::waiter_hide()
      shinyCatch(stop("No methylation data uploaded."), blocking_level = "error", position = 'bottom-left',shiny = T)
      return(NULL)
    }

    # create config file in temporary location
    config_text <- create_config(input, annotation_data()$df)
    temp_config <- tempfile(fileext = '.config')
    
    # create temporary output directory
    temp_outdir <- paste0(tempdir(check = TRUE),'/output')
    dir_create(temp_outdir)
    config_text$output_path <- sprintf('output_path: %s', temp_outdir)
    
    # write location of annotation file into config
    config_text$annotation_path <- sprintf('annotation_path: %s', annotation_data()$anno_file)

    if(!is.null(config_text)){
      write(unlist(config_text), file = temp_config, sep = '\t')
      dimmer_cmd <- paste0('java -jar ',dimmer_path, ' ',temp_config)
      
      system(command = dimmer_cmd, 
              stdout = paste0(temp_outdir,'/logs_stdout.txt'), 
              stderr = paste0(temp_outdir,'/logs_stderr.txt'))
      waiter::waiter_hide()
    }else{
      waiter::waiter_hide()
      return(NULL)
    }
  })
  
  # write config file to disc
  output$dimmer_create_config <- downloadHandler(
    
    filename = function() {
      paste0(Sys.Date(), '_dimmer_config.txt')
    },
    content = function(file){
      config_text <- create_config(input, annotation_data()$df)
      if(!is.null(config_text)){
        write(unlist(config_text), file = file, sep = '\t')
        showModal(infoModal('The config file has successfully been downloaded by your browser. Please check your downloads folder.'))
      }else{
        return(NULL)
      }
    }
  )
}
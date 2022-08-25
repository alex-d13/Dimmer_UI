server <- function(input, output, session) {
  waiter_hide()
  # Upload max of 10Gb
  options(shiny.maxRequestSize = 10000 * 1024^2)
  session$onSessionEnded(stopApp)
  
  status_list <- reactiveValues(annotation_checked = FALSE,
                                methylation_checked = FALSE,
                                dimmer_finished = FALSE)
  
  # automatically read annotation file if something is uploaded
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
      status_list$annotation_checked <- TRUE
      return(list(df=df, anno_file=inFileNew))
      
    }, error = function(e){
      shinyCatch(stop("Error while reading annotation file."), blocking_level = "error", position = 'bottom-left',shiny = T)
      print(e$message)
      return(NULL)
    })
  })
  
  # methylation data needs to be checked manually by pressing button
  observeEvent(input$dimmer_methylation_check, {
    
    if(is.null(input$dimmer_upload_methylation$datapath)){
      shinyCatch(stop('Upload of methylation data incomplete.'), position = 'bottom-left', shiny = T, blocking_level = 'error')
    }
    inFile <- input$dimmer_upload_methylation
    
    tryCatch({
      met_files <- decompress(inFile$datapath, global_dir)
      status_list$methylation_checked <- TRUE
      shinyCatch(message('Files are extracted and ready!'), position = 'bottom-left', shiny = T, blocking_level = 'none')
      
    }, error = function(e){
      print(e$message)
    })
  })
  
  # read annotation file as soon as it is uploaded and update select input
  observe({
    if(!is.null(annotation_data()$df)){
      updateSelectInput(session, 'dimmer_variable', choices = colnames(annotation_data()$df))
      updateSelectInput(session, 'dimmer_confounding_variables', choices = colnames(annotation_data()$df), selected = c(''))
    }
  })

  observe({
    if(input$dimmer_cell_composition){
      shinyjs::show('dimmer_cell_types',anim = T)
    }else{
      shinyjs::hide('dimmer_cell_types',anim = T)
    }
    
    if(status_list$methylation_checked){
      shinyjs::hide('dimmer_methylation_check')
      shinyjs::hide('dimmer_methylation_text_ready')
      shinyjs::show('dimmer_methylation_text_notready')
    }else{
      shinyjs::show('dimmer_methylation_check')
      shinyjs::show('dimmer_methylation_text_ready')
      shinyjs::hide('dimmer_methylation_text_notready')
    }
  })
  
  output$dimmer_download <- renderUI({
    if(status_list$dimmer_finished){
      downloadBttn("dimmer_download_button",
                   label = "Download results",
                   icon = icon('download'),
                   color = "primary",
                   size = 'l',
                   block = F,
                   style = 'simple')
    }else{
      return(NULL)
    }
  })

  # write config to temporary file and start DiMmer
  observeEvent(input$dimmer_start, {
    waiter::waiter_show(html = tagList(spin_rotating_plane(),"Running DiMmer ..." ),color=overlay_color)
    status_list$dimmer_finished <- FALSE
    
    if(!status_list$annotation_checked){
      waiter::waiter_hide()
      shinyCatch(stop("No annotation file uploaded."), blocking_level = "error", position = 'bottom-left',shiny = T)
      return(NULL)
    }
    if(!status_list$methylation_checked){
      waiter::waiter_hide()
      shinyCatch(stop("Methylation data not checked or uploaded."), blocking_level = "error", position = 'bottom-left',shiny = T)
      return(NULL)
    }

    # create config file in temporary location
    config_text <- create_config(input, annotation_data()$df)
    temp_config <- tempfile(fileext = '.config')
    
    # create temporary output directory
    if(dir_exists(output_dir)){unlink(output_dir)}
    dir_create(output_dir)
    config_text$output_path <- sprintf('output_path: %s', output_dir)
    
    # write location of annotation file into config
    config_text$annotation_path <- sprintf('annotation_path: %s', annotation_data()$anno_file)
    
    # for now: disable plotting when starting dimmer from shiny
    shinyCatch(message("Plotting is currently disabled when starting DiMmer from this webapp. We are working on a solution."), blocking_level = "none", position = 'bottom-left',shiny = T)
    config_text$save_permu_plots <- sprintf('save_permu_plots: false')
    config_text$save_search_plots <- sprintf('save_search_plots: false')
    config_text$save_dmr_permu_plots <- sprintf('save_dmr_permu_plots: false')

    if(!is.null(config_text)){
      write(unlist(config_text), file = temp_config, sep = '\t')
      dimmer_cmd <- paste0('-jar ',dimmer_path, ' ',temp_config)
      
      shinyCatch(message("Starting DiMmer ..."), blocking_level = "none", position = 'bottom-left',shiny = T)
      system2(command = 'java', args = dimmer_cmd, 
              stdout = paste0(output_dir,'/logs_stdout.txt'), 
              stderr = paste0(output_dir,'/logs_stderr.txt'))
      waiter::waiter_hide()
      shinyalert("Success!", "DiMmer has finished with processing your files. The download button in the sidebar has now been activated to download your results.", 
                 type = "success", closeOnEsc = T,showCancelButton = T)
      status_list$dimmer_finished <- TRUE

    }else{
      waiter::waiter_hide()
      return(NULL)
    }
  })
  
  output$dimmer_download_button <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), '_dimmer_output.zip')
    }, content = function(file){
      if(!dir_exists(output_dir)){
        shinyCatch(stop("Output directory does not exist."), blocking_level = "error", position = 'bottom-left',shiny = T)
      }
      shinyCatch(message("Output directory is compressed and the automatic download will begin shortly."), blocking_level = "none", position = 'bottom-left',shiny = T)
      
      zip(file, files = dir(output_dir, full.names = T), flags = '-r')
    },
    contentType = 'application/zip'
  )
    

  
  # write config parameters to file
  output$dimmer_create_config <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), '_dimmer_config.txt')
    },
    content = function(file){
      
      if(!status_list$annotation_checked){
        waiter::waiter_hide()
        shinyCatch(stop("No annotation file uploaded."), blocking_level = "error", position = 'bottom-left',shiny = T)
        return(NULL)
      }
      
      config_text <- create_config(input, annotation_data()$df)
      if(!is.null(config_text)){
        write(unlist(config_text), file = file, sep = '\t')
        shinyalert("Success!", "The config file has successfully been downloaded by your browser. Please check your downloads folder.", 
                   type = "success", closeOnEsc = T,showCancelButton = T)
      }else{
        return(NULL)
      }
    }
  )
}
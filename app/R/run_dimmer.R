
# function to run Dimmer with a config file
# function to create config file from shiny inputs
create_config <- function(input, annotation_data){
  
  # if(is.null(input$dimmer_output_path)){
  #   shinyCatch(stop('You need to specify the output directory!'), blocking_level = "error", position = 'bottom-left',shiny = T)
  #   waiter::waiter_hide()
  #   return(NULL)
  # }
  if(is.null(input$dimmer_annotation_path)){
    waiter::waiter_hide()
    shinyCatch(stop('You need to upload a sample annotation file!'), blocking_level = "error", position = 'bottom-left',shiny = T)
    return(NULL)
  }
  
  dimmer_variable_type <- check_variable(input$dimmer_variable, annotation_data)
  if(input$dimmer_model == 'Regression' && !dimmer_variable_type$numeric){
    waiter::waiter_hide()
    shinyCatch(stop('Selected variable needs to be numeric when using Regression model!'), blocking_level = "error", position = 'bottom-left',shiny = T)
    return(NULL)
  }
  if(input$dimmer_model == 'T-test' && !dimmer_variable_type$binary){
    waiter::waiter_hide()
    shinyCatch(stop('Selected variable needs to be binary when using T-test model!'), blocking_level = "error", position = 'bottom-left',shiny = T)
    return(NULL)
  }
  
  shinyCatch(message("Creating config file ..."), blocking_level = "none", position = 'bottom-left',shiny = T)
  
  #config_file <- tempfile('dimmer_config_', fileext = '.txt')
  config_text <- list()
  
  config_text$output_path <- ifelse(is.null(input$dimmer_output_path), sprintf('output_path:'), sprintf('output_path: %s', input$dimmer_output_path))
  config_text$annotation_path <- ifelse(is.null(input$dimmer_annotation_path), sprintf('annotation_path:'), sprintf('annotation_path: %s', input$dimmer_annotation_path))  
  config_text$variable <- sprintf('variable: %s', input$dimmer_variable)
  config_text$dimmer_project_path <- ifelse(is.null(input$dimmer_project_path), sprintf('dimmer_project_path:'), sprintf('dimmer_project_path: %s', input$dimmer_project_path))
  config_text$threads <- sprintf('threads: %i', input$dimmer_threads)
  
  config_text$input_type <- sprintf('input_type: %s', input$dimmer_input_type)
  
  config_text$background_correction <- ifelse(input$dimmer_background_correction, print('background_correction: true'), print('background_correction: false'))
  config_text$probe_filtering <- ifelse(input$dimmer_probe_filtering, print('probe_filtering: true'), print('probe_filtering: false')) 
  config_text$cell_composition <- ifelse(input$dimmer_cell_composition, print('cell_composition: true'), print('cell_composition: false'))
  config_text$cell_composition_path <- ifelse(is.null(input$dimmer_cell_composition_path), sprintf('cell_composition_path:'), sprintf('cell_composition_path: %s', input$dimmer_cell_composition_path))
  config_text$cd8t <- ifelse('cd8t' %in% input$dimmer_cell_types, print('cd8t: true'), print('cd8t: false'))
  config_text$cd4t <- ifelse('cd4t' %in% input$dimmer_cell_types, print('cd4t: true'), print('cd4t: false'))
  config_text$nk <- ifelse('nk' %in% input$dimmer_cell_types, print('nk: true'), print('nk: false'))
  config_text$ncell <- ifelse('ncell' %in% input$dimmer_cell_types, print('ncell: true'), print('ncell: false'))
  config_text$mono <- ifelse('mono' %in% input$dimmer_cell_types, print('mono: true'), print('mono: false'))
  config_text$gran <- ifelse('gran' %in% input$dimmer_cell_types, print('gran: true'), print('gran: false'))
  
  config_text$beta_path <- ifelse(is.null(input$dimmer_beta_path), print('beta_path:'), sprintf('beta_path: %s', input$dimmer_beta_path))
  config_text$array_type <- ifelse(is.null(input$dimmer_array_type), print('array_type:'), sprintf('array_type: %s', input$dimmer_array_type))
  
  config_text$min_reads <- sprintf('min_reads: %i', input$dimmer_min_reads)
  config_text$n_min_read_exceptions <- sprintf('n_min_read_exceptions: %i', input$dimmer_n_min_read_exceptions)
  config_text$min_variance <- sprintf('min_variance: %f', input$dimmer_min_variance)
  
  config_text$data_type <- sprintf('data_type: %s', input$dimmer_data_type)
  config_text$n_permutations_cpg <- sprintf('n_permutations_cpg: %i', input$dimmer_n_permutations_cpg)
  config_text$model <- sprintf('model: %s', input$dimmer_model)
  config_text$alternative_hypothesis <- sprintf('alternative_hypothesis: %s', input$dimmer_alternative_hypothesis)
  config_text$assume_equal_variance <- ifelse(input$dimmer_assume_equal_variance, print('assume_equal_variance: true'), print('assume_equal_variance: false'))
  config_text$confounding_variables <- sprintf('confounding_variables: %s', paste(input$dimmer_confounding_variables, sep=', '))
  
  config_text$dmr_search <- ifelse(input$dimmer_dmr_search, print('dmr_search: true'), print('dmr_search: false'))
  config_text$pause <- sprintf('pause: false')   # ------> hardcoded to FALSE, did not find solution to this yet
  config_text$max_cpg_dist <- sprintf('max_cpg_dist: %i', input$dimmer_max_cpg_dist)
  config_text$w_size <- sprintf('w_size: %i', input$dimmer_w_size)
  config_text$n_exceptions <- sprintf('n_exceptions: %i', input$dimmer_n_exceptions)
  config_text$p_value_cutoff <- sprintf('p_value_cutoff: %f', input$dimmer_p_value_cutoff)
  config_text$min_diff <- sprintf('min_diff: %f', input$dimmer_min_diff)
  config_text$p_value_type <- sprintf('p_value_type: %s', input$dimmer_p_value_type)
  config_text$n_permutations_dmr <- sprintf('n_permutations_dmr: %i', input$dimmer_n_permutations_dmr)
  config_text$n_random_regions <- sprintf('n_random_regions: %i', input$dimmer_n_random_regions)
  
  config_text$save_permu_plots <- ifelse(input$dimmer_save_permu_plots, print('save_permu_plots: true'), print('save_permu_plots: false'))
  config_text$save_beta <- ifelse(input$dimmer_save_beta, print('save_beta: true'), print('save_beta: false'))
  config_text$save_search_plots <- ifelse(input$dimmer_save_search_plots, print('save_search_plots: true'), print('save_search_plots: false'))
  config_text$save_dmr_permu_plots <- ifelse(input$dimmer_save_dmr_permu_plots, print('save_dmr_permu_plots: true'), print('save_dmr_permu_plots: false'))
  config_text$save_search_tables <- ifelse(input$dimmer_save_search_tables, print('save_search_tables: true'), print('save_search_tables: false'))
  
  return(config_text)
}

check_variable <- function(variable, df){
  binary <- FALSE
  numeric <- FALSE
  
  if(length(unique(df[,variable])) == 2){
    binary <- TRUE
  }
  
  if(is.numeric(df[,variable])){
    numeric <- TRUE
  }
  
  return(list(numeric=numeric,
              binary=binary))
}


#function to unzip/untar files 
decompress <- function(archive, new_location){
  file_exts <- strsplit(archive, split="\\.")[[1]][2]
  
  if(c("tar") %in% file_exts){
    met_files <- untar(archive, list=T)
    untar(archive, exdir=new_location)
  }else if(c("zip") %in% file_exts){
    met_files <- unzip(archive, list=T)[["Name"]]
    unzip(archive, exdir=new_location)
  }else{
    return(1)# no valid file extension/compression
  }
  unlink(archive)
  return(met_files)
}

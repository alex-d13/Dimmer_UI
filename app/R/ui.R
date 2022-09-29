ui <- dashboardPage(
  
  skin = 'black',
  title = 'DiMmer',
  
  dashboardHeader(
    title = span("DiMmer 2.0"),
    titleWidth = 300,
    
    dropdownMenu(
      type = 'notifications',
      headerText = strong("HELP"),
      icon = icon('question'),
      badgeStatus = NULL,
      notificationItem(
        text = ('DiMmer Manual'),
        icon = icon("spinner")
      )
    ),
    tags$li(
      a(
        strong("ABOUT DiMmer"),
        height = 40,
        href = "https://github.com/baumbachlab/Dimmer",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      useShinyjs(),
      extendShinyjs(text = jscode, functions = c('collapse','expand')),
      id='sidebarmenu',
      menuItem('Welcome', tabName='dimmer_welcome',icon=icon('fas fa-home')),
      menuItem('DiMmer workflow',tabName = 'dimmer_workflow',icon=icon('fas fa-wrench')),
      downloadBttn("dimmer_create_config", 
                   label = "Create config file", 
                   icon = icon('fas fa-check-double'), 
                   color = "success",
                   size = 'l',
                   block = F,
                   style = 'simple'),
      actionBttn("dimmer_start", 
                 label = "START DiMmer", 
                 icon = icon('play-circle'), 
                 color = "success",
                 size = 'l',
                 block = F,
                 style = 'simple'),
      uiOutput('dimmer_download')
    )
  ),
  
  dashboardBody(
    
    includeCSS('../www/style.css'),
    
    #useShinyjs(),
    introjsUI(),
    waiter::use_waiter(),
    #waiter_show_on_load(html = tagList(spin_rotating_plane(), "Preparing Dimmer UI ...")),
    
    tabItems(
      tabItem(
        tabName = 'dimmer_welcome',
        h1('Dynamically configure and run DiMmer to study DNA Methylation'),
        
        fluidRow(
          box(
            title='Welcome to DiMmer', width=12, collapsible = F,
            HTML(welcome.dimmer())
          )
        ),
        fluidRow(
          column(5,           actionBttn("dimmer_get_started", 
                                         label = "Get started ...", 
                                         icon = icon('arrow-right'), 
                                         color = "primary",
                                         size = 'l',
                                         block = F,
                                         style = 'simple'))
        )
      ),
      
      tabItem(
        tabName = 'dimmer_workflow',
        h1('Methylation analysis with DiMmer'),
        column(12, spsTimeline(
          'timeLine',
          up_labels = c('','','','','','',''),
          down_labels = c('Sample Annotation & Input Type',
                        'Input Configuration',  
                        'Data Upload',
                        'CpG Statistics',
                        'DMR Search',
                        'Output',
                        'Finish'),
          completes = c(F,F,F,F,F,F,F),
          icons = list(icon("upload"), icon("cog"), icon("upload"), icon("cog"), icon("cog"), icon("cog"), icon('fas fa-check-double'))
        )),
        
        box(id='box1',
          title = 'Sample Annotation & Input Data Type', width = 12, collapsible = T, solidHeader = T, status = 'primary', collapsed = F,
          #clearableTextInput("dimmer_output_path", 'Enter path of existing output directory',placeholder = '~/dimmer_output'),
          #shinyDirButton('dimmer_output_path',label = 'Select existing output directory', title = 'Please choose a directory'),
          #hr(),
          fluidRow(
            column(4,
                   fileInput('dimmer_annotation_path', label = 'Select a sample annotation file', multiple = F),
                   actionBttn("dimmer_upload_annotation", 
                              label = "Upload annotation", 
                              color = "warning",
                              size = 's',
                              block = F,
                              style = 'simple'),
                   div(id='dimmer_upload_text_div',
                       p('Please upload the annotation file and press the orange upload button to continue.')),
                   selectInput('dimmer_input_type', ' Select data input type', choices = c('idat','beta','bisulfite')),
                   conditionalPanel(
                     condition = "input.dimmer_input_type == 'idat'",
                     wellPanel(HTML(upload.info.idat.annotation()))
                   ),
                   conditionalPanel(
                     condition = "input.dimmer_input_type == 'bisulfite'",
                     wellPanel(HTML(upload.info.bisulfite.annotation()))
                   ),
                   conditionalPanel(
                     condition = "input.dimmer_input_type == 'beta'",
                     selectInput('dimmer_array_type','Choose the array type, the beta-matrix originates from', choices = c('450k','epic','custom')),
                     wellPanel(HTML(upload.info.beta.annotation()))
                   ),
                   hidden(div(id='dimmer_upload_div',
                       selectInput('dimmer_variable','Choose your variable of interest', choices = c('Please upload a sample annotation file first.')),
                       p('TODO: warning that numeric of binary variable has to be selected')
                   )),

          ),
          column(8,
                 DT::dataTableOutput('dimmer_annotation_table_output')
          )
          ),
          hidden(actionBttn("dimmer_confirm1", 
                     label = "Confirm", 
                     color = "primary",
                     size = 'l',
                     block = F,
                     style = 'simple'))
          #hr(),
          #fileInput('dimmer_project_path', label = 'Select existing DiMmer project file', multiple = F),
          #p('If an existing project is loaded, DiMmer will directly start the DMR search.')
        ),
        
        box(id='box2',
          title = 'Select input data type', width = 12, collapsible = T, solidHeader = T, status = 'primary', collapsed = T,
          
          conditionalPanel(
            condition = "input.dimmer_input_type == 'idat'",
            h4('Configuration for idat input type:'),
            p('Preprocessing (the idat preprocessing steps are only available for the 450k chip):'),
            prettyCheckbox('dimmer_background_correction', 'Perform background correction', value = TRUE, status='primary'),
            prettyCheckbox('dimmer_probe_filtering','Perform probe quality filtering', value = TRUE, status='primary'),
            p('Cell composition estimation (only available for the "Regression" model):'),
            prettyCheckbox('dimmer_cell_composition','Perform cell composition estimation', value = FALSE, status='primary'),
            fileInput('dimmer_cell_composition_path','Path to cell composition estimation files', multiple = F),
            hidden(pickerInput('dimmer_cell_types','Select considered cell types', 
                               choices = c('cd8t', 'cd4t', 'nk', 'ncell', 'mono', 'gran'), 
                               selected = c('cd8t', 'cd4t', 'nk', 'ncell', 'mono', 'gran'),
                               multiple = T
            ))
          ),
          
          conditionalPanel(
            condition = "input.dimmer_input_type == 'beta'",
            h4('Configuration for beta input type:'),
            p('No additional configurations necessary. Please press "Confirm" to continue.')
          ),
          
          conditionalPanel(
            condition = "input.dimmer_input_type == 'bisulfite'",
            h4('Configuration for bisulfite input type:'),
            numericInput('dimmer_min_reads','Set the minimum number of reads mapped to a site (min_reads). If a site in a sample has less counts, it will be excluded from CpG statistics.',
                         min=1, max=10000, value = 10, step = 1),
            numericInput('dimmer_n_min_read_exceptions','Set the maximum allowed number of samples with a lower count than min_reads. If a higher number of samples does not fulfill min_reads, the site will be removed.',
                         min=1, max=10000, value = 2, step = 1),
            numericInput('dimmer_min_variance','Set a minimum variance for the beta-values of a site. Sites with a lower variance will be filtered out.',
                         min=0, max=1, value=0.0001, step=0.0001)
          ),
          actionBttn("dimmer_confirm2", 
                     label = "Confirm", 
                     color = "primary",
                     size = 'l',
                     block = F,
                     style = 'simple')
        ),
        
        box(id='box3',
          title='Data Upload', width=12, collapsible = T, solidHeader = T, status = 'primary', collapsed = T,
          conditionalPanel(
            condition = "input.dimmer_input_type == 'bisulfite' || input.dimmer_input_type == 'idat'",
            fileInput('dimmer_upload_methylation',
                      'Select compressed archive with methylation data',
                      multiple = F, accept = c('.zip','.tar.gz','.gz'))
          ),
          conditionalPanel(
            condition = "input.dimmer_input_type == 'beta'",
            fileInput('dimmer_beta_path','Select beta matrix file',
                      multiple = F, accept = c('.csv'))
          ),
          conditionalPanel(
            condition = "input.dimmer_input_type == 'idat'",
            wellPanel(HTML(upload.info.idat.methyl()))
          ),
          conditionalPanel(
            condition = "input.dimmer_input_type == 'beta'",
            wellPanel(HTML(upload.info.beta.methyl()))
          ),
          conditionalPanel(
            condition = "input.dimmer_input_type == 'bisulfite'",
            wellPanel(HTML(upload.info.bisulfite.methyl()))
          ),
          
          actionBttn("dimmer_confirm3", 
                     label = "Confirm", 
                     color = "primary",
                     size = 'l',
                     block = F,
                     style = 'simple')
        ),
        
        box(id='box4',
          title = 'CpG statistics and permutations', width=12, collapsible = T, solidHeader = T, status = 'primary', collapsed = T,
          selectInput('dimmer_data_type', 'Select a data type', choices = c('unpaired', 'paired')),
          numericInput('dimmer_n_permutations_cpg','Set the number of permutations for empirical CpG p-value estimation',
                       min=1, max=100000, step=1, value=1000),
          selectInput('dimmer_model','Select a statistical model for CpG p-values. Regression does only work with unpaired data.',
                      choices = c('T-test','Regression')),
          hidden(wellPanel(HTML(model.warning()))),
          
          conditionalPanel(
            condition = "input.dimmer_model == 'T-test'",
            selectInput('dimmer_alternative_hypothesis', 'Alternative hypothesis', choices = c('both','left','right')),
            prettyCheckbox('dimmer_assume_equal_variance','Assume, that both groups have equal variance', TRUE, status='primary')
          ),
          
          conditionalPanel(
            condition = "input.dimmer_model == 'Regression'",
            selectInput('dimmer_confounding_variables','Choose confounding variables from your sample annotation file', choices = c())
          ),
          actionBttn("dimmer_confirm4", 
                     label = "Confirm", 
                     color = "primary",
                     size = 'l',
                     block = F,
                     style = 'simple')
        ),
        
        box(id='box5',
          title = 'DMR search settings', width=12, collapsible = T, solidHeader = T, status = 'primary', collapsed = T,
          prettyCheckbox('dimmer_dmr_search','Select whether the program should execute the DMR search. If unchecked, DiMmer will terminate after the CpG permutations.', TRUE, status='primary'),
          #prettyCheckbox('dimmer_pause','Select if the program should pause before the DMR search. Some of the following variables might need information from the previous results to be set properly. If the pause option is set, the program will pause and let you inspect the interim results. Then you have the option to refine the variables from the next section by hand via additional inputs.', TRUE, status='primary'),
          numericInput('dimmer_max_cpg_dist', 'Set the maximum distance between CpGs in an island', min=1, max=100000, step=1, value=1000),
          numericInput('dimmer_w_size', 'Set the window size for the DMR search', min=1, max=100, step=1, value=5),
          numericInput('dimmer_n_exceptions', 'Set the number of exceptions (number of not significantly diff. methylated CpGs allowed in the window)', min=1, max=100, step=1, value=2),
          numericInput('dimmer_p_value_cutoff', 'Set the p-value cutoff (CpGs with a lower value are will be considered as significantly diff. methylated)', min=0, max=1, step=0.001, value=0.05),
          numericInput('dimmer_min_diff', 'Set the minimum mean methylation difference. If T-test is selected, the minimum mean methylation difference is an additional criterium for a CpG to count as significantly diff. methylated.', min=0, max=1, step=0.01, value=0.0),
          selectInput('dimmer_p_value_type','Select, which p-value type should be used to check the significance of the CpGs', choices = c('empirical','original','FWER','FDR','minP')),
          numericInput('dimmer_n_permutations_dmr', 'Set the number of permutations to calculate the statistical significance of the DMRs', min=0, max=100000, step=1, value=1000),
          numericInput('dimmer_n_random_regions', 'Set the number of random regions used to get a distribution for the p-value calculation', min=0, max=10000000, step=1, value=100000),
          
          actionBttn("dimmer_confirm5", 
                     label = "Confirm", 
                     color = "primary",
                     size = 'l',
                     block = F,
                     style = 'simple')
        ),
        
        box(id='box6',
          title = 'Output settings', width=12, collapsible = T, solidHeader = T, status = 'primary', collapsed = T,
          h4('CpG statistics'),
          prettyCheckbox('dimmer_save_permu_plots','Select whether the result plots of the permutation tests should be saved', FALSE, status='primary'),
          prettyCheckbox('dimmer_save_beta','Select whether the beta matrix should be saved', TRUE, status='primary'),
          hr(),
          h4('DMR search'),
          prettyCheckbox('dimmer_save_search_plots','Select whether the result plots of the DMR search should be saved', FALSE, status='primary'),
          prettyCheckbox('dimmer_save_dmr_permu_plots','Select whether the result plots of the DMR permutations should be saved (one plot for every DMR size)', FALSE, status='primary'),
          prettyCheckbox('dimmer_save_search_tables','Select whether the result tables of the DMR search should be saved', TRUE, status='primary'),
          
          actionBttn("dimmer_confirm6", 
                     label = "Confirm", 
                     color = "primary",
                     size = 'l',
                     block = F,
                     style = 'simple')
        ),
        
        box(id='box7',
          title = 'Other settings', width=12, collapsible = T, solidHeader = T, status = 'primary',collapsed = T,
          numericInput('dimmer_threads', 'Select number of threads to run DiMmer', min=1, max=300, step=1,value=4),
          
          actionBttn("dimmer_confirm7", 
                     label = "Confirm", 
                     color = "primary",
                     size = 'l',
                     block = F,
                     style = 'simple')
        )
      ),
      tabItem(
        tabName = 'dimmer_config',
        h1('Setup configuration for DiMmer'),
        
      ),
      
      tabItem(
        tabName = 'dimmer_upload',
        h1('Upload methylation data')
        
      )
    ) 

  )
)
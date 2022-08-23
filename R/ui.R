ui <- dashboardPage(
  
  skin = 'black',
  title = 'DiMmer',
  
  dashboardHeader(
    title = span("DiMmer"),
    titleWidth = 300,
    dropdownMenu(
      type = 'notifications',
      headerText = strong("HELP"),
      icon = icon('question'),
      badgeStatus = NULL,
      notificationItem(
        text = ('TEST'),
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
      id='sidebarmenu',
      menuItem('Welcome', tabName='dimmer_welcome',icon=icon('fas fa-home')),
      menuItem('Configuration', tabName='dimmer_config',icon=icon('fas fa-wrench')),
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
                 color = "danger",
                 size = 'l',
                 block = F,
                 style = 'simple')
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "style.css")
    ),
    
    useShinyjs(),
    introjsUI(),
    
    tabItems(
      tabItem(
        tabName = 'dimmer_welcome',
        h1('Dynamically configure and run DiMmer to study DNA Methylation'),
        
        fluidRow(
          box(
            title='Welcome to DiMmer', width=12, collapsible = F,
            HTML(welcome.dimmer())
          )
        )
      ),
      
      tabItem(
        tabName = 'dimmer_config',
        h1('Setup configuration for DiMmer'),
         
      #   column(12, spsTimeline(
      #     'timeLine',
      #     up_labels = c('','','','','',''),
      #     down_labels = c('Settings without defaults',
      #                   'Input & Preprocessing',
      #                   'CpG statistics and permutations',
      #                   'DMR search settings',
      #                   'Output settings',
      #                   'Finish'),
      #     completes = c(F,F,F,F,F,F),
      #     icons = list(icon("cog"), icon("cog"), icon("cog"), icon("cog"), icon("cog"), icon('fas fa-check-double'))
      #   )),
        
        fluidRow(
          box(
            title = 'Settings without defaults', width = 6, collapsible = F, solidHeader = T, status = 'primary',
            clearableTextInput("dimmer_output_path", 'Enter path of existing output directory',placeholder = '~/dimmer_output'),
            #shinyDirButton('dimmer_output_path',label = 'Select existing output directory', title = 'Please choose a directory'),
            verbatimTextOutput("dimmer_output_dir_text", placeholder = TRUE),
            hr(),
            fileInput('dimmer_annotation_path', label = 'Select a sample annotation file', multiple = F),
            conditionalPanel(
              condition = "input.dimmer_input_type == 'idat'",
              wellPanel(HTML(idat.info()))
            ),
            conditionalPanel(
              condition = "input.dimmer_input_type == 'bisulfite'",
              wellPanel(HTML(bisulfite.info()))
            ),
            
            selectInput('dimmer_variable','Choose your variable of interest', choices = c('Please upload a sample annotation file first.') ),
            hr(),
            fileInput('dimmer_project_path', label = 'Select existing DiMmer project file', multiple = F),
            p('If an existing project is loaded, DiMmer will directly start the DMR search.')
          ),
          
          box(
            title = 'Input and preprocessing', width=6, collapsible = F, solidHeader = T, status = 'primary',
            selectInput('dimmer_input_type', ' Select data input type', choices = c('idat','beta','bisulfite')),
            hr(),
            
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
              fileInput('dimmer_beta_path','Select beta matrix file'),
              selectInput('dimmer_array_type','Choose the array type, the beta-matrix originates from', choices = c('450k','epic','custom')),
              wellPanel(HTML(beta.info()))
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
            )
          )
        ),
        
        fluidRow(
          box(
            title = 'CpG statistics and permutations', width=6, collapsible = F, solidHeader = T, status = 'primary',
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
          ),
          
          box(
            title = 'DMR search settings', width=6, collapsible = F, solidHeader = T, status = 'primary',
            prettyCheckbox('dimmer_dmr_search','Select whether the program should execute the DMR search. If unchecked, DiMmer will terminate after the CpG permutations.', TRUE, status='primary'),
            prettyCheckbox('dimmer_pause','Select if the program should pause before the DMR search. Some of the following variables might need information from the previous results to be set properly. If the pause option is set, the program will pause and let you inspect the interim results. Then you have the option to refine the variables from the next section by hand via additional inputs.', TRUE, status='primary'),
            numericInput('dimmer_max_cpg_dist', 'Set the maximum distance between CpGs in an island', min=1, max=100000, step=1, value=1000),
            numericInput('dimmer_w_size', 'Set the window size for the DMR search', min=1, max=100, step=1, value=5),
            numericInput('dimmer_n_exceptions', 'Set the number of exceptions (number of not significantly diff. methylated CpGs allowed in the window)', min=1, max=100, step=1, value=2),
            numericInput('dimmer_p_value_cutoff', 'Set the p-value cutoff (CpGs with a lower value are will be considered as significantly diff. methylated)', min=0, max=1, step=0.001, value=0.05),
            numericInput('dimmer_min_diff', 'Set the minimum mean methylation difference. If T-test is selected, the minimum mean methylation difference is an additional criterium for a CpG to count as significantly diff. methylated.', min=0, max=1, step=0.01, value=0.0),
            selectInput('dimmer_p_value_type','Select, which p-value type should be used to check the significance of the CpGs', choices = c('empirical','original','FWER','FDR','minP')),
            numericInput('dimmer_n_permutations_dmr', 'Set the number of permutations to calculate the statistical significance of the DMRs', min=0, max=100000, step=1, value=1000),
            numericInput('dimmer_n_random_regions', 'Set the number of random regions used to get a distribution for the p-value calculation', min=0, max=10000000, step=1, value=100000)
          )
        ),
        
        fluidRow(
          box(
            title = 'Output settings', width=6, collapsible = F, solidHeader = T, status = 'primary',
            h4('CpG statistics'),
            prettyCheckbox('dimmer_save_permu_plots','Select whether the result plots of the permutation tests should be saved', TRUE, status='primary'),
            prettyCheckbox('dimmer_save_beta','Select whether the beta matrix should be saved', TRUE, status='primary'),
            hr(),
            h4('DMR search'),
            prettyCheckbox('dimmer_save_search_plots','Select whether the result plots of the DMR search should be saved', TRUE, status='primary'),
            prettyCheckbox('dimmer_save_dmr_permu_plots','Select whether the result plots of the DMR permutations should be saved (one plot for every DMR size)', FALSE, status='primary'),
            prettyCheckbox('dimmer_save_search_tables','Select whether the result tables of the DMR search should be saved', TRUE, status='primary')
          ),
          
          box(
            title = 'Other settings', width=6, collapsible = F, solidHeader = T, status = 'primary',
            numericInput('dimmer_threads', 'Select number of threads to run DiMmer', min=1, max=300, step=1,value=4)
          )
        )

      )
    )
  )
)
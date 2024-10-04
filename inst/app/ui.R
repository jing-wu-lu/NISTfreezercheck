jscode <- "shinyjs.focus = function(e_id) {document.getElementById(e_id).focus();}"


ui <- dashboardPage(
  # Header -------------------------------------------------------------------------------------
  header <- dashboardHeader(
    title = span(icon('check-circle'), "FreezerCheck"), 
    titleWidth = bar_width
  ),
  
  # Sidebar ------------------------------------------------------------------------------------
  sidebar <- dashboardSidebar(
    width = bar_width,
    sidebarMenu(id = 'left_menu',
                menuItem('Status Dashboard', 
                         tabName = 'status', 
                         icon = icon('home', lib = 'font-awesome')),
                menuItem('Audit History', 
                         tabName = 'audit_history', 
                         icon = icon('list-ul', lib = 'font-awesome')),
                menuItem('Create an Audit', 
                         tabName = 'create_audit', 
                         icon = icon('plus-square', lib = 'font-awesome')),
                menuItem('Current Audit', 
                         tabName = 'do_audit', 
                         icon = icon('search', lib = 'font-awesome')),
                menuItem('Discrepancies', 
                         tabName = 'discrepancies', 
                         icon = icon('warning', lib = 'font-awesome'))
    )
  ),
  
  # Body ---------------------------------------------------------------------------------------
  body <- dashboardBody(
    ## _Styling --------------------------------------------------------------------------------
    useShinyjs(),
    # extendShinyjs(text = DOMwidth),
    extendShinyjs(text = jscode, functions = "focus"),
    tags$head(
      tags$style(
        HTML('.main-header .logo {text-align:left; width: 200px;} 
              .small-box {margin-top: 20px; margin-bottom: 0px; border-radius: 0px;}
              .box {margin-bottom: 0px; border-radius: 0px;}
              .box-title {margin-top: 10px;}
              .display .dataTable .no-footer {width: 100%;}
              .content {overflow: visible;}
              .shiny-input-container:not(.shiny-input-container-inline) {width: 100%;}
              #scan_info {margin-top: 10px;}
              #scan-controls {display: inline-flex; padding: 0px; width: 100%; height: 52px; margin: 0px; margin: 0px;}
              #scan_barcode {height: 52px;}
              .nav-button {padding-right: 15px; padding-left: 15px; padding-top: 0px; border-radius: 0px; margin-top: 0px; background-color: #00629c; border-color: #00629c; color: #fff; text-align: right;}
              .success {color: #00a65a; width: 100%; height: 100%; text-align: center}
              .failure {color: red; width: 100%; height: 100%; text-align: center}
              #progress_text {display: block; max-width: 200px;} 
              #create_audit_overlay {height: 100%; width: 100%; position: absolute; padding-left: 15px; padding-right: 15px;}
              #create_audit_controls {position: relative; padding: 0px; margin: 0px;}
              #scan_overlay {height: 100%; width: 100%; position: absolute; padding-left: 15px; padding-right: 15px;}
              #scan_controls {position: relative; padding: 0px; margin: 0px;}
              #audit_preserve {min-width: 110px;}
              #audit_approve {min-width: 125px;}
              #audit_history_accuracy .small-box {margin-top: 0px;}
              #box_current_audit {padding-right: 2px;}
              #box_current_audit .col-sm-12 {padding-right: 0px; padding-left: 0px;}
              #box_aux_info {padding-left: 2px;}
              #box_aux_info .col-sm-12 {padding-right: 0px; padding-left: 0px;}
              .floating-button {position: relative; float: right; padding-right: 15px}
              #show_pending_audits {background-color: #dd4b39; color: white;}
              .label-red {color: #ff0000;}
              .label-normal {color: #333;}
              .button-focus {border-color: #00629c;}
              #modal_finalize_cancel {margin-left: 0px;}
              #start_audit {height: 50px; margin-top: 20px; font-size: 2em; text-align: center;}')
      )
    ),
    ## _Tabs -----------------------------------------------------------------------------------
    tabItems(
      ### __Status -----------------------------------------------------------------------------
      tabItem(tabName = 'status',
              uiOutput(outputId = 'outstanding_audits', 
                       class = "floating-button", 
                       inline = TRUE),
              HTML('<h1 style = "padding-left: 15px; padding-right: 15px; margin: 5px;">NIST Biorepository</h1>'),
              if (demo_mode) {
                HTML('<h2 style = "padding-left: 15px; padding-right: 15px; margin: 5px;">Aliquot Position Quality Control Center (DEMONSTRATION MODE)</h2>')
              } else {
                HTML('<h2 style = "padding-left: 15px; padding-right: 15px; margin: 5px;">Aliquot Position Quality Control Center</h2>')
              },
              box(class = "small-box",
                  h3(textOutput('annual_progress')),
                  width = 12, 
                  height = 122,
                  units = 'px',
                  solidHeader = FALSE, 
                  background = 'blue',
                  plotOutput(outputId = 'coverage',
                             height = '100%', 
                             click = 'coverage_click')),
              valueBoxOutput(outputId = 'audits_last',
                             width = 6),
              valueBoxOutput(outputId = 'audits_n',
                             width = 6),
              column(6, actionButton(inputId = 'go_audit', 
                                     label = 'View Audit History', 
                                     width = '100%', 
                                     class = 'nav-button')),
              column(6, actionButton(inputId = 'go_last', 
                                     label = 'View Last Audit', 
                                     width = '100%', 
                                     class = 'nav-button')),
              valueBoxOutput(outputId = 'audits_next', 
                             width = 6),
              valueBoxOutput(outputId = 'discrepancies_n', 
                             width = 6),
              column(6, actionButton(inputId = 'go_schedule', 
                                     label = 'View/Edit Audit Schedule', 
                                     width = '100%', 
                                     class = 'nav-button')),
              column(6, actionButton(inputId = 'go_discrep', 
                                     label = 'View Discrepancies', 
                                     width = '100%', 
                                     class = 'nav-button')),
              column(12, 
                     actionButton(inputId = 'start_audit', 
                                  label = 'Start a New Audit of Aliquot Positions',
                                  width = '100%', 
                                  class = 'nav-button'))
      ),
      
      ### __Audit History ----------------------------------------------------------------------
      tabItem(tabName = 'audit_history',
              valueBoxOutput('audit_history_accuracy', 
                             width = 12),
              box(title = "Past Audits (click to view details)", 
                  width = 12,
                  DTOutput('audit_history_table')
              ),
              div(id = 'audit_details',
                  box(title = "Audit Details (click to expand on discrepancies)",
                      width = 12,
                      DTOutput('audit_history_details')
                  )
              )
      ),
      
      ### __Create Audit -----------------------------------------------------------------------
      tabItem(tabName = 'create_audit',
              div(id = 'create_audit_overlay', 
                  h4("An audit is underway."),
                  column(3,
                         actionButton(inputId = 'existing_audit_continue', 
                                      label = 'Continue Current Audit',
                                      width = '100%'),
                         actionButton(inputId = 'existing_audit_abort', 
                                      label = 'Abort Current Audit', icon = icon('exclamation-circle'),
                                      width = '100%'))
                  ),
              div(id = 'create_audit_controls',
                  box(title = 'Start a new audit of freezer positions', 
                      width = 12,
                      fluidRow(style = 'padding:0px;',
                               column(10,
                                      style = 'padding-right:0px;',
                                      selectInput(inputId = "freezer",
                                                  # label = NULL,
                                                  label = "Select a freezer",
                                                  choices = freezer_list,
                                                  selected = NULL, 
                                                  multiple = FALSE,
                                                  width = '100%')
                               ),
                               column(2,
                                      style = 'padding-left:0px; padding-top: 25px;',
                                      actionButton(inputId = 'random_freezer', 
                                                   label = NULL, 
                                                   icon = icon("random"), 
                                                   width = '100%')
                               )
                      ),
                      fluidRow(style = 'padding:0px;',
                               column(3,
                                      selectInput(inputId = "random",
                                                  label = "Choice Basis",
                                                  width = '100%',
                                                  choices = c("True Random", "By Container"),
                                                  selected = "By Container")
                               ),
                               column(9,
                                      selectizeInput(inputId = "addl_columns",
                                                     label = "Include Additional Information Columns", 
                                                     width = '100%',
                                                     choices = fields,
                                                     selected = NA,
                                                     multiple = TRUE)
                               )
                      ),
                      HTML("<b>Type of Position Audit</b>"),
                      p(""),
                      fluidRow(
                        column(2,
                               switchInput("get_some", 
                                           offLabel = "Full", 
                                           onLabel = "Standard", 
                                           value = TRUE, 
                                           width = '205px'),
                               switchInput("n_or_fraction", 
                                           offLabel = "Absolute", 
                                           onLabel = "Fractional", 
                                           value = TRUE, 
                                           width = '219px'),
                               checkboxInput("addl_controls", 
                                             label = 'More Controls', 
                                             value = FALSE)
                        ),
                        column(10,
                               textOutput('label_slider'),
                               sliderInput(inputId = "nf_value",
                                           width = '100%',
                                           label = "",
                                           min = 5,
                                           max = 55,
                                           value = 10,
                                           step = 5),
                               div(id = 'additional_controls', class = 'hidden',
                                   fluidRow(
                                     column(6, sliderInput(inputId = 'condense_rack',
                                                           label = 'Condense Racks to (%):',
                                                           min = 20,
                                                           max = 50,
                                                           value = 35,
                                                           step = 5)
                                     ),
                                     column(6,sliderInput(inputId = 'condense_box',
                                                          label = 'Condense Boxes to (%):',
                                                          min = 20,
                                                          max = 50,
                                                          value = 25,
                                                          step = 5)
                                     )
                                   )
                               )
                        )
                      ),
                      actionButton(inputId = 'start', 
                                   label = 'Begin a New Audit', 
                                   width = '100%'),
                      actionButton(inputId = 'start_prior', 
                                   label = 'Return to Prior Audit', 
                                   width = '100%')
                  )
              )
      ),
      ### __Perform Audit ----------------------------------------------------------------------
      tabItem(tabName = 'do_audit',
              div(id = 'scan_overlay',
                  h4("Please create or load an audit first.")),
              div(id = 'scan_controls',
                  box(id = 'scan_panel', 
                      title = NULL,
                      width = 12,
                      box(title = NULL, 
                          background = 'blue', 
                          width = '100%', 
                          height = 'auto',
                          htmlOutput('scan_info'),
                          htmlOutput('progress_box', 
                                     width = '100%', 
                                     height = '20px')
                      ),
                      div(id = 'scan-controls',
                          icon('barcode', 'fa-4x'),
                          textInput('scan_barcode', 
                                    label = NA,
                                    width = '100%',
                                    placeholder = 'Scan a barcode'), 
                          actionButton('audit_preserve', icon = icon('save'), label = "Pause Audit", width = '25%'), 
                          actionButton('audit_approve', icon = icon('check'), label = "Finalize Audit", width = '25%')#,
                          # actionButton('scan_manual',
                          #              label = 'No Barcode Available',
                          #              width = '26%')
                      )
                  ),
                  column(8, id = 'box_current_audit', 
                         box(width = 12, 
                             title = "Aliquot Positions to Audit",
                             DTOutput('current_audit')
                         )
                  ),
                  column(4, id = 'box_aux_info', 
                         box(width = 12, 
                             collapsible = TRUE, 
                             collapsed = TRUE,
                             title = "Metadata", 
                             selectizeInput('aliquot_info',
                                            label = "Select UDFs for more...",
                                            choices = fields[!fields %in% typical_identifying_columns],
                                            selected = NA,
                                            multiple = TRUE),
                             DTOutput('GUAID_details'),
                             br(),
                             fluidRow(
                               column(6,
                                      actionButton('manual_verify', icon = icon('check'), label = "Verify", width = '100%')
                               ),
                               column(6,
                                      actionButton('manual_reject', icon = icon('times'), label = "Reject", width = '100%')
                               )
                             )
                         )
                  )
              )
      ),
      ### __Discrepancies ------------------------------------------------------------------------
      tabItem(tabName = 'discrepancies',
              box(title = 'Resolve Outstanding Discrepancies', 
                  width = 12,
                  htmlOutput('discrepancies_remaining_status'),
                  br(),
                  DTOutput('discrepancies_remaining'),
                  actionButton(inputId = 'discrepancies_download_outstanding', 
                               label = 'Download all for import to Freezerworks',
                               icon = icon('download'),
                               class = "floating-button",
                               width = '100%')
              ),
              box(title = 'Previously Resolved Discrepancies',
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  DTOutput('discrepancies_all')
              )
      )
    )
  )#,
  
  # (DISABLED) Right Sidebar (notices, users, etc.) -------------------------------------------------------
  # rightsidebar <- rightSidebar(
  #   background = "dark",
  #   rightSidebarTabContent(
  #     id = 1,
  #     title = "Tab 1",
  #     icon = "desktop",
  #     active = TRUE,
  #     sliderInput(
  #       "obs",
  #       "Number of observations:",
  #       min = 0, max = 1000, value = 500
  #     )
  #   ),
  #   rightSidebarTabContent(
  #     id = 2,
  #     title = "Tab 2",
  #     textInput("caption", "Caption", "Data Summary")
  #   ),
  #   rightSidebarTabContent(
  #     id = 3,
  #     icon = "paint-brush",
  #     title = "Tab 3",
  #     numericInput("obs", "Observations:", 10, min = 1, max = 100)
  #   )
  # ),
  # title = "Right Sidebar"
)

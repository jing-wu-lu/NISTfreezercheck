server <- function(session, input, output) {

  useShinyjs()
  # SESSION VALUES --------------------------------------------------------------------------------------
  audit_list <- reactiveVal(value = audits)
  audit_count <- reactive(nrow(audit_list()))
  audit_complete <- reactiveVal(FALSE)
  audit_complete_once <- reactiveVal(TRUE)
  audit_period <- reactiveVal(value = audit_schedule_period)
  audit_date_last <- reactive(max(audit_list()$date_scanned))
  audit_date_next <- reactive(audit_date_last() + audit_period())
  audit_accuracy <- reactiveVal(value = round(mean(100 * audit_history_summary$Correct), 1))
  audit_spread <- reactiveVal(value = round(sd(100 * audit_history_summary$Correct), 1))
  audit_indices <- reactiveVal(value = as.numeric(isolate(audit_list())$audit_index))
  audit_indices_max <- reactive(max(audit_indices()))
  audit_temp <- reactive(audit_list() %>% filter(audit_index == audit_indices_max()))
  audit_index_active <- reactiveVal(NULL)
  audit_container <- reactiveVal(NULL)
  discrep <- reactiveVal(value = make_discrep())
  discrep_count <- reactive(nrow(discrep()))
  discrep_remaining <- reactive(discrep() %>% filter(Resolved == FALSE))
  discrep_left <- reactive(nrow(discrep_remaining()))
  discrep_resolved <- reactive(discrep() %>% filter(Resolved == TRUE))
  discrep_indices <- reactive(audit_temp() %>%
                                filter(!is.na(discrepancy_index)) %>%
                                pull(discrepancy_index))
  discrep_temp <- reactive(discrep() %>% filter(discrepancy_index %in% discrep_indices()))
  discrep_index_active <- reactiveVal(NULL)
  discrep_resolution_DT <- reactive(discrep_remaining() %>%
                                      filter(discrepancy_index == discrep_index_active()) %>%
                                      select(-date_scanned) %>%
                                      left_join(audits, by = 'discrepancy_index') %>%
                                      select(Issue, Action_Taken, lag, date_scanned,
                                             User, GUALIQUOTID, SECTION, POSITION1, POSITION2, POSITION3) %>%
                                      mutate(lag = paste(lag, 'days')) %>%
                                      rename("Stale By" = "lag",
                                             "Scanned" = "date_scanned",
                                             "GU Aliquot ID" = "GUALIQUOTID",
                                             "Section" = "SECTION",
                                             "Position 1" = "POSITION1",
                                             "Position 2" = "POSITION2",
                                             "Position 3" = "POSITION3") %>%
                                      t() %>%
                                      as.data.frame(stringsAsFactors= FALSE) %>%
                                      rownames_to_column()
  )
  discrep_down <- reactiveValues(table = NULL,
                                 date = NULL,
                                 freezer = NULL)
  discrep_down_table <- reactiveVal(NULL)
  this_audit <- reactiveValues(positions = NULL,
                               discrepancies = data.frame(GUAID = character(),
                                                          discrepancy_index = integer(),
                                                          Resolved = logical(),
                                                          Issue = character(),
                                                          Action_Taken = character(),
                                                          date_resolved = as.Date(character()),
                                                          stringsAsFactors = FALSE),
                               time_start = NULL,
                               time_end = NULL,
                               duration = NULL,
                               current_GUAID = NULL,
                               freezer = NULL)
  this_GUAID <- reactiveVal(NULL)
  progress_audit <- reactive(sum(this_audit$positions$Status == "Pending"))
  progress_total <- reactiveVal(progress)
  freezer_n_aliquots <- reactiveVal(0)
  trigger_finalize <- reactiveVal(FALSE)
  if (demo_mode) {
    paused_audits_path <- paste(dir_pack_demo, "paused_audits", sep = dir_sep)
  } else {
    paused_audits_path <- paste(dir_user, "paused_audits", sep = dir_sep)
  }
  paused_audits <- list.files(paused_audits_path)
  outstanding_audits <- reactiveVal(length(paused_audits))
  loaded_audit <- reactiveVal(NULL)
  is_old_audit <- reactiveVal(FALSE)
  shinyjs::hide('audit_details')
  shinyjs::hide('scan_controls')
  shinyjs::hide('create_audit_overlay')
  shinyjs::hide('start_prior')
  shinyjs::disable('audit_approve')
  observe({
    ifelse(input$scan_barcode != "", this_GUAID(str_trim(input$scan_barcode)), this_GUAID(this_audit$current_GUAID))
  })


  # STATUS DASHBOARD TAB ------------------------------------------------------------------------------
  ## _Status Rendered Outputs -------------------------------------------------------------------------
  output$audits_n <- renderValueBox(
    valueBox(value = audit_count(),
             subtitle = 'Total Positions Audited',
             icon = icon('list'),
             color = 'blue')
  )
  output$audits_last <- renderValueBox(
    valueBox(value = ifelse(nrow(audit_list()) == 0,
                            "(None performed)",
                            format.Date(audit_date_last(), "%d %b %Y")),
             subtitle = 'Last Audit Performed',
             icon = icon('calendar'),
             color = 'blue')
  )
  output$audits_next <- renderValueBox(
    valueBox(value = ifelse(nrow(audit_list()) == 0,
                            "(None performed)",
                            format.Date(audit_date_next(), "%d %b %Y")),
             subtitle = ifelse(audit_date_next() > Sys.Date(), 'Next Audit Due', 'Audit Overdue'),
             icon = icon('calendar'),
             color = ifelse(audit_date_next() > Sys.Date(), 'blue',
                            ifelse(audit_date_last() + (3 * audit_period()) > Sys.Date(), 'yellow', 'red'))))
  output$discrepancies_n <- renderValueBox(valueBox(value = paste(discrep_count() - discrep_left(), "of", discrep_count()),
                                                    subtitle = 'Discrepancies Resolved',
                                                    icon = icon(ifelse(discrep_left() == 0, 'thumbs-up', 'warning')),
                                                    color = ifelse(discrep_left() == 0, 'green', 'yellow')))
  output$coverage <- renderPlot(
    progress_total() %>%
      filter(category == "Overall") %>%
      ggplot(aes(y = percent, x = category)) +
      geom_col(fill = "#73b700", show.legend = FALSE) +
      scale_y_continuous(limits = c(0, 1),
                         breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("0%", "25%", "50%", "75%", "100%"),
                         expand = expand_scale(add = c(0.01, 0.02))) +
      coord_flip() +
      theme_classic() +
      theme(axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.line = element_blank(),
            plot.background = element_blank(),
            axis.ticks = element_blank(), axis.text = element_text(size = 15)),
    height = 50,
    units = 'px')
  output$annual_progress <- renderText({
    paste0(year(Sys.Date()),
           ' cumulative progress is ',
           round(progress_total()[1, 1] * 100, 1), '%')
  })
  observe({
    if (outstanding_audits() > 0) {
      output$outstanding_audits <- renderUI({
        actionButton(inputId = 'show_pending_audits',
                     icon = icon('warning'),
                     label = paste(outstanding_audits(),
                                   ifelse(outstanding_audits() > 1, 'Outstanding Audits', 'Outstanding Audit')))
      })
      shinyjs::show('outstanding_audits')
    } else {
      shinyjs::hide('outstanding_audits')
    }
  })

  ## _Status Fill Modals ------------------------------------------------------------------------------
  ### __modal_last_audit ------------------------------------------------------------------------------
  output$modal_last_audit_DT <- renderDT(view_audit(audit_temp()))
  output$modal_last_audit_description <- renderText({
    t <- audit_history_summary %>% filter(audit_index == audit_indices_max())
    discrep <- round(t$Scanned * (1 - t$Correct))
    dis_i <- audit_temp() %>%
      filter(!is.na(discrepancy_index)) %>%
      pull(discrepancy_index)
    resolved <- discrepancies %>%
      filter(discrepancy_index %in% dis_i,
             Resolved) %>%
      nrow()
    HTML(paste0("<strong>On ",
                format.Date(t$date_scanned, '%d %b %Y'),
                ", ",
                simple_cap(t$User, sep = "."),
                " scanned " ,
                t$Scanned,
                " positions and resolved ",
                resolved,
                " of ",
                discrep,
                ifelse(discrep == 1, " discrepancy.", " discrepancies."),
                "</strong>"
    )
    )
  })
  observeEvent(input$modal_last_audit_DT_rows_selected, {
    ind <- input$modal_last_audit_DT_rows_selected
    d <- discrep_temp() %>% filter(discrepancy_index == audit_temp()[ind, 'discrepancy_index'])
    if (nrow(d) == 0) {
      output$modal_discrep_description <- NULL
    } else {
      output$modal_discrep_description <- renderText(view_discrep_details(d$discrepancy_index))
    }
  })

  ### __modal_schedule --------------------------------------------------------------------------------
  output$modal_schedule_DT <- renderDT({
    if (nrow(audits) == 0) {
      dates <- seq(from = Sys.Date(),
                   to = Sys.Date() + (5 * audit_period()),
                   by = audit_period())
    } else {
      dates <- seq(from = audit_date_last(),
                   to = Sys.Date() + (5 * audit_period()),
                   by = audit_period())
    }
    DT::datatable(
      data.frame(Date = format.Date(dates, "%d %b %Y")),
      rownames = FALSE,
      colnames = NULL,
      options = list(dom = 'tp',
                     columnDefs = list(list(className = 'dt-center', targets = 0)))
    )
  })

  ### __modal_progress --------------------------------------------------------------------------------
  output$progress_plot <- renderPlot({
    first <- progress %>% filter(x == 1)
    second <- progress %>% filter(x == 3)
    prog_plot <- progress[-1, ] %>%
      ggplot(aes(x = x,
                 y = y,
                 label = paste0(100 * percent, "%"),
                 colour = as.factor(ifelse(percent < 0.1, 0, 1))),
             hjust = 0) +
      geom_text(data = first, size = 5)
    if (nrow(second) > 0) {
      prog_plot <- prog_plot +
        geom_text(data = second, size = 5) +
        scale_y_continuous(breaks = 1:nrow(first),
                           labels = rev(first$category),
                           sec.axis = sec_axis(~.,
                                               breaks = 1:nrow(second),
                                               labels = rev(second$category))) +
        scale_x_continuous(limits = c(0.5, 3.5))
    } else {
      prog_plot <- prog_plot +
        scale_y_continuous(breaks = 1:nrow(first),
                           labels = rev(first$category)) +
        scale_x_continuous(limits = c(0.5, 1.5))
    }
    prog_plot +
      scale_color_manual(values = c(rgb(221, 75, 57, maxColorValue = 255),
                                    rgb(1, 166, 90, maxColorValue = 255)),
                         guide = FALSE) +
      theme_classic() +
      expand_limits(y = 0.5) +
      theme(legend.position = 'none',
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_text(size = 14),
            axis.text.x = element_blank(),
            axis.line = element_blank())
  })
  ## _Status Action Observers -------------------------------------------------------------------------
  observeEvent(input$go_audit, {
    if (nrow(audits) > 0) {
      audit_indices(NULL)
      updateTabItems(session, 'left_menu', selected = 'audit_history')
    } else {
      shinyjs::alert("Record an audit to view it.")
    }
  })
  observeEvent(input$go_discrep, updateTabItems(session, 'left_menu', selected = 'discrepancies'))
  observeEvent(input$go_last, {
    if (nrow(audits) > 0) {
      audit_indices(max(audits$audit_index))
      showModal(modal_last_audit())
    } else {
      shinyjs::alert("Record an audit to view it.")
    }
  })
  observeEvent(input$go_schedule, showModal(modal_schedule(audit_period())))
  observeEvent(input$modal_schedule_interval, {
    audit_period(input$modal_schedule_interval)
    if (demo_mode) {
      path_schedule <- paste(dir_pack_demo, "demo_schedule.RDS", sep = dir_sep)
    } else {
      path_schedule <- paste(dir_user, "schedule.RDS", sep = dir_sep)
    }
    write_rds(input$modal_schedule_interval, path_schedule)
  })
  observeEvent(input$start_audit, updateTabItems(session, 'left_menu', selected = 'create_audit'))
  observeEvent(input$coverage_click, showModal(modal_progress()))
  observeEvent(input$progress_dblclick, {
    frz <- nearPoints(progress,
                      input$progress_dblclick,
                      threshold = 20,
                      maxpoints = 1)$category
    updateSelectInput(session,
                      inputId = 'freezer',
                      selected = frz)
    updateTabItems(session, inputId = 'left_menu', selected = 'create_audit')
    removeModal()
  })
  observeEvent(input$show_pending_audits, showModal(modal_paused_audits()))
  ### __ Load a paused audit --------------------------------------------------------------------------
  observeEvent(input$load_paused_audit, {
    old_audit <- read_rds(paste0(dir_pause, dir_sep, input$paused_audits, ".RDS"))
    this_audit$positions <- old_audit$positions
    this_audit$discrepancies <- old_audit$discrepancies
    this_audit$time_start <- Sys.time()
    this_audit$time_end <- NULL
    this_audit$duration <- old_audit$duration
    this_audit$current_GUAID <- old_audit$current_GUAID
    this_audit$freezer <- old_audit$freezer
    output$current_audit <- renderDT(isolate(this_audit$positions) %>% select(-discrepancy_index),
                                     rownames = FALSE,
                                     colnames = c(pretty_colnames[-2], simple_cap(input$addl_columns, sep = "_")),
                                     selection = list(mode = 'single', selected = NULL),
                                     filter = 'top',
                                     options = list(smart = FALSE,
                                                    autoWidth = TRUE,
                                                    columnDefs = list(list(class = 'dt-center', targets = 0:5),
                                                                      list(class = 'compact', targets = 0:5))))
    shinyjs::show('scan_controls')
    shinyjs::hide('scan_overlay')
    shinyjs::enable('scan_barcode')
    shinyjs::enable('scan_manual')
    shinyjs::show('create_audit_overlay')
    shinyjs::hide('create_audit_controls')
    updateSelectInput(session, 'freezer', selected = old_audit$freezer)
    is_old_audit(TRUE)
    loaded_audit(input$paused_audits)
    audit_complete(FALSE)
    removeModal()
    updateTabItems(session, 'left_menu', selected = 'do_audit')
    audit_index_active(which(old_audit$positions$GUALIQUOTID == old_audit$current_GUAID))
  })
  # AUDIT HISTORY TAB ---------------------------------------------------------------------------------
  ## _History Rendered Outputs ------------------------------------------------------------------------
  output$audit_history_table <- renderDT(
    audit_history_summary %>%
      select(date_scanned, User, Scanned, Accuracy, type, style, basis, freezer) %>%
      mutate_at(c("User", "type", "style", "basis", "freezer"),
                as.factor),
    rownames = FALSE,
    colnames = c("Date", "Staff", "Scanned", "Accuracy", "Type", "Style", "Basis", "Freezer"),
    selection = list(mode = 'single', selected = NULL),
    options = list(pageLength = 5,
                   dom = 'lftip',
                   autoWidth = TRUE,
                   order = list(list(0, 'desc')),
                   columnDefs = list(list(className = 'dt-center', targets = 2:7),
                                     list(className = 'compact', targets = 0:7)))
  )
  output$audit_history_accuracy <- renderValueBox(
    valueBox(
      value = ifelse(is.na(audit_spread()),
                     HTML(paste0(audit_accuracy(), "% (n = 1)")),
                     HTML(paste0(audit_accuracy(), " &plusmn; ", audit_spread(), "%"))),
      subtitle = "Aggregate Accuracy",
      icon = icon(ifelse(audit_accuracy() > 90, 'thumbs-up',
                         ifelse(audit_accuracy() < 80, 'exclamation', 'warning'))),
      color = ifelse(audit_accuracy() > 90, 'green',
                     ifelse(audit_accuracy() < 80, 'red', 'yellow'))
    )
  )
  output$audit_history_details <- renderDT({
    t <- input$audit_history_table_rows_selected
    if (!is.null(t)) {
      audit_indices(t)
      view_audit(audit_temp())
    }
  })

  ## _History Action Observers ------------------------------------------------------------------------
  ### __Get selected audit history --------------------------------------------------------------------
  observeEvent(input$audit_history_table_rows_selected, ignoreNULL = TRUE, {
    audit_indices(input$audit_history_table_rows_selected)
    shinyjs::show('audit_details')
  })
  ### __Get discrepancy description -------------------------------------------------------------------
  observeEvent(input$audit_history_details_rows_selected, {
    ind <- input$audit_history_details_rows_selected
    d <- discrep_temp() %>% filter(discrepancy_index == audit_temp()[ind, 'discrepancy_index'])
    if (nrow(d) != 0) {
      output$modal_discrep_description2 <- renderText(view_discrep_details(d$discrepancy_index))
      showModal(modal_discrep())
    }
  })


  # CREATE AN AUDIT TAB -------------------------------------------------------------------------------
  ## _Create Audit Action Observers -------------------------------------------------------------------
  ### __Toggle fraction or total ----------------------------------------------------------------------
  observeEvent(input$n_or_fraction, {
    if (!input$n_or_fraction) {
      updateSliderInput(session,
                        inputId = "nf_value",
                        label = "Maximum Number",
                        min = 0, max = 1000, value = 100, step = 50)
      updateCheckboxInput(session, 'addl_controls', value = FALSE)
      disable("addl_controls")
    } else {
      updateSliderInput(session,
                        inputId = "nf_value",
                        label = "Percentage",
                        min = 5, max = 55, value = 10, step = 5)
      enable("addl_controls")
    }
  })
  ### __Choose full inventory -------------------------------------------------------------------------
  observe({
    if (!input$get_some) {
      updateSwitchInput(session,
                        inputId = "n_or_fraction", disabled = TRUE)
      updateSliderInput(session,
                        inputId = "nf_value",
                        label = "Complete QA",
                        min = 5, max = 100, value = 100)
      disable("nf_value")
      showModal(modal_fullQA(freezer = input$freezer))
    } else {
      if (!input$n_or_fraction) {
        updateSliderInput(session,
                          inputId = "nf_value",
                          label = "Maximum Number",
                          min = 0, max = 1000, value = 100, step = 50)
        updateCheckboxInput(session, 'addl_controls', value = FALSE)
        disable("addl_controls")
      } else {
        updateSliderInput(session,
                          inputId = "nf_value",
                          label = "Percentage",
                          min = 5, max = 55, value = 10, step = 5)
        enable("addl_controls")
      }
      enable("nf_value")
      updateSwitchInput(session, inputId = "n_or_fraction", disabled = FALSE)
    }
  })
  ### __Get more controls -----------------------------------------------------------------------------
  observeEvent(input$addl_controls, {
    if (input$addl_controls) {
      removeClass(id = 'additional_controls', class = 'hidden')
    } else {
      addClass(id = 'additional_controls', class = 'hidden')
    }
  })
  ### __Randomize Freezer -----------------------------------------------------------------------------
  observeEvent(input$random_freezer,  {
    # Eventually need to ignore freezers scanned within a defined time frame if that freezer passes QA
    updateSelectInput(session, inputId = 'freezer', selected = sample(freezer_list, 1))
  })
  ### __Start an audit -------------------------------------------------------------------------------
  observeEvent(input$start, ignoreNULL = TRUE, {
    focus <- ifelse(input$random == "True Random", FALSE, TRUE)
    if (demo_mode) {
      freezer_n_aliquots(nrow(demo_data %>% filter(FREEZERPHYSNAME == input$freezer)))
    } else {
      freezer_n_aliquots(get_total_positions(input$freezer))
    }
    this_audit$positions <- get_scan_positions(dsn = DSN,
                                               freezer = input$freezer,
                                               addl_columns = input$addl_columns,
                                               fraction = ifelse(input$n_or_fraction, input$nf_value/100, NA),
                                               return_all = !input$get_some,
                                               random = ifelse(input$random == "By Container", FALSE, TRUE),
                                               max_returns = ifelse(input$n_or_fraction, NA, input$nf_value),
                                               focus_racks = focus,
                                               condense_rack_to = input$condense_rack/100,
                                               condense_box_to = input$condense_box/100)
    if (!is.data.frame(this_audit$positions)) {
      showModal(modalDialog(title = NULL, h3(this_audit$positions), footer = tagList(modalButton("OK"))))
    } else {
      this_audit$positions <- this_audit$positions %>%
        mutate('discrepancy_index' = NA,
               'date_scanned' = as.Date(NA))
      this_audit$time_start <- Sys.time()
      this_audit$freezer <- input$freezer
      updateTabItems(session, 'left_menu', selected = 'do_audit')
      shinyjs::show('scan_controls')
      shinyjs::hide('scan_overlay')
      shinyjs::enable('scan_barcode')
      shinyjs::enable('scan_manual')
      audit_complete(FALSE)
      shinyjs::show('create_audit_overlay')
      shinyjs::hide('create_audit_controls')
      js$focus('scan_barcode')
      audit_index_active(1)
      is_old_audit(FALSE)
      this_scan <- this_audit$positions %>% select(-discrepancy_index)
      date_scanned_col <- which(names(this_scan) == "date_scanned") - 1
      output$current_audit <- renderDT(isolate(this_scan),
                                       rownames = FALSE,
                                       colnames = c(pretty_colnames[-2],
                                                    simple_cap(input$addl_columns, sep = "_"),
                                                    "date_scanned"),
                                       selection = list(mode = 'single', selected = NULL),
                                       filter = 'top',
                                       options = list(smart = FALSE,
                                                      autoWidth = TRUE,
                                                      columnDefs = list(list(class = 'dt-center', targets = 0:5),
                                                                        list(visible = FALSE, targets = date_scanned_col),
                                                                        list(class = 'compact', targets = 0:5))))
      proxy_current_audit <<- dataTableProxy('current_audit')
    }
  })
  observeEvent(input$existing_audit_continue, {
    updateTabItems(session, 'left_menu', 'do_audit')
    js$focus('scan_barcode')
  })
  observeEvent(input$existing_audit_abort, {
    shinyjs::show('create_audit_controls')
    shinyjs::hide('create_audit_overlay')
    shinyjs::show('scan_overlay')
    shinyjs::hide('scan_controls')
    shinyjs::show('start_prior')
  })
  observeEvent(input$start_prior, {
    updateTabItems(session, 'left_menu', selected = 'do_audit')
    audit_index_active(min(grep("Pending", this_audit$positions$Status)))
    shinyjs::show('scan_controls')
    shinyjs::hide('scan_overlay')
    shinyjs::enable('scan_barcode')
    shinyjs::enable('scan_manual')
    audit_complete(FALSE)
    shinyjs::show('create_audit_overlay')
    shinyjs::hide('create_audit_controls')
    js$focus('scan_barcode')

  })
  # CURRENT AUDIT TAB ---------------------------------------------------------------------------------
  ## _Audit Rendered Outputs --------------------------------------------------------------------------
  output$download_discrepancies <- downloadHandler(
    filename = function() {
      paste0(discrep_down$date,
             ' ',
             discrep_down$freezer,
             '.csv')
    },
    content = function(file) {
      write.csv(discrep_down_table(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  output$modal_download_discrepancies_preview <- renderDT(
    expr = head(discrep_down_table()),
    rownames = FALSE,
    options = list(dom = 't',
                   bSort = FALSE,
                   columnDefs = list(list(className = 'dt-center', targets = c(0:1)),
                                     list(className = 'compact', targets = c(0:1))))
  )
  #### Current audit table is rendered from ___Start an audit::observeEvent(input$start, ...)
  observe({
    n_rows <- nrow(this_audit$positions)
    if (!is.null(n_rows)) {
      if (n_rows > 0) {
        i <- round(100 * (n_rows - progress_audit())/n_rows, 0)
        output$progress_box <- renderText(
          paste0('<div id="progress_text" width="200px">Completed ', n_rows - progress_audit(), ' of ', n_rows, ' (', i, '%)&nbsp&nbsp</div>',
                 # '<canvas id="progress_bar" style="background-color: #00a65a; height: 10px;"></canvas>')
                 '<canvas id="progress_bar" style="background-color: #00a65a; height: 10px; width: ', i,'%;"></canvas>')
        )
        # Here are three techniques attempting to use jQuery to change progress bar width to keep it inline with the text.
        # Could not get this to work using htmlwidgets, V8, or extendshinyjs. Extendshinyjs changed the width but was immediately overridden back to 20px.
        # adjust_progress_bar_width(i)
        # V8::JS(paste0("$('#progress_bar').width(($('#progress_box').width() - $('#progress_text').width() - 1) * ", i / 100, ")", collase = ""))
        # js$DOMwidth(i)
        if (i == 100) {
          audit_complete(TRUE)
        }
      }
    }
  })
  ## _Audit Observers ---------------------------------------------------------------------------------
  ### __Table Click -----------------------------------------------------------------------------------
  observeEvent(input$current_audit_rows_selected, ignoreNULL = TRUE, {
    if (audit_index_active() != input$current_audit_rows_selected) {
      audit_index_active(input$current_audit_rows_selected)
      shinyjs::reset('scan_barcode')
      js$focus('scan_barcode')
    }
  })
  ### __Active Index Changes --------------------------------------------------------------------------
  observe( {
    i <- audit_index_active()
    table_row <- isolate(input$current_audit_rows_selected)
    if (!is.null(i)) {
      if (i != 0) {
        shinyjs::disable('manual_verify')
        shinyjs::disable('manual_reject')
        audit_containers <- parse_container(this_audit$positions$SECTION[i])
        audit_container(audit_containers$c1)
        this_audit$current_GUAID <- this_audit$positions$GUALIQUOTID[i]
        output$scan_info <- renderText({
          paste("<h3 id='scan_info'>Scanning #", i, "of", nrow(this_audit$positions), "positions in<b>", this_audit$freezer, "</b><br><br>",
                "<b>",
                this_audit$current_GUAID, "</b> expected in ",
                # paste0(audit_containers$c1, ": <b>"),
                # paste0(this_audit$positions$POSITION1[i], "</b><br>",
                #        paste0(rep('&emsp;', 11), collapse = "")),
                paste0("<b>", this_audit$positions$POSITION1[i], "</b>", collapse = ""),
                ifelse(is.null(audit_containers$c2),
                       "",
                       # paste0(audit_containers$c2, ": <b>",
                       #        this_audit$positions$POSITION2[i], "</b><br>",
                       #        paste0(rep('&emsp;', 11), collapse = ""))),
                       paste0(", <b>", this_audit$positions$POSITION2[i], "</b>", collapse = "")),
                ifelse(is.null(audit_containers$c3) | audit_containers$c3 == '',
                       "",
                       # paste0(audit_containers$c3, ": <b>",
                       #        this_audit$positions$POSITION3[i], "</b>")),
                       paste0(", <b>", this_audit$positions$POSITION3[i], "</b>")),
                "</h3><br>")
        })
        if (!is.null(input$aliquot_info)) {
          get_cols <- unique(c(input$aliquot_info, typical_identifying_columns))
        } else {
          get_cols <- typical_identifying_columns
        }
        details <- get_GUAID_info(GUAID = this_audit$current_GUAID,
                                  addl_columns = get_cols,
                                  include_defaults = FALSE)
        names(details) <- simple_cap(names(details), sep = "_")
        details <- t(details)
        output$GUAID_details <- renderDT(details,
                                         colnames = c("UDF", "Value"),
                                         selection = list(mode = 'none', selected = NULL),
                                         options = list(dom = 't', bSort = FALSE))
        selectRows(proxy = proxy_current_audit, i)
        selectPage(proxy = proxy_current_audit, ceiling(i/input$current_audit_state$length))
        shinyjs::reset('scan_barcode')
        js$focus('scan_barcode')
        shinyjs::enable('manual_verify')
        shinyjs::enable('manual_reject')
      }
    }
  })
  ### __No Barcode Available Clicked ------------------------------------------------------------------
  # Removed 2019-02-04 in favor of having this information always available
  # observeEvent(input$scan_manual, {
  #   if (!is.null(input$addl_columns)) {
  #     get_cols <- unique(c(input$addl_columns, typical_identifying_columns))
  #   } else {
  #     get_cols <- typical_identifying_columns
  #   }
  #   details <- get_GUAID_info(GUAID = this_audit$current_GUAID,
  #                           addl_columns = get_cols,
  #                           include_defaults = FALSE)
  #   names(details) <- simple_cap(names(details), sep = "_")
  #   details <- t(details)
  #   output$modal_GUAID_details <- renderDT(details,
  #                                          colnames = c(""),
  #                                          selection = list(mode = 'none', selected = NULL),
  #                                          options = list(dom = 't', bSort = FALSE))
  #   showModal(modal_manual_check(this_audit$current_GUAID))
  # })
  ### __Barcode is scanned ----------------------------------------------------------------------------
  observeEvent(input$scan_barcode, ignoreInit = TRUE, {
    scanned_GUAID <- str_trim(isolate(input$scan_barcode))
    if (!scanned_GUAID == "") {
      # ___GUAID match check --------------------------------------------------------------------------
      if (scanned_GUAID == this_audit$current_GUAID) {
        # ____Direct match ----------------------------------------------------------------------------
        showModal(modal_verify_success())
        Sys.sleep(0.5)
        if (this_audit$positions$Status[audit_index_active()] == "Discrepancy") {
          discrep_index <- this_audit$positions$discrepancy_index[audit_index_active()]
          discrep_i <- which(this_audit$discrepancies$discrepancy_index == discrep_index)
          this_audit$discrepancies <- this_audit$discrepancies %>% slice(-discrep_i)
        }
        this_audit$positions$Status[audit_index_active()] <- "Verified"
        this_audit$positions$date_scanned[audit_index_active()] <- Sys.Date()
        audit_index_active(next_audit_index(audit_index_active(), this_audit$positions))
        replaceData(proxy_current_audit,
                    data = this_audit$positions %>%
                      select(-discrepancy_index),
                    resetPaging = FALSE,
                    rownames = FALSE,
                    clearSelection = 'none')
        removeModal()
      } else {
        # ____Alt match in same Section, Position1, Position2, Position3 ------------------------------
        if (audit_container() == "Tube") {
          same_box <- this_audit$positions %>%
            filter(GUALIQUOTID == this_audit$current_GUAID) %>%
            select(SECTION, POSITION1)
        } else {
          same_box <- this_audit$positions %>%
            filter(GUALIQUOTID == this_audit$current_GUAID) %>%
            select(SECTION, POSITION1, POSITION2)
        }
        same_box <- same_box %>%
          left_join(this_audit$positions) %>%
          pull(GUALIQUOTID)
        if (scanned_GUAID %in% same_box) {
          showModal(modal_verify_alt_success())
        } else {
          # ____Fail audit ----------------------------------------------------------------------------
          GUAIDinfo <- get_GUAID_info(scanned_GUAID) %>% select(-GUALIQUOTID)
          if (nrow(GUAIDinfo) == 0) {
            output$modal_failure_FW <- renderText({
              paste0('<h4 class = "failure">No aliquot matching "', scanned_GUAID, '" exists within FreezerWorks!</h4>')
            })
          } else if (nrow(GUAIDinfo) > 1) {
            output$modal_failure_FW <- renderText({
              paste('<h4 class = "failure">More than one instance of "', scanned_GUAID, '" is present within FreezerWorks!</h4>')
            })
          } else {
            FW <- get_GUAID_info(scanned_GUAID)
            output$modal_failure_FW <- renderText({
              paste('Freezerworks shows<b>', scanned_GUAID, '</b>in',
                    FW$FREEZERNAME, FW[, 3], 'at',
                    'Position', paste(FW[, 4:5], collapse = " "))
            })
          }
          showModal(modal_verify_failure())
        }
      }
    }
  })
  ### __Alt success record ----------------------------------------------------------------------------
  observeEvent(input$modal_alt_verify, {
    i <- which(this_audit$positions$GUALIQUOTID == str_trim(isolate(input$scan_barcode)))
    if (this_audit$positions$Status[i] == 'Discrepancy') {
      remove_discrep <- which(this_audit$discrepancies$discrepancy_index == this_audit$positions$discrepancy_index[i])
      this_audit$discrepancies <- this_audit$discrepancies %>% slice(-remove_discrep)
    }
    this_audit$positions$Status[i] <- 'Verified'
    this_audit$positions$date_scanned[i] <- Sys.Date()
    this_audit$positions$discrepancy_index[i] <- NA
    replaceData(proxy_current_audit,
                data = this_audit$positions %>%
                  select(-discrepancy_index),
                resetPaging = FALSE,
                rownames = FALSE,
                clearSelection = 'none')
    shinyjs::reset('scan_barcode')
    removeModal()
  })
  ### __Alt success reject ----------------------------------------------------------------------------
  observeEvent(input$modal_alt_reject, {
    removeModal()
    FW <- get_GUAID_info(input$scan_barcode)
    output$modal_failure_FW <- renderText({
      paste('Freezerworks shows', input$scan_barcode, 'in',
            FW$FREEZERNAME, 'at',
            'Position', paste(FW[, 3:5], collapse = " "))
    })
    showModal(modal_verify_failure())
  })
  ### __Manual verify ---------------------------------------------------------------------------------
  observeEvent(input$manual_verify, {
    i <- which(this_audit$positions$GUALIQUOTID == this_audit$current_GUAID)
    if (this_audit$positions$Status[i] == 'Discrepancy') {
      remove_discrep <- which(this_audit$discrepancies$discrepancy_index == this_audit$positions$discrepancy_index[i])
      this_audit$discrepancies <- this_audit$discrepancies %>% slice(-remove_discrep)
    }
    this_audit$positions$Status[audit_index_active()] <- 'Verified'
    this_audit$positions$date_scanned[audit_index_active()] <- Sys.Date()
    this_audit$positions$discrepancy_index[audit_index_active()] <- NA
    replaceData(proxy_current_audit,
                data = this_audit$positions %>%
                  select(-discrepancy_index),
                resetPaging = FALSE,
                rownames = FALSE,
                clearSelection = 'none')
    audit_index_active(next_audit_index(audit_index_active(), this_audit$positions))
  })
  ### __Manual reject ---------------------------------------------------------------------------------
  observeEvent(input$manual_reject, {
    output$modal_failure_FW <- renderText({
      paste0('Would you like to record or resolve the discrepancy for <b>', this_audit$current_GUAID, '</b>?')
    })
    showModal(modal_verify_failure())
  })
  ### __Failure resolve -----------------------------------------------------------------------------
  observeEvent(input$modal_failure_resolve, {
    removeModal()
    output$modal_temp_discrepancy_resolve_DT <- renderDT({
      datatable(
        get_GUAID_info(this_GUAID(), addl_columns = typical_identifying_columns) %>%
          t() %>%
          as.data.frame() %>%
          rownames_to_column() %>%
          mutate(rowname = simple_cap(rowname, sep = "_")),
        rownames = NULL,
        colnames = c("", ""),
        options = list(dom = 't')
      )
    })
    showModal(modal_temp_discrepancy_resolve())
  })
  observeEvent(input$modal_temp_discrepancy_resolve, {
    this_discrep <- ifelse(nrow(this_audit$discrepancies) == 0,
                           1,
                           max(this_audit$discrepancies$discrepancy_index) + 1)
    this_audit$discrepancies <- bind_rows(this_audit$discrepancies,
                                          setNames(list(this_GUAID(),
                                                        this_discrep,
                                                        TRUE,
                                                        input$modal_discrepancy_issue,
                                                        input$modal_discrepancy_action,
                                                        Sys.Date()),
                                                   names(this_audit$discrepancies)))
    i <- which(this_audit$positions$GUALIQUOTID == this_GUAID())
    this_audit$positions$Status[i] <- 'Discrepancy'
    this_audit$positions$date_scanned[i] <- Sys.Date()
    this_audit$positions$discrepancy_index[i] <- this_discrep
    replaceData(proxy_current_audit,
                data = this_audit$positions %>%
                  select(-discrepancy_index),
                resetPaging = FALSE,
                rownames = FALSE,
                clearSelection = 'none')
    removeModal()
    shinyjs::reset('scan_barcode')
    audit_index_active(next_audit_index(audit_index_active(), this_audit$positions))
  })
  observeEvent(input$modal_temp_discrepancy_cancel_resolve, {
    if (!is.null(this_audit$current_GUAID))  {
      GUAID_active <- ifelse(input$scan_barcode != "", input$scan_barcode, this_audit$current_GUAID)
      i <- which(this_audit$positions$GUALIQUOTID == this_GUAID())
      # this_audit$positions$Status[i] <- "Pending"
      replaceData(proxy_current_audit,
                  data = this_audit$positions %>%
                    select(-discrepancy_index),
                  resetPaging = FALSE,
                  rownames = FALSE,
                  clearSelection = 'none')
      removeModal()
      shinyjs::reset('scan_barcode')
    }
  })
  ### __Failure record ------------------------------------------------------------------------------
  observeEvent(input$modal_failure_record, {
    removeModal()
    output$modal_temp_discrepancy_record_DT <- renderDT(
      datatable(
        get_GUAID_info(this_GUAID(), addl_columns = typical_identifying_columns) %>%
          t() %>%
          as.data.frame() %>%
          rownames_to_column() %>%
          mutate(rowname = simple_cap(rowname, sep = "_")),
        rownames = NULL,
        colnames = c("", ""),
        options = list(dom = 't')
      )
    )
    showModal(modal_temp_discrepancy_record())
  })
  observeEvent(input$modal_temp_discrepancy_record, {
    this_discrep <- ifelse(nrow(this_audit$discrepancies) == 0,
                           1,
                           max(this_audit$discrepancies$discrepancy_index) + 1)
    this_audit$discrepancies <- bind_rows(this_audit$discrepancies,
                                          setNames(list(this_GUAID(),
                                                        this_discrep,
                                                        FALSE,
                                                        input$modal_discrepancy_record_issue,
                                                        NA,
                                                        NA),
                                                   names(this_audit$discrepancies)))
    i <- which(this_audit$positions$GUALIQUOTID == this_GUAID())
    this_audit$positions$Status[i] <- 'Discrepancy'
    this_audit$positions$date_scanned[i] <- Sys.Date()
    this_audit$positions$discrepancy_index[i] <- this_discrep
    replaceData(proxy_current_audit,
                data = this_audit$positions %>%
                  select(-discrepancy_index),
                resetPaging = FALSE,
                rownames = FALSE,
                clearSelection = 'none')
    removeModal()
    shinyjs::reset('scan_barcode')
    audit_index_active(next_audit_index(audit_index_active(), this_audit$positions))
  })
  observeEvent(input$modal_temp_discrepancy_cancel_record, {
    if (!is.null(this_audit$current_GUAID))  {
      GUAID_active <- ifelse(input$scan_barcode != "", input$scan_barcode, this_audit$current_GUAID)
      i <- which(this_audit$positions$GUALIQUOTID == this_GUAID())
      # this_audit$positions$Status[i] <- "Pending"
      replaceData(proxy_current_audit,
                  data = this_audit$positions %>%
                    select(-discrepancy_index),
                  resetPaging = FALSE,
                  rownames = FALSE,
                  clearSelection = 'none')
      removeModal()
      shinyjs::reset('scan_barcode')
    }
  })
  ### __Pause an audit --------------------------------------------------------------------------------
  observeEvent(input$audit_preserve, {
    dir_pause <- ifelse(demo_mode,
                        paste(dir_pack_demo, "paused_audits", sep = dir_sep),
                        paste(dir_user, "paused_audits", sep = dir_sep))
    this_audit$time_end <- Sys.time()
    this_audit$duration <- as.numeric(this_audit$time_end) - as.numeric(this_audit$time_start)
    if (is_old_audit()) {
      audit_filename <- paste0(dir_pause,  loaded_audit())
    } else {
      audit_filename <- paste0(dir_pause,  format(Sys.Date(), "%Y-%m-%d"), " ", input$freezer)
      core_filename <- audit_filename
      i <- 2
      while (file.exists(paste0(audit_filename, ".RDS"))) {
        audit_filename <- paste0(core_filename, " [", i, "]")
        i <- i + 1
      }
    }
    write_rds(reactiveValuesToList(this_audit), paste0(audit_filename, ".RDS"))
    outstanding_audits(length(list.files(dir_pause)))
    # Flush current
    this_audit$positions <- this_audit$positions %>% slice(0)
    this_audit$discrepancies <- this_audit$discrepancies %>% slice(0)
    this_audit$time_start <-NULL
    this_audit$time_end <- NULL
    this_audit$duration <- NULL
    this_audit$current_GUAID <- NULL
    this_audit$freezer <- NULL
    # Reset presentation to defaults and move to dashboard
    shinyjs::show('scan_overlay')
    shinyjs::hide('scan_controls')
    shinyjs::disable('scan_barcode')
    shinyjs::disable('scan_manual')
    shinyjs::show('create_audit_controls')
    shinyjs::hide('create_audit_overlay')
    updateTabItems(session, inputId = 'left_menu', selected = 'status')
    shinyjs::alert(paste0('Audit saved as ', gsub("data/paused_audits/", "", audit_filename), '.'))
  })
  ### __Complete an audit -----------------------------------------------------------------------------
  observe({
    if (audit_complete() & audit_complete_once()) {
      shinyjs::enable('audit_approve')
      audit_complete_once(FALSE)
      trigger_finalize(TRUE)
    }
  })
  observeEvent(input$audit_approve, {
    trigger_finalize(TRUE)
  })
  observe({
    if (trigger_finalize()) {
      trigger_finalize(FALSE)
      positions_checked <- nrow(this_audit$positions)
      positions_verified <- sum(this_audit$positions$Status == "Verified")
      positions_discrep <- nrow(this_audit$discrepancies)
      positions_recorded <- sum(this_audit$discrepancies$Resolved == FALSE)
      positions_resolved <- sum(this_audit$discrepancies$Resolved == TRUE)
      audit_duration <- as.numeric(Sys.time()) - as.numeric(this_audit$time_start)
      if (!is.null(this_audit$duration)) audit_duration <- audit_duration + this_audit$duration
      this_audit$duration <- audit_duration
      output$modal_finalize_audit_review <- renderText(
        paste0(
          "<p>On ", format(Sys.Date(), "%B %d, %Y"), " you audited <b>", positions_checked, "</b> ",
          "aliquot positions in Freezer <b>", input$freezer, "</b>.</p>",
          "<ul>",
          "<li>A total of <b>", positions_verified, "</b> positions were verified.</li>",
          "<li>You identified <b>", positions_discrep, "</b> position discrepancies.</li>",
          ifelse(positions_discrep == 0,
                 "",
                 paste0("<ul>",
                        "<li>", positions_recorded, " were recorded for later resolution.</li>",
                        "<li>", positions_resolved, " were resolved during this audit.</li>",
                        "</ul>",
                        collapse = "")
          ),
          "<li>Audit accuracy was <b>", round(100*positions_verified/positions_checked, 1), "%</b>.</li>",
          "<li>This audit required <b>", round(audit_duration/60, 2), " minutes</b>.",
          "</ul>",
          collapse = ""
        )
      )
      if (nrow(this_audit$discrepancies) > 0) {
        shinyjs::show('download_discrep_check')
      } else {
        shinyjs::hide('download_discrep_check')
      }
      showModal(modal_finalize_audit())
    }
  })
  ### ___Finalize an audit ----------------------------------------------------------------------------
  observeEvent(input$modal_finalize_sign, {
    if (input$modal_finalize_audit_staff == '') {
      shinyjs::addClass('staff_label', 'label-red')
      shinyjs::alert('Please select a staff member to finalize this audit.')
    } else {
      shinyjs::removeClass('staff_label', 'label-red')
      shinyjs::removeClass('modal_finalize_sign', 'button-focus')
      this_user <- staff$usernames[which(staff$names == input$modal_finalize_audit_staff)]
      this_n <- nrow(this_audit$positions)
      this_c <- nrow(this_audit$positions %>% filter(Status == "Verified"))
      # Reload all sources to be updated
      if (demo_mode) {
        audits        <- read_rds(paste(dir_pack_demo, "demo_audits.RDS", sep = dir_sep))
        summaries     <- read_rds(paste(dir_pack_demo, "demo_audit_history_summary.RDS", sep = dir_sep))
        discrepancies <- read_rds(paste(dir_pack_demo, "demo_discrepancies.RDS", sep = dir_sep))
      } else {
        audits        <- read_rds(paste(dir_user, "audits.RDS", sep = dir_sep))
        summaries     <- read_rds(paste(dir_user, "audit_history_summary.RDS", sep = dir_sep))
        discrepancies <- read_rds(paste(dir_user, "discrepancies.RDS", sep = dir_sep))
      }
      # Append Audit
      if (nrow(audits) == 0) {
        index_audit <- 1
      } else {
        index_audit <- max(audits$audit_index) + 1
      }
      discrepancy_count <- max(discrepancies$discrepancy_index)
      if (discrepancy_count == -Inf) discrepancy_count <- 1
      audits <- bind_rows(audits,
                          this_audit$positions %>%
                            select(one_of(names(audits))) %>%
                            mutate(audit_index = index_audit,
                                   discrepancy_index = discrepancy_index + discrepancy_count,
                                   User = this_user))
      # Append Audit Summary
      summaries[index_audit, ] <- list(as.integer(index_audit),
                                       Sys.Date(),
                                       as.character(this_user),
                                       as.integer(this_n),
                                       this_c / this_n,
                                       paste0(round(100 * this_c / this_n, 1), "%"),
                                       ifelse(input$n_or_fraction, "Fractional", "Absolute"),
                                       ifelse(input$get_some, "Standard", "Full"),
                                       as.numeric(input$nf_value),
                                       input$random,
                                       this_audit$freezer,
                                       as.numeric(this_audit$duration))
      # Append Discrepancies
      discrep_down$table <- this_audit$discrepancies %>%
        left_join(this_audit$positions, by = 'discrepancy_index') %>%
        select(-discrepancy_index, -Status, -GUALIQUOTID, -date_resolved)
      if (nrow(this_audit$discrepancies) > 0) {
        discrep_down$date <- as.character(Sys.Date())
        discrep_down$freezer <- this_audit$freezer
      }
      discrepancies <- bind_rows(discrepancies,
                                 this_audit$discrepancies %>%
                                   select(one_of(names(discrepancies))) %>%
                                   mutate(discrepancy_index = discrepancy_index + discrepancy_count))
      # Flush current
      this_audit$positions     <- this_audit$positions %>% slice(0)
      this_audit$discrepancies <- this_audit$discrepancies %>% slice(0)
      this_audit$time_start    <- NULL
      this_audit$time_end      <- NULL
      this_audit$duration      <- NULL
      this_audit$current_GUAID <- NULL
      this_audit$freezer       <- NULL
      # Remove old audit if used
      if (is_old_audit()) {
        file.remove(paste0(dir_pause, input$paused_audits, ".RDS"))
        outstanding_audits(length(list.files(dir_pause)))
        is_old_audit(FALSE)
      }
      # Update progress tracker
      if (demo_mode) {
        progress <- read_rds(paste(dir_pack_demo, "demo_progress.RDS", sep = dir_sep))
      } else {
        progress <- read_rds(paste(dir_user, "progress.RDS", sep = dir_sep))
      }
      last_year_scans <- audit_history_summary %>%
        filter(freezer != "Multiple Freezers",
               date_scanned > Sys.Date() - 365) %>%
        pull(freezer) %>%
        as.character()
      progress[-which(progress$category %in% last_year_scans), 1] <- 0
      frz <- summaries$freezer[index_audit]
      frz_i <- which(progress$category == frz)
      if (summaries$type[index_audit] == "Fractional") {
        progress$percent[frz_i] <- progress$percent[frz_i] + summaries$magnitude[index_audit] / 100
      } else {
        n <- get_total_positions(frz)
        progress$percent[frz_i] <- progress$percent[frz_i] + (summaries$magnitude[index_audit] / n)
      }
      progress[1, 1] <- nrow(progress[-1, ] %>% filter(percent >= 0.1)) / nrow(progress[-1, ])
      # Push up to global
      audits <<- audits
      audit_history_summary <<- summaries
      discrepancies <<- discrepancies
      progress <<- progress
      # Update reactives
      audit_list(audits)
      discrep(make_discrep())
      progress_total(progress)
      # Write back to all sources
      if (demo_mode) {
        write_rds(audits,
                  paste(dir_pack_demo, "demo_audits.RDS", sep = dir_sep))
        write_rds(audit_history_summary,
                  paste(dir_pack_demo, "demo_audit_history_summary.RDS", sep = dir_sep))
        write_rds(discrepancies,
                  paste(dir_pack_demo, "demo_discrepancies.RDS", sep = dir_sep))
        write_rds(progress,
                  paste(dir_pack_demo, "demo_progress.RDS", sep = dir_sep))
      } else {
        write_rds(audits,
                  paste(dir_user, "audits.RDS", sep = dir_sep))
        write_rds(audit_history_summary,
                  paste(dir_user, "audit_history_summary.RDS", sep = dir_sep))
        write_rds(discrepancies,
                  paste(dir_user, "discrepancies.RDS", sep = dir_sep))
        write_rds(progress,
                  paste(dir_user, "progress.RDS", sep = dir_sep))
      }
      # Go back to the dashboard to see all updates
      updateTabItems(session, inputId = 'left_menu', selected = 'status')
      removeModal()
      # Trigger download if necessary
      if (nrow(discrep_down$table) > 0 & input$download_discrep_check) {
        showModal(modal_download_discrepancies())
      }
    }
  })
  observe({
    if (!is.null(discrep_down$table)) {
      more_cols <- input$modal_download_discrepancies_fields
      only_rslv <- input$modal_download_discrepancies_unresolved
      if (!is.null(only_rslv)) {
        if (only_rslv) {
          tmp_down <- discrep_down$table %>%
            filter(!Resolved) %>%
            left_join(get_GUAID_info(discrep_down$table[, 1],
                                     addl_columns = more_cols,
                                     include_defaults = FALSE),
                      by = c('GUAID' = 'GUALIQUOTID'))
        } else {
          tmp_down <- discrep_down$table %>%
            left_join(get_GUAID_info(discrep_down$table[, 1],
                                     addl_columns = more_cols,
                                     include_defaults = FALSE),
                      by = c('GUAID' = 'GUALIQUOTID'))
        }
      } else {
        tmp_down <- discrep_down$table
      }
      names(tmp_down) <- gsub("Guaid", "GUAID", simple_cap(names(tmp_down), sep = "_"))
      discrep_down_table(tmp_down)
    }
  })
  observeEvent(input$modal_finalize_audit_staff, {
    shinyjs::removeClass('staff_label', 'label-red')
    if (input$modal_finalize_audit_staff != '') shinyjs::addClass('modal_finalize_sign', 'button-focus')
  })
  observeEvent(input$modal_finalize_cancel, {
    removeModal()
  })
  # DISCREPANCIES TAB ---------------------------------------------------------------------------------
  ## _Discrepancies Rendered Outputs ------------------------------------------------------------------
  output$discrepancies_remaining <- renderDT(
    discrep_remaining(),
    # discrep_remaining() %>% select(discrepancy_index, Issue),
    rownames = FALSE,
    selection = list(mode = 'single', selected = NULL),
    # colnames = c("Index", "Issue"),
    options = list(columnDefs = list(list(className = 'dt-center', targets = c(0:1)),
                                     list(className = 'compact', targets = c(0:1))))
  )
  output$discrepancies_all <- renderDT(
    discrep_resolved() %>% select(-Resolved),
    rownames = FALSE,
    selection = list(mode = 'single', selected = NULL),
    colnames = c("Index", "Issue", "Action", "Scanned", "Resolved", "Lag"),
    options = list(autowidth = TRUE,
                   columnDefs = list(list(className = 'dt-center', targets = c(3:5)),
                                     list(className = 'compact', targets = c(0:5)),
                                     list(visible = FALSE, targets = 0)))
  )
  ## _Discrepancies Fill Modals -----------------------------------------------------------------------
  output$modal_discrepancy_resolution <- renderDT(
    discrep_resolution_DT() %>% slice(-c(1:2)),
    rownames = FALSE,
    colnames = c("", ""),
    selection = list(mode = 'none'),
    options = list(dom = 't',
                   bSort = FALSE,
                   columnDefs = list(list(className = 'dt-center', targets = 1),
                                     list(className = 'compact', targets = 0:1)))
  )
  ## _Discrepancies Observers -------------------------------------------------------------------------
  observe({
    if (nrow(discrep_remaining()) == 0) {
      output$discrepancies_remaining_status <- renderText("<h4 class = 'success' style = 'text-align:left;'>No action needed at this time.</h4>")
      shinyjs::hideElement('discrepancies_remaining')
      shinyjs::hideElement('discrepancies_download_outstanding')
    } else {
      output$discrepancies_remaining_status <- renderText("<h4>Click a discrepancy to resolve it.</h4>")
      shinyjs::showElement('discrepancies_remaining')
      shinyjs::showElement('discrepancies_download_outstanding')
    }
  })
  observeEvent(input$discrepancies_download_outstanding, {
    discrep_down$date <- as.character(Sys.time())
    discrep_down$freezer <- 'Outstanding Discrepancies'
    discrep_down$table <- discrep_remaining() %>%
      left_join(audits %>% select(-date_scanned), by = 'discrepancy_index') %>%
      rename("GUAID" = "GUALIQUOTID") %>%
      select(GUAID, User, Resolved, Issue, SECTION:POSITION3, date_scanned)
    showModal(modal_download_discrepancies())
  })
  observeEvent(input$discrepancies_remaining_rows_selected, {
    discrep_index_active(discrep_remaining()[input$discrepancies_remaining_rows_selected, 'discrepancy_index'])
    updateTextAreaInput(session, 'modal_discrepancy_issue', value = discrep_resolution_DT()[1, 2])
    updateTextAreaInput(session, 'modal_discrepancy_action', value = discrep_resolution_DT()[2, 2])
    showModal(modal_resolve_discrepancy())
  })
  observeEvent(input$modal_discrepancy_resolve, {
    i <- which(discrep()$discrepancy_index == discrep_index_active())
    t <- discrep()[i, ] %>%
      mutate(Resolved = TRUE,
             Issue = input$modal_discrepancy_issue,
             Action_Taken = input$modal_discrepancy_action,
             date_resolved = Sys.Date())
    i <- which(discrepancies$discrepancy_index == discrep_index_active())
    discrepancies[i, ] <<- t %>% select(-date_scanned, -lag)
    discrep(make_discrep())
    if (demo_mode) {
      write_rds(discrepancies, paste(dir_pack_demo, "demo_discrepancies.RDS", sep = dir_sep))
    } else {
      write_rds(discrepancies, paste(dir_user, "discrepancies.RDS", sep = dir_sep))
    }
    removeModal()
  })
}

# Modal dialogs
modal_last_audit <- function() {
  modalDialog(
    title = "Last Audit Results",
    htmlOutput('modal_last_audit_description'),
    htmlOutput('modal_discrep_description'),
    hr(),
    DTOutput('modal_last_audit_DT'),
    footer = tagList(modalButton("Close")),
    easyClose = TRUE,
    size = 'l'
  )
}

modal_schedule <- function(period) {
  modalDialog(
    title = "Edit Audit Schedule",
    sliderInput('modal_schedule_interval',
                label = 'Number of days between audits',
                min = 7, max = 56, value = period, step = 7, width = '100%'),
    HTML('<strong>Resulting schedule</strong>'),
    DTOutput('modal_schedule_DT'),
    footer = tagList(modalButton('Close')),
    easyClose = TRUE,
    size = 's'
  )
}

modal_discrep <- function() {
  modalDialog(
    title = NULL,
    htmlOutput('modal_discrep_description2', height = '300px'),
    footer = tagList(modalButton('Close')),
    easyClose = TRUE,
    size = 'l'
  )
}

modal_fullQA <- function(freezer) {
  modalDialog(
    title = NULL,
    h4(paste('This will trigger a full inventory audit for', freezer, '.')),
    footer = tagList(modalButton('OK')),
    easyClose = TRUE,
    size = 's'
  )
}

modal_paused_audits <- function() {
  paused <- sort(gsub(".RDS", "", list.files("data/paused_audits")))
  modalDialog(
    title = 'Audits currently paused.',
    selectInput(inputId = 'paused_audits',
                label = NULL,
                choices = paused,
                selected = NULL,
                multiple = FALSE,
                width = '100%'),
    footer = tagList(actionButton(inputId = 'load_paused_audit', label = "Load Audit"),
                     modalButton('Dismiss')),
    easyClose = TRUE,
    size = 's'
  )
}

# modal_manual_check <- function(GUAID) {
#   modalDialog(
#     title = paste('Identifying details for', GUAID, '.'),
#     DTOutput('modal_GUAID_details'),
#     footer = tagList(actionButton('record_verification', label = 'Verify'),
#                      actionButton('record_discrepancy', label = 'Discrepancy'),
#                      modalButton('Cancel')),
#     easyClose = FALSE,
#     size = 'm'
#   )
# }

modal_resolve_discrepancy <- function() {
  modalDialog(
    title = 'Discrepancy Resolution',
    fluidRow(
      column(7,
             textAreaInput(inputId = 'modal_discrepancy_issue', label = "Issue", width = '100%', height = '125px'),
             textAreaInput(inputId = 'modal_discrepancy_action', label = "Action Taken", width = '100%', height = '125px')
      ),
      column(5,
             DTOutput('modal_discrepancy_resolution')
      )
    ),
    footer = tagList(actionButton('modal_discrepancy_resolve', label = 'Mark as Resolved'),
                     modalButton('Cancel')),
    easyClose = FALSE,
    size = 'l'
  )
}

modal_temp_discrepancy_resolve <- function() {
  modalDialog(
    title = 'Discrepancy Resolution',
    fluidRow(
      column(7,
             textAreaInput(inputId = 'modal_discrepancy_issue', label = "Issue", width = '100%', height = '125px'),
             textAreaInput(inputId = 'modal_discrepancy_action', label = "Action Taken", width = '100%', height = '125px')
      ),
      column(5,
             DTOutput('modal_temp_discrepancy_resolve_DT')
      )
    ),
    footer = tagList(actionButton('modal_temp_discrepancy_resolve', label = 'Mark as Resolved'),
                     actionButton('modal_temp_discrepancy_cancel_resolve', label = 'Cancel')),
    easyClose = FALSE,
    size = 'l'
  )
}

modal_temp_discrepancy_record <- function() {
  modalDialog(
    title = 'Record a Discrepancy',
    fluidRow(
      column(7,
             textAreaInput(inputId = 'modal_discrepancy_record_issue', label = "Issue", width = '100%', height = '125px')
      ),
      column(5,
             DTOutput('modal_temp_discrepancy_record_DT')
      )
    ),
    footer = tagList(actionButton('modal_temp_discrepancy_record', label = 'Record'),
                     actionButton('modal_temp_discrepancy_cancel_record', label = 'Cancel')),
    easyClose = FALSE,
    size = 'l'
  )
}

modal_verify_success <- function() {
  modalDialog(
    title = NULL,
    div(style = 'text-align: center;',
        icon('check-circle', 'fa-5x success'),
        h3('Verified', class = 'success')
    ),
    footer = NULL,
    easyClose = TRUE,
    size = 's'
  )
}

modal_verify_alt_success <- function() {
  modalDialog(
    title = NULL,
    div(style = 'text-align: center;',
        icon('question', 'fa-5x success'),
        h3('Found in the same container. Please verify.')
        ),
    footer = tagList(actionButton('modal_alt_verify', label = 'Verify', width = '48%'),
                     actionButton('modal_alt_reject', label = 'Reject', width = '48%')),
    easyClose = FALSE,
    size = 's'
  )
}

modal_verify_failure <- function() {
  modalDialog(
    title = NULL,
    div(style = 'text-align: center;',
        icon('times-circle', 'fa-5x failure'),
        h3('Discrepancy', class = 'failure')
    ),
    htmlOutput('modal_failure_FW'),
    footer = tagList(actionButton('modal_failure_resolve', label = 'Resolve', width = '34%'),
                     actionButton('modal_failure_record', label = 'Record', width = '34%'),
                     modalButton('Cancel')),
    easyClose = FALSE,
    size = 's'
  )
}

modal_finalize_audit <- function() {
  modalDialog(
    title = 'Finalize This Audit',
    HTML('<label id="staff_label" class="control-label" for="modal_finalize_audit_staff">Staff Member</label>'),
    selectInput(inputId = 'modal_finalize_audit_staff',
                label = NULL,
                choices = c(as.character(staff$names), ''),
                selected = ''
    ),
    htmlOutput('modal_finalize_audit_review'),
    checkboxInput(inputId = 'download_discrep_check', label = 'Download discrepancy list', value = TRUE),
    footer = tagList(actionButton(inputId = 'modal_finalize_sign',
                                  label = 'Finalize',
                                  width = '49.5%'),
                     actionButton(inputId = 'modal_finalize_cancel',
                                  label = 'Cancel',
                                  width = '49.5%')),
    easyClose = FALSE,
    size = 'm'
  )
}

modal_download_discrepancies <- function() {
  modalDialog(
    title = 'Download Discrepancies',
    selectizeInput(inputId = 'modal_download_discrepancies_fields',
                   label = 'Select any additional metadata fields',
                   choices = fields,
                   selected = NA,
                   multiple = TRUE,
                   width = '100%'),
    checkboxInput(inputId = 'modal_download_discrepancies_unresolved',
                  label = 'Include only unresolved issues',
                  value = FALSE),
    HTML('<label for = "modal_download_discrepancies_preview">Preview...</label>'),
    DTOutput(outputId = 'modal_download_discrepancies_preview'),
    footer = tagList(downloadButton(outputId = 'download_discrepancies',
                                    label = 'Save Discrepancies'),
                     modalButton(label = 'Close',
                                 icon = icon('close'))),
    easyClose = FALSE,
    size = 'l'
  )
}

modal_progress <- function() {
  modalDialog(
    title = paste('Audit Progress for', year(Sys.Date())),
    h5("Double click the progress value (%) to audit that freezer."),
    column(12, align = "center",
           plotOutput(outputId = 'progress_plot',
                      dblclick = 'progress_dblclick',
                      height = min(26, nrow(progress)-1) * 25,
                      width = ceiling((nrow(progress)-1) / 26) * 175)
    ),
    footer = tagList(modalButton("Close")),
    easyClose = TRUE,
    size = 'm'
  )
}

#' Marker Call Rate UI Function
#'
#' @description A Shiny module that summarizes and plots KASP marker call rates
#'   using \code{kasp_call_rate()}.
#'
#' @param id Internal ID parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList sidebarLayout sidebarPanel mainPanel selectizeInput textInput numericInput plotOutput downloadButton fluidRow column selectInput helpText
#' @importFrom bslib navset_card_tab nav_panel card_footer
#'
mod_mv_call_rate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          inputId = ns("snp_id"),
          label = "SNP ID Column",
          choices = NULL,
          multiple = FALSE
        ),
        selectizeInput(
          inputId = ns("plate_id"),
          label = "Master Plate ID Column",
          choices = NULL,
          multiple = FALSE
        ),
        selectizeInput(
          inputId = ns("geno_call_id"),
          label = "Genotype Call Column",
          choices = NULL,
          multiple = FALSE
        ),
        numericInput(
          inputId = ns("threshold"),
          label = "Call Rate Threshold (%)",
          value = 95, min = 0, max = 100, step = 1
        ),
        textInput(
          inputId = ns("sep"),
          label = "Allele Separator",
          value = ":"
        ),
        textInput(
          inputId = ns("no_call"),
          label = "No Call Label",
          value = "?"
        ),
        textInput(
          inputId = ns("uncallable"),
          label = "Uncallable Label",
          value = "Uncallable"
        ),
        textInput(
          inputId = ns("blank"),
          label = "NTC / Blank Label",
          value = "NTC"
        ),
        helpText(tags$em("NTC wells are excluded from call rates."))
      ),
      mainPanel(
        bslib::navset_card_tab(
          id = ns("call_rate_tabs"),
          bslib::nav_panel(
            title = "Marker Call Rate",
            value = "marker_call_rate",
            plotOutput(ns("marker_plot"), height = "450px")
          ),
          bslib::nav_panel(
            title = "No-call Reasons",
            value = "no_call_reason",
            plotOutput(ns("no_call_plot"), height = "450px")
          ),
          bslib::nav_panel(
            title = "Per Plate",
            value = "plate_call_rate",
            plotOutput(ns("plate_plot"), height = "450px")
          ),
          bslib::nav_panel(
            title = "Summary Table",
            value = "table",
            selectInput(
              inputId = ns("table_level"),
              label = NULL,
              choices = c("By marker" = "marker_summary",
                          "By marker and plate" = "plate_summary")
            ),
            DT::DTOutput(ns("call_rate_table"))
          )
        ),
        bslib::card_footer(
          fluidRow(
            column(width = 3, textInput(ns("file_name"), "Filename", value = "call_rate")),
            column(width = 3, selectInput(ns("file_type"), "Format", choices = c("pdf", "png"))),
            column(width = 2, numericInput(ns("width"), "Width (in)", value = 8, min = 2, max = 30)),
            column(width = 2, numericInput(ns("height"), "Height (in)", value = 5, min = 2, max = 30)),
            column(
              width = 2,
              tags$div(
                style = "margin-top: 25px;",
                downloadButton(ns("download_plot"), "Download Plot", class = "btn-success")
              )
            )
          )
        )
      )
    )
  )
}


#' Marker Call Rate Server Function
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param kasp_data A data frame of KASP genotype calls.
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive observe req updateSelectizeInput renderPlot downloadHandler
#'
mod_mv_call_rate_server <- function(id, kasp_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    col_names <- reactive({
      req(kasp_data)
      colnames(kasp_data)
    })

    observe({
      req(col_names())
      updateSelectizeInput(session, inputId = "snp_id", server = TRUE,
                           choices = col_names(),
                           selected = safe_grep_match(pattern = "snp", choices = col_names()))
      updateSelectizeInput(session, inputId = "plate_id", server = TRUE,
                           choices = col_names(),
                           selected = safe_grep_match(pattern = "masterplate", choices = col_names()))
      updateSelectizeInput(session, inputId = "geno_call_id", server = TRUE,
                           choices = col_names(),
                           selected = safe_grep_match(pattern = "call", choices = col_names()))
    })

    call_rate_res <- reactive({
      req(kasp_data, input$snp_id, input$plate_id, input$geno_call_id,
          input$sep, input$threshold)
      req(all(c(input$snp_id, input$plate_id, input$geno_call_id) %in% col_names()))

      tryCatch(
        kasp_call_rate(
          x = kasp_data,
          snp_id = input$snp_id,
          plate_id = input$plate_id,
          geno_call = input$geno_call_id,
          sep = input$sep,
          blank = input$blank,
          no_call = input$no_call,
          uncallable = input$uncallable,
          threshold = input$threshold / 100
        ),
        error = function(e) {
          shinyWidgets::show_alert(
            title = "Error",
            text = paste("Error computing call rates:", e$message),
            type = "error",
            showCloseButton = TRUE
          )
          NULL
        }
      )
    })

    output$marker_plot <- renderPlot({
      req(call_rate_res())
      call_rate_res()$marker_call_rate
    }, res = 110)

    output$no_call_plot <- renderPlot({
      req(call_rate_res())
      call_rate_res()$no_call_reason
    }, res = 110)

    output$plate_plot <- renderPlot({
      req(call_rate_res())
      call_rate_res()$plate_call_rate
    }, res = 110)

    output$call_rate_table <- DT::renderDT({
      req(call_rate_res(), input$table_level)
      df <- call_rate_res()[[input$table_level]]
      req(df)
      num_cols <- intersect(c("call_rate", "ci_lower", "ci_upper"), colnames(df))
      df[num_cols] <- lapply(df[num_cols], function(v) round(100 * v, 1))
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    # Download whichever plot tab is open (the marker plot when the table is shown)
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0(input$file_name, ".", input$file_type)
      },
      content = function(file) {
        req(call_rate_res())
        tab <- input$call_rate_tabs
        if (is.null(tab) || tab == "table") tab <- "marker_call_rate"
        ggplot2::ggsave(file,
                        plot = call_rate_res()[[tab]],
                        width = input$width,
                        height = input$height,
                        device = input$file_type)
      }
    )
  })
}

## To be copied in the UI
# mod_mv_call_rate_ui("mv_call_rate_1")

## To be copied in the server
# mod_mv_call_rate_server("mv_call_rate_1", kasp_data = kasp_data)

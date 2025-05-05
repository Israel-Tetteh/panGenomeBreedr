source("Helper functions/utils_Marker_Validation.R") # source
source("modules/Marker_validation ui_helper functions.R")

proc_kaspUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = ns("plate_name"), choices = NULL,
          label = "Select Plate", multiple = FALSE
        ),
        selectInput(
          inputId = ns("sample_name"), choices = NULL,
          label = "Select Column for Sample ID", multiple = FALSE
        ),
        numericInput(
          inputId = ns("marker_start_id"), label = "Indicate Marker Start",
          value = 2, max = 100, min = 1, step = 1
        ),
        bslib::input_switch(
          id = ns("kasp_map_q"),
          value = TRUE,
          label = " Automatically Generate KASP Map"
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("kasp_map_q")),
          fileInput(
            inputId = ns("kasp_map_file"),
            label = "Upload Kasp Map File",
            accept = c(".csv", ".xlsx"),
            placeholder = ".csv / .xlsx",
            multiple = FALSE
          ),
          selectInput(
            inputId = ns("snpid_name"), choices = NULL,
            label = "Select Column for SNP ID", multiple = FALSE
          ),
          selectInput(
            inputId = ns("chr_name"), choices = NULL,
            label = "Select Column for Chromosomes", multiple = FALSE
          ),
          selectInput(
            inputId = ns("chr_pos_name"), choices = NULL,
            label = "Select Column for Chromosome Positions", multiple = FALSE
          )
        ),
        div(
          style = "display: flex; justify-content: center;",
          actionButton(
            inputId = ns("run"), label = "Submit",
            icon = icon("rocket"), class = "btn-primary", width = "50%"
          )
        )
      ),
      mainPanel(
        bslib::accordion(
          bslib::accordion_panel(
            title = "Generated KASP Map",
            DT::dataTableOutput(outputId = ns("g_Kasp_map"))
          ), br(),
          bslib::accordion_panel(
            title = "Re-ordered SNPs by Chromosome and Position",
            DT::dataTableOutput(outputId = ns("reodered_Kasp_data"))
          ), br()
        )
      )
    )
  )
}


proc_kaspServer <- function(id, reshape_result, kasp_dat) {
  moduleServer(
    id,
    function(input, output, session) {
      # Get list object names
      reshape_result_names <- reactive({
        req(reshape_result)
        names(reshape_result)
      })

      # Update the select input for plates
      observeEvent(reshape_result_names(), {
        updateSelectInput(session, inputId = "plate_name", choices = reshape_result_names())
      })

      # Get user selected plates_name and subset data
      selected_table_names <- reactive({
        req(input$plate_name)
        names(kasp_dat)
      })

      # Update select input
      observeEvent(selected_table_names(), {
        updateSelectInput(session, inputId = "sample_name", choices = selected_table_names(), selected = 'SubjectID')
      })

      # Define a single reactive for the map - behavior changes based on input$kasp_map_q
      generated_auto_map <- reactive({
        if (isTRUE(input$kasp_map_q)) {
          # Auto-generate map
          req(kasp_dat)
          kasp_marker_map(kasp_data = kasp_dat)
        } else {
          # User uploads map
          req(input$kasp_map_file)
          file_path <- input$kasp_map_file$datapath
          file_ext <- tools::file_ext(input$kasp_map_file$name)

          if (tolower(file_ext) == "csv") {
            read.csv(file_path, stringsAsFactors = FALSE)
          } else if (tolower(file_ext) %in% c("xls", "xlsx")) {
            readxl::read_excel(file_path)
          } else {
            stop("Unsupported file type. Please upload a CSV or Excel file.")
          }
        }
      })

      # Store processed result
      proc_kasp_result <- reactiveVal(NULL)

      # Field names for select inputs
      map_column_names <- reactive({
        req(generated_auto_map())
        colnames(generated_auto_map())
      })

      # Update select inputs when manual map is uploaded (only when kasp_map_q is FALSE)
      observeEvent(map_column_names(), {
        # Only update these select inputs when in manual map mode
        if (!isTRUE(input$kasp_map_q)) {
          updateSelectInput(session, inputId = "snpid_name", choices = map_column_names())
          updateSelectInput(session, inputId = "chr_name", choices = map_column_names())
          updateSelectInput(session, inputId = "chr_pos_name", choices = map_column_names())
        }
      })

      # Process data when run button is clicked
      observeEvent(input$run, {
        if (isTRUE(input$kasp_map_q)) {
          # Auto-generate map workflow
          req(reshape_result, input$sample_name, input$marker_start_id, generated_auto_map())

          proc_kasp_result(
            proc_kasp(
              x = reshape_result[[input$plate_name]],
              sample_id = input$sample_name,
              marker_start = input$marker_start_id,
              kasp_map = generated_auto_map(),
              map_snp_id = colnames(generated_auto_map())[1]
            )
          )
        } else {
          # User-provided map workflow
          req(reshape_result, input$sample_name, input$marker_start_id,
              generated_auto_map(), input$chr_pos_name, input$chr_name, input$snpid_name)

          proc_kasp_result(
            proc_kasp(
              x = reshape_result[[input$plate_name]],
              sample_id = input$sample_name,
              marker_start = input$marker_start_id,
              kasp_map = generated_auto_map(),
              map_snp_id = input$snpid_name,
              chr = input$chr_name,
              chr_pos = input$chr_pos_name
            )
          )
        }
      })

      # Output tables
      output$g_Kasp_map <- DT::renderDT({
        req(generated_auto_map())
        DT::datatable(generated_auto_map(), options = list(scrollX = TRUE))
      })

      output$reodered_Kasp_data <- DT::renderDT({
        req(proc_kasp_result())
        DT::datatable(proc_kasp_result(), options = list(scrollX = TRUE))
      })
    }
  )
}

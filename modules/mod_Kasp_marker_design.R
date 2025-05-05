#' Kasp_marker_design UI Function
# Source for functions used in module.
source("Helper functions/utils_Kasp_Marker_Design.R")
source("modules/kasp_marker_design ui_helper_functions.R")

library(promises)
#' mod_Kasp_marker_design_ui <- function(id) {
#'   ns <- NS(id)
#'   tagList(
#'     # A spinner to display while work is carried at the background.
#'     shinybusy::add_busy_spinner(spin = "fading-circle", color = "white", timeout = 100),
#'
#'     sidebarLayout(
#'       sidebarPanel(
#'         fileInput(ns("genome_file"), label = "Upload Genome File", accept = c(".fa", ".fasta", ".gz")),
#'         hr(),
#'         fileInput(ns("vcf_file"), label = "Upload VCF File", accept = ".vcf"),
#'         selectInput(ns("chr_ID"), label = "Chromosome ID", choices = NULL ,multiple = FALSE),
#'         selectInput(ns("marker_ID"), label = "Marker ID", choices = NULL, multiple = FALSE),
#'         textInput(ns("reg_name"), label = "Region Name (User Defined)", placeholder = "e.g., drought resistance locus"),
#'         numericInput(ns("maf"), label = "Minor Allele Frequency (MAF)", value = 0.05, min = 0, max = 1),
#'         bslib::input_switch(ns("draw_plot"), label = "Generate Alignment Plot", value = TRUE),
#'
#'         div(style = "display: flex; justify-content: center;",
#'             actionButton(ns("run_but"), label = "Design Marker",
#'                          icon = icon("drafting-compass"), class = "btn-primary", width = "70%"))
#'       ),
#'
#'       mainPanel(
#'         bslib::accordion(width = "100%",
#'                          open = TRUE,
#'                          bslib::accordion_panel("Comprehensive Table of KASP Marker Design Data and DNA Sequence Alignment to the Reference Genome",
#'                                                 DT::dataTableOutput(ns("kasp_table")),br(),hr(),
#'                                                 fluidRow(
#'                                                   column(width = 3, selectInput(inputId = ns('exten') ,
#'                                                                                 label = 'Download file as?',
#'                                                                                 choices = c('.csv','.xlsx'),
#'                                                                                 selected = '.csv',
#'                                                                                 multiple = FALSE)
#'                                                          ),
#'                                                   column(width = 4 ,
#'                                                          textInput(inputId = ns('file_name'),
#'                                                                    label = 'Enter File Name',
#'                                                                     value = 'Kasp M_D for Intertek')
#'                                                          )
#'                                                   ),
#'                                                        downloadButton(ns("download_table"),
#'                                                                       label = "Download File",
#'                                                                       class = "btn-success",
#'                                                                       icon = icon('download')
#'                                                        )
#'
#'
#'                                                )
#'         )
#'         ,br(),
#'
#'         conditionalPanel(
#'           condition = "input['Kasp_marker_design-draw_plot'] === true",
#'           bslib::accordion( width = "100%",
#'                             open = TRUE,
#'                             bslib::accordion_panel("Plot displaying the 100 bp upstream and downstream KASP sequence alignment with the reference genome",
#'                                                    addSpinner(plotOutput(ns("plot")), spin = "circle", color = "#112446"),
#'                                                    downloadButton(ns("download_plot"), label = "Download Plot (pdf)", class = "btn-success", icon = icon("download")))
#'           )
#'         )
#'       )
#'     )
#'
#'   )
#' }
#'
#' #' Kasp_marker_design Server Functions
#' mod_Kasp_marker_design_server <- function(id){
#'   moduleServer(id, function(input, output, session){
#'     ns <- session$ns
#'
#'     # Read VCF data
#'     vcf_data <- reactive({
#'       req(input$vcf_file)
#'       marker.chr_ID(input$vcf_file$datapath)
#'     })
#'
#'     # Populate dropdowns with VCF data
#'     observeEvent(vcf_data(), {
#'       updateSelectInput(session, inputId = "marker_ID",
#'                         choices = vcf_data()$vcf_matrix_markerID, selected = NULL)
#'       updateSelectInput(session, inputId = "chr_ID",
#'                         choices = vcf_data()$vcf_matrix_chromID, selected = NULL)
#'     })
#'
#'     # Extract genotype codes
#'     geno_code <- reactive({
#'       req(input$vcf_file)
#'       extract_geno_codes(input$vcf_file$datapath)
#'     })
#'
#'     # Reactive value for KASP design results
#'     kasp_des.result <- reactiveVal(NULL)
#'
#'     # Run KASP marker design function
#'     observeEvent(input$run_but, {
#'       # on submit show spinner
#'       shinybusy::show_spinner()
#'
#' req(input$vcf_file,input$chr_ID,input$marker_ID,input$maf, input$genome_file)
#'
#'       kasp_des.result(Kasp_marker_design(
#'         vcf_file = input$vcf_file$datapath,
#'         marker_ID = input$marker_ID,
#'         chr = input$chr_ID,
#'         genome_file = input$genome_file$datapath,
#'         #plot_draw = isTRUE(input$draw_plot) ,
#'         plot_file = tempdir(),
#'         region_name = input$reg_name,
#'         vcf_geno_code = geno_code(),
#'         maf = input$maf
#'       ))
#'
#'       # Hide spinner when completed.
#'       shinybusy::hide_spinner()
#'
#'     })
#'
#'     # Show an alert.
#'     observe({
#'       if (!is.null(kasp_des.result())) {
#'         show_alert(
#'           title = 'Status',
#'           text = 'Marker design completed successfully!',
#'           type = 'success',
#'           showCloseButton = TRUE
#'         )
#'       }
#'     })
#'
#'    # RAssistant::R_assistant("correct me if I am wrong.  observe( kasp_des.result(),{
#'    #   show_alert(title = 'KASP Marker Design',
#'    #              text = 'Successfull',
#'    #              type = 'success',showCloseButton = TRUE)
#'    # })")
#'
#'     # Render KASP table
#'     output$kasp_table <- DT::renderDT({
#'       req(kasp_des.result())
#'       if (is.list(kasp_des.result())) {
#'         DT::datatable(kasp_des.result()$marker_data , options = list(pageLength = 10, scrollX = TRUE))
#'       } else {
#'
#'         DT::datatable(kasp_des.result(), options = list(pageLength = 10, scrollX = TRUE))
#'       }
#'     })
#'
#'
#'     # Render Plot
#'     output$plot <- renderPlot({
#'       req(kasp_des.result())
#'       if (is.list(kasp_des.result())) {
#'         kasp_des.result()$pp
#'       }
#'     })
#'
#'     # Download Table
#'     output$download_table <- downloadHandler(
#'       filename = function() {
#'         paste0(input$file_name, input$exten)
#'       },
#'       content = function(file) {
#'         req(kasp_des.result())
#'
#'         if (input$exten == ".csv") {
#'           # Save as CSV
#'           if (is.list(kasp_des.result())) {
#'             write.csv(kasp_des.result()$marker_data, file, row.names = FALSE)
#'           } else {
#'             write.csv(kasp_des.result(), file, row.names = FALSE)
#'           }
#'         } else if (input$exten == ".xlsx") {
#'           # Save as Excel file
#'           if (is.list(kasp_des.result())) {
#'             openxlsx::write.xlsx(kasp_des.result()$marker_data, file)
#'           } else {
#'             openxlsx::write.xlsx(kasp_des.result(), file)
#'           }
#'         }
#'       }
#'     )
#'
#'
#'
#'     # Download Plot
#'     output$download_plot <- downloadHandler(
#'       filename = function() { paste0("alignment_", input$marker_ID, ".pdf") },
#'       content = function(file) {
#'         req(kasp_des.result(), input$draw_plot)
#'         if (is.list(kasp_des.result())) {
#'           ggplot2::ggsave(
#'             filename = file,
#'             plot = kasp_des.result()$pp,
#'             device = "pdf",
#'             width = 24,
#'             height = 9,
#'             units = "in"
#'           )
#'         }
#'       }
#'     )
#'
#'   })
#' }

## To be copied in the UI
# mod_Kasp_marker_design_ui("Kasp_marker_design_1")

## To be copied in the server
# mod_Kasp_marker_design_server("Kasp_marker_design_1")

# ================ From claude, code enhancement and robustness.
Kasp_marker_design_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Include shinyjs for dynamic UI control
    shinyjs::useShinyjs(),

    # A spinner to display while work is carried at the background.
    shinybusy::add_busy_spinner(spin = "fading-circle", color = "white", timeout = 100),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("genome_file"), label = "Upload Genome File", accept = c(".fa", ".fasta", ".gz"),placeholder = c(".fa", " .fasta", " .gz") ),
        hr(),
        fileInput(ns("vcf_file"), label = "Upload VCF File", accept = ".vcf",placeholder = '.vcf'),
        selectInput(ns("chr_ID"), label = "Chromosome ID", choices = NULL, multiple = FALSE),
        selectInput(ns("marker_ID"), label = "Marker ID", choices = NULL, multiple = FALSE),
        textInput(ns("reg_name"), label = "Region Name (User Defined)", placeholder = "e.g., drought resistance locus"),
        numericInput(ns("maf"), label = "Minor Allele Frequency (MAF)", value = 0.05, min = 0, max = 1),
        bslib::input_switch(ns("draw_plot"), label = "Generate Alignment Plot", value = TRUE),
        div(
          style = "display: flex; justify-content: center;",
          actionButton(ns("run_but"),
            label = "Design Marker",
            icon = icon("drafting-compass"), class = "btn-primary", width = "70%"
          )
        )
      ),
      mainPanel(
        # Error messages panel
        uiOutput(ns("error_messages")),

        # Progress bar (hidden by default)
        div(
          id = ns("progress_container"), style = "display: none;",
          h4("Designing KASP marker, this may take some time..."),
          shinyWidgets::progressBar(ns("design_progress"),
            value = 0, total = 100,
            title = "", display_pct = TRUE
          )
        ),

        # Results container (hidden by default)
        div(
          id = ns("results_container"), style = "display: none;",
          bslib::accordion(
            id = ns("results_accordion"),
            width = "100%",
            open = TRUE,
            bslib::accordion_panel(
              "Comprehensive Table of KASP Marker Design Data and DNA Sequence Alignment to the Reference Genome",
              DT::dataTableOutput(ns("kasp_table")), br(), hr(),
              fluidRow(
                column(width = 3, selectInput(
                  inputId = ns("exten"),
                  label = "Download file as?",
                  choices = c(".csv", ".xlsx"),
                  selected = ".csv",
                  multiple = FALSE
                )),
                column(
                  width = 4,
                  textInput(
                    inputId = ns("file_name"),
                    label = "Enter File Name",
                    value = "Kasp M_D for Intertek"
                  )
                )
              ),
              downloadButton(ns("download_table"),
                label = "Download File",
                class = "btn-success",
                icon = icon("download")
              )
            )
          ),
          br(),

          # Conditional plot panel
          uiOutput(ns("plot_container"))
        )
      )
    )
  )
}

#' Kasp_marker_design Server Functions
Kasp_marker_design_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for error states
    error_state <- reactiveVal(FALSE)
    error_message <- reactiveVal("")

    # Reactive value for processing state
    processing <- reactiveVal(FALSE)

    # Reactive value for KASP design results
    kasp_des.result <- reactiveVal(NULL)

    # Render error message UI
    output$error_messages <- renderUI({
      if (error_state()) {
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          tags$strong("Error: "),
          error_message()
        )
      }
    })

    # Render plot container UI conditionally
    output$plot_container <- renderUI({
      req(input$draw_plot, kasp_des.result())
      result <- kasp_des.result()

      if (is.list(result) && "pp" %in% names(result)) {
        bslib::accordion(
          width = "100%",
          open = TRUE,
          bslib::accordion_panel(
            "Plot displaying the 100 bp upstream and downstream KASP sequence alignment with the reference genome",
            plotOutput(ns("plot")),
            downloadButton(ns("download_plot"),
              label = "Download Plot (pdf)",
              class = "btn-success", icon = icon("download")
            )
          )
        )
      }
    })

    # Read VCF data with error handling
    vcf_data <- reactive({
      if (is.null(input$vcf_file)) {
        return(NULL)
      }

      tryCatch(
        {
          result <- marker.chr_ID(input$vcf_file$datapath)
          error_state(FALSE)
          return(result)
        },
        error = function(e) {
          error_state(TRUE)
          error_message(paste("Failed to read VCF file:", e$message))
          show_alert(
            title = "Error",
            text = paste("Failed to read VCF file:", e$message),
            type = "error",
            showCloseButton = TRUE
          )
          return(NULL)
        }
      )
    })

    # Populate dropdowns with VCF data
    observeEvent(vcf_data(), {
      if (!is.null(vcf_data())) {
        # Check if required elements exist
        if (!all(c("vcf_matrix_markerID", "vcf_matrix_chromID") %in% names(vcf_data()))) {
          error_state(TRUE)
          error_message("The VCF file does not contain expected marker or chromosome data")
          return(NULL)
        }

        # Check if there's at least one marker ID
        if (length(vcf_data()$vcf_matrix_markerID) == 0) {
          error_state(TRUE)
          error_message("No marker IDs found in the VCF file")
          return(NULL)
        }

        updateSelectInput(session,
          inputId = "marker_ID",
          choices = vcf_data()$vcf_matrix_markerID, selected = NULL
        )
        updateSelectInput(session,
          inputId = "chr_ID",
          choices = vcf_data()$vcf_matrix_chromID, selected = NULL
        )
      }
    })

    # Extract genotype codes with error handling
    geno_code <- reactive({
      if (is.null(input$vcf_file)) {
        return(NULL)
      }

      tryCatch(
        {
          result <- extract_geno_codes(input$vcf_file$datapath)
          return(result)
        },
        error = function(e) {
          error_state(TRUE)
          error_message(paste("Failed to extract genotype codes:", e$message))
          show_alert(
            title = "Error",
            text = paste("Failed to extract genotype codes:", e$message),
            type = "error",
            showCloseButton = TRUE
          )
          return(NULL)
        }
      )
    })

    # Validate inputs before enabling the run button
    observe({
      is_valid <- !is.null(input$vcf_file) &&
        !is.null(input$genome_file) &&
        !is.null(input$chr_ID) &&
        !is.null(input$marker_ID) &&
        !is.na(input$maf) &&
        input$maf >= 0 &&
        input$maf <= 1

      shinyjs::toggleState("run_but", condition = is_valid)
    })

    # Function to update progress bar
    update_progress <- function(value, message = NULL) {
      updateProgressBar(
        session = session,
        id = "design_progress",
        value = value,
        title = if (!is.null(message)) message else NULL
      )
    }

    # Run KASP marker design function
    observeEvent(input$run_but, {
      # Clear previous errors and results
      error_state(FALSE)
      error_message("")
      kasp_des.result(NULL)

      # Hide results container and show progress container
      shinyjs::hide("results_container")
      shinyjs::show("progress_container")
      processing(TRUE)

      # Reset progress bar
      update_progress(0, "Initializing...")

      # Validate required inputs
      if (is.null(input$vcf_file)) {
        error_state(TRUE)
        error_message("Please upload a VCF file")
        show_alert(title = "Error", text = "Please upload a VCF file", type = "error", showCloseButton = TRUE)
        shinyjs::hide("progress_container")
        processing(FALSE)
        return(NULL)
      }

      if (is.null(input$genome_file)) {
        error_state(TRUE)
        error_message("Please upload a genome file")
        show_alert(title = "Error", text = "Please upload a genome file", type = "error", showCloseButton = TRUE)
        shinyjs::hide("progress_container")
        processing(FALSE)
        return(NULL)
      }

      if (is.null(input$chr_ID) || input$chr_ID == "") {
        error_state(TRUE)
        error_message("Please select a chromosome ID")
        show_alert(title = "Error", text = "Please select a chromosome ID", type = "error", showCloseButton = TRUE)
        shinyjs::hide("progress_container")
        processing(FALSE)
        return(NULL)
      }

      if (is.null(input$marker_ID) || input$marker_ID == "") {
        error_state(TRUE)
        error_message("Please select a marker ID")
        show_alert(title = "Error", text = "Please select a marker ID", type = "error", showCloseButton = TRUE)
        shinyjs::hide("progress_container")
        processing(FALSE)
        return(NULL)
      }

      if (is.na(input$maf) || input$maf < 0 || input$maf > 1) {
        error_state(TRUE)
        error_message("MAF must be a value between 0 and 1")
        show_alert(title = "Error", text = "MAF must be a value between 0 and 1", type = "error", showCloseButton = TRUE)
        shinyjs::hide("progress_container")
        processing(FALSE)
        return(NULL)
      }

      # Create a separate R session to run the long computation
      # and update progress in the main session
      future::future({
        # Update progress at key points
        update_progress(10, "Reading VCF file...")
        Sys.sleep(0.5) # Simulate step completion

        update_progress(25, "Loading genome data...")
        Sys.sleep(0.5) # Simulate step completion

        update_progress(40, "Processing marker information...")
        Sys.sleep(0.5) # Simulate step completion

        update_progress(60, "Designing KASP markers...")
        Sys.sleep(0.5) # Simulate step completion

        update_progress(75, "Generating sequences...")
        Sys.sleep(0.5) # Simulate step completion

        if (input$draw_plot) {
          update_progress(85, "Creating alignment plot...")
          Sys.sleep(0.5) # Simulate step completion
        }

        update_progress(95, "Finalizing results...")

        # Use tryCatch to handle errors in the main function
        tryCatch(
          {
            result <- Kasp_marker_design(
              vcf_file = input$vcf_file$datapath,
              marker_ID = input$marker_ID,
              chr = input$chr_ID,
              genome_file = input$genome_file$datapath,
              plot_file = tempdir(),
              region_name = input$reg_name,
              vcf_geno_code = geno_code(),
              maf = input$maf
            )

            update_progress(100, "Complete!")

            # Return result
            return(list(success = TRUE, result = result))
          },
          error = function(e) {
            return(list(success = FALSE, message = e$message))
          }
        )
      }) %...>% (function(future_result) {
        # Process the results from the future
        if (future_result$success) {
          result <- future_result$result

          # Check if result is empty or NULL
          if (is.null(result) || (is.list(result) && length(result) == 0) ||
            (is.data.frame(result) && nrow(result) == 0)) {
            error_state(TRUE)
            error_message("No valid marker design data was generated. Check your inputs and try again.")
            show_alert(
              title = "Warning",
              text = "No valid marker design data was generated. Check your inputs and try again.",
              type = "warning",
              showCloseButton = TRUE
            )
          } else {
            # Store valid result
            kasp_des.result(result)

            # Show results container
            shinyjs::show("results_container")

            # Success notification
            show_alert(
              title = "Success",
              text = "Marker design completed successfully!",
              type = "success",
              showCloseButton = TRUE
            )
          }
        } else {
          error_state(TRUE)
          error_message(paste("Error in marker design:", future_result$message))
          show_alert(
            title = "Error",
            text = paste("Failed to design marker:", future_result$message),
            type = "error",
            showCloseButton = TRUE
          )
        }

        # Hide progress container and set processing to false
        shinyjs::hide("progress_container")
        processing(FALSE)
      })
    })

    # Render KASP table with error handling
    output$kasp_table <- DT::renderDT({
      req(kasp_des.result())

      if (error_state()) {
        return(NULL)
      }

      result <- kasp_des.result()

      tryCatch(
        {
          if (is.list(result)) {
            if (!("marker_data" %in% names(result))) {
              return(DT::datatable(data.frame(Error = "No marker data available"), options = list(pageLength = 10, scrollX = TRUE)))
            }
            return(DT::datatable(result$marker_data, options = list(pageLength = 10, scrollX = TRUE)))
          } else {
            return(DT::datatable(result, options = list(pageLength = 10, scrollX = TRUE)))
          }
        },
        error = function(e) {
          error_state(TRUE)
          error_message(paste("Failed to display table:", e$message))
          return(DT::datatable(data.frame(Error = paste("Failed to display data:", e$message)), options = list(pageLength = 10, scrollX = TRUE)))
        }
      )
    })

    # Render Plot with error handling
    output$plot <- renderPlot({
      req(kasp_des.result(), input$draw_plot)

      if (error_state()) {
        return(NULL)
      }

      tryCatch(
        {
          result <- kasp_des.result()
          if (is.list(result) && "pp" %in% names(result)) {
            return(result$pp)
          } else {
            return(NULL)
          }
        },
        error = function(e) {
          error_state(TRUE)
          error_message(paste("Failed to generate plot:", e$message))
          # Return an empty plot with error message
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0, y = 0, label = paste("Error:", e$message), size = 5) +
            ggplot2::theme_minimal() +
            ggplot2::theme(panel.grid = ggplot2::element_blank())
        }
      )
    })

    # Download Table with error handling
    output$download_table <- downloadHandler(
      filename = function() {
        # Sanitize filename
        clean_name <- gsub("[^[:alnum:]_-]", "_", input$file_name)
        paste0(clean_name, input$exten)
      },
      content = function(file) {
        if (is.null(kasp_des.result())) {
          stop("No data available for download")
        }

        result <- kasp_des.result()

        tryCatch(
          {
            if (input$exten == ".csv") {
              # Save as CSV
              if (is.list(result) && "marker_data" %in% names(result)) {
                write.csv(result$marker_data, file, row.names = FALSE)
              } else {
                write.csv(result, file, row.names = FALSE)
              }
            } else if (input$exten == ".xlsx") {
              # Save as Excel file
              if (is.list(result) && "marker_data" %in% names(result)) {
                openxlsx::write.xlsx(result$marker_data, file)
              } else {
                openxlsx::write.xlsx(result, file)
              }
            }
          },
          error = function(e) {
            show_alert(
              title = "Download Error",
              text = paste("Failed to download file:", e$message),
              type = "error",
              showCloseButton = TRUE
            )
            stop(e)
          }
        )
      }
    )

    # Download Plot with error handling
    output$download_plot <- downloadHandler(
      filename = function() {
        # Sanitize marker ID for filename
        clean_marker <- gsub("[^[:alnum:]_-]", "_", input$marker_ID)
        paste0("alignment_", clean_marker, ".pdf")
      },
      content = function(file) {
        if (is.null(kasp_des.result()) || !input$draw_plot) {
          stop("No plot available for download")
        }

        result <- kasp_des.result()

        tryCatch(
          {
            if (is.list(result) && "pp" %in% names(result)) {
              ggplot2::ggsave(
                filename = file,
                plot = result$pp,
                device = "pdf",
                width = 24,
                height = 9,
                units = "in"
              )
            } else {
              stop("Plot data not available")
            }
          },
          error = function(e) {
            show_alert(
              title = "Download Error",
              text = paste("Failed to download plot:", e$message),
              type = "error",
              showCloseButton = TRUE
            )
            stop(e)
          }
        )
      }
    )
  })
}

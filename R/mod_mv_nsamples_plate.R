#' Number of Samples per Plate UI Function
#'
#' @description A Shiny module for displaying the number of samples per plate in KASP genotyping data
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList sidebarLayout sidebarPanel mainPanel selectInput actionButton icon div
#' @importFrom bslib accordion accordion_panel
#'
mod_mv_nsamples_plate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Summary of plates samples
    sidebarLayout(
      sidebarPanel(
        # widget for plates
        selectizeInput(
          inputId = ns("subset_id"),
          label = "Select Plate Subset",
          choices = NULL,
          multiple = FALSE
        ),
        # Widget  for snpID
        selectizeInput(
          inputId = ns("snps_id"),
          label = "Choose SNP Identifier Column",
          choices = NULL,
          multiple = FALSE
        ),
        # Widget for plates
        selectizeInput(
          inputId = ns("plates_id"),
          label = "Choose Master Plate Column",
          choices = NULL,
          multiple = FALSE
        )
      ),
      mainPanel(
        bslib::accordion(
          width = "100%",
          open = "Summary of Sample Distribution Across Plates",
          bslib::accordion_panel(
            "Summary of Sample Distribution Across Plates",
            DT::DTOutput(ns("nsumm_output"))
          )
        )
      )
    )
  )
}



#' Number of Samples per Plate Server Function
#'
#' @description Server logic for calculating and displaying the number of samples
#'   per plate in KASP genotyping data
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param kasp_data A reactive expression that returns the KASP data frame
#'
#' @return A summary of samples per plate
#'
#' @noRd
#'
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent req updateSelectInput invalidateLater
#'
mod_mv_nsamples_plate_server <- function(id, kasp_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Get colnames from Kasp data
    subset_names <- reactive({
      req(kasp_data)
          return(colnames(kasp_data))

    })


    # Update select inputs field
    observe({
      req(subset_names())
          # Update the inputs
          updateSelectizeInput(session,
            server = TRUE,
            inputId = "subset_id",
            choices = subset_names(),
            selected = safe_grep_match(pattern = 'plates',
                                       choices = subset_names())
          )

          updateSelectizeInput(session,
            server = TRUE,
            inputId = "snps_id",
            choices = subset_names(),
            selected = safe_grep_match(pattern = 'snp',
                                       choices = subset_names())
          )

          updateSelectizeInput(session,
            server = TRUE,
            inputId = "plates_id",
            choices = subset_names(),
            selected = safe_grep_match(pattern = 'master',
                                       choices = subset_names())
          )

    })

    # Get summary of number of samples per plate
    nsample_plate_result <- reactiveVal(NULL)

    observe({
      req(kasp_data, input$subset_id, input$snps_id, input$plates_id)

      # Validate the selected columns exist in the data
      tryCatch(
        {
          # Process the data
          result <- nsamples_plate(
            x = kasp_data,
            subset = input$subset_id,
            snp_id = input$snps_id,
            plate_id = input$plates_id
          )

          # Store the result
          nsample_plate_result(result$summ)
        },
        error = function(e) {
          # Show error message
          shinyWidgets::show_alert(
            title = "Error",
            text = paste("Error processing plate samples:", e$message),
            type = "error",
            showCloseButton = TRUE
          )
        }
      )
    })


    # Display Summary of plates.
    observe({
      req(nsample_plate_result())

      tryCatch(        {
          output$nsumm_output <- DT::renderDT({
            # Ensure data is properly formatted as a data.frame
            df <- data.frame(nsample_plate_result(), check.names = FALSE)

            # Render output
            DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
          })

          shinyWidgets::show_toast(
            title = "Success",
            type = "success",
            text = "Summary statistics rendered successfully",
            timer = 2000,
            position = "bottom-end"
          )
        },
        error = function(e) {
          # Show alert when there is an error
          shinyWidgets::show_alert(
            title = "Error",
            text = paste("Error displaying results:", e$message),
            type = "error",
            showCloseButton = TRUE
          )
        }
      )
    })
  })
}

## To be copied in the UI
# mod_mv_nsamples_plate_ui("mv_nsamples_plate_1")

## To be copied in the server
# mod_mv_nsamples_plate_server("mv_nsamples_plate_1")

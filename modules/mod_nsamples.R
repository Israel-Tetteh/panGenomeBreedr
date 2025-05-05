source("Helper functions/utils_Marker_Validation.R")
source("modules/Marker_validation ui_helper functions.R")

nsamplesUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Summary of plates samples
    sidebarLayout(
      sidebarPanel(
        # widget for plates
        selectInput(
          inputId = ns('subset_id'),
          label = "Select Subset ",
          choices = NULL,
          multiple = FALSE
        ),
        # Widget  for snpID
        selectInput(
          inputId = ns('snps_id'),
          label = "Select SNP ID Column ",
          choices = NULL,
          multiple = FALSE
        ),
        # Widget for plates
        selectInput(
          inputId = ns('plates_id'),
          label = "Select MasterPlate Column ",
          choices = NULL,
          multiple = FALSE
        ),
        div(
          style = "display: flex; justify-content: center;",
          actionButton(ns("run_but"),
            label = "Submit",
            icon = icon("rocket"), class = "btn-primary", width = "70%"
          )
        )
      ),
      mainPanel(
        bslib::accordion(
          width = "100%",
          open = "Summary of Sample Distribution Across Plates",
          bslib::accordion_panel(
            "Summary of Sample Distribution Across Plates",
            DT::dataTableOutput(ns("nsumm_output"))
          )
        )
      )
    )
  )
}

nsamplesServer <- function(id, kasp_data) {
  moduleServer(
    id,
    function(input, output, session) {

      # Get colnames from Kasp data
      subset_names <- reactive({
        req(kasp_data)
        colnames(kasp_data)
      })


      # Update select inputs field
      observeEvent(subset_names(), {
        req(subset_names())
        updateSelectInput(session,inputId = 'subset_id',
                          choices = subset_names(),
                          selected = 'plates',)
        updateSelectInput(session,inputId = 'snps_id',
                          choices = subset_names(),
                          selected = 'SNPID')
        updateSelectInput(session,inputId = 'plates_id',
                          choices = subset_names(),
                          selected = 'MasterPlate')
      })

      # Get summary of number of samples per plate
      nsample_plate_result <- reactiveVal(NULL)

      observeEvent(input$run_but, {
        req(kasp_data, input$subset_id, input$snps_id, input$plates_id)

        result <- nsamples_plate(
          x = kasp_data,
          subset = input$subset_id,
          snp_id = input$snps_id,
          plate_id = input$plates_id
        )$summ

        nsample_plate_result(result)
      })




      # # Change Table colnames of nsample_summary_0 dynamically.
      # nsample_summary <- reactive({
      #   req(input$subset_id, input$snps_id, input$plates_id, nsample_summary_0())
      #
      #   new_colnames_nsamples(
      #     MasterPlate = input$plates_id,
      #     SNIPID = input$snps_id,
      #     PLATE = input$subset_id,
      #     data_nsamples = nsample_summary_0()
      #   )
      # })

      # Display Summary of plates.
      output$nsumm_output <- DT::renderDT({
        DT::datatable(data.frame(nsample_plate_result()))
      })
    }
  )
}




# Source for functions used in module.
source("Helper functions/utils_Marker_Validation.R")
source("modules/Marker_validation ui_helper functions.R")

get_allelesUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Get alleles
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = ns("data_type_id"),
          label = "Select Data Source",
          choices = c("kasp", "agriplex"),
          multiple = FALSE
        ),
        textInput(
          inputId = ns("sep_id"),
          label = "Genotype Call Separator",
          value = ":"
        )
      ),
      mainPanel(
        # Allele output
        bslib::accordion(
          width = "100%",
          open = "Identified Alleles",
          bslib::accordion_panel(
            "Identified Alleles",
            selectInput(ns("plates_pres"),
              label = "Choose plate",
              choices = NULL,
              multiple = FALSE
            ),
            shiny::tableOutput(ns("alleles_output"))
          )
        ), br(),
        # Genotype output
        bslib::accordion(
          width = "100%",
          open = "Genotype Classifications",
          bslib::accordion_panel(
            "Genotype Classifications",
            tableOutput(ns("genotypes_output"))
          )
        )
      )
    )
  )
}

get_allelesServer <- function(id, kasp_data) {
  moduleServer(
    id,
    function(input, output, session) {
      distinct_plates <- reactive({
        req(kasp_data)
        uniq_plates(kasp_data)
      })

      # Update for get alleles entry for user.
      observeEvent(distinct_plates(), {
        req(distinct_plates())
        updateSelectInput(session,
          inputId = "plates_pres",
          choices = distinct_plates()
        )
      })

      # Get genotype  calls
      Get_calls <- reactive({
        req(kasp_data, input$plates_pres)
        get_calls(x = kasp_data, a = input$plates_pres)
      })


      # get alleles.
      obtain_alleles <- reactive({
        req(Get_calls(), input$data_type_id)
        get_alleles(x = Get_calls(), data_type = input$data_type_id,
                    sep = input$sep_id)
      })

      # Display alleles from get_alleles()
      output$alleles_output <- renderTable({
        alleles_df(x = obtain_alleles()$alleles)
      })

      # Display genotypes from get_alleles()
      output$genotypes_output <- renderTable({
        genotypes(obtain_alleles()$genotypes)
      })
    }
  )
}

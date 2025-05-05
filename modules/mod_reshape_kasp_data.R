# Functions used from marker validation
#-> kasp_reshape_wide

source("Helper functions/utils_Marker_Validation.R") # source
source("modules/Marker_validation ui_helper functions.R")

Kasp_reshapeUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(

        selectInput(inputId = ns("subset_col"), label = "Master Plate Column:",
                    choices = NULL, selected = "MasterPlate"),

        selectInput(inputId = ns("snp_id_col"), label = "SNP ID Column:",
                    choices = NULL, selected = "SNPID"),

        selectInput(inputId = ns("geno_call_col"), label = "Genotype Call Column:",
                    choices = NULL, selected = "Call"),

        selectInput(inputId = ns("id_col"), label = "Subject ID Column:",
                    choices = NULL, selected = "SubjectID"),

       div(
        style = 'display: flex; justify-content: center;',
        actionButton(inputId = ns("reshape_btn"), label = "Reshape Data",
                     class = "btn-primary", width = "70%")
       )
      ),

      # Main panel for output
      mainPanel(
        bslib::accordion(

          accordion_panel(title = 'Reshaped To Wide Format',
                      selectInput(inputId = ns('plate_id'),
                      label = 'Select Plate',
                      choices = NULL,
                      multiple = FALSE),
                          DT::dataTableOutput(outputId = ns('reshaped_table')))
        )

      )
    )

  )
}

Kasp_reshapeServer <- function(id,kasp_data ) {
  moduleServer(
    id,
    function(input, output, session) {

      # Items to populate selectinput widgets
      drop_downs <- reactive({
        req(kasp_data)
        colnames(kasp_data)
      })

      # update the select input columns.
      observe({
        req(drop_downs())
        updateSelectInput(session = session,inputId = 'subset_col',choices = drop_downs(),selected = 'MasterPlate')
        updateSelectInput(session = session,inputId = 'snp_id_col',choices = drop_downs(),selected = 'SNPID')
        updateSelectInput(session = session,inputId = 'geno_call_col',choices = drop_downs(),selected = 'Call')
        updateSelectInput(session = session,inputId = 'id_col',choices = drop_downs(),selected = 'SubjectID')

      })

      # Function to reshape the data.

      Kasp_reshap_result <- reactiveVal()

      observeEvent(input$reshape_btn,{
        req(kasp_data,input$subset_col,input$snp_id_col, input$geno_call_col,input$id_col)

        Kasp_reshap_result(
          kasp_reshape_wide(x = kasp_data,
                            subset = ,
                            snp_id = input$snp_id_col ,
                            geno_call = input$geno_call_col,
                            idvar = input$id_col
          )
        )

      })


      # Get the names of the list and populate in the drop down menu-- accordion panel
      names_reshape <- reactive({
        req(Kasp_reshap_result())
        names(Kasp_reshap_result())
      })

      # Update the drop down
      observeEvent(!is.null(Kasp_reshap_result()),{
        req(names_reshape())
        updateSelectInput(session,inputId = 'plate_id',choices = names_reshape())

      })

      # subset the selected plate id
      show_only <- reactive({
        req(input$plate_id, Kasp_reshap_result())
        Kasp_reshap_result()[[input$plate_id]] |> as.data.frame()
      })
      # show in table.
      output$reshaped_table <- renderDT({
        req(show_only())
        datatable(data = show_only())
      }
      )

      return_output <- reactive({
        req(Kasp_reshap_result())
        Kasp_reshap_result()
      })

      return(return_output)

    }
  )
}





# Source for functions used in module.
source("Helper functions/utils_Marker_Validation.R")
source("modules/Marker_validation ui_helper functions.R")

mod_read_kasp_csv_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # Widget to upload kasp csv file
        fileInput(ns("Kasp_csv.file"),
          label = "Upload KASP Genotyping Results",
          accept = ".csv"
        ),
        # Widget to choose data type
        radioButtons(
          inputId = ns("datatype"),
          label = "Choose Data Format",
          choices = c("raw", "polished"),
          selected = "raw"
        ),
        # Widget for spacing
        numericInput(ns("data_space"),
          label = "Enter Number of Blank Rows Separating Data Segments",
          value = 2, min = 0, max = 100, step = 1
        ),
        # Widget for  row tags
        textInput(
          inputId = ns("row_tags"),
          label = "Row Tags (ordered, comma-separated)",
          value = "Statistics , DNA , SNPs , Scaling , Data"
        ),
        # action button widget
        div(
          style = "display: flex; justify-content: center;",
          actionButton(ns("submit_btn"),
            label = "Submit",
            icon = icon("rocket"), class = "btn-primary", width = "70%"
          )
        )
      ),
      mainPanel(
        # Display results from after  reading csv.
        bslib::accordion(
          width = "100%",
          open = "KASP Genotyping Results File with FAM and HEX Fluorescence Coordinates", # Default open section
          bslib::accordion_panel(
            "KASP Genotyping Results File with FAM and HEX Fluorescence Coordinates",
            tabsetPanel(
              tabPanel(title = "Data", DT::dataTableOutput(ns("kasp_data"))),
              tabPanel(title = "Statistics", DT::dataTableOutput(ns("kasp_statistics"))),
              tabPanel(title = "SNPS", DT::dataTableOutput(ns("kasp_snps"))),
              tabPanel(title = "DNA", DT::dataTableOutput(ns("kasp_DNA"))),
              tabPanel(title = "Scaling", DT::dataTableOutput(ns("kasp_scaling")))
            )
          )
        )
      )
    )
  )
}






mod_read_kasp_csv_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create a null reactive component to store results from the read_kasp_csv()
    import_data <- reactiveVal(NULL)

    # get the list data from the uploaded KASP genotyping file.
    observeEvent(input$submit_btn, {
      req(
        input$Kasp_csv.file$datapath, input$datatype,
        input$data_space, input$row_tags
      )

      read_kasp_result <- read_kasp_csv(
        file = input$Kasp_csv.file$datapath,
        spacing = input$data_space,
        data_type = input$datatype,
        row_tags = trimws(unlist(strsplit(x = input$row_tags ,split = ',')))
      )
      import_data(read_kasp_result) # Store result.
    })

    # Add plates if absent.
    add_plates <- reactiveVal(NULL)

    observeEvent(import_data(), {
      if(is.list(import_data())){

      refined_wplates <- plates_col(import_data()$Data) |> as.data.frame()
      add_plates(refined_wplates)

      } else{

       refined_wplates <- plates_col(import_data())
       add_plates(refined_wplates)

      }

    })

    #  Data output
    output$kasp_data <- DT::renderDT({
      req(add_plates())
      DT::datatable(add_plates(), options = list(pageLength = 10, scrollX = TRUE))
    })

    # Statistics output
    output$kasp_statistics <- DT::renderDT({
      req(import_data())
      DT::datatable(import_data()$Statistics, options = list(pageLength = 10, scrollX = TRUE))
    })

    # SNPS output
    output$kasp_snps <- DT::renderDT({
      req(import_data())
      DT::datatable(import_data()$SNPs, options = list(pageLength = 10, scrollX = TRUE))
    })

    # DNA output.
    output$kasp_DNA <- DT::renderDT({
      req(import_data())
      DT::datatable(import_data()$DNA, options = list(pageLength = 10, scrollX = TRUE))
    })
    # Scaling output
    output$kasp_scaling <- DT::renderDT({
      req(import_data())
      DT::datatable(import_data()$Scaling, options = list(pageLength = 10, scrollX = TRUE))
    })

    # Return reactive containing the data frame with plates added
    return(add_plates)
  })
}


#-----
library(shiny)
library(bslib)
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Test Run"),
  mod_read_kasp_csv_ui(id = "import_kasp_id")
)

server <- function(input, output, session) {
  mod_read_kasp_csv_server(id = "import_kasp_id")
}

shinyApp(ui, server)

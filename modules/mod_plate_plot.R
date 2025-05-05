##Plate layout script
source("modules/Marker_validation ui_helper functions.R")
source("Helper functions/utils_Marker_Validation.R")
plate_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = ns("well_id"),
          label = "Select Column for Genotyping Plate wells",
          choices = NULL,
          multiple = FALSE,
          width = "100%"
        ),
        selectInput(
          inputId = ns("Geno_call_id"),
          label = "Select Column for Genotypes",
          choices = NULL,
          multiple = FALSE,
          width = "100%"
        ),

        selectInput(
          inputId = ns("snp_idd"),
          label = "Select Column for SNP ID",
          choices = NULL,
          multiple = FALSE,
          width = "100%"
        ),
        div(
          style = 'display: flex; justify-content: center;',
          actionButton(inputId = ns("plot_btn"), label = "Generate Layout",
                       class = "btn-primary", width = "70%")
        )

      ),
      mainPanel(
        bslib::accordion(
          open = "KASP Genotyping Plate Layout",
          bslib::accordion_panel(
            title = "KASP Genotyping Plate Layout",
            selectInput(
              inputId = ns("cplate_well"),
              choices = NULL,
              label = "Select Plate",
              multiple = FALSE,
              width = "100%"
            ),
            plotOutput(outputId = ns("plate_layout_plot")), hr(),
            textInput(inputId = ns("file_name"), label = "Enter Filename", value = "Plate layout 1"),
            downloadButton(outputId = ns("download_plateplot"), label = "Download Plot", class = "btn-success")
          )
        )
      )

    )

  )
}

# require kasp_data and color coded from kasp_color
plate_plotServer <- function(id, kasp_data, color_coded ) {
  moduleServer(
    id,
    function(input, output, session) {
      sub_names <- reactive({
        req(kasp_data)
        colnames(kasp_data)
      })

      #Populate choices in subset
      observeEvent(req(sub_names()), {
        updateSelectInput(session , inputId = 'snp_idd', choices = sub_names(),selected = 'SNPID')
        # Plate layout
        updateSelectInput(session , inputId = 'Geno_call_id', choices = sub_names(),selected = 'Call')
        updateSelectInput(session, inputId = 'well_id', choices = sub_names(),selected = 'MasterWell')
      })

      names_col <- reactive({
        req(color_coded())
        names(color_coded())

      })

      # Populate dropdown for subplates
      observeEvent(names_col(), {
        req(names_col())
        updateSelectInput(session ,inputId = 'cplate_well' , choices = names_col())
      })

      observe({
        req(names_col())
        print(names_col())
      })

      # Plate layout plot script.
      plot_plate_result <- reactiveVal(NULL)

      observeEvent(input$plot_btn,{
        req(color_coded(),input$snp_idd,input$Geno_call_id,input$well_id)

        plot_plate_result(
          plot_plate(x = color_coded(),
                   well = input$well_id,
                   geno_call = input$Geno_call_id,
                   snp_id = input$snp_idd)
        )
      })

      # Display the plot for plate layout.
      output$plate_layout_plot <- renderPlot({
        req(plot_plate_result(),input$cplate_well)
        plot_plate_result()[[input$cplate_well]]
      })

      # Download plate layout plot.
      output$download_plateplot <- downloadHandler(
        filename = function() {
          req(input$file_name)
          paste0(input$file_name, ".pdf")
        },
        content = function(file) {
          req(plot_selected())

          pdf(file, width = 8, height = 5)
          grid::grid.draw(plot_selected()) # Draw the grid structure
          dev.off()
        }
      )


    }
  )
}



# Functions from marker validation used here.
#-> plot_plate

# Color code ui Module.
source("modules/Marker_validation ui_helper functions.R")
source("Helper functions/utils_Marker_Validation.R")

kasp_colorUI <- function(id ) {
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = ns('Subset_names'),
                    label = 'Select Column for Substetting',
                    choices = NULL,multiple = FALSE
        ),
        selectInput(inputId = ns('geno_call_col'),
                    label = 'Select Column for Genotype Calls',
                    choices = NULL,multiple = FALSE
        ),
        textInput(inputId = ns('sep'),
                  label = 'Enter Separator for Genotype Calls',
                  value = ':'
        ),
        textInput(inputId = ns('uncallable'),
                  label = 'Enter Uncallable Genotype Calls',
                  value = 'Uncallable'
        ),
        textInput(inputId = ns('unused'),
                     label = 'Enter Unused Genotype Calls',
                     value = '?'
        ),
        textInput(inputId = ns('blank'),
                  label = 'Specify No Template Control Calls',
                  value = 'NTC'
        ),
        textInput(inputId = ns('others'),
                  label = 'Enter Non-genotype Calls',
                  value = 'Missing , Bad , Dupe , Over , Short'
        ),
        selectInput(inputId = ns('color_choose'),
                    label = 'Select Respective Colors for | FAM | HEX | Het | ',
                    choices = colors(),
                    multiple = TRUE,
                    selected = c('blue','gold','forestgreen')
        ),
        div(style = "display: flex; justify-content: center;",
            actionButton(inputId =ns("run_analysis") ,label = "Run",
                         icon = icon("palette"), class = "btn-primary" ,
                         width = "50%",
                         style = "color: white;
                          border-radius: 6px; padding: 10px 20px;
                          font-size: 16px; border: none;
                          cursor: pointer;"))
      ),
      mainPanel(
        bslib::accordion(open = TRUE,
                         bslib::accordion_panel(title = "Plate Status After FAM & HEX Color-Coding by LGC Genomics"
                                                , DT::dataTableOutput(outputId = ns('kasp_color_code_stat')))
        ),br(),
        bslib::accordion(
          bslib::accordion_panel(title = "KASP Genotype Calls Color-Coded by LGC Genomics (FAM & HEX)",
                                 selectInput(inputId = ns('subplates') ,
                                             choices = NULL ,
                                             label = 'Select Plate',multiple = FALSE
                                 )
                                 , DT::dataTableOutput(outputId = ns('kasp_color_code')))
        )

      )
    )

  )
}

kasp_colorServer <- function(id , kasp_data ) {
  moduleServer(
    id,
    function(input, output, session) {
      # get colnames for drop down
      sub_names <- reactive({
        req(kasp_data)
        colnames(kasp_data)
      })

      #Populate choices in subset
      observeEvent(req(sub_names()), {
        # genotype call side bar input
        updateSelectInput(session, inputId = 'Subset_names', choices = sub_names() ,selected = 'plates')
        updateSelectInput(session, inputId = 'geno_call_col', choices = sub_names(),selected = 'Call' )
      })

      color_coded <- reactiveVal()

      observeEvent(input$run_analysis, {
        req(kasp_data, input$Subset_names, input$geno_call_col,
            input$color_choose, input$uncallable,input$unused,
            input$blank ,input$others ,input$sep)

        color_coded(
          panGenomeBreedr::kasp_color(
            x = kasp_data,
            uncallable = input$uncallable ,
            unused = input$unused,
            blank = input$blank,
            others = stringr::str_squish(unlist(strsplit(x = input$others,split = ','))),
            sep = input$sep,
            subset = input$Subset_names,
            geno_call = input$geno_call_col,
            assign_cols = c(FAM = input$color_choose[1],
                            HEX = input$color_choose[2],
                            het = input$color_choose[3]
            )
          )
        )

      })

      # Output: Plate names
      names_col <- reactive({
        req(color_coded())
        names(color_coded())

      })

      # Populate dropdown for subplates
      observeEvent(req(names_col()), {
        updateSelectInput(session, inputId = 'subplates', choices = names_col() )
      })

      # Display color-coded KASP data
      output$kasp_color_code <- DT::renderDT({
        req(input$subplates)
        DT::datatable(color_coded()[[input$subplates]] |> as.data.frame(),options = list(pageLength = 10, scrollX = TRUE))
      })



      # Status of plates
      color_coded_stat <- reactiveVal()

      observeEvent(color_coded(), {
        req(kasp_data, input$Subset_names, input$geno_call_col,color_coded())

        color_coded_stat(
          kasp_color_stat.give(
            x = kasp_data,
            subset = input$Subset_names,
            geno_call = input$geno_call_col
          )
        )
      })

      # Display KASP plate status
      output$kasp_color_code_stat <- DT::renderDT({
        DT::datatable(color_coded_stat(),options = list(pageLength = 10, scrollX = TRUE))
      })

      return(color_coded) # return list of color coded plates

    })
}


#-----
library(shiny)
library(bslib)
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Test Run"),
  kasp_colorUI(id = 'Test1')
)

server <- function(input, output, session) {

}

shinyApp(ui, server)

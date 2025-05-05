# Functions used from marker validation script.
#-> kasp_qc_ggplot
#-> kasp_qc_ggplot2


source("modules/Marker_validation ui_helper functions.R")
source("Helper functions/utils_Marker_Validation.R")

# qc_plotUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#
#     sidebarLayout(
#       sidebarPanel(
#         selectInput(inputId = ns('fam_id'),label = 'Select Column Name for FAM',choices = NULL ,multiple = FALSE), # FAM Input
#         selectInput(inputId = ns('Hex_id'),label = 'Select Column Name for HEX',choices = NULL ,multiple = FALSE),
#         selectInput(inputId = ns('plate_choice') ,label = 'Choose a Plate',choices = NULL,multiple = FALSE),
#         # selectInput(inputId = 'geno.call_id',label = 'Select Column Name for Genotype Calls',choices = NULL ,multiple = FALSE), # fetch input from predictive stat.
#         # selectInput(inputId = 'color_id',label = 'Select Column Name for Color',choices = NULL ,multiple = FALSE),
#         # selectInput(inputId = 'snp_id',label = 'Select Column Name for SNP ID',choices = NULL ,multiple = FALSE),
#         # selectInput(inputId = 'group_id',label = 'Select Column Name for Group ID',choices = NULL ,multiple = FALSE),
#         bslib::input_switch(id = ns('scale'),label = 'Scale FAM & HEX between 0 - 1',value = TRUE),
#         numericInput(inputId = ns('alpha_id'),label = 'Alpha Value',value = 0.5 ,min = 0 ,max = 1, step = 0.05),
#
#       ),
#       mainPanel(
#         bslib::accordion(
#           bslib::accordion_panel(title = 'Kasp QC GGPLOT',
#                                  fluidRow(
#                                    column(width = 4,
#                                           numericInput(inputId = ns('expand_id'),label = 'Expand Axis',value = 0.6 ,min = 0 ,max = 1,step = 0.05)
#                                    ),
#                                    column(width = 4,
#                                           numericInput(inputId = ns('legendx_id'),label = 'Legend Position X',value = 0.6 ,min = 0 ,max = 1,step = 0.05)
#                                    ),column(width = 4,
#                                             numericInput(inputId = ns('legendy_id'),label = 'Legend Position Y',value = 0.8 ,min = 0 ,max = 1,step = 0.05)
#                                    )
#                                  ) ,
#                                  fluidRow(
#                                    column(width = 4,
#                                           selectInput(inputId = ns('legend_box'),label = 'Select Legend Box',choices = c('horizontal' , 'vertical') ,multiple = FALSE)
#                                    ),
#                                    column(width = 4,
#                                           selectInput(inputId = ns('legend_pos'),label = 'Select Legend Position',choices = c('inside' , 'outside') ,multiple = FALSE)
#                                    ),column(width = 4,
#                                             numericInput(inputId = ns('textsize_id'),label = 'Text Size',value = 12,min = 1 ,max = 30, step = 1)
#                                    )
#                                  ),
#                                  plotOutput(outputId = ns('qc_plot1'))
#           )
#         )
#       )
#     )
#
#   )
# }

# qc_plotUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     fluidRow(
#       column(3,
#              selectInput(inputId = ns('fam_id'),label = 'Select Column Name for FAM',choices = NULL,multiple = FALSE)
#
#       ),
#       column(3,
#              selectInput(inputId = ns('Hex_id'),label = 'Select Column Name for HEX',choices = NULL,multiple = FALSE)
#
#       ),
#       column(3,
#              selectInput(inputId = ns('plate_choice'),label = 'Choose Plate',choices = NULL,multiple = FALSE)
#
#       ),
#       column(width = 3,
#              selectInput(inputId = ns('group_id'),label = 'Select Column Name for Positive Controls',choices = NULL,multiple = FALSE)
#       )
#
#
#     ),
#     fluidRow(
#       column(3,
#              selectInput(inputId = ns('scale'),label = 'Scale FAM & HEX between 0 - 1',choices = c(TRUE , FALSE),selected = TRUE)
#
#       ),
#       column(width = 3,
#              numericInput(inputId = ns('expand_id'),label = 'Expand Axis',value = 0.6 ,min = 0 ,max = 1,step = 0.05)
#       ),
#       column(width = 3,
#              numericInput(inputId = ns('legendx_id'),label = 'Legend Position X',value = 0.6 ,min = 0 ,max = 1,step = 0.05)
#       ),column(width = 3,
#                numericInput(inputId = ns('legendy_id'),label = 'Legend Position Y',value = 0.8 ,min = 0 ,max = 1,step = 0.05)
#       )
#     ),
#     fluidRow(
#       column(width = 3,
#              selectInput(inputId = ns('legend_box'),label = 'Select Legend Box',choices = c('horizontal' , 'vertical') ,multiple = FALSE)
#       ),
#       column(width = 3,
#              selectInput(inputId = ns('legend_pos'),label = 'Select Legend Position',choices = c('inside' , 'outside') ,multiple = FALSE)
#       ),column(width = 3,
#                numericInput(inputId = ns('textsize_id'),label = 'Text Size',value = 12,min = 1 ,max = 30, step = 1)
#       ),
#       column(3,
#              numericInput(inputId = ns('alpha_id'),label = 'Alpha Value',value = 0.5 ,min = 0 ,max = 1, step = 0.05)
#       )
#     ),
#     # Display QC plots fo marker and one overlayedwith predictions
#     fluidRow(
#       column(6,
#              bslib::accordion(open = 'KASP marker genotyping QC plot',
#                bslib::accordion_panel(title = 'KASP marker genotyping QC plot',
#
#                                       # div(style = "display: flex; justify-content: center;",
#                                       #     actionButton(inputId = ns('get_qc'),label = 'Generate plot',icon = icon('rocket'),width = '50%',class = 'btn-primary')), hr(),
#                                       plotOutput(outputId = ns('qc_plot1')),
#                                       hr(),
#                                       textInput(inputId = ns('file_name1'),label = 'Enter Filename',value = 'QC_plot1'),
#                                       downloadButton(outputId = ns('Download Plot'),label = 'Download Plot',class = 'btn-success')
#                                       )
#              )
#       ),
#       column(6,
#              bslib::accordion(open = ' KASP marker genotyping QC plot overlaid with predicitons',
#                bslib::accordion_panel(title = ' KASP marker genotyping QC plot overlaid with predicitons',
#                                       selectInput(inputId = ns('pred_col_id'),label = 'Prediction Legend Colors (Blank | False | True | Unverified)',
#                                                   choices = colors(),width = '100%',multiple = TRUE,selected = c('blue', 'orange','forestgreen','black')),
#                                     # div(style = "display: flex; justify-content: center;",
#                                     # actionButton(inputId = ns('get_qc2'),label = 'Generate plot',icon = icon('rocket'),width = '50%',class = 'btn-primary')),
#                                         plotOutput(outputId = ns('qc_plot2')),
#                                     hr(),
#                                     textInput(inputId = ns('file_name2'),label = 'Enter Filename',value = 'QC plot plus prediction'),
#                                     downloadButton(outputId = ns('Download Plot'),class = 'btn-success',label = 'Download Plot')
#                                     )
#              )
#       )
#     )
#
#   )
# }
#
#
#
#
#
# qc_plotServer <- function(id, req_inputs, color_coded) {
#   moduleServer(
#     id,
#     function(input, output, session) {
#
#       #
#       drop_down_choices <-  reactive({
#         req(req_inputs())
#          req_inputs()$drop_down
#       })
#
#       plates_field_choices <- reactive({
#         req(color_coded())
#          names(color_coded())
#       })
#       # Populate the columns for FAM, HEX, and plate choices
#       observeEvent(drop_down_choices(), {
#         req(drop_down_choices())
#         # Update select inputs
#         updateSelectInput(session, inputId = 'fam_id', choices = drop_down_choices(),selected = 'X' )
#         updateSelectInput(session, inputId = 'Hex_id', choices = drop_down_choices(),selected = 'Y')
#         updateSelectInput(session, inputId = 'plate_choice', choices = plates_field_choices())
#         updateSelectInput(session , inputId = 'group_id', choices = c('None' ,drop_down_choices()), selected = 'Group')
#       })
#
#       # Reactive function to generate QC plot 1 withput prediction
#       qc_plots <- reactive({
#         req(color_coded(), input$fam_id, input$Hex_id, req_inputs(),input$group_id,
#             input$expand_id, input$scale, input$textsize_id, input$alpha_id,
#             input$legendx_id, input$legendy_id, input$legend_pos, input$legend_box)
#
#          if(input$group_id == 'None'){
#            # Generate the QC plot
#            kasp_qc_ggplot(
#              x = color_coded(),
#              FAM = input$fam_id,
#              HEX = input$Hex_id,
#              geno_call = req_inputs()$geno_call_id,
#              snp_id = req_inputs()$snp_id,
#              Group_id = NULL,
#              expand_axis = input$expand_id,
#              scale = input$scale,
#              text_size = input$textsize_id,
#              alpha = input$alpha_id,
#              legend.pos.x = input$legendx_id,
#              legend.pos.y = input$legendy_id,
#              legend.pos = input$legend_pos,
#              legend.box = input$legend_box
#            )
#          } else{
#            # Generate the QC plot
#            kasp_qc_ggplot(
#              x = color_coded(),
#              FAM = input$fam_id,
#              HEX = input$Hex_id,
#              geno_call = req_inputs()$geno_call_id,
#              snp_id = req_inputs()$snp_id,
#              Group_id = input$group_id,
#              expand_axis = input$expand_id,
#              scale = input$scale,
#              text_size = input$textsize_id,
#              alpha = input$alpha_id,
#              legend.pos.x = input$legendx_id,
#              legend.pos.y = input$legendy_id,
#              legend.pos = input$legend_pos,
#              legend.box = input$legend_box
#            )
#          }
#
#
#       })
#
# #       RAssistant::R_assistant(" check if this is correct. observeEvent(req(sub_names()), {
# #
# #         updateSelectInput(session, inputId = 'Subset_names', choices = sub_names(),selected = sub_names()['plates'])
# #       })
# #
# # ")
#       # observe({
#       #   req(qc_plots())
#       #   print("Structure of  qc_plots():")
#       #   print(qc_plots()[input$plate_choice] )
#       # })
#
#     output$qc_plot1 <- renderPlot({
#       req(qc_plots(),input$plate_choice)
#       qc_plots()[input$plate_choice]
#     })
#
#     # For plot with predictions
#     qc_int <- reactive({
#
#       req(color_coded(), input$fam_id, input$Hex_id, req_inputs(),input$group_id,
#           input$expand_id, input$scale, input$textsize_id, input$alpha_id,input$pred_col_id,
#           input$legendx_id, input$legendy_id, input$legend_pos, input$legend_box)
#
#       if(input$group_id == 'None'){
#         kasp_qc_ggplot2(x = color_coded() ,
#                         FAM = input$fam_id,
#                         HEX = input$Hex_id,
#                         geno_call = req_inputs()$geno_call_id,
#                         snp_id = req_inputs()$snp_id,
#                         Group_id = NULL,
#                         legend.pos.x = input$legendx_id,
#                         legend.pos.y = input$legendy_id ,
#                         legend.box = input$legend_box,
#                         alpha = input$alpha_id,
#                         text_size = input$textsize_id,
#                         legend.pos = input$legend_pos,
#                         scale = input$scale,
#                         pred_cols = c('Blank' = input$pred_col_id[1], 'False' = input$pred_col_id[2],
#                                       'True' = input$pred_col_id[3], 'Unverified' = input$pred_col_id[4]),
#                         expand_axis = input$expand_id
#         )
#       }else{
#         kasp_qc_ggplot2(x = color_coded() ,
#                         FAM = input$fam_id,
#                         HEX = input$Hex_id,
#                         geno_call = req_inputs()$geno_call_id,
#                         snp_id = req_inputs()$snp_id,
#                         Group_id = input$group_id,
#                         legend.pos.x = input$legendx_id,
#                         legend.pos.y = input$legendy_id ,
#                         legend.box = input$legend_box,
#                         alpha = input$alpha_id,
#                         text_size = input$textsize_id,
#                         legend.pos = input$legend_pos,
#                         scale = input$scale,
#                         pred_cols = c('Blank' = input$pred_col_id[1], 'False' = input$pred_col_id[2],
#                                       'True' = input$pred_col_id[3], 'Unverified' = input$pred_col_id[4]),
#                         expand_axis = input$expand_id
#         )
#       }
#
#
#
#     })
#
#     # Out put for qc_plot with predictions
#     output$qc_plot2 <- renderPlot({
#       req(qc_int(),input$plate_choice)
#       qc_int()[input$plate_choice]
#     })
#
#     }
#   )
# }

# #
# #
# # Assign KASP colors to plates
# dat1 <- kasp_color(x = panGenomeBreedr::kasp_dat,
#                    subset = 'MasterPlate',
#                    sep = ':',
#                    geno_call = 'Call',
#                    uncallable = 'Uncallable',
#                    unused = '?',
#                    blank = 'NTC')
#
# # KASP QC plot for Plate 12
# a <-  kasp_qc_ggplot(x = dat1,
#                     pdf = FALSE,
#                     Group_id = 'Group',
#                     scale = TRUE,
#                     expand_axis = 0.6,
#                     alpha = 0.5,
#                     legend.pos.x = 0.6,
#                     legend.pos.y = 0.8)
# print(a)



#
# library(shiny)
#
#
# ui <- fluidPage(
#
# )
#
# server <- function(input, output, session) {
#
# }
#
# shinyApp(ui, server)

# library(fontawesome)
# ?fontawesome::fa()
#
# fa(name = "r-project")


#=============== QC_plot - Claude =================

qc_plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Add necessary packages for notifications
    shinyjs::useShinyjs(),

    # Error message panel
    uiOutput(ns("error_messages")),

    # A spinner to display while work is carried out in the background
    shinybusy::add_busy_spinner(spin = "fading-circle", color = "white", timeout = 100),
    fluidRow(
      column(
        3,
        selectInput(inputId = ns("fam_id"), label = "Select Column Name for FAM", choices = NULL, multiple = FALSE)
        # tags$div(id = ns("fam_id_help"), class = "text-muted", style = "font-size: 0.8em; margin-top: -15px;",
        #          "Required: Column containing FAM fluorescence values")
      ),
      column(
        3,
        selectInput(inputId = ns("Hex_id"), label = "Select Column Name for HEX", choices = NULL, multiple = FALSE)
        # tags$div(id = ns("hex_id_help"), class = "text-muted", style = "font-size: 0.8em; margin-top: -15px;",
        #          "Required: Column containing HEX fluorescence values")
      ),
      column(
        3,
        selectInput(inputId = ns("plate_choice"), label = "Choose Plate", choices = NULL, multiple = FALSE)
        # tags$div(id = ns("plate_choice_help"), class = "text-muted", style = "font-size: 0.8em; margin-top: -15px;",
        #          "Required: Select which plate to display")
      ),
      column(
        width = 3,
        selectInput(inputId = ns("group_id"), label = "Select Column Name for Positive Controls", choices = NULL, multiple = FALSE)
        # tags$div(id = ns("group_id_help"), class = "text-muted", style = "font-size: 0.8em; margin-top: -15px;",
        #          "Optional: Column identifying control samples")
      )
    ),
    fluidRow(
      column(
        3,
        selectInput(inputId = ns("scale"), label = "Scale FAM & HEX between 0 - 1", choices = c(TRUE, FALSE), selected = TRUE)
      ),
      column(
        width = 3,
        numericInput(inputId = ns("expand_id"), label = "Expand Axis", value = 0.6, min = 0, max = 1, step = 0.05)
      ),
      column(
        width = 3,
        numericInput(inputId = ns("legendx_id"), label = "Legend Position X", value = 0.6, min = 0, max = 1, step = 0.05)
      ),
      column(
        width = 3,
        numericInput(inputId = ns("legendy_id"), label = "Legend Position Y", value = 0.8, min = 0, max = 1, step = 0.05)
      )
    ),
    fluidRow(
      column(
        width = 3,
        selectInput(inputId = ns("legend_box"), label = "Select Legend Box", choices = c("horizontal", "vertical"), multiple = FALSE)
      ),
      column(
        width = 3,
        selectInput(inputId = ns("legend_pos"), label = "Select Legend Position", choices = c('inside','left','right','bottom','top','none'), multiple = FALSE)
      ),
      column(
        width = 3,
        numericInput(inputId = ns("textsize_id"), label = "Text Size", value = 12, min = 1, max = 30, step = 1)
      ),
      column(
        3,
        numericInput(inputId = ns("alpha_id"), label = "Alpha Value", value = 0.5, min = 0, max = 1, step = 0.05)
      )
    ),

    # Display QC plots for marker and one overlayed with predictions
    fluidRow(
      column(
        6,
        bslib::accordion(
          open = "KASP marker genotyping QC plot",
          bslib::accordion_panel(
            title = "KASP marker genotyping QC plot",
            plotOutput(outputId = ns("qc_plot1")),
            hr(),
            fluidRow(
              column(
                8,
                textInput(inputId = ns("file_name1"), label = "Enter Filename", value = "QC_plot1")
              ),
              column(
                4,
                selectInput(
                  inputId = ns("file_type1"), label = "File Format",
                  choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                  selected = "pdf"
                )
              )
            ),
            downloadButton(outputId = ns("download_plot1"), label = "Download Plot", class = "btn-success")
          )
        )
      ),
      column(
        6,
        bslib::accordion(
          open = "KASP marker genotyping QC plot overlaid with predicitons",
          bslib::accordion_panel(
            title = "KASP marker genotyping QC plot overlaid with predicitons",
            selectInput(
              inputId = ns("pred_col_id"), label = "Prediction Legend Colors (Blank | False | True | Unverified)",
              choices = colors(), width = "100%", multiple = TRUE,
              selected = c("blue", "orange", "forestgreen", "black")
            ),
            plotOutput(outputId = ns("qc_plot2")),
            hr(),
            fluidRow(
              column(
                8,
                textInput(inputId = ns("file_name2"), label = "Enter Filename", value = "QC_plot_with_prediction")
              ),
              column(
                4,
                selectInput(
                  inputId = ns("file_type2"), label = "File Format",
                  choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff"),
                  selected = "pdf"
                )
              )
            ),
            downloadButton(outputId = ns("download_plot2"), label = "Download Plot", class = "btn-success")
          )
        )
      )
    )
  )
}

qc_plotServer <- function(id, req_inputs, color_coded) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Reactive values for error handling
      error_state <- reactiveVal(FALSE)
      error_message <- reactiveVal("")

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

      # Function to show notifications
      show_notification <- function(title, text, type = "default") {
        shinyalert::shinyalert(
          title = title,
          text = text,
          type = type,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          timer = 5000,
          showConfirmButton = TRUE,
          animation = "slide-from-top"
        )
      }

      # Validate required inputs
      observe({
        validate_inputs <- function() {
          if (is.null(req_inputs())) {
            error_state(TRUE)
            error_message("Required input data is missing")
            return(FALSE)
          }

          if (is.null(color_coded())) {
            error_state(TRUE)
            error_message("Color coded data is missing")
            return(FALSE)
          }

          # Check if necessary fields exist in req_inputs
          required_fields <- c("drop_down", "geno_call_id", "snp_id")
          missing_fields <- required_fields[!required_fields %in% names(req_inputs())]
          if (length(missing_fields) > 0) {
            error_state(TRUE)
            error_message(paste("Missing required fields in input data:", paste(missing_fields, collapse = ", ")))
            return(FALSE)
          }

          # More specific validations can be added here

          # If all checks pass, clear error state
          error_state(FALSE)
          error_message("")
          return(TRUE)
        }

        # Run validation
        validate_inputs()
      })

      # Get dropdown choices with error handling
      drop_down_choices <- reactive({
        if (is.null(req_inputs())) {
          return(NULL)
        }

        tryCatch(
          {
            if (!("drop_down" %in% names(req_inputs()))) {
              error_state(TRUE)
              error_message("The 'drop_down' field is missing in the required inputs")
              return(NULL)
            }

            choices <- req_inputs()$drop_down
            if (length(choices) == 0) {
              error_state(TRUE)
              error_message("No column choices available in the data")
              return(NULL)
            }

            return(choices)
          },
          error = function(e) {
            error_state(TRUE)
            error_message(paste("Error retrieving dropdown choices:", e$message))
            return(NULL)
          }
        )
      })

      # Get plate field choices with error handling
      plates_field_choices <- reactive({
        if (is.null(color_coded())) {
          return(NULL)
        }

        tryCatch(
          {
            choices <- names(color_coded())
            if (length(choices) == 0) {
              error_state(TRUE)
              error_message("No plates available in the color-coded data")
              show_notification(
                title = "Warning",
                text = "No plates found in the data",
                type = "warning"
              )
              return(NULL)
            }

            return(choices)
          },
          error = function(e) {
            error_state(TRUE)
            error_message(paste("Error retrieving plate choices:", e$message))
            return(NULL)
          }
        )
      })

      # Populate the columns for FAM, HEX, and plate choices
      observeEvent(drop_down_choices(), {
        if (is.null(drop_down_choices()) || is.null(plates_field_choices())) {
          return(NULL)
        }

        tryCatch(
          {
            # Best guesses for default selections
            fam_default <- if ("X" %in% drop_down_choices()) "X" else drop_down_choices()[1]
            hex_default <- if ("Y" %in% drop_down_choices()) "Y" else drop_down_choices()[2]
            group_default <- if ("Group" %in% drop_down_choices()) "Group" else "None"

            # Update select inputs
            updateSelectInput(session, inputId = "fam_id", choices = drop_down_choices(), selected = fam_default)
            updateSelectInput(session, inputId = "Hex_id", choices = drop_down_choices(), selected = hex_default)
            updateSelectInput(session,
              inputId = "plate_choice", choices = plates_field_choices(),
              selected = if (length(plates_field_choices()) > 0) plates_field_choices()[1] else NULL
            )
            updateSelectInput(session, inputId = "group_id", choices = c("None", drop_down_choices()), selected = group_default)
          },
          error = function(e) {
            error_state(TRUE)
            error_message(paste("Error updating dropdown menus:", e$message))
          }
        )
      })

      # Validate color selections
      observe({
        req(input$pred_col_id)
        if (length(input$pred_col_id) < 4) {
          error_state(TRUE)
          error_message("Please select all 4 colors for prediction legend")
          show_notification(
            title = "Input Error",
            text = "Please select all 4 colors for prediction legend",
            type = "error"
          )
        } else {
          error_state(FALSE)
          error_message("")
        }
      })

      # Reactive function to generate QC plot 1 without prediction
      qc_plots <- reactive({
        # Check for required inputs
        if (is.null(input$fam_id) || is.null(input$Hex_id) ||
          is.null(input$plate_choice) || is.null(input$expand_id) ||
          is.null(input$scale) || is.null(input$textsize_id) ||
          is.null(input$alpha_id) || is.null(input$legendx_id) ||
          is.null(input$legendy_id) || is.null(input$legend_pos) ||
          is.null(input$legend_box)) {
          return(NULL)
        }

        if (is.null(color_coded()) || is.null(req_inputs())) {
          return(NULL)
        }

        # Set busy spinner
        shinybusy::show_spinner()

        tryCatch({
          if (input$group_id == "None") {
            # Generate the QC plot without group ID
            result <- kasp_qc_ggplot(
              x = color_coded(),
              FAM = input$fam_id,
              HEX = input$Hex_id,
              geno_call = req_inputs()$geno_call_id,
              snp_id = req_inputs()$snp_id,
              Group_id = NULL,
              expand_axis = input$expand_id,
              scale = input$scale,
              text_size = input$textsize_id,
              alpha = input$alpha_id,
              legend.pos.x = input$legendx_id,
              legend.pos.y = input$legendy_id,
              legend.pos = input$legend_pos,
              legend.box = input$legend_box
            )
          } else {
            # Generate the QC plot with group ID
            result <- kasp_qc_ggplot(
              x = color_coded(),
              FAM = input$fam_id,
              HEX = input$Hex_id,
              geno_call = req_inputs()$geno_call_id,
              snp_id = req_inputs()$snp_id,
              Group_id = input$group_id,
              expand_axis = input$expand_id,
              scale = input$scale,
              text_size = input$textsize_id,
              alpha = input$alpha_id,
              legend.pos.x = input$legendx_id,
              legend.pos.y = input$legendy_id,
              legend.pos = input$legend_pos,
              legend.box = input$legend_box
            )
          }

          # Validate result
          if (is.null(result) || length(result) == 0) {
            error_state(TRUE)
            error_message("Failed to generate QC plots: Empty result returned")
            show_notification(
              title = "Error",
              text = "Failed to generate QC plots. Check your input parameters and try again.",
              type = "error"
            )
            return(NULL)
          }

          # Validate that the selected plate exists in the result
          if (!input$plate_choice %in% names(result)) {
            error_state(TRUE)
            error_message(paste("Selected plate", input$plate_choice, "not found in results"))
            show_notification(
              title = "Error",
              text = paste("Selected plate", input$plate_choice, "not found in results"),
              type = "error"
            )
            return(NULL)
          }

          return(result)
        }, error = function(e) {
          error_state(TRUE)
          error_message(paste("Error generating QC plots:", e$message))
          show_notification(
            title = "Error",
            text = paste("Error generating QC plots:", e$message),
            type = "error"
          )
          return(NULL)
        }, finally = {
          shinybusy::hide_spinner()
        })
      })

      # Render the first QC plot with error handling
      output$qc_plot1 <- renderPlot({
        if (error_state()) {
          return(NULL)
        }

        result <- qc_plots()
        if (is.null(result) || is.null(input$plate_choice) || !input$plate_choice %in% names(result)) {
          # Return an empty plot with message if no valid data
          return(
            ggplot2::ggplot() +
              ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No plot data available. Please check your inputs.", size = 5) +
              ggplot2::theme_minimal() +
              ggplot2::theme(panel.grid = ggplot2::element_blank())
          )
        }

        tryCatch(
          {
            return(result[[input$plate_choice]])
          },
          error = function(e) {
            error_state(TRUE)
            error_message(paste("Error rendering QC plot 1:", e$message))
            return(
              ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 5) +
                ggplot2::theme_minimal() +
                ggplot2::theme(panel.grid = ggplot2::element_blank())
            )
          }
        )
      })

      # Reactive function for plot with predictions
      qc_int <- reactive({
        # Check for required inputs
        if (is.null(input$fam_id) || is.null(input$Hex_id) ||
          is.null(input$plate_choice) || is.null(input$expand_id) ||
          is.null(input$scale) || is.null(input$textsize_id) ||
          is.null(input$alpha_id) || is.null(input$legendx_id) ||
          is.null(input$legendy_id) || is.null(input$legend_pos) ||
          is.null(input$legend_box) || is.null(input$pred_col_id)) {
          return(NULL)
        }

        if (is.null(color_coded()) || is.null(req_inputs())) {
          return(NULL)
        }

        # Validate color selections
        if (length(input$pred_col_id) < 4) {
          error_state(TRUE)
          error_message("Please select all 4 colors for prediction legend")
          return(NULL)
        }

        # Set busy spinner
        shinybusy::show_spinner()

        tryCatch({
          if (input$group_id == "None") {
            # Generate plot without group ID
            result <- kasp_qc_ggplot2(
              x = color_coded(),
              FAM = input$fam_id,
              HEX = input$Hex_id,
              geno_call = req_inputs()$geno_call_id,
              snp_id = req_inputs()$snp_id,
              Group_id = NULL,
              legend.pos.x = input$legendx_id,
              legend.pos.y = input$legendy_id,
              legend.box = input$legend_box,
              alpha = input$alpha_id,
              text_size = input$textsize_id,
              legend.pos = input$legend_pos,
              scale = input$scale,
              pred_cols = c(
                "Blank" = input$pred_col_id[1], "False" = input$pred_col_id[2],
                "True" = input$pred_col_id[3], "Unverified" = input$pred_col_id[4]
              ),
              expand_axis = input$expand_id
            )
          } else {
            # Generate plot with group ID
            result <- kasp_qc_ggplot2(
              x = color_coded(),
              FAM = input$fam_id,
              HEX = input$Hex_id,
              geno_call = req_inputs()$geno_call_id,
              snp_id = req_inputs()$snp_id,
              Group_id = input$group_id,
              legend.pos.x = input$legendx_id,
              legend.pos.y = input$legendy_id,
              legend.box = input$legend_box,
              alpha = input$alpha_id,
              text_size = input$textsize_id,
              legend.pos = input$legend_pos,
              scale = input$scale,
              pred_cols = c(
                "Blank" = input$pred_col_id[1], "False" = input$pred_col_id[2],
                "True" = input$pred_col_id[3], "Unverified" = input$pred_col_id[4]
              ),
              expand_axis = input$expand_id
            )
          }

          # Validate result
          if (is.null(result) || length(result) == 0) {
            error_state(TRUE)
            error_message("Failed to generate QC plots with predictions: Empty result returned")
            show_notification(
              title = "Error",
              text = "Failed to generate QC plots with predictions. Check your input parameters and try again.",
              type = "error"
            )
            return(NULL)
          }

          # Validate that the selected plate exists in the result
          if (!input$plate_choice %in% names(result)) {
            error_state(TRUE)
            error_message(paste("Selected plate", input$plate_choice, "not found in results with predictions"))
            return(NULL)
          }

          return(result)
        }, error = function(e) {
          error_state(TRUE)
          error_message(paste("Error generating QC plots with predictions:", e$message))
          show_notification(
            title = "Error",
            text = paste("Error generating QC plots with predictions:", e$message),
            type = "error"
          )
          return(NULL)
        }, finally = {
          shinybusy::hide_spinner()
        })
      })

      # Render the second QC plot with error handling
      output$qc_plot2 <- renderPlot({
        if (error_state()) {
          return(NULL)
        }

        result <- qc_int()
        if (is.null(result) || is.null(input$plate_choice) || !input$plate_choice %in% names(result)) {
          # Return an empty plot with message if no valid data
          return(
            ggplot2::ggplot() +
              ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No plot data available. Please check your inputs.", size = 5) +
              ggplot2::theme_minimal() +
              ggplot2::theme(panel.grid = ggplot2::element_blank())
          )
        }

        tryCatch(
          {
            return(result[[input$plate_choice]])
          },
          error = function(e) {
            error_state(TRUE)
            error_message(paste("Error rendering QC plot 2:", e$message))
            return(
              ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message), size = 5) +
                ggplot2::theme_minimal() +
                ggplot2::theme(panel.grid = ggplot2::element_blank())
            )
          }
        )
      })

      # Download Plot 1
      output$download_plot1 <- downloadHandler(
        filename = function() {
          # Sanitize filename
          clean_name <- gsub("[^[:alnum:]_-]", "_", input$file_name1)
          paste0(clean_name, ".", input$file_type1)
        },
        content = function(file) {
          if (is.null(qc_plots()) || is.null(input$plate_choice) || !input$plate_choice %in% names(qc_plots())) {
            stop("No plot data available for download")
          }

          tryCatch(
            {
              # Set width and height based on plot type
              width <- 10
              height <- 8

              ggplot2::ggsave(
                filename = file,
                plot = qc_plots()[[input$plate_choice]],
                device = input$file_type1,
                width = width,
                height = height,
                units = "in",
                dpi = 300
              )

              show_notification(
                title = "Success",
                text = paste("Plot downloaded successfully as", basename(file)),
                type = "success"
              )
            },
            error = function(e) {
              show_notification(
                title = "Download Error",
                text = paste("Failed to download plot:", e$message),
                type = "error"
              )
              stop(e)
            }
          )
        }
      )

      # Download Plot 2
      output$download_plot2 <- downloadHandler(
        filename = function() {
          # Sanitize filename
          clean_name <- gsub("[^[:alnum:]_-]", "_", input$file_name2)
          paste0(clean_name, ".", input$file_type2)
        },
        content = function(file) {
          if (is.null(qc_int()) || is.null(input$plate_choice) || !input$plate_choice %in% names(qc_int())) {
            stop("No plot data available for download")
          }

          tryCatch(
            {
              # Set width and height based on plot type
              width <- 10
              height <- 8

              ggplot2::ggsave(
                filename = file,
                plot = qc_int()[[input$plate_choice]],
                device = input$file_type2,
                width = width,
                height = height,
                units = "in",
                dpi = 300
              )

              show_notification(
                title = "Success",
                text = paste("Plot downloaded successfully as", basename(file)),
                type = "success"
              )
            },
            error = function(e) {
              show_notification(
                title = "Download Error",
                text = paste("Failed to download plot:", e$message),
                type = "error"
              )
              stop(e)
            }
          )
        }
      )

      # Return reactive values that might be useful for debugging or further development
      return(list(
        error_state = error_state,
        qc_plots = qc_plots,
        qc_int = qc_int
      ))
    }
  )
}



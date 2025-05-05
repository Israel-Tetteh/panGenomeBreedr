# Functions from marker validation used
#-> pred_summary()

# Module for predictive summary and status
source("Helper functions/utils_Marker_Validation.R")

#Module ui
pred_sum_statUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = ns("snp_id"),
          label = "Select Column for SNP ID",
          choices = NULL,
          multiple = FALSE
        ),
        selectInput(
          inputId = ns("geno_call_id"),
          label = "Select Column for Genotype Calls",
          choices = NULL,
          multiple = FALSE
        ),
        selectInput(
          inputId = ns("group_id"),
          label = "Select Column for Group ID",
          choices = NULL,
          multiple = FALSE
        ),
        textInput(inputId = ns('group_unknown'),
                  label = 'Unverified Genotype Indicator',
                  value = '?'
        ),
        textInput(inputId = ns('blank'),
                  label = 'Specify No Template Control Calls',
                  value = 'NTC'
        ),
        bslib::input_switch(
          id = ns("rate_out_id"),
          label = "Return Proportions",
          value = TRUE
        ),
        div(
          style = "display: flex; justify-content: center;",
          actionButton(
            inputId = ns("run"), label = "Predict",
            icon = icon("rocket"), class = "btn-primary", width = "50%"
          )
        )
      ),
      mainPanel(
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Predictive Summary for Positive Controls in KASP Genotype Data",
            DT::dataTableOutput(outputId = ns("predict_sum"))
          )
        ), br(),
        shiny::conditionalPanel(
          condition = "input.rate_out_id == true",
          ns = ns,
          bslib::accordion(
            bslib::accordion_panel(
              title = "Predictive Summary Plot",
              fluidRow(
                column(
                  width = 4,
                  selectInput(inputId = ns("SNP_id"), label = "Choose SNP ID", choices = NULL, multiple = FALSE)
                ),
                column(
                  width = 4,
                  selectInput(
                    inputId = ns("pred_col_id"), label = "Pos. Ctrl Colors (F | T | U)",
                    choices = colors(),
                    selected = c("red", "blue", "orange"),
                    multiple = TRUE
                  )
                ),
                column(
                  width = 4,
                  numericInput(inputId = ns("alpha_id"), label = "Adjust Alpha Value", value = 1, min = 0, max = 1, step = 0.05)
                )
              ),
              fluidRow(
                column(
                  width = 4,
                  numericInput(inputId = ns("height_id"), label = "Adjust Plot Height", value = 5, min = 1, max = 30, step = 1)
                ),
                column(
                  width = 4,
                  numericInput(inputId = ns("width_id"), label = "Adjust Plot Width", value = 8, min = 1, max = 30, step = 1)
                ),
                column(
                  width = 4,
                  numericInput(inputId = ns("textsize_id"), label = "Text Size", value = 12, min = 1, max = 30, step = 1)
                )
              ), br(),
              plotOutput(outputId = ns("pred_plot")), hr(),
              textInput(inputId = ns("file_name"), label = "Enter Filename", value = "Predictive_plot"),
              downloadButton(outputId = ns("download_pred_plot"), label = "Download Plot", class = "btn-success")
            )
          )
        ), br(),
        # conditionalPanel(condition ='input.pred_stat',
        bslib::accordion(
          bslib::accordion_panel(
            title = "Predictive Status for Positive Controls in KASP Genotype Data",
            selectInput(inputId = ns("pred_stat_dd"), label = "Select Plate ID", choices = NULL, multiple = FALSE), DT::dataTableOutput(outputId = ns("predict_stat"))
          )
        ), br(), br()
      )
    )
  )
}

pred_sum_statServer <- function(id, color_code_res , kasp_data) {
  moduleServer(
    id,
    function(input, output, session) {
      # Script to give the user drop down inputs for the select input tabs.
      drop_down <- reactive({
        req(kasp_data)
        colnames(kasp_data)
      })

      # Populate the select input values.
      observeEvent(drop_down(), {
        req(drop_down())
        updateSelectInput(session, inputId = "snp_id", choices = drop_down(), selected = "SNPID")
        updateSelectInput(session, inputId = "geno_call_id", choices = drop_down(), selected = "Call")
        updateSelectInput(session, inputId = "group_id", choices = drop_down(), selected = "Group")
      })

      pred_sum_result <- reactiveVal()

      # Store result in the reactive value when "Run" button is clicked
      observeEvent(input$run, {
        req(input$group_id, input$geno_call_id, input$snp_id, color_code_res())

        # Execute prediction function
        pred_result <- tryCatch(
          {
            pred_summary(
              x = color_code_res() ,
              Group_unknown = input$group_unknown ,
              blank = input$blank,
              snp_id = input$snp_id,
              Group_id = input$group_id,
              geno_call = input$geno_call_id,
              rate_out = input$rate_out_id %||% FALSE
            )
          },
          error = function(e) {
            print(paste("Error in pred_summary:", e))
            return(NULL)
          }
        )

        # Ensure result is valid before storing
        req(pred_result)
        pred_sum_result(pred_result)
      })

      # Extract plate names (ensure pred_sum_result() has valid data)
      plates_field <- reactive({
        req(pred_sum_result())
        names(pred_sum_result()$plates)
      })

      # Update dropdown for plates selection
      observeEvent(plates_field(), {
        req(plates_field())
        updateSelectInput(session, inputId = "pred_stat_dd", choices = plates_field(), selected = plates_field()[1])
      })

      # Get predictive status based on user's selected plate
      pred_stat_prime <- reactive({
        req(pred_sum_result(), input$pred_stat_dd)

        # Ensure selected plate exists before accessing
        if (input$pred_stat_dd %in% names(pred_sum_result()$plates)) {
          return(pred_sum_result()$plates[[input$pred_stat_dd]])
        } else {
          print("Error: Selected plate not found in pred_sum_result")
          return(NULL)
        }
      })

      # Display predictive status
      output$predict_stat <- DT::renderDT({
        req(pred_stat_prime())
        DT::datatable(pred_stat_prime(), options = list(pageLength = 10, scrollX = TRUE))
      })

      # Display prediction summary
      output$predict_sum <- DT::renderDT({
        req(pred_sum_result()$summ)
        DT::datatable(as.data.frame(pred_sum_result()$summ), options = list(pageLength = 10, scrollX = TRUE))
      })

      # Generating predictive summary plots. returns a list object containing plots
      pred_sum_plot <- reactive({
        req(
          pred_sum_result()$summ, input$pred_col_id, input$textsize_id, input$height_id,
          input$alpha_id, input$width_id
        )
        panGenomeBreedr::pred_summary_plot(
          x = as.data.frame(pred_sum_result()$summ),
          pred_cols = c(
            "false" = input$pred_col_id[1], "true" = input$pred_col_id[2],
            "unverified" = input$pred_col_id[3]
          ),
          alpha = input$alpha_id,
          width = input$width_id,
          height = input$height_id,
          text_size = input$textsize_id
        )
      })

      # Get unique SNP names from summary.
      name_snpid <- reactive({
        req(pred_sum_plot())
        names(pred_sum_plot())
      })

      # Update dropdown for SNP selection
      observeEvent(pred_sum_plot(), {
        req(pred_sum_plot())
        updateSelectInput(session, inputId = "SNP_id", choices = name_snpid(), selected = name_snpid()[1])
      })

      # Dynamic plots
      plot_me <- reactive({
        req(pred_sum_plot(), input$SNP_id)
        pred_sum_plot()[[input$SNP_id]]
      })

      # Plot predictive plot
      output$pred_plot <- renderPlot({
        req(plot_me())
        plot_me()
      })

      # Download Plot
      output$download_pred_plot <- downloadHandler(
        filename = function() {
          req(input$file_name)
          paste0(input$file_name, ".pdf")
        },
        content = function(file) {
          req(plot_me(), input$width_id, input$height_id)

          pdf(file, width = as.numeric(input$width_id), height = as.numeric(input$height_id))
          grid::grid.draw(plot_me()) # Draw the grid structure
          dev.off()
        }
      )

      # Return inputs from pred sum to use for qcplot.
      return(reactive({
        list(
          snp_id = input$snp_id,
          geno_call_id = input$geno_call_id,
          group_id = input$group_id,
          plates_field = plates_field(),
          drop_down = drop_down()
        )
      }))
    }
  )
}

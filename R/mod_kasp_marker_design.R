#' KASP Marker Design UI Function
#'
#' @description A Shiny module for KASP marker design
#'
#' @param id Internal parameter for {shiny}.
#'
#' @return A UI for KASP Marker Design
#'
#' @noRd
#'
#' @importFrom shiny NS tagList radioButtons
#' @importFrom bslib layout_sidebar sidebar card card_header card_footer input_switch accordion accordion_panel
#' @importFrom DT dataTableOutput
#'
mod_kasp_marker_design_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "KASP Marker Design Parameters",
        width = 400,

        # File Uploads Card
        bslib::card(
          bslib::card_header(tags$b("File Uploads")),
          fileInput(ns("genome_file"),
                    label = "Genome File (.fa, .fasta, .gz)",
                    accept = c(".fa", ".fasta", ".gz")
          ),
          radioButtons(
            inputId = ns("upload_choice"),
            label = "VCF File Type",
            choices = c("Raw VCF File", "Processed VCF File"),
            selected = character(0)
          ),
          uiOutput(ns("choice_output")),
        ),

        # Column Mapping Card
        bslib::card(
          bslib::card_header(tags$b("Column Mapping")),
          selectInput(ns("variant_id_col"),
                      label = "Variant IDs Column",
                      choices = NULL
          ),
          selectInput(ns("chrom_col"),
                      label = "Chromosome Column",
                      choices = NULL
          ),
          selectInput(ns("pos_col"),
                      label = "Position Column",
                      choices = NULL
          ),
          selectInput(ns("ref_al_col"),
                      label = "Reference Allele Column",
                      choices = NULL
          ),
          selectInput(ns("alt_al_col"),
                      label = "Alternate Allele Column",
                      choices = NULL
          ),
          numericInput(ns("geno_start"),
                       label = "Genotype Data Start Column",
                       value = 1
          )
        ),

        # Marker Selection Card
        bslib::card(
          bslib::card_header(tags$b("Marker Selection")),
          selectInput(ns("chr_ID"),
                      label = "Chromosome ID",
                      choices = NULL
          ),
          selectInput(ns("marker_ID"),
                      label = "Marker ID",
                      choices = NULL
          ),
          textInput(ns("reg_name"),
                    label = "Region Name",
                    placeholder = "e.g., drought resistance locus"
          )
        ),

        # Analysis Parameters Card
        bslib::card(
          bslib::card_header(tags$b("Analysis Parameters")),
          numericInput(ns("maf"),
                       label = "Minor Allele Frequency (MAF)",
                       value = 0.05, min = 0, max = 1
          ),
          bslib::input_switch(ns("draw_plot"),
                              label = "Generate Alignment Plot",
                              value = TRUE
          )
        ),
        bslib::card_footer(
          div(
            style = "text-align: center; margin-top: 15px;",
            actionButton(ns("run_but"),
                         label = "Design Marker",
                         icon = icon("drafting-compass"),
                         class = "btn-primary",
                         width = "100%"
            )
          )
        )
      ),
      # Main panel
      # Results container
      bslib::accordion(
        style = "margin-bottom: 70px;",
        id = ns("results_accordion"),
        width = "100%",
        open = TRUE,
        bslib::accordion_panel(
          "Comprehensive Table of KASP Marker Design Data and DNA Sequence Alignment to the Reference Genome",
          DT::dataTableOutput(ns("kasp_table")),
          bslib::card(
            bslib::card_footer(
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
                    label = "Enter File Prefix",
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
          )
        )
      ),

      # Conditional plot panel
      uiOutput(ns("plot_container"))
    )
  )
}

#' KASP Marker Design Server Function
#'
#' @description Server logic for KASP marker design module
#'
#' @param id Internal parameter for {shiny}.
#' @param input,output,session Internal parameters for {shiny}.
#' @importFrom shiny NS moduleServer observeEvent renderUI req reactiveVal
#' @importFrom shiny div icon tags strong
#' @importFrom shinyjs toggleState delay
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom shinyWidgets show_alert
#' @importFrom DT renderDT datatable
#' @importFrom utils read.table
#' @importFrom readxl read_excel
#' @importFrom openxlsx write.xlsx
#' @noRd
#'
mod_kasp_marker_design_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Server logic goes here
    observeEvent(input$upload_choice, {
      if (input$upload_choice == "Raw VCF File") {
        output$choice_output <- renderUI({
          fileInput(ns("vcf_file"),
                    label = "Raw VCF File (.vcf)",
                    accept = ".vcf"
          )
        })
      } else {
        output$choice_output <- renderUI({
          fileInput(ns("gt_df"),
                    label = "Processed VCF Data (.xlsx)",
                    accept = ".xlsx"
          )
        })
      }
    })
    # Reactive values
    kasp_des.result <- reactiveVal(NULL)

    # Read VCF data
    vcf_data <- reactive({
      if (is.null(input$vcf_file)) {
        return(NULL)
      }
      result <- marker.chr_ID(input$vcf_file$datapath)
      return(result)
    })

    # Update other parameter column for user.
    read_vcf_as_df <- function(vcf_file) {
      # Read VCF header to get column names
      header_lines <- readLines(vcf_file)
      header <- header_lines[grep("^#CHROM", header_lines)]
      colnames <- strsplit(header, "\t")[[1]]
      colnames[1] <- "CHROM" # fix formatting

      # Read VCF data (skip header lines)
      vcf_df <- utils::read.table(vcf_file,
                                  comment.char = "#", header = FALSE, sep = "\t",
                                  col.names = colnames, stringsAsFactors = FALSE
      )

      return(vcf_df)
    }

    # Reactive expression to populate inputs
    vcf_colnames <- reactive({
      req(input$vcf_file)
      colnames(read_vcf_as_df(input$vcf_file$datapath))
    })

    # Read Excel file
    gt_data <- reactive({
      req(input$gt_df)
        tryCatch({
              result <- readxl::read_excel(input$gt_df$datapath)

              return(as.data.frame(result))
          },
          error = function(e) {

             shinyWidgets::show_toast(title = paste("Error reading Excel file:", e$message),
                                      type = 'error',
                                      timer = 2000,
                                      position = 'bottom-end')
            NULL
          }
        )
    })


    # Extract column names
    gt_colnames <- reactive({
      req(gt_data())
      colnames(gt_data())[1:6]
    })

    # Extract unique chromosomes
    unique_chrom <- reactive({
      req(gt_data(), gt_colnames())

      unique(gt_data()[[gt_colnames()[2]]])
    })

    # Extract unique marker IDs
    unique_marker_id <- reactive({
      req(gt_data(), gt_colnames())

      unique(gt_data()[[gt_colnames()[1]]])
    })

    # Update column selection dropdowns
    observe({
      if (!is.null(input$vcf_file)) {
        # For VCF files
        updateSelectInput(session, "variant_id_col",
                          choices = vcf_colnames(),
                          selected = vcf_colnames()[grep("id", x = vcf_colnames(), ignore.case = TRUE)[1]]
        )

        updateSelectInput(session, "chrom_col",
                          choices = vcf_colnames(),
                          selected = vcf_colnames()[grep("chro", x = vcf_colnames(), ignore.case = TRUE)[1]]
        )

        updateSelectInput(session, "pos_col",
                          choices = vcf_colnames(),
                          selected = vcf_colnames()[grep("pos", x = vcf_colnames(), ignore.case = TRUE)[1]]
        )

        updateSelectInput(session, "ref_al_col",
                          choices = vcf_colnames(),
                          selected = vcf_colnames()[grep("ref", x = vcf_colnames(), ignore.case = TRUE)[1]]
        )

        updateSelectInput(session, "alt_al_col",
                          choices = vcf_colnames(),
                          selected = vcf_colnames()[grep("alt", x = vcf_colnames(), ignore.case = TRUE)[1]]
        )
      } else if (!is.null(gt_data())) {
        # For Excel files
        updateSelectInput(session, "variant_id_col",
                          choices = gt_colnames(),
                          selected = gt_colnames()[grep("id", x = gt_colnames(), ignore.case = TRUE)[1]]
        )

        updateSelectInput(session, "chrom_col",
                          choices = gt_colnames(),
                          selected = gt_colnames()[grep("chro", x = gt_colnames(), ignore.case = TRUE)[1]]
        )

        updateSelectInput(session, "pos_col",
                          choices = gt_colnames(),
                          selected = gt_colnames()[grep("pos", x = gt_colnames(), ignore.case = TRUE)[1]]
        )

        updateSelectInput(session, "ref_al_col",
                          choices = gt_colnames(),
                          selected = gt_colnames()[grep("ref", x = gt_colnames(), ignore.case = TRUE)[1]]
        )

        updateSelectInput(session, "alt_al_col",
                          choices = gt_colnames(),
                          selected = gt_colnames()[grep("alt", x = gt_colnames(), ignore.case = TRUE)[1]]
        )
      }
    })

    # Update marker/chromosome dropdowns vcf
    observe({
      req(vcf_data())
        updateSelectInput(session, "marker_ID",
                          choices = vcf_data()$vcf_matrix_markerID)

        updateSelectInput(session, "chr_ID",
                          choices = vcf_data()$vcf_matrix_chromID)

    })

    # Update marker/chromosome dropdowns excel
    observe({
      req(gt_data())
        # Excel file logic
        req(unique_chrom(), unique_marker_id())

        updateSelectInput(session, "marker_ID",
                          choices = unique_marker_id()
        )
        updateSelectInput(session, "chr_ID",
                          choices = unique_chrom()
        )
    })



    # Validate inputs before enabling the run button
    observe({
      # Check which input type is being used (VCF or processed Excel)
      using_vcf <- !is.null(input$vcf_file)
      using_excel <- !is.null(gt_data())

      # Validate based on input type
      is_valid <- if (using_vcf) {
        # VCF file validation
        !is.null(input$genome_file) &&
          !is.null(input$chr_ID) &&
          !is.null(input$marker_ID) &&
          !is.na(input$maf) &&
          input$maf >= 0 &&
          input$maf <= 1 &&
          !is.null(input$variant_id_col) &&
          !is.null(input$chrom_col)
      } else if (using_excel) {
        # Excel file validation
        !is.null(input$genome_file) &&
          !is.null(input$chr_ID) &&
          !is.null(input$marker_ID) &&
          !is.na(input$maf) &&
          input$maf >= 0 &&
          input$maf <= 1 &&
          !is.null(input$variant_id_col) &&
          !is.null(input$chrom_col)
      } else {
        FALSE # Neither file type provided
      }

      shinyjs::toggleState("run_but", condition = is_valid)
    })



    # Check which input is available and make use of it
    observeEvent(input$run_but, {
      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "#0dc5c1",
        text = "Designing KASP Marker... Please wait."
      )

      tryCatch({
        if (!is.null(input$vcf_file)) {
          result_data <- kasp_marker_design_alt(
            vcf_file = input$vcf_file$datapath,
            gt_df = NULL,
            variant_id_col = input$variant_id_col,
            chrom_col = input$chrom_col,
            pos_col = input$pos_col,
            ref_al_col = input$ref_al_col,
            alt_al_col = input$alt_al_col,
            geno_start = input$geno_start,
            marker_ID = input$marker_ID,
            chr = input$chr_ID,
            genome_file = input$genome_file$datapath,
            plot_file = tempdir(),
            plot_draw = if (is.null(input$draw_plot)) FALSE else input$draw_plot,
            region_name = input$reg_name,
            maf = input$maf
          )
          kasp_des.result(result_data)
        } else if (!is.null(gt_data())) {
          result_data <- kasp_marker_design_alt(
            vcf_file = NULL,
            gt_df = gt_data(),
            variant_id_col = input$variant_id_col,
            chrom_col = input$chrom_col,
            pos_col = input$pos_col,
            ref_al_col = input$ref_al_col,
            alt_al_col = input$alt_al_col,
            geno_start = input$geno_start,
            marker_ID = input$marker_ID,
            chr = input$chr_ID,
            genome_file = input$genome_file$datapath,
            plot_file = tempdir(),
            region_name = input$reg_name,
            maf = input$maf,
            plot_draw = if (is.null(input$draw_plot)) FALSE else input$draw_plot
          )
          kasp_des.result(result_data)
        }
        shinyjs::delay(100, {
          show_alert(
            title = "Success!",
            text = "KASP marker designed successfully",
            type = "success",
            showCloseButton = TRUE,
            timer = 5000
          )
        })
      }, error = function(e) {
        kasp_des.result(NULL)
        shinyjs::delay(100, {
          show_alert(
            title = "Error!",
            text = "Failed to design KASP",
            type = "error",
            showCloseButton = TRUE,
            timer = 5000
          )
        })
      }, finally = {
        shinybusy::remove_modal_spinner()
      })
    })


    # Render KASP table
    output$kasp_table <- DT::renderDT({
      req(kasp_des.result()$marker_data)
      DT::datatable(kasp_des.result()$marker_data,
                    options = list(pageLength = 10, scrollX = TRUE)
      )
    })


    # Download Table
    output$download_table <- downloadHandler(
      filename = function() {
        # Clean file name
        clean_name <- gsub("[^[:alnum:]_-]", "_", input$file_name)
        paste0(clean_name, input$exten)
      },
      content = function(file) {
        if (input$exten == ".csv") {
          write.csv(kasp_des.result()$marker_data, file, row.names = FALSE)
        } else if (input$exten == ".xlsx") {
          openxlsx::write.xlsx(kasp_des.result()$marker_data, file)
        }
      }
    )



    # Plot container UI - show if user selects true
    observeEvent(input$draw_plot, {
      if (input$draw_plot == TRUE) {
        output$plot_container <- renderUI({
          bslib::accordion(
            width = "100%",
            open = TRUE,
            bslib::accordion_panel(
              "KASP Sequence Alignment: 100 bp Upstream and Downstream of Target Site",
              plotOutput(ns("plot")),
              downloadButton(ns("download_plot"),
                             label = "Download Plot (pdf)",
                             class = "btn-success", icon = icon("download")
              )
            )
          )
        })
      } else {
        output$plot_container <- renderUI({
          NULL
        })
      }
    })

    # Render Plot
    output$plot <- renderPlot({
      req(kasp_des.result())

      kasp_des.result()$plot # show plot
    })

    # Download Plot
    output$download_plot <- downloadHandler(
      filename = function() {
        # Clean marker ID for filename
        clean_marker <- gsub("[^[:alnum:]_-]", "_", input$marker_ID)
        paste0("alignment_", clean_marker, ".pdf")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file,
          plot = kasp_des.result()$plot,
          device = "pdf",
          width = 24,
          height = 9,
          units = "in"
        )
      }
    )
  })
}

## To be copied in the UI
# mod_kasp_marker_design_ui("kasp_marker_design_1")

## To be copied in the server
# mod_kasp_marker_design_server("kasp_marker_design_1")

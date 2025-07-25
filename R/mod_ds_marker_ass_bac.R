#' Decision Support for Marker Assisted Backcrossing (UI)
#'
#' @description A Shiny module UI for analyzing marker data in backcross breeding programs.
#'
#' @param id Unique namespace ID
#'
#' @return A shiny::tagList containing the UI elements
#'
#' @importFrom shiny NS tagList fileInput radioButtons selectInput textInput
#' @importFrom shiny actionButton uiOutput icon plotOutput
#' @importFrom bslib navset_card_underline nav_panel layout_sidebar sidebar
#' @importFrom bslib card card_header card_body card_footer accordion
#' @importFrom bslib accordion_panel navset_card_tab input_switch
#' @noRd
#'
mod_ds_marker_ass_bac_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_card_underline(
      nav_panel(
        title = "Decision Support for MABC",
        icon = icon("flask"),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = 400,
            position = "left",
            bslib::card(
              height = "100%",
              bslib::card_body(
                bslib::card(
                  bslib::card_header(tags$b('Upload Input Files')),
                  bslib::card_body(
                    fileInput(
                      inputId = ns("data_id"),
                      label = "Upload Kasp/Agriplex File",
                      multiple = FALSE,
                      accept = ".csv",
                      width = "100%"
                    ),
                    selectInput(
                      inputId = ns("data_type"),
                      label = "Indicate Data Format",
                      choices = c("agriplex", "Kasp"),
                      selected = "agriplex",
                      width = "100%"
                    ), # user defines data format

                    textInput(
                      inputId = ns("allele_sep"),
                      label = "Enter Allele Separator for Data Format",
                      value = " / ",
                      width = "100%"
                    )
                  )
                ),
                bslib::card(
                  bslib::card_header(tags$b('Mapfile Setup')),
                  bslib::card_body(
                    radioButtons(
                      inputId = ns("choice"),
                      label = "Do you have a map file?",
                      choices = c("Yes" = "yes", "No, generate one for me" = "no"),
                      selected = "yes"
                    ),

                    # Dynamic rendering based on choice
                    conditionalPanel(
                      condition = paste0('input["', ns("choice"), '"] == "yes"'),
                      tagList(
                        fileInput(
                          inputId = ns("mapfile"),
                          label = "Upload Map file",
                          accept = ".csv",
                          width = "100%"
                        ),
                        selectInput(
                          inputId = ns("snp_id"), #
                          label = "Select Column for SNP ID",
                          choices = NULL,
                          width = "100%"
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = paste0('input["', ns("choice"), '"] == "no"'),
                      tagList(
                        textInput(
                          inputId = ns("prefix_marker"),
                          label = "Enter Prefix for Marker ID",
                          value = "S",
                          width = "100%"
                        ),
                        textInput(
                          inputId = ns("sep_marker"),
                          label = "Enter Separator for Marker ID",
                          value = "_",
                          width = "100%"
                        )
                      )
                    )
                  )
                ),

                bslib::card(
                  bslib::card_header(tags$b('Genotype & Batch Settings')),
                  bslib::card_body(
                    # Batch  & genotype settings.
                    selectInput(
                      inputId = ns("genotype_col"),
                      label = "Select Genotype Column",
                      choices = NULL,
                      width = "100%"
                    ),
                    selectInput(
                      inputId = ns("batch_col"),
                      label = "Select Batch Column",
                      choices = NULL,
                      width = "100%"
                    ), # select batch column will populates

                    selectInput(
                      inputId = ns("batch"),
                      label = "Select Focused Batch",
                      choices = NULL,
                      width = "100%"
                    ), # unique batches will come here.

                    uiOutput(ns("marker_sep")), # this must be dynamic based on users choice

                    selectInput(
                      inputId = ns("dp"),
                      label = "Select Donor Parent",
                      choices = NULL,
                      width = "100%"
                    ),
                    selectInput(
                      inputId = ns("rp"),
                      label = "Select Recurrent Parent",
                      choices = NULL,
                      width = "100%"
                    )
                  )
                )
              ),
              bslib::card(
                bslib::card_header(tags$b('Quality Control Switches')),
                bslib::card_body(
                  tagList(
                    bslib::input_switch(
                      id = ns("apply_par_poly"),
                      label = "Remove Monomorphic Parents",
                      value = TRUE
                    ),
                    bslib::input_switch(
                      id = ns("apply_par_miss"),
                      label = "Remove Missing Parent Data",
                      value = TRUE
                    ),
                    bslib::input_switch(
                      id = ns("apply_geno_good"),
                      label = "Apply Genotype Error Check",
                      value = TRUE
                    ),
                    bslib::input_switch(
                      id = ns("apply_par_homo"),
                      label = "Filter Heterozygous Parents",
                      value = TRUE
                    )
                  )
                )
              ),
              bslib::card_footer(
                actionButton(
                  inputId = ns("config"),
                  label = "Submit",
                  icon = icon("check"),
                  width = "100%",
                  #class = "btn-info"
                  style = "background-color: forestgreen; color: white; font-weight: bold; border: none;",
                  `onmouseover` = "this.style.backgroundColor='#145214'",
                  `onmouseout` = "this.style.backgroundColor='forestgreen'"
                )
              )
            )
          ),

          # Main content area
          bslib::input_switch(id = ns("configure"), label = "Configure Plot", value = FALSE),
          conditionalPanel(
            condition = paste0('input["', ns("configure"), '"] == true'),
            fluidRow(
              # Card 1: Heatmap Configuration
              column(
                width = 4,
                bslib::card(
                  max_height = "600px",
                  height = "100%",
                  bslib::card_header(tags$b("RPP Calculation Settings for BC Progenies"), class = "bg-success"),
                  bslib::card_body(
                    selectInput(
                      inputId = ns("snp_ids"),
                      label = "Select Column for SNP ID",
                      choices = NULL,
                      width = "100%"
                    ),
                    selectInput(
                      inputId = ns("chr"),
                      label = "Select Column for Chromosome",
                      choices = NULL,
                      width = "100%"
                    ),
                    selectInput(
                      inputId = ns("chr_pos"),
                      label = "Select Column for Chromosome Position",
                      choices = NULL,
                      width = "100%"
                    ),
                    numericInput(
                      inputId = ns("rp_index"),
                      label = "Set Row Index for Reccurent Parent",
                      value = 1,
                      min = 1,
                      step = 1,
                      width = "100%"
                    ),
                    numericInput(
                      inputId = ns("rp_num_code"),
                      label = "Numeric Code for RP Background",
                      value = 1,
                      min = 1,
                      step = 1,
                      width = "100%"
                    ),
                    numericInput(
                      inputId = ns("het_code"),
                      label = "Numeric Code for Heterozygous Background",
                      value = 0.5,
                      min = 0.5,
                      max = 0.5,
                      width = "100%"
                    ),
                    numericInput(
                      inputId = ns("na_code"),
                      label = "Value Indicating Missing Data",
                      value = -5
                    ),
                    radioButtons(
                      inputId = ns("weight_rpp"),
                      label = "Weight RPP Values?",
                      choices = c("Yes" = TRUE, "No" = FALSE),
                      selected = FALSE,
                      inline = TRUE
                    )
                  )
                )
              ),

              # Card 2: BC Progenies RPP Plot Settings
              column(
                width = 8,
                bslib::card(
                  max_height = "600px",
                  height = "100%",
                  bslib::card_header(tags$b("BC Progenies RPP Plot Settings"), class = "bg-info"),
                  bslib::card_body(
                    fluidRow(
                      # Left Column - 4 widgets
                      column(
                        width = 6,
                        selectInput(
                          inputId = ns("rpp_col"),
                          label = "Select RPP Values Column",
                          choices = NULL,
                          width = "100%"
                        ),
                        selectInput(
                          inputId = ns("rpp_sample_id"),
                          label = "Select Progeny ID Column",
                          choices = NULL,
                          width = "100%"
                        ),
                        numericInput(
                          inputId = ns("bc_gen"),
                          label = "Specify BC Generation for Progenies",
                          value = NULL,
                          min = 1,
                          width = "100%"
                        ),
                        numericInput(
                          inputId = ns("rpp_threshold"),
                          label = "Set RPP Threshold for Selecting BC Progenies ",
                          value = 0.93,
                          min = 0,
                          max = 1,
                          width = "100%"
                        ),
                        selectInput(
                          inputId = ns("thresh_line_col"),
                          label = "Color of Threshold Line",
                          choices = grDevices::colors(),
                          selected = "firebrick",
                          width = "100%"
                        ),
                        bslib::input_switch(
                          id = ns("show_above_thresh"),
                          label = "Show Progenies Above Threshold",
                          value = FALSE
                        )
                      ),
                      # Right Column - 4 widgets
                      column(
                        width = 6,
                        selectInput(
                          inputId = ns("bar_col"),
                          label = "Set Bar Fill Color",
                          choices = grDevices::colors(),
                          selected = "cornflowerblue",
                          width = "100%"
                        ),
                        numericInput(
                          inputId = ns("alpha"),
                          label = "Point Transparency",
                          value = 0.9,
                          min = 0,
                          max = 1,
                          step = 0.1,
                          width = "100%"
                        ),
                        numericInput(
                          inputId = ns("text_size"),
                          label = "Text Size",
                          value = 15,
                          min = 1,
                          width = "100%"
                        ),
                        numericInput(
                          inputId = ns("bar_width"),
                          label = "Set Bar Width",
                          value = 0.5,
                          min = 0.1,
                          width = "100%"
                        ),
                        numericInput(
                          inputId = ns("aspect_ratio"),
                          label = "Set Aspect Ratio of Barplot",
                          value = 0.5,
                          min = 0.1,
                          width = "100%"
                        ),
                        numericInput(
                          inputId = ns("text_scale_fct"),
                          label = "Set Text Size Scaling Factor",
                          value = 0.1,
                          min = 0.1,
                          width = "100%"
                        )
                      )
                    )
                  )
                )
              )
            )

          ),

          # Results Section
          fluidRow(
            column(
              width = 12,
              bslib::accordion(
                bslib::accordion_panel(
                  title = "Heatmap Results & Analysis",
                  icon = icon("chart-line"),
                  bslib::navset_card_tab(
                    bslib::nav_panel(
                      title = "Computed RPP Values",
                      icon = icon("th"),
                      DT::DTOutput(
                        outputId = ns("comp_rpp_val"),
                        width = "100%",
                        height = "600px"
                      )
                    ),
                    bslib::nav_panel(
                      title = "RPP Barplot",
                      icon = icon("tags"),
                          plotOutput(
                            outputId = ns("rpp_bar"),
                            width = "100%",
                            height = "600px"
                          ),
                      bslib::card(card_footer(
                        fluidRow(
                          column(
                            3,
                            textInput(
                              inputId = ns("file_name2"),
                              label = "Enter Filename",
                              value = "rpp_barplot"
                            )
                          ),
                          column(
                            3,
                            numericInput(
                              inputId = ns("width2"),
                              label = "Set Plot Width",
                              value = 8, min = 1
                            )
                          ), column(
                            3,
                            numericInput(
                              inputId = ns("height2"),
                              label = "Set Plot Height",
                              value = 6, min = 1
                            )
                          )
                        ),
                        downloadButton(
                          outputId = ns("download_plot2"),
                          label = "Download Plot", class = "btn-success"
                        )
                      ))

                    )
                  )
                )
              )
            )
          )
        )
      ),
      nav_panel(
        title = "Data QC", icon = icon("bell"),
        splitLayout(
          bslib::card(
            card_header(tags$b("SNP Loci with Potential Genotype Call Errors")),
            bslib::card_body(
              DT::DTOutput(outputId = ns("geno_error_tbl"))
            )
          ),
          bslib::card(
            card_header(tags$b("SNP Loci with Parent Missing")),
            bslib::card_body(
              DT::DTOutput(outputId = ns("par_missing_tbl"))
            )
          )
        ),
        splitLayout(
          bslib::card(
            card_header(tags$b("SNP Loci with Heterozygote Parent")),
            bslib::card_body(
              DT::DTOutput(outputId = ns("par_het_tbl"))
            )
          ),
          bslib::card(
            card_header(tags$b("SNP Loci with Unexpected Locus")),
            bslib::card_body(
              DT::DTOutput(outputId = ns("unexp_locu_tbl"))
            )
          )
        )
      )
    )
  )
}

#' Decision Support for Marker Assisted Backcrossing (Server)
#'
#' @description Server logic for the MABC analysis module
#'
#' @param id Unique namespace ID
#'
#' @importFrom shiny moduleServer observeEvent req reactive renderUI observe
#' @importFrom utils read.csv
#' @importFrom stats setNames
#' @noRd
mod_ds_marker_ass_bac_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Process file.
    # Dynamic ui based on choice of individual.
    # # Dynamic ui based on choice of individual.
    observe({
      if (input$choice == "no") {
        showModal(
          modalDialog(
            title = tags$b("Important Note!!"),
            tagList(
              p("Marker names must follow a structured format to be parsed into the map file."),
              tags$ul(
                tags$li("A common prefix before each marker (e.g. ", tags$b("S"), ")."),
                tags$li("Chromosome number immediately after the prefix (e.g. ", tags$b("1"), ")."),
                tags$li("A separator character (e.g. ", tags$b("_"), ")."),
                tags$li("Position number after the separator (e.g. ", tags$b("101"), ").")
              )
              ,
              p(
                "Example: ", tags$b("S1_101"),
                ".Where ", tags$b("S"), " is the prefix, ",
                tags$b("1"), " is the chromosome number, and ",
                tags$b("101"), " is the position."
              )

            ),
            easyClose = FALSE,
            footer = modalButton("Got it!")
          )
        )
      }
    })


    # Read the csv file uploadeed.
    data <- reactive({
      req(input$data_id)
      read.csv(file = input$data_id$datapath,stringsAsFactors = FALSE, colClasses="character") |> as.data.frame()
    })


    validated_data <- reactive({
      req(data())

      tryCatch({
        check_colnames_validate(data())
        data() # Return the data if validation passes
      }, error = function(e) {
        shinyWidgets::show_alert(
          title = "Column Validation Error",
          text = e$message,
          type = "error"
        )
        NULL # Return NULL if validation fails
      })
    })


    # Get colnames of data and populate.
    data_colnames <- reactive({
      req(validated_data())
      Get_dt_coln(validated_data())
    })

    # Populate batch and genotype columns with data colnames
    observe({
      updateSelectInput(session,
                        inputId = "batch_col",
                        choices = data_colnames(),
                        selected = grep("batch",
                                        x = data_colnames(),
                                        ignore.case = TRUE,
                                        value = TRUE
                        )[1]
      )

      updateSelectInput(session,
                        inputId = "genotype_col",
                        choices = data_colnames(),
                        selected = grep("genotype",
                                        x = data_colnames(),
                                        ignore.case = TRUE,
                                        value = TRUE
                        )[1]
      )
    })




    # Get unique batch from it.
    uniq_batch <- reactive({
      req(validated_data(), input$batch_col)
      validated_data()[[input$batch_col]] |> unique()
    })

    # Populate batch widget
    observe({
      req(uniq_batch())
      updateSelectInput(session,
                        inputId = "batch",
                        choices = uniq_batch(),
                        selected = uniq_batch()[1]
      )
    })

    # Get genotypes and populate for parents.
    Genotype_names <- reactive({
      req(input$batch, validated_data())
      Genotypes_user(data = validated_data(), Batch = input$batch )
    })

    observe({
      req(Genotype_names())
      updateSelectInput(session, inputId = "dp",
                        choices = Genotype_names())

      updateSelectInput(session, inputId = "rp",
                        choices = Genotype_names(),
                        selected = Genotype_names()[3])

      updateSelectInput(session,
                        inputId = "parents",
                        choices = Genotype_names(),
                        selected = Genotype_names()[c(1, 3)]
      )

    })

    # Read map file if user has.
    map_file <- reactive({
      req(input$mapfile)
      read_mapfile(filepath = input$mapfile$datapath)

    })

    # Populate field for snp id column based on mapfile
    observe({
      req(map_file())
      updateSelectInput(session,
                        inputId = "snp_id",
                        choices = colnames(map_file()),
                        selected = grep("id",
                                        x = colnames(map_file()), ignore.case = TRUE,
                                        value = TRUE)[1]
      )
    })


    # process data.
    Result <- eventReactive(input$config, {
      req(
        validated_data(), input$batch, input$batch_col,
        input$data_type, input$allele_sep,
        input$rp, input$dp, input$choice, data_colnames(),
        input$genotype_col, Genotype_names()
      )

      shinybusy::show_modal_spinner(
        spin = "fading-circle",
        color = "#0dc5c1",
        text = "Getting results... Please wait."
      )

      result <- tryCatch({
        proc_nd_map_func(
          data = validated_data(),
          Batch = input$batch,
          batch_col = input$batch_col,
          marker_sep = if (input$choice == "no") input$sep_marker else NULL,
          apply_par_poly = input$apply_par_poly,
          apply_par_miss = input$apply_par_miss,
          apply_geno_good = input$apply_geno_good,
          apply_par_homo = input$apply_par_homo,
          genotype = input$genotype_col,
          snp_id = if (input$choice == "yes") input$snp_id else NULL,
          calls_sep = check_sep(input$allele_sep),
          data_type = input$data_type,
          rp = input$rp,
          dp = input$dp,
          Prefix = if (input$choice == "no") input$prefix_marker else NULL,
          geno_vec = Genotype_names(),
          feedback = input$choice,
          na_code = NA,
          data_col = data_colnames(),
          mapfile_path = if (!is.null(map_file())) map_file() else NULL
        )
      }, error = function(e) {
        shinyWidgets::show_alert(
          title = 'Error',
          text = paste("An error occurred while processing the data:", e$message),
          type = "error"
        )
        return(NULL)
      }, finally = {
        shinyjs::delay(ms = 2000, {
          shinybusy::remove_modal_spinner()
        })
      })

      result
    })


    observe({
      input$config
      req(input$parents)
      # Locking  parents selection
      values$locked_parents <- input$parents
    })


    # Get colnames of Mapfile
    map_file_col <- reactive({
      req(Result()$mapfile)
      colnames(Result()$mapfile)
    })



    # Observer for updating map-related inputs
    observe({
      req(Result()$mapfile, map_file_col())

      # Update SNP ID selection
      updateSelectInput(session,
                        inputId = "snp_ids",
                        choices = map_file_col(),
                        selected = grep(
                          pattern = "id", x = map_file_col(),
                          ignore.case = TRUE, value = TRUE
                        )[1]
      )

      # Update Chromosome selection
      updateSelectInput(session,
                        inputId = "chr",
                        choices = map_file_col(),
                        selected = grep(
                          pattern = "chr", x = map_file_col(),
                          ignore.case = TRUE, value = TRUE
                        )[1]
      )

      # Update Position selection
      updateSelectInput(session,
                        inputId = "chr_pos",
                        choices = map_file_col(),
                        selected = grep(
                          pattern = "pos", x = map_file_col(),
                          ignore.case = TRUE, value = TRUE
                        )[1]
      )

    })

    # calculate recurrent parent
    calc_rpp_bc_result <- reactive({
      req(
        Result(), input$chr_pos, input$chr, input$het_code, input$weight_rpp,
        input$snp_ids, input$rp_num_code, input$rp_index, input$na_code
      )

      calc_rpp_bc(
        x = Result()$proc_kasp_f,
        map_file = Result()$mapfile,
        map_chr = input$chr,
        map_pos = input$chr_pos,
        map_snp_ids = input$snp_ids,
        rp_num_code = input$rp_num_code,
        rp = input$rp_index,
        het_code = input$het_code,
        na_code = input$na_code,
        weighted = input$weight_rpp
      )
    })


    # Render output.
    output$comp_rpp_val <- DT::renderDT({
      req(calc_rpp_bc_result())
      DT::datatable(calc_rpp_bc_result(), options = list(scrollX = TRUE))
    })

    # UPdate colnames.
    observe({
      req(calc_rpp_bc_result())
      updateSelectInput(session,
        inputId = "rpp_col",
        choices = colnames(calc_rpp_bc_result()),
        selected = grep("total_rpp",
          x = colnames(calc_rpp_bc_result()),
          ignore.case = TRUE,
          value = TRUE
        )
      )

      updateSelectInput(session,
        inputId = "rpp_sample_id",
        choices = colnames(calc_rpp_bc_result()),
        selected = grep("sample_id",
          x = colnames(calc_rpp_bc_result()),
          ignore.case = TRUE,
          value = TRUE
        )
      )
    })

    # recurrent parent barplot
    rpp_barplot_result <- reactive({
      req(
        calc_rpp_bc_result(), input$text_size, input$text_scale_fct,
        input$alpha, input$bar_width, input$aspect_ratio, input$bar_col,
        input$thresh_line_col, input$rpp_col
      )
      tryCatch(
        {
          #
          rpp_barplot(
            rpp_df = calc_rpp_bc_result(),
            rpp_sample_id = input$rpp_sample_id,
            rpp_col = input$rpp_col,
            rpp_threshold = input$rpp_threshold,
            text_size = input$text_size,
            text_scale_fct = input$text_scale_fct,
            alpha = input$alpha,
            bar_width = input$bar_width,
            aspect_ratio = input$aspect_ratio,
            bar_col = input$bar_col,
            thresh_line_col = input$thresh_line_col,
            show_above_thresh = input$show_above_thresh,
            bc_gen = if(is.null(input$bc_gen)) NULL else input$bc_gen,
            pdf = FALSE
          )
        },
        error = function(e) {



        }
      )
    })

    # plot it.
    output$rpp_bar <- renderPlot({

      tryCatch({
        plot_obj <- rpp_barplot_result()
        req(plot_obj)

        if (inherits(plot_obj, "ggplot")) {
          print(plot_obj)
          shinyWidgets::show_toast(
            title = 'Success',
            text = 'Plot rendered successfully',
            type = "success"
          )
        } else {

          plot_obj
        }

      }, error = function(e) {
        return(NULL)
        shinyWidgets::show_alert(
          title = 'Error',
          text = paste("An error occurred while generating plot", e$message),
          type = "error"
        )
      })

    })



    # Download rpp plot
    output$download_plot2 <- downloadHandler(
      filename = function() {
        req(input$file_name2)
        paste0(input$file_name2, ".pdf")
      },
      content = function(file) {
        req(rpp_barplot_result())

        tryCatch({
          grDevices::pdf(file,
                         width = input$width2,
                         height = input$height2,
                         onefile = FALSE)

          # Just print the single plot
          print(rpp_barplot_result())

          grDevices::dev.off()
        },
        error = function(e) {
          shinyWidgets::show_toast(
            title = 'Error',
            type = "error",
            text = paste("Download failed:", e$message),
            timer = 5000
          )
        })
      }
    )


    #------------------------------------------------------
    # Information to the user
    #------------------------------------------------------
    # parent missing
    output$par_missing_tbl <- DT::renderDT({
      req(Result())
      DT::datatable(Result()$par_missing_dat, options = list(scrollX = TRUE))
    })

    # Genotype error
    output$geno_error_tbl <- DT::renderDT({
      req(Result())
      DT::datatable(Result()$genotype_error, options = list(scrollX = TRUE))
    })

    # # parent hetero
    output$par_het_tbl <- DT::renderDT({
      req(Result())
      DT::datatable(Result()$parent_het, options = list(scrollX = TRUE))
    })

    # # find_unexpected homo.
    # panGenomeBreedr::find_unexp_homs()
  })
}

## To be copied in the UI
# mod_ds_marker_ass_bac_ui("ds_marker_ass_bac_1")

## To be copied in the server
# mod_ds_marker_ass_bac_server("ds_marker_ass_bac_1")

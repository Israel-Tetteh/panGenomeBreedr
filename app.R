#-------------------------------------------------
# Module Sources
source("modules/mod_Kasp_marker_design.R") # Kasp marker design

source("modules/mod_read_kasp_csv.R") # MV- read kasp file

source("modules/mod_get_alleles.R") # MV- get alleles

source("modules/mod_nsamples.R") # MV- nsamples plate

source("modules/mod_kasp_color.R") # MV- kasp_pch and kasp_color

source("modules/mod_kasp_qc_ggplot.R") # #MV- kasp_qc_ggplot

source("modules/mod_plate_plot.R") # MV- plate layout

source("modules/mod_pred_sum_stat.R") # Predictive status and summary/plot

source("modules/mod_reshape_kasp_data.R") # Reshape Data

source("modules/mod_process_kasp.R") # Kasp_proc
#---------------------------------------------------


# Install pacman -> installs and loads packages at once.
if (!require("pacman")) install.packages("pacman")
# Load libraries
pacman::p_load(
  character.only = TRUE,
  # install = TRUE,
  # update = TRUE,
  char = c("shiny", "data.table", "DT", "bslib", "shinyWidgets")
)


options(shiny.maxRequestSize = 500 * 1024^2) # extend file upload size

# Main UI
ui <- navbarPage(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    font_scale = 1,
    heading_font = font_google("Open Sans", wght = 500), # theme
  ),
  # UI Background theme
  setBackgroundColor(
    color = "#f8f9fa",
    gradient = "linear",
    direction = "bottom"
  ),
  fluid = TRUE,
  title = "PanGB prototype 1",
  id = "nav_bar",

  # Home Tab
  nav_panel(
    title = "Home",
    icon = icon("home")
  ),

  # KASP Marker Design UI
  nav_panel(
    title = "KASP Marker Design",
    icon = icon("dna"),
    Kasp_marker_design_ui("marker_design_id") # kasp marker design UI module
  ),

  # Marker validation tab with sub functionalities.
  nav_panel(
    title = "Marker Validation",
    icon = icon("check-circle"),
    navset_card_tab(
      # Tab for kasp file import
      nav_panel(
        title = "Import Data",
        icon = icon("file-import"),
        card(
          card_header(h5(tags$b("Import KASP Genotyping File"))),
          mod_read_kasp_csv_ui("import_id")
        )
      ),

      # Tab for data summary
      nav_panel(
        title = "Data Summary",
        icon = icon("table"),
        layout_column_wrap(
          width = 1,
          card(
            card_header(h5(tags$b("Sample Statistics"))),
            nsamplesUI(id = "nsamples_id")
          ),
          card(
            card_header(h5(tags$b("Allele Information"))),
            get_allelesUI(id = "get_alleles_id")
          )
        )
      ),

      # Tab for color coding
      nav_panel(
        title = "Color Code",
        icon = icon("palette"),
        card(
          card_header(h5(tags$b("Color Code Calls"))),
          kasp_colorUI(id = "kasp_color_id")
        )
      ),

      # Tab to generate predictions
      nav_panel(
        title = "QC Predictions",
        icon = icon("chart-line"),
        card(
          card_header(h5(tags$b("Quality Control Predictions"))),
          pred_sum_statUI(id = "pred_id")
        )
      ),

      # Tab for Visualization
      nav_panel(
        title = "Visualize",
        icon = icon("chart-bar"),
        navset_pill(
          nav_panel(
            title = "QC Plots",
            div(style = "margin-top: 20px;", qc_plotUI(id = "qc_plots_id"))
          ),
          nav_panel(
            title = "Plate Layout",
            div(style = "margin-top: 20px;", plate_plotUI(id = "plate_layout_id"))
          )
        )
      ),


      # Tab for reshaping data
      nav_panel(
        title = "Reshape",
        icon =  icon("table-columns"),
        layout_column_wrap(
          width = 1,
          card(
            card_header(h5(tags$b("Reshape Kasp Data"))),
            Kasp_reshapeUI(id = "reshape_id")
          ),
          card(
            card_header(h5(tags$b("Process Reshaped Data"))),
            proc_kaspUI('proc_id')
          )
        )
      )

    )
  ),
  # Other Breeder Centered functionalities
  nav_panel(
    title = "Additional Breeding Capabilities",
    icon = icon("cogs")
  ),

  # More tab for other information
  navbarMenu(
    title = "More",
    icon = icon("ellipsis-v"),

    # Tab for issues report
    nav_panel(
      title = "Report Issues",
    ),
    # Help tab
    nav_panel(title = "Help")
  )
)

# Server side.
server <- function(input, output, session) {
  # kasp marker design server
  Kasp_marker_design_server(id = "marker_design_id")

  # import_data_entities; returns kasp data after being read
  import_data_entities <- mod_read_kasp_csv_server("import_id")

  # nsamples & alleles
  observeEvent(import_data_entities(), {
    req(import_data_entities())
    nsamplesServer(id = "nsamples_id", kasp_data = import_data_entities())
    get_allelesServer(id = "get_alleles_id", kasp_data = import_data_entities())
  })

  # Color coding server side
  color_code_res <- reactiveVal(NULL) # Create a reactive null to hold result.

  observeEvent(import_data_entities(), {
    req(import_data_entities())
    color_code_res(
      kasp_colorServer(id = "kasp_color_id", kasp_data = import_data_entities())
    )
  })

  # Predictive summary and status server side
  for_qc_use <- reactiveVal(NULL)

  observeEvent(list(import_data_entities(), color_code_res()), {
    req(import_data_entities(), color_code_res())
    for_qc_use(
      pred_sum_statServer(
        id = "pred_id", kasp_data = import_data_entities(),
        color_code_res = color_code_res()
      )
    )
  })

  # Qc plot server portion
  observeEvent(list(for_qc_use(), color_code_res()), {
    req(for_qc_use(), color_code_res(),import_data_entities())
    qc_plotServer(id = "qc_plots_id", req_inputs = for_qc_use(), color_coded = color_code_res())
    plate_plotServer(id = 'plate_layout_id',kasp_data = import_data_entities(),color_coded = color_code_res() )
  })

  # Reshape data.
  reshape_result <- reactiveVal(NULL)

  observeEvent(import_data_entities(),{
    req(import_data_entities())
    reshape_result(
      Kasp_reshapeServer(id = "reshape_id", kasp_data = import_data_entities())
    )
  })
  # Process reshape
  observeEvent(list(reshape_result(), import_data_entities()),{
    req(reshape_result(), import_data_entities())
    proc_kaspServer(id = 'proc_id',reshape_result = reshape_result(),kasp_dat = import_data_entities())
  })



}

# Run the application
shinyApp(ui = ui, server = server)

############################
## iTReX UI script        ##
## Author: Dina ElHarouni ##
############################

# Heatmap drop-down choices
choices <- c(
  "euclidean", "maximum", "manhattan", "pearson", "spearman", "kendall"
)

# Download link to a file in inst/shiny/www/demo
fileDownloadButton <- function(filename, description) {
  path <- file.path("demo", filename)
  stopifnot(file.exists(file.path("www", path)))
  a(icon("download"), description, href = path, class = "btn btn-default")
}

inlineConditionalPanel <- function(condition, ..., ns = NS(NULL)) {
  span(`data-display-if` = condition, `data-ns-prefix` = ns(""), ...)
}

myDropDownButton <- function(...) {
  shinyWidgets::dropdownButton(
    ...,
    tags$style(
      ".btn-custom {background-color: #C4C2C4; color: #F7F5F7;}"
    ),
    circle = FALSE, status = "custom", size = "xs", icon = icon("info"),
    width = 400, inline = TRUE
  )
}

omniPathLicenseTerms <- function(mod) {
  tagList(
    HTML(paste0(
      mod, "-mod uses OmniPath databases under the Creative Commons ",
      "Attribution-<b>NonCommercial</b>-ShareAlike 3.0 International (",
      tags$a(
        "CC BY-NC-SA 3.0",
        href = "https://creativecommons.org/licenses/by-nc-sa/3.0/"
      ),
      ") license."
    )),
    checkboxInput(
      inputId = paste0(mod, "License"),
      label = glue::glue("I agree to the terms of this license in {mod}-mod."),
    ),
  )
}

omniPathLicenseButton <- function(mod) {
  tagList(
    conditionalPanel(
      glue::glue("!input.{mod}License"),
      "Please accept the license terms above to continue.",
    ),
    conditionalPanel(
      glue::glue("input.{mod}License"),
      actionButton(mod, "Create Network"),
    ),
  )
}

itrex_footer <- function() {
  tags$footer(tags$br(), tags$small(tags$b(
    HTML("&copy;"), "2021", tags$a("DKFZ", href = "https://www.dkfz.de/")
  )))
}

itrex_ui <- function() fluidPage( # styler: off
  shinyjs::useShinyjs(),
  # "Terms of Use" session storage cookie
  tags$script(HTML('
    $(document).on("shiny:connected", function() {
      try {
        var terms_accepted = sessionStorage.getItem("terms_accepted");
      } catch {
        var terms_accepted = false;
      }
      if (terms_accepted === "true") return;
      Shiny.setInputValue("terms_accepted", false);
      Shiny.addCustomMessageHandler("terms-accepted", function(value) {
        try {
          sessionStorage.setItem("terms_accepted", value);
        } catch {}
      })
    });
  ')),

  # theme
  theme = shinythemes::shinytheme("flatly"),
  br(),
  # logos and title
  fluidRow(
    column(2, img(src = "logos/KiTZ.png", width = 115, height = 40)),
    column(8, h4(
      paste0("iTReX: interactive Therapy Response eXploration v", version()),
      title = version(long = TRUE),
      align = "center",
    )),
    column(2, img(src = "logos/DKFZ.png", width = 170, height = 50, align = "right"))
  ),
  br(),
  # iTReX app interface

  tags$head(
    tags$style(HTML(
      "body {
        min-height: 100vh;
        position: relative;
      }
      body::after {
        content: '';
        display: block;
        height: 50px;
      }
      footer {
        position: absolute;
        bottom: 0;
        left: 0;
        width: 100%;
        height: 50px;
        color: black;
        background-color: white;
        text-align: center;
      }"
    )),
    tags$base(target = "_blank"),
  ),
  navbarPage(
    "iTReX",
    id = "iTReX",
    tabPanel(
      "Home",
      fluidRow(
        column(
          4,
          h4("What is iTReX?"),
          hr(),
          span(
            h5("iTReX provides a complete workflow for analyzing therapy
              responses through five main functional modules:"),
            tags$ul(
              tags$li(
                "The Quality Control and Normalization module (QCN-mod)."
              ),
              tags$li(
                "The Mono-therapy Response Analysis module (MRA-mod)."
              ),
              tags$li(
                "The  Combination therapy Response Analysis module (CRA-mod)."
              ),
              tags$li(
                "The drug Hits interaction Network mapping module (HitNet-mod)."
              ),
              tags$li(
                "A module for identifying potential sample-specific omics-based
                biomarkers from drug target connectivity networks (Omics-mod)."
              ),
            ),
            style = "text-align:justify",
          ),
          hr(),
          h5(
            "Step-by-step instructions can be found in the",
            a(href = "docs/iTReX-User-Manual.html", "iTReX User Manual."),
          ),
        ),
        column(
          4,
          h4("Required Input Files"),
          hr(),
          radioButtons("number_of_samples", "Number of Samples",
            choiceNames = list(
              tagList(
                "Single Sample",
                myDropDownButton(
                  inputId = "dropdown_single_sample",
                  h6("Here you can upload your therapy response data for a single sample (kindly note that this app can currently run drug response data with 5 tested concentrations only [(n)replicates are compatible with the app]), please check the demo files for the given options:"),
                  h6("*Make sure that your column names are the same as shown in the demo files."),
                  h6("- For uploading the plate layout togther with your raw data readouts in one spreadsheet select 'Single File'. Demo:"),
                  fileDownloadButton("iTReX-Demo_MRA_LayoutAndReadouts_BT-40_ST04.xlsx", "Mono Layout and Raw Values"),
                  h6("- For uploading the plate layout and raw data readouts separately select 'Separate Files' and upload your plate layout as the following demo:"),
                  fileDownloadButton("iTReX-Demo_CRA_Layout.xlsx", "Combo Layout only"),
                  h6("Upload your raw data readouts compressed as a zip file including the plate reader output as .xlsx spreadsheets or .txt files named with the following nomenclature (SampleID_PlateNumber), e.g: BT-40_P1. Please avoid using '_' in the SampleID part. Demo:"),
                  h6("If you have replicate plates please add letters to indicate the plate replicate of the screen e.g: SampleID_P1A, SampleID_P1B"),
                  fileDownloadButton("iTReX-Demo_CRA_1ReadoutXlsx_INF-R-1632.zip", "combo ZIP")
                ),
              ),
              tagList(
                "Cohort",
                myDropDownButton(
                  inputId = "dropdown_cohort",
                  h6("Here you can upload your therapy response data for a cohort analysis (kindly note that this app can currently run drug response data with 5 tested concentrations only [(n)replicates are compatible with the app]), please check the demo files below."),
                  h6("*Make sure that your column names are the same as shown in the demo files."),
                  h6("Cohort analysis can be performed if all samples were screened with the same layout."),
                  fileDownloadButton("iTReX-Demo_MRA_Layout.xlsx", "Layout"),
                  h6("Upload your raw data readouts compressed as a zip file including the plate reader output as .xlsx spreadsheets or .txt files named with the following nomenclature (SampleID_PlateNumber), e.g: BT-40_P1. Please avoid using '_' in the SampleID part. Demo:"),
                  h6("If you have replicate plates please add letters to indicate the plate replicate of the screen e.g: SampleID_P1A, SampleID_P1B"),
                  fileDownloadButton("iTReX-Demo_MRA_2ReadoutsXlsx.zip", "cohort ZIP")
                ),
              )
            ),
            choiceValues = list("single_sample", "cohort"),
            inline = TRUE,
          ),
          radioButtons("layout_and_readouts", "Layout and Readout Files",
            choiceNames = list(
              tagList("Single File", tags$br(), "(Layout Table including Readouts)"),
              tagList("Separate Files", tags$br(), "(Layout Table and Readout Matrices)")
            ),
            choiceValues = list("single_file", "separate_files"),
          ),
          fileInput(
            "layout_table_file", "Layout Table including Readouts (.xlsx)",
            accept = ".xlsx"
          ),
          fileInput(
            "readout_matrices_file", "Readout Matrices (.txt/.xlsx in .zip)",
            accept = ".zip,.xlsx,.txt"
          ),
          tags$br(),
          h4(
            "Optional Input Files",
            myDropDownButton(
              inputId = "dropdown_healthy_controls",
              h6("iTReX can compute selective scores if healthy reference control samples are available."),
              h6("Please chek the box below to upload the healthy controls spreadsheet (optional). 
                 The healthy control samples must by analyzed first using iTReX, The DSS_asym scores must be used to compute the mean of multiple healthy samples and the spreadsheet should be with the same format as the available demo below:"),
              fileDownloadButton("iTReX-Demo_Controls_DKFZ_ST10-min.xlsx", "Healthy Reference Controls"),
              h6("Please note that the therapy names must match the uploaded therapies specified in the screen layout, any mismatched therapy names will be ignored and computed as NA [NB: Naming is case sensitive].")
            ),
          ),
          hr(),
          checkboxInput("upload_healthy_controls", "Upload Healthy Controls"),
          fileInput(
            "healthy_controls_file", "Healthy Controls (.xlsx)",
            accept = ".xlsx"
          ),
        ),
        column(
          4,
          h4("Analysis Parameters"),
          hr(),
          radioButtons("type_of_readout", "Type of Readout",
            choiceNames = list("Cell Viability", "Cell Death"),
            choiceValues = list("cell_viability", "cell_death"),
            inline = TRUE,
          ),
          selectInput(
            "Amin_select",
            "DSS Activity Threshold (Amin)",
            choices = c(
              "Variable 10% of Imax (ElHarouni et al.)" = "var10",
              "Const. 10% Inhibition (Yadav et al. 2014)" = "10",
              "Const. 0% Inhibition" = "0",
              "Custom Threshold" = "other"
            ),
          ),
          sliderInput(
            "Amin_slider",
            "Custom Threshold (% Inhibition)",
            min = 0, max = 100, value = 10, step = 1,
          ),
          tags$br(),
          h4("Start of Analysis"),
          hr(),
          radioButtons("type_of_analysis", "Type of Analysis",
            choices = list("One-Click" = "OCA", "Stepwise" = "StepA"),
            inline = TRUE,
          ),
          conditionalPanel(
            "input.type_of_analysis == 'OCA'",
            conditionalPanel(
              paste(
                "output.layout_table_uploaded &&",
                "((input.layout_and_readouts == 'single_file') ||",
                " output.readout_matrices_uploaded) &&",
                "(!input.upload_healthy_controls ||",
                " output.healthy_controls_uploaded)"
              ),
              actionButton("start_oca", "Start One-Click Analysis"),
            ),
            conditionalPanel(
              paste(
                "!output.layout_table_uploaded ||",
                "((input.layout_and_readouts != 'single_file') &&",
                " !output.readout_matrices_uploaded) ||",
                "(input.upload_healthy_controls &&",
                " !output.healthy_controls_uploaded)"
              ),
              "Please upload all required input files to continue.",
            ),
            h6(textOutput("end_oca")),
            uiOutput("download_oca"),
          ),
          conditionalPanel(
            "input.type_of_analysis == 'StepA'",
            "Please head to the QCN-mod tab to start your analysis.",
          ),
        ),
      ),
      itrex_footer(),
    ),
    tabPanel(
      "QCN-mod",
      sidebarLayout(
        sidebarPanel(
          "Quality Control and Normalization",
          selectInput("sample", "Sample", choices = NULL),
          conditionalPanel(
            "input.type_of_analysis == 'StepA'",
            conditionalPanel(
              paste(
                "output.layout_table_uploaded &&",
                "((input.layout_and_readouts == 'single_file') ||",
                " output.readout_matrices_uploaded) &&",
                "(!input.upload_healthy_controls ||",
                " output.healthy_controls_uploaded)"
              ),
              actionButton("viewQCreport", "Run QCN Analysis"),
            ),
            conditionalPanel(
              paste(
                "!output.layout_table_uploaded ||",
                "((input.layout_and_readouts != 'single_file') &&",
                " !output.readout_matrices_uploaded) ||",
                "(input.upload_healthy_controls &&",
                " !output.healthy_controls_uploaded)"
              ),
              "Please upload all required input files first.",
            ),
          ),
          br(),
          conditionalPanel(
            "input.start_oca",
            actionButton("vis_QCN", "Visualize QCN")
          ),
          conditionalPanel(
            "input.type_of_analysis == 'StepA'",
            uiOutput("download_QCNreport")
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "QC_mod",
            tabPanel("Screen Summary", h3(textOutput("text1")), DT::dataTableOutput("summary"), DT::dataTableOutput("summary2")),
            tabPanel("Raw Count Distribution", h3(textOutput("text2")), plotOutput("screen")),
            tabPanel(
              "Plate Layout", h3(textOutput("text3")), h4(textOutput("subt1")), plotOutput("layout1", width = "300px", height = "700px"),
              br(), h4(textOutput("subt2"), plotOutput("layout3", width = "300px", height = "700px"))
            ),
            tabPanel("Well Distribution", h3(textOutput("text4")), plotOutput("well", width = "300px", height = "1000px")),
            tabPanel("Replicate Scatter Distribution", h3(textOutput("text9")), plotOutput("scatter", width = "300px", height = "600px")),
            tabPanel(
              "Controls QC", h3(textOutput("text6")), h4(textOutput("textsmall1")), plotOutput("BzCl", width = "300px", height = "600px"),
              h4(textOutput("textsmall2")), plotOutput("DMSO", width = "300px", height = "600px")
            ),
            tabPanel(
              "Viability Heatmap", h4(textOutput("text7")), plotOutput("normalized", width = "300px", height = "600px"),
              h4(textOutput("text8")), plotOutput("unnormalized", width = "300px", height = "600px")
            ),
            tabPanel("Therapy Control Response", h4(textOutput("text10")), h4(textOutput("text11"), plotOutput("STSlikeplot")))
          )
        )
      )
    ),
    navbarMenu(
      "MRA-mod",
      tabPanel(
        "Sample Screen",
        value = "MRA-mod/Sample Screen",
        sidebarLayout(
          sidebarPanel(
            "Monotherapy Response Analysis",
            selectInput("sample_MRA", "Sample", choices = NULL),
            conditionalPanel(
              "input.type_of_analysis == 'StepA'",
              conditionalPanel(
                "output.summary != null",
                actionButton("nplranalyzeperreplica", "Run Analysis"),
              ),
              conditionalPanel(
                "output.summary == null",
                "In stepwise analysis, please run QCN-mod first.",
              ),
            ),
            conditionalPanel(
              "input.start_oca",
              actionButton("vis_MRA", "Visualize MRA")
            )
          ),
          mainPanel(
            tabsetPanel(
              id = "MRA_mod",
              tabPanel(
                "MRA Therapy Parameters",
                conditionalPanel(
                  "input.type_of_analysis == 'StepA'",
                  uiOutput("download_MRAtable"),
                  br(),
                ),
                DT::dataTableOutput("parTable")
              ),
              tabPanel(
                "iHeatmap",
                conditionalPanel(
                  "input.type_of_analysis == 'StepA'",
                  uiOutput("download_ihp")
                ),
                fluidRow(
                  column(
                    width = 6,
                    plotly::plotlyOutput("heatmap")
                  ),
                )
              ),
              tabPanel(
                "MRA DSS_asym Waterfall Plots",
                conditionalPanel(
                  "input.type_of_analysis == 'StepA'",
                  uiOutput("download_dssWF")
                ),
                fluidRow(
                  plotly::plotlyOutput("dsswfall"),
                  plotly::plotlyOutput("gof1"),
                  plotly::plotlyOutput("dssgof")
                )
              ),
              tabPanel(
                "sDSS_asym Waterfall Plots",
                conditionalPanel(
                  "input.type_of_analysis == 'StepA'",
                  uiOutput("download_sdssWF")
                ),
                fluidRow(
                  plotly::plotlyOutput("sDSSwfall"),
                  plotly::plotlyOutput("gof2"),
                  plotly::plotlyOutput("sdssgof")
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "Cohort Screen",
        value = "MRA-mod/Cohort Screen",
        sidebarLayout(
          sidebarPanel(
            "Cohort (Project) Heatmap",
            selectInput("MRA_rowcluster", "Clustering Distance Rows:", choices = choices),
            selectInput("MRA_colcluster", "Clustering Distance Columns:", choices = choices),
            checkboxInput("MRA_sdss", "Show sDSS"),
            checkboxInput("MRA_include_sd", "Show Drug SDs", value = TRUE),
            checkboxInput("MRA_include_zp", "Show Plate z'", value = TRUE),
            sliderInput("MRAc_width", "Plot Width:",
              min = 10, max = 2000, value = 500
            ),
            sliderInput("MRAc_height", "Plot Height:",
              min = 10, max = 2000, value = 250
            ),
            conditionalPanel(
              "(input.number_of_samples == 'cohort') && input.start_oca",
              uiOutput("download_MRAcohorthp")
            ),
            conditionalPanel(
              "(input.number_of_samples == 'single_sample') && input.start_oca",
              h6("This is not a cohort screen, a heatmap cannot be generated.")
            )
          ),
          mainPanel(
            plotOutput("MRAcohort_hp")
          )
        )
      )
    ),
    navbarMenu(
      "CRA-mod",
      tabPanel(
        "Sample Screen",
        value = "CRA-mod/Sample Screen",
        sidebarLayout(
          sidebarPanel(
            "Combotherapy Response Analysis",
            selectInput("sample_CRA", "Sample", choices = NULL),
            conditionalPanel(
              "input.type_of_analysis == 'StepA'",
              conditionalPanel(
                "output.parTable != null",
                actionButton("TCA", "Run Analysis"),
              ),
              conditionalPanel(
                "output.parTable == null",
                "In stepwise analysis, please run QCN-mod and MRA-mod first.",
              ),
            ),
            conditionalPanel(
              "input.start_oca",
              actionButton("vis_CRA", "Visualize CRA")
            )
          ),
          mainPanel(
            tabsetPanel(
              id = "CRA_mod",
              tabPanel(
                "CRA Therapy Parameters",
                conditionalPanel(
                  "input.type_of_analysis == 'StepA'",
                  uiOutput("download_combotable"),
                  br(),
                ),
                DT::dataTableOutput("parTable2"), h4(textOutput("textnocombo"))
              ),
              tabPanel(
                "CRA DSS_asym Waterfall Plot",
                conditionalPanel(
                  "input.type_of_analysis == 'StepA'",
                  uiOutput("download_comboWF")
                ),
                fluidRow(
                  plotly::plotlyOutput("dsswfall_combo"),
                  plotly::plotlyOutput("gof1_combo"),
                  plotly::plotlyOutput("dssgof_combo")
                )
              ),
              tabPanel(
                "CRA dcDSS_asym & dPI Table",
                conditionalPanel(
                  "input.type_of_analysis == 'StepA'",
                  uiOutput("download_CRAtable"),
                  br(),
                ),
                DT::dataTableOutput("parTablecombo")
              ),
              tabPanel(
                "dcDSS_asym Waterfall Plot & dPI Matrix",
                conditionalPanel(
                  "input.type_of_analysis == 'StepA'",
                  uiOutput("download_comboreport"),
                ),
                fluidRow(
                  plotly::plotlyOutput("dcDSSwfall"),
                  splitLayout(cellWidths = c("50%", "50%"), plotly::plotlyOutput("dPI_heatmap"), plotly::plotlyOutput("dPI_heatmap_short"))
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "Cohort Screen",
        value = "CRA-mod/Cohort Screen",
        sidebarLayout(
          sidebarPanel(
            "Cohort (Project) Heatmap",
            selectInput("CRA_rowcluster", "Clustering Distance Rows:", choices = choices),
            selectInput("CRA_colcluster", "Clustering Distance Columns:", choices = choices),
            checkboxInput("CRA_sdss", "Show sDSS"),
            checkboxInput("CRA_include_sd", "Show Drug SDs", value = TRUE),
            checkboxInput("CRA_include_zp", "Show Plate z'", value = TRUE),
            sliderInput("CRAc_width", "PLot Width:",
              min = 10, max = 2000, value = 500
            ),
            sliderInput("CRAc_height", "PLot Height:",
              min = 10, max = 2000, value = 250
            ),
            conditionalPanel(
              "(input.number_of_samples == 'cohort') && input.start_oca",
              uiOutput("download_cohorthp")
            ),
            conditionalPanel(
              "(input.number_of_samples == 'single_sample') && input.start_oca",
              h6("This is not a cohort screen, a heatmap cannot be generated.")
            )
          ),
          mainPanel(
            plotOutput("CRAcohort_hp")
          )
        )
      )
    ),
    tabPanel(
      "HitNet-mod",
      sidebarLayout(
        sidebarPanel(
          omniPathLicenseTerms("HitNet"),
          selectInput("sample_net", "Sample", choices = NULL),
          radioButtons("DTanno", "Drug Target Annotation",
            choices = list("Upload Table" = "Tupload", "Use Drug Bank" = "DBupload"),
            selected = "Tupload"
          ),
          conditionalPanel(
            "input.DTanno == 'Tupload'",
            shinyWidgets::dropdownButton(
              inputId = "dropdownHitNetDrugAnnotation",
              h6(
                "Please upload here a .xlsx spreadsheet with your drug names and annotated drug target genes as HUGO gene symbols,
                the column names must be identical to the names in the demo file:",
                fileDownloadButton("iTReX-Demo_Targets_ST03-min.xlsx", "Drug Target Annotation Demo")
              ),
              circle = FALSE, status = "custom", size = "xs", icon = icon("info")
            ),
            fileInput("Tupload_input", "Upload Annotation Table:")
          ),
          conditionalPanel(
            "input.DTanno == 'DBupload'",
            shinyWidgets::dropdownButton(
              inputId = "dropdownHitNetDrugDank",
              h6(
                "Drug interaction networks are constructed using the drug gene target reference dataset from",
                a(href = "https://go.drugbank.com", "DrugBank"), "and interaction from",
                a(href = "https://omnipathdb.org", "OmniPath")
              ),
              h6("Please make sure that your drug names are following the same name spelling and structure as the DrugBank
                    database in order to use this module."),
              circle = FALSE, status = "custom", size = "xs", icon = icon("info")
            ),
            h6(
              "The DrugBank",
              a(href = "https://go.drugbank.com/legal/terms_of_use", "Terms of Use"),
              "must be followed",
            ),
            h6(
              "Upload the DrugBank Target Links annotation .csv file, you can download the .csv from",
              a(href = "https://go.drugbank.com/releases/5-1-8/downloads/target-all-uniprot-links", "here")
            ),
            fileInput("file_drugbank", "DrugBank annotation file"),
            h6(
              "Upload the DrugBank drug targets all.csv file, you can download the .csv from",
              a(href = "https://go.drugbank.com/releases/5-1-8/downloads/target-all-polypeptide-ids", "here")
            ),
            fileInput("file_drugtarget", "DrugBank targets file")
          ),
          sliderInput("threshold", "View Top (n) Drugs:",
            min = 2, max = 20,
            value = 2
          ),
          selectInput("screen_type", "Screen Type:",
            choices = c("Monotherapy", "Combotherapy")
          ),
          omniPathLicenseButton("HitNet"),
          br(),
          uiOutput("download_hitnet"),
        ),
        mainPanel(
          h4(textOutput("text_hit1")),
          plotOutput("HitNetplot"),
          br(),
          br(),
          h4(textOutput("text_hit2")),
          plotOutput("HitNetplot2")
        )
      )
    ),
    tabPanel(
      "Omics-mod",
      sidebarLayout(
        sidebarPanel(
          omniPathLicenseTerms("Omics"),
          selectInput("sample_omics", "Sample", choices = NULL),
          shinyWidgets::dropdownButton(
            inputId = "dropdownOmicsTable",
            h6("Upload your omics data with gene names according to the Hugo genes symbol nomenclature."),
            h6("A demo file can be downloaded for use, please use the same column naming for your uploaded spreadsheet."),
            fileDownloadButton("iTReX-Demo_Omics_BT-40_ST11.xlsx", "Omics demo"),
            circle = FALSE, status = "custom", size = "xs", icon = icon("info")
          ),
          fileInput("fileOmics", "Upload Omics Table"),
          radioButtons("DTannoO", "Drug Target Annotation",
            choices = list("Upload Table" = "Tupload", "Use Drug Bank" = "DBupload"),
            selected = "Tupload"
          ),
          conditionalPanel(
            "input.DTannoO == 'Tupload'",
            shinyWidgets::dropdownButton(
              inputId = "dropdownOmicsAnnotationTable",
              h6(
                "Please upload here a .xlsx spreadsheet with your drug names and annotated drug target genes as HUGO gene symbols,
                the column names must be identical to the names in the demo file:",
                fileDownloadButton("iTReX-Demo_Targets_ST03-min.xlsx", "Drug Target Annotation Demo")
              ),
              circle = FALSE, status = "custom", size = "xs", icon = icon("info")
            ),
            fileInput("Tupload_inputO", "Upload Annotation Table:")
          ),
          conditionalPanel(
            "input.DTannoO == 'DBupload'",
            h6(
              "The DrugBank",
              a(href = "https://go.drugbank.com/legal/terms_of_use", "Terms of Use"),
              "must be followed",
            ),
            h6(
              "Upload the DrugBank Target Links annotation .csv file, you can download the .csv from",
              a(href = "https://go.drugbank.com/releases/5-1-8/downloads/target-all-uniprot-links", "here")
            ),
            fileInput("file_drugbankO", "DrugBank annotation file"),
            h6(
              "Upload the DrugBank drug targets all.csv file, you can download the .csv from",
              a(href = "https://go.drugbank.com/releases/5-1-8/downloads/target-all-polypeptide-ids", "here")
            ),
            fileInput("file_drugtargetO", "DrugBank targets file")
          ),
          sliderInput("threshold_O", "View Top (n) Drugs:",
            min = 2, max = 20,
            value = 2
          ),
          selectInput("screen_type_O", "Screen Type:",
            choices = c("Monotherapy", "Combotherapy")
          ),
          omniPathLicenseButton("Omics"),
          br(),
          uiOutput("download_omics"),
        ),
        mainPanel(
          h4(textOutput("text_omics1")),
          plotOutput("Omicsplot"),
          br(),
          br(),
          h4(textOutput("text_omics2")),
          plotOutput("Omicsplot2")
        )
      )
    ),
    tabPanel(
      "About",
      fluidRow(
        column(3),
        column(
          6,
          h5(strong("About iTReX:")),
          span(h5("iTReX is an R/Shiny web application for interactive analysis,
                visualization and exploration of mono- and combination therapy dose response profiling data
                and drug target interaction network mapping.
                iTReX features an extended version of the drug sensitivity score (DSS_asym),
                integrating an advanced five-parameter logistic dose-response curve model
                and a differential drug sensitivity score (dcDSS) for combination therapy profiling."), style = "text-align:justify"),
          hr(),
          h5(strong("License:")),
          span(h5(
            "The iTReX package is being developed on",
            a(href = "https://github.com/D-Harouni/iTReX", "GitHub"),
            " under the", a(href = "https://www.gnu.org/licenses/gpl-3.0.en.html", "GLPv3"), "license"
          ), style = "text-align:justify"),
          hr(),
          h5(strong("Disclaimer:")),
          span(h5("The use of the app is purely for research purpose."), style = "text-align:justify"),
          span(h5("The application is under development and might be subject to changes.
              It has not been verified and has not been clinically validated."), style = "text-align:justify"),
          span(h5("Use of the provided results for diagnostic or therapy stratification or any implementation of the results
              in a clinical setting is in the sole responsibility of the treating physician and/or user."), style = "text-align:justify"),
          span(h5("The user is responsible for the quality of the uploaded data and ensures that data
              do not contain any personal data."), style = "text-align:justify"),
          hr(),
          h5(strong("Privacy:")),
          span(h5(
            "Further information about our Terms of Use and Data Protection Policy can be found here:",
            a(href = "docs/iTReX-Datenschutz.pdf", "in German"),
            ",",
            a(href = "docs/iTReX-Data-Privacy.pdf", "in English"),
          ), style = "text-align:justify"),
          hr(),
          h5(strong("Contact:")),
          span(h5("Dina ElHarouni", a(href = "mailto:d.elharouni@kitz-heidelberg.de", "\u2709"),
            ", Yannick Berker", a(href = "mailto:yannick.berker@kitz-heidelberg.de", "\u2709"),
            ", Sina Oppermann", a(href = "mailto:sina.oppermann@kitz-heidelberg.de", "\u2709"),
            ", Matthias Schlesner", a(href = "mailto:matthias.schlesner@informatik.uni-augsburg.de", "\u2709"),
            style = "text-align:justify"
          )),
          # span(h5("Email: iTReX@dkfz.de"), style = "text-align:justify"),
          hr(),
          h5(strong("Contribution:")),
          span(h5("iTReX was developed at the German Cancer Research Center (DKFZ), Hopp Children's Cancer Center (KiTZ), Heidelberg, Germany in a collaboration between the group of
              Translational Pediatric Pharmacology, Clinical Cooperation Unit - Pediatric Oncology (Dr. Sina Oppermann) and the Bioinformatics and Omics Data Analytics group
              (Prof. Dr. Matthias Schlesner)",
            style = "text-align:justify"
          )),
          span(h5("Developers: Dina ElHarouni, Yannick Berker, Sina Oppermann, Matthias Schlesner"), style = "text-align:justify"),
          hr(),
          h5(strong("How to cite:")),
          span(h5("ElHarouni D, Schlesner M & Oppermann S. iTReX:
              Interactive exploration of mono-and combination therapy dose response profiling data.
              Manuscript in Submission (2021).",
            style = "text-align:justify"
          )),
          hr(),
          h5(strong("iTReX project usage:")),
          span(h5("-", a(href = "https://www.dkfz.de/en/PaedOnko/compass.html", "COMPASS"),
            "(Clinical implementation Of Multidimensional PhenotypicAl drug SenSitivities in paediatric precision oncology)",
            style = "text-align:justify"
          )),
          span(h5("-", a(href = "https://www.unite-glioblastoma.de", "UNITE"),
            "(Understanding and Targeting Resistance In Glioblastoma)",
            style = "text-align:justify"
          )),
          hr()
        )
      ),
      itrex_footer(),
    ),
    tabPanel(
      "FAQs",
      strong(textOutput("Q1")),
      h5(textOutput("answer1")),
      br(),
      strong(textOutput("Q2")),
      h5(textOutput("answer2")),
      br(),
      strong(textOutput("Q3")),
      h5(textOutput("answer3")),
      br(),
      strong(textOutput("Q4")),
      h5(textOutput("answer4")),
      br(),
      strong(textOutput("Q5")),
      h5(textOutput("answer5")),
      br(),
      strong(textOutput("Q6")),
      h5(textOutput("answer6")),
      br(),
      strong(textOutput("Q7")),
      h5(textOutput("answer7")),
      br(),
      strong(textOutput("Q8")),
      h5(textOutput("answer8")),
      br(),
      strong(textOutput("Q9")),
      h5(textOutput("answer9")),
      br(),
      strong(textOutput("Q10")),
      h5(textOutput("answer10")),
      br(),
      strong(textOutput("Q11")),
      h5(textOutput("answer11")),
      br(),
      strong(textOutput("Q12")),
      h5(textOutput("answer12")),
      br(),
      strong(textOutput("Q13")),
      h5(textOutput("answer13")),
      br(),
      h5(span(textOutput("extrainfo"), style = "color:steelblue")),
      itrex_footer(),
    )
  )
)

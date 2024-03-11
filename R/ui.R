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

cohortTabPanel <- function(mod = c("MRA", "CRA")) {
  tabPanel(
    "Cohort Screen",
    value = paste0(mod, "-mod/Cohort Screen"),
    sidebarLayout(
      sidebarPanel(
        "Cohort (Project) Heatmap",
        selectInput(
          paste0(mod, "_rowcluster"), "Clustering Distance Rows:",
          choices = choices,
        ),
        selectInput(
          paste0(mod, "_colcluster"), "Clustering Distance Columns:",
          choices = choices,
        ),
        checkboxInput(
          paste0(mod, "_sdss"), "Show sDSS",
        ),
        checkboxInput(
          paste0(mod, "_include_sd"), "Show Drug SDs",
          value = TRUE,
        ),
        checkboxInput(
          paste0(mod, "_include_zp"), "Show z' Values",
          value = TRUE,
        ),
        checkboxInput(
          paste0(mod, "_filter_zp"), "Filter by z' Values",
        ),
        conditionalPanel(
          paste0("input.", mod, "_filter_zp"),
          sliderInput(
            paste0(mod, "_threshold_zp"), "z' Threshold Value:",
            min = -1, max = 1, value = 0.5, step = 0.05,
          ),
          radioButtons(
            paste0(mod, "_filter_mode_zp"), "Filter Mode:",
            choices = list(
              "Screen" = "screen",
              "All Plates" = "all",
              "Any Plate" = "any"
            ),
            selected = "screen",
            inline = TRUE,
          ),
        ),
        sliderInput(
          paste0(mod, "c_width"), "Plot Width:",
          min = 10, max = 2000, value = 500,
        ),
        sliderInput(paste0(mod, "c_height"), "Plot Height:",
          min = 10, max = 2000, value = 250,
        ),
        conditionalPanel(
          "(input.number_of_samples == 'cohort') && input.start_oca",
          uiOutput(paste0("download_", mod, "cohorthp")),
        ),
        conditionalPanel(
          "(input.number_of_samples == 'single_sample') && input.start_oca",
          h6("This is not a cohort screen, a heatmap cannot be generated."),
        ),
      ),
      mainPanel(
        plotOutput(paste0(mod, "cohort_hp")),
      ),
    )
  )
}

doiLink <- function(doi) {
  url <- paste0("https://doi.org/", doi)
  a(href = url, url)
}

mailtoLink <- function(email, suffix = "") {
  tagList(a(href = paste0("mailto:", email), "\u2709"), paste0(email, suffix))
}

drugbankDownload <- "https://go.drugbank.com/releases/5-1-9/downloads/"

itrex_footer <- function() {
  tags$footer(tags$br(), tags$small(tags$b(
    HTML("&copy;"), "2021", tags$a("DKFZ", href = "https://www.dkfz.de/")
  )))
}

itrex_ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    # "Terms of Use" session storage cookie
    tags$script(HTML('
      $(document).on("shiny:connected", function() {
        try {
          var terms_accepted = sessionStorage.getItem("terms_accepted");
        } catch (e) { // Corrected catch block
          var terms_accepted = false;
        }
        if (terms_accepted === "true") return;
        Shiny.setInputValue("terms_accepted", false);
        Shiny.addCustomMessageHandler("terms-accepted", function(value) {
          try {
            sessionStorage.setItem("terms_accepted", value);
          } catch (e) {} // Corrected catch block
        })
      });
    '))
  )
}

    # favicon
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    # theme
    theme = shinythemes::shinytheme("flatly"),
    br(),
    # logos and title
    fluidRow(
      column(2, img(src = "logos/KiTZ.png", width = 115, height = 40)),
      column(8, h4(
        "iTReX: interactive Therapy Response eXploration",
        if (!isTRUE(getOption("shiny.testmode"))) paste0("v", version()),
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
        }",
        ".modal-footer {
          text-align: center;
        }",
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
                inputId = "dropdown_reference_samples",
                h6("iTReX can compute selective scores if reference sample(s) are available."),
                h6("Please check the box below to upload the reference sample(s) spreadsheet (optional).
                  The reference sample(s) must by analyzed first using iTReX, The DSS_asym scores must be used to compute the mean of multiple reference samples and the spreadsheet should be with the same format as the available demo below:"),
                fileDownloadButton("iTReX-Demo_Controls_DKFZ_ST10-min.xlsx", "Reference Sample(s)"),
                h6("Please note that the therapy names must match the uploaded therapies specified in the screen layout, any mismatched therapy names will be ignored and computed as NA [NB: Naming is case sensitive].")
              ),
            ),
            hr(),
            checkboxInput("upload_reference_samples", "Upload Reference Sample(s)"),
            fileInput(
              "reference_samples_file", "Reference Sample(s) (.xlsx)",
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
              "conc_select",
              "Unit of Concentration",
              choices = c(
                "Nanomolar (nM)" = "nM",
                "Molar (M)" = "M",
                "Gray (Gy)" = "Gy",
                "Custom Unit" = "other"
              ),
            ),
            textInput("conc_text", "Custom Unit", value = "Unit"),
            radioButtons("type_of_normalization", "Level of Normalization",
              choiceNames = list("Plate/Replicate", "Treatment/Cell", "Both"),
              choiceValues = list("per_plate", "per_treat", "norm_both"),
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
                  "(!input.upload_reference_samples ||",
                  " output.reference_samples_uploaded)"
                ),
                actionButton("start_oca", "Start One-Click Analysis"),
              ),
              conditionalPanel(
                paste(
                  "!output.layout_table_uploaded ||",
                  "((input.layout_and_readouts != 'single_file') &&",
                  " !output.readout_matrices_uploaded) ||",
                  "(input.upload_reference_samples &&",
                  " !output.reference_samples_uploaded)"
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
                  "(!input.upload_reference_samples ||",
                  " output.reference_samples_uploaded)"
                ),
                actionButton("viewQCreport", "Run QCN Analysis"),
              ),
              conditionalPanel(
                paste(
                  "!output.layout_table_uploaded ||",
                  "((input.layout_and_readouts != 'single_file') &&",
                  " !output.readout_matrices_uploaded) ||",
                  "(input.upload_reference_samples &&",
                  " !output.reference_samples_uploaded)"
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
                  DT::dataTableOutput("parTable"),
                  conditionalPanel(
                    "input.type_of_analysis == 'StepA'",
                    uiOutput("download_MRAtable_rep"),
                    br(),
                  ),
                  br(),
                  DT::dataTableOutput("parTable_rep"),
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
        cohortTabPanel("MRA"),
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
        cohortTabPanel("CRA"),
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
                a(href = paste0(drugbankDownload, "target-all-uniprot-links"), "here"),
              ),
              fileInput("file_drugbank", "DrugBank annotation file"),
              h6(
                "Upload the DrugBank drug targets all.csv file, you can download the .csv from",
                a(href = paste0(drugbankDownload, "target-all-polypeptide-ids"), "here"),
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
                a(href = paste0(drugbankDownload, "target-all-uniprot-links"), "here"),
              ),
              fileInput("file_drugbankO", "DrugBank annotation file"),
              h6(
                "Upload the DrugBank drug targets all.csv file, you can download the .csv from",
                a(href = paste0(drugbankDownload, "target-all-polypeptide-ids"), "here"),
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
            span(
              style = "text-align:justify",
              h5(strong("About iTReX")),
              p("iTReX is an R/Shiny web application for interactive analysis,
                  visualization and exploration of mono- and combination therapy dose response profiling data
                  and drug target interaction network mapping.
                  iTReX features an extended version of the drug sensitivity score (DSS_asym),
                  integrating an advanced five-parameter logistic dose-response curve model
                  and a differential drug sensitivity score (dcDSS) for combination therapy profiling."),
              hr(),
              h5(strong("License information")),
              p(
                "The iTReX source code is available on",
                a(href = "https://github.com/iTReX-Shiny/iTReX", "GitHub"),
                " and licensed under the",
                a(
                  href = "https://www.gnu.org/licenses/gpl-3.0.en.html",
                  "GNU General Public License v3.0", .noWS = "after",
                ),
                ".",
              ),
              hr(),
              h5(strong("Disclaimer")),
              p("The use of the app is purely for research purposes."),
              p("The application is under development and might be subject to changes.
                It has not been verified and has not been clinically validated."),
              p("Use of the provided results for diagnostic or therapy stratification or any implementation of the results
                in a clinical setting is in the sole responsibility of the treating physician and/or user."),
              p("The user is responsible for the quality of the uploaded data and ensures that data
                do not contain any personal data."),
              hr(),
              h5(strong("Privacy")),
              p(
                "Further information about our Terms of Use and Data Protection Policy can be found",
                a(href = "docs/iTReX-Data-Privacy.pdf", "in English"),
                " or ",
                a(href = "docs/iTReX-Datenschutz.pdf", "in German", .noWS = "after"),
                ".",
              ),
              hr(),
              h5(strong("Version information")),
              p(
                "We follow", a(href = "https://semver.org/", "semantic versioning", .noWS = "after"), ":",
                "changes in the first digit of the version number",
                "(e.g., v", strong("1", .noWS = "outside"), ".1.1)",
                "indicate major updates potentially with different result outputs.",
                "Changes in the second digit",
                "(e.g., v1.", strong("1", .noWS = "outside"), ".1)",
                "indicate added features, while changes in the third digit",
                "(e.g., v1.1.", strong("1", .noWS = "outside"), ")",
                "indicate patches and bug fixes.",
              ),
              hr(),
              h5(strong("Contact")),
              p(
                "In case of any questions, feel free to open an issue on",
                a("GitHub", href = "https://github.com/iTReX-Shiny/iTReX/issues"),
                "or contact us via email at",
                mailtoLink("itrex@dkfz-heidelberg.de", "."),
              ),
              hr(),
              h5(strong("Contribution")),
              p(
                "iTReX was developed in a collaboration between the groups of",
                tags$ul(
                  tags$li("Translational Pediatric Pharmacology, Clinical Cooperation Unit Pediatric Oncology (Dr. Sina Oppermann) and"),
                  tags$li("Bioinformatics and Omics Data Analytics (Prof. Dr. Matthias Schlesner)"),
                ),
                "at the Hopp Children's Cancer Center (KiTZ) and",
                "the German Cancer Research Center (DKFZ), Heidelberg, Germany.",
              ),
              p(
                "Authors:",
                "Dina ElHarouni, Yannick Berker (Programmers),",
                "Sina Oppermann, Matthias Schlesner",
              ),
              hr(),
              h5(strong("How to cite")),
              p(
                "ElHarouni D, Berker Y, \u2026, Schlesner M, Oppermann S (2022).",
                "iTReX: Interactive exploration of mono- and combination therapy dose response profiling data.",
                tags$i("Pharmacological Research", .noWS = "after"), ",",
                tags$i("175", .noWS = "after"), ",", "105996.",
                doiLink("10.1016/j.phrs.2021.105996"),
              ),
              hr(),
              h5(strong("iTReX usage")),
              p("iTReX is currently being used in the following projects:"),
              tags$ul(
                tags$li(
                  a(href = "https://www.dkfz.de/en/PaedOnko/compass.html", "COMPASS"),
                  "(Clinical implementation Of Multidimensional PhenotypicAl drug SenSitivities in paediatric precision oncology)",
                ),
                tags$li(
                  a(href = "https://www.unite-glioblastoma.de", "UNITE Glioblastoma"),
                  "(Understanding and Targeting Resistance in Glioblastoma)",
                ),
              ),
              hr(),
            ),
          ),
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
        h5(span(textOutput("extrainfo"), style = "color:steelblue")),
        itrex_footer(),
      )
    )
  )
}

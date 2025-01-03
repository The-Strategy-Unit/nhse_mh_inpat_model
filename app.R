# Mental health inpatient model - shiny application

library(tidyverse)
library(shiny)                    # shiny core
library(shinydashboard)           # layout and display functions
library(StrategyUnitTheme)        # corporate colours
library(bslib)
library(DT)
library(janitor)
library(waterfalls) # https://www.rdocumentation.org/packages/waterfalls/versions/1.0.0/topics/waterfall 
library(shinyWidgets)
library(readxl)
library(rlang)

#### Set SU theme ####
SU_colours <- c (
  `orange`                     = grDevices::rgb(248,191,7, maxColorValue = 255),# "#f9bf07",
  `charcoal`                   = grDevices::rgb(44,40,37, maxColorValue = 255),# "#2c2825",
  `slate`                      = grDevices::rgb(104,111,115, maxColorValue = 255), # "#686f73",
  `blue`                       = grDevices::rgb(88,29,193, maxColorValue = 255), # "#5881c1",
  `red`                        = grDevices::rgb(236,101,85, maxColorValue = 255), # "#ec6555",
  #additional accent colours from word doc template
  `yellow`                     = grDevices::rgb(252,229,155, maxColorValue = 255),
  `grey`                       = grDevices::rgb(163,168,172, maxColorValue = 255),
  `white`                      = grDevices::rgb(255,255,255, maxColorValue = 255),
  #light and dark ends from colour theme in word doc
  `light orange`               = grDevices::rgb(253,242,205, maxColorValue = 255),
  `dark orange`                = grDevices::rgb(124,95,3, maxColorValue = 255),
  `light charcoal`             = grDevices::rgb(235,233,231, maxColorValue = 255),
  `dark charcoal`              = 	"#000000",#black
  `light slate`                = grDevices::rgb(224,226,227, maxColorValue = 255),
  `dark slate`                 = grDevices::rgb(51,55,57, maxColorValue = 255),
  `light blue`                 = grDevices::rgb(221,229,242, maxColorValue = 255),
  `dark blue`                  = grDevices::rgb(38,61,102, maxColorValue = 255),
  `light red`                  = grDevices::rgb(251,224,220, maxColorValue = 255),
  `dark red`                   = grDevices::rgb(144,29,16, maxColorValue = 255),
  `light yellow`               = grDevices::rgb(254,249,235, maxColorValue = 255),
  `dark yellow`                = grDevices::rgb(197,152,5, maxColorValue = 255),
  `light grey`                 = grDevices::rgb(236,237,238, maxColorValue = 255),
  `dark grey`                  = grDevices::rgb(79,84,88, maxColorValue = 255),
  `light white`                = grDevices::rgb(242,242,242, maxColorValue = 255),
  `dark white`                 = grDevices::rgb(127,127,127, maxColorValue = 255),
  `red2`                       = grDevices::rgb(215,25,28, maxColorValue = 255),
  `orange2`                    = grDevices::rgb(253,174,97, maxColorValue = 255),
  `yellow2`                    = grDevices::rgb(255,255,191, maxColorValue = 255),
  `green2`                     = grDevices::rgb(171,221,164, maxColorValue = 255),
  `blue2`                      = grDevices::rgb(43,131,186, maxColorValue = 255) #"#2b83ba"
)

SU_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (SU_colours)
  
  SU_colours[cols]
}

SU_palettes <- list(
  `main` = SU_cols("orange","charcoal","slate","blue","red"),
  `oranges` = SU_cols("light orange","orange","dark orange"),
  `slates` = SU_cols("light slate","slate","dark slate"),
  `mixed` = SU_cols("dark red","orange","yellow","light blue","slate"),
  `oj_coal` = SU_cols("yellow","orange","red","dark red","dark charcoal"),
  `oj_red` = SU_cols("yellow","orange","red","dark red"),
  `white_oj_coal` = SU_cols("white","yellow","orange","red","dark red","dark charcoal"),#added since shared
  `lyellow_oj_coal` = SU_cols("light yellow","orange","red","dark red","dark charcoal"),#added since shared
  `wy_oj_coal` = SU_cols("white","light yellow","yellow","orange","red","dark red","charcoal","dark charcoal"),
  `red_coal` = SU_cols("red","dark red","charcoal","dark charcoal"),
  `blue_yellow_red` = SU_cols("red2","orange2","yellow2","green2","blue2"),
  `red_yellow_blue` = SU_cols("blue2","green2","yellow2","orange2","red2")
)


SU_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- SU_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


scale_color_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}  

theme_SU <-   function (base_size){
  theme_minimal(
    #base_family = "Segoe UI", 
    base_size=12
  ) %+replace% 
    theme(axis.title = element_text(size=11, face="bold",colour=SU_cols("charcoal")),
          plot.title = element_text(hjust=0,face="bold",size=12,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")),
          plot.subtitle = element_text(hjust=0,face="italic",size=10,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")),
          plot.caption = element_text(hjust = 0,face="italic",size=9,colour=SU_cols("slate"),margin=margin(b=4,unit="pt")),
          legend.text = element_text(size=10,colour=SU_cols("charcoal")),
          legend.title = element_text(face="bold",size=11,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")))
}

theme_set(theme_SU())
  
#### Define UI ####
ui <- navbarPage(
  "Mental health inpatient strategy",
  theme = bs_theme(bootswatch = "united",
                   primary = "#f9bf07",
                   secondary = "#686f73"),
  tags$head(
    
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    
    tags$style(HTML("
    .negative-value {
      color: red !important;
    }

    .sidebar {
      position: fixed !important;
      top: 0 !important;
      left: 0 !important;
      height: 100% !important;
      overflow-y: auto !important;
      z-index: 1000 !important;
      background-color: #f8f9fa !important; /* Adjust the background color as needed */
      padding: 15px !important;
      border-right: 1px solid #ddd !important;
    }

    .main-content {
      margin-left: 250px !important; /* Adjust this value based on the width of your sidebar */
    }

    .top-panel {
      background-color: #f8f9fa !important; /* Adjust the background color as needed */
      padding: 15px !important;
      border-bottom: 1px solid #ddd !important;
      position: relative !important;
    }

    .logo {
      position: absolute !important;
      top: -53px !important;
      right: 15px !important;
    }
  ")),
    tags$script(HTML("
    $(document).ready(function() {
      var sidebar = $('.sidebar');
      var offset = sidebar.offset();
      $(window).scroll(function() {
        if ($(window).scrollTop() > offset.top) {
          sidebar.css('top', $(window).scrollTop() - offset.top);
        } else {
          sidebar.css('top', '0');
        }
      });
    });
  "))
  ),
  tags$div(
    class = "top-panel",
    tags$div(
      class = "logo",
      tags$img(src = "tsu_logo_black.png", height = 50)
    )
  ),
  
  tabPanel("Introduction",
           fluidPage(
             titlePanel("Welcome to the Mental Health inpatient demand & capacity tool"),
             h3("Background"),
             p("Integrated Care Boards (ICB) are required to submit 3-year plans for inpatient bed provision to NHS England. This tool development has been commissioned by NHSE Midlands to support ICBs to that end and fundamentally aims to predict the expected volume of inpatient activity and therefore bed requirements to 2028."),
             
             h3("Who the tool is for"),
             p("The tool is specifically designed for use by ICB commissioners to help develop their system-wide medium-term plans for mental health beds. It may be best used collaboratively by mental health commissioners, analysts and other relevant stakeholders though can in theory be used by anyone in isolation. It is likely that analysts will be required to take and manipulate the outputs of the model alongside other local information and bespoke assumptions to support the overall strategic plan - this model is an approximation of the factors likely to impact on demand in the future and does not account for everything nor take into account local circumstances. All of our default assumptions should be screened and adjusted (or omitted)."),
             
             h3("What the model does"),
             p("The model takes a 12-month baseline of data (July 2023 to June 2024) and applies a set of adjustments to that for a range of parameters to estimate future demand for inpatient beds. These parameters cover population change, service changes, indirect impacts, external factors and specific policies on bed management."),
             
             h3("How the tool works"),
             p("The tool has been developed to allow users to interact with the model by adjusting the various parameters up or down to scenario plan and adjust for local perspectives. The impact of changing those parameters can be seen instantly within the model outputs. The numerical outputs of the model can be exported for additional sub-group analysis and/or extended use alongside local information & assumptions not included in the model. The parameters that were agreed and set in the tool for a particular modelling scenario can also be downloaded (and re-uploaded) for stress testing and developing alternative scenario using the tool."),
             )
           ),
  
  tabPanel("Instructions & Data",
           fluidPage(
             titlePanel("Loading data and navigating through the tool"),
             p("The below instructions provide a brief set of instructions to get you going with the tool. We have recorded a tutorial video (link when video ready and posted) that walks through in detail all the functions (and quirks) that you can expect when using the tool. It's advised that you watch this before your first use."),
            
              h3("Instructions"),
             p("Before you start using the tool for the first time, you may also want to scan the information on the 'Metadata and glossary' tab so you know more about what underlying data that is being used within the baseline, how the aggregate file you were sent is generated and what some of the terms used in the tool mean."),
             
             p("1. Upload the CSV file provided to your ICB using the 'Upload CSV File' button below."),
             br(),
             
             fileInput("file", "Upload CSV File", accept = ".csv"),
             br(),
             
             p("2a. Navigate to the 'Modelling Assumptions' tab to view the parameters and change as required (skip to step 3 if accepting the defaults)."),
             p("2b. If you change the parameters from the default ones, download and save them to reload later - the server session does time out after 1 hour of inactivity and will start as fresh default session when you next access the app!"),
             p("3. Navigate to the 'Main outputs' tab to view a plot/table of the baseline, modelled demand and relevant capacity conversion."),
             p("4. Navigate to the 'Supplemetary outputs' tab to view sub-group level changes according to the model and other useful information."),
             p("5. When happy the model reflects your local position on the assumptions, export a csv of the full modelled grouped data for your own post-hoc analysis using 'Download Projected Data' button in that tab"),
             br(),
             p("If you want to generate multiple models with different assumptions e.g. demographics only, high or low growth scenario etc... then please repeat steps 2-5 after loading the baseline data. REMEMBER to save your model outputs AND your parameter file if you want to revisit/recreate these in the tool later!"),
             br(),
             
             h3("Upload growth factor parameters (optional):"),
             p("In the first instance, we suggest you explore the mental health inpatient baseline and projections using our 
             default growth variables (found in the Modelling Assumptions tab)."),
             p("You can export the default or adjusted growth factors to save your adjustments and read them in as a csv file 
               the next time you use the app."),
             p("If you read in a parameters csv file, the values in the file will override our default settings but you will 
               still be able to adjust the growth variables in the side bar."),
             p("When reading in your own parameters, the format of the csv must match that of the downloaded 'adjusted parameters' 
               csv file - i.e. must contain a 'Parameter' column and a 'Value' column."),
             
             fileInput("file1", "Choose CSV File",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
             ),
           )),
  
  tabPanel("Modelling assumptions",
           tabPanel("Analysis",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          h3("Adjust model parameters"),
                          p("After confirming your ICB from the drop-down box, use the controls below to change each parameter in turn using either the step arrows or typing over the values. Every parameter is a percentage change value allowing for 1 decimal place accuracy that estimates total changes over the next 3 years."),
                          
                          selectInput("icb", "Select ICB:", choices = NULL),
                          
                          h5("Adjust Growth Variables:"),
                          
                          fluidRow(
                            column(6,
                                   h6(strong("Population changes:")),
                                   numericInput("incidence_change", "Incidence Change",           value = 3.5,   step = 0.1),
                                   numericInput("acuity_change", "Acuity Change",                 value = 6.7, step = 0.1),
                                   
                                   h6(strong("Indirect changes:")),
                                   numericInput("waiting_list_reduction", "Waiting List Management", value = -3.7,   step = 0.1),
                                   numericInput("prevention_programme", "Prevention Programme",     value = -4, step = 0.1),
                                   
                                   h6(strong("External influences:")),
                                   numericInput("social_care_pressures", "Social Care Pressures", value = 6.6, step = 0.1),
                                   numericInput("national_policy", "National Policy",             value = -4.8,  step = 0.1),
                                   numericInput("mha_changes", "Mental Health Act Changes",       value = -5,  step = 0.1),
                                   ),
                            column(6,
                                   
                                   h6(strong("Direct changes:")),
                                   numericInput("service_models", "Service Models",               value = -5,step = 0.1),
                                   numericInput("admission_avoidance", "Admission Avoidance",       value = -4, step = 0.1),
                                   
                                   h6(strong("Bed policy:")),
                                   numericInput("ooa_repat", "Out of Area Repatriation",            value = 50,   step = 0.1),
                                   numericInput("shift_to_ip", "Shift to Independent setting",      value = 0,   step = 0.1),
                                   
                                   br(),
                                   
                                   actionButton("reset", "Reset Growth Variables to Default")
                                  )
                            ),
                          
                          br(),
                          
                          h5("Export paramters:"),
                          h6("Click to download the paramaters at the levels set above for the next time you use the app. Upload your parameters 
                             on the `Instructions & Data` tab to apply parameters from a previous session."),
                          
                          downloadButton("downloadParameters", "Download Adjusted Parameters"),
                          
                          br(),
                          br(),
                          br(),
                          
                          ),
                        
                        
                        
                        
            mainPanel(
                h3("Demand factor assumptions:"),
                p("Demographic growth values are externally sourced from ONS population projections published at local authority level.
               We have extracted age and gender specific population projections which are applied to our data extract and grouped to ICB level.
               As such, demographic growth is a fixed point and not modifiable unlike our other growth factors."),
               br(),
               
               "For reference, the demographic growth factor for the selected ICB is:",
               
               DTOutput("icb_demographic_growth"),
               
               br(),
               
               h5(strong("Population changes:")),
               p(strong("Incidence change"),": The most likely diagnoses for admissions are Psychosis, PTSD, Severe Anxiety and Drug dependence. Average growth in incidence of these (sourced from APMS, QOF and published prevalence studies) for a 3-year period is estimated at 10.7%. Assuming these conditions make up 2/3 of all admissions and that 50% will feature in multiple groups, we arrive at an adjusted estimate of 3.5% for impact of incidence changes over 3 years."),
               p(strong("Acuity change"),": We have assumed a general change in acuity (length of stay as a proxy) for all admissions of 6.7% increase over 3 years based on national trends in LoS between 2017 and 2023"),
               
               #br(),
               
               h5(strong("Direct changes:")),
               p(strong("Service Models"),": Other local changes to service models, discharge pathways and prevention may reduce admissions or LoS. This is best estimated locally depending on commissioning plans. We propose a notional 5% bedday reduction over 3 years for each of these transformational activities."),
               p(strong("Admission avoidance"),": National programmes to prevent mental ill-health, extend talking therapies, parental and maternal support and older adult support could reduce some demand on inpatient services. This effect is likely to be small in the short-term - we estimate up to 4% reduction."),
               
               #br(),
               
               h5(strong("Indirect changes:")),
               p(strong("Waiting list management"),": Larger waiting lists with longer waits as well as 'hidden' waiting lists are thought to increase risk of admission for some. Reducing waiting lists could reverse rising admission trends. We estimate for a 10% waiting list reduction, 3.7% fewer emergency admissions may occur over 3 years."),
               
               #br(),
               
               h5(strong("External influences:")),
               p(strong("Social care pressures"),": Social care cost and resource pressures are likely to continue in the future. We have assumed there will be an increase in delayed discharge spells over the next 3 years of 6.6%, based on national trends in rates of DD (per 1000 spells) between 2017 and 2023."),
               p(strong("National Policy"),": The government's latest Long-term Plan is funding alternatives to prevent admission (crisis support, safe havens etc...). Given the scale of investment relative to overall budgets (£2.3bn vs £12bn, https://www.kingsfund.org.uk/insight-and-analysis/long-reads/mental-health-360-funding-costs) and indicative impacts (https://pmc.ncbi.nlm.nih.gov/articles/PMC10753954/), we estimate this may reduce admissions by 4.8% over the next 3 years."),
               p(strong("MHA changes"),": Changes to the Mental Health Act are designed to tighten up detention criteria, only use when treatment success is likely, increase the frequency of assessment and reduce detention time for those with LD or autism. Speculatively, we are assuming that these changes will reduce detention bed days by 10% over 3 years. However we anticipate this may be offset by increased informal/planned admissions so have adjusted to 5%."),
               
               br(),
               
               h3("Bed Policy and Capacity conversions:"),
               p(strong("Out of area repatriation"),": This applies to patients resident in your ICB but receiving care outside, although a reciprocal arrangement is also computed for OAP hosted in your beds - your ICB may be a net importer or exporter of OAP. A starting assumption is to repatriate 40% of this activity to in-area beds over 3 years."),
               p(strong("Shift to independent setting"),": Utilising independent provider beds will free existing NHS beds or negate the need for more. The starting assumption for this is net zero or no change - please adjust this up or down to increase the % of NHS activity you might want to commission (in-area) IP beds for in the future."),
               p(strong("Occupancy rates"),": In order to convert both the baseline and modelled demand into number of beds we must convert the bed days. For baseline we will assume a current occupancy rate of 92% and for future desirable OR of 85%.", strong("THESE VALUES CAN BE CHANGED ON THE MAIN RESULTS TAB."))
                          )
                        )
                    ))
           
           ),
  
  tabPanel("Main outputs",
           tabPanel("Analysis",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          
                          h5("Demand factor changes:"),
                          
                          h6(
                            "The waterfall chart displays the baseline number of spells or bed days and the progressive change from the baseline when each 
                            growth factor (modelling assumptions tab) is applied.", 
                            br(),
                            br(),
                            "The final bar represents the projected activity level that is the sum of the baseline and the combined growth factor changes.",
                            br(),
                            br(),
                            "When you are happy with the parameters and these model projections you can download the data for your own post-hoc analysis using 
                            the yellow button below.",
                            
                            br(),
                            br(),
                            
                            h5("Adjust Occupancy rate:"),
                            
                            h6(
                              "Below the waterfall chart, we convert the bed days measure from our baseline extract and projected activity counts to annualised 
                              bed days.",
                              br(),
                              br(),
                              "We apply a 92% occupancy rate to the baseline bed days and divide by 365.25 to calculate annualised bed days. We apply an 85% 'target' 
                              occupancy rate to our bed day projection and divide by 365.25 to calculate the future annualised bed day requirement.",
                              br(),
                              br(),
                              "Calculation: Annualised beds = (Bed days / variable occupancy rate) / 365.25"
                            ),
                            
                            fluidRow(
                              numericInput("current_occupancy", "Current occupancy rate",           value = 92, step = 0.1),
                              numericInput("future_occupancy", "Desirable occupancy rate",             value = 85, step = 0.1)
                              ),
                            
                            actionButton("reset_occupancy", "Reset Occupancy Rates to Default"),
                            
                            br(),
                            br(),
                            
                            
                            h5("Export Projections"),
                            downloadButton("downloadData", "Download Projected Data")
                          )
                          
                        ),
                        
                        mainPanel(
                          h3("Modelled change in demand"),
                          tabsetPanel(
                            tabPanel("Bed days", plotOutput("waterfall_Plot_bed_days", height = "800px", width = "1200px")),
                            tabPanel("Bed days - excl. Home Leave", plotOutput("waterfall_Plot_bed_days_exHL", height = "800px", width = "1200px")),
                            tabPanel("Spells", plotOutput("waterfall_Plot", height = "800px", width = "1200px")),
                            #tabPanel("Projection Table", DTOutput("dataTable"))
                          ),
                          
                          h5(br(),
                             "Occupancy rate adjusted",
                             br()
                          ),
                          tabPanel("Annualised bed days", DTOutput("dataTable_occupancy")),
                          )
                        )
                      )
                    )
           ),
  
  
  tabPanel("Supplementary outputs",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 
                 
                 h5(br(),
                    "Out-Of-Area Placements:",
                    br()
                    ),
                 h6(
                   "For the selected ICB, we present the number of spells by whether they were (1.) not Out-of-area placements (OAP)  (resident in ICB 
                   and treated in ICB), (2.) Outgoing OAP (residents in selected ICB but treated elsewhere) or (3.) Incoming OAP (resident outside of selected 
                   ICB and treated within).",
                   br(),
                   br(),
                   "For outgoing OAP's, the out-of-area reparation growth factor (Model assumptions) is applied as an inflator to illustrate the increased activity 
                   demand if outgoing OAP's were treated within the ICB.", 
                   br(),
                   br(),
                   "For incoming OAP's, the out-of-area reparation is applied as a reduction in demand, applying the assumption that external ICB's would reduce 
                   the number of patients from outside the selected ICB being treated by the selected ICB, as such a reduction in demand for care would be seen.",
                   br(),
                   ),
                 h5(br(),
                    "Sub-group Analysis:"
                 ),
                 h6(
                   "Finally, we present the baseline and projected activity levels by patient group or pathway, in both spells and bed days.", 
                   br(),
                   br(),
                   "Cycle through the 'grouping variable' control (below) to change the sub-group measure by which we present the baseline and 
                   projected activity.",
                   br(),
                   br(),
                   selectInput("group_selection", "Select grouping variable:", 
                               choices = 
                                 c(
                                   "Age Group Admission" = "age_group_admission",
                                   "Gender" = "gender",
                                   "Ethnic Category" = "ethnic_category_2",
                                   "IMD Quintile" = "imd_quintile",
                                   "Provider Type" = "provider_type",
                                   "Legal Status Group" = "legal_status_group",
                                   #"LDA Flag" = "lda_flag",
                                   "Ward Type Description" = "der_ward_type_desc_first"
                                 )
                               )
                   ),
                 
                 
               ),
               
               mainPanel(
                 #h3("ICB Outputs"),
                 
                 h5(br(),
                    "Out-Of-Area Placements",
                    br()
                    ),
                 DTOutput("dataTable_oap"),
                 
                 h5(br(),
                    "Sub-group Analysis"
                    ),
                 tabPanel("Sub-group Plot", plotOutput("sub_group_Plot", height = "700px", width = "1000px"))
                 )
               )
             )
           ),
  
  tabPanel("Metadata and glossary",
           fluidPage(
             titlePanel("Metadata and glossary:"),
             h3("Metadata"),
             p("The MHSDS data hosted within NCDR is our baseline datasource.
               Specified inclusion and exclusion criteria have been applied and are detailed below along with the format in which data exsists and has been aggregated."),
             
             DTOutput("baseline_extract_meta")
             
             )
           )
  )


#### Define server logic ####
server <- function(input, output, session) {
  
  # Set up ----
  # Read in demographic factor
  icb_weighted_demographic_change <- read_csv("demographic_projections/icb_weighted_demographic_change.csv")
  
  output$icb_demographic_growth <- renderDT({
    req(icb_weighted_demographic_change)
    req(input$icb)
    
    DT::datatable(
      icb_weighted_demographic_change %>% 
        filter(residence_icb_name == input$icb) %>% 
        mutate(`Demographic growth projection` = paste0(round(weighted_perc_change * 100,1), "%")) %>% 
        select(residence_icb_name, `Demographic growth projection`) %>% 
        rename(ICB = residence_icb_name)
      )
    })
  
  
  # Read in grouped data
  baseline_aggregate <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  
  # Update selectInput choices based on the highest frequency ICB
  observe({
    req(baseline_aggregate())
    highest_ranked_icb <- 
      baseline_aggregate() %>%
      group_by(residence_icb_name) |> 
      summarise(n = sum(spell_count)) |> 
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(residence_icb_name)
    
    updateSelectInput(session, "icb", choices = highest_ranked_icb)
  })
  
  
  # Intro tab ----
  
  # Analysis tab ----
  
  # Look for parameters file input to set growth factors from user csv:
  observeEvent(input$file1, {
    req(input$file1)
    params <- read.csv(input$file1$datapath)
    updateNumericInput(session, "incidence_change", value = params$Value[params$Parameter == "Incidence Change"])
    updateNumericInput(session, "acuity_change", value = params$Value[params$Parameter == "Acuity Change"])
    updateNumericInput(session, "social_care_pressures", value = params$Value[params$Parameter == "Social Care Pressures"])
    updateNumericInput(session, "mha_changes", value = params$Value[params$Parameter == "Mental Health Act Changes"])
    updateNumericInput(session, "national_policy", value = params$Value[params$Parameter == "National Policy"])
    updateNumericInput(session, "service_models", value = params$Value[params$Parameter == "Service Models"])
    updateNumericInput(session, "prevention_programme", value = params$Value[params$Parameter == "Prevention Programme"])
    updateNumericInput(session, "admission_avoidance", value = params$Value[params$Parameter == "Admission Avoidance"])
    updateNumericInput(session, "waiting_list_reduction", value = params$Value[params$Parameter == "Waiting List Reduction"])
    updateNumericInput(session, "ooa_repat", value = params$Value[params$Parameter == "Out of Area Repatriation"])
    updateNumericInput(session, "shift_to_ip", value = params$Value[params$Parameter == "Shift to Independent setting"])
  })
  
  # Growth factor inputs 
  demographic_growth     <- reactive({ icb_weighted_demographic_change$weighted_perc_change[icb_weighted_demographic_change$residence_icb_name == input$icb] * 100})
  incidence_change       <- reactive({ input$incidence_change })
  acuity_change          <- reactive({ input$acuity_change })
  social_care_pressures  <- reactive({ input$social_care_pressures })
  mha_changes            <- reactive({ input$mha_changes })
  national_policy        <- reactive({ input$national_policy })
  service_models         <- reactive({ input$service_models })
  prevention_programme   <- reactive({ input$prevention_programme })
  admission_avoidance    <- reactive({ input$admission_avoidance })
  waiting_list_reduction <- reactive({ input$waiting_list_reduction })
  ooa_repat              <- reactive({ input$ooa_repat })
  shift_to_ip            <- reactive({ input$shift_to_ip })
  
  # Apply growth inflators to grouped activity counts - separately
  baseline_growth <- reactive({
    req(baseline_aggregate()#, input$icb
        )
    
    baseline_aggregate() %>%
      #filter(residence_icb_name == input$icb) %>%
      mutate(ooa_group = 
               case_when(
                 oap_flag == 0 ~ "not_oap",
                 oap_flag == 1 & residence_icb_code == "QGH" ~ "oap_outgoing",
                 oap_flag == 1 & residence_icb_code != "QGH" ~ "oap_incoming"
               )) |> 
      mutate(sp_demographic_growth       = spell_count * (demographic_growth()/100),
             sp_incidence_change         = spell_count * (incidence_change()/100),
             sp_acuity_change            = spell_count * (acuity_change()/100),
             sp_social_care_pressures    = spell_count * (social_care_pressures()/100),
             sp_mha_changes              = spell_count * (mha_changes()/100),
             sp_national_policy          = spell_count * (national_policy()/100),
             sp_service_models           = spell_count * (service_models()/100),
             sp_prevention_programme     = spell_count * (prevention_programme()/100),
             sp_admission_avoidance      = spell_count * (admission_avoidance()/100),
             sp_waiting_list_reduction   = spell_count * (waiting_list_reduction()/100),
             #sp_ooa_repat                = case_when(oop_flag == 1 ~ spell_count * (ooa_repat()/100), TRUE ~ 0),
             sp_oap_repat_outgoing       = case_when(ooa_group == "oap_outgoing" ~ spell_count * (ooa_repat()/100), TRUE ~ 0),
             sp_oap_repat_incoming       = case_when(ooa_group == "oap_incoming" ~ spell_count * ((ooa_repat()*-1)/100), TRUE ~ 0),
             sp_shift_to_ip              = case_when(provider_type == "Independent" ~ spell_count * (shift_to_ip()/100), TRUE ~ 0),
             
             bd_demographic_growth       = bed_days * (demographic_growth()/100),
             bd_incidence_change         = bed_days * (incidence_change()/100),
             bd_acuity_change            = bed_days * (acuity_change()/100),
             bd_social_care_pressures    = bed_days_delayed_days * (social_care_pressures()/100),
             bd_mha_changes              = bed_days * (mha_changes()/100),
             bd_national_policy          = bed_days * (national_policy()/100),
             bd_service_models           = bed_days * (service_models()/100),
             bd_prevention_programme     = bed_days * (prevention_programme()/100),
             bd_admission_avoidance      = bed_days * (admission_avoidance()/100),
             bd_waiting_list_reduction   = bed_days * (waiting_list_reduction()/100),
             #bd_ooa_repat                =  case_when(oop_flag == 1 ~ bed_days * (ooa_repat()/100), TRUE ~ 0),
             bd_oap_repat_outgoing       = case_when(ooa_group == "oap_outgoing" ~ bed_days *(ooa_repat()/100), TRUE ~ 0),
             bd_oap_repat_incoming       = case_when(ooa_group == "oap_incoming" ~ bed_days *((ooa_repat()*-1)/100), TRUE ~ 0),
             bd_shift_to_ip              =  case_when(provider_type == "Independent" ~ bed_days * (shift_to_ip()/100), TRUE ~ 0),
             
             exHL_bedday_demographic_growth       =  bed_days_exHL * (demographic_growth()/100),
             exHL_bedday_incidence_change         =  bed_days_exHL * (incidence_change()/100),
             exHL_bedday_acuity_change            =  bed_days_exHL * (acuity_change()/100),
             exHL_bedday_social_care_pressures    =  bed_days_delayed_days * (social_care_pressures()/100),
             exHL_bedday_mha_changes              =  bed_days_exHL * (mha_changes()/100),
             exHL_bedday_national_policy          =  bed_days_exHL * (national_policy()/100),
             exHL_bedday_service_models           =  bed_days_exHL * (service_models()/100),
             exHL_bedday_prevention_programme     =  bed_days_exHL * (prevention_programme()/100),
             exHL_bedday_admission_avoidance      =  bed_days_exHL * (admission_avoidance()/100),
             exHL_bedday_waiting_list_reduction   =  bed_days_exHL * (waiting_list_reduction()/100),
             #exHL_bedday_ooa_repat                =  case_when(oop_flag == 1 ~ bed_days_exHL * (ooa_repat()/100), TRUE ~ 0),
             exHL_bedday_oap_repat_outgoing       = case_when(ooa_group == "oap_outgoing" ~ bed_days_exHL * (ooa_repat()/100), TRUE ~ 0),
             exHL_bedday_oap_repat_incoming       = case_when(ooa_group == "oap_incoming" ~ bed_days_exHL * ((ooa_repat()*-1)/100), TRUE ~ 0),
             exHL_bedday_shift_to_ip              =  case_when(provider_type == "Independent" ~ bed_days_exHL * (shift_to_ip()/100), TRUE ~ 0)
             ) %>%
      mutate(spell_proj = spell_count + rowSums(across(contains("sp_"))),
             bed_days_proj = bed_days + rowSums(across(contains("bd_"))),
             bed_days_exHL_proj = bed_days_exHL + rowSums(across(contains("exHL_bedday_")))
      )
  })
  
  # Aggregate up growth/reduction in activity for each factor to ICB level
  waterfall_data <- reactive({
    
    baseline_growth() |> 
      #group_by(residence_icb_name) %>%
      summarise(spell_count = sum(spell_count),
                bed_days = sum(bed_days),
                bed_days_exHL = sum(bed_days_exHL),
                
                sp_demographic_growth      = sum(sp_demographic_growth),
                sp_incidence_change        = sum(sp_incidence_change),
                sp_acuity_change           = sum(sp_acuity_change),
                sp_social_care_pressures   = sum(sp_social_care_pressures),
                sp_mha_changes             = sum(sp_mha_changes),
                sp_national_policy         = sum(sp_national_policy),
                sp_service_models          = sum(sp_service_models),
                sp_prevention_programme    = sum(sp_prevention_programme),
                sp_admission_avoidance     = sum(sp_admission_avoidance),
                sp_waiting_list_reduction  = sum(sp_waiting_list_reduction),
                #sp_ooa_repat               = sum(sp_ooa_repat),
                sp_ooa_repat_outgoing      = sum(sp_oap_repat_outgoing),
                sp_ooa_repat_incoming      = sum(sp_oap_repat_incoming),
                sp_shift_to_ip             = sum(sp_shift_to_ip),
                
                bd_demographic_growth      = sum(bd_demographic_growth),
                bd_incidence_change        = sum(bd_incidence_change),
                bd_acuity_change           = sum(bd_acuity_change),
                bd_social_care_pressures   = sum(bd_social_care_pressures),
                bd_mha_changes             = sum(bd_mha_changes),
                bd_national_policy         = sum(bd_national_policy),
                bd_service_models          = sum(bd_service_models),
                bd_prevention_programme    = sum(bd_prevention_programme),
                bd_admission_avoidance     = sum(bd_admission_avoidance),
                bd_waiting_list_reduction  = sum(bd_waiting_list_reduction),
                #bd_ooa_repat               = sum(bd_ooa_repat),
                bd_ooa_repat_outgoing      = sum(bd_oap_repat_outgoing ),
                bd_ooa_repat_incoming      = sum(bd_oap_repat_incoming ),
                bd_shift_to_ip             = sum(bd_shift_to_ip),
                
                exHL_bedday_demographic_growth     = sum(exHL_bedday_demographic_growth    ),
                exHL_bedday_incidence_change       = sum(exHL_bedday_incidence_change      ),
                exHL_bedday_acuity_change          = sum(exHL_bedday_acuity_change         ),
                exHL_bedday_social_care_pressures  = sum(exHL_bedday_social_care_pressures ),
                exHL_bedday_mha_changes            = sum(exHL_bedday_mha_changes           ),
                exHL_bedday_national_policy        = sum(exHL_bedday_national_policy       ),
                exHL_bedday_service_models         = sum(exHL_bedday_service_models        ),
                exHL_bedday_prevention_programme   = sum(exHL_bedday_prevention_programme  ),
                exHL_bedday_admission_avoidance    = sum(exHL_bedday_admission_avoidance   ),
                exHL_bedday_waiting_list_reduction = sum(exHL_bedday_waiting_list_reduction),
                #exHL_bedday_ooa_repat              = sum(exHL_bedday_ooa_repat             ),
                exHL_bedday_ooa_repat_outgoing     = sum(exHL_bedday_oap_repat_outgoing    ),
                exHL_bedday_ooa_repat_incoming     = sum(exHL_bedday_oap_repat_incoming    ),
                exHL_bedday_shift_to_ip            = sum(exHL_bedday_shift_to_ip           ),
                
                spell_proj = sum(spell_proj),
                bed_days_proj = sum(bed_days_proj),
                bed_days_exHL_proj = sum(bed_days_exHL_proj)
                ) %>%
      mutate(icb_dummy = "ICB") |> 
      pivot_longer(cols = -icb_dummy)
    
  })
  
  # Plot waterfall - Spells and bed days
  waterfall_plot <- reactive({
    
    data <-
      waterfall_data() |> 
      select(-icb_dummy) |>
      filter(name == "spell_count" | str_detect(name, "sp_")) %>%
      mutate(name = 
               case_when(
                 name == "spell_count"                 ~ "A. Baseline year (2024)",
                 name == "sp_demographic_growth"       ~ "B. Demographic growth",
                 name == "sp_incidence_change"         ~ "C. Incidence change",
                 name == "sp_acuity_change"            ~ "D. Acuity change",
                 name == "sp_social_care_pressures"    ~ "E. Social care pressures",
                 name == "sp_mha_changes"              ~ "F. Mental health act changes",
                 name == "sp_national_policy"          ~ "G. National policy",
                 name == "sp_service_models"           ~ "H. Service models",
                 name == "sp_prevention_programme"     ~ "I. Prevention programme",
                 name == "sp_admission_avoidance"      ~ "J. Admission avoidance",
                 name == "sp_waiting_list_reduction"   ~ "K. Waiting list reduction",
                 name == "sp_ooa_repat_outgoing"       ~ "L. Out of area repatriation - outgoing",
                 name == "sp_ooa_repat_incoming"       ~ "M. Out of area repatriation - incoming",
                 name == "sp_shift_to_ip"              ~ "N. Shift to independent sector",
                 name == "spell_proj"                  ~ "O. Projection"  
               )) |>
      mutate(value = round(value, 0)) |> 
      arrange(name) |> 
      mutate(colour = 
               case_when(name == "A. Baseline year (2024)" ~ "#686f73",
                         value >= 0 ~ "#f9bf07",
                         value < 0 ~ "#ec6555")) %>% 
      mutate(value = round(value,0))
    
    waterfall(data,
              calc_total = TRUE, 
              total_axis_text = "Projection (2028)", 
              rect_text_size = 1.8,
              rect_text_labels = rep("", nrow(data)),  # This will hide the value labels
              fill_by_sign = FALSE, 
              fill_colours = data$colour
              ) +
      geom_label(data = data, 
                 aes(x = name,
                     #y = -100,
                     y = (max(value) * 0.07)*-1,
                     #y = max(value) + max(value)*0.7, 
                     label = round(value,1),
                     colour = case_when(value == max(value) ~ "baseline",
                                        value > 0 ~ "positive",
                                        value < 0 ~ "negative"),
                     
                     fill = case_when(value == max(value) ~ "baseline",
                                      value > 0 ~ "positive",
                                      value < 0 ~ "negative")
                     ),
                 size = 6
                 ) +
      scale_color_manual(values = c("baseline" = "black","positive" = "black", "negative" = "black")) +
      scale_fill_manual(values = c("baseline" = "#686f73","positive" = "#f9bf07", "negative" = "#ec6555")) +
      scale_y_continuous(labels = scales::comma) +
      su_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 15),
            axis.text.y = element_text(size = 15),
            axis.title = element_text(size = 16),
            legend.position = "none") +
      labs(x = "Growth factor",
           y = "Spells",
           title = "Demand factor changes to baseline activity",
           #subtitle = paste0("Mental health inpatient model | ", input$icb)
           subtitle = "Mental health inpatient model | 2024 baseline projection to 2028"
      )
  })
  
  waterfall_plot_bed_days <- reactive({
    
    data <-
      waterfall_data() |> 
      select(-icb_dummy) %>%
      filter(name == "bed_days" | str_detect(name, "bd_")) %>%
      mutate(name = case_when(
        name == "bed_days"                ~ "A. Baseline year (2024)",
        name == "bd_demographic_growth"      ~ "B. Demographic growth",
        name == "bd_incidence_change"        ~ "C. Incidence change",
        name == "bd_acuity_change"           ~ "D. Acuity change",
        name == "bd_social_care_pressures"   ~ "E. Social care pressures",
        name == "bd_mha_changes"             ~ "F. Mental health act changes",
        name == "bd_national_policy"         ~ "G. National policy",
        name == "bd_service_models"          ~ "H. Service models",
        name == "bd_prevention_programme"    ~ "I. Prevention programme",
        name == "bd_admission_avoidance"     ~ "J. Admission avoidance",
        name == "bd_waiting_list_reduction"  ~ "K. Waiting list reduction",
        name == "bd_ooa_repat_outgoing"      ~ "L. Out of area repatriation - outgoing",
        name == "bd_ooa_repat_incoming"      ~ "M. Out of area repatriation - incoming",
        name == "bd_shift_to_ip"             ~ "N. Shift to independent sector",
        name == "bed_day_proj"               ~ "O. Projection"
      )) |> 
      mutate(value = round(value, 0)) |> 
      arrange(name) |> 
      mutate(colour = 
               case_when(name == "A. Baseline year (2024)" ~ "#686f73",
                         value >= 0 ~ "#f9bf07",
                         value < 0 ~ "#ec6555")) %>% 
      mutate(value = round(value,0))
    
    waterfall(data, 
              calc_total = TRUE, 
              total_axis_text = "Projection (2028)", 
              rect_text_size = 1.8,
              rect_text_labels = rep("", nrow(data)),  # This will hide the value labels
              fill_by_sign = FALSE, 
              fill_colours = data$colour
              ) +
      geom_label(data = data, 
                  aes(x = name,
                      #y = -100,
                      y = (max(value) * 0.07)*-1,
                      #y = max(value) + max(value)*0.7, 
                      label = round(value,1),
                      colour = case_when(value == max(value) ~ "baseline",
                                         value > 0 ~ "positive",
                                         value < 0 ~ "negative"),
                      
                      fill = case_when(value == max(value) ~ "baseline",
                                       value > 0 ~ "positive",
                                       value < 0 ~ "negative")
                      ),
                 size = 6
                 ) +
      scale_color_manual(values = c("baseline" = "black","positive" = "black", "negative" = "black")) +
      scale_fill_manual(values = c("baseline" = "#686f73","positive" = "#f9bf07", "negative" = "#ec6555")) +
      scale_y_continuous(labels = scales::comma) +
      su_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 15),
            axis.text.y = element_text(size = 15),
            axis.title = element_text(size = 16),
            legend.position = "none") +
      labs(x = "Growth factor",
           y = "Bed days",
           title = "Demand factor changes to baseline activity",
           #subtitle = paste0("Mental health inpatient model | ", input$icb)
           subtitle = "Mental health inpatient model | 2024 baseline projection to 2028"
      )
  })
  
  waterfall_plot_bed_days_ex_HL <- reactive({
    
    data <-
      waterfall_data() |> 
      select(-icb_dummy) %>%
      filter(name == "bed_days_exHL" | str_detect(name, "exHL_bedday_")) %>% 
      mutate(name = case_when(
        name == "bed_days_exHL"                       ~ "A. Baseline year (2024)",
        name == "exHL_bedday_demographic_growth"      ~ "B. Demographic growth",
        name == "exHL_bedday_incidence_change"        ~ "C. Incidence change",
        name == "exHL_bedday_acuity_change"           ~ "D. Acuity change",
        name == "exHL_bedday_social_care_pressures"   ~ "E. Social care pressures",
        name == "exHL_bedday_mha_changes"             ~ "F. Mental health act changes",
        name == "exHL_bedday_national_policy"         ~ "G. National policy",
        name == "exHL_bedday_service_models"          ~ "H. Service models",
        name == "exHL_bedday_prevention_programme"    ~ "I. Prevention programme",
        name == "exHL_bedday_admission_avoidance"     ~ "J. Admission avoidance",
        name == "exHL_bedday_waiting_list_reduction"  ~ "K. Waiting list reduction",
        name == "exHL_bedday_ooa_repat_outgoing"      ~ "L. Out of area repatriation - outgoing",
        name == "exHL_bedday_ooa_repat_incoming"      ~ "M. Out of area repatriation - incoming",
        name == "exHL_bedday_shift_to_ip"             ~ "N. Shift to independent sector",
        name == "bed_day_proj"                        ~ "O. Projection"
      )) %>%
      mutate(value = round(value, 0)) |> 
      arrange(name) |> 
      mutate(colour = 
               case_when(name == "A. Baseline year (2024)" ~ "#686f73",
                         value >= 0 ~ "#f9bf07",
                         value < 0 ~ "#ec6555")) %>% 
      mutate(value = round(value,0))
    
    waterfall(data,
              calc_total = TRUE, 
              total_axis_text = "Projection (2028)", 
              rect_text_size = 1.8,
              rect_text_labels = rep("", nrow(data)),  # This will hide the value labels
              fill_by_sign = FALSE, 
              fill_colours = data$colour
              ) +
      geom_label(data = data, 
                 aes(x = name,
                     #y = -100,
                     y = (max(value) * 0.07)*-1,
                     #y = max(value) + max(value)*0.7, 
                     label = round(value,1),
                     colour = case_when(value == max(value) ~ "baseline",
                                        value > 0 ~ "positive",
                                        value < 0 ~ "negative"),
                     fill = case_when(value == max(value) ~ "baseline",
                                      value > 0 ~ "positive",
                                      value < 0 ~ "negative")
                     ),
                 size = 6
                 ) +
      scale_color_manual(values = c("baseline" = "black","positive" = "black", "negative" = "black")) +
      scale_fill_manual(values = c("baseline" = "#686f73","positive" = "#f9bf07", "negative" = "#ec6555")) +
      scale_y_continuous(labels = scales::comma) +
      su_theme() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 15),
            axis.text.y = element_text(size = 15),
            axis.title = element_text(size = 16),
            legend.position = "none") +
      labs(x = "Growth factor",
           y = "Bed days",
           title = "Demand factor changes to baseline activity",
           #subtitle = paste0("Mental health inpatient model | ", input$icb)
           subtitle = "Mental health inpatient model | 2024 baseline projection to 2028"
      )
  })
  
  # Output objects
  output$waterfall_Plot <- renderPlot({
    req(baseline_growth())
    req(waterfall_data())
    req(waterfall_plot())
    
    waterfall_plot()
  })
  
  output$waterfall_Plot_bed_days <- renderPlot({
    req(baseline_growth())
    req(waterfall_data())
    req(waterfall_plot_bed_days())
    
    waterfall_plot_bed_days()
  })
  
  output$waterfall_Plot_bed_days_exHL <- renderPlot({
    req(baseline_growth())
    req(waterfall_data())
    req(waterfall_plot_bed_days_ex_HL())
    
    waterfall_plot_bed_days_ex_HL()
  })
  
  output$dataTable <- renderDT({
    req(baseline_growth())
    req(waterfall_data())
    
    DT::datatable(
      waterfall_data(),
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',
                     buttons = c('copy', 'csv')
      )
    )
    
  })
  
  # Reset growth factor variables to default position
  observeEvent(input$reset, {
    updateNumericInput(session, "incidence_change", value = 3.5)
    updateNumericInput(session, "acuity_change", value = 6.7)
    updateNumericInput(session, "social_care_pressures", value = 6.6)
    updateNumericInput(session, "mha_changes", value = -5)
    updateNumericInput(session, "national_policy", value = -4.8)
    updateNumericInput(session, "service_models", value = -5)
    updateNumericInput(session, "prevention_programme", value = -4)
    updateNumericInput(session, "admission_avoidance", value = -4)
    updateNumericInput(session, "waiting_list_reduction", value = -3.7)
    updateNumericInput(session, "ooa_repat", value = 50)
    updateNumericInput(session, "shift_to_ip", value = 0)
  })
  
  # Export adjusted parameters
  output$downloadParameters <- downloadHandler(
    filename = function() {
      paste("parameters-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      params <- data.frame(
        Parameter = c("Incidence Change", 
                      "Acuity Change", 
                      "Social Care Pressures", 
                      "Mental Health Act Changes", 
                      "National Policy", 
                      "Service Models", 
                      "Prevention Programme", 
                      "Admission Avoidance", 
                      "Waiting List Reduction", 
                      "Out of Area Repatriation", 
                      "Shift to Independent setting"),
        Value = c(input$incidence_change, 
                  input$acuity_change, 
                  input$social_care_pressures, 
                  input$mha_changes, 
                  input$national_policy, 
                  input$service_models, 
                  input$prevention_programme, 
                  input$admission_avoidance, 
                  input$waiting_list_reduction, 
                  input$ooa_repat, 
                  input$shift_to_ip)
      )
      write.csv(params, file, row.names = FALSE)
    }
  )
  
  
  # Occupancy rate table ----
  
  current_occupancy   <- reactive({ input$current_occupancy })
  future_occupancy    <- reactive({ input$future_occupancy })
  
  output$dataTable_occupancy <- renderDT({
    req(baseline_growth())
    req(waterfall_data())
    req(current_occupancy())
    req(future_occupancy())
    
    DT::datatable(
      waterfall_data() |> 
        filter(str_detect(name, "bed_days")) |> 
        mutate(value = round(value,0)) |> 
        mutate(beds_annualised = 
                 case_when(
                   name %in% c("bed_days", "bed_days_exHL") ~ round((value/(current_occupancy()/100)/365.25), 0),
                   name %in% c("bed_days_proj", "bed_days_exHL_proj") ~ round((value/(future_occupancy()/100)/365.25),0)
                 )
               ) |> 
        mutate(name = 
                 case_when(
                   name == "bed_days" ~ "Baseline",
                   name == "bed_days_exHL" ~ "Baseline - excl home leave",
                   name == "bed_days_proj" ~ "Projected",
                   name == "bed_days_exHL_proj" ~ "Projected - excl home leave"
                 )) |>
        select(-icb_dummy) |>
        mutate(value = scales::comma(value)
               #beds_annualised = scales::comma(beds_annualised)
        ) %>% 
        rename(#ICB = residence_icb_name,
               Measure = name,
               `Bed days` = value,
               `Annualised beds` = beds_annualised),
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',
                     buttons = c('copy', 'csv')
      )
    )
    
  })
  
  # Out of area table
  baseline_oap_activity_icb <- reactive({   
    req(baseline_aggregate(),
        input$icb
        )
    
    baseline_aggregate() |> 
      mutate(ooa_group = 
               case_when(
                 oap_flag == 0 ~ "not_oap",
                 oap_flag == 1 & residence_icb_code == "QGH" ~ "oap_outgoing",
                 oap_flag == 1 & residence_icb_code != "QGH" ~ "oap_incoming"
               )) |> 
      group_by(ooa_group) |> 
      summarise(baseline_spells = sum(spell_count)) |> 
      mutate(ooa_group = 
               case_when(
                 ooa_group == "not_oap" ~ "1. Not Out-of-area placement",
                 ooa_group == "oap_outgoing" ~ "2. Outgoing OAP",
                 ooa_group == "oap_incoming" ~ "3. Incoming OAP"
               )) |>
      mutate(projected_spells = 
               case_when(
                 ooa_group == "2. Outgoing OAP" ~ baseline_spells * 0.4,
                 ooa_group == "3. Incoming OAP" ~ baseline_spells * (0.4*-1)
               )) |> 
      pivot_longer(-ooa_group) |> 
      arrange(ooa_group) |> 
      pivot_wider(id_cols = name,
                  names_from = ooa_group,
                  values_from = value) %>% 
      mutate(name = 
               case_when(
                 name == "baseline_spells" ~ "Baseline - spells",
                 name == "projected_spells" ~ "Projected change - spells"
               )) %>% 
      rename(Measure = name)
    
    })
    
  
  
  output$dataTable_oap <- renderDT({
    req(baseline_oap_activity_icb()
    )
    
    DT::datatable(
      baseline_oap_activity_icb(),
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',
                     buttons = c('copy', 'csv')
                     )
      )
    
    
  })
  

  # Reset occupancy rates to default position
  observeEvent(input$reset_occupancy, {
    updateNumericInput(session, "current_occupancy", value = 92)
    updateNumericInput(session, "future_occupancy", value = 85)
    
  })
  
  # Sub-group plots ----
  
  # Calculate sub-group activity
  sub_plot_data <- reactive({
    req(baseline_growth(), input$group_selection)
    
    baseline_growth() |> 
      mutate(gender = case_when(gender == "1" ~ "Male", 
                                gender == "2" ~ "Female")) %>% 
      mutate(imd_quintile = as.character(imd_quintile)) %>% 
      group_by(!!sym(input$group_selection)) |> 
      summarise(spell_count = sum(spell_count),
                bed_days = sum(bed_days),
                
                spell_proj = sum(spell_proj),
                bed_days_proj = sum(bed_days_proj)
      ) |>
      rename(group_name = 1) |> 
      drop_na(group_name) %>% 
      pivot_longer(-group_name) |> 
      mutate(flag = case_when(str_detect(name, "spell_") ~ "1. Spells", TRUE ~ "2. Bed days"),
             current_projection = case_when(str_detect(name, "proj") ~ "Projection", TRUE ~ "Current")
      ) 
  })
  
  # Plot sub-group
  sub_group_plot <- reactive({
    
    sub_plot_data() |> 
      ggplot(aes(x = group_name, y = value, fill = current_projection)) +
      geom_col(position = "dodge") +
      facet_wrap(~flag, scale = "free_y") +
      scale_fill_SU() +
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      theme(strip.background = element_rect(fill = NA, colour = "grey"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_blank(),
            strip.text = element_text(size = 16)
      ) +
      labs(x =  "Sub-group",
           fill = "",
           title = "Sub-group projections",
           subtitle = "Mental health inpatient model | 2024 baseline projection to 2028")
  })
  
  # Output objects
  output$sub_group_Plot <- renderPlot({
    req(baseline_growth())
    req(sub_plot_data())
    req(sub_group_plot())
    
    sub_group_plot()
  })
  
  output$dataTable_subplot <- renderDT({
    req(baseline_growth())
    req(sub_plot_data())
    
    #DT::datatable(sub_plot_data(), buttons = c("copy", "csv"))
    
    DT::datatable(
      sub_plot_data(),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv')
      )
    )
    
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("projected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- 
        baseline_growth() |> 
        select(-c(spell_count, contains("sp_"), spell_proj)) |> 
        filter(bed_days > 5)
      
      write.csv(data, file)
    }
  )
  
  # Meta data table
  baseline_extract_meta <- read_excel("reference files/baseline_extract_meta.xlsx")
    
  
  output$baseline_extract_meta <- renderDT({
    req(icb_weighted_demographic_change)
    
    DT::datatable(
      baseline_extract_meta
    )
  })
  
}

# Run the application ----
shinyApp(ui = ui, server = server)
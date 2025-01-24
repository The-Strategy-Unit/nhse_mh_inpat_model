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
library(scales)
library(openxlsx)
  
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
  "Mental Health Inpatient Bed Model",
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
      
      document.getElementById('contact_button').onclick = function() {
        window.location.href = 'mailto:strategy.unit@nhs.net?subject=Mental Health Inpatient Demand Model query';
      };
      
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
             p("Integrated Care Boards (ICB) are required to submit strategic plans for inpatient bed provision to NHS England. This tool development has been commissioned by NHSE Midlands to support ICBs in the region to that end and fundamentally aims to predict the expected volume of inpatient activity and therefore bed requirements over the next 3 years."),
             
             h3("Who the tool is for"),
             p("The tool is specifically designed for use by ICB commissioners to help develop their system-wide medium-term plans for mental health beds. It may be best used collaboratively by mental health commissioners, analysts, providers and other relevant stakeholders though can in theory be used by anyone in isolation. It is likely that analysts will be required to take and manipulate the outputs of the model alongside other local information and bespoke assumptions to support the overall strategic plan - this model is an approximation of the factors likely to impact on demand in the future and does not account for everything nor take into account local circumstances.", strong("All")," of our default assumptions should be screened and adjusted (or omitted)."),
             
             h3("What the model does"),
             p("The model takes a 12-month baseline of data (July 2023 to June 2024) and applies a set of adjustments to that for a range of parameters to estimate future demand for inpatient beds. These parameters cover elements of population change, service changes, indirect impacts, external factors and specific policies on bed management."),
             
             h3("How the tool works"),
             p("The tool has been developed to allow users to interact with the model by adjusting the various parameters up or down to scenario plan and adjust for local perspectives. The impact of changing those parameters can be seen instantly within the model outputs. The numerical outputs of the model can be exported for additional sub-group analysis and/or extended use alongside local information & assumptions not included in the model. The parameters that were agreed and set in the tool for a particular modelling scenario can also be downloaded (and re-uploaded) for stress testing and comparing scenario using the tool."),
             br(),
             h3("Other useful resources"),
             p("Due to time and budget constraints, this tool and model are limited and likely don't consider the full range of potential drivers of demand. Systems should consider additional information when establishing a concensus view of the future state of mental health and demand for inpatient beds. The following may be useful:"),
             p(a("OHID Fingertips", href = "https://fingertips.phe.org.uk/profiles", target = "_blank"), " - Information on diseases prevalence, wider determinants of health and outcomes related to mental health."),
             p(a("Mental Health Data Hub", href = "https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/mental-health-data-hub", target = "_blank"), " - NHS digital data and reports on variety of aspects of Mental Health."),
             p(a("Centre for Mental Health", href = "https://www.centreformentalhealth.org.uk/", target = "_blank"), " - Lots of resources, evidence and publications on range of MH topics."),
             p(a("Mental Health Foundation", href = "https://www.mentalhealth.org.uk/explore-mental-health/publications/economic-case-investing-prevention-mental-health-conditions-UK", target = "_blank"), " - resources, evidence and publications on range of MH topics including prevention and health economics."),
             p(a("Review of mental health inpatient capacity", href = "https://www.strategyunitwm.nhs.uk/publications/exploring-mental-health-inpatient-capacity", target = "_blank"), " - Strategy Unit report for RCP on capacity in England."),
             p(a("The Inbetweeners", href = "https://www.ncepod.org.uk/2023transition/The%20Inbetweeners_summary%20report.pdf", target = "_blank"), " - Report from NECPOD on the challenges of transition to adult services for children with complex needs."),
             p(a("Reference costs for Mental Health", href = "https://www.england.nhs.uk/long-read/integrated-national-cost-collection-guidance-2024/", target = "_blank"), " - Costs associated with mental health activity to support economic modelling."),
             p(a("Workforce Statistics", href = "https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/mental-health-data-hub/dashboards/mental-health-and-learning-disabilities-workforce-in-the-nhs", target = "_blank"), " - Data on mental health specific workforce in NHS in England."),
             
             br(),
             
             h4("Acknowledgements"),
             p("This tool has been developed with a range of input (data extraction, tool dev't, advice, QA) from the following people:"),
             p("Andy Hood, Alex Lawless, Anya Ferguson, Andy Wilson, Diane Domenico, Hannah Bedford, Sarah Fellows, Jake Parsons, Sarah Lucas."),
             br(),
             p("Kind thanks to Matt Dray in the Strategy Unit data science team for technical support on workflow and hosting arrangements for the tool."),
             
             h4("Contact Us"),
             p("Click the 'Contact Us' button below to get in touch with any questions or queries:"),
             
             actionButton("contact_button", "Contact Us"),
             
             br(),
             br()
             
             )
           ),
  
  tabPanel("Instructions & Data",
           fluidPage(
             titlePanel("Loading data and navigating through the tool"),
             p("The below instructions provide a brief guide to get you going with the tool. We have recorded 
               a webinar with some stakeholders (link when video ready and posted) that walks through in detail 
               all the functions (and quirks) that you can expect when using the tool. It's advised that you 
               watch this before your first use."),
            
              h3("Instructions"),
             p("Before you start using the tool for the first time, you may also want to scan the information 
               on the 'Metadata and glossary' tab so you know more about what underlying data that is being 
               used within the baseline, how the aggregate file you were sent is generated and what some of the 
               terms used in the tool mean."),
             
             p("1. Upload the CSV file provided to your ICB using the 'Upload CSV File' button below."),
             br(),
             
             fileInput("file", "Upload CSV File", accept = ".csv"),
             br(),
             
             DTOutput("glimpse_baseline_aggregate"),
             br(),
             
             p(strong("2a. "),"Navigate to the 'Modelling Assumptions' tab to view the demand parameters and change as required 
               (skip to step 3 if accepting the defaults)."),
             p(strong("2b. "),"If you change the parameters from the default ones, download and save them to reload later - ",
               strong("the server session does time out after 1 hour of inactivity")," and will start as fresh default 
               session when you next access the app!"),
             p(strong("3a. "),"Navigate to the 'Main outputs' tab to view a plot/table of the baseline, modelled demand 
               and capacity conversion to beds."),
             p(strong("3b. "),"Navigate to the 'Sub-group Outputs' tab to view the baseline vs modelled demand data for the sub-groups of 
               patients / beds in the data."),
             p(strong("4. "),"Now switch to the 'Bed Policy and Management' tab to specify any plans for out of area and 
               independent beds and the switches in bed numbers these might mean."),
             p(strong("5. "),"When happy the model reflects your local position on the assumptions, you can export a csv of 
               the full modelled grouped data for your own post-hoc analysis using 'Download Projected Data' button in 
               the 'Main Outputs' tab."),br(),
             p("If you want to generate multiple models with different assumptions e.g. demographics only, high 
               or low growth scenario etc... then please repeat the above steps after loading the baseline data.",
               strong(" REMEMBER")," to save your model outputs ",strong("AND")," your parameter file if you want to 
               revisit/recreate these scenario in the tool later!"),
             br(),
             
             h3("Upload growth factor parameters (optional):"),
             p("In the first instance, we suggest you explore the mental health inpatient baseline and 
               projections using our 
             default growth variables (found in the Modelling Assumptions tab)."),
             p("You can export any of your adjusted parameters and read them in as a csv file 
               the next time you use the app."),
             p("If you read in a parameters csv file, the values in the file will override our default settings 
               but you will still be able to adjust the growth variables in the side bar."),
             p("When reading in your own parameters, the format of the csv must match that of the downloaded 
               'adjusted parameters' csv file - i.e. must contain a 'Parameter' column and a 'Value' column."),
             
             fileInput("file1", "Choose CSV File",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
                       ),
             )
           ),
  
  tabPanel("Modelling assumptions",
           tabPanel("Analysis",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          h3("Adjust model parameters"),
                          p("After confirming your ICB from the drop-down box, use the controls below to change 
                            each parameter in turn using either the step arrows or typing over the values. 
                            Every parameter is a percentage change value allowing for 1 decimal place accuracy 
                            that estimates ",strong("total changes over the next 3 years.")," The text opposite gives a description 
                            of each parameter and where relevant the source and/or logic for our default values."),
                          
                          selectInput("icb", "Select ICB:", choices = NULL),
                          
                          h5("Adjust Growth Variables:"),
                          
                          fluidRow(
                            column(6,
                                   h6(strong("Population changes:")),
                                   numericInput("incidence_change", "Incidence Change",           value = 3.5,   step = 0.1),
                                   numericInput("acuity_change", "Acuity Change",                 value = 6.7, step = 0.1),
                                   
                                   h6(strong("External influences:")),
                                   numericInput("social_care_pressures", "Social Care Pressures", value = 6.6, step = 0.1),
                                   numericInput("national_policy", "National Policy",             value = -4.8,  step = 0.1),
                                   numericInput("mha_changes", "Mental Health Act Changes",       value = -5,  step = 0.1),
                                   ),
                            
                            column(6,
                                   h6(strong("Demand Management:")),
                                   numericInput("waiting_list_reduction", "Waiting List Management", value = -3.7,   step = 0.1),
                                   numericInput("prevention_programme", "Prevention Programme",     value = -4.8, step = 0.1),
                                   numericInput("service_models", "Service Models",               value = -5,step = 0.1),
                                   numericInput("admission_avoidance", "Admission Avoidance",       value = -4, step = 0.1),
                                   
                                   br(),
                                   
                                   actionButton("reset", "Reset Growth Variables to Default")
                                  )
                            ),
                          
                          br(),
                          
                          h5("Export parameters:"),
                          h6("Click to download the parameters at the levels set above for the next time you 
                             use the app. Upload your parameters on the `Instructions & Data` tab to apply 
                             parameters from a previous session."),
                          
                          downloadButton("downloadParameters", "Download Adjusted Parameters"),
                          br(),
                          
                          ),
                        
            mainPanel(
                h3("Demand factor assumptions:"),
                p("Demographic growth values are externally sourced from ONS population projections published 
                  at local authority level ", a("here", href = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/subnationalpopulationprojectionsforengland/2018based", target = "_blank"),
                  ". We have extracted age and gender specific population projections which are applied to our 
                  data extract and grouped to ICB level. As such, demographic growth is a fixed point and not 
                  modifiable unlike our other growth factors."
                ),
               br(),
               
               "For reference, the demographic growth factor for the selected ICB is:",
               
               DTOutput("icb_demographic_growth"),
               
               br(),
               
               h5(strong("Population changes:")),
               p(strong("Incidence change"),": The most likely diagnoses for admissions are Psychosis, PTSD, 
                 Severe Anxiety and Drug dependence. Average growth in incidence of these (sourced from APMS, 
                 QOF and published prevalence studies) for a 3-year period is estimated at 10.7%. Assuming these 
                 conditions make up 2/3 of all admissions and that 50% will feature in multiple groups, we 
                 arrive at an adjusted estimate of 3.5% for impact of incidence changes over 3 years."),
               p(strong("Acuity change"),": We have assumed a general change in acuity (length of stay as a proxy) 
                 for all admissions of 6.7% increase over 3 years based on national trends in LoS between 2017 and 2023"),
              
               h5(strong("Demand Management:")),
               p(strong("Waiting list management"),": Larger waiting lists with longer waits as well as 'hidden' 
                 waiting lists are thought to increase risk of admission for some",
                 a("(source)", href = "https://www.rcpsych.ac.uk/news-and-features/latest-news/detail/2022/10/10/hidden-waits-force-more-than-three-quarters-of-mental-health-patients-to-seek-help-from-emergency-services", target = "_blank"),
                 ". Reducing waiting lists could reverse rising admission trends. We estimate, based on historic data on 
                 emergency method admissions that for a 10% waiting list reduction, 3.7% fewer admissions may occur over 3 years."),
               p(strong("Prevention Programmes"),": A prevous review of evidence on impact of preventive interventions",
                 a("(source)", href = "https://www.strategyunitwm.nhs.uk/sites/default/files/2019-11/Exploring%20Mental%20Health%20Inpatient%20Capacity%20accross%20Sustainability%20and%20Transformation%20Partnerships%20in%20England%20-%20191030_1.pdf", target = "_blank"),
                 "suggested there was strong evidence that Early Intervention in Psychosis, CBT and Family Interventions 
                 could reduce demand on inpatient settings for some patients. Based on weighted impact of these studies",
                 a("(source)", href = "https://pubmed.ncbi.nlm.nih.gov/21037211/", target = "_blank"), "and an investment 
                 reach of 50% to these groups we estimate an overall impact of 4.8% reduction in admitted bed days."),
               p(strong("Service Models"),": Other local changes to service models, discharge pathways and prevention may 
                 reduce admissions or LoS. This is best estimated locally depending on commissioning plans. We propose a 
                 notional 5% bedday reduction over 3 years for these transformational activities."),
               p(strong("Admission avoidance"),": National programmes to prevent mental ill-health, extend talking therapies, 
                 parental and maternal support and older adult support could reduce some demand on inpatient services. 
                 This effect is likely to be small in the short-term - we estimate up to 4% reduction."),
           
               h5(strong("External influences:")),
               p(strong("Social care pressures"),": Social care cost and resource pressures are likely to continue in the future. 
                 We have assumed there will be an increase in delayed discharge bed days over the next 3 years of 6.6%, 
                 based on national trends in rates of DD (per 1000 spells) between 2017 and 2023."),
               p(strong("National Policy"),": The government's latest Long-term Plan is funding alternatives to prevent admission 
                 (crisis support, safe havens etc...). Given the scale of investment relative to overall budgets (£2.3bn vs £12bn ",
                 a("(source)", href = "https://www.kingsfund.org.uk/insight-and-analysis/long-reads/mental-health-360-funding-costs", target = "_blank"),
                 ", and indicative impacts", a("(source)", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC10753954/", target = "_blank"),
                 ", we estimate this may reduce admissions by 4.8% over the next 3 years."),
               p(strong("MHA changes"),": Changes to the Mental Health Act are designed to tighten up detention criteria, 
                 only use when treatment success is likely, increase the frequency of assessment and reduce detention time 
                 for those with LD or autism. Speculatively, we are assuming that these changes will reduce detention bed 
                 days by 10% over 3 years. However we anticipate this may be offset by increased informal/planned 
                 admissions so have adjusted to 5%."),
               
               br(),
               )
            )
            )
            )
           
           ),
  
  tabPanel("Main outputs",
           tabPanel("Analysis",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          
                          h5("Demand factor changes:"),
                          
                          h6(
                            "The waterfall chart opposite displays the baseline number of bed days or spells and the 
                            progressive change from the baseline when each growth factor (modelling assumptions tab) 
                            is applied.", 
                            br(),
                            br(),
                            "The final bar represents the projected activity level that is the sum of the baseline 
                            and the combined demand factor changes.",
                            br(),
                            br(),
                            "When you are happy with the parameters and these model projections you can download the 
                            data for your own post-hoc analysis using the yellow button below.",
                            
                            br(),
                            br(),
                            
                            h5("Adjust Occupancy rate:"),
                            
                            p(strong("Occupancy rates"),": In order to convert both the baseline and modelled demand 
                              into number of beds we must convert the bed days. For baseline we will assume a current 
                              occupancy rate of 92% and for future desirable OR of 85%."),
                            
                            h6("Calculation: Annualised beds = (Bed days / variable occupancy rate) / 365.25"),
                            
                            fluidRow(
                              numericInput("current_occupancy", "Current occupancy rate",           value = 92, step = 0.1),
                              numericInput("future_occupancy", "Desirable occupancy rate",             value = 85, step = 0.1)
                              ),
                            
                            actionButton("reset_occupancy", "Reset Occupancy Rates to Default"),
                            
                            br(),
                            br(),
                            
                            
                            h5("Export Projections"),
                            p(em("(this will also include the bed flow changes specified on the next tab.)")),
                            downloadButton("downloadData", "Download Projected Data")
                          )
                          
                        ),
                        
                        mainPanel(
                          h3("Modelled change in demand"),
                          tabsetPanel(
                            tabPanel("Bed days", plotOutput("waterfall_Plot_bed_days", height = "800px", width = "1200px")),
                            tabPanel("Bed days - excl. Home Leave", plotOutput("waterfall_Plot_bed_days_exHL", height = "800px", width = "1200px")),
                            tabPanel("Spells", plotOutput("waterfall_Plot", height = "800px", width = "1200px"))
                          ),
                          
                          h5(br(),
                             "Occupancy rate adjusted beds",
                             br()
                          ),
                          tabPanel("Annualised bed days", DTOutput("dataTable_occupancy")),
                          )
                        )
                      )
                    )
           ),
  
  tabPanel("Sub-group outputs",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 
                 h5(br(),
                    "Sub-group Analysis:"
                    ),
                 h6(
                   "In order to inform patient and service demand perspectives, below we present the baseline and projected activity levels by patient group or pathway, in both spells and bed days.", 
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
                                   "LDA Flag" = "lda_flag",
                                   "Ward Type Description" = "der_ward_type_desc_first"
                                 )
                   )
                 ),
               ),
               
               mainPanel(
                 
                 #h5(br(),
                 #   "Sub-group Analysis"
                 #   ),
                 tabPanel("Sub-group Plot", plotOutput("sub_group_Plot", height = "700px", width = "1000px"))
               )
             )
           )
  ),
  
  
  tabPanel("Bed policy and management",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 
                 
                 h5(br(),
                    "Bed policy & Management:",
                    br()
                    ),
                 p("This panel allows the user to test assumptions on bed flows in and out of area and also 
                   between NHS and Independent Provider beds.", strong(" This has no effect on demand")," rather the shift 
                   of beds around the system in the future state",
                   br(),
                   p("The text and tables to the right explain the concepts for the assumptions, provide some 
                     baseline information to inform judgements on setting them and finally demonstrate the 
                     effects of those parameters on bed flows."),
                   ),
                 
                 h6(strong("Bed policy & Management assumptions:")),
                 numericInput("ooa_repat", "Out of Area Repatriation",            value = 50,   step = 0.1),
                 numericInput("ooa_expat", "Out of Area Expatriation",            value = 50,   step = 0.1),
                 numericInput("shift_to_ip", "Shift to Independent setting",      value = 0,   step = 0.1),
                 
                 br(),
                 
                 actionButton("reset_bed_policy", "Reset Bed Policy Factors to Default")
                 
               ),
               
               mainPanel(
                 
                 h3("Bed Policy & Management:"),
                 p(strong("Out of area repatriation and expatriation"),": For outgoing OAP's, the repatriation factor 
                   (left panel) is applied as an inflator to illustrate the increased internal bed requirement if outgoing 
                   OAP's were treated at providers within the ICB in the future. For incoming OAP's, the 
                   expatriation factor is expressed as a reduction in internal demand, as patients are 'sent back' 
                   to providers in their own ICB area. A starting assumption is to adjust 50% of this activity in both directions over 3 years."),
                 
                 h5(br(),
                    "Out-Of-Area Placements at baseline",
                    br()
                 ),
                 tabsetPanel(
                   tabPanel("Bed days", DTOutput("dataTable_oap_bed_days")),
                   tabPanel("Bed days - excl. Home Leave", DTOutput("dataTable_oap_bed_days_exHL")),
                   tabPanel("Spells", DTOutput("dataTable_oap"))
                 ),
                 br(),
                 tags$ul(
                   tags$li("[1 - Upper left quadrant] Residence and Provider are in ICB boundary (i.e. not OAP)"),
                   tags$li("[2 - Lower left quadrant] Residence outside of selected ICB and treated within 
                             (i.e. Incoming OAP)"),
                   tags$li("[3 - Upper right quadrant] Residence in selected ICB but treated elsewhere 
                             (i.e. Outgoing OAP)"),
                   tags$li("[4 - Lower right quadrant] Resident outside and treated outside of selected ICB 
                             (data not included)")
                 ),
                 p("If the value of group 2 is higher than group 3 then you are a 'net importer' of OAP and 
                   if the value of group 3 is higher than group 2 you are a 'net exporter' of OAP."),
                 br(),
                 
                 p(strong("Shift to independent setting"),": Utilising independent provider (IP) beds will, in theory, 
                   free existing NHS beds or negate the need for more. Our starting assumption for this is net zero 
                   or no change - please adjust this up or down to increase the % of NHS activity you might want to 
                   commission IP beds for in the future."),
                 
                 h5(br(),
                    "Provider type and out-of-area activity at baseline",
                    br()
                 ),
                 p("Use the table below to gauge the baseline levels of activity that is delivered by NHS and 
                   Independent sector providers along with whether it is in or out of area care."),
                 tabsetPanel(
                   tabPanel("Bed days", DTOutput("bed_policy_oap_group_baseline_bd")),
                   tabPanel("Bed days - excl. Home Leave", DTOutput("bed_policy_oap_group_baseline_bd_exHL")),
                   tabPanel("Spells", DTOutput("bed_policy_oap_group_baseline"))
                 ),
                 
                 
                 h5(br(),
                    "Bed policy changes to projected activity",
                    br(),
                 ),
                    p("This table shows the effect of the above changes on the ",strong("modelled future demand "), 
                      "in respect of bed days and beds that may need to rebalance the future system."),
                 tabsetPanel(
                   tabPanel("Bed days", DTOutput("bed_policy_table_bd")),
                   tabPanel("Bed days - excl. Home Leave", DTOutput("bed_policy_table_bd_exHL")),
                   tabPanel("Spells", DTOutput("bed_policy_table_spells"))
                   )
                 )
               )
             )
           ),
  
  tabPanel("Metadata and glossary",
           fluidPage(
             titlePanel("Metadata and glossary:"),
             h3("Metadata"),
             p("The MHSDS data hosted within UDAL is our baseline datasource. Data in the raw extract that underpins 
               the ICB-based activity files which are used by the tool are for the 1-year period 1st July 2023 to 
               30th June 2024. All admissions are included where not recorded as a specialised commissioning category 
               and where the patient was either resident or treated within the Midlands region (11 ICB boundaries).

               The table below details the format of the aggregated ICB files used to feed into the model with any 
               sub-categories of fields and notes on derivation."),

             
             DTOutput("baseline_extract_meta"),
             
             br(),
             
             h3("Glossary of terms and abbreviations"),
             
             p("The following table lists and describes most abbreviations and complex terms included in the tool."),
             
             DTOutput("glossary")
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
        rename(ICB = residence_icb_name),
      options = list(dom = 't', paging = FALSE, ordering = FALSE)
      )
    })
  
  # Read in grouped data
  baseline_aggregate <- reactive({
    req(input$file)
    
    read.csv(input$file$datapath) |>
      mutate(ooa_group = 
               case_when(
                 oap_flag == 0 ~ "not_oap",
                 oap_flag == 1 & residence_icb_name == input$icb ~ "oap_outgoing",
                 oap_flag == 1 & residence_icb_name != input$icb ~ "oap_incoming"
               ))
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
  
  output$info <- renderText({
    "Click the 'Contact Us' button to send an email."
  })
  
  # Intro tab ----
  
 output$glimpse_baseline_aggregate <- renderDT({
    req(baseline_aggregate())
    
    DT::datatable(
      baseline_aggregate() %>% 
        arrange(desc(spell_count)) %>% 
        head(5)
      )
    })
  
  
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
    updateNumericInput(session, "ooa_expat", value = params$Value[params$Parameter == "Out of Area Expatriation"])
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
  ooa_expat              <- reactive({ input$ooa_expat })
  shift_to_ip            <- reactive({ input$shift_to_ip })
  
  
  # Baseline growth function
  baseline_growth_function <- function(oap_filter) {
    
    baseline_aggregate() |> 
      filter(ooa_group %in% c( {{oap_filter}} )) %>% 
      mutate(sp_demographic_growth       =  spell_count * (demographic_growth()/100),  
             sp_incidence_change         =  spell_count * (incidence_change()/100),
             sp_acuity_change            =  spell_count * (acuity_change()/100),
             sp_social_care_pressures    =  spell_count * (social_care_pressures()/100),
             sp_mha_changes              =  spell_count * (mha_changes()/100),
             sp_national_policy          =  spell_count * (national_policy()/100),
             sp_service_models           =  spell_count * (service_models()/100),
             sp_prevention_programme     =  spell_count * (prevention_programme()/100),
             sp_admission_avoidance      =  spell_count * (admission_avoidance()/100),
             sp_waiting_list_reduction   =  spell_count * (waiting_list_reduction()/100),
             #sp_ooa_repat                = case_when(oop_flag == 1 ~ spell_count * (ooa_repat), TRUE ~ 0),
             #sp_oap_repat_outgoing       = case_when(ooa_group == "oap_outgoing" ~ spell_count*(ooa_repat/100), TRUE ~ 0),
             #sp_oap_repat_incoming       = case_when(ooa_group == "oap_incoming" ~ spell_count*((ooa_repat/100)*-1), TRUE ~ 0),
             #sp_shift_to_ip              = case_when(provider_type == "NHS" ~ spell_count * ((shift_to_ip()/100)*-1), TRUE ~ 0),
             
             bd_demographic_growth       =  bed_days * (demographic_growth()/100),
             bd_incidence_change         =  bed_days * (incidence_change()/100),
             bd_acuity_change            =  bed_days * (acuity_change()/100),
             bd_social_care_pressures    =  bed_days_delayed_days * (social_care_pressures()/100),  # switch to bed days - delayed discharges
             bd_mha_changes              =  bed_days * (mha_changes()/100),
             bd_national_policy          =  bed_days * (national_policy()/100),
             bd_service_models           =  bed_days * (service_models()/100),
             bd_prevention_programme     =  bed_days * (prevention_programme()/100),
             bd_admission_avoidance      =  bed_days * (admission_avoidance()/100),
             bd_waiting_list_reduction   =  bed_days * (waiting_list_reduction()/100),
             #bd_ooa_repat                =  case_when(oop_flag == 1 ~ bed_days * (ooa_repat), TRUE ~ 0),
             #bd_oap_repat_outgoing       = case_when(ooa_group == "oap_outgoing" ~ bed_days*(ooa_repat/100), TRUE ~ 0),
             #bd_oap_repat_incoming       = case_when(ooa_group == "oap_incoming" ~ bed_days*((ooa_repat/100)*-1), TRUE ~ 0),
             #bd_shift_to_ip              =  case_when(provider_type == "NHS" ~ bed_days * ((shift_to_ip()/100)*-1), TRUE ~ 0),
             
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
             #exHL_bedday_ooa_repat                =  case_when(oop_flag == 1 ~ bed_days_exHL * (ooa_repat), TRUE ~ 0),
             #exHL_bedday_oap_repat_outgoing       = case_when(ooa_group == "oap_outgoing" ~ bed_days_exHL*(ooa_repat/100), TRUE ~ 0),
             #exHL_bedday_oap_repat_incoming       = case_when(ooa_group == "oap_incoming" ~ bed_days_exHL*((ooa_repat/100)*-1), TRUE ~ 0),
             #exHL_bedday_shift_to_ip              =  case_when(provider_type == "NHS" ~ bed_days_exHL * ((shift_to_ip()/100)*-1), TRUE ~ 0),
             ) |> 
      mutate(spell_proj = spell_count + rowSums(across(contains("sp_"))),
             bed_days_proj =    bed_days + rowSums(across(contains("bd_"))),
             bed_days_exHL_proj = bed_days_exHL + rowSums(across(contains("exHL_bedday_")))
      ) 
    }
  
  # Apply growth inflators to grouped activity counts - separately
  baseline_growth <- reactive({
    req(baseline_aggregate(), 
        input$icb)
    
    baseline_growth_function(c("not_oap", "oap_incoming", "oap_outgoing"))
    
  })
  
  # Aggregate up growth/reduction in activity for each factor to ICB level
  waterfall_data <- reactive({
    
    req(baseline_growth()
        #waterfall_data_outgoing(),
        #waterfall_data_incoming()
        )
    
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
                #sp_ooa_repat_outgoing      = sum(sp_oap_repat_outgoing),
                #sp_ooa_repat_incoming      = sum(sp_oap_repat_incoming),
                #sp_shift_to_ip             = sum(sp_shift_to_ip),
                
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
                #bd_ooa_repat_outgoing      = sum(bd_oap_repat_outgoing ),
                #bd_ooa_repat_incoming      = sum(bd_oap_repat_incoming ),
                #bd_shift_to_ip             = sum(bd_shift_to_ip),
                
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
                #exHL_bedday_ooa_repat_outgoing     = sum(exHL_bedday_oap_repat_outgoing    ),
                #exHL_bedday_ooa_repat_incoming     = sum(exHL_bedday_oap_repat_incoming    ),
                #exHL_bedday_shift_to_ip            = sum(exHL_bedday_shift_to_ip           ),
                
                spell_proj = sum(spell_proj),
                bed_days_proj = sum(bed_days_proj),
                bed_days_exHL_proj = sum(bed_days_exHL_proj)
                ) %>%
      mutate(icb_dummy = "ICB") |> 
      pivot_longer(cols = -icb_dummy) #%>% 
      #union_all(waterfall_data_outgoing()) %>% 
      #union_all(waterfall_data_incoming()) 
    
  })
  
  # Plot waterfall - Spells and bed days
  waterfall_plot <- reactive({
    req(waterfall_data())
    
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
    updateNumericInput(session, "prevention_programme", value = -4.8)
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
                      "Out of Area Expatriation",
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
                  input$ooa_expat,
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
  
  # Reset occupancy rates to default position
  observeEvent(input$reset_occupancy, {
    updateNumericInput(session, "current_occupancy", value = 92)
    updateNumericInput(session, "future_occupancy", value = 85)
    
  })
  
  # Out of area table ----
  baseline_oap_activity_icb <- reactive({   
    req(baseline_aggregate(),
        input$icb
        )
    
    baseline_aggregate() |>
      mutate(flag_residence = case_when(residence_icb_name == input$icb ~ "1. Internal residence",
                                        TRUE ~ "2. External residence"),
             flag_provision = case_when(provider_icb_name == input$icb ~ "1. Internal provision",
                                        TRUE ~ "2. External provision")) %>% 
      group_by(flag_residence, flag_provision) %>% 
      summarise(spells = comma(sum(spell_count))) %>% 
      ungroup() %>% 
      pivot_wider(id_cols = flag_residence, 
                  names_from = flag_provision, 
                  values_from = spells
                  ) %>% 
      rename(` ` = flag_residence)
    
    })
  
  baseline_oap_activity_icb_bed_days <- reactive({   
    req(baseline_aggregate(),
        input$icb
    )
    
    baseline_aggregate() |>
      mutate(flag_residence = case_when(residence_icb_name == input$icb ~ "1. Internal residence",
                                        TRUE ~ "2. External residence"),
             flag_provision = case_when(provider_icb_name == input$icb ~ "1. Internal provision",
                                        TRUE ~ "2. External provision")) %>% 
      group_by(flag_residence, flag_provision) %>% 
      summarise(bed_days = comma(sum(bed_days))) %>% 
      ungroup() %>% 
      pivot_wider(id_cols = flag_residence, 
                  names_from = flag_provision, 
                  values_from = bed_days
                  ) %>% 
      rename(` ` = flag_residence)
    
  })
  
  baseline_oap_activity_icb_bed_days_exHL <- reactive({   
    req(baseline_aggregate(),
        input$icb
        )
    
    baseline_aggregate() |>
      mutate(flag_residence = case_when(residence_icb_name == input$icb ~ "1. Internal residence",
                                      TRUE ~ "2. External residence"),
           flag_provision = case_when(provider_icb_name == input$icb ~ "1. Internal provision",
                                      TRUE ~ "2. External provision")) %>% 
      group_by(flag_residence, flag_provision) %>% 
      summarise(bed_days_exHL = comma(sum(bed_days_exHL))) %>% 
      ungroup() %>% 
      pivot_wider(id_cols = flag_residence, 
                  names_from = flag_provision, 
                  values_from = bed_days_exHL
                  ) %>% 
      rename(` ` = flag_residence)
    
  })
  
  # Output objects
  output$dataTable_oap <- renderDT({
    req(baseline_oap_activity_icb()
    )
    
    DT::datatable(
      baseline_oap_activity_icb(),
      extensions = "Buttons",              
      rownames = F, 
      options = list(dom = 'Blfrtip', 
                     buttons = list(list(extend = 'copy', title = NULL))
                     ) 
      ) %>%
      formatStyle(
        columns = 1,
        fontWeight = 'bold'
      )
    
  })
  
  output$dataTable_oap_bed_days <- renderDT({
    req(baseline_oap_activity_icb_bed_days()
    )
    
    DT::datatable(
      baseline_oap_activity_icb_bed_days(),
      extensions = "Buttons",              rownames = F, 
      options = list(dom = 'Blfrtip', 
                     buttons = list(list(extend = 'copy', title = NULL))
                     ) 
      ) %>%
      formatStyle(
        columns = 1,
        fontWeight = 'bold'
        )
    })
  
  output$dataTable_oap_bed_days_exHL <- renderDT({
    req(baseline_oap_activity_icb_bed_days_exHL()
    )

    DT::datatable(
      baseline_oap_activity_icb_bed_days_exHL(),
      extensions = "Buttons",              rownames = F, 
      options = list(dom = 'Blfrtip', 
                     buttons = list(list(extend = 'copy', title = NULL))
                     ) 
      ) %>%
      formatStyle(
        columns = 1,
        fontWeight = 'bold'
        )
    })
  

  # Reset occupancy rates to default position
  observeEvent(input$reset_occupancy, {
    updateNumericInput(session, "current_occupancy", value = 92)
    updateNumericInput(session, "future_occupancy", value = 85)
    
  })

  # Bed policy table ---- 
  
  # Spells
  bed_policy_table_spells <- reactive({
    req(baseline_growth(),
        input$shift_to_ip,
        input$ooa_repat,
        input$ooa_expat
    )
    
    baseline_growth() |> 
      mutate(adjusted_proj_oap =
               case_when(
                 ooa_group == "oap_outgoing" ~ spell_proj * (input$ooa_repat / 100),
                 ooa_group == "oap_incoming" ~ spell_proj * ((input$ooa_expat / 100) * -1),
                 ooa_group == "not_oap" ~ 0
               ),
             
             adj_shift_to_ind = 
               case_when(
                 (input$shift_to_ip > 0 & provider_type == "NHS") ~ spell_proj * ((input$shift_to_ip / 100) * -1),
                 TRUE ~ 0
               ),
             
             adj_shift_from_ind = 
               case_when(
                 (input$shift_to_ip < 0 & provider_type == "Independent") ~ spell_proj * ((input$shift_to_ip / 100) * -1),
                 TRUE ~ 0
               )
      ) |>  
      summarise(
        projected = comma(sum(spell_proj)),
        adj_proj_oap = comma(sum(adjusted_proj_oap)),
        adj_shift_to_ind = comma(sum(adj_shift_to_ind)),
        adj_shift_from_ind = comma(sum(adj_shift_from_ind))
      ) |> 
      rename(
        `Projected demand` = projected,
        `Out-of-area policy impact` = adj_proj_oap,
        `Shift to independent sector` = adj_shift_to_ind,
        `Shift from independent sector` = adj_shift_from_ind
      )
  })
  
  # Bed days
  bed_policy_table_bd <- reactive({
    req(baseline_growth(),
        input$shift_to_ip,
        input$ooa_repat,
        input$ooa_expat
    )
    
    baseline_growth() |> 
      mutate(adjusted_proj_oap =
               case_when(
                 ooa_group == "oap_outgoing" ~ bed_days_proj * (input$ooa_repat / 100),
                 ooa_group == "oap_incoming" ~ bed_days_proj * ((input$ooa_expat / 100) * -1),
                 ooa_group == "not_oap" ~ 0
               ),
             
             adj_shift_to_ind = 
               case_when(
                 (input$shift_to_ip > 0 & provider_type == "NHS") ~ bed_days_proj * ((input$shift_to_ip / 100) * -1),
                 TRUE ~ 0
               ),
             
             adj_shift_from_ind = 
               case_when(
                 (input$shift_to_ip < 0 & provider_type == "Independent") ~ bed_days_proj * ((input$shift_to_ip / 100) * -1),
                 TRUE ~ 0
               )
      ) |>  
      summarise(
        projected = sum(bed_days_proj),
        adj_proj_oap = sum(adjusted_proj_oap),
        adj_shift_to_ind = sum(adj_shift_to_ind),
        adj_shift_from_ind = sum(adj_shift_from_ind)
        ) |> 
      rename(
        `Projected demand` = projected,
        `Out-of-area policy impact` = adj_proj_oap,
        `Shift to independent sector` = adj_shift_to_ind,
        `Shift from independent sector` = adj_shift_from_ind
      ) |> 
      mutate(dummy = "") |> 
      pivot_longer(-dummy) |>
      rename(`Bed days` = value) |> 
      mutate(`Annualised beds` = round((`Bed days`/ (future_occupancy()/100)/365.25),1)) |> 
      pivot_longer(cols = c(`Bed days`, `Annualised beds`),
                   names_to = "Metric") |> 
      select(-dummy) |> 
      mutate(value = comma(value)) |> 
      pivot_wider(id_cols = Metric, 
                  names_from = name, 
                  values_from = value)
  })
  
  # Bed days - excl HL
  bed_policy_table_bd_exHL <- reactive({
    req(baseline_growth(),
        input$shift_to_ip,
        input$ooa_repat,
        input$ooa_expat
    )
    
    baseline_growth() |> 
      mutate(adjusted_proj_oap =
               case_when(
                 ooa_group == "oap_outgoing" ~ bed_days_exHL_proj * (input$ooa_repat / 100),
                 ooa_group == "oap_incoming" ~ bed_days_exHL_proj * ((input$ooa_expat / 100) * -1),
                 ooa_group == "not_oap" ~ 0
               ),
             
             adj_shift_to_ind = 
               case_when(
                 (input$shift_to_ip > 0 & provider_type == "NHS") ~ bed_days_exHL_proj * ((input$shift_to_ip / 100) * -1),
                 TRUE ~ 0
               ),
             
             adj_shift_from_ind = 
               case_when(
                 (input$shift_to_ip < 0 & provider_type == "Independent") ~ bed_days_exHL_proj * ((input$shift_to_ip / 100) * -1),
                 TRUE ~ 0
               )
      ) |>  
      summarise(
        projected = sum(bed_days_exHL_proj),
        adj_proj_oap = sum(adjusted_proj_oap),
        adj_shift_to_ind = sum(adj_shift_to_ind),
        adj_shift_from_ind = sum(adj_shift_from_ind)
        ) |> 
      rename(
        `Projected demand` = projected,
        `Out-of-area policy impact` = adj_proj_oap,
        `Shift to independent sector` = adj_shift_to_ind,
        `Shift from independent sector` = adj_shift_from_ind
      ) |> 
      mutate(dummy = "") |> 
      pivot_longer(-dummy) |>
      rename(`Bed days` = value) |> 
      mutate(`Annualised beds` = round((`Bed days`/ (future_occupancy()/100)/365.25),1)) |> 
      pivot_longer(cols = c(`Bed days`, `Annualised beds`),
                   names_to = "Metric") |> 
      select(-dummy) |> 
      mutate(value = comma(value)) |> 
      pivot_wider(id_cols = Metric, 
                  names_from = name, 
                  values_from = value)
  })
  
  # Output objects
  output$bed_policy_table_spells <- renderDT({
    req(bed_policy_table_spells())
    
    DT::datatable(bed_policy_table_spells(), 
                  extensions = "Buttons",              rownames = F, 
                  options = list(dom = 'Blfrtip', 
                                 buttons = list(list(extend = 'copy', title = NULL))))
    
  })
  
  output$bed_policy_table_bd <- renderDT({
    req(bed_policy_table_bd())
    
    DT::datatable(bed_policy_table_bd(), 
                  extensions = "Buttons",              rownames = F, 
                  options = list(dom = 'Blfrtip', 
                                 buttons = list(list(extend = 'copy', title = NULL))))
    
  })
  
  output$bed_policy_table_bd_exHL <- renderDT({
    req(bed_policy_table_bd_exHL())
    
    DT::datatable(bed_policy_table_bd_exHL(), 
                  extensions = "Buttons",              rownames = F, 
                  options = list(dom = 'Blfrtip', 
                                 buttons = list(list(extend = 'copy', title = NULL))))
    
  })
  
  
  # Reset bed policy factors to default position
  observeEvent(input$reset_bed_policy, {
    updateNumericInput(session, "ooa_repat", value = 50)
    updateNumericInput(session, "ooa_expat", value = 50)
    updateNumericInput(session, "shift_to_ip", value = 0)
  })
  
  
  # Provider and OAP-group breakdown at baseline ----
  
  # Spells
  bed_policy_oap_group_baseline <- reactive({
    req(baseline_growth())
    
    baseline_growth() |> 
      group_by(provider_type, ooa_group) |> 
      mutate(ooa_group = 
               case_when(
                 ooa_group == "not_oap" ~ "Not OAP",
                 ooa_group == "oap_outgoing" ~ "Outgoing placement",
                 ooa_group == "oap_incoming" ~ "Incoming placement"
               )) |> 
      summarise(count = sum(spell_count)) |> 
      mutate(Total = comma(sum(count))) |> 
      mutate(count = comma(count)) |>
      
      pivot_wider(id_cols = c(provider_type, Total), 
                  names_from = ooa_group,
                  values_from = count) |> 
      select(provider_type, `Not OAP`, `Outgoing placement`, `Incoming placement`, Total) |> 
      rename(" " = provider_type)
    })
  
  # Bed days
  bed_policy_oap_group_baseline_bd <- reactive({
    req(baseline_growth())
    
    baseline_growth() |> 
      group_by(provider_type, ooa_group) |> 
      mutate(ooa_group = 
               case_when(
                 ooa_group == "not_oap" ~ "Not OAP",
                 ooa_group == "oap_outgoing" ~ "Outgoing placement",
                 ooa_group == "oap_incoming" ~ "Incoming placement"
               )) |> 
      summarise(count = sum(bed_days)) |> 
      mutate(Total = comma(sum(count))) |> 
      mutate(count = comma(count)) |>
      
      pivot_wider(id_cols = c(provider_type, Total), 
                  names_from = ooa_group,
                  values_from = count) |> 
      select(provider_type, `Not OAP`, `Outgoing placement`, `Incoming placement`, Total) |> 
      rename(" " = provider_type)
    })
  
  # Bed days - exHL
  bed_policy_oap_group_baseline_bd_exHL <- reactive({
    req(baseline_growth())
    
    baseline_growth() |> 
      group_by(provider_type, ooa_group) |> 
      mutate(ooa_group = 
               case_when(
                 ooa_group == "not_oap" ~ "Not OAP",
                 ooa_group == "oap_outgoing" ~ "Outgoing placement",
                 ooa_group == "oap_incoming" ~ "Incoming placement"
               )) |> 
      summarise(count = sum(bed_days_exHL)) |> 
      mutate(Total = comma(sum(count))) |> 
      mutate(count = comma(count)) |>
      
      pivot_wider(id_cols = c(provider_type, Total), 
                  names_from = ooa_group,
                  values_from = count) |> 
      select(provider_type, `Not OAP`, `Outgoing placement`, `Incoming placement`, Total) |> 
      rename(" " = provider_type)
    
    })
  
  # Output objects
  output$bed_policy_oap_group_baseline <- renderDT({
    req(bed_policy_oap_group_baseline())
    
    DT::datatable(bed_policy_oap_group_baseline(), 
                  extensions = "Buttons",              rownames = F, 
                  options = list(dom = 'Blfrtip', 
                                 buttons = list(list(extend = 'copy', title = NULL))))
    
  })
  
  output$bed_policy_oap_group_baseline_bd <- renderDT({
    req(bed_policy_oap_group_baseline_bd())
    
    DT::datatable(bed_policy_oap_group_baseline_bd(), 
                  extensions = "Buttons",              rownames = F, 
                  options = list(dom = 'Blfrtip', 
                                 buttons = list(list(extend = 'copy', title = NULL))))
    
  })
  
  output$bed_policy_oap_group_baseline_bd_exHL <- renderDT({
    req(bed_policy_oap_group_baseline_bd_exHL())
    
    DT::datatable(bed_policy_oap_group_baseline_bd_exHL(), 
                  extensions = "Buttons",              rownames = F, 
                  options = list(dom = 'Blfrtip', 
                                 buttons = list(list(extend = 'copy', title = NULL))))
    
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
                bed_days_exHL = sum(bed_days_exHL),
                
                spell_proj = sum(spell_proj),
                bed_days_proj = sum(bed_days_proj),
                bed_days_exHL_proj = sum(bed_days_exHL_proj)
      ) |>
      rename(group_name = 1) |>
      drop_na(group_name) |> 
      pivot_longer(-group_name) |> 
      mutate(flag = 
               case_when(str_detect(name, "spell_") ~ "1. Spells", 
                         str_detect(name, "bed_days_exHL") ~ "3. Bed days - excl Home Leave", 
                         TRUE ~ "2. Bed days"),
             current_projection = 
               case_when(str_detect(name, "proj") ~ "Projection", 
                         TRUE ~ "Current")
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
  
 
  
  # Meta data table and glossary ----
  
  # Meta data table
  baseline_extract_meta <- read_excel("reference files/baseline_extract_meta.xlsx")
    
  
  output$baseline_extract_meta <- renderDT({
    req(icb_weighted_demographic_change)
    
    DT::datatable(
      baseline_extract_meta,
      options = list(pageLength = 20)
    )
  })
  
  # Glossary of terms
  output$glossary <- renderDT({
    
    
    DT::datatable(
      tribble(
        
        ~Term, ~Description,
        
        "CSV file"  , "Comma Separated Values file - a type of spreadsheet.",
        "Parameters", "A value that is used to alter another - in our context the baseline of bed utilisation.",
        "Baseline"  , "The activity at the start of our modelling period. In this case, the NHS bed utilisation for residents of the ICB or treated in the ICB, 12 months to June 2024.",
        "Projection", "The activity at the end of our modelling period i.e. accounting for the increases or decreases specified by our parameters.",
        "Demand"    , "A term to describe the utililsation of something - in our case spells of care for patient in mental health inpatient beds.",
        "Capacity"  , "A term to describe what resource is used to fulfil the above demand - in our case, the beds themselves.",
        "PTSD"      , "Post-Traumatic Stress Disorder.",
        "APMS"      , "Adult Psychiatric Morbidity Survey, periodical survey of adult mental health in England.",
        "QOF"       , "Quality and Outcomes Framework, an incentive framework to encourage disease management in primary care.",
        "LoS"       , "Length of Stay, the time a patient spends in a hospital, ward or bed.",
        "CBT"       , "Cognitive Behavioural Therapy, a psychological approach to changing thoughts and behaviours.",
        "LDA"       , "Learning Disability or Autism, the presence or otherwise of these diagnoses in patient records.",
        "OAP"       , "Out of Area Placement, a patient receiving care outside of their resident or responsible healthcare boundary.",
        "Repatriate", "Bring ICB residents in out of area beds back to beds in the ICB",
        "Expatriate", "Move non-ICB residents in beds in our area back to beds in their own ICB",
        "Occupancy" , "The share of available resource that is utilised at a point in time - in our case, inpatient beds.",
        "Annualised", "Converting time-based data into typical yearly units - in our case total bed days to beds.",
        "Home Leave", "A period of inpatient care spent/managed at home. In our tool/data, the user can choose to count the full period of inpatient care or excluding the days on home leave.",
        "IMD"       , "Index of Multiple Deprivation, a composite measure of social disadvantage across England."
        ),
      options = list(pageLength = 20)
      )
    })
  
  
  
  # Download data ----

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("projected_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data <- 
        baseline_growth() |> 
        select(-c(spell_count, contains("sp_"), spell_proj)) |>
        mutate(aadjusted_proj_oap =
                 case_when(
                   ooa_group == "oap_outgoing" ~ bed_days_proj * (input$ooa_repat / 100),
                   ooa_group == "oap_incoming" ~ bed_days_proj * ((input$ooa_expat / 100) * -1),
                   ooa_group == "not_oap" ~ 0
                 ),
               
               adj_shift_to_ind = 
                 case_when(
                   (input$shift_to_ip > 0 & provider_type == "NHS") ~ bed_days_proj * ((input$shift_to_ip / 100) * -1),
                   TRUE ~ 0
                 ),
               
               adj_shift_from_ind = 
                 case_when(
                   (input$shift_to_ip < 0 & provider_type == "Independent") ~ bed_days_proj * ((input$shift_to_ip / 100) * -1),
                   TRUE ~ 0
                 ),
               
               adjusted_proj_oap_exHL =
                 case_when(
                   ooa_group == "oap_outgoing" ~ bed_days_exHL_proj * (input$ooa_repat / 100),
                   ooa_group == "oap_incoming" ~ bed_days_exHL_proj * ((input$ooa_expat / 100) * -1),
                   ooa_group == "not_oap" ~ 0
                 ),
               
               adj_shift_to_ind_exHL = 
                 case_when(
                   (input$shift_to_ip > 0 & provider_type == "NHS") ~ bed_days_exHL_proj * ((input$shift_to_ip / 100) * -1),
                   TRUE ~ 0
                 ),
               
               adj_shift_from_ind_exHL = 
                 case_when(
                   (input$shift_to_ip < 0 & provider_type == "Independent") ~ bed_days_exHL_proj * ((input$shift_to_ip / 100) * -1),
                   TRUE ~ 0
                 ),
               
               baseline_annualised_beds = (bed_days/(current_occupancy()/100)/365.25),
               baseline_annualised_beds_exHL = (bed_days_exHL/(current_occupancy()/100)/365.25),
               
               proj_annualised_beds = (bed_days_proj/(future_occupancy()/100)/365.25), 
               proj_annualised_beds_exHL = (bed_days_exHL_proj/(future_occupancy()/100)/365.25)
               
        )
      
      # Create a workbook and add worksheets
      wb <- createWorkbook()
      addWorksheet(wb, "Data")
      addWorksheet(wb, "Metadata")
      
      # Write data to the first sheet
      writeData(wb, "Data", data)
      
      # Create metadata
      field_names <- names(data)
      descriptions <- c(
        
        "Code representing the Integrated Care Board (ICB) of the patient's residence.",
        "Name of the Integrated Care Board (ICB) of the patient's residence.",
        "Code representing the Integrated Care Board (ICB) of the healthcare provider.",
        "Name of the Integrated Care Board (ICB) of the healthcare provider.",
        "Age group of the patient at the time of admission.",
        "Gender of the patient.",
        "Ethnic category of the patient, using a broad classification system.",
        "Index of Multiple Deprivation (IMD) quintile, indicating the level of deprivation of the patient's area of residence.",
        "Type of healthcare provider (e.g., NHS, Independent).",
        "Legal status group of the patient (e.g., voluntary, detained under the Mental Health Act).",
        "Flag indicating whether the patient has a learning disability or autism.",
        "Description of the type of ward where the patient was first admitted.",
        "Flag indicating whether the patient is an out-of-area placement.",
        "Total number of bed days for the patient group at baseline.",
        "Total number of bed days at baseline excluding home-leave periods.",
        "Number of delayed-discharge bed days for the patient (bed days occuring after ready for discharge date before discharge date).",
        "Group classification for out-of-area patients.",
        "Bed days attributed to demographic growth.",
        "Bed days attributed to changes in incidence rates.",
        "Bed days attributed to changes in patient acuity.",
        "Bed days attributed to social care pressures.",
        "Bed days attributed to changes in the Mental Health Act.",
        "Bed days attributed to national policy changes.",
        "Bed days attributed to changes in service models.",
        "Bed days attributed to prevention programs.",
        "Bed days attributed to admission avoidance strategies.",
        "Bed days attributed to waiting list reduction efforts.",
        "Bed days excluding home-leave care attributed to demographic growth.",
        "Bed days excluding home-leave care attributed to changes in incidence rates.",
        "Bed days excluding home-leave care attributed to changes in patient acuity.",
        "Bed days excluding home-leave care attributed to social care pressures.",
        "Bed days excluding home-leave care attributed to changes in the Mental Health Act.",
        "Bed days excluding home-leave care attributed to national policy changes.",
        "Bed days excluding home-leave care attributed to changes in service models.",
        "Bed days excluding home-leave care attributed to prevention programs.",
        "Bed days excluding home-leave care attributed to admission avoidance strategies.",
        "Bed days excluding home-leave care attributed to waiting list reduction efforts.",
        "Projected number of bed days.",
        "Projected number of bed days excluding home-leave periods.",
        "Adjusted projected bed days for out-of-area patients.",
        "Adjusted shift to independent providers.",
        "Adjusted shift from independent providers.",
        "Adjusted projected bed days excluding home-leave periods for out-of-area patients.",
        "Adjusted shift to independent providers excluding home-leave periods.",
        "Adjusted shift from independent providers excluding home-leave periods.",
        "Baseline annualised number of beds.",
        "Baseline annualised number of beds excluding home-leave periods.",
        "Projected annualised number of beds.",
        "Projected annualised number of beds excluding home-leave periods."
        )
      
      # Ensure descriptions match the number of fields
      if (length(descriptions) < length(field_names)) {
        descriptions <- c(descriptions, rep("Description not provided", length(field_names) - length(descriptions)))
      }
      
      metadata <- data.frame(
        Field_Name = field_names,
        Description = descriptions
      )
      
      # Write metadata to the second sheet
      writeData(wb, "Metadata", metadata)
      
      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  
  
  
  
  
  
  }


# Run the application ----
shinyApp(ui = ui, server = server)
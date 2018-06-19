# Define UI ----
source("functions.R")


##########################################################################################################################
# Set up the UI using fluid rows #########################################################################################
ui <- fluidPage(theme = "bootstrap.css", tags$head(includeHTML("google-analytics.html")),

  fluidRow(style="background-color:#EA8A35;;color:white;",
    column(2, imageOutput('image1', height="160px")),
    column(9, h1(tags$b("MetaMEx")),
              h3("Transcriptomic meta-analysis of skeletal muscle response to exercise"),
              h4(a("Nicolas J. Pillon,", href="https://nicopillon.com",         style="color:#ffeb3d;", target="_blank"),
                 a("Anna Krook,",        href="https://ki.se/en/people/annkro", style="color:#ffeb3d;", target="_blank"),
                 a("Juleen R. Zierath",  href="https://ki.se/en/people/julzie", style="color:#ffeb3d;", target="_blank")))),

  fluidRow(style="background-color:#F3BB8A;padding:1%",
    column(12, "Use MetaMEx to test the behavior of a gene in skeletal muscle during exercise and inactivity. Type the official gene symbol and select your population of interest.")),
  
  fluidRow(style="background-color:#F3BB8A;padding:1%",
    column(2, selectInput("genename", "Official Gene Name", choice=genelist, selectize=T, selected='PPARGC1A')), #input official gene name
    column(1, checkboxGroupInput("muscle", "Muscle", selected=c("VAL", "BIB", "SOL", "N.A"), muscle_choice), #checkbox to select category
              checkboxInput('bar_muscle', 'All/None', value=T)), #checkbox to select all
    column(1, checkboxGroupInput("sex", "Sex", selected=c("M", "F", "U"), sex_choice), #checkbox to select category
              checkboxInput('bar_sex', 'All/None', value=T)), #checkbox to select all
    column(1, checkboxGroupInput("age", "Age", selected=c("YNG", "MDL", "ELD"), age_choice), #checkbox to select category
              checkboxInput('bar_age', 'All/None', value=T)), #checkbox to select all
    column(1, checkboxGroupInput("training", "Fitness", selected=c("SED", "ACT", "ATH"), training_choice), #checkbox to select category
              checkboxInput('bar_training', 'All/None', value=T)), #checkbox to select all
    column(2, checkboxGroupInput("disease", "Health status", selected=c("HLY", "OBE", "T2D", "MTS", "CKD", "COP"), disease_choice), #checkbox to select category
              checkboxInput('bar_disease', 'All/None', value=T)), #checkbox to select all
    column(2, checkboxGroupInput("biopsy", "Biopsy collection", selected=c("IMM", "REC"), biopsy_choice), #checkbox to select category
              checkboxInput('bar_biopsy', 'All/None', value=T)), #checkbox to select all
    column(1, checkboxGroupInput("exercisetype", "Type", selected=c("CON", "ECC", "MIX"), exercise_choice), #checkbox to select category
              checkboxInput('bar_exercisetype', 'All/None', value=T))), #checkbox to select all

  fluidRow(
    column(6,  h3("Acute Aerobic Exercise"), 
               plotOutput("Acute_A"),
           tags$tr(),
               checkboxGroupInput("AA_studies", "Acute Aerobic Studies", selected=AA_names, AA_names, inline=TRUE),
                  checkboxInput('AA_all', 'Select all/none', value=T), style="padding:0 0 0 3%"),
    
    column(6,  h3("Acute Resistance Exercise"),
               plotOutput("Acute_R", height="400px"),
               checkboxGroupInput("AR_studies", "Acute Resistance Studies", selected=AR_names, AR_names, inline=TRUE),
                  checkboxInput('AR_all', 'Select all/none', value=T), style="padding:0 0 0 3%")
  ),
  
  tags$hr(),
  
  fluidRow(align="top",
    column(6,  
               h3("Training Aerobic Exercise"),
               plotOutput("Training_A", height="550px"),
               checkboxGroupInput("TA_studies", "Training Aerobic Studies", selected=TA_names, TA_names, inline=TRUE),
                  checkboxInput('TA_all', 'Select all/None', value=T), style="padding:0 0 0 3%"),
    column(6,  h3("Training Resistance Exercise"),
               plotOutput("Training_R", height="550px"),
               checkboxGroupInput("TR_studies", "Training Resistance Studies", selected=TR_names, TR_names, inline=TRUE),
                  checkboxInput('TR_all', 'Select all/none', value=T), style="padding:0 0 0 3%")
  ),
  
  tags$hr(),
  
  fluidRow(
    column(6, h3("Training Combined Exercise"),
              plotOutput("Training_C", height="230px"),
              checkboxGroupInput("TC_studies", "Training Combined Studies", selected=TC_names, TC_names, inline=TRUE),
                 checkboxInput('TC_all', 'Select all/none', value=T), style="padding:0 0 0 3%"),
    column(6, h3("Physical Inactivity"),
              plotOutput("Inact", height="230px"),
              checkboxGroupInput("IN_studies", "Physical Inactivity studies", selected=IN_names, IN_names, inline=TRUE),
                 checkboxInput('IN_all', 'Select all/none', value=T), style="padding:0 0 0 3%")
  ),
  

  fluidRow(style="background-color:#F3BB8A;padding:1%",
    column(3, h3("Get the data"),
              tags$p(downloadButton("downloadData", "Download Statistics (.xlsx)")), 
              tags$p(downloadButton("downloadReport", "Download Report (.pdf)")),
              h3("Notes"),
              h5("This meta-analysis was created by collecting publicly available studies on
                    mRNA expression levels in human skeletal muscle after exercise or inactivity. Statistics were first perfomed
                    individually for each array. The meta-analysis summary was then calculated using a random effects model (REML).
                    Forest plots present the log2(fold-change) and 95% confidence intervals for each study as well as the meta-analysis score and adjusted p value."),
              h5("The complete parameters for each study are available in a",
                    a("list of all datasets.", href="https://docs.google.com/spreadsheets/d/19MIe6-GgtBXzyyAxNwN6j7snzo-1xSKk98h6VopgSEk/edit?usp=sharing", target="_blank")),
              imageOutput('image2'), align="centre"),
    column(9, h3("Categorization of studies"),
              tableOutput("Annotation"),
              h5("Your study is not included? You have information on age, sex, BMI or else for one of the studies? Please",
                 a("contact us!", href="mailto:nicolas.pillon@ki.se")),
              h5(tags$b("The more data we collect, the stronger MetaMex becomes!")))
  ),
  
  fluidRow(style="background-color:#EA8A35;padding:0 0 1% 1%;color:white;",
    column(12, h3("Copyrights - MetaMex 2.8"),
               h5("MetaMEx was created by", a("Nicolas J. Pillon", href="http://nicopillon.com", style="color:#ffeb3d;", target="_blank"),
                  "and illustrated by", a("Csil", href="http://misshue.net", style="color:#ffeb3d;", target="_blank"),
                  "under the Creative Commons Attribution-NonCommercial 4.0 International",
                  a("(CC BY-NC 4.0).", href="https://creativecommons.org/licenses/by-nc/4.0/", style="color:#ffeb3d;", target="_blank")))
  )
)

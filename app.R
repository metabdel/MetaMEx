# Define UI ----

#load libraries
library(metafor)
library(forestplot)
library(ggpubr)
library(xlsx)
library(stringr)
library(rmarkdown)
library(grid)
library(gridExtra)
library(readr)

# Load the different datasets, all in csv format
# Each dataset file contains several columns for each study: fold-change, false discovery rate, mean, standard deviation, n size.
Stats_AA <- read_csv("Acute_Aerobic_Merged_Stats_SYMBOL.csv", col_names = TRUE,
                     col_types = cols(.default= col_number(), X1 = col_character()))
Stats_AA <- data.frame(Stats_AA, row.names = 1)

Stats_AR <- read_csv("Acute_Resistance_Merged_Stats_SYMBOL.csv", col_names = TRUE,
                     col_types = cols(.default= col_number(), X1 = col_character()))
Stats_AR <- data.frame(Stats_AR, row.names = 1)

Stats_TA <- read_csv("Training_Aerobic_Merged_Stats_SYMBOL.csv", col_names = TRUE,
                     col_types = cols(.default= col_number(), X1 = col_character()))
Stats_TA <- data.frame(Stats_TA, row.names = 1)

Stats_TR <- read_csv("Training_Resistance_Merged_Stats_SYMBOL.csv", col_names = TRUE,
                     col_types = cols(.default= col_number(), X1 = col_character()))
Stats_TR <- data.frame(Stats_TR, row.names = 1)

Stats_TC <- read_csv("Training_Combined_Merged_Stats_SYMBOL.csv", col_names = TRUE,
                     col_types = cols(.default= col_number(), X1 = col_character()))
Stats_TC <- data.frame(Stats_TC, row.names = 1)

Stats_IN <- read_csv("Inactivity_Merged_Stats_SYMBOL.csv", col_names = TRUE,
                     col_types = cols(.default= col_number(), X1 = col_character()))
Stats_IN <- data.frame(Stats_IN, row.names = 1)


# Make a list of the different studies in each file
AA_names <- sub('.*GSE', 'GSE', colnames(Stats_AA)) #select study names 'GSEXXXXX'
AA_names <- sort(unique(sub('_.*', '', AA_names)))  #delete study info after '_', take unique and sort

AR_names <- sub('.*GSE', 'GSE', colnames(Stats_AR))
AR_names <- sort(unique(sub('_.*', '', AR_names)))

TA_names <- sub('.*GSE', 'GSE', colnames(Stats_TA))
TA_names <- sort(unique(sub('_.*', '', TA_names)))

TR_names <- colnames(Stats_TR[grepl('logFC', colnames(Stats_TR))])
TR_names <- gsub("logFC_","",TR_names)
TR_names <- gsub("_.*","",TR_names)
TR_names <- sort(TR_names[!duplicated(TR_names)])

TC_names <- sub('.*GSE', 'GSE', colnames(Stats_TC))
TC_names <- sort(unique(sub('_.*', '', TC_names)))

IN_names <- sub('.*GSE', 'GSE', colnames(Stats_IN))
IN_names <- sort(unique(sub('_.*', '', IN_names)))

TR_names
# Load the table describing the legend of the tables
annotation <- read_csv("Datasets_categories.csv", col_names = TRUE,
                       col_types=cols(.default = col_character()))


# Set up the different categories to be selected
muscle_choice <- c("Vastus Lateralis" = "_VAL_",
                   "Biceps Brachii"   = "_BIB_",
                   "Soleus"           = "_SOL_",
                   "N/A"              = "_N.A_")
sex_choice <- c("Male"       = "_M_",
                "Female"     = "_F_",
                "Undefined"  = "_U_")
age_choice <- c("Young"   = "YNG",
                "Middle"    = "MDL",
                "Elderly" = "ELD")
training_choice <- c("Sedentary" = "SED",
                     "Active"    = "ACT",
                     "Athlete"   = "ATH")
exercise_choice <- c("Concentric" = "CON",
                     "Eccentric"  = "ECC",
                     "Mixed"      = "MIX")
biopsy_choice <- c("Immediately after" = "IMM",
                   "After a recovery period"    = "REC")
disease_choice <- c("Healthy" = "HLY",
                   "Overweight/Obese" = "OBE",
                   "Type 2 diabetes" = "T2D",
                   "Metabolic Syndrome" = "MTS",
                   "Chronic Kidney Disease" = "CKD",
                   "Chronic Obstructive Pulmonary Disease" = "COP")


# Set up the graphical parameters for the forest plots
own <- fpTxtGp()
own$ticks$cex <- 0.8 #tick labels
own$xlab$cex <- 1
own$label$cex <- 0.9
own$summary$cex <- 1.2


# sanitize errors
options(shiny.sanitize.errors=T)


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
    column(2, textInput("genename", "Official Gene Name", value = "PPARGC1A")), #input official gene name
    column(1, checkboxGroupInput("muscle", "Muscle", selected=c("_VAL_", "_BIB_", "_SOL_", "_N.A_"), muscle_choice), #checkbox to select category
              checkboxInput('bar_muscle', 'All/None', value=T)), #checkbox to select all
    column(1, checkboxGroupInput("sex", "Sex", selected=c("_M_", "_F_", "_U_"), sex_choice), #checkbox to select category
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
               plotOutput("Acute_A", height="400px"),
           tags$tr(),
               checkboxGroupInput("AA_studies", "Acute Aerobic Studies", selected=AA_names, AA_names, inline=TRUE),
                  checkboxInput('AA_all', 'Select all/none', value=T), style="padding:0 0 0 3%"),
    
    column(6,  h3("Acute Resistance Exercise"),
               plotOutput("Acute_R", height="400px"),
               checkboxGroupInput("AR_studies", "Acute Resistance Studies", selected=AR_names, AR_names, inline=TRUE),
                  checkboxInput('AR_all', 'Select all/none', value=T), style="padding:0 0 0 3%")
  ),
  
  tags$hr(),
  
  fluidRow(
    column(6,  h3("Training Aerobic Exercise"),
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


####################################################################################################################
# Define server logic ##############################################################################################
server <- function(input, output, session) {

#- Make all checkboxes selected by default ----------------------------------------
  observe({ updateCheckboxGroupInput(session, 'muscle',      choices = muscle_choice,   selected = if (input$bar_muscle) muscle_choice)})
  observe({ updateCheckboxGroupInput(session, 'sex',         choices = sex_choice,      selected = if (input$bar_sex) sex_choice)})
  observe({ updateCheckboxGroupInput(session, 'age',         choices = age_choice,      selected = if (input$bar_age) age_choice)})
  observe({ updateCheckboxGroupInput(session, 'training',    choices = training_choice, selected = if (input$bar_training) training_choice)})
  observe({ updateCheckboxGroupInput(session, 'disease',     choices = disease_choice,  selected = if (input$bar_disease) disease_choice)})
  observe({ updateCheckboxGroupInput(session, 'biopsy',      choices = biopsy_choice,   selected = if (input$bar_biopsy) biopsy_choice)})
  observe({ updateCheckboxGroupInput(session, 'exercisetype',choices = exercise_choice, selected = if (input$bar_exercisetype) exercise_choice)})
  observe({ updateCheckboxGroupInput(session, 'AA_studies',  choices = AA_names,        selected = if (input$AA_all) AA_names, inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'AR_studies',  choices = AR_names,        selected = if (input$AR_all) AR_names, inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'TA_studies',  choices = TA_names,        selected = if (input$TA_all) TA_names, inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'TR_studies',  choices = TR_names,        selected = if (input$TR_all) TR_names, inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'TC_studies',  choices = TC_names,        selected = if (input$TC_all) TC_names, inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'IN_studies',  choices = IN_names,        selected = if (input$IN_all) IN_names, inline=TRUE)})

  
#- Make image outputs ------------------------------------------------------------
  output$image1 <- renderImage({ list(src='Nico-Macrophage-bike-R.png',  height="95%") }, deleteFile = FALSE)
  output$image2 <- renderImage({ list(src='Nico-Macrophage-weight-L.png', width="90%") }, deleteFile = FALSE)

  
#- make annotation table for legend ----------------------------------------------
  output$Annotation <- renderTable(spacing='xs',{ annotation })
  
  
#- Make reactive functions to select data ----------------------------------------
  AA_data <- reactive({    
    # Select gene name and calculate parameters on filtered data
    genename   <- toupper(input$genename)
    data <- Stats_AA[genename,]
    data <- data.frame(t(data[grepl('logFC',    colnames(data))]), # M-value (M) is the log2-fold change
                       t(data[grepl('adj.P.Val',colnames(data))]), # Benjamini and Hochberg's method to control the false discovery rate
                       t(data[grepl('CI.L',     colnames(data))]), # lower limit of the 95% confidence interval
                       t(data[grepl('CI.R',     colnames(data))]), # upper limit of the 95% confidence interval
                       t(data[grepl('mean_pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean_post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd_pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd_post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size')
    data$study <- gsub("logFC_","", rownames(data))
    
    tryCatch({
    # Filter dataset based on input "datasets"      
      x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
      studies <- input$AA_studies
      for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
      data <- unique(x)
    # Filter dataset based on input "muscle"          
      x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
      studies <- input$muscle
      for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
      data <- unique(x)
    # Filter dataset based on input "sex"          
      x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
      studies <- input$sex
      for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
      data <- unique(x)
    # Filter dataset based on input "age"          
      x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
      studies <- input$age
      for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
      data <- unique(x)
    # Filter dataset based on input "training"          
      x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
      studies <- input$training
      for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
      data <- unique(x)
    # Filter dataset based on input "exercise type"          
      x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
      studies <- input$exercisetype
      for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
      data <- unique(x)
    # Filter dataset based on input "disease"          
      x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
      studies <- input$disease
      for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
      data <- unique(x)
    # Filter dataset based on input "biopsy"          
      x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
      studies <- input$biopsy
      for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
      data <- unique(x)
    # order datasets by alphabetical order
      data <- data[order(rownames(data)),]
    # meta-analysis stats
      meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                  method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
      fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
      data <- (rbind(data,
                     Acute_Aerobic_Meta_Analysis=c(meta$beta, fdr, meta$ci.lb, meta$ci.ub, rep(NA, 4), sum(data$size, na.rm=T))))
  },error=function(e) NULL)
})
  
  AR_data <- reactive({    
    # Select gene name and calculate parameters on filtered data
    genename   <- toupper(input$genename)
    data <- Stats_AR[genename,]
    data <- data.frame(t(data[grepl('logFC',    colnames(data))]), # M-value (M) is the log2-fold change
                       t(data[grepl('adj.P.Val',colnames(data))]), # Benjamini and Hochberg's method to control the false discovery rate
                       t(data[grepl('CI.L',     colnames(data))]), # lower limit of the 95% confidence interval
                       t(data[grepl('CI.R',     colnames(data))]), # upper limit of the 95% confidence interval
                       t(data[grepl('mean_pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean_post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd_pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd_post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size')
    data$study <- gsub("logFC_","", rownames(data))
    
    tryCatch({
    # Filter dataset based on input "datasets"      
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$AR_studies
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "muscle"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$muscle
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "sex"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$sex
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "age"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$age
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "training"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$training
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "exercise type"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$exercisetype
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "disease"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$disease
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "biopsy"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$biopsy
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    #order datasets by alphabetical order
    data <- data[order(rownames(data)),]
    #meta-analysis stats
    meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
    fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
    data <- (rbind(data,
                   Acute_Resistance_Meta_Analysis=c(meta$beta, fdr, meta$ci.lb, meta$ci.ub, rep(NA, 4), sum(data$size, na.rm=T))))
  },error=function(e) NULL)
  })
  
  TA_data <- reactive({    
    # Select gene name and calculate parameters on filtered data
    genename   <- toupper(input$genename)
    data <- Stats_TA[genename,]
    data <- data.frame(t(data[grepl('logFC',    colnames(data))]), # M-value (M) is the log2-fold change
                       t(data[grepl('adj.P.Val',colnames(data))]), # Benjamini and Hochberg's method to control the false discovery rate
                       t(data[grepl('CI.L',     colnames(data))]), # lower limit of the 95% confidence interval
                       t(data[grepl('CI.R',     colnames(data))]), # upper limit of the 95% confidence interval
                       t(data[grepl('mean_pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean_post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd_pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd_post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size')
    data$study <- gsub("logFC_","", rownames(data))
    
    tryCatch({
    # Filter dataset based on input "datasets"      
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$TA_studies
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "muscle"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$muscle
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "sex"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$sex
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "age"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$age
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "training"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$training
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "disease"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$disease
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    #order datasets by alphabetical order
    data <- data[order(rownames(data)),]
    #meta-analysis stats
    meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
    fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
    data <- (rbind(data,
                   Training_Aerobic_Meta_Analysis=c(meta$beta, fdr, meta$ci.lb, meta$ci.ub, rep(NA, 4), sum(data$size, na.rm=T))))
  },error=function(e) NULL)
  }) 
  
  TR_data <- reactive({    
    # Select gene name and calculate parameters on filtered data
    genename   <- toupper(input$genename)
    data <- Stats_TR[genename,]
    data <- data.frame(t(data[grepl('logFC',    colnames(data))]), # M-value (M) is the log2-fold change
                       t(data[grepl('adj.P.Val',colnames(data))]), # Benjamini and Hochberg's method to control the false discovery rate
                       t(data[grepl('CI.L',     colnames(data))]), # lower limit of the 95% confidence interval
                       t(data[grepl('CI.R',     colnames(data))]), # upper limit of the 95% confidence interval
                       t(data[grepl('mean_pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean_post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd_pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd_post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size')
    data$study <- gsub("logFC_","", rownames(data))
    
    tryCatch({
    # Filter dataset based on input "datasets"      
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$TR_studies
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "muscle"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$muscle
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "sex"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$sex
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "age"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$age
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "training"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$training
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "disease"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$disease
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    #order datasets by alphabetical order
    data <- data[order(rownames(data)),]
    #meta-analysis stats
    meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
    fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
    data <- (rbind(data,
                   Training_Resistance_Meta_Analysis=c(meta$beta, fdr, meta$ci.lb, meta$ci.ub, rep(NA, 4), sum(data$size, na.rm=T))))
  },error=function(e) NULL)
  })
  
  TC_data <- reactive({    
    # Select gene name and calculate parameters on filtered data
    genename   <- toupper(input$genename)
    data <- Stats_TC[genename,]
    data <- data.frame(t(data[grepl('logFC',    colnames(data))]), # M-value (M) is the log2-fold change
                       t(data[grepl('adj.P.Val',colnames(data))]), # Benjamini and Hochberg's method to control the false discovery rate
                       t(data[grepl('CI.L',     colnames(data))]), # lower limit of the 95% confidence interval
                       t(data[grepl('CI.R',     colnames(data))]), # upper limit of the 95% confidence interval
                       t(data[grepl('mean_pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean_post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd_pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd_post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size')
    data$study <- gsub("logFC_","", rownames(data))
    
    tryCatch({
    # Filter dataset based on input "datasets"      
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$TC_studies
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "muscle"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$muscle
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "sex"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$sex
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "age"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$age
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "training"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$training
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "disease"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$disease
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    #order datasets by alphabetical order
    data <- data[order(rownames(data)),]
    #meta-analysis stats
    meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
    fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
    data <- (rbind(data,
                   Training_Combined_Meta_Analysis=c(meta$beta, fdr, meta$ci.lb, meta$ci.ub, rep(NA, 4), sum(data$size, na.rm=T))))
  },error=function(e) NULL)
  })
  
  IN_data <- reactive({    
    # Select gene name and calculate parameters on filtered data
    genename   <- toupper(input$genename)
    data <- Stats_IN[genename,]
    data <- data.frame(t(data[grepl('logFC',    colnames(data))]), # M-value (M) is the log2-fold change
                       t(data[grepl('adj.P.Val',colnames(data))]), # Benjamini and Hochberg's method to control the false discovery rate
                       t(data[grepl('CI.L',     colnames(data))]), # lower limit of the 95% confidence interval
                       t(data[grepl('CI.R',     colnames(data))]), # upper limit of the 95% confidence interval
                       t(data[grepl('mean_pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean_post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd_pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd_post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size')
    data$study <- gsub("logFC_","", rownames(data))
    
    tryCatch({
    # Filter dataset based on input "datasets"      
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$IN_studies
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "muscle"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$muscle
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "sex"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$sex
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "age"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$age
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "training"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$training
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    # Filter dataset based on input "disease"          
    x <- data.frame(matrix(nrow=0, ncol=ncol(data)))
    studies <- input$disease
    for (i in 1:length(studies)) { x <- rbind(x, data[grepl(studies[i], data$study),]) }
    data <- unique(x)
    #order datasets by alphabetical order
    data <- data[order(rownames(data)),]
    #meta-analysis stats
    meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
    fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
    data <- (rbind(data,
                   Inactivity_Meta_Analysis=c(meta$beta, fdr, meta$ci.lb, meta$ci.ub, rep(NA, 4), sum(data$size, na.rm=T))))
    },error=function(e) NULL)
    
})
  
################################################################################################
# Make forest plot from selected data in reactives #############################################

#- Forest plot for acute aerobic--------------
  plotInputAA <- function(){
    # get the selected data for acute aerobic
    data <- AA_data()
    # add a column with the names of the studies included in the analysis
    data$study <- c(gsub("logFC_","", rownames(data[1:nrow(data)-1,])), "Acute Aerobic score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data$study),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data$study))-1),TRUE),
               xlog=F,  txt_gp = own, xlab="logFC",
               col=fpColors(box="orange2",line="orange3", summary="orange3"))
  }
  output$Acute_A <- renderPlot({
  #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$AA_studies!="",  "Please select at least one group in the study list")) 
    validate(need(!is.null(AA_data()),   "Impossible to find datasets for the selected criteria"))
  #Plot the forest plot:
    plotInputAA()
  })
  

#- forest plot for acute resistance ----------------------------
  plotInputAR <- function(){
    # get the selected data for acute resistance
    data <- AR_data()
    # add a column with the names of the studies included in the analysis
    data$study <- c(gsub("logFC_","", rownames(data[1:nrow(data)-1,])), "Acute Resistance score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data$study),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data$study))-1),TRUE),
               xlog=F,  txt_gp = own, xlab="logFC",
               col=fpColors(box="orangered2",line="orangered3", summary="orangered3"))
  }
  output$Acute_R <- renderPlot({
  #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$AR_studies!="",  "Please select at least one group in the study list"))
    validate(need(!is.null(AR_data()),   "Impossible to find datasets for the selected criteria"))
  #Plot the forest plot:
    plotInputAR()
  })
  

#- forest plot for training aerobic ----------------------------
  plotInputTA <- function(){
    # get the selected data for training aerobic
    data <- TA_data()
    # add a column with the names of the studies included in the analysis
    data$study <- c(gsub("logFC_","", rownames(data[1:nrow(data)-1,])), "Training Aerobic score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data$study),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data$study))-1),TRUE),
               xlog=F,  txt_gp = own, xlab="logFC",
               col=fpColors(box="dodgerblue3",line="dodgerblue4", summary="dodgerblue4"))
  }
    output$Training_A <- renderPlot({
    #Validate selection criteria:
      validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
      validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
      validate(need(input$age!="",         "Please select at least one group in the age category")) 
      validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
      validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
      validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
      validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
      validate(need(input$TA_studies!="",  "Please select at least one group in the study list")) 
      validate(need(!is.null(TA_data()),   "Impossible to find datasets for the selected criteria"))
   #Plot the forest plot:
      plotInputTA()
  })
  
    
#- forest plot for training resistance ----------------------------
  plotInputTR <- function(){
    # get the selected data for training resistance
    data <- TR_data()
    # add a column with the names of the studies included in the analysis
    data$study <- c(gsub("logFC_","", rownames(data[1:nrow(data)-1,])), "Training Resistance score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data$study),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data$study))-1),TRUE),
               xlog=F,  txt_gp = own, xlab="logFC",
               col=fpColors(box="green3",line="green4", summary="green4"))
  }
  output$Training_R <- renderPlot({
  #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$TR_studies!="",  "Please select at least one group in the study list")) 
    validate(need(!is.null(TR_data()),   "Impossible to find datasets for the selected criteria"))
  #Plot the forest plot:
    plotInputTR()
})

  
#- forest plot for training combined ----------------------------
  plotInputTC <- function(){
    # get the selected data for training combined
    data <- TC_data()
    # add a column with the names of the studies included in the analysis
    data$study <- c(gsub("logFC_","", rownames(data[1:nrow(data)-1,])), "Training Combined score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data$study),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data$study))-1),TRUE),
               xlog=F,  txt_gp = own, xlab="logFC",
               col=fpColors(box="aquamarine3",line="aquamarine4", summary="aquamarine4"))
  }
  output$Training_C <- renderPlot({
  #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$TC_studies!="",  "Please select at least one group in the study list"))
    validate(need(!is.null(TC_data()),   "Impossible to find datasets for the selected criteria"))
  #Plot the forest plot:
    plotInputTC()
  })

    
#- forest plot for Inactivity ----------------------------
  plotInputIN <- function(){
    # get the selected data for inactivity
    data <- IN_data()
    # add a column with the names of the studies included in the analysis
    data$study <- c(gsub("logFC_","", rownames(data[1:nrow(data)-1,])), "Physical Inactivity score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data$study),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext,
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data$study))-1),TRUE),
               xlog=F,  txt_gp = own, xlab="logFC",
               col=fpColors(box="maroon3",line="maroon4", summary="maroon4"))
  }
  output$Inact <- renderPlot({
  #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$IN_studies!="",  "Please select at least one group in the study list")) 
    validate(need(!is.null(IN_data()),   "Impossible to find datasets for the selected criteria"))
  #Plot the forest plot:
    plotInputIN()
  })
  
  
###########################################################################################################
# Make button to download data
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$genename, "_MetaMEx.xlsx", sep="") },
    content = function(file) {
      dataset <- rbind(AA_data(),
                       AR_data(),
                       TA_data(),
                       TR_data(),
                       TC_data(),
                       IN_data())
      dataset <- dataset[,c(10,1:4,9)]
      dataset$study <- gsub("logFC_","", rownames(dataset))
      write.xlsx(dataset, file, row.names=F)
    })

###########################################################################################################
# Make button to save as PDF
  output$downloadReport = downloadHandler(
    filename = function() { paste(input$genename, "_MetaMEx.pdf", sep="") },
    content = function(file) {
      #matrix
      matrix <- rbind(c(1,2),c(1,2),
                      c(3,4),c(3,4),c(3,4),
                      c(5,6))
      margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
      if(!is.null(AA_data())) { AA <- grid.grabExpr(print(plotInputAA()))} 
        else {AA <- textGrob("Acute Aerobic:\nNo data available for the selected criteria")}
      if(!is.null(AR_data())) { AR <- grid.grabExpr(print(plotInputAR()))}
        else {AR <- textGrob("Acute Resistance:\nNo data available for the selected criteria")}
      if(!is.null(TA_data())) { TA <- grid.grabExpr(print(plotInputTA()))}
        else {TA <- textGrob("Training Aerobic:\nNo data available for the selected criteria")}
      if(!is.null(TR_data())) { TR <- grid.grabExpr(print(plotInputTR()))}
        else {TR <- textGrob("Training Resistance:\nNo data available for the selected criteria")}
      if(!is.null(TC_data())) { TC <- grid.grabExpr(print(plotInputTC()))}
        else {TC <- textGrob("Training Combined:\nNo data available for the selected criteria")}
      if(!is.null(IN_data())) { IN <- grid.grabExpr(print(plotInputIN()))}
        else {IN <- textGrob("Physical Inactivity:\nNo data available for the selected criteria")}
      
      pdf(file, onefile = TRUE, width=16, height=22.6)
      grid.arrange(AA, AR, TA, TR, TC, IN,
                   top = textGrob(paste(input$genename, "transcriptional response to physical activity"), gp=gpar(fontsize=30, font=7)),
                   bottom = textGrob("MetaMEx v2.7 Copyright 2018 Nicolas J. Pillon. For more information, contact nicolas.pillon@ki.se or visit www.nicopillon.com",
                                     gp=gpar(fontsize=16)),
                   layout_matrix=matrix, vp=viewport(width=0.95, height=0.9))
      dev.off()
  })
  
  

}

# Run the app ----
shinyApp(ui = ui, server = server)
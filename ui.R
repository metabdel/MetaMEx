source("functions.R")

navbarPage(title="MetaMEx", id="inTabset",
                   #add css theme
                   theme="bootstrap.css",
                   #include google analytics
                   header=tags$head(includeHTML("google-analytics.html")),
                   #make sticky footer
                   footer=tags$footer(fluidRow(
                                   column(9, style="padding:1% 1% 1% 3%;", align="left",
                                             tags$b("Cite MetaMEx:"), tags$br(),
                                             a("Transcriptomic Meta-Analysis of Skeletal Muscle Responses to Physical Inactivity and Exercise",
                                               href="https://www.ncbi.nlm.nih.gov/pubmed/", target="_blank", style="color:#D9DADB"), tags$br(),
                                              "Nicolas J. Pillon, Anna Krook & Juleen R. Zierath. Under review 2018"),
                                   column(3, style="padding:1% 3% 1% 1%;", align="right",
                                             tags$b(HTML("Share:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp
                                                         &nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp")), tags$br(),
                                             actionButton("twitter_share", label="", icon=icon("twitter"),
                                                          onclick = sprintf("window.open('%s')", url_twitter)),
                                             actionButton("linkedin_share", label="", icon=icon("linkedin"),
                                                          onclick = sprintf("window.open('%s')", url_linkedin)),
                                             actionButton("facebook_share", label="", icon=icon("facebook"),
                                                          onclick = sprintf("window.open('%s')", url_facebook)))
                                   ), style="position:fixed;
                                             bottom:0;
                                             width:100%;
                                             height:100px;
                                             color: white;
                                             background-color: black;
                                             z-index: 1000;"),

#=======================================================================================================================        
        tabPanel("Home",
                 #code to make link between tabs
                 tags$head(tags$script(HTML('var fakeClick = function(tabName) {
                                                      var dropdownList = document.getElementsByTagName("a");
                                            for (var i = 0; i < dropdownList.length; i++) {
                                            var link = dropdownList[i];
                                            if(link.getAttribute("data-value") == tabName) {link.click();};}};'))),
                 fluidRow(
                    column(7, h1(tags$b("MetaMEx")), 
                              h4("The app to meta-analyse skeletal muscle transcriptomic response to inactivity and exercise by",
                                 a("Nicolas J. Pillon,", href="https://nicopillon.com",         target="_blank"),
                                 a("Anna Krook,",        href="https://ki.se/en/people/annkro", target="_blank"), "and",
                                 a("Juleen R. Zierath.",  href="https://ki.se/en/people/julzie", target="_blank")),
                              h4("Use MetaMEx to get a complete overview the behavior of a specific gene accross
                                 all published exercise and inactivity transcriptomic studies."),
                              tags$br(),
                              actionButton('jumpToApp', 'Get started!', width="200px",
                                           style="background-color:#E95420;border-color:#C34113;box-shadow: 0 12px 16px 0 rgba(0,0,0,0.24), 0 17px 50px 0 rgba(0,0,0,0.19);")),
                    column(5, tags$img(src='Nico-Macrophage-weight-L.png', width="100%", style="padding:0 5% 0 0")))
        ),

#=======================================================================================================================        
        tabPanel("Meta-analysis", value="panelApp",
                 fluidRow(style="background-color:#edcdc2;padding:1%",
                          h3("Meta-analysis"), "Find out how your gene of interest behaves in response to exercise and inactivity.
                          Type the official gene symbol and select your population of interest."),
                 
                 fluidRow(style="background-color:#edcdc2;padding:1% 0 0 0",
                          column(2, selectizeInput("genename", "Official Gene Name", choices=NULL, selected=NULL, options=NULL)), #input official gene name
                          column(1, checkboxGroupInput("muscle", "Muscle", selected=c("VAL", "BIB", "SOL", "N.A"), list_categories[['muscle_choice']]), #checkbox to select category
                                 checkboxInput('bar_muscle', 'All/None', value=T)), #checkbox to select all
                          column(1, checkboxGroupInput("sex", "Sex", selected=c("M", "F", "U"), list_categories[['sex_choice']]), #checkbox to select category
                                 checkboxInput('bar_sex', 'All/None', value=T)), #checkbox to select all
                          column(1, checkboxGroupInput("age", "Age", selected=c("YNG", "MDL", "ELD"), list_categories[['age_choice']]), #checkbox to select category
                                 checkboxInput('bar_age', 'All/None', value=T)), #checkbox to select all
                          column(1, checkboxGroupInput("training", "Fitness", selected=c("SED", "ACT", "ATH"), list_categories[['training_choice']]), #checkbox to select category
                                 checkboxInput('bar_training', 'All/None', value=T)), #checkbox to select all
                          column(2, checkboxGroupInput("disease", "Health status", selected=c("HLY", "OBE", "T2D", "MTS", "CKD", "COP"), list_categories[['disease_choice']]), #checkbox to select category
                                 checkboxInput('bar_disease', 'All/None', value=T)), #checkbox to select all
                          column(2, checkboxGroupInput("biopsy", "Biopsy collection", selected=c("IMM", "REC"), list_categories[['biopsy_choice']]), #checkbox to select category
                                 checkboxInput('bar_biopsy', 'All/None', value=T)), #checkbox to select all
                          column(1, checkboxGroupInput("exercisetype", "Type", selected=c("CON", "ECC", "MIX"), list_categories[['exercise_choice']]), #checkbox to select category
                                 checkboxInput('bar_exercisetype', 'All/None', value=T)) #checkbox to select all
                 ),

                 fluidRow(
                   column(6,  h3("Acute Aerobic Exercise"), 
                          plotOutput("Acute_A", height="450px"),
                          checkboxGroupInput("AA_studies", "Acute Aerobic Studies", selected=list_datasets[[1]], list_datasets[[1]], inline=TRUE),
                          checkboxInput('AA_all', 'Select all/none', value=T), style="padding:0 0 0 3%"),
                   
                   column(6,  h3("Acute Resistance Exercise"),
                          plotOutput("Acute_R", height="450px"),
                          checkboxGroupInput("AR_studies", "Acute Resistance Studies", selected=list_datasets[[2]], list_datasets[[2]], inline=TRUE),
                          checkboxInput('AR_all', 'Select all/none', value=T), style="padding:0 0 0 3%")
                 ),
                 
                 tags$hr(),
                 
                 fluidRow(align="top",
                          column(6,  
                                 h3("Training Aerobic Exercise"),
                                 plotOutput("Training_A", height="500px"),
                                 checkboxGroupInput("TA_studies", "Training Aerobic Studies", selected=list_datasets[[3]], list_datasets[[3]], inline=TRUE),
                                 checkboxInput('TA_all', 'Select all/None', value=T), style="padding:0 0 0 3%"),
                          column(6,  h3("Training Resistance Exercise"),
                                 plotOutput("Training_R", height="500px"),
                                 checkboxGroupInput("TR_studies", "Training Resistance Studies", selected=list_datasets[[4]], list_datasets[[4]], inline=TRUE),
                                 checkboxInput('TR_all', 'Select all/none', value=T), style="padding:0 0 0 3%")
                 ),
                 
                 tags$hr(),
                 
                 fluidRow(
                   column(6, h3("Training HIIT Exercise"),
                          plotOutput("Training_H", height="180px"),
                          checkboxGroupInput("HI_studies", "Training HIIT studies", selected=list_datasets[[5]], list_datasets[[5]], inline=TRUE),
                          checkboxInput('HI_all', 'Select all/none', value=T), style="padding:0 0 0 3%"),
                   column(6, h3("Training Combined Exercise"),
                          plotOutput("Training_C", height="250px"),
                          checkboxGroupInput("TC_studies", "Training Combined Studies", selected=list_datasets[[6]], list_datasets[[6]], inline=TRUE),
                          checkboxInput('TC_all', 'Select all/none', value=T), style="padding:0 0 0 3%")

                 ),
                 
                 tags$hr(),
                 
                 fluidRow(
                   column(6, h3("Physical Inactivity"),
                          plotOutput("Inact", height="300px"),
                          checkboxGroupInput("IN_studies", "Physical Inactivity studies", selected=list_datasets[[7]], list_datasets[[7]], inline=TRUE),
                          checkboxInput('IN_all', 'Select all/none', value=T), style="padding:0 0 0 3%")
                 ),
                 
                 tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br()
        ),

#=======================================================================================================================        
tabPanel("Correlations", value="panelCorr",
         fluidRow(style="background-color:#edcdc2;padding:1%",
                  h3("Correlations"), "Display the correlation between your gene of interest and all other genes in response to exercise.
                  Type the official gene symbol and highlight your criteria of interest."),
         
         fluidRow(style="background-color:#edcdc2;padding:1% 0 0 0",
                  column(2, selectizeInput("gene1", "Official Gene Name", choices=NULL, selected=NULL, options=NULL)), #input official gene name
                  column(6,   selectInput("selectgroup", label="Highlight", 
                                          choices = list("All"=1,
                                                         "Protocol"=3, 
                                                         "Muscle"=5, 
                                                         "Sex"=6,
                                                         "Age"=7,
                                                         "Training"=8,
                                                         "Disease"=9),
                                          selected = "All"))
         ),
         
         
         fluidRow(
           column(6,  h3("Correlation Table"),
                  DT::dataTableOutput("CorrTable")),
           
           column(6,  h3("Correlation Plot"),
                  plotOutput("CorrPlot"))
         ),
         
         tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br()
),

#=======================================================================================================================        
        tabPanel("Downloads", 
                 fluidRow(style="background-color:#edcdc2;padding:1%",
                          h3("Downloads"),
                          "All the data used in MetMEx is freely available. You can download only the plots corresponding to
                          the criteria used in the app or download the individual statistics for all studies.", tags$br(),
                          "For more advanced subsetting or analysis of the data, please", a("contact us!", href="mailto:nicolas.pillon@ki.se")),
                  tags$br(),
                 fluidRow(
                column(6, h3("Data selected in the App"),
                          downloadButton("downloadReport", "Forest plots (.pdf)"),tags$br(),tags$br(),
                          downloadButton("downloadCorr", "Correlation plot (.pdf)"),tags$br(),tags$br(),
                          downloadButton("downloadData", "Statistics of selected data (.csv)"), tags$br(), tags$br(),
                          tags$img(src='Nico-Macrophage-weight-R.png', width="50%", align="center"), tags$br(),tags$br()),
                column(6, h3("Complete datasets"),
                          downloadButton("downloadAA", "Acute Aerobic (.csv, 66MB)"),tags$br(),tags$br(),
                          downloadButton("downloadAR", "Acute Resistance (.csv, 45MB)"),tags$br(),tags$br(), 
                          downloadButton("downloadTA", "Training Aerobic (.csv, 76MB)"),tags$br(),tags$br(),
                          downloadButton("downloadTR", "Training Resistance (.csv, 76MB)"),tags$br(),tags$br(),
                          downloadButton("downloadTC", "Training Combined (.csv, 17MB)"),tags$br(),tags$br(),
                          downloadButton("downloadHI", "Training HIIT (.csv, 17MB)"),tags$br(),tags$br(),
                          downloadButton("downloadIN", "Inactivity (.csv, 18MB)"), tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br())
        )),

#=======================================================================================================================        
navbarMenu("Datasets",
    tabPanel("Annotation", 
             fluidRow(style="background-color:#edcdc2;padding:1%",
                      h3("Annotation"), "Summary of the criteria used to categorize the studies.", tags$br(),
                "Your study is not included? You have information on age, sex, BMI or else for one of the studies? Please",
                a("contact us!", href="mailto:nicolas.pillon@ki.se"), tags$b("The more data we collect, the stronger MetaMex becomes!")),
             tags$br(),
             DT::dataTableOutput("Annotation"), tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br()),         
    tabPanel("Acute Studies", 
             fluidRow(style="background-color:#edcdc2;padding:1%",
                      h3("Acute Exercise Studies"), "Summarized clinical data and experimental conditions.", tags$br(),
                "Your study is not included? You have information on age, sex, BMI or else for one of the studies? Please",
                a("contact us!", href="mailto:nicolas.pillon@ki.se"), tags$b("The more data we collect, the stronger MetaMex becomes!")),
             tags$br(),
             DT::dataTableOutput("StudiesAcute"), tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br()),    
    tabPanel("Training Studies", 
             fluidRow(style="background-color:#edcdc2;padding:1%",
                      h3("Training Exercise Studies"), "Summarized clinical data and experimental conditions.", tags$br(),
                "Your study is not included? You have information on age, sex, BMI or else for one of the studies? Please",
                a("contact us!", href="mailto:nicolas.pillon@ki.se"), tags$b("The more data we collect, the stronger MetaMex becomes!")),
             tags$br(),
             DT::dataTableOutput("StudiesTraining"), tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br()),    
    tabPanel("Inactivity Studies", 
             fluidRow(style="background-color:#edcdc2;padding:1%",
                      h3("Inactivity Studies"), "Summarized clinical data and experimental conditions.", tags$br(),
                "Your study is not included? You have information on age, sex, BMI or else for one of the studies? Please",
                a("contact us!", href="mailto:nicolas.pillon@ki.se"), tags$b("The more data we collect, the stronger MetaMex becomes!")),
             tags$br(),
             DT::dataTableOutput("StudiesInactivity"), tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br())    

),

#=======================================================================================================================
        tabPanel("About", fluidRow(
                 column(8, 
                 h3("Method"),
                 "The meta-analysis was created by collecting publicly available studies on
                 mRNA expression levels in human skeletal muscle after exercise or inactivity. Statistics were first perfomed
                 individually for each array. The meta-analysis summary was then calculated using a random effects model (REML).
                 Forest plots present the log2(fold-change) and 95% confidence intervals for each study as well as the meta-analysis score and adjusted p value.",
                 h3("Citation"),
                 "Nicolas J. Pillon, Anna Krook & Juleen R. Zierath.", tags$br(),
                 a("Transcriptomic Meta-Analysis of Skeletal Muscle Responses to Physical Inactivity and Exercise",
                   href="https://www.ncbi.nlm.nih.gov/pubmed/", target="_blank"), tags$br(),
                 "Submitted and under review, July 2018",
                 h3("Copyrights"),
                 "MetaMEx was created by", a("Nicolas J. Pillon", href="https://nicopillon.com/contact",         target="_blank"),
                 ",",                      a("Anna Krook",        href="https://ki.se/en/people/annkro", target="_blank"),
                 "and",                    a("Juleen R. Zierath",  href="https://ki.se/en/people/julzie", target="_blank"),
                 "and illustrated by", a("Csil.", href="http://misshue.net", target="_blank"),
                 "All content and code are published under the Creative Commons Attribution-NonCommercial 4.0 International",
                    a("(CC BY-NC 4.0).", href="https://creativecommons.org/licenses/by-nc/4.0/", target="_blank")),
                 column(4, tags$img(src='Nico-Macrophage-bike-L.png', width = "100%", style="padding:0 5% 0 0"))
        ))

)




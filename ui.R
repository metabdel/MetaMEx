source("functions.R")

navbarPage(title="MetaMEx", id="inTabset",
                   #add css theme
                   theme="bootstrap.css",
                   #include google analytics & adjust progress bar position
                   header=tags$head(includeHTML("google-analytics.html"),
                                    tags$style(HTML(".shiny-notification { height: 50px;
                                                                           width: 800px;
                                                                           position:fixed;
                                                                           top: calc(50% - 50px);;
                                                                           left: calc(50% - 400px);;}"))),
                   #make sticky footer
                   footer=tags$footer(fluidRow(
                                   column(9, style="padding:0.4% 1% 1% 3%;", align="left",
                                             a("Transcriptomic Profiling of Skeletal Muscle Adaptations to Exercise and Inactivity",
                                               href="https://doi.org/10.1038/s41467-019-13869-w", target="_blank", style="color:#D9DADB"), tags$br(),
                                              "Pillon, Zierath et al. Nat Commun. 2020; 11: 470."),
                                   column(3, style="padding:0.4% 3% 1% 1%;", align="right",
                                             tags$b(HTML("Share:&nbsp")),
                                             actionButton("twitter_share", label="", icon=icon("twitter"),
                                                          onclick = sprintf("window.open('%s')", url_twitter)),
                                             actionButton("linkedin_share", label="", icon=icon("linkedin"),
                                                          onclick = sprintf("window.open('%s')", url_linkedin)),
                                             actionButton("facebook_share", label="", icon=icon("facebook"),
                                                          onclick = sprintf("window.open('%s')", url_facebook)))
                                   ), style="position:fixed;
                                             bottom:0;
                                             width:100%;
                                             height:60px;
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
                              h3("Meta-analysis of skeletal Muscle response to Exercise"), 
                              tags$br(),
                              actionButton('jumpToApp', 'Get started!', width="200px",
                                           style="background-color:#E95420;border-color:#C34113;box-shadow: 0 12px 16px 0 rgba(0,0,0,0.24), 0 17px 50px 0 rgba(0,0,0,0.19);"),
                              HTML('&nbsp;'), "or", HTML('&nbsp;'),
                              actionButton('jumpToHelp', 'Get help!', width="200px",
                                           style="background-color:#808080;border-color:#3c2a26;box-shadow: 0 12px 16px 0 rgba(0,0,0,0.24), 0 17px 50px 0 rgba(0,0,0,0.19);"),
                           tags$br(), tags$br(),
                           h5("The app to meta-analyse skeletal muscle transcriptomic response to inactivity and exercise.
                           Use MetaMEx to get a complete overview the behavior of a specific gene accross
                                 all published exercise and inactivity transcriptomic studies."),
                           h5("Please acknowledge MetaMEx in your publications by citing: Pillon, Zierath et al.",  
                           a("Transcriptomic Profiling of Skeletal Muscle Adaptations to Exercise and Inactivity.",
                             href="https://doi.org/10.1038/s41467-019-13869-w", target="_blank"), "Nat Commun. 2020; 11: 470."),
                           h5("Last update of the database: 03/04/2020"),
                           ),
                    column(5, tags$img(src='Nico-Macrophage-weight-L.png', width="100%", style="padding:0 5% 0 0"))
                   )
        ),

#=======================================================================================================================        
        tabPanel("Meta-analysis", value="panelApp",
                 fluidRow(style="background-color:#edcdc2;padding:1% 0 0 0",
                          column(2, selectizeInput("genename", "Official Gene Name", choices=NULL, selected=NULL, options=NULL)),
                          column(1, checkboxGroupInput("muscle", "Muscle", list_categories[['muscle_choice']]), #checkbox to select category
                                 checkboxInput('bar_muscle', 'All/None', value=T)), #checkbox to select all
                          column(1, checkboxGroupInput("sex", "Sex", list_categories[['sex_choice']]), #checkbox to select category
                                 checkboxInput('bar_sex', 'All/None', value=T)), #checkbox to select all
                          column(1, checkboxGroupInput("age", "Age", list_categories[['age_choice']]), #checkbox to select category
                                 checkboxInput('bar_age', 'All/None', value=T)), #checkbox to select all
                          column(1, checkboxGroupInput("training", "Fitness", list_categories[['training_choice']]), #checkbox to select category
                                 checkboxInput('bar_training', 'All/None', value=T)), #checkbox to select all
                          column(1, checkboxGroupInput("obesity", "Obesity", selected="LEA", list_categories[['obesity_choice']]), #checkbox to select category
                                 checkboxInput('bar_obesity', 'All/None', value=T)), #checkbox to select all
                          column(4, checkboxGroupInput("disease", "Health status", selected="HLY", list_categories[['disease_choice']]), #checkbox to select category
                                 checkboxInput('bar_disease', 'All/None', value=T)) #checkbox to select all
                          

                 ),

                 fluidRow(
                   column(2, h3("Acute Exercise"),
                          checkboxGroupInput("exercisetype", "Type", list_categories[['exercise_choice']]), #checkbox to select category
                          checkboxInput('bar_exercisetype', 'All/None', value=T), #checkbox to select all
                          checkboxGroupInput("biopsy", "Biopsy collection", list_categories[['biopsy_choice']]), #checkbox to select category
                          checkboxInput('bar_biopsy', 'All/None', value=T)
                          #tags$b(" Download"), tags$br(), tags$br(),
                          #downloadButton("AA_plot_download", "Acute Aerobic"),
                          #tags$br(), tags$br(),
                          #downloadButton("AR_plot_download", "Acute Resistance")
                          ),
                   column(8, align="top", tags$br(),
                          plotOutput("AA_plot", height="500px"),
                          plotOutput("AR_plot", height="500px")
                          ),
                   column(2, tags$br(), tags$br(),
                          checkboxGroupInput("AA_studies", "Acute Aerobic Datasets", selected=list_datasets[['AA_names']], list_datasets[['AA_names']]),
                          tags$br(), tags$br(), tags$br(), tags$br(),
                          checkboxGroupInput("AR_studies", "Acute Resistance Datasets", selected=list_datasets[['AR_names']], list_datasets[['AR_names']])
                          )
                   ),
                 
                 tags$hr(),
                 
                 fluidRow(column(2, h3("Exercise Training"),
                                 checkboxGroupInput("training_duration", "Duration", list_categories[['training_duration_choice']]), #checkbox to select category
                                 checkboxInput('bar_training_duration', 'All/None', value=T), #checkbox to select all
                                 checkboxGroupInput("training_biopsy", "Biopsy time", list_categories[['training_biopsy_choice']]), #checkbox to select category
                                 checkboxInput('bar_training_biopsy', 'All/None', value=T)),
                          column(8,  tags$br(),
                                 plotOutput("TA_plot", height="400px"),
                                 plotOutput("TR_plot", height="550px"),
                                 plotOutput("TH_plot", height="150px"),
                                 plotOutput("TC_plot", height="250px")),
                          column(2, tags$br(), tags$br(),
                                 checkboxGroupInput("TA_studies", "Aerobic Training Studies", selected=list_datasets[[3]], list_datasets[[3]]),
                                 tags$br(),
                                 checkboxGroupInput("TR_studies", "Resistance Training Studies", selected=list_datasets[[4]], list_datasets[[4]]),
                                 tags$br(),
                                 checkboxGroupInput("TH_studies", "HIIT Training studies", selected=list_datasets[[5]], list_datasets[[5]]),
                                 tags$br(),
                                 checkboxGroupInput("TC_studies", "Combined Training Studies", selected=list_datasets[[6]], list_datasets[[6]]))),
                 
                 tags$hr(),
                 
                 fluidRow(style="padding:0 0 10% 0",
                   column(2,  h3("Inactivity"),
                          checkboxGroupInput("inactivity_protocol", "Protocol", list_categories[['inactivity_protocol_choice']]), #checkbox to select category
                          checkboxInput('bar_inactivity_protocol', 'All/None', value=T),
                          checkboxGroupInput("inactivity_duration", "Duration", list_categories[['inactivity_duration_choice']]), #checkbox to select category
                          checkboxInput('bar_inactivity_duration', 'All/None', value=T)),
                   column(8, tags$br(),
                          plotOutput("IN_plot", height="300px")),
                   column(2, tags$br(), tags$br(),
                          checkboxGroupInput("IN_studies", "Physical Inactivity studies", selected=list_datasets[['IN_names']], list_datasets[['IN_names']])))
        ),

#=======================================================================================================================        
tabPanel("Timeline", value="panelCorr",
         fluidRow(style="background-color:#edcdc2;padding:1% 0 0 0",
                  column(2, selectizeInput("timeline_gene", "Official Gene Name", choices=NULL, selected=NULL, options=NULL)), #input official gene name
                  column(10, tags$br(), "Using the MetaMEx database, this tool displays the behaviour of a gene of interest
                  at different times after exercise. This dataset pools data from acute aerobic and resistance studies,
                  and only includes data in healthy individuals."),
                  uiOutput("download_timeline")
         ),
         
         fluidRow(style="padding:0 0 10% 0",
                  column(5, plotOutput("TimelinePlot")),
                  column(5,  h3("Statistics"),
                         tableOutput("TimeTable"))
         )
),


#=======================================================================================================================        
tabPanel("Correlations", value="panelCorr",
         fluidRow(style="background-color:#edcdc2;padding:1%",
                  "Display the correlation between your gene of interest with all other genes in response to exercise and inactivity.
                  First select your gene of interest using its official gene symbol (Spearman calculation takes a dozen of seconds). Then click on any gene
                  in the correlation table to display the correlation plot. You can then highlight dots with your criteria of interest."),
         
         fluidRow(style="background-color:#edcdc2;padding:1% 0 0 0",
                  column(2, selectizeInput("gene1", "Official Gene Name", choices=NULL, selected=NULL, options=NULL)), #input official gene name
                  column(4,   selectInput("selectgroup", label="Highlight", 
                                          choices = list("All"=1,
                                                         "Protocol"=5, 
                                                         "Muscle"=7, 
                                                         "Sex"=8,
                                                         "Age"=9,
                                                         "Training"=10,
                                                         "Obesity"=11,
                                                         "Disease"=12),
                                          selected = "All")),
                  uiOutput("download")
         ),
         
         tags$br(),
         
         fluidRow(style="padding:0 0 10% 0",
           column(6, DT::dataTableOutput("CorrTable")),
           
           column(6, plotOutput("CorrPlot"),
                     textOutput("Corr_description"),
                     uiOutput ("Corr_link"))
         )
),

#=======================================================================================================================        
#        tabPanel("Downloads", 
#                 fluidRow(style="background-color:#edcdc2;padding:1%",
#                          h3("Downloads"),
#                          "All the data used in MetMEx is freely available.",
#                          "For more advanced subsetting or analysis of the data, please", a("contact us!", href="mailto:nicolas.pillon@ki.se")),
#                  tags$br(),
#                 fluidRow(
#                column(4, downloadButton("downloadAA", "Acute Aerobic (.csv, 66MB)"),tags$br(),tags$br(),
#                          downloadButton("downloadAR", "Acute Resistance (.csv, 45MB)"),tags$br(),tags$br(), 
#                          downloadButton("downloadTA", "Training Aerobic (.csv, 76MB)"),tags$br(),tags$br(),
#                          downloadButton("downloadTR", "Training Resistance (.csv, 76MB)"),tags$br(),tags$br(),
#                          downloadButton("downloadTC", "Training Combined (.csv, 17MB)"),tags$br(),tags$br(),
#                          downloadButton("downloadHI", "Training HIIT (.csv, 17MB)"),tags$br(),tags$br(),
#                          downloadButton("downloadIN", "Inactivity (.csv, 18MB)"), tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br(),tags$br()),
#                column(6, tags$img(src='Nico-Macrophage-weight-R.png', width="60%", align="left"))
#                
#        )),
#
#=======================================================================================================================        
navbarMenu("Datasets",
    tabPanel("Annotation", 
             fluidRow(style="font-size: 70%;padding:0% 0% 10% 0%",
                      h3("Legend for the annotation of studies"),
             DT::dataTableOutput("Annotation"))
             ),         
    tabPanel("Acute Studies", 
             fluidRow(style="font-size: 70%;padding:0% 0% 10% 0%",
                      h3("Acute Exercise Studies"),
             DT::dataTableOutput("StudiesAcute"))
             ),    
    tabPanel("Training Studies", 
             fluidRow(style="font-size: 70%;padding:0% 0% 10% 0%",
                      h3("Exercise Training Studies"),
             DT::dataTableOutput("StudiesTraining"))
             ),    
    tabPanel("Inactivity Studies", 
             fluidRow(style="font-size: 70%;padding:0% 0% 10% 0%",
                      h3("Inactivity Studies"),
             DT::dataTableOutput("StudiesInactivity"))
             )
    ),

#=======================================================================================================================        
        tabPanel("Help", value="Tutorial",
                 fluidRow(style="padding:0% 5% 10% 5%",
                          includeMarkdown("tutorial/tutorial.md"))
                 ),
           
#=======================================================================================================================
        tabPanel("Citations", 
                 fluidRow(style="padding:0% 5% 10% 5%",
                  includeMarkdown("tutorial/citations.md"))
                 ),

#=======================================================================================================================
        tabPanel("Acknowledgments", 
                 fluidRow(style="padding:0% 5% 10% 5%",
                 column(7, 
                        includeMarkdown("tutorial/acknowledgments.md")),
                 column(4, tags$img(src='Nico-Macrophage-bike-L.png', width = "100%", style="padding:0 5% 0 0"))
                 )
                 )

)




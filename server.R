####################################################################################################################
# Define server logic ##############################################################################################
server <- function(input, output, session) {

  updateSelectizeInput(session, 'genename', choices=list_genes, server=TRUE, selected='PPARGC1A' , options=NULL)
  updateSelectizeInput(session, 'gene1', choices=list_genes2, server=TRUE, selected=NA , options=NULL)
  updateSelectizeInput(session, 'gene2', choices=list_genes2, server=TRUE, selected=NA , options=NULL)
  updateSelectizeInput(session, 'gene_timeline', choices=list_genes, server=TRUE, selected='PPARGC1A' , options=NULL)
  observeEvent(input$jumpToApp, {updateTabsetPanel(session, "inTabset", selected="panelApp") })
  observeEvent(input$jumpToContribute, {updateTabsetPanel(session, "inTabset", selected="Contribute") })
  
 #=======================================================================================
 # Make all checkboxes selected by default - necessary for the select all button to work
 #=======================================================================================  
  observe({ updateCheckboxGroupInput(session, 'muscle',      choices = list_categories[['muscle_choice']],   selected = if (input$bar_muscle) list_categories[['muscle_choice']])})
  observe({ updateCheckboxGroupInput(session, 'sex',         choices = list_categories[['sex_choice']],      selected = if (input$bar_sex) list_categories[['sex_choice']])})
  observe({ updateCheckboxGroupInput(session, 'age',         choices = list_categories[['age_choice']],      selected = if (input$bar_age) list_categories[['age_choice']])})
  observe({ updateCheckboxGroupInput(session, 'training',    choices = list_categories[['training_choice']], selected = if (input$bar_training) list_categories[['training_choice']])})
  observe({ updateCheckboxGroupInput(session, 'obesity',     choices = list_categories[['obesity_choice']],  selected = if (input$bar_obesity) list_categories[['obesity_choice']])})
  observe({ updateCheckboxGroupInput(session, 'disease',     choices = list_categories[['disease_choice']],  selected = if (input$bar_disease) list_categories[['disease_choice']])})
  observe({ updateCheckboxGroupInput(session, 'biopsy',      choices = list_categories[['biopsy_choice']],   selected = if (input$bar_biopsy) list_categories[['biopsy_choice']])})
  observe({ updateCheckboxGroupInput(session, 'exercisetype',choices = list_categories[['exercise_choice']], selected = if (input$bar_exercisetype) list_categories[['exercise_choice']])})
  observe({ updateCheckboxGroupInput(session, 'AA_studies',  choices = list_datasets[['AA_names']],        selected = if (input$AA_all) list_datasets[['AA_names']], inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'AR_studies',  choices = list_datasets[['AR_names']],        selected = if (input$AR_all) list_datasets[['AR_names']], inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'TA_studies',  choices = list_datasets[['TA_names']],        selected = if (input$TA_all) list_datasets[['TA_names']], inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'TR_studies',  choices = list_datasets[['TR_names']],        selected = if (input$TR_all) list_datasets[['TR_names']], inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'TC_studies',  choices = list_datasets[['TC_names']],        selected = if (input$TC_all) list_datasets[['TC_names']], inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'HI_studies',  choices = list_datasets[['HI_names']],        selected = if (input$HI_all) list_datasets[['HI_names']], inline=TRUE)})
  observe({ updateCheckboxGroupInput(session, 'IN_studies',  choices = list_datasets[['IN_names']],        selected = if (input$IN_all) list_datasets[['IN_names']], inline=TRUE)})

  
#=======================================================================================
# Make annotation table for legend 
#=======================================================================================
output$Annotation <- DT::renderDataTable(escape = FALSE, rownames = FALSE, options=list(paging = FALSE), { annotation })

output$StudiesAcute <- DT::renderDataTable(escape = FALSE, rownames = FALSE, { StudiesAcute },
                                           options=list(autoWidth = TRUE,
                                                        columnDefs = list(list(targets=c(11), visible=TRUE, width='3000'),
                                                                     list(targets=c(7,15), visible=TRUE, width='2000'))))

output$StudiesTraining <- DT::renderDataTable(escape = FALSE, rownames = FALSE, { StudiesTraining },
                                              options=list(autoWidth = TRUE,
                                                           columnDefs = list(list(targets=c(13), visible=TRUE, width='4000'),
                                                                        list(targets=c(17), visible=TRUE, width='2000'))))

output$StudiesInactivity <- DT::renderDataTable(escape = FALSE, rownames = FALSE, { StudiesInactivity })

output$MissingData <- DT::renderDataTable(escape = FALSE, rownames = FALSE, { MissingData },
                                          options = list(paging = FALSE, searching = FALSE))

#=======================================================================================  
# Make reactive functions to select data
#=======================================================================================
  AA_data <- reactive({
    tryCatch({
      #data for selected gene name
      selectedata <- Stats_AA[toupper(input$genename),]
      #custom function to make a useable data frame
      selectedata <- DataForGeneName(selectedata) 
      #filter according to selected categories
      selectedata <- dplyr::filter(selectedata,
                            GEO %in% input$AA_studies, 
                            Muscle %in% input$muscle, 
                            Sex %in% input$sex, 
                            Age %in% input$age, 
                            Training %in% input$training,
                            Obesity %in% input$obesity,
                            Disease %in% input$disease,
                            Biopsy %in% input$biopsy)
      #custom function for meta-analysis
      selectedata <- suppressWarnings(MetaAnalysis(selectedata))
      #add a column with the names of the studies included
      selectedata$Studies <- c(gsub("logFC_", "", selectedata$Studies[1:(nrow(selectedata)-1)]),
                               "Acute Aerobic score")
      return(selectedata)
    }, error=function(e) NULL)
  })

  AR_data <- reactive({
    tryCatch({
      selectedata<- Stats_AR[toupper(input$genename),]
      selectedata<- DataForGeneName(selectedata) #call the custom function to make data table
      selectedata <- dplyr::filter(selectedata,
                     GEO %in% input$AR_studies, 
                     Muscle %in% input$muscle, 
                     Sex %in% input$sex, 
                     Age %in% input$age, 
                     Training %in% input$training,
                     Obesity %in% input$obesity,
                     Disease %in% input$disease,
                     Biopsy %in% input$biopsy,
                     Exercisetype %in% input$exercisetype)
      selectedata <- suppressWarnings(MetaAnalysis(selectedata))
      selectedata$Studies <- c(gsub("logFC_", "", selectedata$Studies[1:(nrow(selectedata)-1)]),
                               "Acute Resistance score")
      return(selectedata)
    }, error=function(e) NULL)
  })
  
  TA_data <- reactive({
    tryCatch({
      selectedata <- Stats_TA[toupper(input$genename),]
      selectedata <- DataForGeneName(selectedata) #call the custom function to make data table
      selectedata <- dplyr::filter(selectedata,
                     GEO %in% input$TA_studies, 
                     Muscle %in% input$muscle, 
                     Sex %in% input$sex, 
                     Age %in% input$age, 
                     Training %in% input$training,
                     Obesity %in% input$obesity,
                     Disease %in% input$disease)
      selectedata <- suppressWarnings(MetaAnalysis(selectedata))
      selectedata$Studies <- c(gsub("logFC_", "", selectedata$Studies[1:(nrow(selectedata)-1)]),
                               "Training Aerobic score")
      return(selectedata)
    }, error=function(e) NULL)
  }) 
  
  TR_data <- reactive({
    tryCatch({
      selectedata <- Stats_TR[toupper(input$genename),]
      selectedata <- DataForGeneName(selectedata) #call the custom function to make data table
      selectedata <- dplyr::filter(selectedata,
                            GEO %in% input$TR_studies, 
                            Muscle %in% input$muscle, 
                            Sex %in% input$sex, 
                            Age %in% input$age, 
                            Training %in% input$training,
                            Obesity %in% input$obesity,
                            Disease %in% input$disease)
      selectedata <- suppressWarnings(MetaAnalysis(selectedata))
      selectedata$Studies <- c(gsub("logFC_", "", selectedata$Studies[1:(nrow(selectedata)-1)]),
                               "Training Resistance score")
      return(selectedata)
    }, error=function(e) NULL)
  })
  
  TC_data <- reactive({
    tryCatch({
      selectedata <- Stats_TC[toupper(input$genename),]
      selectedata <- DataForGeneName(selectedata) #call the custom function to make data table
      selectedata <- dplyr::filter(selectedata,
                            GEO %in% input$TC_studies, 
                            Muscle %in% input$muscle, 
                            Sex %in% input$sex, 
                            Age %in% input$age, 
                            Training %in% input$training,
                            Obesity %in% input$obesity,
                            Disease %in% input$disease)
      selectedata <- suppressWarnings(MetaAnalysis(selectedata))
      selectedata$Studies <- c(gsub("logFC_", "", selectedata$Studies[1:(nrow(selectedata)-1)]),
                               "Training Combined score")
      return(selectedata)
    }, error=function(e) NULL)
  })
  
  HI_data <- reactive({
    tryCatch({
      selectedata <- Stats_HI[toupper(input$genename),]
      selectedata <- DataForGeneName(selectedata) #call the custom function to make data table
      selectedata <- dplyr::filter(selectedata,
                                   GEO %in% input$HI_studies, 
                                   Muscle %in% input$muscle, 
                                   Sex %in% input$sex, 
                                   Age %in% input$age, 
                                   Training %in% input$training,
                                   Obesity %in% input$obesity,
                                   Disease %in% input$disease)
      selectedata <- suppressWarnings(MetaAnalysis(selectedata))
      selectedata$Studies <- c(gsub("logFC_", "", selectedata$Studies[1:(nrow(selectedata)-1)]),
                               "Training HIIT score")
      return(selectedata)
    }, error=function(e) NULL)
  })
  
  IN_data <- reactive({
    tryCatch({
      selectedata <- Stats_IN[toupper(input$genename),]
      selectedata <- DataForGeneName(selectedata) #call the custom function to make data table
      selectedata <- dplyr::filter(selectedata,
                            GEO %in% input$IN_studies, 
                            Muscle %in% input$muscle, 
                            Sex %in% input$sex, 
                            Age %in% input$age, 
                            Training %in% input$training,
                            Obesity %in% input$obesity,
                            Disease %in% input$disease)
      selectedata <- suppressWarnings(MetaAnalysis(selectedata))
      selectedata$Studies <- c(gsub("logFC_", "", selectedata$Studies[1:(nrow(selectedata)-1)]),
                               "Physical Inactivity score")
      return(selectedata)
    }, error=function(e) NULL)
  })


#=======================================================================================
# Make reactive forest plot from selected data
#=======================================================================================

  plotInputAA <- function() ({
    #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$AA_studies!="",  "Please select at least one group in the study list")) 
    validate(need(!is.null(AA_data()),   "Dataset not available for the selected criteria"))
    #Plot the forest plot:
    selectedata <- AA_data()
    # make forest plot
    tabledata <- cbind(mean = c(NA , selectedata[,1]), 
                       lower= c(NA , selectedata[,3]),
                       upper= c(NA , selectedata[,4]))
    tabletext <- cbind(c('Acute Aerobic Studies' , selectedata[,10]),
                       c("logFC" , format(round(selectedata[,1], digits=2))),
                       c("FDR"   , format(selectedata[,2],   scientific=T, digits=2)))
    finalplot <- forestplot(tabletext, 
                            tabledata, new_page=TRUE,
                            is.summary=c(TRUE,rep(FALSE,(nrow(selectedata)-1)),TRUE),
                            xlog=F,  txt_gp = own, xlab="logFC",
                            col=fpColors(box="orange2",line="orange3", summary="orange3"))
    return(finalplot)
  })
  
  plotInputAR <- function() ({
  #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$AR_studies!="",  "Please select at least one group in the study list"))
    validate(need(!is.null(AR_data()),   "Dataset not available for the selected criteria"))
  #Plot the forest plot:
    selectedata <- data.frame(AR_data())
    # make forest plot
    tabledata <- cbind(mean = c(NA , selectedata[,1]), 
                       lower= c(NA , selectedata[,3]),
                       upper= c(NA , selectedata[,4]))
    tabletext <- cbind(c('Acute Resistance Studies' , selectedata[,10]),
                       c("logFC" , format(round(selectedata[,1], digits=2))),
                       c("FDR"   , format(selectedata[,2],   scientific=T, digits=2)))
    finalplot <- forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(nrow(selectedata)-1)),TRUE),
               xlog=F,  txt_gp = own, xlab="logFC",
               col=fpColors(box="orangered2",line="orangered3", summary="orangered3"))
    return(finalplot)
  })
  
  plotInputTA <- function() ({
    #Validate selection criteria:
      validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
      validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
      validate(need(input$age!="",         "Please select at least one group in the age category")) 
      validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
      validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
      validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
      validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
      validate(need(input$TA_studies!="",  "Please select at least one group in the study list")) 
      validate(need(!is.null(TA_data()),   "Dataset not available for the selected criteria"))
   #Plot the forest plot:
      selectedata <- data.frame(TA_data())
      # make forest plot
      tabledata <- data.frame(mean = c(NA , selectedata[,1]), 
                              lower= c(NA , selectedata[,3]),
                              upper= c(NA , selectedata[,4]))
      tabletext<-cbind(c('Training Aerobic Studies' , selectedata[,10]),
                       c("logFC" , format(round(selectedata[,1], digits=2))),
                       c("FDR"   , format(selectedata[,2],   scientific=T, digits=2)))
      finalplot <- forestplot(tabletext, 
                 tabledata, new_page=TRUE,
                 is.summary=c(TRUE,rep(FALSE,(length(selectedata[,10]))-1),TRUE),
                 xlog=F,  txt_gp = own, xlab="logFC",
                 col=fpColors(box="dodgerblue3",line="dodgerblue4", summary="dodgerblue4"))
      return(finalplot)
  })
  
  plotInputTR <- function() ({
  #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$TR_studies!="",  "Please select at least one group in the study list")) 
    validate(need(!is.null(TR_data()),   "Dataset not available for the selected criteria"))
  #Plot the forest plot:
    selectedata <- data.frame(TR_data())
    # make forest plot
    tabledata <- data.frame(mean = c(NA , selectedata[,1]), 
                            lower= c(NA , selectedata[,3]),
                            upper= c(NA , selectedata[,4]))
    tabletext<-cbind(c('Training Resistance Studies' , selectedata[,10]),
                     c("logFC" , format(round(selectedata[,1], digits=2))),
                     c("FDR"   , format(selectedata[,2],   scientific=T, digits=2)))
    finalplot <- forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(selectedata[,10]))-1),TRUE),
               xlog=F,  txt_gp = own, xlab="logFC",
               col=fpColors(box="green3",line="green4", summary="green4"))
    return(finalplot)
})

  plotInputTC <- function() ({
  #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$TC_studies!="",  "Please select at least one group in the study list"))
    validate(need(!is.null(TC_data()),   "Dataset not available for the selected criteria"))
  #Plot the forest plot:
    selectedata <- data.frame(TC_data())
    # make forest plot
    tabledata <- data.frame(mean = c(NA , selectedata[,1]), 
                            lower= c(NA , selectedata[,3]),
                            upper= c(NA , selectedata[,4]))
    tabletext<-cbind(c('Training Combined Studies' , selectedata[,10]),
                     c("logFC" , format(round(selectedata[,1], digits=2))),
                     c("FDR"   , format(selectedata[,2],   scientific=T, digits=2)))
    finalplot <- forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(selectedata[,10]))-1),TRUE),
               xlog=F,  txt_gp = own, xlab="logFC",
               col=fpColors(box="aquamarine3",line="aquamarine4", summary="aquamarine4"))
    return(finalplot)
  })

  plotInputHI <- function() ({
    #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$HI_studies!="",  "Please select at least one group in the study list"))
    validate(need(!is.null(HI_data()),   "Dataset not available for the selected criteria"))
    #Plot the forest plot:
    selectedata <- data.frame(HI_data())
    # make forest plot
    tabledata <- data.frame(mean = c(NA , selectedata[,1]), 
                            lower= c(NA , selectedata[,3]),
                            upper= c(NA , selectedata[,4]))
    tabletext<-cbind(c('Training HIIT Studies' , selectedata[,10]),
                     c("logFC" , format(round(selectedata[,1], digits=2))),
                     c("FDR"   , format(selectedata[,2],   scientific=T, digits=2)))
    finalplot <- forestplot(tabletext, 
                            tabledata, new_page=TRUE,
                            is.summary=c(TRUE,rep(FALSE,(length(selectedata[,10]))-1),TRUE),
                            xlog=F,  txt_gp = own, xlab="logFC",
                            col=fpColors(box="skyblue3",line="skyblue4", summary="skyblue3"))
    return(finalplot)
  })
  
  plotInputIN <- function() ({
  #Validate selection criteria:
    validate(need(input$muscle!="",      "Please select at least one group in the muscle category")) 
    validate(need(input$sex!="",         "Please select at least one group in the sex category")) 
    validate(need(input$age!="",         "Please select at least one group in the age category")) 
    validate(need(input$training!="",    "Please select at least one group in the fitness category")) 
    validate(need(input$disease!="",     "Please select at least one group in the health status category")) 
    validate(need(input$biopsy!="",      "Please select at least one group in the biopsy collection category"))   
    validate(need(input$exercisetype!="","Please select at least one group in the exercise type category"))     
    validate(need(input$IN_studies!="",  "Please select at least one group in the study list")) 
    validate(need(!is.null(IN_data()),   "Dataset not available for the selected criteria"))
  #Plot the forest plot:
    selectedata <- data.frame(IN_data())
    # make forest plot
    tabledata <- data.frame(mean = c(NA , selectedata[,1]), 
                            lower= c(NA , selectedata[,3]),
                            upper= c(NA , selectedata[,4]))
    tabletext <- cbind(c('Physical Inactivity Studies' , selectedata[,10]),
                       c("logFC" , format(round(selectedata[,1], digits=2))),
                       c("FDR"   , format(selectedata[,2],   scientific=T, digits=2)))
    finalplot <- forestplot(tabletext,
                 tabledata, new_page=TRUE,
                 is.summary=c(TRUE,rep(FALSE,(length(selectedata[,10]))-1),TRUE),
                 xlog=F,  txt_gp = own, xlab="logFC",
                 col=fpColors(box="maroon3",line="maroon4", summary="maroon4"))
    return(finalplot)
  })


#=======================================================================================
# print plots
#=======================================================================================
  output$Acute_A    <- renderPlot({ plotInputAA() })
  output$Acute_R    <- renderPlot({ plotInputAR() })
  output$Training_A <- renderPlot({ plotInputTA() })
  output$Training_R <- renderPlot({ plotInputTR() })
  output$Training_C <- renderPlot({ plotInputTC() })
  output$Training_H <- renderPlot({ plotInputHI() })
  output$Inact      <- renderPlot({ plotInputIN() })

  
#=======================================================================================
# Make correlation table
#=======================================================================================

  Corr_stats <- reactive({
    tryCatch({
    withProgress(message = 'Calculating', value = 0, max=10, {
        selectedata <- Individual_FC
        geneofinterest <- as.numeric(selectedata[toupper(input$gene1),])
        estimate <- function(x) cor.test(x, geneofinterest, method="spearman", exact=F)$estimate
        p.value  <- function(x) cor.test(x, geneofinterest, method="spearman", exact=F)$p.value
      
      incProgress(1, detail="Spearman coefficients")
        Spearman.r1 <- apply(selectedata[1:3000,], 1, estimate)
      incProgress(1, detail="Spearman coefficients")
        Spearman.r2 <- apply(selectedata[3001:6000,], 1, estimate)
      incProgress(1, detail="Spearman coefficients")
        Spearman.r3 <- apply(selectedata[6001:9000,], 1, estimate)
      incProgress(1, detail="Spearman coefficients")
        Spearman.r4 <- apply(selectedata[9001:nrow(selectedata),], 1, estimate)
        Spearman.r <- c(Spearman.r1, Spearman.r2, Spearman.r3, Spearman.r4)
        
      incProgress(1, detail="Spearman statistics")
        Spearman.p1 <- apply(selectedata[1:3000,], 1, p.value)
      incProgress(1, detail="Spearman statistics")
        Spearman.p2 <- apply(selectedata[3001:6000,], 1, p.value)
      incProgress(1, detail="Spearman statistics")
        Spearman.p3 <- apply(selectedata[6001:9000,], 1, p.value)
      incProgress(1, detail="Spearman statistics")
        Spearman.p4 <- apply(selectedata[9001:nrow(selectedata),], 1, p.value)
        Spearman.p <- c(Spearman.p1, Spearman.p2, Spearman.p3, Spearman.p4)
        
      incProgress(2, detail="Making table")
        Spearman.FDR <- p.adjust(Spearman.p, method="bonferroni")
        Spearman.r <- round(Spearman.r, digits=3)
        Spearman.p <- signif(Spearman.p, digits=2)
        Spearman.FDR <- signif(Spearman.FDR, digits=2)
        coeff <- data.frame(Spearman.r, Spearman.p, Spearman.FDR)
        colnames(coeff) <- c("Spearman.r", "P.value", "FDR")
        coeff <- coeff[order(coeff$FDR),]
        
  #make hyperlinks on gene names
        #coeff$GeneCards <- paste("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", rownames(coeff), sep="")
        #coeff$GeneCards  <- sapply(coeff$GeneCards, createLink)
        #coeff <- coeff[,c(4,1,2,3)]
        
      return(coeff)
    })
    }, error=function(e) NULL)
  })
  
    output$CorrTable <- DT::renderDataTable(escape = FALSE, rownames = T, selection = "single", {
      validate(need(!is.null(Corr_stats()),   "Start by selecting a gene in the list of official gene names"))
      
      Corr_stats()

  })

  
#=======================================================================================
# Make correlation plot
#=======================================================================================
  
    plotInputCOR <- function() ({
    validate(need(!is.null(Corr_stats()),     " "))
    validate(need(input$CorrTable_rows_selected!="",  "Select a second gene in the table to display the correlation")) 
    
    Gene1 <- Individual_FC[toupper(input$gene1),]
    Gene2 <- Corr_stats()
    Gene2 <- rownames(Gene2[input$CorrTable_rows_selected,])
    Gene2 <- Individual_FC[Gene2,]
    data  <- data.frame(t(Gene1), t(Gene2))
    data <- cbind(data, str_split_fixed(rownames(data), "_", 10))
    colnames(data) <- c("Gene1", "Gene2", "Protocol", "Study", "Muscle", "Sex", "Age", "Training", "Obesity", "Disease")
    active <- ggplot(data, aes(x=Gene2, y=Gene1, color=data[,as.numeric(input$selectgroup)])) +
      geom_smooth(method=lm, se=F, fullrange=TRUE) +
      geom_point(shape=19) +
      labs(x=paste(rownames(Gene2), ", log2(fold-change)", sep=""),
           y=paste(input$gene1, ", log2(fold-change)", sep=""),
           title="") +
      theme(axis.text.x = element_text(color="black", size=10, angle=0, hjust = 1),
            axis.text.y = element_text(color="black", size=10, angle=0, hjust = 1),
            axis.title  = element_text(face="bold", color="black", size=12, angle=0),
            legend.text   = element_text(face="bold", color="black", size=10, angle=0),
            legend.position="right", legend.title = element_blank())
    return(active)
    })
  
  output$CorrPlot      <- renderPlot({ plotInputCOR() })

#=======================================================================================
# Make correlation description
#=======================================================================================
  output$Corr_description <- renderText({
    validate(need(!is.null(Corr_stats()),     " "))
    validate(need(input$CorrTable_rows_selected!="",  "Select a second gene in the table to display the correlation")) 
    
    #find gene selected in the table
    GENENAME <- Corr_stats()
    GENENAME <- rownames(GENENAME[input$CorrTable_rows_selected,])
    
    #annotate with ENTREZID
    Annotation <- descriptions
    ENTREZID <- Annotation[Annotation$GENENAME %in% GENENAME,2]
    
    #Find information on NCBI webpage
    webpage <- read_html(paste("https://www.ncbi.nlm.nih.gov/gene/", ENTREZID, sep=''))
    data_html <- html_nodes(webpage,'#summaryDl')
    data_html <- html_text(data_html)
    data_html <- str_replace_all(data_html, "[\r\n]" , "")
    data_html <- str_replace_all(data_html, "provided by HGNC" , "")
    data_html <- gsub('.*Summary', '', data_html)
    data_html <- gsub('\\].*', ']', data_html)
    data_html <- gsub('.*all', '', data_html)
    data_html <- trimws(data_html)
    return(data_html)
  })

  output$Corr_link <- renderUI({
    validate(need(!is.null(Corr_stats()),     " "))
    validate(need(input$CorrTable_rows_selected!="",  " ")) 
    
    #find gene selected in the table
    GENENAME <- Corr_stats()
    GENENAME <- rownames(GENENAME[input$CorrTable_rows_selected,])

    #Make link to genecard
    GeneCards <- paste("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", GENENAME, sep="")
    GeneCards <- sprintf(paste0('<a href="', paste("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", GENENAME, sep=""),'" target="_blank">',
                   'Learn more about ', GENENAME, ' on GeneCards' ,'</a>'))
    GeneCards <- HTML(GeneCards)
    return(GeneCards)
  })
  
#=======================================================================================
# Make timeline plot
#=======================================================================================
  plotInputTimeline <- function() ({
    validate(need(input$gene_timeline!="",  "Start by selecting a gene in the list of official gene names")) 
    genename <- toupper(input$gene_timeline)
    res  <- timeline_acute
    mydata <- data.frame(cessation=factor(colnames(res), levels=c('pre', '0-1', '2-3', '4-6', '24', '48', '72')),
                         logFC=as.numeric(res[genename,])) 
    active <- ggplot(mydata, aes(x=cessation, y=logFC, fill=cessation)) + theme_bw() +
      geom_hline(yintercept=0, color="gray50", linetype="dashed") +
      geom_boxplot() +
      labs(x="Time after exercise (h)",
           y=paste(genename, ", log2(FC)", sep=""),
           title="") + 
      theme(axis.text.x = element_text(color="black", size=10, angle=0, hjust = 1),
            axis.text.y = element_text(color="black", size=10, angle=0, hjust = 1),
            axis.title  = element_text(face="bold", color="black", size=12, angle=0),
            legend.text   = element_text(face="bold", color="black", size=10, angle=0),
            legend.position="none") +
      scale_fill_brewer(palette="Reds", direction=-1)
    return(active)
  })
  
  output$TimelinePlot      <- renderPlot({ plotInputTimeline() })

#=======================================================================================
# Make timeline table
#=======================================================================================
  output$TimeTable <- renderTable(rownames = TRUE, align='c', { 
    validate(need(input$gene_timeline!="",  "Start by selecting a gene in the list of official gene names")) 
    genename <- toupper(input$gene_timeline)
    res  <- timeline_stats[genename,]
    stats <- data.frame(p.value=t(res[,14:19]),
                        FDR=t(res[,20:25]))
    colnames(stats) <- c('p.value', 'FDR')
    KWANOVA <- res[,12:13]
    names(KWANOVA) <- c('p.value', 'FDR')
    stats <- rbind(KWANOVA, stats)
    stats$significance <- ''
    stats$significance[stats$FDR < 0.05] <- "*"
    stats$significance[stats$FDR < 0.01] <- "**"
    stats$significance[stats$FDR < 0.001] <- "***"
    stats$p.value <- scientific(stats$p.value, 1)
    stats$FDR <- scientific(stats$FDR, 1)
    rownames(stats) <- c('Kruskal-Wallis ANOVA',
                         'T-test 0-1h vs Pre', 'T-test 2-3h vs Pre',
                         'T-test 4-6h vs Pre', 'T-test 24h vs Pre',
                         'T-test 48h vs Pre', 'T-test 72h vs Pre')
    return(stats)
    })
  

#=======================================================================================
# Make buttons to download data
#=======================================================================================
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$genename, "_MetaMEx.csv", sep="") },
    content = function(file) {
      dataset <- rbind(AA_data(), AR_data(), TA_data(), TR_data(), HI_data(), TC_data(), IN_data())[,c(10,1:4,9)]
      write.csv(dataset, file, row.names = F)
    })

  output$downloadAA <- downloadHandler(
    filename = function() { "MetaMEx_Acute_Aerobic.csv" },
    content = function(file) { write.csv(Stats_AA, file) })
  
  output$downloadAR <- downloadHandler(
    filename = function() { "MetaMEx_Acute_Resistance.csv" },
    content = function(file) { write.csv(Stats_AR, file) })
  
  output$downloadTA <- downloadHandler(
    filename = function() { "MetaMEx_Training_Aerobic.csv" },
    content = function(file) { write.csv(Stats_TA, file) })
  
  output$downloadTR <- downloadHandler(
    filename = function() { "MetaMEx_Training_Resistance.csv" },
    content = function(file) { write.csv(Stats_TR, file) })
  
  output$downloadTC <- downloadHandler(
    filename = function() { "MetaMEx_Training_Combined.csv" },
    content = function(file) { write.csv(Stats_TC, file) })
  
  output$downloadHI <- downloadHandler(
    filename = function() { "MetaMEx_Training_HIIT.csv" },
    content = function(file) { write.csv(Stats_HI, file) })
  
  output$downloadIN <- downloadHandler(
    filename = function() { "MetaMEx_Inactivity.csv" },
    content = function(file) { write.csv(Stats_IN, file) })

#=======================================================================================
# Make button to save forrest plots
#=======================================================================================
  output$downloadReport = downloadHandler(
    filename = function() { paste(input$genename, "_MetaMEx.jpeg", sep="") },
    content = function(file) {
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
      if(!is.null(HI_data())) { HI <- grid.grabExpr(print(plotInputHI()))}
        else {HI <- textGrob("Training HIIT:\nNo data available for the selected criteria")}
      if(!is.null(IN_data())) { IN <- grid.grabExpr(print(plotInputIN()))}
        else {IN <- textGrob("Physical Inactivity:\nNo data available for the selected criteria")}
      if(!is.null(Corr_stats())) { COR <- plotInputCOR()}
        else {COR <- textGrob("Correlation:\nNo data available for the selected criteria")}
      
      library(rmarkdown)
      library(ggpubr)
      library(readr)
      library(grid)
      library(gridExtra)
      library(grDevices)
      matrix <- rbind(c(1,2),
                      c(1,2),
                      c(1,2),
                      c(1,2),
                      c(1,2),
                      c(1,2),
                      c(1,2),
                      c(3,2),
                      c(3,4),
                      c(3,4),
                      c(3,4),
                      c(3,4),
                      c(3,4),
                      c(5,4),
                      c(5,4),
                      c(6,4),
                      c(6,7),
                      c(6,7),
                      c(6,7),
                      c(6,7))
      
      #Code for PDF 
      #blank <- grid.rect(gp=gpar(col="white"))
      #margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
      #pdf(file, onefile = TRUE, width=16, height=22.6)
      #grid.arrange(AA, TA, AR, TR, HI, IN, TC, 
      #             top = textGrob(paste(input$genename, "transcriptional response to physical activity"), gp=gpar(fontsize=30, font=7)),
      #             bottom = textGrob("MetaMEx Copyright 2019 Nicolas J. Pillon. For more information, visit www.metamex.eu",
      #                               gp=gpar(fontsize=16)),
      #             layout_matrix=matrix, vp=viewport(width=0.95, height=0.9))
      #dev.off()
    
      #Code for JPEG
      jpeg(file, unit="cm", width=40, height=46, res=300)
      grid.arrange(AA, TA, AR, TR, HI, IN, TC, 
                   top = textGrob(paste(input$genename, "transcriptional response to physical activity"), gp=gpar(fontsize=30, font=7)),
                   bottom = textGrob("MetaMEx Copyright 2019 Nicolas J. Pillon. For more information, visit www.metamex.eu",
                                     gp=gpar(fontsize=16)),
                   layout_matrix=matrix)
      dev.off()
  })

#=======================================================================================
# Make button to save correlation plot
#=======================================================================================

  output$download <- renderUI({
    if(!is.null(input$gene1) & !is.null(input$CorrTable_rows_selected)) {
      column(4,   tags$b("Download"),
      tags$br(),
      downloadButton("downloadCorrPlot", "Correlation plot (.jpeg)"),
      downloadButton("downloadCorrData", "Correlation table (.csv)"))
    }
  })
  
  
  output$downloadCorrPlot = downloadHandler(
    filename = function() { paste(input$gene1, "_vs_", rownames(Corr_stats()[input$CorrTable_rows_selected,]), "_MetaMEx.jpeg", sep="") },
    content = function(file) {
      library(rmarkdown)
      library(ggpubr)
      library(readr)
      library(grid)
      library(gridExtra)
      library(grDevices)
      COR <- plotInputCOR()
      
      #Code for PDF 
      #matrix <- rbind(c(1,1), c(2,2))
      #blank <- grid.rect(gp=gpar(col="white"))
      #margin = theme(plot.margin = unit(c(4,4,4,4), "cm"))
      #grDevices::pdf(file, onefile = TRUE, width=16, height=22.6)
      #grid.arrange(COR, t,
      #             top = textGrob(paste(input$gene1, "correlation with", rownames(Corr_stats()[input$CorrTable_rows_selected,]), "during exercise and inactivity"), gp=gpar(fontsize=30, font=7)),
      #             bottom = textGrob("MetaMEx Copyright 2019 Nicolas J. Pillon. For more information, visit www.metamex.eu",
      #                               gp=gpar(fontsize=16)),
      #             layout_matrix=matrix, vp=viewport(width=0.95, height=0.9))
      #dev.off()
      
      #Code for JPEG
      jpeg(file, unit="cm", width=20, height=18, res=300)
      grid.arrange(COR,
                   top = textGrob(paste(input$gene1, "correlation with", rownames(Corr_stats()[input$CorrTable_rows_selected,]), "during exercise and inactivity"), gp=gpar(fontsize=18, font=7)),
                   bottom = textGrob("MetaMEx Copyright 2019 Nicolas J. Pillon. For more information, visit www.metamex.eu",
                                     gp=gpar(fontsize=9)))
      dev.off()
    })
  
  
  output$downloadCorrData <- downloadHandler(
    filename = function() { paste(input$gene1, "_MetaMEx.csv", sep="") },
    content = function(file) {
      dataset <- Corr_stats()
      dataset$SYMBOL <- rownames(dataset)
      dataset <- dataset[,c(5,2,3,4)]
      write.csv(dataset, file, row.names = F)
    })
  
}

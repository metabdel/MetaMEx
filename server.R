
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
                       t(data[grepl('mean.pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean.post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd.pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd.post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    data <- cbind(data, str_split_fixed(rownames(data), "_", 9))
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size',
                        'Studies', 'GEO', 'Muscle', 'Sex', 'Age', 'Training', 'Disease', 'Biopsy', 'Exercisetype')
    data$Studies <- gsub("logFC_","", rownames(data))
    
    tryCatch({
      data <- filter(data,
                     GEO %in% input$AA_studies, 
                     Muscle %in% input$muscle, 
                     Sex %in% input$sex, 
                     Age %in% input$age, 
                     Training %in% input$training,
                     Disease %in% input$disease,
                     Biopsy %in% input$biopsy,
                     Exercisetype %in% input$exercisetype)
    #order datasets by alphabetical order
      data <- data[order(rownames(data)),]
    # meta-analysis stats
      meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                  method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
      fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
      data <- (rbind(data[,1:10],
                     MetaAnalysis=c(meta$beta, fdr, meta$ci.lb, meta$ci.ub, rep(NA, 4), sum(data$size, na.rm=T))))
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
                       t(data[grepl('mean.pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean.post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd.pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd.post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    data <- cbind(data, str_split_fixed(rownames(data), "_", 9))
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size',
                        'Studies', 'GEO', 'Muscle', 'Sex', 'Age', 'Training', 'Disease', 'Biopsy', 'Exercisetype')
    data$Studies <- gsub("logFC_","", rownames(data))
    
    tryCatch({
    data <- filter(data,
                   GEO %in% input$AR_studies, 
                   Muscle %in% input$muscle, 
                   Sex %in% input$sex, 
                   Age %in% input$age, 
                   Training %in% input$training,
                   Disease %in% input$disease,
                   Biopsy %in% input$biopsy,
                   Exercisetype %in% input$exercisetype)
    #order datasets by alphabetical order
    data <- data[order(rownames(data)),]
    #meta-analysis stats
    meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
    fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
    data <- (rbind(data[,1:10],
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
                       t(data[grepl('mean.pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean.post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd.pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd.post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    data <- cbind(data, str_split_fixed(rownames(data), "_", 9))
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size',
                        'Studies', 'GEO', 'Muscle', 'Sex', 'Age', 'Training', 'Disease', 'Biopsy', 'Exercisetype')
    data$Studies <- gsub("logFC_","", rownames(data))
    
    tryCatch({
    data <- filter(data,
                   GEO %in% input$TA_studies, 
                   Muscle %in% input$muscle, 
                   Sex %in% input$sex, 
                   Age %in% input$age, 
                   Training %in% input$training,
                   Disease %in% input$disease)
    #order datasets by alphabetical order
    data <- data[order(rownames(data)),]
    #meta-analysis stats
    meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
    fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
    data <- (rbind(data[,1:10],
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
                       t(data[grepl('mean.pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean.post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd.pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd.post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    data <- cbind(data, str_split_fixed(rownames(data), "_", 9))
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size',
                        'Studies', 'GEO', 'Muscle', 'Sex', 'Age', 'Training', 'Disease', 'Biopsy', 'Exercisetype')
    data$Studies <- gsub("logFC_","", rownames(data))
    
    tryCatch({
      data <- filter(data,
                     GEO %in% input$TR_studies, 
                     Muscle %in% input$muscle, 
                     Sex %in% input$sex, 
                     Age %in% input$age, 
                     Training %in% input$training,
                     Disease %in% input$disease)
      #order datasets by alphabetical order
      data <- data[order(rownames(data)),]
    #meta-analysis stats
    meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
    fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
    data <- (rbind(data[,1:10],
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
                       t(data[grepl('mean.pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean.post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd.pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd.post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    data <- cbind(data, str_split_fixed(rownames(data), "_", 9))
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size',
                        'Studies', 'GEO', 'Muscle', 'Sex', 'Age', 'Training', 'Disease', 'Biopsy', 'Exercisetype')
    data$Studies <- gsub("logFC_","", rownames(data))
    
    tryCatch({
      data <- filter(data,
                     GEO %in% input$TC_studies, 
                     Muscle %in% input$muscle, 
                     Sex %in% input$sex, 
                     Age %in% input$age, 
                     Training %in% input$training,
                     Disease %in% input$disease)
      #order datasets by alphabetical order
      data <- data[order(rownames(data)),]
    #meta-analysis stats
    meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
    fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
    data <- (rbind(data[,1:10],
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
                       t(data[grepl('mean.pre', colnames(data))]), # mean of control condition
                       t(data[grepl('mean.post', colnames(data))]), # mean of exercise condition
                       t(data[grepl('Sd.pre',   colnames(data))]), # standard deviation of control condition
                       t(data[grepl('Sd.post',   colnames(data))]), # standard deviation of exercise condition
                       t(data[grepl('size',     colnames(data))])) # number of subjects in the study
    data <- cbind(data, str_split_fixed(rownames(data), "_", 9))
    colnames(data) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                        'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size',
                        'Studies', 'GEO', 'Muscle', 'Sex', 'Age', 'Training', 'Disease', 'Biopsy', 'Exercisetype')
    data$Studies <- gsub("logFC_","", rownames(data))
    
    tryCatch({
    data <- filter(data,
                   GEO %in% input$IN_studies, 
                   Muscle %in% input$muscle, 
                   Sex %in% input$sex, 
                   Age %in% input$age, 
                   Training %in% input$training,
                   Disease %in% input$disease)
    #order datasets by alphabetical order
    data <- data[order(rownames(data)),]
      
    #meta-analysis stats
    meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
                method = "REML", measure = "MD", data = data, control=list(maxiter=1000, stepadj=0.5))
    fdr  <- p.adjust(meta$pval, method='BH')
    #merge table with meta data
    data <- (rbind(data[,1:10],
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
    data$Studies <- c(gsub("logFC_", "", data$Studies[1:(length(data$Studies)-1)]), "Acute Aerobic score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data[,10]),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data[,10]))-1),TRUE),
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
    validate(need(!is.null(AA_data()),   "Dataset not available for the selected criteria"))
  #Plot the forest plot:
    plotInputAA()
  })
  

#- forest plot for acute resistance ----------------------------
  plotInputAR <- function(){
    # get the selected data for acute resistance
    data <- AR_data()
    # add a column with the names of the studies included in the analysis
    data$Studies <- c(gsub("logFC_", "", data$Studies[1:(length(data$Studies)-1)]), "Acute Resistance score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data[,10]),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data[,10]))-1),TRUE),
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
    validate(need(!is.null(AR_data()),   "Dataset not available for the selected criteria"))
  #Plot the forest plot:
    plotInputAR()
  })
  

#- forest plot for training aerobic ----------------------------
  plotInputTA <- function(){
    # get the selected data for training aerobic
    data <- TA_data()
    # add a column with the names of the studies included in the analysis
    data$Studies <- c(gsub("logFC_", "", data$Studies[1:(length(data$Studies)-1)]), "Training Aerobic score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data[,10]),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data[,10]))-1),TRUE),
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
      validate(need(!is.null(TA_data()),   "Dataset not available for the selected criteria"))
   #Plot the forest plot:
      plotInputTA()
  })
  
    
#- forest plot for training resistance ----------------------------
  plotInputTR <- function(){
    # get the selected data for training resistance
    data <- TR_data()
    # add a column with the names of the studies included in the analysis
    data$Studies <- c(gsub("logFC_", "", data$Studies[1:(length(data$Studies)-1)]), "Training Resistance score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data[,10]),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data[,10]))-1),TRUE),
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
    validate(need(!is.null(TR_data()),   "Dataset not available for the selected criteria"))
  #Plot the forest plot:
    plotInputTR()
})

  
#- forest plot for training combined ----------------------------
  plotInputTC <- function(){
    # get the selected data for training combined
    data <- TC_data()
    # add a column with the names of the studies included in the analysis
    data$Studies <- c(gsub("logFC_", "", data$Studies[1:(length(data$Studies)-1)]), "Training Combined score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data[,10]),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext, 
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data[,10]))-1),TRUE),
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
    validate(need(!is.null(TC_data()),   "Dataset not available for the selected criteria"))
  #Plot the forest plot:
    plotInputTC()
  })

    
#- forest plot for Inactivity ----------------------------
  plotInputIN <- function(){
    # get the selected data for inactivity
    data <- IN_data()
    # add a column with the names of the studies included in the analysis
    data$Studies <- c(gsub("logFC_", "", data$Studies[1:(length(data$Studies)-1)]), "Physical Inactivity score (REML)")
    # make forest plot
    tabledata <- data.frame(mean = c(NA , data[,1]), 
                            lower= c(NA , data[,3]),
                            upper= c(NA , data[,4]))
    tabletext<-cbind(c('Study' , data[,10]),
                     c("logFC" , format(round(data[,1], digits=2))),
                     c("FDR"   , format(data[,2],   scientific=T, digits=2)))
    forestplot(tabletext,
               tabledata, new_page=TRUE,
               is.summary=c(TRUE,rep(FALSE,(length(data[,10]))-1),TRUE),
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
    validate(need(!is.null(IN_data()),   "Dataset not available for the selected criteria"))
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

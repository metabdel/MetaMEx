#load other required libraries
library(DT)
library(gplots)
library(ggplot2)
library(ggfortify)
library(stringr)
library(scales)
library(rvest)

#URLs for sharing buttons
url_twitter  <- "https://twitter.com/intent/tweet?text=MetaMEx:%20Meta-Analysis%20of%20skeletal%20muscle%20response%20to%20inactivity%20and%20exercise.%20@NicoPillon%20@JuleenRZierath&url=http://www.metamex.eu"
url_linkedin <- "https://www.linkedin.com/shareArticle?mini=true&url=http://www.metamex.eu&title=MetaMEx:%20Meta-Analysis%20of%20skeletal%20muscle%20response%20to%20inactivity%20and%20exercise.%20&summary=MetaMEx&source=LinkedIn"
url_facebook <- "https://www.facebook.com/sharer.php?u=https://www.metamex.eu&title=MetaMEx:%20Meta-Analysis%20of%20skeletal%20muscle%20response%20to%20inactivity%20and%20exercise.%20"


# load the lists of the different studies, categories and gene names
list_datasets   <- readRDS("data/Names_datasets.Rds")
list_genes      <- readRDS("data/Names_genes.Rds")
list_categories <- readRDS("data/Names_categories.Rds")


## sanitize errors
options(shiny.sanitize.errors=T)


# Set up the graphical parameters for the forest plots
library(forestplot)
own <- fpTxtGp()
own$ticks$cex <- 0.8 #tick labels
own$xlab$cex <- 0.8
own$label$cex <- 0.8
own$summary$cex <- 1.1


#Function to make data table for selected gene
library(dplyr)
DataForGeneName <- function(x){
  x <- data.frame(t(x[grepl('logFC',    colnames(x))]), # M-value (M) is the log2-fold change
                     t(x[grepl('adj.P.Val',colnames(x))]), # Benjamini and Hochberg's method to control the false discovery rate
                     t(x[grepl('CI.L',     colnames(x))]), # lower limit of the 95% confidence interval
                     t(x[grepl('CI.R',     colnames(x))]), # upper limit of the 95% confidence interval
                     t(x[grepl('mean.pre', colnames(x))]), # mean of control condition
                     t(x[grepl('mean.post', colnames(x))]), # mean of exercise condition
                     t(x[grepl('Sd.pre',   colnames(x))]), # standard deviation of control condition
                     t(x[grepl('Sd.post',   colnames(x))]), # standard deviation of exercise condition
                     t(x[grepl('size',     colnames(x))])) # number of subjects in the study
  x <- cbind(x, str_split_fixed(rownames(x), "_", 10))
  colnames(x) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                   'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size',
                   'Studies', 'GEO', 'Muscle', 'Sex', 'Age', 'Training', 'Obesity', 'Disease',
                   'Biopsy', 'Exercisetype')
  x$Studies <- gsub("logFC_","", rownames(x))
  x
}


#Function to make meta-analysis table
library(metafor)
MetaAnalysis <- function(x){
  x <- x[order(x$Studies),]
  meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl, sd1 = Sd_Ex, sd2 = Sd_Ctrl, n1 = size, n2 = size,
              method = "REML", measure = "MD", data = x, control=list(maxiter=1000, stepadj=0.5),
              weighted=T, weights=x$size)
  fdr  <- p.adjust(meta$pval, method='BH')
  x <- rbind(x[,1:10],
             c(meta$beta, fdr, meta$ci.lb, meta$ci.ub, rep(NA, 4), sum(x$size, na.rm=T)))
  x$Studies <- c(gsub("logFC_", "", x$Studies[1:(nrow(x)-1)]),
                           "Restricted Maximum Likelihood")
  x
}


# Load the different datasets, each dataset file contains several columns for each study: fold-change, false discovery rate, mean, standard deviation, n size.
AA_Stats <- readRDS("data/Acute_Aerobic_Merged_Stats_SYMBOL.Rds")
AR_Stats <- readRDS("data/Acute_Resistance_Merged_Stats_SYMBOL.Rds")
TA_Stats <- readRDS("data/Training_Aerobic_Merged_Stats_SYMBOL.Rds")
TR_Stats <- readRDS("data/Training_Resistance_Merged_Stats_SYMBOL.Rds")
TC_Stats <- readRDS("data/Training_Combined_Merged_Stats_SYMBOL.Rds")
TH_Stats <- readRDS("data/Training_HIIT_Merged_Stats_SYMBOL.Rds")
IN_Stats <- readRDS("data/Inactivity_Merged_Stats_SYMBOL.Rds")
annotation <- readRDS("data/Datasets_legend.Rds") # Load the table describing the legend of the tables
StudiesAcute <- readRDS("data/StudiesAcute.Rds") # Load the table describing the legend of the tables
StudiesTraining <- readRDS("data/StudiesTraining.Rds") # Load the table describing the legend of the tables
StudiesInactivity <- readRDS("data/StudiesInactivity.Rds") # Load the table describing the legend of the tables
Individual_FC <- readRDS("data/Allindividuals_foldchange.Rds")
list_genes2 <- rownames(Individual_FC)
timeline_acute <- readRDS("data/Data_logFC.TimeCessation.Rds")
timeline_stats <- readRDS("data/Data_Statistics_TimeCessation.Rds")
descriptions <- readRDS("data/AllDescriptions.Rds")
  
#function to make hyperlinks
createLink <- function(val) {
  sprintf(paste0('<a href="', URLdecode(val),'" target="_blank">', gsub("(.*org/)|(.*=)", "", val) ,'</a>'))
}

#===========================================================================================
# Libraries
#===========================================================================================
library(dplyr)
library(DT)
library(forestplot)
library(ggfortify)
library(ggplot2)
library(gplots)
library(metafor)
library(stringr)
library(scales)
library(rvest)


#===========================================================================================
# URLs for sharing buttons
#===========================================================================================
url_twitter  <- "https://twitter.com/intent/tweet?text=MetaMEx:%20Meta-Analysis%20of%20skeletal%20muscle%20response%20to%20inactivity%20and%20exercise.%20@NicoPillon%20@JuleenRZierath&url=http://www.metamex.eu"
url_linkedin <- "https://www.linkedin.com/shareArticle?mini=true&url=http://www.metamex.eu&title=MetaMEx:%20Meta-Analysis%20of%20skeletal%20muscle%20response%20to%20inactivity%20and%20exercise.%20&summary=MetaMEx&source=LinkedIn"
url_facebook <- "https://www.facebook.com/sharer.php?u=https://www.metamex.eu&title=MetaMEx:%20Meta-Analysis%20of%20skeletal%20muscle%20response%20to%20inactivity%20and%20exercise.%20"


#===========================================================================================
# Graphical parameters
#===========================================================================================
options(shiny.sanitize.errors=F) # sanitize errors

# Forest plot graphical parameters
own <- fpTxtGp()
own$ticks$cex <- 0.9 #tick labels
own$xlab$cex <- 0.9
own$label$cex <- 0.9
own$summary$cex <- 1.2


#===========================================================================================
# Functions
#===========================================================================================

# Function to make data table for selected gene
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
  x <- cbind(x, str_split_fixed(rownames(x), "_", 11))
  colnames(x) <- c('logFC', 'adj.P.Val', 'CI.L', 'CI.R',
                   'Mean_Ctrl', 'Mean_Ex', 'Sd_Ctrl', 'Sd_Ex', 'size',
                   'Studies', 'GEO', 'Muscle', 'Sex', 'Age', 'Training',
                   'Obesity', 'Disease', 'Biopsy', 'Exercisetype')
  x$Studies <- gsub("logFC_","", rownames(x))
  x
}


# Function to make meta-analysis table
MetaAnalysis <- function(x){
  #Order by logFC
  x <- x[order(x$logFC, decreasing=T),]
  #Calculate metascore
  meta <- rma(m1 = Mean_Ex, m2 = Mean_Ctrl,
              sd1 = Sd_Ex, sd2 = Sd_Ctrl,
              n1 = size, n2 = size,
              method = "REML",
              measure = "MD",
              data = x,
              control=list(maxiter=1000, stepadj=0.5),
              weighted=T, weights=x$size)
  fdr  <- p.adjust(meta$pval, method='BH')
  x <- rbind(x[,1:10],
             c(meta$beta, fdr, meta$ci.lb, meta$ci.ub, rep(NA, 4), sum(x$size, na.rm=T)))
  x$Studies <- c(gsub("logFC_", "", x$Studies[1:(nrow(x)-1)]),
                           "Restricted Maximum Likelihood")
  return(x)
  }


# Function to make hyperlinks
createLink <- function(val) {
  sprintf(paste0('<a href="', URLdecode(val),'" target="_blank">', 
                 gsub("(.*org/)|(.*=)", "", val) ,'</a>'))
  }



#===========================================================================================
# Datasets
#===========================================================================================

# Lists of the different studies, categories and gene names
list_datasets   <- readRDS("data/annotation/names_datasets.Rds")
list_categories <- readRDS("data/annotation/names_categories.Rds")
list_genes      <- readRDS("data/annotation/names_genes.Rds")

# Datasets of exercise and inactivity containing several columns for each study:
# fold-change, false discovery rate, mean, standard deviation, n size.
AA_Stats <- readRDS("data/merged_stats/acute_aerobic.Rds")
AR_Stats <- readRDS("data/merged_stats/acute_resistance.Rds")
TA_Stats <- readRDS("data/merged_stats/training_aerobic.Rds")
TR_Stats <- readRDS("data/merged_stats/training_resistance.Rds")
TC_Stats <- readRDS("data/merged_stats/training_combined.Rds")
TH_Stats <- readRDS("data/merged_stats/training_hiit.Rds")
IN_Stats <- readRDS("data/merged_stats/inactivity.Rds")

# Data for reference tables
annotation <- readRDS("data/annotation/reftable_legend.Rds") # Load the table describing the legend of the tables
StudiesAcute <- readRDS("data/annotation/reftable_acute.Rds") # Load the table describing the legend of the tables
StudiesTraining <- readRDS("data/annotation/reftable_training.Rds") # Load the table describing the legend of the tables
StudiesInactivity <- readRDS("data/annotation/reftable_inactivity.Rds") # Load the table describing the legend of the tables

# Data for timeline
timeline_data <- readRDS("data/timeline/timeline_data.Rds")
timeline_stats <- readRDS("data/timeline/timeline_statistics.Rds")
timeline_genes <- rownames(timeline_data)
  
# Data for correlations
correlations_data <- readRDS("data/correlations/correlations_data.Rds")
correlations_genes <- rownames(correlations_data)
correlations_refseq <- readRDS("data/correlations/correlations_refseq.Rds")


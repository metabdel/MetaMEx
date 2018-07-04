
#load libraries
library(forestplot)
library(metafor)
library(ggpubr)
library(xlsx)
library(stringr)
library(rmarkdown)
library(grid)
library(gridExtra)
library(readr)
library(dplyr)
library(shinyjs)

# Load the different datasets, all in csv format
# Each dataset file contains several columns for each study: fold-change, false discovery rate, mean, standard deviation, n size.
Stats_AA <- readRDS("Acute_Aerobic_Merged_Stats_SYMBOL.Rds")
Stats_AR <- readRDS("Acute_Resistance_Merged_Stats_SYMBOL.Rds")
Stats_TA <- readRDS("Training_Aerobic_Merged_Stats_SYMBOL.Rds")
Stats_TR <- readRDS("Training_Resistance_Merged_Stats_SYMBOL.Rds")
Stats_TC <- readRDS("Training_Combined_Merged_Stats_SYMBOL.Rds")
Stats_IN <- readRDS("Inactivity_Merged_Stats_SYMBOL.Rds")


# Make a list of the different studies in each file
AA_names <- str_extract(string=colnames(Stats_AA), pattern = "(?<=_).*[0-9]")  #select study names 'XXXX_'
AA_names <- sort(unique(sub('_.*', '', AA_names)))                             #delete study info after '_', take unique and sort

AR_names <- str_extract(string=colnames(Stats_AR), pattern = "(?<=_).*[0-9]")
AR_names <- sort(unique(sub('_.*', '', AR_names)))

TA_names <- str_extract(string=colnames(Stats_TA), pattern = "(?<=_).*[0-9]")
TA_names <- sort(unique(sub('_.*', '', TA_names)))

TR_names <- str_extract(string=colnames(Stats_TR), pattern = "(?<=_).*[0-9]")
TR_names <- sort(unique(sub('_.*', '', TR_names)))

TC_names <- str_extract(string=colnames(Stats_TC), pattern = "(?<=_).*[0-9]")
TC_names <- sort(unique(sub('_.*', '', TC_names)))

IN_names <- str_extract(string=colnames(Stats_IN), pattern = "(?<=_).*[0-9]")
IN_names <- sort(unique(sub('_.*', '', IN_names)))


# Make a list of genes
genelist <- c(rownames(Stats_AA), rownames(Stats_AR), rownames(Stats_TA), rownames(Stats_TR), rownames(Stats_TC), rownames(Stats_IN))
genelist <- unique(genelist)


# Load the table describing the legend of the tables
annotation <- readRDS("Datasets_categories.Rds")

# Set up the different categories to be selected
muscle_choice <- c("Vastus Lateralis" = "VAL",
                   "Biceps Brachii"   = "BIB",
                   "Soleus"           = "SOL",
                   "N/A"              = "N.A")
sex_choice <- c("Male"       = "M",
                "Female"     = "F",
                "Undefined"  = "U")
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


#Function to make data table for selected gene
DataForGeneName <- function(data){
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
  data
}
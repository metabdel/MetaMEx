library(AnnotationDbi)
library(org.Hs.eg.db)
library(data.table)
library(reshape2)
setwd("C:/ownCloud/R/Shiny/MetaMEx/preprocessing")
#Custom Aggregate function
Aggregate <- dget("C:/ownCloud/R/Functions/Aggregate.R") 


#Acute Aerobic
annotdata <- readRDS("C:/ownCloud/Projects/Meta-Analysis_Exercise/Acute_Aerobic/Acute_Aerobic_Merged_Stats.Rds")
GENESYMBOL <- as.character(mapIds(org.Hs.eg.db, keys=as.character(rownames(annotdata)),
                                  column="SYMBOL",   keytype="ENSEMBL", multiVals="first"))
annotdata <- Aggregate(annotdata, GENESYMBOL)
saveRDS(annotdata, file="C:/ownCloud/R/Shiny/MetaMEx/data/Acute_Aerobic_Merged_Stats_SYMBOL.Rds")


#Acute Resistance
annotdata <- readRDS("C:/ownCloud/Projects/Meta-Analysis_Exercise/Acute_Resistance/Acute_Resistance_Merged_Stats.Rds")
GENESYMBOL <- as.character(mapIds(org.Hs.eg.db, keys=as.character(rownames(annotdata)),
                                  column="SYMBOL",   keytype="ENSEMBL", multiVals="first"))
annotdata <- Aggregate(annotdata, GENESYMBOL)
saveRDS(annotdata, file="C:/ownCloud/R/Shiny/MetaMEx/data/Acute_Resistance_Merged_Stats_SYMBOL.Rds")


#Inactivity
annotdata <- readRDS("C:/ownCloud/Projects/Meta-Analysis_Exercise/Inactivity/Inactivity_Merged_Stats.Rds")
GENESYMBOL <- as.character(mapIds(org.Hs.eg.db, keys=as.character(rownames(annotdata)),
                                  column="SYMBOL",   keytype="ENSEMBL", multiVals="first"))
annotdata <- Aggregate(annotdata, GENESYMBOL)
saveRDS(annotdata, file="C:/ownCloud/R/Shiny/MetaMEx/data/Inactivity_Merged_Stats_SYMBOL.Rds")


#Training Aerobic
annotdata <- readRDS("C:/ownCloud/Projects/Meta-Analysis_Exercise/Training_Aerobic/Training_Aerobic_Merged_Stats.Rds")
GENESYMBOL <- as.character(mapIds(org.Hs.eg.db, keys=as.character(rownames(annotdata)),
                                  column="SYMBOL",   keytype="ENSEMBL", multiVals="first"))
annotdata <- Aggregate(annotdata, GENESYMBOL)
saveRDS(annotdata, file="C:/ownCloud/R/Shiny/MetaMEx/data/Training_Aerobic_Merged_Stats_SYMBOL.Rds")


#Training Combined
annotdata <- readRDS("C:/ownCloud/Projects/Meta-Analysis_Exercise/Training_Combined/Training_Combined_Merged_Stats.Rds")
GENESYMBOL <- as.character(mapIds(org.Hs.eg.db, keys=as.character(rownames(annotdata)),
                                  column="SYMBOL",   keytype="ENSEMBL", multiVals="first"))
annotdata <- Aggregate(annotdata, GENESYMBOL)
saveRDS(annotdata, file="C:/ownCloud/R/Shiny/MetaMEx/data/Training_Combined_Merged_Stats_SYMBOL.Rds")


#Training HIIT
annotdata <- readRDS("C:/ownCloud/Projects/Meta-Analysis_Exercise/Training_HIIT/Training_HIIT_Merged_Stats.Rds")
GENESYMBOL <- as.character(mapIds(org.Hs.eg.db, keys=as.character(rownames(annotdata)),
                                  column="SYMBOL",   keytype="ENSEMBL", multiVals="first"))
annotdata <- Aggregate(annotdata, GENESYMBOL)
saveRDS(annotdata, file="C:/ownCloud/R/Shiny/MetaMEx/data/Training_HIIT_Merged_Stats_SYMBOL.Rds")


#Training Resistance
annotdata <- readRDS("C:/ownCloud/Projects/Meta-Analysis_Exercise/Training_Resistance/Training_Resistance_Merged_Stats.Rds")
GENESYMBOL <- as.character(mapIds(org.Hs.eg.db, keys=as.character(rownames(annotdata)),
                                  column="SYMBOL",   keytype="ENSEMBL", multiVals="first"))
annotdata <- Aggregate(annotdata, GENESYMBOL)
saveRDS(annotdata, file="C:/ownCloud/R/Shiny/MetaMEx/data/Training_Resistance_Merged_Stats_SYMBOL.Rds")


#Individual fold-changes
annotdata <- readRDS("C:/ownCloud/Projects/Meta-Analysis_Exercise/Allindividuals_foldchange.Rds")
saveRDS(annotdata, file="C:/ownCloud/R/Shiny/MetaMEx/data/Allindividuals_foldchange.Rds")

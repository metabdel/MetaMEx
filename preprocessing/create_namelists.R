setwd("C:/Dropbox/NICO/R/Shiny/MetaMEx")
library(stringr)

# Make a table of the studies and annotation
library(readxl)
#function to make hyperlinks
createLink <- function(val) {
  sprintf(paste0('<a href="', URLdecode(val),'" target="_blank">', gsub("(.*org/)|(.*=)", "", val) ,'</a>'))
}

StudiesAcute <- read_xlsx("C:/Dropbox/NICO/R/Meta-Analysis_Exercise/Datasets.xlsx", na='NA', sheet=1)
StudiesAcute <- StudiesAcute[,1:16]
StudiesAcute$GEO <- paste("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", StudiesAcute$GEO, sep="")
StudiesAcute$GEO  <- sapply(StudiesAcute$GEO, createLink)
StudiesAcute$Publication <- sapply(StudiesAcute$Publication, createLink)

StudiesTraining <- read_xlsx("C:/Dropbox/NICO/R/Meta-Analysis_Exercise/Datasets.xlsx", na='NA', sheet=2)
StudiesTraining <- StudiesTraining[,1:18]
StudiesTraining$GEO <- paste("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", StudiesTraining$GEO, sep="")
StudiesTraining$GEO  <- sapply(StudiesTraining$GEO, createLink)
StudiesTraining$Publication <- sapply(StudiesTraining$Publication, createLink)

StudiesInactivity <- read_xlsx("C:/Dropbox/NICO/R/Meta-Analysis_Exercise/Datasets.xlsx", na='NA', sheet=3)
StudiesInactivity <- StudiesInactivity[,1:16]
StudiesInactivity$GEO <- paste("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", StudiesInactivity$GEO, sep="")
StudiesInactivity$GEO  <- sapply(StudiesInactivity$GEO, createLink)
StudiesInactivity$Publication <- sapply(StudiesInactivity$Publication, createLink)

CategoryTable <- read_xlsx("C:/Dropbox/NICO/R/Meta-Analysis_Exercise/Datasets.xlsx", na='NA', sheet=4)

saveRDS(StudiesAcute, "data/StudiesAcute.Rds")
saveRDS(StudiesTraining, "data/StudiesTraining.Rds")
saveRDS(StudiesInactivity, "data/StudiesInactivity.Rds")
saveRDS(CategoryTable, "data/Datasets_legend.Rds")

# Make a list of the different studies names in each file
Stats_AA <- readRDS("data/Acute_Aerobic_Merged_Stats_SYMBOL.Rds")
Stats_AR <- readRDS("data/Acute_Resistance_Merged_Stats_SYMBOL.Rds")
Stats_TA <- readRDS("data/Training_Aerobic_Merged_Stats_SYMBOL.Rds")
Stats_TR <- readRDS("data/Training_Resistance_Merged_Stats_SYMBOL.Rds")
Stats_HI <- readRDS("data/Training_HIIT_Merged_Stats_SYMBOL.Rds")
Stats_TC <- readRDS("data/Training_Combined_Merged_Stats_SYMBOL.Rds")
Stats_IN <- readRDS("data/Inactivity_Merged_Stats_SYMBOL.Rds")

AA_names <- str_extract(string=colnames(Stats_AA), pattern = "(?<=_).*[0-9]")  #select study names 'XXXX_'
AA_names <- sort(unique(sub('_.*', '', AA_names)))                             #delete study info after '_', take unique and sort
AR_names <- str_extract(string=colnames(Stats_AR), pattern = "(?<=_).*[0-9]")
AR_names <- sort(unique(sub('_.*', '', AR_names)))
TA_names <- str_extract(string=colnames(Stats_TA), pattern = "(?<=_).*[0-9]")
TA_names <- sort(unique(sub('_.*', '', TA_names)))
TR_names <- str_extract(string=colnames(Stats_TR), pattern = "(?<=_).*[0-9]")
TR_names <- sort(unique(sub('_.*', '', TR_names)))
HI_names <- str_extract(string=colnames(Stats_HI), pattern = "(?<=_).*[0-9]")
HI_names <- sort(unique(sub('_.*', '', HI_names)))
TC_names <- str_extract(string=colnames(Stats_TC), pattern = "(?<=_).*[0-9]")
TC_names <- sort(unique(sub('_.*', '', TC_names)))
IN_names <- str_extract(string=colnames(Stats_IN), pattern = "(?<=_).*[0-9]")
IN_names <- sort(unique(sub('_.*', '', IN_names)))

list_names <- list(AA_names=AA_names,
                   AR_names=AR_names,
                   TA_names=TA_names,
                   TR_names=TR_names,
                   HI_names=HI_names,
                   TC_names=TC_names,
                   IN_names=IN_names)
saveRDS(list_names, "data/Names_datasets.Rds")

# Make a list of genes
list_genes <- c(rownames(Stats_AA),
                rownames(Stats_AR),
                rownames(Stats_TA),
                rownames(Stats_TR),
                rownames(Stats_HI),
                rownames(Stats_TC),
                rownames(Stats_IN))
list_genes <- unique(list_genes)
saveRDS(list_genes, "data/Names_genes.Rds")

# Set up the different categories to be selected
list_categories <- list(
  muscle_choice = c("Vastus Lateralis" = "VAL",
                     "Biceps Brachii"   = "BIB",
                     "Soleus"           = "SOL",
                     "N/A"              = "N.A"),
  sex_choice = c("Male"       = "M",
                  "Female"     = "F",
                  "Undefined"  = "U"),
  age_choice = c("Young"   = "YNG",
                  "Middle"    = "MDL",
                  "Elderly" = "ELD"),
  training_choice = c("Sedentary" = "SED",
                       "Active"    = "ACT",
                       "Athlete"   = "ATH"),
  exercise_choice = c("Concentric" = "CON",
                       "Eccentric"  = "ECC",
                       "Mixed"      = "MIX"),
  biopsy_choice = c("Immediately after" = "IMM",
                     "After a recovery period"    = "REC"),
  disease_choice = c("Healthy" = "HLY",
                      "Overweight/Obese" = "OBE",
                      "Type 2 diabetes" = "T2D",
                      "Metabolic Syndrome" = "MTS",
                      "Chronic Kidney Disease" = "CKD",
                      "Chronic Obstructive Pulmonary Disease" = "COP")
)
saveRDS(list_categories, "data/Names_categories.Rds")

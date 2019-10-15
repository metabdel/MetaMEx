setwd("C:/ownCloud/R/Shiny/MetaMEx/preprocessing")

Individual_FC <- readRDS("../data/Allindividuals_foldchange.Rds")



selectedata <- Individual_FC #[c('NR4A3', 'EGR1', 'HEY1', 'HES1'),]
selectedata <- selectedata[rowSums(is.na(selectedata)) / ncol(selectedata) < 25/100, ]

estimate <- function(x) cor.test(x, geneofinterest, method="spearman", exact=F)$estimate
p.value  <- function(x) cor.test(x, geneofinterest, method="spearman", exact=F)$p.value


AllStats <- vector("list", nrow(selectedata))
names(AllStats) <- rownames(selectedata)
for (i in 1:nrow(selectedata)){
geneofinterest <- as.numeric(selectedata[i,])
Spearman.r <- apply(selectedata, 1, estimate)
Spearman.p <- apply(selectedata, 1, p.value)
Spearman.FDR <- p.adjust(Spearman.p, method="bonferroni")
Spearman.r <- round(Spearman.r, digits=2)
Spearman.p <- signif(Spearman.p, digits=2)
Spearman.FDR <- signif(Spearman.FDR, digits=2)
coeff <- data.frame(Spearman.r, Spearman.p, Spearman.FDR)
colnames(coeff) <- c("Spearman.r", "P.value", "FDR")
coeff <- coeff[order(coeff$FDR),]
AllStats[[i]] <- coeff
}

saveRDS(AllStats, "../data/AllCorrelations.Rds")



# gene description
library(AnnotationDbi)
library(org.Hs.eg.db)
library(rvest)
GENENAME <- rownames(Individual_FC)
ENTREZID <- as.character(mapIds(org.Hs.eg.db, keys=GENENAME,
                                  column="ENTREZID",   keytype="SYMBOL", multiVals="first"))
Annotation <- data.frame(GENENAME, ENTREZID, REFSEQ=NA)
saveRDS(Annotation, "../data/AllDescriptions.Rds")


for (i in 1:nrow(Annotation)){
webpage <- read_html(paste("https://www.ncbi.nlm.nih.gov/gene/", Annotation$ENTREZID[i], sep=''))
data_html <- html_nodes(webpage,'dd:nth-child(20)')
data_html <- gsub('<dd>', '', data_html)
data_html <- gsub('</dd>', '', data_html)
Annotation$REFSEQ[i] <- data_html
print(paste(i, "out of", nrow(Annotation), "done!"))
}

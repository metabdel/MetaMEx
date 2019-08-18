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

# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(dplyr, factoextra, fastICA, ggplot2, ggpubr, NMF,  party, psych, randomForest,
               reshape2, Rtsne, shipunov, tidyverse, tseries, umap, vegan)

#Empty Global Enviroment
rm(list = ls())

setwd("E:/Eranthis/PR_DPO")


pacman::p_load(seqinr, phangorn, msa, ape)

# FASTA_Analyze and align -----------------------------------------------------------------------------------------


dnafile <- "atpb_rbcl.fa" 
DNAfile <- read.fasta(file = dnafile, as.string = TRUE, forceDNAtolower = FALSE)

myFirstAlignment <- msa(DNAfile)

myalign <- msa("atpb_rbcl.fa", method=c("ClustalW"), type="dna",verbose=FALSE)
print(myalign, show="complete")
myalign_ap <- msaConvert(myalign, type="seqinr::alignment")
d <- dist.alignment(myalign_ap, "identity")
hemoTree <- nj(d)
plot(hemoTree, main="Phylogenetic Tree")


# STAT ------------------------------------------------------------------------------------------------------------

pacman::p_load(httr, XML, dplyr, xml2, rvest, writexl)

flora <- read_html("http://panarcticflora.org/distribution")

tables = flora %>% html_table()
arctic_flora <- data.frame(tables[[4]])
#write_xlsx(arctic_flora, "E:/Eranthis/arctic_flora.xlsx")

arctic_flora$Family <- NA
arctic_flora$Genus <- NA
arctic_flora$Species <- NA

arctic_flora$X1 <- as.numeric(arctic_flora$X1)

count_digits <- function(x) {
  sum(grepl("\\d", unlist(strsplit(x, ""))))
}


for (i in 1:nrow(arctic_flora)) {
  num_digits <- count_digits(arctic_flora$X2[i])
  if (is.na(arctic_flora$X1[i]) || num_digits != 2 && num_digits != 4) {
    arctic_flora$Species[i] <- arctic_flora$X2[i]
  } else if (num_digits == 2) {
    arctic_flora$Family[i] <- arctic_flora$X2[i]
  } else if (num_digits == 4) {
    arctic_flora$Genus[i] <- arctic_flora$X2[i]
  }
}

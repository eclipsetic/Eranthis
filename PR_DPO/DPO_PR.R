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



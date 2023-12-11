library(xlsx)
library(dplyr)

#sample data
species <- read.csv("species_data.csv", header = T,row.names = 1)
Group <- read.xlsx("group.xlsx",sheetIndex = 1)

group_Hgcf <- Group%>%filter(Group$group=='Hgcf')
species_Hgcf <- species[,group_Hgcf$sample]
species_Hgcf_core <- data.frame(rowSums(species_Hgcf != 0)/ncol(species_Hgcf))
species_Hgcf_core$abundance <- rowSums(species_Hgcf)/ncol(species_Hgcf)
colnames(species_Hgcf_core) <- c('occurrence','abundance')
core_Hgcf <- species_Hgcf_core%>%filter(species_Hgcf_core$occurrence>=0.8 & species_Hgcf_core$abundance >=0.001)

group_PLgcf <- Group%>%filter(Group$group=='PLgcf')
species_PLgcf <- species[,group_PLgcf$sample]
species_PLgcf_core <- data.frame(rowSums(species_PLgcf != 0)/ncol(species_PLgcf))
species_PLgcf_core$abundance <- rowSums(species_PLgcf)/ncol(species_PLgcf)
colnames(species_PLgcf_core) <- c('occurrence','abundance')
core_PLgcf <- species_PLgcf_core%>%filter(species_PLgcf_core$occurrence>=0.8 & species_PLgcf_core$abundance >=0.001)

group_PMgcf <- Group%>%filter(Group$group=='PMgcf')
species_PMgcf <- species[,group_PMgcf$sample]
species_PMgcf_core <- data.frame(rowSums(species_PMgcf != 0)/ncol(species_PMgcf))
species_PMgcf_core$abundance <- rowSums(species_PMgcf)/ncol(species_PMgcf)
colnames(species_PMgcf_core) <- c('occurrence','abundance')
core_PMgcf <- species_PMgcf_core%>%filter(species_PMgcf_core$occurrence>=0.8 & species_PMgcf_core$abundance >=0.001)

group_PHgcf <- Group%>%filter(Group$group=='PHgcf')
species_PHgcf <- species[,group_PHgcf$sample]
species_PHgcf_core <- data.frame(rowSums(species_PHgcf != 0)/ncol(species_PHgcf))
species_PHgcf_core$abundance <- rowSums(species_PHgcf)/ncol(species_PHgcf)
colnames(species_PHgcf_core) <- c('occurrence','abundance')
core_PHgcf <- species_PHgcf_core%>%filter(species_PHgcf_core$occurrence>=0.8 & species_PHgcf_core$abundance >=0.001)

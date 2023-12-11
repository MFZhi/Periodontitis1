library(TCseq)
library(dplyr)
library(xlsx)

#sample data
species <- read.csv("plot_data.csv",row.names=1, header = T)
species <- as.matrix(species)

set.seed(123)
cluster_num <- 8
tcseq_cluster <- timeclust(species, algo = 'cm', k = cluster_num, standardize = TRUE)

p <- timeclustplot(tcseq_cluster, value = 'z-score', cols = 4, 
                   axis.line.size = 0.6, axis.title.size = 8, axis.text.size = 8, 
                   title.size = 8, legend.title.size = 8, legend.text.size = 8)


species_cluster <- tcseq_cluster@cluster
species_cluster <- cbind(species[names(species_cluster), ], species_cluster)
head(species_cluster)
write.table(species_cluster, 'species_cluster_gcf.txt', sep = '\t', col.names = NA, quote = FALSE)
write.csv(species_cluster, 'species_cluster_gcf.csv',row.names = T)

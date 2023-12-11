library(reshape2)
library(ggplot2)
library(ggrepel)

#sample data
species <- read.csv("data.csv",row.names = 1, header = T)
species <- aggregate(species[,-ncol(species)],by = list(group = species$group),FUN = mean) 
rownames(species) <- species$group
species <- species[,-1]
species <- data.frame(t(species))
species$saltb <- species$Sal/species$TB
species$gcfsaltb <- species$GCF/((species$TB+species$Sal)/2)

tax <- read.csv("tax.csv", header = T)
species <- merge(species,tax,by.x = 'row.names',by.y = 'tax.species')

p <- ggplot(species, aes(x=gcfsaltb, y=saltb,color=tax.phylum,size=GCF)) + 
  scale_x_log10(labels=scales::dollar)+
  scale_y_log10(labels=scales::dollar)+
  scale_size(limits =c(0,0.15),range = c(1,10) )+
  geom_point()+
  scale_color_manual(values = c("Actinobacteria"= '#e41a1c',"Bacteroidetes"= '#377eb8',"Firmicutes"='#4daf4a',
                                "Fusobacteria"='#984ea3',"Proteobacteria"= '#ff7f00',"Spirochaetes"= '#CD8C95',"Synergistetes"= '#a65628')) +
  geom_vline(xintercept = 20,linetype = "dashed")+
  geom_vline(xintercept = 0.05,linetype = "dashed")+
  geom_vline(xintercept = 5,linetype = "dashed")+
  geom_vline(xintercept = 0.2,linetype = "dashed")+
  geom_hline(yintercept = 20,linetype = "dashed")+
  geom_hline(yintercept = 0.05,linetype = "dashed")+
  geom_hline(yintercept = 5,linetype = "dashed")+
  geom_hline(yintercept = 0.2,linetype = "dashed")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank())
p



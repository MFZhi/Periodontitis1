library(dplyr)
library(xlsx)
library(ppcor)
set.seed(123)

#sample data
species <- read.csv("species_data.csv",row.names = 1,header=T) 
species <- data.frame(t(species))
clinical <- read.csv("clinical_data.csv",row.names = 1,header=T) 
center <- sweep(clinical, 2, apply(clinical, 2, min),'-') 
R <- apply(clinical, 2, max) - apply(clinical,2,min)  
clinical<- sweep(center, 2, R, "/") 
clinical_z <- clinical[,c(2,3)]
clinical <- clinical[,-c(2,3)]
clinical <- clinical[rownames(species),]

#partial correlation
result <- data.frame()
col_species = colnames(species)
col_clinical = colnames(clinical)
for(i in (1:ncol(species)))
{
  for(j in (1:ncol(clinical)))
  {
    data <- data.frame(species[,i],clinical[,j],clinical_z[,1],clinical_z[,2])
    data<- na.omit(data)
    colnames(data) <- c('y','x','Gender','Age')
    a=(data[,1])
    b=(data[,2])
    cc = pcor.test(x=b,y=a,z=data[,c('Gender','Age')],method="spearman")
    result[(i-1)*ncol(clinical)+j,1]=col_species[i]
    result[(i-1)*ncol(clinical)+j,2]=col_clinical[j]
    result[(i-1)*ncol(clinical)+j,3]=cc$estimate
    result[(i-1)*ncol(clinical)+j,4]=cc$p.value
  }
}
colnames(result) <- c("species","meta","cor","pvalue")
result$qvalue= p.adjust(result$pvalue,method="fdr",length(result$pvalue))
write.csv(result,"result_partial_correlation.csv",row.names = F)

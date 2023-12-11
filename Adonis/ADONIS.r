library(vegan)
library(xlsx)
library(dplyr)
set.seed(123)
#sample data
clinical <- read.csv("clinical_data.csv",header=T) 
data <- read.csv("data.csv",row.names=1, header = T)
data <- data.frame(t(data))
clinical_new <- clinical
quantile(clinical_new$Age,probs = seq(0,1,0.25))
clinical_new[which(clinical_new$Age>=quantile(clinical_new$Age,0) & clinical_new$Age<quantile(clinical_new$Age,.25),arr.ind = F),4]=1
clinical_new[which(clinical_new$Age>=quantile(clinical_new$Age,.25) & clinical_new$Age<quantile(clinical_new$Age,.5),arr.ind = F),4]=2
clinical_new[which(clinical_new$Age>=quantile(clinical_new$Age,.5) & clinical_new$Age<quantile(clinical_new$Age,.75),arr.ind = F),4]=3
clinical_new[which(clinical_new$Age>=quantile(clinical_new$Age,.75),arr.ind = F),4]=4

rownames(clinical_new) <- clinical_new$Sample
colname <- colnames(clinical_new)
result <- data.frame()
for(i in 2:ncol(clinical_new))
{
  clinic <- clinical_new[,c(1,i)]
  clinic <- na.omit(clinic)
  data_temp <- data[clinic$Sample,]
  adonis_result <- adonis2(data_temp ~ as.factor(clinic[,2]), clinic, distance = 'bray', permutations = 1000)
  result[i,1] <- adonis_result[1,3]
  result[i,2] <- adonis_result[1,5]
  anosim_result <- anosim(data_temp, as.factor(clinic[,2]), distance = 'bray', permutations = 1000)
  result[i,3] <- anosim_result$statistic
  result[i,4] <- anosim_result$signif
  mrpp_result <- mrpp(data_temp, as.factor(clinic[,2]), distance = 'bray', permutations = 1000)
  result[i,5] <- mrpp_result$A
  result[i,6] <- mrpp_result$Pvalue
  dbRDA_result <- capscale(data_temp ~ as.factor(clinic[,2]), clinic,distance = "bray",add = TRUE)
  result[i,7] <- RsquareAdj(dbRDA_result)$r.squared
  dbrda_cca = anova.cca(dbRDA_result,permutations = 1000)
  result[i,8] <- dbrda_cca$`Pr(>F)`[1]
}
rownames(result) <- colnames(clinical_new)
colnames(result) <- c('adonis_R2','adonis_P','anosim_R2','anosim_P','mrpp_R2','mrpp_P','dbRDA_R2','dbRDA_P')
result <- result[-1,]
write.csv(result,'result.csv',row.names =T)

result_p <- result%>%filter(result$adonis_P<0.05)
result_p <- result_p%>%filter(result_p$anosim_P<0.05)
result_p <- result_p%>%filter(result_p$mrpp_P<0.05)
result_p <- result_p%>%filter(result_p$dbRDA_P<0.05)
write.csv(result_p,'result_p.csv',row.names =T)




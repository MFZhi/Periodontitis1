library(mediation)
library(dplyr)
library(xlsx)
library(ggsankey)
library(tidyverse)
set.seed(123)
#sample data
species <- read.csv("species_data.csv",row.names = 1,header=T) 
species <- data.frame(t(species))
clinical <- read.csv("clinical_data.csv",row.names = 1,header=T) 
center <- sweep(clinical, 2, apply(clinical, 2, min),'-') 
R <- apply(clinical, 2, max) - apply(clinical,2,min)  
clinical<- sweep(center, 2, R, "/") 
clinical <- clinical[rownames(species),]

#mediation
result <- data.frame()
clinical <- clinical[rownames(species),]
name_species <- colnames(species)
name_clinical <- colnames(clinical)
for(i in 1:ncol(species))
{
  for(j in 4:ncol(clinical))
  {
    n <- (i-1)*8+j-3
    result[n,1] <- name_species[i]
    result[n,2] <- name_clinical[j]
    result[n,3] <- name_clinical[1]
    
    data <- cbind(species[,i],clinical[,j],clinical[,1],clinical[,2],clinical[,3])
    data <- na.omit(data)
    x <- data[,1]
    f <- data[,2]
    y <- data[,3]
    age <- data[,4]
    gender <- data[,5]

    a <- lm(f ~ x+age+gender)
    b <- lm(y ~ x + f+age+gender)
    contcont <- mediate(a, b, treat="x", mediator="f",sims = 1000, boot = T)
    temp <- summary(contcont)
    result[n,4] <- temp$d0
    result[n,5] <- temp$d0.ci[1]
    result[n,6] <- temp$d0.ci[2]
    result[n,7] <- temp$d0.p
    result[n,8] <- temp$z0
    result[n,9] <- temp$z0.ci[1]
    result[n,10] <- temp$z0.ci[2]
    result[n,11] <- temp$z0.p
    result[n,12] <- temp$tau.coef
    result[n,13] <- temp$tau.ci[1]
    result[n,14] <- temp$tau.ci[2]
    result[n,15] <- temp$tau.p
    result[n,16] <- temp$n0
    result[n,17] <- temp$n0.ci[1]
    result[n,18] <- temp$n0.ci[2]
    result[n,19] <- temp$n0.p
    #plot(contcont)
  }
}
colnames(result) <- c('x','factor','y','ACME Estimate','ACME 95% CI','ACME Lower 95% CI','ACME p-value','ADE Estimate','ADE 95% CI','ADE Lower 95% CI','ADE p-value'
                      ,'Total Effect Estimate','Total Effect 95% CI','Total Effect Lower 95% CI','Total Effect p-value','Prop. Mediated Estimate','Prop. Mediated 95% CI','Prop. Mediated Lower 95% CI','Prop. Mediated p-value')
result <- na.omit(result)
result$ACME_qvalue <- p.adjust(result$`ACME p-value`,method="fdr",length(result$`ACME p-value`))
result$Total_Effect_qvalue <- p.adjust(result$`Total Effect p-value`,method="fdr",length(result$`Total Effect p-value`))
result$Prop_Mediated_qvalue <- p.adjust(result$`Prop. Mediated p-value`,method="fdr",length(result$`Prop. Mediated p-value`))
write.csv(result,"result.csv",row.names = F)
result <- result%>%filter(result$ACME_qvalue < 0.05)
result <- result%>%filter(result$Total_Effect_qvalue < 0.05)
result <- result%>%filter(result$`Prop. Mediated Estimate` < 1)
result <- result[,c(1,2,3)]
colnames(result) <- c('x','factor','y')

#plot
result$x <- gsub('_',' ',result$x)
data$factor <- gsub('_',' ',result$factor)
result$y <- gsub('_',' ',result$y)
df <- result %>% make_long(x, factor, y)
mycol <- c('#8DD3C7', "#696969", '#FFFFB3',"#000000","#696969", '#BEBADA',
           "#696969", '#FB8072', "#696969", '#80B1D3', '#FDB462',
           "#696969", "#696969", '#B3DE69', '#BC80BD')
p <- ggplot(df, aes(x = x, next_x = next_x,
                    node = node, next_node = next_node,
                    fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = 1.6,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "black", fill = "white") +
  scale_fill_manual(values = mycol)+
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) 

p







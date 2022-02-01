
library(data.table)
library(pheatmap)
library(RColorBrewer)
library(plyr)
library(grid)

cluster_company<- fread('Company_Community_Louvain.csv')
cluster_length<- cluster_company[,.(.N),Cluster]
setorder(cluster_length,-N)
la<- 20
cluster_num<- cluster_length$Cluster[1:la]

company_industry_new<- fread('Company_Sector_Community.csv')
Industry<- unique(company_industry_new$Industry)
industry_num<- company_industry_new[,.(.N),by=Industry]

id<- match(Industry,industry_num$Industry)
industry_num<- industry_num[id,]

combine_num<- ddply(company_industry_new,.(Cluster,Industry),nrow)

industry_combine2<- matrix(0,nrow = la,ncol = length(Industry))
row.names(industry_combine2)<- 1:la
colnames(industry_combine2)<- Industry
for(i in 1:la){
  id<- which(combine_num$Cluster==cluster_num[i])
  id1<- match(combine_num$Industry[id],Industry)
  industry_combine2[i,id1]<- combine_num$V1[id]/industry_num$N[id1]
}

### plot
draw_colnames_45 <- function (coln, ...) {
  m = length(coln)
  x = (1:m)/m - 1/2/m
  grid.text(coln, x = x, y = unit(0.96, "npc"), vjust = .5,
            hjust = 1, rot = 90, gp = gpar(...)) 
}
assignInNamespace(x="draw_colnames", value="draw_colnames_45",
                  ns=asNamespace("pheatmap"))

id_indu<- c(9,7,6,2,1,5,4,10,8,3,11) ## just reorder
industry_combine2<- industry_combine2[,id_indu]
colnames(industry_combine2)<- c('Health Care',	'Consumer Discretionary',	'Information Technology',	
                                'Financials',	'Materials',	'Energy',	'Utilities',	'Communication Services',	
                                'Real Estate',	'Consumer Staples',	'Industrials')
rownames(industry_combine2)<-  c('Media','Finance','Electronics','Defense','Education',
                                 'Clothing','Software','Pharma & Healthcare','Education West','Geography - US-MA',
                                 'Energy','Non-US(Global)','Global Tech','Political','Hospitality','Consulting','Geography - US-IL',
                                 'Communications','Banks','Consumable')
pheatmap(industry_combine2,cluster_col=F,cluster_rows=F,display_numbers = T,
         number_format = "%.2f",fontsize = 12,angle_col = '270',
         color = colorRampPalette(rev(brewer.pal(n = 3, name ="RdYlBu")))(100),
         main='(a)')

## sector homophily
dt<- industry_combine2
Entropies <- vector()
for (i in 1: dim(dt)[2]){
  Entropies[i]<- 1-sum(dt[,i]*log2(dt[,i]),na.rm = T)/log2(1/dim(dt)[1])
}
Entropies<- data.frame(Industry[id_indu],Entropies)

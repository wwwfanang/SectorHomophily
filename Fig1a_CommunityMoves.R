library(data.table)
library(igraph)
library(pheatmap)
library(RColorBrewer)
library(grid)

data_all<- fread('Company_Community_Sector.csv')

## choose the top 20 largest communities
cluster_community<- unique(rbind(data.table(Company=data_all$From,Cluster=data_all$From.C),
                                 data.table(Company=data_all$To,Cluster=data_all$To.C)))
community_length<- cluster_community[,.(.N),Cluster]
colnames(community_length)[2]<- 'Size'
setorder(community_length,-Size)
la<- 20
cluster_la<- community_length$Cluster[1:la]

data2<- data_all[which(From.C%in%cluster_la)]
data2<- data2[which(To.C%in%cluster_la)]

## number of move between two community
data1_num<- data2[,sum(Weight),by=.(From.C,To.C)]
colnames(data1_num)[3]<- 'N'

cluster_c<- matrix(0,ncol = la,nrow = la)## log 10 number of moves
for(j in 1:dim(data1_num)[1]){
  # print(j)
  id<- c(data1_num$From.C[j],data1_num$To.C[j])
  id_c<- match(id,cluster_la)
  cluster_c[id_c[1],id_c[2]]<- log10(data1_num$N[j])
}

## plot
draw_colnames_45 <- function (coln, ...) {
  m = length(coln)
  x = (1:m)/m - 1/2/m
  grid.text(coln, x = x, y = unit(0.96, "npc"), vjust = .5,
            hjust = 1, rot = 90, gp = gpar(...)) 
}
assignInNamespace(x="draw_colnames", value="draw_colnames_45",
                  ns=asNamespace("pheatmap"))

industry_name<- c('Media','Finance','Electronics','Defense','Education',
                  'Clothing','Software','Pharma & Healthcare','Education West','Geography - US-MA',
                  'Energy','Non-US(Global)','Global Tech','Political','Hospitality','Consulting','Geography - US-IL',
                  'Communications','Banks','Consumable')
colnames(cluster_c)<- industry_name
rownames(cluster_c)<- industry_name
pheatmap(cluster_c,cluster_col=F,cluster_rows=F,display_numbers = T,
         number_format = "%.1f",breaks=seq(3,6,0.1),fontsize_col = 15,
         color = colorRampPalette(rev(brewer.pal(n = 3, name ="RdYlBu")))(31))


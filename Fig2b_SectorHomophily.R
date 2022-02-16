library(data.table)
library(igraph)
library(plyr)
library(tidyr)

data_all<- fread('Company_Community_Sector.csv')
company_industry<- fread('Company_Sector.csv')
Industry<- unique(company_industry$Industry)
id_indu<- c(9,7,6,2,1,5,4,10,8,3,11)

cluster_company<- unique(rbind(data.table(Company=data_all$From,Cluster=data_all$From.C),
                               data.table(Company=data_all$To,Cluster=data_all$To.C)))
cluster_length<- cluster_company[,.(.N),Cluster]
setorder(cluster_length,-N)
la<- 20
cluster_num<- cluster_length$Cluster[1:la]

id<- which(company_industry$Cluster%in%cluster_num)
company_industry<- company_industry[id,]

#### shuffle data
iter<- 1000

entropy<- vector()
for(j in 1:iter){
  print(j)
  company_industry_new<- company_industry
  company_industry_new$Cluster<- sample(cluster_num,dim(company_industry_new)[1],replace = T)
  
  combine_num<- ddply(company_industry_new,.(Cluster,Industry),nrow)
  
  industry_num<- company_industry_new[,.(.N),by=Industry]
  industry_num<- industry_num[match(Industry,industry_num$Industry),]
  
  industry_combine<- matrix(0,nrow = la,ncol = length(Industry))
  row.names(industry_combine)<- 1:la
  colnames(industry_combine)<- Industry
  for(i in 1:la){
    id<- which(combine_num$Cluster==cluster_num[i])
    id1<- match(combine_num$Industry[id],Industry)
    
    industry_combine[i,id1]<- combine_num$V1[id]/industry_num$N[id1]
  }
  
  ## homopjily
  dt<- industry_combine
  Entropies <- vector()
  for (i in 1: dim(dt)[2]){
    Entropies[i]<- 1-sum(dt[,i]*log2(dt[,i]),na.rm = T)/log2(1/dim(dt)[1])
  }
  entropy<- cbind(entropy,Entropies)
}

entropy_all<- data.table()
for(i in seq_along(Industry)){
  entropy_all<- cbind(entropy_all,data.frame(P=entropy[i,]))
}
entropy_all<- data.frame(entropy_all)[,id_indu]

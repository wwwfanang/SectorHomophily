library(data.table)
library(igraph)

data_all<- fread('Company_Community_Sector.csv')
cluster_community<- unique(rbind(data.table(Company=data_all$From,Cluster=data_all$From.C),
                                 data.table(Company=data_all$To,Cluster=data_all$To.C)))

ends_unique<- data_all
colnames(ends_unique)[3]<- 'Moves'
setorder(ends_unique,-Moves)
g_all<- graph_from_data_frame(ends_unique,directed = T)
E(g_all)$weight<- ends_unique$Moves

# Gephi company node ------------------------------------------------------

cluster_length<- cluster_community[,.(.N),by=Cluster]
setorder(cluster_length,-N)

la<- 20
cluster_la<- cluster_length$Cluster[1:la]

move_out<- data_all[,sum(Weight),From]
move_in<-data_all[,sum(Weight),To]
move_all<- merge(move_out,move_in,by.x='From',by.y = 'To',all=T)
move_sum<- move_all[,sum(N.x,N.y,na.rm=T),From]

cluster_company_move<- merge(cluster_community,move_sum,by.x='Company',by.y = 'From',all=T)

## choose the company with move >100 
sa<- 500

company_name<- data.table()
num<- 0
for(i in 1:la){
  print(i)
  comm1<- cluster_company_move[which(cluster_company_move$Cluster==cluster_la[i]),]
  setorder(comm1,-V1)
  id<- which(comm1$V1>sa)
  company1<- comm1$Company[id]
  
  company_name<- rbind(company_name,data.table(Company=company1,Id=(num+1):(num+length(company1)),Cluster=cluster_la[i]))
  num<- num+length(company1)
}

g_all1<- induced.subgraph(g_all,company_name$Company)
id<- match(V(g_all1)$name,company_name$Company)
V(g_all1)$name<- company_name$Id[id]

V<- data.table(Id=1:length(V(g_all1)),Name=1:length(V(g_all1)),Community=company_name$Cluster)

ends<- ends(g_all1,E(g_all1))
E<- data.table(Source=ends[,1],Target=ends[,2],Weight=E(g_all1)$weight,Id=1:length(E(g_all1)))
fwrite(V,'Gephi_CompanyV.csv')
fwrite(E,'Gephi_CompanyE.csv')






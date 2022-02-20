library(data.table)
library(networkD3)
library(dplyr)

nodes<- fread('Fig3_Nodes.csv')
links<- fread('Fig3_Links.csv')
id<- which(links$value>1000)
links1<- links[id,]
p <- sankeyNetwork(Links = links1, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", fontSize = 14,
                   sinksRight=FALSE)
p

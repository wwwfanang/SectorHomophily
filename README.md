# Group_Homophily

This repository contains data and codes we used in support of our work 
"Group Homophily as a Measure of Self-Liking of Communities: Application in Vocational Networks.".

This code provides the dataset of mobility network structure, network community, and companies with GICS code. 

Please note that the data is located in the Data tag a the right side of the main Git repository. We use this method due to the file`s sizes. 

Data：
All data files are in "Releases -> Data". 
1. The file "Company_Community_Sector.csv" is the flows between any two companies and the community number of each company based on Louvain method. 
2. The file "Company_Sector_Community.csv" contains the cluster and GICS sector for each company.
3. Files "Fig3_Nodes.csv" and "Fig3_Links.csv" are the data used to create Fig3, the first is the nodes and the second is links used to create the image.

The rest of the R codes can correspond to the figures in the paper according to the file name.

If you have any questions, please contact me by wwwwfanang@gmail.com.

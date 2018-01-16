# ---------------------------------- 29th Oct ----------------------------------------------

install.packages("skmeans",dependencies=TRUE)
library(skmeans)

winedata<-read.csv("../day 12  ( 29nd Oct )/Wine.csv")
winedata  # same as Matrix data in csv

          # Note: this the MATRIX data sheet, just that blank cells have become NA
          # in excel, if we have NA it automatically converts it to 0 but R does not do it.

winedata[is.na(winedata)]<-0 # where there is NA, make that cell 0
winedata

# ?skmeans
# skmeans is same as KMedian which we did in Excel ( s- sperical )
# we want to partition the customers in this data and cutomers are in columns and not rows.
# note we have customers in column and not in rows. 
# so we have to transpose the customers ONLY ( not the entire data )so as to use skmeans.
# because skmeans works on rows

winedata.transposed<- t(winedata[,8:107])
head(winedata.transposed)

#clustering in R
winedata.clusters<-skmeans(winedata.transposed,5,method="genetic") # "genetic' same as "evolutionary in EXCEL SOLVER, 
                                                                   # 5 clusters
winedata.clusters                                                 #  skmeans does MEDIAN Clustering
                                                                   #skmeans works on NUMERIC data :note:
class(winedata.clusters)  # note this
winedata.clusters$cluster   # to check which customer has been given which cluster

#cluster of allen
winedata.clusters$cluster["Allen"] # it is evolutinary, so cluster might differ. NOTE
winedata.clusters$cluster["Adams"]
winedata.clusters$cluster["Bennett"]
winedata.clusters$cluster["Edwards"]




write.csv(winedata.clusters$cluster,"checkWineCluster.csv")  # see personal/Data Science/R folder , you will see this csv

aggregate(winedata.transposed,by=list(winedata.clusters$cluster),sum) # same as sumifs in excel - 5Med-topoffers sheet
             # try and do sapply and lapply here. NOte: aggregate is best here in this case


#But we have to transpose it
winedata.clustercounts<- t(aggregate(winedata.transposed,by=list(winedata.clusters$cluster),sum)[,2:33])
      # i dont need that header. so take evrything other than that header and hence [,2:33], 
      # and then transpose as above command

winedata.clustercounts


# but this lacks offer details. so..add those offer columns 
winedata.clusteroffers<- cbind(winedata[,1:7],winedata.clustercounts)
winedata.clusteroffers


# NOW I WANT TO STUDY it as i studied the excel ( all those sorting by 1,2,3,4,5 offer , largest to smaller)
# NOTE : --->the clustering algorithm is same in excel and R but the OPTIMIZATION technique is different


write.csv(winedata.clusteroffers[order(-winedata.clusteroffers[,8]),],"cluster1.csv") 
                     # descending order hence the -ve symbol, 
                     # 8th column ( cluster 1)to be sorted and write the files into a csv
                     # check personal/data science/R folder
                     

                 

# showing in GRAPHS------->

#install.packages("cluster",dependencies = TRUE)
library(cluster)

plot(silhouette(winedata.clusters)) 
    #note : avg silhouette value is 0.23.... there are 3 places where graph is left or negative so it is not good.

clusplot(winedata.transposed,winedata.clusters$cluster) # see this plot

#make this plot little different now
clusplot(winedata.transposed,winedata.clusters$cluster,labels=2) # labels are seen now

clusplot(winedata.transposed,winedata.clusters$cluster,color=TRUE,shade=TRUE,labels=2)

 


winedata.transposed
class(winedata.transposed)
windeKmeans<-kmeans(winedata.transposed,4)  #note : KMEANS 
windeKmeans




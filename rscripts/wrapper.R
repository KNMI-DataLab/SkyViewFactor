source("rscripts/allTilesComputation.R")

loginfo("preparing cluster")
cl<-prepareCluster()
loginfo("Cluster ready for use")
loginfo("Calling main function")
main()

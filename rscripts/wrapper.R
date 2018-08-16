source("rscripts/allTilesComputation.R")

loginfo("preparing cluster")
cl<-prepareCluster()
loginfo("Cluter ready for use")
loginfo("Calling main function")
main()

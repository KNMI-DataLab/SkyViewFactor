library(ncdf4)



original<-"/home/ubuntu/efs/output/SVF_1m_regionsNew/layer_tile10.nc"
new<-"/home/ubuntu/efs/output/SVF_1m_regionsNew/layer_tile10_kdcVers.nc"

ncFileOriginal<-nc_open(original)
ncFileNew<-nc_open(new)
oldSvf<-ncvar_get(ncFileOriginal,"layer")
newSvf<-ncvar_get(ncFileNew,"svf")

identical(oldSvf,newSvf)


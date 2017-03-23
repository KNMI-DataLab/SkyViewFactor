mergeNeighborTiles <- function(path,tileNumberXCoord, tileNumberYCoord, extensionMainTile, maxView,projection){
  
  #########################################################
  
  #########################################################
  #Load all the 8 neighboring tiles
  #########################################################
  LeftLower	<- loadNeighborTile_v2(xadd=-1000,yadd=-1000)
  Left		<- loadNeighborTile_v2(xadd=-1000)
  LeftUpper	<- loadNeighborTile_v2(xadd=-1000,yadd=1000)
  RightLower	<- loadNeighborTile_v2(xadd=1000,yadd=-1000)
  Right		<- loadNeighborTile_v2(xadd=1000)
  RightUpper	<- loadNeighborTile_v2(xadd=1000,yadd=-1000)
  CenterDown	<- loadNeighborTile_v2(yadd=-1000)
  CenterUp	<- loadNeighborTile_v2(yadd=1000) 
  #########################################################
  
  #########################################################  
  #Combine all the tiles and return result
  #########################################################
  dfs <- c(LeftLower,Left,
           LeftUpper,RightLower,
           Right,RightUpper,
           CenterDown,CenterUp)
  
  return(dfs)
  #########################################################
  rm(LeftLower,Left,LeftUpper,RightLower,Right,RightUpper,CenterDown,CenterUp)
  gc()
}
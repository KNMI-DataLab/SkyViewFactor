# ext.zoom<-as(raster::extent(matrix(c(xmin=5.17,ymin=52.12,xmax=5.19,ymax=52.13),nrow=2)),"SpatialPolygons")
# proj4string(ext.zoom)<-cfg$WGS84

svf.testing<-function (x, nAngles = 16, maxDist = 1000, ll = TRUE, filename = "", 
          blockSize = NULL, verbose = TRUE) 
{
  if (!nchar(filename)) 
    filename <- rasterTmpFile()
  if (is.null(blockSize)) 
    blockSize <- ceiling(nrow(x)/10)
  angles <- seq(0, 360 - 360/nAngles, 360/nAngles) * pi/180
  print(angles)
  sa <- terrain(x, opt = c("slope", "aspect"), unit = "radians")
  outImg <- raster(x)
  outImg <- writeStart(outImg, filename, overwrite = TRUE, 
                       dataType = "FLT4S")
  resY <- min(pointDistance(xyFromCell(x, cellFromRowCol(x, 
                                                         c(1, nrow(x)), c(1, ncol(x)))), xyFromCell(x, cellFromRowCol(x, 
                                                                                                                      c(2, nrow(x) - 1), c(1, ncol(x)))), lonlat = ll))
  if (verbose) {
    nSteps <- ceiling(nrow(x)/blockSize) * nAngles
    step.count <- 0
    step.message <- seq(10, 110, 5)
    cat("0%...")
  }
  for (firstRow in seq(1, nrow(x), blockSize)) {
    sl_as_row <- getValues(sa, firstRow, blockSize)
    svf_row <- numeric(nrow(sl_as_row)) + 0
    for (azimuth in angles) {
      if (verbose) {
        step.count <- step.count + 1
        step.frac <- 100 * step.count/nSteps
        if (step.frac > step.message[1]) {
          cat(step.message[1], "%...", sep = "")
          step.message <- step.message[-1]
        }
      }
      bufferSize <- ceiling(ceiling((maxDist/resY) * abs(cos(azimuth))) * 
                              1.25) * sign(cos(azimuth))
      dX <- sin(azimuth)/max(abs(c(sin(azimuth), cos(azimuth))))
      dY <- cos(azimuth)/max(abs(c(sin(azimuth), cos(azimuth))))
      print(paste0("dx=",dX,"dy=",dY))
      firstCell <- 1 + (firstRow - 1) * ncol(x)
      lastCell <- firstCell + blockSize * ncol(x) - 1
      if (lastCell > ncell(x)) 
        lastCell <- ncell(x)
      cells_row <- firstCell:lastCell
      elev_row <- getValues(x, firstRow, blockSize)
      firstRow_buffer <- max(1, min(firstRow, firstRow - 
                                      bufferSize))
      lastRow_buffer <- min(max(firstRow + blockSize - 
                                  1 - bufferSize, firstRow + blockSize - 1), nrow(x))
      firstCell_buffer <- 1 + (firstRow_buffer - 1) * ncol(x)
      lastCell_buffer <- lastRow_buffer * ncol(x)
      cells_buffer <- firstCell_buffer:lastCell_buffer
      elev_buffer <- getValues(x, firstRow_buffer, lastRow_buffer - 
                                 firstRow_buffer + 1)
      i = 1
      cells_d <- cellFromRowCol(x, rowFromCell(x, cells_row) - 
                                  round(i * dY), colFromCell(x, cells_row) + round(i * 
                                                                                     dX))
      valid <- validCell(x, cells_d)
      elev_d <- numeric(length = length(cells_d)) + NA
      elev_d[valid] <- elev_buffer[cells_d[valid] - firstCell_buffer + 
                                     1]
      dist_d <- pointDistance(xyFromCell(x, cells_row), 
                              xyFromCell(x, cells_d), lonlat = ll)
      meanDist <- mean(dist_d[valid], na.rm = TRUE)
      iHoriz <- atan((elev_d - elev_row)/dist_d)
      outHoriz_p <- numeric(length(cells_d)) + NA
      valid <- valid & !is.na(iHoriz)
      outHoriz_p[valid] <- iHoriz[valid]
      while ((meanDist < maxDist) & sum(valid) > 0) {
        i = i + 1
        cells_d <- cellFromRowCol(x, rowFromCell(x, cells_row) - 
                                    round(i * dY), colFromCell(x, cells_row) + 
                                    round(i * dX))
        valid <- validCell(x, cells_d)
        elev_d <- numeric(length = length(cells_d)) + 
          NA
        elev_d[valid] <- elev_buffer[cells_d[valid] - 
                                       firstCell_buffer + 1]
        dist_d <- pointDistance(xyFromCell(x, cells_row), 
                                xyFromCell(x, cells_d), lonlat = ll)
        meanDist <- mean(dist_d[valid], na.rm = TRUE)
        iHoriz <- atan((elev_d - elev_row)/dist_d)
        valid <- valid & !is.na(iHoriz) & (is.na(outHoriz_p) | 
                                             (!is.na(iHoriz) & !is.na(outHoriz_p) & iHoriz > 
                                                outHoriz_p))
        outHoriz_p[valid] <- iHoriz[valid]
      }
      svf_angle <- ((cos(sl_as_row[, 1]) * (cos(outHoriz_p)^2)) + 
                      sin(sl_as_row[, 1]) * cos(azimuth - sl_as_row[, 
                                                                    2]) * ((pi/2) - outHoriz_p - (sin(outHoriz_p) * 
                                                                                                    cos(outHoriz_p))))
      svf_row <- svf_row + svf_angle/nAngles
    }
    print(paste0("svf_row=",svf_row))
    outImg <- writeValues(outImg, svf_row, firstRow)
  }
  outImg <- writeStop(outImg)
  return(outImg)
}
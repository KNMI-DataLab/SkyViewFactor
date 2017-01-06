#!/bin/bash

#load the lastool
export PATH=/usr/people/dirksen/packages/LAStools/bin:$PATH

#folders
filesin='/run/media/dirksen/knmi/ahn2_clean/tileslaz/tile_0_3/'
filesout='/nobackup/users/dirksen/SVF_highres/SVF/test/LAS/'

#copy, unpack and delete
cd $filesin$AHN $filesout
echo $AHN
laszip $filesout*.laz
rm $filesout*.laz

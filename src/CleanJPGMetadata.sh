#!/bin/sh

exiftool -all= \
         -P \
         -overwrite_original \
         -tagsfromfile @ \
         -ICC_Profile \
         -EXIF:Artist \
         -EXIF:Copyright \
         -EXIF:UserComment \
         -EXIF:ExifImageWidth \
         -EXIF:ExifImageHeight \
         -EXIF:OwnerName \
         -EXIF:GPSVersionID \
         -EXIF:GPSLatitudeRef \
         -EXIF:GPSLatitude \
         -EXIF:GPSLongitudeRef \
         -EXIF:GPSLongitude \
         -XMP:Caption \
         -XMP:GPSLatitudeRef \
         -XMP:GPSLongitudeRef \
         -XMP:GPSMapDatum \
         -XMP:GPSVersionID \
         -XMP:GPSLongitude \
         -XMP:GPSLatitude \
         -XMP:GPSAltitudeRef \
         -XMP:GPSAltitude \
         -XMP:Artist \
         -XMP:CreatorWorkEmail \
         -XMP:CreatorWorkURL \
         -XMP:Copyright \
         -XMP:ImageDescription \
         -XMP:Creator \
         -XMP:Title \
         -XMP:Description \
         -XMP:Rights \
         $*

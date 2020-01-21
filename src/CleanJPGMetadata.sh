#!/bin/sh

exiftool -all= \
         -P \
         -overwrite_original \
         -tagsfromfile @ \
         -ICC_Profile \
         -EXIF:Artist \
         -EXIF:Copyright \
         -EXIF:GPSVersionID \
         -EXIF:GPSLatitudeRef \
         -EXIF:GPSLatitude \
         -EXIF:GPSLongitudeRef \
         -EXIF:GPSLongitude \
         -EXIF:ImageDescription \
         -XMP:Creator \
         -XMP:Title \
         -XMP:Description \
         -XMP:Rights \
         -XMP:UsageTerms \
         -XMP:CreatorWorkEmail \
         -XMP:CreatorWorkURL \
         -IPTC:CopyrightNotice \
         $*

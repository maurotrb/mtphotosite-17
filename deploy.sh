#!/bin/bash

BUCKET_NAME=www.maurotaraborelliphoto.com

rm -rf public
hugo -v
aws s3 sync public/ s3://$BUCKET_NAME/ --acl public-read --storage-class REDUCED_REDUNDANCY --delete

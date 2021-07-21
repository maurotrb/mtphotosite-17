#========================
# Web Site Build Makefile
#========================

css-prereq      = ./src/sass/mtphotosite.sass
# Dart Saas
css-proc        = sass
css-target      = ./static/css/mtphotosite.min.css
public-dir      = ./public
img-local-dir   = ~/lib/photography/derivatives/mauro/web/maurotaraborelliphoto.com
pix-size-csv    = ./src/images-pixel-size.csv
metadata-sql    = ./src/ExtractMetadataDigikam.sql
metadata-csv    = ./src/images-digikam-metadata.csv
digikam-db      = ~/lib/photography/catalogues/digikam/digikam4.db
metadata-clean  = ./src/CleanJPGMetadata.sh
photo-pages-dir = ./content/photos
photo-pages-csv = ./src/PhotoPagesFromCSV.hs
s3-bucket       = www.maurotaraborelliphoto.com
site-proc       = hugo -v

all: site

$(css-target): $(css-prereq)
	@echo -e "Creating minified css file...\t\t\c"
	@$(css-proc) -s compressed $(css-prereq):$(css-target)
	@echo -e "[ Done ]"

site: $(css-target)
	@echo -e "Removing public dir...\t\t\t\c"
	@rm -rf $(public-dir)
	@echo -e "[ Done ]"
	@echo -e "Creating site..."
	@$(site-proc)
	@echo -e "Site created."

deploy: site
	@echo -e "Deploying site to S3...\t\t\c"
	@aws s3 sync public/ s3://$(s3-bucket)/ --profile maurotaraborelliphotosite --acl public-read --storage-class REDUCED_REDUNDANCY --delete
	@echo -e "[Done]"

clean:
	@echo -e "Removing public dir...\t\t\t\c"
	@rm -rf $(public-dir)
	@echo -e "[ Done ]"
	@echo -e "Removing generated css file...\t\t\t\c"
	@rm -f $(css-target)*
	@echo -e "[ Done ]"

cleanjpgmetadata:
	@echo -e "Clean JPG metadata...\t\t\t\c"
	@$(metadata-clean) $(img-local-dir)/*.jpg
	@echo -e "[ Done ]"

imgpixsize:
	@echo -e "Extracting pixel size from published images...\t\c"
	@echo -e "\"image_th\",\"image_width\",\"image_height\"" > $(pix-size-csv)
	@identify -format "\"%t\",\"%w\",\"%h\"\n" $(img-local-dir)/*.jpg >> $(pix-size-csv)
	@echo -e "[ Done ]"

imgmetadata:
	@echo -e "Extracting metadata from Digikam catalog...\t\c"
	@sqlite3 -header -csv $(digikam-db) < $(metadata-sql) > $(metadata-csv)
	@echo -e "[ Done ]"

updatephotopages: imgpixsize imgmetadata
	@echo -e "Removing photo page files...\t\t\t\c"
	@rm -rf $(photo-pages-dir)/*.md
	@echo -e "[ Done ]"
	@echo -e "Creating photo page files...\t\t\t\c"
	@$(photo-pages-csv)
	@echo -e "[ Done ]"

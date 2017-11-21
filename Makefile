#========================
# Web Site Build Makefile
#========================

css-prereq = ./src/sass/mtphotosite.sass
css-proc   = sass
css-target = ./static/css/mtphotosite.min.css
public-dir = ./public
s3-bucket  = www.maurotaraborelliphoto.com
site-proc  = hugo -v

all: site

$(css-target): $(css-prereq)
	@echo "Creating minified css file...\t\t\c"
	@$(css-proc) -t compressed $(css-prereq):$(css-target)
	@echo "[ Done ]"

site: $(css-target)
	@echo "Removing public dir...\t\t\t\c"
	@rm -rf $(public-dir)
	@echo "[ Done ]"
	@echo "Creating site..."
	@$(site-proc)
	@echo "Site created."

deploy: site
	@echo "Deploying site to S3...\t\t\c"
	@aws s3 sync public/ s3://$(s3-bucket)/ --profile maurotaraborelliphotosite --acl public-read --storage-class REDUCED_REDUNDANCY --delete
	@echo "[Done]"

clean:
	@echo "Removing public dir...\t\t\t\c"
	@rm -rf $(public-dir)
	@echo "[ Done ]"
	@echo "Removing generated css file...\t\t\c"
	@rm -f $(css-target)*
	@echo "[ Done ]"

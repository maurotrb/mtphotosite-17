Mauro Taraborelli Photography Site
==================================

What is it?
-----------
The source of my photographic portfolio site: [www.maurotaraborelliphoto.com](http://www.maurotaraborelliphoto.com/).

This is a static site built with [HUGO](https://gohugo.io/), using the CSS framework
[Bulma](http://bulma.io/), and hosted on [AWS S3](https://aws.amazon.com/s3/).

Documentation
-------------

### Prerequisites

- Build the site requires [HUGO](https://gohugo.io/)
- Customize the CSS requires [Sass](http://sass-lang.com/)
- Update the photo pages (see below) requires [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- Deploy the site to [AWS S3](https://aws.amazon.com/s3/) requires the [AWS Command Line Interface](https://aws.amazon.com/cli/)

### Photo pages

[Photo pages](/content/photos/) are not edited manually, but they are created from
the photo metadata contained in two CSV file, [ImageLightroomMetadata.csv](src/ImageLightroomMetadata.csv),
and [ImagePixelSize.csv](src/ImagePixelSize.csv), using the Haskell script [PhotoPagesFromCSV.hs](src/PhotoPagesFromCSV.hs).

Running the [update_photo_pages.sh](update_photo_pages.sh) bash script will delete
the old photo pages and will create the new ones with the Haskell script.

Pages for galleries and photo tags are created by HUGO thanks to Taxonomies support.

Licensing
---------
Please see the file called LICENSE.

Reference
---------

- [HUGO: A Fast & Modern Static Website Engine](https://gohugo.io/)
- [Bulma: A modern CSS framework based on Flexbox](http://bulma.io/)
- [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

Contacts
--------
For question and comments:

- [MauroTaraborelli@gmail.com](mailto:MauroTaraborelli@gmail.com)

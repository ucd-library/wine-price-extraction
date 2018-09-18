# wine-price-extraction

This repository relates to Template, and Machine Extraction of Wine Prices from Sherry Lehmann Catalogs.

## Connecting to the database

We have created a read-only public access postgres dataset.  You can connect via:

```bash
psql postgres://datafest@35.230.56.243/datafest -P
```

The password is available from our team.  Using this, make sure to add the catalogs schema to your search_path

``` sql
set search_path=catalogs,public;
```


## QGIS

If you are interested in using QGIS to view the data, there is an Example.qgs file.  This connects to the postgres database for
the OCR, and overlays the data on the original images. For rotated extraction, the tesseract Bounding boxes are reprojected back to the original image, so we can compare the two setups more easily.

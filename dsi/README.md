The folder contains code produced by the DSI (mostly Jane Carlen and Stanislaw Saganowski, building on work by Duncan Temple Lang) with the goal of extracting prices tables from the Sherry Lehmann wine catalogs. 

Organization:

- The **R** folder contains functions. Currently the hierarchy is that the main function to extract price tables from an image is in wine_price_tables.R, which is composed mainly of functions in wine_price_\*.R scripts, and those variously call the shorter one-task functions in helper.R. Scripts with truth or evaluate in the name are related to creating or organizing truth data or evaluating output of the table extraction code. 

- The **scripts** folder contains scripts to run price table extraction on a folder (currently the images with price tables from Duncan's original (random?) sample of 100 images, SampleCatalogPages) or one image file. Subsequent run_\*.R scripts parse the names found and turn results into output tables for our database. See the **wine price table workflow.pdf** for an overview of the process and how these scripts work together.
 
Notes on running scripts:

- You can run scripts like **run_wine_price_tables.R** from the command line. Examples:

Example 1 to run only OCR on a single file:
```
Rscript run_wine_price_tables.R FILESET=OCR_SherryLehmann/Sample/Sample1/UCD_Lehmann_0015.jpg . DATA.OUTPUT.DIR=Data/sample_output OCR.ONLY=TRUE
```
Example 2 to run a whole folder, supplying data from previous 
```
Rscript run_wine_price_tables.R FILESET=OCR_SherryLehmann/Sample/Sample1 OUTPUT.DIR=Data/sample_output DATA.INPUT.DIR=Data/sample_output
```
Possible arguments for **run_wine_price_tables.R**:
Seperate name from value with = sign and no spaces, as shown above.
Entry order doesn't matter.

- FILESET: Required argument. A path to a specifiic image or a folder containing images.
- OCR.ONLY = TRUE or FALSE. If true only create OCR-output data objects (no price table extraction).
- OUTPUT.DIR: Directory where to store price table extraction objects. Needed if OCR.ONLY is false.
- DATA.OUTPUT.DIR: Optional directory to store data objects. (If not given data is not stored.)
- DATA.INPUT.DIR: Optional directory where to look for pre-run data objects (to avoid re-OCRing when possible).
- SAVE.DESKEWED = TRUE or FALSE. Save the deskewed image (usually not needed)? Defaults to false.
- PIX.THRESHOLD = Optional numeric in 1-255 to override the default.
- BINARY.THRESHOLD = Optional numeric in 1-255 to override the default.

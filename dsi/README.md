The folder contains code produced by the DSI (mostly Jane Carlen and Stanislaw Saganowski, building on work by Duncan Temple Lang) with the goal of extracting prices tables from the Sherry Lehmann wine catalogs. 

Organization:

- The *R* folder contains functions. Currently the hierarchy is that the main function is in wine_price_tables.R, which is composed mainly of functions in wine_price_\*.R scripts, and those variously call the shorter one-task functions in helper.R. Scripts with truth or evaluate in the name are related to creating or organizing truth data or evaluating output of the price_tables functions. 

- The **scripts** folder contains scripts to run price table extraction on a folder (currently the images with price tables from Duncan's original (random?) sample of 100 images, SampleCatalogPages) or one image file. Subsequent run_\*.R scripts parse the names found and turn results into output tables for our database. See the **wine price table workflow.pdf** for an overview of the process and how these scripts work together.
 

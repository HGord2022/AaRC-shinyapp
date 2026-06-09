# metAaRCive-shinyapp

This is the repo for the R shiny app for exploring the [metAaRCive](https://github.com/AaRC-Animal-aDNA-Research-Community/metAaRCive) database, a resource of metadata for published ancient animal genomes curated by members of [AaRC](https://animal-adna.org/).

The app is hosted here: https://aarc.shinyapps.io/metaarcive/

## Repository structure and files

- `metAaRCive/`
  - `rsconnect/` - Deployment configuration for the Shiny app
  - `www/` - Static assets used by the app, such as images
  - `app.R` - Main R Shiny application script
  - `get_df.R` - Downloads and filters the latest version of the database from the metAaRCive [repo](https://github.com/AaRC-Animal-aDNA-Research-Community/metAaRCive)
  - `aarc_metadata.csv` - Generated data file used by the app

# NIST FreezerCheck {NISTfreezercheck}

Quality assurance and quality control (QA/QC) policies are important for any repository. The National Institute of Standards and Technology hosts the [NIST Biorepository](https://www.nist.gov/programs-projects/marine-environmental-specimen-bank), located in Charleston, SC, USA, housing more than 118,500 samples (as of 21 March 2019) at cryogenic temperatures for preservation and archival. Matrices from sediment to rare marine mammal tissues are available for a wide variety of biochemical analysis. Such samples are frequently added, making for a dynamic repository with increasing use cases as NIST expands its Biorepository Science research. One important aspect of QA/QC for large collections of physical artifacts is to ensure the highest quality of physical location records for each and every sample. Placing semi-automated or curated data tools into daily workflows saves far more resources (both time and money) than that spent developing the tools. Toward that end, a custom QA/QC tool was created as part of an effort to formalize the process of position audits and improve quality protocols. We've dubbed this particular tool "NIST FreezerCheck" and hope it can start a larger conversation about the potentials of custom-driven software to solve real world challenges among sample repositories.

NIST FreezerCheck seeks to streamline the process of achieving adequate coverage of position audits (seeking 10% annual coverage) in as time-conscious a manner as possible. Though this is tailored to the NIST Biorepository, techniques employed to minimize effort while maximizing focused coverage may be of use to other repositories. For this reason, the tool was wrapped in an R package to foster communication and offer assistance to other biorepositories globally.W e chose the [Shiny](https://shiny.rstudio.com/) platform running on R for its modularity, rapid prototyping, maximum flexibility, and ease of use. Though it runs on R under the hood, the entire interface is implemented as a web application via Shiny and can be launched with a single console command. All internal functions are exposed to enable customization in other repositories. While the NIST Biorepository uses a commercial database product to track sample position and metadata which is heavily leveraged here, NIST FreezerCheck can be easily adapted to any SQL-based database. Every attempt has been made to make this tool as easy to use as possible. This is only one of several data tools under active development at NIST.

## Features

- direct connection to a database containing sample position and metadata
- full featured demonstration mode for evaluation purposes
- supports multiple instance installation for multi-faceted repositories
- audit effort reduction and focus algorithms to achieve desired audit coverage
- multiple audit style properties to support changing goals and protocols
- barcode scanning (depends on scanner hardware availability)
- allows recording of discrepancies for later investigation and resolution
- allows immediate resolution of discrepancies (does not push back to your database)
- aggregate metrics of repository position accuracy
- ongoing history of all audit activities
- records the time necessary to complete each audit (internal only)
- export of audit discrepancies to transfer data to other systems
- dynamically pull additional sample metadata without interrupting an audit
- pause an audit and continue at a later time
- ...and others.

### Running NIST FreezerCheck
Install from github with `devtools::install_github("jmr-nist-gov/NISTfreezercheck")`.  
After package installation, complete installation of NIST FreezerCheck using `library(NISTfreezercheck)` and follow the prompts.  
Run the demo with `freezercheck_demo()`.

### Notes
1. FreezerCheck will ensure appropriate connections and data structures exist upon being called with `library()`.
2. After installing from github, you may need to restart R in order to be able to call `library(NISTfreezercheck)`.
3. It is suggested that you detach all packages prior to running FreezerCheck.
4. For this version, objects powering the app (including paths to the chosen installation) are retained in the global environment when the app is closed for easy manipulation. As always, if any changes are made, write them back to the user directory as appropriate.
5. Reinstall from scratch or add another repository at any time using `freezercheck_setup()`, see `?freezercheck_setup` for options.
6. For this version, closing the browser tab does not close the application. FreezerCheck remains running in R (and available as a new session )

### EXPERIMENTAL
If your repository does not use a formal database, it is theoretically possible to replace `demo_data.RDS` in the package `data/demo` directory with your repository's position records. The largest caveat to doing this is to ensure necessary headers contain the appropriate information. 

| Header | Description | 
| --- | --- | 
| GUALIQUOTID | Unique identifier for a given aliquot | 
| FREEZERPHYSNAME | Unique identifier for a physical freezer | 
| FREEZERNAME | (Do not delete) Can be left blank - name of a section within FREEZERPHYSNAME | 
| POSITION1 | Top level position identifier, typically a rack or tube | 
| POSITION2 | Second level position identifier, typically a box or position | 
| POSITION3 | Third level position identifier, typically a position | 

If this is not done appropriately, FreezerCheck may not work or may behave in strange ways. All other metadata columns are optional and should be tailored to your collection. 

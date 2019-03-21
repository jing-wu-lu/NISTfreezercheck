# NIST FreezerCheck {NISTfreezercheck}

Quality assurance and quality control (QA/QC) policies are important for any repository. The National Institute of Standards and Technology hosts the [NIST Biorepository](https://www.nist.gov/programs-projects/marine-environmental-specimen-bank), located in Charleston, SC, USA, housing more than 118,500 samples (as of 21 March 2019) at cryogenic temperatures for preservation and archival. Matrices from sediment to rare marine mammal tissues are available for a wide variety of biochemical analysis. Such samples are frequently added, making for a dynamic repository with increasing use cases as NIST expands its Biorepository Science research. One important aspect of QA/QC for large collections of physical artifacts is to ensure the highest quality of physical location records for each and every sample. Placing semi-automated or curated data tools into daily workflows saves far more resources (both time and money) than that spent developing the tools. Toward that end, a custom QA/QC tool was created as part of an effort to formalize the process of position audits and improve quality protocols. We've dubbed this particular tool "NIST FreezerCheck" and hope it can start a larger conversation about the potentials of custom-driven software to solve real world challenges among sample repositories.

NIST FreezerCheck seeks to streamline the process of achieving adequate coverage of position audits (seeking 10% annual coverage) in as time-conscious a manner as possible. Though this is tailored to the NIST Biorepository, techniques employed to minimize effort while maximizing focused coverage may be of use to other repositories. For this reason, the tool was wrapped in an R package to foster communication and offer assistance to other biorepositories globally.W e chose the [Shiny](https://shiny.rstudio.com/) platform running on R for its modularity, rapid prototyping, maximum flexibility, and ease of use. Though it runs on R under the hood, the entire interface is implemented as a web application via Shiny and can be launched with a single console command. All internal functions are exposed to enable customization in other repositories. While the NIST Biorepository uses a commercial database product to track sample position and metadata which is heavily leveraged here, NIST FreezerCheck can be easily adapted to any SQL-based database. Every attempt has been made to make this tool as easy to use as possible. This is only one of several data tools under active development at NIST.

### Features

- Direct connection to a database containing sample position and metadata.
- Full featured demonstration mode for evaluation purposes.
- Supports multiple instance installation for multi-faceted repositories.
- Audit effort reduction and focus algorithms to achieve desired audit coverage.
- Multiple audit-style properties to support changing goals and protocols.
- Barcode scanning (depends on scanner availability).
- Allows recording of discrepancies for later investigation and resolution.
- Allows immediate resolution of discrepancies.
- Aggregate metrics of repository position accuracy.
- Ongoing history of all audit activities.
- Records the time necessary to complete an audit.
- Export of audit discrepancies to transfer data to other systems.
- Dynamically pull sample metadata without interrupting an audit.
- Pause an audit and continue at a later time.
- ...and others.

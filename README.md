
<!-- README.md is generated from README.Rmd. Please edit that file -->

# built_environment_ce

This repository contains the data and code for the article:

> [Lanfear, Charles C. (2022). Collective Efficacy and the Built Environment. *Criminology*, 60(2)](https://doi.org/10.1111/1745-9125.12304)

### How to cite

Please cite this compendium as:

> Lanfear, Charles C. (2022). *Repository of R code and data for
> ‘Collective Efficacy the Built Environment’*. Accessed 07 Feb 2022.

### How to download

You can download the compendium as a zip from from this URL:
[/archive/master.zip](https://github.com/clanfear/built_environment_ce/archive/master.zip)

### Structure:

-   `data`
    -   `raw` contains non-access restricted raw replication data (only
        1990 IL block data)
    -   `derived` is empty but populated when scripts run. If your data
        use agreement requires temp files to be in a specific secure
        directory, make sure to change paths or put entire project in
        the secure directory.
    -   `analytical_data` contains the deidentified block-level
        replication data. This is sufficient to re-run models and
        reproduce tables and plots.
-   `docs` contains the files to replicate the present article draft.
-   `syntax` contains all necessary code to fully replicate the paper
    from raw data. Two files are important:
    -   `00_build_article.R` replicates the entire project and documents
        the files necessary to do this.
    -   `file_path_index.R` specifies the file paths to all relevant
        files (i.e., to secure data drives and folders).
-   `output` is empty but populated by running the replication scripts.

### Licenses

**Text and figures :**
[CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)

**Code :** [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/)

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse.

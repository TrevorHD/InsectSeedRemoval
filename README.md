# Overview

Ants and other insects are often a source of localized secondary dispersal for wind-dispersed plants and thus play an important ecological role in their spatial dynamics, but there is limited information on how climate change will affect such dispersal processes. Here, we use field experiments to investigate how climate warming affects seed removal, as this initiation of movement represents the first step in insect-driven secondary dispersal. Our results indicate that for the invasive thistles *Carduus nutans* and *Carduus acanthoides*, increased growing temperature influences seed attractiveness to insect dispersers, with seeds from maternal plants grown at temperatures 0.6°C above ambient removed by insect dispersers at higher rates than their unwarmed counterparts. We also observe that seed elaiosomes in these two species play an important role in dispersal, as seeds without elaiosomes were significantly less likely to be removed over the same time period. Significant interactions between elaiosome presence/absence and warming treatment were also observed, though only for *C. acanthoides*, with the boost in seed removal from warming dampened when the elaiosome was present compared to when it was absent. These findings provide evidence that climate warming may alter aspects of dispersal such as seed removal by secondary dispersers, with potential ramifications for dispersal in future climates since seed-bearing plants around the world may be subject to increased growing temperatures, and many of these plant species bear elaiosomes and experience seed dispersal by insects.

<br/>

# Files

## Data

**SeedRemovalData** *(.csv)* - Seed removal data, formatted as a time series. The number of remaining seeds are counted every 30 minutes for the first 12 hours, and then every 12 hours after that until 48 hours have elapsed. Each row represents one of ten replicates for each unique combination of three binary variables: warmed/unwarmed parent plants, elaiosome present/absent, and species *Carduus* *nutans*/*acanthoides*; there are thus a total of 80 replicates.

**SeedMassData** *(.csv)* - Seed mass data, with each entry representing the mass (in grams) of a group of 20 seeds.

## Scripts

**01_Setup** *(.R)* - Code used to load in data and define functions for analysis and plotting.

**02_Stats** *(.R)* - Code used for various statistical analyses on the data.

**03_Plots** *(.R)* - Code used for plotting figures.

**S1_Extras** *(.R)* - Supplementary code not used in the main analyses.

## Other

**AntSeedDispersalMS_v8_Ecology** *(.docx)* - Latest version of the manuscript for this research, submitted to *Ecology*.

**AntSeedDispersalMS_v8_Ecology_Appendix_S1** *(.docx)* - Supplemental matierial containing parametric survival model methods and results, as well as qualitative comparisons to GLMs for seed removal.

**AntSeedDispersalMS_v8_Ecology_Appendix_S1** *(.docx)* - Supplemental matierial containing photographs of frequently observed seed-removing insects, and additional figures for both model results and observed seed removal patterns.

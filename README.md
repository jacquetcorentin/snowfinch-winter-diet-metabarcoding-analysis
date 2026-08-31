# Snowfinch winter diet metabarcoding analysis

This analysis was conducted as part of a study on the winter diet of the White-winged Snowfinch using fecal metabarcoding.
This repository contains the code, data structure, and figures used for the metabarcoding-based diet analysis project.

## Repository structure

```
project/
├── src/                # R scripts for cleaning, analysis, plotting
├── data/               # CSV and TSV files
│   ├── data/
│   └── data-instructions/  # Instructions for downloading large datasets
└──figures/            # Plots and thesis figures
```

## Large Data Files (Important)

This project uses a very large reference dataset (~300 GB) containing species present in Europe.  
Because of its size, **it cannot be stored on GitHub**.

## GBIF filters required before downloading

To reproduce the European plant dataset used in this project, apply the following filters on GBIF:

- **Continent:** Europe  
- **Elevation:** 1200–9999 m  
- **Taxon:** Plantae  
- **Occurrence status:** Present  
- **IUCN category:** EN, VU, NT, LC, DD, NE  
- **Year:** 2015–2026  

These filters correspond to the JSON query:

{
  "and" : [
    "Continent is Europe",
    "Elevation 1200.0-9999.0m",
    "IucnRedListCategory is one of (EN, VU, NT, LC, DD, NE)",
    "OccurrenceStatus is Present",
    "TaxonKey is Plantae",
    "Year 2015-2026"
  ]
}

After applying these filters, download the GBIF occurrence dataset (the ZIP archive containing `occurrence.txt`).

You can download the dataset from:

**Download link:** <https://www.gbif.org/fr/occurrence/search?occurrenceStatus=PRESENT&q=&view=download>

## Interactive Krona plots

- [Krona plot (RRA)](https://jacquetcorentin.github.io/snowfinch-winter-diet-metabarcoding-analysis/figures/krona_RRA.html)
- [Krona plot (FOO)](https://jacquetcorentin.github.io/snowfinch-winter-diet-metabarcoding-analysis/figures/krona_FOO.html)
  
## How to Run the Analysis

1. Install required R packages (see script headers)
2. Open the script you want to run (e.g., `src/alluvialplot.R`).
3. Run the script using the “Run” button.

## Requirements

- R (version 4.x or later)
- RStudio (recommended)
- Required packages listed at the top of each script

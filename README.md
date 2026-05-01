# Baseline Reef Survey Data: Bocas del Toro, Panama

This repository contains the data and analysis code for the baseline coral reef monitoring surveys in Solarte, Bocas del Toro, Panama (2023–2025).

## Structure

-   `data/`: Raw survey data (`raw_data.xlsx`)
-   `scripts/`: R scripts for data cleaning, analysis, and visualization
-   `output/`: Generated plots and summary tables
-   `renv/`, `renv.lock`: Reproducible R environment setup

## Usage

1.  Clone the repository
2.  Open `baseline_analysis.Rproj` in RStudio
3.  Run `renv::restore()` to install the required packages
4.  Run analysis scripts in `scripts/`

This script requires Google Maps. - Go to [Google Cloud Console](https://console.cloud.google.com/), create a project, and enable the Maps Static API. - Generate an API key. - Add your API key to your R session before running any script that uses `ggmap`:

```         
register_google(key = "YOUR_API_KEY")
```

For convenience, you may store your API key in a local `.Renviron` [file:\\](file:\){.uri}

```         
GOOGLE_MAPS_API_KEY=your_key_here
```

Then in your script, use:\
`r  register_google(key = Sys.getenv("GOOGLE_MAPS_API_KEY"))`

**Notes:**\
- All data required for analysis is provided in the `data/` folder (raw_data.xlsx).\
- If you encounter missing package errors, ensure you have run `renv::restore()` in the R console. 
- For full reproducibility, see `sessionInfo()` at the end of main scripts.

## Data

Survey data are provided under a CC-BY 4.0 license.\
See the `data/` folder for the raw Excel file.

The dataset associated with this project is available on Zenodo: <https://doi.org/10.5281/zenodo.17048193>

Please cite as: Paula Sills. (2025). *Two Years of Reef Survey Data from Bocas del Toro, Panama: Baseline for Coral Reef Monitoring* [Data set]. Zenodo. <https://doi.org/10.5281/zenodo.17048193>

## Contact

For questions, contact Paula Sills ([pau\@ankayconservation.com](mailto:pau@ankayconservation.com){.email}) or Rosie Young ([rosie.young\@merton.ox.ac.uk](mailto:rosie.young@merton.ox.ac.uk){.email})

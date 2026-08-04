# Investment Decision-Making Task (Research Prototype)

An interactive R Shiny application designed to study financial decision-making in an experimental setting. This prototype simulates a modern investing interface to elicit and log participants’ asset exploration and trading behavior for research purposes.

## Overview

This project implements a web-based investment task used in behavioral and consumer research. Participants are presented with a set of assets, historical price information, and a simulated cash balance. They can explore assets, view summary statistics and analyst-style ratings, and place hypothetical trades. All interactions are logged for downstream analysis of decision processes and information search patterns.

The interface is intentionally stylized to resemble contemporary retail investing platforms while remaining fully controlled for experimental use.

## Key Features

- **Simulated investing environment** with a fixed initial cash balance and a curated set of assets.
- **Asset exploration** via a list view with key metrics (e.g., price, YTD performance).
- **Detailed asset views** including:
  - Interactive price charts with multiple time horizons (1W–ALL).
  - Summary statistics (e.g., 52-week high/low, volatility).
  - Aggregated analyst-style rating distributions.
- **Watchlist and holdings panels** to track selected and owned assets.
- **Order entry and review flow** with validation (e.g., insufficient funds).
- **Comprehensive interaction logging**:
  - Page views, button clicks, chart interactions.
  - Order submissions with shares, price, and total cost.
  - Session identifiers for linkage to survey platforms (e.g., Prolific).

## Research Use Case

This app is intended for:

- Studying how individuals search for and use financial information.
- Analyzing portfolio construction and trading behavior under controlled conditions.
- Capturing process data (clickstreams, time-on-task) in addition to outcome data (holdings, returns).

Data collection is fully local: interaction logs are saved as CSV files per session. The app can be embedded in online experiments by passing participant and study identifiers via URL query parameters.

## Tech Stack

- **R** (primary language)
- **Shiny** (web application framework)
- **shinyjs**, **shinyWidgets** (UI interactivity)
- **ggplot2**, **plotly** (data visualization)
- **dplyr**, **purrr**, **stringr**, **lubridate** (data wrangling)
- **qs** (fast serialization of preprocessed data)

## Project Structure

```text
.
├── app.R                  # Main Shiny application (UI + server)
├── R/
│   └── config.R           # Configuration and global settings
├── data/
│   ├── SP500.qs           # Preprocessed price and fundamentals data
│   └── ratings.qs         # Preprocessed analyst-style ratings data
└── logs/                  # Interaction logs (generated at runtime)
```

> **Note:** The `data/` directory contains preprocessed, research-specific datasets and is not included in this repository.

## Getting Started

### Prerequisites

- R (recent version recommended)
- R packages listed in the **Tech Stack** section

### Installation

1. Clone the repository:

   ```bash
   git clone <repository-url>
   cd <repository-name>
   ```

2. Install required R packages:

   ```r
   install.packages(c(
     "shiny", "shinyjs", "shinyWidgets",
     "ggplot2", "plotly",
     "dplyr", "purrr", "stringr", "lubridate",
     "qs", "readr"
   ))
   ```

3. Prepare the `data/` directory with appropriately formatted `.qs` files (not provided).

4. Run the app:

   ```r
   shiny::runApp()
   ```

## Configuration

Global settings (e.g., initial balance, number of assets, time horizon defaults, color scheme) are defined at the top of `app.R` and in `R/config.R`. Adjust these to match your experimental design.

### URL Parameters

The app supports optional query parameters for experiment integration:

- `PROLIFIC_PID`
- `STUDY_ID`
- `SESSION_ID`

When present, these identifiers are logged with each interaction and appended to the exit URL to redirect participants back to the survey platform.

## Data and Logging

Interaction data are stored as CSV files in the `logs/` directory, with filenames encoding participant and session identifiers plus a timestamp. Each row corresponds to a single interaction (e.g., button click, order submission) and includes:

- Participant/study/session IDs
- Timestamp
- Interaction type and label
- Context (page, asset)
- Order details (when applicable)

These logs can be merged with survey responses and outcome measures for analysis.

## Ethics and Data Privacy

This prototype is intended for research use under appropriate ethical oversight. Ensure compliance with your institution’s policies regarding:

- Informed consent and participant information.
- Data storage, retention, and anonymization.
- Secure handling of any personally identifiable information.

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

## Acknowledgements

This project was developed as part of ongoing work on consumer financial decision-making and experimental methods in behavioral research.
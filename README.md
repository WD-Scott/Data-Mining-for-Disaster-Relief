<p align="center">
  <img src="images/logo.png" alt="Disaster Relief Project Logo" width="400"/>
</p>

<p align="center">
  <img src="https://img.shields.io/badge/Status-Final_Version-blue?style=flat" alt="Repo Status: Final Version"/>
  <a href="https://opensource.org/licenses/MIT"><img src="https://img.shields.io/badge/License-MIT-yellow.svg" alt="License: MIT"/></a>
  <img src="https://img.shields.io/badge/Language-R-276DC3?logo=r" alt="Language: R"/>
</p>

---

## Overview

Classification models trained on satellite imagery to locate displaced persons sheltering under blue tarps after the 2010 Haiti earthquake. Seven algorithms — Logistic Regression, LDA, QDA, KNN, Penalized Logistic Regression (Elastic Net), Random Forest, and SVM — are tuned via 10-fold cross-validation and evaluated on a 2M-observation holdout set.

**Key result:** Penalized Logistic Regression delivered the best holdout performance (highest AUC, Kappa, and F1; second-lowest FPR) while also being the fastest model to deploy.

> Final project for the University of Virginia's DS6021 (Statistical Learning) course.

---

## Repository Structure

```
Data-Mining-for-Disaster-Relief/
├── data/
│   └── .gitkeep
├── images/               # Figures and logo used in the README and report
├── scripts/
│   ├── utils.R           # Reusable helper functions (plotting, tables, evaluation)
│   └── analysis.Rmd      # Main analysis notebook (knit to produce the report)
├── Report.pdf            # Final rendered report
├── .gitignore
├── LICENSE
└── README.md
```

---

## Key Results

| Model | AUC (Holdout) | FPR (Holdout) | F1 (Holdout) | Deploy Time |
|-------|:---:|:---:|:---:|:---:|
| **PLR (recommended)** | **Highest** | **2nd-lowest** | **Highest** | **Fastest** |
| Logistic | Close 2nd | Lowest | Close 2nd | Fast |
| LDA | Close 3rd | Low | Close 3rd | Fast |
| SVM (RBF) | Best in CV | Low | Good | ~5× slower |

The SVM performed best during cross-validation but the PLR generalized better to the holdout set, likely due to the Elastic Net penalty mitigating multicollinearity among the highly correlated RGB predictors (pairwise correlations ≥ 0.94).

---

## Data

The training set (`HaitiPixels.csv`) contains 63,241 pixel observations with columns `Class`, `Red`, `Green`, and `Blue`. The holdout set (~2M observations) is provided as multiple `.txt` files.

**To obtain the data:** This dataset was provided through UVA's DS6021 course. Place `HaitiPixels.csv` and the holdout `.txt` files in `data/`.

---

## How to Reproduce

### Prerequisites

R ≥ 4.1 (uses the native `|>` pipe) with the following packages:

```r
# Core
install.packages(c(
  "data.table", "ggplot2", "caret", "ROCR",
  "gridExtra", "kableExtra", "here",
  "parallel", "doParallel"
))

# Model-specific (caret dispatches to these)
install.packages(c(
  "glmnet", "randomForest", "e1071", "kernlab", "MASS"
))

# Optional (3D scatter plot in EDA only)
install.packages("plotly")
```

Alternatively, restore the environment from the lockfile if using `renv`:

```r
renv::restore()
```

### Run the Analysis

1. Clone the repo:
   ```bash
   git clone https://github.com/WD-Scott/Data-Mining-for-Disaster-Relief.git
   cd Data-Mining-for-Disaster-Relief
   ```
2. Place the data files in `data/` as described above.
3. Open and knit `scripts/analysis.Rmd`, or source the helpers and run interactively:
   ```r
   source("scripts/utils.R")
   ```

---

## Methodology at a Glance

1. **Data wrangling** — Binary target (`Tarp = Yes/No`) created from the multi-class `Class` variable; holdout text files cleaned and combined.
2. **EDA** — Density plots and 3D RGB scatterplots confirm class separability in the blue channel; severe class imbalance (3.2% positive in training, 0.7% in holdout).
3. **Model training** — 10-fold CV with AUC as the optimization metric; threshold selection balances FPR, Balanced Accuracy, Kappa, and F1.
4. **Holdout evaluation** — All models deployed on the 2M-observation holdout; confusion matrices, ROC curves, and timing benchmarks compared.

---

## Limitations & Future Directions

- **Color space:** RGB features exhibit high multicollinearity. Transforming to HSL or CIELUV could decouple intensity from chromaticity and improve discrimination.
- **Class imbalance:** Techniques like SMOTE were not explored but could improve Random Forest performance on the minority class.
- **Generalization:** Models were trained on a single geographic region; performance on imagery from other disaster sites is unknown.
- **Operational gap:** Pixel-level classification is only the first step — translating model output into actionable location data for ground teams remains a critical challenge.

---

## License

This project is licensed under the [MIT License](LICENSE).

---

## Acknowledgments

- University of Virginia, School of Data Science — DS6021: Statistical Learning
- Rochester Institute of Technology — satellite imagery collection
- [American Red Cross Haiti Earthquake Report (2011)](https://www.redcross.org/content/dam/redcross/atg/PDF_s/HaitiEarthquake_OneYearReport.pdf)
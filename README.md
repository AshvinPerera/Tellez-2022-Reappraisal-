# Reappraisal of Land, Opportunism, and Displacement in Civil Wars: Evidence from Colombia, Juan Fernando Tellez, 2022

This repository contains all code and data needed to reproduce the empirical reappraisal of **Tellez (2022)**.  
The work follows the MY457 summative-assessment specification (see `MY457_reappraisal_instructions.pdf`).

---

## 1  Quick-start

```r
# In R (4.3 or later) ----------------------------------------------------------
# 1. Clone the repo and set the working directory to its root

# 2. Run the full pipeline (≈ 5–10 min depending on hardware)
# Install all packages in package_manager.R if you have not done before 
source("code/package_manager.R")
source("code/analysis/clean-data.R")
source("code/analysis/exploratory-analysis.R")
source("code/analysis/replication-analysis.R")
source("code/analysis/callaway-analysis.R")
source("code/analysis/assumption-tests.R")

# 4. Knit the report
rmarkdown::render("reappraisal/MY457_reappraisal-46673")
```

## Acknowledgements

I gratefully acknowledge:

* **Téllez, F. J. (2022)** — for making the original replication package for _“Reappraisal of Land, Opportunism, and Displacement in Civil Wars: Evidence from Colombia”_ publicly
  available. Portions of this repository adapt functions from that package; any errors in adaptation are mine.

# POMP-Based Volatility Modeling for Bitcoin
Cryptocurrency markets exhibit extreme price swings and sentiment-driven regime shifts, which traditional volatility models often fail to capture. This project develops an advanced **Partially Observed Markov Process (POMP)** model, extending the Bretó framework to include **sentiment information** (Fear and Greed Index) and **heavy-tailed innovations** via a Student's t distribution. The model is applied to **Bitcoin return data from 2020 to 2025** and evaluated against standard benchmarks including GARCH and stochastic volatility models.

## Abstract
Cryptocurrency volatility is challenging to model due to its complex, non-linear behavior and sensitivity to market sentiment. We extend Bretó’s POMP framework by:
- Incorporating the **Fear and Greed Index (FGI)** as an exogenous input to the latent volatility dynamics.
- Replacing the standard **Gaussian observation noise** with a **Student’s t-distribution** to account for heavy tails.
- Using **simulation-based inference** for model estimation.

The model shows substantial improvements in **log-likelihood** and **filter stability** over benchmark models. Additionally, the latent parameters inferred align with known stylized facts in financial volatility.

## Project Components
- **`datasets/`**: Contains the datasets used in the project, including:
  - Daily **Bitcoin returns** (2020–2025)
  - **Fear and Greed Index (FGI)** open and close values

- **`breto/`**: Scripts for implementing the core **Breto-POMP model** and its extensions with sentiment and Student’s t noise.

- **`bitcoin-garch-analysis.R`**: Implements a traditional **GARCH model** as a benchmark for Bitcoin return volatility.

- **`bitcoin-preprocessing-HSV.Rmd`**: 
  - Loads and preprocesses the Bitcoin return data
  - Conducts **exploratory data analysis (EDA)**
  - Implements a simple **stochastic volatility POMP model** as a baseline

- **`fng-analysis.Rmd`**:
  - Loads and preprocesses **FGI data**
  - Includes visualizations and exploratory analysis of sentiment patterns

- **`requirements.txt`**: Lists the **R version** and all packages required to run the project.

- **`runf.sbat`**:
  - SLURM batch script to run the full simulation and inference pipeline across **36 compute nodes**
  - Useful for **high-performance computing (HPC)** environments; includes model estimation for POMP via parallel simulation

- **`volatility-project.html`**: Complete documentation of the project with detailed analysis, results, and figures. Generated via R Markdown.

## Requirements 
All dependencies are listed in `requirements.txt`. Make sure to install them in an R environment matching the specified version.

To install packages manually:
```r
install.packages("devtools")  # If not already installed
devtools::install_version("pkgname", version = "x.y.z")
```

## Acknowledgements
We would like to express our sincere gratitude to Professor Edward L. Ionides, whose course on Time Series Analysis provided the foundational knowledge and rigorous understanding of state space models that underpins this project. His constant guidance, insightful feedback, and encouragement throughout the research were instrumental in elevating the technical depth and clarity of the work. This project greatly benefited from his expertise, and it would not have reached its current level of sophistication without his support.

## References
King, A. A., Nguyen, D., & Ionides, E. L. (2016). Statistical Inference for Partially Observed Markov Processes via the R Package pomp. Journal of Statistical Software, 69(12), 1–43. https://doi.org/10.18637/jss.v069.i12

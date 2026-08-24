# The Presidential Puzzle: the continuum — analysis code

R code behind **[The Presidential Puzzle: the continuum](https://isrobertma.github.io/research/presidential-puzzle/Ma_Presidential_Puzzle_Continuum.pdf)**,
an empirical revisit of Santa-Clara and Valkanov (2003), *The Presidential Puzzle: Political
Cycles and the Stock Market* (**Journal of Finance** 58(5)).

The original study covers 1927–1998. This extends the sample to **January 1934 – December 2023**,
so it spans the 2008 credit crisis and COVID-19 — two volatility regimes the original could not
see, which materially thicken the tails of the monthly excess-return distribution.

The write-up lives in [isRobertMa/research](https://github.com/isRobertMa/research/tree/main/presidential-puzzle).

## What the analysis does

`presidential_puzzle_analysis.R` builds monthly excess returns over the 3-month Treasury bill for
two CRSP S&P 500 portfolios, splits them by the party holding the presidency, and tests whether
the two regimes differ:

| Step | Method |
|---|---|
| Returns | Continuously compounded T-bill return; excess returns `V_T = VWP − T`, `E_T = EWP − T` |
| Visual | `ggplot2` / `ggpubr` — series by party, and kernel densities of both portfolios |
| Difference in means | Two-sample `t.test`, Republican vs Democrat months |
| Regression | `lm(excess ~ party_status)`, with Q–Q plots of residuals |
| Variance | One-way `aov` |
| Stationarity | Augmented Dickey–Fuller (`tseries::adf.test`), whole sample and each party |
| Serial correlation | `acf` / `pacf`, then `forecast::auto.arima` fitted overall and per party |

Fitting ARIMA separately by party matters: the party subsamples are not contiguous in time, so a
result that survives that split is not simply picking up a trend.

## Data

| File | Source | In this repo |
|---|---|---|
| `Presidential_Party_by_Month.csv` | Own coding — `0` Democrat, `1` Republican, 1934-01 to 2023-12 | **yes** |
| `TB3MS.csv` | [FRED TB3MS](https://fred.stlouisfed.org/series/TB3MS), 3-Month Treasury Bill Secondary Market Rate | **yes** |
| `Value_weighted.csv` | CRSP US Stock & Indexes, INDNO **1000510** — value-weighted S&P 500 universe, monthly total return | **no — licensed** |
| `Equal_weighted.csv` | CRSP US Stock & Indexes, INDNO **1000511** — equal-weighted, monthly total return | **no — licensed** |

The two CRSP files are licensed data and are deliberately not redistributed here. They are listed
in `.gitignore`, so the analysis runs unchanged for anyone who has them.

**To obtain them:** through an institutional CRSP subscription (most university libraries have
one), export monthly total returns for INDNO 1000510 and 1000511 over 1934-01 to 2023-12, and save
each as CSV with the columns `MthCalDt, INDNO, _NAME_, COL1` in this directory.

## Running it

```r
# from this directory
source("presidential_puzzle_analysis.R")
```

Requires `ggplot2`, `ggpubr`, `dplyr`, `lme4`, `tseries`, and `forecast`:

```r
install.packages(c("ggplot2", "ggpubr", "dplyr", "lme4", "tseries", "forecast"))
```

Without the two CRSP files the script stops at the first `read.csv` that cannot find its input —
that is expected, not a bug.

## A note on what this shows

The regressions find a difference in mean monthly excess returns across parties. That is a
correlation across 90 years and roughly a dozen presidencies, not a causal estimate: interest
rates, inflation, unemployment, fiscal and trade policy are all omitted, and the count of
independent presidential terms is small. The paper is explicit about this. The interesting
question the extended sample raises is whether the original finding survives two more financial
crises at all.

---

ECO421, Department of Mathematical & Computational Sciences,
University of Toronto Mississauga. Instructor: Harry G.G. Burke.

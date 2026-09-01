

<!-- Generated from README-source.qmd. Do not edit README.md directly. -->

<div align="center">

<img src="https://github.com/user-attachments/assets/085c224f-de75-4fb6-a828-e9a468df4c60"/>

<br>

*Reproducible R project for studying the longevity of popular Steam
games through a curated dataset and statistical analysis.*

> **Project status:** *Work in progress*

</div>

## Overview

This project uses a reproducible R pipeline to clean and validate a
[fixed source snapshot](https://zenodo.org/records/21567902) and produce
the [**Popular Steam Games
Dataset**](https://www.kaggle.com/datasets/jokeich/popular-steam-games-dataset),
a curated dataset of fully released paid Steam games from **2013–2024**
with at least **10000 reviews**.

The *exploratory data analysis*, the *statistical analysis, including
model building and evaluation*, and the *Italian and English reports*,
covering methods, results, limitations, and reproducibility, are
currently under development.

## Quick start

The project requires **R 4.5 or later** and **Quarto 1.9 or later**.
**TinyTeX**, or another compatible TeX distribution, is needed only for
PDF reports.

``` powershell
Rscript setup.R
Rscript main.R
```

- [`setup.R`](setup.R) restores the packages recorded in
  [`renv.lock`](renv.lock).
- [`main.R`](main.R) runs the pipeline.

## "Cooperation with Strangers: Spillover of Community Norms"
#### By Mario Molina, Victor Nee, and Hakan Holm

This repository includes a replication package for the results with experimental data on our paper (a link to the paper will be posted soon). The package is written in `R` and it includes:

- `run.R`: A file that replicates all analyses and uses relative paths to create folders. You can run this file if you open a terminal or command line in your computer and write `Rscript run.R`. If some of the `R` libraries we use are not installed, they will be installed before reproducing results on the paper.
- `mturk_study.R`: A file that reproduces analyses from the online experiment on Amazon Mechanical Turk.
- `credamo_study.R`: A file that reproduces analyses from the online experiment on Credamo (a Chinese crowdsourcing site similar to AMT).
- `data/`: A folder with the data used for analyses. It contains two `csv` files:
  - `mturk_data_all.csv`
  - `credamo_data_all.csv`

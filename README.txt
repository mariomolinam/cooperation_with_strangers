"Cooperation with Strangers: Spillover of Community Norms"

By Mario Molina, Victor Nee, and Hakan Holm

This repository includes all materials to reproduce the results with observational and experimental data on our paper. The code is written in R and it includes:

- `run.R`: A file that replicates all analyses and uses relative paths to create folders. You can run this file if you open a terminal or command line in your computer and write `Rscript run.R`. If some of the R libraries we use are not installed, they will be automatically installed before reproducing results on the paper. Make sure that you are in the right folder on the terminal when you use `Rscript` (i.e., the folder where the package was downloaded and unzipped).

- `mturk_study.R`: A file that reproduces analyses from the online experiment on Amazon Mechanical Turk.

- `credamo_study.R`: A file that reproduces analyses from the online experiment on Credamo (a Chinese crowdsourcing site similar to AMT).

- data/: A folder with the data used for analyses. It contains three .csv files:
      - yangzi-delta_data_all.csv
      - mturk_data_all.csv
      - credamo_data_all.csv

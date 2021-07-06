
Article: "Cooperation with Strangers: Spillover of Community Norms"
Authors: Mario Molina, Victor Nee, and Hakan Holm


**Instructions to reproduce the study**


This repository includes all materials to reproduce the results with observational and experimental data on the paper. To reproduce the main results on the paper, open a terminal or command line in your computer and write `Rscript master.R`. `R` libraries used for the analyses will be automatically installed before reproducing results on the paper. Make sure that you are in the right folder on the terminal when you use `Rscript` (i.e., the folder where the package was downloaded and unzipped). Results will be stored in a newly created `results` folder.

All code is written in `R`. 

Pre-registration materials for the two experimental studies included in this repository can be found in these links: 

- Online experiment on Credamo: https://osf.io/tu8vc.
- Online experiment on Amazon Mechanical Turk: https://osf.io/5mkz4.


**File description**


- `./master.R`: A file that replicates all analyses and uses relative paths to create folders. 

- `./code/yangzi-delta_study.R`: A file that reproduces analyses using data from a large random sample of CEOs of manufacturing firms in the Yangzi River Delta region of China. 

- `./code/mturk_study.R`: A file that reproduces analyses from the online experiment on Amazon Mechanical Turk.

- `./code/credamo_study.R`: A file that reproduces analyses from the online experiment on Credamo (a Chinese crowdsourcing site similar to AMT).

- data/: A folder with the data used for analyses. It contains three .csv files:
      - `./data/yangzi-delta_data_all.csv`
      - `./data/mturk_data_all.csv`
      - `./data/credamo_data_all.csv`

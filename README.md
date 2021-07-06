## "Cooperation with Strangers: Spillover of Community Norms"
#### By Mario Molina, Victor Nee, and Hakan Holm


**Instructions to reproduce the study**

This repository includes all materials to reproduce the results on the paper using experimental data (currently accepted at _Organization Science_). To reproduce the main results on the paper, open a terminal or command line in your computer and write `Rscript master.R`. `R` libraries used for the analyses will be automatically installed before reproducing results on the paper. Make sure that you are in the right folder on the terminal when you use `Rscript` (i.e., the folder where the materials were downloaded). All results will be stored in a new the folder named `results`.

All code is written in `R`. 

Pre-registration materials for the two experimental studies included in this repository can be found in these links: 

- Online experiment on Credamo: https://osf.io/tu8vc.
- Online experiment on Amazon Mechanical Turk: https://osf.io/5mkz4.

**Important Note:** On this Github repository, visitors can only see the code to reproduce results that use observational data (i.e. the Yangzi Delta study). To download the data that matches the code, please visit the Cornell Institute for Social and Economic Research ([CISER](https://archive.ciser.cornell.edu/reproduction-packages/2858)). You must agree to use these data **only** to reproduce the results on the paper.

**File description**


- `./master.R`: A file that replicates all analyses and uses relative paths to create folders. 

- `./code/yangzi-delta_study.R`: A file that reproduces analyses using data from a large random sample of CEOs of manufacturing firms in the Yangzi River Delta region of China. 

- `./code/mturk_study.R`: A file that reproduces analyses from the online experiment on Amazon Mechanical Turk.

- `./code/credamo_study.R`: A file that reproduces analyses from the online experiment on Credamo (a Chinese crowdsourcing site similar to AMT).

- data/: A folder with the data used for analyses. It contains three .csv files:
      - ~~`./data/yangzi-delta_data_all.csv`~~
      - `./data/mturk_data_all.csv`
      - `./data/credamo_data_all.csv`




This repository includes all materials to reproduce the results with observational and experimental data on our paper. The code is written in `R` and it includes the following files:

- `run.R`: A file that replicates all analyses and uses relative paths to create folders. You can run this file if you open a terminal or command line in your computer and write `Rscript run.R`. If some of the R libraries we use are not installed, they will be automatically installed before reproducing results on the paper. Make sure that you are in the right folder on the terminal when you use `Rscript` (i.e., the folder where the package was downloaded and unzipped).

- `mturk_study.R`: A file that reproduces analyses from the online experiment on Amazon Mechanical Turk (AMT).

- `credamo_study.R`: A file that reproduces analyses from the online experiment on Credamo (a Chinese crowdsourcing site similar to AMT).

- `data/`: A folder with the data used for analyses. It contains three `csv` files:
   * yangzi-delta_data_all.csv
   * mturk_data_all.csv
   * credamo_data_all.csv

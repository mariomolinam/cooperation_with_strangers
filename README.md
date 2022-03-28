## "Cooperation with Strangers: Spillover of Community Norms"*
#### By Mario Molina, Victor Nee, and Hakan Holm

(This article is published at _Organization Science_ - [click here](https://pubsonline.informs.org/doi/10.1287/orsc.2021.1521))

---

### Instructions to reproduce the study

This repository includes all materials to reproduce the results on the paper using observational and experimental data. To reproduce all the results on the paper, open a terminal or command line in your computer and write `Rscript master.R`. All `R` libraries used for the analyses will be automatically installed before reproducing the results on the paper. Make sure that you are in the right folder on the terminal when you use `Rscript` (i.e., the folder where the materials were downloaded). All results will be stored in a new folder named `results`.

All code is written in `R`. 

**Important Note:** On this Github repository, visitors can only see the code to reproduce results that use observational data (i.e., the Yangzi Delta study). To download the data that matches the code, please visit the Cornell Institute for Social and Economic Research ([CISER](https://archive.ciser.cornell.edu/reproduction-packages/2858)). You must agree to use these data **only** to reproduce the results on the paper.

Pre-registration materials for the two experimental studies on the paper can be found by clicking the following links: 

- Online experiment on Credamo: https://osf.io/tu8vc.
- Online experiment on Amazon Mechanical Turk: https://osf.io/5mkz4.

---

### File description


- `./master.R`: A file that runs all files within the `./code` folder. It uses relative paths to create new folders. 

- `./code/yangzi-delta_study.R`: A file that reproduces analyses using data from a large random sample of CEOs of manufacturing firms in the Yangzi River Delta region of China. 

- `./code/mturk_study.R`: A file that reproduces analyses from the online experiment on Amazon Mechanical Turk.

- `./code/credamo_study.R`: A file that reproduces analyses from the online experiment on Credamo (a Chinese crowdsourcing site similar to AMT).

- data/: A folder with the data used for analyses. It contains three .csv files:
    - ~~`./data/yangzi-delta_data_all.csv`~~
    - `./data/mturk_data_all.csv`
    - `./data/credamo_data_all.csv`

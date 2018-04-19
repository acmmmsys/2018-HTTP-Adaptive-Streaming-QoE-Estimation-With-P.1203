# Scripts

The following scripts can be used to:

- Run the ITU-T P.1203 model on the features
- Create the output statistics

## Requirements

To run the scripts, you need:

- Python 3
  - Install the software from [https://github.com/itu-p1203/itu-p1203/](https://github.com/itu-p1203/itu-p1203/) using `pip3`
  - `pip3 install numpy tqdm pandas`
- R
  - Installing RStudio is recommended
  - Additional packages via `install.packages(tidyverse, magrittr, extrafont, broom, ggforce, ggrepel, xtable)`

## Usage

You need to run the scripts from this folder.

```
python3 create_model_outputs.py -c
Rscript dataset_analysis.R
```
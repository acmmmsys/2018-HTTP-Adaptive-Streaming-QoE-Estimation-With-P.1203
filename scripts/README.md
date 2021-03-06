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

## License

Copyright 2018 Werner Robitza, David Lindegren

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
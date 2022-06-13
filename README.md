# Installation

# Table of contents

- Introduction
- Installation
- Running
- Manifest

## Introduction

This repository provides raw data and code for the paper 
"Reshaping musical consonance with timbral manipulations and massive online experiments" 
by Marjieh, Harrison, Lee, Deligiannaki, & Jacoby (2022).

- To load the software onto your computer, see 'Installation'.
- To run the software, see 'Running'.
- To access the raw data, see 'Manifest'.
- To access the supplementary materials web interface, open `supplementary.html`.

## Installation

There are two ways to load the software onto your computer.
One is to use *Docker*. Docker is a tool for guaranteeing long-term reproducibility.
It encapsulates the entire computing environment within a single image, 
including operating system and all dependencies. 
The Docker approach is good if you want to rerun analyses but you don't
want to extend the code significantly yourself.
Alternatively, you can take the *renv* approach. renv is more lightweight,
and only provides a snapshot of the R packages used by the analyses,
not the operating system or other dependencies. This makes less
guarantees about reproducibility, but is the easier option if you want to
extend the code yourself.

### Docker

Note: Unfortunately the Docker approach doesn't support M1 Macs
on account of their ARM64 architecture. This will hopefully be fixed
by a future update to RStudio.

To install Docker, follow the [official Docker instructions](https://docs.docker.com/get-docker/).

Next you need to acquire the Docker image. There are two ways to do this:
from GitLab, or from a downloaded file. The former will generally be easiest,
but the latter option is useful for reproducibility.

#### Docker from GitLab

1. Pull the image from GitLab:

```
docker pull registry.gitlab.com/pmcharrison/harmony/timbre-and-consonance-paper
```

2. Run it: 

```
docker run --rm -p 127.0.0.1:8787:8787 -e DISABLE_AUTH=true -e ROOT=TRUE registry.gitlab.com/pmcharrison/harmony/timbre-and-consonance-paper
```

Navigate to http://localhost:8787/ in your web browser.
This should take you to an RStudio window hosting the analyses.

#### Docker from downloaded file

The starting point is a file of the form `timbre-and-consonance-paper.tar`.

You load the Docker file like this:

```
docker load < timbre-and-consonance-paper.tar
```

Note: the initial part of this `docker load` stage might take about a minute, 
without any progress bar or other status indications.

Then you launch a Docker container from it:

```
docker run --rm -p 127.0.0.1:8787:8787 -e DISABLE_AUTH=true -e ROOT=TRUE timbre-and-consonance-paper
```

Note: You will probably see a message in the log that says

```
Authentication token manipulation error
chpasswd: (line 1, user rstudio) password not changed
```

Don't worry, this is normal.

Finally, navigate to http://localhost:8787/ in your web browser.
This should take you to an RStudio window hosting the analyses.

#### Creating a Docker image from the Git repository

*The following instructions are primarily intended for the papers' authors*.
*You do not need to run them if you're just trying to reproduce the analyses.*

You can create a Docker image from the Git repository by following these steps:

1. Log into your Docker repository:

```
docker login registry.gitlab.com/pmcharrison/harmony/timbre-and-consonance-paper
```

2. Pull the image from the repository:

```
docker pull registry.gitlab.com/pmcharrison/harmony/timbre-and-consonance-paper
```

3. Tag it:

```
docker tag registry.gitlab.com/pmcharrison/harmony/timbre-and-consonance-paper timbre-and-consonance-paper
```

4. Save it to a tar file:

```
docker save timbre-and-consonance-paper -o ~/Downloads/timbre-and-consonance-paper.tar
```

Note: this `docker save` stage might take about a minute, without any progress bar 
or other status indications.

## renv

To install the project via renv, follow these steps:

1. Install R: https://www.r-project.org/

2. Install RStudio: https://www.rstudio.com/products/rstudio/download/

3. Unzip the Supplementary Materials zip file, and open the project by double-clicking on timbre-and-consonance-paper.Rproj.

4. Install `renv`: `install.packages("renv")`

6. Run `renv::restore()` in your RStudio R console (see also 'Troubleshooting R package installation').

7. Follow the instructions in 'Setting up your Python environment'.

### Troubleshooting R package installation

When running this command on MacOS Big Sur, one may encounter the following 
error:

```
library not found for -lgfortran
```

If this happens, return to the R installation instructions and make sure
you followed all the stated steps, in particular the instructions
about installing external libraries so that you can build packages from source.
If this doesn't work, you can try the following:

1. Install `Homebrew` (follow [these instructions](https://brew.sh/)).

2. Install `gcc`.

```
brew install gcc
```

3. Create an `~/.R` folder, if it doesn't exist already.

```
mkdir ~/.R
```

4. Create a Makevars file, if it doesn't exist already.

```
touch ~/.R/Makevars
```

5. Add the following lines to `Makevars`:

```
FLIBS=-L/opt/homebrew/Cellar/gcc/11.1.0_1/lib/gcc/11/
F77="/opt/homebrew/bin/gfortran"
FC="/opt/homebrew/bin/gfortran"
```

You may need to update the `FLIBS` paths to match
your current installation.

Test whether this worked by running the following command:

```
install.packages(c("Matrix", "nlme"), type = "source")
```

If it worked, you can try again with the `renv` initialisation:

```
renv::restore()
```

### Setting up your Python environment

Running the consonance models requires a Python 3 installation,
as well as a few additional Python packages.
You can skip this step as long as you don't want to run the 
`010-run-model-batches.R` script, and will instead just work with
pre-cached outputs. Otherwise:

1. Download and install Python from the [official website](https://www.python.org/downloads/).

2. Prepare a virtual environment in which to install `praat-parselmouth`.
One recommended way is as follows (instructions for Mac, run one line at a time in your Terminal):

```
pip3 install virtualenv
pip3 install virtualenvwrapper
export WORKON_HOME=$HOME/.virtualenvs
mkdir -p $WORKON_HOME
export VIRTUALENVWRAPPER_PYTHON=$(which python3)
source $(which virtualenvwrapper.sh)
mkvirtualenv parselmouth --python $(which python3)
echo "export VIRTUALENVWRAPPER_PYTHON=$(which python3)" >> ~/.zshrc
echo "source $(which virtualenvwrapper.sh)" >> ~/.zshrc
```

For Linux you will probably want to replace `~/.zshrc` with `~/.bashrc`.

Install the required Python packages in the virtual environment
(make sure you have navigated to the project's root directory):

```
workon parselmouth
pip3 install -r requirements.txt
```

### Cloning the repository from Git 

If you wish to download this repository via Git instead of working via the 
Supplementary Materials, you must install 
`git LFS`, an add-on for Git that supports the usage of large files.
Please follow the instructions on the [official website](https://git-lfs.github.com/) 
to accomplish this.

# Running

The following section assumes that you have RStudio open, launched either via
Docker or via the renv instructions above.

> How do I rerun all the analyses and recreate all the plots?

Delete the `output` directory and rerun the files in `output/scripts`
(for example, by opening them in RStudio, and clicking 'Source' for each one)
in numeric ascending order. Note that `010-run-model-batches.R` may take
a few hours (~ 6?) depending on the speed of your computer (more cores is better).

> How do I recreate the supplementary materials web page?

Open `supplementary.Rmd` in RStudio then press the 'Knit' button.

> How do I access the main outputs for further data analyses?

Navigate to `output/batches/` and find the RDS file(s) corresponding to your
data of interest. Load these into R using `readRDS`, and explore the files
in your R console, hopefully you find what you need there.
In many cases you will also find analogous `csv` files, which you can 
read with R or with an alternative program.
As long as you're happy working with these pregenerated analyses,
you can get going right away with the files as provided in the 
Supplementary Materials version of the project,
without having to run the time-consuming 6-hour analysis script.

# Manifest

`input/` - Contains the input data files.

`media/` - Contains a few media files used to illustrate
certain concepts.

`output/` - Contains the outputs generated from the analysis scripts.
By default many of these outputs are cached. To regenerate all outputs from 
scratch, delete this folder and rerun the analysis scripts (in `scripts`).

`output/audio-examples` - Contains some audio examples.

`output/batches/` - Contains the main analysis outputs 
(consonance models, smoothed behavioral data) for the different experiments
under consideration. Some of the files are `rds` files, which can be 
read into R using `readRDS()`. Some are `csv` files, which can be read 
using `read.csv`.

`output/illustrations` - Contains some illustrations.

`output/plots` - Contains the generated plots as well as some accompanying
summar csv files.

`run-all-analyses.R` - Convenience script that runs all the analysis 
scripts in sequence. Will take a while to run!

Note: when running this script, we have occasionally observed an error
of the following form:

```
Error in x %>% data.table::rbindlist() %>% { : object 'digits' not found 
```

This error seems to be something to do with the multiprocessing code.
Curiously, simply rerunning the script has been sufficient for bypassing
the error in our case. Note that the caching means that the code
should pick up more or less where it left off.

`scripts/` - Contains scripts to be sourced in R using `source()`.

`scripts/stimulus-generation/` - Contains scripts for stimulus generation.

`scripts/analysis/` - Contains scripts for stimulus generation.
These scripts are intended to be run in numeric order. 

`scripts/analysis/010-run-model-batches.R` - Performs the time-consuming
analyses and saves the results to `output/batches/`.
Some of the files are `rds` files, which can be 
read into R using `readRDS()`. Some are `csv` files, which can be read 
using `read.csv`.
The results are cached, and by default the computations will not be rerun
if the output files exist already. 
To trigger rerunning of a given analysis, delete the corresponding
folder from `output/batches`.

See above for discussion of a possible error here, concerning the following
error message:

```
Error in x %>% data.table::rbindlist() %>% { : object 'digits' not found
```

`scripts/clear-behavioral-output.R` - A helper script that deletes all the 
outputs of the behavioral analyses in `output/batches`, so that they 
will be regenerated from scratch in the next call to 
`scripts/analysis/010-run-model-batches.R`.

`scripts/illustrations/` - Contains some scripts for creating illustrations.

`src/` - Source code used by the files in `scripts/`.

`renv/` - `renv` automatically maintains an isolated R package library 
for this project, separated from your global R installation,
and stored in this folder.

`renv.lock` - Tabulates the R dependencies for the project.

`requirements.txt` - Tabulates the Python dependencies.

`supplementary.Rmd` - An RMarkdown file that, when run, generates the supplementary
materials webpage (`supplementary.html`).

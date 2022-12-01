FROM rocker/rstudio:4.2.1

ENV PROJECT_NAME=consonance-simulations
ENV RENV_VERSION 0.12.0

ENV HOME_DIR=/home/rstudio
ENV PROJECT_ROOT=${HOME_DIR}/${PROJECT_NAME}

RUN apt-get update && apt-get install -y \
  ffmpeg \
  libpng-dev \
  libmagick++-dev \
  libpoppler-cpp-dev \
  && rm -rf /var/lib/apt/lists/*

USER rstudio

RUN R -e "install.packages(c('rstudioapi'), repos = c(CRAN = 'https://cloud.r-project.org'))"

# Set the RStudio default project
RUN echo 'setHook("rstudio.sessionInit", function(newSession) if (newSession && is.null(rstudioapi::getActiveProject())) rstudioapi::openProject(Sys.getenv("PROJECT_ROOT")), action = "append")' >> ${HOME_DIR}/.Rprofile

COPY . $PROJECT_ROOT
WORKDIR $PROJECT_ROOT

RUN R -e "install.packages(c('remotes', 'rstudioapi'), repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

RUN R -e "Sys.setenv(MAKEFLAGS = sprintf('-j%d', parallel::detectCores())); options(renv.consent = TRUE); renv::restore()"

USER root

RUN chown -R rstudio ${PROJECT_ROOT}
WORKDIR $HOME_DIR

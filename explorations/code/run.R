MAMI.CODI.STRETCHED.TIBBLE = tibble::tibble(major_minor=numeric(),
                                            consonance_dissonance=numeric(),
                                            tonic=numeric(),
                                            tonic_angle=numeric(),
                                            tonic_selector=character(),
                                            interval=integer())

source("explorations/code/setup.R")
source("explorations/code/models.R")
source("explorations/code/create.data.R")
# source("explorations/code/plot.R")

saveRDS(MAMI.CODI.STRETCHED.TIBBLE,'/Users/landlessness/Documents/git/pmcharrison/timbre-and-consonance-paper/explorations/results/tibbles/bonang.rds')


models.names = c('behaviour','interference.simplex','periodicity.duplex')
experiment.names = c('stretched')
orig.dir = './output/batches/'
mami.codi.dir = './explorations/data/'
base.dir <- list(
  'behaviour' = orig.dir,
  'interference.simplex' = orig.dir,
  'periodicity.duplex' = mami.codi.dir
)
experiment.dir <- list(
  'stretched' = 'Stretched dyads (3 dB roll-off)/'
)
model.file.name <- list(
  'behaviour' = 'behaviour/profile.rds',
  'interference.simplex' = 'models/Hutchinson & Knopoff (1978).rds',
  'periodicity.duplex' = 'models/mami.codi.m.1.t.1.h.2.l.-1.r.73.5759.rds'
)
file.name.for <- function(experiment.name,model.name) {
  paste0(base.dir[[model.name]],
         experiment.dir[[experiment.name]],
         model.file.name[[model.name]])
}
results.for <- function(experiment.name,model.name) {
  checkmate::assert_true(getwd() == rprojroot::find_rstudio_root_file())
  checkmate::assert_choice(model.name, models.names)
  checkmate::assert_choice(experiment.name, experiment.names)
  readRDS(file.name.for(experiment.name,model.name))
}


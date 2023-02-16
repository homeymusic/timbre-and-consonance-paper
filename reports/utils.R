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

homey.brown       = '#664433'
homey.cream       = '#F3DDAB'
homey.dark.cream  = '#7F745A'
homey.blue        = '#ABDAF3'
homey.red         = '#FF5500'
homey.maize       = '#F3A904'
homey.green       = '#73DE73'

plot.interval.consonance <- function(x.lines,y.lines,x.points,y.points=NULL,title=NULL) {
  plot(x.lines, y.lines, col=homey.red, lwd = 3, type='l',
       main=title, xlab='Intervals', ylab = 'Consonance')
  if (!is.null(y.points)) {
    points(x.points, y.points, col=homey.dark.cream)
  }
  abline(v = 0:15,lty = 2, col = "gray")
  axis(1, at=0:15)
}
variable.name <- function(v) {
  deparse(substitute(v))
}
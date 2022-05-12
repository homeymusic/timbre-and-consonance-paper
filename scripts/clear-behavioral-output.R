x <- list.dirs("output/batches", recursive = FALSE)

for (dir in x) {
  unlink(file.path(dir, "behaviour"), recursive = TRUE)
}

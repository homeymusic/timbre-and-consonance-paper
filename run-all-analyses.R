for (f in list.files("scripts/illustrations", pattern = "\\.R$", full.names = TRUE)) {
  message("Running ", f, "...")
  source(f)
}

for (f in sort(list.files("scripts/analysis", pattern = "\\.R$", full.names = TRUE))) {
  message("Running ", f, "...")
  source(f)
}

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(f)
sample_dir <- file.path(root, "data-sample")

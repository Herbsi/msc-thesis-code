library(data.table, warn.conflicts = FALSE)

renameForCSV <- Vectorize(function(string) {
  rename_list <- list(
    "CP_LOC" = "\\textsc{\\idx{cp-loc}}",
    "CP_OLS" = "\\textsc{\\idx{cp-ols}}",
    "DR" = "\\textsc{glmdr}", # HACK 2024-08-20 This should be \\abb{glmdr}, but this caused issues in the tables in LaTeX.
    ## But \\abb{idr} works for some reason
    "IDR" = "\\abb{idr}",
    "QR" = "\\abb{qr}",
    "IDR*" = "\\dcpstar{\\abb{idr}}",
    "QR*" = "\\dcpstar{\\abb{qr}}"
    )
  new_name <- rename_list[[string]]
  ifelse(is.null(new_name), string, new_name)
})


writeTable <- function(x, filename, ...) {
  tableDir <- file.path("..", "tex", "tables")
  dir.create(tableDir, showWarnings = FALSE, recursive = TRUE)
  fwrite(x, file.path(tableDir, filename), scipen = 1000, ...)
}

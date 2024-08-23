library(ggplot2)
library(showtext)

### Configurations -------------------------------------------------------------

# NOTE <2024-05-22 Wed> Hardcoded pointsize and `\the\textwidth' from LaTeX document here.
pointsize <- 11
textwidth <- 418.2555

family <- "Valkyrie A"
familyTab <- "Valkyrie A Tab"
familyCaps <- "Valkyrie A Tab Caps"
familyMath <- "Asana Math"
font_add(family = family, regular = "Valkyrie A Regular.otf")
font_add(family = familyTab, regular = "Valkyrie A Tab Regular.otf")
font_add(family = familyCaps, regular = "Valkyrie A Tab Caps Regular.otf")
font_add(family = familyMath, regular = "Asana Math.otf")

showtext_auto()


### Functions ------------------------------------------------------------------

theme_dcp <- function() {
  list(
    theme_minimal(),
    theme(
      text = element_text(
        size = pointsize,
        ## Use tabular figures by default
        family = familyTab),
      legend.text = element_text(
        ## Use small caps by default in legend because most of the time,
        ## the legends are the methods.
        family = familyCaps),
      strip.placement = "outside")
  )
}


renameForPlot <- Vectorize(function(string) {
  rename_list <- list(
    ## We use a font with small caps at the right points, which is why we use lowercase letters here.
    ## NOTE 2024-08-23 This hardcodes \idx{cp-loc}, \abb{glmdr}, etc.
    "CP_LOC" = "cp-loc",
    "CP_OLS" = "cp-ols",
    "DR" = "glmdr",
    "IDR" = "idr",
    "IDR*" = "idr★",
    "QR" = "qr",
    "QR*" = "qr★",
    "conditional_coverage_mse_full" = "full",
    "conditional_coverage_mse_hres" = "hres",
    ## TODO 2024-08-20 Change approach names.
    "dcp" = "DCP",
    "ziegel" = "Ziegel",
    "bru" = "Brussels",
    "fra" = "Frankfurt",
    "lhr" = "London",
    "zrh" = "Zurich"
  )
  new_name <- rename_list[[string]]
  ifelse(is.null(new_name), string, new_name)
})


savePlot <- function(filename, ...) {
  plotDir <- file.path("..", "tex", "images", "vectors")
  ggsave(
    filename,
    device = "pdf",
    path = plotDir,
    width = textwidth / 72.27,
    units = "in",
    create.dir = TRUE,
    ...)
}

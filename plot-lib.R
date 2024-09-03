library(ggplot2)
library(showtext)

### Configurations -------------------------------------------------------------

# NOTE <2024-05-22 Wed> Hardcoded pointsize and `\the\textwidth' from LaTeX document here.
pointsize <- 10.5
textwidth <- 418.25555555

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

methodLevels <- c("cp-ols", "cp-loc", "dcp-glmdr", "dcp-idr", "dcp-qr", "dcp-idr★", "dcp-qr★")

scale_dcp <- function(breaks = methodLevels, limits = methodLevels) {
  list(
    scale_colour_brewer(palette = "Set1", breaks = breaks, limits = limits),
    suppressWarnings(scale_shape_discrete(breaks = breaks, limits = limits))
  )
}

theme_dcp <- function() {
  list(
    theme_minimal(),
    theme(
      axis.title = element_text(
        size = pointsize,
        family = familyTab), # Use tabular figures by default
      axis.text = element_text(
        size = 8,
        colour = "black",
        family = familyTab),
      legend.title = element_text(
        size = pointsize,
        family = familyTab),
      legend.text = element_text(
        size = pointsize,
        ## Use small caps by default in legend because most of the time,
        ## the legends are the methods.
        family = familyCaps),
      strip.text = element_text(
        size = pointsize,
        family = familyTab),
      strip.placement = "outside")
  )
}


renameForPlot <- Vectorize(function(string) {
  rename_list <- list(
    ## We use a font with small caps at the right points, which is why we use lowercase letters here.
    ## NOTE 2024-08-23 This hardcodes \idx{cp-loc}, \abb{glmdr}, etc.
    "CP_LOC" = "cp-loc",
    "CP_OLS" = "cp-ols",
    "DR" = "dcp-glmdr",
    "IDR" = "dcp-idr",
    "IDR*" = "dcp-idr★",
    "QR" = "dcp-qr",
    "QR*" = "dcp-qr★",
    "dcp" = "A5",
    "ziegel" = "A1",
    "bru" = "Brussels",
    "fra" = "Frankfurt",
    "lhr" = "London",
    "zrh" = "Zurich"
  )
  new_name <- rename_list[[string]]
  ifelse(is.null(new_name), string, new_name)
})


savePlot <- function(filename, aspect = 2 / 3, ...) {
  plotDir <- file.path("..", "tex", "images", "vectors")
  ggsave(
    filename,
    device = "pdf",
    path = plotDir,
    width = textwidth / 72.27,
    height = aspect * textwidth / 72.27,
    limitsize = FALSE,
    units = "in",
    create.dir = TRUE,
    ...)
}

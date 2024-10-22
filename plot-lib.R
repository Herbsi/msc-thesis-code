### Configurations -------------------------------------------------------------

# NOTE <2024-05-22 Wed> Hardcoded pointsize and `\the\textwidth' from LaTeX document here.
pointsize <- 10.5 # HACK 2024-09-03 This is slightly smaller than in LaTeX, but looks closer.
textwidth <- 418.25555555

family <- "Valkyrie A"
familyCaps <- "Valkyrie A Caps"
familyMath <- "Asana Math"

sysfonts::font_add(family = family, regular = "Valkyrie A Regular.otf")
sysfonts::font_add(family = familyCaps, regular = "Valkyrie A Caps Regular.otf")
sysfonts::font_add(family = familyMath, regular = "Asana Math.otf")

showtext::showtext_auto()


### Functions ------------------------------------------------------------------

method_levels <- c("cp-ols", "cp-loc", "dcp-glmdr", "dcp-idr", "dcp-qr", "dcp-idr★", "dcp-qr★")

scale_dcp <- function(breaks = method_levels, limits = method_levels) {
  list(
    ggplot2::scale_colour_brewer(palette = "Set1", breaks = breaks, limits = limits)
  )
}

theme_dcp <- function() {
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      axis.title = element_text(
        size = pointsize,
        family = family
      ),
      axis.title.x = element_text(family = familyMath),
      axis.text = element_text(
        size = 8,
        colour = "black",
        family = familyMath
      ),
      legend.title = element_text(
        size = pointsize,
        family = family
      ),
      legend.text = element_text(
        size = pointsize,
        ## Use small caps by default in legend because most of the time,
        ## the legends are the methods.
        family = familyCaps
      ),
      strip.text = element_text(
        size = pointsize,
        family = family
      ),
      strip.placement = "outside")
  )
}


prepare_for_plot <- function(dt, cols) {
  rename_for_plot <- Vectorize(function(string) {
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

  dt |>
    mutate(across(all_of(cols), rename_for_plot)) |>
    mutate(method = factor(method, method_levels))
}


save_plot <- function(filename, aspect = 2 / 3, sub_dir = ".", ...) {
  plot_dir <- file.path("..", "tex", "images", "vectors", sub_dir)
  ggplot2::ggsave(
    filename,
    device = "pdf",
    path = plot_dir,
    width = textwidth / 72.27,
    height = aspect * textwidth / 72.27,
    limitsize = FALSE,
    units = "in",
    create.dir = TRUE,
    ...)
  message(stringr::str_c("Wrote ", plot_dir, "/", filename))
}

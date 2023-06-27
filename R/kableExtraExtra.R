kableExtraExtra_fit_to_page <- function(kbl, caption = FALSE) {
  table_info <- kableExtra::magic_mirror(kbl)
  width <- "\\\\linewidth"
  totalheight <- "\\\\textheight"
  if (caption) {
    totalheight <- "\\\\dimexpr\\\\textheight -\\\\abovecaptionskip -\\\\belowcaptionskip -\\\\height\\\\relax"
  }
  kbl <- sub(table_info$begin_tabular,
             paste0("\\\\adjustbox\\{width=",
                    width,
                    ",totalheight=",
                    totalheight,
                    "\\}\\{%\n",
                    table_info$begin_tabular),
             kbl)
  kbl <- sub(table_info$end_tabular, paste0(table_info$end_tabular, "\\}"), kbl)
  kbl
}

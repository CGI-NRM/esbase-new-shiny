# This should maybe be placed in the esbaser package
# But it is easier to create here, and to move later
# Mainly so that all development will be trcaken in the
# branch connected to the issue #11
report_content <- function(
  file,
  selected,
  biologdata,
  provlistas,
  provlistas_metas,
  db
) {
  # Place in tempdir since we don't have write access when deployed
  temp_report <- file.path(tempdir(), "report.Rmd")
  file.copy("report.Rmd", temp_report, overwrite = TRUE)

  params <- list(
    selected = selected,
    biologdata = biologdata,
    provlistas = provlistas,
    provlistas_metas = provlistas_metas,
    db = db
  )

  # Knit the document, passing in the `params` list, and eval it in a child of the gloabl environment
  # This isolates the code in the document from the code in this app.
  rmarkdown::render(temp_report, output_file = file,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}

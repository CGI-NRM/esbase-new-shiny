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

# DEBUG START
accnr <- params$provlistas[[1]][, "accnr"]
df <- accnr
cnames <- c("Acc.nr.")
ccount <- c(1)

provid_table_cols <- c("accnr", "provid", "aces", "delvikt", "provvikt")
provid_table_cols_pretty <- c("Acc.nr.", "ProvID", "ACES NR", "Delvikt (g)", "Provvikt (g)")

for (n in names(params$provlistas)) {
    cols <- c("provid")
    if (params$provlistas_metas[n, "analyslab", drop = TRUE] == "ACES") {
        cols <- c(cols, "aces")
    }
    if (params$provlistas_metas[n, "homogenat", drop = TRUE]) {
        cols <- c(cols, "delvikt")
    }
    cols <- c(cols, "provvikt")

    df <- dplyr::bind_cols(df, params$provlistas[[n]] |> dplyr::select(dplyr::all_of(cols)))

    cnames <- c(cnames, provid_table_cols_pretty[match(cols, provid_table_cols)])
    ccount <- c(ccount, length(cols))
}

ccount_provname <- ccount
ccount_analystyp <- ccount
ccount_analyslab <- ccount
ccount_analytiker <- ccount
ccount_provtagningsinst <- ccount
ccount_vavnad <- ccount
ccount_storage <- ccount

metas <- params$provlistas_metas
metas[metas == "" | is.na(metas)] <- " "

names(ccount_provname) <- c(" ", names(params$provlistas))
names(ccount_analystyp) <- c(" ", metas[, "analystyp", drop = TRUE])
names(ccount_analyslab) <- c(" ", metas[, "analyslab", drop = TRUE])
names(ccount_analytiker) <- c(" ", metas[, "analytiker", drop = TRUE])
names(ccount_provtagningsinst) <- c(" ", metas[, "provtagningsinst", drop = TRUE])
names(ccount_vavnad) <- c(" ", metas[, "vavnad", drop = TRUE]) |> as.character()
names(ccount_storage) <- c(" ", metas[, "storage", drop = TRUE]) |> as.character()
# DEBUG END
print(df)
print(ccount)
print(ccount_provname)
print(ccount_analystyp)
print(ccount_vavnad)
print(ccount_storage)

  # Knit the document, passing in the `params` list, and eval it in a child of the gloabl environment
  # This isolates the code in the document from the code in this app.
  rmarkdown::render(temp_report, output_file = file,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}

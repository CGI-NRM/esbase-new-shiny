#' Create Biologdata Table
#'
#' @param selected A dataHolder containing $acc, $bio, $specimen, $material. As created in mod_provberedning_server.
#' @param db A db dataHolder which contains $conn and helpertables
#' @return A list containing $df, the combined tibble with data from accession, material, specimen and fish/mammal/clam etc.
#' $formats, a named vector with numbrojs formatting strings to be passed into handsontable
#' $colnames, the pretty names to use for showing to the user
create_biologdata_table <- function(db, selected) {
  shiny::req(selected$acc)
  shiny::req(selected$bio)
  shiny::req(selected$specimen)
  shiny::req(selected$material)

  if (selected$acc |> select(catalog_id) |> unique() |> length() != 1) {
    stop("Not all catalog_ids are the same.")
    return()
  }

  df <- (selected$acc |> rename_w_prefix("acc.") |>
         left_join(selected$bio |> rename_w_prefix("bio."), by = join_by(acc.id == bio.accession_id)) |>
         left_join(selected$specimen |> rename_w_prefix("specimen."), by = join_by(acc.id == specimen.id)))

  catalog_id <- selected$acc |> select(catalog_id) |> first()
  if (catalog_id == 2) { # fish
    df[, "bio.gender"] <- (
      df |> select(bio.gender_id) |> unlist() |>
      factor(levels = db$gender |> select(id) |> unlist(),
             labels = db$gender |> select(swe_name) |> unlist()))

    df <- (df |>
           select(acc.id, specimen.age_start, specimen.age_end, specimen.weight, bio.totallength,
                  bio.bodylength, bio.gender, bio.gonadweight, bio.liverweight))

    formats <- c("specimen.age_start" = "0", "specimen.age_end" = "0", "specimen.weight" = "0",
                 "bio.totallength" = "0", "bio.bodylength" = "0", "bio.gonadweight" = "0",
                 "bio.liverweight" = "0")

    colnames <- c("AccNR", "Ålder från (år/K)", "Ålder till (år/K)", "Vikt (g)", "Totallängd (cm)",
                  "Kroppslängd (cm)", "Kön", "Gonadvikt (g)", "Levervikt (g)")

  } else {
    shiny::showNotification("Kan endast hantera fisk.", duration = 10, type = "error")
  }

  df[, "acc.id"] <- df |> select(acc.id) |> unlist() |> as.character() |> esbaser::accdb_to_accnr()

  list(df = df, formats = formats, colnames = colnames)
}

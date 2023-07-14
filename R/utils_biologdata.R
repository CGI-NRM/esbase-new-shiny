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

#' Update Biologdata Overrides From Biologdata Table
#'
#' Note that both new_table$acc.id and selected$acc$id must match.
#'
#' @param db A db dataHolder which contains $conn and helpertables
#' @param selected A dataHolder containing $acc, $bio, $specimen, $material, $*_override. As created in mod_provberedning_server.
#' @param new_table The modified table edited by the user. A modified version of the df returned by \link[esbaser]{create_biologdata_table}
#' @return A boolean of success
update_biologdata_overrides <- function(db, selected, new_table) {
  if (selected$acc |> select(catalog_id) |> unique() |> length() != 1) {
    stop("Not all catalog_ids are the same.")
    return(FALSE)
  }

  if (any(selected$acc$id != esbaser::accnr_to_accdb(new_table$acc.id))) {
    stop("Acc.id does not match")
    return(FALSE)
  }

  catalog_id <- selected$acc |> select(catalog_id) |> first()
  if (catalog_id == 2) { # fish
    selected$specimen_override$age_start <- new_table$specimen.age_start
    selected$specimen_override$age_end <- new_table$specimen.age_end
    selected$specimen_override$weight <- new_table$specimen.weight
    selected$bio_override$totallength <- new_table$bio.totallength
    selected$bio_override$bodylength <- new_table$bio.bodylength
    selected$bio_override$gender_id <- new_table$bio.gender |> factor(levels = db$gender$swe_name, labels = db$gender$id)
    selected$bio_override$gonadweight <- new_table$bio.gonadweight
    selected$bio_override$liverweight <- new_table$bio.liverweight
  } else {
    shiny::showNotification("Kan endast hantera fisk.", duration = 10, type = "error")
  }

  return(TRUE)
}

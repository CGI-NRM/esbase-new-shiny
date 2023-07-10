#' Create Biologdata Table
#'
#' @param selected A dataHolder containing $acc, $bio, $specimen, $material. As created in mod_provberedning_server.
#' @param db A db dataHolder which contains $conn and helpertables
#' @return A list containing $df, the combined tibble with data from accession, material, specimen and fish/mammal/clam etc.
#' $formats, a named vector with numbrojs formatting strings to be passed into handsontable
#' $colnames, the pretty names to use for showing to the user
create_biologdata_table <- function(selected, db, added_material) {
  shiny::req(selected$acc)
  shiny::req(selected$bio)
  shiny::req(selected$specimen)
  shiny::req(selected$material)

  df <- (selected$acc |> rename_w_prefix("acc.") |>
         left_join(selected$bio |> rename_w_prefix("bio."), by = join_by(acc.id == bio.accession_id)) |>
         left_join(selected$specimen |> rename_w_prefix("specimen."), by = join_by(acc.id == specimen.id)))

  catalog_id <- selected$acc |> select(catalog_id) |> first()
  if (catalog_id == 2) { # fish
    material_type_lever_id <- db$material_type |> filter(swe_name == "lever") |> select(id) |> unlist(use.names = FALSE)
    material_type_parasit_id <- db$material_type |> filter(swe_name == "parasit") |> select(id) |> unlist(use.names = FALSE)
    material_type_skrott_id <- db$material_type |> filter(swe_name == "skrott") |> select(id) |> unlist(use.names = FALSE)
    material_type_gonader_id <- db$material_type |> filter(swe_name == "gonader") |> select(id) |> unlist(use.names = FALSE)

    if (material_type_lever_id != 2) {
      shiny::showNotification("Oväntat id för 'lever' material_type.", duration = 10, type = "warning")
    }
    if (material_type_parasit_id != 50) {
      shiny::showNotification("Oväntat id för 'parasit' material_type.", duration = 10, type = "warning")
    }
    if (material_type_skrott_id != 26) {
      shiny::showNotification("Oväntat id för 'skrott' material_type.", duration = 10, type = "warning")
    }
    if (material_type_gonader_id != 16) {
      shiny::showNotification("Oväntat id för 'gonader' material_type.", duration = 10, type = "warning")
    }

#    df <- (
#      df |>
#      left_join(
#        selected$material |> filter(material_type_id == material_type_lever_id) |> rename_w_prefix("mat.lever."),
#        by = join_by(acc.id == mat.lever.accession_id)) |>
#      left_join(
#        selected$material |> filter(material_type_id == material_type_parasit_id) |> rename_w_prefix("mat.parasit."),
#        by = join_by(acc.id == mat.parasit.accession_id)) |>
#      left_join(
#        selected$material |> filter(material_type_id == material_type_skrott_id) |> rename_w_prefix("mat.skrott."),
#        by = join_by(acc.id == mat.skrott.accession_id)) |>
#      left_join(
#        selected$material |> filter(material_type_id == material_type_gonader_id) |> rename_w_prefix("mat.gonader."),
#        by = join_by(acc.id == mat.gonader.accession_id))
#    )

    material_cols_to_select <- character(0)
    material_cols_names <- character(0)
    for (row in seq_len(nrow(added_material$mats))) {
      type_id <- added_material$mats[row, "type_id", drop = TRUE]
      type <- db$material_type |> filter(id == type_id) |> select(swe_name) |> unlist(use.names = FALSE)
      storage_id <- added_material$mats[row, "storage_id", drop = TRUE]
      storage <- db$material_storage |> filter(id == storage_id) |> select(name) |> unlist(use.names = FALSE)

      prefix <- paste0("mat.", type, ".", storage_id, ".")

      df <- df |> left_join(
        selected$material |>
        filter(material_type_id == !!type_id, storage_type_id == !!storage_id) |>
        rename_w_prefix(prefix),
        by = join_by(acc.id == !!paste0(prefix, "accession_id"))
      )

      material_cols_to_select <- c(material_cols_to_select, paste0(prefix, "amount_original"), paste0(prefix, "amount_left"))
      material_cols_names <- c(material_cols_names, paste0(type, " sparat (g) - ", storage), paste0(type, " kvar (g) - ", storage))
    }

    df[, "bio.gender"] <- (
      df |> select(bio.gender_id) |> unlist() |>
      factor(levels = db$gender |> select(id) |> unlist(),
             labels = db$gender |> select(swe_name) |> unlist()))

    df <- (df |>
           select(acc.id, specimen.age_start, specimen.age_end, specimen.weight, bio.totallength,
                  bio.bodylength, bio.gender, bio.gonadweight, bio.liverweight, all_of(material_cols_to_select)))

    material_cols_to_select_format <- rep("0", length(material_cols_to_select))
    names(material_cols_to_select_format) <- material_cols_to_select
    formats <- c("specimen.age_start" = "0", "specimen.age_end" = "0", "specimen.weight" = "0",
                 "bio.totallength" = "0", "bio.bodylength" = "0", "bio.gonadweight" = "0",
                 "bio.liverweight" = "0", material_cols_to_select_format)

    colnames <- c("AccNR", "Ålder från (år/K)", "Ålder till (år/K)", "Vikt (g)", "Totallängd (cm)",
                  "Kroppslängd (cm)", "Kön", "Gonadvikt (g)", "Levervikt (g)", material_cols_names)

  } else {
    shiny::showNotification("Kan endast hantera fisk.", duration = 10, type = "error")
  }

  df[, "acc.id"] <- df |> select(acc.id) |> unlist() |> as.character() |> esbaser::accdb_to_accnr()

  list(df = df, formats = formats, colnames = colnames)
}

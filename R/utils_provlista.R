#' Save Provlista to ESbase
#'
#' @param db A dataHolder containing $conn, and helpertables
#' @param selected A dataHolder containing $acc, $bio, $specimen, $material. As created in mod_provberedning_server.
#' @param provlista A dataHolder containing $dfs and $metas, $dfs a list of dataframes (one for each analysis) with
#' weight/provid information, and $metas holding information about the laboratory, analyst, etc
#' @param provberednings_protokoll A dataHolder containing metainformation such as $project, $provberedare, $beredningsdatum
#' @param prov_name Which provname to save: prov1, prov2 etc
save_provlista <- function(db, account, selected, provlista, provberednings_protokoll, prov_name) {
  if (is_blank(provberednings_protokoll$project)) {
    shiny::showNotification(paste0(prov_name, ": Inget projekt valt."), duration = 10, type = "warning")
    return()
  }

  if (is_blank(provberednings_protokoll$provberedare)) {
    shiny::showNotification(paste0(prov_name, ": Ingen provberedare vald."), duration = 10, type = "warning")
    return()
  }

  if (is_blank(provlista$metas[prov_name, "analytiker"])) {
    shiny::showNotification(paste0(prov_name, ": Ingen analytiker vald."), duration = 10, type = "warning")
    return()
  }

  if (is_blank(provlista$metas[prov_name, "analystyp"])) {
    shiny::showNotification(paste0(prov_name, ": Ingen analystyp vald."), duration = 10, type = "warning")
    return()
  }

  if (all(unlist(lapply(provlista$dfs[[prov_name]]$material_id, is_blank)))) {
    shiny::showNotification(paste0(prov_name, ": Finns inget prov."), duration = 10, type = "warning")
    return()
  }

  insertion <- esbaser::insert_analysisrecord(
    conn = db$conn,
    account_id = account$id,
    project_id = provberednings_protokoll$project,
    creator_id = provberednings_protokoll$provberedare,
    contact_id = provlista$metas[prov_name, "analytiker", drop = TRUE],
    date = provberednings_protokoll$beredningsdatum,
    shippingdate = NULL,
    analysis_type_id = provlista$metas[prov_name, "analystyp", drop = TRUE],
    result = "",
    analysis_type_note = "")
  analysisrecord_id <- insertion$new_row_id

  for (row in seq_len(nrow(provlista$dfs[[prov_name]]))) {
    mat_id <- provlista$dfs[[prov_name]][row, "material_id", drop = TRUE]
    if (!is_blank(mat_id)) {
      insertion <- esbaser::insert_analysisrecord_row(
        conn = db$conn,
        amount = provlista$dfs[[prov_name]][row, "provvikt", drop = TRUE],
        homogenate_amount = provlista$dfs[[prov_name]][row, "delvikt", drop = TRUE],
        tag = provlista$dfs[[prov_name]][row, "provid", drop = TRUE])
      record_row_id <- insertion$new_row_id

      esbaser::insert_analysisrecord_row_assn(conn = db$conn, analysisrecord_id = analysisrecord_id, analysisrecord_row_id = record_row_id)
      esbaser::insert_analysisrecord_material_assn(conn = db$conn, analysisrecord_row_id = record_row_id, material_id = mat_id)
    }
  }

  shiny::showNotification(paste0("Provberedning sparad för ", prov_name, "."))
  analysisrecord_id
}

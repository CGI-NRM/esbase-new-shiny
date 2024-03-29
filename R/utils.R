# Usefull funtion to pipe into when not all elements/columns contains data
paste_collapse <- function(x, collapse = ", ") {
  x <- unname(x)
  paste(x[x != "" & !is.na(x)], collapse = collapse)
}

# Useful when renaming |> rename_with(function(name) paste0("prefix", name))
rename_w_prefix <- function(x, prefix) {
  rename_with(x, function(name) paste0(prefix, name))
}

# Could maybe be moved to esbaser
repr_locality <- function(x, db) {
  x |>
  left_join(db$locality, by = join_by(locality_id == id)) |>
  left_join(db$county |> rename_w_prefix("county."), by = join_by(county_id == county.id)) |>
  left_join(db$province |> rename_w_prefix("province."), by = join_by(province_id == province.id)) |>
  left_join(db$coast |> rename_w_prefix("coast."), by = join_by(coast_id == coast.id)) |>
  select(name, closecity, county.swe_name, county.eng_name, coast.swe_name,
         coast.eng_name, province.swe_name, province.eng_name) |>
  apply(1, paste_collapse)
}

repr_species <- function(x, db) {
  x |>
  left_join(db$species |> rename_w_prefix("species."), by = join_by(species_id == species.id)) |>
  select(species.swe_name, species.eng_name, species.lat_name) |>
  apply(1, paste_collapse)
}

repr_catalog <- function(x, db) {
  x |>
  left_join(db$catalog |> rename_w_prefix("catalog."), by = join_by(catalog_id == catalog.id)) |>
  select(catalog.name) |>
  apply(1, paste_collapse)
}

repr_gender <- function(x, db) {
  x |>
  left_join(db$gender |> rename_w_prefix("gender."), by = join_by(gender_id == gender.id)) |>
  select(gender.code, gender.swe_name) |>
  apply(1, paste_collapse, collapse = "-")
}

is_blank <- function(x) {
  is.na(x) || is.null(x) || length(x) == 0 || x == ""
}

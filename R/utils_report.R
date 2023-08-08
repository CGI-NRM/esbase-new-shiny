#' Add help columns
#'
#' @param catalog The catalog of the accessions
#' @param species Some species have special, the species ids are expected to never change, this function might need to be
#' updated if they would change to handle special cases.
#' @return A vector with columns to add
biologdata_add_columns <- function(catalog, species) {
  cols <- c()
  if (catalog == 2) { # Fisk
    cols <- c(cols,
              "Gonader kvar (g) - frys",
              "Lever kvar (g) - frys")
    if (species == 528) { # Röding
      cols <- c(cols,
                "Skrott kvar (g) - frys",
                "Sparad mage (J/N) - frys",
                "Sparad otoliter (J/N) - otolitlåda")
    } else if (species == 500) { # Aborre
      cols <- c(cols,
                "Skrott kvar (g) - frys",
                "Parasit vikt (g) - kasserad",
                "Sparad mage (J/N) - frys",
                "Sparad otoliter (J/N) - otolitlåda")
    } else if (species == 511) { # Gädda
      cols <- c(cols,
                "Lever kvar LT (g)",
                "Muskel kvar (g) - frys",
                "Muskel kvar (g) - LT",
                "Sparad mage (J/N) - frys",
                "Sparad otoliter (J/N) - otolitlåda",
                "Övrigt cleithrum - otolitlåda",
                "Gällock (operculum) - otolitlåda")
    }
  }

  cols
}

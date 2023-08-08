mod_provberedning_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(id = ns("provberedning"),
             shiny::div(style = "margin: 20px",
                        shiny::fluidRow(
                          shiny::column(7,
                                        shiny::actionButton(
                                          inputId = ns("download_action_button"),
                                          label = "Skapa provberedningsrapport",
                                          icon = shiny::icon("download")),
                                        shiny::downloadButton(
                                          outputId = ns("download_report"),
                                          style = "visibility: hidden;")
                          ),
                          shiny::column(3,
                                        style = "padding-right: 0px;",
                                        shiny::textInput(
                                          inputId = ns("provberedning_id"),
                                          label = NULL,
                                          placeholder = "Provberedning: 2023-0..."),
                          ),
                          shiny::column(2,
                                        style = "padding-left: 0px;",
                                        shiny::actionButton(inputId = ns("provberedning_load"), label = "Ladda Provberedning")
                          )
                        ),
             ),
             shiny::wellPanel(
               shiny::selectizeInput(
                 inputId = ns("project"),
                 label = "Projekt /Program",
                 choices = c(""),
                 options = list(placeholder = "Projekt", highlight = FALSE),
                 selected = NA,
                 width = "100%"
               ),
               shiny::fluidRow(
                 shiny::column(6,
                               shiny::textInput(inputId = ns("accnr_start"), label = "AccNR från", placeholder = "[A]YYYY/XXXXX"),
                               shiny::textOutput(outputId = ns("accnr_start_message"))
                 ),
                 shiny::column(6,
                               shiny::textInput(inputId = ns("accnr_end"), label = "AccNR till", placeholder = "[A]YYYY/XXXXX"),
                               shiny::textOutput(outputId = ns("accnr_end_message"))
                 )
               ),
               shiny::textOutput(outputId = ns("accnr_selection_message")),
               DT::DTOutput(outputId = ns("accnr_selection_dt"))
             ),

             shiny::tabsetPanel(
               id = ns("tabs"), type = "tabs",
               shiny::tabPanel(title = "Biologdata",
                               shiny::wellPanel(
                                 mod_biologdata_ui(ns("biologdata"))
                               )
               ),
               shiny::tabPanel(title = "Material/Inventering",
                               shiny::wellPanel(
                                 mod_material_ui(ns("material"))
                               )
               ),
               shiny::tabPanel(title = "Provlista",
                               shiny::wellPanel(
                                 mod_provlista_ui(ns("provlista"))
                               )
               )
             )
             )
}

mod_provberedning_server <- function(id, db, account) {
  # A new holder to move into, to replace selected_accnrs, biologdata_table
  # $acc                - accession table/tibble of selected
  # $accs_db            - vector of selected accnrs, formated in database format
  # $acc_min            - formatted AYYYY/XXXXX
  # $acc_max            - formatted AYYYY/XXXXX
  # $bio                - clam/fish/mammal etc table/tibble of selected accession_id
  # $material           - all materials that points to selected accession_id (can be multilpe per accession)
  # $material_override  - override, what the user has entered
  # $specimen           - all specimen that points to selected accession_id (one per accession)
  # $update             - a reactive val that should be increased when this data is updated
  selected <- dataHolder(
    acc = tibble(),
    accs_db = character(0),
    acc_min = "",
    acc_max = "",
    bio = tibble(),
    material = tibble(),
    material_override = tibble(),
    material_override_backup = tibble(),
    specimen = tibble(),
    update = shiny::reactiveVal(0)
  )

  # A holder for combined biologdata
  # $df         - The combined data from the database
  # $override   - The data as the user has entered it
  # $formats    - A named vector with numbrojs
  # $colnames   - The pretty colnames
  biologdata <- dataHolder()

  # Containing provlista$dfs which is a list with prov-names as keys and the coresponding dataframe as values
  #                       and $metas which is a dataframe where the
  #                           rowsnames are prov-names, and the
  #                           columns are 'homogenat analyslab analystyp analytiker provtagningsinst vavnads'
  provlista <- dataHolder()

  # A holder for metadata for provberedning
  # $project
  # $beredningsdatum
  # $provberedare
  provberednings_meta <- dataHolder()

  # A holder for when a provberedning is loaded
  # $update triggers when the data has been updated and all modules should restore from the data
  restore <- dataHolder(
    accnr_min = "",
    accnr_max = "",
    metas = data.frame(),
    dfs = list(),
    provberednings_meta = NULL,
    update = shiny::reactiveVal(0)
  )

  shiny::moduleServer(id, function(input, output, session) {
    loginfo("mod_provberedning_server.R: module server start")

    # ---------- FUNCTIONS ----------
    update_select_inputs_with_stodlistor <- function() {
      logdebug("mod_provberedning.R - update_select_inputs_with_stodlistor: called")
      # Update project choices from stödlista
      projects_vector <- db$project |> select(id) |> unlist(use.names = FALSE)
      names(projects_vector) <- db$project |> select(number, name) |> apply(1, paste_collapse)
      names(projects_vector)[names(projects_vector) == ""] <- "-"
      shiny::updateSelectizeInput(session, "project", choices = projects_vector,
                                  selected = NA, server = TRUE)
      logfine("mod_provberedning.R - update_select_inputs_with_stodlistor: finished")
    }

    create_download_handler <- function() {
      logdebug("mod_provberedning.R - create_download_handler: called")
      content_wrapper <- function(file) {
        y <- as.character(year(today()))
        prevs <- grep(paste0("^", y, "-[0-9]+\\.rds$"), list.files("provberedningar/", pattern = y), value = TRUE) |> sort()

        print(prevs)

        if (length(prevs) == 0) {
          code <- paste0(y, "-0")
        } else {
          last <- prevs[length(prevs)]
          value <- substring(last, 6, nchar(last) - 4) |> as.numeric()
          code <- paste0(y, "-", value + 1)
        }

        saveRDS(
          list(
            metas = provlista$metas,
            dfs = provlista$dfs,
            accnr_min = selected$acc_min,
            accnr_max = selected$acc_max,
            provberednings_meta = list(
              project = provberednings_meta$project,
              provberedare = provberednings_meta$provberedare,
              beredningsdatum = provberednings_meta$beredningsdatum
            )),
          file = paste0("provberedningar/", code, ".rds")
        )

        report_content(file = file,
                       selected = selected,
                       biologdata = biologdata,
                       provlistas = provlista$dfs,
                       provlistas_metas = provlista$metas,
                       db = db,
                       provberedning_id = code
        )
      }

      output$download_report <- shiny::downloadHandler(
        filename = "report.pdf",
        content = content_wrapper
      )
    }

    render_dt_clean <- function(df, colnames) {
      logdebug("mod_provberedning.R - render_dt_clean: called")
      DT::renderDT(df,
                   options = list(dom = "tp"),
                   rownames = FALSE,
                   colnames = colnames
      )
    }

    deselect_selected_accnrs <- function() {
      logdebug("mod_provberedning.R - deselect_selected_accnrs: called")
      prev_data <- !is.null(selected$acc) && nrow(selected$acc) > 0

      selected$acc <- tibble()
      selected$acc_min <- ""
      selected$acc_max <- ""
      selected$accs_db <- character(0)
      selected$bio <- tibble()
      selected$specimen <- tibble()
      selected$material <- tibble()

      selected$bio_override <- tibble()
      selected$specimen_override <- tibble()
      selected$material_override <- tibble()

      if (prev_data) {
        selected$update(selected$update() + 1)
      }
      logfine("mod_provberedning.R - deselect_selected_accnrs: finished")
    }

    update_selected_accnrs <- function() {
      logdebug("mod_provberedning.R - update_selected_accnrs: called")
      shiny::req(input$accnr_start)
      shiny::req(input$accnr_end)

      if (!esbaser::accnr_validate(input$accnr_start) ||
          !esbaser::accnr_validate(input$accnr_end)) {
        return()
      }

      if (identical(input$accnr_start, selected$acc_min) &&
          identical(input$accnr_end, selected$acc_max)) {
        return()
      }

      accnr_start_tib <- esbaser::accnr_parse(input$accnr_start)
      accnr_end_tib <- esbaser::accnr_parse(input$accnr_end)

      if (accnr_start_tib$letter != accnr_end_tib$letter) {
        output$accnr_selection_message <- shiny::renderText(
          "AccNR start and end are not part of the same series. Ensure they start with the same letter.")
        deselect_selected_accnrs()
        return()
      } else if (accnr_start_tib$year != accnr_end_tib$year) {
        output$accnr_selection_message <- shiny::renderText(
          "AccNR start and end are not from the same year.")
        deselect_selected_accnrs()
        return()
      } else if (accnr_end_tib$value < accnr_start_tib$value) {
        output$accnr_selection_message <- shiny::renderText("AccNR start comes after end. Ensure AccNR start is before AccNR end.")
        deselect_selected_accnrs()
        return()
      } else if (accnr_end_tib$value - accnr_start_tib$value >= 256) {
        output$accnr_selection_message <- shiny::renderText("To large range, select fewer than 256.")
        deselect_selected_accnrs()
        return()
      } else {
        output$accnr_selection_message <- shiny::renderText("")
      }

      accession_data <- esbaser::get_accessions_between(db$conn, input$accnr_start, input$accnr_end)

      series_tib <- esbaser::accnr_add(accnr_start_tib, seq(0, accnr_end_tib$value - accnr_start_tib$value))
      series <- esbaser::accnr_sprint(series_tib)
      series_db <- esbaser::accnr_db_sprint(series_tib)

      catalog_id <- accession_data |> select(catalog_id) |> unique()
      locality_id <- accession_data |> select(locality_id) |> unique()
      species_id <- accession_data |> select(species_id) |> unique()

      if (!identical(
          series_db |> as.character() |> sort(),
          accession_data |> select(id) |> unlist() |> as.character() |> sort()
      )) {
        output$accnr_selection_message <- shiny::renderText("Not all AccNRs exists in the database.")
        output$accnr_selection_dt <- render_dt_clean(
          data.frame(
            missing = setdiff(series_db, accession_data |> select(id)) |> esbaser::accdb_to_accnr()
          ),
          colnames = c("Saknas i ESbase"))
        deselect_selected_accnrs()
        return()
      } else if (catalog_id |> nrow() != 1) {
        output$accnr_selection_message <- shiny::renderText("Accessionsnummrerna tillhör olika kataloger.")
        output$accnr_selection_dt <- render_dt_clean(
          data.frame(
            id = accession_data |> select(id) |> unlist() |> esbaser::accdb_to_accnr(),
            catalog = accession_data |> select(catalog_id) |> repr_catalog(db)
          ),
          colnames = c("AccNR", "Katalog"))
        deselect_selected_accnrs()
        return()
      } else if (species_id |> nrow() != 1) {
        output$accnr_selection_message <- shiny::renderText("Accessionsnummrerna tillhör olika arter.")
        output$accnr_selection_dt <- render_dt_clean(
          data.frame(
            id = accession_data |> select(id) |> unlist() |> esbaser::accdb_to_accnr(),
            species = accession_data |> select(species_id) |> repr_species(db)
          ),
          colnames = c("AccNR", "Art"))
        deselect_selected_accnrs()
        return()
      } else if (locality_id |> nrow() != 1) {
        output$accnr_selection_message <- shiny::renderText("Accessionsnummrerna tillhör olika lokaler.")
        output$accnr_selection_dt <- render_dt_clean(
          data.frame(
            id = accession_data |> select(id) |> unlist() |> esbaser::accdb_to_accnr(),
            locality = accession_data |> select(locality_id) |> repr_locality(db)
          ),
          colnames = c("AccNR", "Lokal"))
        deselect_selected_accnrs()
        return()
      } else {
        output$accnr_selection_message <- shiny::renderText("")
        output$accnr_selection_dt <- DT::renderDT(data.frame())
      }

      selected$acc_min <- input$accnr_start
      selected$acc_max <- input$accnr_end
      selected$acc <- accession_data
      selected$accs_db <- series_db

      if (catalog_id == 0) { # Alla
        shiny::showNotification("Kan inte hämta biologdata för 'Alla'.", duration = 10, type = "warning")
        selected$bio <- tibble(accession_id = series_db)
      } else if (catalog_id == 1) { # Fågel
        selected$bio <- esbaser::get_bird_between(db$conn, selected$acc_min, selected$acc_max)
      } else if (catalog_id == 2) { # Fisk
        selected$bio <- esbaser::get_fish_between(db$conn, selected$acc_min, selected$acc_max)
      } else if (catalog_id == 3) { # Däggdjur
        selected$bio <- esbaser::get_mammal_between(db$conn, selected$acc_min, selected$acc_max)
      } else if (catalog_id == 4) { # Ägg
        selected$bio <- esbaser::get_egg_between(db$conn, selected$acc_min, selected$acc_max)
      } else if (catalog_id == 5) { # Mossa
        shiny::showNotification("Kan inte hämta biologdata för 'Mossa'.", duration = 10, type = "warning")
        selected$bio <- tibble(accession_id = series_db)
      } else if (catalog_id == 6) { # Mussla
        selected$bio <- esbaser::get_clam_between(db$conn, selected$acc_min, selected$acc_max)
      } else if (catalog_id == 7) { # Övrigt
        shiny::showNotification("Kan inte hämta biologdata för 'Övrigt'.", duration = 10, type = "warning")
        selected$bio <- tibble(accession_id = series_db)
      } else {
        shiny::showNotification(paste0("Okänt catalog_id: ", catalog_id, ". Kan inte hämta biologdata."), duration = 10, type = "warning")
        selected$bio <- tibble(accession_id = series_db)
      }

      selected$specimen <- esbaser::get_specimen_between(db$conn, selected$acc_min, selected$acc_max)
      selected$material <- esbaser::get_material_between(db$conn, selected$acc_min, selected$acc_max)

      selected$bio_override <- selected$bio
      selected$specimen_override <- selected$specimen
      selected$material_override <- selected$material
      selected$material_override_backup <- tibble()

      selected$update(selected$update() + 1)
      logfine("mod_provberedning.R - update_selected_accnrs: finished")
    }

    # ---------- ONE-TIME SETUP ----------
    update_select_inputs_with_stodlistor()
    create_download_handler()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$accnr_start, {
      logdebug("mod_provberedning.R - observeEvent(input$accnr_start, {}): called")
      if (is.null(input$accnr_start) || input$accnr_start == "") {
        output$accnr_start_message <- shiny::renderText("")
        deselect_selected_accnrs()
        return()
      }

      if (isFALSE(esbaser::accnr_validate(input$accnr_start))) {
        output$accnr_start_message <- shiny::renderText(
          "Ogiltigt accessionsnummer. Vänligen fyll i enligt formen '[ABCDGHLXP]YYYY/XXXXX' eller '[ABCDGHLXP]YYYYXXXXX'")
        deselect_selected_accnrs()
        return()
      } else {
        output$accnr_start_message <- shiny::renderText("")
      }

      update_selected_accnrs()
      logfine("mod_provberedning.R - observeEvent(input$accnr_start, {}): finished")
    })

    shiny::observeEvent(input$accnr_end, {
      logdebug("mod_provberedning.R - observeEvent(input$accnr_end, {}): called")
      if (is.null(input$accnr_end) || input$accnr_end == "") {
        output$accnr_end_message <- shiny::renderText("")
        deselect_selected_accnrs()
        return()
      }

      if (isFALSE(esbaser::accnr_validate(input$accnr_end))) {
        output$accnr_end_message <- shiny::renderText(
          "Ogiltigt accessionsnummer. Vänligen fyll i enligt formen '[ABCDGHLXP]YYYY/XXXXX' eller '[ABCDGHLXP]YYYYXXXXX'")
        deselect_selected_accnrs()
        return()
      } else {
        output$accnr_end_message <- shiny::renderText("")
      }

      update_selected_accnrs()
      logfine("mod_provberedning.R - observeEvent(input$accnr_end, {}): finished")
    })

    shiny::observeEvent(input$download_action_button, {
      logdebug("mod_provberedning.R - observeEvent(input$download_action_button, {}): called")
      if (length(selected$accs_db) == 0) {
        shiny::showNotification("Inga valda accessionsnummer", duration = 10, type = "warning")
        return()
      }

      shinyjs::runjs(paste0("document.getElementById('", session$ns("download_report"), "').click()"))
      logfine("mod_provberedning.R - observeEvent(input$download_action_button, {}): finished")
    })

    shiny::observeEvent(input$project, {
      logdebug("mod_provberedning.R - observeEvent(input$project, {}): called")
      provberednings_meta$project <- ifelse(input$project == "", NA, input$project)
      logfine("mod_provberedning.R - observeEvent(input$project, {}): finished")
    })

    shiny::observeEvent(input$provberedning_load, {
      logdebug("mod_provberedning.R - observeEvent(input$provberedning_load, {}): called")
      if (!stringr::str_detect(input$provberedning_id, "^[0-9]{4}-[0-9]*$")) {
        shiny::showNotification("Ogiltigt format på provberedningsid, skriv i på formen 'YYYY-X...'", duration = 10, type = "warning")
        return()
      }

      path <- paste0("provberedningar/", input$provberedning_id, ".rds")
      if (!file.exists(path)) {
        shiny::showNotification(
          paste0("Kan inte hitta sparad provberedning med id: ", input$provberedning_id, "."),
          duration = 10, type = "warning")
        return()
      }

      res <- readRDS(path)
      restore$accnr_min <- res$accnr_min
      restore$accnr_max <- res$accnr_max
      restore$metas <- res$metas
      restore$dfs <- res$dfs
      restore$provberednings_meta <- res$provberednings_meta

      shiny::updateTextInput(session, inputId = "accnr_start", value = restore$accnr_min)
      shiny::updateTextInput(session, inputId = "accnr_end", value = restore$accnr_max)
      shiny::updateSelectizeInput(session, "project",
                                  selected = ifelse(is.null(restore$provberednings_meta$project), NA, restore$provberednings_meta$project))

      restore$update(restore$update() + 1)

      shiny::showNotification(paste0("Hämtat provberedning: ", input$provberedning_id, "."), duration = 10)
      logfine("mod_provberedning.R - observeEvent(input$provberedning_load, {}): finished")
    })

    # ---------- MODULE SERVERS ----------
    mod_biologdata_server("biologdata",
                          db = db,
                          account = account,
                          selected = selected,
                          biologdata = biologdata,
                          provberednings_meta = provberednings_meta,
                          restore = restore)
    mod_material_server("material",
                        db = db,
                        account = account,
                        selected = selected)
    mod_provlista_server("provlista",
                         db = db,
                         account = account,
                         selected = selected,
                         provlista = provlista,
                         provberednings_meta = provberednings_meta,
                         restore = restore)
  })
}

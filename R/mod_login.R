mod_login_ui <- function(id) {
  ns <- shiny::NS(id)

  js_cookie_code <- paste0('
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie === "undefined") {
      var cookie = "";
    }
    Shiny.setInputValue("', ns("jscookie"), '", cookie);
  }
  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params), { expires: 31, sameSite: "Strict" });
    Shiny.setInputValue("', ns("jscookie"), '", params);
  }
  shinyjs.rmcookie = function(params) {
    Cookies.remove("id");
    Shiny.setInputValue("jscookie", "");
    Shiny.setInputValue("', ns("jscookie"), '", "");
  }
  ')

  shiny::tagList(
    shinyjs::extendShinyjs(text = js_cookie_code, functions = c("getcookie", "setcookie", "rmcookie")),
    shiny::uiOutput(ns("page")),
    shiny::span(
      shiny::actionButton(
        inputId = ns("logout"),
        icon = shiny::icon("sign-out-alt"),
        label = "Logga ut"),
      style = "position: absolute; right: 20px; top: 20px;"
    )
  )
}

mod_login_server <- function(id, db, account) {
  # A holder for wether to the user has logged in to stop it from logging in multiple times
  status <- dataHolder(
    logged_in = FALSE
  )

  shiny::moduleServer(id, function(input, output, session) {

    js_login_on_enter <- paste0('
                                $(document).keyup(function(event) {
                                  if ($("#', session$ns("passwd"), '").is(":focus") && (event.keyCode == 13)) {
                                    $("#', session$ns("login"), '").click();
                                  }
                                });
                                ')

    loginpage <- shiny::div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                            shiny::wellPanel(
                              shiny::tags$script(shiny::HTML(js_login_on_enter)),
                              shiny::tags$h2("Logga in", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                              shiny::textInput(session$ns("user_name"), placeholder = "Användarnamn",
                                               label = shiny::tagList(shiny::icon("user"), "Användarnamn")),
                              shiny::passwordInput(session$ns("passwd"), placeholder = "Lösenord",
                                                   label = shiny::tagList(shiny::icon("unlock-alt"), "Lösenord")),
                              shiny::br(),
                              shiny::div(
                                style = "text-align: center;",
                                shiny::actionButton(session$ns("login"), "LOGGA IN", style = "color: white; background-color:#3c8dbc;
padding: 10px 15px; width: 150px; cursor: pointer;
font-size: 18px; font-weight: 600;"),
                                shinyjs::hidden(
                                  shiny::div(id = session$ns("nomatch"),
                                             shiny::tags$p("Ogiltigt användarnamn eller lösenord!",
                                                           style = "color: red; font-weight: 600;
                                                           padding-top: 5px;font-size:16px;",
                                                           class = "text-center"))),
                              ))
    )

    # ---------- FUNCTIONS ----------
    render_provberedning <- function() {
      output$page <- shiny::renderUI(mod_provberedning_ui(session$ns("provberedning")))
      mod_provberedning_server("provberedning", db = db, account = account)
    }

    # ---------- ONE-TIME SETUP ----------
    shinyjs::js$getcookie()

    # ---------- OBSERVE EVENTS ----------
    shiny::observeEvent(input$logout, {
      logdebug("mod_login.R - observeEvent(input$logout, {}): called")
      if (uuid::UUIDvalidate(input$jscookie) && file.exists(paste0("active_sessions/", input$jscookie))) {
        file.remove(paste0("active_sessions/", input$jscookie))
      }
      shinyjs::js$rmcookie()
      status$logged_in <- FALSE
      shinyjs::refresh()
      logfine("mod_login.R - observeEvent(input$logout, {}): finished")
    })

    shiny::observeEvent(input$jscookie, {
      logdebug("mod_login.R - observeEvent(input$jscookie, {}): called")
      logfiner(paste0("mod_login.R - observeEvent(input$jscookie, {}): login attempt with session uuid: ", input$jscookie))
      if (!is.null(input$jscookie) &&
          uuid::UUIDvalidate(input$jscookie) &&
          file.exists(paste0("active_sessions/", input$jscookie))) {
        if (status$logged_in) {
          return()
        }
        account$id <- as.numeric(readLines(paste0("active_sessions/", input$jscookie)))
        logfiner(paste0("mod_login.R - observeEvent(input$jscookie, {}): logged in to user with id: ", account$id))
        status$logged_in <- TRUE
        render_provberedning()
      } else {
        output$page <- shiny::renderUI(loginpage)

        shiny::observeEvent(input$login, {
          logdebug("mod_login.R - observeEvent(input$login, {}): called")
          if (status$logged_in) {
            return()
          }
          if (!is.null(input$user_name) && !is.null(input$passwd)) {
            user <- db$person |> filter(username == input$user_name & password == input$passwd)

            if (nrow(user) > 0) {
              account$id <- user[1, "id", drop = TRUE]

              if (is.numeric(account$id)) {
                uuid_session <- uuid::UUIDgenerate(use.time = FALSE, n = 1L, output = "string")
                writeLines(as.character(account$id), paste0("active_sessions/", uuid_session))

                  shinyjs::js$setcookie(uuid_session)
                  logfiner(
                    paste0(
                      "mod_login.R - observeEvent(input$login, {}): logged in to user with id: ",
                      account$id,
                      ", using session: ",
                      uuid_session))
                  render_provberedning()
                } else {
                  shinyjs::show(id = "lowpermisions", anim = TRUE, time = 1, animType = "fade")
                  shinyjs::delay(3000, shinyjs::hide(id = "lowpermisions", anim = TRUE, time = 1, animType = "fade"))
                }
              } else {
                logfiner("mod_login.R - observeEvent(input$login, {}): account$id not numeric, cannot login")
                shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
              }
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
          logfine("mod_login.R - observeEvent(input$login, {}): finished")
        })
      }
      logfine("mod_login.R - observeEvent(input$jscookie, {}): finished")
    }, once = TRUE)
  })
}

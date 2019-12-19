library(shiny)

# which fields get saved
fieldsAll <- c("name", "country", "sidemount", "solo",
               "divetime", "divedepth", "cylinder_type")

# which fields are mandatory
fieldsMandatory <- c("name", "country")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))

  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# directory where responses get stored
responsesDir <- file.path("data")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

# usernames that are admins
adminUsers <- c("admin", "prof")

# info for sharing this app on facebook/twitter
share <- list(
  title = "dive logbook Shiny app",
  url = "https://github.com/RS-eco",
  description = "Shiny app that allows users to submit dive logs."
)

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "dive logbook Shiny app",
    tags$head(
      tags$link(rel = "shortcut icon", type="image/x-icon", href="http://daattali.com/shiny/img/favicon.ico"),

      # Facebook OpenGraph tags
      tags$meta(property = "og:title", content = share$title),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:url", content = share$url),
      tags$meta(property = "og:image", content = share$image),
      tags$meta(property = "og:description", content = share$description),

      # Twitter summary cards
      tags$meta(name = "twitter:card", content = "summary"),
      tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
      tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
      tags$meta(name = "twitter:title", content = share$title),
      tags$meta(name = "twitter:description", content = share$description),
      tags$meta(name = "twitter:image", content = share$image)
    ),
    div(id = "header",
        h1("dive logbook Shiny app"),
        h4("This app is a supplement to my",
           a(href = "http://deanattali.com/2015/06/14/mimicking-google-form-shiny/",
             "blog post on the topic")
        ),
        strong(
          span("Created by "),
          a("RS-eco"), #, href = "http://deanattali.com"),
          HTML("&bull;"),
          span("Code"),
          a("on GitHub", href = "https://github.com/RS-eco"))
    ),

    fluidRow(
      column(6,
             div(
               id = "form",

               textInput("name", labelMandatory("Name of dive site"), ""),
               textInput("country", labelMandatory("Country")),
               checkboxInput("sidemount", "I've dived using Sidemount", FALSE),
               checkboxInput("solo", "I've dived solo", FALSE),
               sliderInput("divetime", "Dive time", 0, 120, 1, ticks = FALSE),
               sliderInput("divedepth", "Dive depth", 0, 60, 1, ticks = FALSE),

               selectInput("cylinder_type", "Cylinder used",
                           c("10l",  "11l", "100cft", "2*10l")),

               actionButton("submit", "Submit", class = "btn-primary"),

               shinyjs::hidden(
                 span(id = "submit_msg", "Submitting..."),
                 div(id = "error",
                     div(br(), tags$b("Error: "), span(id = "error_msg"))
                 )
               ),
               sliderInput("bins",
                           "Number of bins:",
                           min = 1, max = 50, value = 30)
             ),

             shinyjs::hidden(
               div(
                 id = "thankyou_msg",
                 h3("Thanks, your response was submitted successfully!"),
                 actionLink("submit_another", "Submit another response")
               )
             )
      ),
      column(6,
             uiOutput("adminPanelContainer"),
             # Show a plot of the generated distribution
             plotOutput("distPlot")
      )
    )
  ),
  server = function(input, output, session) {

    # Enable the Submit button when all mandatory fields are filled out
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)

      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })

    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })

    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {

      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")

      # Save the data (show an error message in case of error)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })

    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })

    # render the admin panel
    output$adminPanelContainer <- renderUI({
      if (!isAdmin()) return()

      div(
        id = "adminPanel",
        h2("Previous responses (only visible to admins)"),
        downloadButton("downloadBtn", "Download responses"), br(), br(),
        DT::dataTableOutput("responsesTable"), br())
    })

    # determine if current user is admin
    isAdmin <- reactive({
      is.null(session$user) || session$user %in% adminUsers
    })

    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)

      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })

    # Show the responses in the admin table
    output$responsesTable <- DT::renderDataTable({
      data <- loadData()
      data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
      DT::datatable(
        data,
        rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE)
      )
    })

    # Allow user to download responses
    output$downloadBtn <- downloadHandler(
      filename = function() {
        sprintf("mimic-google-form_%s.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE)
      }
    )
  }
)

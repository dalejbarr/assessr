#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

outfile <- getShinyOption("outfile")
htbl <- getShinyOption("htbl")

sub_id <- as.character(htbl$sub_id)
names(sub_id) = paste(sub_id, " [ ]")
texts <- htbl$html
names(texts) <- sub_id

scores <- rep(NA_integer_, length(sub_id))
fbk <- list()
first <- TRUE

ftbl <- tibble::tibble(texts = htbl$html,
                       sub_id = sub_id)

if (file.exists(outfile)) {
  ff <- readRDS(outfile)
  ff2 <- dplyr::mutate(ff, sub_id = as.character(sub_id))
  ff3 <- dplyr::inner_join(ff2, ftbl, "sub_id")
  sub_id <- as.character(ff3$sub_id)
  scores <- ff3$score
  have_fbk <- !sapply(ff3$fbk, is.null)
  fbk <- ff3$fbk[have_fbk]
  names(fbk) <- ff3$sub_id[have_fbk]
  texts <- ff3$texts
  names(texts) <- ff3$sub_id
  names(sub_id) <- paste0(sub_id, " [",
                          ifelse(is.na(ff3$score), " ", ff3$score),
                          "]")
}

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput("sub_id",
                             "Submission",
                             choices = sub_id),
                 textOutput("where"),
                 actionButton("fwd",
                              "Next"),
                 actionButton("sort",
                              "Sort"),
                 actionButton("quit",
                              "Quit")
                 ),
    
    mainPanel(width = 9,
              uiOutput("id"),
              htmlOutput("inc"),
              sliderInput("score", "Score", 0L, 5L,
                          ifelse(is.na(scores[1]), 3L, scores[1]),
                          1L),
              textAreaInput("feedback", "Feedback",
                            fbk[[sub_id[1]]],
                            width = 600),
              actionButton("update", "Update")
              )
  )
)

server <- function(input, output, session) {
  save_all <- function() {
    t1 <- tibble::tibble(sub_id = as.integer(rv$sub_id),
                         score = rv$scores)
    t2 <- tibble::tibble(sub_id = as.integer(names(rv$fbk)),
                         fbk = rv$fbk)
    saveRDS(dplyr::left_join(t1, t2, "sub_id"), outfile)
  }

  rv <- reactiveValues(
    texts = texts,
    sub_id = sub_id,
    fbk = fbk,
    scores = scores)

  output$where <- renderText({
    paste0(which(rv$sub_id == input$sub_id), " of ", length(rv$sub_id))
  })
  
  output$id <- renderUI({
    h3(input$sub_id)
  })
  
  output$inc <- renderUI({
    withMathJax(HTML(rv$texts[[input$sub_id]]))
  })

  observeEvent(input$fwd, {
    ix <- which(rv$sub_id == input$sub_id)
    if ((ix + 1L) <= length(rv$sub_id))
      updateSelectInput(session, "sub_id", selected = rv$sub_id[ix + 1L])
  })
  
  observeEvent(input$update, {
    ix <- which(rv$sub_id == input$sub_id)
    ## update values
    rv$scores[ix] <- input$score
    rv$fbk[rv$sub_id[ix]] <- input$feedback
    save_all()
    
    names(rv$sub_id)[ix] <- paste0(
      rv$sub_id[ix], " [", ifelse(is.na(rv$scores)[ix], " ",
                                  rv$scores[ix]), "]")

    if ((ix + 1L) <= length(rv$sub_id)) {
      newfbk <- rv$fbk[[rv$sub_id[ix + 1L]]]

      updateSelectInput(
        session, "sub_id",
        choices = rv$sub_id,
        selected = rv$sub_id[ix + 1L]
      )

      updateTextAreaInput(
        session, "feedback",
        value = ifelse(is.null(newfbk), "", newfbk)
      )
    } else {
      updateSelectInput(
        session, "sub_id",
        choices = rv$sub_id,
        selected = rv$sub_id[ix]
      )
    }
    
    ##updateSliderInput(session, "score",
    ##                  value = ifelse(is.na(rv$scores[ix + 1L]), 3L,
    ##                              rv$scores[ix + 1L]))
  })

  observeEvent(input$sub_id, {
    ix <- which(rv$sub_id == input$sub_id)
    sub <- rv$sub_id[ix]
    updateSliderInput(session, "score",
                      value = ifelse(is.na(rv$scores[ix]), 3L,
                                     rv$scores[ix]))
    updateTextAreaInput(session, "feedback",
                        value = ifelse(is.null(rv$fbk[[sub]]), "",
                                       rv$fbk[[sub]]))
  })
  
  observeEvent(input$sort, {
    ix <- order(rv$scores)
    rv$texts <- rv$texts[ix]
    rv$scores <- rv$scores[ix]
    rv$sub_id <- rv$sub_id[ix]
    updateSelectInput(session, "sub_id", choices = rv$sub_id)
  })

  observeEvent(input$quit, {
    save_all()
    stopApp(outfile)
  })
}

shinyApp(ui = ui, server = server)

uname <- function(x, t, s, z = NULL) {
  if (is.null(z))
    paste(x, t, s, sep = "___")
  else
    paste(x, t, s, z, sep = "___")
}

#' Visit a submission file
#'
#' @param files a vector with names of submission files
#' @param x moodle id number (see \code{\link{moodle_id}})
#' @details Finds the filename within \code{files} that corresponds to
#'   a given moodle id, and opens it with \code{file.edit}
#' @export
visit <- function(files, x) {
  file.edit(files[which(moodle_id(files) == x)])
}

#' Browse assessment
#'
#' View assessment code and figure output and modify assessment values
#' in a shiny gadget
#'
#' @importFrom magrittr %>%
#' @param x filename of the assessment results \code{.rds} file or the tibble contained in such file
#' @details launches a shiny gadget allowing the assessor to change the auto-generated values
#' @return table containing the new values and feedback for the task
#' @export
browse_assessment <- function(x) {
  ## set things up
  ## we will potential modify columns vars and fbk. keep the other cols
  if (is.character(x)) {
    ares <- readRDS(x)
  } else if (inherits(x, "data.frame")) {
    ares <- x
  } else {
    stop("unrecognized argument for 'x'; must be a character string or tibble")
  }
  
  keep_cols <- setdiff(names(ares), c("sub_id", "task", "vars", "fbk"))
  orig_colorder <- names(ares)
  all_tasks <- unique(ares[["task"]])
  
  ## make tabset panels
  ## each task will be defined in output$task_data
  tp <-
    purrr::map(all_tasks,
               ~ miniUI::miniTabPanel(.x,
                                      miniUI::miniContentPanel(shiny::uiOutput(.x, ))
                                      )
               )
  ## make tabstrip
  ts <- do.call(miniUI::miniTabstripPanel,#  tp)
                c(list(id = "tabpanel"), tp))
  
  ## pull together to create the UI
  ui <- do.call(miniUI::miniPage,
                c(list(miniUI::gadgetTitleBar("Assessment Browser")),
                  ts))  

  ui2 <- miniUI::miniPage(
                   miniUI::gadgetTitleBar("Assessment B"),
                   miniUI::miniTabstripPanel(id = "tabpanel",
                                             miniUI::miniTabPanel(miniUI::miniContentPanel(shiny::uiOutput("t1"))),
                                             miniUI::miniTabPanel(miniUI::miniContentPanel(shiny::uiOutput("t2")))
                                             )
                 )
  
  server <- function(input, output, session) {
    op <-
      purrr::map(all_tasks, function(t) {
        ares_task_unsort <- ares[ares[["task"]] == t, , drop = FALSE]
        ares_task <- ares_task_unsort %>%
          dplyr::mutate(sort_str = purrr::map_chr(vars,
                                                  ~ paste(paste(.x[["var"]], .x[["value"]]),
                                                          collapse = "_"))) %>%
          dplyr::arrange(sort_str, sub_id) %>%
          dplyr::select(-sort_str)
        ##browser()
        if (is.null(ares_task[["html"]])) {
          ares_task[["html"]] <- rep("", length(ares_task[["sub_id"]]))
        }
        shiny::renderUI({
          ilist <- purrr::pmap(list(ares_task[["sub_id"]],
                                    ares_task[["code"]],
                                    ares_task[["fbk"]],
                                    ares_task[["vars"]],
                                    ares_task[["fig"]],
                                    ares_task[["html"]]),
                               function(x, y, z, v, f, h) {
                                 s1 <- list(shiny::hr(),
                                            if (h == "") {
                                              shiny::p(x)
                                            } else {
                                              shiny::a(x,
                                                       href = paste0("file://", file.path(getwd(), h)),
                                                       target = "_blank")
                                            },
                                            ##ifelse(h != "",
                                            ##       shiny::a(href = h, x),
                                            ##       shiny::p(x)),
                                            ##shiny::p(x), 
                                            shiny::pre(paste(y,
                                                             collapse = "\n")))
                                 s2 <- list()
                                 if (f != "") {
                                   ## browser()
                                   if (substr(f, 1, 10) == "data:image") {
                                     s2 <- list(shiny::img(src = f))
                                   } else {
                                     s2 <- list(HTML(f))
                                   }
                                 }
                                 s3 <- list(shiny::textAreaInput(uname("fbk",
                                                                       x, t),
                                                                 NULL, z,
                                                                 width = "500px",
                                                                 height = "70px"),
                                            purrr::map2(v[["var"]], v[["value"]],
                                                        ~ shiny::checkboxInput(uname("ci", x, t, .x),
                                                                               .x, .y)))
                                 c(s1, s2, s3)
                               })
          do.call(shiny::tagList, purrr::flatten(ilist))
        })
      })
    names(op) <- all_tasks

    for (n in all_tasks) {
      output[[n]] <- op[[n]]
    }

    for (i in unique(ares[["sub_id"]])) {
      for (j in all_tasks) {
        local({
          ii <- i
          jj <- j
          observeEvent(input[[paste0("ab_", jj, "_", ii)]],
          {
            htm <- ares[["html"]][min(which(ares[["sub_id"]] == ii))]
            if (file.exists(htm)) {
              browseURL(htm)
            }
          })
        })
      }
    }
    
    shiny::observeEvent(input$tabpanel, {
      ## print(input$tabpanel)
      ## cat(names(input), "\n")
      ## cat("*** tabpanel:", input$tabpanel, "\n\n")
    })
    
    shiny::observeEvent(input$done, {
      ## Process all the things and return the values
      ## browser()
      ci_ix <- grep("^ci___[0-9]+___", names(input), value = TRUE)
      ff <- strsplit(ci_ix, "___") %>%
        purrr::transpose() %>%
        purrr::map(purrr::flatten_chr) %>%
        purrr::modify_at(2, as.integer) %>%
        `[`(-1)
        names(ff) <- c("sub_id", "task", "var")

        ## replace old with new values  
        res <- tibble::as_tibble(ff) %>%
          dplyr::mutate(value = purrr::map_lgl(ci_ix, ~ `[[`(input, .x)))
        orig_data <- ares %>%
          dplyr::select(sub_id, task, vars) %>%
          tidyr::unnest()
          new_vals <- dplyr::setdiff(res, orig_data)
          vars <- dplyr::bind_rows(dplyr::anti_join(orig_data, new_vals,
                                                    c("sub_id", "task", "var")),
                                   new_vals) %>%
            dplyr::group_by(sub_id, task) %>%
            tidyr::nest(.key = "vars")

          ## now do the same for feedback
          fbk_ix <- grep("^fbk___[0-9]+___.*$", names(input), value = TRUE)
          ff <- strsplit(fbk_ix, "___") %>%
            purrr::transpose() %>%
            purrr::map(purrr::flatten_chr) %>%
            purrr::modify_at(2, as.integer) %>%
            `[`(-1)
            names(ff) <- c("sub_id", "task")

            fbk2 <- tibble::as_tibble(ff) %>%
              dplyr::mutate(fbk = purrr::map_chr(fbk_ix, ~ `[[`(input, .x)))
            orig_fbk <- ares[, c("sub_id", "task", "fbk")]
            new_fbk <- dplyr::setdiff(fbk2, orig_fbk)
            fbk3 <- dplyr::bind_rows(dplyr::anti_join(orig_fbk, new_fbk,
                                                      c("sub_id", "task")),
                                     new_fbk)
            
            ## combine into the master table and return
            final <- dplyr::inner_join(ares[, c("sub_id", "task", keep_cols)],
                                       vars, c("sub_id", "task")) %>%
              dplyr::inner_join(fbk3, c("sub_id", "task")) %>%
              dplyr::arrange(sub_id, task)
            
            shiny::stopApp(final[, orig_colorder])
    })
  }
  
  viewer <- shiny::browserViewer()
  shiny::runGadget(ui, server, viewer = viewer)
}

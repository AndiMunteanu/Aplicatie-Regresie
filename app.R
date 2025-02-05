library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readxl)
library(ciTools)
library(shinyjs)

# creates a shiny app that reads an excel file containing a dataset with at least
# two columns and performs the following regressions: linear, logistic, exponential,
# gaussian and quadric. The user will be able to upload the dataset, to choose
# the columns to be used in the regressions.
regression_functions <- list(
    "liniară" = function(x, y) {
        glm(y ~ x, family = gaussian(link = "identity"))
    },
    "exponențială" = function(x, y) {
        y[y == 0] <- 1e-10
        glm(y ~ x, family = gaussian(link = "log"))
    },
    "quadratică" = function(x, y) {
        glm(y ~ poly(x, 2), family = gaussian(link = "identity"))
    }
)

coef_functions <- list(
    "liniară" = function(coef, col_names) {
        y <- col_names[2]
        x <- col_names[1]
        paste0("<strong>", y, " = ", coef[2], " * ", x, " + ", coef[1], "<br>sau<br>", "y = ", coef[2], " * x + ", coef[1], "</strong>")
    },
    "exponențială" = function(coef, col_names) {
        y <- col_names[2]
        x <- col_names[1]
        paste0("<strong>", y, " = exp(", coef[1], " + ", coef[2], " * ", x, ")", "<br>sau<br>", "y = exp(", coef[1], " + ", coef[2], " * x)</strong>")
    },
    "quadratică" = function(coef, col_names) {
        y <- col_names[2]
        x <- col_names[1]
        paste0("<strong>", y, " = ", coef[3], " * ", x, "^2 + ", coef[2], " * ", x, " + ", coef[1], "<br>sau<br>", "y = ", coef[3], " * x^2 + ", coef[2], " * x + ", coef[1], "</strong>")
    }
)

ui <- fluidPage(
    useShinyjs(),
    titlePanel("Regresie liniară"),
    wellPanel(
        splitLayout(
            fileInput(
                "fisier",
                "Alege fișierul excel",
                accept = c(".xlsx", ".xls")
            ),
            checkboxInput(
                "antet",
                "Are antet?",
                value = TRUE
            )
        ),
        DTOutput("tabel")
    ),
    sidebarLayout(
        sidebarPanel(
            selectInput("x", "Alege coloana x", choices = NULL),
            selectInput("y", "Alege coloana y", choices = NULL),
            actionButton("calcul", "Calculează regresiile")
        ),
        mainPanel(
            downloadButton(
                "download",
                "Descarcă graficele",
                class = "btn-danger"
            ),
            HTML("<br><br>"),
            do.call(
                tagList,
                lapply(names(regression_functions), function(x) {
                    splitLayout(
                        cellWidths = c("60%", "40%"),
                        plotOutput(paste0("plot_", x)),
                        htmlOutput(paste0("sumar_", x))
                    )
                })
            )
        )
    )
)

server <- function(input, output, session) {
    disable("download")

    data_frame <- reactive({
        req(input$fisier)

        isolate({
            df <- read_excel(input$fisier$datapath, col_names = input$antet)
            df <- df[, colSums(is.na(df)) != nrow(df)]

            current_colnames <- colnames(df)
            for (i in seq_along(current_colnames)) {
                if (startsWith(current_colnames[i], "...")) {
                    current_colnames[i] <- paste0("Coloana ", i)
                }
            }
            colnames(df) <- current_colnames
            return(as.data.frame(df))
        })
    })

    output$tabel <- renderDT({
        data_frame()
    })

    observe({
        df <- data_frame()

        isolate({
            numeric_columns <- sapply(colnames(df), function(x) {
                na_mask <- !is.na(df[[x]])
                return(all(is.numeric(df[[x]][na_mask])))
            })
            numeric_columns <- colnames(df)[numeric_columns]
            updateSelectInput(session, "x", choices = numeric_columns, selected = numeric_columns[1])
            updateSelectInput(session, "y", choices = numeric_columns, selected = numeric_columns[2])
        })
    })

    filtered_df <- reactive({
        choice_x <- input$x
        choice_y <- input$y

        isolate({
            req(choice_x %in% colnames(data_frame()), choice_y %in% colnames(data_frame()))
            if (choice_x == choice_y) {
                return(NULL)
            }
            new_df <- data_frame()[, c(choice_x, choice_y)]
            new_df <- new_df[complete.cases(new_df), ]
            return(new_df)
        })
    })

    observe({
        df <- filtered_df()

        isolate({
            disable("calcul")
            req(!is.null(df), nrow(df) > 0)
            enable("calcul")
        })
    })

    base_plot <- reactive({
        df <- filtered_df()

        isolate({
            req(!is.null(df), nrow(df) > 0)

            return(ggplot(df, aes(x = .data[[colnames(df)[1]]], y = .data[[colnames(df)[[2]]]])) +
                geom_point() +
                theme_classic())
        })
    })

    # linear
    regr_model_list <- reactive({
        enable("download")
        df <- filtered_df()

        isolate({
            req(!is.null(df), nrow(df) > 0)
            learned_model <- lapply(names(regression_functions), function(x) {
                regression_functions[[x]](df[, 1], df[, 2])
            })
            names(learned_model) <- names(regression_functions)
            return(learned_model)
        })     
    }) %>% bindEvent(input$calcul)

    regr_stat_list <- reactive({
        regr_mod <- regr_model_list()

        isolate({
            orig_cn <- colnames(filtered_df())
            stats_list <- lapply(names(regression_functions), function(x) {
                current_mod <- regr_mod[[x]]
                r2 <- 1 - (current_mod$deviance / current_mod$null.deviance)
                aic <- current_mod$aic

                table_stats <- data.frame(summary(current_mod)$coefficients)
                colnames(table_stats) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
                tabel_stats <- knitr::kable(table_stats, format = "html")

                formula_str <- coef_functions[[x]](
                    # trim to 2 decimal places
                    round(coef(current_mod), 2),
                    orig_cn
                )

                # create a string

                text_mod <- paste0(
                    "Formulă: ", formula_str, "<br>",
                    "R^2: ", round(r2, 2), "<br>",
                    "AIC: ", round(aic, 2), "<br>",
                    "Coeficienți: ", tabel_stats
                )

                return(text_mod)
            })
            names(stats_list) <- names(regression_functions)

            return(stats_list)
        })
    })


    regr_plot_list <- reactive({
        regr_mod <- regr_model_list()
        ggobj <- base_plot()
        conf_level <- 0.05

        isolate({
            plt_list <- lapply(names(regression_functions), function(x) {
                prediction <- add_ci(df = NULL, fit = regr_mod[[x]], alpha = conf_level, names = c("lwr", "upr"))
                return(ggobj +
                    geom_ribbon(aes(ymin = prediction$lwr, ymax = prediction$upr), fill = "gray", alpha = 0.5) +
                    geom_line(aes(y = prediction$pred), color = "red") +
                    ggtitle(paste("Regresie", x))
                )
            })
            names(plt_list) <- names(regression_functions)

            return(plt_list)
        })
    }) %>% bindEvent(input$calcul)

    observe({
        regr_plots <- regr_plot_list()
        regr_stats <- regr_stat_list()

        isolate({
            lapply(names(regression_functions), function(regr_name) {
                output[[paste0("plot_", regr_name)]] <- renderPlot({
                    regr_plots[[regr_name]]
                })

                output[[paste0("sumar_", regr_name)]] <- renderUI({
                    HTML(regr_stats[[regr_name]])
                })
            })
        })
    })

    output$download <- downloadHandler(
        filename = function() {
            paste("regresii", ".pdf", sep = "")
        },
        content = function(file) {
            cairo_pdf(file, width = 10, height = 10)
            lapply(names(regression_functions), function(x) {
                print(regr_plot_list()[[x]])
            })
            dev.off()
        }
    )
}

shinyApp(ui = ui, server = server)
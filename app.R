library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readxl)
library(ciTools)
library(shinyjs)
library(latex2exp)

downloadButton <- function(...) {
    tag <- shiny::downloadButton(...)
    tag$attribs$download <- NULL
    tag
}
# creates a shiny app that reads an excel file containing a dataset with at least
# two columns and performs the following regressions: linear, logistic, exponential,
# gaussian and quadric. The user will be able to upload the dataset, to choose
# the columns to be used in the regressions.
regression_functions <- list(
    "liniara" = function(x, y) {
        glm(y ~ x, family = gaussian(link = "identity"))
    },
    "exponentiala" = function(x, y) {
        y[y == 0] <- 1e-10
        glm(y ~ x, family = gaussian(link = "log"))
    },
    "quadratica" = function(x, y) {
        glm(y ~ poly(x, 2), family = gaussian(link = "identity"))
    }
)

coef_functions <- list(
    "liniara" = function(coef, col_names) {
        y <- col_names[2]
        x <- col_names[1]
        paste0("<strong>", y, " = ", coef[2], " * ", x, " + ", coef[1], "<br>sau<br>", "y = ", coef[2], " * x + ", coef[1], "</strong>")
    },
    "exponentiala" = function(coef, col_names) {
        y <- col_names[2]
        x <- col_names[1]
        paste0("<strong>", y, " = exp(", coef[1], " + ", coef[2], " * ", x, ")", "<br>sau<br>", "y = exp(", coef[1], " + ", coef[2], " * x)</strong>")
    },
    "quadratica" = function(coef, col_names) {
        y <- col_names[2]
        x <- col_names[1]
        paste0("<strong>", y, " = ", coef[3], " * ", x, "^2 + ", coef[2], " * ", x, " + ", coef[1], "<br>sau<br>", "y = ", coef[3], " * x^2 + ", coef[2], " * x + ", coef[1], "</strong>")
    }
)

coef_functions_latex <- list(
    "liniara" = function(coef, col_names) {
        y <- col_names[2]
        x <- col_names[1]
       TeX(paste0("$ y = ", round(coef[2], 2), " \\cdot x + ", round(coef[1], 2), "$"))
    },
    "exponentiala" = function(coef, col_names) {
        y <- col_names[2]
        x <- col_names[1]
        TeX(paste0("$ y  = e^{", round(coef[1], 2), " + ", round(coef[2], 2), " \\cdot x}$"))
    },
    "quadratica" = function(coef, col_names) {
        y <- col_names[2]
        x <- col_names[1]
        TeX(paste0("y = ", round(coef[3], 2), " $\\cdot x^2$ + ", round(coef[2], 2), " $\\cdot$ x + ", round(coef[1], 2)))
    }
)

regression_plot <- function(df, regr_model, regr_name, r2 = "", equation_latex = "", conf_level = 0.05, legend.position = c(0, 1), axis_size = 12, title_size = 16, legend_size = 14, point_size = 3, line_size = 1.5) {
    base_plot <- ggplot() +
        geom_point(data = df, mapping = aes(x = .data[[colnames(df)[1]]], y = .data[[colnames(df)[2]]]), size = point_size) +
        theme_classic()

    prediction <- add_ci(df = NULL, fit = regr_model, alpha = conf_level, names = c("lwr", "upr"))
    prediction$x <- df[, 1]
    prediction$y <- df[, 2]

    return(base_plot +
        geom_ribbon(data = prediction, mapping = aes(x = x, y = y, ymin = lwr, ymax = upr), fill = "gray", alpha = 0.5) +
        geom_line(data = prediction, mapping = aes(x = x, y = pred, colour = "line_eq"), size = line_size) +
        scale_colour_manual(values = c("line_eq" = "red"), name = "", labels = unname(equation_latex)) +
        labs(
            title = paste0("Regresie ", regr_name, " - ", paste0(colnames(df), collapse = " vs ")),
            subtitle = TeX(paste0("$R^2 = ", r2, "$"))
        ) +
        theme(
            legend.position = legend.position,
            legend.justification = c(0, 1),
            plot.title = element_text(hjust = 0.5, size = title_size),
            plot.subtitle = element_text(hjust = 0.5, size = title_size * 0.8),
            axis.title = element_text(size = axis_size),
            axis.text = element_text(size = axis_size * 0.8),
            legend.text = element_text(size = legend_size)
        ) 
    )
}

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
            actionButton("calcul", "Calculează regresiile"),
            shiny::sliderInput("conf_level", "Nivel de încredere", min = 0.01, max = 0.1, value = 0.05, step = 0.01),
            shiny::p("Parametri grafici:"),
            shiny::sliderInput("axis_size", "Font text axe", min = 4, max = 25, value = 14, step = 1),
            shiny::sliderInput("title_size", "Font titlu grafic", min = 4, max = 30, value = 16, step = 1),
            shiny::sliderInput("legend_size", "Font ecautie", min = 4, max = 25, value = 14, step = 1),
            shiny::sliderInput("point_size", "Dimensiune puncte", min = 1, max = 10, value = 3, step = 1),
            shiny::sliderInput("line_size", "Dimensiune linie regresie", min = 0.5, max = 5, value = 1.5, step = 0.5)
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

                formula_str_latex <- coef_functions_latex[[x]](
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

                return(
                    list(
                        text = text_mod,
                        r2 = r2,
                        aic = aic,
                        coef_table = table_stats,
                        formula = formula_str,
                        formula_latex = formula_str_latex
                    )
                )
            })
            names(stats_list) <- names(regression_functions)

            return(stats_list)
        })
    })


    regr_plot_list <- reactive({
        regr_mod <- regr_model_list()
        # ggobj <- base_plot()
        conf_level <- input$conf_level
        axis_size <- input$axis_size
        title_size <- input$title_size
        legend_size <- input$legend_size
        point_size <- input$point_size
        line_size <- input$line_size
        current_df <- filtered_df()
        regr_stats <- regr_stat_list()

        shiny::req(regr_mod, conf_level, current_df, regr_stats, axis_size, title_size, legend_size, point_size, line_size)


        isolate({
            plt_list <- lapply(names(regression_functions), function(x) {
                return(
                    regression_plot(
                        df = current_df,
                        regr_model = regr_mod[[x]],
                        regr_name = x,
                        r2 = round(regr_stats[[x]]$r2, 4),
                        equation_latex = regr_stats[[x]]$formula_latex,
                        conf_level = conf_level,
                        legend.position = c(0, 1),
                        axis_size = axis_size,
                        title_size = title_size,
                        legend_size = legend_size,
                        point_size = point_size,
                        line_size = line_size
                    )
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
                    HTML(regr_stats[[regr_name]]$text)
                })
            })
        })
    })

    observe({

        output$download <- downloadHandler(
            filename = function() {
                paste("regresii", ".pdf", sep = "")
            },
            content = function(file) {
                regr_plots <- regr_plot_list()
                isolate({
                    pdf(file, width = 10, height = 10)
                    lapply(names(regression_functions), function(x) {
                        print(regr_plots[[x]])
                    })
                    dev.off()
                })
            }
        )
    })
}

shinyApp(ui = ui, server = server)
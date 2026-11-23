library(shiny)

# Завантаження даних
data("mtcars")

cars <- mtcars

# Побудова лінійної регресійної моделі
fit <- lm(mpg ~ wt + hp, data = cars)

# Розрахунок RMSE на навчальних даних
model_rmse <- sqrt(mean(residuals(fit)^2))

# Інтерфейс користувача
ui <- fluidPage(

  titlePanel("Інтерактивний аналіз автомобільних даних"),

  sidebarLayout(

    sidebarPanel(

      h4("Параметри автомобіля"),

      sliderInput(
        "wt",
        "Маса автомобіля (1000 фунтів):",
        min = 1.5,
        max = 5.5,
        value = 3,
        step = 0.1
      ),

      sliderInput(
        "hp",
        "Потужність двигуна (к. с.):",
        min = 50,
        max = 350,
        value = 120,
        step = 5
      ),

      checkboxInput(
        "show_line",
        "Показати лінію моделі",
        value = TRUE
      ),

      selectInput(
        "color",
        "Групування точок:",
        choices = c(
          "За кількістю циліндрів" = "cyl",
          "За типом коробки передач" = "am"
        )
      ),

      actionButton(
        "reset",
        "Скинути параметри"
      )
    ),

    mainPanel(

      h3("Результати прогнозування"),

      verbatimTextOutput("prediction"),

      h4("Коефіцієнти регресійної моделі"),

      tableOutput("coefficients"),

      h4("Графік залежності"),

      plotOutput("scatter", height = "400px"),

      h4("Оцінювання якості моделі"),

      verbatimTextOutput("rmse"),

      p(
        "RMSE розраховано на навчальних даних. ",
        "Цей показник не є незалежною оцінкою ",
        "точності прогнозу для нових спостережень."
      )
    )
  )
)

# Серверна частина
server <- function(input, output, session) {

  # Скидання параметрів
  observeEvent(input$reset, {

    updateSliderInput(
      session,
      "wt",
      value = 3
    )

    updateSliderInput(
      session,
      "hp",
      value = 120
    )

    updateCheckboxInput(
      session,
      "show_line",
      value = TRUE
    )

    updateSelectInput(
      session,
      "color",
      selected = "cyl"
    )

  })

  # Формування нових вхідних даних
  new_car <- reactive({

    data.frame(
      wt = input$wt,
      hp = input$hp
    )

  })

  # Розрахунок прогнозу
  predicted <- reactive({

    predict(
      fit,
      newdata = new_car(),
      interval = "prediction",
      level = 0.95
    )

  })

  # Виведення прогнозу
  output$prediction <- renderPrint({

    pred <- predicted()

    cat(
      sprintf(
        "Прогнозована економічність: %.2f mpg\n",
        pred[1, "fit"]
      )
    )

    cat(
      sprintf(
        "95%% прогнозний інтервал: [%.2f; %.2f] mpg\n",
        pred[1, "lwr"],
        pred[1, "upr"]
      )
    )

  })

  # Таблиця коефіцієнтів
  output$coefficients <- renderTable({

    result <- as.data.frame(
      round(coef(summary(fit)), 4)
    )

    result$Параметр <- rownames(result)

    rownames(result) <- NULL

    result <- result[
      ,
      c(
        "Параметр",
        "Estimate",
        "Std. Error",
        "t value",
        "Pr(>|t|)"
      )
    ]

    names(result) <- c(
      "Параметр",
      "Коефіцієнт",
      "Стандартна помилка",
      "t-статистика",
      "p-значення"
    )

    result

  }, striped = TRUE, bordered = TRUE)

  # Побудова графіка
  output$scatter <- renderPlot({

    group <- if (input$color == "cyl") {
      factor(cars$cyl)
    } else {
      factor(cars$am)
    }

    group_colors <- as.integer(group) + 1L

    plot(
      cars$wt,
      cars$mpg,
      pch = 19,
      col = group_colors,
      xlab = "Маса автомобіля (1000 фунтів)",
      ylab = "Економічність (mpg)",
      main = "Залежність економічності від маси автомобіля"
    )

    grid()

    # Лінія моделі
    if (input$show_line) {

      wt_grid <- seq(
        min(cars$wt),
        max(cars$wt),
        length.out = 100
      )

      line_data <- data.frame(
        wt = wt_grid,
        hp = rep(input$hp, length(wt_grid))
      )

      lines(
        wt_grid,
        predict(fit, newdata = line_data),
        col = "blue",
        lwd = 2
      )

    }

    # Поточний прогноз
    pred <- predicted()

    points(
      input$wt,
      pred[1, "fit"],
      pch = 8,
      col = "red",
      cex = 2
    )

    legend(
      "topright",
      legend = levels(group),
      col = seq_along(levels(group)) + 1L,
      pch = 19,
      title = if (input$color == "cyl") {
        "Циліндри"
      } else {
        "Коробка передач"
      },
      bty = "n"
    )

    legend(
      "bottomleft",
      legend = "Поточний прогноз",
      col = "red",
      pch = 8,
      bty = "n"
    )

  })

  # Виведення RMSE
  output$rmse <- renderPrint({

    cat(
      sprintf(
        "RMSE моделі на навчальних даних: %.3f mpg\n",
        model_rmse
      )
    )

    cat(
      "Оцінка розрахована на тих самих даних, ",
      "які використовувалися для побудови моделі.\n",
      sep = ""
    )

  })

}

# Запуск застосунку
shinyApp(
  ui = ui,
  server = server
)
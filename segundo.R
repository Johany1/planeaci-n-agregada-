library(shiny)
library(lpSolve)
library(ggplot2)
library(reshape2)

ui <- fluidPage(
  titlePanel("Planeación Agregada - Optimización de Costos con Proveedores Mínimos"),
  sidebarLayout(
    sidebarPanel(
      h4("⚙ Parámetros de Entrada"),
      numericInput("cap_prod", "Capacidad máxima de producción interna:", 300),
      numericInput("prod_unit_cost", "Costo por unidad producida internamente:", 12),
      numericInput("inv_cost", "Costo de inventario por unidad:", 0.8),
      numericInput("hire_cost", "Costo por contratar empleado:", 350),
      numericInput("fire_cost", "Costo por despedir empleado:", 250),
      numericInput("prod_per_worker", "Producción mensual por empleado:", 25),
      numericInput("initial_workers", "Número de empleados iniciales:", 6),
      numericInput("cost_prov1", "Costo por unidad del proveedor 1:", 22),
      numericInput("cost_prov2", "Costo por unidad del proveedor 2:", 24),
      actionButton("run", "Calcular planificación")
    ),
    mainPanel(
      h4("✅ Solución Óptima:"),
      tableOutput("result_table"),
      h4("💰 Costo Total"),
      verbatimTextOutput("total_cost"),
      h4("👷‍♂️ Empleados, Contratación y Despido"),
      plotOutput("emp_plot"),
      h4("🏭 Producción Interna y Compras Externas"),
      plotOutput("prod_plot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$run, {
    meses <- 12
    demanda <- c(200, 250, 230, 270, 300, 280, 320, 310, 290, 330, 310, 300)
    n_vars <- meses * 7
    
    # Objetivo
    f.obj <- rep(0, n_vars)
    for (t in 1:meses) {
      idx <- (t - 1) * 7
      f.obj[idx + 1] <- input$prod_unit_cost
      f.obj[idx + 2] <- input$inv_cost
      f.obj[idx + 3] <- input$cost_prov1
      f.obj[idx + 4] <- input$cost_prov2
      f.obj[idx + 6] <- input$hire_cost
      f.obj[idx + 7] <- input$fire_cost
    }
    
    rows <- meses * 4   # una fila extra para la restricción de proveedores mínimos
    f.con <- matrix(0, nrow = rows, ncol = n_vars)
    f.dir <- rep("=", rows)
    f.rhs <- rep(0, rows)
    row <- 1
    
    for (t in 1:meses) {
      idx <- (t - 1) * 7
      
      # 1. Cumplimiento demanda
      f.con[row, idx + 1] <- 1
      f.con[row, idx + 2] <- -1
      f.con[row, idx + 3] <- 1
      f.con[row, idx + 4] <- 1
      if (t > 1) f.con[row, idx - 5 + 2] <- 1
      f.rhs[row] <- demanda[t]
      row <- row + 1
      
      # 2. Límite producción interna
      f.con[row, idx + 1] <- 1
      f.con[row, idx + 5] <- -input$prod_per_worker
      f.dir[row] <- "<="
      f.rhs[row] <- 0
      row <- row + 1
      
      # 3. Evolución del personal
      if (t == 1) {
        f.con[row, idx + 5] <- 1
        f.con[row, idx + 6] <- -1
        f.con[row, idx + 7] <- 1
        f.rhs[row] <- input$initial_workers
      } else {
        f.con[row, idx + 5] <- 1
        f.con[row, idx + 6] <- -1
        f.con[row, idx + 7] <- 1
        f.con[row, idx - 5 + 5] <- -1
        f.rhs[row] <- 0
      }
      row <- row + 1
      
      # 4. Restricción: Prov1 + Prov2 >= 4 unidades
      f.con[row, idx + 3] <- -1
      f.con[row, idx + 4] <- -1
      f.dir[row] <- "<="
      f.rhs[row] <- -4
      row <- row + 1
    }
    
    # Resolver
    res <- lp("min", f.obj, f.con, f.dir, f.rhs)
    
    if (res$status == 0) {
      sol <- matrix(res$solution, nrow = meses, byrow = TRUE)
      colnames(sol) <- c("Producción", "Inventario", "Prov1", "Prov2", "Empleados", "Contratados", "Despedidos")
      df <- as.data.frame(sol)
      df$Mes <- paste0("Mes ", 1:meses)
      
      total_costo <- sum(sol * matrix(f.obj, nrow = meses, byrow = TRUE))
      
      output$result_table <- renderTable({
        df[, c("Mes", colnames(sol))]
      }, digits = 0)
      
      output$total_cost <- renderText({
        paste0("Costo Total: $", format(round(total_costo, 2), big.mark = ","))
      })
      
      df_emp <- melt(df[, c("Mes", "Empleados", "Contratados", "Despedidos")], id.vars = "Mes")
      output$emp_plot <- renderPlot({
        ggplot(df_emp, aes(x = Mes, y = value, color = variable, group = variable)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          labs(x = "Mes", y = "Cantidad", color = "Variable") +
          theme_minimal()
      })
      
      df_prod <- melt(df[, c("Mes", "Producción", "Prov1", "Prov2")], id.vars = "Mes")
      output$prod_plot <- renderPlot({
        ggplot(df_prod, aes(x = Mes, y = value, fill = variable)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = "Mes", y = "Unidades", fill = "Origen") +
          theme_minimal()
      })
    } else {
      output$result_table <- renderTable({
        data.frame(Mensaje = "No se encontró solución factible.")
      })
      output$total_cost <- renderText("")
      output$emp_plot <- renderPlot(NULL)
      output$prod_plot <- renderPlot(NULL)
    }
  })
}

shinyApp(ui, server)

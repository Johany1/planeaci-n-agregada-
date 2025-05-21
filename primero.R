library(shiny)
library(lpSolve)
library(ggplot2)
library(reshape2)
library(writexl)

ui <- fluidPage(
  titlePanel("Planeación Agregada - Optimización de Costos (12 meses)"),
  sidebarLayout(
    sidebarPanel(
      h4("⚙ Parámetros generales"),
      numericInput("cap_prod", "Capacidad máxima de producción interna:", 300),
      numericInput("prod_unit_cost", "Costo por unidad producida internamente:", 18),
      numericInput("inv_cost", "Costo de inventario por unidad:", 1),
      numericInput("hire_cost", "Costo por contratar empleado:", 400),
      numericInput("fire_cost", "Costo por despedir empleado:", 300),
      numericInput("prod_per_worker", "Producción mensual por empleado:", 20),
      numericInput("initial_workers", "Número de empleados iniciales:", 6),
      numericInput("cost_prov1", "Costo por unidad del proveedor 1:", 25),
      numericInput("cost_prov2", "Costo por unidad del proveedor 2:", 27),
      numericInput("min_inventory", "Inventario mínimo requerido por mes:", 10),
      numericInput("max_firing", "Máximo de despidos por mes:", 5),
      
      br(), h4("📋 Demanda mensual"),
      uiOutput("demand_inputs"),
      
      actionButton("run", "📊 Calcular planificación"),
      downloadButton("download", "📥 Exportar resultados")
    ),
    mainPanel(
      h4("📊 Resultados de planificación"),
      tableOutput("result_table"),
      h4("💰 Costo Total"),
      verbatimTextOutput("total_cost"),
      h4("📦 Producción y Compras"),
      plotOutput("prod_plot"),
      h4("👥 Empleados y Cambios"),
      plotOutput("emp_plot")
    )
  )
)

server <- function(input, output, session) {
  meses <- 12
  plan_result <- reactiveVal(NULL)
  
  output$demand_inputs <- renderUI({
    lapply(1:meses, function(i) {
      numericInput(paste0("demanda_", i), paste("Demanda Mes", i), value = c(200, 250, 230, 270, 300, 280, 320, 310, 290, 330, 310, 300)[i])
    })
  })
  
  observeEvent(input$run, {
    demanda <- sapply(1:meses, function(i) input[[paste0("demanda_", i)]])
    n_vars <- meses * 7
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
    
    filas <- meses * 5 + 1
    f.con <- matrix(0, nrow = filas, ncol = n_vars)
    f.dir <- rep("=", filas)
    f.rhs <- rep(0, filas)
    row <- 1
    
    for (t in 1:meses) {
      idx <- (t - 1) * 7
      
      # 1. Cumplimiento de demanda
      f.con[row, idx + 1] <- 1
      f.con[row, idx + 2] <- -1
      f.con[row, idx + 3] <- 1
      f.con[row, idx + 4] <- 1
      if (t > 1) f.con[row, idx - 5] <- 1
      f.rhs[row] <- demanda[t]
      row <- row + 1
      
      # 2. Capacidad máxima
      f.con[row, idx + 1] <- 1
      f.con[row, idx + 5] <- -input$prod_per_worker
      f.dir[row] <- "<="
      f.rhs[row] <- 0
      row <- row + 1
      
      # 3. Inventario mínimo
      f.con[row, idx + 2] <- 1
      f.dir[row] <- ">="
      f.rhs[row] <- input$min_inventory
      row <- row + 1
      
      # 4. Evolución de empleados
      if (t == 1) {
        f.con[row, idx + 5] <- 1
        f.con[row, idx + 6] <- -1
        f.con[row, idx + 7] <- 1
        f.rhs[row] <- input$initial_workers
      } else {
        f.con[row, idx + 5] <- 1
        f.con[row, idx + 6] <- -1
        f.con[row, idx + 7] <- 1
        f.con[row, idx - 2] <- -1
        f.rhs[row] <- 0
      }
      row <- row + 1
      
      # 5. Límite de despidos
      f.con[row, idx + 7] <- 1
      f.dir[row] <- "<="
      f.rhs[row] <- input$max_firing
      row <- row + 1
    }
    
    # 6. Uso de al menos un proveedor
    f.con[row, seq(3, n_vars, 7)] <- 1
    f.con[row, seq(4, n_vars, 7)] <- 1
    f.dir[row] <- ">="
    f.rhs[row] <- 1
    
    res <- lp("min", f.obj, f.con, f.dir, f.rhs)
    
    if (res$status == 0) {
      sol <- matrix(res$solution, nrow = meses, byrow = TRUE)
      colnames(sol) <- c("Producción", "Inventario", "Prov1", "Prov2", "Empleados", "Contratados", "Despedidos")
      df <- as.data.frame(sol)
      df$Mes <- paste0("Mes ", 1:meses)
      df$CostoTotal <- round(rowSums(sol * matrix(f.obj, nrow = meses, byrow = TRUE)), 2)
      plan_result(df)
      
      output$result_table <- renderTable({
        df[, c("Mes", colnames(sol))]
      }, digits = 0)
      
      output$total_cost <- renderText({
        paste0("Costo Total: $", format(sum(df$CostoTotal), big.mark = ","))
      })
      
      df_plot1 <- melt(df[, c("Mes", "Producción", "Prov1", "Prov2")], id.vars = "Mes")
      output$prod_plot <- renderPlot({
        ggplot(df_plot1, aes(x = Mes, y = value, fill = variable)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = "Mes", y = "Unidades", fill = "Variable") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      df_plot2 <- melt(df[, c("Mes", "Empleados", "Contratados", "Despedidos")], id.vars = "Mes")
      output$emp_plot <- renderPlot({
        ggplot(df_plot2, aes(x = Mes, y = value, color = variable, group = variable)) +
          geom_line(size = 1.2) +
          geom_point(size = 3) +
          labs(x = "Mes", y = "Personas", color = "Variable") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
    } else {
      output$result_table <- renderTable({
        data.frame(Mensaje = "No se encontró solución factible.")
      })
      output$total_cost <- renderText("")
      output$prod_plot <- renderPlot(NULL)
      output$emp_plot <- renderPlot(NULL)
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {"planificacion_agregada.xlsx"},
    content = function(file) {
      if (!is.null(plan_result())) {
        write_xlsx(plan_result(), path = file)
      }
    }
  )
}

shinyApp(ui, server)

# Bibliotheken laden
library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)

# Benutzeroberfläche (UI)
ui <- fluidPage(
  titlePanel("Anhang B: Aufbau Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Einstellungen"),
      numericInput("m_rpa", "M-RPA Wert:", value = 65.0, step = 0.1, min = 60, max = 100),
      numericInput("w_rpa", "W-RPA Wert:", value = 60.0, step = 0.1, min = 60, max = 100),
      actionButton("update", "Update Werte") # Button zum Aktualisieren
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Beschreibung",
                 h4("Ausgangsdaten"),
                 HTML("<p>Das Ausgangsdatenset enthält für jedes Jahr zwischen 1869 und 2100 eine Beobachtung (Zeilen) und mehrere Variablen (Spalten), wobei die Daten für einige Jahre fehlen. Die Lücken aufgrund der fehlenden Werte werden mit Interpolation geschlossen. Im Anschluss werden visuelle Vergleiche in Bezug auf die demografische Entwicklung verschiedener Personengruppen ausgegeben.</p>")
        ),
        tabPanel("Tabelle", tableOutput("table")), # Tabelle anzeigen
        tabPanel("Plot", plotOutput("plot"))      # Plot anzeigen
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reaktiver Datensatz
  data <- reactiveVal({
    # Initiale Daten aus Excel-Datei laden
    read_excel("https://github.com/christian-da-cunha/Pensionssystem/blob/493d448beffec35316834265afcd3e0ae1ec8beb/Bev%C3%B6lkerung_Belastungsquote_Pensionen.xlsx")
  })
  
  # Berechnungsfunktionen
  calculate_sum <- function(m_rpa, row) {
    if (is.na(m_rpa) || m_rpa < 60 || m_rpa > 100) return(NA)
    num_values <- floor(m_rpa) - 20
    sum_values <- sum(as.numeric(row[7:(6 + num_values)]), na.rm = TRUE)
    if (m_rpa %% 1 != 0) {
      decimal_part <- m_rpa %% 1
      sum_values <- sum_values + decimal_part * as.numeric(row[7 + num_values])
    }
    return(sum_values)
  }
  
  calculate_increase <- function(w_rpa, row) {
    if (is.na(w_rpa) || w_rpa < 60 || w_rpa > 100) return(0)
    num_values <- floor(w_rpa) - 20
    sum_values <- sum(as.numeric(row[83:(82 + num_values)]), na.rm = TRUE)
    if (w_rpa %% 1 != 0) {
      decimal_part <- w_rpa %% 1
      sum_values <- sum_values + decimal_part * as.numeric(row[83 + num_values])
    }
    return(sum_values)
  }
  
  # Aktualisierung der Werte bei Klick auf den Button
  observeEvent(input$update, {
    new_data <- data()
    new_data$`M-RPA` <- input$m_rpa
    new_data$`W-RPA` <- input$w_rpa
    
    # Berechnung von P-EB
    new_data$`P-EB` <- apply(new_data, 1, function(row) {
      calculate_sum(row["M-RPA"], row)
    })
    
    # Berechnung von P-PB
    new_data$`P-PB` <- apply(new_data, 1, function(row) {
      calculate_increase(row["W-RPA"], row)
    })
    
    # Berechnung von P-BQ
    new_data$`P-BQ` <- with(new_data, {
      result <- as.numeric(`P-EB`) / as.numeric(`P-PB`)
      round(result, 2) # Auf zwei Nachkommastellen runden
    })
    
    data(new_data) # Aktualisierte Daten speichern
  })
  
  # Tabelle anzeigen
  output$table <- renderTable({
    data()
  })
  
  # Plot erstellen
  output$plot <- renderPlot({
    ggplot(data(), aes(x = Year, y = `P-BQ`)) +
      geom_line(color = "blue", size = 1.2) +
      geom_vline(xintercept = 2023, linetype = "dashed", color = "red", size = 0.8) +
      geom_vline(xintercept = 2033, linetype = "dashed", color = "green", size = 0.8) +
      labs(
        title = "Verlauf von P-BQ im Zeitverlauf",
        x = "Jahr",
        y = "P-BQ"
      ) +
      theme_minimal()
  })
}

# App starten
shinyApp(ui = ui, server = server)

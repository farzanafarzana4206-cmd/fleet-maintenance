library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# Professional FleetCare Theme
fleet_theme <- tags$head(
  tags$style(HTML("
    body {
      font-family: Arial, sans-serif;
    }

    .content-wrapper {
      background: #f4f6f9;
    }

    .small-box {
  border-radius: 18px;
  box-shadow: 0 5px 18px rgba(0,0,0,0.12);
  min-height: 125px;
}

.small-box .inner h3 {
  font-size: 32px;
  font-weight: 700;
}

.small-box .inner p {
  font-size: 15px;
  font-weight: 500;
}

.small-box .icon {
  font-size: 55px;
  top: 20px;
  right: 18px;
}

    .box {
  border-radius: 18px !important;
  overflow: hidden;
  box-shadow: 0 5px 18px rgba(0,0,0,0.12);
  border: none !important;
}
  "))
)

ui <- dashboardPage(
  
  dashboardHeader(title = "FleetCare"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Risk Analysis", tabName = "risk", icon = icon("truck")),
      menuItem("Reports", tabName = "reports", icon = icon("file"))
    )
  ),
  
  dashboardBody(
    fleet_theme,
    tags$head(
      tags$script(HTML("
  Shiny.addCustomMessageHandler('criticalAlert', function(message) {
    var audio = new Audio(
      'https://actions.google.com/sounds/v1/alarms/alarm_clock.ogg'
    );
    audio.loop = true;
    audio.play();

    setTimeout(function() {
      audio.pause();
      audio.currentTime = 0;
    }, 5000);
  });
")),
      tags$style(HTML("
        .content-wrapper {
          background-color: #f5f7fb;
        }
        .small-box {
          border-radius: 12px;
        }
        .box {
          border-radius: 12px;
        }
        /* Purple active menu */
.skin-blue .main-sidebar .sidebar-menu > li.active > a,
.skin-blue .main-sidebar .sidebar-menu > li:hover > a {
  background-color: #7C3AED !important;
  color: white !important;
}

.skin-blue .main-sidebar .sidebar-menu > li.active > a .fa,
.skin-blue .main-sidebar .sidebar-menu > li:hover > a .fa {
  color: white !important;
}
      "))
    ),
    
    tabItems(
      
      # DASHBOARD
      tabItem(
        tabName = "dashboard",
        
        div(
          style = "padding: 10px 5px 20px 5px;",
          h1(
            "FleetCare",
            style = "font-weight: 700; margin-bottom: 5px;"
          ),
          p(
            "Smart Fleet Maintenance & Risk Monitoring System",
            style = "font-size: 16px; color: #666;"
          )
        ),
        
        fluidRow(
          valueBoxOutput("totalVehicles"),
          valueBoxOutput("lowRisk"),
          valueBoxOutput("mediumRisk"),
          valueBoxOutput("highRisk")
        ),
        
        fluidRow(
          box(
            title = "Fleet Overview",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            h3("Welcome to FleetCare"),
            p("This system helps monitor vehicle maintenance conditions and forecast breakdown risk.")
          )
        )
      ),
      
      # RISK ANALYSIS
      tabItem(
        tabName = "risk",
        
        h2("Vehicle Risk Analysis"),
        
        fluidRow(
          box(
            title = "Vehicle Details",
            width = 5,
            status = "primary",
            solidHeader = TRUE,
            
            textInput(
              "vehicle",
              "Vehicle Number",
              value = "TN32AB1234"
            ),
            
            numericInput(
              "km",
              "Kilometers Travelled",
              value = 25000,
              min = 0
            ),
            
            textInput(
              "status",
              "Maintenance Status",
              value = "Good",
              placeholder = "Good / Average / Poor / Critical"
            ),
            
            numericInput(
              "breakdowns",
              "Previous Breakdowns",
              value = 1,
              min = 0
            ),
            
            actionButton(
              "calculate",
              "Calculate Risk",
              class = "btn-primary"
            )
          ),
          
          box(
            title = "Risk Result",
            width = 7,
            status = "success",
            solidHeader = TRUE,
            
            uiOutput("riskBadge"),
            h4(textOutput("probabilityResult")),
            br(),
            plotOutput("poissonPlot", height = "350px")
          )
        )
      ),
      
      # REPORTS
      tabItem(
        tabName = "reports",
        
        h2("Maintenance Reports"),
        
        box(
          title = "Fleet Report",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          tableOutput("reportTable")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Sample fleet data
  fleet <- data.frame(
    Vehicle = c(
      "TN32AB1234",
      "TN31CD5678",
      "TN34EF9012",
      "TN36GH3456",
      "TN38JK7890"
    ),
    Status = c(
      "Good",
      "Poor",
      "Average",
      "Critical",
      "Good"
    )
  )
  
  output$totalVehicles <- renderValueBox({
    valueBox(
      nrow(fleet),
      "Total Vehicles",
      icon = icon("truck"),
      color = "aqua"
    )
  })
  
  output$lowRisk <- renderValueBox({
    valueBox(
      sum(fleet$Status == "Good"),
      "Low Risk",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  output$mediumRisk <- renderValueBox({
    valueBox(
      sum(fleet$Status == "Average"),
      "Medium Risk",
      icon = icon("exclamation-circle"),
      color = "yellow"
    )
  })
  
  output$highRisk <- renderValueBox({
    valueBox(
      sum(fleet$Status %in% c("Poor", "Critical")),
      "High Risk",
      icon = icon("warning"),
      color = "red"
    )
  })
  
  # Risk calculation
  riskData <- eventReactive(input$calculate, {
    
    status <- trimws(input$status)
    
    valid_status <- c("Good", "Average", "Poor", "Critical")
    
    if (status == "Critical") {
      session$sendCustomMessage(
        "criticalAlert",
        list(message = "CRITICAL VEHICLE ALERT!")
      )
    }
    
    if (!(status %in% valid_status)) {
      showNotification(
        "Invalid maintenance status! Please enter Good, Average, Poor, or Critical.",
        type = "error",
        duration = 5
      )
      return(NULL)
    }
    
    risk <- if (status == "Good") {
      "LOW"
    } else if (status == "Average") {
      "MEDIUM"
    } else {
      "HIGH"
    }
    
    # Simple failure rate
    lambda <- if (risk == "LOW") {
      0.05
    } else if (risk == "MEDIUM") {
      0.15
    } else {
      0.30
    }
    
    probability <- 1 - exp(-lambda)
    
    list(
      risk = risk,
      probability = probability,
      lambda = lambda
    )
  })
  output$riskBadge <- renderUI({
    req(riskData())
    
    risk <- riskData()$risk
    
    badgeColor <- if (risk == "LOW") {
      "#16a34a"
    } else if (risk == "MEDIUM") {
      "#f59e0b"
    } else {
      "#dc2626"
    }
    
    div(
      style = paste0(
        "display:inline-block;",
        "padding:10px 22px;",
        "border-radius:25px;",
        "background:", badgeColor, ";",
        "color:white;",
        "font-size:20px;",
        "font-weight:700;"
      ),
      paste("Risk Level:", risk)
    )
  })
  output$riskResult <- renderText({
    req(riskData())
    paste("Risk Level:", riskData()$risk)
  })
  
  output$probabilityResult <- renderText({
    req(riskData())
    paste(
      "Estimated Breakdown Probability:",
      round(riskData()$probability * 100, 2),
      "%"
    )
  })
  
  # Poisson curve
  output$poissonPlot <- renderPlot({
    
    req(riskData())
    
    lambda <- riskData()$lambda
    
    time <- seq(0, 12, by = 0.1)
    
    probability <- 1 - exp(-lambda * time)
    
    data <- data.frame(
      Time = time,
      Probability = probability
    )
    
    ggplot(data, aes(x = Time, y = Probability)) +
      geom_line(linewidth = 1.3) +
      scale_y_continuous(
        labels = function(x) paste0(round(x * 100), "%")
      ) +
      labs(
        title = "Breakdown Probability Over Time",
        x = "Time",
        y = "Failure Probability"
      ) +
      theme_minimal()
  })
  
  # Report table
  output$reportTable <- renderTable({
    data.frame(
      Vehicle = fleet$Vehicle,
      Status = fleet$Status,
      Risk_Level = ifelse(
        fleet$Status == "Good",
        "LOW",
        ifelse(
          fleet$Status == "Average",
          "MEDIUM",
          "HIGH"
        )
      ),
      Breakdown_Probability = ifelse(
        fleet$Status == "Good",
        "4.88%",
        ifelse(
          fleet$Status == "Average",
          "13.93%",
          "25.92%"
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(bslib)
library(scales)

# ── DATA ──────────────────────────────────────────────────────────────────────
df <- read_csv("FINAL_DATASET.csv", show_col_types = FALSE) %>%
  mutate(
    label_factor         = factor(label, levels = c("REAL", "FAKE")),
    detection_difficulty = factor(detection_difficulty, levels = c("Easy", "Medium", "Hard")),
    dataset_split        = factor(dataset_split, levels = c("train", "test", "val"))
  )

COL_REAL <- "#2980b9"
COL_FAKE <- "#c0392b"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      body { background: #f5f7fa; }

      .hero {
        background: linear-gradient(135deg, #1a1a2e, #2c3e50);
        color: white;
        padding: 24px 20px;
        margin-bottom: 20px;
        border-radius: 0 0 16px 16px;
      }
      .hero h3 { font-weight: 700; margin: 0 0 4px; font-size: clamp(1.1rem, 4vw, 1.5rem); }
      .hero p  { margin: 0; opacity: 0.65; font-size: 0.85rem; }

      .kpi {
        background: white;
        border-radius: 12px;
        padding: 18px 14px;
        text-align: center;
        margin-bottom: 16px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
      }
      .kpi .num { font-size: 1.9rem; font-weight: 700; line-height: 1; margin-bottom: 4px; }
      .kpi .lbl { font-size: 0.75rem; color: #999; text-transform: uppercase; letter-spacing: 1px; }

      .card-panel {
        background: white;
        border-radius: 12px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
      }
      .card-panel .panel-title {
        font-size: 0.75rem;
        letter-spacing: 2px;
        text-transform: uppercase;
        color: #aaa;
        margin-bottom: 12px;
        border-bottom: 1px solid #f0f0f0;
        padding-bottom: 8px;
      }

      .slr-equation {
        background: #1a1a2e;
        color: #f0f0f0;
        border-radius: 10px;
        padding: 16px 18px;
        font-family: 'Courier New', monospace;
        font-size: 0.9rem;
        margin-top: 12px;
      }
      .slr-equation .eq  { color: #f39c12; font-size: 1.05rem; font-weight: bold; }
      .slr-equation .met { color: #3498db; }

      .filter-box {
        background: white;
        border-radius: 12px;
        padding: 16px;
        margin-bottom: 16px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
      }
      .filter-box h6 {
        font-size: 0.72rem;
        letter-spacing: 2px;
        text-transform: uppercase;
        color: #aaa;
        margin-bottom: 12px;
      }

      .app-footer {
        text-align: center;
        font-size: 0.75rem;
        color: #bbb;
        padding: 16px 0 8px;
        border-top: 1px solid #eee;
        margin-top: 10px;
      }

      @media (max-width: 576px) {
        .hero { border-radius: 0; padding: 18px 14px; }
        .kpi .num { font-size: 1.5rem; }
      }
    "))
  ),
  
  # Hero
  div(class = "hero",
      h3("Deepfake Detection Explorer"),
      
  ),
  
  div(class = "container-fluid px-3",
      
      # kpi
      fluidRow(
        column(3, div(class="kpi",
                      div(class="num", style="color:#2c3e50;", textOutput("kpi_total", inline=TRUE)),
                      div(class="lbl", "Total Images")
        )),
        column(3, div(class="kpi",
                      div(class="num", style=paste0("color:",COL_REAL,";"), textOutput("kpi_real", inline=TRUE)),
                      div(class="lbl", "Real Faces")
        )),
        column(3, div(class="kpi",
                      div(class="num", style=paste0("color:",COL_FAKE,";"), textOutput("kpi_fake", inline=TRUE)),
                      div(class="lbl", "Fake Faces")
        )),
        column(3, div(class="kpi",
                      div(class="num", style="color:#27ae60;", textOutput("kpi_conf", inline=TRUE)),
                      div(class="lbl", "Avg Confidence")
        ))
      ),
      
      fluidRow(
        
        #side bar
        column(12, lg=3,
               div(class="filter-box",
                   h6("Filters"),
                   
                   checkboxGroupInput("fil_label", "Label",
                                      choices=c("REAL","FAKE"), selected=c("REAL","FAKE"), inline=TRUE),
                   
                   checkboxGroupInput("fil_diff", "Difficulty",
                                      choices=c("Easy","Medium","Hard"), selected=c("Easy","Medium","Hard")),
                   
                   sliderInput("fil_conf", "Confidence Score",
                               min=0.80, max=1.00, value=c(0.80,1.00), step=0.01, ticks=FALSE),
                   
                   hr(style="margin:12px 0;"),
                   div(style="font-size:0.78rem;color:#bbb;text-align:center;",
                       textOutput("n_shown"))
               )
        ),
        
        #MAIN
        column(12, lg=9,
               
               tabsetPanel(type="tabs",
                           
                           # TAB 1: Overview 
                           tabPanel("Overview",
                                    br(),
                                    div(class="card-panel",
                                        div(class="panel-title", "Label Distribution"),
                                        plotlyOutput("plt_bar", height="240px")
                                    ),
                                    div(class="card-panel",
                                        div(class="panel-title", "Mean Confidence Score by Difficulty"),
                                        plotlyOutput("plt_diff_bar", height="240px")
                                    )
                           ),
                           
                           #TAB 2: Boxplots 
                           tabPanel("Boxplot",
                                    br(),
                                    div(class="card-panel",
                                        div(class="panel-title", "Confidence Score by Label"),
                                        plotlyOutput("plt_box_label", height="280px")
                                    ),
                                    div(class="card-panel",
                                        div(class="panel-title", "Confidence Score by Difficulty"),
                                        plotlyOutput("plt_box_diff", height="280px")
                                    )
                           ),
                           
                           # ─── TAB 3: SLR ───
                           tabPanel("SLR Model",
                                    br(),
                                    div(class="card-panel",
                                        div(class="panel-title", "Simple Linear Regression: Confidence Score ~ Label"),
                                        plotlyOutput("plt_slr", height="340px"),
                                        div(class="slr-equation",
                                            p("MODEL", style="font-size:0.65rem;letter-spacing:3px;color:#555;margin-bottom:6px;"),
                                            p(class="eq", "confidence_score = 0.891 + 0.030 x label_numeric"),
                                            hr(style="border-color:#2a2a3e;margin:10px 0;"),
                                            fluidRow(
                                              column(6,
                                                     p(HTML("<span class='met'>R\u00B2</span> = 0.086")),
                                                     p(HTML("<span class='met'>r</span>  = 0.293"))
                                              ),
                                              column(6,
                                                     p(HTML("<span class='met'>Slope</span>   = +0.030")),
                                                     p(HTML("<span class='met'>p-value</span> < 0.001"))
                                              )
                                            ),
                                            p("Real faces score ~3 percentage points higher confidence than fake ones. Statistically significant.",
                                              style="color:#888;font-size:0.78rem;margin:0;")
                                        )
                                    )
                           ),
                           
                           # ─── TAB 4: Demographics ───
                           tabPanel("Demographics",
                                    br(),
                                    div(class="card-panel",
                                        div(class="panel-title", "Age Group Distribution (Real Images)"),
                                        plotlyOutput("plt_age", height="260px")
                                    ),
                                    div(class="card-panel",
                                        div(class="panel-title", "Mean Confidence Score by Age Group (Real)"),
                                        plotlyOutput("plt_age_conf", height="260px")
                                    )
                           ),
                           
                           # ─── TAB 5: Data Table ───
                           tabPanel("Data",
                                    br(),
                                    div(class="card-panel",
                                        div(class="panel-title", "Filtered Dataset"),
                                        DTOutput("tbl")
                                    )
                           )
               )
        )
      ),
      
  )
)

#SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Filtered data
  data <- reactive({
    req(input$fil_label, input$fil_diff)
    df %>% filter(
      label                %in% input$fil_label,
      detection_difficulty %in% input$fil_diff,
      between(confidence_score, input$fil_conf[1], input$fil_conf[2])
    )
  })
  
  # KPIs
  output$kpi_total <- renderText(comma(nrow(data())))
  output$kpi_real  <- renderText(comma(sum(data()$label == "REAL")))
  output$kpi_fake  <- renderText(comma(sum(data()$label == "FAKE")))
  output$kpi_conf  <- renderText(paste0(round(mean(data()$confidence_score)*100,1),"%"))
  output$n_shown   <- renderText(paste0(comma(nrow(data())), " of ", comma(nrow(df)), " images"))
  
  # Shared plotly config
  clean <- function(p) {
    p %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font   = list(family="Arial, sans-serif", size=12),
        margin = list(l=50, r=20, t=30, b=50),
        legend = list(orientation="h", y=-0.2)
      ) %>%
      config(displayModeBar=FALSE, responsive=TRUE)
  }
  
  # Bar: Label counts
  output$plt_bar <- renderPlotly({
    d <- data() %>%
      count(label) %>%
      mutate(pct = paste0(round(n/sum(n)*100,1),"%"))
    
    plot_ly(d, x=~label, y=~n, type="bar",
            color=~label, colors=c(REAL=COL_REAL, FAKE=COL_FAKE),
            text=~paste0(comma(n), " (", pct, ")"),
            textposition="outside",
            hovertemplate="<b>%{x}</b><br>Count: %{y}<extra></extra>") %>%
      layout(xaxis=list(title=""),
             yaxis=list(title="Count", gridcolor="#f0f0f0"),
             showlegend=FALSE) %>%
      clean()
  })
  
  # ── Bar: Mean confidence by difficulty ──
  output$plt_diff_bar <- renderPlotly({
    d <- data() %>%
      group_by(detection_difficulty) %>%
      summarise(mean_conf = mean(confidence_score), .groups="drop") %>%
      mutate(
        detection_difficulty = factor(detection_difficulty, levels=c("Easy","Medium","Hard")),
        bar_col = case_when(
          detection_difficulty == "Easy"   ~ "#27ae60",
          detection_difficulty == "Medium" ~ "#e67e22",
          TRUE                             ~ COL_FAKE
        )
      )
    
    plot_ly(d, x=~detection_difficulty, y=~mean_conf, type="bar",
            marker=list(color=~bar_col),
            text=~round(mean_conf,3), textposition="outside",
            hovertemplate="<b>%{x}</b><br>Mean: %{y:.3f}<extra></extra>") %>%
      layout(xaxis=list(title="Difficulty"),
             yaxis=list(title="Mean Confidence Score", range=c(0.87,0.94),
                        gridcolor="#f0f0f0"),
             showlegend=FALSE) %>%
      clean()
  })
  
  # ── Boxplot: Confidence by Label ──
  output$plt_box_label <- renderPlotly({
    plot_ly(data(), x=~label, y=~confidence_score, type="box",
            color=~label, colors=c(REAL=COL_REAL, FAKE=COL_FAKE),
            boxpoints="outliers",
            hovertemplate="<b>%{x}</b><br>Score: %{y:.3f}<extra></extra>") %>%
      layout(xaxis=list(title=""),
             yaxis=list(title="Confidence Score", range=c(0.77,1.02),
                        gridcolor="#f0f0f0"),
             showlegend=FALSE) %>%
      clean()
  })
  
  # ── Boxplot: Confidence by Difficulty ──
  output$plt_box_diff <- renderPlotly({
    d <- data() %>%
      mutate(detection_difficulty = factor(detection_difficulty, levels=c("Easy","Medium","Hard")))
    
    plot_ly(d, x=~detection_difficulty, y=~confidence_score, type="box",
            color=~detection_difficulty,
            colors=c(Easy="#27ae60", Medium="#e67e22", Hard=COL_FAKE),
            boxpoints="outliers",
            hovertemplate="<b>%{x}</b><br>Score: %{y:.3f}<extra></extra>") %>%
      layout(xaxis=list(title="Difficulty"),
             yaxis=list(title="Confidence Score", range=c(0.77,1.02),
                        gridcolor="#f0f0f0"),
             showlegend=FALSE) %>%
      clean()
  })
  
  # ── SLR Scatter + regression line ──
  output$plt_slr <- renderPlotly({
    d <- data()
    
    # Split into FAKE and REAL for separate traces so legend works cleanly
    d_fake <- d %>% filter(label == "FAKE") %>%
      mutate(x_jit = jitter(label_numeric, amount = 0.08))
    d_real <- d %>% filter(label == "REAL") %>%
      mutate(x_jit = jitter(label_numeric, amount = 0.08))
    
    # Regression line over full range
    x_line <- c(-0.25, 1.25)
    if (nrow(d) > 2 && length(unique(d$label_numeric)) > 1) {
      m      <- lm(confidence_score ~ label_numeric, data = d)
      y_line <- predict(m, newdata = data.frame(label_numeric = x_line))
      r2     <- round(summary(m)$r.squared, 3)
      sl     <- round(coef(m)[2], 3)
      ic     <- round(coef(m)[1], 3)
      subtitle <- paste0("y = ", ic, " + ", sl, "x     |     R\u00B2 = ", r2)
    } else {
      y_line   <- c(0.891, 0.921)
      subtitle <- "Select both REAL & FAKE to compute regression"
    }
    
    plot_ly() %>%
      # FAKE dots — open circle
      add_trace(
        data = d_fake,
        x = ~x_jit, y = ~confidence_score,
        type = "scatter", mode = "markers",
        name = "FAKE",
        marker = list(
          symbol  = "circle-open",
          size    = 7,
          color   = COL_FAKE,
          opacity = 0.7,
          line    = list(color = COL_FAKE, width = 1.5)
        ),
        hovertemplate = "<b>FAKE</b><br>Confidence: %{y:.3f}<extra></extra>"
      ) %>%
      # REAL dots — filled circle
      add_trace(
        data = d_real,
        x = ~x_jit, y = ~confidence_score,
        type = "scatter", mode = "markers",
        name = "REAL",
        marker = list(
          symbol  = "circle",
          size    = 7,
          color   = COL_REAL,
          opacity = 0.55
        ),
        hovertemplate = "<b>REAL</b><br>Confidence: %{y:.3f}<extra></extra>"
      ) %>%
      # Regression line
      add_trace(
        x = x_line, y = y_line,
        type = "scatter", mode = "lines",
        name = "Regression Line",
        line = list(color = "#2c3e50", width = 2.5, dash = "solid"),
        hovertemplate = "Fitted: %{y:.3f}<extra></extra>"
      ) %>%
      layout(
        title = list(text = subtitle, font = list(size = 12, color = "#888")),
        xaxis = list(
          title     = "Label Numeric  (0 = FAKE  |  1 = REAL)",
          tickvals  = c(0, 1),
          ticktext  = c("0   (FAKE)", "1   (REAL)"),
          range     = c(-0.4, 1.4),
          gridcolor = "#f0f0f0",
          zeroline  = FALSE
        ),
        yaxis = list(
          title     = "Confidence Score",
          range     = c(0.77, 1.02),
          gridcolor = "#f0f0f0",
          zeroline  = FALSE
        ),
        legend = list(orientation = "h", y = -0.22)
      ) %>%
      clean()
  })
  
  # ── Bar: Age group (real only) ──
  output$plt_age <- renderPlotly({
    d <- data() %>%
      filter(label=="REAL", !age_group %in% c("Unknown")) %>%
      count(age_group) %>%
      arrange(age_group)
    
    plot_ly(d, x=~age_group, y=~n, type="bar",
            marker=list(color=COL_REAL, opacity=0.85),
            text=~comma(n), textposition="outside",
            hovertemplate="<b>%{x}</b><br>Count: %{y}<extra></extra>") %>%
      layout(xaxis=list(title="Age Group"),
             yaxis=list(title="Count", gridcolor="#f0f0f0"),
             showlegend=FALSE) %>%
      clean()
  })
  
  # ── Bar: Mean confidence by age (real only) ──
  output$plt_age_conf <- renderPlotly({
    d <- data() %>%
      filter(label=="REAL", !age_group %in% c("Unknown")) %>%
      group_by(age_group) %>%
      summarise(mean_conf = mean(confidence_score), .groups="drop")
    
    plot_ly(d, x=~age_group, y=~mean_conf, type="bar",
            marker=list(color=COL_REAL, opacity=0.8),
            text=~round(mean_conf,3), textposition="outside",
            hovertemplate="<b>%{x}</b><br>Mean: %{y:.3f}<extra></extra>") %>%
      layout(xaxis=list(title="Age Group"),
             yaxis=list(title="Mean Confidence Score", range=c(0.90,0.95),
                        gridcolor="#f0f0f0"),
             showlegend=FALSE) %>%
      clean()
  })
  
  # ── Data Table ──
  output$tbl <- renderDT({
    data() %>%
      select(image_id, label, detection_difficulty, gender, age_group,
             confidence_score, dataset_split, image_quality) %>%
      rename(ID=image_id, Label=label, Difficulty=detection_difficulty,
             Gender=gender, Age=age_group, Confidence=confidence_score,
             Split=dataset_split, Quality=image_quality) %>%
      datatable(
        options=list(pageLength=10, scrollX=TRUE, dom="ftipr",
                     columnDefs=list(list(className="dt-center", targets="_all"))),
        class="table table-striped table-hover",
        rownames=FALSE, filter="top"
      ) %>%
      formatRound("Confidence", digits=3) %>%
      formatStyle("Label",
                  backgroundColor=styleEqual(c("REAL","FAKE"), c("#d5e8f7","#fde8e8")),
                  fontWeight="bold")
  })
}

shinyApp(ui=ui, server=server)

library(shiny)
library(tidyverse)
library(elo)
library(plotly)
library(shinydashboard)
source("dashboard_helper.R")

ui <- dashboardPage(
    dashboardHeader(title = "Crypto Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("consensus",
                     tabName = "class_tab",
                     icon = icon("dashboard")),
            menuItem("1 vs 1",
                     tabName = "head_tab",
                     icon = icon("dashboard")),
            menuItem("Crypto",
                     tabName = "crypto_tab",
                     icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "class_tab",
                    box(plotlyOutput("elo_timeseries")),
                    box(plotlyOutput("elo_dist")),
                    box(dataTableOutput("top_5_table")),
                    box(uiOutput("class_selector_1")),
                    box(sliderInput(inputId = "v_k_1",
                                    label = "K for ELO",
                                    min = 1, 
                                    max = 100,
                                    value = 20))
            ),
            tabItem(tabName = "head_tab",
                    fluidRow(box(uiOutput("crypto_selector")), box(uiOutput("opponent_selector"))),
                    fluidRow(box(valueBoxOutput("crypto_card")), box(valueBoxOutput("opponent_card"))),
                    box(uiOutput("class_selector_2")),
                    box(sliderInput("v_k_2",
                                    label = "K for ELO",
                                    min = 1,
                                    max = 100,
                                    value = 20))),
            tabItem(tabName = "crypto_tab",
                    fluidRow(box(uiOutput("crypto_selector2")), box(uiOutput("class_selector_3"))),
                    fluidRow(box(plotlyOutput("crypto_radar"), width = 12, column(align = "center", width = 12))))
        )
    )
    
)

server <- function(input, output) {
    helper <- reactive({
        load("advanced_dashboard_helper.Rdata")
    })
    
    elo_1 <- reactive(create_elo_data(input$v_k_1))
    elo_2 <- reactive(create_elo_data(input$v_k_2))
    elo <- reactive(elo.run(winner ~ crypto + opponent,
                            k = input$v_k_2,
                            data = elo_df))
    output$class_selector_1 <- renderUI({
        selectInput(inputId = "v_class_1",
                    label = "consensus",
                    choices = elo_1() %>% clean_class())
        
    })
    output$class_selector_2 <- renderUI({
        selectInput(inputId = "v_class_2",
                    label = "consensus",
                    choices = elo_2() %>% clean_class())
    })
    output$class_selector_3 <- renderUI({
        selectInput(inputId = "v_class_3",
                    label = "consensus",
                    choices = elo_2() %>% clean_class())
    })
    output$crypto_selector <- renderUI({
        selectInput(inputId = "v_crypto",
                    label = "Crypto",
                    choices = elo_2() %>% filter(consensus == input$v_class_2) %>% clean_crypto())
    })
    output$crypto_selector2 <- renderUI({
        selectInput(inputId = "v_crypto_desc",
                    label = "Crypto",
                    choices = elo_2() %>% filter(consensus == input$v_class_3) %>% clean_crypto())
    })
    output$opponent_selector <- renderUI({
        selectInput(inputId = "v_opponent",
                    label = "Opponent",
                    choices = elo_2() %>% 
                        filter(consensus == input$v_class_2) %>% 
                        filter(crypto != input$v_crypto) %>% 
                        clean_crypto())
    })
    output$top_5_table <- renderDataTable({
        elo_1() %>% 
            filter(consensus == input$v_class_1) %>% 
            group_by(crypto) %>% 
            arrange(desc(elo)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            top_n(elo, n = 5) %>% 
            arrange(desc(elo)) %>% 
            select(crypto, elo) %>% 
            mutate(rank = row_number())     
    })
    output$elo_timeseries <- renderPlotly({
        elo_timeseries_df <- elo_1() %>% filter(consensus == input$v_class_1)
        
        top_5_cryptos <- elo_timeseries_df %>% 
            group_by(crypto) %>% 
            arrange(desc(elo)) %>% 
            slice(1) %>% 
            ungroup() %>% 
            top_n(elo, n = 5) %>% 
            select(crypto)
        
        ggplotly(
            ggplot(data = elo_timeseries_df, aes(x = date, y = elo)) + 
                geom_point() + 
                geom_point(data = elo_timeseries_df %>% filter(crypto %in% top_5_cryptos$crypto),
                           aes(x = date, y = elo, color = crypto)) +
                theme(legend.position = "top")
        )
    })
    output$elo_dist <- renderPlotly({
        ggplotly(ggplot(data = elo_1() %>% filter(consensus == input$v_class_1), aes(x = elo)) + geom_histogram())
    })
    output$crypto_card <- renderValueBox({
        valueBox(
            value = paste(round(100*predict(elo(), data.frame(crypto = input$v_crypto, opponent = input$v_opponent)),0), "%", sep = ""),
            subtitle = paste(input$v_crypto, " Probability", sep = ""),
            color = "blue",
            icon = icon("code-fork")
        )
    })
    output$opponent_card <- renderValueBox({
        valueBox(
            value = paste(round(100*predict(elo(), data.frame(crypto = input$v_opponent, opponent = input$v_crypto)),0), "%", sep = ""),
            subtitle = paste(input$v_opponent, " Probability", sep = ""),
            color = "red",
            icon = icon("link")
        )
    })
    
    output$crypto_radar <- renderPlotly({
        radar_df <- df %>%
            select(crypto, match_id,avg_Ecosystem_Structure, avg_Roadmap_Progress, avg_Market_Opportunity, avg_Underlying_Technology, avg_token_Economics, avg_Token_Performance,avg_Core_Team) %>% 
            drop_na() %>% 
            group_by(crypto) %>% 
            filter(match_id == max(match_id)) %>% 
            ungroup() %>% 
            select(-match_id) %>% 
            rename_all(~str_replace_all(.x, "avg_", "") %>% str_to_title()) %>% 
            pivot_longer(-Crypto) %>% 
            filter(Crypto == input$v_crypto_desc)
        
        plot_ly(
            type = "scatterpolar",
            r = radar_df$value,
            theta = radar_df$name,
            fill = "toself"
        ) %>% 
            layout(title = paste0(input$v_crypto_desc))
    })
}

shinyApp(ui = ui, server = server)

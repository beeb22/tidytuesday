library(rsconnect)
library(shiny)
library(tidyverse)
library(tidytuesdayR)
library(gghighlight)

big_mac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv') 

big_mac_countries <- big_mac %>%
  filter(date == as.Date("2020-07-01")) %>%
  mutate(dollar_ratio = dollar_price/5.71 - 1)
  
big_mac_time <- big_mac %>%
  select(date, name, dollar_price) %>%
  pivot_wider(names_from = name, values_from = dollar_price) %>%
  mutate(across(2:last_col(), function(x) x/`United States` - 1)) %>%
  pivot_longer(cols = c(2:last_col()), names_to = "country", values_to = "dollar_ratio")


ui <- fluidPage(
  titlePanel("Big Mac Index"),
  selectInput("name", "Country", choices = big_mac_countries$name, selected = "Britain"),
  textOutput("explanation"),
  br(),
  br(),
  plotOutput("dollar_ratio"),
  br(),
  br(),
  plotOutput("overtime")
)

server <- function(input, output) {
  
  output$explanation <- renderText(
    paste0("In economics, the law of one price implies that identical goods should cost the same no matter which country you're in (under multiple stringent conditions!). 
           This should mean that, when goods' local price is expressed in dollars using the exchange rate, there is no difference between the US price and local price. 
           One fun way of testing this is through the Big Mac index: since the Big Mac is a relatively homogenous good (it doesn't differ much between countries), its dollar price
           should be similar between countries. When the local price of Big Macs is actually expressed in dollars, using exchange rates, though, this does not turn out to be the case,
           implying that currencies may be over-valued or under-valued compared to the dollar.")
  )

  output$dollar_ratio <- renderPlot(
    big_mac_countries %>%
      mutate(iso_a3 = factor(iso_a3)) %>%
      ggplot()+
      geom_point(aes(x = fct_reorder(iso_a3, dollar_ratio),
                     y = dollar_ratio, col = dollar_ratio >= 0), show.legend = F, size = rel(5))+
      geom_point(data = big_mac_countries %>%
                   filter(name == input$name),
                 aes(x = fct_reorder(iso_a3, dollar_ratio),
                     y = dollar_ratio), col = "darkblue",
                 show.legend = F, size = rel(5))+
      geom_point(data = big_mac_countries %>%
                   filter(name == "United States"),
                 aes(x = fct_reorder(iso_a3, dollar_ratio),
                     y = dollar_ratio), col = "black", fill = "darkblue",
                 show.legend = F, size = rel(5),
                 shape = 1)+
      geom_hline(yintercept = 0, col = "lightgray")+
      geom_segment(aes(y = dollar_ratio, yend = 0, x = iso_a3, xend = iso_a3), col = "lightgray")+
      geom_text(data = big_mac_countries %>%
                  filter(name == input$name & name != "United States"),
                aes(y = ifelse(dollar_ratio >= 0, dollar_ratio + 0.05, dollar_ratio - 0.05), x = iso_a3, label = name),
                family = "Helvetica", size = rel(5), hjust = 0.2)+
      geom_text(data = big_mac_countries %>%
                  filter(name == "United States"),
                aes(y = ifelse(dollar_ratio >= 0, dollar_ratio + 0.05, dollar_ratio - 0.05), x = iso_a3, label = "US"),
                family = "Helvetica", size = rel(5), hjust = 0.5)+
      geom_text(aes(y = 0.05, x = 1), label = "Overvalued", col = "lightblue", hjust = 0, size = rel(5),
                family = "Helvetica")+
      geom_text(aes(y = -0.05, x = 1), label = "Undervalued", col = "red", hjust = 0, size = rel(5),
                family = "Helvetica")+
      scale_y_continuous(labels = scales::percent)+
      labs(
        x = "",
        y = "",
        title = paste0("A Big Mac in",
                       ifelse(input$name %in% c("United Arab Emirates", "Euro area", "Czech Republic", "United States", "Philippines"), " the ", " "),
                       input$name,
                       " was ",
                       round(abs((big_mac_countries %>%
                                    filter(name == input$name))$dollar_ratio)*100, 2),
                       "%",
                       ifelse((big_mac_countries %>%
                                 filter(name == input$name))$dollar_ratio >= 0, " overvalued", " undervalued"),
                       " against the US dollar in July 2020."),
        subtitle = "Each dot is a different country, and its position reflects its currency's valuation against the US dollar based on the Big Mac index"
      )+
      theme_minimal(base_family = "Helvetica", base_size = 18)+
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_blank()
      )
  )
  
  output$overtime <- renderPlot(
    big_mac_time %>%
      ggplot()+
      geom_point(aes(x = date, y = dollar_ratio, col = dollar_ratio >= 0), 
                 show.legend = F, size = rel(5))+
      geom_path(data = big_mac_time %>%
                  filter(!is.na(dollar_ratio)),
                aes(x = date, y = dollar_ratio, group = country))+
      gghighlight(country == input$name)+
      geom_hline(aes(yintercept = 0), alpha = 0.4)+
      scale_y_continuous(labels = scales::percent)+
      labs(
        x = "",
        y = "",
        title = paste0("The percentage difference between the dollar price of a Big Mac in ",
                       ifelse(input$name %in% c("United Arab Emirates", "Euro area", "Czech Republic", "United States", "Philippines"), "the ", ""),
                       input$name,
                       " compared to the US, 2000-2020"),
        subtitle = "Points above 0 imply the currency was overvalued compared to the US dollar; those below suggest it was undervalued"
      )+
      theme_minimal(base_family = "Helvetica", base_size = 18)
  )

}

shinyApp(ui, server)


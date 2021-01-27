library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

preferences <- c("Ladder score",
    "Logged GDP per capita", "Social support", "Healthy life expectancy",
    "Freedom to make life choices","Generosity",
    "Perceptions of corruption","Population (2020)","Density (P/Km²)",
    "Land Area (Km²)","Migrants (net)","Cost of Living Index",
    "Rent Index","Cost of Living Plus Rent Index","Groceries Index",
    "Restaurant Price Index","Local Purchasing Power Index")

controls = dbcCardGroup(
    list(
        dbcCard(
            dbcCardBody(
                list(
                    dbcLabel("Select Region"),
                    dccDropdown(
                        id="region",
                        value="Western Europe",
                        options = purrr::map(preferences, function(col) list(label = col, value = col))
                        
                    ))),  style = list("width" = "20rem")),
                    dbcCard(
                        dbcCardBody(
                            list(
                                dbcLabel("Select Preferences"),
                                dccDropdown(
                                    id="column_name",
                                    value="Ladder score",
                                    options = purrr::map(preferences, function(col) list(label = col, value = col))
                                )
                            )
                        )
                    )
                    )
            )
        
    



app$layout(
    dbcContainer(
        list(
        htmlH1("Country Happiness Visualization", 
               style=list('textAlign' = "center", 'marginTop' = 25,
                          "color" = "white",
                          "border-radius" = "10px",
                          "background-color" = "turquoise")),
        htmlH5("This app is designed to explore world's happiness 
                    scores and the ranking of its related matrix to help 
                    user make  country specific decisions.", 
               style=list("textAlign" = "center", "color" ="black")),
        htmlBr(),
        dbcRow(list(dbcCard(controls, color="secondary", outline=TRUE)))
        )     
)
)



app$run_server(host = '0.0.0.0')

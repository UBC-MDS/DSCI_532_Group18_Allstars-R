library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(dashTable)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
server = app$server

df <- read.csv('data/processed/happiness_merge_all.csv')
world_df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv") %>% 
    select(CODE, COUNTRY)

merged_df <- merge(df, world_df, by.x = "Country", by.y= "COUNTRY", all = TRUE)
#names(merged_df)[4] <- "Ladder_score" 
#merged_df$Ladder_score[is.na(merged_df$Ladder_score)] <- 0
merged_df$happiness_rank <- rank(-merged_df[, 4], na.last = "keep", ties.method = 'min' )

names(df)[3] <- "Region"

region_list <- unique(df[, 3])
country_list <- unique(df[, 2])

preferences <- c(
    "Ladder score",
    "Logged GDP per capita",
    "Social support",
    "Healthy life expectancy",
    "Freedom to make life choices",
    "Generosity",
    "Perceptions of corruption",
    "Population (2020)",
    "Density (P/Km²)",
    "Land Area (Km²)",
    "Migrants (net)",
    "Cost of Living Index",
    "Rent Index",
    "Cost of Living Plus Rent Index",
    "Groceries Index",
    "Restaurant Price Index",
    "Local Purchasing Power Index"
)
region_indicator <- lapply(region_list, function(x) 
    list(label = x, value = x))

preferences_indicator <- lapply(preferences, function(x) 
    list(label = x, value = x))

country_indicator <- lapply(country_list, function(x) 
    list(label = x, value = x))

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
)

world_fig <- plot_geo(merged_df)
world_fig <- world_fig %>% add_trace(
    z = ~happiness_rank, color = ~happiness_rank, colors = 'Blues',
    text = ~Country, locations = ~CODE, marker = list(line = l)
)
world_fig <- world_fig %>% colorbar(title = 'Happiness World Rank')
world_fig <- world_fig %>% layout(
    title = 'World Happiness Ranking',
    geo = g,
    clickmode = 'event+select'
)


app$layout(
    htmlDiv(list(
        htmlH1(
            id="site_header",
            "Country Happiness Visualization"
        ),
        htmlH5(
            "This app is designed to explore world's happiness
                    scores and the ranking of its related matrix to help
                    user make  country specific decisions.",
            style = list("textAlign" = "center", "color" = "black")
        ),
        htmlBr(),
        htmlDiv(id = "topbar", list(
            htmlDiv(list(
                dbcLabel("Select Region"),
                dccDropdown(
                    id = 'region',
                    value = 'Western Europe',
                    options = region_indicator
                ))
            ),
            htmlDiv(list(
                dbcLabel("Select Preferences"),
                dccDropdown(
                    id = 'column_name',
                    value = 'Ladder score',
                    options = preferences_indicator
                ))
            ),
            htmlDiv(list(
                dbcLabel("Select Country"),
                dccDropdown(
                    id = 'country_name',
                    options = country_indicator
                ))
            )
        )),
        htmlBr(),
        htmlDiv(list(
            dccGraph(
                id = 'world_map',
                figure = world_fig)
        ))#continue adding here
        )
    )
)

app$callback(
    output=list(id='country_name', property='options'),
    params=list(input(id='region', property='value')),
    function(input_value) {
        filtered_df <- df %>% filter(Region == input_value)
        filtered_list <- unique(filtered_df[, 2])
        
        lapply(filtered_list, function(x) list(label = x, value = x))
})



app$run_server(debug = T)

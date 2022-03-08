library(dash)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(purrr)

#' Get COVID-19 data as data frame
#'
#' Retrieve covid data in pandas dataframe format witg tge time periods provided
#'
#' @return data.frame
#' @export
#'
#' @examples
#' get_data()
get_data <- function() {
  url <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"

  tryCatch(
    {
      df <- readr::read_csv(url)
    },
    error = function(e) {
      stop("The link to the data is broken.")
    }
  )

  columns <- c(
    "iso_code",
    "continent",
    "location",
    "date",
    "total_cases",
    "new_cases",
    "total_deaths",
    "new_deaths",
    "total_cases_per_million",
    "new_cases_per_million",
    "total_deaths_per_million",
    "new_deaths_per_million",
    "icu_patients",
    "icu_patients_per_million",
    "hosp_patients",
    "hosp_patients_per_million",
    "weekly_icu_admissions",
    "weekly_icu_admissions_per_million",
    "weekly_hosp_admissions",
    "weekly_hosp_admissions_per_million",
    "total_vaccinations",
    "people_vaccinated",
    "people_fully_vaccinated",
    "new_vaccinations",
    "population"
  )

  df <- df %>% dplyr::select(all_of(columns))
  df <- dplyr::filter(df, !stringr::str_detect(iso_code, "^OWID"))
}

#' Get COVID-19 data as data frame
#'
#' Retrieve covid data in pandas dataframe format witg tge time periods provided
#'
#' @param date_from Start date of the data range with format like '2021-10-31'.
#' @param date_to End date of data range with format like '2021-10-31'.
#' @param countries Charactor vector of target country names. By default it retrieves all countries
#'
#' @return data.frame
#' @export
#'
#' @examples
#' get_data(date_from = "2022-01-01", date_to = "2022-01-07", location = c("Canada", "United State"))
filter_data <- function(df, date_from, date_to, countries) {
  if (missing(date_from)) {
    date_from <- df$date %>% min()
  }

  if (missing(date_to)) {
    date_to <- df$date %>% max()
  }

  df <- df %>%
    dplyr::filter(date >= date_from, date <= date_to)

  if (!missing(countries)) {
    df <- df %>%
      dplyr::filter(location %in% countries)
  }

  df
}

app <- Dash$new(external_stylesheets = dbcThemes$FLATLY)

df <- get_data()
filter_df <- filter_data(df, date_from = "2022-02-01", countries = c("Canada", "United States", "Germany"))

# Country selector
country_selector <- dccDropdown(
 id = "country-selector",
  multi = TRUE,
  options = df$location %>% unique() %>% purrr::map(function(col) list(label = col, value = col)),
  value=c("Canada", "United States", "United Kingdom", "France", "Singapore"),
)

map_tab <- dbcRow(
  list(
    htmlP(" "),
    htmlP(
      "Animated World Map",
      style = list("font-size" = "25px"),
    ),
    htmlP(
      "The map below depicts the selected COVID-19 indicator for the selected countries. Use the play button to animate the timeline of this indicator over the date range selected by the slider above.",
    ),
    htmlB("Indicator:"),
    htmlP(
      "Select an indicator to explore on the map and line plot using the dropdown below.",
    ),
    htmlBr(),
    htmlBr(),
#    feature_dropdown,
    dccLoading(
      dccGraph(
        id = "map-plot",
        style = list("height" = "70vh")
      )
    )
  )
)

app$layout(
  dbcContainer(
    dbcRow(
      list(
        dbcCol(dbcRow(
          list(
            htmlP("sidebar"),
            dccDropdown(
              id='col-select',
              options = msleep %>%
                colnames() %>%
                purrr::map(function(col) list(label = col, value = col)), 
              value='bodywt'),
            country_selector
            )
          ),
          width = 2
        ),
        dbcCol(
          list(
            dbcRow(
              list(
                htmlP(" "),
                htmlB("date_display"),
                htmlBr(),
                htmlBr(),
                htmlP(" "),
                htmlB("date_slider"),
                htmlBr(),
                htmlBr(),
                htmlP(" "),
                dbcTabs(
                  list(
                    dbcTab(
                      map_tab,
                      label = "Global COVID-19 Map",
                      tab_id="map-tab"
                      ),
                    dbcTab(
                      label="Global COVID-19 Plot",
                      tab_id="line-tab"
                      ),
                    dbcTab( 
                      label="Vaccination and Hospitalization Indicators",
                      tab_id="charts-tab"
                      )
                  )
                )
              )
            )
          ),
          width = 10
        )
      )
    ),
    fluid=TRUE
  )
)

app$callback(
  output('map-plot', 'figure'),
  list(input('col-select', 'value'),
       input('country-selector', 'value')),
  function(xcol, countries) {
    
    filter_df <- filter_data(df, countries=countries)
    
    p <- ggplot(msleep, aes(x = !!sym(xcol),
                            y = sleep_total,
                            color = vore,
                            text = name)) +
      geom_point() +
      scale_x_log10() +
      ggthemes::scale_color_tableau()
    ggplotly(p)
  }
)

app$run_server(host = "0.0.0.0")

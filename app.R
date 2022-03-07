library(dash)
#library(dashHtmlComponents)

app = Dash$new()

app$layout(div('I am alive!!'))

app$run_server(debug = T)
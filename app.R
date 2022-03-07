library(dash)
#library(dashHtmlComponents)

app = Dash$new()

app$layout(div('I am alive!!'))

app$run_server(host = '0.0.0.0')
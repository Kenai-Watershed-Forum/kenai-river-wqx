# render_parameter_plots()
#
# Renders the two faceted boxplots (tributary + river mile) produced by
# static_boxplot_function.R. Must be called as a top-level expression in a
# knitr chunk so that the returned htmltools::tagList is auto-printed and
# captured as widget output. Calling this via source() alone will NOT capture
# widget output — call the function after sourcing this file.
#
# Arguments:
#   plots  Named list with elements $tributary_plot and $river_mile_plot,
#          as returned by create_facet_plots() in static_boxplot_function.R.
#
# clean_plotly_legend() is expected to already be defined in the session
# (it is sourced as part of static_boxplot_function.R).

render_parameter_plots <- function(plots) {
  if (knitr::is_html_output()) {
    htmltools::tagList(
      plotly::ggplotly(plots$tributary_plot, tooltip = "text", height = 700) |>
        clean_plotly_legend(),
      plotly::ggplotly(plots$river_mile_plot, tooltip = "text", height = 550) |>
        clean_plotly_legend()
    )
  } else {
    print(plots$tributary_plot)
    print(plots$river_mile_plot)
  }
}

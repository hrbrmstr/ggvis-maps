Making maps with ggvis with some examples showing:

-   basic map creation
-   basic maps with points/labels
-   dynamic choropleths (with various scales)
-   applying projections and custom color fills
-   apply projections and projecting coordiantes for plotting

The `R` file lets you play interactively with each example. The `Rmd` file shows what the plots look like in static format. The Shiny code (`ui.R`, `server.R`) provides an interactive version in a Shiny app context.

One of the caveats with `ggvis` is that you need an online context for interactively (making things like interactive choropleths useless without such a context). You'd need to account for this in the `Rmd` by setting up a faux-facet series for the maps, either with a `<table>` or `<div>` grid.

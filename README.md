# Double_Y_axis_plotly
This repo contains a function that aligns the gridlines of plots with a double y axis created in plotly using R. This can be used for both positive and negative values. Simply source it :)

Big thanks to Victor, whose code has ben adapted to create this function https://github.com/VictorBezak/Plotly_Multi-Axes_Gridlines 

This function accepts as inputs a vector of values for each of the Y's you want to plot and the number of desired gridlines and outputs a data frame with the correct y range and dtick arguments to be used inside plotly's layout as follows:
  layout(
    title = "", 
    yaxis = list(
      title = "",
      range=c(parameters$y1_range_min, parameters$y1_range_max),
      dtick=parameters$y1_dtick
    )
and similarly for the second y. 

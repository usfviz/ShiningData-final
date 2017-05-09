# ShiningData- Airbnb dataset

| **Name**  | Evelyn Peng | Vyakhya Sachdeva|
|----------:|:-------------|:-------------|
| **Email** | ypeng25@usfca.edu |vsachdeva@usfca.edu|

Instructions
----------------------

The following packages must be installed prior to running this code:

- `ggplot2`
- `shiny`
- `leaflet`
- `plotly`
- `sunburstR`



To run this code, please enter the following commands in R:

```
library(shiny)
shiny::runGitHub('usfviz', 'ShiningData-', subdir='project-prototype')
```

This will start the `shiny` app. See below for details on how to interact with the visualization.

## Data

Data collect from website Inside Airbnb (http://insideairbnb.com/). It is an independent, non-commercial dataset that allows you to explore how Airbnb is really being used in cities around the world.

This dataset contains 8,706 listings in San Francisco of 6,919 hosts from 2008-2017.

## Tools
Use ggplot2 to make scatter plots, boxplots, etc., and RShiny to perform linking, filtering and other interactions. Also leverage ggvis to create time series animation plot to visualize the growth of AirBnBs in San Francisco.
## Techniques
  ● Geo-scatter plots: Create a scatter plot on San Francisco map, where size/color will be controlled with different parameters like price, rating, etc. There will be a toggle to select one of these parameters. Hovering on a point would show all the other properties about that listing.
  
  ● Box and whisker plot: This plot is linked to the geo-scatter plot. It is controlled using the same toggle of price, rating, etc, and will display the median and quartiles of the selected feature. In addition, we link this with the geo-scatter plot.
  
  ● Change over time (Star plot): Make animated time series plot that show the growth in number of listings in San Francisco.
  
  ● Bar Chart: Horizontal bars to show comparisons among different regions. Results are sorted by value with confidence interval. This gives a general idea of how precise the measurement is, or conversely, how far from the reported value the true (error free) value might be.
  
  ● Sunburst Diagram: It shows hierarchy propotion through a series of rings. Rings are sliced up and divided based on their hierarchical relationship to the parent slice. The angle of each slice is proportional to total number (counts) of each value.

## Discussion

Below are three screenshots of the interface of the shiny app.

![IMAGE](app1.png)

![IMAGE](app2.png)

![IMAGE](app3.png)

## Interactions
  ● Geo-scatter plot: Drop down selection option for region, room type, cancellation policy. Listings that meet the criteria be highlighted.
  
  ● Linkage between geo-scatter plot and box-and-whisker plot. Hovering/brushing on the box-and-whisker plot highlight the listings belonging to that range.
  
  ● Bar Chart: User is able to select which value to be plotted and sorted by.
  
  ● Sunburst Diagram: Hover on each node and display detail on the left upper corner.


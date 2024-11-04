# mikit
This package will contain functions to help with data visualization. 

Currently, it contains one function: `likertbar()`.

`likertbar()` takes a data frame with likert data and generates a percentage stacked bar plot using `ggplot2::geom_bar()`. 
If data is in wide form, `likertbar()` will convert it to long form and add a `Count` column before plotting. It offers options
to customize the plot in the following ways: 
- Reverse the order of sub-bars within each bar,
- Sort bars based on different criteria,
- Choose between horizontal or vertical bar orientations,
- Handle missing values with flexible options, and
- Customize colors for the sub-bars.

# How to Install
You can install this package through use devtools:
```
devtools::install_github("Miki273/mikit")
```

# Univariate Analysis Function for R

A robust and automated R function designed to perform **Exploratory Data Analysis (EDA)**. This script iterates through a dataset and generates summary statistics and visualizations for each variable, automatically adapting the output based on the data type (Numerical vs. Categorical).

## Key Features

* **Smart Variable Detection**: Automatically determines if a variable is numerical, categorical, or an ID.
* **Auto-Recoding**: Includes logic to automatically detect and label common coded patterns:
    * **Gender**: Detects column names like "sex", "gender", or "genero" (Spanish) with values `1, 2` and maps them to `Female, Male`.
    * **Binary**: Maps values `0, 1` to `No, Yes`.
    * **Ordinal**: Maps values `1, 2, 3` to `Low, Medium, High`.
* **ID Handling**: Identifies unique Identifier columns (IDs) and skips plotting them to save processing time.
* **Flexible Visualization**: Supports both modern **`ggplot2`** (default) and Base R graphics.
* **Customizable**: Options to switch between Histograms/Boxplots for numbers and Bar/Pie charts for categories.

##  Dependencies

To use the full capabilities (default `ggplot` mode), you need the `ggplot2` package installed.

```r
install.packages("ggplot2")
library(ggplot2)

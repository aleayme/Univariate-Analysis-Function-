univariant_analysis <- function(data, 
                                variable = NULL,
                                id_check = FALSE,
                                plot_type = "ggplot", 
                                plot_num = "hist",     
                                plot_cat = "bar",      
                                max_cat_plot = 50, 
                                num_cat_threshold = 15) { 
  
  # If variable not specified, analyze all variables from dataset 
  if (is.null(variable)) {
    variables <- names(data)
  } else {
    variables <- variable
  }
  
  # Header for each variable
  for (var in variables) {
    cat("\n======================================================\n")
    cat("Summary of the variable:", var, "\n")
    
    # individual selection of the column
    original_col <- data[[var]]
    current_col <- original_col 
    
    # Lowercase the variable name for matching 
    var_lower <- tolower(var)
    
    # number of rows and unique values 
    n_rows <- nrow(data)
    n_unique <- length(unique(current_col))
    ordered_uniq_val <- sort(unique(current_col))
    
    # classification of the variable/column 
    is_numeric <- is.numeric(original_col)
    # Logic: It is categorical if it's NOT numeric, OR if it IS numeric but has few unique values
    is_categorical <- !is_numeric || (is_numeric && n_unique <= num_cat_threshold)
    
    # ID check, false by default; TRUE when trying to check equal number of samples and unique values
    if (!id_check) {
      if (grepl("id$", var_lower) || grepl("^id", var_lower) || grepl("_id_", var_lower) || grepl("\\.id", var_lower)) {
        cat("   Type: Identifier (ID) - Skipping analysis as per id_check = FALSE\n\n")
        next # Skip to the next iteration of the loop
      }
    }
    
    # if for independet analysis on variable from id check 
    if (is_categorical) {
      
      if (is_numeric) {
        
        # Detection of gender variable by name and values
        if ((grepl("gender", var_lower) || grepl("genero", var_lower) || grepl("sex", var_lower)) && 
            all(ordered_uniq_val %in% c(1, 2))) {
          current_col <- factor(current_col, levels = c(1, 2), labels = c("Female", "Male"))
          cat("   Type: Categorical (Name similar to gender: 1->Female, 2->Male)\n")
          
          # Binary values (0, 1) -> No/Yes
        } else if (all(ordered_uniq_val %in% c(0, 1))) {
          current_col <- factor(current_col, levels = c(0, 1), labels = c("No", "Yes"))
          cat("   Type: Categorical (0/1 -> No/Yes)\n")
          
          # Ordinal variables (1, 2, 3) -> Low/Medium/High
        } else if (all(ordered_uniq_val %in% c(1, 2, 3))) {
          current_col <- factor(current_col, levels = c(1, 2, 3), labels = c("Low", "Medium", "High"))
          cat("   Type: Categorical (1/2/3 -> Low/Medium/High)\n")
          
        } else {
          current_col <- factor(current_col) 
          cat("   Type: Categorical (Codified numerical)\n")
        }
        
      } else {
        current_col <- factor(current_col) 
        cat("   Type: Categorical / Text\n")
      }
      
      # Summary of categorical variable
      cat("   Unique values:", n_unique, "\n")
      counting <- table(current_col)
      
      print(counting)
      cat("\n")
      
      # Categorical graphs
      if (n_unique <= max_cat_plot) {
        df_temp <- data.frame(Category = current_col)
        
        # barplot
        if (plot_cat == "bar") {
          cat("   Bar plot...\n")
          if (plot_type == "ggplot") {
            p <- ggplot(df_temp, aes(x = Category, fill = Category)) + 
              geom_bar(color = "black", alpha = 0.8) +
              labs(title = paste("Barplot of", var), x = var, y = "Frequency") +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_fill_viridis_d(option = "A", guide = "none") 
            print(p)
          } else {
            barplot(counting, main = paste("Barplot of", var), col = rainbow(length(counting)))
          }
          
          # pie chart
        } else if (plot_cat == "pie") {
          cat("   Pie chart...\n")
          df_pie <- as.data.frame(counting)
          names(df_pie) <- c("Category", "Frequency")
          
          if (plot_type == "ggplot") {
            p <- ggplot(df_pie, aes(x = "", y = Frequency, fill = Category)) +
              geom_bar(stat = "identity", width = 1, color="white") +
              coord_polar("y", start = 0) + 
              labs(title = paste("Pie Chart of", var), x = NULL, y = NULL) +
              theme_void() +
              scale_fill_viridis_d(option = "A")
            print(p)
          } else {
            pie(counting, main = paste("Pie Chart of", var), col = rainbow(length(counting)))
          }
        }
      }
      
    } else {
      # Numerical category 
      cat("   Type: Numerical Continuous\n")
      
      # Basic summary statistics
      print(summary(original_col))
      cat("\n")
      
      # histogram
      if (plot_num == "hist") {
        cat("   Histogram...\n")
        # with ggplot 
        if (plot_type == "ggplot") {
          p <- ggplot(data, aes_string(x = var)) +
            geom_histogram(aes(fill = ..x..), bins = 30, color = "black", alpha = 0.8) +
            scale_fill_gradient(low = "mediumpurple", high = "salmon") + 
            labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
            theme_minimal() +
            theme(legend.position = "none")
          print(p)
        }
        # with R base
        else {
          hist(original_col, main = paste("Histogram of", var), col = "lightblue")
        }
        
        # boxplot 
      } else if (plot_num == "box") {
        cat("   Boxplot...\n")
        # with ggplot
        if (plot_type == "ggplot") {
          p <- ggplot(data, aes_string(x = "factor(1)", y = var)) + 
            geom_boxplot(fill = "mediumpurple", alpha = 0.7, outlier.colour = "grey") + # grey transparency outliers to check frequency
            labs(title = paste("Boxplot of", var), y = var, x = "") +
            theme_minimal() +
            theme(axis.text.x = element_blank()) 
          print(p)
        } 
        # with R base
        else {
          boxplot(original_col, main = paste("Boxplot of", var), col = "mediumpurple")
        }
      }
    }
  }
}

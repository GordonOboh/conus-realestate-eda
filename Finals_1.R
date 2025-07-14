# Define a vector of package names to install
packages_to_install <- c("tidyverse","here","skimr", "janitor", "dplyr", "Hmisc", "ggplot2","ggmap", "ggcorrplot", "scales")#,"tmap") #

# Install the specified packages from the vector (if not already installed)
#install.packages(packages_to_install)

# Load all the installed packages using lapply
# 'character.only = TRUE' ensures that the package names are treated as strings
lapply(packages_to_install, library, character.only = TRUE)

setwd("/home/gordon/Documents/617/Finals/")
getwd()
# Read the CSV file 'housing.csv' and load it into a data frame
df_housing <- readr::read_csv('housing.csv')



######################EDA######################
head(df_housing, 2)
str(df_housing)
#colnames(df_housing)
summary(df_housing)

df_housing_vars_of_interest <- df_housing %>% select(-id, -url, -region_url, -image_url)

ggplot(df_housing_vars_of_interest, aes(price, type)) + geom_boxplot()

df_housing_vars_of_interest_1 <- df_housing_vars_of_interest %>%
  group_by(type) %>% # Group by 'type'
  filter({
    Q1 <- quantile(price, 0.25, na.rm = TRUE)
    Q3 <- quantile(price, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    price >= lower_bound & price <= upper_bound
  }) %>% ungroup() 
ggplot(df_housing_vars_of_interest_1, aes(x = price, y = type)) + geom_boxplot()


######################Cleaning######################
# Function to remove outliers for multiple specified columns
remove_outliers_multi <- function(df, columns, group_col) {
  df %>%
    group_by(across(all_of(group_col))) %>% # Group by the specified column
    filter({
      # Combine filtering conditions for all specified columns
      conditions <- lapply(columns, function(col) {
        Q1 <- quantile(get(col), 0.25, na.rm = TRUE)
        Q3 <- quantile(get(col), 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        get(col) >= lower_bound & get(col) <= upper_bound
      })
      Reduce(`&`, conditions) # Combine conditions with logical AND
    }) %>%
    ungroup() # Ungroup after filtering
}

# Specify the columns to check for outliers
columns_to_check <- c("price", "sqfeet", "beds", "baths")
group_column <- "type"  

# Apply the function to the dataset
df_housing_cleaned <- remove_outliers_multi(
  df = df_housing_vars_of_interest,
  columns = columns_to_check,
  group_col = group_column
)

df_housing_cleaned <- df_housing_cleaned %>% filter(lat >= 25.836333, lat <= 70, long >= -155, long <= -50)

# Capitalize the 'type' column
df_housing_cleaned <- df_housing_cleaned %>%
  mutate(type = tools::toTitleCase(type))

# Capitalize the 'state' column
df_housing_cleaned <- df_housing_cleaned %>%
  mutate(state = toupper(state))


######################Plots###########################

ggplot(df_housing_cleaned, aes(x = type, y = price, fill = type)) +
  geom_boxplot(
    outlier.color = "red", 
    outlier.size = 2, 
    alpha = 0.8, 
    color = "white") +  # Highlight outliers
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Boxplot of Price grouped by Type of Rental",
    x = "Type of Rental",
    y = "Price ($)"
  ) +
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +  # Use a colorblind-friendly palette
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white", size = 14, face = "bold"),    # Title text
    axis.title = element_text(color = "white", size = 12),                  # Axis titles
    axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
    axis.text.x = element_text(color = "white", size = 10, angle = 45, hjust = 1), # Rotated x-axis labels
    legend.text = element_text(color = "white"),                            # Legend text
    legend.title = element_text(color = "white", face = "bold"),            # Legend title
    plot.background = element_rect(fill = "black", color = NA),             # Black background
    panel.background = element_rect(fill = "black", color = NA),            # Black panel background
    panel.grid.major = element_line(color = "gray30", size = 0.5),          # Subtle gridlines
    panel.grid.minor = element_line(color = "gray30", size = 0.25)          # Subtle gridlines
  ) -> my_plot

print(my_plot)

# Save the plot to a file
ggsave(filename = "./Plots/boxplot_price_by_type.png", plot = my_plot, width = 8, height = 6, dpi = 300)


#library(scales)
ggplot(df_housing_cleaned, aes(x = type)) +
  geom_bar(fill = "skyblue", color = "lightblue") +  # Customize bar colors
  scale_y_continuous(labels = comma) +          # Format y-axis with commas
  labs(
    title = "Distribution of listings by Rental Type",
    x = "Type of Rental",
    y = "Count"
  ) +
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +  # Use a colorblind-friendly palette
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white", size = 14, face = "bold"),    # Title text
    axis.title = element_text(color = "white", size = 12),                  # Axis titles
    axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
    axis.text.x = element_text(color = "white", size = 10, angle = 45, hjust = 1), # Rotated x-axis labels
    legend.text = element_text(color = "white"),                            # Legend text
    legend.title = element_text(color = "white", face = "bold"),            # Legend title
    plot.background = element_rect(fill = "black", color = NA),             # Black background
    panel.background = element_rect(fill = "black", color = NA),            # Black panel background
    panel.grid.major = element_line(color = "gray70", size = 0.5),          # Subtle gridlines
    panel.grid.minor = element_line(color = "gray50", size = 0.25)          # Subtle gridlines
  ) -> my_plot

print(my_plot)
ggsave(filename = "./Plots/barplot_count_of_listing.png", plot = my_plot, width = 8, height = 6, dpi = 300)

# Exclude "apartment" from the dataset
df_filtered <- df_housing_cleaned #%>%
  #filter(type != "apartment")  # Remove rows where type is "apartment"

# Create the plot
ggplot(df_filtered, aes(x = type)) +
  geom_bar(fill = "skyblue", color = "lightblue") +  # Customize bar colors
  scale_y_continuous(
    limits = c(0, 20000),  # Set the y-axis range
    labels = comma         # Format y-axis with commas
  ) +
  labs(
    title = "Distribution of Listings by Rental Type (Excluding Apartment)",
    x = "Type of Rental",
    y = "Count"
  ) +
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +  # Use a colorblind-friendly palette
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white", size = 14, face = "bold"),    # Title text
    axis.title = element_text(color = "white", size = 12),                  # Axis titles
    axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
    axis.text.x = element_text(color = "white", size = 10, angle = 45, hjust = 1), # Rotated x-axis labels
    legend.text = element_text(color = "white"),                            # Legend text
    legend.title = element_text(color = "white", face = "bold"),            # Legend title
    plot.background = element_rect(fill = "black", color = NA),             # Black background
    panel.background = element_rect(fill = "black", color = NA),            # Black panel background
    panel.grid.major = element_line(color = "gray70", size = 0.5),          # Subtle gridlines
    panel.grid.minor = element_line(color = "gray50", size = 0.25)          # Subtle gridlines
  ) -> my_plot

print(my_plot)
ggsave(filename = "./Plots/barplot_count_of_listing_1.png", plot = my_plot, width = 8, height = 6, dpi = 300)


###############


# Define the variables and their corresponding labels and file names
variables <- list(
  sqfeet = list(
    x_label = "Area (Sq Feet)",
    title = "Distribution of Property Size",
    filename = "./Plots/histplot_distribution_of_area.png"
  ),
  price = list(
    x_label = "Price ($)",
    title = "Distribution of Prices",
    filename = "./Plots/histplot_distribution_of_prices.png"
  )
)

# Loop through variables and create/save plots
for (var in names(variables)) {
  ggplot(df_housing_cleaned, aes_string(x = var)) +
    geom_histogram(binwidth = 100, fill = "skyblue", color = "lightblue") +
    labs(
      title = variables[[var]]$title,
      x = variables[[var]]$x_label,
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(color = "white", size = 14, face = "bold"),    # Title text
      axis.title = element_text(color = "white", size = 12),                  # Axis titles
      axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
      axis.text.x = element_text(color = "white", size = 10, angle = 45, hjust = 1), # Rotated x-axis labels
      legend.text = element_text(color = "white"),                            # Legend text
      legend.title = element_text(color = "white", face = "bold"),            # Legend title
      plot.background = element_rect(fill = "black", color = NA),             # Black background
      panel.background = element_rect(fill = "black", color = NA),            # Black panel background
      panel.grid.major = element_line(color = "gray70", size = 0.5),          # Subtle gridlines
      panel.grid.minor = element_line(color = "gray50", size = 0.25)          # Subtle gridlines
    ) -> my_plot
  
  # Print the plot
  print(my_plot)
  
  # Save the plot to a file
  ggsave(filename = variables[[var]]$filename, plot = my_plot, width = 8, height = 6, dpi = 300)
}



######################################################

# Define the variables and their corresponding labels and file names
variables <- list(
  baths = list(
    x_label = "Number of Bathrooms",
    title = "Distribution of Bathrooms",
    filename = "./Plots/histplot_distribution_of_baths.png"
  ),
  beds = list(
    x_label = "Number of Bedrooms",
    title = "Distribution of Bedrooms",
    filename = "./Plots/histplot_distribution_of_beds.png"
  )
)

# Loop through variables and create/save plots
for (var in names(variables)) {
  ggplot(df_housing_cleaned, aes_string(x = var)) +
    geom_histogram(binwidth = 0.5, fill = "skyblue", color = "lightblue") +
    labs(
      title = variables[[var]]$title,
      x = variables[[var]]$x_label,
      y = "Count"
    ) +
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +  # Use a colorblind-friendly palette
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white", size = 14, face = "bold"),    # Title text
    axis.title = element_text(color = "white", size = 12),                  # Axis titles
    axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
    axis.text.x = element_text(color = "white", size = 10, angle = 45, hjust = 1), # Rotated x-axis labels
    legend.text = element_text(color = "white"),                            # Legend text
    legend.title = element_text(color = "white", face = "bold"),            # Legend title
    plot.background = element_rect(fill = "black", color = NA),             # Black background
    panel.background = element_rect(fill = "black", color = NA),            # Black panel background
    panel.grid.major = element_line(color = "gray70", size = 0.5),          # Subtle gridlines
    panel.grid.minor = element_line(color = "gray50", size = 0.25)          # Subtle gridlines
  ) -> my_plot
  
  # Print the plot
  print(my_plot)
  
  # Save the plot to a file
  ggsave(filename = variables[[var]]$filename, plot = my_plot, width = 8, height = 6, dpi = 300)
}



















###################


ggplot(df_housing_cleaned, aes(x = type, y = baths)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Boxplot of Number of Bathrooms by Type of rental",
       x = "Type of rental",
       y = "Number of Bathrooms") -> my_plot
print(my_plot)

# Save the plot to a file
ggsave(filename = "./Plots/boxplot_type_by_bath.png", plot = my_plot, width = 8, height = 6, dpi = 300)

ggplot(df_housing_cleaned, aes(x = baths)) +
  geom_bar(fill = "skyblue", color = "lightblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Count of Listings by Number of Bathrooms per rental", #Distribution of restrooms
       x = "Number of Bathrooms",
       y = "Count")+
  theme_minimal()  +
  theme(
    plot.title = element_text(color = "white", size = 14, face = "bold"),    # Title text
    axis.title = element_text(color = "white", size = 12),                  # Axis titles
    axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
    axis.text.x = element_text(color = "white", size = 10, angle = 45, hjust = 1), # Rotated x-axis labels
    legend.text = element_text(color = "white"),                            # Legend text
    legend.title = element_text(color = "white", face = "bold"),            # Legend title
    plot.background = element_rect(fill = "black", color = NA),             # Black background
    panel.background = element_rect(fill = "black", color = NA),            # Black panel background
    panel.grid.major = element_line(color = "gray70", size = 0.5),          # Subtle gridlines
    panel.grid.minor = element_line(color = "gray50", size = 0.25)          # Subtle gridlines
  ) -> my_plot
print(my_plot)

# Save the plot to a file
ggsave(filename = "./Plots/histplot_count_of_bath.png", plot = my_plot, width = 8, height = 6, dpi = 300)


######################################################

#Area
ggplot(df_housing_cleaned, aes(x = type, y = sqfeet, fill = type)) +
  geom_boxplot(
    outlier.color = "red", 
    outlier.size = 2, 
    alpha = 0.8, 
    color = "white") +  # Highlight outliers
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Boxplot of Property size grouped by Type of Rental",
    x = "Type of Rental",
    y = "Area (Sq Feet)"
  ) +
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +  # Use a colorblind-friendly palette
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white", size = 14, face = "bold"),    # Title text
    axis.title = element_text(color = "white", size = 12),                  # Axis titles
    axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
    axis.text.x = element_text(color = "white", size = 10, angle = 45, hjust = 1), # Rotated x-axis labels
    legend.text = element_text(color = "white"),                            # Legend text
    legend.title = element_text(color = "white", face = "bold"),            # Legend title
    plot.background = element_rect(fill = "black", color = NA),             # Black background
    panel.background = element_rect(fill = "black", color = NA),            # Black panel background
    panel.grid.major = element_line(color = "gray30", size = 0.5),          # Subtle gridlines
    panel.grid.minor = element_line(color = "gray30", size = 0.25)          # Subtle gridlines
  ) -> my_plot

print(my_plot)
# Save the plot to a file
ggsave(filename = "./Plots/boxplot_sqfeet_by_type.png", plot = my_plot, width = 8, height = 6, dpi = 300)



# Define labels for the columns
my_dict <- c(
  sqfeet = "Area (Sq Feet)",
  lat = "Latitude",
  long = "Longitude",
  price = "Price ($)"
)

columns_to_use <- c("price", "sqfeet")

df_encoded <- df_housing_cleaned 


# Loop through pairs of numeric columns
for (i in seq_along(columns_to_use)) {
  for (j in seq_along(columns_to_use)) {
    if (i != j) {
      ggplot(df_housing_cleaned, aes_string(x = columns_to_use[j], y = columns_to_use[i])) +
        geom_point(alpha = 0.5, color ='white') +
        geom_smooth(method = "lm", col = "red", se = FALSE) +  # Linear trendline
        labs(title = paste("Scatterplot of", columns_to_use[i], "vs", columns_to_use[j]),
             x = my_dict[[columns_to_use[j]]],
             y = my_dict[[columns_to_use[i]]]) +
        theme_minimal() +
        theme(
          plot.title = element_text(color = "white", size = 14, face = "bold"),    # Title text
          axis.title = element_text(color = "white", size = 12),                  # Axis titles
          axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
          axis.text.x = element_text(color = "white", size = 10, angle = 45, hjust = 1), # Rotated x-axis labels
          legend.text = element_text(color = "white"),                            # Legend text
          legend.title = element_text(color = "white", face = "bold"),            # Legend title
          plot.background = element_rect(fill = "black", color = NA),             # Black background
          panel.background = element_rect(fill = "black", color = NA),            # Black panel background
          panel.grid.major = element_line(color = "gray70", size = 0.5),          # Subtle gridlines
          panel.grid.minor = element_line(color = "gray50", size = 0.25)          # Subtle gridlines
        ) -> plot #%>%
      print(plot)
      # Generate file name
      file_name <- paste0("./Plots/scatter_plot_of_", columns_to_use[i], "_vs_", columns_to_use[j], ".png")
      # Save the plot
      ggsave(filename = file_name, plot = plot, width = 8, height = 6, dpi = 300)
    }
  }
}



# Transform 'state' to uppercase
#df_housing_cleaned <- df_housing_cleaned %>%
#  mutate(state = toupper(state))  # Convert state column to uppercase

# Define the columns to compare with price
columns_to_compare_with_price <- c("sqfeet")  # Add other columns if needed



# Create and save scatter plots
for (col in columns_to_compare_with_price) {
  # Create the scatter plot dynamically using the column name
  ggplot(df_housing_cleaned, aes_string(x = col, y = "price", color = "state")) +
    geom_point() +
    labs(
      title = paste("Price vs", my_dict[[col]]),
      x = my_dict[[col]],
      y = "Price ($)",
      color = "State"  # Legend title for clarity
    ) +
    theme_minimal()  +
    theme(
      plot.title = element_text(color = "white", size = 14, face = "bold"),    # Title text
      axis.title = element_text(color = "white", size = 12),                  # Axis titles
      axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
      axis.text.x = element_text(color = "white", size = 10, angle = 45, hjust = 1), # Rotated x-axis labels
      legend.text = element_text(color = "white"),                            # Legend text
      legend.title = element_text(color = "white", face = "bold"),            # Legend title
      plot.background = element_rect(fill = "black", color = NA),             # Black background
      panel.background = element_rect(fill = "black", color = NA),            # Black panel background
      panel.grid.major = element_line(color = "gray70", size = 0.5),          # Subtle gridlines
      panel.grid.minor = element_line(color = "gray50", size = 0.25)          # Subtle gridlines
    ) -> plot 
  
  # Print the plot (or save it if needed)
  print(plot)
  
  # Optionally, save each plot to a file
  file_name <- paste0("./Plots/price_vs_", col, ".png")
  ggsave(filename = file_name, plot = plot, width = 8, height = 6, dpi = 300)
}





# Calculate average price per average square foot for each type
df_avg_price_per_avg_sqft <- df_housing_cleaned %>%
  group_by(type) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),      # Average price for each type
    avg_sqfeet = mean(sqfeet, na.rm = TRUE),    # Average square footage for each type
    avg_price_per_avg_sqft = avg_sqfeet / avg_price  # Average price per average square foot
  ) %>%
  ungroup()

# Print the result
print(df_avg_price_per_avg_sqft)

ggplot(df_avg_price_per_avg_sqft, aes(x = reorder(type, avg_price_per_avg_sqft), y = avg_price_per_avg_sqft, fill = type)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_viridis_d(option = "C", begin = 0.2, end = 0.8) +  # Color palette
  labs(
    title = "Average Property Size per Average Price by Rental Type",
    x = "Type of Rental",
    y = "Average Property size per Average Price"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white", size = 14, face = "bold"),    # Title text
    axis.title = element_text(color = "white", size = 12),                  # Axis titles
    axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
    axis.text.x = element_text(color = "white", size = 10, angle = 45, hjust = 1), # Rotated x-axis labels
    legend.position = "none",                                               # Hide legend
    plot.background = element_rect(fill = "black", color = NA),             # Black background
    panel.background = element_rect(fill = "black", color = NA),            # Black panel background
    panel.grid.major = element_line(color = "gray70", size = 0.5),          # Subtle gridlines
    panel.grid.minor = element_line(color = "gray50", size = 0.25)          # Subtle gridlines
  ) -> plot 

# Print the plot (or save it if needed)
print(plot)

# Optionally, save each plot to a file
file_name <- paste0("./Plots/avg_price_per_avg_area", ".png")
ggsave(filename = file_name, plot = plot, width = 8, height = 6, dpi = 300)




###############Pieplot#################


library(dplyr)
library(ggplot2)

# Extend my_dict to include descriptions for laundry_options and parking_options
my_dict <- c(
  cats_allowed = "Cat Friendly",
  dogs_allowed = "Dog Friendly",
  smoking_allowed = "Smoker Friendly",
  wheelchair_access = "Wheelchair Accessiblity",
  electric_vehicle_charge = "EV Charging",
  comes_furnished = "Furnished Rental",
  laundry_options = "Laundry Options Availability",
  parking_options = "Parking Options Availability"
)

amenities <- c("cats_allowed", "dogs_allowed", "smoking_allowed", 
                      "wheelchair_access", "electric_vehicle_charge", "comes_furnished")

# Loop through columns
for (amenity in amenities) {
  # Summarize data for each column
  df_pie <- df_housing_cleaned %>%
    count(!!sym(amenity)) %>%
    #mutate(percentage = n / sum(n) * 100)
    mutate(
      !!sym(amenity) := recode(!!sym(amenity), `0` = "No", `1` = "Yes"),  # Replace 0 with "No" and 1 with "Yes"
      percentage = n / sum(n) * 100
    )
  
  # Dynamically assign colors based on unique levels in the column
  unique_levels <- unique(df_pie[[amenity]])
  colors <- scales::hue_pal()(length(unique_levels))  # Generate unique colors
  
  # Create the pie chart
  pie_chart <- ggplot(df_pie, aes(x = "", y = n, fill = factor(!!sym(amenity)))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste(" ", my_dict[[amenity]]),
         fill = my_dict[[amenity]]) +
    theme_void() +
    theme(
      plot.title = element_text(color = "white", size = 16, face = "bold"),    # Title in white
      legend.title = element_text(color = "white", size = 12),               # Legend title in white
      legend.text = element_text(color = "white", size = 10),                # Legend text in white
      plot.background = element_rect(fill = "black", color = NA),            # Black background
      panel.background = element_rect(fill = "black", color = NA)            # Black panel background
    ) +
    scale_fill_manual(values = setNames(colors, unique_levels)) +            # Custom dynamic colors
    geom_text(aes(label = paste0(round(percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), color = "white")        # White text for percentage labels
  
  # Print the plot
  print(pie_chart)
  file_path <- paste0("./Plots", "/pieplot_", amenity, ".png")
  ggsave(filename = file_path, plot = pie_chart, width = 8, height = 6, dpi = 300)
}


columns_to_check <- c("laundry_options", "parking_options")



for (amenity in columns_to_check) {
  # Summarize data for each column
  df_pie <- df_housing_cleaned %>%
    count(!!sym(amenity)) %>%
    mutate(percentage = n / sum(n) * 100) # Calculate percentage
  
  # Dynamically assign colors based on unique levels in the column
  unique_levels <- unique(df_pie[[amenity]])
  colors <- scales::hue_pal()(length(unique_levels))  # Generate unique colors
  
  
  # Create the pie chart
  pie_chart <- ggplot(df_pie, aes(x = "", y = n, fill = factor(!!sym(amenity)))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste(" ", my_dict[[amenity]]),
         fill = my_dict[[amenity]]) +
    theme_void() +
    theme(
      plot.title = element_text(color = "white", size = 16, face = "bold"),    # Title in white
      legend.title = element_text(color = "white", size = 12),               # Legend title in white
      legend.text = element_text(color = "white", size = 10),                # Legend text in white
      plot.background = element_rect(fill = "black", color = NA),            # Black background
      panel.background = element_rect(fill = "black", color = NA)            # Black panel background
    ) +
    scale_fill_manual(values = setNames(colors, unique_levels)) +            # Custom dynamic colors
    geom_text(aes(label = paste0(round(percentage, 1), "%")), 
              position = position_stack(vjust = 0.5), color = "white")        # White text for percentage labels
  
 
  
  # Print the plot
  print(pie_chart)
  file_path <- paste0("./Plots", "/pieplot_", amenity, ".png")
  ggsave(filename = file_path, plot = pie_chart, width = 8, height = 6, dpi = 300)
}









######################MAPS######################

register_stadiamaps(key = "f350ad5b-9f8d-4fa2-a02e-f22840a9b4d8")

# Define bounding box
bbox <- c(left = -155, bottom = 20, right = -50, top = 66)

# Fetch the base map
base_map <- get_stadiamap(bbox = bbox, zoom = 6, maptype = "stamen_toner")



# Display the map with overlay
ggmap(base_map) +
  geom_point(data = df_housing_cleaned, aes(x = long, y = lat, color = sqfeet), alpha = 0.3, size = 2) +
  scale_color_viridis_c() +
  labs(title = "Property Size Distribution across CONUS",
       x = "Longitude",
       y = "Latitude",
       color = "Area (Sq Feet)") +
  theme_minimal() +
  theme(
        plot.title = element_text(color = "white", size = 14, face = "bold"),   # Title text
        axis.title = element_text(color = "white", size = 12),                  # Axis titles
        axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
        legend.text = element_text(color = "white"),                            # Legend text
        legend.title = element_text(color = "white", face = "bold"),            # Legend title
        plot.background = element_rect(fill = "black", color = NA),       # Optional: Black background
        panel.background = element_rect(fill = "black", color = NA)             # Optional: Black panel background
  ) -> plot

# Print the plot (or save it if needed)
print(plot)

# Optionally, save each plot to a file
file_name <- paste0("./Plots/scatterplot_of_USA_with_map_sqfeet", ".png")
ggsave(filename = file_name, plot = plot, width = 8, height = 6, dpi = 600)



# Define bounding box
bbox <- c(left = -155, bottom = 20, right = -50, top = 66)

# Fetch the base map
base_map <- get_stadiamap(bbox = bbox, zoom = 6, maptype = "stamen_toner")

# Aggregate data: calculate average price per region (latitude and longitude)
df_avg_sqfeet <- df_housing_cleaned %>%
  group_by(region) %>%
  summarise(
    avg_sqfeet = mean(sqfeet, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    long = mean(long, na.rm = TRUE)
  ) %>%
  ungroup()

# Display the map with overlay showing average price per region
plot <- ggmap(base_map) +
  geom_point(data = df_avg_sqfeet, aes(x = long, y = lat, color = avg_sqfeet), alpha = 0.8, size = 3) +
  scale_color_viridis_c(option = "C") +  # Use a perceptually uniform color scale
  labs(title = "Average Property Area across CONUS",
       x = "Longitude",
       y = "Latitude",
       color = "Average Area") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "white", size = 14, face = "bold"),   # Title text
    axis.title = element_text(color = "white", size = 12),                  # Axis titles
    axis.text = element_text(color = "white", size = 10),                   # Axis tick labels
    legend.text = element_text(color = "white"),                            # Legend text
    legend.title = element_text(color = "white", face = "bold"),            # Legend title
    plot.background = element_rect(fill = "black", color = NA),       # Optional: Black background
    panel.background = element_rect(fill = "black", color = NA)             # Optional: Black panel background
  )

# Print the plot
print(plot)

# Save the plot
file_name <- paste0("./Plots/average_sqfeet_per_region_map.png")
ggsave(filename = file_name, plot = plot, width = 10, height = 8, dpi = 600)


write.csv(installed.packages()[, c("Package", "Version")], "installed_packages.csv", row.names = FALSE)

package_versions <- sapply(packages_to_install, function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    as.character(packageVersion(pkg))
  } else {
    "Not Installed"
  }
})


package_versions




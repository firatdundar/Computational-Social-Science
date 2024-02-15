install.packages("httr", repos = "http://cran.us.r-project.org")
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
install.packages("dplyr")
install.packages("magrittr")
install.packages("stringr")
install.packages("knitr")


library(httr) 
library (tidyverse)
library(dplyr)
library(magrittr)
library(stringr)
library(knitr)

# Base URL for search results on World Politics Review
base_url_world_politics <- "https://www.worldpoliticsreview.com/"

# Search query
# For this scraping we are focusing on politic news around the world about migration
search_query <- "?s=migration&post_type=post"

# Number of pages to scrape
# Can be updated for wider researches
num_iterations <- 3

# Create an empty list container for HTML code
html_code_world_politics <- vector("list", length = num_iterations)

# Loop through search pages and collect the HTML code
for (i in seq_along(1:num_iterations)) {
  # Generate the URL for the current iteration
  current_url <- ifelse(i == 1, paste0(base_url_world_politics, search_query), paste0(base_url_world_politics, "page/", i, "/", search_query))
  
  # Access the page to retrieve information
  page <- httr::GET(current_url) 
  # Extract HTML content
  page_content <- httr::content(page, "text") 
  # Add raw page content to the list
  html_code_world_politics[[i]] <- page_content 
}

# Go through the HTML code of each search page
for (i in seq_along(html_code_world_politics)) { 
  # Extract class with the URLs with the specified format
  extracted_urls <- str_extract_all(html_code_world_politics[[i]], '<a class="block mb-15px lg:mb-6" href="([^"]+)"[^>]+>')
  
  # Flatten the list and extract only the URLs
  extracted_urls <- unlist(extracted_urls)
  
  # Extract only the URLs without the additional characters
  extracted_urls <- str_extract(extracted_urls, 'https://www.worldpoliticsreview.com/[^\\s"\'>]+')
  
  # Combine the URLs with the list
  urls_final_world_politics[[i]] <- extracted_urls
}

# Combine the list of URLs from all pages into a single vector
all_final_urls_world_politics <- unlist(urls_final_world_politics, use.names = FALSE)

# Remove any NA values
all_final_urls_world_politics <- all_final_urls_world_politics[complete.cases(all_final_urls_world_politics)]

# Print the final URLs
print(all_final_urls_world_politics)

# Creating an (almost) empty data frame in which information will be stored later

#first column (variable) stores our article urls
article_df <- data.frame (url = all_final_urls_world_politics, 
                          #variable 2: will store the text of the news
                          text = NA,
                          #variable 3: will store the region of the news
                          region = NA,
                          #variable 4: number of words in the news
                          length = NA,
                          #variable 5: type of the news (interview, Q&A or so on)
                          type = NA,
                          # by default the function data.frame() wants to convert any string data to factorial data. As we want to keep our strings as strings we will tell R manually to not transform string to factors. 
                          stringsAsFactors = FALSE) 

# Loop through each row of the data frame
for (i in 1:nrow(article_df)) {
  print(i)
  # Access each news-page to retrieve information
  raw_file <- httr::GET(article_df[i, 1])
  
  # Extract the content-part from the raw_file object
  article_content <- httr::content(raw_file, "text") 
  
  # Extract the text body inside the <p> tag
  article_df[i, 2] <- str_extract_all(article_content, '<p>(?s).*?</p>') %>%
    str_remove_all("<.*?>") %>%
    str_squish()
  
  # Remove "c(" at the beginning and ")" at the end of the text
  article_df[i, 2] <- sub("^c\\(", "", article_df[i, 2])
  article_df[i, 2] <- sub("\\)$", "", article_df[i, 2])
  
  # Extract the title attribute from the specified HTML tag
  region_title <- str_extract_all(article_content, '<a\\s+href="https://www.worldpoliticsreview.com/region/[^"]+"\\s+class="article-category[^"]+"[^>]+title="([^"]+)"') %>%
    unlist()
  
  # If there are matches, extract the value inside the title attribute which is the region
  if (length(region_title) > 0) {
    # Extract the value inside the title attribute
    region_title <- str_extract(region_title, 'title="([^"]+)"') %>%
      str_replace_all('title="|"$', '')  # Remove 'title="' and '"' from the extracted string
    
    # Update the region column in the data frame
    article_df[i, 3] <- region_title
  }
  # Calculate the number of words in the article
  article_df[i, 4] <- str_count(article_df[i, 2], '\\w+')
  
  # Extract the type of news from the specified HTML tag
  news_type <- str_extract_all(article_content, '<a\\s+href="https://www.worldpoliticsreview.com/section/[^"]+"\\s+class="article-category[^"]+"[^>]+title="([^"]+)"') %>%
    unlist()
  
  # If there are matches, extract the value inside the title attribute
  if (length(news_type) > 0) {
    # Extract the value inside the title attribute
    news_type <- str_extract(news_type, 'title="([^"]+)"') %>%
      str_replace_all('title="|"$', '')  # Remove 'title="' and '"' from the extracted string
    
    # Update the type column in the data frame
    article_df[i, 5] <- news_type
  }
  
  # Ask the system to "fall asleep" for a few seconds to avoid being blocked
  Sys.sleep(1 + sample(c(0.5, 1, 1.5, 2), 1))
  
}

# Print the updated data frame
print(article_df)

# We tell R to only keep observations that has a length that is greater than zero, thus excluding empty row
df <- article_df[article_df$length >0,]

# Filter out rows where the text is "character(0)" or empty
df <- df[df$text != "character(0" & df$text != "", ]


table (df$region)

summary (df$length)

plot(article_df$length)

ggplot(article_df, aes(x=url, y=length)) +
  geom_bar(stat = "identity")

ggplot(article_df, aes(x=region, y=length)) +
  geom_bar(stat = "identity")

ggplot(article_df, aes(x=type, y=length)) +
  geom_bar(stat = "identity")
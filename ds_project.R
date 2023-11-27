
# calling necessary packages
library(tidyverse)
library(janitor)
library(lubridate)
library(skimr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)


# loading the data
getwd()
books<-read.csv("C:\\Users\\User\\Downloads\\bestsellers with categories.csv")
View(books)

## DATA CLEANING
# checking and removing duplicates
duplicated(books)
books[duplicated(books)]
sum(is.na(books))

dim(books)

# trimming unnecessary leading and trailing whitespaces
books <- books %>%
  mutate(name = trimws(Name),
         author = trimws(Author),
         genre = trimws(Genre))

# Checking the distinct genre column values
unique_genres <- unique(books$Genre)
print(unique_genres)

books$book_count <- 1

# Creating categories for ratings
books <- books %>%
  mutate(rating_category = case_when(
    User.Rating >= 3.0 & User.Rating < 3.5 ~ "Good",
    User.Rating >= 3.5 & User.Rating < 4.0 ~ "Very Good",
    User.Rating >= 4.0 & User.Rating < 4.5 ~ "Excellent",
    User.Rating >= 4.5 ~ "Outstanding",
    TRUE ~ "Others"
  ))

# Creating categories for reviews
books$review_category <- cut(books$Reviews, 
                             breaks = c(0, 2000, 7500, 16000, 25000, Inf),
                             labels = c("Very Low Reviews", "Low Reviews", "Moderate Reviews",
                                        "High Reviews", "Very High Reviews"))

# Creating categories for prices
books <- books %>%
  mutate(price_category = case_when(
    Price == 0 ~ "Free",
    Price > 0 & Price <= 10 ~ "Low Price",
    Price > 10 & Price <= 20 ~ "Moderate Price",
    Price > 20 & Price <= 30 ~ "High Price",
    Price > 30 ~ "Very High Price",
    TRUE ~ "Others"
  ))

# Creating the column title_length
books$title_length <- nchar(books$Name)

# Creating categories for the length of the titles
books$title_category <- cut(nchar(books$Name), breaks = c(0, 30, 60, Inf), 
                            labels = c("Short Title", "Moderate Title", "Long Title"))

books$Year <- factor(books$Year, levels = 2009:2019)

write.csv(books, file = 'Amazon_CleanedBestsellers.csv') # cleaned dataset

View(books)

## EXPLORATORY DATA ANALYSIS
# general statistics of the data based on numerical variables using boxplot
boxplot(books$User.Rating, col = "skyblue")
boxplot(books$Reviews, col = "red")
boxplot(books$Price, col = "yellow")
boxplot(books$title_length, col = "green")

#Rating Category
#Pie chart for Distribution of Books by Rating Category 
rating_counts <- books %>%
  group_by(rating_category) %>%
  summarise(count = n())

ggplot(rating_counts, aes(x = "", y = count, fill = rating_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Books by Rating Category",
       fill = "Rating Category") +
  scale_fill_manual(values = c("Outstanding" = "red", "Excellent" = "aquamarine", 
                               "Very Good" = "green", "Good" = "grey")) +
  geom_text(aes(label = paste0(count, " (", scales::percent(count / sum(count), accuracy = 0.01), ")")),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))
ggsave("Distribution of Books by Rating Category.png")

#Pie Chart for Distribution of Fiction Books by Rating Category
fiction_rating_counts <- books %>%
  filter(genre == "Fiction") %>%
  group_by(rating_category) %>%
  summarise(count = n())

ggplot(fiction_rating_counts, aes(x = "", y = count, fill = rating_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Fiction Books by Rating Category",
       fill = "Rating Category") +
  scale_fill_manual(values = c("Outstanding" = "red", "Excellent" = "aquamarine", 
                               "Very Good" = "green", "Good" = "grey")) +
  geom_text(aes(label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))

#Pie Chart for Distribution of Nonfiction Books by Rating Category
nonfiction_rating_counts <- books %>%
  filter(genre == "Non Fiction") %>%
  group_by(rating_category) %>%
  summarise(count = n())

ggplot(nonfiction_rating_counts, aes(x = "", y = count, fill = rating_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Nonfiction Books by Rating Category",
       fill = "Rating Category") +
  scale_fill_manual(values = c("Outstanding" = "red", "Excellent" = "aquamarine", 
                               "Very Good" = "green", "Good" = "grey")) +
  geom_text(aes(label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))

#Review Category
#Pie chart of the Distribution of Books by Review Category
review_counts <- books %>%
  group_by(review_category) %>%
  summarise(count = n())

ggplot(review_counts, aes(x = "", y = count, fill = review_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Books by Review Category",
       fill = "Review Category") +
  scale_fill_manual(values = c("Very High Reviews" = "blueviolet", "High Reviews" = "magenta", 
                               "Moderate Reviews" = "brown", "Low Reviews" = "azure3", 
                               "Very Low Reviews" = "aquamarine2")) +
  geom_text(aes(label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))
ggsave("Distribution of Books by Review Category.png")

#Pie Chart for Distribution of Fiction Books by Review Category
fiction_review_counts <- books %>%
  filter(genre == "Fiction") %>%
  group_by(review_category) %>%
  summarise(count = n())

ggplot(fiction_review_counts, aes(x = "", y = count, fill = review_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Fiction by Review Category",
       fill = "Review Category") +
  scale_fill_manual(values = c("Very High Reviews" = "blueviolet", "High Reviews" = "magenta", 
                               "Moderate Reviews" = "brown", "Low Reviews" = "azure3", 
                               "Very Low Reviews" = "aquamarine2")) +
  geom_text(aes(label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))

#Pie Chart for Distribution of Nonfiction Books by Review Category
nonfiction_review_counts <- books %>%
  filter(genre == "Non Fiction") %>%
  group_by(review_category) %>%
  summarise(count = n())

ggplot(nonfiction_review_counts, aes(x = "", y = count, fill = review_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Nonfiction by Review Category",
       fill = "Review Category") +
  scale_fill_manual(values = c("Very High Reviews" = "blueviolet", 
                               "High Reviews" = "magenta", "Moderate Reviews" = "brown",
                               "Low Reviews" = "azure3", "Very Low Reviews" = "aquamarine2")) +
  geom_text(aes(label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))

#Price Category
#Pie chart of the Distribution of Books by Price Category
price_counts <- books %>%
  group_by(price_category) %>%
  summarise(count = n()) %>%
  filter(!is.na(price_category))  # Remove null values

ggplot(price_counts, aes(x = "", y = count, fill = price_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Books by Price Category",
       fill = "Price Category") +
  scale_fill_manual(values = c("Very High Price" = "firebrick3", "High Price" = "darkorange", 
                               "Moderate Price" = "steelblue", "Low Price" = "darkseagreen", 
                               "Free" = "darkturquoise")) +
  geom_text(aes(x=1.7, label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))
ggsave("Distribution of Books by Price Category.png")

#Pie Chart for Distribution of Fiction Books by Price Category
fiction_price_counts <- books %>%
  filter(genre == "Fiction") %>%
  group_by(price_category) %>%
  summarise(count = n()) %>%
  filter(!is.na(price_category))  # Remove null values

ggplot(fiction_price_counts, aes(x = "", y = count, fill = price_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Fiction Books by Price Category",
       fill = "Price Category") +
  scale_fill_manual(values = c("Very High Price" = "firebrick3", "High Price" = "darkorange", 
                               "Moderate Price" = "steelblue", "Low Price" = "darkseagreen", 
                               "Free" = "darkturquoise")) +
  geom_text(aes(x=1.7, label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))

#Pie Chart for Distribution of Nonfiction Books by Price Category
nonfiction_price_counts <- books %>%
  filter(genre == "Non Fiction") %>%
  group_by(price_category) %>%
  summarise(count = n()) %>%
  filter(!is.na(price_category))  # Remove null values

ggplot(nonfiction_price_counts, aes(x = "", y = count, fill = price_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Nonfiction by Price Category",
       fill = "Price Category") +
  scale_fill_manual(values = c("Very High Price" = "firebrick3", "High Price" = "darkorange", 
                               "Moderate Price" = "steelblue", "Low Price" = "darkseagreen", 
                               "Free" = "darkturquoise")) +
  geom_text(aes(x=1.7, label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))

#Title Category
#Pie chart of Distribution of Books by Title Category
title_counts <- books %>%
  group_by(title_category) %>%
  summarise(count = n())

ggplot(title_counts, aes(x = "", y = count, fill = title_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Books by Title Category",
       fill = "Title Category") +
  scale_fill_manual(values = c("Long Title" = "red", "Moderate Title" = "azure", 
                               "Short Title" = "coral")) +
  geom_text(aes(label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))
ggsave("Distribution of Books by Title Category.png")

#Pie Chart for Distribution of Fiction Books by Title Category
fiction_title_counts <- books %>%
  filter(genre == "Fiction") %>%
  group_by(title_category) %>%
  summarise(count = n())

ggplot(fiction_title_counts, aes(x = "", y = count, fill = title_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Fiction Books by Title Category",
       fill = "Title Category") +
  scale_fill_manual(values = c("Long Title" = "red", "Moderate Title" = "azure", 
                               "Short Title" = "coral")) +
  geom_text(aes(label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))

#Pie Chart for Distribution of Nonfiction Books by Title Category
nonfiction_title_counts <- books %>%
  filter(genre == "Non Fiction") %>%
  group_by(title_category) %>%
  summarise(count = n())

ggplot(nonfiction_title_counts, aes(x = "", y = count, fill = title_category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Nonfiction by Title Category",
       fill = "Title Category") +
  scale_fill_manual(values = c("Long Title" = "red", "Moderate Title" = "azure", 
                               "Short Title" = "coral")) +
  geom_text(aes(label = scales::percent(count / sum(count), accuracy = 0.01)),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 20)),
        legend.position = "right",
        legend.margin = margin(0, 0, 0, -20))



















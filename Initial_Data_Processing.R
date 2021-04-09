
# ---------------------------------------------------------------------------------------------------------
# AO3 Data Dump Processing
# ---------------------------------------------------------------------------------------------------------
# Created by Mouse Mode, 3/27/2021
# Send questions to myofficialdumpster@gmail.com  Always happy to help R beginners!
# Data dump announcement: https://archiveofourown.org/admin_posts/18804
# ---------------------------------------------------------------------------------------------------------

# Packages required in this code. If the package hasn't already been installed, require() will download them
pckg.list <- c("scales", "ggplot2", "tidyverse", "dplyr", "stringr", "readr", "data.table")
sapply(pckg.list, require, character.only = TRUE)
rm(pckg.list)

# I have created a folder "AO3_Project" on my desktop, containing a subfolder called "raw_data"
# This is where I've saved the files downloaded from AO3, and uncompressed.
# There should be 2 files, works-20210226.csv and tags-20210226.csv

# Set the working directory to your project folder. This would be where the datasets are stored as well.
# Be sure the file path uses "/" and not "\". This will cause errors in R. 
setwd("~/Desktop/AO3_Project")

# Read in the datasets saved in the raw_data folder
# [works.raw] contains 7,269,693 rows, one for each work on AO3.
works.raw <- read_csv("./raw_data/works-20210226.csv")
# [tags.raw] contains 14,467,138 rows, one for each tag
tags.raw <- read_csv("./raw_data/tags-20210226.csv")

# There is also an important codebook I have saved on GitHub. https://github.com/mousemode/AO3_Data_Dump
# Save the "language_codetable.csv" into the raw data folder as well
# This translate the 2/3 digit language code Ao3 uses into a more reader-friendly language name
lang.codebook <- read_csv("./raw_data/language_codetable.csv")

# View the first 10 rows of each dataset
View(works.raw[1:10,])
View(tags.raw[1:10,])
View(lang.codebook[1:10,])

# ---------------------------------------------------------------------------------------------------------

# CLEAN UP WORKS DATA

# The raw.works table doesn't have any identifying info about the work itself, like title or author
# Create work_id as a way of referring to each work
# Drop the empty column "X"
works.dat <- works.raw %>% mutate(work_id = row_number()) %>% relocate(work_id) %>% select(-X7)

# Replace spaces with underscores in the column names
colnames(works.dat)[2] <- "creation_date"

# The works dataset is huge. Anything that can be done to reduce this dataset should be done upfront here.
# I have decided to look at works created since 2015
works.dat <- works.dat %>%
  mutate(creation_year = as.numeric(substr(creation_date, 1,4))) %>%
  filter(creation_year >= 2015)

# The "tags" column contains a list of tag_id associated with the work, seperated by "+" signs
# Count the number of tags associated with each work.
works.dat <- works.dat %>% mutate(NTags = 1+str_count(tags, "\\+"))

# Transform the tag list from wide to long, and put it in it's own table "work.tags"
# This table will be the crosswalk between "work.dat" and "tags.dat", and will have sum(NTags) rows
tags.long <- strsplit(as.character(works.dat$tags), '+', fixed = T)
tags.unlist <- as.numeric(unlist(tags.long))
tags.attrib <- unlist(mapply(rep, 1:nrow(works.dat), lengths(tags.long)))
work.tags <- tibble(work_id = tags.attrib, tag_id = tags.unlist)
View(work.tags[1:10,])

# Save as a tab-delim text file, so I don't have to do these steps again
write_delim(work.tags, "./raw_data/work_tags_cross.txt", delim = "\t")

# ---------------------------------------------------------------------------------------------------------

# CLEAN UP TAGS DATA

# Rename "id" to "tag_id" to match the table "work.tags"
# Transform "name" from factor to character
tags.dat <- tags.raw %>%
  rename(tag_id = id) %>%
  mutate(name = as.character(name))

# Other Notes:
# I'd suggest reducing the size of "tags.dat" by filtering for tags used >= 5 times
# The rating for the fic is coded as a tag type
table(tags.dat$type)

# ---------------------------------------------------------------------------------------------------------

# Now there are 3 datasets ready for analysis! Hope that helps someone out there :)
# I might post code showing what analyses I am going to be doing, but they'll likely be a little messier













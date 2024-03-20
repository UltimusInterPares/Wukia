# Data Selection
# Thomas Broadwater: Thomas.Broadwater@uga.edu

### PACKAGES --------------------------------------------------------------
library(dplyr)    # . . . . . . . . . . . . . . . .  For %>%, filter(), etc
library(stringr)  # . . . . . . . . . . . . . . . . . . .  For str_detect()
library(rvest)    # . . . . . . . . . . . . . . . . . .  Web Scraping Tools
library(ggplot2)  # . . . . . . . . . . . . . . . . . . Data visualization
library(progress) # . . . . . . . . . . . . . . . . . . . For progress bars
library(stringi)  # . . . . . . . . . For getting UTF8 codes out of char.'s
library(tidytext) # . . . . For unnest_tokens() and other text manipulation
library(readr)    # . . . . . . . . . . . . . . . . . . . .  For read_csv()

### ### ### ### ### ### ### ### First Round ### ### ### ### ### ### ### ###

# The Scrape() function returns a data frame with 147791 observations
# of 9 variables.

# Oropus data manually merged with the rest of the corpus

### Remove NA Rows --------------------------------------------------------
# Count NA Rows
PackHum[is.na(PackHum$ig_book), ] %>% count()

# Remove NA Rows
PackHum.2 <-
  PackHum[!is.na(PackHum$ig_book) & !is.na(PackHum$text), ]

#                  ~~~~~ All Scraped Books ~~~~~                 #
# | Book          | Region                                       |
# |---------------|----------------------------------------------|
# | ID            | Delos                                        |
# | IG I²         | Attica                                       |
# | IG I³         | Attica                                       |
# | IG II²        | Attica                                       |
# | IG IV         | Peloponnesos                                 |
# | IG IV²,1      | Peloponnesos                                 |
# | IG IX,1       | Central Greece                               |
# | IG IX,1²      | Central Greece                               |
# | IG V,1        | Peloponnesos                                 |
# | IG V,2        | Peloponnesos                                 |
# | IG VII        | Central Greece                               |
# | IG X,2 1      | Northern Greece; Thrace and the Lower Danube |
# | IG X,2 2      | Northern Greece; Thrace and the Lower Danube |
# | IG XI,2       | Aegean Islands, inc. Crete                   |
# | IG XI,4       | Aegean Islands, inc. Crete                   |
# | IG XII Suppl. | Aegean Islands, inc. Crete                   |
# | IG XII,1      | Aegean Islands, inc. Crete                   |
# | IG XII,2      | Aegean Islands, inc. Crete                   |
# | IG XII,3      | Aegean Islands, inc. Crete                   |
# | IG XII,5      | Aegean Islands, inc. Crete                   |
# | IG XII,7      | Aegean Islands, inc. Crete                   |
# | IG XII,8      | Aegean Islands, inc. Crete                   |
# | IG XII,9      | Aegean Islands, inc. Crete                   |
# | IG XIV        | Sicily, Italy, and the West                  |
# | SEG           | Diversae                                     |

### Remove Non-IG Books ---------------------------------------------------
# Count Inscriptions de Délos (ID) Rows
PackHum.2[PackHum.2$ig_book == "ID", ] %>% count()

# Remove ID Rows
PackHum.3 <- PackHum.2[PackHum.2$ig_book != "ID", ]

# Drop Unused Levels
PackHum.3$ig_book <- droplevels(PackHum.3$ig_book)

# Count Supplementum Epigraphicum Graecum (SEG) Rows
PackHum.3[PackHum.3$ig_book == "SEG", ] %>% count()

# Remove SEG Rows
PackHum.4 <- PackHum.3[PackHum.3$ig_book != "SEG", ]

# Drop Unused Levels
PackHum.4$ig_book <- droplevels(PackHum.4$ig_book)

#                      ~~~~~ IG Books ~~~~~                      #
# | Book          | Region                                       |
# |---------------|----------------------------------------------|
# | IG I²         | Attica                                       |
# | IG I³         | Attica                                       |
# | IG II²        | Attica                                       |
# | IG IV         | Peloponnesos                                 |
# | IG IV²,1      | Peloponnesos                                 |
# | IG IX,1       | Central Greece                               |
# | IG IX,1²      | Central Greece                               |
# | IG V,1        | Peloponnesos                                 |
# | IG V,2        | Peloponnesos                                 |
# | IG VII        | Central Greece                               |
# | IG X,2 1      | Northern Greece; Thrace and the Lower Danube |
# | IG X,2 2      | Northern Greece; Thrace and the Lower Danube |
# | IG XI,2       | Aegean Islands, inc. Crete                   |
# | IG XI,4       | Aegean Islands, inc. Crete                   |
# | IG XII Suppl. | Aegean Islands, inc. Crete                   |
# | IG XII,1      | Aegean Islands, inc. Crete                   |
# | IG XII,2      | Aegean Islands, inc. Crete                   |
# | IG XII,3      | Aegean Islands, inc. Crete                   |
# | IG XII,5      | Aegean Islands, inc. Crete                   |
# | IG XII,7      | Aegean Islands, inc. Crete                   |
# | IG XII,8      | Aegean Islands, inc. Crete                   |
# | IG XII,9      | Aegean Islands, inc. Crete                   |
# | IG XIV        | Sicily, Italy, and the West                  |

### Remove Irrelevant Inscriptiones Graecae (IG) Books by Region ----------
# Original scrape includes 24 IG books. Many do not pertain to the region
# in question.

# Count anything that doesn't belong to Attica or Central Greece
count(PackHum.4) - count(PackHum.4[PackHum.4$ig_book %in% c("IG I²",
                                                            "IG I³",
                                                            "IG II²",
                                                            "IG IX,1",
                                                            "IG IX,1²",
                                                            "IG VII"),])

# Remove anything that doesn't belong to Attica or Central Greece
PackHum.5 <- PackHum.4[PackHum.4$ig_book %in% c("IG I²",
                                                "IG I³",
                                                "IG II²",
                                                "IG IX,1",
                                                "IG IX,1²",
                                                "IG VII"), ]

# Drop Unused Levels
PackHum.5$ig_book <- droplevels(PackHum.5$ig_book)

### Remove Irrelevant Inscriptiones Graecae (IG) Books by Subregion -------

#                                 ~~~~~ IG Books ~~~~~                                  #
# | Book     | Region         | Subregion                                               |
# |----------|----------------|---------------------------------------------------------|
# | IG I²    | Attica         | Attica                                                  |
# | IG I³    | Attica         | Attica, Rhamnous, Eleusis                               |
# | IG II²   | Attica         | Attica                                                  |
# | IG IX,1  | Central Greece | Phokis, Lokris, Aitolia, Akarnania, and Ionian Islands  |
# | IG IX,1² | Central Greece | Thessaly                                                |
# | IG VII   | Central Greece | Megaris, Oropia, Boiotia                                |

# IG IX,1 and IG IX,1², while pertaining to Central Greece, all handle
# subregions far further north than the survey requires.

# Count anything that doesn't belong to Attica, Megaris, Oropia, or Boiotia
count(PackHum.5) - count(PackHum.5[PackHum.5$ig_book %in% c("IG I²",
                                                            "IG I³",
                                                            "IG II²",
                                                            "IG VII"), ])

# Remove anything not belonging to AAttica, Megaris, Oropia, or Boiotia
PackHum.6 <- PackHum.5[PackHum.5$ig_book %in% c("IG I²",
                                                "IG I³",
                                                "IG II²",
                                                "IG VII"), ]

# Drop Unused Levels
PackHum.6$ig_book <- droplevels(PackHum.6$ig_book)

### Final Inscription Count -----------------------------------------------
PackHum.6 %>% nrow()

### Write to a verifiable CSV ---------------------------------------------
write_csv(PackHum.6, "SelectedPackHum.csv")

### ### ### ### ### ### ### ## Second Round ## ### ### ### ### ### ### ###
# Validation is taking forever, there are far too many unusable
# inscriptions clogging up the process.
# I'm going to load it all back in and remove some of the more egregious
# offenders.

PackHum.7 <-
  read_csv(
    "Thesis/DataCollection/VerifiedPackHumWIP.csv",
    col_types = cols(
      ...1 = col_skip(),
      ig_book = col_factor(levels = c()),
      location = col_factor(levels = c()),
      cross_cen = col_logical(),
      after_bce = col_logical(),
      before_bce = col_logical(),
      ...13 = col_skip(),
      ...14 = col_skip(),
      ...15 = col_skip(),
      ...16 = col_skip(),
      ...17 = col_skip(),
      ...18 = col_skip()
    )
  )

### "s. X p." dates -------------------------------------------------------
## Finding the dates
# 2,463 rows
PackHum.7 %>% filter(str_detect(header, "s\\. .{1,7} p\\.") == T) %>%
  View()

## Check which ones have "a." in them
# 85 rows, all "s. I a./s. X p."
PackHum.7 %>% filter(str_detect(header, "s\\. .{1,7} p\\.") == T) %>%
  filter(str_detect(header, "a\\.") == T) %>% View()

# Remove "s. X p." dates
PackHum.8 <-
  PackHum.7 %>% filter(str_detect(header, "s\\. .{1,7} p\\.") == F)

### "AD dates" ------------------------------------------------------------
## Finding the Dates
# 147 rows
PackHum.8[PackHum.8$ig_book == "IG VII", ] %>%
  filter(str_detect(header, " BC") == F) %>%
  filter(str_detect(header, " AD") == T) %>%
  View()

# Make a list to drop
# Doing this in two steps rather than with a single index since the
# data requires three steps to select.
PackHum.8.ToDrop <- PackHum.8[PackHum.8$ig_book == "IG VII", ] %>%
  filter(str_detect(header, " BC") == F) %>%
  filter(str_detect(header, " AD") == T)

# Take inscriptions with PH #s NOT in the drop list
PackHum.9 <-
  PackHum.8[(PackHum.8$phi_no %in% PackHum.8.ToDrop$phi_no == F),]

### Roman Inscriptions
# Sulla only took athens in 86, making any Roman Era inscriptions unusable

# aet. Rom: 384 rows
PackHum.9 %>% filter(str_detect(header, "aet\\. Rom\\.") == T) %>% View()
PackHum.10 <- PackHum.9 %>%
  filter(str_detect(header, "aet\\. Rom\\.") == F)

# aet. Imp: 596 rows
PackHum.10 %>% filter(str_detect(header, "aet\\. imp\\.") == T) %>% View()
PackHum.11 <- PackHum.10 %>%
  filter(str_detect(header, "aet\\. imp\\.") == F)

# aet. Augusti: 58 rows
PackHum.11 %>% filter(str_detect(header, "aet\\. Augusti") == T) %>% View()
PackHum.12 <- PackHum.11 %>%
  filter(str_detect(header, "aet\\. Augusti") == F)

# aet. Hadriani: 74 rows
PackHum.12 %>% filter(str_detect(header, "aet\\. Hadriani") == T) %>% View()
PackHum.13 <- PackHum.12 %>%
  filter(str_detect(header, "aet\\. Hadriani") == F)

# Rom. Imp. period (IG VII): 213 rows
PackHum.13 %>% filter(str_detect(header, "Rom\\. Imp\\. period") == T) %>% View()
PackHum.14 <- PackHum.13 %>%
  filter(str_detect(header, "Rom\\. Imp\\. period") == F)

# Undated: 3112 rows
PackHum.14 %>% filter(str_detect(header, "undated") == T) %>% View()
PackHum.15 <- PackHum.14 %>%
  filter(str_detect(header, "undated") == F)

# Roman Period: 55 rows
PackHum.15 %>% filter(str_detect(header, "Roman period") == T) %>% View()
PackHum.16 <- PackHum.15 %>%
  filter(str_detect(header, "Roman period") == F)

# Write
write_csv(PackHum.16, "VerifiedPackHumWIP2.csv")

### ### ### ### ### ### ### ### Third Round ### ### ### ### ### ### ### ###
# Read it back in
PackHum.17 <- read_csv(
  "DataCollection/VerifiedPackHumWIP3.csv",
  col_types = cols(location = col_factor(levels = c()),
                   text = col_factor(levels = c()))
)

# There are just .... SO many first cen. inscriptions
# Get rid of them

# s. I a.: 775 rows
PackHum.17 %>% filter(str_detect(header, "s\\. I a\\.") == T) %>% View()
PackHum.18 <- PackHum.17 %>%
  filter(str_detect(header, "s\\. I a\\.") == F)

# 1st c. BC: 11 rows
PackHum.18 %>% filter(str_detect(header, "— 1st c\\. BC") == T) %>% View()
PackHum.19 <- PackHum.18 %>%
  filter(str_detect(header, "— 1st c\\. BC") == F)

# Write
write_csv(PackHum.19, "VerifiedPackHumWIP4.csv")

### ### ### ### ### ### ### ## Fourth Round ## ### ### ### ### ### ### ###
# Lets get rid of the roman emperors
# Indicated by the word "reign"

PackHum.20 <-
  VerifiedPackHumWIP5 <- read_csv("VerifiedPackHumWIP5.csv",
                                  col_types = cols(
                                    ig_book = col_factor(levels = c()),
                                    location = col_factor(levels = c())
                                  ))

# "reign" dates: 41
PackHum.20 %>% filter(str_detect(header, "reign") == T) %>% View()
PackHum.21 <-
  PackHum.20 %>% filter(str_detect(header, "reign") == F)

# Christianity takes off in 1st cen CE
PackHum.21 %>% filter(str_detect(header, "Christian") == T) %>% View()
PackHum.22 <-
  PackHum.21 %>% filter(str_detect(header, "Christian") == F)

# Write it out again
write_csv(PackHum.22, "VerifiedPackHumWIP6.csv")

### ### ### ### ### ### ### ### Fifth Round ### ### ### ### ### ### ### ###
# Dates are verified! We can start making big changes
# Read the data in
PackHum.23 <- read_csv("VerifiedPackHumWIP7.csv",
                       col_types = cols(
                         ig_book = col_factor(levels = c()),
                         location = col_factor(levels = c())
                       ))

# Count undated inscriptions
PackHum.23[(is.na(PackHum.23$date_after)) &
             (is.na(PackHum.23$date_before)),] %>% nrow()

# Count "post fin." dates: 240
PackHum.23 %>% filter(str_detect(header, "post fin\\.") == T) %>%
  View("Post Fin")

# Correct dates
for (i in c(1:nrow(PackHum.23))) {
  if (str_detect(PackHum.23[i, ]$header, "post fin\\. s\\. IV a\\.") == T) {
    PackHum.23[i, ]$date_after <- 300
    PackHum.23[i, ]$date_before <- NA
  } else if (str_detect(PackHum.23[i, ]$header, "post fin\\. s\\. III a\\.") == T) {
    PackHum.23[i, ]$date_after <- 200
    PackHum.23[i, ]$date_before <- NA
  }  else if (str_detect(PackHum.23[i, ]$header, "post fin\\. s\\. II a\\.") == T) {
    PackHum.23[i, ]$date_after <- 100
    PackHum.23[i, ]$date_before <- NA
  }
}

# Count "med." dates: 1,101
PackHum.23 %>% filter(str_detect(header, "(?<!(ante|post) )med\\.") == T) %>%
  View("Bare Meds")

# Count "med." dates where date_after == date_before
PackHum.23 %>% filter(str_detect(header, "(?<!(ante|post) )med\\.") == T) %>%
  filter(date_after == date_before) %>%
  View("Matching Meds")

# Correct dates
for (i in c(1:nrow(PackHum.23))) {
  if (str_detect(PackHum.23[i, ]$header, "(?<!(ante|post) )med\\.") == T) {
    # paste(
    #   PackHum.23[i, ]$date_after, PackHum.23[i, ]$date_before, sep = "-"
    #   ) %>%
    #   paste("->", sep = " ") %>%
    #   paste(PackHum.23[i, ]$date_after+25, sep = " ") %>%
    #   paste(PackHum.23[i, ]$date_before-25, sep = "-") %>%
    #   print()
    PackHum.23[i,]$date_after <- PackHum.23[i,]$date_after + 25
    PackHum.23[i,]$date_before <- PackHum.23[i,]$date_before - 25
  }
}

# Write it out and do the century logic in numbers
# I just... don't want to write another loop
write_csv(PackHum.23, "VerifiedPackHumWIP8.csv")

# Read it back in
PackHum.24 <- read_csv("VerifiedPackHumWIP9.csv",
                       col_types = cols(
                         ig_book = col_factor(levels = c()),
                         location = col_factor(levels = c())
                       ))

# Centuries: 4, 3, 2
# Selecting
PackHum.24 %>% filter((after_cen %in% c(400, 300, 200)) |
                        (before_cen %in% c(400, 300, 200))) %>% View()

# Looking at the unwanted centuries
# NAs
PackHum.24 %>% filter(is.na(after_cen)) %>% nrow()
PackHum.24 %>% filter(is.na(before_cen)) %>% nrow()
PackHum.24 %>% filter(is.na(after_cen) &
                        is.na(before_cen)) %>% nrow()

# Wide ranges
PackHum.24 %>% filter((after_cen - before_cen) > 100) %>% View()
PackHum.24 %>% filter((date_after - date_before) > 300) %>% View()

# 5th/6th cen
PackHum.24 %>% filter((after_cen %in% c(600, 500) &
                         !(before_cen %in% c(400, 300, 200)))) %>%
  View("5th/6th")

# Actually select now
PackHum.25 <-
  PackHum.24 %>% filter((after_cen %in% c(400, 300, 200)) |
                          (before_cen %in% c(400, 300, 200)))


# See the differences
# NA dates
PackHum.24 %>% filter(!(PackHum.24$text %in% PackHum.25$text)) %>% View()
# Bad centuries: 1st c
PackHum.24 %>% filter(!(PackHum.24$text %in% PackHum.25$text)) %>%
  filter(!is.na(after_cen) & !is.na(before_cen)) %>%
  filter(after_cen == 100) %>% View()
# Bad centuries: 5th c
PackHum.24 %>% filter(!(PackHum.24$text %in% PackHum.25$text)) %>%
  filter(!is.na(after_cen) & !is.na(before_cen)) %>%
  filter(after_cen == 500) %>% View()
# Bad Centuries: 6th c
PackHum.24 %>% filter(!(PackHum.24$text %in% PackHum.25$text)) %>%
  filter(!is.na(after_cen) & !is.na(before_cen)) %>%
  filter(after_cen == 600) %>% View()
# Remainders
PackHum.24 %>% filter(!(PackHum.24$text %in% PackHum.25$text)) %>%
  filter(!is.na(after_cen) & !is.na(before_cen)) %>%
  filter(after_cen != 100) %>%
  filter(after_cen != 500) %>%
  filter(after_cen != 600) %>% View()

# Write out PackHum.25
write_csv(PackHum.25, "PackHumVerifiedDates.csv")

# Read in the verified dates with centuries of analysis
PackHum.26 <- read_csv(
  "PackHumVerifiedDates.2.csv",
  col_types = cols(
    ig_book = col_factor(levels = c()),
    location = col_factor(levels = c())
  )
)

# Remove dates in the common era! Some slipped by!
PackHum.26 %>% filter(tpq_bce == F & taq_bce == F) %>% View()
PackHum.26 %>% filter(tpq_bce == F) %>% View()
PackHum.26 %>% filter(tpq_bce == F & taq_bce == F) %>% nrow()

PackHum.27 <- PackHum.26 %>%
  filter(tpq_bce == T)

PackHum.28 <-
  PackHum.27 %>% filter(analysis_cen %in% c(400, 300, 200))


### ### ### ### ### ### ### ### Locations ### ### ### ### ### ### ### ###
# And it begins again
# Write out a csv for manual validation
write_csv(PackHum.27, "LocationsWIP.csv")

# MANUALLY VALIDATE
# read in
PackHum.28 <- read_csv(
  "LocationsWIP2.csv",
  col_types = cols(
    ig_book = col_factor(levels = c()),
    ig_no = col_factor(levels = c()),
    location = col_factor(levels = c())
  )
)

# Redo final chronological selection (got it a bit out of order above)
PackHum.28 <- PackHum.28 %>%
  filter(tpq_bce == T)

PackHum.28 <-
  PackHum.28 %>% filter(analysis_cen %in% c(400, 300, 200))

### NA Locations
# Need to see what we're working with
# Locations:
levels(PackHum.28$location)

## Fix missed locations
# Create new tibble
PackHum.29 <- PackHum.28

# Convert to characters (can't add new levels to factors)
PackHum.29$location <- as.character(PackHum.29$location)

# Fix Agora
for (i in c(1:nrow(PackHum.29))) {
  if (grepl("Agora", PackHum.29$header[i]) == T) {
    PackHum.29$location[i] <- "Athens"
  }
}

# Fix Athmonon (Deme)
for (i in c(1:nrow(PackHum.29))) {
  if (grepl("Athmonon", PackHum.29$header[i]) == T) {
    PackHum.29$location[i] <- "Athmonon"
  }
}

# Fix Patissia (Neighborhood)
for (i in c(1:nrow(PackHum.29))) {
  if (grepl("Patissia", PackHum.29$header[i]) == T) {
    PackHum.29$location[i] <- "Patissia"
  }
}

# Fix Peiraieus (Deme)
for (i in c(1:nrow(PackHum.29))) {
  if (grepl("Peiraieus", PackHum.29$header[i]) == T) {
    PackHum.29$location[i] <- "Peiraieus"
  }
}

# Fix Rhamnous: Rhamnountos | Rhamnonte < "Rhamno"
for (i in c(1:nrow(PackHum.29))) {
  if (grepl("Rhamno", PackHum.29$header[i]) == T) {
    PackHum.29$location[i] <- "Rhamnous"
  }
}

# Return column to factor
PackHum.29$location <- as.factor(PackHum.29$location)

## Count NAs again
PackHum.29 %>%
  filter(is.na(location)) %>% View()

# Count bad locations
PackHum.29 %>%
  filter(
    str_detect(
      location,
      "(Aigosthena|Athens|Eleusis|Megara|Oropos|Pagai|Tanagra)"
    ) == F
  ) %>%
  nrow()

# Count good locations
PackHum.29 %>%
  filter(
    str_detect(
      location,
      "(Aigosthena|Athens|Eleusis|Megara|Oropos|Pagai|Tanagra)"
    ) == T
  ) %>%
  View()

# Save good locations
PackHum.30 <- PackHum.29 %>%
  filter(
    str_detect(
      location,
      "(Aigosthena|Athens|Eleusis|Megara|Oropos|Pagai|Tanagra)"
    ) == T
  )

# Relevel location factor
PackHum.30$location <- relevel(PackHum.30$location, ref = "Athens")

# Count locations
PackHum.30 %>% count(location)


### Final Cleaning --------------------------------------------------------
## Combining Accents:  ͂ (Combining Greek Perispomeni)
for (i in c(1:nrow(PackHum.30))) {
  PackHum.30$text[i] <- gsub("͂", "", PackHum.30$text[i])
}

## Combining Accents: ̓ (Combining Comma Above)
for (i in c(1:nrow(PackHum.30))) {
  PackHum.30$text[i] <- gsub("̓", "", PackHum.30$text[i])
}

## Removing Latin text
# Full stops
for (i in c(1:nrow(PackHum.30))) {
  PackHum.30$text[i] <-
    gsub("\\.{1}", "", PackHum.30$text[i])
}

# Any h not followed by a Greek letter
for (i in c(1:nrow(PackHum.30))) {
  PackHum.30$text[i] <-
    gsub("h(?![\\W])",
         "",
         PackHum.30$text[i],
         ignore.case = T,
         perl = T)
}

# All letters except for h
for (i in c(1:nrow(PackHum.30))) {
  PackHum.30$text[i] <-
    gsub("[abcdefgijklmnopqrstuvwxyz]+",
         "",
         PackHum.30$text[i],
         ignore.case = T)
}

# All Arabic Numerals
for (i in c(1:nrow(PackHum.30))) {
  PackHum.30$text[i] <-
    gsub("\\d", "", PackHum.30$text[i])
}

### Unnesting -------------------------------------------------------------
PackHum.30 %>% unnest_tokens("Word", text) %>% View()

### Final Tables ----------------------------------------------------------
PackHum <- PackHum.30
PackHumUnnest <- PackHum.30 %>% unnest_tokens("Token", text)
# Make sure its all lower case
PackHum <- rename_with(PackHum, tolower)
PackHumUnnest <- rename_with(PackHumUnnest, tolower)
# Droop unused levels
PackHum$location <- droplevels(PackHum$location)
PackHumUnnest$location <- droplevels(PackHumUnnest$location)

# Write final tables
write_csv(PackHum, "Data.csv")
write_csv(PackHumUnnest, "UnnestedData.csv")

## Cleanup
# Drop List
rm("PackHum.8.ToDrop")

# Long name
rm("VerifiedPackHumWIP5")

# Sequential Names
list <- c()
for (i in c(1:30)) {
  list[i] <- paste("PackHum.", i, sep = "")
}
rm(list = list)
rm(list)

# Loop values
rm(f, i)

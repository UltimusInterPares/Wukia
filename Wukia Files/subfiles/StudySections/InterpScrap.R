# Make a contingency table of oi data
stats::xtabs(~ location + analysis_cen, data = Data.oi)

# Box Plot per cen
Data.oi %>% group_by(location) %>% count(analysis_cen) %>%
  ggplot(aes(x = analysis_cen, y = n, group = analysis_cen)) +
  geom_boxplot()

# Box Plot per city
Data.oi %>% group_by(location) %>% count(analysis_cen) %>%
  ggplot(aes(x = location, y = n)) +
  geom_boxplot()

# Make a contingency table of oi data from Athens and Tanagra
stats::xtabs(~ location + analysis_cen,
             data = Data.oi[Data.oi$location == "Athens" |
                              Data.oi$location == "Tanagra",] %>%
               filter(analysis_cen == 200 | analysis_cen == 300) %>%
               droplevels()) %>% fisher.test()

# Check inscriptions
stats::xtabs(phi_no ~ location + analysis_cen,
             data = Data.oi[Data.oi$location == "Athens" |
                              Data.oi$location == "Tanagra",] %>%
               filter(analysis_cen == 200 | analysis_cen == 300) %>%
               droplevels()) %>% fisher.test()

# data types: oi btwn 2 and 3 (ath/tan)
stats::xtabs(~ location + analysis_cen,
             data = Data.oi[Data.oi$location == "Athens" |
                              Data.oi$location == "Tanagra",] %>%
               filter(analysis_cen == 200 | analysis_cen == 300) %>%
               filter(data_type == "οι") %>%
               droplevels()) %>% fisher.test()
# significantly uneven distribution

# data types: oi btwn 3 and 4 (ath/tan)
stats::xtabs(~ location + analysis_cen,
             data = Data.oi[Data.oi$location == "Athens" |
                              Data.oi$location == "Tanagra",] %>%
               filter(analysis_cen == 300 | analysis_cen == 400) %>%
               filter(data_type == "οι") %>%
               droplevels()) %>% fisher.test()
# significantly uneven distribution

# Testing Tanagran οι/υ discrepancy ---------------------------------------
## Lets make a contingency table for each pairing

# All
stats::xtabs(~ location + analysis_cen,
              data = Data.oi[Data.oi$location == "Athens" |
                               Data.oi$location == "Tanagra",] %>%
                #filter(analysis_cen == 200 | analysis_cen == 300) %>%
                filter(data_type == "οι") %>%
                droplevels())

# 4th and 3rd cens
stats::xtabs(~ location + analysis_cen,
             data = Data.oi[Data.oi$location == "Athens" |
                              Data.oi$location == "Tanagra",] %>%
               filter(analysis_cen == 400 | analysis_cen == 300) %>%
               filter(data_type == "οι") %>%
               droplevels()) %>% fisher.test()

# 3rd and 2nd cens
stats::xtabs(~ location + analysis_cen,
             data = Data.oi[Data.oi$location == "Athens" |
                              Data.oi$location == "Tanagra",] %>%
               filter(analysis_cen == 300 | analysis_cen == 200) %>%
               filter(data_type == "οι") %>%
               droplevels()) %>% fisher.test()

# 4th and 2nd cens
stats::xtabs(~ location + analysis_cen,
             data = Data.oi[Data.oi$location == "Athens" |
                              Data.oi$location == "Tanagra",] %>%
               filter(analysis_cen == 400 | analysis_cen == 200) %>%
               filter(data_type == "οι") %>%
               droplevels()) %>% fisher.test()

# Compare inscription counts
stats::xtabs(phi_no ~ location + analysis_cen,
             data = NestedData)# [NestedData$location == "Athens" |
                                 #NestedData$location == "Tanagra",] %>%
               #filter(analysis_cen == 300 | analysis_cen == 200) %>%
               #droplevels())

stats::xtabs(~ location + analysis_cen,
             data = UnnestedData)# UnnestedData[UnnestedData$location == "Athens" |
                                   #UnnestedData$location == "Tanagra",] %>%
               #filter(analysis_cen == 300 | analysis_cen == 200) %>%
               #droplevels())

# Just Tanagran Data
stats::xtabs(~ location + analysis_cen,
             data = Data.oi[Data.oi$location == "Tanagra",] %>%
               droplevels())

### Hoi Epigraphes Tou Oropou ---------------------------------------------
## Range: 180185-180961 (tot. 776)

# Create empty DF
Oropou <- tibble(ig_book = NA,
                  ig_no = NA,
                  phi_no = NA,
                  header = NA,
                  location = NA,
                  date_after = NA,
                  date_before = NA,
                  text = NA)

# Define total number of captures
scrape_total <- 776

# Define progress bar
pb <- pb <- progress_bar$new(
  format = "  scraping Oropou [:bar] :percent eta: :eta (:spin)",
  total = scrape_total, clear = FALSE, width= 60)

# Capture start time (for run time reporting)
start_time <- Sys.time()

# Run Scrape() iteratievly
for (i in c(1:scrape_total)) {
  Oropou[i,] <- Scrape(phi_no=(i+180185))
  pb$tick()
  end_time <- Sys.time()
}

# Terminate progress bar
pb$terminate()

# Print run time reporting
print("Target: PHIno 180185-180961 (tot. 776)")
print(c("end_time - start_time: ", end_time-start_time))
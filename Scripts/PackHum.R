### Everything ------------------------------------------------------------
## IG I^2 - IG VII
## Range: 0 - 147791

# Create empty DF
PackHum <- tibble(ig_book = NA,
                    ig_no = NA,
                    phi_no = NA,
                    header = NA,
                    location = NA,
                    date_after = NA,
                    date_before = NA,
                    text = NA)

# Define total number of captures
scrape_total <- 183
#scrape_total <- 2000

# Define progress bar
pb <- pb <- progress_bar$new(
  format = "  scraping PackHum [:bar] :percent eta: :eta (:spin)",
  total = scrape_total, clear = FALSE, width= 60)

# Capture start time (for run time reporting)
start_time <- Sys.time()

# Run Scrape() iteratievly
for (i in c(1:scrape_total)) {
  PackHum[i+147608,] <- Scrape(phi_no=(i+147608))
  pb$tick()
  end_time <- Sys.time()
}

# Terminate progress bar
pb$terminate()

# Print run time reporting
print("Target: PHIno 0-147,791")
print(c("end_time - start_time: ", end_time-start_time))
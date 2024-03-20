###########################################################################
# ██████╗░██████╗░░█████╗░██████╗░                                        #
# ██╔══██╗██╔══██╗██╔══██╗██╔══██╗                                        #
# ██████╔╝██████╔╝██║░░██║██████╔╝                                        #
# ██╔═══╝░██╔══██╗██║░░██║██╔═══╝░                                        #
# ██║░░░░░██║░░██║╚█████╔╝██║░░░░░                                        #
# ╚═╝░░░░░╚═╝░░╚═╝░╚════╝░╚═╝░░░░░                                        #
#                                                                         #
# ████████╗░█████╗░██████╗░██╗░░░░░███████╗░██████╗                       #
# ╚══██╔══╝██╔══██╗██╔══██╗██║░░░░░██╔════╝██╔════╝                       #
# ░░░██║░░░███████║██████╦╝██║░░░░░█████╗░░╚█████╗░                       #
# ░░░██║░░░██╔══██║██╔══██╗██║░░░░░██╔══╝░░░╚═══██╗                       #
# ░░░██║░░░██║░░██║██████╦╝███████╗███████╗██████╔╝                       #
# ░░░╚═╝░░░╚═╝░░╚═╝╚═════╝░╚══════╝╚══════╝╚═════╝░                       #
#                                                                         #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#        Proportion Tables for Quantitative Testing of Isoglosses         #
###########################################################################

# Isogloss Chart Colors ---------------------------------------------------
iso_colors <- c(οι = "#E69F00",
                η = "#E69F00",
                υ ="#56B4E9",
                α = "#56B4E9")
  
  # E69F00- cb orange
  # 56B4E9- cb blue

# oi / y ------------------------------------------------------------------

# Important Note on oi
# Boeotian dat sg -οι from -ωι
# Dosuna, 469

## OIKIA ----------------------------------------------------------------
# Filter for oikia/ykia/woikia/wykia
Data.oi.oikia <- UnnestedData %>%
  filter(str_detect(token, "(ϝοι|ϝυ|οι|υ)(κι)(.{1,3})") == T)

# Write to .csv for data-type assignment
write_csv(Data.oi.oikia, "data/Data.oi.oikia.csv")

# Read in assigned data
Data.oi.oikia <- read_csv(
  "data/Data.oi.oikia.CONF.csv",
    col_types = cols(
    ig_book = col_factor(levels = c()),
    ig_no = col_factor(levels = c()),
    location = col_factor(levels = c()),
    token = col_factor(levels = c()),
    data_type = col_factor(levels = c()),
    simplex = col_logical()
  )
)

# Remove erroneous captures
Data.oi.oikia <- Data.oi.oikia[!is.na(Data.oi.oikia$data_type), ]

# Drop unused levels
Data.oi.oikia <- droplevels(Data.oi.oikia)

## AUTOS ------------------------------------------------------------------
# Filter for o-stem endings with -i
Data.oi.autos <- UnnestedData %>%
  filter(str_detect(token, "αυτ(οι|υ|ω)(ι|ς|ν){0,1}") == T)

# Write to .csv for data-type assignment
write_csv(Data.oi.autos, "data/Data.oi.autos.csv")

# Read in assigned data
Data.oi.autos <- read_csv(
  "data/Data.oi.autos.CONF.csv",
  col_types = cols(
    ig_book = col_factor(levels = c()),
    ig_no = col_factor(levels = c()),
    location = col_factor(levels = c()),
    token = col_factor(levels = c()),
    data_type = col_factor(levels = c()),
    simplex = col_logical()
  )
)

# Remove erroneous captures
Data.oi.autos <- Data.oi.autos[!is.na(Data.oi.autos$data_type), ]

## DHMOS ------------------------------------------------------------------
# Filter for o-stem endings with -i
Data.oi.dhmos <- UnnestedData %>%
  filter(str_detect(token, "δ(η|α)μ(οι|υ|ω)(ι|ς|ν){0,1}") == T)

# Write to .csv for data-type assignment
write_csv(Data.oi.dhmos, "data/Data.oi.dhmos.csv")

# Read in assigned data
Data.oi.dhmos <- read_csv(
  "data/Data.oi.dhmos.CONF.csv",
  col_types = cols(
    ig_book = col_factor(levels = c()),
    ig_no = col_factor(levels = c()),
    location = col_factor(levels = c()),
    token = col_factor(levels = c()),
    data_type = col_factor(levels = c()),
    simplex = col_logical()
  )
)

# Remove erroneous captures
Data.oi.dhmos <- Data.oi.dhmos[!is.na(Data.oi.dhmos$data_type), ]

## O ----------------------------------------------------------------------
# Filter for definite / relative articles with o-stem endings in -i
Data.oi.o <- UnnestedData %>%
  filter(str_detect(token, "^τ(οι|υ|ω|ω)(ι|ς|ν){0,1}$") == T) %>%
  filter(str_detect(token, "των") == F) # Remove gen pl

# Write to .csv for data-type assignment
write_csv(Data.oi.o, "data/Data.oi.o.csv")

# Read in assigned data
Data.oi.o <- read_csv(
  "data/Data.oi.o.CONF.csv",
  col_types = cols(
    ig_book = col_factor(levels = c()),
    ig_no = col_factor(levels = c()),
    location = col_factor(levels = c()),
    token = col_factor(levels = c()),
    data_type = col_factor(levels = c()),
    simplex = col_logical()
  )
)

# Remove erroneous captures
Data.oi.o <- Data.oi.o[!is.na(Data.oi.o$data_type), ]

# Final cleanup (extraneous capture)
# RegEx captured τυ as a fragment of τύχη
Data.oi.o <- Data.oi.o[-37, ]
Data.oi.o <- Data.oi.o[-166, ]

# RegEx captured τοι from, maybe, τοῖς
Data.oi.o <- Data.oi.o[-915, ]

## COMBINED ---------------------------------------------------------------
# Combine all assigned data tables
Data.oi <- Data.oi.oikia %>%
  full_join(Data.oi.o) %>%
  full_join(Data.oi.dhmos) %>%
  full_join(Data.oi.autos)

# Unify ωι and οι
# for (i in c(1:nrow(Data.oi))) {
#   if(Data.oi$data_type[i] == "ωι") {
#     Data.oi$data_type[i] <- "οι"
#   }
# }

# Remove ωι
Data.oi <- Data.oi[Data.oi$data_type != "ωι", ]

# Remove fragments τοι
Data.oi <- Data.oi %>%
  filter((
    str_detect(location, "(Athens|Eleusis)") == T &
      str_detect(token, "^τοι$") == T
  ) == F)

# Remove δήμοι for δήμωι
Data.oi <- Data.oi[-602, ]

# Drop unused levels
Data.oi <- droplevels(Data.oi)

# Count tokens
Data.oi %>% nrow()

# Count complex tokens
Data.oi[Data.oi$simplex == F, ] %>% nrow()

# Complex tokens
Data.oi[Data.oi$simplex == F, ]$token %>% droplevels() %>% levels()


## 4th Century Isoglosses -------------------------------------------------
### City Counts -----------------------------------------------------------
Data.oi.4 <-
  Data.oi[Data.oi$analysis_cen == 400, ] %>% # . . oi for 4th cen
  droplevels()

# > levels(Data.oi.4$location)
# [1] "Athens"  "Eleusis" "Megara"  "Oropos"  "Tanagra"

# Visualize location counts
Data.oi.4 %>%
  ggplot(aes(x = forcats::fct_infreq(location), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Location", y = "Token Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Athens ----------------------------------------------------------------
# Visualize tokens in Athens
Data.oi.4[Data.oi.4$location == "Athens", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
  labs(
    title = "Athenian οι/υ Tokens",
    subtitle = "Fourth Century BCE Inscriptions",
    x = "Token",
    y = "Count"
  ) +
  theme_classic() +
  theme(text = element_text(family = "DejaVu Sans Mono"))


### Eleusis ---------------------------------------------------------------
# Visualize tokens in Eleusis
Data.oi.4[Data.oi.4$location == "Eleusis",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))
### Megara ----------------------------------------------------------------
# Visualize tokens in Megara
Data.oi.4[Data.oi.4$location == "Megara",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Oropos ----------------------------------------------------------------
# Visualize tokens in Oropos
Data.oi.4[Data.oi.4$location == "Oropos",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Tanagra ---------------------------------------------------------------
# Filter for Tanagran Data
Data.oi.4.Tanagra <- Data.oi.4 %>%
  filter(str_detect(location, "(Athens|Tanagra)") == T) %>%
  droplevels()

# Visualize tokens in Tanagra
Data.oi.4[Data.oi.4$location == "Tanagra",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

# Contingency Table of Tanagran Data
XT.oi.4.Tanagra <-
  stats::xtabs(~ data_type + location, data = Data.oi.4.Tanagra)

# Proportion Table of Tanagran Data; rows add up to 1
Prop.oi.4.Tanagra <- prop.table(XT.oi.4.Tanagra, 1)

# Fisher's Exact Test for Tanagran Data
fisher.test(XT.oi.4.Tanagra) # . . . . Tanagran fisher's exact test


## 3rd Century Isoglosses -------------------------------------------------
### City Counts -----------------------------------------------------------
Data.oi.3 <-
  Data.oi[Data.oi$analysis_cen == 300, ] %>% # . . oi for.3th cen
  droplevels()

# > levels(Data.oi.3$location)
# [1] "Athens"  "Eleusis" "Megara"  "Oropos"  "Tanagra"

# Visualize location counts
Data.oi.3 %>%
  ggplot(aes(x = forcats::fct_infreq(location))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
  labs(
    title = "Token Counts by City",
    subtitle = "Third Century BCE Inscriptions",
    x = "Location",
    y = "Token Count"
  ) +
  theme_classic()

# Count tokens
nrow(Data.oi.3)

### Athens ----------------------------------------------------------------
# Visualize tokens in Athens
Data.oi.3[Data.oi.3$location == "Athens", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Eleusis ---------------------------------------------------------------
# Visualize tokens in Eleusis
Data.oi.3[Data.oi.3$location == "Eleusis", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Megara ---------------------------------------------------------------
# Visualize tokens in Megara
Data.oi.3[Data.oi.3$location == "Megara", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Pagai -----------------------------------------------------------------
# Visualize tokens in Pagai
Data.oi.3[Data.oi.3$location == "Pagai", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Aigosthena ------------------------------------------------------------
# Visualize tokens in Aigosthena
Data.oi.3[Data.oi.3$location == "Aigosthena", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Oropos ----------------------------------------------------------------
# Filter for Oropian Data
Data.oi.3.Oropos <- Data.oi.3 %>%
  filter(str_detect(location, "(Athens|Oropos)") == T) %>%
  droplevels()

Data.oi.3.Oropos[Data.oi.3.Oropos$location == "Oropos", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
  labs(
    title = "Oropian Token Counts",
    subtitle = "Third Century BCE Inscriptions",
    x = "Location",
    y = "Token Count"
  ) +
  theme_classic()

# Contingency Table of Oropian Data
XT.oi.3.Oropos <-
  stats::xtabs(~ data_type + location, data = Data.oi.3.Oropos)

# Proportion Table of Oropian Data; rows add up to 1
Prop.oi.3.Oropos <- prop.table(XT.oi.3.Oropos, 1)

fisher.test(XT.oi.3.Oropos) # . . . . Oropian fisher's exact test

### Tanagra ---------------------------------------------------------------
# Filter for Tanagran Data
Data.oi.3.Tanagra <- Data.oi.3 %>%
  filter(str_detect(location, "(Athens|Tanagra)") == T) %>%
  droplevels()

Data.oi.3.Tanagra[Data.oi.3.Tanagra$location == "Tanagra", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token))) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -1) +
  labs(
    title = "Tanagran Token Counts",
    subtitle = "Third Century BCE Inscriptions",
    x = "Location",
    y = "Token Count"
  ) +
  theme_classic() +
  theme(text = element_text(family = "DejaVu Sans Mono"))

# Contingency Table of Tanagran Data
XT.oi.3.Tanagra <-
  stats::xtabs(~ data_type + location, data = Data.oi.3.Tanagra)

# Proportion Table of Tanagran Data; rows add up to 1
Prop.oi.3.Tanagra <- prop.table(XT.oi.3.Tanagra, 1)

fisher.test(XT.oi.3.Tanagra) # . . . . Oropian fisher's exact test


## 2nd Century Isoglosses -------------------------------------------------
### City Counts -----------------------------------------------------------
Data.oi.2 <- Data.oi[Data.oi$analysis_cen == 200, ] %>%
  droplevels()

# > levels(Data.oi.3$location)
# [1] "Athens"  "Eleusis" "Megara"  "Oropos"  "Tanagra"

# Visualize location counts
Data.oi.2 %>%
  ggplot(aes(x = forcats::fct_infreq(location), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Location", y = "Token Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

# Count tokens
nrow(Data.oi.2)

### Athens ---------------------------------------------------------------
# Visualize tokens in Athens
Data.oi.2[Data.oi.2$location == "Athens", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

# View Athenian Data
Data.oi.2[Data.oi.2$location == "Athens", ] %>% View("2nd cen Athens")

### Eleusis ---------------------------------------------------------------
# Visualize tokens in Eleusis
Data.oi.2[Data.oi.2$location == "Eleusis", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Megara ---------------------------------------------------------------
# Visualize tokens in Megara
Data.oi.2[Data.oi.2$location == "Megara", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Aigosthena ------------------------------------------------------------
# Visualize tokens in Aigosthena
Data.oi.2[Data.oi.2$location == "Aigosthena", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Oropos ----------------------------------------------------------------
# Visualize tokens in Oropos
Data.oi.2[Data.oi.2$location == "Oropos", ] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

### Tanagra ---------------------------------------------------------------
# Filter for Tanagran Data
Data.oi.2.Tanagra <- Data.oi.2 %>%
  filter(str_detect(location, "(Athens|Tanagra)") == T) %>%
  droplevels()

# Visualize tokens in Tanagra
Data.oi.2[Data.oi.2$location == "Tanagra",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45))

# View Tanagran Data
Data.oi.2[Data.oi.2$location == "Tanagra", ] %>% View("2nd cen Tanagra")

# Contingency Table of Tanagran Data
XT.oi.2.Tanagra <-
  stats::xtabs(~ data_type + location, data = Data.oi.2.Tanagra)

# Proportion Table of Tanagran Data; rows add up to 1
Prop.oi.2.Tanagra <- prop.table(XT.oi.2.Tanagra, 1)

# Fisher's Exact Test for Tanagran Data
fisher.test(XT.oi.2.Tanagra) # . . . . Tanagran fisher's exact test

## Cleanup ----------------------------------------------------------------
# Cleanup Data.oi. type variables
rm(list=ls(pattern="^Data\\.oi\\."))

# Cleanup Prop.oi. type variables
rm(list=ls(pattern="^Prop\\.oi\\."))

# Cleanup XT.oi. type variables
rm(list=ls(pattern="^XT\\.oi\\."))

# h / a: ------------------------------------------------------------------
## DHMOS ------------------------------------------------------------------
# Filter for h in dhmos
Data.h.dhmos <- UnnestedData %>%
  filter(str_detect(token, "δ(η|α)μ.+") == T)

# Write to .csv for data-type assignment
write_csv(Data.h.dhmos, "data/Data.h.dhmos.csv")

# Read in assigned data
Data.h.dhmos <- read_csv(
  "data/Data.h.dhmos.CONF.csv",
  col_types = cols(
    ig_book = col_factor(levels = c()),
    ig_no = col_factor(levels = c()),
    location = col_factor(levels = c()),
    token = col_factor(levels = c()),
    data_type = col_factor(levels = c()),
    simplex = col_logical()
  )
)

# Remove erroneous captures
Data.h.dhmos <- Data.h.dhmos[!is.na(Data.h.dhmos$data_type), ]
Data.h.dhmos <- Data.h.dhmos[!is.na(Data.h.dhmos$simplex), ]

# I... think I need to drop Δημήτριος
Data.h.dhmos <- Data.h.dhmos %>%
  filter(str_detect(token, "(δημητρ|δαματρ)") == F)

# Name Δάμων != δαμ- + ...
Data.h.dhmos <- Data.h.dhmos[-227,]

# Drop unused levels
Data.h.dhmos <- droplevels(Data.h.dhmos)

## GH ---------------------------------------------------------------------
# filter for h in gh
Data.h.gh <- UnnestedData %>%
  filter(str_detect(token, "^γ(η|α)(ι|ς|ν){0,1}$") == T)

# Write to .csv for data-type assignment
write_csv(Data.h.gh, "data/Data.h.gh.csv")

# Read in assigned data
Data.h.gh <- read_csv(
  "data/Data.h.gh.CONF.csv",
  col_types = cols(
    ig_book = col_factor(levels = c()),
    ig_no = col_factor(levels = c()),
    location = col_factor(levels = c()),
    token = col_factor(levels = c()),
    data_type = col_factor(levels = c()),
    simplex = col_logical()
  )
)

# Remove erroneous captures
Data.h.gh <- Data.h.gh[!is.na(Data.h.gh$data_type), ]

# Drop unused levels
Data.h.gh <- droplevels(Data.h.gh)

## BOULH ------------------------------------------------------------------
# filter for h in boulh
Data.h.boulh <- UnnestedData %>%
  filter(str_detect(token, "β(ω|ου)λ(η|α)(ι|ς|ν){0,1}") == T)

# Write to .csv for data-type assignment
write_csv(Data.h.boulh, "data/Data.h.boulh.csv")

# Read in assigned data
Data.h.boulh <- read_csv(
  "data/Data.h.boulh.CONF.csv",
  col_types = cols(
    ig_book = col_factor(levels = c()),
    ig_no = col_factor(levels = c()),
    location = col_factor(levels = c()),
    token = col_factor(levels = c()),
    data_type = col_factor(levels = c()),
    simplex = col_logical()
  )
)

# Remove erroneous captures
Data.h.boulh <- Data.h.boulh[!is.na(Data.h.boulh$data_type), ]

# Drop unused levels
Data.h.boulh <- droplevels(Data.h.boulh)

## STHLH ------------------------------------------------------------------
# filter for h's in sthlh
# For the sake of clarity, analyze by root vowel
# (There's five plurals with -a- and I don't want to get things mixed up!)
Data.h.sthlh <- UnnestedData %>%
  filter(str_detect(token, "στ(η|α)λ(η|α)(ι|ς|ν){0,1}") == T)

# Write to .csv for data-type assignment
write_csv(Data.h.sthlh, "data/Data.h.sthlh.csv")

# Read in assigned data
Data.h.sthlh <- read_csv(
  "data/Data.h.sthlh.CONF.csv",
  col_types = cols(
    ig_book = col_factor(levels = c()),
    ig_no = col_factor(levels = c()),
    location = col_factor(levels = c()),
    token = col_factor(levels = c()),
    data_type = col_factor(levels = c()),
    simplex = col_logical()
  )
)

# Remove erroneous captures
Data.h.sthlh <- Data.h.sthlh[!is.na(Data.h.sthlh$data_type), ]

# Drop unused levels
Data.h.sthlh <- droplevels(Data.h.sthlh)

## COMBINED ---------------------------------------------------------------
# Combine all assigned data tables
Data.h <- Data.h.sthlh %>%
  full_join(Data.h.gh) %>%
  full_join(Data.h.dhmos) %>%
  full_join(Data.h.boulh)

## 4th Century Isoglosses -------------------------------------------------
### City Counts -----------------------------------------------------------
Data.h.4 <-
  Data.h[Data.h$analysis_cen == 400, ] %>% # . . oi for 4th cen
  droplevels()

# Count
Data.h.4 %>% nrow()

# > levels(Data.oi.4$location)
# [1] "Athens"  "Eleusis" "Megara"  "Oropos"  "Tanagra"

# Visualize location counts
Data.h.4 %>%
  ggplot(aes(x = forcats::fct_infreq(location), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Location", y = "Token Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

### Athens ----------------------------------------------------------------
#### A0 gen data ----------------------------------------------------------
# Visualize tokens in Athens
Data.h.4[Data.h.4$location == "Athens",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian tokens
Data.h.4[Data.h.4$location == "Athens",] %>%
  nrow()

# Display table of Athenian tokens
Data.h.4 %>% filter(str_detect(location, "Athens") == T) %>% View("Athens")

#### A1 δημ data ----------------------------------------------------------
# Athenian η-type δημ data
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type δημ data
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  nrow()

# Count non-simplex δημ data
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  filter(str_detect(token, "^δημωι$") == F) %>%
  filter(str_detect(token, "^δημου$") == F) %>%
  filter(str_detect(token, "^δημον$") == F) %>%
  filter(str_detect(token, "^δημος$") == F) %>%
  nrow()

#### A2 στηλ data ---------------------------------------------------------
# Athenian η-type στηλη data
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type στηλη data
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  nrow()

# View Athenian η-type στηλ data
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  View()

#### A3 γη-type data ------------------------------------------------------
# Athenian η-type γη data
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type γη data
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  nrow()

#### A4 βουλη-type data ---------------------------------------------------
# Athenian η-type βουλη data
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type βουλη data
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

#### A6 α-type data -------------------------------------------------------
# Athenian α-type δαμ data 
Data.h.4 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

### Eleusis ---------------------------------------------------------------
#### E0 gen data ----------------------------------------------------------
# Visualize tokens in Eleusis
Data.h.4[Data.h.4$location == "Eleusis",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count
Data.h.4[Data.h.4$location == "Eleusis",] %>%
  nrow()

# View
Data.h.4[Data.h.4$location == "Eleusis",] %>%
  View("Eleusis")

#### E1 δημ data ----------------------------------------------------------
# Eleusinean η-type δημ data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count the data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  nrow()

#### E2 στηλ data ---------------------------------------------------------
# Eleusinean η-type στηλ data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count the data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  nrow()

#### E3 βουλη-type data ---------------------------------------------------
# Eleusinean η-type βουλη data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean η-type βουλη data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()#nrow()

#### E4 γη-type data ------------------------------------------------------
# Eleusinean η-type γη data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "(γη|δημητρ)") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean η-type γη data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  nrow()

# View Eleusinean η-type γη data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  View("4.el.γη")

#### E5 α-type data --------------------------------------------------------
# Eleusinean α-type data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean η-type γη data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  nrow()

# View Eleusinean α-type data
Data.h.4 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View("4.el.α")

#### E6 Test --------------------------------------------------------------
Data.h.4.Eleusis <- Data.h.4 %>%
  filter(str_detect(location, "(Athens|Eleusis)") == T) %>%
  droplevels()

# Contingency Table of Eleusinean Data
XT.h.4.Eleusis <-
  stats::xtabs(~ data_type + location, data = Data.h.4.Eleusis)

# Proportion Table of Eleusinean Data; rows add up to 1
# Prop.h.4.Eleusis <- prop.table(XT.h.4.Eleusis, 1)

# Fisher's Exact Test for Eleusinean Data
fisher.test(XT.h.4.Eleusis) # . . . . Eleusinean fisher's exact test

### Oropos ----------------------------------------------------------------
#### O0 gen data ----------------------------------------------------------
# Visualize tokens in Oropos
Data.h.4[Data.h.4$location == "Oropos",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count
Data.h.4[Data.h.4$location == "Oropos",] %>%
  nrow()

#### O1 δημ data ----------------------------------------------------------
# Oropian η-type δημ data
Data.h.4 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count the data
Data.h.4 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  nrow()

#### O2 στηλ data ---------------------------------------------------------
# Oropian η-type στηλ data
Data.h.4 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count the data
Data.h.4 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  nrow()

#### O3 βουλη-type data ---------------------------------------------------
# Oropian η-type βουλη data
Data.h.4 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian η-type βουλη data
Data.h.4 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Oropian η-type βουλη data
Data.h.4 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View("4.el.γη")

#### O4 γη-type data ------------------------------------------------------
# Oropian η-type γη data
Data.h.4 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian η-type γη data
Data.h.4 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  nrow()

# View Oropian η-type γη data
Data.h.4 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  View("4.el.γη")

### Megara ----------------------------------------------------------------
#### M0 gen data ----------------------------------------------------------
# Visualize tokens in Megara
Data.h.4[Data.h.4$location == "Megara",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count the Megarian Tokens
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  nrow()

#### M1 δαμ data ----------------------------------------------------------
# Megarian α-type δαμ data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "δαμ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian α-type δαμ data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "δαμ") == T) %>%
  nrow()

# View Megarian α-type δαμ data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "δαμ") == T) %>%
  View("4.meg.δαμ")

#### M2 σταλ data ---------------------------------------------------------
# Megarian α-type σταλ data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "σταλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian α-type σταλα data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "σταλ") == T) %>%
  nrow()

# View Megarian α-type σταλα data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View("4.meg.σταλ")

#### M3 βουλα-type data ---------------------------------------------------
# Megarian α-type βουλα data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian α-type βουλα data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Megarian α-type βουλα data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View("4.meg.βουλ")

#### M4 γα-type data ------------------------------------------------------
# Megarian α-type γα data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "γα") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian α-type γα data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "γα") == T) %>%
  nrow()

# View Megarian α-type γα data
Data.h.4 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "γα") == T) %>%
  View("4.meg.γα")

#### M5 Test --------------------------------------------------------------
Data.h.4.Megara <- Data.h.4 %>%
  filter(str_detect(location, "(Athens|Megara)") == T) %>%
  droplevels()

# Contingency Table of Megarian Data
XT.h.4.Megara <-
  stats::xtabs(~ data_type + location, data = Data.h.4.Megara)

# Proportion Table of Megarian Data; rows add up to 1
# Prop.h.4.Megara <- prop.table(XT.h.4.Megara, 1)

# Fisher's Exact Test for Megarian Data
fisher.test(XT.h.4.Megara) # . . . . Megarian fisher's exact test

### Tanagra ---------------------------------------------------------------
#### T0 gen data ----------------------------------------------------------
# Visualize tokens in Tanagra
Data.h.4[Data.h.4$location == "Tanagra",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

#### T1 δαμ data ----------------------------------------------------------
# Tanagran α-type δαμ data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "δαμ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Tanagran α-type δαμ data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "δαμ") == T) %>%
  nrow()

# View Tanagran α-type δαμ data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "δαμ") == T) %>%
  View("4.meg.δαμ")

#### T2 σταλ data ---------------------------------------------------------
# Tanagran α-type σταλ data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "σταλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Tanagran α-type σταλα data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "σταλ") == T) %>%
  nrow()

# View Tanagran α-type σταλα data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View("4.meg.σταλ")

#### T3 βουλα-type data ---------------------------------------------------
# Tanagran α-type βουλα data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Tanagran α-type βουλα data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Tanagran α-type βουλα data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View("4.meg.βουλ")

#### T4 γα-type data ------------------------------------------------------
# Tanagran α-type γα data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "γα") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Tanagran α-type γα data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "γα") == T) %>%
  nrow()

# View Tanagran α-type γα data
Data.h.4 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "γα") == T) %>%
  View("4.meg.γα")

#### T5 Test --------------------------------------------------------------
Data.h.4.Tanagra <- Data.h.4 %>%
  filter(str_detect(location, "(Athens|Tanagra)") == T) %>%
  droplevels()

# Contingency Table of Tanagran Data
XT.h.4.Tanagra <-
  stats::xtabs(~ data_type + location, data = Data.h.4.Tanagra)

# Proportion Table of Tanagran Data; rows add up to 1
# Prop.h.4.Tanagra <- prop.table(XT.h.4.Tanagra, 1)

# Fisher's Exact Test for Tanagran Data
fisher.test(XT.h.4.Tanagra) # . . . . Tanagran fisher's exact test

## 3rd Century Isoglosses -------------------------------------------------
### City Counts -----------------------------------------------------------
Data.h.3 <-
  Data.h[Data.h$analysis_cen == 300, ] %>% # . . oi for 4th cen
  droplevels()

# Count
Data.h.3 %>% nrow()

# > levels(Data.oi.4$location)
# [1] "Athens"  "Eleusis" "Megara"  "Oropos"  "Tanagra"

# Visualize location counts
Data.h.3 %>%
  ggplot(aes(x = forcats::fct_infreq(location), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Location", y = "Token Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

### Athens ----------------------------------------------------------------
#### A0 General Data ------------------------------------------------------
# Visualize
Data.h.3[Data.h.3$location == "Athens",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  nrow()

#### A1 δημ data ----------------------------------------------------------
# Athenian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  nrow()

# Count non-simplex δημ data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  filter(str_detect(token, "^δημωι$") == F) %>%
  filter(str_detect(token, "^δημου$") == F) %>%
  filter(str_detect(token, "^δημον$") == F) %>%
  filter(str_detect(token, "^δημος$") == F) %>%
  nrow()

#### A2 στηλ data ---------------------------------------------------------
# Athenian η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  nrow()

# View Athenian η-type στηλ data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  View()

#### A3 γη-type data ------------------------------------------------------
# Athenian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  nrow()

#### A4 βουλη-type data ---------------------------------------------------
# Athenian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

#### A5 α-type data -------------------------------------------------------
# Athenian α-type δαμ data 
Data.h.3 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

### Eleusis ----------------------------------------------------------------
#### E0 General Data ------------------------------------------------------
# Visualize
Data.h.3[Data.h.3$location == "Eleusis",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  nrow()

#### E1 δημ data ----------------------------------------------------------
# Eleusinean η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  nrow()

# View Eleusinean η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  View()

#### E2 στηλ data ---------------------------------------------------------
# Eleusinean η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  nrow()

# View Eleusinean η-type στηλ data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  View()

#### E3 γη-type data ------------------------------------------------------
# Eleusinean η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  nrow()

# View Eleusinean η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  View()

#### E4 βουλη-type data ---------------------------------------------------
# Eleusinean η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Eleusinean η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()

#### E5 α-type data -------------------------------------------------------
# Eleusinean α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

### Oropos ----------------------------------------------------------------
#### O0 General Data ------------------------------------------------------
# Visualize
Data.h.3[Data.h.3$location == "Oropos",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  nrow()

#### O1 δημ data ----------------------------------------------------------
# Oropian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  nrow()

# View Oropian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  View()

# Count non-simplex δημ data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  filter(str_detect(token, "^δημωι$") == F) %>%
  filter(str_detect(token, "^δημου$") == F) %>%
  filter(str_detect(token, "^δημον$") == F) %>%
  filter(str_detect(token, "^δημος$") == F) %>%
  nrow()

#### O2 στηλ data ---------------------------------------------------------
# Oropian η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  nrow()

# View Oropian η-type στηλ data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  View()

#### O3 γη-type data ------------------------------------------------------
# Oropian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  nrow()

# View Oropian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  View()

#### O4 βουλη-type data ---------------------------------------------------
# Oropian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Oropian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()

#### O5 α-type data -------------------------------------------------------
# Oropian α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

# Count α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  nrow()

#### 06 Test --------------------------------------------------------------
Data.h.3.Oropos <- Data.h.3 %>%
  filter(str_detect(location, "(Athens|Oropos)") == T) %>%
  droplevels()

# Contingency Table of Oropian Data
XT.h.3.Oropos <-
  stats::xtabs(~ data_type + location, data = Data.h.3.Oropos)

# Proportion Table of Oropian Data; rows add up to 1
# Prop.h.3.Oropos <- prop.table(XT.h.3.Oropos, 1)

# Fisher's Exact Test for Oropian Data
fisher.test(XT.h.3.Oropos) # . . . . Oropian fisher's exact test

### Megara ----------------------------------------------------------------
#### M0 General Data ------------------------------------------------------
# Visualize
Data.h.3[Data.h.3$location == "Megara",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  nrow()

#### M1 δημ data ----------------------------------------------------------
# Megarian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  nrow()

# View Megarian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  View()

#### M2 στηλ data ---------------------------------------------------------
# Megarian η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  nrow()

# View Megarian η-type στηλ data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  View()

#### M3 γη-type data ------------------------------------------------------
# Megarian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  nrow()

# View Megarian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  View()

#### M4 βουλη-type data ---------------------------------------------------
# Megarian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Megarian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()

#### M5 α-type data -------------------------------------------------------
# Megarian α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

# Count α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  nrow()

#### M6 Test --------------------------------------------------------------
Data.h.3.Megara <- Data.h.3 %>%
  filter(str_detect(location, "(Athens|Megara)") == T) %>%
  droplevels()

# Contingency Table of Megarian Data
XT.h.3.Megara <-
  stats::xtabs(~ data_type + location, data = Data.h.3.Megara)

# Proportion Table of Megarian Data; rows add up to 1
# Prop.h.3.Megara <- prop.table(XT.h.3.Megara, 1)

# Fisher's Exact Test for Megarian Data
fisher.test(XT.h.3.Megara) # . . . . Megarian fisher's exact test

### Pagai ----------------------------------------------------------------
#### P0 General Data ------------------------------------------------------
# Visualize
Data.h.3[Data.h.3$location == "Pagai",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Pagaian data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  nrow()

#### P1 δημ data ----------------------------------------------------------
# Pagaian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Pagaian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  nrow()

# View Pagaian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  View()

#### P2 στηλ data ---------------------------------------------------------
# Pagaian η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Pagaian η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  nrow()

# View Pagaian η-type στηλ data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  View()

#### P3 γη-type data ------------------------------------------------------
# Pagaian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Pagaian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  nrow()

# View Pagaian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  View()

#### P4 βουλη-type data ---------------------------------------------------
# Pagaian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Pagaian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Pagaian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()

#### P5 α-type data -------------------------------------------------------
# Pagaian α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

# Count α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Pagai") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  nrow()

#### P6 Test --------------------------------------------------------------
Data.h.3.Pagai <- Data.h.3 %>%
  filter(str_detect(location, "(Athens|Pagai)") == T) %>%
  droplevels()

# Contingency Table of Pagaian Data
XT.h.3.Pagai <-
  stats::xtabs(~ data_type + location, data = Data.h.3.Pagai)

# Proportion Table of Pagaian Data; rows add up to 1
# Prop.h.3.Pagai <- prop.table(XT.h.3.Pagai, 1)

# Fisher's Exact Test for Pagaian Data
fisher.test(XT.h.3.Pagai) # . . . . Pagaian fisher's exact test

### Aigosthena ------------------------------------------------------------
#### A0 General Data ------------------------------------------------------
# Visualize
Data.h.3[Data.h.3$location == "Aigosthena",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Aigosthenian data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  nrow()

#### A1 δημ data ----------------------------------------------------------
# Aigosthenian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Aigosthenian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  nrow()

# View Aigosthenian η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  View()

#### A2 στηλ data ---------------------------------------------------------
# Aigosthenian η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Aigosthenian η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  nrow()

# View Aigosthenian η-type στηλ data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  View()

#### A3 γη-type data ------------------------------------------------------
# Aigosthenian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Aigosthenian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  nrow()

# View Aigosthenian η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  View()

#### A4 βουλη-type data ---------------------------------------------------
# Aigosthenian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Aigosthenian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Aigosthenian η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()

#### A5 α-type data -------------------------------------------------------
# Aigosthenian α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

# Count α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  nrow()

#### A6 Test --------------------------------------------------------------
Data.h.3.Aigosthena <- Data.h.3 %>%
  filter(str_detect(location, "(Athens|Aigosthena)") == T) %>%
  droplevels()

# Contingency Table of Aigosthenian Data
XT.h.3.Aigosthena <-
  stats::xtabs(~ data_type + location, data = Data.h.3.Aigosthena)

# Proportion Table of Aigosthenian Data; rows add up to 1
# Prop.h.3.Aigosthena <- prop.table(XT.h.3.Aigosthena, 1)

# Fisher's Exact Test for Aigosthenian Data
fisher.test(XT.h.3.Aigosthena) # . . . . Aigosthenian fisher's exact test

### Tanagra ------------------------------------------------------------
#### T0 General Data ------------------------------------------------------
# Visualize
Data.h.3[Data.h.3$location == "Tanagra",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Tanagran data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  nrow()

#### T1 δημ data ----------------------------------------------------------
# Tanagran η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Tanagran η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  nrow()

# View Tanagran η-type δημ data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  View()

#### T2 στηλ data ---------------------------------------------------------
# Tanagran η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Tanagran η-type στηλη data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  nrow()

# View Tanagran η-type στηλ data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  View()

#### T3 γη-type data ------------------------------------------------------
# Tanagran η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Tanagran η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  nrow()

# View Tanagran η-type γη data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  View()

#### T4 βουλη-type data ---------------------------------------------------
# Tanagran η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Tanagran η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Tanagran η-type βουλη data
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()

#### T5 α-type data -------------------------------------------------------
# Tanagran α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

# Count α-type data 
Data.h.3 %>%
  filter(str_detect(location, "Tanagra") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  nrow()

#### T6 Test --------------------------------------------------------------
Data.h.3.Tanagra <- Data.h.3 %>%
  filter(str_detect(location, "(Athens|Tanagra)") == T) %>%
  droplevels()

# Contingency Table of Tanagran Data
XT.h.3.Tanagra <-
  stats::xtabs(~ data_type + location, data = Data.h.3.Tanagra)

# Proportion Table of Tanagran Data; rows add up to 1
# Prop.h.3.Tanagra <- prop.table(XT.h.3.Tanagra, 1)

# Fisher's Exact Test for Tanagran Data
fisher.test(XT.h.3.Tanagra) # . . . . Tanagran fisher's exact test

## 2th Century Isoglosses -------------------------------------------------
### City Counts -----------------------------------------------------------
Data.h.2 <-
  Data.h[Data.h$analysis_cen == 200, ] %>% # . . oi for 4th cen
  droplevels()

# Count
Data.h.2 %>% nrow()

# Visualize location counts
Data.h.2 %>%
  ggplot(aes(x = forcats::fct_infreq(location), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Location", y = "Token Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

### Athens ----------------------------------------------------------------
#### A0 gen data ----------------------------------------------------------
# Visualize tokens in Athens
Data.h.2[Data.h.2$location == "Athens",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian tokens
Data.h.2[Data.h.2$location == "Athens",] %>%
  nrow()

# Display table of Athenian tokens
Data.h.2 %>% filter(str_detect(location, "Athens") == T) %>% View("Athens")

#### A1 δημ data ----------------------------------------------------------
# Athenian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  nrow()

# Athenian α-type tokens:
# Δαμόκριτος G II² 913 (PH# 3,133) - proper noun, same inscription as
# Δημοφάνης & δήμου (x2);
# Δάμων IG II² 977 (PH# 3,197) - proper noun, same inscription as 
# δήμωι (x2), δήμου (x2), and δῆμον (x1); 

#### A2 στηλ data ---------------------------------------------------------
# Athenian η-type στηλη data
Data.h.2 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type στηλη data
Data.h.2 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  nrow()

# View Athenian η-type στηλ data
Data.h.2 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  View()

#### A3 γη-type data ------------------------------------------------------
# Athenian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  nrow()

#### A4 βουλη-type data ---------------------------------------------------
# Athenian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Athenian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

#### A6 α-type data -------------------------------------------------------
# Athenian α-type δαμ data 
Data.h.2 %>%
  filter(str_detect(location, "Athens") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

### Eleusis ----------------------------------------------------------------
#### E0 General Data ------------------------------------------------------
# Visualize
Data.h.2[Data.h.2$location == "Eleusis",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  nrow()

#### E1 δημ data ----------------------------------------------------------
# Eleusinean η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# ὁ δῆμος (x2) from IG II² 949 (PH# 3,169) are good to go

# Count Eleusinean η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  nrow()

# View Eleusinean η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "δημ") == T) %>%
  View()

#### E2 στηλ data ---------------------------------------------------------
# Eleusinean η-type στηλη data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean η-type στηλη data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  nrow()

# View Eleusinean η-type στηλ data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "στηλ") == T) %>%
  View()

#### E3 γη-type data ------------------------------------------------------
# Eleusinean η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Eleusinean η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  nrow()

# View Eleusinean η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "γη") == T) %>%
  View()

#### E4 βουλη-type data ---------------------------------------------------
# Eleusinean η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# ἡ βουλή (x1) from IG II² 949 (PH# 3,169) is good to go
# ἡ βουλή (x1) from IG II² 1,013 (PH# 3,233) is good to go

# Count Eleusinean η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Eleusinean η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()

#### E5 α-type data -------------------------------------------------------
# Eleusinean α-type data 
Data.h.2 %>%
  filter(str_detect(location, "Eleusis") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

### Oropos ----------------------------------------------------------------
#### O0 General Data ------------------------------------------------------
# Visualize
Data.h.2[Data.h.2$location == "Oropos",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  nrow()

#### O1 δημ data ----------------------------------------------------------
# Oropian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Δαμάρχου (x1) from IG VII 3,498 (PH# 147,019) - "Inventory of silver
# dedications in the sanctuary of Amphiaraos" - is a patronym:
# ἱεραρχούντων Ἱεροκλέους τοῦ Δαμάρχου "during the hierarchies of Hierokles
# son of Damarchos, ..."
# Appears alongside Δήμαρχος (x1), Δημαινέτου, (x1) Ἀλεξιδήμου (x3),
# Δημονείκου (x1), and Ἀριστοδήμου (x1).
# However, no simplex δημ or στηλ, needs further testing

# Count Oropian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  nrow()

# View Oropian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  View()

#### O2 στηλ data ---------------------------------------------------------
# Oropian η-type στηλη data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian η-type στηλη data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  nrow()

# View Oropian η-type στηλ data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  View()

#### O3 γη-type data ------------------------------------------------------
# Oropian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  nrow()

# View Oropian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  View()

#### O4 βουλη-type data ---------------------------------------------------
# Oropian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Oropian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Oropian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()

#### O5 α-type data -------------------------------------------------------
# Oropian α-type data 
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

# Count α-type data 
Data.h.2 %>%
  filter(str_detect(location, "Oropos") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  nrow()

#### 06 Test --------------------------------------------------------------
Data.h.2.Oropos <- Data.h.2 %>%
  filter(str_detect(location, "(Athens|Oropos)") == T) %>%
  droplevels()

# Contingency Table of Oropian Data
XT.h.2.Oropos <-
  stats::xtabs(~ data_type + location, data = Data.h.2.Oropos)

# Proportion Table of Oropian Data; rows add up to 1
# Prop.h.2.Oropos <- prop.table(XT.h.2.Oropos, 1)

# Fisher's Exact Test for Oropian Data
fisher.test(XT.h.2.Oropos) # . . . . Oropian fisher's exact test

# P = 1

### Megara ----------------------------------------------------------------
#### M0 General Data ------------------------------------------------------
# Visualize
Data.h.2[Data.h.2$location == "Megara",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  nrow()

#### M1 δημ data ----------------------------------------------------------
# Megarian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# ὁ δᾶμος (x2) from IG VII 58 (PH# 143,534) and IG VII 3,490 (PH# 147,011)
# are good to go

# Count Megarian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  nrow()

# View Megarian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  View()

#### M2 στηλ data ---------------------------------------------------------
# Megarian η-type στηλη data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian η-type στηλη data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  nrow()

# View Megarian η-type στηλ data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  View()

#### M3 γη-type data ------------------------------------------------------
# Megarian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  nrow()

# View Megarian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  View()

#### M4 βουλη-type data ---------------------------------------------------
# Megarian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Megarian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Megarian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()

#### M5 α-type data -------------------------------------------------------
# Megarian α-type data 
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

# Count α-type data 
Data.h.2 %>%
  filter(str_detect(location, "Megara") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  nrow()

#### M6 Test --------------------------------------------------------------
Data.h.2.Megara <- Data.h.2 %>%
  filter(str_detect(location, "(Athens|Megara)") == T) %>%
  droplevels()

# Contingency Table of Megarian Data
XT.h.2.Megara <-
  stats::xtabs(~ data_type + location, data = Data.h.2.Megara)

# Proportion Table of Megarian Data; rows add up to 1
# Prop.h.2.Megara <- prop.table(XT.h.2.Megara, 1)

# Fisher's Exact Test for Megarian Data
fisher.test(XT.h.2.Megara) # . . . . Megarian fisher's exact test

# p-value = 2.949e-13

### Aigosthena ------------------------------------------------------------
#### A0 General Data ------------------------------------------------------
# Visualize
Data.h.2[Data.h.2$location == "Aigosthena",] %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values=iso_colors)

# Count Aigosthenian data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  nrow()

#### A1 δημ data ----------------------------------------------------------
# Aigosthenian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(subtitle = "δημ- Tokens", x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Aigosthenian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  nrow()

# View Aigosthenian η-type δημ data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "δ(η|α)μ") == T) %>%
  View()

#### A2 στηλ data ---------------------------------------------------------
# Aigosthenian η-type στηλη data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Aigosthenian η-type στηλη data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  nrow()

# View Aigosthenian η-type στηλ data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "στ(η|α)λ") == T) %>%
  View()

#### A3 γη-type data ------------------------------------------------------
# Aigosthenian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Aigosthenian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  nrow()

# View Aigosthenian η-type γη data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "γ(η|α)") == T) %>%
  View()

#### A4 βουλη-type data ---------------------------------------------------
# Aigosthenian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  ggplot(aes(x = forcats::fct_infreq(token), fill = data_type)) +
  geom_bar() +
  geom_label(stat = "count", aes(label = after_stat(count))) +
  labs(x = "Token", y = "Count") +
  theme_classic() +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  scale_fill_manual(values=iso_colors)

# Count Aigosthenian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  nrow()

# View Aigosthenian η-type βουλη data
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(token, "βουλ") == T) %>%
  View()

#### A5 α-type data -------------------------------------------------------
# Aigosthenian α-type data 
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  View()

# Count α-type data 
Data.h.2 %>%
  filter(str_detect(location, "Aigosthena") == T) %>%
  filter(str_detect(data_type, "α") == T) %>%
  nrow()

#### A6 Test --------------------------------------------------------------
Data.h.2.Aigosthena <- Data.h.2 %>%
  filter(str_detect(location, "(Athens|Aigosthena)") == T) %>%
  droplevels()

# Contingency Table of Aigosthenian Data
XT.h.2.Aigosthena <-
  stats::xtabs(~ data_type + location, data = Data.h.2.Aigosthena)

# Proportion Table of Aigosthenian Data; rows add up to 1
# Prop.h.2.Aigosthena <- prop.table(XT.h.2.Aigosthena, 1)

# Fisher's Exact Test for Aigosthenian Data
fisher.test(XT.h.2.Aigosthena) # . . . . Aigosthenian fisher's exact test

# p-value = 9.581e-06


# 0 / w -------------------------------------------------------------------
# OIKIA ...................................................................
# # Filter for w in oikos
# Data.w.oikia <- UnnestedData %>%
#   filter(str_detect(token, "(ϝοι|ϝυ|οι|υ)(κι)(.{1,3})") == T)
# 
# # Write to .csv for data-type assignment
# write_csv(Data.w.oikia, "data/Data.w.oikia.csv")
# 
# # Read in assigned data
# Data.w.oikia <- read_csv(
#   "data/Data.w.oikia.CONF.csv",
#   col_types = cols(
#     ig_book = col_factor(levels = c()),
#     ig_no = col_factor(levels = c()),
#     location = col_factor(levels = c()),
#     token = col_factor(levels = c()),
#     data_type = col_factor(levels = c()),
#     simplex = col_logical()
#   )
# )
# 
# # Remove erroneous captures
# Data.w.oikia <- Data.w.oikia[!is.na(Data.w.oikia$data_type), ]
# 
# # ISOTELEIA ...............................................................
# # Filter for w in isoteleia
# # ϝισοτέλιαν in DEG 459.3
# Data.w.isoteleia <- UnnestedData %>%
#   filter(str_detect(token, "ισοτελ") == T)
# 
# # Write to .csv for data-type assignment
# write_csv(Data.w.isoteleia, "data/Data.w.isoteleia.csv")
# 
# # Read in assigned data
# Data.w.isoteleia <- read_csv(
#   "data/Data.w.isoteleia.CONF.csv",
#   col_types = cols(
#     ig_book = col_factor(levels = c()),
#     ig_no = col_factor(levels = c()),
#     location = col_factor(levels = c()),
#     token = col_factor(levels = c()),
#     data_type = col_factor(levels = c()),
#     simplex = col_logical()
#   )
# )
# 
# # Remove erroneous captures
# Data.w.isoteleia <-
#   Data.w.isoteleia[!is.na(Data.w.isoteleia$data_type), ]
# 
# # ETOS ....................................................................
# # Filter for w in etos
# # ϝέτια for ἔτεα and ϝετίων for ἐτέων in DEG 462.A
# # No matches ;(
# 
# 
# 
# # ai / h ----------------------------------------------------------------
# Data.ai.kai <- UnnestedData %>%
#   filter(str_detect(token, "^κ(η|αι)$") == T)
# 
# UnnestedData %>% filter(str_detect(token, "φ(αι|η)ν") == T) %>% View()
# 
# # eyergeths for eyergetais in DEG 460.1
# 
# # zd / dd ---------------------------------------------------------------
# Data.zd.psafizd <- UnnestedData %>%
#   filter(str_detect(token, "ψαφι(ζ|δδ)") == T)
# Data.zd.grammatizd <- UnnestedData %>%
#   filter(str_detect(token, "γραμματι(ζ|δδ)") == T)
# Data.zd.perizdug <- UnnestedData %>%
#   filter(str_detect(token, "περι(ζ|δδ).{1,3}") == T)
# 
# # -nai / -men -----------------------------------------------------------
# Data.nai.einai <- UnnestedData %>%
#   filter(str_detect(token, "^ει(ναι|μεν)$") == T)
# # ἀνθέμεν for ἀναθεῖναι in DEG 154
# # cf. The Aeolic Dialects (Dosuna, 463):
# # "Extension of the athematic infinitive suffix -μεν to thematic verbs in
# # Beoetian and in Thessalian (Pelasgiotis)".
# Data.nai.anaqeinai <- UnnestedData %>%
#   filter(str_detect(token, "θε(μεν|ναι)$") == T)
# # ὑπαρχέμεν for ὑπαρχέναι in DEG 450
# Data.nai.uparxeinai <- UnnestedData %>%
#   filter(str_detect(token, "υπαρχε(ναι|μεν)$") == T)
# 
# # poimen in DEG 462.A
# 
# # h / ei ----------------------------------------------------------------
# Data.h.mhnows <- UnnestedData %>%
#   filter(str_detect(token, "μ(η|ει)ν.{1,3}") == T)
# 
# # ti/si -----------------------------------------------------------------
# # τίθητι for τίθησι in DEG 154 et al.
# UnnestedData %>% filter(str_detect(token, "τιθη(τι|σι)") == T) %>% View()
# UnnestedData %>% filter(str_detect(token, "διδω(τι|σι)") == T) %>% View()
# UnnestedData %>% filter(str_detect(token, "δ(ει|ε|ι)κνυ") == T) %>% View()
# UnnestedData %>% filter(str_detect(token, "τι$") == T) %>% View()

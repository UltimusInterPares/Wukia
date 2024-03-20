### Test Run --------------------------------------------------------------

# TanTestMeg <-
#   PackHumUnnest %>% filter(str_detect(token,
#                                       "^(ταν|την|ταμ|τημ|ταγ|τηγ)$")
#                            == T) %>%
#   filter(str_detect(location,
#                     "(Athens|Megara)")
#          == T) %>%
#   droplevels()
# 
# TanMegXT <-
#   stats::xtabs(~ token + location,
#                data = TanTestMeg[TanTestMeg$analysis_cen == 200,])
# 
# prop.table(TanMegXT, 1)
# fisher.test(TanMegXT)
# 
# 
# count(PackHum, location)
# count(UnnestedData, location)
# ggplot(NestedData, aes(x = location)) +
#   geom_histogram(stat = "count")

# # OIKIA ...................................................................
# # ELEUSIS
# Data.oi.oikia.4 <- #  . . . . . . . . . . . . Filter for "oikia" in 4th cen
#   Data.oi.oikia[Data.oi.oikia$analysis_cen == 400, ]
# 
# Data.oi.oikia.4.Eleusis <- # . . . . . Filter for Athens and Eleusis tokens
#   Data.oi.oikia.4[Data.oi.oikia.4$location == "Athens" |
#                   Data.oi.oikia.4$location == "Eleusis", ] %>%
#   droplevels() # . . . . . . . . . . . . . . . . . . . . Drop unused levels
# 
# XT.oi.oikia.4.Eleusis <- # . . . . . . . . . . Eleusinian contingency table
#   stats::xtabs(~ data_type + location, 
#                data = Data.oi.oikia.4.Eleusis)
# 
# prop.table(XT.oi.oikia.4.Eleusis, 1) #  . . Eleusinian prop table (row = 1)
# fisher.test(XT.oi.oikia.4.Eleusis) # . . . . Eleusinean fisher's exact test
# 
# # MEGARA
# Data.oi.oikia.4.Megara <- # . . . . . . Filter for Athens and Megara tokens
#   Data.oi.oikia.4[Data.oi.oikia.4$location == "Athens" |
#                   Data.oi.oikia.4$location == "Megara", ] %>%
#   droplevels() # . . . . . . . . . . . . . . . . . . . . Drop unused levels
# 
# XT.oi.oikia.4.Megara <- #  . . . . . . . . . . . Megarian contingency table
#   stats::xtabs(~ data_type + location, 
#                data = Data.oi.oikia.4.Megara)
# 
# prop.table(XT.oi.oikia.4.Megara, 1) # . . . . Megarian prop table (row = 1)
# fisher.test(XT.oi.oikia.4.Megara) #  . . . . . Megarian fisher's exact test
# 
# # TANAGRA
# Data.oi.oikia.4.Tanagra <- # . . . . . Filter for Athens and Tanagra tokens
#   Data.oi.oikia.4[Data.oi.oikia.4$location == "Athens" |
#                   Data.oi.oikia.4$location == "Tanagra", ] %>%
#   droplevels() # . . . . . . . . . . . . . . . . . . . . Drop unused levels
# 
# XT.oi.oikia.4.Tanagra <- # . . . . . . . . . . . Tanagran contingency table
#   stats::xtabs(~ data_type + location, 
#                data = Data.oi.oikia.4.Tanagra)
# 
# prop.table(XT.oi.oikia.4.Tanagra, 1) #  . . . Tanagran prop table (row = 1)
# fisher.test(XT.oi.oikia.4.Tanagra) # . . . . . Tanagran fisher's exact test
# 
# # OROPOS
# Data.oi.oikia.4.Oropos <- # . . . . . . Filter for Athens and Oropos tokens
#   Data.oi.oikia.4[Data.oi.oikia.4$location == "Athens" |
#                   Data.oi.oikia.4$location == "Oropos", ] %>%
#   droplevels() # . . . . . . . . . . . . . . . . . . . . Drop unused levels
# 
# XT.oi.oikia.4.Oropos <- # . . . . . . . . . . . . Oropian contingency table
#   stats::xtabs(~ data_type + location, 
#                data = Data.oi.oikia.4.Oropos)
# 
# prop.table(XT.oi.oikia.4.Oropos, 1) #  . . . . Oropian prop table (row = 1)
# fisher.test(XT.oi.oikia.4.Oropos) # . . . . . . Oropian fisher's exact test
# 
# 
# # AUTOS ...................................................................
# Data.oi.autos.4 <- #  . . . . . . . . . . . . Filter for "autos" in 4th cen
#   Data.oi.autos[Data.oi.autos$analysis_cen == 400, ]
# 
# Data.oi.autos.4.Eleusis <- # . . . . . Filter for Athens and Eleusis tokens
#   Data.oi.autos[Data.oi.autos$location == "Athens" |
#                   Data.oi.autos$location == "Eleusis", ] %>%
#   droplevels() # . . . . . . . . . . . . . . . . . . . . Drop unused levels
# 
# 
# # DHMOS ...................................................................
# Data.oi.dhmos.4 <- Data.oi.dhmos[Data.oi.dhmos$analysis_cen == 400,]# dhmos

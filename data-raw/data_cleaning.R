# Load required packages
library(readxl)
library(dplyr)
library(janitor)
library(tibble)
library(usethis)
library(tidyr)
library(stringr)

# Read the raw sheet
raw_data <- read_excel("data-raw/sciadv.abm3624_table_data.xlsx", sheet = "Raw")

# Clean column names (snake_case)
raw_data <- raw_data %>%
  janitor::clean_names() %>%
  mutate(report_date = as.Date(report_date))

data_time <- raw_data %>%
  pivot_longer(
    cols = -report_date,
    names_to = c("state", "type"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    type = ifelse(is.na(type), "total", "breach"),
    state = str_to_upper(state)
  ) %>%
  pivot_wider(
    names_from = type,
    values_from = value
  ) %>%
  arrange(report_date, state)

usethis::use_data(data_time, overwrite = TRUE, compress = "xz")

# Create tibble for breach data
breaches_data <- tibble::tribble(
  ~no, ~state, ~date, ~facility, ~case_name, ~variant, ~onward, ~vax, ~link,
  1, "WA", as.Date("2020-04-14"), "Duxton Hotel", "Security Guard", "-", FALSE, NA, "https://ww2.health.wa.gov.au/Media-releases/2020/COVID19-update-15-April-2020",
  2, "WA", as.Date("2020-05-12"), "Pan Pacific Perth Hotel", "Contractor", "-", FALSE, NA, "https://ww2.health.wa.gov.au/Media-releases/2020/COVID19-update-12-May-2020",
  3, "VIC", as.Date("2020-05-25"), "Rydges Hotel", "Staff and Security Guards", "-", TRUE, NA, "https://www.parliament.vic.gov.au/file_uploads/0387_RC_Covid-19_Final_Report_Volume_2_v21_Digital_h1LPjbnZ.pdf",
  4, "VIC", as.Date("2020-06-14"), "Stamford Hotel", "Security Guard", "-", TRUE, NA, "https://www.parliament.vic.gov.au/file_uploads/0387_RC_Covid-19_Final_Report_Volume_2_v21_Digital_h1LPjbnZ.pdf",
  4, "VIC", as.Date("2020-06-15"), "Stamford Hotel", "Couple", "-", TRUE, NA, "https://www.parliament.vic.gov.au/file_uploads/0387_RC_Covid-19_Final_Report_Volume_2_v21_Digital_h1LPjbnZ.pdf",
  5, "NSW", as.Date("2020-08-17"), "Marriot Hotel", "Security Guard", "-", FALSE, NA, "https://www.health.nsw.gov.au/news/Pages/20200818_01.aspx",
  6, "NSW", as.Date("2020-08-20"), "Marriot Hotel", "Security Guard", "-", FALSE, NA, "https://www.health.nsw.gov.au/news/Pages/20200823_00.aspx",
  7, "SA", as.Date("2020-11-14"), "Peppers Hotel", "Parafield", "-", TRUE, NA, "https://www.abc.net.au/news/2020-11-25/explainer-what-we-know-about-south-australia-coronavirus-cluster/12918444",
  8, "NSW", as.Date("2020-12-04"), "Novotel Hotel", "HQ Cleaner", "USA", FALSE, NA, "https://www.health.nsw.gov.au/news/Pages/20201204_00.aspx",
  NA, "NSW", as.Date("2020-12-17"), "N/A Unknown", "Bus Driver", "USA", FALSE, NA, "https://www.health.nsw.gov.au/news/Pages/20201217_00.aspx",
  NA, "NSW", as.Date("2020-12-17"), "N/A Unknown", "Avalon", "USA", TRUE, NA, "https://www.health.nsw.gov.au/news/Pages/20201217_00.aspx",
  NA, "NSW", as.Date("2021-01-03"), "N/A Airport", "Patient Transport", "USA", TRUE, NA, "https://www.health.nsw.gov.au/news/Pages/20210103_00.aspx",
  9, "QLD", as.Date("2021-01-07"), "Grand Chancellor Hotel", "Cleaner", "Alpha", TRUE, NA, "https://www.facebook.com/QLDHealth/posts/3660028480771874",
  12, "WA", as.Date("2021-01-31"), "Four Points Sheraton Hotel", "Case 903", "Alpha", FALSE, NA, "https://ww2.health.wa.gov.au/Media-releases/2021/COVID-19-update-31-January-2021",
  10, "VIC", as.Date("2021-02-03"), "Park Royal Hotel", "Family", "Alpha", FALSE, NA, "https://www.dhhs.vic.gov.au/coronavirus-update-Victoria-3-February-2021",
  11, "VIC", as.Date("2021-02-05"), "Grand Hyatt Hotel", "Resident Support Officer", "Alpha", FALSE, NA, "https://www.dhhs.vic.gov.au/coronavirus-update-victoria-4-february-2021",
  13, "VIC", as.Date("2021-02-08"), "Holiday Inn Hotel", "Nebuliser", "Alpha", TRUE, NA, "https://www.dhhs.vic.gov.au/coronavirus-update-victoria-8-february-2021",
  14, "QLD", as.Date("2021-03-12"), "Princess Alexandria Hospital", "Doctor", "Alpha", TRUE, 0, "https://www.health.qld.gov.au/news-events/doh-media-releases/releases/covid-19-case-identified-at-pa-hospital2",
  15, "NSW", as.Date("2021-03-14"), "Sofitel Wentworth Hotel", "HQ Worker", "Alpha", FALSE, 2, "https://www.health.nsw.gov.au/news/Pages/20210314_00.aspx",
  14, "QLD", as.Date("2021-03-26"), "Princess Alexandria Hospital", "Nurse", "Alpha", TRUE, 0, "https://www.health.qld.gov.au/news-events/doh-media-releases/releases/210326-covid-19-update",
  17, "NSW", as.Date("2021-04-17"), "Adina Apartments Hotel", "Families", "Beta", FALSE, 0, "https://www.health.nsw.gov.au/news/Pages/20210417_00.aspx",
  16, "WA", as.Date("2021-04-21"), "Mercure Hotel", "Guests", "Alpha", TRUE, 0, "https://ww2.health.wa.gov.au/Media-releases/2021/COVID-19-update-21-April-2021-Hotel-quarantine-acquired-cases-confirmed",
  18, "NSW", as.Date("2021-04-21"), "Mercure Hotel", "Adjacent Room", "Beta", FALSE, 0, "https://www.health.nsw.gov.au/news/Pages/20210422_00.aspx",
  19, "WA", as.Date("2021-05-01"), "Pan Pacific Hotel", "Case 1001", "USA", TRUE, 1, "https://www.facebook.com/abcperth/videos/1659525380911538",
  NA, "NSW", as.Date("2021-05-05"), "Park Royal / SHA Unknown", "BBQ", "Delta", TRUE, 0, "https://www.health.nsw.gov.au/news/Pages/20210506_00.aspx",
  20, "SA", as.Date("2021-05-11"), "Playford Hotel", "Wollert Man", "Kappa", TRUE, 0, "https://twitter.com/SAHealth/status/1391981778357817346",
  21, "WA", as.Date("2021-05-31"), "Pan Pacific Hotel", "Adjoining Room", "B.1.1381", FALSE, 0, "https://www.facebook.com/abcperth/videos/389264902398378",
  NA, "VIC", as.Date("2021-06-02"), "Novotel Hotel", "Jervis Bay Family", "Delta", TRUE, 0, "https://www.abc.net.au/news/2021-06-08/melbourne-covid-outbreak-delta-strain-link-hotel-quarantine/100183468",
  NA, "NSW", as.Date("2021-06-15"), "Radisson Blu Hotel", "Adjacent Room", "Alpha", FALSE, 0, "https://twitter.com/NSWHealth/status/1404762991896141828",
  NA, "NSW", as.Date("2021-06-16"), "N/A Airport", "Limo Driver", "Delta", TRUE, 0, "https://twitter.com/NSWHealth/status/1405038475246596099",
  NA, "QLD", as.Date("2021-06-20"), "Four Points Hotel", "Flight Attendant", "Alpha", TRUE, 0, "https://www.health.qld.gov.au/news-events/doh-media-releases/releases/brisbanes-covid-19-community-case-update-variant-confirmed",
  NA, "QLD", as.Date("2021-06-23"), "Novotel Hotel", "Adjacent Room", "Delta", FALSE, 0, "https://twitter.com/AnnastaciaMP/status/1407476171088875522",
  NA, "QLD", as.Date("2021-06-26"), "Novotel Hotel", "NT Granite Mine", "Delta", TRUE, 0, "https://coronavirus.nt.gov.au/updates/items/2021-06-26-positive-covid-19-case-update",
  NA, "QLD", as.Date("2021-06-29"), "Prince Charles Hospital", "Receptionist", "Delta", TRUE, NA, "https://twitter.com/AnnastaciaMP/status/1409685159620714496",
  NA, "QLD", as.Date("2021-07-01"), "Brisbane Airport", "Luggage Handler", "Delta", TRUE, 0, "https://www.abc.net.au/news/2021-07-01/covid-qld-coronavirus-update-latest-cases-delta/100252538",
  NA, "NSW", as.Date("2021-07-15"), "HQ / Community Unknown", "12yo Boy", "Delta", TRUE, 0, "https://www.news.com.au/national/queensland/news/nsw-health-investigating-covid19-hotel-quarantine-transmission-after-brisbane-family-tests-positive/news-story/30205cf2020a675811de59156ae1218b",
  NA, "QLD", as.Date("2021-07-15"), "Brisbane Airport", "Airport Worker", "Delta", FALSE, 2, "https://twitter.com/AnnastaciaMP/status/1415470728367919104",
  NA, "QLD", as.Date("2021-07-26"), "Amora Hotel", "Adjacent Room", "Delta", FALSE, 0, "https://www.facebook.com/annastaciamp/posts/378031947015744",
  NA, "QLD", as.Date("2021-07-31"), "SCUH Hospital", "Medical Student", "Delta", TRUE, 0, "https://www.facebook.com/annastaciamp/videos/531067324881714"
)

usethis::use_data(breaches_data, overwrite = TRUE)

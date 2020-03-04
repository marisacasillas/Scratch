# rm(list=ls())
library(tidyverse)

strt.clock.ptrn <- ".*<Recording num=\"1\" startClockTime=\"([0-9-]+)T([0-9:]+)Z\" endClockTime=\"([0-9-]+)T([0-9:]+)Z\".*"

its.time.data.tbl <- tibble()

all.its.files <- list.files(".", "*.its$", recursive = TRUE)
for (its.file in all.its.files) {
  print(its.file)
  its.data <- read_lines(its.file)
  rec1.line <- which(grepl("Recording num=\"1\"", its.data))[1]
  rec.time.info <- stringr::str_match(
      its.data[rec1.line], strt.clock.ptrn)[,2:5]
  rec.time.tbl <- tibble(
    recording.filepath = its.file,
    rec.start.date = lubridate::date(rec.time.info[1]),
    rec.start.time = hms::as.hms(rec.time.info[2]),
    rec.stop.date = lubridate::date(rec.time.info[3]),
    rec.stop.time = hms::as.hms(rec.time.info[4])
    ) %>%
    mutate(
      recording.filename = stringr::str_extract(recording.filepath,
                                                "[a-zA-Z0-9-_]+\\.its"),
      starts.in.am = lubridate::am(rec.start.time)
    ) %>%
    select(recording.filepath, recording.filename,
           rec.start.date, rec.start.time, rec.stop.date, rec.stop.time,
           starts.in.am)
  its.time.data.tbl <- bind_rows(its.time.data.tbl, rec.time.tbl)
}

write_csv(its.time.data.tbl, "its_start_and_stop_times.csv")





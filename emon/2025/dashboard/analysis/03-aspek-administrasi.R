library(tidyverse)
library(readxl)
library(magrittr)

file_presensi <- "analysis/Rekap Presensi Satker 1756524561856.xlsx"

presensi <-
  file_presensi |>
  read_xlsx(skip = 6) |>
  janitor::clean_names() |>
  select(pegawai = nama, hd, ht, psw)

hitung_aspek_kehadiran_mei <- function(data_presensi) {
  
  y <- 
  data_presensi |>
    mutate(
      hd  = as.integer(hd),
      ht  = as.integer(ht),
      psw = as.integer(psw),
      pct_hadir = (1 - (ht + psw)  / hd) * 100
    ) |>
    mutate(teguran = 0) |>
    mutate(
      kehadiran = 
        case_when(
          pct_hadir == 100 ~ 6, 
          pct_hadir |> between(95.00, 99.99) ~ 5,
          pct_hadir |> between(90.00, 94.99) ~ 4,
          pct_hadir |> between(85.00, 89.99) ~ 3,
          pct_hadir |> between(80.00, 84.99) ~ 2,
          pct_hadir < 80.00 ~ 1,
          .default = NA
        ),
      kedisiplinan = case_when(
        teguran == 0 ~ 6,
        teguran == 1 ~ 1,
        teguran > 1 ~ 0,
        .default = NA
      )
    ) |>
    mutate(
      aspek_administrasi = (kehadiran * 0.6 + kedisiplinan * 0.4) * 0.2 #bobot 20%
    )
  
  return(y)
}


tbl_presensi <- hitung_aspek_kehadiran_mei(presensi) 

saveRDS(tbl_presensi, file = str_c(Sys.Date(), "_aspek_administrasi.rds"))

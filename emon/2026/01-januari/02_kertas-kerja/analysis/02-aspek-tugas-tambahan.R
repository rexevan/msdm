library(tidyverse)
library(readxl)
library(googlesheets4)

my_email <- "rex.evan96@gmail.com"
bulan <- "januari"

gs4_auth(email = my_email)

ss <- "https://docs.google.com/spreadsheets/d/1WTj1kB83yp4_jM7aTNuBV_0h4kFFiydDJzLqKq4T348/edit?usp=sharing"

tt_tbl <- read_sheet(ss, sheet = "tugas_tambahan_2026") |>
  select(pegawai = nama_pegawai, tugas_tambahan = !!bulan) |>
  mutate(tugas_tambahan = str_replace(tugas_tambahan, "-", NA_character_))

# tt_tbl <- read_xlsx("analysis/penilaian emon - kegiatan tambahan.xlsx", skip = 1, sheet = "juli") |> janitor::clean_names()

tt_tbl_hitung <- 
tt_tbl |> 
  mutate(
  test = str_split(tugas_tambahan, pattern = ","), 
  test2 = map_int(test, \(x) length(x)),
  tt_jml = if_else(tugas_tambahan |> is.na(), 0, test2)
) |>
  select(pegawai, tugas_tambahan, tt_jml) |>
  mutate(
  tt_jml_max = max(tt_jml), # jumlah tugas tambahan terbanyak untuk dijadikan patokan
  tt_pct = (tt_jml / tt_jml_max), # persentase terhadap tugas tambahan
  aspek_tugas_tambahan = tt_pct * (20/100) * 6 # bobot 20% terhadap keseluruhan indeks EMON. 
  #Karena indeks emon menggunakan skala 1 - 6, maka niali aspek ini juga paling tinggi ini adalah 6
)

saveRDS(tt_tbl_hitung, file = str_c(Sys.Date(), "_aspek_tambahan.rds"))

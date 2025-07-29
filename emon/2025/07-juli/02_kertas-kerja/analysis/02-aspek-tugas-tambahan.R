library(tidyverse)
library(readxl)


tt_tbl <- read_xlsx("analysis/penilaian emon Juni.xlsx", skip = 1) |> janitor::clean_names()

tt_tbl_hitung <- 
tt_tbl |> 
  mutate(
  test = str_split(tugas_tambahan, pattern = ";"), 
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

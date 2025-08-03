
## Pendahuluan ----------------------------------
library(tidyverse)
library(googlesheets4)
library(janitor)

my_email <- "rex.evan96@gmail.com"

gs4_auth(email = my_email)

link_response <- "https://docs.google.com/spreadsheets/d/1WTj1kB83yp4_jM7aTNuBV_0h4kFFiydDJzLqKq4T348/edit?usp=sharing"

penilaian_dimulai <- "2025-07-30 07:30:01"
penilaian_selesai <- "2025-08-02 00:00:00"


## Getting data -------------------------------
tbl_colnames  <- read_sheet(ss = link_response, sheet = "colnames")
tbl_exclude   <- read_sheet(ss = link_response, sheet = "exclude")
tbl_surat     <- read_sheet(ss = link_response, sheet = "surat")

tbl_response_all <- read_sheet(ss = link_response, sheet = "response") |>
  rename_with(~ tbl_colnames$new_name, all_of(tbl_colnames$old_name))

tbl_response  <-  
  tbl_response_all |> 
  anti_join(tbl_exclude, join_by(timestamp)) |>
  filter(timestamp > penilaian_dimulai) |>
  filter(timestamp <= penilaian_selesai)


## Pemeriksaan penilaian ganda --------------------------

## sesuai surat, tidak ada penilaian ganda

cek_ganda <- function(data) {
  
  y <- 
    data |>
    select(ketua_pmo, pegawai) |>
    group_by(ketua_pmo, pegawai) |>
    count() |>
    arrange(desc(n))  
  
  return(y)
}

cek_ganda(tbl_surat)
penilaian_ganda <- cek_ganda(tbl_response) |> filter(n > 1)

## Pemeriksaan kesesuaian -----------------------------

penilaian_sesuai <- 
  inner_join(
    x = tbl_response,
    y = tbl_surat,
    by = join_by(ketua_pmo, pegawai)
  )


penilaian_keliru <- 
  anti_join(
    x = tbl_response,
    y = penilaian_sesuai,
    by = join_by(ketua_pmo, pegawai)
  )


penilaian_belum <- 
  anti_join(
    x = tbl_surat,
    y = penilaian_sesuai,
    by = join_by(ketua_pmo, pegawai)
  )

## Penghitungan indeks EMON -----------------------------------

tbl_aspek_administrasi <- read_rds(str_c(Sys.Date(), "_aspek_administrasi.rds"))
tbl_aspek_tambahan <- read_rds(str_c(Sys.Date(), "_aspek_tambahan.rds"))

indeks_emon_2 <- 
penilaian_sesuai |>
  select(pegawai, mutu:kolaboratif) |>
  summarise(
    across(mutu:kolaboratif, \(x) mean(x, na.rm = TRUE)),
    .by = pegawai
  ) |>
  mutate(
    mutu          = mutu * (10/100), # bobot 10%
    produktivitas = produktivitas * (10/100), # bobot 10%
    pengetahuan   = pengetahuan * (10/100), # bobot 10%
    kehandalan    = kehandalan * (10/100), # bobot 10%
    waktu         = waktu * (30/100), # bobot 30%
    inisiatif     = inisiatif * (10/100), # bobot 10%
    kerjasama     = kerjasama * (20/100) # bobot 20%  
  ) |>
  mutate(
    ber_pelayanan = ber_pelayanan * (10/100), # bobot 10% 
    akuntabel     = akuntabel * (10/100), # bobot 10% 
    kompeten      = kompeten * (10/100), # bobot 10% 
    harmonis      = harmonis * (10/100), # bobot 10% 
    loyal         = loyal * (10/100), # bobot 10% 
    adaptif       = adaptif * (30/100), # bobot 30% 
    kolaboratif   = kolaboratif * (20/100) # bobot 20% 
  ) |>
  rowwise() |>
  mutate(
    aspek_kinerja = sum(c_across(mutu:kerjasama), na.rm = TRUE) * (30/100),
    aspek_berAKHLAK = sum(c_across(ber_pelayanan:kolaboratif), na.rm = TRUE) * (30/100)
  ) |>
  ungroup() |>
  left_join(tbl_aspek_administrasi, by = join_by(pegawai)) |>
  left_join(tbl_aspek_tambahan, by = join_by(pegawai)) |>
  mutate(indeks_emon = aspek_administrasi + aspek_kinerja + aspek_tugas_tambahan + aspek_berAKHLAK) |>
  select(pegawai, starts_with("aspek_"), indeks_emon)


## Save ------

save.image(str_c(Sys.Date(), ".RData"))





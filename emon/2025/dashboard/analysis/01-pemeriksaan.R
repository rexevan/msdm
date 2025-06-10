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
    y = tbl_sesuai,
    by = join_by(ketua_pmo, pegawai)
  )


penilaian_belum <- 
  anti_join(
    x = tbl_surat,
    y = tbl_sesuai,
    by = join_by(ketua_pmo, pegawai)
  )

save.image(file = "Penilaian.RData")









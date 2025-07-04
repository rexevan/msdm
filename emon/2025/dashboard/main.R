# Running ini aja lah


# Pertama, running getting data sudah kita bisa dapat data terbaru dari Google Sheet


source("analysis/02-aspek-tugas-tambahan.R")
source("analysis/03-aspek-administrasi.R")
source("analysis/04-aspek-kinerja-berakhlak.R")

# Kedua, run Dashboard untuk update Dashboardnya
quarto::quarto_render("emon_dashboard.qmd")

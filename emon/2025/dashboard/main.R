# Running ini aja lah


# Pertama, running getting data sudah kita bisa dapat data terbaru dari Google Sheet

source("analysis/00-getting-data.R")

# Kedua, run Dashboard untuk update Dashboardnya
quarto::quarto_render("emon_dashboard.qmd")

library(readxl)

# parameters om in te stellen
v_file_input_bakkers <- 'demo_input_bakkers.xlsx'
v_startdatum <- as.Date("2020-07-02")
v_aantal_weken = 20

# Lees lijst met bakkers in
herenbakkers <- read_excel(paste("Input",v_file_input_bakkers, sep = '/'))


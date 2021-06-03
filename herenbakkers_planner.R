# Geheugen leeg maken
rm(list=ls())

# Benodigde packages
library(readxl)

# parameters om in te stellen
v_file_input_bakkers         = 'demo_input_bakkers.xlsx'
v_file_output_planning       = 'herenbakkers_planning.xlsx'
v_startdatum                 = as.Date("2021-07-02")
v_aantal_weken               = 20
v_aantal_bakkers_op_vrijdag  = 1
v_aantal_bakkers_op_zaterdag = 2

# Lees lijst met bakkers in
df_herenbakkers <- read_excel(paste("Input",v_file_input_bakkers, sep = '/'))
df_herenbakkers$atl_keer_ingepland = 0

# Genereer een dataframe met alle vrijdagen en zaterdag in de komende x weken
v_einddatum = v_startdatum + 7 * v_aantal_weken
datums_in_range <- seq(v_startdatum, v_einddatum, by = 'day')
df_planning <- data.frame(datum = datums_in_range,
                          weekdag = weekdays(datums_in_range))
df_planning <- df_planning[df_planning$weekdag %in% c("vrijdag", "zaterdag"),]
df_planning$bakkers <- ''

# Plan bakkers in

for (i in 1:nrow(df_planning))
     {
       if (df_planning[i,'weekdag'] == 'vrijdag')
       {
         df_vrijdag <- df_herenbakkers[df_herenbakkers$`Inplannen op vrijdag` == 'JA', ]
         df_vrijdag$rank <- rank(order(order(df_vrijdag$atl_keer_ingepland, df_vrijdag$Naam)))
         df_vrijdag <- df_vrijdag[df_vrijdag$rank <= v_aantal_bakkers_op_vrijdag,]
         df_planning[i, 'bakkers'] <- paste(df_vrijdag$Naam, collapse = ",")
         df_herenbakkers[df_herenbakkers$Naam %in% df_vrijdag$Naam, 'atl_keer_ingepland'] <- df_herenbakkers[df_herenbakkers$Naam %in% df_vrijdag$Naam, 'atl_keer_ingepland'] + 1
       }
  
      if (df_planning[i,'weekdag'] == 'zaterdag')
      {
        df_zaterdag <- df_herenbakkers[df_herenbakkers$`Inplannen op Zaterdag` == 'JA', ]
        df_zaterdag$rank <- rank(order(order(df_zaterdag$atl_keer_ingepland, df_zaterdag$Naam)))
        df_zaterdag <- df_zaterdag[df_zaterdag$rank <= v_aantal_bakkers_op_zaterdag,]
        df_planning[i, 'bakkers'] <- paste(df_zaterdag$Naam, collapse = ",")
        df_herenbakkers[df_herenbakkers$Naam %in% df_zaterdag$Naam, 'atl_keer_ingepland'] <- df_herenbakkers[df_herenbakkers$Naam %in% df_zaterdag$Naam, 'atl_keer_ingepland'] + 1
      }
  }


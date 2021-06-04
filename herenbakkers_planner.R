# Geheugen leeg maken
rm(list=ls())

# Benodigde packages
library(openxlsx)

# parameters om in te stellen voor het runnen van je script
# De lijst met bakkers wordt als Excel bestand ingelezen en de output als een Excel bestand weggeschreven
# Je dient aan te geven hoeveel weken je dient in te plannen en vanaf welke datum
# Daarnaast dien je aan te geven hoeveel bakkers ingepland dienen te worden op vrijdag en zaterdag
v_file_input_bakkers        = 'demo_input_bakkers.xlsx'
v_file_output_planning       = 'herenbakkers_planning.xlsx'
v_startdatum                 = as.Date("2021-07-02")
v_aantal_weken               = 24
v_aantal_bakkers_op_vrijdag  = 1
v_aantal_bakkers_op_zaterdag = 2

# Lees lijst met bakkers in
df_bakkers <- openxlsx::read.xlsx(paste("Input",v_file_input_bakkers, sep = '/'))
# We gaan tellen hoevaak een bakker is ingepland, dus de teller begint op 0
df_bakkers$atl_keer_ingepland = 0

# Genereer een dataframe met alle vrijdagen en zaterdag in de komende x weken
# Daarnaast willen we weten of de dagen een vrijdag/zaterdag zijn en of het een even/oneven week zijn.
# Niet alle bakkeres zijn namelijk alle dagen beschikbaar.
v_einddatum = v_startdatum + 7 * v_aantal_weken
datums_in_range <- seq(v_startdatum, v_einddatum, by = 'day')
df_planning <- data.frame(datum = datums_in_range,
                          weekdag = weekdays(datums_in_range))
df_planning <- df_planning[df_planning$weekdag %in% c("vrijdag", "zaterdag"),]
df_planning$weeknummer <- as.numeric(format(as.Date(df_planning$datum), "%W"))
df_planning$week_even_oneven <- "oneven"
df_planning[(df_planning$weeknummer %% 2 == 0), 'week_even_oneven'] <- 'even'


# Plan bakkers in; met behulp van een foor-loop gaan we alle datums in de planning doorlopen.
# Binnen elke dag selecteren we de bescchikbare bakkers en kijken we welke bakkers aan de beurt zijn.

for (i in 1:nrow(df_planning))
     {
       # Binnen de loop-iteratie bepaal of het een vrijdag/zaterdag is, een even/oneven week en hoeveel bakkers we
       # nodig hebben.
       v_dag <- df_planning[i,'weekdag']
       v_week_even_oneven <- df_planning[i,'week_even_oneven']
       if(v_dag == 'vrijdag') {v_atl_bakkers <- v_aantal_bakkers_op_vrijdag}
       if(v_dag == 'zaterdag') {v_atl_bakkers <- v_aantal_bakkers_op_zaterdag}
       
       # Afhankelijk van of het vrijdag/zaterdag is en of het een even/oneven week is, selecteer
       # een subset van de bakkers die beschikbaar zijn om te bakken.
       if (v_dag == 'vrijdag' && v_week_even_oneven == 'oneven') { df_bakkers_subset <- df_bakkers[(df_bakkers$Vrijdag == "JA") & (df_bakkers$OnevenWeken == "JA") , ]}
       if (v_dag == 'vrijdag' && v_week_even_oneven == 'even') { df_bakkers_subset <- df_bakkers[(df_bakkers$Vrijdag == "JA") & (df_bakkers$EvenWeken == "JA") , ]}
       if (v_dag == 'zaterdag' && v_week_even_oneven == 'oneven') { df_bakkers_subset <- df_bakkers[(df_bakkers$Zaterdag == "JA") & (df_bakkers$OnevenWeken == "JA") , ]}
       if (v_dag == 'zaterdag' && v_week_even_oneven == 'even') { df_bakkers_subset <- df_bakkers[(df_bakkers$Zaterdag == "JA") & (df_bakkers$EvenWeken == "JA") , ]}
 
       # Binnen de selecteerde subset van bakkers bepaal je wie het meest aan de beurt is om te bakken.
       # Dit doe je door te ranken op het aantal keer dat iemand al heeft gebakt in de te plannen periode.
       # Als dat gelijkt is doen we het op alfabetische volgorde van naam.
       # Rank 1 is al eerste aan de beurt, rank 2 daarna, etc.
       df_bakkers_subset$rank <- rank <- rank(order(order(df_bakkers_subset$atl_keer_ingepland, df_bakkers_subset$Naam)))
       
       # Selecteer alleen het aantal bakkers dat je nodig hebt
       df_bakkers_subset <- df_bakkers_subset[df_bakkers_subset$rank <= v_atl_bakkers,]
       
       # Schrijf de bakker of bakkers weg in de planning die aan de beurt zijn
       df_planning[i, 'bakkers'] <- paste(df_bakkers_subset$Naam, collapse = ",")
       
       # Verhoog het aantal bakbeurten met 1 bij de bakkers die je ingepland hebt.
       df_bakkers[df_bakkers$Naam %in% df_bakkers_subset$Naam, 'atl_keer_ingepland'] <- df_bakkers[df_bakkers$Naam %in% df_bakkers_subset$Naam, 'atl_keer_ingepland'] + 1
     }

#schrijf de planning weg in Excel met een timestamp.
filename <- paste(format(Sys.time(), "%Y-%m-%d_%H%M%S"), v_file_output_planning, sep = "_")
filename <- paste(paste("Output",filename, sep = '/'))
openxlsx::write.xlsx(df_planning, filename)
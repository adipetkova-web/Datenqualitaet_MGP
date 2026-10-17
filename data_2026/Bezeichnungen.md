```
Erklärung Messgrößen HuMVe-Logger

CR1000_HuMVe_Wind.dat

"TIMESTAMP" Zeitstempel JJJJ-MM-DD hh-mm-ss (UTC+0!?)
"RECORD" fortlaufende Nummer der individuellen Messung
"Wind_Speed", Windgeschwindigkeit in m/s
"Wind_Direction", Windrichtung in °
"Std_Dev_Wind_Dir" Standardabweichung im Zeitraum der Mittelung durch den Logger in m/s


CR1000_HuMVe_Meteo.dat

"TIMESTAMP" Zeitstempel JJJJ-MM-DD hh-mm-ss (UTC+0!?)
"RECORD" fortlaufende Nummer der individuellen Messung
"Temp_t_Avg" vom Logger gemittelte Temperatur in °C
"RH_t_Avg" vom Logger gemittelte Relative Luftfeuchte in %
"NetRad_Avg" Nettostrahlungsbilanz in W/m²
"ShortIn_Avg" Globalstrahlung in W/m²
"ShortOut_Avg",Kurzwellige Refelexstrahlung in W/m³
"IRTS_T_Avg" Oberflächentemüeratur in °C
"IRTS_B_Avg" Gehäusetemüperatur in °C
"IRTS_T_Corr_Avg" aus "IRTS_T_Avg" und "IRTS_B_Avg" korrigierte Oberflächentemperatur

CR1000_HuMVe_Gill_Windmaster.dat

"TIMESTAMP" Zeitstempel JJJJ-MM-DD hh-mm-ss (UTC+0!?)
"RECORD" fortlaufende Nummer der individuellen Messung
"met_u_Avg" u Windvector (meteorologische Konvention)
"met_v_Avg",v Windvector (meteorologische Konvention)
"Sonic_u_Avg" u Windvector (mathematische Konvention - gemittelt)
"Sonic_v_Avg" v Windvector (mathematische Konvention - gemittelt)
"Sonic_w_Avg" w Windvector (mathematische Konvention - gemittelt)
"Sonic_t_Avg" akutsiche temperatur - gemittelt
"Sonic_u_Cov(1)" Kovarianzen der Windvectoren (nicht relevant fuer < 30 min)
"Sonic_u_Cov(2)" Kovarianzen der Windvectoren (nicht relevant fuer < 30 min)
"Sonic_u_Cov(3)" Kovarianzen der Windvectoren (nicht relevant fuer < 30 min)
"Sonic_u_Cov(4)" Kovarianzen der Windvectoren (nicht relevant fuer < 30 min)
"Sonic_u_Cov(5)" Kovarianzen der Windvectoren (nicht relevant fuer < 30 min)
"Sonic_u_Cov(6)" Kovarianzen der Windvectoren (nicht relevant fuer < 30 min)
"Sonic_u_Cov(7)" Kovarianzen der Windvectoren (nicht relevant fuer < 30 min)
"Sonic_u_Cov(8)" Kovarianzen der Windvectoren (nicht relevant fuer < 30 min)
"Sonic_u_Cov(9)" Kovarianzen der Windvectoren (nicht relevant fuer < 30 min)
"Sonic_u_Cov(10)" Kovarianzen der Windvectoren (nicht relevant fuer < 30 min)
"Samples_Sonic"  zur Mittelung verwendete Anzahl der Messungen (Rohdaten)
"disable_flag" Kontrollflag

CR1000_HuMVe_Control.dat

"TIMESTAMP" Zeitstempel JJJJ-MM-DD hh-mm-ss (UTC+0!?)
"RECORD" fortlaufende Nummer der individuellen Messung
"Bat12V_Avg" Batteriespannung in V
"CaseT_Avg" Gehäusetemperatur im Loggerkasten in °C
```
cd "/Users/dawnstaana/Documents/NUS/Year 4/Sem 2/EC4304/Data"

import delimited "/Users/dawnstaana/Documents/NUS/Year 4/Sem 2/EC4304/Data/final2.csv", encoding(ISO-8859-1)

gen t = _n
tsset t

ac dji_pctchg
pac dji_pctchg
corrgram dji_pctchg

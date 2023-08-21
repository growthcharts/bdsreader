recode_edu <- function(x) {
  case_match(x,
             0 ~ "Onbekend",
             1 ~ "Geen",
             2 ~ "Basis",
             3 ~ "VMBO-LWOO/Praktijkonderwijs",
             4 ~ "VMBO-BBL&KBL",
             5 ~ "MAVO/VMBO-GL&TL",
             6 ~ "MBO",
             7 ~ "HAVO/VWO",
             8 ~ "HBO/HTS/HEAO",
             9 ~ "WO",
             98 ~ "Anders")
}

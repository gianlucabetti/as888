000100100210       // -?Parametri x ubLeg3C = Reperimento legami in tab. "3C"?
000200100210     d ubLeg3C_FlgPF   s              1a   inz
000300100210     d ubLeg3C_Padre   s              7p 0 inz
000400100210       // -?Codici clienti della stessa famiglia in tab. "3C"?
000500100210     d ubLeg3C_SKC     ds          3200    inz
000600100210     d  sch_SKC                       7p 0 dim(800)  inz
000700100210       // -?Relativi flags relativi all'abilitazione "EasyWEB"?
000800100210     d ubLeg3C_SKEW    ds           800    inz
000900100210     d  sch_SKEW                       n   dim(800)  inz

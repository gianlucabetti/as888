000100121207       // -?Parametri x ubChkABL = Verifica "blocco" clienti?
000200121207       // -?Codici clienti di cui verificare l'eventuale blocco?
000300121207     d ubChkABL_Ksc    ds          3200    inz
000400121207     d   sk_KSC                       7p 0 dim(800)  inz
000500121207       // -?Relativi flags relativi al blocco?
000600121207     d ubChkABL_Abl    ds           800    inz
000700121210     d   sk_ABL                       1a   dim(800)  inz

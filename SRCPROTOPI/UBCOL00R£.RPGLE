000100160223     d c_Dim           c                   const(200)
000200160223       // - Parametri ricevuti:
000300160223     d UBCOLOpz        s              3a
000400160223     d UBCOLFlgOpe     s             20a
000500160223       // tipo lancio: 'C'=LR   *blank=RT
000600160223     d UBCOLTLa        s              1a
000700160223     d UBCOLAAS        s              4s 0
000800160223     d UBCOLLNP        s              3s 0
000900160223     d UBCOLNRS        s              2s 0
001000160223     d UBCOLNSP        s              7s 0
001100160223     d UBCOLNCL        s              5s 0
001200160223       // tipo consegna: TT=Totale  PP=Parziale  TP=Consegnata con parziale  '  '=nessuna consegna
001300160223     d UBCOLTIP        s              2a
001400160223     d UBCOLDCM        s              8s 0
001500160223     d UBCOLHMC        s              4s 0
001600160223     d UBCOLSkBRT      s             18a
001700160223     d                                     dim(c_Dim)
001800160223     d UBCOLSkCLI      s             35a
001900160223     d                                     dim(c_Dim)
002000160223     d UBCOLSkDCM      s              8s 0
002100160223     d                                     dim(c_Dim)
002200160223     d UBCOLSkHMC      s              4s 0
002300160223     d                                     dim(c_Dim)
002400160223     d UBCOLSkDFS      s              8s 0
002500160223     d                                     dim(c_Dim)
002600160223     d UBCOLSkHMS      s              6s 0
002700160223     d                                     dim(c_Dim)
002800160223     d UBCOLSkVUC      s              7s 6
002900160223     d                                     dim(c_Dim)
003000160223     d UBCOLSkPUC      s              7s 3
003100160223     d                                     dim(c_Dim)
003200160223       // esito: 0=OK  1=Err input  2=KO
003300160223     d UBCOLErr        s              1s 0

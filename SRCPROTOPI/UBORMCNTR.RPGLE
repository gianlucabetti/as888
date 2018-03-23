000100160817       // - Parametri in entrata:
000200160817       // opz: 1=elabora  C=chiudi
000300160817     d pIn_Opz         s              1a
000400160817     d pIn_DS_FNVAO  e ds                  extname(FNVAOEIIR:FNVAO000)
000500160817     d                                     qualified
000600160817       // - Parametri in uscita:
000700160817     d pOut_Count      s              3s 0
000800160817       // esito: 1=OK  E=Err input  2=KO
000900160817     d pOut_Esito      s              1a

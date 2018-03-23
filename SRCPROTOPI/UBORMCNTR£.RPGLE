000100100203       //--------------------------------------------------------------
000200160817       //?Procedure name: UBORMCNTR                                   ?
000300160817       // Purpose:        Driver ctrl/conteggio ORM
000400160817       // Returns:        nothing
000500160817       // Parameter:      pIn_Opz   => opzioni: 1=?elabora  C=chiudi
000600160817       //                 pIn_DS_FNVAO  => DS FNVAOEIIR
000700160817       //                 pOut_Count  => conteggio
000800160817       //                 pOut_Esito  => esito: 1=OK  E=Err input  2=KO
000900100203       //--------------------------------------------------------------
001000160817     d UBORMCNTR       pr
001100160817     d pIn_Opz                        1a
001200160817     d pIn_DS_FNVAO                        likefile(FNVAOEIIR)
001300160817     d pOut_Count                     3s 0
001400160817     d pOut_Esito                     1a

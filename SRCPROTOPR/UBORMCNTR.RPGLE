000100100203       //--------------------------------------------------------------
000200160817       //?Procedure name: UBORMCNTR                                   ?
000300160817       // Purpose:        Driver ctrl/conteggio ORM
000400160817       // Returns:        nothing
000500160817       // Parameter:      In_Opz   => opzioni: 1=elabora  C=chiudi
000600160817       //                 In_DS_FNVAO  => DS FNVAOEIIR
000700160817       //                 Out_Count  => conteggio
000800160817       //                 Out_Esito  => esito: 1=OK  E=Err input  2=KO
000900100203       //--------------------------------------------------------------
001000160818       // perché il pgm che usa questo prototipo si compili c'è bisogno che sia caricato anche
001100160818       // /COPY GAITRASRC/SRCPROTOPI,UBORMCNTR
001200160818       // per avere la definizione della DS:
001300160817       // d pIn_DS_FNVAO  e ds                  extname(FNVAOEIIR:FNVAO000)
001400160817       // d                                     qualified
001500160817       //--------------------------------------------------------------
001600160817     d UBORMCNTR       pr
001700160817     d In_Opz                         1a
001800160817     d In_DS_FNVAO                         likeds(pIn_DS_FNVAO)
001900160817     d Out_Count                      3s 0
002000160817     d Out_Esito                      1a

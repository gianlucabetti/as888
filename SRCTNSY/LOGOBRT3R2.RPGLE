000100090423      /free
000200090423       //--------------------------------------------------------------
000300110331       //?Posizionamento del simil-logo BRT                            ?
000400090423       //--------------------------------------------------------------
000500090423       //?Richiede:                                                    ?
000600090424       //?· il free-format ("/free") DISattivato nel sorgente;         ?
000700090423       //?· la posizione dello spigolo di partenza (in alto a sinistra)?
000800090423       //?  già impostata;                                             ?
000900090423       //?Esempio:                                                     ?
001000090423       //?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?
001100110331       //?  // - Simil-Logo BRT                                        ?
001200110331       //?  §P2blS  = §Box1As;                                         ?
001300110331       //?  §P2blVS = 7.628;                                           ?
001400110331       //?  exsr  sr_SimilLogoBRT;                                     ?
001500090424       //?  ...                                                        ?
001600090424       //? /end-free                                                   ?
001700110331       //? /copy gaitrasrc/srcTNSY LogoBRT_R2                          ?
001800090424       //? /free                                                       ?
001900090424       //?  ...                                                        ?
002000090423       //--------------------------------------------------------------
002100110331       BEGSR  sr_SimilLogoBRT;
002200090423
002300110331         // *?Dati già impostati a programma?
002400110331         // -?Posiz. verticale linea orizzontale superiore del box?
002500110331         //§P2blS = §PlblS;
002600110331         // -?Posiz. orizzontale linea verticale sinistra nel box?
002700110401         //§P2blVS = §PlblVD + 0.050;
002800110331
002900110330         // -?Posiz. verticale linea orizzontale inferiore del box?
003000110401         §P2blI = §P2blS + 0.267 - 0.053;
003100110330         // -?Posiz. orizzontale linea verticale destra nel box?
003200110401         //§P2blVD = §P2blVS + 0.400;                ?[calcolata DOPO]?
003300110331         // -?Posiz. verticale "CORRIERE" ed "ESPRESSO"?
003400110401         §P2CC  = §P2blI + 0.053;
003500110401         §P2CE  = §P2CC  + 0.053;
003600110330         // -?Posiz. verticale ultima linea orizzontale (spessa)?
003700110401         §P2LOI = §P2CE  + 0.014;
003800110401         //§P2LOI = §PLOI;
003900110330         // -?Lunghezza ultima linea orizzontale (spessa)?
004000110331         //§P2LUL  = §P2blVD - §P2blVS;              ?[calcolata DOPO]?
004100110330         // -?Spessore linee?
004200090423         §PSPE  = c_Spessore;
004300090423
004400110331         // -?Posiz. verticali "BRT"?
004500110401         §P2bVa = §P2blS + 0.050;
004600110331         §P2bVb = §P2bVa + c_DeltaSpig;
004700110331         §P2bVc = §P2bVa + c_LunghMaxV - c_DeltaSpig - c_LunghV;
004800110331         §P2bOa = §P2bVa;
004900110331         §P2bOb = §P2bVa + ((c_LunghMaxV - c_Spessore) / 2) - 0.001;
005000110331         §P2bOc = §P2bVa + c_LunghMaxV - c_Spessore;
005100090423
005200110401         // *?Dati già impostati per il logo "BARTOLINI"?
005300110331         // -?Lunghezza linee verticali lunghe in "B" e "R"?
005400110331         //§PLU1  = c_LunghMaxV;
005500110330         // -?Lunghezza linee verticali corte in "B" e "R"?
005600110331         //§PLU2  = c_LunghV;
005700110330         // -?Lunghezza linee orizzontali in "B" e "R"?
005800110331         //§PLU3  = c_LunghO;
005900110331         // -?Lunghezza linee verticali in "T"?
006000110331         //§PLU4  = c_LunghMaxV - c_DeltaSpig;
006100110330         // -?Lunghezza 2ª linea verticale in "R" (inferiore)?
006200110331         //§PLU7  = c_LunghV + c_DeltaSpig;
006300110330         // -?Lunghezza linea orizzontale in "T"?
006400110401         //§PLU8  = c_Spessore + (0.017 * 2);
006500090423
006600110330         // -?Posiz. orizzontali "B"?
006700110401         §P2bl1a = §P2blVS + c_BOXspace + c_XspaceX + c_DeltaSpig;
006800110331         §P2bl1b = §P2bl1a + c_Spessore + c_LunghO -
006900090423                            (c_Spessore - c_DeltaSpig);
007000110331         §P2bl1C = §P2bl1a + c_Spessore;
007100110330         // -?Posiz. orizzontali "R"?
007200110331         §P2bl3a = §P2bl1b + c_Spessore + c_XspaceX;
007300110331         §P2bl3b = §P2bl3a + c_Spessore + c_LunghO -
007400090423                            (c_Spessore - c_DeltaSpig);
007500110331         §P2bl3c = §P2bl3a + c_Spessore;
007600110330         // -?Posiz. orizzontali "T"?
007700110331         §P2bl4a = §P2bl3b + c_Spessore + c_XspaceX;
007800110401         §P2bl4b = §P2bl4a + ((0.057 - c_Spessore) / 2);
007900110401
008000110401         // -?Posiz. orizzontali "CORRIERE"?
008100110401         §P2blV2 = §P2blVS + c_DeltaSpig;
008200090423
008300110330         // -?Posiz. orizzontale linea verticale destra nel box?
008400110401         §P2blVD = §P2bl4a + §Plu3 + c_Spessore + c_BOXspace
008500110401                                   + c_DeltaSpig + c_XspaceX;
008600110330         // -?Lunghezza ultima linea orizzontale (spessa)?
008700110331         §P2LUL  = §P2blVD - §P2blVS;
008800090423
008900090423       ENDSR;
009000090423      /end-free

000100090423      /free
000200090423       //--------------------------------------------------------------
000300090423       //?Posizionamento del simil-logo Bartolini                      ?
000400090423       //--------------------------------------------------------------
000500090423       //?Richiede:                                                    ?
000600090424       //?· il free-format ("/free") DISattivato nel sorgente;         ?
000700090423       //?· la posizione dello spigolo di partenza (in alto a sinistra)?
000800090423       //?  già impostata;                                             ?
000900090423       //?Esempio:                                                     ?
001000090423       //?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?
001100090423       //?  // - Simil-Logo Bartolini                                  ?
001200090423       //?  §PlblS  = §Box1As;                                         ?
001300090423       //?  §PlblVS = 7.128;                                           ?
001400090423       //?  exsr  sr_SimilLogoBART;                                    ?
001500090424       //?  ...                                                        ?
001600090424       //? /end-free                                                   ?
001700090424       //? /copy gaitrasrc/srcTNSY LogoBAR_R2                          ?
001800090424       //? /free                                                       ?
001900090424       //?  ...                                                        ?
002000090423       //--------------------------------------------------------------
002100110414       BEGSR  sr_SimilLogoBAR;
002200090423
002300110330         // -?Posiz. verticale linea orizzontale inferiore del box?
002400110331         §PlblI = §PlblS + 0.267;
002500110330         // -?Posiz. orizzontale linea verticale destra nel box?
002600110331         //§PlblVD = §PlblVS + 0.580;                ?[calcolata DOPO]?
002700110330         // -?Posiz. verticale "CORRIERE ESPRESSO"?
002800110331         §PCCE  = §PlblI + 0.053;
002900110330         // -?Posiz. verticale ultima linea orizzontale (spessa)?
003000110331         §PLOI  = §PlblI + 0.067;
003100110330         // -?Lunghezza ultima linea orizzontale (spessa)?
003200110330         //§PLUL  = §PlblVD - §PlblVS;               ?[calcolata DOPO]?
003300110330         // -?Spessore linee?
003400090423         §PSPE  = c_Spessore;
003500090423
003600110330         // -?Posiz. verticali "BARTOLINI"?
003700110331         §PlbVa = §PlblS + 0.073;
003800090423         §PlbVb = §PlbVa + c_DeltaSpig;
003900090423         §PlbVc = §PlbVa + c_LunghMaxV - c_DeltaSpig - c_LunghV;
004000090423         §PlbOa = §PlbVa;
004100090423         §PlbOb = §PlbVa + ((c_LunghMaxV - c_Spessore) / 2) - 0.001;
004200090423         §PlbOc = §PlbVa + c_LunghMaxV - c_Spessore;
004300090423
004400110330         // - -?Posiz. verticali parte interna di "N" in "BARTOLINI"?
004500110413         §PlbOd = §PlbVa + c_DeltaSpig;
004600110413         //§PlbOd = §PlbVa;
004700090423         §PlbOe = §PlbOd + c_DeltaSpig;
004800090423         §PlbOf = §PlbOe + c_DeltaSpig;
004900090423         §PlbOg = §PlbOf + c_DeltaSpig;
005000090423         §PlbOh = §PlbOg + c_DeltaSpig;
005100090423
005200110330         // -?Lunghezza linee verticali lunghe in "B","R","L","I","N"?
005300090423         §PLU1  = c_LunghMaxV;
005400110330         // -?Lunghezza linee verticali corte in "B" e "R"?
005500090423         §PLU2  = c_LunghV;
005600110330         // -?Lunghezza linee orizzontali in "B" e "R"?
005700090423         §PLU3  = c_LunghO;
005800110330         // -?Lunghezza linee verticali in "A" e "T"?
005900090423         §PLU4  = c_LunghMaxV - c_DeltaSpig;
006000110330         // -?Lunghezza linea orizzontale superiore in "A" e "O"?
006100090423         §PLU5  = c_LunghO + c_Spessore - c_DeltaSpig;
006200110330         // -?Lunghezza linee verticali in "O"?
006300090423         §PLU6  = c_LunghMaxV - (c_DeltaSpig * 2);
006400110330         // -?Lunghezza 2ª linea verticale in "R" (inferiore)?
006500090423         §PLU7  = c_LunghV + c_DeltaSpig;
006600110330         // -?Lunghezza linea orizzontale in "T"?
006700110331         §PLU8  = c_Spessore + (0.017 * 2);
006800110330         // -?Lunghezza linea orizzontale in "L"?
006900090423         §PLU9  = c_Spessore;
007000110330         // -?Altezza "linea obliqua" in "N"?
007100110331         //§PLU0  = 0.043;                           ?[calcolata DOPO]?
007200110330         // -?Lunghezza 2ª linea orizzontale in "A" (centrale)?
007300110330         //§PLUA  = §Plbl2B - §Plbl2A - c_Spessore;  ?[calcolata DOPO]?
007400090423
007500110330         // -?Posiz. orizzontali "B"?
007600090424         §Plbl1a = §PlblVS + c_BOXspace;
007700090423         §Plbl1b = §Plbl1a + c_Spessore + c_LunghO -
007800090423                            (c_Spessore - c_DeltaSpig);
007900090423         §Plbl1C = §Plbl1a + c_Spessore;
008000110330         // -?Posiz. orizzontali "A"?
008100090423         §Plbl2a = §Plbl1b + c_Spessore + c_XspaceX;
008200090423         §Plbl2b = §Plbl2a + c_Spessore + c_LunghO -
008300090423                            (c_Spessore - c_DeltaSpig);
008400090423         §Plbl2c = §Plbl2a + c_DeltaSpig;
008500090423         §Plbl2d = §Plbl2a + c_Spessore;
008600110330         // -?Posiz. orizzontali "R"?
008700090423         §Plbl3a = §Plbl2b + c_Spessore + c_XspaceX;
008800090423         §Plbl3b = §Plbl3a + c_Spessore + c_LunghO -
008900090423                            (c_Spessore - c_DeltaSpig);
009000090423         §Plbl3c = §Plbl3a + c_Spessore;
009100110330         // -?Posiz. orizzontali "T"?
009200090423         §Plbl4a = §Plbl3b + c_Spessore + c_XspaceX;
009300110331         §Plbl4b = §Plbl4a + ((0.057 - c_Spessore) / 2);
009400110330         // -?Posiz. orizzontali "O"?
009500110331         §Plbl5a = §Plbl4a + 0.057 + c_XspaceX;
009600090423         §Plbl5b = §Plbl5a + c_Spessore + c_LunghO -
009700090423                            (c_Spessore - c_DeltaSpig);
009800090423         §Plbl5c = §Plbl5a + c_DeltaSpig;
009900110330         // -?Posiz. orizzontali "L"?
010000090423         §Plbl6a = §Plbl5b + c_Spessore + c_XspaceX;
010100090423         §Plbl6b = §Plbl6a + c_Spessore;
010200110330         // -?Posiz. orizzontali "I"?
010300090423         §Plbl7a = §Plbl6a + c_Spessore + §PLU9 + c_XspaceX;
010400110330         // -?Posiz. orizzontali "N"?
010500110331         //  ?(spessore di 0.003 inches fissato all'interno del PrtF?
010600110330         //   ?per la parte in diagonale nella lettera "N")?
010700090423         §Plbl8a = §Plbl7a + c_Spessore + c_XspaceX;
010800090423         §Plbl8b = §Plbl8a + c_Spessore + c_LunghO -
010900110331                            (c_Spessore - c_DeltaSpig) - 0.003;
011000110331                          //(c_Spessore - c_DeltaSpig) + 0.001;
011100090423         §Plbl8c = §Plbl8a + c_Spessore;
011200110331         §Plbl8d = §Plbl8c + 0.003;
011300110331         §Plbl8e = §Plbl8d + 0.003;
011400110331         §Plbl8f = §Plbl8e + 0.003;
011500110331         §Plbl8g = §Plbl8f + 0.003;
011600110330         // -?Posiz. orizzontali "I"?
011700090423         §Plbl9A = §Plbl8b + c_Spessore + c_XspaceX;
011800090423
011900110330         // -?Lunghezza 2ª linea orizzontale in "A" (centrale)?
012000091015         //§PLUA   = §Plbl2b - §Plbl2a - c_Spessore;
012100110331         §PLUA   = §Plbl2b - §Plbl2a - c_Spessore + 0.001;
012200110330         // -?Altezza "linea obliqua" in "N"?
012300110413         //  ?(spessa)?
012400110413         //§PLU0  = §PlbVa + c_LunghMaxV - §PlbOh - 0.003;
012500110413         §PLU0  = §PlbVa + c_LunghMaxV - §PlbOh - 0.007;
012600110331         //  ?(altezza fissa)?
012700110413         //§PLU0  = 0.043;
012800090423
012900110330         // -?Posiz. orizzontale linea verticale destra nel box?
013000090424         §PlblVD = §Plbl9a + c_Spessore + c_BOXspace;
013100110330         // -?Lunghezza ultima linea orizzontale (spessa)?
013200090423         §PLUL   = §PlblVD - §PlblVS;
013300090423
013400090423       ENDSR;
013500090423      /end-free

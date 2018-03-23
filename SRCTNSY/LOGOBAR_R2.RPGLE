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
002100090423       BEGSR  sr_SimilLogoBART;
002200090423
002300110330         // -?Posiz. verticale linea orizzontale inferiore del box?
002400090423         §PlblI = §PlblS + 0.400;
002500110330         // -?Posiz. orizzontale linea verticale destra nel box?
002600110330         //§PlblVD = §PlblVS + 0.870;                ?[calcolata DOPO]?
002700110330         // -?Posiz. verticale "CORRIERE ESPRESSO"?
002800090423         §PCCE  = §PlblI + 0.080;
002900110330         // -?Posiz. verticale ultima linea orizzontale (spessa)?
003000090423         §PLOI  = §PlblI + 0.100;
003100110330         // -?Lunghezza ultima linea orizzontale (spessa)?
003200110330         //§PLUL  = §PlblVD - §PlblVS;               ?[calcolata DOPO]?
003300110330         // -?Spessore linee?
003400090423         §PSPE  = c_Spessore;
003500090423
003600110330         // -?Posiz. verticali "BARTOLINI"?
003700090423         §PlbVa = §PlblS + 0.109;
003800090423         §PlbVb = §PlbVa + c_DeltaSpig;
003900090423         §PlbVc = §PlbVa + c_LunghMaxV - c_DeltaSpig - c_LunghV;
004000090423         §PlbOa = §PlbVa;
004100090423         §PlbOb = §PlbVa + ((c_LunghMaxV - c_Spessore) / 2) - 0.001;
004200090423         §PlbOc = §PlbVa + c_LunghMaxV - c_Spessore;
004300090423
004400110330         // - -?Posiz. verticali parte interna di "N" in "BARTOLINI"?
004500090423         §PlbOd = §PlbVa + c_DeltaSpig;
004600090423         §PlbOe = §PlbOd + c_DeltaSpig;
004700090423         §PlbOf = §PlbOe + c_DeltaSpig;
004800090423         §PlbOg = §PlbOf + c_DeltaSpig;
004900090423         §PlbOh = §PlbOg + c_DeltaSpig;
005000090423
005100110330         // -?Lunghezza linee verticali lunghe in "B","R","L","I","N"?
005200090423         §PLU1  = c_LunghMaxV;
005300110330         // -?Lunghezza linee verticali corte in "B" e "R"?
005400090423         §PLU2  = c_LunghV;
005500110330         // -?Lunghezza linee orizzontali in "B" e "R"?
005600090423         §PLU3  = c_LunghO;
005700110330         // -?Lunghezza linee verticali in "A" e "T"?
005800090423         §PLU4  = c_LunghMaxV - c_DeltaSpig;
005900110330         // -?Lunghezza linea orizzontale superiore in "A" e "O"?
006000090423         §PLU5  = c_LunghO + c_Spessore - c_DeltaSpig;
006100110330         // -?Lunghezza linee verticali in "O"?
006200090423         §PLU6  = c_LunghMaxV - (c_DeltaSpig * 2);
006300110330         // -?Lunghezza 2ª linea verticale in "R" (inferiore)?
006400090423         §PLU7  = c_LunghV + c_DeltaSpig;
006500110330         // -?Lunghezza linea orizzontale in "T"?
006600090423         §PLU8  = c_Spessore + (0.025 * 2);
006700110330         // -?Lunghezza linea orizzontale in "L"?
006800090423         §PLU9  = c_Spessore;
006900110330         // -?Altezza "linea obliqua" in "N"?
007000110330         //§PLU0  = 0.065;                           ?[calcolata DOPO]?
007100110330         // -?Lunghezza 2ª linea orizzontale in "A" (centrale)?
007200110330         //§PLUA  = §Plbl2B - §Plbl2A - c_Spessore;  ?[calcolata DOPO]?
007300090423
007400110330         // -?Posiz. orizzontali "B"?
007500090424         §Plbl1a = §PlblVS + c_BOXspace;
007600090423         §Plbl1b = §Plbl1a + c_Spessore + c_LunghO -
007700090423                            (c_Spessore - c_DeltaSpig);
007800090423         §Plbl1C = §Plbl1a + c_Spessore;
007900110330         // -?Posiz. orizzontali "A"?
008000090423         §Plbl2a = §Plbl1b + c_Spessore + c_XspaceX;
008100090423         §Plbl2b = §Plbl2a + c_Spessore + c_LunghO -
008200090423                            (c_Spessore - c_DeltaSpig);
008300090423         §Plbl2c = §Plbl2a + c_DeltaSpig;
008400090423         §Plbl2d = §Plbl2a + c_Spessore;
008500110330         // -?Posiz. orizzontali "R"?
008600090423         §Plbl3a = §Plbl2b + c_Spessore + c_XspaceX;
008700090423         §Plbl3b = §Plbl3a + c_Spessore + c_LunghO -
008800090423                            (c_Spessore - c_DeltaSpig);
008900090423         §Plbl3c = §Plbl3a + c_Spessore;
009000110330         // -?Posiz. orizzontali "T"?
009100090423         §Plbl4a = §Plbl3b + c_Spessore + c_XspaceX;
009200090423         §Plbl4b = §Plbl4a + ((0.085 - c_Spessore) / 2);
009300110330         // -?Posiz. orizzontali "O"?
009400090423         §Plbl5a = §Plbl4a + 0.085 + c_XspaceX;
009500090423         §Plbl5b = §Plbl5a + c_Spessore + c_LunghO -
009600090423                            (c_Spessore - c_DeltaSpig);
009700090423         §Plbl5c = §Plbl5a + c_DeltaSpig;
009800110330         // -?Posiz. orizzontali "L"?
009900090423         §Plbl6a = §Plbl5b + c_Spessore + c_XspaceX;
010000090423         §Plbl6b = §Plbl6a + c_Spessore;
010100110330         // -?Posiz. orizzontali "I"?
010200090423         §Plbl7a = §Plbl6a + c_Spessore + §PLU9 + c_XspaceX;
010300110330         // -?Posiz. orizzontali "N"?
010400110330         //  ?(spessore di 0.005 inches fissato all'interno del PrtF?
010500110330         //   ?per la parte in diagonale nella lettera "N")?
010600090423         §Plbl8a = §Plbl7a + c_Spessore + c_XspaceX;
010700090423         §Plbl8b = §Plbl8a + c_Spessore + c_LunghO -
010800090423                            (c_Spessore - c_DeltaSpig) + 0.002;
010900090423         §Plbl8c = §Plbl8a + c_Spessore;
011000090424         §Plbl8d = §Plbl8c + 0.005;
011100090424         §Plbl8e = §Plbl8d + 0.005;
011200090424         §Plbl8f = §Plbl8e + 0.005;
011300090423         §Plbl8g = §Plbl8f + 0.005;
011400110330         // -?Posiz. orizzontali "I"?
011500090423         §Plbl9A = §Plbl8b + c_Spessore + c_XspaceX;
011600090423
011700110330         // -?Lunghezza 2ª linea orizzontale in "A" (centrale)?
011800091015         //§PLUA   = §Plbl2b - §Plbl2a - c_Spessore;
011900091015         §PLUA   = §Plbl2b - §Plbl2a - c_Spessore + 0.002;
012000110330         // -?Altezza "linea obliqua" in "N"?
012100110330         //  ?(bozza "bocciata" da ND)?
012200090424         //§PLU0  = §PlbVa + c_LunghMaxV - §PlbOh - 0.005;
012300090424         §PLU0  = 0.065;
012400090423
012500110330         // -?Posiz. orizzontale linea verticale destra nel box?
012600090424         §PlblVD = §Plbl9a + c_Spessore + c_BOXspace;
012700110330         // -?Lunghezza ultima linea orizzontale (spessa)?
012800090423         §PLUL   = §PlblVD - §PlblVS;
012900090423
013000090423       ENDSR;
013100090423      /end-free

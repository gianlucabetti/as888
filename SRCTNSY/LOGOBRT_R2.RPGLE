000100110526      /free
000200110526       //--------------------------------------------------------------
000300110526       //?Posizionamento del simil-logo BRT                            ?
000400110526       //--------------------------------------------------------------
000500110526       //?Richiede:                                                    ?
000600110526       //?· il free-format ("/free") DISattivato nel sorgente;         ?
000700110526       //?· la posizione dello spigolo di partenza (in alto a sinistra)?
000800110526       //?  già impostata;                                             ?
000900110526       //?Esempio:                                                     ?
001000110526       //?- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -?
001100110526       //?  // - Simil-Logo BRT                                        ?
001200110526       //?  §PlblS  = §Box1As;                                         ?
001300110526       //?  §PlblVS = 7.128;                                           ?
001400110526       //?  exsr  sr_SimilLogoBRT;                                     ?
001500110526       //?  ...                                                        ?
001600110526       //? /end-free                                                   ?
001700110526       //? /copy gaitrasrc/srcTNSY LogoBRT_R2                          ?
001800110526       //? /free                                                       ?
001900110526       //?  ...                                                        ?
002000110526       //--------------------------------------------------------------
002100110526       BEGSR  sr_SimilLogoBRT_;
002200110526
002300110526         // -?Posiz. verticali "BRT"?
002400110606         §PlbVa = §PlblS + (c_BOXspace * 2);
002500110526         §PlbVb = §PlbVa + c_DeltaSpig;
002600110526         §PlbVc = §PlbVa + c_LunghMaxV - c_DeltaSpig - c_LunghV;
002700110526         §PlbOa = §PlbVa;
002800110526         §PlbOb = §PlbVa + ((c_LunghMaxV - c_Spessore) / 2) - 0.001;
002900110526         §PlbOc = §PlbVa + c_LunghMaxV - c_Spessore;
003000110526
003100110526         // -?Lunghezza linee verticali lunghe in "B","R"?
003200110526         §PLU1  = c_LunghMaxV;
003300110526         // -?Lunghezza linee verticali corte in "B" e "R"?
003400110526         §PLU2  = c_LunghV;
003500110526         // -?Lunghezza linee orizzontali in "B" e "R"?
003600110526         §PLU3  = c_LunghO;
003700110526         // -?Lunghezza linee verticali in "T"?
003800110526         §PLU4  = c_LunghMaxV - c_DeltaSpig;
003900110526         // -?Lunghezza 2ª linea verticale in "R" (inferiore)?
004000110526         §PLU7  = c_LunghV + c_DeltaSpig;
004100110526         // -?Lunghezza linea orizzontale in "T"?
004200110606         //§PLU8  = c_Spessore + (0.025 * 2);
004300110606         §PLU8  = c_LunghO + c_Spessore + c_DeltaSpig;
004400110527
004500110527         // -?Posiz. verticale linea orizzontale inferiore del box?
004600120314         §PlblI = §PlblS + (c_BOXspace * 2) + c_LunghMaxV +
004700120314                           (c_BOXspace * 2);
004800110527         // -?Posiz. orizzontale linea verticale destra nel box?
004900110527         //§PlblVD = §PlblVS + 0.870;                ?[calcolata DOPO]?
005000110527         // -?Posiz. verticale "CORRIERE" ed "ESPRESSO"?
005100110606         //§PCC   = §PlblI + c_BOXspace + c_XspaceX;
005200110606         §PCC   = §PlblI + (c_BOXspace * 2) - c_LineNarrow;
005300120314         §PCE   = §PCC   + c_BOXspace + c_XspaceX;
005400120314         //§PCE   = §PCC   + (c_XspaceX * 2) + c_LineNarrow;
005500110527         // -?Posiz. verticale ultima linea orizzontale (spessa)?
005600120314         //§PLOI  = §PCE   + c_XspaceX;
005700120314         §PLOI  = §PCE   + (c_DeltaSpig * 2);
005800110527         // -?Lunghezza ultima linea orizzontale (spessa)?
005900110527         //§PLUL  = §PlblVD - §PlblVS;               ?[calcolata DOPO]?
006000110527         // -?Spessore linee?
006100110527         §PSPE  = c_Spessore;
006200110526
006300110526         // -?Posiz. orizzontali "B"?
006400110527         §Plbl1a = §PlblVS + c_BOXspace;
006500110526         §Plbl1b = §Plbl1a + c_Spessore + c_LunghO -
006600110526                            (c_Spessore - c_DeltaSpig);
006700110526         §Plbl1C = §Plbl1a + c_Spessore;
006800110526         // -?Posiz. orizzontali "R"?
006900110526         §Plbl3a = §Plbl1b + c_Spessore + c_XspaceX;
007000110526         §Plbl3b = §Plbl3a + c_Spessore + c_LunghO -
007100110526                            (c_Spessore - c_DeltaSpig);
007200110526         §Plbl3c = §Plbl3a + c_Spessore;
007300110526         // -?Posiz. orizzontali "T"?
007400110526         §Plbl4a = §Plbl3b + c_Spessore + c_XspaceX;
007500110606         //§Plbl4b = §Plbl4a + ((0.085 - c_Spessore) / 2);
007600110606         §Plbl4b = §Plbl4a + ((§PLU8 - c_Spessore) / 2);
007700110527
007800110527         // -?Posiz. orizzontali "CORRIERE"?
007900110606         §PlblV2 = §PlblVS + (c_DeltaSpig / 3 * 2);
008000110526
008100110526         // -?Posiz. orizzontale linea verticale destra nel box?
008200110527         §PlblVD = §Plbl4a + §Plu8 + c_BOXspace;
008300110526         // -?Lunghezza ultima linea orizzontale (spessa)?
008400110526         §PLUL   = §PlblVD - §PlblVS;
008500110526
008600110526       ENDSR;
008700110526      /end-free

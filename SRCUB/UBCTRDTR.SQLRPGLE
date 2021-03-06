000100121217       //==============================================================
000200150915       //?Reperimento Tariffa decorrente alla data              ?
000300121217       //==============================================================
000400150916       // __________________
000500150916       // ESEMPI DI CHIAMATA
000600150916       // ������������������
000700150916     C*                  CALLB     'UBCTRDTR'
000800150916     c*                  parm                    pIn_CTR
000900150916     c*                  parm                    pIn_KSC
001000150916     c*                  parm                    pIn_DtRif
001100150916     c*                  parm                    pOut_Esito
001200150916     c*                  parm                    pOut_EsitoDesc
001300150916     c*                  parm                    pOut_Rcd
001400150916       //
001500150916       // ______________________________
001600150916       // ISTRUZIONI PER LA COMPILAZIONE
001700150916       // ������������������������������
001800150916       // 1. Creare il modulo UBCTRDTR (Opz 15 PDM)
001900150916       // 2. Creare/Aggiornare il programma di servizio UBCTRDTR (CRTSRVPGM / UPDSRVPGM)
002000150916       // 4. Cancellare il modulo.
002100150916       //
002200150916       //==============================================================
002300141008
002400121217       //--------------------------------------------------------------
002500121217       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
002600121217       //--------------------------------------------------------------
002700121217
002800150916     /*MODULE
002900121217     /*PRM dbgview(*source)
003000121217     /*PRM commit(*none)
003100121217     /*END
003200121217
003300121217       //--------------------------------------------------------------
003400121217       //?Specifiche di controllo.                                     ?
003500121217       //--------------------------------------------------------------
003600121217
003700121217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003800150916       // h dftactgrp(*no)
003900121217     h alwnull(*inputonly)
004000121217     h bnddir('UBBNDDIR')
004100141029       // per dire che i calcoli intermedi devono avere lo stesso num. decimali del risultato
004200150915       // h expropts(*resdecpos)
004300121217
004400121217       //--------------------------------------------------------------
004500121217       //?Dichiarazione file.                                          ?
004600121217       //--------------------------------------------------------------
004700121217
004800150915
004900121217       //--------------------------------------------------------------
005000121217       //?Definizione costanti.                                        ?
005100121217       //--------------------------------------------------------------
005200121217
005300121217
005400121217       //--------------------------------------------------------------
005500121217       //?Definizione schiere.                                         ?
005600121217       //--------------------------------------------------------------
005700121217
005800121217
005900121217       //--------------------------------------------------------------
006000121217       //?Definizione aree dati.                                       ?
006100121217       //--------------------------------------------------------------
006200121217
006300121217       //--------------------------------------------------------------
006400121217       //?Definizione strutture dati.                                  ?
006500121217       //--------------------------------------------------------------
006600121217
006700121217       // -?Parametri in input?
006800150915     D pIn_CTR         s              3s 0
006900150915     D pIn_KSC         s              7s 0
007000150915     D pIn_DtRif       s              8s 0
007100150915     D pOut_Esito      s              2s 0
007200150915     D pOut_EsitoDesc  s             50
007300150915     D pOut_Rcd        s            256
007400121218
007500121217       // -?Status?
007600121217     d Psds           sds
007700121217     d   SDSpgm          *proc
007800121217     d*//JobName             244    253                                         Job name
007900121217     d*//JobUser             254    263                                         User name
008000121217     d*//JobNumber           264    269s 0                                      Job number
008100121217
008200121217       //--------------------------------------------------------------
008300121217       //?Definizione variabili.                                       ?
008400121217       //--------------------------------------------------------------
008500121217
008600121217       // -?Flags booleani?
008700121217     d $EoF            s               n   inz
008800121217
008900121217       // -?Indici di schiera?
009000121217     d xx              s              5  0 inz
009100121217     d yy              s              5  0 inz
009200121217     d ww              s              5  0 inz
009300140925     d wX              s              5  0 inz
009400140925     d wY              s              5  0 inz
009500121217
009600150916       // - Campi di comodo
009700141028     d dateISO         s               d
009800121218     d wErr            s             10i 0 inz
009900141027
010000121217       // -?Stringa SQL da eseguire?
010100121217     d wSQL            s           2048    inz  varying
010200150915     d wSQL_dati     e ds                  inz extname(TNTAM00F)
010300121217
010400121217       //--------------------------------------------------------------
010500121217       //?Definizione prototipi procedure.                             ?
010600121217       //--------------------------------------------------------------
010700121217
010800121217       //--------------------------------------------------------------
010900121217       //?Definizione key-list.                                        ?
011000121217       //--------------------------------------------------------------
011100121217
011200121217
011300121217       //--------------------------------------------------------------
011400121217       //?Definizione parametri procedura.                             ?
011500121217       //--------------------------------------------------------------
011600121217
011700121217     c     *Entry        plist
011800150915     c                   parm                    pIn_CTR
011900150915     c                   parm                    pIn_KSC
012000150915     c                   parm                    pIn_DtRif
012100150915     c                   parm                    pOut_Esito
012200150915     c                   parm                    pOut_EsitoDesc
012300150915     c                   parm                    pOut_Rcd
012400121217
012500121217      /free
012600121217
012700121217       //--------------------------------------------------------------
012800121217       //?M A I N - L I N E                                            ?
012900121217       //--------------------------------------------------------------
013000121217
013100121217       // -?Operazioni iniziali?
013200121217       exsr sr_RoutInz;
013300121217
013400150915       // -?Ciclo di reperimento
013500121218       doW  Not $EoF;
013600121217         exsr  sr_ReadCursor;
013700121218       enddo;
013800121217
013900121217       // -?Operazioni finali?
014000121217       exsr sr_RoutEnd;
014100121217
014200121217       //--------------------------------------------------------------
014300121217       //?Operazioni iniziali.                                         ?
014400121217       //--------------------------------------------------------------
014500121217       BEGSR  sr_RoutInz;
014600141217
014700121217         exec sql  set option  dynusrprf = *owner,
014800121217                               closqlcsr = *endmod;
014900121217
015000150916         // se data=0 imposto oggi
015100150916         IF pIn_DtRif = 0;
015200150916           pIn_DtRif = %dec(%date() : *ISO);
015300150916         ENDIF;
015400150916
015500121217         // -?Preparazione stringa SQL?
015600121217         exsr  sr_PrepSQL;
015700121217
015800121217         // -?Apertura cursore SQL?
015900121217         exsr  sr_OpenCursor;
016000140925
016100150916         $EoF = *off;
016200150916         clear  wErr;
016300150916         clear  wX;
016400140925         clear  wY;
016500130208
016600140925       ENDSR;
016700121219
016800141028       //--------------------------------------------------------------
016900141028       //?Preparazione stringa SQL                                     ?
017000141028       //--------------------------------------------------------------
017100141028       BEGSR  sr_PrepSQL;
017200141028
017300150915         // prendo i dati solo della prima tariffa decorrente alla data, anche perch� NON
017400150915         // dovrebbero essercene di pi�
017500150915         wSQL = 'SELECT * FROM TNTAM00F +
017600150915                 WHERE TAMKSC = ' + %editc(pIn_KSC:'X') +
017700150915                ' and TAMCTR = '  + %editc(pIn_CTR:'X') +
017800150915                ' and ' + %editc(pIn_DTRif:'X') + ' between TAMDDT and TAMDST +
017900150915                 and TAMATB <> ''A'' +
018000150915                 FETCH FIRST 1 ROWS ONLY';
018100141020
018200141020       ENDSR;
018300121217
018400121217       //--------------------------------------------------------------
018500121217       //?Apertura cursore.                                            ?
018600121217       //--------------------------------------------------------------
018700121217       BEGSR  sr_OpenCursor;
018800121217
018900121217         // -?Dichiarazione cursore?
019000121217         exec sql   prepare S1   from :wSQL;
019100141029         exec sql   declare C1  asensitive   cursor for S1;
019200121217
019300121217         // -?Apertura del cursore?
019400121217         exec sql   open C1;
019500121217
019600121217         if  SQLcode < *zero;
019700121217           $EoF = *on;
019800150915           wErr = 2;
019900150915           exsr sr_RoutEnd;
020000121217         endif;
020100121217
020200121217       ENDSR;
020300121217
020400121217       //--------------------------------------------------------------
020500121217       //?Lettura cursore.                                             ?
020600121217       //--------------------------------------------------------------
020700121217       BEGSR  sr_ReadCursor;
020800121217
020900121217         clear  wSQL_dati;
021000121217
021100121217         exec sql   fetch next   from C1   into :wSQL_dati;
021200141008
021300141008         //le righe successive servono per facilitare il debug
021400141008         //wY += 1;
021500141008         //if %rem(wY:1000) = 0;
021600141008         //  debug1000 = *on;
021700141008         //endif;
021800150915         //debug1000 = *off;
021900121217
022000150915         // il risultato � un solo cursore
022100141021
022200121217         select;
022300121217
022400121217           when  SQLcode < *zero;
022500121217             $EoF = *on;
022600150915             // errore SQL
022700121217             wErr = 2;
022800121217
022900121217           when  SQLcode = 100;
023000121217             $EoF = *on;
023100150915             // nessuna tariffa con i dati impostati
023200150915             wErr = 1;
023300121217
023400121217           other;
023500150915             // mando il risultato al chiamante
023600141021
023700141021             // dico che � finito il ciclo perch� ho gi� ricevuto tutti i dati
023800141021             $EoF = *on;
023900121217
024000121217         endsl;
024100121217
024200121217       ENDSR;
024300121217
024400121217       //--------------------------------------------------------------
024500121217       //?Chiusura cursore.                                            ?
024600121217       //--------------------------------------------------------------
024700121217       BEGSR  sr_CloseCursor;
024800121217
024900121217         // -?Chiusura del cursore?
025000121217         exec sql   close C1;
025100121217
025200121217       ENDSR;
025300121217
025400121217       //--------------------------------------------------------------
025500121217       //?Operazioni finali.                                           ?
025600121217       //--------------------------------------------------------------
025700121217       BEGSR  sr_RoutEnd;
025800121217
025900121217         // -?Chiusura cursore SQL?
026000121217         exsr  sr_CloseCursor;
026100141028
026200141028         // -?valorizza i parametri di output?
026300150915         pOut_Esito = wErr;
026400150915         select;
026500150915          when wErr = 0;
026600150915           pOut_EsitoDesc = 'Esito OK';
026700150915          when wErr = 1;
026800150915           pOut_EsitoDesc = 'Non trovata nessuna tariffa con i dati impostati';
026900150915          when wErr = 2;
027000150915           pOut_EsitoDesc = 'Errore SQL';
027100150915         endsl;
027200150915         pOut_Rcd   = wSQL_Dati;
027300121217
027400121217         // -?Chiusura pgm?
027500121217         return;
027600121217
027700121217       ENDSR;
027800121217
027900121217      /end-free

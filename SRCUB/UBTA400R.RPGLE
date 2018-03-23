000100091228       //==============================================================
000200160202       // Driver reperimento dati da TITA400F
000300091228       //==============================================================
000400091228
000500091228       //--------------------------------------------------------------
000600121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700091228       //--------------------------------------------------------------
000800091228
000900100325     /*PRM dbgview(*source)
001000091223     /*END
001100160202     **
001200160202     ** ISTRUZIONI PER LA COMPILAZIONE
001300160202     **
001400160202     ** 1. Creare il modulo UBTA400R (Opz 15 PDM)
001500160202     ** 2. Creare/Aggiornare il programma di servizio UBTA400R (CRTSRVPGM / UPDSRVPGM)
001600160202     **    con ACTGRP(*caller)
001700160202     **    aggiungere nella BNDDIR(UBBNDDIR)
001800160202     ** 3. Creare/Aggiornare il programma UBTA400R (CRTPGM / UPDPGM)
001900160202     **    con ACTGRP(*caller)
002000160202     ** 4. Cancellare il modulo.
002100160202     **
002200091228
002300091228       //--------------------------------------------------------------
002400121106       // Specifiche di controllo.
002500091228       //--------------------------------------------------------------
002600091223
002700091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002800091223     h alwnull(*inputonly)
002900091223
003000091223       //--------------------------------------------------------------
003100121106       // Dichiarazione file.
003200091223       //--------------------------------------------------------------
003300100112
003400160202     FTITA430C  IF   E           K DISK
003500091223
003600091223       //--------------------------------------------------------------
003700121106       // Definizione costanti.
003800091223       //--------------------------------------------------------------
003900100302
004000091223
004100091223       //--------------------------------------------------------------
004200121106       // Definizione schiere.
004300091223       //--------------------------------------------------------------
004400091223
004500091223
004600091223       //--------------------------------------------------------------
004700121106       // Definizione aree dati.
004800091223       //--------------------------------------------------------------
004900091223
005000091223
005100140724       //--------------------------------------------------------------
005200140724       // Definizione strutture dati.
005300140724       //--------------------------------------------------------------
005400140724
005500121106       // - Status
005600110516     d Psds           sds
005700110516     d   SDSpgm          *proc
005800121106     d   JobName             244    253                                         Job name
005900121106     d   JobUser             254    263                                         User name
006000121106     d   JobNumber           264    269s 0                                      Job number
006100160202
006200160202       // - DS per TITA400f tipo record "A"
006300160204     d DTA4A         e ds
006400160209       // - DS per TITA400f tipo record "C"
006500160209     d DTA4C         e ds
006600160209       // - DS per TITA400f tipo record "K"
006700160209     d DTA4K         e ds
006800160209       // - DS per TITA400f tipo record "M"
006900160209     d DTA4M         e ds
007000160209       // - DS per TITA400f tipo record "Q"
007100160209     d DTA4Q         e ds
007200160209       // - DS per TITA400f tipo record "4"
007300160209     d DTA44         e ds
007400091223
007500091223       //--------------------------------------------------------------
007600121106       // Definizione variabili.
007700091223       //--------------------------------------------------------------
007800091223
007900121106       // - Parametri ricevuti:
008000160202     d pIn_Opz         s              1a
008100160202       // tipo lancio: 'C'=LR   *blank=RT
008200160202     d pIn_TLa         s              1a
008300160202     d pIn_AAS         s              4s 0
008400160202     d pIn_LNP         s              3s 0
008500160202     d pIn_NRS         s              2s 0
008600160202     d pIn_NSP         s              7s 0
008700160202     d pIn_TRC         s              1a
008800160202       // esito chiamata: 0=OK   1=ERR INPUT   2=KO
008900160202     d pOut_ERR        s              1s 0
009000160202     d pOut_DS         s             10a
009100160202     d pOut_Len        s              2s 0
009200160202     d pOut_Dati       s             35a
009300121106
009400121106       // Campi di comodo
009500121106     d wDate           s              8  0 inz
009600091223
009700091223       //--------------------------------------------------------------
009800121106       // Definizione prototipi procedure.
009900091223       //--------------------------------------------------------------
010000130307
010100091223       //--------------------------------------------------------------
010200121106       // Definizione key-list.
010300091223       //--------------------------------------------------------------
010400091223
010500160202       // - File TITA430C
010600160202     d k05tita430    e ds                  extname(TITA430C : *key)
010700140724     d                                     prefix(k_)   inz
010800091223
010900091223       //--------------------------------------------------------------
011000121107       // Definizione parametri procedura.
011100091223       //--------------------------------------------------------------
011200091223
011300091223     c     *Entry        plist
011400160202     c                   parm                    pIn_Opz
011500160204     c                   parm                    pIn_TLa
011600160202     c                   parm                    pIn_AAS
011700160202     c                   parm                    pIn_LNP
011800160202     c                   parm                    pIn_NRS
011900160202     c                   parm                    pIn_NSP
012000160202     c                   parm                    pIn_TRC
012100160202     c                   parm                    pOut_ERR
012200160202     c                   parm                    pOut_DS
012300160202     c                   parm                    pOut_Len
012400160202     c                   parm                    pOut_Dati
012500091223
012600130307      /free
012700091223
012800091223       //--------------------------------------------------------------
012900121106       // M A I N - L I N E
013000091223       //--------------------------------------------------------------
013100140806
013200130307       // Operazioni iniziali?
013300130307       exsr sr_RoutInz;
013400130307
013500140724       // attività richiesta dal chiamante se non chiamato per chiusura
013600160202       if pIn_TLa = *blank;
013700160202         K_TA4AAS = pIn_AAS;
013800160202         K_TA4LNP = pIn_LNP;
013900160202         K_TA4NRS = pIn_NRS;
014000160202         K_TA4NSP = pIn_NSP;
014100160202         K_TA4TRC = pIn_TRC;
014200160202         chain %kds(K05TITA430) TITA430C ;
014300160202         If  %found(TITA430C);
014400160202           select;
014500160202            when pIn_TRC='A';
014600160202             DTA4A = TA4NOT;
014700160202             pOut_Dati = DTA4A;
014800160202             pOut_DS   = 'DTA4A';
014900160202             pOut_Len  = %size(DTA4A);
015000160209            when pIn_TRC='C';
015100160209             DTA4C = TA4NOT;
015200160209             pOut_Dati = DTA4C;
015300160209             pOut_DS   = 'DTA4C';
015400160209             pOut_Len  = %size(DTA4C);
015500160209            when pIn_TRC='K';
015600160209             DTA4K = TA4NOT;
015700160209             pOut_Dati = DTA4K;
015800160209             pOut_DS   = 'DTA4K';
015900160209             pOut_Len  = %size(DTA4K);
016000160209            when pIn_TRC='M';
016100160209             DTA4M = TA4NOT;
016200160209             pOut_Dati = DTA4M;
016300160209             pOut_DS   = 'DTA4M';
016400160209             pOut_Len  = %size(DTA4M);
016500160209            when pIn_TRC='Q';
016600160209             DTA4Q = TA4NOT;
016700160209             pOut_Dati = DTA4Q;
016800160209             pOut_DS   = 'DTA4Q';
016900160209             pOut_Len  = %size(DTA4Q);
017000160209            when pIn_TRC='4';
017100160209             DTA44 = TA4NOT;
017200160209             pOut_Dati = DTA44;
017300160209             pOut_DS   = 'DTA44';
017400160209             pOut_Len  = %size(DTA44);
017500160202           endsl;
017600160202           pOut_Err = 0;
017700140724         endif;
017800140724       endif;
017900091223
018000121106       // Operazioni finali?
018100091223       exsr sr_RoutEnd;
018200091223
018300091223       //--------------------------------------------------------------
018400121107       // Operazioni iniziali.
018500091223       //--------------------------------------------------------------
018600091223       BEGSR  sr_RoutInz;
018700121108
018800121108         // Reperimento data odierna (fmt aaaa/mm/gg)
018900121108         wDate = %dec( %date() );
019000160202
019100160202         // Inizializzazione parametri di output
019200160202         clear pOut_ERR     ;
019300160202         clear pOut_DS      ;
019400160202         clear pOut_Len     ;
019500160202         clear pOut_Dati    ;
019600160202
019700160202         // Inizializzo risultato a errore
019800160202         pOut_Err = 2;
019900160202
020000160202         // Controllo i parametri di input
020100160202         exsr sr_CtrlPIn;
020200160202
020300160202       ENDSR;
020400160202
020500160202       //--------------------------------------------------------------
020600160202       // Controllo i parametri di input
020700160202       //--------------------------------------------------------------
020800160202       BEGSR  sr_CtrlPIn;
020900140724
021000160202         // L'opzione in questo caso non serve'
021100160202
021200160202         // Tipo lancio deve essere blank o 'C'
021300160202         if pIn_TLa <> *blank and pIn_TLa <>'C';
021400160202           // esco dando errore
021500160202           pOut_Err = 1;
021600160202           exsr sr_RoutEnd;
021700140724         endif;
021800160202
021900160205         // Anno spedizione deve essere valorizzato (se non chiusura)
022000160205         if pIn_AAS <= 0 and pIn_TLa <>'C';
022100160202           // esco dando errore
022200160202           pOut_Err = 1;
022300160202           exsr sr_RoutEnd;
022400160202         endif;
022500160202
022600160205         // Linea di partenza deve essere valorizzato (se non chiusura)
022700160205         if pIn_LNP <= 0 and pIn_TLa <>'C';
022800160202           // esco dando errore
022900160202           pOut_Err = 1;
023000160202           exsr sr_RoutEnd;
023100160202         endif;
023200160202
023300160205         // Nr. Spedizione deve essere valorizzato (se non chiusura)
023400160205         if pIn_NSP <= 0 and pIn_TLa <>'C';
023500160202           // esco dando errore
023600160202           pOut_Err = 1;
023700160202           exsr sr_RoutEnd;
023800160202         endif;
023900160202
024000160205         // Tipo tracciato deve contenere uno dei valori gestiti: (se non chiusura)
024100160209         // A-C-K-M-Q-4
024200160209         if pIn_TRC <> 'A'
024300160209          and pIn_TRC <> 'C'
024400160209          and pIn_TRC <> 'K'
024500160209          and pIn_TRC <> 'M'
024600160209          and pIn_TRC <> 'Q'
024700160209          and pIn_TRC <> '4'
024800160209          and pIn_TLa <>'C';
024900160202           // esco dando errore
025000160202           pOut_Err = 1;
025100160202           exsr sr_RoutEnd;
025200160202         endif;
025300091223
025400091223       ENDSR;
025500121106
025600091223       //--------------------------------------------------------------
025700121107       // Operazioni finali.
025800091223       //--------------------------------------------------------------
025900091223       BEGSR  sr_RoutEnd;
026000140724
026100140724         // Chiusura pgm
026200160202         if pIn_TLa = 'C';
026300140724           *inlr = *on;
026400140724         else;
026500140724           *inrt = *on;
026600140724         endif;
026700140724         return;
026800091223
026900091223       ENDSR;
027000091223
027100091223      /end-free

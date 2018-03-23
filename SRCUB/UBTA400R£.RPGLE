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
006400091223
006500091223       //--------------------------------------------------------------
006600121106       // Definizione variabili.
006700091223       //--------------------------------------------------------------
006800091223
006900121106       // - Parametri ricevuti:
007000160202     d pIn_Opz         s              1a
007100160202       // tipo lancio: 'C'=LR   *blank=RT
007200160202     d pIn_TLa         s              1a
007300160202     d pIn_AAS         s              4s 0
007400160202     d pIn_LNP         s              3s 0
007500160202     d pIn_NRS         s              2s 0
007600160202     d pIn_NSP         s              7s 0
007700160202     d pIn_TRC         s              1a
007800160202       // esito chiamata: 0=OK   1=ERR INPUT   2=KO
007900160202     d pOut_ERR        s              1s 0
008000160202     d pOut_DS         s             10a
008100160202     d pOut_Len        s              2s 0
008200160202     d pOut_Dati       s             35a
008300121106
008400121106       // Campi di comodo
008500121106     d wDate           s              8  0 inz
008600091223
008700091223       //--------------------------------------------------------------
008800121106       // Definizione prototipi procedure.
008900091223       //--------------------------------------------------------------
009000130307
009100091223       //--------------------------------------------------------------
009200121106       // Definizione key-list.
009300091223       //--------------------------------------------------------------
009400091223
009500160202       // - File TITA430C
009600160202     d k05tita430    e ds                  extname(TITA430C : *key)
009700140724     d                                     prefix(k_)   inz
009800091223
009900091223       //--------------------------------------------------------------
010000121107       // Definizione parametri procedura.
010100091223       //--------------------------------------------------------------
010200091223
010300091223     c     *Entry        plist
010400160202     c                   parm                    pIn_Opz
010500160204     c                   parm                    pIn_TLa
010600160202     c                   parm                    pIn_AAS
010700160202     c                   parm                    pIn_LNP
010800160202     c                   parm                    pIn_NRS
010900160202     c                   parm                    pIn_NSP
011000160202     c                   parm                    pIn_TRC
011100160202     c                   parm                    pOut_ERR
011200160202     c                   parm                    pOut_DS
011300160202     c                   parm                    pOut_Len
011400160202     c                   parm                    pOut_Dati
011500091223
011600130307      /free
011700091223
011800091223       //--------------------------------------------------------------
011900121106       // M A I N - L I N E
012000091223       //--------------------------------------------------------------
012100140806
012200130307       // Operazioni iniziali?
012300130307       exsr sr_RoutInz;
012400130307
012500140724       // attività richiesta dal chiamante se non chiamato per chiusura
012600160202       if pIn_TLa = *blank;
012700160202         K_TA4AAS = pIn_AAS;
012800160202         K_TA4LNP = pIn_LNP;
012900160202         K_TA4NRS = pIn_NRS;
013000160202         K_TA4NSP = pIn_NSP;
013100160202         K_TA4TRC = pIn_TRC;
013200160202         chain %kds(K05TITA430) TITA430C ;
013300160202         If  %found(TITA430C);
013400160202           select;
013500160202            when pIn_TRC='A';
013600160202             DTA4A = TA4NOT;
013700160202             pOut_Dati = DTA4A;
013800160202             pOut_DS   = 'DTA4A';
013900160202             pOut_Len  = %size(DTA4A);
014000160202           endsl;
014100160202           pOut_Err = 0;
014200140724         endif;
014300140724       endif;
014400091223
014500121106       // Operazioni finali?
014600091223       exsr sr_RoutEnd;
014700091223
014800091223       //--------------------------------------------------------------
014900121107       // Operazioni iniziali.
015000091223       //--------------------------------------------------------------
015100091223       BEGSR  sr_RoutInz;
015200121108
015300121108         // Reperimento data odierna (fmt aaaa/mm/gg)
015400121108         wDate = %dec( %date() );
015500160202
015600160202         // Inizializzazione parametri di output
015700160202         clear pOut_ERR     ;
015800160202         clear pOut_DS      ;
015900160202         clear pOut_Len     ;
016000160202         clear pOut_Dati    ;
016100160202
016200160202         // Inizializzo risultato a errore
016300160202         pOut_Err = 2;
016400160202
016500160202         // Controllo i parametri di input
016600160202         exsr sr_CtrlPIn;
016700160202
016800160202       ENDSR;
016900160202
017000160202       //--------------------------------------------------------------
017100160202       // Controllo i parametri di input
017200160202       //--------------------------------------------------------------
017300160202       BEGSR  sr_CtrlPIn;
017400140724
017500160202         // L'opzione in questo caso non serve'
017600160202
017700160202         // Tipo lancio deve essere blank o 'C'
017800160202         if pIn_TLa <> *blank and pIn_TLa <>'C';
017900160202           // esco dando errore
018000160202           pOut_Err = 1;
018100160202           exsr sr_RoutEnd;
018200140724         endif;
018300160202
018400160205         // Anno spedizione deve essere valorizzato (se non chiusura)
018500160205         if pIn_AAS <= 0 and pIn_TLa <>'C';
018600160202           // esco dando errore
018700160202           pOut_Err = 1;
018800160202           exsr sr_RoutEnd;
018900160202         endif;
019000160202
019100160205         // Linea di partenza deve essere valorizzato (se non chiusura)
019200160205         if pIn_LNP <= 0 and pIn_TLa <>'C';
019300160202           // esco dando errore
019400160202           pOut_Err = 1;
019500160202           exsr sr_RoutEnd;
019600160202         endif;
019700160202
019800160205         // Nr. Spedizione deve essere valorizzato (se non chiusura)
019900160205         if pIn_NSP <= 0 and pIn_TLa <>'C';
020000160202           // esco dando errore
020100160202           pOut_Err = 1;
020200160202           exsr sr_RoutEnd;
020300160202         endif;
020400160202
020500160205         // Tipo tracciato deve contenere uno dei valori gestiti: (se non chiusura)
020600160202         // 'A'
020700160205         if pIn_TRC <> 'A' and pIn_TLa <>'C';
020800160202           // esco dando errore
020900160202           pOut_Err = 1;
021000160202           exsr sr_RoutEnd;
021100160202         endif;
021200091223
021300091223       ENDSR;
021400121106
021500091223       //--------------------------------------------------------------
021600121107       // Operazioni finali.
021700091223       //--------------------------------------------------------------
021800091223       BEGSR  sr_RoutEnd;
021900140724
022000140724         // Chiusura pgm
022100160202         if pIn_TLa = 'C';
022200140724           *inlr = *on;
022300140724         else;
022400140724           *inrt = *on;
022500140724         endif;
022600140724         return;
022700091223
022800091223       ENDSR;
022900091223
023000091223      /end-free

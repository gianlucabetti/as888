000100091228       //==============================================================
000200160204       // Driver reperimento dati da TNTBE00F per tabelle aventi il campo KE2 usato come
000300160204       // data decorrenza
000400091228       //==============================================================
000500091228
000600091228       //--------------------------------------------------------------
000700121106       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800091228       //--------------------------------------------------------------
000900091228
001000100325     /*PRM dbgview(*source)
001100091223     /*END
001200160202     **
001300160202     ** ISTRUZIONI PER LA COMPILAZIONE
001400160202     **
001500160204     ** 1. Creare il modulo UBTBE01R (Opz 15 PDM)
001600160204     ** 2. Creare/Aggiornare il programma di servizio UBTBE01R (CRTSRVPGM / UPDSRVPGM)
001700160202     **    con ACTGRP(*caller)
001800160202     **    aggiungere nella BNDDIR(UBBNDDIR)
001900160204     ** 3. Creare/Aggiornare il programma UBTBE01R (CRTPGM / UPDPGM)
002000160202     **    con ACTGRP(*caller)
002100160202     ** 4. Cancellare il modulo.
002200160202     **
002300091228
002400091228       //--------------------------------------------------------------
002500121106       // Specifiche di controllo.
002600091228       //--------------------------------------------------------------
002700091223
002800091223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002900091223     h alwnull(*inputonly)
003000091223
003100091223       //--------------------------------------------------------------
003200121106       // Dichiarazione file.
003300091223       //--------------------------------------------------------------
003400100112
003500160204     FTNTBE01L  IF   E           K DISK
003600091223
003700091223       //--------------------------------------------------------------
003800121106       // Definizione costanti.
003900091223       //--------------------------------------------------------------
004000100302
004100091223
004200091223       //--------------------------------------------------------------
004300121106       // Definizione schiere.
004400091223       //--------------------------------------------------------------
004500091223
004600091223
004700091223       //--------------------------------------------------------------
004800121106       // Definizione aree dati.
004900091223       //--------------------------------------------------------------
005000091223
005100091223
005200140724       //--------------------------------------------------------------
005300140724       // Definizione strutture dati.
005400140724       //--------------------------------------------------------------
005500140724
005600121106       // - Status
005700110516     d Psds           sds
005800110516     d   SDSpgm          *proc
005900121106     d   JobName             244    253                                         Job name
006000121106     d   JobUser             254    263                                         User name
006100121106     d   JobNumber           264    269s 0                                      Job number
006200160202
006300160205       // - DS per TNTBE00f cod='AOP' key='LOG-GG'
006400160204     d DAOP001       e ds
006500091223
006600091223       //--------------------------------------------------------------
006700121106       // Definizione variabili.
006800091223       //--------------------------------------------------------------
006900091223
007000121106       // - Parametri ricevuti:
007100160202     d pIn_Opz         s              1a
007200160202       // tipo lancio: 'C'=LR   *blank=RT
007300160202     d pIn_TLa         s              1a
007400160204     d pIn_COD         s              3a
007500160204     d pIn_KEY         s             15a
007600160204     d pIn_Data        s              8s 0
007700160202       // esito chiamata: 0=OK   1=ERR INPUT   2=KO
007800160202     d pOut_ERR        s              1s 0
007900160202     d pOut_DS         s             10a
008000160204     d pOut_Len        s              3s 0
008100160204     d pOut_Dati       s            256a
008200121106
008300121106       // Campi di comodo
008400121106     d wDate           s              8  0 inz
008500160204     d wTrovato        s               n   inz
008600091223
008700091223       //--------------------------------------------------------------
008800121106       // Definizione prototipi procedure.
008900091223       //--------------------------------------------------------------
009000130307
009100091223       //--------------------------------------------------------------
009200121106       // Definizione key-list.
009300091223       //--------------------------------------------------------------
009400091223
009500160204       // - File TNTBE01L
009600160204     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
009700160204     d                                     prefix(k_)   inz
009800091223
009900091223       //--------------------------------------------------------------
010000121107       // Definizione parametri procedura.
010100091223       //--------------------------------------------------------------
010200091223
010300091223     c     *Entry        plist
010400160202     c                   parm                    pIn_Opz
010500160204     c                   parm                    pIn_TLa
010600160204     c                   parm                    pIn_Cod
010700160204     c                   parm                    pIn_Key
010800160204     c                   parm                    pIn_Data
010900160202     c                   parm                    pOut_ERR
011000160202     c                   parm                    pOut_DS
011100160202     c                   parm                    pOut_Len
011200160202     c                   parm                    pOut_Dati
011300091223
011400130307      /free
011500091223
011600091223       //--------------------------------------------------------------
011700121106       // M A I N - L I N E
011800091223       //--------------------------------------------------------------
011900160204
012000160204       // Operazioni iniziali?
012100160204       exsr sr_RoutInz;
012200160204
012300160204       // attività richiesta dal chiamante se non chiamato per chiusura
012400160204       if pIn_TLa = *blank;
012500160204         K_TBECOD = pIn_Cod;
012600160204         K_TBEKE1 = pIn_Key;
012700160204         K_TBEKE2 = %editc(pIn_Data:'X');
012800160204         K_TBELIN = *blank;
012900160204         K_TBESIF = *blank;
013000160204         wTrovato = *off;
013100160204         setgt %kds(k05tntbe01:3) TNTBE000;
013200160204         dow %found(TNTBE01L);
013300160204           readp TNTBE000;
013400160204           // se il rcd non è annullato
013500160204           //    il codice tab. e la chiave primaria sono quelli ricevuti
013600160204           // col READP la data decorrenza può essere solo <= di quella di riferimento
013700160204           If %found(TNTBE01L);
013800160204             if TBEATB = *blank and TBECod = pIn_Cod and TBEKE1 = pIn_Key;
013900160204              // and %dec(TBEKE2)<=pIn_Data;
014000160204               // allora ho trovato il rcd ed esco
014100160204               wTrovato = *on;
014200160204               leave;
014300160204             else;
014400160204               // se il rcd non è annullato ma sono qui, vuol dire che sono cambiati i dati
014500160204               // della chiave primaria - e quindi non c'è un dato utile
014600160204               If TBEATB = *blank;
014700160204                 wTrovato = *off;
014800160204                 leave;
014900160204               endif;
015000160204             endif;
015100160204           endif;
015200160204         enddo;
015300160204         If  wTrovato=*on;
015400160202           select;
015500160204            when pIn_COD='AOP' and pIn_Key='LOG-GG';
015600160204             DAOP001 = TBEUNI;
015700160204             pOut_Dati = DAOP001;
015800160204             pOut_DS   = 'DAOP001';
015900160204             pOut_Len  = %size(DAOP001);
016000160202           endsl;
016100160202           pOut_Err = 0;
016200140724         endif;
016300140724       endif;
016400091223
016500121106       // Operazioni finali?
016600091223       exsr sr_RoutEnd;
016700091223
016800091223       //--------------------------------------------------------------
016900121107       // Operazioni iniziali.
017000091223       //--------------------------------------------------------------
017100091223       BEGSR  sr_RoutInz;
017200121108
017300121108         // Reperimento data odierna (fmt aaaa/mm/gg)
017400121108         wDate = %dec( %date() );
017500160202
017600160202         // Inizializzazione parametri di output
017700160202         clear pOut_ERR     ;
017800160202         clear pOut_DS      ;
017900160202         clear pOut_Len     ;
018000160202         clear pOut_Dati    ;
018100160202
018200160202         // Inizializzo risultato a errore
018300160202         pOut_Err = 2;
018400160202
018500160202         // Controllo i parametri di input
018600160202         exsr sr_CtrlPIn;
018700160202
018800160202       ENDSR;
018900160202
019000160202       //--------------------------------------------------------------
019100160202       // Controllo i parametri di input
019200160202       //--------------------------------------------------------------
019300160202       BEGSR  sr_CtrlPIn;
019400140724
019500160202         // L'opzione in questo caso non serve'
019600160202
019700160202         // Tipo lancio deve essere blank o 'C'
019800160202         if pIn_TLa <> *blank and pIn_TLa <>'C';
019900160202           // esco dando errore
020000160202           pOut_Err = 1;
020100160202           exsr sr_RoutEnd;
020200140724         endif;
020300160202
020400160204         // Codice tabella deve essere valorizzato (se non chiusura)
020500160204         if pIn_Cod = *blank and pIn_TLa <>'C';
020600160202           // esco dando errore
020700160202           pOut_Err = 1;
020800160202           exsr sr_RoutEnd;
020900160202         endif;
021000160204
021100160204         // Chiave deve essere valorizzato (se non chiusura)
021200160204         if pIn_Key = *blank and pIn_TLa <>'C';
021300160204           // esco dando errore
021400160204           pOut_Err = 1;
021500160204           exsr sr_RoutEnd;
021600160204         endif;
021700160204
021800160204         // Se la data di riferimento non è stata impostata, assumo data corrente
021900160204         if pIn_Data = 0;
022000160204           // assumo data corrente
022100160204           pIn_Data = WDate;
022200160204         endif;
022300091223
022400091223       ENDSR;
022500121106
022600091223       //--------------------------------------------------------------
022700121107       // Operazioni finali.
022800091223       //--------------------------------------------------------------
022900091223       BEGSR  sr_RoutEnd;
023000140724
023100140724         // Chiusura pgm
023200160202         if pIn_TLa = 'C';
023300140724           *inlr = *on;
023400140724         else;
023500140724           *inrt = *on;
023600140724         endif;
023700140724         return;
023800091223
023900091223       ENDSR;
024000091223
024100091223      /end-free

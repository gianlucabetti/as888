000100110221       //==============================================================
000200170323       // UBLGOB02R // Logica 02 reperimento dati oggetto
000300110221       //==============================================================
000400170125
000500170125       //--------------------------------------------------------------
000600170125       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700170125       //--------------------------------------------------------------
000800170125
000900170125     /*PRM dbgview(*source)
001000170125     /*PRM commit(*none)
001100170125     /*END
001200170125     **
001300170125     ** ISTRUZIONI PER LA COMPILAZIONE
001400170125     **
001500170323     ** 1. Creare il modulo UBLGOB02R (Opz 15 PDM)
001600170323     ** 2. Creare/Aggiornare il programma di servizio UBLGOB02R (CRTSRVPGM / UPDSRVPGM)
001700170130     **    con ACTGRP(*caller)
001800170130     **    aggiungere nella BNDDIR(UBBNDDIR)
001900170130     ** 3. Visto chi userà questo servizio, non serve l'oggetto programma
002000170130     ** 4. Cancellare il modulo.
002100170125     **
002200170125
002300120511       //
002400170126       // i parametri di ingresso devono essere:
002500170125       // iDati: obbligatorio
002600170125
002700170125       // oDati
002800170130       // oError = 0 => l'oggetto rientra tra quelli da segnalare per questa logica
002900170130       // oError = 1 => l'oggetto NON rientra tra quelli da segnalare per questa logica
003000170126       // oError = 2 => una o tutte le date nella DS non sono convertibili o se lo sono, non sono
003100170126       //               corrette
003200170130       // oError = 3 => i dati ricevuti non contenevano ciò che la logica si aspettava
003300120522
003400110221       //--------------------------------------------------------------
003500120511       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
003600110221       //--------------------------------------------------------------
003700110221
003800110221     /*PRM dbgview(*source)
003900110221     /*END
004000110221
004100110221       //--------------------------------------------------------------
004200120511       // Specifiche di controllo.                                     ?
004300110221       //--------------------------------------------------------------
004400110221
004500110221     h decedit('0,') datedit(*dmy/) option(*nodebugio)
004600170130     h alwnull(*inputonly)
004700110221
004800110221       //--------------------------------------------------------------
004900120511       // Dichiarazione file.                                          ?
005000110221       //--------------------------------------------------------------
005100110221
005200110221
005300110221       //--------------------------------------------------------------
005400120511       // Definizione costanti.                                        ?
005500110221       //--------------------------------------------------------------
005600110221
005700110221
005800110221       //--------------------------------------------------------------
005900120511       // Definizione schiere.                                         ?
006000110221       //--------------------------------------------------------------
006100110221
006200110221
006300110221       //--------------------------------------------------------------
006400120511       // Definizione aree dati.                                       ?
006500110221       //--------------------------------------------------------------
006600110221
006700110221
006800110221       //--------------------------------------------------------------
006900120511       // Definizione strutture dati.                                  ?
007000110221       //--------------------------------------------------------------
007100110221
007200120511       // Status ds?
007300110221     d Status         sds
007400120511     d  SDSproc          *proc
007500120511     d  SDSparms         *parms
007600110224     d  SDSdta               191    198
007700110224     d  SDSjob               244    253
007800110221     d  SDSusr               254    263
007900110224     d  SDSjnr               264    269  0
008000170130
008100170130       // - Parametri in input?
008200170130     d KPJBA         e ds
008300170130
008400170323     d VPR_LOG02       ds                  qualified
008500170130     d   Num                          3a
008600170130     d   Cosa                         2a
008700170130
008800170130       // Ricerca/Controllo tabelle (TNTBE)?
008900170130     d TIBS02ds      e ds                  inz
009000170130     d   T02mod      e                     inz('C')
009100170130     d   T02cod      e                     inz('VPR')
009200170323     d   T02ke1      e                     inz('CTRLOBJ_LOG02')
009300170130     d   T02ke2      e                     inz
009400170126
009500170131     D oDatiDS         DS                  qualified
009600170131     D  USCUP01                      10a
009700170131     D  USDUC                         4a
009800170131     D  USCDT06                       8a
009900170131     D  USLUDT                        8a
010000170131     d  Num                           3a
010100170131     d  Cosa                          2a
010200170303     d  QUSSFILN02                   10a
010300170303     d  QUSSFLN02                    10a
010400170303     d  QUSSFMN01                    10a
010500170131
010600170131     D Curdate         DS
010700170131     D Curdat                         8S 0
010800170130     D CurTim                         9S 0
010900170131
011000170131       //--------------------------------------------------------------
011100170131       // Definizione variabili globali.                               ?
011200170131       //--------------------------------------------------------------
011300170130
011400170130     d Data08A         s              8
011500170126     d wQUSCDT06       s              8
011600170126     d wQUSLUDT        s              8
011700170303
011800170303     D SrcFileN        S             10                                         Source File
011900170303     D SrcLibN         S             10                                         Source File Library
012000170303     D SrcMbrN         S             10                                         Source File Member
012100120511
012200120511       // indici
012300120511
012400120511       // booleani
012500120511
012600110221       //--------------------------------------------------------------
012700120511       // Definizione prototipi procedure.                             ?
012800110221       //--------------------------------------------------------------
012900170126
013000170126       // strutture generiche per API
013100170126      /copy QSYSINC/QRPGLESRC,QUSGEN
013200170126
013300170126       // prototipi per QUS...
013400170126      /copy gaitrasrc/srcprotopr,QUS
013500110221
013600120511       // lista oggetti
013700120511      /copy QSYSINC/QRPGLESRC,QUSLOBJ
013800170126
013900170126       // Parametri API QCAPCMD (Process Commands)?
014000170126     d Qcmd            s           1024    inz  varying
014100170126      /copy qSysInc/qRpgleSrc,QCAPCMD
014200170126      /copy gaitrasrc/srcProtoPR,QCAPCMD
014300170126
014400170126       // Parametri gestione errori API.?
014500170126      /copy qSysInc/qRpgleSrc,QUSEC
014600170126
014700170126       // Parametri per conversione data da un formato a un altro
014800170126      /copy gaitrasrc/srcprotopr,qwccvtdt
014900170126
015000170126      /copy gaitrasrc/srcProtoPR,TIBS02R
015100170130
015200170323      /copy gaitrasrc/srcProtoPI,UBLGOB02R
015300110221
015400110221       //--------------------------------------------------------------
015500120511       // Definizione key-list.                                        ?
015600110221       //--------------------------------------------------------------
015700110221
015800110221
015900110221       //==============================================================
016000120511       // M A I N - L I N E                                            ?
016100110221       //==============================================================
016200110221
016300120511       // Operazioni iniziali?
016400110221       exsr  sr_RoutInz;
016500110221
016600170125       // Eseguo i test della logica
016700170323       exsr  ExeLog02;
016800170125
016900170125       // esco
017000170125       exsr sr_RoutEnd;
017100110221
017200110221       //--------------------------------------------------------------
017300120511       // Operazioni iniziali.                                         ?
017400110221       //--------------------------------------------------------------
017500110221       begsr  sr_RoutInz;
017600110221
017700110221         *inLR = *on;
017800170125
017900170126         oDati  = *blank;
018000170126         oError = '0';
018100120518
018200120511         // ctrl parametri in ingresso
018300120511         exsr CtrlParmIn;
018400110221
018500110221       endsr;
018600110221
018700110221       //--------------------------------------------------------------
018800120511       // Controllo parametri in ingresso                             ?
018900110221       //--------------------------------------------------------------
019000120511       begsr  CtrlParmIn;
019100110221
019200170125         // i dati sono obbligatori
019300170125         if iDati = *blank;
019400170130           oError = '3';
019500170125           //oErrorD = 'ERRORE: il parametro iDati è obbligatorio';
019600120511           exsr sr_RoutEnd;
019700120511         endif;
019800170125
019900170125         // sposto i dati nella DS più appropriata per questa logica
020000170125         // in questo caso la 0600
020100170125         QUSL0600 = iDati;
020200120511
020300120511       endsr;
020400120511
020500120511       //--------------------------------------------------------------
020600170323       // Eseguo i test della logica 02
020700120511       //--------------------------------------------------------------
020800170323       begsr  ExeLog02;
020900170130
021000170323         // testo se la data creazione è più vecchia del numero "qualcosa" riportato
021100170323         // in tab. VPR key CTRLOBJ_LOG02
021200170323         if VPR_LOG02.Num <> *blank;
021300170323
021400170323           // data creazione
021500170323           if QUSCDT06 <> *blank;
021600170323             clear qusec;
021700170323             Qusbprv = %size(Qusec);
021800170323             // la data in DS è in formato system timestamp (*DTS) e va convertito
021900170323             ConvertDateAndTimeFormat('*DTS': QUSCDT06 : '*YYMD': CurDate:
022000170323                                      Qusec);
022100170323             // per errore esco
022200170323             if QUSEI <> *blank;
022300170323               oError = '2';
022400170323               exsr sr_RoutEnd;
022500170323             endif;
022600170323             Data08A = %char(CurDat);
022700170323             // valorizzo la variabile di appoggio
022800170323             wQUSCDT06 = Data08A;
022900170323             // controllo data rispetto a differenza da oggi
023000170323             // metto sotto monitor perché non so se il valore risultante è davvero una data
023100170323             monitor;
023200170323               select;
023300170323                when VPR_LOG02.Cosa = '*Y';
023400170323                 if %dec(VPR_LOG02.Num:3:0) >
023500170323                  %diff(%date() : %date(%dec(Data08A:8:0)) : *Y);
023600170323                   oError = '1';
023700170323                   exsr sr_RoutEnd;
023800170323                 endif;
023900170323                when VPR_LOG02.Cosa = '*M';
024000170323                 if %dec(VPR_LOG02.Num:3:0) >
024100170323                  %diff(%date() : %date(%dec(Data08A:8:0)) : *M);
024200170323                   oError = '1';
024300170323                   exsr sr_RoutEnd;
024400170323                 endif;
024500170323                when VPR_LOG02.Cosa = '*D';
024600170323                 if %dec(VPR_LOG02.Num:3:0) >
024700170323                  %diff(%date() : %date(%dec(Data08A:8:0)) : *D);
024800170323                   oError = '1';
024900170323                   exsr sr_RoutEnd;
025000170323                 endif;
025100170323               endsl;
025200170323             on-error;
025300170323               oError = '2';
025400170323               exsr sr_RoutEnd;
025500170323             endmon;
025600170323
025700170323             // devo passare indietro anche la data ultimo uso convertita
025800170503             // se i giorni di utilizzo sono > 0, altrimenti è sicuramente sbagliata
025900170503             if QUSDUC > 0;
026000170503               clear qusec;
026100170503               Qusbprv = %size(Qusec);
026200170503               // la data in DS è in formato system timestamp (*DTS) e va convertito
026300170503               ConvertDateAndTimeFormat('*DTS': QUSLUDT: '*YYMD': CurDate:
026400170503                                        Qusec);
026500170503               // per errore esco
026600170503               if QUSEI <> *blank;
026700170503                 oError = '2';
026800170503                 exsr sr_RoutEnd;
026900170503               endif;
027000170503               // se la data ottenuta è < di 1/1/1995
027100170503               // o > di oggi
027200170503               // vuol dire che è accaduto qualcosa di strano all'oggetto
027300170503               // e quindi passo all'oggetto successivo, segnalando la stranezza
027400170503               Data08A = %char(CurDat);
027500170503               if %dec(Data08A:8:0) < 19950101
027600170503                or %dec(Data08A:8:0) > %dec(%date() : *ISO);
027700170503                 oError = '2';
027800170503                 exsr sr_RoutEnd;
027900170503               endif;
028000170503
028100170503             // se i giorni di utilizzo sono 0, passo come data ultimo utilizzo quella di creazione
028200170503             else;
028300170503               Data08A = wQUSCDT06;
028400170503             endif;
028500170503             // se tutto a posto, valorizzo la variabile di appoggio
028600170503             wQUSLUDT = Data08A;
028700170126
028800170323           endif;
028900170131
029000170323         else;
029100170323           oError = '1';
029200170323           exsr sr_RoutEnd;
029300170323         endif;
029400170130
029500170131         // se arrivo qui vuol dire che l'oggetto rientra
029600170131         oDatiDS.USCUP01 = QUSCUP01;
029700170131         oDatiDS.USDUC = %subst(%editc(QUSDUC:'X') : 6 : 4);
029800170131         oDatiDS.USCDT06 = wQUSCDT06;
029900170131         oDatiDS.USLUDT  = wQUSLUDT;
030000170323         oDatiDS.Num     = VPR_LOG02.Num;
030100170323         oDatiDS.Cosa    = VPR_LOG02.Cosa;
030200170303         oDatiDS.QUSSFILN02 = QUSSFILN02;
030300170303         oDatiDS.QUSSFLN02  = QUSSFLN02;
030400170303         oDatiDS.QUSSFMN01  = QUSSFMN01;
030500170303
030600170303         // se l'attributo di un pgm è RPGLE o CLLE, reperisco i dati del sorgente da altra API
030700170303     C                   IF        QUSOBJTU04 = '*PGM' and
030800170303     C                             (QUSEOA03 = 'RPGLE' or
030900170303     C                              QUSEOA03 = 'CLLE')
031000170303     C                   CALL      'UBACTGPMR4'
031100170303     c                   parm                    QUSOBJNU04
031200170303     c                   parm                    QUSOLNU04
031300170303     C                   PARM                    SrcFileN
031400170303     C                   PARM                    SrcLibN
031500170303     C                   PARM                    SrcMbrN
031600170303     C* ecco i dati del sorgente
031700170303         oDatiDS.QUSSFILN02 = SrcFileN ;
031800170303         oDatiDS.QUSSFLN02  = SrcLibN  ;
031900170303         oDatiDS.QUSSFMN01  = SrcMbrN  ;
032000170303     C                   ENDIF
032100170303
032200170131         oDati = oDatiDS;
032300170131         oError = '0';
032400110221
032500110221       endsr;
032600120514
032700120514       //--------------------------------------------------------------
032800120514       // Operazioni finali.                                           ?
032900120514       //--------------------------------------------------------------
033000120514       begsr  sr_RoutEnd;
033100120514
033200170126         // Uscita dal *pgm?
033300170130         eval *inrt=*on;
033400110221         return;
033500110221
033600110221       endsr;
033700170126
033800170126       //--------------------------------------------------------------
033900170126       // *INZSR
034000170126       //--------------------------------------------------------------
034100170126       begsr  *inzsr;
034200170126
034300170323         // recupero dati da tab. VPR  key CTRLOBJ_LOG02
034400170130         // limite data ultimo utilizzo
034500170126         clear kpjbu;
034600170130         //T02mod = 'C';              (già così)?
034700170130         //T02cod = 'VPS';            (già così)?
034800170323         //T02ke1 = 'CTRLOBJ_LOG02';  (già così)
034900170130         //T02sif = knsif;            (non serve perché quasta è una tab. solo sede)
035000170126         TNTBE_RicercaControllo ( kpjba : tibs02ds );
035100170126         if T02err = *blank;
035200170323           VPR_LOG02 = %subst(T02uni:1:5);
035300170126         endif;
035400170126
035500170126       endsr;

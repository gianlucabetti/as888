000100110221       //==============================================================
000200170125       // UBLGOB01R // Logica 01 reperimento dati oggetto
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
001500170130     ** 1. Creare il modulo UBLGOB01R (Opz 15 PDM)
001600170130     ** 2. Creare/Aggiornare il programma di servizio UBLGOB01R (CRTSRVPGM / UPDSRVPGM)
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
008400170130     d VPR_LOG01       ds                  qualified
008500170130     d   Num                          3a
008600170130     d   Cosa                         2a
008700170130
008800170130       // Ricerca/Controllo tabelle (TNTBE)?
008900170130     d TIBS02ds      e ds                  inz
009000170130     d   T02mod      e                     inz('C')
009100170130     d   T02cod      e                     inz('VPR')
009200170130     d   T02ke1      e                     inz('CTRLOBJ_LOG01')
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
015200170130      /copy gaitrasrc/srcProtoPI,UBLGOB01R
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
016700170125       exsr  ExeLog01;
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
020600170125       // Eseguo i test della logica 01
020700120511       //--------------------------------------------------------------
020800170125       begsr  ExeLog01;
020900170130
021000170125         // se il numero giorni uso è 0
021100170125         if QUSDUC = 0;
021200170125           // l'oggetto ricade nella logica e passo indietro questi dati:
021300170125           // data creazione
021400170126           clear qusec;
021500170126           Qusbprv = %size(Qusec);
021600170126           // la data in DS è in formato system timestamp (*DTS) e va convertito
021700170130           ConvertDateAndTimeFormat('*DTS': QUSCDT06: '*YYMD': CurDate:
021800170126                                    Qusec);
021900170126           // per errore esco
022000170126           if QUSEI <> *blank;
022100170126             oError = '2';
022200170126             exsr sr_RoutEnd;
022300170126           endif;
022400170126           // se la data ottenuta è < di 1/1/1995
022500170126           // o > di oggi
022600170126           // vuol dire che è accaduto qualcosa di strano all'oggetto
022700170126           // e quindi passo all'oggetto successivo, segnalando la stranezza
022800170130           Data08A = %char(CurDat);
022900170126           if %dec(Data08A:8:0) < 19950101
023000170126             or %dec(Data08A:8:0) > %dec(%date() : *ISO);
023100170126             oError = '2';
023200170126             exsr sr_RoutEnd;
023300170126           endif;
023400170126           // se tutto a posto, valorizzo la variabile di appoggio
023500170126           wQUSCDT06 = Data08A;
023600170126
023700170126           // data ultimo uso
023800170126           if QUSLUDT <> *blank;
023900170126             clear qusec;
024000170126             Qusbprv = %size(Qusec);
024100170126             // la data in DS è in formato system timestamp (*DTS) e va convertito
024200170130             ConvertDateAndTimeFormat('*DTS': QUSLUDT: '*YYMD': CurDate:
024300170126                                      Qusec);
024400170126             // per errore esco
024500170126             if QUSEI <> *blank;
024600170126               oError = '2';
024700170126               exsr sr_RoutEnd;
024800170126             endif;
024900170126             // se i giorni d'uso sono 0 qua d'entro c'è una data ma assurda, la passo as is
025000170126             // se tutto a posto, valorizzo la variabile di appoggio
025100170130             Data08A = %char(CurDat);
025200170126             wQUSLUDT = Data08A;
025300170126           else;
025400170126             wQUSLUDT = *blank;
025500170126           endif;
025600170126
025700170126         // se il numero giorni uso > 0
025800170125         else;
025900170130           // testo se la data ultimo uso è più vecchia di oggi del numero "qualcosa" riportato
026000170126           // in tab. VPR key CTRLOBJ_LOG01
026100170130           if VPR_LOG01.Num <> *blank;
026200170126
026300170126             // data ultimo uso
026400170126             if QUSLUDT <> *blank;
026500170126               clear qusec;
026600170126               Qusbprv = %size(Qusec);
026700170126               // la data in DS è in formato system timestamp (*DTS) e va convertito
026800170130               ConvertDateAndTimeFormat('*DTS': QUSLUDT : '*YYMD': CurDate:
026900170126                                        Qusec);
027000170126               // per errore esco
027100170126               if QUSEI <> *blank;
027200170126                 oError = '2';
027300170126                 exsr sr_RoutEnd;
027400170126               endif;
027500170130               Data08A = %char(CurDat);
027600170131               // valorizzo la variabile di appoggio
027700170131               wQUSLUDT = Data08A;
027800170130               // controllo data rispetto a differenza da oggi
027900170126               // metto sotto monitor perché non so se il valore risultante è davvero una data
028000170126               monitor;
028100170130                 select;
028200170130                  when VPR_LOG01.Cosa = '*Y';
028300170131                   if %dec(VPR_LOG01.Num:3:0) >=
028400170131                    %diff(%date() : %date(%dec(Data08A:8:0)) : *Y);
028500170131                     oError = '1';
028600170130                     exsr sr_RoutEnd;
028700170130                   endif;
028800170130                  when VPR_LOG01.Cosa = '*M';
028900170131                   if %dec(VPR_LOG01.Num:3:0) >=
029000170131                    %diff(%date() : %date(%dec(Data08A:8:0)) : *M);
029100170131                     oError = '1';
029200170130                     exsr sr_RoutEnd;
029300170130                   endif;
029400170130                  when VPR_LOG01.Cosa = '*D';
029500170131                   if %dec(VPR_LOG01.Num:3:0) >=
029600170131                    %diff(%date() : %date(%dec(Data08A:8:0)) : *D);
029700170131                     oError = '1';
029800170130                     exsr sr_RoutEnd;
029900170130                   endif;
030000170130                 endsl;
030100170130               on-error;
030200170130                 oError = '2';
030300170130                 exsr sr_RoutEnd;
030400170126               endmon;
030500170131
030600170131               // devo passare indietro anche la data creazione convertita
030700170131              clear qusec;
030800170131              Qusbprv = %size(Qusec);
030900170131              // la data in DS è in formato system timestamp (*DTS) e va convertito
031000170131              ConvertDateAndTimeFormat('*DTS': QUSCDT06: '*YYMD': CurDate:
031100170131                                       Qusec);
031200170131              // per errore esco
031300170131              if QUSEI <> *blank;
031400170131                oError = '2';
031500170131                exsr sr_RoutEnd;
031600170131              endif;
031700170131              // se la data ottenuta è < di 1/1/1995
031800170131              // o > di oggi
031900170131              // vuol dire che è accaduto qualcosa di strano all'oggetto
032000170131              // e quindi passo all'oggetto successivo, segnalando la stranezza
032100170131              Data08A = %char(CurDat);
032200170131              if %dec(Data08A:8:0) < 19950101
032300170131                or %dec(Data08A:8:0) > %dec(%date() : *ISO);
032400170131                oError = '2';
032500170131                exsr sr_RoutEnd;
032600170131              endif;
032700170131              // se tutto a posto, valorizzo la variabile di appoggio
032800170131              wQUSCDT06 = Data08A;
032900170126
033000170126             endif;
033100170131
033200170131           else;
033300170131             oError = '1';
033400170131             exsr sr_RoutEnd;
033500170131           endif;
033600170130
033700170125         endif;
033800170131
033900170131         // se arrivo qui vuol dire che l'oggetto rientra
034000170131         oDatiDS.USCUP01 = QUSCUP01;
034100170131         oDatiDS.USDUC = %subst(%editc(QUSDUC:'X') : 6 : 4);
034200170131         oDatiDS.USCDT06 = wQUSCDT06;
034300170131         oDatiDS.USLUDT  = wQUSLUDT;
034400170131         oDatiDS.Num     = VPR_LOG01.Num;
034500170131         oDatiDS.Cosa    = VPR_LOG01.Cosa;
034600170303         oDatiDS.QUSSFILN02 = QUSSFILN02;
034700170303         oDatiDS.QUSSFLN02  = QUSSFLN02;
034800170303         oDatiDS.QUSSFMN01  = QUSSFMN01;
034900170303
035000170303         // se l'attributo di un pgm è RPGLE o CLLE, reperisco i dati del sorgente da altra API
035100170303     C                   IF        QUSOBJTU04 = '*PGM' and
035200170303     C                             (QUSEOA03 = 'RPGLE' or
035300170303     C                              QUSEOA03 = 'CLLE')
035400170303     C                   CALL      'UBACTGPMR4'
035500170303     c                   parm                    QUSOBJNU04
035600170303     c                   parm                    QUSOLNU04
035700170303     C                   PARM                    SrcFileN
035800170303     C                   PARM                    SrcLibN
035900170303     C                   PARM                    SrcMbrN
036000170303     C* ecco i dati del sorgente
036100170303         oDatiDS.QUSSFILN02 = SrcFileN ;
036200170303         oDatiDS.QUSSFLN02  = SrcLibN  ;
036300170303         oDatiDS.QUSSFMN01  = SrcMbrN  ;
036400170303     C                   ENDIF
036500170303
036600170131         oDati = oDatiDS;
036700170131         oError = '0';
036800110221
036900110221       endsr;
037000120514
037100120514       //--------------------------------------------------------------
037200120514       // Operazioni finali.                                           ?
037300120514       //--------------------------------------------------------------
037400120514       begsr  sr_RoutEnd;
037500120514
037600170126         // Uscita dal *pgm?
037700170130         eval *inrt=*on;
037800110221         return;
037900110221
038000110221       endsr;
038100170126
038200170126       //--------------------------------------------------------------
038300170126       // *INZSR
038400170126       //--------------------------------------------------------------
038500170126       begsr  *inzsr;
038600170126
038700170126         // recupero dati da tab. VPR  key CTRLOBJ_LOG01
038800170130         // limite data ultimo utilizzo
038900170126         clear kpjbu;
039000170130         //T02mod = 'C';              (già così)?
039100170130         //T02cod = 'VPS';            (già così)?
039200170130         //T02ke1 = 'CTRLOBJ_LOG01';  (già così)
039300170130         //T02sif = knsif;            (non serve perché quasta è una tab. solo sede)
039400170126         TNTBE_RicercaControllo ( kpjba : tibs02ds );
039500170126         if T02err = *blank;
039600170130           VPR_LOG01 = %subst(T02uni:1:5);
039700170126         endif;
039800170126
039900170126       endsr;

000100091228       //==============================================================
000200160223       // Driver reperimento dati del collo
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
001400160217     ** 1. Creare il modulo UBCOL00R (Opz 15 PDM)
001500160217     ** 2. Creare/Aggiornare il programma di servizio UBCOL00R (CRTSRVPGM / UPDSRVPGM)
001600160218     **    con ACTGRP(*caller) e BNDDIR('UBRTVNETA')
001700160218     ** 2.1   aggiungere nella BNDDIR(UBBNDDIR)
001800160217     ** 3. Creare/Aggiornare il programma UBCOL00R (CRTPGM / UPDPGM)
001900160218     **    con ACTGRP(*caller) e BNDDIR('UBRTVNETA')
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
003300160218
003400160219     FFNBLP01L  IF   E           K DISK    extfile(wLibFileBLP)  usropn
003500160219     F                                     extdesc('FILTRAPRD/FNBLP01L')
003600160218     FFNARB01L  IF   E           K DISK    extfile(wLibFileARB)  usropn
003700160219     F                                     extdesc('FILTRAPRD/FNARB01L')
003800160218     FFNBLT01L  IF   E           K DISK    extfile(wLibFileBLT)  usropn
003900160219     F                                     extdesc('FILTRAPRD/FNBLT01L')
004000160218     FFNEVB04L  IF   E           K DISK    extfile(wLibFileEVB)  usropn
004100160219     F                                     extdesc('UNITRAGRP/FNEVB04L')
004200160218     FFIARS01L  IF   E           K DISK    extfile(wLibFileARS)  usropn
004300160219     F                                     extdesc('FILTRAPRD/FIARS01L')
004400160218     FFNBRV07L  IF   E           K DISK    extfile(wLibFileBRV)  usropn
004500160219     F                                     extdesc('FILTRAPRD/FNBRV07L')
004600091223       //--------------------------------------------------------------
004700121106       // Definizione costanti.
004800091223       //--------------------------------------------------------------
004900100302
005000160217     d c_Dim           c                   const(200)
005100091223
005200091223       //--------------------------------------------------------------
005300121106       // Definizione schiere.
005400091223       //--------------------------------------------------------------
005500091223
005600160217     d pOut_SkBRT      s             18a
005700160217     d                                     dim(c_Dim)
005800160217     d pOut_SkCLI      s             35a
005900160217     d                                     dim(c_Dim)
006000160217     d pOut_SkDCM      s              8s 0
006100160217     d                                     dim(c_Dim)
006200160217     d pOut_SkHMC      s              4s 0
006300160217     d                                     dim(c_Dim)
006400160217     d pOut_SkDFS      s              8s 0
006500160217     d                                     dim(c_Dim)
006600160217     d pOut_SkHMS      s              6s 0
006700160217     d                                     dim(c_Dim)
006800160217     d pOut_SkVUC      s              7s 6
006900160217     d                                     dim(c_Dim)
007000160217     d pOut_SkPUC      s              7s 3
007100160217     d                                     dim(c_Dim)
007200091223
007300091223       //--------------------------------------------------------------
007400121106       // Definizione aree dati.
007500091223       //--------------------------------------------------------------
007600091223
007700091223
007800140724       //--------------------------------------------------------------
007900140724       // Definizione strutture dati.
008000140724       //--------------------------------------------------------------
008100140724
008200121106       // - Status
008300110516     d Psds           sds
008400110516     d   SDSpgm          *proc
008500121106     d   JobName             244    253                                         Job name
008600121106     d   JobUser             254    263                                         User name
008700121106     d   JobNumber           264    269s 0                                      Job number
008800160219
008900160219       // Calcolo check digit
009000160219     D TRUL28DS      E DS
009100091223
009200091223       //--------------------------------------------------------------
009300121106       // Definizione variabili.
009400091223       //--------------------------------------------------------------
009500091223
009600121106       // - Parametri ricevuti:
009700160217     d pIn_Opz         s              3a
009800160217     d pIn_FlgOpe      s             20a
009900160202       // tipo lancio: 'C'=LR   *blank=RT
010000160202     d pIn_TLa         s              1a
010100160202     d pIn_AAS         s              4s 0
010200160202     d pIn_LNP         s              3s 0
010300160202     d pIn_NRS         s              2s 0
010400160202     d pIn_NSP         s              7s 0
010500160217     d pOut_NCL        s              5s 0
010600160217       // tipo consegna: TT=Totale  PP=Parziale  TP=Consegnata con parziale  '  '=nessuna consegna
010700160217     d pOut_TIP        s              2a
010800160217     d pOut_DCM        s              8s 0
010900160217     d pOut_HMC        s              4s 0
011000160217     d* vedi schiere
011100160217       // esito: 0=OK  1=Err input  2=KO
011200160217     d pOut_Err        s              1s 0
011300121106
011400121106       // Campi di comodo
011500160217     d wBarcodeI       s             18    inz
011600160217     d wBarcodeO       s             18    inz
011700160217     D wDCM            s                   inz like(ARBDCM)
011800160217     D wDCP            s                   inz like(ARBDCP)
011900160217     D wHMC            s                   inz like(ARBHMC)
012000160217     d wDate           s              8  0 inz
012100160217     d nrCollo         s              3s 0 inz
012200160217     d wArrivo         s               n   inz
012300160218       // Nome della libreria in cui è il file FNBRV00F
012400160218     d wLibFileBRV     s             21a   inz
012500160218       // Nome della libreria in cui è il file FNBLP00F
012600160218     d wLibFileBLP     s             21a   inz
012700160218       // Nome della libreria in cui è il file FNBLT00F
012800160218     d wLibFileBLT     s             21a   inz
012900160218       // Nome della libreria in cui è il file FNARB00F
013000160218     d wLibFileARB     s             21a   inz
013100160218       // Nome della libreria in cui è il file FIARS00F
013200160218     d wLibFileARS     s             21a   inz
013300160218       // Nome della libreria in cui è il file FNEVB00F
013400160218     d wLibFileEVB     s             21a   inz
013500160218       // Nome del sistema
013600160218     d currSysNeta     s              8a   inz
013700091223
013800091223       //--------------------------------------------------------------
013900121106       // Definizione prototipi procedure.
014000091223       //--------------------------------------------------------------
014100160218
014200160218       // Reperimento NETA sistema AS/400 corrente
014300160218     d/copy gaitrasrc/srcProtoPr,UBRTVNETA
014400130307
014500091223       //--------------------------------------------------------------
014600121106       // Definizione key-list.
014700091223       //--------------------------------------------------------------
014800091223
014900091223
015000091223       //--------------------------------------------------------------
015100121107       // Definizione parametri procedura.
015200091223       //--------------------------------------------------------------
015300091223
015400091223     c     *Entry        plist
015500160217     c                   parm                    pIn_Opz
015600160217     c                   parm                    pIn_FlgOpe
015700160217     c                   parm                    pIn_TLa
015800160217     c                   parm                    pIn_AAS
015900160217     c                   parm                    pIn_LNP
016000160217     c                   parm                    pIn_NRS
016100160217     c                   parm                    pIn_NSP
016200160217     c                   parm                    pOut_NCL
016300160217     c                   parm                    pOut_TIP
016400160217     c                   parm                    pOut_DCM
016500160217     c                   parm                    pOut_HMC
016600160217     c                   parm                    pOut_SkBRT
016700160217     c                   parm                    pOut_SkCLI
016800160217     c                   parm                    pOut_SkDCM
016900160217     c                   parm                    pOut_SkHMC
017000160217     c                   parm                    pOut_SkDFS
017100160217     c                   parm                    pOut_SkHMS
017200160217     c                   parm                    pOut_SkVUC
017300160217     c                   parm                    pOut_SkPUC
017400160217     c                   parm                    pOut_Err
017500091223
017600130307      /free
017700091223
017800091223       //--------------------------------------------------------------
017900121106       // M A I N - L I N E
018000091223       //--------------------------------------------------------------
018100140806
018200130307       // Operazioni iniziali?
018300130307       exsr sr_RoutInz;
018400130307
018500140724       // attività richiesta dal chiamante se non chiamato per chiusura
018600160202       if pIn_TLa = *blank;
018700160217         exsr esegui;
018800140724       endif;
018900091223
019000121106       // Operazioni finali?
019100091223       exsr sr_RoutEnd;
019200091223
019300091223       //--------------------------------------------------------------
019400121107       // Operazioni iniziali.
019500091223       //--------------------------------------------------------------
019600091223       BEGSR  sr_RoutInz;
019700160218
019800160218       // Verifica del sistema AS/400 corrente
019900160218       if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
020000160218         // esco dando errore
020100160218         pOut_Err = 1;
020200160218         exsr sr_RoutEnd;
020300160218       endif;
020400160218
020500160218       // Apertura archivi
020600160218       if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
020700160218         wLibFileBRV = 'FILTRA201/FNBRV07L';
020800160218         wLibFileBLP = 'FILTRA201/FNBLP01L';
020900160218         wLibFileBLT = 'FILTRA201/FNBLT01L';
021000160218         wLibFileARB = 'FILTRA201/FNARB01L';
021100160218         wLibFileARS = 'FILTRA201/FIARS01L';
021200160218         wLibFileEVB = 'UNITRAGRU/FNEVB04L';
021300160218       else;
021400160218         wLibFileBRV = 'FILTRAPRD/FNBRV07L';
021500160218         wLibFileBLP = 'FILTRAPRD/FNBLP01L';
021600160218         wLibFileBLT = 'FILTRAPRD/FNBLT01L';
021700160218         wLibFileARB = 'FILTRAPRD/FNARB01L';
021800160218         wLibFileARS = 'FILTRAPRD/FIARS01L';
021900160218         wLibFileEVB = 'UNITRAGRP/FNEVB04L';
022000160218       endif;
022100160218
022200160218       if  not %open(FNBRV07L);
022300160218         open  FNBRV07L;
022400160218       endif;
022500160218       if  not %open(FNBLP01L);
022600160218         open  FNBLP01L;
022700160218       endif;
022800160218       if  not %open(FNBLT01L);
022900160218         open  FNBLT01L;
023000160218       endif;
023100160218       if  not %open(FNARB01L);
023200160218         open  FNARB01L;
023300160218       endif;
023400160218       if  not %open(FIARS01L);
023500160218         open  FIARS01L;
023600160218       endif;
023700160218       if  not %open(FNEVB04L);
023800160218         open  FNEVB04L;
023900160218       endif;
024000121108
024100121108         // Reperimento data odierna (fmt aaaa/mm/gg)
024200121108         wDate = %dec( %date() );
024300160217         clear pOut_NCL     ;
024400160217         clear pOut_TIP     ;
024500160217         clear pOut_DCM     ;
024600160217         clear pOut_HMC     ;
024700160217         clear pOut_SkBRT   ;
024800160217         clear pOut_SkCLI   ;
024900160217         clear pOut_SkDCM   ;
025000160217         clear pOut_SkHMC   ;
025100160217         clear pOut_SkDFS   ;
025200160217         clear pOut_SkHMS   ;
025300160217         clear pOut_SkVUC   ;
025400160217         clear pOut_SkPUC   ;
025500160217         clear pOut_Err     ;
025600160218
025700160218         // Inizializzazione variabili
025800160218         nrCollo = 0;
025900160202
026000160202         // Controllo i parametri di input
026100160202         exsr sr_CtrlPIn;
026200160219
026300160219         // se devo eseguire e non chiudere
026400160219         if pIn_TLa = *blank;
026500160219           // Inizializzo risultato a errore
026600160219           pOut_Err = 2;
026700160219         endif;
026800160202
026900160202       ENDSR;
027000160202
027100160202       //--------------------------------------------------------------
027200160202       // Controllo i parametri di input
027300160202       //--------------------------------------------------------------
027400160202       BEGSR  sr_CtrlPIn;
027500140724
027600160217         // Associazione opzione - flag operativi
027700160217         // se non chiusura
027800160217         if pIn_TLa <>'C';
027900160217           select;
028000160223            // oggi vengono gestite solo le opzioni:
028100160223            // ' ' o 'STD' che forzano il primo flag operativo
028200160223            // le altre danno errore
028300160223            when pIn_Opz <> *blank and
028400160223                  pIn_Opz <> 'STD';
028500160223             // esco dando errore
028600160223             pOut_Err = 1;
028700160223             exsr sr_RoutEnd;
028800160217           endsl;
028900160217         endif;
029000160202
029100160202         // Tipo lancio deve essere blank o 'C'
029200160202         if pIn_TLa <> *blank and pIn_TLa <>'C';
029300160202           // esco dando errore
029400160202           pOut_Err = 1;
029500160202           exsr sr_RoutEnd;
029600140724         endif;
029700160202
029800160205         // Anno spedizione deve essere valorizzato (se non chiusura)
029900160205         if pIn_AAS <= 0 and pIn_TLa <>'C';
030000160202           // esco dando errore
030100160202           pOut_Err = 1;
030200160202           exsr sr_RoutEnd;
030300160202         endif;
030400160202
030500160205         // Linea di partenza deve essere valorizzato (se non chiusura)
030600160205         if pIn_LNP <= 0 and pIn_TLa <>'C';
030700160202           // esco dando errore
030800160202           pOut_Err = 1;
030900160202           exsr sr_RoutEnd;
031000160202         endif;
031100160202
031200160205         // Nr. Spedizione deve essere valorizzato (se non chiusura)
031300160205         if pIn_NSP <= 0 and pIn_TLa <>'C';
031400160202           // esco dando errore
031500160202           pOut_Err = 1;
031600160202           exsr sr_RoutEnd;
031700160202         endif;
031800091223
031900091223       ENDSR;
032000121106
032100091223       //--------------------------------------------------------------
032200160217       // Esecuzione
032300091223       //--------------------------------------------------------------
032400160217       BEGSR  esegui;
032500140724
032600160217         // reperisco partenza
032700160219         chain  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP)  FNBLP000;
032800160217         if not %found(fnblp01l);
032900160217           // esco dando errore
033000160219           pOut_Err = 2;
033100160217           exsr sr_RoutEnd;
033200160217         endif;
033300160217
033400160217         // reperisco arrivo
033500160219         chain  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP)  FNARB000;
033600160217         if not %found(fnarb01l);
033700160217           // l'arrivo potrebbe non esistere
033800160217           wArrivo = *off;
033900160217         else;
034000160217           wArrivo = *on;
034100160217         endif;
034200160217
034300160217         // salvo in variabili i dati sia in arrivo che partenza (quando il primo non esiste)
034400160217         if wArrivo = *on;
034500160217           wDCM = ARBDCM;
034600160217           wDCP = ARBDCP;
034700160217           wHMC = ARBHMC;
034800160217         else;
034900160217           wDCM = BLPDCM;
035000160217           wDCP = BLPDCP;
035100160217           wHMC = BLPHMC;
035200160217         endif;
035300160217
035400160217         // leggo colli partiti
035500160219         chain  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP)  FNBLT000;
035600160217         if not %found(fnblt01l);
035700160217           // esco dando errore
035800160217           pOut_Err = 1;
035900160217           exsr sr_RoutEnd;
036000160217         else;
036100160218           dou %eof(fnblt01l);
036200160218             // incremento l'indice del numero Collo
036300160218             nrCollo += 1;
036400160218             // valorizzo i dati in skiera per quel collo
036500160218             pOut_SkVUC(nrCollo) = BLTVUC;
036600160218             pOut_SkPUC(nrCollo) = BLTPUC;
036700160218             pOut_SkDCM(nrCollo) = BLTDCM;
036800160218             // controllo se c'è stata una consegna parziale
036900160218             if wDCP = 0;
037000160218               // se non c'è stata allora ho anche una sola ora di consegna
037100160218               pOut_SkHMC(nrCollo) = wHMC;
037200160218             else;
037300160218               // se c'è stata una consegna parziale reperisco gli eventi che l'hanno scatenata
037400160219               chain  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP:'P':BLTDCM)  FNEVB000;
037500160218               if %found(fnevb04l);
037600160218                 pOut_SkHMC(nrCollo) = EVBHEV;
037700160218                 pOut_Tip = 'P';
037800160218               endif;
037900160218             endif;
038000160218             // calcolo il segnacollo
038100160218             wBarcodeI = %editc(BLTLNP:'X') +
038200160217                       %editc(BLTLNA:'X') +
038300160217                       %editc(BLTNRS:'X') +
038400160217                       %editc(BLTNSC:'X') +
038500160217                       %editc(BLPZNC:'X')  ;
038600160218             exsr calbarcode;
038700160218             pOut_SkBRT(nrCollo) = wBarcodeO;
038800160218             // reperisco il segnacollo cliente
038900160219             chain  (BLTFLS:BLTLNA:BLTNRS:BLTNSC:'C')  FIARS000;
039000160218             if %found(fiars01l);
039100160218               pOut_SkCLI(nrCollo) = ARSNOT;
039200160218             endif;
039300160226             // reperisco la spunta come da opzione
039400160219             chain  (BLTFLS:BLTLNA:BLTNRS:BLTNSC)  FNBRV000;
039500160218             if %found(fnbrv07l);
039600160218               dou %eof(fnbrv07l);
039700160226                 // opzione standartd
039800160223                 if pIn_Opz = *blank or pIn_Opz = 'STD';
039900160226                   // reperisco data/ora di spunta più bassa tra tutte quelle effettuate
040000160226                   if BRVDFS < pOut_SkDFS(nrCollo)           or
040100160226                      (BRVDFS = pOut_SkDFS(nrCollo) and
040200160226                       BRVHMS < pOut_SkHMS(nrCollo))         or
040300160226                      (pOut_SkDFS(nrCollo) = 0 and
040400160226                       pOut_SkHMS(nrCollo) = 0);
040500160218                     pOut_SkDFS(nrCollo) = BRVDFS;
040600160218                     pOut_SkHMS(nrCollo) = BRVHMS;
040700160218                   endif;
040800160218                 endif;
040900160219                 reade  (BLTFLS:BLTLNA:BLTNRS:BLTNSC)  FNBRV000;
041000160218               enddo;
041100160218             endif;
041200160217
041300160219             reade  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP)  FNBLT000;
041400160218           enddo;
041500160218         endif;
041600160217
041700160217         // valorizzazioni finali
041800160217         pOut_NCL = BLPNCL;
041900160217         select;
042000160217          when pOut_TIP = *blank and
042100160217           wDCM > 0            and
042200160217           wDCP = 0;
042300160217           pOut_TIP = 'TT';
042400160217          when pOut_TIP = *blank and
042500160217           wDCM = 0            and
042600160217           wDCP = 0;
042700160217           pOut_TIP = *blank;
042800160217          when wDCM > 0       and
042900160217           (pOut_TIP = 'P' or wDCP > 0);
043000160217           pOut_TIP = 'TP';
043100160217          when wDCM = 0       and
043200160217           (pOut_TIP = 'P' or wDCP > 0);
043300160217           pOut_TIP = 'PP';
043400160218         endsl;
043500160217         pOut_DCM = wDCM;
043600160217         pOut_HMC = wHMC;
043700160218         // se il tipo consegna risulta TP (alcune consegne parziali, ma alla fine completata)
043800160218         // si può valorizzare l'ora consegna merce colli con l'ora di consegna spedizione
043900160218         if pOut_TIP = 'TP';
044000160218           // ciclo sulla schiera delle ore consegna merce colli per i colli della spedizione
044100160218           // valorizzando gli elementi a 0 con l'ora della consegna finale
044200160218           for nrCollo = 1 to pOut_NCL ;
044300160218             if pOut_SkHMC(nrCollo) = 0;
044400160218               pOut_SkHMC(nrCollo) = pOut_HMC;
044500160218             endif;
044600160218           endfor;
044700160218         endif;
044800160217         pOut_Err = 0   ;
044900160217
045000160217       ENDSR;
045100160217
045200160217       //--------------------------------------------------------------
045300160217       // Operazioni finali.
045400160217       //--------------------------------------------------------------
045500160217       BEGSR  sr_RoutEnd;
045600160217
045700140724         // Chiusura pgm
045800160202         if pIn_TLa = 'C';
045900140724           *inlr = *on;
046000140724         else;
046100140724           *inrt = *on;
046200140724         endif;
046300140724         return;
046400091223
046500091223       ENDSR;
046600091223
046700091223      /end-free
046800160217
046900160217     C     calbarcode    BEGSR
047000160217     C*
047100160217     C* Inizializzo la ds d procedura x calcolo barcode standard BRT e variabili d wrk
047200160217     C                   clear                   TRUL28DS
047300160217     C                   clear                   wBarcodeO
047400160217     C*
047500160217     C* Valorizzo il buffer d input
047600160217     C                   eval      I28TLA = 'E'
047700160217     C                   eval      I28MOD = 'BAR'
047800160217     C                   eval      I28COD = wBarcodeI
047900160217     C                   call      'TRUL28R1'
048000160217     C                   parm                    TRUL28DS
048100160217     C                   if        O28ERR = *blanks
048200160217     C                   eval      wBarcodeO = O28COD
048300160217     C                   endif
048400160217     C*
048500160217     C                   ENDSR

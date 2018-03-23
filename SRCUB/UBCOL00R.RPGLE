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
005000161201     d c_Dim           c                   const(300)
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
009100160627
009200160627     d SortA_DS        DS                  qualified
009300160627     d                                     dim(c_Dim)
009400160627     d SkBRT                         18a
009500160627     d SkCLI                         35a
009600160627     d SkDCM                          8s 0
009700160627     d SkHMC                          4s 0
009800160627     d SkDFS_HMS                     14a
009900160627     d SkVUC                          7s 6
010000160627     d SkPUC                          7s 3
010100091223
010200091223       //--------------------------------------------------------------
010300121106       // Definizione variabili.
010400091223       //--------------------------------------------------------------
010500091223
010600121106       // - Parametri ricevuti:
010700160217     d pIn_Opz         s              3a
010800160217     d pIn_FlgOpe      s             20a
010900160202       // tipo lancio: 'C'=LR   *blank=RT
011000160202     d pIn_TLa         s              1a
011100160202     d pIn_AAS         s              4s 0
011200160202     d pIn_LNP         s              3s 0
011300160202     d pIn_NRS         s              2s 0
011400160202     d pIn_NSP         s              7s 0
011500160217     d pOut_NCL        s              5s 0
011600160217       // tipo consegna: TT=Totale  PP=Parziale  TP=Consegnata con parziale  '  '=nessuna consegna
011700160217     d pOut_TIP        s              2a
011800160217     d pOut_DCM        s              8s 0
011900160217     d pOut_HMC        s              4s 0
012000160217     d* vedi schiere
012100160217       // esito: 0=OK  1=Err input  2=KO
012200160217     d pOut_Err        s              1s 0
012300121106
012400121106       // Campi di comodo
012500160217     d wBarcodeI       s             18    inz
012600160217     d wBarcodeO       s             18    inz
012700160217     D wDCM            s                   inz like(ARBDCM)
012800160217     D wDCP            s                   inz like(ARBDCP)
012900160217     D wHMC            s                   inz like(ARBHMC)
013000160217     d wDate           s              8  0 inz
013100160217     d nrCollo         s              3s 0 inz
013200160217     d wArrivo         s               n   inz
013300160218       // Nome della libreria in cui è il file FNBRV00F
013400160218     d wLibFileBRV     s             21a   inz
013500160218       // Nome della libreria in cui è il file FNBLP00F
013600160218     d wLibFileBLP     s             21a   inz
013700160218       // Nome della libreria in cui è il file FNBLT00F
013800160218     d wLibFileBLT     s             21a   inz
013900160218       // Nome della libreria in cui è il file FNARB00F
014000160218     d wLibFileARB     s             21a   inz
014100160218       // Nome della libreria in cui è il file FIARS00F
014200160218     d wLibFileARS     s             21a   inz
014300160218       // Nome della libreria in cui è il file FNEVB00F
014400160218     d wLibFileEVB     s             21a   inz
014500160218       // Nome del sistema
014600160218     d currSysNeta     s              8a   inz
014700091223
014800091223       //--------------------------------------------------------------
014900121106       // Definizione prototipi procedure.
015000091223       //--------------------------------------------------------------
015100160218
015200160218       // Reperimento NETA sistema AS/400 corrente
015300160218     d/copy gaitrasrc/srcProtoPr,UBRTVNETA
015400130307
015500091223       //--------------------------------------------------------------
015600121106       // Definizione key-list.
015700091223       //--------------------------------------------------------------
015800091223
015900091223
016000091223       //--------------------------------------------------------------
016100121107       // Definizione parametri procedura.
016200091223       //--------------------------------------------------------------
016300091223
016400091223     c     *Entry        plist
016500160217     c                   parm                    pIn_Opz
016600160217     c                   parm                    pIn_FlgOpe
016700160217     c                   parm                    pIn_TLa
016800160217     c                   parm                    pIn_AAS
016900160217     c                   parm                    pIn_LNP
017000160217     c                   parm                    pIn_NRS
017100160217     c                   parm                    pIn_NSP
017200160217     c                   parm                    pOut_NCL
017300160217     c                   parm                    pOut_TIP
017400160217     c                   parm                    pOut_DCM
017500160217     c                   parm                    pOut_HMC
017600160217     c                   parm                    pOut_SkBRT
017700160217     c                   parm                    pOut_SkCLI
017800160217     c                   parm                    pOut_SkDCM
017900160217     c                   parm                    pOut_SkHMC
018000160217     c                   parm                    pOut_SkDFS
018100160217     c                   parm                    pOut_SkHMS
018200160217     c                   parm                    pOut_SkVUC
018300160217     c                   parm                    pOut_SkPUC
018400160217     c                   parm                    pOut_Err
018500091223
018600130307      /free
018700091223
018800091223       //--------------------------------------------------------------
018900121106       // M A I N - L I N E
019000091223       //--------------------------------------------------------------
019100140806
019200130307       // Operazioni iniziali?
019300130307       exsr sr_RoutInz;
019400130307
019500140724       // attività richiesta dal chiamante se non chiamato per chiusura
019600160202       if pIn_TLa = *blank;
019700160217         exsr esegui;
019800140724       endif;
019900091223
020000121106       // Operazioni finali?
020100091223       exsr sr_RoutEnd;
020200091223
020300091223       //--------------------------------------------------------------
020400121107       // Operazioni iniziali.
020500091223       //--------------------------------------------------------------
020600091223       BEGSR  sr_RoutInz;
020700160218
020800160218       // Verifica del sistema AS/400 corrente
020900160218       if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
021000160218         // esco dando errore
021100160218         pOut_Err = 1;
021200160218         exsr sr_RoutEnd;
021300160218       endif;
021400160218
021500160218       // Apertura archivi
021600160218       if %subst ( currSysNeta : 1 : 6 ) = 'SETRAS';
021700160218         wLibFileBRV = 'FILTRA201/FNBRV07L';
021800160218         wLibFileBLP = 'FILTRA201/FNBLP01L';
021900160218         wLibFileBLT = 'FILTRA201/FNBLT01L';
022000160218         wLibFileARB = 'FILTRA201/FNARB01L';
022100160218         wLibFileARS = 'FILTRA201/FIARS01L';
022200160218         wLibFileEVB = 'UNITRAGRU/FNEVB04L';
022300160218       else;
022400160218         wLibFileBRV = 'FILTRAPRD/FNBRV07L';
022500160218         wLibFileBLP = 'FILTRAPRD/FNBLP01L';
022600160218         wLibFileBLT = 'FILTRAPRD/FNBLT01L';
022700160218         wLibFileARB = 'FILTRAPRD/FNARB01L';
022800160218         wLibFileARS = 'FILTRAPRD/FIARS01L';
022900160218         wLibFileEVB = 'UNITRAGRP/FNEVB04L';
023000160218       endif;
023100160218
023200160218       if  not %open(FNBRV07L);
023300160218         open  FNBRV07L;
023400160218       endif;
023500160218       if  not %open(FNBLP01L);
023600160218         open  FNBLP01L;
023700160218       endif;
023800160218       if  not %open(FNBLT01L);
023900160218         open  FNBLT01L;
024000160218       endif;
024100160218       if  not %open(FNARB01L);
024200160218         open  FNARB01L;
024300160218       endif;
024400160218       if  not %open(FIARS01L);
024500160218         open  FIARS01L;
024600160218       endif;
024700160218       if  not %open(FNEVB04L);
024800160218         open  FNEVB04L;
024900160218       endif;
025000121108
025100121108         // Reperimento data odierna (fmt aaaa/mm/gg)
025200121108         wDate = %dec( %date() );
025300160217         clear pOut_NCL     ;
025400160217         clear pOut_TIP     ;
025500160217         clear pOut_DCM     ;
025600160217         clear pOut_HMC     ;
025700160217         clear pOut_SkBRT   ;
025800160217         clear pOut_SkCLI   ;
025900160217         clear pOut_SkDCM   ;
026000160217         clear pOut_SkHMC   ;
026100160217         clear pOut_SkDFS   ;
026200160217         clear pOut_SkHMS   ;
026300160217         clear pOut_SkVUC   ;
026400160217         clear pOut_SkPUC   ;
026500160627         clear pOut_Err     ;
026600160627         clear SortA_DS;
026700160627         //clear SortA_DS.SkBRT;
026800160627         //clear SortA_DS.SkCLI;
026900160627         //clear SortA_DS.SkDCM;
027000160627         //clear SortA_DS.SkHMC;
027100160627         //clear SortA_DS.SkDFS_HMS;
027200160627         //clear SortA_DS.SkVUC   ;
027300160627         //clear SortA_DS.SkPUC   ;
027400160218
027500160218         // Inizializzazione variabili
027600160218         nrCollo = 0;
027700160202
027800160202         // Controllo i parametri di input
027900160202         exsr sr_CtrlPIn;
028000160219
028100160219         // se devo eseguire e non chiudere
028200160219         if pIn_TLa = *blank;
028300160219           // Inizializzo risultato a errore
028400160219           pOut_Err = 2;
028500160219         endif;
028600160202
028700160202       ENDSR;
028800160202
028900160202       //--------------------------------------------------------------
029000160202       // Controllo i parametri di input
029100160202       //--------------------------------------------------------------
029200160202       BEGSR  sr_CtrlPIn;
029300140724
029400160217         // Associazione opzione - flag operativi
029500160217         // se non chiusura
029600160217         if pIn_TLa <>'C';
029700160217           select;
029800160223            // oggi vengono gestite solo le opzioni:
029900160223            // ' ' o 'STD' che forzano il primo flag operativo
030000160223            // le altre danno errore
030100160223            when pIn_Opz <> *blank and
030200160705                  pIn_Opz <> 'STD' and
030300160705                  pIn_Opz <> 'FV5';
030400160223             // esco dando errore
030500160223             pOut_Err = 1;
030600160223             exsr sr_RoutEnd;
030700160217           endsl;
030800160217         endif;
030900160202
031000160202         // Tipo lancio deve essere blank o 'C'
031100160202         if pIn_TLa <> *blank and pIn_TLa <>'C';
031200160202           // esco dando errore
031300160202           pOut_Err = 1;
031400160202           exsr sr_RoutEnd;
031500140724         endif;
031600160202
031700160205         // Anno spedizione deve essere valorizzato (se non chiusura)
031800160205         if pIn_AAS <= 0 and pIn_TLa <>'C';
031900160202           // esco dando errore
032000160202           pOut_Err = 1;
032100160202           exsr sr_RoutEnd;
032200160202         endif;
032300160202
032400160205         // Linea di partenza deve essere valorizzato (se non chiusura)
032500160205         if pIn_LNP <= 0 and pIn_TLa <>'C';
032600160202           // esco dando errore
032700160202           pOut_Err = 1;
032800160202           exsr sr_RoutEnd;
032900160202         endif;
033000160202
033100160205         // Nr. Spedizione deve essere valorizzato (se non chiusura)
033200160205         if pIn_NSP <= 0 and pIn_TLa <>'C';
033300160202           // esco dando errore
033400160202           pOut_Err = 1;
033500160202           exsr sr_RoutEnd;
033600160202         endif;
033700091223
033800091223       ENDSR;
033900121106
034000091223       //--------------------------------------------------------------
034100160217       // Esecuzione
034200091223       //--------------------------------------------------------------
034300160217       BEGSR  esegui;
034400140724
034500160217         // reperisco partenza
034600160219         chain  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP)  FNBLP000;
034700160217         if not %found(fnblp01l);
034800160217           // esco dando errore
034900160219           pOut_Err = 2;
035000160217           exsr sr_RoutEnd;
035100160217         endif;
035200160217
035300160217         // reperisco arrivo
035400160219         chain  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP)  FNARB000;
035500160217         if not %found(fnarb01l);
035600160217           // l'arrivo potrebbe non esistere
035700160217           wArrivo = *off;
035800160217         else;
035900160217           wArrivo = *on;
036000160217         endif;
036100160217
036200160217         // salvo in variabili i dati sia in arrivo che partenza (quando il primo non esiste)
036300160217         if wArrivo = *on;
036400160217           wDCM = ARBDCM;
036500160217           wDCP = ARBDCP;
036600160217           wHMC = ARBHMC;
036700160217         else;
036800160217           wDCM = BLPDCM;
036900160217           wDCP = BLPDCP;
037000160217           wHMC = BLPHMC;
037100160217         endif;
037200160217
037300160217         // leggo colli partiti
037400160219         chain  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP)  FNBLT000;
037500160217         if not %found(fnblt01l);
037600160217           // esco dando errore
037700160217           pOut_Err = 1;
037800160217           exsr sr_RoutEnd;
037900160217         else;
038000161201           dou %eof(fnblt01l) OR nrCollo=c_Dim;
038100160218             // incremento l'indice del numero Collo
038200160218             nrCollo += 1;
038300160218             // valorizzo i dati in skiera per quel collo
038400160218             pOut_SkVUC(nrCollo) = BLTVUC;
038500160218             pOut_SkPUC(nrCollo) = BLTPUC;
038600160218             pOut_SkDCM(nrCollo) = BLTDCM;
038700160218             // controllo se c'è stata una consegna parziale
038800160218             if wDCP = 0;
038900160218               // se non c'è stata allora ho anche una sola ora di consegna
039000160218               pOut_SkHMC(nrCollo) = wHMC;
039100160218             else;
039200160218               // se c'è stata una consegna parziale reperisco gli eventi che l'hanno scatenata
039300160219               chain  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP:'P':BLTDCM)  FNEVB000;
039400160218               if %found(fnevb04l);
039500160218                 pOut_SkHMC(nrCollo) = EVBHEV;
039600160218                 pOut_Tip = 'P';
039700160218               endif;
039800160218             endif;
039900160218             // calcolo il segnacollo
040000160218             wBarcodeI = %editc(BLTLNP:'X') +
040100160217                       %editc(BLTLNA:'X') +
040200160217                       %editc(BLTNRS:'X') +
040300160217                       %editc(BLTNSC:'X') +
040400160217                       %editc(BLPZNC:'X')  ;
040500160218             exsr calbarcode;
040600160218             pOut_SkBRT(nrCollo) = wBarcodeO;
040700160218             // reperisco il segnacollo cliente
040800160219             chain  (BLTFLS:BLTLNA:BLTNRS:BLTNSC:'C')  FIARS000;
040900160218             if %found(fiars01l);
041000160218               pOut_SkCLI(nrCollo) = ARSNOT;
041100160218             endif;
041200160226             // reperisco la spunta come da opzione
041300160219             chain  (BLTFLS:BLTLNA:BLTNRS:BLTNSC)  FNBRV000;
041400160218             if %found(fnbrv07l);
041500160218               dou %eof(fnbrv07l);
041600160226                 // opzione standartd
041700160223                 if pIn_Opz = *blank or pIn_Opz = 'STD';
041800160226                   // reperisco data/ora di spunta più bassa tra tutte quelle effettuate
041900160226                   if BRVDFS < pOut_SkDFS(nrCollo)           or
042000160226                      (BRVDFS = pOut_SkDFS(nrCollo) and
042100160226                       BRVHMS < pOut_SkHMS(nrCollo))         or
042200160226                      (pOut_SkDFS(nrCollo) = 0 and
042300160226                       pOut_SkHMS(nrCollo) = 0);
042400160218                     pOut_SkDFS(nrCollo) = BRVDFS;
042500160218                     pOut_SkHMS(nrCollo) = BRVHMS;
042600160218                   endif;
042700160218                 endif;
042800160705                 // opzione FV5
042900160705                 if pIn_Opz = 'FV5';
043000160705                   // reperisco data/ora di spunta più bassa tra tutte quelle effettuate
043100160705                   if BRVNPG = 5  AND (
043200160705                      BRVDFS < pOut_SkDFS(nrCollo)           or
043300160705                      (BRVDFS = pOut_SkDFS(nrCollo) and
043400160705                       BRVHMS < pOut_SkHMS(nrCollo))         or
043500160705                      (pOut_SkDFS(nrCollo) = 0 and
043600160705                       pOut_SkHMS(nrCollo) = 0)
043700160705                      );
043800160705                     pOut_SkDFS(nrCollo) = BRVDFS;
043900160705                     pOut_SkHMS(nrCollo) = BRVHMS;
044000160705                   endif;
044100160705                 endif;
044200160219                 reade  (BLTFLS:BLTLNA:BLTNRS:BLTNSC)  FNBRV000;
044300160218               enddo;
044400160218             endif;
044500160217
044600160219             reade  (pIn_AAS:pIn_LNP:pIn_NRS:pIn_NSP)  FNBLT000;
044700160218           enddo;
044800160218         endif;
044900160217
045000160217         // valorizzazioni finali
045100160217         pOut_NCL = BLPNCL;
045200160217         select;
045300160217          when pOut_TIP = *blank and
045400160217           wDCM > 0            and
045500160217           wDCP = 0;
045600160217           pOut_TIP = 'TT';
045700160217          when pOut_TIP = *blank and
045800160217           wDCM = 0            and
045900160217           wDCP = 0;
046000160217           pOut_TIP = *blank;
046100160217          when wDCM > 0       and
046200160217           (pOut_TIP = 'P' or wDCP > 0);
046300160217           pOut_TIP = 'TP';
046400160217          when wDCM = 0       and
046500160217           (pOut_TIP = 'P' or wDCP > 0);
046600160217           pOut_TIP = 'PP';
046700160218         endsl;
046800160217         pOut_DCM = wDCM;
046900160217         pOut_HMC = wHMC;
047000160218         // se il tipo consegna risulta TP (alcune consegne parziali, ma alla fine completata)
047100160218         // si può valorizzare l'ora consegna merce colli con l'ora di consegna spedizione
047200160218         if pOut_TIP = 'TP';
047300160218           // ciclo sulla schiera delle ore consegna merce colli per i colli della spedizione
047400160218           // valorizzando gli elementi a 0 con l'ora della consegna finale
047500160218           for nrCollo = 1 to pOut_NCL ;
047600160218             if pOut_SkHMC(nrCollo) = 0;
047700160218               pOut_SkHMC(nrCollo) = pOut_HMC;
047800160218             endif;
047900160218           endfor;
048000160218         endif;
048100160217         pOut_Err = 0   ;
048200160217
048300160217       ENDSR;
048400160217
048500160217       //--------------------------------------------------------------
048600160217       // Operazioni finali.
048700160217       //--------------------------------------------------------------
048800160217       BEGSR  sr_RoutEnd;
048900160217
049000140724         // Chiusura pgm
049100160202         if pIn_TLa = 'C';
049200140724           *inlr = *on;
049300140724         else;
049400160627           // se non ci sono stati errori
049500160627           if pOut_Err = 0;
049600160627             // opzione standartd
049700160705             if pIn_Opz = *blank or pIn_Opz = 'STD' or
049800160705             // opzione foglio viaggio 5
049900160705                pIn_Opz = 'FV5';
050000160627               // ordino le schiere per data/ora spunta
050100160627               exsr sr_Sort_SK;
050200160627             endif;
050300160627           endif;
050400140724           *inrt = *on;
050500140724         endif;
050600140724         return;
050700160627
050800160627       ENDSR;
050900160627
051000160627       //--------------------------------------------------------------
051100160627       // Ordino schiere
051200160627       //--------------------------------------------------------------
051300160627       BEGSR  sr_Sort_SK;
051400160627
051500160627         // valorizzo schiere della DS
051600160627         for nrCollo = 1 to pOut_NCL ;
051700160627           SortA_DS(nrCollo).SkBRT = pOut_SkBRT(nrCollo);
051800160627           SortA_DS(nrCollo).SkCLI = pOut_SkCLI(nrCollo);
051900160627           SortA_DS(nrCollo).SkDCM = pOut_SkDCM(nrCollo);
052000160627           SortA_DS(nrCollo).SkHMC = pOut_SkHMC(nrCollo);
052100160627             SortA_DS(nrCollo).SkDFS_HMS =
052200160627                  %editc(pOut_SkDFS(nrCollo) : 'X')
052300160627                + %editc(pOut_SkHMS(nrCollo) : 'X');
052400160627           SortA_DS(nrCollo).SkVUC = pOut_SkVUC(nrCollo);
052500160627           SortA_DS(nrCollo).SkPUC = pOut_SkPUC(nrCollo);
052600160627         endfor;
052700160627
052800160627         // ordino per data/ora spunta
052900160627         sorta %subarr(SortA_DS(*).SkDFS_HMS : 1 : pOut_NCL);
053000160627
053100160627         // rivalorizzo le schiere di output
053200160627         for nrCollo = 1 to pOut_NCL ;
053300160627           pOut_SkBRT(nrCollo) = SortA_DS(nrCollo).SkBRT;
053400160627           pOut_SkCLI(nrCollo) = SortA_DS(nrCollo).SkCLI;
053500160627           pOut_SkDCM(nrCollo) = SortA_DS(nrCollo).SkDCM;
053600160627           pOut_SkHMC(nrCollo) = SortA_DS(nrCollo).SkHMC;
053700160627           pOut_SkDFS(nrCollo) =
053800160627             %dec(%subst(SortA_DS(nrCollo).SkDFS_HMS : 1 : 8) : 8 : 0);
053900160627           pOut_SkHMS(nrCollo) =
054000160627             %dec(%subst(SortA_DS(nrCollo).SkDFS_HMS : 9 : 6) : 6 : 0);
054100160627           pOut_SkVUC(nrCollo) = SortA_DS(nrCollo).SkVUC;
054200160627           pOut_SkPUC(nrCollo) = SortA_DS(nrCollo).SkPUC;
054300160627         endfor;
054400091223
054500091223       ENDSR;
054600091223
054700091223      /end-free
054800160217
054900160217     C     calbarcode    BEGSR
055000160217     C*
055100160217     C* Inizializzo la ds d procedura x calcolo barcode standard BRT e variabili d wrk
055200160217     C                   clear                   TRUL28DS
055300160217     C                   clear                   wBarcodeO
055400160217     C*
055500160217     C* Valorizzo il buffer d input
055600160217     C                   eval      I28TLA = 'E'
055700160217     C                   eval      I28MOD = 'BAR'
055800160217     C                   eval      I28COD = wBarcodeI
055900160217     C                   call      'TRUL28R1'
056000160217     C                   parm                    TRUL28DS
056100160217     C                   if        O28ERR = *blanks
056200160217     C                   eval      wBarcodeO = O28COD
056300160217     C                   endif
056400160217     C*
056500160217     C                   ENDSR

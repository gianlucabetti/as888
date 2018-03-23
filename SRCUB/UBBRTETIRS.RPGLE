000100160704       //==============================================================
000200160704       // Driver stampa etichetta segnacollo BRT
000300160704       //==============================================================
000400160704
000500160704       //--------------------------------------------------------------
000600160704       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160704       //--------------------------------------------------------------
000800160704
000900160704     /*PRM dbgview(*source)
001000160704     /*END
001100160704     **
001200160704     ** ISTRUZIONI PER LA COMPILAZIONE
001300160704     **
001400160704     ** 1. Creare il modulo UBBRTETIR (Opz 15 PDM)
001500160704     ** 2. Creare/Aggiornare il programma di servizio UBBRTETIR (CRTSRVPGM / UPDSRVPGM)
001600160704     **    con ACTGRP(*caller)
001700160704     **    aggiungere nella BNDDIR(UBBNDDIR)
001800170421     ** 3. Visto chi userà questo oggetto, non serve l'oggetto programma
001900160704     ** 4. Cancellare il modulo.
002000160704     **
002100160704
002200160704       //--------------------------------------------------------------
002300160704       // Specifiche di controllo.
002400160704       //--------------------------------------------------------------
002500160704     h decedit('0,') datedit(*dmy/) option(*nodebugio)
002600160704     h alwnull(*inputonly)
002700160704
002800160704       //--------------------------------------------------------------
002900160704       // Dichiarazione file.
003000160704       //--------------------------------------------------------------
003100160704
003200160704     FUBBRTETIP O    E             printer usropn
003300160704
003400160704       //--------------------------------------------------------------
003500160704       // Definizione costanti.
003600160704       //--------------------------------------------------------------
003700160704
003800160704       // - Spessore standard delle linee in inches/pollici (*memo)
003900160704     d c_LineNarrow    c                   const(0.008)
004000160704     d c_LineMedium    c                   const(0.017)
004100160704     d c_LineWide      c                   const(0.025)
004200160704
004300160704       //--------------------------------------------------------------
004400160704       // Definizione schiere.
004500160704       //--------------------------------------------------------------
004600160704
004700160704
004800160704       //--------------------------------------------------------------
004900160704       // Definizione aree dati.
005000160704       //--------------------------------------------------------------
005100160704
005200160704
005300160704       //--------------------------------------------------------------
005400160704       // Definizione strutture dati.
005500160704       //--------------------------------------------------------------
005600161107
005700161107       // parametro d'ingresso
005800161107     D UBBRTETIDS    e ds
005900160704
006000160704       // - Status
006100160704     d Psds           sds
006200160704     d   SDSpgm          *proc
006300160704     d   JobName             244    253                                         Job name
006400160704     d   JobUser             254    263                                         User name
006500160704     d   JobNumber           264    269s 0                                      Job number
006600160719
006700160719     d jobinf          ds
006800160719     d    job                        10
006900160719     d    user                       10
007000160719     d    jobn                        6
007100161011
007200161011       // reperimento check digits
007300161011     D TRUL28DS      e ds                  inz
007400160704
007500160719       // strutture per QUSRSPLA
007600160719       /copy qsysinc/qrpglesrc,QUSRSPLA
007700160615
007800160719       // parametri gestione errori API.
007900160719       /copy qsysinc/qrpglesrc,QUSEC
008000160719
008100160704       //--------------------------------------------------------------
008200160704       // Definizione variabili.
008300160704       //--------------------------------------------------------------
008400160704
008500160704       // Campi di comodo
008600160704     d wBarcode        s             18a
008700160704     d wSegnacollo     s              7s 0
008800160706     d wCollo          s              7s 0
008900160705     D wX              s              2s 0
009000160705     D w2S             s              2s 0
009100160705     D w3S             s              3s 0
009200160705     D wCheckValue     s              3s 0
009300170310     D wOutq           s                   like(pIn_OutQ)
009400170310     D wUsrDta         s                   like(pIn_UsrDta)
009500170406     D wFnumDS         ds
009600170406     D  wFnum                         6a   dim(100)
009700170420     D wUDtaDS         ds
009800170420     D  wUDta                        10a   dim(100)
009900160704
010000160719       // testo segnacollo
010100160704     d NCX             s              7
010200160718
010300160719       // parametri binary(4) di input per QUSRSPLA
010400160719     D  RCVLEN         s             10I 0
010500160719     D  FSPLN          s             10I 0
010600160623
010700160623       // - Dimensioni base per i simil-loghi Bartolini e BRT
010800160623      /copy gaitrasrc/srcTNSY,LogoBRT_R1
010900160704
011000160704       //--------------------------------------------------------------
011100160704       // Definizione prototipi procedure.
011200160704       //--------------------------------------------------------------
011300160704
011400160719       /copy gaitrasrc/srcProtoPR,QUSRSPLA
011500161011     d TRUL28R1        pr                  extpgm('TRUL28R1')
011600161011     D  pTRUL28DS                          like(TRUL28DS)
011700170309
011800170309       // Parametri API QCAPCMD (Process Commands)?
011900170309     d Qcmd            s           2048    inz  varying
012000170309      /copy qSysInc/qRpgleSrc,QCAPCMD
012100170309       // API QCAPCMD (Process Commands)?
012200170309      /copy gaitrasrc/srcProtoPR,QCAPCMD
012300160704
012400160704       //--------------------------------------------------------------
012500160704       // Definizione key-list.
012600160704       //--------------------------------------------------------------
012700160704
012800160704
012900160704       //--------------------------------------------------------------
013000160704       // Definizione parametri procedura.
013100160704       //--------------------------------------------------------------
013200160704
013300160704     c     *Entry        plist
013400161107     c                   parm                    UBBRTETIDS
013500160623
013600160615      /free
013700160623
013800160623       //--------------------------------------------------------------
013900160623       // M A I N - L I N E
014000160623       //--------------------------------------------------------------
014100160623
014200160623       // Operazioni iniziali?
014300160623       exsr sr_RoutInz;
014400160704
014500160704       // se non chiusura
014600160704       if pIn_TLa <>'C';
014700160705         // la prima volta il segnacollo è il primo richiesto
014800160705         wSegnacollo = pIn_NCD;
014900170410         for wCollo = pIn_NCD to pIn_NCA;
015000170410           // apro il file di stampa se non già aperto
015100170410           // (mi serve farlo all'interno del ciclo perché il nr.segnacollo può essere lo USRDTA)
015200170410           exsr sr_ovrprtf;
015300160704           // stampa le N etiichette richieste
015400160704           exsr sr_StampaEtichetta;
015500160705           // il prossimo segnacollo sarà uno più del precedente
015600160705           wSegnacollo += 1;
015700160704         endfor;
015800160704       endif;
015900160704
016000160704       // Operazioni finali?
016100160704       exsr sr_RoutEnd;
016200160704
016300160704       //--------------------------------------------------------------
016400160704       // Operazioni iniziali.
016500160704       //--------------------------------------------------------------
016600160704       BEGSR  sr_RoutInz;
016700170406
016800170406         // inizializzazioni
016900170406         clear wFnum;
017000170420         clear wUDta;
017100170411         if pIn_NCA = 0;
017200170411           pIn_NCA = pIn_NCD;
017300170411         endif;
017400160704
017500160704         // se non chiusura
017600170209         if pIn_TLa <>'C'
017700170209         //  e non segnalazione errore
017800170406          and pIn_SegErr = *blank;
017900160704
018000160704           // dati obbligatori
018100160704           if pIn_FLS = 0      OR
018200160718           // pIn_NRS = 0      OR
018300160704              pIn_NCD = 0      OR
018400160704              pIn_LNA = 0      OR
018500160704              pIn_TMA = 0      OR
018600160704              pIn_ZNC = 0      OR
018700160704              pIn_NC2 = 0;
018800160704             pOut_Esito = 2;
018900160704             exsr sr_RoutEnd;
019000160704           endif;
019100160704
019200160704           // dati errati
019300160705           // tipo etichetta può valere solo ' '/ITA/DPD/FED/EEX
019400160705           if pIn_TiE <> *blank AND
019500160705              pIn_TiE <> 'ITA'  AND
019600160705              pIn_TiE <> 'DPD'  AND
019700160705              pIn_TiE <> 'FED'  AND
019800160705              pIn_TiE <> 'EEX';
019900160705             pOut_Esito = 2;
020000160705             exsr sr_RoutEnd;
020100160705           endif;
020200160704           // tipo servizio può valere solo ' '/C/E/H/D
020300160704           if pIn_TSP <> *blank AND
020400160704              pIn_TSP <> 'C'    AND
020500160704              pIn_TSP <> 'E'    AND
020600160704              pIn_TSP <> 'H'    AND
020700160704              pIn_TSP <> 'D';
020800160704             pOut_Esito = 2;
020900160704             exsr sr_RoutEnd;
021000160704           endif;
021100160704           // Tipo lancio deve essere blank o 'C'
021200160704           if pIn_TLa <> *blank and pIn_TLa <>'C';
021300160704             // esco dando errore
021400160704             pOut_Esito = 2;
021500160704             exsr sr_RoutEnd;
021600160704           endif;
021700160704
021800160704           // congruenza segnacollo da/a - nr.colli
021900170411           //
022000170411           // i colli totali possono essere solo >= di A - DA +1
022100170411           // (i colli gestiti sono T ma ne stampo solo X<T)
022200170411           if pIn_NCA - pIn_NCD + 1 >
022300170411              pIn_NC2;
022400170411             pOut_Esito = 2;
022500170411             exsr sr_RoutEnd;
022600170411           endif;
022700160706           // se il segnacollo DA < A
022800170406           // allora c'è incongruenza
022900160706           if pIn_NCA < pIn_NCD;
023000160706             pOut_Esito = 2;
023100160706             exsr sr_RoutEnd;
023200160706           endif;
023300170411
023400170406           // se è stato chiesto uno spool per collo
023500170410           if pIn_1SplXC = 'S'
023600170406           // ma sono stati chiesti più di 100 colli (che è la dim della schiera)
023700170406            and pIn_NC2 > 100;
023800170406             // allora c'è incongruenza
023900170406             pOut_Esito = 2;
024000170406             exsr sr_RoutEnd;
024100170406           endif;
024200160704         endif;
024300160704
024400160704         // offset box / margini
024500170419         // offset Y è il First Corner Down e deve valere tra 0 e 22,750 inch.
024600170419         // offset X è il First Corner Acrosse deve valere tra 0 e 22,750 inch.
024700170419         // ma siccome nella compilazione del PRTF è definito che il AFPDS deve essere
024800170419         // Y = 2,9134 inch = 7,4 cm  e X = 4,1339 inch = 10.5 cm
024900170419         // questi sono i limiti (anzi li facciamo ancora più stringenti altrimenti non si capisce
025000170419         // perché l'etichetta non si vede)
025100170419         // se gli offset superano i limiti, vengono dati dei default. Lo stesso se sono 0.
025200170419         if pIn_OfsY > 2,8 or
025300170419            pIn_OfsY = 0;
025400170419           pIn_OfsY = 0,1772;
025500170419         endif;
025600170419         if pIn_OfsX > 4,0 or
025700170419            pIn_OfsX = 0;
025800170419           pIn_OfsX = 0,1969;
025900170419         endif;
026000170419
026100170419         §FCD = pIn_OfsY;
026200161222         §FCA = pIn_OfsX;
026300160704         // altezza etichetta fissa a 6,5 cm = 2,559 inch
026400160704         §DCD = §FCD + 2,559;
026500160704         // larghezza etichetta fissa a 9,5 cm = 3,740 inch
026600160704         §DCA = §FCA + 3,74;
026700160704         // spess
026800160704         §LW  = 0,01;
026900160704
027000160704         // dati valorizzabili a pgm
027100160704         if pIn_AAS = 0;
027200160705           pIn_AAS = %dec(%subst(%char(%date()) : 1 : 4) : 4 : 0);
027300160704         endif;
027400160705         if pIn_MGS = 0;
027500160705           pIn_MGS = %dec(%subst(%char(%date()) : 6 : 2) : 2 : 0) * 1000 +
027600160705                     %dec(%subst(%char(%date()) : 9 : 2) : 2 : 0);
027700160704         endif;
027800160704
027900160704       ENDSR;
028000160704
028100160704       //--------------------------------------------------------------
028200160704       // Operazioni finali.
028300160704       //--------------------------------------------------------------
028400160704       BEGSR  sr_RoutEnd;
028500170110
028600170110         // Chiusura pgm
028700170110         if pIn_TLa = 'C';
028800170110           *inlr = *on;
028900170110         else;
029000160718
029100170110           // se l'esito è corretto
029200170406           if pOut_Esito = 0
029300170406           // e non è stato chiesto uno spool per collo (altrimenti quello che segue è già stato
029400170406           // fatto)
029500170410            and pIn_1SplXC <> 'S';
029600170110             // reperisco i dati dello spool
029700170110             // 1. chiudo il file di stampa
029800170309             if %open(UBBRTETIP);
029900170110               close UBBRTETIP;
030000170110             endif;
030100170110             // 2. reperisco l'ultimo spool creato da questo job e col nome del PRTF
030200170110             job  = JobName;
030300170110             user = JobUser;
030400170801             jobn = %editc(JobNumber:'X');
030500170110             FSPLN = -1;
030600170110             RCVLEN = %len(QUSA010001);
030700170110             RetrieveSpooledFileAttributes
030800170110              ( QUSA010001 :
030900170110                RCVLEN :
031000170110                'SPLA0100' :
031100170110                jobinf :
031200170110                *blank : *blank :
031300170110                'UBBRTETIP' :
031400170110                FSplN     );
031500170110             // 3. valorizzo i dati di output
031600170110             pOut_FNam = QUSSN01;
031700170110             pOut_JNam = QUSJN10;
031800170110             pOut_UNam = QUSUN12;
031900170110             pOut_JNum = QUSJNBR09;
032000170110             pOut_Fnum = %char(QUSSNBR);
032100170110           endif;
032200170406
032300170406           // se l'esito è corretto
032400170406           if pOut_Esito = 0
032500170406           // ma è stato chiesto uno spool per collo
032600170410            and pIn_1SplXC = 'S';
032700170406             // aggiorno il numero di file con la schiera
032800170406             pOut_Fnum = wFnumDS;
032900170420             pOut_UDTA = wUDtaDS;
033000170406           endif;
033100170110
033200170110           if  qUsei = *blank;
033300170110           //s_ObjTxt = qUstd12;
033400170110           else;
033500170110           //s_ObjTxt = *all'? ';
033600170110           endif;
033700170110
033800160704           *inrt = *on;
033900160704         endif;
034000160704         return;
034100160704
034200160704       ENDSR;
034300160704
034400160704       //--------------------------------------------------------------
034500160704       // Stampa le N etiichette richieste
034600160704       //--------------------------------------------------------------
034700160704       BEGSR  sr_StampaEtichetta;
034800160704
034900160704         // preparo l'etichetta
035000160704         exsr sr_PreparaEtichetta;
035100160704
035200160704         // stampo
035300170209
035400170209         if pIn_SegErr = *blank;
035500160704
035600160704         select;
035700160704         // pagseg con tipo servizio E
035800160704          when *in02;
035900170627           write TSP_RI;
036000160704         // pagseg con tipo servizio H
036100160704          when *in03;
036200170627           write TSP_RI;
036300160704         endsl;
036400160705         // tipo etichetta
036500160705         select;
036600160705          // network Italia
036700160705          when pIn_TiE = 'ITA' or pIn_TiE = *blank;
036800160705           write ZNCITA;
036900160705          // network DPD
037000160705          when pIn_TiE = 'DPD';
037100160705           // per ora non ci sono differenze dall'Italia
037200160705           write ZNCITA;
037300160705          // network FEDEX
037400160705          when pIn_TiE = 'FED';
037500160705           // per ora non ci sono differenze dall'Italia
037600160705           write ZNCITA;
037700161007          // network EuroExpress (stampo sia i dati esteri che la zona)
037800160705          when pIn_TiE = 'EEX';
037900160705           write ZNCEEX;
038000160705         endsl;
038100160704         // etichetta
038200160704         write BRTETI01;
038300170209
038400170209         // se c'è da stampare una segnalazione d'errore
038500170209         else;
038600170209           write MsgErr;
038700170209         endif;
038800170209
038900170210         // logo
039000170210         if pIn_Logo <> 'N';
039100170210           write  LogoBRT_SM;
039200170210         endif;
039300160706         // fine pagina
039400160706         write ENDPAGE;
039500170406
039600170406         // se è richiesta la stampa per singolo collo
039700170410         if pIn_1SplXC= 'S';
039800170406           // reperisco i dati dello spool
039900170406           // 1. chiudo il file di stampa
040000170406           if %open(UBBRTETIP);
040100170406             close UBBRTETIP;
040200170406           endif;
040300170406           // 2. reperisco l'ultimo spool creato da questo job e col nome del PRTF
040400170406           job  = JobName;
040500170406           user = JobUser;
040600170801           jobn = %editc(JobNumber:'X');
040700170406           FSPLN = -1;
040800170406           RCVLEN = %len(QUSA010001);
040900170406           RetrieveSpooledFileAttributes
041000170406            ( QUSA010001 :
041100170406              RCVLEN :
041200170406              'SPLA0100' :
041300170406              jobinf :
041400170406              *blank : *blank :
041500170406              'UBBRTETIP' :
041600170406              FSplN     );
041700170406           // 3. valorizzo i dati di output
041800170406           pOut_FNam = QUSSN01;
041900170406           pOut_JNam = QUSJN10;
042000170406           pOut_UNam = QUSUN12;
042100170406           pOut_JNum = QUSJNBR09;
042200170406           wFnum(wCollo - pIn_NCD + 1) = %char(QUSSNBR);
042300170420           wUDta(wCollo - pIn_NCD + 1) = QUSUD01;
042400170406         endif;
042500160704
042600160704       ENDSR;
042700160704
042800160704       //--------------------------------------------------------------
042900160704       // Prepara etichetta
043000160704       //--------------------------------------------------------------
043100160704       BEGSR  sr_PreparaEtichetta;
043200170209
043300170209         if pIn_SegErr = *blank;
043400160704
043500160704       // stampa bordo etichetta
043600160704       if pIn_Bordo = 'S';
043700160704         *in01 = *on;
043800160704       else;
043900160704         *in01 = *off;
044000160704       endif;
044100160704
044200160704       // stampa riga di contrasto
044300160704       if pIn_Row <> 'N';
044400160704         *in04 = *on;
044500160704       else;
044600160704         *in04 = *off;
044700160704       endif;
044800160705
044900160705       // R di ristampa
045000161005       *in05 = *off;
045100161005       clear Rst;
045200161005       clear Val;
045300161005sel 1  select;
045400161005w   1   when pIn_rst  =  *blanks;
045500161005w   1   when pIn_rst  =  'V';
045600161005         Val =  'V';
045700161005w   1   when pIn_rst  =  'X';
045800161005         Rst  =  'R';
045900161005         Val  =  'V';
046000161005         *in05 = *on;
046100161005w   1     when pIn_rst  =  'W';
046200161005         Rst  =  'M';
046300161005         Val  =  'V';
046400161005         *in05 = *on;
046500161005x   1   other;
046600161005         Rst  =  pIn_rst;
046700161005         *in05 = *on;
046800161005e   1  endsl;
046900160704
047000160616       // box terminal arrivo
047100160628       // 0,20 cm
047200160628       §FCD_TMA = §FCD + 0,079;
047300160621       // 1,50 cm
047400160621       §FCA_TMA = §FCA + 0,591;
047500161010       // altezza box 1,15 cm
047600161010       §DCD_TMA = §FCD_TMA + 0,453;
047700161010       // larghezza box 1,93 cm
047800161010       §DCA_TMA = §FCA_TMA + 0,760;
047900160704       // spessore box
048000160617       §LW_TMA  = 0,01;
048100160616
048200160616       // box zona consegna
048300160705       // box e testo dipendono dal tipo etichetta
048400160705       exsr sr_ZNC_TiE;
048500160620
048600160620       // box tipo servizio
048700160706       if pIn_TSP = 'C'     OR
048800160706          pIn_TSP = *blank;
048900160706         // 2,30 cm
049000160706         §FCD_TSP = §FCD + 0,906;
049100160706         // 0,35 cm
049200160706         §FCA_TSP = §FCA + 0,138;
049300160706         // altezza box 0,80 cm
049400160706         §DCD_TSP = §FCD_TSP + 0,315;
049500170626         // larghezza box 0,80 cm
049600160706         §DCA_TSP = §FCA_TSP + 0,315;
049700160706         // spessore box
049800160706         §LW_TSP  = 0,01;
049900160706       endif;
050000170525       // linea tipo servizio
050100170525       if pIn_TSP = 'H'     OR
050200170525          pIn_TSP = 'E';
050300170525         // spessore linea per simulare quadrato
050400170627         §LW_TSP  = 0,315;
050500170525       endif;
050600160620
050700160620       // linea di separazione serie segnacollo
050800160620       // dove inizia la linea dall'alto (bordo + distanza)
050900160628       // 5,15 cm
051000160628       §PD_L01 = §FCD + 2,028;
051100160620       // dove inizia la linea partendo da sx (bordo + distanza)
051200160620       // 1,65 cm
051300160620       §PA_L01 = §FCA + 0,650;
051400160620       // 0,3 cm lunghezza linea
051500160620       §LL_L01 = 0,118;
051600160620       // spessore linea
051700160620       §LW_L01  = 0,03;
051800160615
051900160615       // linea di separazione mittente/destinatario
052000160706       // 1,60 cm (la linea è verticale ma punta verso il basso)
052100160706       §PD = §FCD + 0,630;
052200160706       // 8,2 cm
052300160617       §PA = §FCA + 3,228;
052400160706       // 4,6 cm lunghezza linea
052500160706       §LL = 1,811;
052600160615       // stesso spessore delle righe del box
052700160615       //§LW  = 0,01;
052800160615
052900160704       // calcolo barcode 18 char (l'ultimo è il check digit)
053000160704       // compongo barcode e calcolo chek digit
053100161011       wBarcode = %editc(pIn_FLS:'X') +
053200161011                   %editc(pIn_LNA:'X') +
053300161011                   %editc(pIn_NRS:'X') +
053400161011                   %editc(wSegnacollo:'X') +
053500161011                   %editc(pIn_ZNC:'X');
053600161011       // siccome ho testato all'inizio che i dati non siano vuoti, anche il barcode è valorizzato
053700161011       // calcolo il check digit
053800161011       clear TRUL28DS;
053900161011       I28Mod  = 'BAR';
054000161011       I28COD = %trim(wBarcode);
054100161011       TRUL28R1(TRUL28DS);
054200161011       // Se tutto ok aggiungo il check digit
054300161011       if O28err = *blanks;
054400161011         wBarcode = O28Cod;
054500161011       endif;
054600160704       Bcd = wBarcode;
054700160615       // dove inizia il barcode dall'alto (in alto)
054800160706       // che coincide con dove inizia la linea di separazione/mittente destinatario
054900160628       // 1,60 cm
055000160706       //§PD_BCD = §FCD + 0,630;
055100160706       §PD_BCD = §PD;
055200160621       // dove inizia il barcode da sx
055300160627       // 2,00 cm
055400160704       §PA_BCD = §FCA + 0,787;
055500160630
055600160630       // linea sopra al barcode
055700160630       // spessore linea
055800160630       §LW_L02  = 0,06;
055900160630       // i punti di inizio sono:
056000160630       // verticale: quello del barcode meno la metà dello spessore della linea
056100160630       // orizzontale: la distanza dal bordo del box terminal arrivo
056200160704       §PD_L02 = §PD_BCD - §LW_L02/2;
056300160630       §PA_L02 = §FCA_TMA;
056400160706       // la lunghezza tra l'inizio del box TMA e 0,04 cm prima della riga di separazione
056500160706       §LL_L02 = (§PA - 0,157) - §FCA_TMA;
056600160617
056700160617       // testo terminal arrivo
056800160705       TMA = %editc(pIn_TMA : 'X');
056900160628       // dove inizia il testo dall'alto (come inizio box + distanza)
057000161010       // 1,00 cm
057100161010       §PD_TMA = §FCD_TMA + 0,394;
057200160617       // dove inizia il testo partendo da sx (come l'inizio box + 0,05 cm)
057300160617       §PA_TMA = §FCA_TMA + 0,020;
057400160616
057500160616       // testo linea di arrivo
057600160705       LNA = %editc(pIn_LNA : 'X');
057700160616       // dove inizia il testo dall'alto (bordo + distanza)
057800161010       // 1,05 cm
057900161010       §PD_LNA = §FCD + 0,413;
058000160616       // dove inizia il testo partendo da sx (bordo + distanza)
058100160616       // 0,10 cm
058200160616       §PA_LNA = §DCA_TMA + 0,039;
058300160616
058400160621       // testo tipo spedizione
058500160704       *in02 = *off;
058600160704       *in03 = *off;
058700160621       select;
058800160621        when pIn_TSP = 'C';
058900160621         TSP = 'C';
059000160623         // dove inizia il testo dall'alto (bordo + distanza)
059100160623         // 3,00 cm
059200160623         §PD_TSP = §FCD + 1,181;
059300160623         // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
059400160623         §PA_TSP = §FCA_TSP + 0,02;
059500170526        when pIn_TSP = 'E';
059600160704         *in02 = *on;
059700170526         TSP_Rev= 'E';
059800160623         // 2,30 cm
059900170525         //§PD_TSP_E = §FCD + 0,906;
060000170525         §FCD_TSP = §FCD + 0,906;
060100170525         // dove inizia il testo dall'alto (bordo + distanza)
060200170525         §PD_TSP = §FCD + 1,181;
060300160623         // 0,35 cm
060400170525         //§PA_TSP_E = §FCA + 0,138;
060500170525         §FCA_TSP = §FCA + 0,138;
060600170525         // dove inizia il testo partendo da sx (come inizio linea + 0,127 cm)
060700170525         §PA_TSP = §FCA_TSP + 0,05;
060800170525        when pIn_TSP = 'H';
060900170526         *in03 = *on;
061000170526         TSP_Rev= 'H';
061100170525         // 2,30 cm
061200170525         //§PD_TSP_E = §FCD + 0,906;
061300170525         §FCD_TSP = §FCD + 0,906;
061400170525         // dove inizia il testo dall'alto (bordo + distanza)
061500170525         §PD_TSP = §FCD + 1,181;
061600170525         // 0,35 cm
061700170525         //§PA_TSP_E = §FCA + 0,138;
061800170525         §FCA_TSP = §FCA + 0,138;
061900170526         // dove inizia il testo partendo da sx (come inizio linea + 0,1 cm)
062000170526         §PA_TSP = §FCA_TSP + 0,0394;
062100160704        when pIn_TSP = 'D';
062200160704         TSP = *blank;
062300160704         // dove inizia il testo dall'alto (bordo + distanza)
062400160704         // 3,00 cm
062500160704         §PD_TSP = §FCD + 1,181;
062600160704         // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
062700160704         §PA_TSP = §FCA_TSP + 0,02;
062800160706        when pIn_TSP = *blank;
062900160706         TSP = *blank;
063000160706         // dove inizia il testo dall'alto (bordo + distanza)
063100160706         // 3,00 cm
063200160706         §PD_TSP = §FCD + 1,181;
063300160706         // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
063400160706         §PA_TSP = §FCA_TSP + 0,02;
063500160621       endsl;
063600160620
063700160620       // testo linea di partenza
063800160705       LNP = %editc(pIn_FLS : 'X');
063900160620       // dove inizia il testo dall'alto (bordo + distanza)
064000160621       // 3,85 cm
064100160621       §PD_LNP = §FCD + 1,516;
064200160620       // dove inizia il testo partendo da sx (bordo + distanza)
064300160620       // 0,35 cm
064400160620       §PA_LNP = §FCA + 0,138;
064500160620
064600160620       // testo nr. serie
064700160705       NRS = %editc(pIn_NRS : 'X');
064800160620       // dove inizia il testo dall'alto (bordo + distanza)
064900160628       // 5,55 cm
065000160628       §PD_NRS = §FCD + 2,185;
065100160620       // dove inizia il testo partendo da sx (bordo + distanza)
065200160620       // 0,35 cm
065300160620       §PA_NRS = §FCA + 0,138;
065400161006       // se il numero serie è 0, non lo devo stampare, nè questo nè il trattino
065500161006       *in06 = (pIn_Nrs = 0);
065600160620
065700160620       // testo segnacollo (si divide in 2 parti)
065800160704       NCx = %editc(wSegnacollo : 'X');
065900160620       NCx1 = %subst(NCx : 1 : 4);
066000160620       NCx2 = %subst(NCx : 5 : 3);
066100160620       // dove inizia il testo dall'alto (bordo + distanza)
066200160628       // 5,55 cm
066300160628       §PD_NCx1 = §FCD + 2,185;
066400160620       // dove inizia il testo partendo da sx (bordo + distanza)
066500160620       // 2,10 cm
066600160620       §PA_NCx1 = §FCA + 0,827;
066700160620       // dove inizia il testo dall'alto (bordo + distanza)
066800160628       // 5,65 cm
066900160628       §PD_NCx2 = §FCD + 2,224;
067000160620       // dove inizia il testo partendo da sx (bordo + distanza)
067100160620       // 4,55 cm
067200160620       §PA_NCx2 = §FCA + 1,791;
067300160622
067400160704       // testo Riferimeno RMA/CDP
067500160622       //          Rif.: - RMA 15 CHAR -
067600160707       Rif_RMA  = 'Rif.: ' + %trim(pIn_CDP);
067700160622       // dove inizia il testo dall'alto
067800160706       // che coincide con inizia linea separazione mittente/destinatario + sua lunghezza
067900160706       §PD_RMA = §PD + §LL;
068000160622       // dove inizia il testo partendo da sx
068100160622       // che coincide con dove inizia la serie
068200160622       §PA_RMA = §PA_NRS;
068300160622
068400160622       // testo data spedizione
068500160622       MGS_AAS  = %subst(%editc(pIn_MGS:'X'):3:2) + '/' +
068600160622                  %subst(%editc(pIn_MGS:'X'):1:2) + '/' +
068700160622                  %char(pIn_AAS);
068800160622       // dove inizia il testo dall'alto
068900160706       // che coincide con dove finisce la riga di separazione
069000160706       §PD_DTS = §PD + §LL;
069100160622       // dove inizia il testo partendo da sx (bordo + distanza)
069200160622       // 4,50 cm
069300160622       §PA_DTS = §FCA + 1,772;
069400160705
069500161005       // Flag Ristampa
069600160705       // dove inizia il testo dall'alto
069700160706       // che coincide con dove finisce la riga di separazione
069800160706       §PD_RST = §PD + §LL;
069900160705       // dove inizia il testo partendo da sx (dove inizia la data + distanza)
070000161005       // 1,65 cm
070100161005       §PA_RST = §PA_DTS + 0,650;
070200161005
070300161005       // Flag Merce a valore
070400161005       // dove inizia il testo dall'alto
070500161005       // che coincide con dove finisce la riga di separazione
070600161005       §PD_VAL = §PD + §LL;
070700161005       // dove inizia il testo partendo da sx (dove inizia il flag Ristampa + distanza)
070800161005       // 0,50 cm
070900161005       §PA_VAL = §PA_RST + 0,197;
071000160616
071100160616       // testo mittente ragione sociale / provincia
071200160706       //          RAGIONE SOCIALE 26 CHAR --  Pr
071300160706       Mittente = %trim(%subst(pIn_RSM : 1 : 26)) + '  ' + pIn_PRM;
071400160616       // dove inizia il testo dall'alto
071500160616       // che coincide con dove finisce la riga di separazione
071600160616       §PD_01 = §PD + §LL;
071700160616       // dove inizia il testo partendo da sx (bordo + distanza)
071800160628       // 0,04 cm prima della riga di separazione
071900160620       §PA_01 = §PA - 0,016;
072000160616
072100160616       // testo destinatario 1 ragione sociale + provincia
072200160706       //          RAGIONE SOCIALE 26 CHAR --  Pr
072300160706       // oppure ragione sociale + nazione se no ITALIA
072400160706       //          RAGIONE SOCIALE 26 CHAR --  Pr
072500160706       if pIn_NZD = *blank;
072600160718         Dest_01 = %trim(%subst(pIn_RSD : 1 : 26)) +
072700160718                   '  ' + pIn_PRD;
072800160706       else;
072900160718         Dest_01 = %trim(%subst(pIn_RSD : 1 : 26)) +
073000160718                   '  ' + pIn_NZD;
073100160706       endif;
073200160616       // dove inizia il testo dall'alto
073300160616       // che coincide con dove finisce la riga di separazione
073400160616       §PD_D01 = §PD + §LL;
073500160616       // dove inizia il testo partendo da sx (bordo + distanza)
073600160616       // 0,3  cm dopo     la riga di separazione
073700160616       §PA_D01 = §PA + 0,118;
073800160616
073900160620       // testo destinatario 2 indirizzo + località + CAP (CAP no)
074000160706       //          INDIRIZZO 21 CHAR ----   LOCALIT 10
074100160718       Dest_02 = %trim(%subst(pIn_IND : 1 : 21)) + '   ' +
074200160718                 %trim(%subst(pIn_LOD : 1 : 10)) ;
074300160616       // dove inizia il testo dall'alto
074400160616       // che coincide con dove finisce la riga di separazione
074500160616       §PD_D02 = §PD + §LL;
074600160616       // dove inizia il testo partendo da sx (bordo + distanza)
074700160616       // 0,3 + 0,3  cm dopo     la riga di separazione
074800160616       §PA_D02 = §PA + 0,118 + 0,118;
074900160621
075000160621       // testo spedizione MC. xx,xxx   xxxxx - xxxxx   KG. xxxxx,x
075100160622       //123456789012345678901234567890123456789012345678901234567890
075200160706       //MC. xx,xxx   xxxxx - xxxxx   KG. xxxxxx,x
075300160622       Spedizione =
075400160706        'MC.                -         KG.         ';
075500160706       %subst(Spedizione : 5 : 6) = %trim(%editw(pIn_VOL:'0 ,   '));
075600170411       // se è stato passato un nr.segnacollo preciso, sarà quello il primo valore
075700160704       if pIn_NC1 <> 0;
075800160706         %subst(Spedizione : 14 : 5) = %trim(%editc(pIn_NC1:'Z'));
075900170411       // se NON è stato passato un nr.segnacollo preciso, il primo valore sarà il
076000170411       // progressivo collo
076100160704       else;
076200160706         %subst(Spedizione : 14 : 5) =
076300170411           %trim(%editc(wCollo :'Z'));
076400160704       endif;
076500160706       %subst(Spedizione : 22 : 5) = %trim(%editc(pIn_NC2:'Z'));
076600160706       %subst(Spedizione : 34 : 8) = %trim(%editw(pIn_PKG:'    0 , '));
076700160621       // dove inizia il testo dall'alto
076800160621       // che coincide con dove finisce la riga di separazione
076900160621       §PD_S01 = §PD + §LL;
077000160621       // dove inizia il testo partendo da sx (bordo + distanza)
077100160621       // 0,3 + 0,3 + 0,4 cm dopo     la riga di separazione
077200160621       §PA_S01 = §PA + 0,118 + +0,118 + 0,157;
077300170209
077400170209         // se c'è da stampare una segnalazione d'errore
077500170209         else;
077600170209           Msg_Tit = pIn_MsgErT;
077700170209           Msg_Er1   = %subst(pIn_MsgErr:1:20)  ;
077800170209           Msg_Er2   = %subst(pIn_MsgErr:21:20) ;
077900170209           Msg_Er3   = %subst(pIn_MsgErr:41:20) ;
078000170209           Msg_Er4   = %subst(pIn_MsgErr:61:40) ;
078100170209           Msg_Er5   = %subst(pIn_MsgErr:101:40) ;
078200170209           Msg_Er6   = %subst(pIn_MsgErr:141:40) ;
078300170209           Msg_Er7   = %subst(pIn_MsgErr:181:40) ;
078400170209           // dove inizia dall'alto (bordo + distanza)
078500170209           // 1,00 cm
078600170209           §PD_MsgT = §FCD + 0,3937;
078700170209           // dove inizia da sx (bordo + distanza)
078800170209           // 2,00 cm
078900170209           §PA_MsgT = §FCA + 0,7874;
079000170209           // dove inizia dall'alto (bordo + distanza)
079100170210           // 2,50 cm
079200170210           §PD_MsgE1= §FCD + 0,9842;
079300170209           // dove inizia da sx (bordo + distanza)
079400170210           // 0,50 cm
079500170210           §PA_MsgE1= §FCA + 0,1969;
079600170209           // dove inizia dall'alto (bordo + distanza)
079700170210           // 3,20 cm
079800170210           §PD_MsgE2= §FCD + 1,2598;
079900170209           // dove inizia da sx (bordo + distanza)
080000170210           // 0,50 cm
080100170210           §PA_MsgE2= §FCA + 0,1969;
080200170209           // dove inizia dall'alto (bordo + distanza)
080300170210           // 3,90 cm
080400170210           §PD_MsgE3= §FCD + 1,5354;
080500170209           // dove inizia da sx (bordo + distanza)
080600170210           // 0,50 cm
080700170210           §PA_MsgE3= §FCA + 0,1969;
080800170209           // dove inizia dall'alto (bordo + distanza)
080900170210           // 4,60 cm
081000170210           §PD_MsgE4= §FCD + 1,8110;
081100170214           // dove inizia da sx (bordo + distanza)
081200170214           // 0,10 cm
081300170214           §PA_MsgE4= §FCA + 0,0394;
081400170209           // dove inizia dall'alto (bordo + distanza)
081500170210           // 5,10 cm
081600170210           §PD_MsgE5= §FCD + 2,0079;
081700170209           // dove inizia da sx (bordo + distanza)
081800170214           // 0,10 cm
081900170214           §PA_MsgE5= §FCA + 0,0394;
082000170209           // dove inizia dall'alto (bordo + distanza)
082100170210           // 5,60 cm
082200170210           §PD_MsgE6= §FCD + 2,2047;
082300170209           // dove inizia da sx (bordo + distanza)
082400170214           // 0,10 cm
082500170214           §PA_MsgE6= §FCA + 0,0394;
082600170209           // dove inizia dall'alto (bordo + distanza)
082700170210           // 6,10 cm
082800170210           §PD_MsgE7= §FCD + 2,4016;
082900170209           // dove inizia da sx (bordo + distanza)
083000170214           // 0,10 cm
083100170214           §PA_MsgE7= §FCA + 0,0394;
083200170209         endif;
083300160615
083400160704       clear LogoBRT;
083500160704       // Simil-Logo BRT se richiesto
083600160704       if pIn_Logo <> 'N';
083700160623         //box logo
083800160623         // 0,20 cm
083900160623         §PlblS  = §FCD    + 0,079;
084000160623         // 0,35 cm
084100160623         §PlblVS = §FCA    + 0.079;
084200160623         // altezza box 0,70 cm
084300160623         §PlblI  = §PlblS  + 0,276;
084400160623         // larghezza box 0,85 cm
084500160623         §PlblVD = §PlblVS + 0,335;
084600160704         exsr  sr_SimilLogoBRT_;
084700160704       endif;
084800160704
084900160704       ENDSR;
085000160615
085100160623       // valorizzo campi programma per stampa logo BRT
085200160623      /end-free
085300160705
085400160705      /copy gaitrasrc/srcTNSY,LogoBRT_R2
085500160705
085600160623      /free
085700160705
085800160705       //--------------------------------------------------------------
085900160705       // Compongo Box e Testo Zona Consegna a seconda del Tipo Etichetta
086000160705       //--------------------------------------------------------------
086100160705       BEGSR  sr_ZNC_TiE;
086200160616
086300161010         // la zona di consegna la stampo da solo per ogni network non EEX
086400161010         if pIn_TiE <>'EEX';
086500161010           // 0,20 cm
086600161010           §FCD_ZNC = §FCD + 0,079;
086700161010           // 8,00 cm
086800161010           §FCA_ZNC = §FCA + 3,150;
086900161010           // altezza box 1,25 cm
087000161010           §DCD_ZNC = §FCD_ZNC + 0,398;
087100161010           // larghezza box 1,3  cm
087200161010           §DCA_ZNC = §FCA_ZNC + 0,512;
087300161010           // spessore box
087400161010           §LW_ZNC  = 0,01;
087500161010           // testo zona di consegna
087600161010           ZNC = %editc(pIn_ZNC : 'X');
087700161010           // dove inizia il testo dall'alto (come inizio box + distanza)
087800161010           // 0,90 cm
087900161010           §PD_ZNC = §FCD_ZNC + 0,354;
088000161010           // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
088100161010           §PA_ZNC = §FCA_ZNC + 0,02;
088200161010         endif;
088300160705
088400161010         // se network EUROEXPRESS
088500161010         if pIn_TiE = 'EEX';
088600161010           // 0,20 cm
088700161010           §FCD_ZNCEX = §FCD + 0,079;
088800161010           // 4,90 cm
088900161010           §FCA_ZNCEX = §FCA + 1,929;
089000161010           // altezza box 0,65 cm
089100161010           §DCD_ZNCEX = §FCD_ZNCEX + 0,256;
089200161010           // larghezza box 3,40 cm
089300161010           §DCA_ZNCEX = §FCA_ZNCEX + 1,339;
089400160705           // spessore box
089500161010           §LW_ZNCEX  = 0,01;
089600160705           // testo zona di consegna
089700161010           ZNC_ISA = %trim(pIn_ISA);
089800161010           ZNC_COD = %trim(pIn_CAD);
089900160705           // dove inizia il testo dall'alto (come inizio box + distanza)
090000161010           // 0,50 cm
090100161010           §PD_ISAEX = §FCD_ZNCEX + 0,197;
090200160705           // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
090300161010           §PA_ISAEX = §FCA_ZNCEX + 0,02;
090400161010           // dove inizia il testo dall'alto (come inizio box + distanza)
090500161010           // 0,40 cm
090600161010           §PD_CODEX = §FCD_ZNCEX + 0,157;
090700161010           // dove inizia il testo partendo da sx (come inizio box + 0950 cm)
090800161010           §PA_CODEX = §FCA_ZNCEX + 0,374;
090900161010
091000161010           // la zona di consegna la stampo in ogni caso non solo nei network ITA/DPD/FED
091100161010           // 0,20 cm
091200161010           §FCD_ZNC = §FCD + 0,079;
091300161010           // 8,40 cm
091400161010           §FCA_ZNC = §FCA + 3,307;
091500161010           // altezza box 0,70 cm
091600161010           §DCD_ZNC = §FCD_ZNC + 0,276;
091700161010           // larghezza box 0,90  cm
091800161010           §DCA_ZNC = §FCA_ZNC + 0,354;
091900161010           // spessore box
092000161010           §LW_ZNC  = 0,01;
092100161010           // testo zona di consegna
092200161010           ZNC = %editc(pIn_ZNC : 'X');
092300161010           // dove inizia il testo dall'alto (come inizio box + distanza)
092400161010           // 0,60 cm
092500161010           §PD_ZNC = §FCD_ZNC + 0,236;
092600161010           // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
092700161010           §PA_ZNC = §FCA_ZNC + 0,02;
092800161010         endif;
092900160705
093000160705       ENDSR;
093100170309
093200170309       //--------------------------------------------------------------
093300170309       // Impostazioni PRTF personalizzate
093400170309       //--------------------------------------------------------------
093500170309       BEGSR  sr_OVRPRTF;
093600170309
093700170309         // se il prtf è chiuso oppure è già aperto ma i parametri di OVR personalizzati passati
093800170309         // sono diversi da quelli con cui è stata fatta la open
093900170309         if  Not %open(UBBRTETIP) OR
094000170309             %open(UBBRTETIP) and
094100170309              (wOutQ <> pIn_OutQ OR wUsrDta <> pIn_UsrDta);
094200170309           // se PRTF aperto ma i parametri di OVR personalizzati passati
094300170309           // sono diversi da quelli con cui è stata fatta la open
094400170309           if %open(UBBRTETIP) and
094500170309            (wOutQ <> pIn_OutQ OR wUsrDta <> pIn_UsrDta);
094600170310             // chiudo il PRTF
094700170309             close UBBRTETIP;
094800170309           endif;
094900170410           //if pIn_OutQ <> *blank OR pIn_UsrDta <> *blank;
095000170310             Qcmd = 'OVRPRTF file(UBBRTETIP)';
095100170310             if pIn_OutQ <> *blank;
095200170310             Qcmd = %trim(Qcmd) +
095300170310                    ' outq(' + %trim(pIn_OutQ) + ')';
095400170410             else;
095500170410             Qcmd = %trim(Qcmd) +
095600170410                    ' outq(QPRINTS)';
095700170410             endif;
095800170310             if pIn_UsrDta <> *blank;
095900170410               Qcmd = %trim(Qcmd) +
096000170310                    ' usrdta(' + %trim(pIn_UsrDta) + ')';
096100170410             else;
096200170410               Qcmd = %trim(Qcmd) +
096300170410                    ' usrdta(P' + %editc(wCollo:'X') + ')';
096400170410             endif;
096500170309             exsr  sr_ExecCmd;
096600170310             if  Qusei <> *blank;
096700170310               pOut_Esito = 2;
096800170406               exsr sr_RoutEnd;
096900170406             else;
097000170406               // salvo i parametri di OVR personalizzati
097100170406               wOutQ = pIn_OutQ;
097200170406               wUsrDta = pIn_UsrDta;
097300170406             endif;
097400170410           //endif;
097500170309           open  UBBRTETIP;
097600170309         endif;
097700170309
097800170309       ENDSR;
097900170309
098000170309       //--------------------------------------------------------------
098100170309       // Esecuzione del comando (già impostato).                      ?
098200170309       //--------------------------------------------------------------
098300170309       BEGSR  sr_ExecCmd;
098400170309
098500170309         clear Qcap0100;
098600170309         Qcabcsdh = *off;
098700170309         Qcapa    = *off;
098800170309         Qcacmdss = *off;
098900170309         Qcaerved = *allX'00';
099000170309
099100170309         clear Qusec;
099200170309         Qusbprv  = %size(Qusec);
099300170309
099400170309         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
099500170309                           %size(Qcap0100) : 'CPOP0100' : *omit :
099600170309                           0 : 0 : Qusec);
099700170309
099800170309       ENDSR;

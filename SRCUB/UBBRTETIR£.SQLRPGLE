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
001700170807     **    aggiungere BNDDIR(UBBNDDIR) e (UBBRTETI)
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
003300170807     f                                     infds(curINFDS)
003400160704
003500160704       //--------------------------------------------------------------
003600160704       // Definizione costanti.
003700160704       //--------------------------------------------------------------
003800160704
003900160704       // - Spessore standard delle linee in inches/pollici (*memo)
004000160704     d c_LineNarrow    c                   const(0.008)
004100160704     d c_LineMedium    c                   const(0.017)
004200160704     d c_LineWide      c                   const(0.025)
004300160704
004400160704       //--------------------------------------------------------------
004500160704       // Definizione schiere.
004600160704       //--------------------------------------------------------------
004700160704
004800160704
004900160704       //--------------------------------------------------------------
005000160704       // Definizione aree dati.
005100160704       //--------------------------------------------------------------
005200160704
005300160704
005400160704       //--------------------------------------------------------------
005500160704       // Definizione strutture dati.
005600160704       //--------------------------------------------------------------
005700161107
005800161107       // parametro d'ingresso
005900161107     D UBBRTETIDS    e ds
006000160704
006100170807       // - Status DS
006200160704     d Psds           sds
006300160704     d   SDSpgm          *proc
006400160704     d   JobName             244    253                                         Job name
006500160704     d   JobUser             254    263                                         User name
006600160704     d   JobNumber           264    269s 0                                      Job number
006700170807     d   JobUserCurr         358    367                                         Current User name
006800170807
006900170807       // - Information DS
007000170807     d curINFDS        ds
007100170807     d   PFile_Nam            83     92                                         File Name
007200170807     d   PFile_Num           160    163i 0                                      File Number
007300160719
007400160719     d jobinf          ds
007500160719     d    job                        10
007600160719     d    user                       10
007700160719     d    jobn                        6
007800161011
007900161011       // reperimento check digits
008000161011     D TRUL28DS      e ds                  inz
008100160704
008200160719       // strutture per QUSRSPLA
008300160719       /copy qsysinc/qrpglesrc,QUSRSPLA
008400160615
008500160719       // parametri gestione errori API.
008600160719       /copy qsysinc/qrpglesrc,QUSEC
008700160719
008800160704       //--------------------------------------------------------------
008900160704       // Definizione variabili.
009000160704       //--------------------------------------------------------------
009100160704
009200160704       // Campi di comodo
009300160704     d wBarcode        s             18a
009400160704     d wSegnacollo     s              7s 0
009500160706     d wCollo          s              7s 0
009600160705     D wX              s              2s 0
009700160705     D w2S             s              2s 0
009800160705     D w3S             s              3s 0
009900160705     D wCheckValue     s              3s 0
010000170310     D wOutq           s                   like(pIn_OutQ)
010100170310     D wUsrDta         s                   like(pIn_UsrDta)
010200170406     D wFnumDS         ds
010300170406     D  wFnum                         6a   dim(100)
010400170420     D wUDtaDS         ds
010500170420     D  wUDta                        10a   dim(100)
010600170807     D wSpecialSpl     s               n   inz
010700170807     D wFormType       s             10a   inz
010800170807     D wJob_Nam        s             28a   inz
010900170807     D wJob_Num        s              6s 0 inz
011000170807     D wStr            s              2s 0 inz
011100170807     D wPos            s              2s 0 inz
011200170807     D oVCPEsito       s              2S 0 inz
011300170807     D oVCPNum         s             15S 0 inz
011400170807     D oVCPNumDal      s             15S 0 inz
011500170807     D oVCPIncr        s              3S 0 inz
011600170824     D DftOUTQ         s             10a   inz
011700160704
011800160719       // testo segnacollo
011900160704     d NCX             s              7
012000160718
012100160719       // parametri binary(4) di input per QUSRSPLA
012200160719     D  RCVLEN         s             10I 0
012300160719     D  FSPLN          s             10I 0
012400160623
012500160623       // - Dimensioni base per i simil-loghi Bartolini e BRT
012600160623      /copy gaitrasrc/srcTNSY,LogoBRT_R1
012700160704
012800160704       //--------------------------------------------------------------
012900160704       // Definizione prototipi procedure.
013000160704       //--------------------------------------------------------------
013100160704
013200160719       /copy gaitrasrc/srcProtoPR,QUSRSPLA
013300161011     d TRUL28R1        pr                  extpgm('TRUL28R1')
013400161011     D  pTRUL28DS                          like(TRUL28DS)
013500170309
013600170309       // Parametri API QCAPCMD (Process Commands)?
013700170309     d Qcmd            s           2048    inz  varying
013800170309      /copy qSysInc/qRpgleSrc,QCAPCMD
013900170309       // API QCAPCMD (Process Commands)?
014000170309      /copy gaitrasrc/srcProtoPR,QCAPCMD
014100170807     D/COPY GAITRASRC/SRCPROTOPR,UBVCP0R_01
014200170824     D/COPY GAITRASRC/SRCPROTOPI,TIS713R
014300160704
014400160704       //--------------------------------------------------------------
014500160704       // Definizione key-list.
014600160704       //--------------------------------------------------------------
014700160704
014800160704
014900160704       //--------------------------------------------------------------
015000160704       // Definizione parametri procedura.
015100160704       //--------------------------------------------------------------
015200160704
015300160623
015400160623       //--------------------------------------------------------------
015500160623       // M A I N - L I N E
015600160623       //--------------------------------------------------------------
015700170824     c*
015800170824     c* Definisco le opzioni con cui verranno di seguito utilizzate le istruzioni SQL
015900170824     c
016000170824     c/EXEC SQL
016100170824     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
016200170824     c/END-EXEC
016300160623
016400160623       // Operazioni iniziali?
016500160623       exsr sr_RoutInz;
016600160704
016700160704       // se non chiusura
016800160704       if pIn_TLa <>'C';
016900160705         // la prima volta il segnacollo è il primo richiesto
017000160705         wSegnacollo = pIn_NCD;
017100170410         for wCollo = pIn_NCD to pIn_NCA;
017200170410           // apro il file di stampa se non già aperto
017300170410           // (mi serve farlo all'interno del ciclo perché il nr.segnacollo può essere lo USRDTA)
017400170410           exsr sr_ovrprtf;
017500160704           // stampa le N etiichette richieste
017600160704           exsr sr_StampaEtichetta;
017700160705           // il prossimo segnacollo sarà uno più del precedente
017800160705           wSegnacollo += 1;
017900160704         endfor;
018000160704       endif;
018100160704
018200160704       // Operazioni finali?
018300160704       exsr sr_RoutEnd;
018400160704
018500160704       //--------------------------------------------------------------
018600160704       // Operazioni iniziali.
018700160704       //--------------------------------------------------------------
018800160704       BEGSR  sr_RoutInz;
018900170406
019000170406         // inizializzazioni
019100170406         clear wFnum;
019200170420         clear wUDta;
019300170411         if pIn_NCA = 0;
019400170411           pIn_NCA = pIn_NCD;
019500170411         endif;
019600160704
019700160704         // se non chiusura
019800170209         if pIn_TLa <>'C'
019900170209         //  e non segnalazione errore
020000170406          and pIn_SegErr = *blank;
020100160704
020200160704           // dati obbligatori
020300160704           if pIn_FLS = 0      OR
020400160718           // pIn_NRS = 0      OR
020500160704              pIn_NCD = 0      OR
020600160704              pIn_LNA = 0      OR
020700160704              pIn_TMA = 0      OR
020800160704              pIn_ZNC = 0      OR
020900160704              pIn_NC2 = 0;
021000160704             pOut_Esito = 2;
021100160704             exsr sr_RoutEnd;
021200160704           endif;
021300160704
021400160704           // dati errati
021500160705           // tipo etichetta può valere solo ' '/ITA/DPD/FED/EEX
021600160705           if pIn_TiE <> *blank AND
021700160705              pIn_TiE <> 'ITA'  AND
021800160705              pIn_TiE <> 'DPD'  AND
021900160705              pIn_TiE <> 'FED'  AND
022000160705              pIn_TiE <> 'EEX';
022100160705             pOut_Esito = 2;
022200160705             exsr sr_RoutEnd;
022300160705           endif;
022400160704           // tipo servizio può valere solo ' '/C/E/H/D
022500160704           if pIn_TSP <> *blank AND
022600160704              pIn_TSP <> 'C'    AND
022700160704              pIn_TSP <> 'E'    AND
022800160704              pIn_TSP <> 'H'    AND
022900160704              pIn_TSP <> 'D';
023000160704             pOut_Esito = 2;
023100160704             exsr sr_RoutEnd;
023200160704           endif;
023300160704           // Tipo lancio deve essere blank o 'C'
023400160704           if pIn_TLa <> *blank and pIn_TLa <>'C';
023500160704             // esco dando errore
023600160704             pOut_Esito = 2;
023700160704             exsr sr_RoutEnd;
023800160704           endif;
023900160704
024000160704           // congruenza segnacollo da/a - nr.colli
024100170411           //
024200170411           // i colli totali possono essere solo >= di A - DA +1
024300170411           // (i colli gestiti sono T ma ne stampo solo X<T)
024400170411           if pIn_NCA - pIn_NCD + 1 >
024500170411              pIn_NC2;
024600170411             pOut_Esito = 2;
024700170411             exsr sr_RoutEnd;
024800170411           endif;
024900160706           // se il segnacollo DA < A
025000170406           // allora c'è incongruenza
025100160706           if pIn_NCA < pIn_NCD;
025200160706             pOut_Esito = 2;
025300160706             exsr sr_RoutEnd;
025400160706           endif;
025500170411
025600170406           // se è stato chiesto uno spool per collo
025700170410           if pIn_1SplXC = 'S'
025800170406           // ma sono stati chiesti più di 100 colli (che è la dim della schiera)
025900170406            and pIn_NC2 > 100;
026000170406             // allora c'è incongruenza
026100170406             pOut_Esito = 2;
026200170406             exsr sr_RoutEnd;
026300170406           endif;
026400160704         endif;
026500160704
026600160704         // offset box / margini
026700170419         // offset Y è il First Corner Down e deve valere tra 0 e 22,750 inch.
026800170419         // offset X è il First Corner Acrosse deve valere tra 0 e 22,750 inch.
026900170419         // ma siccome nella compilazione del PRTF è definito che il AFPDS deve essere
027000170419         // Y = 2,9134 inch = 7,4 cm  e X = 4,1339 inch = 10.5 cm
027100170419         // questi sono i limiti (anzi li facciamo ancora più stringenti altrimenti non si capisce
027200170419         // perché l'etichetta non si vede)
027300170419         // se gli offset superano i limiti, vengono dati dei default. Lo stesso se sono 0.
027400170419         if pIn_OfsY > 2,8 or
027500170419            pIn_OfsY = 0;
027600170419           pIn_OfsY = 0,1772;
027700170419         endif;
027800170419         if pIn_OfsX > 4,0 or
027900170419            pIn_OfsX = 0;
028000170419           pIn_OfsX = 0,1969;
028100170419         endif;
028200170419
028300170419         §FCD = pIn_OfsY;
028400161222         §FCA = pIn_OfsX;
028500160704         // altezza etichetta fissa a 6,5 cm = 2,559 inch
028600160704         §DCD = §FCD + 2,559;
028700160704         // larghezza etichetta fissa a 9,5 cm = 3,740 inch
028800160704         §DCA = §FCA + 3,74;
028900160704         // spess
029000160704         §LW  = 0,01;
029100160704
029200160704         // dati valorizzabili a pgm
029300160704         if pIn_AAS = 0;
029400160705           pIn_AAS = %dec(%subst(%char(%date()) : 1 : 4) : 4 : 0);
029500160704         endif;
029600160705         if pIn_MGS = 0;
029700160705           pIn_MGS = %dec(%subst(%char(%date()) : 6 : 2) : 2 : 0) * 1000 +
029800160705                     %dec(%subst(%char(%date()) : 9 : 2) : 2 : 0);
029900160704         endif;
030000160704
030100160704       ENDSR;
030200160704
030300160704       //--------------------------------------------------------------
030400160704       // Operazioni finali.
030500160704       //--------------------------------------------------------------
030600160704       BEGSR  sr_RoutEnd;
030700170110
030800170110         // Chiusura pgm
030900170110         if pIn_TLa = 'C';
031000170110           *inlr = *on;
031100170110         else;
031200160718
031300170110           // se l'esito è corretto
031400170406           if pOut_Esito = 0
031500170406           // e non è stato chiesto uno spool per collo (altrimenti quello che segue è già stato
031600170406           // fatto)
031700170410            and pIn_1SplXC <> 'S';
031800170110             // reperisco i dati dello spool
031900170110             // 1. chiudo il file di stampa
032000170309             if %open(UBBRTETIP);
032100170110               close UBBRTETIP;
032200170110             endif;
032300170110             // 2. reperisco l'ultimo spool creato da questo job e col nome del PRTF
032400170807             if wSpecialSpl;
032500170807                exsr sr_RtvSpoolJob;
032600170807             else;
032700170807                job  = JobName;
032800170807                user = JobUserCurr;
032900170807                jobn = %editc(JobNumber:'X');
033000170807             endif;
033100170110             FSPLN = -1;
033200170110             RCVLEN = %len(QUSA010001);
033300170110             RetrieveSpooledFileAttributes
033400170110              ( QUSA010001 :
033500170110                RCVLEN :
033600170110                'SPLA0100' :
033700170110                jobinf :
033800170110                *blank : *blank :
033900170110                'UBBRTETIP' :
034000170110                FSplN     );
034100170110             // 3. valorizzo i dati di output
034200170110             pOut_FNam = QUSSN01;
034300170110             pOut_JNam = QUSJN10;
034400170110             pOut_UNam = QUSUN12;
034500170110             pOut_JNum = QUSJNBR09;
034600170110             pOut_Fnum = %char(QUSSNBR);
034700170110           endif;
034800170406
034900170406           // se l'esito è corretto
035000170406           if pOut_Esito = 0
035100170406           // ma è stato chiesto uno spool per collo
035200170410            and pIn_1SplXC = 'S';
035300170406             // aggiorno il numero di file con la schiera
035400170406             pOut_Fnum = wFnumDS;
035500170420             pOut_UDTA = wUDtaDS;
035600170406           endif;
035700170110
035800170110           if  qUsei = *blank;
035900170110           //s_ObjTxt = qUstd12;
036000170110           else;
036100170110           //s_ObjTxt = *all'? ';
036200170110           endif;
036300170110
036400160704           *inrt = *on;
036500160704         endif;
036600160704         return;
036700160704
036800160704       ENDSR;
036900160704
037000160704       //--------------------------------------------------------------
037100160704       // Stampa le N etiichette richieste
037200160704       //--------------------------------------------------------------
037300160704       BEGSR  sr_StampaEtichetta;
037400160704
037500160704         // preparo l'etichetta
037600160704         exsr sr_PreparaEtichetta;
037700160704
037800160704         // stampo
037900170209
038000170209         if pIn_SegErr = *blank;
038100160704
038200160704         select;
038300160704         // pagseg con tipo servizio E
038400160704          when *in02;
038500170627           write TSP_RI;
038600160704         // pagseg con tipo servizio H
038700160704          when *in03;
038800170627           write TSP_RI;
038900160704         endsl;
039000160705         // tipo etichetta
039100160705         select;
039200160705          // network Italia
039300160705          when pIn_TiE = 'ITA' or pIn_TiE = *blank;
039400160705           write ZNCITA;
039500160705          // network DPD
039600160705          when pIn_TiE = 'DPD';
039700160705           // per ora non ci sono differenze dall'Italia
039800160705           write ZNCITA;
039900160705          // network FEDEX
040000160705          when pIn_TiE = 'FED';
040100160705           // per ora non ci sono differenze dall'Italia
040200160705           write ZNCITA;
040300161007          // network EuroExpress (stampo sia i dati esteri che la zona)
040400160705          when pIn_TiE = 'EEX';
040500160705           write ZNCEEX;
040600160705         endsl;
040700160704         // etichetta
040800160704         write BRTETI01;
040900170209
041000170209         // se c'è da stampare una segnalazione d'errore
041100170209         else;
041200170209           write MsgErr;
041300170209         endif;
041400170209
041500170210         // logo
041600170210         if pIn_Logo <> 'N';
041700170210           write  LogoBRT_SM;
041800170210         endif;
041900160706         // fine pagina
042000160706         write ENDPAGE;
042100170406
042200170406         // se è richiesta la stampa per singolo collo
042300170410         if pIn_1SplXC= 'S';
042400170406           // reperisco i dati dello spool
042500170406           // 1. chiudo il file di stampa
042600170406           if %open(UBBRTETIP);
042700170406             close UBBRTETIP;
042800170406           endif;
042900170406           // 2. reperisco l'ultimo spool creato da questo job e col nome del PRTF
043000170807           if wSpecialSpl;
043100170807              exsr sr_RtvSpoolJob;
043200170807           else;
043300170807              job  = JobName;
043400170807              user = JobUserCurr;
043500170807              jobn = %editc(JobNumber:'X');
043600170807           endif;
043700170406           FSPLN = -1;
043800170406           RCVLEN = %len(QUSA010001);
043900170406           RetrieveSpooledFileAttributes
044000170406            ( QUSA010001 :
044100170406              RCVLEN :
044200170406              'SPLA0100' :
044300170406              jobinf :
044400170406              *blank : *blank :
044500170406              'UBBRTETIP' :
044600170406              FSplN     );
044700170406           // 3. valorizzo i dati di output
044800170406           pOut_FNam = QUSSN01;
044900170406           pOut_JNam = QUSJN10;
045000170406           pOut_UNam = QUSUN12;
045100170406           pOut_JNum = QUSJNBR09;
045200170406           wFnum(wCollo - pIn_NCD + 1) = %char(QUSSNBR);
045300170420           wUDta(wCollo - pIn_NCD + 1) = QUSUD01;
045400170406         endif;
045500160704
045600160704       ENDSR;
045700160704
045800160704       //--------------------------------------------------------------
045900160704       // Prepara etichetta
046000160704       //--------------------------------------------------------------
046100160704       BEGSR  sr_PreparaEtichetta;
046200170209
046300170209         if pIn_SegErr = *blank;
046400160704
046500160704       // stampa bordo etichetta
046600160704       if pIn_Bordo = 'S';
046700160704         *in01 = *on;
046800160704       else;
046900160704         *in01 = *off;
047000160704       endif;
047100160704
047200160704       // stampa riga di contrasto
047300170830       if pIn_Row =  'S';
047400160704         *in04 = *on;
047500160704       else;
047600160704         *in04 = *off;
047700160704       endif;
047800160705
047900160705       // R di ristampa
048000161005       *in05 = *off;
048100161005       clear Rst;
048200161005       clear Val;
048300161005sel 1  select;
048400161005w   1   when pIn_rst  =  *blanks;
048500161005w   1   when pIn_rst  =  'V';
048600161005         Val =  'V';
048700161005w   1   when pIn_rst  =  'X';
048800161005         Rst  =  'R';
048900161005         Val  =  'V';
049000161005         *in05 = *on;
049100161005w   1     when pIn_rst  =  'W';
049200161005         Rst  =  'M';
049300161005         Val  =  'V';
049400161005         *in05 = *on;
049500161005x   1   other;
049600161005         Rst  =  pIn_rst;
049700161005         *in05 = *on;
049800161005e   1  endsl;
049900160704
050000160616       // box terminal arrivo
050100160628       // 0,20 cm
050200160628       §FCD_TMA = §FCD + 0,079;
050300160621       // 1,50 cm
050400160621       §FCA_TMA = §FCA + 0,591;
050500161010       // altezza box 1,15 cm
050600161010       §DCD_TMA = §FCD_TMA + 0,453;
050700161010       // larghezza box 1,93 cm
050800161010       §DCA_TMA = §FCA_TMA + 0,760;
050900160704       // spessore box
051000160617       §LW_TMA  = 0,01;
051100160616
051200160616       // box zona consegna
051300160705       // box e testo dipendono dal tipo etichetta
051400160705       exsr sr_ZNC_TiE;
051500160620
051600160620       // box tipo servizio
051700160706       if pIn_TSP = 'C'     OR
051800160706          pIn_TSP = *blank;
051900160706         // 2,30 cm
052000160706         §FCD_TSP = §FCD + 0,906;
052100160706         // 0,35 cm
052200160706         §FCA_TSP = §FCA + 0,138;
052300160706         // altezza box 0,80 cm
052400160706         §DCD_TSP = §FCD_TSP + 0,315;
052500170626         // larghezza box 0,80 cm
052600160706         §DCA_TSP = §FCA_TSP + 0,315;
052700160706         // spessore box
052800160706         §LW_TSP  = 0,01;
052900160706       endif;
053000170525       // linea tipo servizio
053100170525       if pIn_TSP = 'H'     OR
053200170525          pIn_TSP = 'E';
053300170525         // spessore linea per simulare quadrato
053400170627         §LW_TSP  = 0,315;
053500170525       endif;
053600160620
053700160620       // linea di separazione serie segnacollo
053800160620       // dove inizia la linea dall'alto (bordo + distanza)
053900160628       // 5,15 cm
054000160628       §PD_L01 = §FCD + 2,028;
054100160620       // dove inizia la linea partendo da sx (bordo + distanza)
054200160620       // 1,65 cm
054300160620       §PA_L01 = §FCA + 0,650;
054400160620       // 0,3 cm lunghezza linea
054500160620       §LL_L01 = 0,118;
054600160620       // spessore linea
054700160620       §LW_L01  = 0,03;
054800160615
054900160615       // linea di separazione mittente/destinatario
055000160706       // 1,60 cm (la linea è verticale ma punta verso il basso)
055100160706       §PD = §FCD + 0,630;
055200160706       // 8,2 cm
055300160617       §PA = §FCA + 3,228;
055400160706       // 4,6 cm lunghezza linea
055500160706       §LL = 1,811;
055600160615       // stesso spessore delle righe del box
055700160615       //§LW  = 0,01;
055800160615
055900160704       // calcolo barcode 18 char (l'ultimo è il check digit)
056000160704       // compongo barcode e calcolo chek digit
056100161011       wBarcode = %editc(pIn_FLS:'X') +
056200161011                   %editc(pIn_LNA:'X') +
056300161011                   %editc(pIn_NRS:'X') +
056400161011                   %editc(wSegnacollo:'X') +
056500161011                   %editc(pIn_ZNC:'X');
056600161011       // siccome ho testato all'inizio che i dati non siano vuoti, anche il barcode è valorizzato
056700161011       // calcolo il check digit
056800161011       clear TRUL28DS;
056900161011       I28Mod  = 'BAR';
057000161011       I28COD = %trim(wBarcode);
057100161011       TRUL28R1(TRUL28DS);
057200161011       // Se tutto ok aggiungo il check digit
057300161011       if O28err = *blanks;
057400161011         wBarcode = O28Cod;
057500161011       endif;
057600160704       Bcd = wBarcode;
057700160615       // dove inizia il barcode dall'alto (in alto)
057800160706       // che coincide con dove inizia la linea di separazione/mittente destinatario
057900160628       // 1,60 cm
058000160706       //§PD_BCD = §FCD + 0,630;
058100160706       §PD_BCD = §PD;
058200160621       // dove inizia il barcode da sx
058300160627       // 2,00 cm
058400160704       §PA_BCD = §FCA + 0,787;
058500160630
058600160630       // linea sopra al barcode
058700160630       // spessore linea
058800160630       §LW_L02  = 0,06;
058900160630       // i punti di inizio sono:
059000160630       // verticale: quello del barcode meno la metà dello spessore della linea
059100160630       // orizzontale: la distanza dal bordo del box terminal arrivo
059200160704       §PD_L02 = §PD_BCD - §LW_L02/2;
059300160630       §PA_L02 = §FCA_TMA;
059400160706       // la lunghezza tra l'inizio del box TMA e 0,04 cm prima della riga di separazione
059500160706       §LL_L02 = (§PA - 0,157) - §FCA_TMA;
059600160617
059700160617       // testo terminal arrivo
059800160705       TMA = %editc(pIn_TMA : 'X');
059900160628       // dove inizia il testo dall'alto (come inizio box + distanza)
060000161010       // 1,00 cm
060100161010       §PD_TMA = §FCD_TMA + 0,394;
060200160617       // dove inizia il testo partendo da sx (come l'inizio box + 0,05 cm)
060300160617       §PA_TMA = §FCA_TMA + 0,020;
060400160616
060500160616       // testo linea di arrivo
060600160705       LNA = %editc(pIn_LNA : 'X');
060700160616       // dove inizia il testo dall'alto (bordo + distanza)
060800161010       // 1,05 cm
060900161010       §PD_LNA = §FCD + 0,413;
061000160616       // dove inizia il testo partendo da sx (bordo + distanza)
061100160616       // 0,10 cm
061200160616       §PA_LNA = §DCA_TMA + 0,039;
061300160616
061400160621       // testo tipo spedizione
061500160704       *in02 = *off;
061600160704       *in03 = *off;
061700160621       select;
061800160621        when pIn_TSP = 'C';
061900160621         TSP = 'C';
062000160623         // dove inizia il testo dall'alto (bordo + distanza)
062100160623         // 3,00 cm
062200160623         §PD_TSP = §FCD + 1,181;
062300160623         // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
062400160623         §PA_TSP = §FCA_TSP + 0,02;
062500170526        when pIn_TSP = 'E';
062600160704         *in02 = *on;
062700170526         TSP_Rev= 'E';
062800160623         // 2,30 cm
062900170525         //§PD_TSP_E = §FCD + 0,906;
063000170525         §FCD_TSP = §FCD + 0,906;
063100170525         // dove inizia il testo dall'alto (bordo + distanza)
063200170525         §PD_TSP = §FCD + 1,181;
063300160623         // 0,35 cm
063400170525         //§PA_TSP_E = §FCA + 0,138;
063500170525         §FCA_TSP = §FCA + 0,138;
063600170525         // dove inizia il testo partendo da sx (come inizio linea + 0,127 cm)
063700170525         §PA_TSP = §FCA_TSP + 0,05;
063800170525        when pIn_TSP = 'H';
063900170526         *in03 = *on;
064000170526         TSP_Rev= 'H';
064100170525         // 2,30 cm
064200170525         //§PD_TSP_E = §FCD + 0,906;
064300170525         §FCD_TSP = §FCD + 0,906;
064400170525         // dove inizia il testo dall'alto (bordo + distanza)
064500170525         §PD_TSP = §FCD + 1,181;
064600170525         // 0,35 cm
064700170525         //§PA_TSP_E = §FCA + 0,138;
064800170525         §FCA_TSP = §FCA + 0,138;
064900170526         // dove inizia il testo partendo da sx (come inizio linea + 0,1 cm)
065000170526         §PA_TSP = §FCA_TSP + 0,0394;
065100160704        when pIn_TSP = 'D';
065200160704         TSP = *blank;
065300160704         // dove inizia il testo dall'alto (bordo + distanza)
065400160704         // 3,00 cm
065500160704         §PD_TSP = §FCD + 1,181;
065600160704         // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
065700160704         §PA_TSP = §FCA_TSP + 0,02;
065800160706        when pIn_TSP = *blank;
065900160706         TSP = *blank;
066000160706         // dove inizia il testo dall'alto (bordo + distanza)
066100160706         // 3,00 cm
066200160706         §PD_TSP = §FCD + 1,181;
066300160706         // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
066400160706         §PA_TSP = §FCA_TSP + 0,02;
066500160621       endsl;
066600160620
066700160620       // testo linea di partenza
066800160705       LNP = %editc(pIn_FLS : 'X');
066900160620       // dove inizia il testo dall'alto (bordo + distanza)
067000160621       // 3,85 cm
067100160621       §PD_LNP = §FCD + 1,516;
067200160620       // dove inizia il testo partendo da sx (bordo + distanza)
067300160620       // 0,35 cm
067400160620       §PA_LNP = §FCA + 0,138;
067500160620
067600160620       // testo nr. serie
067700160705       NRS = %editc(pIn_NRS : 'X');
067800160620       // dove inizia il testo dall'alto (bordo + distanza)
067900160628       // 5,55 cm
068000160628       §PD_NRS = §FCD + 2,185;
068100160620       // dove inizia il testo partendo da sx (bordo + distanza)
068200160620       // 0,35 cm
068300160620       §PA_NRS = §FCA + 0,138;
068400161006       // se il numero serie è 0, non lo devo stampare, nè questo nè il trattino
068500161006       *in06 = (pIn_Nrs = 0);
068600160620
068700160620       // testo segnacollo (si divide in 2 parti)
068800160704       NCx = %editc(wSegnacollo : 'X');
068900160620       NCx1 = %subst(NCx : 1 : 4);
069000160620       NCx2 = %subst(NCx : 5 : 3);
069100160620       // dove inizia il testo dall'alto (bordo + distanza)
069200160628       // 5,55 cm
069300160628       §PD_NCx1 = §FCD + 2,185;
069400160620       // dove inizia il testo partendo da sx (bordo + distanza)
069500160620       // 2,10 cm
069600160620       §PA_NCx1 = §FCA + 0,827;
069700160620       // dove inizia il testo dall'alto (bordo + distanza)
069800160628       // 5,65 cm
069900160628       §PD_NCx2 = §FCD + 2,224;
070000160620       // dove inizia il testo partendo da sx (bordo + distanza)
070100160620       // 4,55 cm
070200160620       §PA_NCx2 = §FCA + 1,791;
070300160622
070400160704       // testo Riferimeno RMA/CDP
070500170830       clear Rif_RMA;
070600170830       if pIn_CDP <> *blank;
070700170830         //          Rif.: - RMA 15 CHAR -
070800170830         Rif_RMA  = 'Rif.: ' + %trim(pIn_CDP);
070900170830       endif;
071000160622       // dove inizia il testo dall'alto
071100160706       // che coincide con inizia linea separazione mittente/destinatario + sua lunghezza
071200160706       §PD_RMA = §PD + §LL;
071300160622       // dove inizia il testo partendo da sx
071400160622       // che coincide con dove inizia la serie
071500160622       §PA_RMA = §PA_NRS;
071600160622
071700160622       // testo data spedizione
071800160622       MGS_AAS  = %subst(%editc(pIn_MGS:'X'):3:2) + '/' +
071900160622                  %subst(%editc(pIn_MGS:'X'):1:2) + '/' +
072000160622                  %char(pIn_AAS);
072100160622       // dove inizia il testo dall'alto
072200160706       // che coincide con dove finisce la riga di separazione
072300160706       §PD_DTS = §PD + §LL;
072400160622       // dove inizia il testo partendo da sx (bordo + distanza)
072500160622       // 4,50 cm
072600160622       §PA_DTS = §FCA + 1,772;
072700160705
072800161005       // Flag Ristampa
072900160705       // dove inizia il testo dall'alto
073000160706       // che coincide con dove finisce la riga di separazione
073100160706       §PD_RST = §PD + §LL;
073200160705       // dove inizia il testo partendo da sx (dove inizia la data + distanza)
073300161005       // 1,65 cm
073400161005       §PA_RST = §PA_DTS + 0,650;
073500161005
073600161005       // Flag Merce a valore
073700161005       // dove inizia il testo dall'alto
073800161005       // che coincide con dove finisce la riga di separazione
073900161005       §PD_VAL = §PD + §LL;
074000161005       // dove inizia il testo partendo da sx (dove inizia il flag Ristampa + distanza)
074100161005       // 0,50 cm
074200161005       §PA_VAL = §PA_RST + 0,197;
074300160616
074400160616       // testo mittente ragione sociale / provincia
074500160706       //          RAGIONE SOCIALE 26 CHAR --  Pr
074600160706       Mittente = %trim(%subst(pIn_RSM : 1 : 26)) + '  ' + pIn_PRM;
074700160616       // dove inizia il testo dall'alto
074800160616       // che coincide con dove finisce la riga di separazione
074900160616       §PD_01 = §PD + §LL;
075000160616       // dove inizia il testo partendo da sx (bordo + distanza)
075100160628       // 0,04 cm prima della riga di separazione
075200160620       §PA_01 = §PA - 0,016;
075300160616
075400160616       // testo destinatario 1 ragione sociale + provincia
075500160706       //          RAGIONE SOCIALE 26 CHAR --  Pr
075600160706       // oppure ragione sociale + nazione se no ITALIA
075700160706       //          RAGIONE SOCIALE 26 CHAR --  Pr
075800160706       if pIn_NZD = *blank;
075900160718         Dest_01 = %trim(%subst(pIn_RSD : 1 : 26)) +
076000160718                   '  ' + pIn_PRD;
076100160706       else;
076200160718         Dest_01 = %trim(%subst(pIn_RSD : 1 : 26)) +
076300160718                   '  ' + pIn_NZD;
076400160706       endif;
076500160616       // dove inizia il testo dall'alto
076600160616       // che coincide con dove finisce la riga di separazione
076700160616       §PD_D01 = §PD + §LL;
076800160616       // dove inizia il testo partendo da sx (bordo + distanza)
076900160616       // 0,3  cm dopo     la riga di separazione
077000160616       §PA_D01 = §PA + 0,118;
077100160616
077200160620       // testo destinatario 2 indirizzo + località + CAP (CAP no)
077300160706       //          INDIRIZZO 21 CHAR ----   LOCALIT 10
077400160718       Dest_02 = %trim(%subst(pIn_IND : 1 : 21)) + '   ' +
077500160718                 %trim(%subst(pIn_LOD : 1 : 10)) ;
077600160616       // dove inizia il testo dall'alto
077700160616       // che coincide con dove finisce la riga di separazione
077800160616       §PD_D02 = §PD + §LL;
077900160616       // dove inizia il testo partendo da sx (bordo + distanza)
078000160616       // 0,3 + 0,3  cm dopo     la riga di separazione
078100160616       §PA_D02 = §PA + 0,118 + 0,118;
078200160621
078300160621       // testo spedizione MC. xx,xxx   xxxxx - xxxxx   KG. xxxxx,x
078400160622       //123456789012345678901234567890123456789012345678901234567890
078500160706       //MC. xx,xxx   xxxxx - xxxxx   KG. xxxxxx,x
078600160622       Spedizione =
078700160706        'MC.                -         KG.         ';
078800160706       %subst(Spedizione : 5 : 6) = %trim(%editw(pIn_VOL:'0 ,   '));
078900170411       // se è stato passato un nr.segnacollo preciso, sarà quello il primo valore
079000160704       if pIn_NC1 <> 0;
079100160706         %subst(Spedizione : 14 : 5) = %trim(%editc(pIn_NC1:'Z'));
079200170411       // se NON è stato passato un nr.segnacollo preciso, il primo valore sarà il
079300170411       // progressivo collo
079400160704       else;
079500160706         %subst(Spedizione : 14 : 5) =
079600170411           %trim(%editc(wCollo :'Z'));
079700160704       endif;
079800160706       %subst(Spedizione : 22 : 5) = %trim(%editc(pIn_NC2:'Z'));
079900160706       %subst(Spedizione : 34 : 8) = %trim(%editw(pIn_PKG:'    0 , '));
080000160621       // dove inizia il testo dall'alto
080100160621       // che coincide con dove finisce la riga di separazione
080200160621       §PD_S01 = §PD + §LL;
080300160621       // dove inizia il testo partendo da sx (bordo + distanza)
080400160621       // 0,3 + 0,3 + 0,4 cm dopo     la riga di separazione
080500160621       §PA_S01 = §PA + 0,118 + +0,118 + 0,157;
080600170209
080700170209         // se c'è da stampare una segnalazione d'errore
080800170209         else;
080900170209           Msg_Tit = pIn_MsgErT;
081000170209           Msg_Er1   = %subst(pIn_MsgErr:1:20)  ;
081100170209           Msg_Er2   = %subst(pIn_MsgErr:21:20) ;
081200170209           Msg_Er3   = %subst(pIn_MsgErr:41:20) ;
081300170209           Msg_Er4   = %subst(pIn_MsgErr:61:40) ;
081400170209           Msg_Er5   = %subst(pIn_MsgErr:101:40) ;
081500170209           Msg_Er6   = %subst(pIn_MsgErr:141:40) ;
081600170209           Msg_Er7   = %subst(pIn_MsgErr:181:40) ;
081700170209           // dove inizia dall'alto (bordo + distanza)
081800170209           // 1,00 cm
081900170209           §PD_MsgT = §FCD + 0,3937;
082000170209           // dove inizia da sx (bordo + distanza)
082100170209           // 2,00 cm
082200170209           §PA_MsgT = §FCA + 0,7874;
082300170209           // dove inizia dall'alto (bordo + distanza)
082400170210           // 2,50 cm
082500170210           §PD_MsgE1= §FCD + 0,9842;
082600170209           // dove inizia da sx (bordo + distanza)
082700170210           // 0,50 cm
082800170210           §PA_MsgE1= §FCA + 0,1969;
082900170209           // dove inizia dall'alto (bordo + distanza)
083000170210           // 3,20 cm
083100170210           §PD_MsgE2= §FCD + 1,2598;
083200170209           // dove inizia da sx (bordo + distanza)
083300170210           // 0,50 cm
083400170210           §PA_MsgE2= §FCA + 0,1969;
083500170209           // dove inizia dall'alto (bordo + distanza)
083600170210           // 3,90 cm
083700170210           §PD_MsgE3= §FCD + 1,5354;
083800170209           // dove inizia da sx (bordo + distanza)
083900170210           // 0,50 cm
084000170210           §PA_MsgE3= §FCA + 0,1969;
084100170209           // dove inizia dall'alto (bordo + distanza)
084200170210           // 4,60 cm
084300170210           §PD_MsgE4= §FCD + 1,8110;
084400170214           // dove inizia da sx (bordo + distanza)
084500170214           // 0,10 cm
084600170214           §PA_MsgE4= §FCA + 0,0394;
084700170209           // dove inizia dall'alto (bordo + distanza)
084800170210           // 5,10 cm
084900170210           §PD_MsgE5= §FCD + 2,0079;
085000170209           // dove inizia da sx (bordo + distanza)
085100170214           // 0,10 cm
085200170214           §PA_MsgE5= §FCA + 0,0394;
085300170209           // dove inizia dall'alto (bordo + distanza)
085400170210           // 5,60 cm
085500170210           §PD_MsgE6= §FCD + 2,2047;
085600170209           // dove inizia da sx (bordo + distanza)
085700170214           // 0,10 cm
085800170214           §PA_MsgE6= §FCA + 0,0394;
085900170209           // dove inizia dall'alto (bordo + distanza)
086000170210           // 6,10 cm
086100170210           §PD_MsgE7= §FCD + 2,4016;
086200170209           // dove inizia da sx (bordo + distanza)
086300170214           // 0,10 cm
086400170214           §PA_MsgE7= §FCA + 0,0394;
086500170209         endif;
086600160615
086700160704       clear LogoBRT;
086800160704       // Simil-Logo BRT se richiesto
086900160704       if pIn_Logo <> 'N';
087000160623         //box logo
087100160623         // 0,20 cm
087200160623         §PlblS  = §FCD    + 0,079;
087300160623         // 0,35 cm
087400160623         §PlblVS = §FCA    + 0.079;
087500160623         // altezza box 0,70 cm
087600160623         §PlblI  = §PlblS  + 0,276;
087700160623         // larghezza box 0,85 cm
087800160623         §PlblVD = §PlblVS + 0,335;
087900160704         exsr  sr_SimilLogoBRT_;
088000160704       endif;
088100160704
088200160704       ENDSR;
088300160615
088400160623       // valorizzo campi programma per stampa logo BRT
088500160623      /end-free
088600160705
088700160705      /copy gaitrasrc/srcTNSY,LogoBRT_R2
088800160705
088900160623      /free
089000160705
089100160705       //--------------------------------------------------------------
089200160705       // Compongo Box e Testo Zona Consegna a seconda del Tipo Etichetta
089300160705       //--------------------------------------------------------------
089400160705       BEGSR  sr_ZNC_TiE;
089500160616
089600161010         // la zona di consegna la stampo da solo per ogni network non EEX
089700161010         if pIn_TiE <>'EEX';
089800161010           // 0,20 cm
089900161010           §FCD_ZNC = §FCD + 0,079;
090000161010           // 8,00 cm
090100161010           §FCA_ZNC = §FCA + 3,150;
090200161010           // altezza box 1,25 cm
090300161010           §DCD_ZNC = §FCD_ZNC + 0,398;
090400161010           // larghezza box 1,3  cm
090500161010           §DCA_ZNC = §FCA_ZNC + 0,512;
090600161010           // spessore box
090700161010           §LW_ZNC  = 0,01;
090800161010           // testo zona di consegna
090900161010           ZNC = %editc(pIn_ZNC : 'X');
091000161010           // dove inizia il testo dall'alto (come inizio box + distanza)
091100161010           // 0,90 cm
091200161010           §PD_ZNC = §FCD_ZNC + 0,354;
091300161010           // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
091400161010           §PA_ZNC = §FCA_ZNC + 0,02;
091500161010         endif;
091600160705
091700161010         // se network EUROEXPRESS
091800161010         if pIn_TiE = 'EEX';
091900161010           // 0,20 cm
092000161010           §FCD_ZNCEX = §FCD + 0,079;
092100161010           // 4,90 cm
092200161010           §FCA_ZNCEX = §FCA + 1,929;
092300161010           // altezza box 0,65 cm
092400161010           §DCD_ZNCEX = §FCD_ZNCEX + 0,256;
092500161010           // larghezza box 3,40 cm
092600161010           §DCA_ZNCEX = §FCA_ZNCEX + 1,339;
092700160705           // spessore box
092800161010           §LW_ZNCEX  = 0,01;
092900160705           // testo zona di consegna
093000161010           ZNC_ISA = %trim(pIn_ISA);
093100161010           ZNC_COD = %trim(pIn_CAD);
093200160705           // dove inizia il testo dall'alto (come inizio box + distanza)
093300161010           // 0,50 cm
093400161010           §PD_ISAEX = §FCD_ZNCEX + 0,197;
093500160705           // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
093600161010           §PA_ISAEX = §FCA_ZNCEX + 0,02;
093700161010           // dove inizia il testo dall'alto (come inizio box + distanza)
093800161010           // 0,40 cm
093900161010           §PD_CODEX = §FCD_ZNCEX + 0,157;
094000161010           // dove inizia il testo partendo da sx (come inizio box + 0950 cm)
094100161010           §PA_CODEX = §FCA_ZNCEX + 0,374;
094200161010
094300161010           // la zona di consegna la stampo in ogni caso non solo nei network ITA/DPD/FED
094400161010           // 0,20 cm
094500161010           §FCD_ZNC = §FCD + 0,079;
094600161010           // 8,40 cm
094700161010           §FCA_ZNC = §FCA + 3,307;
094800161010           // altezza box 0,70 cm
094900161010           §DCD_ZNC = §FCD_ZNC + 0,276;
095000161010           // larghezza box 0,90  cm
095100161010           §DCA_ZNC = §FCA_ZNC + 0,354;
095200161010           // spessore box
095300161010           §LW_ZNC  = 0,01;
095400161010           // testo zona di consegna
095500161010           ZNC = %editc(pIn_ZNC : 'X');
095600161010           // dove inizia il testo dall'alto (come inizio box + distanza)
095700161010           // 0,60 cm
095800161010           §PD_ZNC = §FCD_ZNC + 0,236;
095900161010           // dove inizia il testo partendo da sx (come inizio box + 0,05 cm)
096000161010           §PA_ZNC = §FCA_ZNC + 0,02;
096100161010         endif;
096200160705
096300160705       ENDSR;
096400170309
096500170309       //--------------------------------------------------------------
096600170309       // Impostazioni PRTF personalizzate
096700170309       //--------------------------------------------------------------
096800170309       BEGSR  sr_OVRPRTF;
096900170309
097000170309         // se il prtf è chiuso oppure è già aperto ma i parametri di OVR personalizzati passati
097100170309         // sono diversi da quelli con cui è stata fatta la open
097200170309         if  Not %open(UBBRTETIP) OR
097300170309             %open(UBBRTETIP) and
097400170309              (wOutQ <> pIn_OutQ OR wUsrDta <> pIn_UsrDta);
097500170309           // se PRTF aperto ma i parametri di OVR personalizzati passati
097600170309           // sono diversi da quelli con cui è stata fatta la open
097700170309           if %open(UBBRTETIP) and
097800170309            (wOutQ <> pIn_OutQ OR wUsrDta <> pIn_UsrDta);
097900170310             // chiudo il PRTF
098000170309             close UBBRTETIP;
098100170309           endif;
098200170410           //if pIn_OutQ <> *blank OR pIn_UsrDta <> *blank;
098300170310             Qcmd = 'OVRPRTF file(UBBRTETIP)';
098400170310             if pIn_OutQ <> *blank;
098500170310             Qcmd = %trim(Qcmd) +
098600170310                    ' outq(' + %trim(pIn_OutQ) + ')';
098700170410             else;
098800170410             Qcmd = %trim(Qcmd) +
098900170410                    ' outq(QPRINTS)';
099000170410             endif;
099100170310             if pIn_UsrDta <> *blank;
099200170410               Qcmd = %trim(Qcmd) +
099300170310                    ' usrdta(' + %trim(pIn_UsrDta) + ')';
099400170410             else;
099500170410               Qcmd = %trim(Qcmd) +
099600170410                    ' usrdta(P' + %editc(wCollo:'X') + ')';
099700170410             endif;
099800170807             exsr  sr_RtvSpoolID;
099900170309             exsr  sr_ExecCmd;
100000170310             if  Qusei <> *blank;
100100170310               pOut_Esito = 2;
100200170406               exsr sr_RoutEnd;
100300170406             else;
100400170406               // salvo i parametri di OVR personalizzati
100500170406               wOutQ = pIn_OutQ;
100600170406               wUsrDta = pIn_UsrDta;
100700170406             endif;
100800170410           //endif;
100900170309           open  UBBRTETIP;
101000170309         endif;
101100170309
101200170309       ENDSR;
101300170309
101400170309       //--------------------------------------------------------------
101500170309       // Esecuzione del comando (già impostato).                      ?
101600170309       //--------------------------------------------------------------
101700170309       BEGSR  sr_ExecCmd;
101800170309
101900170309         clear Qcap0100;
102000170309         Qcabcsdh = *off;
102100170309         Qcapa    = *off;
102200170309         Qcacmdss = *off;
102300170309         Qcaerved = *allX'00';
102400170309
102500170309         clear Qusec;
102600170309         Qusbprv  = %size(Qusec);
102700170309
102800170309         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
102900170309                           %size(Qcap0100) : 'CPOP0100' : *omit :
103000170309                           0 : 0 : Qusec);
103100170309
103200170309       ENDSR;
103300170807
103400170807       //--------------------------------------------------------------
103500170807       // Reperimento ID univoco pe rlo spool corrente                 ?
103600170807       //--------------------------------------------------------------
103700170807       BEGSR  sr_RtvSpoolID;
103800170807
103900170807        wSpecialSpl = *off;
104000170807
104100170807        // Effettuo reperimento ID spool solo se specificatamente richiesta OUTQ = 'VASSPOOL'
104200170824        // o meglio, la OUTQ indicata in tab.VPR
104300170824        if pIn_OutQ = DftOUTQ;
104400170807
104500170807        wSpecialSpl = *on;
104600170807
104700170807        clear oVCPEsito;
104800170807        clear oVCPNum;
104900170807        clear oVCPNumDal;
105000170807        clear oVCPIncr;
105100170807
105200170807        UBVCP0R_PRG('UBBRTETI'
105300170807                   :'SPOOL_ID_UNIQUE'
105400170807                   :oVCPEsito
105500170807                   :oVCPNum
105600170807                   :1
105700170807                   :oVCPNumDal
105800170807                   :oVCPIncr);
105900170807
106000170807       if oVCPEsito <> *zeros;
106100170807          pOut_Esito = 2;
106200170807          exsr sr_RoutEnd;
106300170807       else;
106400170807          wFormType = 'I' + %subst(%editc(oVCPNumDal:'X'):7:9);
106500170807          Qcmd = %trim(Qcmd) +
106600170807                 ' formtype('+%trim(wFormType)+')';
106700170807       endif;
106800170807       endif;
106900170807
107000170807       ENDSR;
107100170807
107200170807       //--------------------------------------------------------------
107300170807       // Reperimento info JOB spool corrente                 ?
107400170807       //--------------------------------------------------------------
107500170807       BEGSR  sr_RtvSpoolJob;
107600170807
107700170807         // Inizializzo ID Job spool
107800170807         clear wJob_Nam;
107900170807         clear job;
108000170807         clear user;
108100170807         clear jobn;
108200170807
108300170807         // Dichiarazione cursore
108400170807         exec sql declare C0 cursor for
108500170807         SELECT JOB_NAME FROM QSYS2.OUTPUT_QUEUE_ENTRIES WHERE
108600170807         OUTPUT_QUEUE_NAME = :pIn_OutQ and
108700170807         USER_NAME = :JobUserCurr and
108800170807         SPOOLED_FILE_NAME = :PFile_Nam and
108900170807         FILE_NUMBER = :PFile_Num and
109000170807         FORM_TYPE = :wFormType;
109100170807
109200170807         // Apro il cursore
109300170807         exec sql open C0;
109400170807
109500170807         // Leggo i dati
109600170807         if sqlcode = *zeros;
109700170807            exec sql
109800170807                 fetch next from C0 into :wJob_Nam;
109900170807         endif;
110000170807
110100170807         // Chiusura cursore
110200170807         exec sql close C0;
110300170807
110400170807         // Se ID Job spool non reperito => errore "fatale"
110500170807         if wJob_Nam = *blanks;
110600170807            pOut_Esito = 2;
110700170807            exsr sr_RoutEnd;
110800170807         else;
110900170807
111000170807            wPos = *zeros;
111100170807
111200170807            // Reperimento Job_Number
111300170807            wStr = wPos+1;
111400170807            wPos = %scan('/':wJob_Nam:wStr);
111500170807            if wPos > *zeros;
111600170807               jobn = %subst(wJob_Nam:wStr:wPos-wStr);
111700170807               wJob_Num = %dec(jobn:6:0);
111800170807               jobn = %editc(wJob_Num:'X');
111900170807            else;
112000170807               pOut_Esito = 2;
112100170807               exsr sr_RoutEnd;
112200170807            endif;
112300170807
112400170807            // Reperimento User_Name
112500170807            wStr = wPos+1;
112600170807            wPos = %scan('/':wJob_Nam:wStr);
112700170807            if wPos > *zeros;
112800170807               user = %subst(wJob_Nam:wStr:wPos-wStr);
112900170807            else;
113000170807               pOut_Esito = 2;
113100170807               exsr sr_RoutEnd;
113200170807            endif;
113300170807
113400170807            // Reperimento Job_Name
113500170807            wStr = wPos+1;
113600170807            job = %trim(%subst(wJob_Nam:wStr));
113700170807            if job = *blanks;
113800170807               pOut_Esito = 2;
113900170807               exsr sr_RoutEnd;
114000170807            endif;
114100170807
114200170807         endif;
114300170824
114400170824       ENDSR;
114500170824
114600170824       //--------------------------------------------------------------
114700170824       // Routine iniziale
114800170824       //--------------------------------------------------------------
114900170824       BEGSR  *inzsr;
115000170824
115100170824     c     *Entry        plist
115200170824     c                   parm                    UBBRTETIDS
115300170824
115400170824         // recupero una volta per tutte alcuni dati in tabella
115500170824         exsr RtvTAB;
115600170807
115700170807       ENDSR;
115800170824
115900170824     C*
116000170824     C     rtvTAB        begsr
116100170824     C*
116200170824     C* Richiami per reperimento vari parametri di procedura
116300170824     C                   eval      IS713TBECOD    = 'VPR'
116400170825     C                   eval      IS713TBEKE1    = '*VASWS'
116500170824     C                   eval      IS713TBEKE2    = '*DFTOUTQ'
116600170824     C                   eval      IS713TBELIN    = *blanks
116700170824     C                   eval      IS713TBESIF    = *blanks
116800170824     C*
116900170824     C                   call      'TIS713R'
117000170824     C                   parm                    IS713TBECOD
117100170824     C                   parm                    IS713TBEKE1
117200170824     C                   parm                    IS713TBEKE2
117300170824     C                   parm                    IS713TBELIN
117400170824     C                   parm                    IS713TBESIF
117500170824     C                   parm                    OS713TBEUNI
117600170824     C                   parm                    OS713ESITO
117700170824     C*
117800170825     C                   if        OS713ESITO = '1'
117900170824     C                   eval      DftOUTQ = OS713TBEUNI
118000170824     C                   else
118100170825             if pIn_OutQ <> *blank;
118200170825               DftOUTQ = %trim(pIn_OutQ);
118300170825             else;
118400170825     C                   eval      DftOUTQ = 'QPRINTS'
118500170825             endif;
118600170824     C                   endif
118700170824     C*
118800170824     C                   endsr

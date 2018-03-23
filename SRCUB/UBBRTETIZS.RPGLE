000100060511     h decedit('0,') datedit(*dmy.)
000200170208     **
000300170208     ** ISTRUZIONI PER LA COMPILAZIONE
000400170208     **
000500170208     ** 1. Creare il modulo UBBRTETIZR (Opz 15 PDM)
000600170208     ** 2. Creare/Aggiornare il programma di servizio UBBRTETIZR (CRTSRVPGM / UPDSRVPGM)
000700170208     **    con ACTGRP(*caller)
000800170208     **    aggiungere nella BNDDIR(UBBNDDIR)
000900170421     ** 3. Visto chi userà questo oggetto, non serve l'oggetto programma
001000170208     ** 4. Cancellare il modulo.
001100170208     **
001200160930
001300160930       //--------------------------------------------------------------
001400160930       // Dichiarazione file.
001500160930       //--------------------------------------------------------------
001600160930
001700160930     fUBBRTETIZPO    E             printer usropn
001800160930
001900160930       //--------------------------------------------------------------
002000160930       // Definizione costanti.
002100160930       //--------------------------------------------------------------
002200160930
002300160930
002400160930       //--------------------------------------------------------------
002500160930       // Definizione schiere.
002600160930       //--------------------------------------------------------------
002700160930
002800160930
002900160930       //--------------------------------------------------------------
003000160930       // Definizione aree dati.
003100160930       //--------------------------------------------------------------
003200160930
003300160930
003400160930       //--------------------------------------------------------------
003500160930       // Definizione strutture dati.
003600160930       //--------------------------------------------------------------
003700160930
003800161107       // parametro d'ingresso
003900161107     D UBBRTETIDS    e ds
004000161107
004100160930       // - Status
004200160930     d Psds           sds
004300160930     d   SDSpgm          *proc
004400160930     d   JobName             244    253                                         Job name
004500160930     d   JobUser             254    263                                         User name
004600160930     d   JobNumber           264    269s 0                                      Job number
004700160930
004800160930     d jobinf          ds
004900160930     d    job                        10
005000160930     d    user                       10
005100160930     d    jobn                        6
005200161011
005300161011       // reperimento check digits
005400161011     D TRUL28DS      e ds                  inz
005500161005
005600161005      //Schiere per calcolo del check-digit
005700161005     d ds_BarCode      ds            18             inz
005800161005     d  $CB                    1     18  0 dim(18)  inz
005900161005     d ds2a            ds             2             inz
006000161005     d  §2N                    1      2  0 dim(2)   inz
006100160930
006200160930       // strutture per QUSRSPLA
006300160930       /copy qsysinc/qrpglesrc,QUSRSPLA
006400160930
006500160930       // parametri gestione errori API.
006600160930       /copy qsysinc/qrpglesrc,QUSEC
006700160930
006800160930       //--------------------------------------------------------------
006900160930       // Definizione variabili.
007000160930       //--------------------------------------------------------------
007100160930
007200160930       // parametri binary(4) di input per QUSRSPLA
007300160930     D  RCVLEN         s             10I 0
007400160930     D  FSPLN          s             10I 0
007500161005
007600161005       // Campi di comodo per calcolo Check-Digit
007700161005     d wCHKDGT         s              1  0 inz
007800161005     d §TT             s              3  0 inz
007900161005     d §TT2            s              3  0 inz
008000161005     d §CTR            s              3  0 inz
008100161005     d §RST            s              3  0 inz
008200161005     d XY              s              3  0 inz
008300161005     d XZ              s              3  0 inz
008400161006     d wBarcode        s             18a
008500161006     d wSegnacollo     s              7s 0
008600161006     d wCollo          s              7s 0
008700161006     D wX              s              2s 0
008800161006     D w2S             s              2s 0
008900161006     D w3S             s              3s 0
009000161006     D wCheckValue     s              3s 0
009100170310     D wOutq           s                   like(pIn_OutQ)
009200170310     D wUsrDta         s                   like(pIn_UsrDta)
009300170406     D wFnumDS         ds
009400170406     D  wFnum                         6a   dim(100)
009500170421     D wUDtaDS         ds
009600170421     D  wUDta                        10a   dim(100)
009700160930
009800160930       //--------------------------------------------------------------
009900160930       // Definizione prototipi procedure.
010000160930       //--------------------------------------------------------------
010100160930
010200160930       /copy gaitrasrc/srcProtoPR,QUSRSPLA
010300161011     d TRUL28R1        pr                  extpgm('TRUL28R1')
010400161011     D  pTRUL28DS                          like(TRUL28DS)
010500170310
010600170310       // Parametri API QCAPCMD (Process Commands)?
010700170310     d Qcmd            s           2048    inz  varying
010800170310      /copy qSysInc/qRpgleSrc,QCAPCMD
010900170310       // API QCAPCMD (Process Commands)?
011000170310      /copy gaitrasrc/srcProtoPR,QCAPCMD
011100160930
011200160930       //--------------------------------------------------------------
011300160930       // Definizione key-list.
011400160930       //--------------------------------------------------------------
011500160930
011600160930
011700160930       //--------------------------------------------------------------
011800160930       // Definizione parametri procedura.
011900160930       //--------------------------------------------------------------
012000160930
012100161005     c     *Entry        plist
012200161107     c                   parm                    UBBRTETIDS
012300160930
012400160930       //--------------------------------------------------------------
012500160930
012600160930       //--------------------------------------------------------------
012700160930       // M A I N - L I N E
012800160930       //--------------------------------------------------------------
012900160930
013000160930       // Operazioni iniziali?
013100160930       exsr sr_RoutInz;
013200161003
013300161006       // se non chiusura
013400161006       if pIn_TLa <>'C';
013500161006         // la prima volta il segnacollo è il primo richiesto
013600161006         wSegnacollo = pIn_NCD;
013700161006         for wCollo = pIn_NCD to pIn_NCA;
013800170410           // apro il file di stampa se non già aperto
013900170410           // (mi serve farlo all'interno del ciclo perché il nr.segnacollo può essere lo USRDTA)
014000170410           exsr sr_ovrprtf;
014100161006           // stampa le N etiichette richieste
014200161006           exsr sr_StampaEtichetta;
014300161006           // il prossimo segnacollo sarà uno più del precedente
014400161006           wSegnacollo += 1;
014500161006         endfor;
014600161006       endif;
014700160930
014800160930       // Operazioni finali?
014900160930       exsr sr_RoutEnd;
015000160930
015100160930       //--------------------------------------------------------------
015200160930       // Operazioni iniziali.
015300160930       //--------------------------------------------------------------
015400160930       BEGSR  sr_RoutInz;
015500170406
015600170406         // inizializzazioni
015700170406         clear wFnum;
015800170421         clear wUDta;
015900170411         if pIn_NCA = 0;
016000170411           pIn_NCA = pIn_NCD;
016100170411         endif;
016200160930
016300160930         // se non chiusura
016400170208         if pIn_TLa <>'C'
016500170208         //  e non segnalazione errore
016600170406          and pIn_SegErr = *blank;
016700160930
016800160930           // dati obbligatori
016900160930           if pIn_FLS = 0      OR
017000160930           // pIn_NRS = 0      OR
017100160930              pIn_NCD = 0      OR
017200160930              pIn_LNA = 0      OR
017300160930              pIn_TMA = 0      OR
017400160930              pIn_ZNC = 0      OR
017500160930              pIn_NC2 = 0;
017600160930             pOut_Esito = 2;
017700160930             exsr sr_RoutEnd;
017800160930           endif;
017900160930
018000160930           // dati errati
018100160930           // tipo etichetta può valere solo ' '/ITA/DPD/FED/EEX
018200160930           if pIn_TiE <> *blank AND
018300160930              pIn_TiE <> 'ITA'  AND
018400160930              pIn_TiE <> 'DPD'  AND
018500160930              pIn_TiE <> 'FED'  AND
018600160930              pIn_TiE <> 'EEX';
018700160930             pOut_Esito = 2;
018800160930             exsr sr_RoutEnd;
018900160930           endif;
019000160930           // tipo servizio può valere solo ' '/C/E/H/D
019100160930           if pIn_TSP <> *blank AND
019200160930              pIn_TSP <> 'C'    AND
019300160930              pIn_TSP <> 'E'    AND
019400160930              pIn_TSP <> 'H'    AND
019500160930              pIn_TSP <> 'D';
019600160930             pOut_Esito = 2;
019700160930             exsr sr_RoutEnd;
019800160930           endif;
019900160930           // Tipo lancio deve essere blank o 'C'
020000160930           if pIn_TLa <> *blank and pIn_TLa <>'C';
020100160930             // esco dando errore
020200160930             pOut_Esito = 2;
020300160930             exsr sr_RoutEnd;
020400160930           endif;
020500160930
020600160930           // congruenza segnacollo da/a - nr.colli
020700170411           //
020800170411           // i colli totali possono essere solo >= di A - DA +1
020900170411           // (i colli gestiti sono T ma ne stampo solo X<T)
021000170411           if pIn_NCA - pIn_NCD + 1 >
021100170411              pIn_NC2;
021200170411             pOut_Esito = 2;
021300170411             exsr sr_RoutEnd;
021400170411           endif;
021500170411           // se il segnacollo DA < A
021600170411           // allora c'è incongruenza
021700170411           if pIn_NCA < pIn_NCD;
021800170411             pOut_Esito = 2;
021900170411             exsr sr_RoutEnd;
022000170411           endif;
022100170411
022200170406           // se è stato chiesto uno spool per collo
022300170410           if pIn_1SplXC = 'S'
022400170406           // ma sono stati chiesti più di 100 colli (che è la dim della schiera)
022500170406            and pIn_NC2 > 100;
022600170406             // allora c'è incongruenza
022700170406             pOut_Esito = 2;
022800170406             exsr sr_RoutEnd;
022900170406           endif;
023000160930         endif;
023100160930
023200160930         // label home position
023300170419         // offset Y è il y axis position e deve valere tra 0 e 32000 dots = 210,5263 inch.
023400170419         // offset X è il x axis position e deve valere tra 0 e 32000 dots = 210,5263 inch.
023500170419         // ma siccome l'etichetta è
023600170419         // Y = 6,5 cm = 2,5591 inch = 388,9832 dots  e X = 9,5 cm = 3,7402 inch = 568,5104 dots
023700170419         // questi sono i limiti della label home
023800170419         // (anzi li facciamo ancora più stringenti altrimenti non si capisce
023900170419         // perché l'etichetta non si vede)
024000170419         // se gli offset superano i limiti, vengono dati dei default. Lo stesso se sono 0.
024100170419         // (ricordare che gli offset cono in dots --> 152 dots = 1 inch)
024200170419         // I default sono risultati questi dopo un certo numero di prove.
024300170419         if pIn_OfsY > 373,5 or
024400170419            pIn_OfsY = 0;
024500170419           pIn_OfsY = 20;
024600170419         endif;
024700170419         if pIn_OfsX > 553,5 or
024800170419            pIn_OfsX = 0;
024900170419           pIn_OfsX = 130;
025000170419         endif;
025100160930
025200160930         // dati valorizzabili a pgm
025300160930         if pIn_AAS = 0;
025400160930           pIn_AAS = %dec(%subst(%char(%date()) : 1 : 4) : 4 : 0);
025500160930         endif;
025600160930         if pIn_MGS = 0;
025700161003           pIn_MGS = %dec(%subst(%char(%date()) : 6 : 2) : 2 : 0) * 100 +
025800160930                     %dec(%subst(%char(%date()) : 9 : 2) : 2 : 0);
025900160930         endif;
026000160930
026100160930       ENDSR;
026200160930
026300160930
026400160930       //--------------------------------------------------------------
026500160930       // Operazioni finali.
026600160930       //--------------------------------------------------------------
026700160930       BEGSR  sr_RoutEnd;
026800161003
026900160930         // se l'esito è corretto
027000170406         if pOut_Esito = 0
027100170406         // e non è stato chiesto uno spool per collo (altrimenti quello che segue è già stato
027200170406         // fatto)
027300170410          and pIn_1SplXC <> 'S';
027400160930           // reperisco i dati dello spool
027500160930           // 1. chiudo il file di stampa
027600160930           if %open(UBBRTETIZP);
027700160930             close UBBRTETIZP;
027800160930           endif;
027900160930           // 2. reperisco l'ultimo spool creato da questo job e col nome del PRTF
028000160930           job  = JobName;
028100160930           user = JobUser;
028200170801           jobn = %editc(JobNumber:'X');
028300160930           FSPLN = -1;
028400160930           RCVLEN = %len(QUSA010001);
028500160930           RetrieveSpooledFileAttributes
028600160930            ( QUSA010001 :
028700160930              RCVLEN :
028800160930              'SPLA0100' :
028900160930              jobinf :
029000160930              *blank : *blank :
029100161007              'UBBRTETIZP' :
029200160930              FSplN     );
029300160930           // 3. valorizzo i dati di output
029400160930           pOut_FNam = QUSSN01;
029500160930           pOut_JNam = QUSJN10;
029600160930           pOut_UNam = QUSUN12;
029700160930           pOut_JNum = QUSJNBR09;
029800160930           pOut_Fnum = %char(QUSSNBR);
029900170421           pOut_UDta = QUSUD01;
030000160930         endif;
030100170406
030200170406         // se l'esito è corretto
030300170406         if pOut_Esito = 0
030400170406         // ma è stato chiesto uno spool per collo
030500170410          and pIn_1SplXC = 'S';
030600170406           // aggiorno il numero di file con la schiera
030700170406           pOut_Fnum = wFnumDS;
030800170421           // aggiorno gli user data con la schiera
030900170421           pOut_UDTA = wUDtaDS;
031000170406         endif;
031100160930
031200160930         if  qUsei = *blank;
031300160930         //s_ObjTxt = qUstd12;
031400160930         else;
031500160930         //s_ObjTxt = *all'? ';
031600160930         endif;
031700161003
031800161005         // Pulisco i campi di stampa
031900161005         clear P01_Home        ;
032000161005         clear TMA             ;
032100161005         clear LNA             ;
032200161005         clear LNP             ;
032300161005         clear Barcode         ;
032400161005         clear Riferim         ;
032500161005         clear DtSped          ;
032600161005         clear FlagRst         ;
032700161005         clear FlagVal         ;
032800161005         clear Mittente        ;
032900161005         clear Destinat01      ;
033000161005         clear Destinat02      ;
033100161005         clear Volume          ;
033200161005         clear NDiTot          ;
033300161005         clear Peso            ;
033400161005         clear TSP             ;
033500161005
033600161005         // Chiusura pgm
033700160930         if pIn_TLa = 'C';
033800160930           *inlr = *on;
033900160930         else;
034000160930           *inrt = *on;
034100160930         endif;
034200160930         return;
034300160930
034400160930       ENDSR;
034500160930
034600160930       //--------------------------------------------------------------
034700161003       // Stampo
034800160930       //--------------------------------------------------------------
034900161006       BEGSR  sr_StampaEtichetta;
035000161003
035100161003         // prima tutta la parte che non ha riempimenti condizionati
035200161003
035300161003         // inizializzazione
035400161003         exsr sr_inizializza;
035500170208
035600170208         write BRTETIZ00;
035700161005
035800170208         if pIn_SegErr = *blank;
035900170208
036000161005         // terminal di arrivo
036100161005         exsr sr_terminalArrivo;
036200161005
036300161005         // filiale di arrivo
036400161005         exsr sr_filialeArrivo;
036500161007
036600161007         // zona di consegnao
036700161007         exsr sr_zonaConsegna;
036800161005
036900161005         // linea di partenza - filiale segnacollo
037000161005         exsr sr_lineaPartenza;
037100161005
037200161005         // barcode
037300161005         exsr sr_barcode;
037400161006
037500161006         // segnacollo
037600161006         exsr sr_segnacollo;
037700161005
037800161005         // riferimento
037900161005         exsr sr_riferimento;
038000161005
038100161005         // data spedizione
038200161005         exsr sr_dataSped;
038300161005
038400161005         // flag ristampa e merce a valore
038500161005         exsr sr_ristampa;
038600161003
038700161003         // mittente
038800161003         exsr sr_mittente;
038900161003
039000161003         // destinatario
039100161003         exsr sr_destinatario;
039200161003
039300161003         // volume
039400161003         exsr sr_volume;
039500161003
039600161003         // Nesimo segnacollo di tot
039700161003         exsr sr_nDiTot;
039800161003
039900161003         // peso
040000161003         exsr sr_peso;
040100161003
040200161003         write BRTETIZ01;
040300161003
040400161003         // poi stampo le parti condizionate
040500161007
040600161007         // se il tipo etichetta è EEX devo stampare, prima della zona,
040700161007         // i dati di consegna esteri
040800161007         if pIn_TiE = 'EEX';
040900161007           ISA = %trim(pIn_ISA);
041000161007           CAD = %trim(pIn_CAD);
041100161007           write BRTETIZ04;
041200161007      *****  e  Nazione di Arrivo (*in55 = EuroEXpress)
041300161007     o*         e    55      LV22d3      1
041400161007     o*                                             '^FO330,040'
041500161007     o*                                             '^GB240,085,3'
041600161007     o*                      C_end
041700161007     o*         e    55      LV22d3      1
041800161007     o*                                             '^FO340,050'
041900161007     o*                                             '^A0N,090,48'
042000161007     o*                                             '^FD'
042100161007     o*                      §DEisa
042200161007     o*                      C_end
042300161007     o*         e    55      LV22d3      1
042400161007     o*                                             '^FO440,050'
042500161007     o*                                             '^A0N,090,48'
042600161007     o*                                             '^FD'
042700161007     o*                      §LDcad
042800161007     o*                      C_end
042900161007         endif;
043000161005
043100161005         // tipo servizio + box
043200161003         // pulizia del flag "Tipo Servizio Bolle" SE:
043300161003         if pIn_TSP =  'D'
043400161003         and  pIn_LNA  <  300;
043500161003           clear pIn_TSP;
043600161003         endif;
043700161003         tsp = pIn_TSP;
043800161003         select;
043900161003          when pIn_Tsp = ' ';
044000161003          when pIn_Tsp = 'E' or
044100161003               pIn_Tsp = 'H';
044200161003           write BRTETIZ02;
044300161003          other;
044400161003           write BRTETIZ03;
044500161003         endsl;
044600161003
044700170208         // se c'è da stampare una segnalazione d'errore
044800170208         else;
044900170208           MsgErrTit = pIn_MsgErT;
045000170209           MsgErr1   = %subst(pIn_MsgErr:1:20)  ;
045100170209           MsgErr2   = %subst(pIn_MsgErr:21:20) ;
045200170209           MsgErr3   = %subst(pIn_MsgErr:41:20) ;
045300170209           MsgErr4   = %subst(pIn_MsgErr:61:40) ;
045400170209           MsgErr5   = %subst(pIn_MsgErr:101:40) ;
045500170209           MsgErr6   = %subst(pIn_MsgErr:141:40) ;
045600170209           MsgErr7   = %subst(pIn_MsgErr:181:40) ;
045700170208           write BRTETIZE1;
045800170208         endif;
045900170208
046000161003         // infine chiudo la stampa
046100161003         write BRTETIZ99;
046200170406
046300170406         // se è richiesta la stampa per singolo collo
046400170410         if pIn_1SplXC= 'S';
046500170406           // reperisco i dati dello spool
046600170406           // 1. chiudo il file di stampa
046700170406           if %open(UBBRTETIZP);
046800170406             close UBBRTETIZP;
046900170406           endif;
047000170406           // 2. reperisco l'ultimo spool creato da questo job e col nome del PRTF
047100170406           job  = JobName;
047200170406           user = JobUser;
047300170801           jobn = %editc(JobNumber:'X');
047400170406           FSPLN = -1;
047500170406           RCVLEN = %len(QUSA010001);
047600170406           RetrieveSpooledFileAttributes
047700170406            ( QUSA010001 :
047800170406              RCVLEN :
047900170406              'SPLA0100' :
048000170406              jobinf :
048100170406              *blank : *blank :
048200170406              'UBBRTETIZP' :
048300170406              FSplN     );
048400170406           // 3. valorizzo i dati di output
048500170406           pOut_FNam = QUSSN01;
048600170406           pOut_JNam = QUSJN10;
048700170406           pOut_UNam = QUSUN12;
048800170406           pOut_JNum = QUSJNBR09;
048900170406           wFnum(wCollo - pIn_NCD + 1) = %char(QUSSNBR);
049000170421           wUDta(wCollo - pIn_NCD + 1) = QUSUD01;
049100170406         endif;
049200160930
049300160930       ENDSR;
049400161003
049500161003       //--------------------------------------------------------------
049600161003       // Inizializza la stampa ZEBRA
049700161003       //--------------------------------------------------------------
049800161003       BEGSR  sr_inizializza;
049900161003
050000161003         //evalr p01_Home = %char(%int(pIn_OfsX):'1');
050100161003         p01_Home = %char(%int(pIn_OfsX)) + ',' +
050200161005                    %char(%int(pIn_OfsY));
050300161003
050400161003       ENDSR;
050500161005
050600161005       //--------------------------------------------------------------
050700161005       // terminal di arrivo
050800161005       //--------------------------------------------------------------
050900161005       BEGSR  sr_terminalArrivo;
051000161005
051100161005         tma = %editc(pIn_TMA:'X');
051200161006         // trasformo per tutte le variabili gli 0 in O
051300161006         tma = %xlate('0':'O':tma);
051400161005
051500161005       ENDSR;
051600161005
051700161005       //--------------------------------------------------------------
051800161005       // filiale di arrivo
051900161005       //--------------------------------------------------------------
052000161005       BEGSR  sr_filialeArrivo;
052100161005
052200161005         lna = %editc(pIn_LNA:'X');
052300161006         // trasformo per tutte le variabili gli 0 in O
052400161006         lna = %xlate('0':'O':lna);
052500161005
052600161005       ENDSR;
052700161003
052800161003       //--------------------------------------------------------------
052900161007       // zona di consegna
053000161003       //--------------------------------------------------------------
053100161007       BEGSR  sr_zonaConsegna;
053200161003
053300161007         znc = %editc(pIn_ZNC:'X');
053400161007         // trasformo per tutte le variabili gli 0 in O
053500161007         znc = %xlate('0':'O':znc);
053600161007
053700161007       ENDSR;
053800161007
053900161007       //--------------------------------------------------------------
054000161007       // linea di partenza - filiale segnacollo
054100161007       //--------------------------------------------------------------
054200161007       BEGSR  sr_lineaPartenza;
054300161007
054400161003         lnp = %editc(pIn_FLS:'X');
054500161003
054600161003       ENDSR;
054700161005
054800161005       //--------------------------------------------------------------
054900161005       // barcode
055000161005       //--------------------------------------------------------------
055100161005       BEGSR  sr_barcode;
055200161006
055300161006         // calcolo barcode 18 char (l'ultimo è il check digit)
055400161006         // compongo barcode e calcolo chek digit
055500161011         wBarcode = %editc(pIn_FLS:'X') +
055600161011                   %editc(pIn_LNA:'X') +
055700161011                   %editc(pIn_NRS:'X') +
055800161011                   %editc(wSegnacollo:'X') +
055900161011                   %editc(pIn_ZNC:'X');
056000161011         // siccome ho testato all'inizio che i dati non siano vuoti, anche il barcode è valorizzato
056100161011         // calcolo il check digit
056200161011         clear TRUL28DS;
056300161011         I28Mod  = 'BAR';
056400161011         I28COD = %trim(wBarcode);
056500161011         TRUL28R1(TRUL28DS);
056600161011         // Se tutto ok aggiungo il check digit
056700161011         if O28err = *blanks;
056800161011           wBarcode = O28Cod;
056900161011         endif;
057000161006         barcode = wBarCode;
057100161005
057200161005       ENDSR;
057300161006
057400161006       //--------------------------------------------------------------
057500161006       // serie - segnacollo
057600161006       //--------------------------------------------------------------
057700161006       BEGSR  sr_segnacollo;
057800161006
057900161006         nrs = *blank;
058000161006         if pIn_nrs > 0;
058100161006           nrs = %editc(pIn_NRS:'X') + '-';
058200161006         endif;
058300161006         ncx1 = %subst(%editc(wSegnacollo:'X'):1:4);
058400161006         ncx2 = %subst(%editc(wSegnacollo:'X'):5:3);
058500161006         // trasformo per tutte le variabili gli 0 in O
058600161006         nrs = %xlate('0':'O':nrs);
058700161006         ncx1 = %xlate('0':'O':ncx1);
058800161006         ncx2 = %xlate('0':'O':ncx2);
058900161006
059000161006       ENDSR;
059100161003
059200161003       //--------------------------------------------------------------
059300161005       // riferimento
059400161003       //--------------------------------------------------------------
059500161005       BEGSR  sr_riferimento;
059600161003
059700161005         Riferim = 'Rif.: ' + pIn_Cdp;
059800161005
059900161005       ENDSR;
060000161005
060100161005       //--------------------------------------------------------------
060200161005       // data spedizione
060300161005       //--------------------------------------------------------------
060400161005       BEGSR  sr_dataSped;
060500161005
060600161005         DtSped = %subst(%editc(pIn_MGS:'X'):3:2) + '/' +
060700161005                  %subst(%editc(pIn_MGS:'X'):1:2) + '/' +
060800161005                  %char(pIn_AAS);
060900161005
061000161005       ENDSR;
061100161005
061200161005       //--------------------------------------------------------------
061300161005       // flag ristampa e merce a valore
061400161005       //--------------------------------------------------------------
061500161005       BEGSR  sr_ristampa;
061600161005
061700161005sel 1    select;
061800161005
061900161005w   1     when pIn_rst  =  *blanks;
062000161005
062100161005w   1     when pIn_rst  =  'V';
062200161005           flagVal =  'V';
062300161005
062400161005w   1     when pIn_rst  =  'X';
062500161005           flagRst  =  'R';
062600161005           flagVal  =  'V';
062700161005
062800161005w   1     when pIn_rst  =  'W';
062900161005           flagRst  =  'M';
063000161005           flagVal  =  'V';
063100161005
063200161005x   1     other;
063300161005           flagRst  =  pIn_rst;
063400161005
063500161005e   1    endsl;
063600161005
063700161005       ENDSR;
063800161005
063900161005       //--------------------------------------------------------------
064000161005       // dati mittente
064100161005       //--------------------------------------------------------------
064200161005       BEGSR  sr_mittente;
064300161005
064400161003         mittente = %subst(pIn_RSM:1:26) + ' ' +
064500161003                    pIn_PRM;
064600161003
064700161003       ENDSR;
064800160930
064900160930       //--------------------------------------------------------------
065000160930       // dati destinatario
065100160930       //--------------------------------------------------------------
065200160930       BEGSR  sr_destinatario;
065300160930
065400160930         destinat01 = %subst(pIn_RSD:1:26) + ' ' +
065500160930                    pIn_PRD;
065600160930         destinat02 = %subst(pIn_IND:1:22) + ' ' +
065700160930                    pIn_LOD;
065800160930
065900160930       ENDSR;
066000160930
066100160930       //--------------------------------------------------------------
066200160930       // volume
066300160930       //--------------------------------------------------------------
066400160930       BEGSR  sr_volume;
066500160930
066600160930         volume = 'MC. ' + %editc(pIn_Vol:'1') + ' K';
066700160930
066800160930       ENDSR;
066900160930
067000160930       //--------------------------------------------------------------
067100160930       // Nesimo segnacollo di tot
067200160930       //--------------------------------------------------------------
067300160930       BEGSR  sr_nDiTot;
067400161006
067500170411       // se è stato passato un nr.segnacollo preciso, sarà quello il primo valore
067600161006       if pIn_NC1 <> 0;
067700161006         %subst(nDiTot : 1 : 5) =
067800161006           %editc(pIn_NC1:'Z');
067900170411       // se NON è stato passato un nr.segnacollo preciso, il primo valore sarà il
068000170411       // progressivo collo
068100161006       else;
068200161006         %subst(nDiTot : 1 : 5) =
068300170411         // wCollo è una variabile 7s0 per cui non ci sta in 5 char, devo prenderne
068400161006         // solo un pezzo
068500161006           %subst(
068600170411            %editc(wCollo :'Z')
068700170411            : %len(%editc(wCollo :'Z')) - 5 + 1
068800161006            : 5);
068900161006       endif;
069000161006       %subst(nDiTot : 6 : 6) = '-' +
069100161006                                %editc(pIn_NC2:'Z');
069200160930
069300160930       ENDSR;
069400160930
069500160930       //--------------------------------------------------------------
069600160930       // peso
069700160930       //--------------------------------------------------------------
069800160930       BEGSR  sr_peso;
069900160930
070000160930         peso = %editc(pIn_PKg:'3');
070100160930         peso = 'KG. ' + %editc(pIn_PKg:'3');
070200160930
070300160930       ENDSR;
070400161011
070500170310       //--------------------------------------------------------------
070600170310       // Impostazioni PRTF personalizzate
070700170310       //--------------------------------------------------------------
070800170310       BEGSR  sr_OVRPRTF;
070900170310
071000170310         // se il prtf è chiuso oppure è già aperto ma i parametri di OVR personalizzati passati
071100170310         // sono diversi da quelli con cui è stata fatta la open
071200170310         if  Not %open(UBBRTETIZP) OR
071300170310             %open(UBBRTETIZP) and
071400170310              (wOutQ <> pIn_OutQ OR wUsrDta <> pIn_UsrDta);
071500170310           // se PRTF aperto ma i parametri di OVR personalizzati passati
071600170310           // sono diversi da quelli con cui è stata fatta la open
071700170310           if %open(UBBRTETIZP) and
071800170310            (wOutQ <> pIn_OutQ OR wUsrDta <> pIn_UsrDta);
071900170310             // chiudo il PRTF
072000170310             close UBBRTETIZP;
072100170310           endif;
072200170410           //if pIn_OutQ <> *blank OR pIn_UsrDta <> *blank;
072300170310             Qcmd = 'OVRPRTF file(UBBRTETIZP)';
072400170310             if pIn_OutQ <> *blank;
072500170410               Qcmd = %trim(Qcmd) +
072600170310                    ' outq(' + %trim(pIn_OutQ) + ')';
072700170410             else;
072800170410               Qcmd = %trim(Qcmd) +
072900170410                    ' outq(QPRINTS)';
073000170310             endif;
073100170310             if pIn_UsrDta <> *blank;
073200170410               Qcmd = %trim(Qcmd) +
073300170310                    ' usrdta(' + %trim(pIn_UsrDta) + ')';
073400170410             else;
073500170410               Qcmd = %trim(Qcmd) +
073600170410                    ' usrdta(P' + %editc(wCollo:'X') + ')';
073700170310             endif;
073800170310             exsr  sr_ExecCmd;
073900170310             if  Qusei <> *blank;
074000170310               pOut_Esito = 2;
074100170310               exsr sr_RoutEnd;
074200170406             else;
074300170406               // salvo i parametri di OVR personalizzati
074400170406               wOutQ = pIn_OutQ;
074500170406               wUsrDta = pIn_UsrDta;
074600170310             endif;
074700170410           //endif;
074800170310           open  UBBRTETIZP;
074900170310         endif;
075000170310
075100170310       ENDSR;
075200170310
075300170310       //--------------------------------------------------------------
075400170310       // Esecuzione del comando (già impostato).                      ?
075500170310       //--------------------------------------------------------------
075600170310       BEGSR  sr_ExecCmd;
075700170310
075800170310         clear Qcap0100;
075900170310         Qcabcsdh = *off;
076000170310         Qcapa    = *off;
076100170310         Qcacmdss = *off;
076200170310         Qcaerved = *allX'00';
076300170310
076400170310         clear Qusec;
076500170310         Qusbprv  = %size(Qusec);
076600170310
076700170310         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
076800170310                           %size(Qcap0100) : 'CPOP0100' : *omit :
076900170310                           0 : 0 : Qusec);
077000170310
077100170310       ENDSR;

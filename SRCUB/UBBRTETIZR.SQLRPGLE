000100060511     h decedit('0,') datedit(*dmy.)
000200170208     **
000300170208     ** ISTRUZIONI PER LA COMPILAZIONE
000400170208     **
000500170208     ** 1. Creare il modulo UBBRTETIZR (Opz 15 PDM)
000600170208     ** 2. Creare/Aggiornare il programma di servizio UBBRTETIZR (CRTSRVPGM / UPDSRVPGM)
000700170208     **    con ACTGRP(*caller)
000800170807     **    aggiungere BNDDIR(UBBNDDIR) e (UBBRTETI)
000900170421     ** 3. Visto chi userà questo oggetto, non serve l'oggetto programma
001000170208     ** 4. Cancellare il modulo.
001100170208     **
001200160930
001300160930       //--------------------------------------------------------------
001400160930       // Dichiarazione file.
001500160930       //--------------------------------------------------------------
001600160930
001700160930     fUBBRTETIZPO    E             printer usropn
001800170804     f                                     infds(curINFDS)
001900160930
002000160930       //--------------------------------------------------------------
002100160930       // Definizione costanti.
002200160930       //--------------------------------------------------------------
002300160930
002400160930
002500160930       //--------------------------------------------------------------
002600160930       // Definizione schiere.
002700160930       //--------------------------------------------------------------
002800160930
002900160930
003000160930       //--------------------------------------------------------------
003100160930       // Definizione aree dati.
003200160930       //--------------------------------------------------------------
003300160930
003400160930
003500160930       //--------------------------------------------------------------
003600160930       // Definizione strutture dati.
003700160930       //--------------------------------------------------------------
003800160930
003900161107       // parametro d'ingresso
004000161107     D UBBRTETIDS    e ds
004100161107
004200170807       // - Status DS
004300160930     d Psds           sds
004400160930     d   SDSpgm          *proc
004500160930     d   JobName             244    253                                         Job name
004600160930     d   JobUser             254    263                                         User name
004700160930     d   JobNumber           264    269s 0                                      Job number
004800170807     d   JobUserCurr         358    367                                         Current User name
004900170807
005000170807       // - Information DS
005100170807     d curINFDS        ds
005200170807     d   PFile_Nam            83     92                                         File Name
005300170807     d   PFile_Num           160    163i 0                                      File Number
005400160930
005500160930     d jobinf          ds
005600160930     d    job                        10
005700160930     d    user                       10
005800160930     d    jobn                        6
005900161011
006000161011       // reperimento check digits
006100161011     D TRUL28DS      e ds                  inz
006200161005
006300161005      //Schiere per calcolo del check-digit
006400161005     d ds_BarCode      ds            18             inz
006500161005     d  $CB                    1     18  0 dim(18)  inz
006600161005     d ds2a            ds             2             inz
006700161005     d  §2N                    1      2  0 dim(2)   inz
006800170824
006900170824     D DftOUTQ         s             10a   inz
007000160930
007100160930       // strutture per QUSRSPLA
007200160930       /copy qsysinc/qrpglesrc,QUSRSPLA
007300160930
007400160930       // parametri gestione errori API.
007500160930       /copy qsysinc/qrpglesrc,QUSEC
007600160930
007700170807
007800160930       //--------------------------------------------------------------
007900160930       // Definizione variabili.
008000160930       //--------------------------------------------------------------
008100160930
008200160930       // parametri binary(4) di input per QUSRSPLA
008300160930     D  RCVLEN         s             10I 0
008400160930     D  FSPLN          s             10I 0
008500161005
008600161005       // Campi di comodo per calcolo Check-Digit
008700161005     d wCHKDGT         s              1  0 inz
008800161005     d §TT             s              3  0 inz
008900161005     d §TT2            s              3  0 inz
009000161005     d §CTR            s              3  0 inz
009100161005     d §RST            s              3  0 inz
009200161005     d XY              s              3  0 inz
009300161005     d XZ              s              3  0 inz
009400161006     d wBarcode        s             18a
009500161006     d wSegnacollo     s              7s 0
009600161006     d wCollo          s              7s 0
009700161006     D wX              s              2s 0
009800161006     D w2S             s              2s 0
009900161006     D w3S             s              3s 0
010000161006     D wCheckValue     s              3s 0
010100170310     D wOutq           s                   like(pIn_OutQ)
010200170310     D wUsrDta         s                   like(pIn_UsrDta)
010300170406     D wFnumDS         ds
010400170406     D  wFnum                         6a   dim(100)
010500170421     D wUDtaDS         ds
010600170421     D  wUDta                        10a   dim(100)
010700170807     D wSpecialSpl     s               n   inz
010800170807     D wFormType       s             10a   inz
010900170807     D wJob_Nam        s             28a   inz
011000170807     D wJob_Num        s              6s 0 inz
011100170807     D wStr            s              2s 0 inz
011200170807     D wPos            s              2s 0 inz
011300170807     D oVCPEsito       s              2S 0 inz
011400170807     D oVCPNum         s             15S 0 inz
011500170807     D oVCPNumDal      s             15S 0 inz
011600170807     D oVCPIncr        s              3S 0 inz
011700170928     D Volta1Logo      s               n   inz(*on)
011800160930
011900160930       //--------------------------------------------------------------
012000160930       // Definizione prototipi procedure.
012100160930       //--------------------------------------------------------------
012200160930
012300160930       /copy gaitrasrc/srcProtoPR,QUSRSPLA
012400161011     d TRUL28R1        pr                  extpgm('TRUL28R1')
012500161011     D  pTRUL28DS                          like(TRUL28DS)
012600170310
012700170310       // Parametri API QCAPCMD (Process Commands)?
012800170310     d Qcmd            s           2048    inz  varying
012900170310      /copy qSysInc/qRpgleSrc,QCAPCMD
013000170310       // API QCAPCMD (Process Commands)?
013100170310      /copy gaitrasrc/srcProtoPR,QCAPCMD
013200170807     D/COPY GAITRASRC/SRCPROTOPR,UBVCP0R_01
013300170824     D/COPY GAITRASRC/SRCPROTOPI,TIS713R
013400160930
013500160930       //--------------------------------------------------------------
013600160930       // Definizione key-list.
013700160930       //--------------------------------------------------------------
013800160930
013900160930
014000160930       //--------------------------------------------------------------
014100160930       // Definizione parametri procedura.
014200160930       //--------------------------------------------------------------
014300160930
014400170807     c*
014500170807     c* Definisco le opzioni con cui verranno di seguito utilizzate le istruzioni SQL
014600170807     c
014700170807     c/EXEC SQL
014800170807     c+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
014900170807     c/END-EXEC
015000160930
015100160930       //--------------------------------------------------------------
015200160930
015300160930       //--------------------------------------------------------------
015400160930       // M A I N - L I N E
015500160930       //--------------------------------------------------------------
015600160930
015700160930       // Operazioni iniziali?
015800160930       exsr sr_RoutInz;
015900161003
016000161006       // se non chiusura
016100161006       if pIn_TLa <>'C';
016200161006         // la prima volta il segnacollo è il primo richiesto
016300161006         wSegnacollo = pIn_NCD;
016400161006         for wCollo = pIn_NCD to pIn_NCA;
016500170410           // apro il file di stampa se non già aperto
016600170410           // (mi serve farlo all'interno del ciclo perché il nr.segnacollo può essere lo USRDTA)
016700170410           exsr sr_ovrprtf;
016800161006           // stampa le N etiichette richieste
016900161006           exsr sr_StampaEtichetta;
017000161006           // il prossimo segnacollo sarà uno più del precedente
017100161006           wSegnacollo += 1;
017200161006         endfor;
017300161006       endif;
017400160930
017500160930       // Operazioni finali?
017600160930       exsr sr_RoutEnd;
017700160930
017800160930       //--------------------------------------------------------------
017900160930       // Operazioni iniziali.
018000160930       //--------------------------------------------------------------
018100160930       BEGSR  sr_RoutInz;
018200170406
018300170406         // inizializzazioni
018400170406         clear wFnum;
018500170421         clear wUDta;
018600170411         if pIn_NCA = 0;
018700170411           pIn_NCA = pIn_NCD;
018800170411         endif;
018900170929         eval Volta1Logo = *on;
019000160930
019100160930         // se non chiusura
019200170208         if pIn_TLa <>'C'
019300170208         //  e non segnalazione errore
019400170406          and pIn_SegErr = *blank;
019500160930
019600160930           // dati obbligatori
019700160930           if pIn_FLS = 0      OR
019800160930           // pIn_NRS = 0      OR
019900160930              pIn_NCD = 0      OR
020000160930              pIn_LNA = 0      OR
020100160930              pIn_TMA = 0      OR
020200160930              pIn_ZNC = 0      OR
020300160930              pIn_NC2 = 0;
020400160930             pOut_Esito = 2;
020500160930             exsr sr_RoutEnd;
020600160930           endif;
020700160930
020800160930           // dati errati
020900160930           // tipo etichetta può valere solo ' '/ITA/DPD/FED/EEX
021000160930           if pIn_TiE <> *blank AND
021100160930              pIn_TiE <> 'ITA'  AND
021200160930              pIn_TiE <> 'DPD'  AND
021300160930              pIn_TiE <> 'FED'  AND
021400160930              pIn_TiE <> 'EEX';
021500160930             pOut_Esito = 2;
021600160930             exsr sr_RoutEnd;
021700160930           endif;
021800160930           // tipo servizio può valere solo ' '/C/E/H/D
021900160930           if pIn_TSP <> *blank AND
022000160930              pIn_TSP <> 'C'    AND
022100160930              pIn_TSP <> 'E'    AND
022200160930              pIn_TSP <> 'H'    AND
022300160930              pIn_TSP <> 'D';
022400160930             pOut_Esito = 2;
022500160930             exsr sr_RoutEnd;
022600160930           endif;
022700160930           // Tipo lancio deve essere blank o 'C'
022800160930           if pIn_TLa <> *blank and pIn_TLa <>'C';
022900160930             // esco dando errore
023000160930             pOut_Esito = 2;
023100160930             exsr sr_RoutEnd;
023200160930           endif;
023300160930
023400160930           // congruenza segnacollo da/a - nr.colli
023500170411           //
023600170411           // i colli totali possono essere solo >= di A - DA +1
023700170411           // (i colli gestiti sono T ma ne stampo solo X<T)
023800170411           if pIn_NCA - pIn_NCD + 1 >
023900170411              pIn_NC2;
024000170411             pOut_Esito = 2;
024100170411             exsr sr_RoutEnd;
024200170411           endif;
024300170411           // se il segnacollo DA < A
024400170411           // allora c'è incongruenza
024500170411           if pIn_NCA < pIn_NCD;
024600170411             pOut_Esito = 2;
024700170411             exsr sr_RoutEnd;
024800170411           endif;
024900170411
025000170406           // se è stato chiesto uno spool per collo
025100170410           if pIn_1SplXC = 'S'
025200170406           // ma sono stati chiesti più di 100 colli (che è la dim della schiera)
025300170406            and pIn_NC2 > 100;
025400170406             // allora c'è incongruenza
025500170406             pOut_Esito = 2;
025600170406             exsr sr_RoutEnd;
025700170406           endif;
025800160930         endif;
025900160930
026000160930         // label home position
026100170419         // offset Y è il y axis position e deve valere tra 0 e 32000 dots = 210,5263 inch.
026200170419         // offset X è il x axis position e deve valere tra 0 e 32000 dots = 210,5263 inch.
026300170419         // ma siccome l'etichetta è
026400170419         // Y = 6,5 cm = 2,5591 inch = 388,9832 dots  e X = 9,5 cm = 3,7402 inch = 568,5104 dots
026500170419         // questi sono i limiti della label home
026600170419         // (anzi li facciamo ancora più stringenti altrimenti non si capisce
026700170419         // perché l'etichetta non si vede)
026800170419         // se gli offset superano i limiti, vengono dati dei default. Lo stesso se sono 0.
026900170419         // (ricordare che gli offset cono in dots --> 152 dots = 1 inch)
027000170419         // I default sono risultati questi dopo un certo numero di prove.
027100170419         if pIn_OfsY > 373,5 or
027200170419            pIn_OfsY = 0;
027300170419           pIn_OfsY = 20;
027400170419         endif;
027500170419         if pIn_OfsX > 553,5 or
027600170419            pIn_OfsX = 0;
027700170419           pIn_OfsX = 130;
027800170419         endif;
027900160930
028000160930         // dati valorizzabili a pgm
028100160930         if pIn_AAS = 0;
028200160930           pIn_AAS = %dec(%subst(%char(%date()) : 1 : 4) : 4 : 0);
028300160930         endif;
028400160930         if pIn_MGS = 0;
028500161003           pIn_MGS = %dec(%subst(%char(%date()) : 6 : 2) : 2 : 0) * 100 +
028600160930                     %dec(%subst(%char(%date()) : 9 : 2) : 2 : 0);
028700160930         endif;
028800160930
028900160930       ENDSR;
029000160930
029100160930
029200160930       //--------------------------------------------------------------
029300160930       // Operazioni finali.
029400160930       //--------------------------------------------------------------
029500160930       BEGSR  sr_RoutEnd;
029600161003
029700160930         // se l'esito è corretto
029800170406         if pOut_Esito = 0
029900170406         // e non è stato chiesto uno spool per collo (altrimenti quello che segue è già stato
030000170406         // fatto)
030100170410          and pIn_1SplXC <> 'S';
030200170928           // scarico logo se richiesto
030300170928           if pIn_Logo <> 'N';
030400170928             write  BRTETIZLGO;
030500170928           endif;
030600160930           // reperisco i dati dello spool
030700160930           // 1. chiudo il file di stampa
030800160930           if %open(UBBRTETIZP);
030900160930             close UBBRTETIZP;
031000160930           endif;
031100160930           // 2. reperisco l'ultimo spool creato da questo job e col nome del PRTF
031200170807           if wSpecialSpl;
031300170807              exsr sr_RtvSpoolJob;
031400170807           else;
031500170807              job  = JobName;
031600170807              user = JobUserCurr;
031700170807              jobn = %editc(JobNumber:'X');
031800170807           endif;
031900170807           FSPLN = -1;
032000160930           RCVLEN = %len(QUSA010001);
032100170807           RetrieveSpooledFileAttributes
032200160930            ( QUSA010001 :
032300160930              RCVLEN :
032400160930              'SPLA0100' :
032500160930              jobinf :
032600160930              *blank : *blank :
032700161007              'UBBRTETIZP' :
032800160930              FSplN     );
032900160930           // 3. valorizzo i dati di output
033000160930           pOut_FNam = QUSSN01;
033100160930           pOut_JNam = QUSJN10;
033200160930           pOut_UNam = QUSUN12;
033300160930           pOut_JNum = QUSJNBR09;
033400160930           pOut_Fnum = %char(QUSSNBR);
033500170421           pOut_UDta = QUSUD01;
033600160930         endif;
033700170406
033800170406         // se l'esito è corretto
033900170406         if pOut_Esito = 0
034000170406         // ma è stato chiesto uno spool per collo
034100170410          and pIn_1SplXC = 'S';
034200170406           // aggiorno il numero di file con la schiera
034300170406           pOut_Fnum = wFnumDS;
034400170421           // aggiorno gli user data con la schiera
034500170421           pOut_UDTA = wUDtaDS;
034600170406         endif;
034700160930
034800160930         if  qUsei = *blank;
034900160930         //s_ObjTxt = qUstd12;
035000160930         else;
035100160930         //s_ObjTxt = *all'? ';
035200160930         endif;
035300161003
035400161005         // Pulisco i campi di stampa
035500161005         clear P01_Home        ;
035600170928         clear P01_Logo        ;
035700161005         clear TMA             ;
035800161005         clear LNA             ;
035900161005         clear LNP             ;
036000161005         clear Barcode         ;
036100161005         clear Riferim         ;
036200161005         clear DtSped          ;
036300161005         clear FlagRst         ;
036400161005         clear FlagVal         ;
036500161005         clear Mittente        ;
036600161005         clear Destinat01      ;
036700161005         clear Destinat02      ;
036800161005         clear Volume          ;
036900161005         clear NDiTot          ;
037000161005         clear Peso            ;
037100161005         clear TSP             ;
037200161005
037300161005         // Chiusura pgm
037400160930         if pIn_TLa = 'C';
037500160930           *inlr = *on;
037600160930         else;
037700160930           *inrt = *on;
037800160930         endif;
037900160930         return;
038000160930
038100160930       ENDSR;
038200160930
038300160930       //--------------------------------------------------------------
038400161003       // Stampo
038500160930       //--------------------------------------------------------------
038600161006       BEGSR  sr_StampaEtichetta;
038700161003
038800161003         // prima tutta la parte che non ha riempimenti condizionati
038900161003
039000161003         // inizializzazione
039100161003         exsr sr_inizializza;
039200170208
039300170928         // disattivo stampa logo
039400170928         eval *in(11) = *off;
039500170928         // se richiesto il logo
039600170928         if pIn_Logo <> 'N';
039700170928           // attivo la sua stampa
039800170928           eval *in(11) = *on;
039900170928         endif;
040000170928
040100170208         write BRTETIZ00;
040200161005
040300170208         if pIn_SegErr = *blank;
040400170208
040500161005         // terminal di arrivo
040600161005         exsr sr_terminalArrivo;
040700161005
040800161005         // filiale di arrivo
040900161005         exsr sr_filialeArrivo;
041000161007
041100161007         // zona di consegnao
041200161007         exsr sr_zonaConsegna;
041300161005
041400161005         // linea di partenza - filiale segnacollo
041500161005         exsr sr_lineaPartenza;
041600161005
041700161005         // barcode
041800161005         exsr sr_barcode;
041900161006
042000161006         // segnacollo
042100161006         exsr sr_segnacollo;
042200161005
042300161005         // riferimento
042400161005         exsr sr_riferimento;
042500161005
042600161005         // data spedizione
042700161005         exsr sr_dataSped;
042800161005
042900161005         // flag ristampa e merce a valore
043000161005         exsr sr_ristampa;
043100161003
043200161003         // mittente
043300161003         exsr sr_mittente;
043400161003
043500161003         // destinatario
043600161003         exsr sr_destinatario;
043700161003
043800161003         // volume
043900161003         exsr sr_volume;
044000161003
044100161003         // Nesimo segnacollo di tot
044200161003         exsr sr_nDiTot;
044300161003
044400161003         // peso
044500161003         exsr sr_peso;
044600161003
044700161003         write BRTETIZ01;
044800161003
044900161003         // poi stampo le parti condizionate
045000161007
045100161007         // se il tipo etichetta è EEX devo stampare, prima della zona,
045200161007         // i dati di consegna esteri
045300161007         if pIn_TiE = 'EEX';
045400161007           ISA = %trim(pIn_ISA);
045500161007           CAD = %trim(pIn_CAD);
045600161007           write BRTETIZ04;
045700161007      *****  e  Nazione di Arrivo (*in55 = EuroEXpress)
045800161007     o*         e    55      LV22d3      1
045900161007     o*                                             '^FO330,040'
046000161007     o*                                             '^GB240,085,3'
046100161007     o*                      C_end
046200161007     o*         e    55      LV22d3      1
046300161007     o*                                             '^FO340,050'
046400161007     o*                                             '^A0N,090,48'
046500161007     o*                                             '^FD'
046600161007     o*                      §DEisa
046700161007     o*                      C_end
046800161007     o*         e    55      LV22d3      1
046900161007     o*                                             '^FO440,050'
047000161007     o*                                             '^A0N,090,48'
047100161007     o*                                             '^FD'
047200161007     o*                      §LDcad
047300161007     o*                      C_end
047400161007         endif;
047500161005
047600161005         // tipo servizio + box
047700161003         // pulizia del flag "Tipo Servizio Bolle" SE:
047800161003         if pIn_TSP =  'D'
047900161003         and  pIn_LNA  <  300;
048000161003           clear pIn_TSP;
048100161003         endif;
048200161003         tsp = pIn_TSP;
048300161003         select;
048400161003          when pIn_Tsp = ' ';
048500161003          when pIn_Tsp = 'E' or
048600161003               pIn_Tsp = 'H';
048700161003           write BRTETIZ02;
048800161003          other;
048900161003           write BRTETIZ03;
049000161003         endsl;
049100161003
049200170208         // se c'è da stampare una segnalazione d'errore
049300170208         else;
049400170208           MsgErrTit = pIn_MsgErT;
049500170209           MsgErr1   = %subst(pIn_MsgErr:1:20)  ;
049600170209           MsgErr2   = %subst(pIn_MsgErr:21:20) ;
049700170209           MsgErr3   = %subst(pIn_MsgErr:41:20) ;
049800170209           MsgErr4   = %subst(pIn_MsgErr:61:40) ;
049900170209           MsgErr5   = %subst(pIn_MsgErr:101:40) ;
050000170209           MsgErr6   = %subst(pIn_MsgErr:141:40) ;
050100170209           MsgErr7   = %subst(pIn_MsgErr:181:40) ;
050200170208           write BRTETIZE1;
050300170208         endif;
050400170208
050500161003         // infine chiudo la stampa
050600161003         write BRTETIZ99;
050700170406
050800170406         // se è richiesta la stampa per singolo collo
050900170410         if pIn_1SplXC= 'S';
051000170928           // scarico logo se richiesto
051100170928           if pIn_Logo <> 'N';
051200170928             write  BRTETIZLGO;
051300170928           endif;
051400170406           // reperisco i dati dello spool
051500170406           // 1. chiudo il file di stampa
051600170406           if %open(UBBRTETIZP);
051700170406             close UBBRTETIZP;
051800170406           endif;
051900170807           // 2. reperisco l'ultimo spool creato da questo job e col nome del PRTF
052000170807           if wSpecialSpl;
052100170807              exsr sr_RtvSpoolJob;
052200170807           else;
052300170807              job  = JobName;
052400170807              user = JobUserCurr;
052500170807              jobn = %editc(JobNumber:'X');
052600170807           endif;
052700170807           FSPLN = -1;
052800170406           RCVLEN = %len(QUSA010001);
052900170807           RetrieveSpooledFileAttributes
053000170406            ( QUSA010001 :
053100170406              RCVLEN :
053200170406              'SPLA0100' :
053300170406              jobinf :
053400170406              *blank : *blank :
053500170406              'UBBRTETIZP' :
053600170406              FSplN     );
053700170406           // 3. valorizzo i dati di output
053800170406           pOut_FNam = QUSSN01;
053900170406           pOut_JNam = QUSJN10;
054000170406           pOut_UNam = QUSUN12;
054100170406           pOut_JNum = QUSJNBR09;
054200170406           wFnum(wCollo - pIn_NCD + 1) = %char(QUSSNBR);
054300170421           wUDta(wCollo - pIn_NCD + 1) = QUSUD01;
054400170406         endif;
054500160930
054600160930       ENDSR;
054700161003
054800161003       //--------------------------------------------------------------
054900161003       // Inizializza la stampa ZEBRA
055000161003       //--------------------------------------------------------------
055100161003       BEGSR  sr_inizializza;
055200170928
055300170928         // logo (preparazione)
055400170928         if pIn_Logo <> 'N'
055500170929         and Volta1Logo = *on;
055600170928           write  BRTETIZLGI;
055700170929           // solo 1 volta
055800170929           // se NON è richiesta la stampa per singolo collo
055900170929           if pIn_1SplXC<>'S';
056000170929             Volta1Logo = *off;
056100170929           endif;
056200170929         endif;
056300161003
056400170928         // se richiesto il logo
056500170928         if pIn_Logo <> 'N';
056600170928           // gli offset sono relativi al logo
056700170928           p01_Logo = %char(%int(pIn_OfsX)) + ',' +
056800170928                      %char(%int(pIn_OfsY));
056900170929           // e la posizione dell'etichetta la sposto di 40 (test manuali)
057000170929           p01_Home = %char(%int(pIn_OfsX)+40) + ',' +
057100170928                      %char(%int(pIn_OfsY));
057200170928         // se NON richiesto il logo
057300170928         else;
057400170928           // gli offset sono relativi a tuttza l'etichetta senza logo
057500170928           p01_Home = %char(%int(pIn_OfsX)) + ',' +
057600170928                      %char(%int(pIn_OfsY));
057700170928         endif;
057800161003
057900170829       // decido se stampare riga di contrasto
058000170830       if pIn_Row =  'S';
058100170829         *in04 = *on;
058200170829       else;
058300170829         *in04 = *off;
058400170829       endif;
058500170829
058600161003       ENDSR;
058700161005
058800161005       //--------------------------------------------------------------
058900161005       // terminal di arrivo
059000161005       //--------------------------------------------------------------
059100161005       BEGSR  sr_terminalArrivo;
059200161005
059300161005         tma = %editc(pIn_TMA:'X');
059400161006         // trasformo per tutte le variabili gli 0 in O
059500161006         tma = %xlate('0':'O':tma);
059600161005
059700161005       ENDSR;
059800161005
059900161005       //--------------------------------------------------------------
060000161005       // filiale di arrivo
060100161005       //--------------------------------------------------------------
060200161005       BEGSR  sr_filialeArrivo;
060300161005
060400161005         lna = %editc(pIn_LNA:'X');
060500161006         // trasformo per tutte le variabili gli 0 in O
060600161006         lna = %xlate('0':'O':lna);
060700161005
060800161005       ENDSR;
060900161003
061000161003       //--------------------------------------------------------------
061100161007       // zona di consegna
061200161003       //--------------------------------------------------------------
061300161007       BEGSR  sr_zonaConsegna;
061400161003
061500161007         znc = %editc(pIn_ZNC:'X');
061600161007         // trasformo per tutte le variabili gli 0 in O
061700161007         znc = %xlate('0':'O':znc);
061800161007
061900161007       ENDSR;
062000161007
062100161007       //--------------------------------------------------------------
062200161007       // linea di partenza - filiale segnacollo
062300161007       //--------------------------------------------------------------
062400161007       BEGSR  sr_lineaPartenza;
062500161007
062600161003         lnp = %editc(pIn_FLS:'X');
062700161003
062800161003       ENDSR;
062900161005
063000161005       //--------------------------------------------------------------
063100161005       // barcode
063200161005       //--------------------------------------------------------------
063300161005       BEGSR  sr_barcode;
063400161006
063500161006         // calcolo barcode 18 char (l'ultimo è il check digit)
063600161006         // compongo barcode e calcolo chek digit
063700161011         wBarcode = %editc(pIn_FLS:'X') +
063800161011                   %editc(pIn_LNA:'X') +
063900161011                   %editc(pIn_NRS:'X') +
064000161011                   %editc(wSegnacollo:'X') +
064100161011                   %editc(pIn_ZNC:'X');
064200161011         // siccome ho testato all'inizio che i dati non siano vuoti, anche il barcode è valorizzato
064300161011         // calcolo il check digit
064400161011         clear TRUL28DS;
064500161011         I28Mod  = 'BAR';
064600161011         I28COD = %trim(wBarcode);
064700161011         TRUL28R1(TRUL28DS);
064800161011         // Se tutto ok aggiungo il check digit
064900161011         if O28err = *blanks;
065000161011           wBarcode = O28Cod;
065100161011         endif;
065200161006         barcode = wBarCode;
065300161005
065400161005       ENDSR;
065500161006
065600161006       //--------------------------------------------------------------
065700161006       // serie - segnacollo
065800161006       //--------------------------------------------------------------
065900161006       BEGSR  sr_segnacollo;
066000161006
066100161006         nrs = *blank;
066200161006         if pIn_nrs > 0;
066300161006           nrs = %editc(pIn_NRS:'X') + '-';
066400161006         endif;
066500161006         ncx1 = %subst(%editc(wSegnacollo:'X'):1:4);
066600161006         ncx2 = %subst(%editc(wSegnacollo:'X'):5:3);
066700161006         // trasformo per tutte le variabili gli 0 in O
066800161006         nrs = %xlate('0':'O':nrs);
066900161006         ncx1 = %xlate('0':'O':ncx1);
067000161006         ncx2 = %xlate('0':'O':ncx2);
067100161006
067200161006       ENDSR;
067300161003
067400161003       //--------------------------------------------------------------
067500161005       // riferimento
067600161003       //--------------------------------------------------------------
067700161005       BEGSR  sr_riferimento;
067800161003
067900170830         clear Riferim;
068000170830         if pIn_Cdp <> *blank;
068100170830           Riferim = 'Rif.: ' + pIn_Cdp;
068200170830         endif;
068300161005
068400161005       ENDSR;
068500161005
068600161005       //--------------------------------------------------------------
068700161005       // data spedizione
068800161005       //--------------------------------------------------------------
068900161005       BEGSR  sr_dataSped;
069000161005
069100161005         DtSped = %subst(%editc(pIn_MGS:'X'):3:2) + '/' +
069200161005                  %subst(%editc(pIn_MGS:'X'):1:2) + '/' +
069300161005                  %char(pIn_AAS);
069400161005
069500161005       ENDSR;
069600161005
069700161005       //--------------------------------------------------------------
069800161005       // flag ristampa e merce a valore
069900161005       //--------------------------------------------------------------
070000161005       BEGSR  sr_ristampa;
070100161005
070200161005sel 1    select;
070300161005
070400161005w   1     when pIn_rst  =  *blanks;
070500161005
070600161005w   1     when pIn_rst  =  'V';
070700161005           flagVal =  'V';
070800161005
070900161005w   1     when pIn_rst  =  'X';
071000161005           flagRst  =  'R';
071100161005           flagVal  =  'V';
071200161005
071300161005w   1     when pIn_rst  =  'W';
071400161005           flagRst  =  'M';
071500161005           flagVal  =  'V';
071600161005
071700161005x   1     other;
071800161005           flagRst  =  pIn_rst;
071900161005
072000161005e   1    endsl;
072100161005
072200161005       ENDSR;
072300161005
072400161005       //--------------------------------------------------------------
072500161005       // dati mittente
072600161005       //--------------------------------------------------------------
072700161005       BEGSR  sr_mittente;
072800161005
072900161003         mittente = %subst(pIn_RSM:1:26) + ' ' +
073000161003                    pIn_PRM;
073100161003
073200161003       ENDSR;
073300160930
073400160930       //--------------------------------------------------------------
073500160930       // dati destinatario
073600160930       //--------------------------------------------------------------
073700160930       BEGSR  sr_destinatario;
073800160930
073900160930         destinat01 = %subst(pIn_RSD:1:26) + ' ' +
074000160930                    pIn_PRD;
074100160930         destinat02 = %subst(pIn_IND:1:22) + ' ' +
074200160930                    pIn_LOD;
074300160930
074400160930       ENDSR;
074500160930
074600160930       //--------------------------------------------------------------
074700160930       // volume
074800160930       //--------------------------------------------------------------
074900160930       BEGSR  sr_volume;
075000160930
075100171002         volume = 'MC. ' + %trim(%editc(pIn_Vol:'1'));
075200160930
075300160930       ENDSR;
075400160930
075500160930       //--------------------------------------------------------------
075600160930       // Nesimo segnacollo di tot
075700160930       //--------------------------------------------------------------
075800160930       BEGSR  sr_nDiTot;
075900161006
076000170411       // se è stato passato un nr.segnacollo preciso, sarà quello il primo valore
076100171003       if pIn_NC1 <> 0;
076200171003         %subst(nDiTot : 1 : 5) =
076300171003           %editc(pIn_NC1:'Z');
076400170411       // se NON è stato passato un nr.segnacollo preciso, il primo valore sarà il
076500170411       // progressivo collo
076600171003       else;
076700171003         %subst(nDiTot : 1 : 5) =
076800170411         // wCollo è una variabile 7s0 per cui non ci sta in 5 char, devo prenderne
076900161006         // solo un pezzo
077000171003           %subst(
077100171003            // %editc(wCollo :'Z')
077200171003            %editc(%dec(wCollo - pIn_NCD + 1:7:0): 'Z')
077300171003            : 3
077400171003            //: %len(%editc(wCollo :'Z')) - 5 + 1
077500171003            : 5);
077600171003       endif;
077700171003
077800161006       %subst(nDiTot : 6 : 6) = '-' +
077900171003              %trim(%editc(pIn_NC2:'Z'));
078000160930
078100160930       ENDSR;
078200160930
078300160930       //--------------------------------------------------------------
078400160930       // peso
078500160930       //--------------------------------------------------------------
078600160930       BEGSR  sr_peso;
078700160930
078800171002         peso = 'KG.  ' + %trim(%editc(pIn_PKg:'1'));
078900160930
079000160930       ENDSR;
079100161011
079200170310       //--------------------------------------------------------------
079300170310       // Impostazioni PRTF personalizzate
079400170310       //--------------------------------------------------------------
079500170310       BEGSR  sr_OVRPRTF;
079600170310
079700170310         // se il prtf è chiuso oppure è già aperto ma i parametri di OVR personalizzati passati
079800170310         // sono diversi da quelli con cui è stata fatta la open
079900170310         if  Not %open(UBBRTETIZP) OR
080000170310             %open(UBBRTETIZP) and
080100170310              (wOutQ <> pIn_OutQ OR wUsrDta <> pIn_UsrDta);
080200170310           // se PRTF aperto ma i parametri di OVR personalizzati passati
080300170310           // sono diversi da quelli con cui è stata fatta la open
080400170310           if %open(UBBRTETIZP) and
080500170310            (wOutQ <> pIn_OutQ OR wUsrDta <> pIn_UsrDta);
080600170310             // chiudo il PRTF
080700170310             close UBBRTETIZP;
080800170310           endif;
080900170410           //if pIn_OutQ <> *blank OR pIn_UsrDta <> *blank;
081000170310             Qcmd = 'OVRPRTF file(UBBRTETIZP)';
081100170310             if pIn_OutQ <> *blank;
081200170410               Qcmd = %trim(Qcmd) +
081300170310                    ' outq(' + %trim(pIn_OutQ) + ')';
081400170410             else;
081500170410               Qcmd = %trim(Qcmd) +
081600170807                    ' outq(QPRINTS)';
081700170310             endif;
081800170310             if pIn_UsrDta <> *blank;
081900170410               Qcmd = %trim(Qcmd) +
082000170310                    ' usrdta(' + %trim(pIn_UsrDta) + ')';
082100170410             else;
082200170410               Qcmd = %trim(Qcmd) +
082300170410                    ' usrdta(P' + %editc(wCollo:'X') + ')';
082400170310             endif;
082500170807             exsr  sr_RtvSpoolID;
082600170310             exsr  sr_ExecCmd;
082700170310             if  Qusei <> *blank;
082800170310               pOut_Esito = 2;
082900170310               exsr sr_RoutEnd;
083000170406             else;
083100170406               // salvo i parametri di OVR personalizzati
083200170406               wOutQ = pIn_OutQ;
083300170406               wUsrDta = pIn_UsrDta;
083400170310             endif;
083500170410           //endif;
083600170310           open  UBBRTETIZP;
083700170310         endif;
083800170310
083900170310       ENDSR;
084000170310
084100170310       //--------------------------------------------------------------
084200170310       // Esecuzione del comando (già impostato).                      ?
084300170310       //--------------------------------------------------------------
084400170310       BEGSR  sr_ExecCmd;
084500170310
084600170310         clear Qcap0100;
084700170310         Qcabcsdh = *off;
084800170310         Qcapa    = *off;
084900170310         Qcacmdss = *off;
085000170310         Qcaerved = *allX'00';
085100170310
085200170310         clear Qusec;
085300170310         Qusbprv  = %size(Qusec);
085400170310
085500170310         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
085600170310                           %size(Qcap0100) : 'CPOP0100' : *omit :
085700170310                           0 : 0 : Qusec);
085800170310
085900170310       ENDSR;
086000170807
086100170807       //--------------------------------------------------------------
086200170807       // Reperimento ID univoco pe rlo spool corrente                 ?
086300170807       //--------------------------------------------------------------
086400170807       BEGSR  sr_RtvSpoolID;
086500170807
086600170807        wSpecialSpl = *off;
086700170807
086800170807        // Effettuo reperimento ID spool solo se specificatamente richiesta OUTQ = 'VASSPOOL'
086900170824        // o meglio, la OUTQ indicata in tab.VPR
087000170824        if pIn_OutQ = DftOUTQ;
087100170807
087200170807        wSpecialSpl = *on;
087300170807
087400170807        clear oVCPEsito;
087500170807        clear oVCPNum;
087600170807        clear oVCPNumDal;
087700170807        clear oVCPIncr;
087800170807
087900170807        UBVCP0R_PRG('UBBRTETI'
088000170807                   :'SPOOL_ID_UNIQUE'
088100170807                   :oVCPEsito
088200170807                   :oVCPNum
088300170807                   :1
088400170807                   :oVCPNumDal
088500170807                   :oVCPIncr);
088600170807
088700170807       if oVCPEsito <> *zeros;
088800170807          pOut_Esito = 2;
088900170807          exsr sr_RoutEnd;
089000170807       else;
089100170807          wFormType = 'I' + %subst(%editc(oVCPNumDal:'X'):7:9);
089200170807          Qcmd = %trim(Qcmd) +
089300170807                 ' formtype('+%trim(wFormType)+')';
089400170807       endif;
089500170807       endif;
089600170807
089700170807       ENDSR;
089800170807
089900170807       //--------------------------------------------------------------
090000170807       // Reperimento info JOB spool corrente                 ?
090100170807       //--------------------------------------------------------------
090200170807       BEGSR  sr_RtvSpoolJob;
090300170807
090400170807         // Inizializzo ID Job spool
090500170807         clear wJob_Nam;
090600170807         clear job;
090700170807         clear user;
090800170807         clear jobn;
090900170807
091000170807         // Dichiarazione cursore
091100170807         exec sql declare C0 cursor for
091200170807         SELECT JOB_NAME FROM QSYS2.OUTPUT_QUEUE_ENTRIES WHERE
091300170807         OUTPUT_QUEUE_NAME = :pIn_OutQ and
091400170807         USER_NAME = :JobUserCurr and
091500170807         SPOOLED_FILE_NAME = :PFile_Nam and
091600170807         FILE_NUMBER = :PFile_Num and
091700170807         FORM_TYPE = :wFormType;
091800170807
091900170807         // Apro il cursore
092000170807         exec sql open C0;
092100170807
092200170807         // Leggo i dati
092300170807         if sqlcode = *zeros;
092400170807            exec sql
092500170807                 fetch next from C0 into :wJob_Nam;
092600170807         endif;
092700170807
092800170807         // Chiusura cursore
092900170807         exec sql close C0;
093000170807
093100170807         // Se ID Job spool non reperito => errore "fatale"
093200170807         if wJob_Nam = *blanks;
093300170807            pOut_Esito = 2;
093400170807            exsr sr_RoutEnd;
093500170807         else;
093600170807
093700170807            wPos = *zeros;
093800170807
093900170807            // Reperimento Job_Number
094000170807            wStr = wPos+1;
094100170807            wPos = %scan('/':wJob_Nam:wStr);
094200170807            if wPos > *zeros;
094300170807               jobn = %subst(wJob_Nam:wStr:wPos-wStr);
094400170807               wJob_Num = %dec(jobn:6:0);
094500170807               jobn = %editc(wJob_Num:'X');
094600170807            else;
094700170807               pOut_Esito = 2;
094800170807               exsr sr_RoutEnd;
094900170807            endif;
095000170807
095100170807            // Reperimento User_Name
095200170807            wStr = wPos+1;
095300170807            wPos = %scan('/':wJob_Nam:wStr);
095400170807            if wPos > *zeros;
095500170807               user = %subst(wJob_Nam:wStr:wPos-wStr);
095600170807            else;
095700170807               pOut_Esito = 2;
095800170807               exsr sr_RoutEnd;
095900170807            endif;
096000170807
096100170807            // Reperimento Job_Name
096200170807            wStr = wPos+1;
096300170807            job = %trim(%subst(wJob_Nam:wStr));
096400170807            if job = *blanks;
096500170807               pOut_Esito = 2;
096600170807               exsr sr_RoutEnd;
096700170807            endif;
096800170807
096900170807         endif;
097000170824
097100170824       ENDSR;
097200170824
097300170824       //--------------------------------------------------------------
097400170824       // Routine iniziale
097500170824       //--------------------------------------------------------------
097600170824       BEGSR  *inzsr;
097700170824
097800170824     c     *Entry        plist
097900170824     c                   parm                    UBBRTETIDS
098000170824
098100170824         // recupero una volta per tutte alcuni dati in tabella
098200170824         exsr RtvTAB;
098300170824
098400170824       ENDSR;
098500170824
098600170824     C*
098700170824     C     rtvTAB        begsr
098800170824     C*
098900170824     C* Richiami per reperimento vari parametri di procedura
099000170824     C                   eval      IS713TBECOD    = 'VPR'
099100170825     C                   eval      IS713TBEKE1    = '*VASWS'
099200170824     C                   eval      IS713TBEKE2    = '*DFTOUTQ'
099300170824     C                   eval      IS713TBELIN    = *blanks
099400170824     C                   eval      IS713TBESIF    = *blanks
099500170824     C*
099600170824     C                   call      'TIS713R'
099700170824     C                   parm                    IS713TBECOD
099800170824     C                   parm                    IS713TBEKE1
099900170824     C                   parm                    IS713TBEKE2
100000170824     C                   parm                    IS713TBELIN
100100170824     C                   parm                    IS713TBESIF
100200170824     C                   parm                    OS713TBEUNI
100300170824     C                   parm                    OS713ESITO
100400170824     C*
100500170824     C                   if        OS713ESITO = '1'
100600170824     C                   eval      DftOUTQ = OS713TBEUNI
100700170824     C                   else
100800170825             if pIn_OutQ <> *blank;
100900170825               DftOUTQ = %trim(pIn_OutQ);
101000170825             else;
101100170825     C                   eval      DftOUTQ = 'QPRINTS'
101200170825             endif;
101300170824     C                   endif
101400170824     C*
101500170824     C                   endsr

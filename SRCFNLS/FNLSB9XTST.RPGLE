000100100722     /*END
000200090112       //--------------------------------------------------------------
000300090112       // - - - - - - - - - - -   P R O V A   - - - - - - - - - - - - -
000400090112       //               S T A M P A   S U   L A S E R
000500090112       //           D O C U M E N T O   D I   R I T I R O
000600090112       //--------------------------------------------------------------
000700030623
000800080704     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000900100414     h dftactgrp(*no)
001000030623
001100090112       //--------------------------------------------------------------
001200090112       //?Dichiarazione file.
001300090112       //--------------------------------------------------------------
001400030623
001500100722     fFNLSB9P   o    e             printer infds(PrtInfo)
001600030623
001700090112       //--------------------------------------------------------------
001800090112       //?Definizione costanti.
001900090112       //--------------------------------------------------------------
002000090112
002100030623     d $Lungh          c                   const('*...+....1....+....2....+....-
002200031104     d                                     3....+....4....+....5....+....6....+-
002300031104     d                                     ....7....+....8....+....A')
002400030623     d $LunghN         c                   const(123456789012345678901234567890)
002500030623     d $LunghA         c                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZABC-
002600030623     d                                     DEFGHIJKLMNOPQRSTUVWXYZ')
002700030623     d $LunghW         c                   const('WWWWWWWWWWWWWWWWWWWWWWWWWWWWW-
002800030623     d                                     WWWWWWWWWWWWWWWWWWWWWWW')
002900100721
003000120123       // -?Intestazione BRT?
003100110503     d $BRT            c                   const('BRT S.p.A.')
003200130218     d $Intest1        c                   const('Sede Operativa ed Am-
003300130218     d                                     ministrativa:')
003400130218     d $Intest2        c                   const('via E. Mattei, 42 -
003500130218     d                                     - 40138 Bologna')
003600130218     d $Intest3        c                   const('Sede Legale: Piazza -
003700130218     d                                     Diaz, 7 - 20123 Milano')
003800130218     d $Intest4        c                   const('C.F. e P.IVA 04507990150')
003900130218     d $Intest5        c                   const(' Capitale Sociale Eu-
004000130218     d                                     ro 24.000.000 int. vers.')
004100130218     d $Intest6        c                   const('info on-line')
004200130218     d $Intest7_brt    c                   const('www.brt.it')
004300100721
004400100721       // -?Costanti nel Documento di Trasporto?
004500090114     d c_Ref           c                   const('(REFERENTE)')
004600090114     d c_Tel           c                   const('(TELEFONO)')
004700090114     d c_Ind           c                   const('(INDIRIZZO)')
004800090114     d c_Cap           c                   const('(CAP)')
004900090114     d c_Loc           c                   const('(LOCALITÀ)')
005000090116     d c_Prv           c                   const('(PROV / NAZIONE)')
005100100721     d*// c_Firma         c                   const('(FIRMA)')
005200100721     d*// c_Data          c                   const('(DATA)')
005300100721
005400090116     ***d c_Accettaz      c                   const('Per accettazione del+
005500090116     ***d                                     le condizioni di trasporto +
005600090116     ***d                                     riportate sul retro.')
005700100721     d*// c_Accettaz      c                   const('Per accettazione del+
005800100721     d*//                                     le condizioni di trasporto.+
005900100721     d*//                                     ')
006000100721
006100100721     d c_Espresso      c                   const('PRIORITY')
006200100721     d c_H1030         c                   const('H 10:30 ')
006300100721
006400100721       // -?Numero dei Box per Pagina "A4"?
006500090112     d $BxP            c                   const(4)
006600100721
006700100721       // -?Spessore delle linee in inches/pollici (*memo)?
006800090112     d c_LineNarrow    c                   const(0.008)
006900090112     d c_LineMedium    c                   const(0.017)
007000090112     d c_LineWide      c                   const(0.025)
007100100721
007200100721       // -?Posizioni "base" per:?
007300120123       //  ?- logo BRT in Pagina "A4"?
007400120123     d $PS0            c                   const(0.250)
007500100721
007600120123       //  ?- intestazione "BRT S.p.A."?
007700130218     d c_PosTBar       c                   const(0.320)
007800130218     d c_PosTBa1       c                   const(0.400)
007900130218     d c_PosTBa2       c                   const(0.480)
008000130218     d c_PosTBa3       c                   const(0.550)
008100130218     d c_PosTBa4       c                   const(0.640)
008200130218     d c_PosTBa5       c                   const(0.710)
008300130218     d c_PosTBa6       c                   const(0.800)
008400100721
008500100721       //  ?-?bordi orizzontali superiori box?
008600100721       //    ?(distanza dal lato superiore dell'A4)?
008700100721       //  ?N.B. - c_PosBoxA => posizione iniziale della linea?
008800100721       //                      ?orizzontale Superiore?
008900100721       //       ?- c_PosBoxB => posizione iniziale della linea?
009000100721       //                      ?orizzontale Inferiore?
009100090112     d c_PosBoxA       c                   const(0.200)
009200090113     d c_PosBoxB       c                   const(2.825)
009300100721
009400100721       //  ?- altre linee orizzontali?
009500090114     d c_PosLO1        c                   const(1.320)
009600090114     d c_PosLO2        c                   const(2.520)
009700090115     d c_PosLO3        c                   const(0.860)
009800090115     d c_PosLO4        c                   const(1.120)
009900090115     d c_PosLO5        c                   const(1.250)
010000090115     d c_PosLO6        c                   const(1.460)
010100090115     d c_PosLO7        c                   const(1.680)
010200090115     d c_PosLO8        c                   const(1.900)
010300090115     d c_PosLO9        c                   const(2.120)
010400090115     d c_PosLO0        c                   const(2.350)
010500100721
010600120123       // -?Dimensioni base per il simil-logo BRT?
010700120123      /copy gaitrasrc/srcTNSY,LogoBRT_R1
010800100721
010900100721       // -?Costanti?
011000090115     ***d c_PosKmre       c                   const(1.300)
011100090115     ***d c_PosKind       c                   const(1.925)
011200090115     ***d c_PosKloc       c                   const(2.150)
011300090115     ***d c_PosKdre       c                   const(2.500)
011400100721
011500100721       // -?Costante per controllo "caratteri solo numerici"?
011600100721     d c_DigitN        c                   const('1234567890')
011700090112
011800090112       //--------------------------------------------------------------
011900090112       //?Definizione schiere.
012000090112       //--------------------------------------------------------------
012100090112
012200090113     d $Cli            s             50    dim(12) ctdata perrcd(1)
012300090112
012400090112       //--------------------------------------------------------------
012500090112       //?Definizione aree dati.
012600090112       //--------------------------------------------------------------
012700090112
012800100721       // -?Dati utente?
012900090112     d §AzUte        e ds                  extname(AZUTE00F)
013000090112     d                                     dtaara
013100090112     d §DatiUte      e ds                  extname(dDatiUte)
013200090112     d                                     dtaara
013300090112
013400090112       //--------------------------------------------------------------
013500090112       //?Definizione strutture dati.
013600090112       //--------------------------------------------------------------
013700090112
013800100721       // -?Parametri ricevuti?
013900030623     d KPJBA         e ds
014000090112
014100100721       // -?Reperimento dati utente?
014200040223     d TIBS34ds      e ds
014300090112
014400100721       // -?InfDS?
014500090113     d PrtInfo         ds
014600090113     d  CurLine              367    368i 0
014700090112
014800100721       // -?Status?
014900030623     d Status         sds           333
015000030623     d  SDSpgm           *proc
015100030623     d  SDSprm           *parms
015200030623     d  SDSdta               191    198
015300030623     d  SDSjob               244    253
015400030623     d  SDSusr               254    263
015500090112
015600090112       //--------------------------------------------------------------
015700090112       //?Definizione variabili globali.
015800090112       //--------------------------------------------------------------
015900090112
016000100721       // -?Variavili per QCMDEXC?
016100030623     d Qcmd            s            150    inz
016200030623     d Qlen            s             15  5 inz(150)
016300100721
016400100721       // -?Variabili per il calcolo delle posizioni per box in A4?
016500090113     d wNumDoc         s              4  0 inz
016600090113     d wSeqDoc         s              1  0 inz
016700100721     d wPosIni         s              5  3 inz
016800100721
016900090113     d wNrCpy          s              3  0 inz(004)
017000090112
017100090112       //--------------------------------------------------------------
017200090112       //?Definizione procedure esterne.
017300090112       //--------------------------------------------------------------
017400090112
017500100721       // -?Reperimento dati utente?
017600100721      /copy gaitrasrc/srcProtoPR,TIBS34R
017700030623
017800090112       //--------------------------------------------------------------
017900030623
018000090112       //--------------------------------------------------------------
018100090112       //?Definizione key-list.
018200090112       //--------------------------------------------------------------
018300090112
018400030623
018500090112       //--------------------------------------------------------------
018600090112       //?M A I N - L I N E
018700090112       //--------------------------------------------------------------
018800030623     c     *Entry        plist
018900090112     c                   parm                    £Dat              1
019000090112     c                   parm                    £NrCpy           15 5
019100090114       // £Dat - tipo dati: ' ' = vuoto
019200090114       //                   '0' = dati max di prova
019300090112       //                   '1' = dati fittizi
019400090112       // £NrCpy - num. copie da stampare
019500090112       //
019600090112      /free
019700090112
019800100721       // -?Operazioni iniziali?
019900100721       exsr  sr_RoutInz;
020000090112
020100100721       // -?Stampa Documento di Ritiro?
020200090114       clear  wNumDoc;
020300090114       for  wNumDoc = 1  to  wNrCpy;
020400090114         exsr  sr_PrtDoc;
020500090114       endfor;
020600090112
020700100721       // -?Operazioni finali?
020800090113       return;
020900090112
021000090112       //--------------------------------------------------------------
021100090112       //?Operazioni iniziali.
021200090112       //--------------------------------------------------------------
021300090112       BEGSR  sr_RoutInz;
021400090112
021500090112         *inLR = *on;
021600090112
021700100721         // -?Reperimento Dati del job (Utente/Operativi)?
021800100721         exsr  sr_DatiJob;
021900090112
022000100721         // -?Impostazione n° copie da stampare?
022100090112         select;
022200090112           when  SDSprm >  1;
022300090112             wNrCpy =  £NrCpy;
022400090112           other;
022500090112             reset  wNrCpy;
022600090112         endsl;
022700090112
022800090112       ENDSR;
022900090112
023000090112       //--------------------------------------------------------------
023100090112       //?Reperimento Dati del job (Utente/Operativi).
023200090112       //--------------------------------------------------------------
023300090112       BEGSR  sr_DatiJob;
023400090112
023500090112         in(E) §AzUte;
023600090112         if NOT %error;
023700090112           in(E) §DatiUte;
023800090112         endif;
023900090112         if %error or RSut = *blanks;
024000090112           clear TIBS34ds;
024100090112           tibs34r(tibs34ds);
024200090112           in §AzUte;
024300090112           in §DatiUte;
024400090112         endif;
024500090112
024600090112       ENDSR;
024700030912
024800090112       //--------------------------------------------------------------
024900090112       //?Stampa Documento di Ritiro
025000090112       //--------------------------------------------------------------
025100090113       BEGSR  sr_PrtDoc;
025200090112
025300100721         // -?Pulizia del PrtF?
025400090113         exsr  sr_ClrPrtF;
025500090114
025600100721         // -?Posizionamento box all'interno del foglio A4?
025700100721         exsr  sr_PosizBox;
025800090112
025900100721         // -?Preparazione dati per stampa Documento Di Ritiro?
026000090114         select;
026100090114           when  SDSprm > *zero  and  £Dat = *blank;
026200090114             exsr  sr_PrtfFld_;
026300090114           when  SDSprm = *zero  or   £Dat = *off;
026400090114             exsr  sr_PrtfFld_0;
026500090114           when  SDSprm > *zero  and  £Dat = *on;
026600090114             exsr  sr_PrtfFld_1;
026700090114         endsl;
026800090114
026900100721         // -?Impostaz. indicatori x Stampa documento di ritiro?
027000090114         *in51 = (pMitt00 = *blank   and
027100090114                  pMitt01 = *all'.'  and
027200090114                  pMitt02 = *all'.'  and
027300090114                  pMitt03 = *all'.'  and
027400090114                  pMitt04 = *all'.'  and
027500090114                  pMitt05 = *all'.'  and
027600090119                  pMitt06 = *all'.');
027700090119         *in52 = (pMitt07 = *all'.'  and
027800090114                  pMitt08 = *all'.');
027900090119         *in53 = (pDest00 = *blank   and
028000090114                  pDest01 = *all'.'  and
028100090114                  pDest02 = *all'.'  and
028200090114                  pDest03 = *all'.'  and
028300090114                  pDest04 = *all'.'  and
028400090114                  pDest05 = *all'.'  and
028500090114                  pDest06 = *all'.'  and
028600090114                  pDest07 = *all'.'  and
028700090114                  pDest08 = *all'.'  and
028800090114                  pDest09 = *all'.');
028900100721
029000100721         // -?Stampa box?
029100100722         write  LSB9box;
029200100721
029300120123         // -?Stampa logo BRT?
029400120123         write  LSB9logBrt;
029500100721
029600100721         // -?Stampa testo?
029700100722         write  LSB9txt;
029800100721
029900100721         // -?Stampa indicazione dei dati da inserire a mano?
030000090114         if  SDSprm > *zero  and  £Dat = *blank;
030100100722           //write  LSB9manM;
030200100722           write  LSB9manD;
030300090114         endif;
030400090112
030500090112       ENDSR;
030600090112
030700090112       //--------------------------------------------------------------
030800090112       //?Pulizia file di stampa
030900090112       //--------------------------------------------------------------
031000090113       BEGSR  Sr_ClrPrtF;
031100090112
031200100722         clear  LSB9pos;
031300100722         //clear  LSB9nul;            (no campi);
031400100722         clear  LSB9rowNr;
031500120123         clear  LSB9logBrt;
031600100722         clear  LSB9Box;
031700100722         clear  LSB9manD;
031800100722         clear  LSB9txt;
031900100722         //clear  LSB9rowPos;
032000100721
032100100721         *in = *off;
032200090114
032300100721         //Kacc   = c_Accettaz;
032400100721         //Kfir   = c_Firma;
032500100721         //Kdat   = c_Data;
032600100722         kXS1   = c_Espresso;
032700100722         kXS2   = c_H1030;
032800090112
032900090112       ENDSR;
033000090114
033100090114       //--------------------------------------------------------------
033200090114       //?Posizionamento del box all'interno della pag. A4
033300090114       //--------------------------------------------------------------
033400100721       BEGSR  sr_PosizBox;
033500090114
033600100721         // -?Calcolo la posizione x box all'interno della pagina "A4"?
033700090114         //wNumDoc += 1;                 (già incrementato)
033800090114         wSeqDoc = %rem( wNumDoc : $BxP );
033900090114
034000090114         if  wSeqDoc = *zero;
034100090114           wSeqDoc = 4;
034200090114         endif;
034300090114
034400100721         // -?Posizionamento nella pagina A4 in base al n° Doc. in?
034500100721         //  ?stampa?
034600090114         *in41 = (wSeqDoc = 1);
034700090114         *in42 = (wSeqDoc = 2);
034800090114         *in43 = (wSeqDoc = 3);
034900090114         *in44 = (wSeqDoc = 4);
035000100722         WRITE     LSB9pos;
035100090114
035200100721         // -?Imposto le posizioni nella pagina in corso di stampa?
035300090114         wPosIni = (CurLine / 9) - 0.120;
035400090114
035500120123         // -?Simil-Logo BRT?
035600100721         §PlblS  = wPosIni + $PS0;
035700120123         §PlblVS = 7.539;
035800120123         exsr  sr_SimilLogoBRT_;
035900090114
036000120123         // -?Impotazione dati BRT S.p.A.?
036100130218         TBAR  = $BRT;                   // - BRT S.p.A.
036200130218         TBA1  = $Intest1;               // - "Sede Operativa ed Amministrativa"
036300130218         TBA2  = $Intest2;               // - Indirizzo (BO)
036400130218         //TBA3  = $Intest3;               // - Sede legale: indirizzo
036500130218         TBA4  = $Intest4;               // - C.F. / P.I.
036600130218         TBA5  = $Intest5;               // - Capitale sociale
036700130218         //TBA6  = $Intest6;               // - "info on-line"
036800130218         TBA7  = $Intest7_BRT;           // - sito internet
036900090114         §PC0  = wPosIni + c_PosTBar;
037000090114         §PC1  = wPosIni + c_PosTBa1;
037100090114         §PC2  = wPosIni + c_PosTBa2;
037200090114         §PC3  = wPosIni + c_PosTBa3;
037300090114         §PC4  = wPosIni + c_PosTBa4;
037400130218         §PC5  = wPosIni + c_PosTBa5;
037500130218         §PC6  = wPosIni + c_PosTBa6;
037600090114
037700100721         // -?Box generale?
037800090114         §BoxA = wPosIni + c_PosBoxA;
037900090114         §BoxB = wPosIni + c_PosBoxB;
038000090114
038100100721         // -?Linee orizzontali?
038200090114         §LO1  = wPosIni + c_PosLO1;
038300090114         §LO2  = wPosIni + c_PosLO2;
038400090114         §LO3  = wPosIni + c_PosLO3;
038500090114         §LO4  = wPosIni + c_PosLO4;
038600090114         §LO5  = wPosIni + c_PosLO5;
038700090114         §LO6  = wPosIni + c_PosLO6;
038800090114         §LO7  = wPosIni + c_PosLO7;
038900090114         §LO8  = wPosIni + c_PosLO8;
039000090114         §LO9  = wPosIni + c_PosLO9;
039100090114         §LO0  = wPosIni + c_PosLO0;
039200090114         §LOA  = §BoxB - 0.080;
039300090114
039400100721         // -?Linee verticali?
039500090114         §LV1  = §BoxB - §BoxA;
039600090115         §LV2  = §LO7  - §LO3;
039700100721         §LV3  = §LO8  - §LO7;
039800100721         §LV4  = §LO9  - §LO8;
039900090114
040000100721         // -?Mini-Box interni?
040100090115         §Box1a = §LO4 + 0.020;
040200090114         §Box1b = §Box1a + 0.100;
040300100721         §Box2a = §LO8 + ( ( c_PosLo9 - c_PosLO8 - 0.100 ) / 2 );
040400100721         §Box2b = §Box2a + 0.100;
040500090114
040600100721         // -?Costanti nel box?
040700100721         §pkXFD = §Box2b - 0.015;
040800090115         §pkFIR = §LOA + 0.062;
040900090114
041000090114       ENDSR;
041100110503
041200110503       //--------------------------------------------------------------
041300120123       //?Posizionamento del simil-logo BRT (unico)                    ?
041400110503       //--------------------------------------------------------------
041500120123      /copy gaitrasrc/srcTNSY,LogoBRT_R2
041600090114
041700090114       //--------------------------------------------------------------
041800090114       //?Impostazione campi in stampa (vuoti)
041900090114       //--------------------------------------------------------------
042000090114       BEGSR  sr_PrtfFld_;
042100090114
042200090114         // Dati Mittente
042300090114         // ¯¯¯¯¯¯¯¯¯¯¯¯¯
042400090114       //pMitt00 = *blank;
042500090114         pMitt01 = *all'.';
042600090114         pMitt02 = *all'.';
042700090114         pMitt03 = *all'.';
042800090114         pMitt04 = *all'.';
042900090114         pMitt05 = *all'.';
043000090114         pMitt06 = *all'.';
043100090114         pMitt07 = *all'.';
043200090114         pMitt08 = *all'.';
043300090114
043400090114         // Dati Destinatario
043500090114         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
043600090114       //pDest00 = *blank;
043700090114         pDest01 = *all'.';
043800090114         pDest02 = *all'.';
043900090114         pDest03 = *all'.';
044000090114         pDest04 = *all'.';
044100090114         pDest05 = *all'.';
044200090114         pDest06 = *all'.';
044300090114         pDest07 = *all'.';
044400090114         pDest08 = *all'.';
044500090114         pDest09 = *all'.';
044600090114
044700090114         // Costanti
044800090114         // ¯¯¯¯¯¯¯¯
044900090114         Kref   = c_Ref;
045000090114         Ktel   = c_Tel;
045100090115         Kind   = c_Ind;
045200090115         Kcap   = c_Cap;
045300090115         Kloc   = c_Loc;
045400090115         Kprv   = c_Prv;
045500090114
045600090114         // Stampa area dati vuota "Mittente"
045700090115         // (posizionamento calcolato in base alla riga sotto la quale
045800090115         //  verranno stampati tali costanti)
045900100414         ////§pkRef = wPosIni + c_PosKmre;
046000100414         //§pkRef = ((Curline + 10) / 9) + 0.055;
046100100722         //write  LSB9manM;
046200090114
046300090114         // Stampa area dati vuota "Destinatario"
046400090115         // (posizionamento calcolato in base alla riga sotto la quale
046500090115         //  verranno stampati tali costanti)
046600090115         //§pkInd = wPosIni + c_PosKind;
046700090115         //§pkLoc = wPosIni + c_PosKloc;
046800090115         //§pkRef = wPosIni + c_PosKdre;
046900090115         §pkInd = ((Curline + 16) / 9) + 0.055;
047000090115         §pkLoc = ((Curline + 18) / 9) + 0.055;
047100090115         §pkRef = ((Curline + 21) / 9) + 0.055;
047200100722         write  LSB9manD;
047300090114
047400090114       ENDSR;
047500090113
047600090113       //--------------------------------------------------------------
047700090114       //?Impostazione campi in stampa (max riempimento)
047800090113       //--------------------------------------------------------------
047900090114       BEGSR  sr_PrtfFld_0;
048000090113
048100090113         // Dati Mittente
048200090113         // ¯¯¯¯¯¯¯¯¯¯¯¯¯
048300090113         pMitt00 = '1234567';
048400090113       //pMitt01 = *blank;
048500090115         pMitt02 = %subst( $LunghA : 1 : 35 );
048600090113       //pMitt03 = *blank;
048700090115         pMitt04 = %subst( $Lungh : 1 : 35 );
048800090113       //pMitt05 = *blank;
048900090113         pMitt06 = '123456789 '
049000090115                 + %subst( $LunghW : 1 : 35 )
049100090113                 + ' AB';
049200090113       //pMitt07 = *blank;
049300090115         pMitt08 = 'Mitt.Orig.: '
049400090115                 + %subst( $Lungh : 1 : 35 );
049500090113
049600090113         // Dati Destinatario
049700090113         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
049800090113       //pDest00 = *blank;
049900090113       //pDest01 = *blank;
050000090115         pDest02 = %subst( $LunghA : 1 : 35 );
050100090113       //pDest03 = *blank;
050200090115         pDest04 = %subst( $LunghA : 1 : 35 );
050300090113       //pDest05 = *blank;
050400090115         pDest06 = %subst( $Lungh : 1 : 35 );
050500090113       //pDest07 = *blank;
050600090113         pDest08 = '123456789 '
050700090115                 + %subst( $LunghW : 1 : 35 )
050800090113                 + ' AB';
050900090113       //pDest09 = *blank;
051000090113       //pDest10 = *blank;
051100090115
051200090115         // Estremi documento
051300090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
051400090115         pNDC    = 7654321;
051500090115         pDDC    = 12345678;
051600090115
051700090115         // Resa / Servizio / Fermo deposito
051800090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
051900090115         pXFR    = 'X';
052000090115         pXAS    = 'X';
052100100722         pXS1    = 'X';
052200100722         pXS2    = 'X';
052300090115         pXFD    = 'X';
052400090115
052500090115         // Data consegna richiesta
052600090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
052700090115         pTDR    = 'PRIMA';
052800090115         pDCR    = 654321;
052900090115         pHCR    = '12:34';
053000090115
053100090115         // Turni di chiusura
053200090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
053300090115         pGC1    = 'LUN-M';
053400090115         pGC2    = 'VEN-P';
053500090115
053600090115         // Importo da assicurare
053700090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
053800090115         pDAS    = 'EUR';
053900090115         pIAS    = 9876543,21;
054000090115
054100090115         // Importo contrassegno
054200090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
054300090115         pDCA    = 'EUR';
054400090115         pCAS    = 1234567,89;
054500090115
054600090115         // Colli / Peso / Volume
054700090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
054800090115         pNCL    = 12345;
054900090115         pPKF    = 123456,7;
055000090115         pVLF    = 12,345;
055100090115
055200090115         // Natura merce
055300090115         // ¯¯¯¯¯¯¯¯¯¯¯¯
055400090115         pNAT    = $LunghA;
055500090115
055600090115         // Note
055700090115         // ¯¯¯¯
055800090115         pNP1    = $Lungh;
055900090113
056000090113       ENDSR;
056100090114
056200090114       //--------------------------------------------------------------
056300090114       //?Impostazione campi in stampa (dati di prova)
056400090114       //--------------------------------------------------------------
056500090114       BEGSR  sr_PrtfFld_1;
056600090114
056700090114         // Dati Mittente
056800090114         // ¯¯¯¯¯¯¯¯¯¯¯¯¯
056900090114         pMitt00 = '1234567';
057000090114       //pMitt01 = *blank;
057100090114         pMitt02 = $Cli(01);
057200090114       //pMitt03 = *blank;
057300090114         pMitt04 = $Cli(02);
057400090114       //pMitt05 = *blank;
057500090114         pMitt06 = %trimr( $Cli(03) )
057600090114                 + ' ' + %subst( $Cli(04) : 1 : 35)
057700090114                 + ' ' + %trimr( $Cli(05) );
057800090114       //pMitt07 = *blank;
057900090116         pMitt08 = 'Mitt.Orig.: '
058000090114                 + %trimr( $Cli(06) );
058100090114
058200090114         // Dati Destinatario
058300090114         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
058400090114       //pDest00 = *blank;
058500090114       //pDest01 = *blank;
058600090114         pDest02 = $Cli(07);
058700090114       //pDest03 = *blank;
058800090114         pDest04 = $Cli(08);
058900090114       //pDest05 = *blank;
059000090114         pDest06 = $Cli(09);
059100090114       //pDest07 = *blank;
059200090114         pDest08 = %trimr( $Cli(10) )
059300090114                 + ' ' + %subst( $Cli(11) : 1 : 35)
059400090114                 + ' ' + %trimr( $Cli(12) );
059500090114       //pDest09 = *blank;
059600090114       //pDest10 = *blank;
059700090115
059800090115         // Estremi documento
059900090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
060000090115         pNDC    = 0665426;
060100090115         pDDC    = *date;
060200090115
060300090115         // Resa / Servizio / Fermo deposito
060400090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
060500090115         pXFR    = 'X';
060600090115       //pXAS    = 'X';
060700100722       //pXS1    = 'X';
060800100722         pXS2    = 'X';
060900100721       //pXFD    = 'X';
061000090115
061100090115         // Data consegna richiesta
061200090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
061300090115         pTDR    = 'PRIMA';
061400090115         pDCR    = Udate + 10000;
061500090115         pHCR    = '12:00';
061600090115
061700090115         // Turni di chiusura
061800090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
061900090115         pGC1    = 'LUN-M';
062000090115         pGC2    = 'VEN-P';
062100090115
062200090115         // Importo da assicurare
062300090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
062400090115         pDAS    = 'EUR';
062500090115         pIAS    = 81,23;
062600090115
062700090115         // Importo contrassegno
062800090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
062900090115         pDCA    = 'EUR';
063000090115         pCAS    = 75,31;
063100090115
063200090115         // Colli / Peso / Volume
063300090115         // ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
063400090115         pNCL    = 13;
063500090115         pPKF    = 23,4;
063600090115         pVLF    = 12,300;
063700090115
063800090115         // Natura merce
063900090115         // ¯¯¯¯¯¯¯¯¯¯¯¯
064000090115         pNAT    = 'CARTONI        ';
064100090115
064200090115         // Note
064300090115         // ¯¯¯¯
064400090115         pNP1    = 'Note ...................*';
064500090114
064600090114       ENDSR;
064700080605
064800080605      /end-free
064900030915
065000090113       //--------------------------------------------------------------
065100090113       //?Schiere a tempo di compilazione.
065200090113       //--------------------------------------------------------------
065300030915
065400100721** -?$Cli?---------------------------------------*
065500030623ROSSI ERNESTO ....................*                 1
065600030623VIA DELLA FANTASIA, 99 ...........*                 2
06570003062340138                                               3
065800030623BOLOGNA ..........................*                 4
065900030623BO                                                  5
066000090113ARANCIONI ERMENEGILDO ............*                 6
066100090113VERDI GIUSEPPE ...................*                 7
066200090113(ESTENSIONE) .....................*                 8
066300090113VIA DELLA MUSICA, 10 .............*                 9
06640009011337128                                              10
066500090113VERONA ...........................*                11
066600090113VR                                                 12

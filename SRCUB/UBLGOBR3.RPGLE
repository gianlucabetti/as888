000100121217       //==============================================================
000200170303       // Carica in schiera gli oggetti presenti in una libreria leggendoli da US
000300121217       //==============================================================
000400141008
000500170303         // Gestione:
000600170303         // *blank = crea la lista oggetti in US mediante srvpgm UNLGOBR e passa una schiera
000700170303         //          di 100 elem. con gli oggetti reperiti fino al massimo della schiera
000800170303         // 'R'    = continua la lettura dello US e scrittura schiera
000900170303
001000170303         // Errori:
001100170303         // sono gli stessi del srvpgm UBLGOBR
001200170303         // propri di questo pgm:
001300170303         // 11 = Parametri obbligatori di ingresso mancanti
001400170303
001500121217       //--------------------------------------------------------------
001600161103       // Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
001700121217       //--------------------------------------------------------------
001800121217
001900121217     /*PRM dbgview(*source)
002000121217     /*PRM commit(*none)
002100121217     /*END
002200161108     **
002300161108     ** ISTRUZIONI PER LA COMPILAZIONE
002400161108     **
002500170509     ** 1. Creare il pgm UBLGOBR3 (Opz 14 PDM)
002600161108     **
002700121217
002800121217       //--------------------------------------------------------------
002900161103       // Specifiche di controllo.                                     ?
003000121217       //--------------------------------------------------------------
003100121217
003200170202     h decedit('0,') datedit(*dmy/) option(*nodebugio)
003300170202     h dftactgrp(*no) actgrp('QILE')
003400170202     h bnddir('UBBNDDIR')
003500161104
003600121217       //--------------------------------------------------------------
003700161103       // Dichiarazione file.                                          ?
003800121217       //--------------------------------------------------------------
003900121217
004000121217       //--------------------------------------------------------------
004100161103       // Definizione costanti.                                        ?
004200121217       //--------------------------------------------------------------
004300121217
004400121217
004500121217       //--------------------------------------------------------------
004600161103       // Definizione schiere.                                         ?
004700121217       //--------------------------------------------------------------
004800121217
004900121217
005000121217       //--------------------------------------------------------------
005100161103       // Definizione aree dati.                                       ?
005200121217       //--------------------------------------------------------------
005300121217
005400121217
005500121217       //--------------------------------------------------------------
005600161103       // Definizione strutture dati.                                  ?
005700121217       //--------------------------------------------------------------
005800121218
005900161103       // - Status?
006000121217     d Psds           sds
006100121217     d   SDSpgm          *proc
006200121217     d*//JobName             244    253                                         Job name
006300161103     d*//JobUser             254    263
006400121217     d*//JobNumber           264    269s 0                                      Job number
006500121217
006600121217       //--------------------------------------------------------------
006700161103       // Definizione variabili.                                       ?
006800121217       //--------------------------------------------------------------
006900170303
007000170303       // variabili per QUSRTVUS
007100170303     d Rtvus01         s             20    inz
007200170303     d Rtvus02         s              9b 0 inz
007300170303     d Rtvus03         s              9b 0 inz
007400170303
007500170303       // indici
007600170303     d wX              s              4s 0
007700170303     d wrkCount        s              9s 0
007800170303
007900170303       // appoggio
008000170303     d wAPIName        s             20
008100121217
008200161103       // - Campi di comodo?
008300170303     d Crtus01         s             20    inz
008400121218
008500121217       //--------------------------------------------------------------
008600161103       // Definizione prototipi procedure.                             ?
008700121217       //--------------------------------------------------------------
008800121217
008900170202       /copy gaitrasrc/srcProtoPR,UBLGOBR
009000170303
009100170303       // strutture generiche per API
009200170303      /copy QSYSINC/QRPGLESRC,QUSGEN
009300170303
009400170303       // prototipi per QUS...
009500170303      /copy gaitrasrc/srcprotopr,QUS
009600170303
009700170303       // Parametri gestione errori API.?
009800170303      /copy qSysInc/qRpgleSrc,QUSEC
009900170303
010000170303       // lista oggetti
010100170303      /copy QSYSINC/QRPGLESRC,QUSLOBJ
010200121217
010300121217       //--------------------------------------------------------------
010400161103       // Definizione key-list.                                        ?
010500121217       //--------------------------------------------------------------
010600161104
010700121217
010800121217       //--------------------------------------------------------------
010900161103       // Definizione parametri procedura.                             ?
011000121217       //--------------------------------------------------------------
011100121217
011200170306       /copy gaitrasrc/srcProtoPI,UBLGOBR3
011300161103
011400121217       //--------------------------------------------------------------
011500121217       //?M A I N - L I N E                                            ?
011600121217       //--------------------------------------------------------------
011700121217
011800170202     C     *ENTRY        PLIST
011900170306     C                   PARM                    LGOB3DS
012000170303
012100170303       // se la gestione è totale
012200170306       if LGOB3DS.iGestione = *blank;
012300170303
012400170303         // Operazioni iniziali?
012500170303         exsr sr_RoutInz;
012600121217
012700170303         // Estrazione
012800170303         exsr  sr_Estrai;
012900170303
013000170303       endif;
013100170303
013200170303       // Riempi schiera da restituire al chiamante
013300170303       exsr  sr_Valorizza;
013400121217
013500161103       // - Operazioni finali?
013600121217       exsr sr_RoutEnd;
013700121217
013800121217       //--------------------------------------------------------------
013900161103       // Operazioni iniziali.                                         ?
014000121217       //--------------------------------------------------------------
014100121217       BEGSR  sr_RoutInz;
014200121217
014300170303         WrkCount = 0;
014400170303
014500161103         // - Controllo parametri d'ingresso
014600161103         exsr  sr_CtrlParms;
014700130208
014800140925       ENDSR;
014900141028
015000141028       //--------------------------------------------------------------
015100161103       // Controllo parametri
015200141028       //--------------------------------------------------------------
015300161103       BEGSR  sr_CtrlParms;
015400170202
015500170202         // dati obbligatori
015600170303         //  libreria
015700170306         if LGOB3DS.iLib = *blank;
015800170306           LGOB3DS.oError = '11';
015900170202           exsr sr_RoutEnd;
016000170202         endif;
016100170303
016200170202         //  tipo oggetto
016300170306         if LGOB3DS.iObjT = *blank;
016400170306           LGOB3DS.iObjT = '*ALL';
016500170202         endif;
016600170303         //  filtro oggetto
016700170306         if LGOB3DS.iFiltroObj = *blank;
016800170306           LGOB3DS.iFiltroObj = '*ALL';
016900170202         endif;
017000161103
017100161103       ENDSR;
017200121217
017300121217       //--------------------------------------------------------------
017400170303       // Estrazione
017500121217       //--------------------------------------------------------------
017600170303       BEGSR  sr_Estrai;
017700170202
017800170303         // richiedo l'estrazione degli oggetti filtrati lasciandoli nello US
017900170509         ublgobr_s('99' : LGOB3DS.iLib : LGOB3DS.iFiltroObj : LGOB3DS.iObjT :
018000170509                   ' ' : LGOB3DS.oError : LGOB3DS.oErrorD);
018100170303
018200170303         // se ci sono stati errori, esco
018300170306         if LGOB3DS.oError <> '0';
018400170303           exsr sr_RoutEnd;
018500170303         endif;
018600170303
018700170303         // Reperimento del numero di elementi ?
018800170303         //       e della lunghezza di un elemento?
018900170303         clear Qusec;
019000170303         Qusbprv = %size(Qusec);
019100170303         Rtvus03 = %size(Qush0100);
019200170303         // so che lo US si chiama così perché assume il nome della procedura
019300170303         //         1234567890
019400170303         Crtus01 = 'UBLGOBR   QTEMP';
019500170303         RetrieveUserSpace(CrtUs01: 1: RtvUs03: QUsh0100: Qusec);
019600170303         wAPIName= 'QUSRTVUS';
019700170303         exsr CtrlAPIErr;
019800170303
019900170303         // Se non trovo dati lo segnalo al chiamante
020000170303if  1    if Qusnbrle = 0;
020100170306           LGOB3DS.oError = '1';
020200170306           LGOB3DS.oErrorD = 'ERRORE: con i parametri indicati non sono stati +
020300170303                         trovati oggetti';
020400170303           exsr sr_RoutEnd;
020500170303e   1    endif;
020600170303
020700170303         // Impostazione della posizione iniziale del primo elemento?
020800170303         // e della lunghezza?di ogni elemento
020900170303         Rtvus02 = Qusold + 1;
021000170303         Rtvus03 = Qussee;
021100161103
021200121217       ENDSR;
021300170303
021400170303       //--------------------------------------------------------------
021500170306       // Valorizzazione schiera
021600170303       //--------------------------------------------------------------
021700170303       BEGSR  sr_Valorizza;
021800170303
021900170306         clear LGOB3DS.oObj;
022000170306         LGOB3DS.oError = '0';
022100170303
022200170306         for wX = 1 to %elem(LGOB3DS.oObj);
022300170303
022400170303           WrkCount += 1;
022500170303
022600170303           // se ho superato il numero di elementi all'interno dello US, esco dal ciclo
022700170303           if WrkCount > Qusnbrle;
022800170303
022900170303             // Cancellazione dello user space?
023000170303             clear Qusec;
023100170303             Qusbprv = %size(Qusec);
023200170303             DeleteUserSpace(CrtUs01: Qusec);
023300170303             wAPIName= 'QUSDLTUS';
023400170303             exsr CtrlAPIErr;
023500170303
023600170303             leave;
023700170303           endif;
023800170303
023900170303           // Reperimento dell'elemento?
024000170303           clear Qusec;
024100170303           Qusbprv = %size(Qusec);
024200170303           RetrieveUserSpace(CrtUs01: RtvUs02: RtvUs03: QUSL0600: Qusec);
024300170303           wAPIName= 'QUSRTVUS';
024400170303           exsr CtrlAPIErr;
024500170303
024600170306           LGOB3DS.oObj(wX) = QUSOBJNU04;
024700170303
024800170303           // Impostazione della posizione iniziale del prossimo elemento?
024900170303           Rtvus02 = Rtvus02 + Qussee;
025000170303
025100170303         endfor;
025200170303
025300170303       ENDSR;
025400170303
025500170303       //--------------------------------------------------------------
025600170303       // Controllo se chiamata API andata in errore                  ?
025700170303       //--------------------------------------------------------------
025800170303       begsr  CtrlAPIErr;
025900170303
026000170303         if QUSEI <> *blank;
026100170306           LGOB3DS.oError = '8';
026200170306           LGOB3DS.oErrorD = 'ERRORE: chiamata API ' + %trimr(wAPIName) +
026300170303                        ' con errore ' + QUSEI;
026400170303
026500170303           exsr sr_RoutEnd;
026600170303         endif;
026700170303
026800170303       endsr;
026900121217
027000121217       //--------------------------------------------------------------
027100161104       // Operazioni finali.                                           ?
027200121217       //--------------------------------------------------------------
027300121217       BEGSR  sr_RoutEnd;
027400161103
027500161103         // - Chiusura pgm?
027600121217         return;
027700121217
027800121217       ENDSR;
027900121217

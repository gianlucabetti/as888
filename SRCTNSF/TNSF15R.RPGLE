000100090710       //--------------------------------------------------------------
000200090710       //?TNSF15R?*?STAMPA ETICHETTE FATTURAZIONE AUTOMATICA           ?
000300090710       //?                    CLIENTI CODIFICATI                       ?
000400090710       //--------------------------------------------------------------
000500090710
000600090710     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090710     h dftactgrp(*no) actgrp(*caller)
000800090710
000900090710       //--------------------------------------------------------------
001000090710       //?Dichiarazione file.                                          ?
001100090710       //--------------------------------------------------------------
001200090710
001300090710       // - WorkFile x Etichette Fatture codificati (AS+CLI+n.doc)
001400090710     fETLAV00F  if   e           k disk
001500090710       // - Anagrafica Luoghi/Spedizionieri
001600090710     fFNSPE03L  if   e           k disk
001700090710
001800090710       // - Etichette in stampa
001900090710     fTNSF15P   o    e             printer usropn
002000090710
002100090710       //--------------------------------------------------------------
002200090710       //?Definizione costanti.                                        ?
002300090710       //--------------------------------------------------------------
002400090710
002500090710       // - Campi fissi in chiave di accesso
002600090710     d c_SPEfls        c                   const('L')
002700090710     d c_SPEcod        c                   const('001')
002800090710
002900090710       // - Comandi da eseguire
003000090710     d c_CmdOvrPrtF    c                   const('OVRPRTF +
003100090710     d                                            file(TNSF15P) +
003200090710     d                                            save(*yes) +
003300090710     d                                            outq(FATTMESE) +
003400090710     d                                            ovrscope(*actgrpdfn)')
003500090710     d C_CmdDltOvr     c                   const('DLTOVR +
003600090710     d                                            file(TNSF15P) +
003700090710     d                                            lvl(*actgrpdfn)')
003800090710
003900090710       //--------------------------------------------------------------
004000090710       //?Definizione schiere.                                         ?
004100090710       //--------------------------------------------------------------
004200090710
004300090710
004400090710       //--------------------------------------------------------------
004500090710       //?Definizione aree dati.                                       ?
004600090710       //--------------------------------------------------------------
004700090710
004800090710
004900090710       //--------------------------------------------------------------
005000090710       //?Definizione strutture dati.                                  ?
005100090710       //--------------------------------------------------------------
005200090710
005300090710       // - Parametri ricevuti
005400090710     d KPJBA         e ds
005500090710
005600090710       //--------------------------------------------------------------
005700090710       //?Definizione variabili globali.                               ?
005800090710       //--------------------------------------------------------------
005900090710
006000090710     d WPOfat          s              3    inz
006100090710
006200090710       // - Variabili per il calcolo delle posizioni iniziali in A4
006300090710     d wNumEti         s              5  0 inz
006400090710     d wSeqEti         s              1  0 inz
006500090710     d wEtiSxDx        s              1  0 inz
006600090722
006700090722       // - Variabili per il controllo elle rotture di livello
006800090722     d wSAVasf         s                   like(ETLasf)  inz(*hival)
006900090710
007000090710       // - Campi per QCMDEXC
007100090710     d Qcmd            s            128    inz  Varying
007200090710
007300090710       //--------------------------------------------------------------
007400090710       //?Definizione prototipi procedure.                             ?
007500090710       //--------------------------------------------------------------
007600090710
007700090710       // - Api QCMDEXD (Esecuzione comando di sistema)
007800090710      /copy gaitrasrc/srcProtoPR,QCMDEXC
007900090710
008000090710       //--------------------------------------------------------------
008100090710       //?Definizione key-list.                                        ?
008200090710       //--------------------------------------------------------------
008300090710
008400090710       // - File ETLAV00F
008500090710     d k03etlav00    e ds                  extname(ETLAV00F : *key)
008600090710     d                                     prefix(k_)   inz
008700090710
008800090710       //--------------------------------------------------------------
008900090710       //?M A I N - L I N E                                            ?
009000090710       //--------------------------------------------------------------
009100090710
009200090710     c     *Entry        plist
009300090710     c                   parm                    KPJBA
009400090710
009500090710      /free
009600090710
009700090710       //?Operazioni iniziali?
009800090710       exsr sr_RoutInz;
009900090710
010000090710       //?Ciclo di lettura del file ETLAV00F?
010100090710       DoW  NOT %eof(ETLAV00F);
010200090710
010300090710         //?Aggancio anagrafica LUOGHI:?
010400090710         // - PRIMA con il codice di bollettazione   e
010500090710         chain  (c_SPEfls : ETLbol : c_SPEcod)  FNSPE000;
010600090710         // - SE NON esiste, con quello di fatturazione
010700090710         if  NOT %found(FNSPE03L);
010800090710           chain  (c_SPEfls : ETLfat : c_SPEcod)  FNSPE000;
010900090710         endif;
011000090710
011100090710         //?Stampa singola etichetta?
011200090710         exsr  sr_StampaETI;
011300090710
011400090710         //?Lettura record successivo?
011500090710         read  ETLAV;
011600090710
011700090710       EndDo;
011800090710
011900090710       //?Operazioni finali?
012000090710       exsr sr_RoutEnd;
012100090710
012200090710       //--------------------------------------------------------------
012300090710       //?Operazioni iniziali.                                         ?
012400090710       //--------------------------------------------------------------
012500090710
012600090710       BEGSR  sr_RoutInz;
012700090710
012800090710         *inLR = *on;
012900090710
013000090710         //?Esecuzione override al file di stampa?
013100090710         ExecuteCommand ( c_CmdOvrPrtF : %len(c_CmdOvrPrtF) );
013200090710
013300090710         //?Apertura file di stampa?
013400090710         open  TNSF15P;
013500090710
013600090710         //?1ª lettura dal file ETLAV00F?
013700090710         setll *loval ETLAV;
013800090710         read  ETLAV;
013900090710
014000090710       ENDSR;
014100090710
014200090710       //--------------------------------------------------------------
014300090710       //?Stampa singola etichetta                                     ?
014400090710       //--------------------------------------------------------------
014500090710
014600090710       BEGSR  sr_StampaETI;
014700090710
014800090710         //?Posizionamento all'interno del foglio A4?
014900090710         exsr  sr_PosizETI;
015000090710
015100090710         //?Valorizzazione campi della singola etichetta?
015200090710         exsr  sr_ValorETI;
015300090710
015400090710         //?Stampa della singola etichetta?
015500090710         //  ?(se riempita la 2ª etichetta in sequenza - quella a dx)?
015600090710         if  wEtiSxDx = 2;
015700090710           write  SF15eti2;
015800090710           clear  SF15eti2;
015900090710           clear  wEtiSxDx;            // Da fare qui (vedi sr_RoutEnd)
016000090710         endif;
016100090710
016200090710       ENDSR;
016300090710
016400090710       //--------------------------------------------------------------
016500090710       //?Posizionamento iniziale all'interno del foglio A4            ?
016600090710       //--------------------------------------------------------------
016700090710
016800090710       BEGSR  sr_PosizETI;
016900090722
017000090722         //?Test di salto pagina (a rottura di filiale)?
017100090722         if  ETLasf <> wSAVasf;
017200090722           // Eventuale stampa dell'ultima etichetta (singola)
017300090722           if  wEtiSxDx > *zero;
017400090722             write  SF15eti2;
017500090722             clear  SF15eti2;
017600090722             clear  wEtiSxDx;
017700090722           endif;
017800090722           wSAVasf = ETLasf;
017900090722           clear  wNumEti;
018000090722         endif;
018100090710
018200090710         //?Incremento il n° di etichette in stampa?
018300090710         wNumEti += 1;
018400090710
018500090710         //?Calcolo della posizione orizzontale x etichetta all'interno?
018600090710         //  ?della pagina "A4"?
018700090710         wEtiSxDx = %rem( wNumEti : 2 );
018800090710         if  wEtiSxDx <> *zero;
018900090710           wEtiSxDx = 1;               // <= etichetta a sinistra
019000090710         else;
019100090710           wEtiSxDx = 2;               // => etichetta a destra
019200090710         endif;
019300090710
019400090710         //?Calcolo della posizione verticale   x etichetta all'interno?
019500090710         //  ?della pagina "A4"?
019600090710         wSeqEti  = %rem( wNumEti : 8 );
019700090710         if  wSeqEti = *zero;
019800090710           wSeqEti  = 8;               // ultima etichetta in A4
019900090710         endif;
020000090710
020100090710         //?Posizionamento nel foglio A4 in base al n° etichetta in?
020200090710         //  ?stampa?
020300090710         if  wEtiSxDx = 1;             // <= etichetta a sinistra
020400090710           *in41 = (wSeqEti = 1);
020500090710           *in42 = (wSeqEti = 3);
020600090710           *in43 = (wSeqEti = 5);
020700090710           *in44 = (wSeqEti = 7);
020800090710           WRITE  SF15pos;
020900090710         endif;
021000090710
021100090710       ENDSR;
021200090710
021300090710       //--------------------------------------------------------------
021400090710       //?Valorizzazione dati di output nella singola etichetta        ?
021500090710       //--------------------------------------------------------------
021600090710
021700090710       BEGSR  sr_ValorETI;
021800090710
021900090710         If  wEtiSxDx = 1;             // <= Etichetta a sinistra
022000090710
022100090710           etiR1S = 'Cliente ' + %editc( ETLbol : 'X' )
022200090710                  + '   '
022300090710                  + 'N.Fattura ' + %editc( ETLndc : 'Z' )
022400090710                  + '     '
022500090710                  + '(' + %editc( ETLasf : 'X' ) + ')';
022600090710           etiR2S = SPErag;
022700090710           etiR3S = SPEind;
022800090710           etiR4S = SPEcap + SPEloc + ' ' + SPEpro;
022900090710           if  SPEper <> *blank;
023000090710             etiR5S = 'Att.ne   ' + SPEper;
023100090710           endif;
023200090710
023300090710         Else;                         // => Etichetta a destra
023400090710
023500090710           etiR1D = 'Cliente ' + %editc( ETLbol : 'X' )
023600090710                  + '   '
023700090710                  + 'N.Fattura ' + %editc( ETLndc : 'Z' )
023800090710                  + '     '
023900090710                  + '(' + %editc( ETLasf : 'X' ) + ')';
024000090710           etiR2D = SPErag;
024100090710           etiR3D = SPEind;
024200090710           etiR4D = SPEcap + SPEloc + ' ' + SPEpro;
024300090710           if  SPEper <> *blank;
024400090710             etiR5D = 'Att.ne   ' + SPEper;
024500090710           endif;
024600090710
024700090710         EndIf;
024800090710
024900090710       ENDSR;
025000090710
025100090710       //--------------------------------------------------------------
025200090710       //?Operazioni finali.                                           ?
025300090710       //--------------------------------------------------------------
025400090710
025500090710       BEGSR  sr_RoutEnd;
025600090710
025700090710         //?Eventuale stampa dell'ultima etichetta (singola)?
025800090710         if  wEtiSxDx > *zero;
025900090710           write  SF15eti2;
026000090710         endif;
026100090710
026200090710         //?Chiusura file di stampa?
026300090710         close  TNSF15P;
026400090710
026500090710         //?Cancellazione override al file di stampa?
026600090710         callp(e) ExecuteCommand ( c_CmdDltOvr : %size(c_CmdDltOvr ) );
026700090710
026800090710       ENDSR;
026900090710
027000090710      /end-free

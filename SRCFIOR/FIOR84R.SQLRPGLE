000100140516      //--------------------------------------------------------------
000200140516      //?FIOR84R - Elenco Clienti con ORM commissionati
000300140516      //--------------------------------------------------------------
000400140516
000500140516     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600140516     h dftactgrp(*no) actgrp(*caller) bnddir('TRUL')
000700140516
000800140516      //---------------------------------------------------------------
000900140516      //?Dichiarazione file.
001000140516      //---------------------------------------------------------------
001100140516     fFIOR84P   o    e             printer infds(prtinfo)
001200140516     f                                     oflind(*in99)
001300140516
001400140516      //---------------------------------------------------------------
001500140516      //?Definizione costanti.
001600140516      //---------------------------------------------------------------
001700140516
001800140516      //---------------------------------------------------------------
001900140516      //?Definizione schiere.
002000140516      //---------------------------------------------------------------
002100140516
002200140516      //---------------------------------------------------------------
002300140516      //?Definizione aree dati.
002400140516      //---------------------------------------------------------------
002500140516      // - Dati utente
002600140516     d §AzUte        e ds                  extname(AZUTE00F)
002700140516     d                                     dtaara
002800140516     d §DatiUte      e ds                  extname(dDatiUte)
002900140516     d                                     dtaara
003000140516
003100140516      //---------------------------------------------------------------
003200140516      //?Definizione strutture dati.
003300140516      //---------------------------------------------------------------
003400140516     d PrtINFO         ds
003500140516     d  CurLine              367    368i 0
003600140516
003700140516      // - Status
003800140516     d Psds           sds
003900140516     d   SDSpgm          *proc
004000140516
004100140516      // - Parametri ricevuti
004200140516     d KPJBA         e ds
004300140516     d FIOR84DS      e ds                  qualified
004400140516
004500140516      // - Reperimento dati utente
004600140516     d TIBS34DS      e ds                  qualified
004700140516
004800140516      // File di work
004900140516     d WFORC00F      e ds                  extname(WFORC00F)
005000140516
005100140516      //---------------------------------------------------------------
005200140516      //?Definizione variabili globali.
005300140516      //---------------------------------------------------------------
005400140516      // - Flags booleani
005500140516     d wEnd            s               n   inz(*off)
005600140516
005700140516       // - Stringa SQL da eseguire
005800140516     d wSQL            s           4096    Varying        inz
005900140516
006000140516      // - Campi di comodo data
006100140516     d wData_EUR       s               d   datfmt(*eur)
006200140516     d wData_ISO       s               d   datfmt(*iso)
006300140516     d wOggi           s              8  0 inz
006400140516
006500140516      // - Campi di comodo
006600140718     d count           s             10i 0 inz
006700140516     d sav_ORCcmm      s                   inz like(ORCcmm)
006800140516
006900140516      //---------------------------------------------------------------
007000140516      //?Definizione procedure esterne.
007100140516      //---------------------------------------------------------------
007200140516
007300140516      //---------------------------------------------------------------
007400140516      //?Definizione prototipi.
007500140516      //---------------------------------------------------------------
007600140516      /copy gaitrasrc/srcprotopr,tibs34r
007700140516
007800140516      //---------------------------------------------------------------
007900140516      //?Definizione key-list.
008000140516      //---------------------------------------------------------------
008100140516
008200140516      //---------------------------------------------------------------
008300140516      //?M A I N - L I N E
008400140516      //---------------------------------------------------------------
008500140516
008600140516     c     *entry        plist
008700140516     c                   parm                    KPJBA
008800140516
008900140516      /free
009000140516
009100140516       //?Operazioni iniziali
009200140516       exsr RoutInz;
009300140516
009400140516       //?Stampo
009500140516       exsr Stampa;
009600140516
009700140516       //?Operazioni finali
009800140516       exsr RoutEnd;
009900140516
010000140516       //--------------------------------------------------------------
010100140516       //?Operazioni iniziali.
010200140516       //--------------------------------------------------------------
010300140516       BEGSR RoutInz;
010400140516
010500140516       //?Imposto la ds con i dati della KPJBU
010600140516         FIOR84DS = kpjbu;
010700140516
010800140516       //?Reperimento dati job
010900140516         exsr DatiJob;
011000140516
011100140516         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011200140516
011300140516       //?Imposto nome programma
011400140516         NomePgm = SDSpgm;
011500140516
011600140516       //?Imposto data e ora sistema
011700140516         wOggi     = %dec(%date());
011800140516         wData_ISO = %date(wOggi:*ISO);
011900140516         wData_EUR = wData_ISO;
012000140516         DataEla   = %dec(wData_EUR);
012100140516         OraEla    = %dec(%time());
012200140516
012300140516       //?Imposto date di lancio
012400140516         wData_ISO = %date(fior84ds.OR84dtad:*ISO);
012500140516         wData_EUR = wData_ISO;
012600140516         datadal   = %dec(wData_EUR);
012700140516         wData_ISO = %date(fior84ds.OR84dtaa:*ISO);
012800140516         wData_EUR = wData_ISO;
012900140516         dataal    = %dec(wData_EUR);
013000140516
013100140516       ENDSR;
013200140516
013300140516       //--------------------------------------------------------------
013400140516       //?Reperimento Dati del job (Utente/Operativi).
013500140516       //--------------------------------------------------------------
013600140516       BEGSR DatiJob;
013700140516
013800140516         in(E) §AzUte;
013900140516         IF  not %error;
014000140516           in(E) §DatiUte;
014100140516         ENDIF;
014200140516         IF  %error or RSut = *blanks;
014300140516           clear TIBS34ds;
014400140516           tibs34r(tibs34ds);
014500140516           in §AzUte;
014600140516           in §DatiUte;
014700140516         ENDIF;
014800140516
014900140516       ENDSR;
015000140516
015100140516       //--------------------------------------------------------------
015200140516       //?Leggo file di WORK
015300140516       //--------------------------------------------------------------
015400140516       BEGSR Stampa;
015500140718
015600140718       //?Verifico che il file di work non sia vuoto
015700140718         exec sql
015800140718         select count(*)
015900140718         into :count
016000140718         from WFORC00F;
016100140718       //?se vuoto stampo errore e vado via
016200140718         IF count = 0;
016300140718           write OR84T1;
016400140718           write OR84R1;
016500140718           write OR84T3;
016600140718           write OR84R2;
016700140718           write OR84ERR;
016800140718           leavesr;
016900140718         ENDIF;
017000140516
017100140516         wEnd = *off;
017200140516         *in40 = (fior84ds.OR84poe = 999);
017300140516
017400140516         exec sql
017500140516         declare WFORC cursor for
017600140516         select * from WFORC00F
017700140516         order by ORCcmm, ORCcom desc, ORCrso;
017800140516
017900140516         exec sql
018000140516         open WFORC;
018100140516         IF sqlcode < 0;
018200140516           wEnd = *on;
018300140516         ENDIF;
018400140516
018500140516         DOW not wEnd;
018600140516           exec sql
018700140516           fetch next from WFORC into :WFORC00F;
018800140516           IF sqlcod = 100 or sqlcod < 0;
018900140516             wEnd = *on;
019000140516             leave;
019100140516           ENDIF;
019200140516
019300140516       //?Rottura per Commerciale
019400140516           IF  ORCcmm <> sav_ORCcmm;
019500140516         //?Stampo Totali per commerciale
019600140516             IF  sav_ORCcmm > 0;
019700140519               write OR84R2;
019800140516               write OR84C1;
019900140519               write OR84R1;
020000140516           //?sommo totali generali
020100140516               nrgcom   += nrccom;
020200140516               nrgvas   += nrcvas;
020300140516               nrgvdati += nrcvdati;
020400140516               nrgvmail += nrcvmail;
020500140516               nrgvsms  += nrcvsms;
020600140516               nrgman   += nrcman;
020700140516               nrgmdati += nrcmdati;
020800140516               nrgmmail += nrcmmail;
020900140516               nrgmsms  += nrcmsms;
021000140516               clear nrccom;
021100140516               clear nrcvas;
021200140516               clear nrcvdati;
021300140516               clear nrcvmail;
021400140516               clear nrcvsms;
021500140516               clear nrcman;
021600140516               clear nrcmdati;
021700140516               clear nrcmmail;
021800140516               clear nrcmsms;
021900140516             ENDIF;
022000140516         //?Testata
022100140516             write OR84T1;
022200140516             *in90 = *off;
022300140519         //?Dati di Lancio
022400140516             write OR84T2;
022500140516             sav_ORCcmm = ORCcmm;
022600140519         //?Riga tratteggio
022700140519             write OR84R1;
022800140519         //?Intestazione
022900140519             write OR84T3;
023000140519         //?Riga tratteggio
023100140519             write OR84R2;
023200140516           ENDIF;
023300140519
023400140519         //?Salto pagina
023500140519           IF  CurLine >= 60;
023600140519             write OR84T1;
023700140519             write OR84T2;
023800140519             write OR84R1;
023900140519             write OR84T3;
024000140519             write OR84R2;
024100140519             *in90 = *off;
024200140519           ENDIF;
024300140516
024400140519         //?Dettaglio
024500140516           write  OR84D1;
024600140519
024700140516         //?sommo totali per commerciale
024800140516           nrccom   += ORCcom;
024900140516           nrcvas   += ORCvas;
025000140516           nrcvdati += ORCvdati;
025100140516           nrcvmail += ORCvmail;
025200140516           nrcvsms  += ORCvsms;
025300140516           nrcman   += ORCman;
025400140516           nrcmdati += ORCmdati;
025500140516           nrcmmail += ORCmmail;
025600140516           nrcmsms  += ORCmsms;
025700140516
025800140516         ENDDO;
025900140516
026000140516         exec sql close WFORC;
026100140516
026200140519       //?Stampo totale ultimo commerciale
026300140519         write OR84R2;
026400140516         write OR84C1;
026500140519         write OR84R1;
026600140516         nrgcom   += nrccom;
026700140516         nrgvas   += nrcvas;
026800140516         nrgvdati += nrcvdati;
026900140516         nrgvmail += nrcvmail;
027000140516         nrgvsms  += nrcvsms;
027100140516         nrgman   += nrcman;
027200140516         nrgmdati += nrcmdati;
027300140516         nrgmmail += nrcmmail;
027400140516         nrgmsms  += nrcmsms;
027500140516
027600140519       //?Stampo totale Generale
027700140519         *in41 = *on;
027800140516         write OR84T1;
027900140519         write OR84T2;
028000140519         write OR84R1;
028100140516         write OR84T3;
028200140519         write OR84R2;
028300140516         write OR84G1;
028400140519         write OR84R1;
028500140718
028600140718       //?Stampo la fine
028700140718       write OR84E1;
028800140516
028900140516       ENDSR;
029000140516
029100140516       //--------------------------------------------------------------
029200140516       //?Operazioni finali.
029300140516       //--------------------------------------------------------------
029400140516       BEGSR RoutEnd;
029500140516
029600140516         *inLR = *on;
029700140516         return;
029800140516
029900140516       ENDSR;
030000100714
030100140516      /end-free

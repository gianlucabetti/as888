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
006600140516     d sav_ORCcmm      s                   inz like(ORCcmm)
006700140516
006800140516      //---------------------------------------------------------------
006900140516      //?Definizione procedure esterne.
007000140516      //---------------------------------------------------------------
007100140516
007200140516      //---------------------------------------------------------------
007300140516      //?Definizione prototipi.
007400140516      //---------------------------------------------------------------
007500140516      /copy gaitrasrc/srcprotopr,tibs34r
007600140516
007700140516      //---------------------------------------------------------------
007800140516      //?Definizione key-list.
007900140516      //---------------------------------------------------------------
008000140516
008100140516      //---------------------------------------------------------------
008200140516      //?M A I N - L I N E
008300140516      //---------------------------------------------------------------
008400140516
008500140516     c     *entry        plist
008600140516     c                   parm                    KPJBA
008700140516
008800140516      /free
008900140516
009000140516       //?Operazioni iniziali
009100140516       exsr RoutInz;
009200140516
009300140516       //?Stampo
009400140516       exsr Stampa;
009500140516
009600140516       //?Stampo la fine
009700140516       write OR84E1;
009800140516
009900140516       //?Operazioni finali
010000140516       exsr RoutEnd;
010100140516
010200140516       //--------------------------------------------------------------
010300140516       //?Operazioni iniziali.
010400140516       //--------------------------------------------------------------
010500140516       BEGSR RoutInz;
010600140516
010700140516       //?Imposto la ds con i dati della KPJBU
010800140516         FIOR84DS = kpjbu;
010900140516
011000140516       //?Reperimento dati job
011100140516         exsr DatiJob;
011200140516
011300140516         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011400140516
011500140516       //?Imposto nome programma
011600140516         NomePgm = SDSpgm;
011700140516
011800140516       //?Imposto data e ora sistema
011900140516         wOggi     = %dec(%date());
012000140516         wData_ISO = %date(wOggi:*ISO);
012100140516         wData_EUR = wData_ISO;
012200140516         DataEla   = %dec(wData_EUR);
012300140516         OraEla    = %dec(%time());
012400140516
012500140516       //?Imposto date di lancio
012600140516         wData_ISO = %date(fior84ds.OR84dtad:*ISO);
012700140516         wData_EUR = wData_ISO;
012800140516         datadal   = %dec(wData_EUR);
012900140516         wData_ISO = %date(fior84ds.OR84dtaa:*ISO);
013000140516         wData_EUR = wData_ISO;
013100140516         dataal    = %dec(wData_EUR);
013200140516
013300140516       ENDSR;
013400140516
013500140516       //--------------------------------------------------------------
013600140516       //?Reperimento Dati del job (Utente/Operativi).
013700140516       //--------------------------------------------------------------
013800140516       BEGSR DatiJob;
013900140516
014000140516         in(E) §AzUte;
014100140516         IF  not %error;
014200140516           in(E) §DatiUte;
014300140516         ENDIF;
014400140516         IF  %error or RSut = *blanks;
014500140516           clear TIBS34ds;
014600140516           tibs34r(tibs34ds);
014700140516           in §AzUte;
014800140516           in §DatiUte;
014900140516         ENDIF;
015000140516
015100140516       ENDSR;
015200140516
015300140516       //--------------------------------------------------------------
015400140516       //?Leggo file di WORK
015500140516       //--------------------------------------------------------------
015600140516       BEGSR Stampa;
015700140516
015800140516         wEnd = *off;
015900140516         *in40 = (fior84ds.OR84poe = 999);
016000140516
016100140516         exec sql
016200140516         declare WFORC cursor for
016300140516         select * from WFORC00F
016400140516         order by ORCcmm, ORCcom desc, ORCrso;
016500140516
016600140516         exec sql
016700140516         open WFORC;
016800140516         IF sqlcode < 0;
016900140516           wEnd = *on;
017000140516         ENDIF;
017100140516
017200140516         DOW not wEnd;
017300140516           exec sql
017400140516           fetch next from WFORC into :WFORC00F;
017500140516           IF sqlcod = 100 or sqlcod < 0;
017600140516             wEnd = *on;
017700140516             leave;
017800140516           ENDIF;
017900140516
018000140516       //?Rottura per Commerciale
018100140516           IF  ORCcmm <> sav_ORCcmm;
018200140516         //?Stampo Totali per commerciale
018300140516             IF  sav_ORCcmm > 0;
018400140519               write OR84R2;
018500140516               write OR84C1;
018600140519               write OR84R1;
018700140516           //?sommo totali generali
018800140516               nrgcom   += nrccom;
018900140516               nrgvas   += nrcvas;
019000140516               nrgvdati += nrcvdati;
019100140516               nrgvmail += nrcvmail;
019200140516               nrgvsms  += nrcvsms;
019300140516               nrgman   += nrcman;
019400140516               nrgmdati += nrcmdati;
019500140516               nrgmmail += nrcmmail;
019600140516               nrgmsms  += nrcmsms;
019700140516               clear nrccom;
019800140516               clear nrcvas;
019900140516               clear nrcvdati;
020000140516               clear nrcvmail;
020100140516               clear nrcvsms;
020200140516               clear nrcman;
020300140516               clear nrcmdati;
020400140516               clear nrcmmail;
020500140516               clear nrcmsms;
020600140516             ENDIF;
020700140516         //?Testata
020800140516             write OR84T1;
020900140516             *in90 = *off;
021000140519         //?Dati di Lancio
021100140516             write OR84T2;
021200140516             sav_ORCcmm = ORCcmm;
021300140519         //?Riga tratteggio
021400140519             write OR84R1;
021500140519         //?Intestazione
021600140519             write OR84T3;
021700140519         //?Riga tratteggio
021800140519             write OR84R2;
021900140516           ENDIF;
022000140519
022100140519         //?Salto pagina
022200140519           IF  CurLine >= 60;
022300140519             write OR84T1;
022400140519             write OR84T2;
022500140519             write OR84R1;
022600140519             write OR84T3;
022700140519             write OR84R2;
022800140519             *in90 = *off;
022900140519           ENDIF;
023000140516
023100140519         //?Dettaglio
023200140516           write  OR84D1;
023300140519
023400140516         //?sommo totali per commerciale
023500140516           nrccom   += ORCcom;
023600140516           nrcvas   += ORCvas;
023700140516           nrcvdati += ORCvdati;
023800140516           nrcvmail += ORCvmail;
023900140516           nrcvsms  += ORCvsms;
024000140516           nrcman   += ORCman;
024100140516           nrcmdati += ORCmdati;
024200140516           nrcmmail += ORCmmail;
024300140516           nrcmsms  += ORCmsms;
024400140516
024500140516         ENDDO;
024600140516
024700140516         exec sql close WFORC;
024800140516
024900140519       //?Stampo totale ultimo commerciale
025000140519         write OR84R2;
025100140516         write OR84C1;
025200140519         write OR84R1;
025300140516         nrgcom   += nrccom;
025400140516         nrgvas   += nrcvas;
025500140516         nrgvdati += nrcvdati;
025600140516         nrgvmail += nrcvmail;
025700140516         nrgvsms  += nrcvsms;
025800140516         nrgman   += nrcman;
025900140516         nrgmdati += nrcmdati;
026000140516         nrgmmail += nrcmmail;
026100140516         nrgmsms  += nrcmsms;
026200140516
026300140519       //?Stampo totale Generale
026400140519         *in41 = *on;
026500140516         write OR84T1;
026600140519         write OR84T2;
026700140519         write OR84R1;
026800140516         write OR84T3;
026900140519         write OR84R2;
027000140516         write OR84G1;
027100140519         write OR84R1;
027200140516
027300140516       ENDSR;
027400140516
027500140516       //--------------------------------------------------------------
027600140516       //?Operazioni finali.
027700140516       //--------------------------------------------------------------
027800140516       BEGSR RoutEnd;
027900140516
028000140516         *inLR = *on;
028100140516         return;
028200140516
028300140516       ENDSR;
028400100714
028500140516      /end-free

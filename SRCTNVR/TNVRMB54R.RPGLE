000100170125      //--------------------------------------------------------------
000200170125      //?TNVRMB54R - Conteggio Tariffe Particolari DPD "*" e "F"
000300170125      //--------------------------------------------------------------
000400170125     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170125
000600170125      //---------------------------------------------------------------
000700170125      //?Dichiarazione file.
000800170125      //---------------------------------------------------------------
000900170125     fTNTAM04L  if   e           k disk
001000170125     fTITPT01L  if   e           k disk
001100170125     fTITPD01L  if   e           k disk
001200170125     fQSYSPRT   o    f  132        printer oflind(*inof)
001300170125
001400170125      //---------------------------------------------------------------
001500170125      //?Definizione costanti.
001600170125      //---------------------------------------------------------------
001700170125     d Dirottamento    c                   const('* ')
001800170125     d FuoriMisura     c                   const('F ')
001900170125
002000170125      //---------------------------------------------------------------
002100170125      //?Definizione schiere.
002200170125      //---------------------------------------------------------------
002300170125
002400170125      //---------------------------------------------------------------
002500170125      //?Definizione aree dati.
002600170125      //---------------------------------------------------------------
002700170125      // - Dati utente
002800170125     d §AzUte        e ds                  extname(AZUTE00F)
002900170125     d                                     dtaara
003000170125     d §DatiUte      e ds                  extname(dDatiUte)
003100170125     d                                     dtaara
003200170125
003300170125      //---------------------------------------------------------------
003400170125      //?Definizione strutture dati.
003500170125      //---------------------------------------------------------------
003600170125      // - Parametri ricevuti
003700170125     d KPJBA         e ds
003800170125
003900170125      // - Reperimento dati utente
004000170125     d TIBS34DS      e ds
004100170125
004200170125      // - Campo TAMFLO
004300170125     d dsTA01        e ds
004400170125
004500170125      //---------------------------------------------------------------
004600170125      //?Definizione variabili globali.
004700170125      //---------------------------------------------------------------
004800170125      // - Flags booleani
004900170125     d Dettaglio       s               n   inz
005000170125     d wEoF            s               n   inz
005100170125
005200170125      // - Campi di comodo
005300170125     d sav_TAMctr      s                   like(TAMctr) inz
005400170125     d sav_TAMksc      s                   like(TAMksc) inz
005500170125     d tNoDT           s              7s 0 inz
005600170125     d tNoDD           s              7s 0 inz
005700170125     d tDT             s              7s 0 inz
005800170125     d tDD             s              7s 0 inz
005900170125     d tDD1            s              7s 0 inz
006000170125     d tDD2            s              7s 0 inz
006100170125     d tNoFMT          s              7s 0 inz
006200170125     d tNoFMD          s              7s 0 inz
006300170125     d tFMT            s              7s 0 inz
006400170125     d tFMD            s              7s 0 inz
006500170125     d tFMD1           s              7s 0 inz
006600170125     d tFMD2           s              7s 0 inz
006700170125
006800170125      //---------------------------------------------------------------
006900170125      //?Definizione Procedure usate.
007000170125      //---------------------------------------------------------------
007100170125
007200170125      //---------------------------------------------------------------
007300170125      //?Definizione Prototipi.
007400170125      //---------------------------------------------------------------
007500170125      /copy gaitrasrc/srcprotopr,TIBS34R
007600170125
007700170125      //---------------------------------------------------------------
007800170125      //?Definizione key-list.
007900170125      //---------------------------------------------------------------
008000170125       // - File TNTAM04L
008100170125     d k03tntam      e ds                  extname(TNTAM04L:*key)
008200170125     d                                     prefix(k_)
008300170125
008400170125      //---------------------------------------------------------------
008500170125      //?M A I N - L I N E
008600170125      //---------------------------------------------------------------
008700170125     c     *Entry        plist
008800170125     c                   parm                    kpjba
008900170125
009000170125      /free
009100170125
009200170125       //?Operazioni iniziali
009300170125       exsr RoutInz;
009400170125
009500170125       //?Elaborazione
009600170125       exsr Elabora;
009700170125
009800170125       //?Stampa
009900170125       exsr Stampa;
010000170125
010100170125       //?Operazioni finali
010200170125       exsr RoutEnd;
010300170125
010400170125       //--------------------------------------------------------------
010500170125       //?Operazioni iniziali.
010600170125       //--------------------------------------------------------------
010700170125       BEGSR RoutInz;
010800170125
010900170125       //?Reperimento dati job
011000170125         exsr DatiJob;
011100170125
011200170125       ENDSR;
011300170125
011400170125       //--------------------------------------------------------------
011500170125       //?Reperimento Dati del job (Utente/Operativi).
011600170125       //--------------------------------------------------------------
011700170125       BEGSR DatiJob;
011800170125
011900170125         in(E) §AzUte;
012000170125         IF  not %error;
012100170125           in(E) §DatiUte;
012200170125         ENDIF;
012300170125         IF  %error or RSut = *blanks;
012400170125           clear TIBS34ds;
012500170125           tibs34r(tibs34ds);
012600170125           in §AzUte;
012700170125           in §DatiUte;
012800170125         ENDIF;
012900170125
013000170125       ENDSR;
013100170125
013200170125       //--------------------------------------------------------------
013300170125       //?Elaborazione.
013400170125       //--------------------------------------------------------------
013500170125       BEGSR Elabora;
013600170125
013700170125       //?Leggo TNTAM04L
013800170125       //?solo tariffe DPD
013900170125       //?no Cartello
014000170125         k_TAMksc = 1;
014100170125         setll k_TAMksc TNTAM04L;
014200170125         read TNTAM04L;
014300170125         DOW  not %eof(TNTAM04L);
014400170125
014500170125           dsTA01 = TAMflo;
014600170125
014700170125         //?Se KSC > di 8000000 è tariffa di cartello e quindi sono alla fine del file
014800170125           IF  TAMksc > 8000000;
014900170125             leavesr;
015000170125           ENDIF;
015100170125
015200170125         //?Leggo solo l'ultima tariffa valida
015300170125         //?(in questo caso la prima che trovo x cliente/tariffa)
015400170125           IF  TAMksc <> sav_TAMksc or TAMctr <> sav_TAMctr;
015500170125
015600170125           //?Se Tariffa DPD conto
015700170125             IF  §TAdpd = 'S';
015800170125               exsr ContaDirottamento;
015900170125               exsr ContaFuoriMisura;
016000170125             ENDIF;
016100170125
016200170125             sav_TAMksc = TAMksc;
016300170125             sav_TAMctr = TAMctr;
016400170125           ENDIF;
016500170125
016600170125           read TNTAM04L;
016700170125
016800170125         ENDDO;
016900170125
017000170125       ENDSR;
017100170125
017200170125       //--------------------------------------------------------------
017300170125       //?Conto tariffa particolare Dirottamento.
017400170125       //--------------------------------------------------------------
017500170125       BEGSR ContaDirottamento;
017600170125
017700170125       //?SOMMO se non trovo Testata Tariffa Particolare oppure è annullata
017800170125         chain (TAMksc:TAMctr:TAMprg:Dirottamento) TITPT01L;
017900170125         IF  not %found(TITPT01L) or TPTatb <> *blanks;
018000170125           tNoDT += 1;
018100170125           leavesr;
018200170125         ENDIF;
018300170125         tDT += 1;
018400170125
018500170125       //?Vado sul dettaglio della Tariffa Particolare
018600170125         Dettaglio = *off;
018700170125         wEoF = *off;
018800170125         setll (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
018900170125         reade (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
019000170125         DOW  not wEoF;
019100170125           IF  %eof(TITPD01L);
019200170125             wEoF = *on;
019300170125             leave;
019400170125           ENDIF;
019500170125           IF  TPDatb = *blanks and TPDitr > 0;
019600170125             Dettaglio = *on;
019700170125             IF  TPDitr > 0;
019800170125               tDD += 1;
019900170125             ENDIF;
020000170125             IF  TPDitr > 0 and TPDitr < 6;
020100170125               tDD1 += 1;
020200170125             ENDIF;
020300170125             IF  TPDitr >= 6;
020400170125               tDD2 += 1;
020500170125             ENDIF;
020600170127             wEoF = *on;
020700170127             leave;
020800170125           ENDIF;
020900170125           reade (TAMksc:TAMctr:TAMprg:Dirottamento) TITPD01L;
021000170125         ENDDO;
021100170125         IF  not Dettaglio;
021200170125           tNoDD += 1;
021300170125           leavesr;
021400170125         ENDIF;
021500170125
021600170125       ENDSR;
021700170125
021800170125       //--------------------------------------------------------------
021900170125       //?Conto tariffa particolare Fuori Misura.
022000170125       //--------------------------------------------------------------
022100170125       BEGSR ContaFuoriMisura;
022200170125
022300170125       //?SOMMO se non trovo Testata Tariffa Particolare oppure è annullata
022400170125         chain (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPT01L;
022500170125         IF  not %found(TITPT01L) or TPTatb <> *blanks;
022600170125           tNoFMT += 1;
022700170125           leavesr;
022800170125         ENDIF;
022900170125         tFMT += 1;
023000170125
023100170125       //?Vado sul dettaglio della Tariffa Particolare
023200170125         Dettaglio = *off;
023300170125         wEoF = *off;
023400170125         setll (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
023500170125         reade (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
023600170125         DOW  not wEoF;
023700170125           IF  %eof(TITPD01L);
023800170125             wEoF = *on;
023900170125             leave;
024000170125           ENDIF;
024100170125           IF  TPDatb = *blanks and TPDitr > 0;
024200170125             Dettaglio = *on;
024300170125             IF  TPDitr > 0;
024400170125               tFMD += 1;
024500170125             ENDIF;
024600170127             IF  TPDitr > 0 and TPDitr < 20;
024700170125               tFMD1 += 1;
024800170125             ENDIF;
024900170127             IF  TPDitr >= 20;
025000170125               tFMD2 += 1;
025100170125             ENDIF;
025200170127             wEoF = *on;
025300170127             leave;
025400170125           ENDIF;
025500170125           reade (TAMksc:TAMctr:TAMprg:FuoriMisura) TITPD01L;
025600170125         ENDDO;
025700170125         IF  not Dettaglio;
025800170125           tNoFMD += 1;
025900170125           leavesr;
026000170125         ENDIF;
026100170125
026200170125       ENDSR;
026300170125
026400170125       //--------------------------------------------------------------
026500170125       //?Stampa.
026600170125       //--------------------------------------------------------------
026700170125       BEGSR Stampa;
026800170125
026900170125          except testa;
027000170125
027100170125       ENDSR;
027200170125
027300170125       //--------------------------------------------------------------
027400170125       //?Operazioni finali.
027500170125       //--------------------------------------------------------------
027600170125       BEGSR RoutEnd;
027700170125
027800170125         *inLR = *on;
027900170125         return;
028000170125
028100170125       ENDSR;
028200170125
028300170125      /end-free
028400170125
028500170125     oQSYSPRT   e            testa          2 02
028600170125     o                                           20 'BRT S.p.A.'
028700170125     o                                           60 '**  Conteggio Varie DPD **'
028800170125     o                       udate              100 '  /  /  '
028900170125     o                                          120 'TNVRMB54R'
029000170125     o                                          128 'Pag.'
029100170125     o                       page          Z    132
029200170125     o          e            testa       2
029300170125     o                                           40 'D I R O T T A M E N T O'
029400170125     o          e            testa       0  2
029500170125     o                                           40 'D I R O T T A M E N T O'
029600170125     o          e            testa          1
029700170125     o                                           10 ' Cartello '
029800170125     o                                           21 '   Negata '
029900170125     o                                           32 '  Dett. >0'
030000170125     o                                           48 'Dett. >0 <6'
030100170125     o                                           64 ' Dett. >=6'
030200170125     o          e            testa          1
030300170125     o                       tNoDT         2      9
030400170125     o                       tNoDD         2     20
030500170125     o                       tDD           2     31
030600170125     o                       tDD1          2     46
030700170125     o                       tDD2          2     62
030800170125     o          e            testa       2
030900170125     o                                           40 'F U O R I   M I S U R A'
031000170125     o          e            testa       0  2
031100170125     o                                           40 'F U O R I   M I S U R A'
031200170125     o          e            testa          1
031300170125     o                                           10 ' Cartello '
031400170125     o                                           21 '   Negata '
031500170125     o                                           32 '  Dett. >0'
031600170127     o                                           50 'Dett. >0 <20'
031700170127     o                                           66 'Dett. >=20'
031800170125     o          e            testa          1
031900170125     o                       tNoFMT        2      9
032000170125     o                       tNoFMD        2     20
032100170125     o                       tFMD          2     31
032200170125     o                       tFMD1         2     46
032300170125     o                       tFMD2         2     62

000100141210      //--------------------------------------------------------------
000200150331      //?AUTA04R - Aggiorna WFAUC00F con dati TNTAM
000300141210      //--------------------------------------------------------------
000400141210
000500141210     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141210     h dftactgrp(*no) actgrp(*caller)
000700141210
000800141210      //---------------------------------------------------------------
000900141210      //?Dichiarazione file.
001000141210      //---------------------------------------------------------------
001100070712
001200141210     fTNTAM04L  if   e           k disk
001300161122     fWFAUC00F  uf a e             disk    usropn extfile('EDPAUMENTI/WFAUC00F')
001400141210
001500141210      //---------------------------------------------------------------
001600141210      //?Definizione costanti.
001700141210      //---------------------------------------------------------------
001800141210
001900141210      //---------------------------------------------------------------
002000141210      //?Definizione schiere.
002100141210      //---------------------------------------------------------------
002200141210
002300141210      //---------------------------------------------------------------
002400141210      //?Definizione aree dati.
002500141210      //---------------------------------------------------------------
002600141210
002700141210      // - Dati utente
002800141210     d §AzUte        e ds                  extname(AZUTE00F)
002900141210     d                                     dtaara
003000141210     d §DatiUte      e ds                  extname(dDatiUte)
003100141210     d                                     dtaara
003200141210
003300141210      //---------------------------------------------------------------
003400141210      //?Definizione strutture dati.
003500141210      //---------------------------------------------------------------
003600141210
003700141210      // - Parametri ricevuti
003800141210     d KPJBA         e ds
003900141210
004000141210      // - Reperimento dati utente
004100141210     d TIBS34DS      e ds
004200150130
004300150130      // - Reperimento dati Anagrafica Clienti
004400150130      /copy gaitrasrc/srcprotopi,TIBS69R
004500150130
004600150130      // - File WFAUC
004700150331     d WFAUCDS       e ds                  extname(WFAUC00F)
004800150130     d                                     prefix(WFC:3)
004900141210
005000141210      //---------------------------------------------------------------
005100141210      //?Definizione variabili globali.
005200141210      //---------------------------------------------------------------
005300141210
005400141210      // - Flags booleani
005500141210     d Fine            s               n   inz(*off)
005600141210     d wEoF            s               n   inz(*off)
005700141210
005800141210      // - Campi di comodo
005900141210     d Comando         s            512a   varying
006000141210     d Oggi            s              8s 0 inz
006100141210     d sav_TAMctr      s                   like(TAMctr) inz(999)
006101161122     d nrrauc          s              9s 0
006200141210
006300141210      //---------------------------------------------------------------
006400141210      //?Definizione Procedure usate.
006500141210      //---------------------------------------------------------------
006600141210
006700141210      //---------------------------------------------------------------
006800141210      //?Definizione Prototipi.
006900141210      //---------------------------------------------------------------
007000141210      /copy gaitrasrc/srcprotopr,TIBS34R
007100150130      /copy gaitrasrc/srcprotopr,TIBS69R
007200141210      /copy gaitrasrc/srcprotopr,SYSTEM
007300141210
007400141210      //---------------------------------------------------------------
007500141210      //?Definizione key-list.
007600141210      //---------------------------------------------------------------
007700141210       // - File TNTAM04L
007800141210     d k03tntam      e ds                  extname(TNTAM04L:*key)
007900141210     d                                     prefix(k_)
008000141210
008100141210      //---------------------------------------------------------------
008200141210      //?M A I N - L I N E
008300141210      //---------------------------------------------------------------
008400141210
008500141210     c     *Entry        plist
008600141210     c                   parm                    kpjba
008700141210
008800141210      /free
008900141210
009000141210       //?Operazioni iniziali
009100141210       exsr RoutInz;
009200141210
009300141210       //?Elaborazione
009400141210       exsr Elabora;
009500141210
009600141210       //?Operazioni finali
009700141210       exsr RoutEnd;
009800141210
009900141210       //--------------------------------------------------------------
010000141210       //?Operazioni iniziali.
010100141210       //--------------------------------------------------------------
010200141210       BEGSR RoutInz;
010300141210
010400141210         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
010500141210
010600141210       //?Imposto oggi
010700141210         Oggi = %dec(%date());
010800141210
010900141210       //?Reperimento dati job
011000141210         exsr DatiJob;
011100141210
011200141210       //?Apro il file di work
011300150331         open WFAUC00F;
011400141210
011500141210       ENDSR;
011600141210
011700141210       //--------------------------------------------------------------
011800141210       //?Reperimento Dati del job (Utente/Operativi).
011900141210       //--------------------------------------------------------------
012000141210       BEGSR DatiJob;
012100141210
012200141210         in(E) §AzUte;
012300141210         IF  not %error;
012400141210           in(E) §DatiUte;
012500141210         ENDIF;
012600141210         IF  %error or RSut = *blanks;
012700141210           clear TIBS34ds;
012800141210           tibs34r(tibs34ds);
012900141210           in §AzUte;
013000141210           in §DatiUte;
013100141210         ENDIF;
013200141210
013300141210       ENDSR;
013400141210
013500141210       //--------------------------------------------------------------
013600141210       //?Elaborazione.
013700141210       //--------------------------------------------------------------
013800141210       BEGSR Elabora;
013900150130
014000150130       //?Leggo WFAUC
014100150130       //?solo 'C'
014200150130       //?No annullato
014300150130       //?No bloccato
014400150130       //?ha tariffe
014500150330       //?Non trattato
014600150130         exec sql
014700150130         DECLARE WRK cursor for
014800161122         SELECT rrn(wfauc00f), wfauc00f.*
014801161122         from EDPAUMENTI/WFAUC00F
014900150130         WHERE WFAtcl = 'C' and WFAflg = ' ' and
015000150330               WFAabl = ' ' and WFAtar = ' ' and
015100150330               WFAttr = ' '
015200150330         ORDER BY WFAksu, WFAncm, WFAksc;
015300150130
015400150130         //?Apertura del cursore
015500150130         exec sql open WRK;
015600150130
015700150130         IF sqlcode < 0;
015800150130           exec sql close WRK;
015900150130           leavesr;
016000150130         ENDIF;
016100150130
016200150130         DOW  not wEoF;
016300150130           exec sql
016400161122           fetch next from WRK into :nrrauc , :WFAUCDS;
016500150130           IF  sqlcod = 100 or sqlcod < 0;
016600150130             wEoF = *on;
016700150130             leave;
016800150130           ENDIF;
016900141210
017000150130         //?Leggo le tariffe
017100150130           setll WFCksc TNTAM04L;
017200150130           reade WFCksc TNTAM04L;
017300150130           DOW not %eof(TNTAM04L);
017400141210
017500150130         //?Faccio i ragionamenti a cambio tariffa
017600150130             IF  TAMctr <> sav_TAMctr;
017700150331           //?Scrivo WFAUC rcd T x aumento tariffe
017800150331               exsr Scrivi;
017900150130               sav_TAMctr = TAMctr;
018000150130             ENDIF;
018100150130
018200150130             reade WFCksc TNTAM04L;
018300141210
018400150130           ENDDO;
018500150130           reset sav_TAMctr;
018600150130
018700150130         ENDDO;
018800150130
018900150130         exec sql close WRK;
019000141210
019100141210       ENDSR;
019200150330
019300150330       //--------------------------------------------------------------
019400150331       //?Aggiorno WFAUC rcd T.
019500150330       //--------------------------------------------------------------
019600150331       BEGSR Scrivi;
019700150130
019701161122           //?Verifico se record cliente ha già impostata la prima tariffa
019702161122         If   wfcddtv = 0 ;
019703161122              chain  nrrauc  wfauc00f;
019704161122              If %Found(wfauc00f);
019705161122                 WFActrv = TAMctr;
019706161122                 WFAprgv = TAMprg;
019707161122                 WFAbap  = TAMbap;
019708161122                 WFAddtv = TAMddt;
019709161122                 WFcddtv = TAMddt;
019710161122                 WFAdstv = TAMdst;
019711161122                 WFApagv = TAMpag;
019712161122                 WFAppav = TAMppa;
019717161122                 update WFAUC000;
019718161122              Endif;
019719161122         Else ;
019800161122              WFAncm = WFCncm;
019900161122              WFAtcl = 'C';
020000161122              WFAksu = WFCksu;
020100161122              WFAksc = WFCksc;
020200161122              WFArag = WFCrag;
020300161122              WFAper = WFCper;
020400161122              WFAflg = WFCflg;
020500161122              WFAabl = WFCabl;
020600161122              WFAtar = WFCtar;
020700161122              WFAttr = WFCttr;
020800161122              WFAnksu = WFCnksu;
020900161122              WFAflt = WFCflt;
021000161122              WFAela = WFCela;
021100150331
021200161122              WFActrv = TAMctr;
021300161122              WFAprgv = TAMprg;
021400161122              WFAbap  = TAMbap;
021500161122              WFAddtv = TAMddt;
021600161122              WFAdstv = TAMdst;
021700161122              WFApagv = TAMpag;
021800161122              WFAppav = TAMppa;
021900161122              clear WFActrn;
022000161122              clear WFAprgn;
022100161122              clear WFAddtn;
022200161122              clear WFAdstn;
022300161122              clear WFApagn;
022400161122              clear WFAppan;
022500141210
022600161122              write WFAUC000;
022601161122         Endif;
022700141210
022800141210       ENDSR;
022900141210
023000141210       //--------------------------------------------------------------
023100141210       //?Operazioni finali.
023200141210       //--------------------------------------------------------------
023300141210       BEGSR RoutEnd;
023400141210
023500141210         *inLR = *on;
023600141210         return;
023700141210
023800141210       ENDSR;
023900141210
024000141210      /end-free

000100130130      * FNLS51R *----------------------------------------------------*
000200130204      *         Scarico richieste BIS  PER P.C. FWFGB
000300040401      *--------------------------------------------------------------*
000400040401
000500040401     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600040401
000700130206     FFNLS51D   CF   E             WORKSTN
000800130206     f
000900130204     fFNFGb04L  uf   e           k disk
001000130206     fWFSAT01L  if   e           k disk
001100130206     ftabel00f  if   e           k disk
001200130204     fWFFGB00F  o    e             disk
001300040401      *
001400040401      *
001500040401      * === STRUTTURE DATI DEFINITE ESTERNAMENTE =================== *
001600040401      *
001700040401      * Passaggio Parametri
001800040401     d KPJBA         e ds
001900130206     d TIBS34ds      e ds
002000130206     d dUte01        e ds
002100130206
002200130206
002300130206      // - Dati utente
002400130206     d §AzUte        e ds                  extname(AZUTE00F)
002500130206     d                                     dtaara
002600130206     d §DatiUte      e ds                  extname(dDatiUte)
002700130206     d                                     dtaara
002800130206     D WLBDA8          DS                  INZ
002900130206     D  G08DAT                 1      8  0
003000130206     D  G08INV                 9     16  0
003100130206     D  G08ERR                17     17
003200130206     D  G08TGI                18     22  0
003300130206     D*------------------
003400130206     D FNLSD9DS        DS
003500130206     D  ilsd9dsd               1      8  0
003600130206     D  ilsd9dsa               9     16  0
003700130206     d* S=stampa F=file E=entrambi
003800130206     D  ilsd9RIC              17     17
003900130206     D  ilsd9TFP              18     20  0
004000130206
004100040401      *
004200040401      * === VARIABILI ============================================== *
004300130206     D MSG             S             78    DIM(3) CTDATA PERRCD(1)
004400040401      *
004500130204     d kdtn            s                   like(fgbdtn)
004600130204     d kdtr            s                   like(fgbdtr) inz
004700130205     d kftr            s                   like(fgbftr) inz
004800040401      * Campi di comodo
004900130205     d datasys         s               d   datfmt(*iso) inz(*sys)
005000130205     d Ora_sys         s               t   timfmt(*eur) inz(*sys)
005100130206     d Dataeur         s               d   datfmt(*eur)
005200130206     d
005300130206     d Qcmd            s           2048    inz  varying
005400130318     d
005500130206      /copy qSysInc/qRpgleSrc,QCAPCMD
005600130206      *   -?API QCAPCMD (Process Commands)?
005700130206      /copy gaitrasrc/srcProtoPR,QCAPCMD
005800130206      *
005900130206      * -?Parametri gestione errori API.?
006000130206      /copy gaitrasrc/srcProtoPR,TIBS34R
006100130206      /copy qsysinc/qrpglesrc,QUSEC
006200130206
006300130318     IFNFGB000
006400130318     I              fgbnot                      fgbnot60
006500040401      * === KEY-LIST =============================================== *
006600040401      *
006700130204     c     kfgb          klist
006800130204     c                   kfld                    Kftr
006900130204     c                   kfld                    KDTN
007000130206     c     ksat          klist
007100130206     c                   kfld                    fgblnp
007200130206     c                   kfld                    fgbtfa
007300040401
007400040401      *--------------------------------------------------------------*
007500040401      * === MAIN LINE PROGRAM ====================================== *
007600040401      *--------------------------------------------------------------*
007700040401
007800040401     c     *Entry        plist
007900040401     c                   parm                    KPJBA
008000130206     c
008100130206     c* Gestione videata richiesta data dal/al
008200130206     c                   clear                   v1cdtn
008300130206     c                   clear                   v1crisc
008400130206     c* visualizzo orario blocco inserimento traini
008500130206     c                   exsr      carTAB
008600130206     c
008700130206     c                   do        *hival
008800130206     c                   exfmt     LS51D01
008900130206     c*  F3 - fine
009000130206     c     *inkc         cabeq     *on           fine
009100130206
009200130206     c* F9 - visulizza file WFFGB00F
009300130206     c                   if        *inki
009400130206     c                   exsr      visualFILE
009500130206     c                   iter
009600130206     c                   endif
009700130206
009800130206     c*F6 o ENTER controlli
009900130206     c                   exsr      chkd01
010000130206     c
010100130206     c     *in28         ifeq      *off
010200130206     c     *inkf         andeq     *on
010300130206     c                   leave
010400130206     c                   endif
010500130206     c                   enddo
010600130206     c*
010700130206     c* richiamo pgm calcolo IMP
010800130206     c                   clear                   fnlsd9ds
010900130206     c                   eval      ilsd9dsa=kdtn
011000130206     c                   eval      ilsd9ric='F'
011100130206     c                   movel     fnlsd9ds      kpjbu
011200130206     c                   call      'FNLSD9R'
011300130206     c                   parm                    kpjba
011400130205     c
011500130206     c                   exsr      Elabora
011600130206     c
011700130206     c     fine          tag
011800130206     c                   seton                                        LR
011900130206     c
012000130206     c*---------------------------------------------------------------------
012100130206     c     Elabora       BEGSR
012200130206     c
012300130205     c* nuovo scarico o ristampa
012400130206     c                   if        v1crisc='  '
012500130204     c                   eval      kftr=' '
012600130206     c                   else
012700130206     c                   eval      kftr='E'
012800130206     c                   endif
012900130205     c* Pulisco il file
013000130205      /FREE
013100130205       exec sql delete from WFFGB00F    ;
013200130205      /END-FREE
013300130205     c
013400130204     c
013500130204     c     kfgb          setll     fnfgb04l
013600130207     c
013700130207     c                   if        v1crisc='  '
013800130205     c     kfgb          reade     fnfgb04l
013900130207     c                   else
014000130207     c     kfgb          reade(n)  fnfgb04l
014100130207     c                   endif
014200130204     c
014300130204     c                   dow       not %eof(fnfgb04l)
014400130206     c* reperisco i dati del volume a terra da partire
014500130206     c     ksat          chain     wfsat01l
014600130206     c                   if        %found(wfsat01l)
014700130318     c                   eval(H)   fgbimp=satvpt
014800130206     c                   else
014900130206     c                   clear                   fgbimp
015000130206     c                   endif
015100130318     c
015200130404     c*****              movel     fgbnot60      fgbnot
015300130404     c                   eval      fgbnot=%trim(FGBnot60)
015400130206     c
015500130204     c                   write     wffgb000
015600130204     c
015700130207     c                   if        v1crisc='  '
015800130205     c                   eval      fgbftr='E'
015900130205     c                   eval      fgbdtr=%dec(datasys)
016000130205     c                   eval      fgbhtr=%dec(ora_sys)
016100130207     c                   update    fnfgb000
016200130207     c                   endif
016300130204     c
016400130205     c
016500130207     c                   if        v1crisc='  '
016600130205     c     kfgb          reade     fnfgb04l
016700130207     c                   else
016800130207     c     kfgb          reade(n)  fnfgb04l
016900130207     c                   endif
017000130207     c
017100130204     c                   enddo
017200040401      *
017300130206     c                   ENDSR
017400040401
017500130206     C*------------------------------------------------------------------------*
017600130206     C* CHKD01  - Controlli videata
017700130206     C*------------------------------------------------------------------------*
017800130206     C     CHKD01        BEGSR
017900130206     C*
018000130206     c                   setoff                                       28
018100130206     c                   clear                   v1cmsg
018200130206     c* controllo data dal se immessa
018300130206     c                   clear                   kdtn
018400130206     C                   MOVE      V1Cdtn        G08DAT
018500130206     C                   MOVEL     *ZERO         G08INV
018600130206     C                   MOVEL     *BLANKS       G08ERR
018700130206     C                   CALL      'XSRDA8'
018800130206     C                   PARM                    WLBDA8
018900130206     C     G08ERR        IFEQ      '1'
019000130206     C                   MOVEL     MSG(1)        V1CMSG
019100130206     C                   SETON                                        28
019200130206     C                   GOTO      ENDCTR
019300130206     C                   ENDIF
019400130206     C                   Z-ADD     G08DAT        V1CDtn
019500130206     C                   Z-ADD     G08INV        KDTN              8 0
019600130206     C*
019700130206     c* controllo se ci sono dati da scaricare
019800130206     c                   if        v1crisc='  '
019900130206     c                   eval      kftr=' '
020000130206     c     kfgb          chain(n)  fnfgb04l
020100130206     c                   if        not %found(fnfgb04l)
020200130206     C                   MOVEL     MSG(2)        V1CMSG
020300130206     C                   SETON                                        28
020400130206     C                   GOTO      ENDCTR
020500130206     C                   ENDIF
020600130206     c
020700130206     c                   else
020800130206     c
020900130206     c                   eval      kftr='E'
021000130206     c     kfgb          chain(n)  fnfgb04l
021100130206     c                   if        not %found(fnfgb04l)
021200130206     C                   MOVEL     MSG(3)        V1CMSG
021300130206     C                   SETON                                        28
021400130206     C                   GOTO      ENDCTR
021500130206     C                   ENDIF
021600130206     c
021700130206     c                   endif
021800130206
021900130206     c     endctr        ENDSR
022000130206      /FREE
022100130206       //--------------------------------------------------------------
022200130206       //?Operazioni iniziali.
022300130206       //--------------------------------------------------------------
022400130206       BEGSR carTAB ;
022500130206
022600130206         in(E) §AzUte;
022700130206         if NOT %error;
022800130206           in(E) §DatiUte;
022900130206         endif;
023000130206         if %error or RSut = *blanks;
023100130206           clear TIBS34ds;
023200130206           tibs34r(tibs34ds);
023300130206           in §AzUte;
023400130206           in §DatiUte;
023500130206         endif;
023600130206
023700130206       // Carico tabella ora limite per richiesta BIS nel giorno
023800130206         clear tblkey  ;
023900130206         tblkey='BIS'       ;
024000130206         chain  (1:'TW':tblkey) tabel00f;
024100130206 6       if %found(tabel00f);
024200130206          v1chmbl=%int(%subst(tbluni:1:6))   ;
024300130206         endif ;
024400130206
024500130206       // Propongo udate nella data
024600130206         dataeur=datasys         ;
024700130206         v1cdtn =%dec(dataeur)   ;
024800130206         ENDSR   ;
024900130206
025000130206       //--------------------------------------------------------------
025100130206       //?F9 - Visualizza file WFSAT00F
025200130206       //--------------------------------------------------------------
025300130206         BEGSR  VisualFILE       ;
025400130206         Qcmd = 'DSPDBF file(*libl/WFFGB00F) mbr(*first) output(*)';
025500130206
025600130206         clear Qcap0100;
025700130206         Qcabcsdh = *off;
025800130206         Qcapa    = *off;
025900130206         Qcacmdss = *off;
026000130206         Qcaerved = *allX'00';
026100130206
026200130206         clear Qusec;
026300130206         Qusbprv  = %size(Qusec);
026400130206
026500130206         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
026600130206                           %size(Qcap0100) : 'CPOP0100' : *omit :
026700130206                           0 : 0 : Qusec);
026800130206
026900130206
027000130206       ENDSR  ;
027100130206
027200130206      /END-FREE
027300130206**
027400130206Data errata                                                                   1
027500130206Non esistono nuove richieste da scaricare nella data TRAINO immessa           2
027600130206Non esistono richieste da RISCARICARE per la data TRAINO immessa              3

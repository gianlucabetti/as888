000100960924     H DECEDIT('0,') DATEDIT(*DMY.)
000200000915      *-------------------------------------------------------------------------*
000300000915      * Modifiche  :                                                            *
000400000915      *      AB     aggiunta la tabella DEP per gestire sullo stesso archivio   *
000500031106      *             il 190 e 191.                                               *
000600000915      *-------------------------------------------------------------------------*
000700000915     F*  DATA BASE                                                              *
000800000915     F*-------------------------------------------------------------------------*
000900960924     FCNACO00F  IF   E           K DISK
001000960924     F*---------
001100960924     FEDTAB01L  IF   E           K DISK
001200000915     F*---------
001300000915     FTNTBE01L  IF   E           K DISK
001400961029     F*---------
001500961029     FTABEL00F  IF   E           K DISK
001600960924     F*---------
001700041123     FEdivab1L  IF   E           K DISK
001800041123     FEdivab4L  IF   E           K DISK    RENAME(edivab00:edivab04)
001900051206     FEdivab3L  IF   E           K DISK    RENAME(edivab00:edivab03)
002000030925     FEdivad1L  IF   E           K DISK
002100031204     FEdivat1L  IF   E           K DISK
002200040713     FEdivax1L  IF   E           K DISK
002300121022     F*---------
002400121022     FAZORG01L  IF   E           K DISK
002500960924     F*---------
002600960924     FTRTC85D   CF   E             WORKSTN
002700960924     F                                     SFILE(TC85S02:NRR2)
002800960924     D*---------------------------------------------------------------------*
002900960924     D*  SCHIERE
003000960924     D*---------------------------------------------------------------------*
003100960924     D* Tabelle capiconto
003200960924     D* Tabella partner
003300121105     D KSC             S              7  0 DIM(300)
003400121105     D DPU             S             90    DIM(300)
003500121105     D CPT             S             35    DIM(300)
003600121105     D CCM             S              7  0 DIM(300)
003700020919     D kpjbus          S                   LIKE(kpjbu)
003800960924     D* Schiera errori
003900141113     D ERR             S             79    DIM(12) CTDATA PERRCD(1)
004000051130     D* Schiera Testate pgm
004100051213     D TEs             S             38    DIM(2) CTDATA PERRCD(1)
004200031106     D* Schiera clienti aventi flag accorpamento bolle x medesimo destinatario su tabella 3C
004300121105     D KSCABD          S              7  0 DIM(500)
004400121105     D KSCABDVX        S              7  0 DIM(500)
004500040713     D* Schiera codici trattamento merce in funzione delle estensioni VAD/VAT/VAX
004600040713     D SkEXT           S              3    DIM(100) INZ(*blanks)
004700040713     D SkCTM           S              2    DIM(100) INZ(*blanks)
004800960924     D*---------------------------------------------------------------------*
004900960924     D* DS
005000960924     D*---------------------------------------------------------------------*
005100960924     D KPJBA         E DS
005200960924     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005300960924     D  TCU                  398    697
005400960924     D                                     DIM(50)                              Flg 8 tp.conto
005500960924     D  KCU                  698    847P 0
005600960924     D                                     DIM(50)                              Capiconto
005700960924     D                                     PACKEVEN
005800960924     D CNCR80        E DS
005900121128     D tntb50ds      E DS
006000031106     D DS3C          E DS
006100040713     D DS1B          E DS
006200121126     D OG143         E DS
006300120424      * Tabella 3BC = Bypass ctrl x cliente
006400120424     d d3BC          e ds                  inz
006500141112     d D3TR          e ds                  inz
006600960924     D* Ds scomposzione tipo capoconti
006700960924     D TCUDS           DS
006800960924     D  F34                    3      4
006900960924     D  F56                    5      6
007000000915     D DDEP          E DS
007100000915     D  §DPKSC       E                     EXTFLD(§DEPKSC)
007200000915     D  §DPLNP       E                     EXTFLD(§DEPLNP)
007300000915     D  §DPDES       E                     EXTFLD(§DEPDES)
007400960924     D DSPT          E DS                  EXTNAME(EDIDSPT)
007500961024     D DSPU          E DS                  EXTNAME(EDIDSPU)
007600960924     D TRTC86        E DS                  EXTNAME(TRTC86DS)
007700020919     D TRUL06DS      E DS
007800020919     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
007900960924     D*  Ds per inversione data
008000960924     D WLBDA8          DS
008100960924     D  G02DAT                 1      8  0
008200960924     D  G02INV                 9     16  0
008300960924     D  G02ERR                17     17
008400960924     D  G02TGI                18     22  0
008500960924     D                SDS
008600960924     D  NOMPGM                 1     10
008700120619     D  SDSusr               254    263
008800051130     D PARAM           DS
008900051130     D  trasfe                 1      1
009000030709     D*------------------
009100030709     D* DS REPERIMENTO DATI CLIENTE
009200030709     D*-------------------
009300030709     D BS69DS        E DS                  EXTNAME(TIBS69DS)
009400030709     D ACODS         E DS                  EXTNAME(CNACO00F)
009500030709     D INDDS         E DS                  EXTNAME(CNIND00F)
009600030709     D CLPDS         E DS                  EXTNAME(CNCLP00F)
009700030709     D CLSDS         E DS                  EXTNAME(FNCLS00F)
009800040713     D*------------------
009900040713     D* DS WRK X VERIFICA ROTTURA CHIAVE BOLLA SU VAX
010000040713     D*-------------------
010100040713     D DSKEYVAX        DS
010200040713     D  VAXFGS
010300040713     D  VAXCCM
010400040713     D  VAXCMR
010500040713     D  VAXCNT
010600040713     D  VAXAAS
010700040713     D  VAXLNP
010800040713     D  VAXNRS
010900040713     D  VAXNSP
011000040713     D*------------------
011100040713     D* DS WRK X VERIFICA ESTENSIONI RICHIESTE X OGNI CODICE TRATTAMENTO MERCE
011200040713     D*-------------------
011300040713     D DSSKEXT         DS
011400040713     D  FLGVAD                        1    INZ(*blanks)
011500040713     D  FLGVAT                        1    INZ(*blanks)
011600040713     D  FLGVAX                        1    INZ(*blanks)
011700040713     D*------------------
011800040713     D* VARIABILI D WRK
011900040713     D*-------------------
012000040713     D DSKEYVAXROT     S                   LIKE(DSKEYVAX) INZ(*blanks)
012100040713     D wTotVADCMR      S              9  0 INZ(*zeros)
012200040713     D wTotVATCMR      S              9  0 INZ(*zeros)
012300040713     D wTotVAXCMR      S              9  0 INZ(*zeros)
012400050708     D savD86CCM       S                   LIKE(D86CCM)
012500050708     D savD86CMR       S                   LIKE(D86CMR)
012600050708     D savD86CNT       S                   LIKE(D86CNT)
012700051206
012800051206     d StringaSql      s            500    Varying
012900120424
013000120424     D* Variabili di appoggio
013100120424     D wY              s              3s 0
013200120619     D Forcing         s               n
013300120620     D Exfmt           s              1
013400120620     D Msg01           s             60
013500120620     D Msg02           s             60
013600120620     D Msg03           s             60
013700141112     d wFilTrasf       s              3s 0
013800141112     d wPOABI          ds
013900141112     d  SkPOABI                       3    dim(10)
014000141113     d wNTW_1          s                   like (§OGNTW)
014100141113     d wNTW_2          s                   like (§OGNTW)
014200121127
014300121127      * - Parametri per richiamo al pgm di controllo profilo utenti
014400121127     d TIBS34ds      e ds
014500121127      * - Ds di riferimento al file esterno AzUte00F
014600121127     d AZUTEds       e ds                  ExtName(AzUte00F)
014700121127      * - Ds per dati organigramma
014800121127     d dDatiUte      e ds
014900121127     d  simfel_dut   e                     extfld(simfel)
015000960924     C*---------------------------------------------------------------------*
015100960924     C* Ciclo principale
015200960924     C*---------------------------------------------------------------------*
015300070820     C*
015400070820     C*  Avvio sempre e cmq il controllo sincronia
015500070820     C                   CALL      'TRTC85C'
015600070820     C                   PARM      'STR'         OPZ               3
015700070820     C*
015800960924     C* Inizializzo prima videata
015900960924     C                   EXSR      INZ01
016000960924      * Loop gestione videate
016100960924     C     WFINE         DOWEQ     'N'
016200960924     C     WTPVID        CASEQ     '1D'          GESD01                         Sce.cliente
016300960924     C     WTPVID        CASEQ     '2S'          GESS02                         Subfile
016400960924     C     WTPVID        CASEQ     '2D'          GESD02                         Subfile vuoto
016500960924     C                   END
016600960924     C                   END
016700960924      * Fine Lavoro
016800960924     C     FINE          TAG
016900070802     C*
017000070802     C* Infine arresto il controllo sincronia
017100070802     C                   CALL      'TRTC85C'
017200070802     C                   PARM      'END'         OPZ               3
017300070802     C*
017400960924     C                   SETON                                        LR
017500960924     C*---------------------------------------------------------------*
017600960924     C*  INZ01: Inizializzo prima videata
017700960924     C*---------------------------------------------------------------*
017800960924     C     INZ01         BEGSR
017900960924     C*
018000960924     C                   SETOFF                                       2840
018100960924     C                   MOVEL     *BLANKS       $MSG
018200960924     C                   Z-ADD     0             V1CCCM
018300960924     C                   MOVEL     NOMPGM        V1CPGM
018400051213     C  n01              MOVE      tes(1)        V1Ctes
018500051213     C   01              MOVE      tes(2)        V1Ctes
018600960924     C                   MOVEL     '1D'          WTPVID
018700030617     C                   MOVEL     'K'           V1CORD
018800031106     C                   MOVEL     *BLANKS       V1CCMR
018900031106     C                   MOVEL     'N'           V1CABD
019000960924     C*
019100960924     C                   ENDSR
019200960924     C*---------------------------------------------------------------*
019300960924     C*  INZ02: Inizializzo seconda videata
019400960924     C*---------------------------------------------------------------*
019500960924     C     INZ02         BEGSR
019600960924     C*
019700070820     C*  Avvio sempre e cmq il controllo sincronia
019800031113     C                   CALL      'TRTC85C'
019900070820     C                   PARM      'EXE'         OPZ               3
020000031113     C*
020100960924     C                   SETOFF                                       28
020200960924     C                   MOVEL     *BLANKS       $MSG
020300051206     c                   clear                   v2cfil
020400960924     C                   MOVEL     NOMPGM        V2CPGM
020500051213     C  n01              MOVE      tes(1)        V2Ctes
020600051213     C   01              MOVE      tes(2)        V2Ctes
020700960924     C*  Pulisco subfile
020800960926     C                   Z-ADD     0             NRR2
020900960924     C                   SETOFF                                       2021
021000960924     C                   WRITE     TC85C02
021100960924     C                   SETON                                        2021
021200031118     C*  Inizializzo variabili d appoggio e d wrk
021300031106     C                   CLEAR                   WSVCNT
021400031106     C                   CLEAR                   WSVCMR
021500031106     C                   CLEAR                   WSVCCM
021600040405     C                   CLEAR                   WSVDTS
021700040405     C                   CLEAR                   WSVHMS
021800031118     C                   CLEAR                   WSVSPE
021900031118     C                   CLEAR                   WSVNCL
022000031118     C                   CLEAR                   WSVPKB
022100040713     C                   CLEAR                   wTotVADCMR
022200040713     C                   CLEAR                   wTotVATCMR
022300040713     C                   CLEAR                   wTotVAXCMR
022400031118     C                   Z-ADD     *zeros        wGiro             1 0
022500030617      *
022600030617      *  Se ordinamento x cliente
022700041115     C                   IF        *in54
022800960924      *  ... se ho selezionato un codice partner carico solo quello
022900960924     C     V1CCCM        IFNE      0
023000960926     C                   Z-ADD     V1CCCM        KCCM
023100960926     C                   Z-ADD     V1CCCM        WCCMAL
023200960924     C                   ELSE
023300960924      *  ... altrimenti comincio leggere dal primo codice
023400960924     C                   Z-ADD     0             KCCM
023500960924     C                   Z-ADD     9999999       WCCMAL            7 0
023600030617     C                   ENDIF
023700041115     c* Verifico se presente anche cmr per fare lettura con 2 campi chiave
023800041115     C     V1CCMR        IFNE      *blanks
023900041115     C                   MOVEL(P)  V1CCMR        KCMR
024000041115     C                   ELSE
024100041115     C                   MOVEL     *blanks       KCMR
024200041115     C                   ENDIF
024300030617     C                   ENDIF
024400030617      *
024500030617      *  Se ordinamento x CMR
024600041115     C                   IF        *in55
024700030617      *  ... se ho selezionato un numero CMR carico solo quello
024800030617     C     V1CCMR        IFNE      *blanks
024900030617     C                   MOVEL(P)  V1CCMR        KCMR
025000030617     C                   MOVEL(P)  V1CCMR        WCMRAL
025100030617     C                   ELSE
025200030617      *  ... altrimenti comincio leggere dal primo numero CMR
025300030617     C                   MOVEL     *blanks       KCMR
025400030617     C                   MOVEL     *hival        WCMRAL           35
025500030617     C                   ENDIF
025600030617     C                   ENDIF
025700960924     C*  Prima lettura
025800041115     c                   select
025900041115     c                   when      *iN54   and kcmr<>*BLANKS
026000041123     C     KVAB5         SETLL     Edivab1L
026100041115     C                   WHEN      *IN54
026200041123     C     KCCM          SETLL     Edivab1L
026300041115     C                   WHEN      *IN55
026400041123     C     KCMR          SETLL     Edivab4L
026500041115     C                   ENDSL
026600041115     C
026700020919     c                   exsr      ReadVAB
026800041115     C
026900960924      *  Eseguo caricamento prima pagina
027000041123     C   54              EXSR      ROLLUP01
027100041123     C   55              EXSR      ROLLUP04
027200960924      *  Se non ho dati da modificare emetto subfile vuoto
027300960924     C     NRR2          IFEQ      0
027400960924     C                   MOVEL     '2D'          WTPVID            2
027500960924     C                   ELSE
027600960924     C                   MOVEL     '2S'          WTPVID
027700960924     C                   END
027800960924     C*
027900960924     C                   ENDSR
028000960924     C*---------------------------------------------------------------*
028100020919     C*  READVAB: Legge il record di EDIVAB
028200960924     C*---------------------------------------------------------------*
028300020919     C     ReadVAB       BEGSR
028400960924     C*
028500020919     c     rileggi       tag
028600041115     c
028700041115     C                   SELECT
028800041115     C                   WHEN      *IN54 AND KCCM>0 AND KCMR<>*BLANKS
028900041123     C     KVAB5         READE     Edivab1L                               23
029000041115     C                   WHEN      *IN54 AND KCCM>0
029100041123     C     kccm          READe     Edivab1L                               23
029200041115     C                   WHEN      *IN54
029300041123     C                   READ      Edivab1L                               23
029400041115     C                   WHEN      *IN55 AND KCMR<>*BLANKS
029500041123     C     kcmr          READe     Edivab4L                               23
029600041115     C                   WHEN      *IN55
029700041123     C                   READ      Edivab4L                               23
029800041115     c                   endSL
029900020919     C*
030000020919     C*  controlla se non è compresa fra le linee gestite dal terminal
030100020919     C*   quindi legge nuovo record.
030200030617     c                   if        *in54 = *on        AND                       * ordinam. edivab1l
030300041123     c                             not %eof(Edivab1L)
030400020919     c     vabfgs        cabeq     0             rileggi
030500020919     c     vabfgs        lookup    lin                                    32
030600020919     c     *in32         cabeq     *off          rileggi
030700030617     c                   endif
030800030617     c                   if        *in55 = *on        AND                       * ordinam. edivab4l
030900041123     c                             not %eof(Edivab4L)
031000030617     c     vabfgs        cabeq     0             rileggi
031100030617     c     vabfgs        lookup    lin                                    32
031200030617     c     *in32         cabeq     *off          rileggi
031300030617     c                   endif
031400031106     C*
031500130103     C* se sono entrato mediante l'azione TC8A, non servono test sull'ABD
031600130103     C* il rcd letto verrà scritto sul sfl
031700130103     C                   IF        %subst(KPJBU : 1 : 1) <> 'S'
031800031106     C*  controlla se il cliente è abilitato in tabella 3C all'accorpamento bolle x
031900031106     C*  medesimo destinatario in relazione al relativo parametro richiesto a video
032000041115     C                   if        *in54 = *on        AND                       * ordinam. edivab5l
032100041123     C                             not %eof(Edivab1L)
032200031106     C     vabCCM        lookup    KSCABD                                 32
032300031106     C                   if        (*in32 = *off AND V1CABD = 'N') OR
032400031106     C                             (*in32 = *on  AND V1CABD = 'S')
032500031106     C                   else
032600031106     C                   goto      rileggi
032700031106     C                   endif
032800031106     C                   endif
032900041115     C                   if        *in55 = *on        AND                       * ordinam. edivab6l
033000041123     C                             not %eof(Edivab4L)
033100031106     C     vabCCM        lookup    KSCABD                                 32
033200031106     C                   if        (*in32 = *off AND V1CABD = 'N') OR
033300031106     C                             (*in32 = *on  AND V1CABD = 'S')
033400031106     C                   else
033500031106     C                   goto      rileggi
033600031106     C                   endif
033700031106     C                   endif
033800130103     C*
033900130103     C                   ENDIF
034000020919     C*
034100020919     C                   ENDSR
034200020919     C*---------------------------------------------------------------*
034300020919     C*  GESD01: Gestione prima videata
034400020919     C*---------------------------------------------------------------*
034500020919     C     GESD01        BEGSR
034600020919     C*
034700960924     C                   EXFMT     TC85D01
034800960924     C                   SETOFF                                       2840
034900960924     C*  Fine Lavoro
035000960924     C     *INKC         IFEQ      '1'
035100960924     C                   MOVEL     'S'           WFINE
035200960924     C                   GOTO      FINVD1
035300960924     C                   END
035400960924     C*  Controlli
035500960924     C                   EXSR      CTR01
035600960924     C*  Caricamento subfile
035700960924     C  N28              EXSR      INZ02
035800960924     C*
035900960924     C     FINVD1        ENDSR
036000960924     C*---------------------------------------------------------------*
036100960924     C*  GESS02: Gestione subfile pieno
036200960924     C*---------------------------------------------------------------*
036300960924     C     GESS02        BEGSR
036400960924     C*
036500031110     C     EMIVS2        TAG
036600051201     C                   WRITE     TC85Z02
036700960924     C                   EXFMT     TC85C02
036800120620     C* memo che c'è stata una exfmt del sfl
036900120620     C                   EVAL      Exfmt = *on
037000031110     C*  Spengo indicatore di errore sui record del sub-file
037100031110     C                   SETOFF                                       28
037200960924     C*  Fine Lavoro
037300960924     C     *INKC         IFEQ      '1'
037400960924     C                   MOVEL     'S'           WFINE
037500960924     C                   GOTO      FINVS2
037600960924     C                   END
037700960924     C*  Ritorno
037800960924     C     *INKL         IFEQ      '1'
037900960924     C                   EXSR      INZ01
038000960924     C                   GOTO      FINVS2
038100960924     C                   END
038200960924     C*  Controlli
038300960930     C                   EXSR      CTR02
038400031110     C   28              GOTO      EMIVS2
038500960924     C*
038600960924     C     FINVS2        ENDSR
038700960924     C*---------------------------------------------------------------*
038800960924     C*  GESD02: Gestione subfile vuoto
038900960924     C*---------------------------------------------------------------*
039000960924     C     GESD02        BEGSR
039100960924     C*
039200960924     C                   EXFMT     TC85D02
039300960924     C*  Fine Lavoro
039400961028     C     *INKC         IFEQ      '1'
039500961028     C                   MOVEL     'S'           WFINE
039600960924     C                   GOTO      FINVD2
039700960924     C                   END
039800960924     C*  Ritorno
039900960924     C     *INKL         IFEQ      '1'
040000960924     C                   EXSR      INZ01
040100960924     C                   GOTO      FINVD2
040200960924     C                   END
040300960924     C*
040400960924     C     FINVD2        ENDSR
040500960924     C*---------------------------------------------------------------*
040600960924     C*  CTR01: Controlli prima videata
040700960924     C*---------------------------------------------------------------*
040800960924     C     CTR01         BEGSR
040900031106     C*
041000031106     C* Inizializzo variabili d wrk
041100031106     C                   CLEAR                   WDTA
041200960924     C*
041300960924     C*  Se immesso il codice cliente ...
041400960924     C     V1CCCM        IFNE      0
041500960924     C*  Controllo se esiste su CNACO
041600960924     C                   Z-ADD     1             KKUT
041700960924     C                   Z-ADD     KCI           KKCC
041800960924     C                   Z-ADD     V1CCCM        KKSC
041900960924     C     KACO          CHAIN     CNACO00F                           30
042000960924     C     *IN30         IFEQ      '1'
042100960924     C     ACOFLG        ORNE      ' '
042200960924     C                   SETON                                        2840
042300960924     C                   MOVEL     ERR(1)        $MSG
042400960924     C                   GOTO      FINCT1
042500960924     C                   END
042600960924     C*  Controllo se è un partner
042700961024     C                   Z-ADD     1             XY                3 0
042800970602     C     V1CCCM        IFEQ      3200004
042900970602     C     0050743       LOOKUP    KSC(XY)                                32
043000970602     C                   ELSE
043100961024     C     V1CCCM        LOOKUP    KSC(XY)                                32
043200970602     C                   END
043300960924     C     *IN32         IFEQ      '0'
043400031106     C***                SETON                                        2840
043500031106     C***                MOVEL     ERR(2)        $MSG
043600031106     C***                GOTO      FINCT1
043700961024     C                   ELSE
043800961024     C                   MOVEL     DPU(XY)       DSPU
043900961024     C                   MOVEL     §PUDTA        WDTA              1
044000960924     C                   END
044100960924     C                   END
044200961029     C*  Reperisco codice cliente x lnp bolla
044300031107     C***                Z-ADD     §PTKSC        WKSC              7 0
044400030617     C*  Verifico il tipo d ordinamento richiesto
044500030617     C                   SETOFF                                       5455
044600041115     c                   select
044700041115     c* Se selezionato cod cliente ordinamento come se fosse per cli
044800041115     c* anche per CMR, tanto la lf dopo il cliente ha cmr
044900041115     C                   when      v1cccm>0
045000030617     C                   SETON                                        54
045100041115     c* Se selezionato CMR ordinamento come se fosse per CMR
045200041115     c* anche per cliente, tanto la lf dopo il cmr ha il cliente
045300041115     C                   when      v1ccmr<>*blanks
045400041115     C                   SETON                                        55
045500041115     c* altrimenti il tipo ordinamento
045600041115     C                   when      V1CORD = 'C'
045700030617     C                   SETON                                        55
045800041115     c                   other
045900041115     C                   SETON                                        54
046000041115     C                   ENDsl
046100030617     C*
046200960924     C     FINCT1        ENDSR
046300960924     C*---------------------------------------------------------------*
046400960924     C*  CTR02: Controlli seconda videata
046500960924     C*---------------------------------------------------------------*
046600960924     C     CTR02         BEGSR
046700040713     C*
046800040713     C                   SETOFF                                       70
046900031118     C*
047000031118     C*  Inizializzo la variabile d wrk relativa al controllo: accorpamento possibile solo su
047100031118     C*  singolo cliente
047200031118     C                   Z-ADD     *zeros        wCCM              7 0
047300960924     C*
047400960924     C*  Controllo selezioni effettuate
047500960924     C                   MOVEL     'N'           WSCELT            1
047600960930     C                   MOVEL     'N'           WSTAMP            1
047700031118     C                   Z-ADD     *ZEROS        KEYNRR2           4 0
047800051201    1C     1             DO        wNRR2         KEYNRR2
047900031118     C     KEYNRR2       CHAIN     TC85S02
048000040713     C*
048100040713     C* Reimposto gli indicatori in funzione dei campi hiddend d salvataggio
048200040713     C                   EVAL      *IN59 = WIN59
048300040713     C                   EVAL      *IN60 = WIN60
048400040713     C                   EVAL      *IN61 = WIN61
048500040713     C                   EVAL      *IN62 = WIN62
048600040713     C                   EVAL      *IN63 = WIN63
048700040713     C                   EVAL      *IN64 = WIN64
048800040713     C*
048900051201    2C     V2CSCE        IFNE      ' '
049000051130     c* controllo se opzione errata
049100051201    3c                   if        v2csce='7' and not *in01
049200051130     C                   MOVEL     ERR(6)        $MSG
049300051130     C                   SETON                                        28
049400051201    3c                   endif
049500051201    3c                   if        v2csce<>'7' and *in01
049600051130     C                   MOVEL     ERR(6)        $MSG
049700051130     C                   SETON                                        28
049800051201    3c                   endif
049900051130     c
050000051201    3c                   if        *in28
050100051130     c                   seton                                        70
050200051130     C                   UPDATE    TC85S02
050300051130     c                   setoff                                       70
050400051130     C                   LEAVE
050500051201    3c                   endif
050600051130     c
050700960924     C                   MOVEL     'S'           WSCELT
050800960924     C*  Richiamo pgm per esecuzione scrittura VAB
050900960924     C                   CLEAR                   TRTC86
051000961024     C                   MOVEL     WDTA          D86DTA
051100040713     C*
051200051201     C* X opzioni 1 e 9 d conferma bolle proseguo solo se no errori su estensioni
051300051201    3C                   IF        V2CSCE = '1' OR  v2csce='7'
051400051201     C                             or V2CSCE = '9'
051500120619
051600120619     C* se c'è un errore
051700120619    4C                   IF        (V2CERRVAD <> *blanks OR
051800120619     C                              V2CERRVAT <> *blanks OR
051900120619     C                              V2CERRVAX <> *blanks   )
052000120619     C* ma l'utente è un EDP*
052100120619     C                             and %subst(SDSusr : 1 : 3) = 'EDP'
052200120619     C* chiedo se voglio forzare
052300120622     C                   EVAL      Msg01 = 'Dettagli mancanti rispetto a CTM: +
052400120622     C                             F6 per forzare'
052500120620     C                   EVAL      Msg02 = 'Codice cliente mittente: ' +
052600120622     C                                     %editc(V2CCCM:'X')  +
052700120622     C                                     '  Numero CMR: ' +
052800120622     C                                     V2CCMR
052900120620     C                   EVAL      Msg03 = 'Utente forzatura: ' +
053000120619     C                                        SDSusr
053100120619     C                   EXSR      WdwForce
053200120619     C* se richiesta forzatura
053300120619     C                   IF        Forcing = *on
053400120619     C                   EVAL      V2CERRVAD = *blank
053500120619     C                   EVAL      V2CERRVAT = *blank
053600120619     C                   EVAL      V2CERRVAX = *blank
053700120620     C                   EVAL      S02Forced = *on
053800120620     C                   UPDATE    TC85S02
053900120620     C* mi riposiziono sul rcd di sfl letto
054000120620     C     KEYNRR2       CHAIN     TC85S02
054100120619     C                   ENDIF
054200120619     C                   ENDIF
054300120619
054400051201    4C                   IF        V2CERRVAD <> *blanks OR
054500040713     C                             V2CERRVAT <> *blanks OR
054600040713     C                             V2CERRVAX <> *blanks
054700040713     C* Emetto errori relativi a dettagli mancanti
054800040713     C                   SETON                                        2870
054900051201     C  n01              EVAL      $MSG = 'Impossibile confermare, dettagli ' +
055000040713     C                             'mancanti:'
055100051201     C   01              EVAL      $MSG = 'Impossibile TRASFERIRE, dettagli ' +
055200051201     C                             'mancanti:'
055300070808     C   03              EVAL      $MSG = 'Impossibile accorpare in presenza' +
055400070808     C                             ' di'
055500051201    5C                   IF        V2CERRVAD <> *blanks
055600040713     C                   EVAL      $MSG = %trim($MSG) + ' VAD'
055700051201    5C                   ENDIF
055800051201    5C                   IF        V2CERRVAT <> *blanks
055900040713     C                   EVAL      $MSG = %trim($MSG) + ' VAT'
056000051201    5C                   ENDIF
056100051201    5C                   IF        V2CERRVAX <> *blanks
056200040713     C                   EVAL      $MSG = %trim($MSG) + ' VAX'
056300051201    5C                   ENDIF
056400040713     C                   UPDATE    TC85S02
056500040713     C                   LEAVE
056600051201    4C                   ENDIF
056700051201    3C                   ENDIF
056800040713     C*
056900051201     c* elaboro il trasferimento: ulteriori controlli
057000051201     c                   if        v2csce='7'
057100051201     c                   exsr      contrTrasfe
057200051206     c
057300051206     c                   if        *in28
057400051206     C                   UPDATE    TC85S02
057500051206     c                   setoff                                       70
057600051206     C                   LEAVE
057700051206     c                   endif
057800051206     c
057900051206     c   kf              exsr      EseguiTrasfe
058000120620     C* se c'è stata conferma e il rcd era forzato
058100120620     C                   IF        *inKF = *on and S02Forced = *on
058200120620     C* invio email di conferma forzatura
058300120620     C                   EXSR      EmailForza
058400120620     C                   ENDIF
058500120620     C*
058600051206     c                   iter
058700051201     c                   endif
058800051201     c
058900051201    3C     V2CSCE        IFEQ      '1'
059000960924     C                   MOVEL     'E'           D86MOD
059100960927     C                   MOVEL     ' '           D86STA
059200051201    3C                   ENDIF
059300960924     C*  Richiamo pgm per stampa dati
059400091124    3C**   V2CSCE        IFEQ      '6'
059500091124     C**                 MOVEL     'S'           WSTAMP            1
059600091124     C**                 MOVEL     *BLANKS       D86MOD
059700091124     C**                 MOVEL     'S'           D86STA
059800091124    3C**                 ENDIF
059900020919     C*  Richiamo pgm per cancellazione Edivab
060000051201    3C     V2CSCE        IFEQ      '4'
060100960924     C                   MOVEL     'C'           D86MOD
060200960924     C                   MOVEL     *BLANKS       D86STA
060300051201    3C                   ENDIF
060400091124     c
060500031110     C*  Se opzione diversa da 9 => eseguo *pgm standard, altrimenti eseguo *pgm x raggruppamento
060600031113     C*  bolle e scrittura FIVAB/FIVAD
060700091124     c                   select
060800091124    3C     V2CSCE        wheneq    '9'
060900031110     C*  Verifico che x opzione 9 il cliente sia abilitato nella tabella 3C all'accorpamento
061000031113     C*  x medesimo destinatario
061100031110     C     V2CCCM        LOOKUP    KSCABD                                 32
061200051201    4C                   IF        (*IN32 = *OFF) OR
061300031201     C*  e che sia stato scelto ingresso x accorpamento bolle, altrimenti do errore
061400031201     C                             V1CABD <> 'S'                                * ridondante
061500031110     C                   MOVEL     ERR(4)        $MSG
061600040713     C                   SETON                                        2870
061700040713     C                   UPDATE    TC85S02
061800031113     C                   LEAVE
061900051201    4C                   ENDIF
062000031118     C* Verifico se impostata opzioine 9 (raggruppamento) su clienti diversi (ERRORE)
062100051201    4C                   IF        wCCM > *zeros
062200051201    5C                   IF        V2CCCM <> wCCM
062300031118     C                   MOVEL     ERR(5)        $MSG
062400040713     C                   SETON                                        28
062500051201    5C                   ENDIF
062600051201   x4C                   ELSE
062700031118     C                   Z-ADD     V2CCCM        wCCM
062800051201    4C                   ENDIF
062900091124
063000091124     c* Opzione di stampa --> richiamo con RT per poi avere la stampa di riepilogo
063100091124    3C     V2CSCE        wheneq    '6'
063200091124     C                   MOVEL     'S'           WSTAMP            1
063300091124     C                   MOVEL     *BLANKS       D86MOD
063400091124     C                   MOVEL     'S'           D86STA
063500091124     C                   MOVEL     V2CCMR        D86CMR
063600091124     C                   MOVEL     'RT'          D86CHI
063700091124     C                   Z-ADD     V2CCNT        D86CNT
063800091124     C                   Z-ADD     V2CCCM        D86CCM
063900091124     C                   MOVEL     TRTC86        KPJBU
064000091124     C   KF              CALL      'TRTC86R1'
064100091124     C                   PARM                    KPJBA
064200120620     C* se c'è stata conferma e il rcd era forzato
064300120620     C                   IF        *inKF = *on and S02Forced = *on
064400120620     C* invio email di conferma forzatura
064500120620     C                   EXSR      EmailForza
064600120620     C                   ENDIF
064700120620     C*
064800091124     c
064900091124    3C     V2CSCE        whenne    ' '
065000960924     C                   MOVEL     V2CCMR        D86CMR
065100091124     c* richiamo con LR perchè carica i dati da elaborare nella INZSR
065200030617     C                   MOVEL     'LR'          D86CHI
065300960924     C                   Z-ADD     V2CCNT        D86CNT
065400961028     C                   Z-ADD     V2CCCM        D86CCM
065500960926     C                   MOVEL     TRTC86        KPJBU
065600031118     C   KF              CALL      'TRTC86C'
065700960924     C                   PARM                    KPJBA
065800120620     C* se c'è stata conferma e il rcd era forzato
065900120620     C                   IF        *inKF = *on and S02Forced = *on
066000120620     C* invio email di conferma forzatura
066100120620     C                   EXSR      EmailForza
066200120620     C                   ENDIF
066300120620     C*
066400091124    3C                   ENDSL
066500051201    2C                   ENDIF
066600091124
066700040713     C                   UPDATE    TC85S02
066800051201    1C                   ENDDO
066900031113     C*
067000031113     C*  Se nn ho errori in opzioni sub-file gestisco...
067100051201    1C                   IF        *IN28 = *OFF
067200031113     C                   Z-ADD     *ZEROS        KEYNRR2           4 0
067300051201    2C     1             DO        wNRR2         KEYNRR2
067400031113     C     KEYNRR2       CHAIN     TC85S02
067500051201    3C                   IF        %found
067600031201     C*  Se scelto ingresso x accorpamento bolle
067700051201    4C                   IF        V1CABD = 'S'
067800031201     C*  E se richiesta opzione 9 d accorpamento bolle
067900051201    5C                   IF        V2CSCE = '9'
068000031113     C                   MOVEL     'R'           D86MOD
068100031113     C                   Z-ADD     V2CCCM        D86CCM
068200031113     C                   MOVEL     V2CCMR        D86CMR
068300031113     C                   Z-ADD     V2CCNT        D86CNT
068400050708     C                   Z-ADD     V2CCCM        savD86CCM
068500050708     C                   MOVEL     V2CCMR        savD86CMR
068600050708     C                   Z-ADD     V2CCNT        savD86CNT
068700031113     C                   MOVEL     'LR'          D86CHI
068800070802     C   KF              CALL(e)   'TRTC86R3'
068900031113     C                   PARM                    TRTC86
069000120620     C* se c'è stata conferma e il rcd era forzato
069100120620     C                   IF        *inKF = *on and S02Forced = *on
069200120620     C* invio email di conferma forzatura
069300120620     C                   EXSR      EmailForza
069400120620     C                   ENDIF
069500120620     C*
069600070802     C* Se errore => rollback
069700070802     C                   IF        %error
069800070802     C                   ROLBK
069900070802     C                   ENDIF
070000070802     C*
070100051201    5C                   ENDIF
070200051201    4C                   ENDIF
070300051201    3C                   ENDIF
070400051201    2C                   ENDDO
070500031201     C*
070600031201     C*  Se scelto ingresso x accorpamento bolle
070700051201    2C                   IF        V1CABD = 'S'
070800031113     C*  Al termine del passaggio dati dai file "buoni" a quelli d lavoro eseguo il programma d
070900031113     C*  accorpamento e scrittuta FIVAB/FIVAD vero e proprio
071000040714     C                   MOVEL     'R'           D86MOD
071100050708     C                   Z-ADD     savD86CCM     D86CCM
071200050708     C                   MOVEL     savD86CMR     D86CMR
071300050708     C                   Z-ADD     savD86CNT     D86CNT
071400040714     C                   MOVEL     'LR'          D86CHI
071500070802     C   KF              CALL(e)   'TRTC86R4'
071600070420     C                   PARM                    KPJBA
071700040714     C                   PARM                    TRTC86
071800120620     C* se c'è stata conferma e il rcd era forzato
071900120620     C                   IF        *inKF = *on and S02Forced = *on
072000120620     C* invio email di conferma forzatura
072100120620     C                   EXSR      EmailForza
072200120620     C                   ENDIF
072300120620     C*
072400070802     C* Se errore => rollback
072500070802     C                   IF        %error
072600070802     C                   ROLBK
072700070802     C                   ELSE
072800070802     C* Se ok     => committ
072900070802     C                   COMMIT
073000070802     C                   ENDIF
073100070802     C*
073200051201    2C                   ENDIF
073300031113     C*
073400960924     C*  Se non ho selezionato nulla do errore
073500051201    2C     *INKF         IFEQ      '1'
073600051201    3C     WSCELT        IFEQ      'N'
073700040713     C                   SETON                                        28
073800960924     C                   MOVEL     ERR(3)        $MSG
073900051201   x3C                   ELSE
074000960924     C*  Se ho selezionato stampa richiamo pgm x totali genarali
074100051201    4C     WSTAMP        IFEQ      'S'
074200960926     C                   MOVEL     *BLANKS       D86STA
074300960924     C                   MOVEL     *BLANKS       D86CMR
074400960926     C                   MOVEL     *BLANKS       D86MOD
074500960926     C                   MOVEL     'S'           D86RIE
074600960924     C                   MOVEL     'LR'          D86CHI
074700960924     C                   Z-ADD     0             D86CNT
074800960924     C                   Z-ADD     0             D86CCM
074900031107     C***                Z-ADD     0             D86CCN
075000960926     C                   MOVEL     TRTC86        KPJBU
075100091127     C                   CALL      'TRTC86R1'
075200960924     C                   PARM                    KPJBA
075300051201    4C                   ENDIF
075400031118     C*  Al termine dell'esecuzione delle opzioni se è stato premuto F6 => ricarico sub-file
075500051201    4C     WSCELT        IFNE      'N'
075600031118     C                   EXSR      INZ02
075700051201    4C                   ENDIF
075800051201    3C                   ENDIF
075900051201    2C                   ENDIF
076000031110     C*
076100051201    1C                   ENDIF
076200960924     C*
076300960924     C                   ENDSR
076400960924     C*----------------------------------------------------*
076500051206     C*   CONTRTRASFE - controllo se posso fare il trasferimento
076600960924     C*----------------------------------------------------*
076700051206     C     ContrTrasfe   BEGSR
076800121022     C*
076900121022     C* Filiale obbligatoria
077000051206     c                   if        v2cfil=0
077100051206     c                   movel     err(7)        $msg
077200051206     c                   seton                                        28
077300051206     c                   goto      endctr
077400051206     c                   endif
077500121022     c*
077600141113     C* Controllo che la Filiale di conferma bolle esista
077700121022     C     V2CFIL        CHAIN     AZORG01L                           28
077800121022    1C     *IN28         IFEQ      *ON
077900121022     C     ORGFVA        ORNE      ' '
078000121022     C     ORGFAG        ORNE      'A'
078100121022     C     ORGFAG        ANDNE     'F'
078200121022     c                   movel     err(9)        $msg
078300121022     C                   GOTO      ENDCTR
078400121022    1C                   ENDIF
078500141113     C* reperisco il suo network
078600141113     C                   MOVEL     ORGDE3        OG143
078700141113     C                   EVAL      wNTW_1 = §OGNTW
078800121128
078900141114     C*** tolgo il richiamo al TNTB51R in forma di controllo perché il test è legato a
079000141114     C*** PO + cliente e non solo a PO come si vuole
079100121128     c* richiamo pgm tntb51r
079200141114     c***                clear                   tntb50ds
079300141114     c***                movel     v2cccm        b50ke2
079400141114     c***                movel     v2cccm        b50ke1
079500141114     c***                movel     'C'           b50opz
079600141114     c***                movel     v2cfil        b50poa
079700141114     c***                call      'TNTB51R'
079800141114     c***                parm                    kpjba
079900141114     c***                parm                    tntb50ds
080000141114     c***
080100141114     c***                if        b50err<>' '
080200141114     c***                movel     b50msg        $msg
080300141114     c***                seton                                        2870
080400141114     c***                goto      endctr
080500141114     c***                endif
080600141114
080700121128     c
080800051206     c
080900051206     c* Verifico se già presente il CMR nel p.o. di ricezione dati
081000051206     c     kvab3         setll     edivab3l
081100051206     c     kvab3         reade     edivab3l
081200051206     c                   dow       not %eof(edivab3l)
081300051206     c                   if        vabcmr=v2ccmr
081400051206     c                   seton                                        2870
081500051206     c                   movel     err(8)        $msg
081600051206     C                   EVAL      $MSG = %trim($MSG) + 'VAB'
081700051206     c                   goto      endctr
081800051206     c                   endif
081900051206     c
082000051206     c     kvab3         reade     edivab3l
082100051206     c                   enddo
082200051206     c
082300051206     c     kdett         setll     edivad1l
082400051206     c                   if        %equal(edivad1l)
082500051206     c                   seton                                        2870
082600051206     c                   movel     err(8)        $msg
082700051206     C                   EVAL      $MSG = %trim($MSG) + 'VAD'
082800051206     c                   goto      endctr
082900051206     c                   endif
083000051206     c     kdett         setll     edivat1l
083100051206     c                   if        %equal(edivat1l)
083200051206     c                   seton                                        2870
083300051206     c                   movel     err(8)        $msg
083400051206     C                   EVAL      $MSG = %trim($MSG) + 'VAT'
083500051206     c                   goto      endctr
083600051206     c                   endif
083700051206     c     kdett         setll     edivax1l
083800051206     c                   if        %equal(edivax1l)
083900051206     c                   seton                                        2870
084000051206     c                   movel     err(8)        $msg
084100051206     C                   EVAL      $MSG = %trim($MSG) + 'VAX'
084200051206     c                   goto      endctr
084300051206     c                   endif
084400141112
084500141112     C*
084600141113     C* Filiale cliente esistente
084700141112     C                   EVAL      wFilTrasf =
084800141112     C                              %dec(%subst(%editc(V2CCCM:'X'):1:3):3:0)
084900141112     C     wFilTrasf     CHAIN     AZORG01L                           28
085000141112    1C     *IN28         IFEQ      *ON
085100141112     C     ORGFVA        ORNE      ' '
085200141112     C     ORGFAG        ORNE      'A'
085300141112     C     ORGFAG        ANDNE     'F'
085400141112     c                   movel     err(9)        $msg
085500141112     C                   GOTO      ENDCTR
085600141112    1C                   ENDIF
085700141113     C* reperisco il suo network
085800141112     C                   MOVEL     ORGDE3        OG143
085900141113     C                   EVAL      wNTW_2 = §OGNTW
086000141112     C*
086100141112     C* Reperimento tabella 3TR
086200141112     C                   MOVE(P)   '3TR'         tbeCOD
086300141112     C                   EVAL      tbeKE1 = %editc(wFilTrasf:'X')
086400141112     C     K02TBE01      CHAIN     TNTBE01L
086500141113     C* se non trovata filiale su tab.3TR significa che la filiale cliente può avere le bolle
086600141113     C* confermate da qualsiasi altra filiale
086700141112     C                   IF        %found(TNTBE01L) AND
086800141112     C                             tbeATB <> 'A'
086900141112     C                   MOVEL     tbeUNI        D3TR
087000141112     C                   EVAL      wPOABI = §3TRPOABI
087100141113     C* se la filiale cliente è presente su tab. 3TR controllo:
087200141113     C* - che la filiale di conferma sia presente nella schiera di quelle abilitate
087300141113     C                   IF        %lookup(%editc(V2CFIL:'X'):SkPOABI) = 0
087400141114     C                             or SkPOABI(1) = '999'
087500141113     c                   seton                                        28
087600141113     C                   movel     err(11)       $msg
087700141112     C                   GOTO      ENDCTR
087800141112     C                   ENDIF
087900141112     C                   ENDIF
088000141112     C*
088100141113     C* controllo che se il network della filiale conferma bolle è estero
088200141113     C                   IF        wNTW_1 = 'DPD' or
088300141113     C                             wNTW_1 = 'EEX' or
088400141113     C                             wNTW_1 = 'FED'
088500141113     C* il network della filiale cliente deve essere lo stesso
088600141113     C                   IF        wNTW_2 <> wNTW_1
088700141113     c                   seton                                        28
088800141113     C                   movel     err(12)       $msg
088900141113     C                   GOTO      ENDCTR
089000141113     C                   ENDIF
089100141113     C                   ENDIF
089200141112
089300051206     c
089400051206     c     endctr        endsr
089500051206     C*----------------------------------------------------*
089600051206     C*   ESEGUITRASFE - eseguo trasferimento dati con _SQL
089700051206     C*----------------------------------------------------*
089800051206     C     EseguiTRasfe  BEGSR
089900051206     C                   EVAL      Stringasql=
090000051206     C                             'UPDATE EDIVAD1L'
090100051206     C                             +
090200051206     C                             ' SET VADFGS = ' + %editc(v2cfil:'1')
090300051206     C                             +
090400051206     C                             ' WHERE VADCMR='''+ V2CCMR+ ''' AND '
090500051206     C                             +
090600051206     C                             ' VADCCM=' + %editc(v2cccm:'4') +' AND'
090700051206     C                             +
090800051206     C                             ' VADCNT=' + %editc(v2ccnt:'3')
090900051206
091000051206     C/EXEC SQL
091100051206     C+ EXECUTE IMMEDIATE :StringaSQL
091200051206     C/END-EXEC
091300051206     C                   EVAL      Stringasql=
091400051206     C                             'UPDATE EDIVAT1L'
091500051206     C                             +
091600051206     C                             ' SET VATFGS = ' + %editc(v2cfil:'1')
091700051206     C                             +
091800051206     C                             ' WHERE VATCMR='''+ V2CCMR+ ''' AND '
091900051206     C                             +
092000051206     C                             ' VATCCM=' + %editc(v2cccm:'4') +' AND'
092100051206     C                             +
092200051206     C                             ' VATCNT=' + %editc(v2ccnt:'3')
092300051206
092400051206     C/EXEC SQL
092500051206     C+ EXECUTE IMMEDIATE :StringaSQL
092600051206     C/END-EXEC
092700051206
092800051206     C                   EVAL      Stringasql=
092900051206     C                             'UPDATE EDIVAX1L'
093000051206     C                             +
093100051206     C                             ' SET VAXFGS = ' + %editc(v2cfil:'1')
093200051206     C                             +
093300051206     C                             ' WHERE VAXCMR='''+ V2CCMR+ ''' AND '
093400051206     C                             +
093500051206     C                             ' VAXCCM=' + %editc(v2cccm:'4') +' AND'
093600051206     C                             +
093700051206     C                             ' VAXCNT=' + %editc(v2ccnt:'3')
093800051206
093900051206     C/EXEC SQL
094000051206     C+ EXECUTE IMMEDIATE :StringaSQL
094100051206     C/END-EXEC
094200051206
094300051206     C                   EVAL      Stringasql=
094400051206     C                             'UPDATE EDIVAB1L'
094500051206     C                             +
094600051206     C                             ' SET VABFGS = ' + %editc(v2cfil:'1')
094700051206     C                             +
094800051206     C                             ' WHERE VABCMR='''+ V2CCMR+ ''' AND '
094900051206     C                             +
095000051206     C                             ' VABCCM=' + %editc(v2cccm:'4') +' AND'
095100051206     C                             +
095200051206     C                             ' VABCNT=' + %editc(v2ccnt:'3')
095300051206
095400051206     C/EXEC SQL
095500051206     C+ EXECUTE IMMEDIATE :StringaSQL
095600051206     C/END-EXEC
095700051206     C
095800051206     c                   seton                                        75
095900051206     C                   UPDATE    TC85S02
096000051206     c                   setoff                                       75
096100051206     c                   endsr
096200051206     C*----------------------------------------------------*
096300051206     C*   ROLLUP01: Caricamento dati da Edivab1l           *
096400051206     C*----------------------------------------------------*
096500051206     C     ROLLUP01      BEGSR
096600960924     C*
096700960930     C     *IN23         IFEQ      '0'
096800960930     C     VABCCM        ANDLE     WCCMAL
096900960930     C*
097000030617     C                   DOW       *IN23     = *OFF    AND
097100030617     C                             VABCCM   <= WCCMAL
097200030617     C* Testo che se richiesto il CMR corrisponda
097300030617     C                   IF        (V1CCMR <> *blanks  AND
097400030617     C                              VABCMR  = V1CCMR)  OR
097500030617     C                             V1CCMR   = *blanks
097600031118     C* Al primo giro incremento subito e salvo il livello d rottura
097700031118     C                   IF        wGiro > *zeros
097800960930     C* Scrivo solo a rottura di CMR - contatore
097900960930     C     VABCNT        IFNE      WSVCNT
098000960930     C     VABCMR        ORNE      WSVCMR
098100960930     C     VABCCM        ORNE      WSVCCM
098200041123     C**   VABDTS        ORNE      WSVDTS
098300041123     C**   VABHMS        ORNE      WSVHMS
098400960930     C*  Scrittura TC85S02
098500960930     C                   EXSR      WRTS02
098600031118     C*  Inizializzo variabili d appoggio NN chiave
098700031118     C                   CLEAR                   WSVSPE
098800031118     C                   CLEAR                   WSVNCL
098900031118     C                   CLEAR                   WSVPKB
099000040713     C                   CLEAR                   wTotVADCMR
099100040713     C                   CLEAR                   wTotVATCMR
099200040713     C                   CLEAR                   wTotVAXCMR
099300031118     C*  Salvataggio nuova chiave di rottura
099400031118     C                   EXSR      SAVDAT
099500031118     C                   ELSE
099600031118     C*  Salvataggio nuova chiave di rottura
099700031118     C                   EXSR      SAVDAT
099800030617     C                   ENDIF
099900031118     C                   ELSE
100000031118     C*  Salvataggio nuova chiave di rottura
100100031118     C                   EXSR      SAVDAT
100200031118     C                   Z-ADD     1             wGiro
100300031118     C                   ENDIF
100400030617     C                   ENDIF
100500960924     C*  Lettura successiva
100600020919     c                   exsr      ReadVAB
100700030617     C                   ENDDO
100800031119     C*
100900031119     C*  Scrittura TC85S02 ultimo CMR valido
101000031119     C                   EXSR      WRTS02
101100030617     C                   ENDIF
101200040713     C*
101300040713     C*  Salvo il numero d record del sub-file caricati
101400040713     C                   Z-ADD     NRR2          wNRR2             4 0
101500040713     C*
101600960924     C                   ENDSR
101700030617     C*----------------------------------------------------*
101800041123     C*   ROLLUP04: Caricamento dati da Edivab04           *
101900030617     C*----------------------------------------------------*
102000041123     C     ROLLUP04      BEGSR
102100030617     C*
102200030617     C     *IN23         IFEQ      '0'
102300030617     C     VABCMR        ANDLE     WCMRAL
102400030617     C*
102500030617     C                   DOW       *IN23     = *OFF    AND
102600030617     C                             VABCMR   <= WCMRAL
102700030617     C* Testo che se richiesto il CMR corrisponda
102800030617     C                   IF        (V1CCCM <> *zeros  AND
102900030617     C                              VABCCM  = V1CCCM) OR
103000030617     C                             V1CCCM   = *zeros
103100031118     C* Al primo giro incremento subito e salvo il livello d rottura
103200031118     C                   IF        wGiro > *zeros
103300030617     C* Scrivo solo a rottura di CMR - contatore
103400030617     C     VABCNT        IFNE      WSVCNT
103500030617     C     VABCMR        ORNE      WSVCMR
103600030617     C     VABCCM        ORNE      WSVCCM
103700041123     C**   VABDTS        ORNE      WSVDTS
103800041123     C**   VABHMS        ORNE      WSVHMS
103900030617     C*  Scrittura TC85S02
104000030617     C                   EXSR      WRTS02
104100031118     C*  Inizializzo variabili d appoggio
104200031118     C                   CLEAR                   WSVSPE
104300031118     C                   CLEAR                   WSVNCL
104400031118     C                   CLEAR                   WSVPKB
104500040713     C                   CLEAR                   wTotVADCMR
104600040713     C                   CLEAR                   wTotVATCMR
104700040713     C                   CLEAR                   wTotVAXCMR
104800031118     C*  Salvataggio nuova chiave di rottura
104900031118     C                   EXSR      SAVDAT
105000031118     C                   ELSE
105100031118     C*  Salvataggio nuova chiave di rottura
105200031118     C                   EXSR      SAVDAT
105300030617     C                   ENDIF
105400031118     C                   ELSE
105500031118     C*  Salvataggio nuova chiave di rottura
105600031118     C                   EXSR      SAVDAT
105700031118     C                   Z-ADD     1             wGiro
105800031118     C                   ENDIF
105900030617     C                   ENDIF
106000030617     C*  Lettura successiva
106100030617     c                   exsr      ReadVAB
106200030617     C                   ENDDO
106300031119     C*
106400031119     C*  Scrittura TC85S02 ultimo CMR valido
106500031119     C                   EXSR      WRTS02
106600030617     C                   ENDIF
106700040713     C*
106800040713     C*  Salvo il numero d record del sub-file caricati
106900040713     C                   Z-ADD     NRR2          wNRR2             4 0
107000030617     C*
107100030617     C                   ENDSR
107200960930     C*----------------------------------------------------*
107300960930     C*   SAVDAT: Salvataggio campi rottura CCM-CMR-CNT    *
107400960930     C*----------------------------------------------------*
107500960930     C     SAVDAT        BEGSR
107600960930     C*
107700960930     C*  Reimposto campi per rottura totale
107800030925     C                   Z-ADD     VABFGS        WSVFGS
107900960930     C                   Z-ADD     VABCCM        WSVCCM
108000960930     C                   Z-ADD     VABCNT        WSVCNT
108100960930     C                   MOVEL     VABCMR        WSVCMR
108200960930     C                   Z-ADD     VABHMS        WSVHMS
108300960930     C                   Z-ADD     VABDTS        WSVDTS
108400960930     C                   Z-ADD     VABDCM        WSVDCM
108500031118     C                   ADD       1             WSVSPE
108600031118     C                   ADD       VABNCL        WSVNCL
108700031118     C                   ADD       VABPKB        WSVPKB
108800040719     C*
108900040719     C* Conteggia i colli in relazione al trattamento merce
109000040719     C                   z-add     1             SkJ
109100040719     C     vabCTM        lookup    SkCTM(SkJ)                             80
109200040719     C                   if        *IN80 = *on
109300040719     C                   eval      DSSKEXT = SkEXT(SkJ)
109400040719     C                   if        FLGVAD = 'S'
109500040719     C                   add       vabNCL        wTotVADCMR
109600040719     C                   endif
109700040719     C                   if        FLGVAT = 'S'
109800040719     C                   add       vabNCL        wTotVATCMR
109900040719     C                   endif
110000040719     C                   if        FLGVAX = 'S'
110100040719     C                   add       1             wTotVAXCMR
110200040719     C                   endif
110300040719     C                   endif
110400040719     C*
110500031118     C                   CLEAR                   TC85S02
110600960930     C*
110700960930     C                   ENDSR
110800960930     C*----------------------------------------------------*
110900030617     C*   WRTS02: Scrittura dati subfile                   *
111000960930     C*----------------------------------------------------*
111100960930     C     WRTS02        BEGSR
111200030926     C*
111300040713     C                   SETOFF                                       70
111400040713     C*
111500030709     C                   Z-ADD     WSVCCM        V2CCCM
111600030709     C                   MOVEL     WSVCMR        V2CCMR
111700030709     C                   Z-ADD     WSVCNT        V2CCNT
111800030709     C                   MOVEL     WSVHMS        V2CHMS
111900030709     C                   Z-ADD     WSVDTS        G02INV
112000030709     C                   MOVEL     '3'           G02ERR
112100030709     C                   CALL      'XSRDA8'
112200030709     C                   PARM                    WLBDA8
112300030709     C                   Z-ADD     G02DAT        V2CDTS
112400030709     C                   Z-ADD     WSVDCM        G02INV
112500030709     C                   MOVEL     '3'           G02ERR
112600030709     C                   CALL      'XSRDA8'
112700030709     C                   PARM                    WLBDA8
112800030709     C                   Z-ADD     G02DAT        V2CDCM
112900040716     C*
113000040716     C* Reperisco la Ragione Sociale del cliente
113100040716     C                   CLEAR                   BS69DS
113200040716     C                   CLEAR                   ACODS
113300040716     C                   CLEAR                   INDDS
113400040716     C                   CLEAR                   CLPDS
113500040716     C                   CLEAR                   CLSDS
113600040716     C                   Z-ADD     V2CCCM        I69KAC
113700040716     C                   CALL      'TIBS69R'
113800040716     C                   PARM                    BS69DS
113900040716     C                   PARM                    ACODS
114000040716     C                   PARM                    INDDS
114100040716     C                   PARM                    CLPDS
114200040716     C                   PARM                    CLSDS
114300040716     C     O69ERR        IFNE      '1'
114400040716     C                   EVAL      V2CRAG = ACORAG
114500040716     C                   ENDIF
114600040716     C*
114700040716     C* Effettuo considerazione particolare su cambi sommati a video
114800040713     C                   IF        (WSVSPE + V2CSPE) > 99999
114900040713     C                   EVAL      V2CSPE = 99999
115000040713     C                   SETON                                        59
115100040713     C                   ELSE
115200031118     C                   ADD       WSVSPE        V2CSPE
115300040713     C                   ENDIF
115400040713     C                   IF        (WSVNCL + V2CNCL) > 99999
115500040713     C                   EVAL      V2CNCL = 99999
115600040713     C                   SETON                                        63
115700040713     C                   ELSE
115800040713     C                   ADD       WSVNCL        V2CNCL
115900040713     C                   ENDIF
116000040713     C                   IF        (WSVPKB + V2CPKG) > 99999
116100040713     C                   EVAL      V2CPKG = 99999
116200040713     C                   SETON                                        64
116300040713     C                   ELSE
116400040713     C                   ADD       WSVPKB        V2CPKG
116500040713     C                   ENDIF
116600030926     C*
116700040713     C* Effettuo lettura x chiave bolla corrente x determinare quanti dettagli
116800040713     C* VAD/VAT/VAX sono presenti
116900040713     C                   Z-ADD     *zeros        V2CNDD
117000040713     C                   Z-ADD     *zeros        V2CNDT
117100040713     C                   Z-ADD     *zeros        V2CNDX
117200040713     C                   Z-ADD     *zeros        wV2CNDD           9 0
117300040713     C                   Z-ADD     *zeros        wV2CNDT           9 0
117400040713     C                   Z-ADD     *zeros        wV2CNDX           9 0
117500031204     C* VAD
117600030926     C     KVAD          SETLL     EDIVAD1L                               50
117700030926     C                   IF        %equal(EDIVAD1L)
117800030926     C     KVAD          READE     EDIVAD1L
117900030926     C                   DOW       not %eof(EDIVAD1L)
118000040713     C                   ADD       1             wV2CNDD
118100030926     C     KVAD          READE     EDIVAD1L
118200030926     C                   ENDDO
118300030926     C                   ENDIF
118400031204     C* VAT
118500031204     C     KVAD          SETLL     EDIVAT1L                               50
118600031204     C                   IF        %equal(EDIVAT1L)
118700031204     C     KVAD          READE     EDIVAT1L
118800031204     C                   DOW       not %eof(EDIVAT1L)
118900040713     C                   ADD       1             wV2CNDT
119000130315     C* SE TIPO RECORD "PDF" CONTEGGIO A FINI PACKING LIST
119100130319     C                   IF        VATTRC = 'P'
119200130315     C                   ADD       1             wV2CNDX
119300130315     C                   ENDIF
119400031204     C     KVAD          READE     EDIVAT1L
119500031204     C                   ENDDO
119600031204     C                   ENDIF
119700040713     C* VAX
119800040713     C     KVAD          SETLL     EDIVAX1L                               50
119900040713     C                   IF        %equal(EDIVAX1L)
120000040713     C     KVAD          READE     EDIVAX1L
120100040713     C                   DOW       not %eof(EDIVAX1L)
120200040713     C* Conteggio a rottura spedizione
120300040713     C                   IF        DSKEYVAXROT <> DSKEYVAX
120400040713     C                   ADD       1             wV2CNDX
120500040713     C                   EVAL      DSKEYVAXROT = DSKEYVAX
120600040713     C                   ENDIF
120700040713     C     KVAD          READE     EDIVAX1L
120800040713     C                   ENDDO
120900040713     C* Conteggio a rottura spedizione
121000040713     C                   IF        DSKEYVAXROT <> DSKEYVAX
121100040713     C                   ADD       1             wV2CNDX
121200040713     C                   EVAL      DSKEYVAXROT = DSKEYVAX
121300040713     C                   ENDIF
121400040713     C                   ENDIF
121500030926     C*
121600040713     C* Verifico se i conteggi sono superiori ai valori max conenibili nei campi
121700040713     C* conteggi dettagli a video
121800040713     C                   IF        wV2CNDD > 99999
121900040713     C                   EVAL      V2CNDD = 99999
122000040713     C                   SETON                                        60
122100040713     C                   ELSE
122200040713     C                   EVAL      V2CNDD = wV2CNDD
122300040713     C                   ENDIF
122400040713     C                   IF        wV2CNDT > 99999
122500040713     C                   EVAL      V2CNDT = 99999
122600040713     C                   SETON                                        61
122700040713     C                   ELSE
122800040713     C                   EVAL      V2CNDT = wV2CNDT
122900040713     C                   ENDIF
123000040713     C                   IF        wV2CNDX > 99999
123100040713     C                   EVAL      V2CNDX = 99999
123200040713     C                   SETON                                        62
123300040713     C                   ELSE
123400040713     C                   EVAL      V2CNDX = wV2CNDX
123500040713     C                   ENDIF
123600040713     C*
123700040713     C* Effettuo considerazioni bloccanti relative a conteggi estensioni dettaglio (VAD/VAT/VAX)
123800040713     C                   EVAL      V2CERRVAD = *blanks
123900040713     C                   EVAL      V2CERRVAT = *blanks
124000040713     C                   EVAL      V2CERRVAX = *blanks
124100040713     C* VAD
124200040713     C                   IF        wV2CNDD = *zeros AND
124300040713     C                             wTotVADCMR > *zeros
124400040713     C                   SETON                                        60
124500040713     C                   EVAL      V2CERRVAD = '1'
124600040713     C                   ENDIF
124700040713     C* VAT
124800040713     C                   IF        wV2CNDT = *zeros AND
124900040713     C                             wTotVATCMR > *zeros
125000040713     C                   SETON                                        61
125100040713     C                   EVAL      V2CERRVAT = '1'
125200040713     C                   ENDIF
125300040713     C* VAX
125400040713     C                   IF        wV2CNDX = *zeros AND
125500040713     C                             wTotVAXCMR > *zeros
125600040713     C                   SETON                                        62
125700120424
125800120502     C* controllo se esiste un bypass su questo cliente (do errore se NON c'è)
125900120502     C                   IF        %lookup(V2CCCM : CCM) = 0
126000040713     C                   EVAL      V2CERRVAX = '1'
126100120424     C                   ENDIF
126200120424
126300040713     C                   ENDIF
126400070808     C* VAX x ACCORPAMENTO
126500070808     C                   SETOFF                                       03
126600070808     C     V2CCCM        LOOKUP    KSCABDVX                               33
126700070808     C                   IF        *IN33 = *OFF AND V1CABD = 'S' AND
126800070808     C                             (wV2CNDX    > *zeros OR
126900070808     C                              wTotVAXCMR > *zeros)
127000070808     C                   SETON                                        62
127100070808     C                   SETON                                        03
127200070808     C                   EVAL      V2CERRVAX = '1'
127300070808     C                   ENDIF
127400040713     C*
127500040713     C* Salvo lo stato degli indicatori a video
127600040713     C                   EVAL      WIN59 = *IN59
127700040713     C                   EVAL      WIN60 = *IN60
127800040713     C                   EVAL      WIN61 = *IN61
127900040713     C                   EVAL      WIN62 = *IN62
128000040713     C                   EVAL      WIN63 = *IN63
128100040713     C                   EVAL      WIN64 = *IN64
128200030709     C*
128300960930     C                   ADD       1             NRR2              4 0
128400960930     C                   WRITE     TC85S02
128500040713     C                   SETOFF                                       596061
128600040713     C                   SETOFF                                       626364
128700960930     C*
128800960930     C                   ENDSR
128900031106     C*------------------------------------------------------------------------*
129000120620     C* WdwForce - chiamata a pgm per window per forzatura
129100031106     C*------------------------------------------------------------------------*
129200120619     C     WdwForce      BEGSR
129300031106     C*
129400120619     C                   EVAL      Forcing = *off
129500120620     C*
129600120620     C                   CALL      'TRTC85R1'
129700120620     C                   PARM                    Kpjba
129800120620     C                   PARM                    Exfmt
129900120620     C                   PARM                    Msg01
130000120620     C                   PARM                    Msg02
130100120620     C                   PARM                    Msg03
130200120620     C                   PARM                    Forcing
130300120620     C*
130400120620     C* tolgo memo che c'è stata una exfmt del sfl
130500120620     C                   EVAL      Exfmt = *off
130600120620     C*
130700120620     C                   ENDSR
130800120620     C*------------------------------------------------------------------------*
130900120620     C* EmailForza - Invio e-mail di conferma forzatura
131000120620     C*------------------------------------------------------------------------*
131100120620     C     EmailForza    BEGSR
131200120620     C*
131300120620     C* invio e-mail con richiesta forzatura
131400120620     C                   EVAL      peml = 'ced@brt.it'
131500120620     C                   EVAL      pcceml = *blanks
131600120620     C                   EVAL      pogg = 'Conferma da CMR - FORZATURA'
131700120620     C                   EVAL      pmsg =
131800120620     C                             'Codice cliente mittente: ' +
131900120622     C                             %editc(V2CCCM:'X')  +
132000120620     C                             ':/N' +
132100120622     C                             'Numero CMR: ' +
132200120620     C                             V2CCMR +
132300120620     C                             ':/N' +
132400120620     C                             'Utente forzatura: ' +
132500120620     C                             SDSusr +
132600120620     C                             ':/N'
132700120620     C                   call      'TIS701C1'
132800120620     C                   parm                    peml            253
132900120620     C                   parm                    pcceml          253
133000120620     C                   parm                    pogg             44
133100120620     C                   parm                    pmsg           5000
133200120620     C                   parm                    pesito            1
133300120619     C*
133400120619     C                   ENDSR
133500120619     C*------------------------------------------------------------------------*
133600120619     C* CARTAB - CARICA LE TABELLE OCCORRENTI
133700120619     C*------------------------------------------------------------------------*
133800120619     C     CARTAB        BEGSR
133900120619     C*
134000031106     C* TABELLA 3C - CLIENTI CON ABILITAZIONE ALL'ACCORPAMENTO BOLLE X MEDESIMO DESTINATARIO
134100031106     C                   Z-ADD     *ZEROS        I                 3 0
134200031106     C                   Z-ADD     CODUT         KTBute
134300031106     C                   MOVEL     '3C'          KTBcod
134400031106     C     KEYtabP       SETLL     TABEL00F
134500031106     C     KEYtabP       READE     TABEL00F                               97
134600031106     C     *IN97         DOWEQ     *OFF
134700031106     C     TBLFLG        IFEQ      *BLANKS
134800031106     C                   MOVEL     TBLUNI        DS3C
134900050610     C                   IF        §3CABD <> *blanks
135000031106     C                   ADD       1             I
135100031106     C                   MOVEL     TBLKEY        KSCABD(I)
135200070808     C                   IF        §3CAVX = 'S'
135300070808     C                   MOVEL     TBLKEY        KSCABDVX(I)
135400070808     C                   ENDIF
135500031106     C                   ENDIF
135600031106     C                   ENDIF
135700031106     C     KEYtabP       READE     TABEL00F                               97
135800031106     C                   ENDDO
135900040713     C*
136000040713     C* TATBELLA '1B' -  CODICI TRATTAMENTO MERCE IN FUNZIONE DELLE ESTENSIONI VAD/VAT/VAX
136100040713     C                   Z-ADD     *ZEROS        SkJ               3 0
136200040713     C                   Z-ADD     CODUT         KTBute
136300040713     C                   MOVEL     '1B'          KTBcod
136400040713     C     KEYtabP       SETLL     TABEL00F
136500040713     C     KEYtabP       READE     TABEL00F                               97
136600040713     C     *IN97         DOWEQ     *OFF
136700040713     C     TBLFLG        IFEQ      *BLANKS
136800040713     C                   MOVEL     TBLUNI        DS1B
136900040713     C                   ADD       1             SkJ
137000040713     C                   MOVEL     TBLKEY        SkCTM(SkJ)
137100040716     C                   CLEAR                   DSSKEXT
137200040713     C* VAD
137300040713     C                   IF        §1BFG7 = 'S'
137400040713     C                   EVAL      FLGVAD = 'S'
137500040713     C                   ENDIF
137600040713     C* VAT
137700040713     C                   IF        §1BF12 = 'E'
137800040713     C                   EVAL      FLGVAT = 'S'
137900040713     C                   ENDIF
138000040713     C* VAX
138100040713     C                   IF        §1BF16 = 'S'
138200040713     C                   EVAL      FLGVAX = 'S'
138300040713     C                   ENDIF
138400040713     C                   EVAL      SkEXT(SkJ) = DSSKEXT
138500040713     C*
138600040713     C                   ENDIF
138700040713     C     KEYtabP       READE     TABEL00F                               97
138800040713     C                   ENDDO
138900031106     C*
139000031106     C                   ENDSR
139100070802     C*----------------------------------------------------*
139200070802      /TITLE Gestione errore generico
139300070802     C*----------------------------------------------------*
139400070802     C     *pssr         BEGSR
139500070802     C*
139600070802     C* Se errore sconosciuto => emetto operazione d ROLLBACK
139700070802     C                   ROLBK
139800070820     C*
139900070820     C* Infine arresto il controllo sincronia
140000070820     C                   CALL      'TRTC85C'
140100070820     C                   PARM      'END'         OPZ               3
140200070802     C*
140300070802     C                   ENDSR     '*CANCL'
140400960924     C*----------------------------------------------------*
140500960924     C*   *INZSR: Operazioni iniziali                      *
140600960924     C*----------------------------------------------------*
140700960924     C     *INZSR        BEGSR
140800960924     C*
140900960924     C     *ENTRY        PLIST
141000960924     C                   PARM                    KPJBA
141100051130     c                   movel     kpjbu         param
141200051130     c* Se richiamo per trasferimento accendo indicatore 01
141300051130     c                   if        trasfe='S'
141400051130     c                   seton                                        01
141500051130     c                   endif
141600960924     C*  Richiamo XPARUT
141700960924     C                   Z-ADD     1             CODUT
141800960924     C                   CALL      'X§PARUT'
141900960924     C                   PARM                    UT§DSE
142000960924     C                   MOVEL     REC80         CNCR80
142100031106     C*
142200031106     C* VERIABILI RIFERITE AL DATABASE
142300031106     C     *LIKE         DEFINE    TBLKUT        KTBute
142400031106     C     *LIKE         DEFINE    TBLCOD        KTBcod
142500031106     C*
142600031106     C* CHIAVE DI LETTURA X TABEL00F - Parziale
142700031106     C     KEYtabP       KLIST
142800031106     C                   KFLD                    KTBute
142900031106     C                   KFLD                    KTBcod
143000041115     C     Kvab5         KLIST
143100041115     C                   KFLD                    kccm
143200041115     C                   KFLD                    kcmr
143300051206     C     Kvab3         KLIST
143400051206     C                   KFLD                    v2cfil
143500051206     C                   KFLD                    v2cccm
143600051206     C     Kdett         KLIST
143700051206     C                   KFLD                    v2cfil
143800051206     C                   KFLD                    v2cccm
143900051206     C                   KFLD                    v2ccmr
144000051206     C                   KFLD                    v2ccnt
144100141112     C* Definizione chiavi - TNTBE01L
144200141112     C     K02tbe01      KLIST
144300141112     C                   KFLD                    tbeCOD
144400141112     C                   KFLD                    tbeKE1
144500121127     C*
144600121127     C                   Z-ADD     1             CODUT
144700121127      * Reperimento dati utente/aziendali
144800121127     c                   exsr      sr_DatiJob
144900031106      *
145000031106      * CARICO LE TABELLE OCCORRENTI
145100031106     C                   EXSR      CARTAB
145200020919      *
145300020919     C                   MOVEL     kpjbu         KPJBUs
145400020919      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
145500020919     C                   CLEAR                   TRUL06DS
145600020919     C                   MOVE      '£1'          D06COD
145700020919     C                   MOVEL     SIMFEL        D06KEY
145800020919     C                   MOVEL     'L'           D06TLA
145900020919     C                   MOVEL     TRUL06DS      KPJBU
146000020919      *
146100020919     C                   CALL      'TRUL06R'
146200020919     C                   PARM                    KPJBA
146300020919     C                   MOVEL     KPJBU         TRUL06DS
146400020919     C                   MOVEL     kpjbus        KPJBU
146500020919      *
146600960924     C*  Ricerca capoconti
146700960924     C                   DO        50            X
146800960924     C                   MOVE      TCU(X)        TCUDS
146900960924     C     F56           IFEQ      'CG'
147000960924     C     F34           ANDEQ     '01'
147100960924     C                   Z-ADD     KCU(X)        KCI               4 0
147200960924     C                   END
147300960924     C                   END
147400960924     C                   MOVEL     RAGUT         RSUT
147500960924     C*  Chiavi di accesso
147600960924     C     KACO          KLIST
147700960924     C                   KFLD                    KKUT
147800960924     C                   KFLD                    KKCC
147900960924     C                   KFLD                    KKSC
148000961029     C     KTAB          KLIST
148100961029     C                   KFLD                    KKUT
148200961029     C                   KFLD                    KCOD1
148300030925     C     KVAD          KLIST
148400030925     C                   KFLD                    WSVFGS
148500030925     C                   KFLD                    WSVCCM
148600030925     C                   KFLD                    WSVCMR
148700030925     C                   KFLD                    WSVCNT
148800960924     C*  Definizione variabili
148900960924     C     *LIKE         DEFINE    ACOKUT        KKUT
149000960924     C     *LIKE         DEFINE    ACOKCC        KKCC
149100960924     C     *LIKE         DEFINE    ACOKSC        KKSC
149200960924     C     *LIKE         DEFINE    TABCOD        KCOD
149300961029     C     *LIKE         DEFINE    TBLCOD        KCOD1
149400960930     C     *LIKE         DEFINE    VABCCM        KCCM
149500030617     C     *LIKE         DEFINE    VABCMR        KCMR
149600960930     C     *LIKE         DEFINE    VABCCM        WSVCCM
149700960930     C     *LIKE         DEFINE    VABCNT        WSVCNT
149800960930     C     *LIKE         DEFINE    VABCMR        WSVCMR
149900960930     C     *LIKE         DEFINE    VABHMS        WSVHMS
150000960930     C     *LIKE         DEFINE    VABDTS        WSVDTS
150100960930     C     *LIKE         DEFINE    VABDCM        WSVDCM
150200030925     C     *LIKE         DEFINE    VABFGS        WSVFGS
150300031118     C     *LIKE         DEFINE    VABNCL        WSVNCL
150400031118     C     *LIKE         DEFINE    VABNCL        WSVSPE
150500031118     C     *LIKE         DEFINE    VABPKB        WSVPKB
150600960924     C*  Carico in schiera codici tabella - PT -
150700961029     C                   Z-ADD     0             X                 4 0
150800960924     C                   Z-ADD     0             KSC
150900960924     C                   MOVEL     'PT'          KCOD
151000960924     C     KCOD          CHAIN     EDTAB01L                           31
151100960924     C     *IN31         DOWEQ     '0'
151200960924     C     TABFLG        IFEQ      ' '
151300020919     C                   MOVEL     TABUNI        DSPT
151400020919      * solo se compresa nelle linee di partenza del terminal
151500020919     c     §ptlnp        lookup    Lin                                    32
151600020919     c                   if        *in32 = *on
151700960924     C                   ADD       1             X
151800961024     C                   MOVEL     TABKEY        CPT(X)
151900961024     C                   Z-ADD     §PTKSC        KSC(X)
152000020919     c                   end
152100960924     C                   END
152200960924     C     KCOD          READE     EDTAB01L                               31
152300960924     C                   END
152400021015      *
152500021015      * Salva il contatore
152600021015     c                   z-add     x             xx                3 0
152700000915      *
152800021015      * Carica Clienti da tabella CL
152900021015     C                   MOVEL     'CL'          TABCOD
153000021015     C     TABCOD        CHAIN     EDTAB01L                           30
153100021015     C     *IN30         DOWEQ     '0'
153200021015     C     TABFLG        IFEQ      ' '
153300021015     C                   MOVEL     TABKEY        CODCL             7 0
153400021015     C     CODCL         LOOKUP    ksc                                    32
153500021015     c                   if        *in32 = *on
153600021015     c                   movel     tabuni        lnpCL             3 0
153700021015     c     LnpCL         lookup    Lin                                    32
153800021015     c                   if        *in32 = *on
153900021015     c                   add       1             xx
154000021015     C                   MOVEL     TABUNI        KSC(xx)
154100021015     C                   endif
154200021015     c                   endif
154300021015     C                   END
154400021015     C     TABCOD        READE     EDTAB01L                               30
154500021015     C                   END
154600021015      *
154700021015      * Salva il contatore
154800021015     c                   z-add     xx            x
154900021015      *
155000021015      * Caricamento Tabella Depot DEP
155100000915     C     'DEP'         CHAIN     TNTBE01L                           31
155200000915      *
155300000915     C     *IN31         DOWEQ     '0'
155400000915      * Se il S.I. è indicato deve essere uguale al mio
155500000915     C     TBEATB        IFEQ      ' '
155600000915     C     TBEKE2        ANDEQ     'I'
155700000915     C     TBESIF        IFEQ      KNSIF
155800000915     C     TBESIF        OREQ      *BLANK
155900020919     C                   MOVEL     TBEUNI        DDEP
156000020919      * solo se compresa nelle linee di partenza del terminal
156100020919     c     §dplnp        lookup    Lin                                    32
156200020919     c                   if        *in32 = *on
156300000918     C                   ADD       1             X
156400000915     C                   Z-ADD     §DPKSC        KSC(X)
156500020919     c                   end
156600000915     C                   END
156700000915     C                   END
156800000915     C     'DEP'         READE     TNTBE01L                               31
156900000915     C                   ENDDO
157000120424      *
157100120424      * Caricamento Tabella Depot 3BC bypass ctrl
157200120424     C                   Z-ADD     0             wY
157300120424     C     '3BC'         CHAIN     TNTBE01L                           31
157400120424      *
157500120424     C     *IN31         DOWEQ     '0'
157600120424      * Se il S.I. è indicato deve essere uguale al mio
157700120424     C     TBEATB        IFEQ      ' '
157800120502     C     TBEKE2        ANDEQ     *blank
157900120424     C     TBESIF        IFEQ      KNSIF
158000120424     C     TBESIF        OREQ      *BLANK
158100120424     C                   MOVEL     TBEUNI        D3BC
158200120424      * salvo il  cliente solo se il bypass relativo al VAX per CMR è attivo
158300120424     C                   IF        §3BCCCC = 'S'
158400120424     c                   if        %lookup(%dec(%trim(%subst(TBEKe1 : 1 : 7))
158500120502     C                              : 7 : 0) : CCM) = 0
158600120424     C                   ADD       1             wY
158700120502     C                   EVAL      CCM(wY) =
158800120424     C                             %dec(%trim(%subst(TBEKe1 : 1 : 7)) : 7 : 0)
158900120424     c                   end
159000120424     C                   ENDIF
159100120424     C                   END
159200120424     C                   END
159300120502     C     '3BC'         READE     TNTBE01L                               31
159400120424     C                   ENDDO
159500000915      *
159600961024     C                   MOVEL     'PU'          TABCOD
159700961024     C     TABCOD        CHAIN     EDTAB01L                           30
159800961024     C     *IN30         DOWEQ     '0'
159900961024     C     TABFLG        IFEQ      ' '
160000961024     C                   MOVEL     TABKEY        CODPU            35
160100961024     C                   Z-ADD     1             X
160200961024     C     CODPU         LOOKUP    CPT(X)                                 32
160300961024     C     *IN32         IFEQ      '1'
160400961024     C                   MOVEL     TABUNI        DPU(X)
160500961024     C                   END
160600961024     C                   END
160700961024     C     TABCOD        READE     EDTAB01L                               30
160800961024     C                   END
160900960924     C*  Inizializzo variabili
161000960924     C                   MOVEL     'N'           WFINE             1
161100960924     C*
161200960924     C                   ENDSR
161300121127
161400121127      *---------------------------------------------------------------*
161500121127      *?Reperimento dati del job (Utente/Operativi)                  ?*
161600121127      *---------------------------------------------------------------*
161700121127     c     sr_DatiJob    BEGSR
161800121127      *
161900121127     c     *dtaara       define    §azute        AZUTEds
162000121127     c     *dtaara       define    §datiute      dDatiUte
162100121127      *
162200121127     c                   in(E)     *dtaara
162300121127     c                   IF        %Error or RSUT = *blanks
162400121127     c                   clear                   TIBS34ds
162500121127     c                   call      'TIBS34R'
162600121127     c                   parm                    TIBS34ds
162700121127     c                   in        *dtaara
162800121127     c                   ENDIF
162900121127      *
163000121127     c                   ENDSR
163100960924**
163200960924Conto cliente inesistente o annullato
163300960924Conto cliente non presente nella tabella dei partner esteri
163400960924Effettuare almeno una scelta
163500031110Il cliente selezionato non è abilitato al raggruppamento bolle ("3C")
163600031120Per raggruppamento bolle selezionare CMR di un solo cliente
163700051130Opzione errata
163800121022Inserire la Filiale che deve ricevere i dati !!
163900141113Impossibile trasferire i dati: presente il nm. CMR nella filiale di ricezione
164000121119La Filiale che deve ricevere i dati non è in organigramma
164100121126La Filiale che deve ricevere i dati non è tra i network possibili
164200141113Filiale di conferma bolle NON abilitata per il cliente, interpellare il POC
164300141113Network Filiale di conf. bolle diverso da quello cliente, interpellare il POC
164400051130**
164500051213 *** Conferma bolle per numero CMR ***
164600121022 * Trasmissione CMR ad altra filiale *

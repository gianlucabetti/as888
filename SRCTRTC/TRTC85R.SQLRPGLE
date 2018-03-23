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
002001171219     FEdivad11l IF   E           K DISK
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
004600171220     D SkEXT           S              4    DIM(100) INZ(*blanks)
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
011601171220     D  FLGVAD10                      1    INZ(*blanks)
011700040713     D*------------------
011800040713     D* VARIABILI D WRK
011900040713     D*-------------------
012000040713     D DSKEYVAXROT     S                   LIKE(DSKEYVAX) INZ(*blanks)
012100040713     D wTotVADCMR      S              9  0 INZ(*zeros)
012101171220     D wTotVA1CMR      S              9  0 INZ(*zeros)
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
014101171221     d s_vadncd        s                   like (vadncd)
014200121127
014300121127      * - Parametri per richiamo al pgm di controllo profilo utenti
014400121127     d TIBS34ds      e ds
014500121127      * - Ds di riferimento al file esterno AzUte00F
014600121127     d AZUTEds       e ds                  ExtName(AzUte00F)
014700121127      * - Ds per dati organigramma
014800121127     d dDatiUte      e ds
014900121127     d  simfel_dut   e                     extfld(simfel)
015000160126     D GEO_in_Sospeso  C                   '§DPD_IN_SOSPESO'
015100960924     C*---------------------------------------------------------------------*
015200960924     C* Ciclo principale
015300960924     C*---------------------------------------------------------------------*
015400070820     C*
015500070820     C*  Avvio sempre e cmq il controllo sincronia
015600070820     C                   CALL      'TRTC85C'
015700070820     C                   PARM      'STR'         OPZ               3
015800070820     C*
015900960924     C* Inizializzo prima videata
016000960924     C                   EXSR      INZ01
016100960924      * Loop gestione videate
016200960924     C     WFINE         DOWEQ     'N'
016300960924     C     WTPVID        CASEQ     '1D'          GESD01                         Sce.cliente
016400960924     C     WTPVID        CASEQ     '2S'          GESS02                         Subfile
016500960924     C     WTPVID        CASEQ     '2D'          GESD02                         Subfile vuoto
016600960924     C                   END
016700960924     C                   END
016800960924      * Fine Lavoro
016900960924     C     FINE          TAG
017000070802     C*
017100070802     C* Infine arresto il controllo sincronia
017200070802     C                   CALL      'TRTC85C'
017300070802     C                   PARM      'END'         OPZ               3
017400070802     C*
017500960924     C                   SETON                                        LR
017600960924     C*---------------------------------------------------------------*
017700960924     C*  INZ01: Inizializzo prima videata
017800960924     C*---------------------------------------------------------------*
017900960924     C     INZ01         BEGSR
018000960924     C*
018100960924     C                   SETOFF                                       2840
018200960924     C                   MOVEL     *BLANKS       $MSG
018300960924     C                   Z-ADD     0             V1CCCM
018400960924     C                   MOVEL     NOMPGM        V1CPGM
018500051213     C  n01              MOVE      tes(1)        V1Ctes
018600051213     C   01              MOVE      tes(2)        V1Ctes
018700960924     C                   MOVEL     '1D'          WTPVID
018800030617     C                   MOVEL     'K'           V1CORD
018900031106     C                   MOVEL     *BLANKS       V1CCMR
019000031106     C                   MOVEL     'N'           V1CABD
019100960924     C*
019200960924     C                   ENDSR
019300960924     C*---------------------------------------------------------------*
019400960924     C*  INZ02: Inizializzo seconda videata
019500960924     C*---------------------------------------------------------------*
019600960924     C     INZ02         BEGSR
019700960924     C*
019800070820     C*  Avvio sempre e cmq il controllo sincronia
019900031113     C                   CALL      'TRTC85C'
020000070820     C                   PARM      'EXE'         OPZ               3
020100031113     C*
020200960924     C                   SETOFF                                       28
020300960924     C                   MOVEL     *BLANKS       $MSG
020400051206     c                   clear                   v2cfil
020500960924     C                   MOVEL     NOMPGM        V2CPGM
020600051213     C  n01              MOVE      tes(1)        V2Ctes
020700051213     C   01              MOVE      tes(2)        V2Ctes
020800960924     C*  Pulisco subfile
020900960926     C                   Z-ADD     0             NRR2
021000960924     C                   SETOFF                                       2021
021100960924     C                   WRITE     TC85C02
021200960924     C                   SETON                                        2021
021300031118     C*  Inizializzo variabili d appoggio e d wrk
021400031106     C                   CLEAR                   WSVCNT
021500031106     C                   CLEAR                   WSVCMR
021600031106     C                   CLEAR                   WSVCCM
021700040405     C                   CLEAR                   WSVDTS
021800040405     C                   CLEAR                   WSVHMS
021900031118     C                   CLEAR                   WSVSPE
022000031118     C                   CLEAR                   WSVNCL
022100031118     C                   CLEAR                   WSVPKB
022200040713     C                   CLEAR                   wTotVADCMR
022201171220     C                   CLEAR                   wTotVA1CMR
022300040713     C                   CLEAR                   wTotVATCMR
022400040713     C                   CLEAR                   wTotVAXCMR
022500031118     C                   Z-ADD     *zeros        wGiro             1 0
022600030617      *
022700030617      *  Se ordinamento x cliente
022800041115     C                   IF        *in54
022900960924      *  ... se ho selezionato un codice partner carico solo quello
023000960924     C     V1CCCM        IFNE      0
023100960926     C                   Z-ADD     V1CCCM        KCCM
023200960926     C                   Z-ADD     V1CCCM        WCCMAL
023300960924     C                   ELSE
023400960924      *  ... altrimenti comincio leggere dal primo codice
023500960924     C                   Z-ADD     0             KCCM
023600960924     C                   Z-ADD     9999999       WCCMAL            7 0
023700030617     C                   ENDIF
023800041115     c* Verifico se presente anche cmr per fare lettura con 2 campi chiave
023900041115     C     V1CCMR        IFNE      *blanks
024000041115     C                   MOVEL(P)  V1CCMR        KCMR
024100041115     C                   ELSE
024200041115     C                   MOVEL     *blanks       KCMR
024300041115     C                   ENDIF
024400030617     C                   ENDIF
024500030617      *
024600030617      *  Se ordinamento x CMR
024700041115     C                   IF        *in55
024800030617      *  ... se ho selezionato un numero CMR carico solo quello
024900030617     C     V1CCMR        IFNE      *blanks
025000030617     C                   MOVEL(P)  V1CCMR        KCMR
025100030617     C                   MOVEL(P)  V1CCMR        WCMRAL
025200030617     C                   ELSE
025300030617      *  ... altrimenti comincio leggere dal primo numero CMR
025400030617     C                   MOVEL     *blanks       KCMR
025500030617     C                   MOVEL     *hival        WCMRAL           35
025600030617     C                   ENDIF
025700030617     C                   ENDIF
025800960924     C*  Prima lettura
025900041115     c                   select
026000041115     c                   when      *iN54   and kcmr<>*BLANKS
026100041123     C     KVAB5         SETLL     Edivab1L
026200041115     C                   WHEN      *IN54
026300041123     C     KCCM          SETLL     Edivab1L
026400041115     C                   WHEN      *IN55
026500041123     C     KCMR          SETLL     Edivab4L
026600041115     C                   ENDSL
026700041115     C
026800020919     c                   exsr      ReadVAB
026900041115     C
027000960924      *  Eseguo caricamento prima pagina
027100041123     C   54              EXSR      ROLLUP01
027200041123     C   55              EXSR      ROLLUP04
027300960924      *  Se non ho dati da modificare emetto subfile vuoto
027400960924     C     NRR2          IFEQ      0
027500960924     C                   MOVEL     '2D'          WTPVID            2
027600960924     C                   ELSE
027700960924     C                   MOVEL     '2S'          WTPVID
027800960924     C                   END
027900960924     C*
028000960924     C                   ENDSR
028100960924     C*---------------------------------------------------------------*
028200020919     C*  READVAB: Legge il record di EDIVAB
028300960924     C*---------------------------------------------------------------*
028400020919     C     ReadVAB       BEGSR
028500960924     C*
028600020919     c     rileggi       tag
028700041115     c
028800041115     C                   SELECT
028900041115     C                   WHEN      *IN54 AND KCCM>0 AND KCMR<>*BLANKS
029000041123     C     KVAB5         READE     Edivab1L                               23
029100041115     C                   WHEN      *IN54 AND KCCM>0
029200041123     C     kccm          READe     Edivab1L                               23
029300041115     C                   WHEN      *IN54
029400041123     C                   READ      Edivab1L                               23
029500041115     C                   WHEN      *IN55 AND KCMR<>*BLANKS
029600041123     C     kcmr          READe     Edivab4L                               23
029700041115     C                   WHEN      *IN55
029800041123     C                   READ      Edivab4L                               23
029900041115     c                   endSL
030000020919     C*
030100020919     C*  controlla se non è compresa fra le linee gestite dal terminal
030200020919     C*   quindi legge nuovo record.
030300030617     c                   if        *in54 = *on        AND                       * ordinam. edivab1l
030400041123     c                             not %eof(Edivab1L)
030500020919     c     vabfgs        cabeq     0             rileggi
030600020919     c     vabfgs        lookup    lin                                    32
030700020919     c     *in32         cabeq     *off          rileggi
030800030617     c                   endif
030900030617     c                   if        *in55 = *on        AND                       * ordinam. edivab4l
031000041123     c                             not %eof(Edivab4L)
031100030617     c     vabfgs        cabeq     0             rileggi
031200030617     c     vabfgs        lookup    lin                                    32
031300030617     c     *in32         cabeq     *off          rileggi
031400030617     c                   endif
031500031106     C*
031600130103     C* se sono entrato mediante l'azione TC8A, non servono test sull'ABD
031700130103     C* il rcd letto verrà scritto sul sfl
031800130103     C                   IF        %subst(KPJBU : 1 : 1) <> 'S'
031900031106     C*  controlla se il cliente è abilitato in tabella 3C all'accorpamento bolle x
032000031106     C*  medesimo destinatario in relazione al relativo parametro richiesto a video
032100041115     C                   if        *in54 = *on        AND                       * ordinam. edivab5l
032200041123     C                             not %eof(Edivab1L)
032300031106     C     vabCCM        lookup    KSCABD                                 32
032400031106     C                   if        (*in32 = *off AND V1CABD = 'N') OR
032500031106     C                             (*in32 = *on  AND V1CABD = 'S')
032600031106     C                   else
032700031106     C                   goto      rileggi
032800031106     C                   endif
032900031106     C                   endif
033000041115     C                   if        *in55 = *on        AND                       * ordinam. edivab6l
033100041123     C                             not %eof(Edivab4L)
033200031106     C     vabCCM        lookup    KSCABD                                 32
033300031106     C                   if        (*in32 = *off AND V1CABD = 'N') OR
033400031106     C                             (*in32 = *on  AND V1CABD = 'S')
033500031106     C                   else
033600031106     C                   goto      rileggi
033700031106     C                   endif
033800031106     C                   endif
033900130103     C*
034000130103     C                   ENDIF
034100020919     C*
034200160126     C*  le spedizioni DPD in SOSPESO arrivate con il GEODATA senza SCAN05
034300160126     C*   Essendo in sospeso devono rimanere NASCOSTE e NON visibili.
034400160229     C*  quindi NON le deve caricare sul SFL.
034401160229     c                   if        *in54 = *on AND %eof(Edivab1L)               * ordinam. edivab1l
034402160229     c                                or
034403160229     c                             *in55 = *on AND %eof(Edivab4L)               * ordinam. edivab1l
034404160229     C                   else
034500160126     C                   if        vabCMR = GEO_in_Sospeso
034600160126     C                   goto      rileggi
034700160126     C                   endif
034701160229     C                   end
034800160126     C*
034900020919     C                   ENDSR
035000020919     C*---------------------------------------------------------------*
035100020919     C*  GESD01: Gestione prima videata
035200020919     C*---------------------------------------------------------------*
035300020919     C     GESD01        BEGSR
035400020919     C*
035500960924     C                   EXFMT     TC85D01
035600960924     C                   SETOFF                                       2840
035700960924     C*  Fine Lavoro
035800960924     C     *INKC         IFEQ      '1'
035900960924     C                   MOVEL     'S'           WFINE
036000960924     C                   GOTO      FINVD1
036100960924     C                   END
036200960924     C*  Controlli
036300960924     C                   EXSR      CTR01
036400960924     C*  Caricamento subfile
036500960924     C  N28              EXSR      INZ02
036600960924     C*
036700960924     C     FINVD1        ENDSR
036800960924     C*---------------------------------------------------------------*
036900960924     C*  GESS02: Gestione subfile pieno
037000960924     C*---------------------------------------------------------------*
037100960924     C     GESS02        BEGSR
037200960924     C*
037300031110     C     EMIVS2        TAG
037400051201     C                   WRITE     TC85Z02
037500960924     C                   EXFMT     TC85C02
037600120620     C* memo che c'è stata una exfmt del sfl
037700120620     C                   EVAL      Exfmt = *on
037800031110     C*  Spengo indicatore di errore sui record del sub-file
037900031110     C                   SETOFF                                       28
038000960924     C*  Fine Lavoro
038100960924     C     *INKC         IFEQ      '1'
038200960924     C                   MOVEL     'S'           WFINE
038300960924     C                   GOTO      FINVS2
038400960924     C                   END
038500960924     C*  Ritorno
038600960924     C     *INKL         IFEQ      '1'
038700960924     C                   EXSR      INZ01
038800960924     C                   GOTO      FINVS2
038900960924     C                   END
039000960924     C*  Controlli
039100960930     C                   EXSR      CTR02
039200031110     C   28              GOTO      EMIVS2
039300960924     C*
039400960924     C     FINVS2        ENDSR
039500960924     C*---------------------------------------------------------------*
039600960924     C*  GESD02: Gestione subfile vuoto
039700960924     C*---------------------------------------------------------------*
039800960924     C     GESD02        BEGSR
039900960924     C*
040000960924     C                   EXFMT     TC85D02
040100960924     C*  Fine Lavoro
040200961028     C     *INKC         IFEQ      '1'
040300961028     C                   MOVEL     'S'           WFINE
040400960924     C                   GOTO      FINVD2
040500960924     C                   END
040600960924     C*  Ritorno
040700960924     C     *INKL         IFEQ      '1'
040800960924     C                   EXSR      INZ01
040900960924     C                   GOTO      FINVD2
041000960924     C                   END
041100960924     C*
041200960924     C     FINVD2        ENDSR
041300960924     C*---------------------------------------------------------------*
041400960924     C*  CTR01: Controlli prima videata
041500960924     C*---------------------------------------------------------------*
041600960924     C     CTR01         BEGSR
041700031106     C*
041800031106     C* Inizializzo variabili d wrk
041900031106     C                   CLEAR                   WDTA
042000960924     C*
042100960924     C*  Se immesso il codice cliente ...
042200960924     C     V1CCCM        IFNE      0
042300960924     C*  Controllo se esiste su CNACO
042400960924     C                   Z-ADD     1             KKUT
042500960924     C                   Z-ADD     KCI           KKCC
042600960924     C                   Z-ADD     V1CCCM        KKSC
042700960924     C     KACO          CHAIN     CNACO00F                           30
042800960924     C     *IN30         IFEQ      '1'
042900960924     C     ACOFLG        ORNE      ' '
043000960924     C                   SETON                                        2840
043100960924     C                   MOVEL     ERR(1)        $MSG
043200960924     C                   GOTO      FINCT1
043300960924     C                   END
043400960924     C*  Controllo se è un partner
043500961024     C                   Z-ADD     1             XY                3 0
043600970602     C     V1CCCM        IFEQ      3200004
043700970602     C     0050743       LOOKUP    KSC(XY)                                32
043800970602     C                   ELSE
043900961024     C     V1CCCM        LOOKUP    KSC(XY)                                32
044000970602     C                   END
044100960924     C     *IN32         IFEQ      '0'
044200031106     C***                SETON                                        2840
044300031106     C***                MOVEL     ERR(2)        $MSG
044400031106     C***                GOTO      FINCT1
044500961024     C                   ELSE
044600961024     C                   MOVEL     DPU(XY)       DSPU
044700961024     C                   MOVEL     §PUDTA        WDTA              1
044800960924     C                   END
044900960924     C                   END
045000961029     C*  Reperisco codice cliente x lnp bolla
045100031107     C***                Z-ADD     §PTKSC        WKSC              7 0
045200030617     C*  Verifico il tipo d ordinamento richiesto
045300030617     C                   SETOFF                                       5455
045400041115     c                   select
045500041115     c* Se selezionato cod cliente ordinamento come se fosse per cli
045600041115     c* anche per CMR, tanto la lf dopo il cliente ha cmr
045700041115     C                   when      v1cccm>0
045800030617     C                   SETON                                        54
045900041115     c* Se selezionato CMR ordinamento come se fosse per CMR
046000041115     c* anche per cliente, tanto la lf dopo il cmr ha il cliente
046100041115     C                   when      v1ccmr<>*blanks
046200041115     C                   SETON                                        55
046300041115     c* altrimenti il tipo ordinamento
046400041115     C                   when      V1CORD = 'C'
046500030617     C                   SETON                                        55
046600041115     c                   other
046700041115     C                   SETON                                        54
046800041115     C                   ENDsl
046900030617     C*
047000960924     C     FINCT1        ENDSR
047100960924     C*---------------------------------------------------------------*
047200960924     C*  CTR02: Controlli seconda videata
047300960924     C*---------------------------------------------------------------*
047400960924     C     CTR02         BEGSR
047500040713     C*
047600040713     C                   SETOFF                                       70
047700031118     C*
047800031118     C*  Inizializzo la variabile d wrk relativa al controllo: accorpamento possibile solo su
047900031118     C*  singolo cliente
048000031118     C                   Z-ADD     *zeros        wCCM              7 0
048100960924     C*
048200960924     C*  Controllo selezioni effettuate
048300960924     C                   MOVEL     'N'           WSCELT            1
048400960930     C                   MOVEL     'N'           WSTAMP            1
048500031118     C                   Z-ADD     *ZEROS        KEYNRR2           4 0
048600051201    1C     1             DO        wNRR2         KEYNRR2
048700031118     C     KEYNRR2       CHAIN     TC85S02
048800040713     C*
048900040713     C* Reimposto gli indicatori in funzione dei campi hiddend d salvataggio
049000040713     C                   EVAL      *IN59 = WIN59
049100040713     C                   EVAL      *IN60 = WIN60
049200040713     C                   EVAL      *IN61 = WIN61
049300040713     C                   EVAL      *IN62 = WIN62
049400040713     C                   EVAL      *IN63 = WIN63
049500040713     C                   EVAL      *IN64 = WIN64
049501171220     C                   EVAL      *IN65 = WIN65
049600040713     C*
049700051201    2C     V2CSCE        IFNE      ' '
049800051130     c* controllo se opzione errata
049900051201    3c                   if        v2csce='7' and not *in01
050000051130     C                   MOVEL     ERR(6)        $MSG
050100051130     C                   SETON                                        28
050200051201    3c                   endif
050300051201    3c                   if        v2csce<>'7' and *in01
050400051130     C                   MOVEL     ERR(6)        $MSG
050500051130     C                   SETON                                        28
050600051201    3c                   endif
050700051130     c
050800051201    3c                   if        *in28
050900051130     c                   seton                                        70
051000051130     C                   UPDATE    TC85S02
051100051130     c                   setoff                                       70
051200051130     C                   LEAVE
051300051201    3c                   endif
051400051130     c
051500960924     C                   MOVEL     'S'           WSCELT
051600960924     C*  Richiamo pgm per esecuzione scrittura VAB
051700960924     C                   CLEAR                   TRTC86
051800961024     C                   MOVEL     WDTA          D86DTA
051900040713     C*
052000051201     C* X opzioni 1 e 9 d conferma bolle proseguo solo se no errori su estensioni
052100051201    3C                   IF        V2CSCE = '1' OR  v2csce='7'
052200051201     C                             or V2CSCE = '9'
052300120619
052400120619     C* se c'è un errore
052500120619    4C                   IF        (V2CERRVAD <> *blanks OR
052501171221    4C                              V2CERRVAD1 <> *blanks OR
052600120619     C                              V2CERRVAT <> *blanks OR
052700120619     C                              V2CERRVAX <> *blanks   )
052800120619     C* ma l'utente è un EDP*
052900120619     C                             and %subst(SDSusr : 1 : 3) = 'EDP'
053000120619     C* chiedo se voglio forzare
053100120622     C                   EVAL      Msg01 = 'Dettagli mancanti rispetto a CTM: +
053200120622     C                             F6 per forzare'
053300120620     C                   EVAL      Msg02 = 'Codice cliente mittente: ' +
053400120622     C                                     %editc(V2CCCM:'X')  +
053500120622     C                                     '  Numero CMR: ' +
053600120622     C                                     V2CCMR
053700120620     C                   EVAL      Msg03 = 'Utente forzatura: ' +
053800120619     C                                        SDSusr
053900120619     C                   EXSR      WdwForce
054000120619     C* se richiesta forzatura
054100120619     C                   IF        Forcing = *on
054200120619     C                   EVAL      V2CERRVAD = *blank
054201171221     C                   EVAL      V2CERRVAD1= *blank
054300120619     C                   EVAL      V2CERRVAT = *blank
054400120619     C                   EVAL      V2CERRVAX = *blank
054500120620     C                   EVAL      S02Forced = *on
054600120620     C                   UPDATE    TC85S02
054700120620     C* mi riposiziono sul rcd di sfl letto
054800120620     C     KEYNRR2       CHAIN     TC85S02
054900120619     C                   ENDIF
055000120619     C                   ENDIF
055100120619
055200051201    4C                   IF        V2CERRVAD <> *blanks OR
055201171221     C                             V2CERRVAD1<> *blanks OR
055300040713     C                             V2CERRVAT <> *blanks OR
055400040713     C                             V2CERRVAX <> *blanks
055500040713     C* Emetto errori relativi a dettagli mancanti
055600040713     C                   SETON                                        2870
055700051201     C  n01              EVAL      $MSG = 'Impossibile confermare, dettagli ' +
055800040713     C                             'mancanti:'
055900051201     C   01              EVAL      $MSG = 'Impossibile TRASFERIRE, dettagli ' +
056000051201     C                             'mancanti:'
056100070808     C   03              EVAL      $MSG = 'Impossibile accorpare in presenza' +
056200070808     C                             ' di'
056300051201    5C                   IF        V2CERRVAD <> *blanks
056400040713     C                   EVAL      $MSG = %trim($MSG) + ' VAD'
056500051201    5C                   ENDIF
056501171221    5C                   IF        V2CERRVAD1<> *blanks
056502171221     C                   EVAL      $MSG = %trim($MSG) + ' VAD10'
056503171221    5C                   ENDIF
056600051201    5C                   IF        V2CERRVAT <> *blanks
056700040713     C                   EVAL      $MSG = %trim($MSG) + ' VAT'
056800051201    5C                   ENDIF
056900051201    5C                   IF        V2CERRVAX <> *blanks
057000040713     C                   EVAL      $MSG = %trim($MSG) + ' VAX'
057100051201    5C                   ENDIF
057200040713     C                   UPDATE    TC85S02
057300040713     C                   LEAVE
057400051201    4C                   ENDIF
057500051201    3C                   ENDIF
057600040713     C*
057700051201     c* elaboro il trasferimento: ulteriori controlli
057800051201     c                   if        v2csce='7'
057900051201     c                   exsr      contrTrasfe
058000051206     c
058100051206     c                   if        *in28
058200051206     C                   UPDATE    TC85S02
058300051206     c                   setoff                                       70
058400051206     C                   LEAVE
058500051206     c                   endif
058600051206     c
058700051206     c   kf              exsr      EseguiTrasfe
058800120620     C* se c'è stata conferma e il rcd era forzato
058900120620     C                   IF        *inKF = *on and S02Forced = *on
059000120620     C* invio email di conferma forzatura
059100120620     C                   EXSR      EmailForza
059200120620     C                   ENDIF
059300120620     C*
059400051206     c                   iter
059500051201     c                   endif
059600051201     c
059700051201    3C     V2CSCE        IFEQ      '1'
059800960924     C                   MOVEL     'E'           D86MOD
059900960927     C                   MOVEL     ' '           D86STA
060000051201    3C                   ENDIF
060100960924     C*  Richiamo pgm per stampa dati
060200091124    3C**   V2CSCE        IFEQ      '6'
060300091124     C**                 MOVEL     'S'           WSTAMP            1
060400091124     C**                 MOVEL     *BLANKS       D86MOD
060500091124     C**                 MOVEL     'S'           D86STA
060600091124    3C**                 ENDIF
060700020919     C*  Richiamo pgm per cancellazione Edivab
060800051201    3C     V2CSCE        IFEQ      '4'
060900960924     C                   MOVEL     'C'           D86MOD
061000960924     C                   MOVEL     *BLANKS       D86STA
061100051201    3C                   ENDIF
061200091124     c
061300031110     C*  Se opzione diversa da 9 => eseguo *pgm standard, altrimenti eseguo *pgm x raggruppamento
061400031113     C*  bolle e scrittura FIVAB/FIVAD
061500091124     c                   select
061600091124    3C     V2CSCE        wheneq    '9'
061700031110     C*  Verifico che x opzione 9 il cliente sia abilitato nella tabella 3C all'accorpamento
061800031113     C*  x medesimo destinatario
061900031110     C     V2CCCM        LOOKUP    KSCABD                                 32
062000051201    4C                   IF        (*IN32 = *OFF) OR
062100031201     C*  e che sia stato scelto ingresso x accorpamento bolle, altrimenti do errore
062200031201     C                             V1CABD <> 'S'                                * ridondante
062300031110     C                   MOVEL     ERR(4)        $MSG
062400040713     C                   SETON                                        2870
062500040713     C                   UPDATE    TC85S02
062600031113     C                   LEAVE
062700051201    4C                   ENDIF
062800031118     C* Verifico se impostata opzioine 9 (raggruppamento) su clienti diversi (ERRORE)
062900051201    4C                   IF        wCCM > *zeros
063000051201    5C                   IF        V2CCCM <> wCCM
063100031118     C                   MOVEL     ERR(5)        $MSG
063200040713     C                   SETON                                        28
063300051201    5C                   ENDIF
063400051201   x4C                   ELSE
063500031118     C                   Z-ADD     V2CCCM        wCCM
063600051201    4C                   ENDIF
063700091124
063800091124     c* Opzione di stampa --> richiamo con RT per poi avere la stampa di riepilogo
063900091124    3C     V2CSCE        wheneq    '6'
064000091124     C                   MOVEL     'S'           WSTAMP            1
064100091124     C                   MOVEL     *BLANKS       D86MOD
064200091124     C                   MOVEL     'S'           D86STA
064300091124     C                   MOVEL     V2CCMR        D86CMR
064400091124     C                   MOVEL     'RT'          D86CHI
064500091124     C                   Z-ADD     V2CCNT        D86CNT
064600091124     C                   Z-ADD     V2CCCM        D86CCM
064700091124     C                   MOVEL     TRTC86        KPJBU
064800091124     C   KF              CALL      'TRTC86R1'
064900091124     C                   PARM                    KPJBA
065000120620     C* se c'è stata conferma e il rcd era forzato
065100120620     C                   IF        *inKF = *on and S02Forced = *on
065200120620     C* invio email di conferma forzatura
065300120620     C                   EXSR      EmailForza
065400120620     C                   ENDIF
065500120620     C*
065600091124     c
065700091124    3C     V2CSCE        whenne    ' '
065800960924     C                   MOVEL     V2CCMR        D86CMR
065900091124     c* richiamo con LR perchè carica i dati da elaborare nella INZSR
066000030617     C                   MOVEL     'LR'          D86CHI
066100960924     C                   Z-ADD     V2CCNT        D86CNT
066200961028     C                   Z-ADD     V2CCCM        D86CCM
066300960926     C                   MOVEL     TRTC86        KPJBU
066400031118     C   KF              CALL      'TRTC86C'
066500960924     C                   PARM                    KPJBA
066600120620     C* se c'è stata conferma e il rcd era forzato
066700120620     C                   IF        *inKF = *on and S02Forced = *on
066800120620     C* invio email di conferma forzatura
066900120620     C                   EXSR      EmailForza
067000120620     C                   ENDIF
067100120620     C*
067200091124    3C                   ENDSL
067300051201    2C                   ENDIF
067400091124
067500040713     C                   UPDATE    TC85S02
067600051201    1C                   ENDDO
067700031113     C*
067800031113     C*  Se nn ho errori in opzioni sub-file gestisco...
067900051201    1C                   IF        *IN28 = *OFF
068000031113     C                   Z-ADD     *ZEROS        KEYNRR2           4 0
068100051201    2C     1             DO        wNRR2         KEYNRR2
068200031113     C     KEYNRR2       CHAIN     TC85S02
068300051201    3C                   IF        %found
068400031201     C*  Se scelto ingresso x accorpamento bolle
068500051201    4C                   IF        V1CABD = 'S'
068600031201     C*  E se richiesta opzione 9 d accorpamento bolle
068700051201    5C                   IF        V2CSCE = '9'
068800031113     C                   MOVEL     'R'           D86MOD
068900031113     C                   Z-ADD     V2CCCM        D86CCM
069000031113     C                   MOVEL     V2CCMR        D86CMR
069100031113     C                   Z-ADD     V2CCNT        D86CNT
069200050708     C                   Z-ADD     V2CCCM        savD86CCM
069300050708     C                   MOVEL     V2CCMR        savD86CMR
069400050708     C                   Z-ADD     V2CCNT        savD86CNT
069500031113     C                   MOVEL     'LR'          D86CHI
069600070802     C   KF              CALL(e)   'TRTC86R3'
069700031113     C                   PARM                    TRTC86
069800120620     C* se c'è stata conferma e il rcd era forzato
069900120620     C                   IF        *inKF = *on and S02Forced = *on
070000120620     C* invio email di conferma forzatura
070100120620     C                   EXSR      EmailForza
070200120620     C                   ENDIF
070300120620     C*
070400070802     C* Se errore => rollback
070500070802     C                   IF        %error
070600070802     C                   ROLBK
070700070802     C                   ENDIF
070800070802     C*
070900051201    5C                   ENDIF
071000051201    4C                   ENDIF
071100051201    3C                   ENDIF
071200051201    2C                   ENDDO
071300031201     C*
071400031201     C*  Se scelto ingresso x accorpamento bolle
071500051201    2C                   IF        V1CABD = 'S'
071600031113     C*  Al termine del passaggio dati dai file "buoni" a quelli d lavoro eseguo il programma d
071700031113     C*  accorpamento e scrittuta FIVAB/FIVAD vero e proprio
071800040714     C                   MOVEL     'R'           D86MOD
071900050708     C                   Z-ADD     savD86CCM     D86CCM
072000050708     C                   MOVEL     savD86CMR     D86CMR
072100050708     C                   Z-ADD     savD86CNT     D86CNT
072200040714     C                   MOVEL     'LR'          D86CHI
072300070802     C   KF              CALL(e)   'TRTC86R4'
072400070420     C                   PARM                    KPJBA
072500040714     C                   PARM                    TRTC86
072600120620     C* se c'è stata conferma e il rcd era forzato
072700120620     C                   IF        *inKF = *on and S02Forced = *on
072800120620     C* invio email di conferma forzatura
072900120620     C                   EXSR      EmailForza
073000120620     C                   ENDIF
073100120620     C*
073200070802     C* Se errore => rollback
073300070802     C                   IF        %error
073400070802     C                   ROLBK
073500070802     C                   ELSE
073600070802     C* Se ok     => committ
073700070802     C                   COMMIT
073800070802     C                   ENDIF
073900070802     C*
074000051201    2C                   ENDIF
074100031113     C*
074200960924     C*  Se non ho selezionato nulla do errore
074300051201    2C     *INKF         IFEQ      '1'
074400051201    3C     WSCELT        IFEQ      'N'
074500040713     C                   SETON                                        28
074600960924     C                   MOVEL     ERR(3)        $MSG
074700051201   x3C                   ELSE
074800960924     C*  Se ho selezionato stampa richiamo pgm x totali genarali
074900051201    4C     WSTAMP        IFEQ      'S'
075000960926     C                   MOVEL     *BLANKS       D86STA
075100960924     C                   MOVEL     *BLANKS       D86CMR
075200960926     C                   MOVEL     *BLANKS       D86MOD
075300960926     C                   MOVEL     'S'           D86RIE
075400960924     C                   MOVEL     'LR'          D86CHI
075500960924     C                   Z-ADD     0             D86CNT
075600960924     C                   Z-ADD     0             D86CCM
075700031107     C***                Z-ADD     0             D86CCN
075800960926     C                   MOVEL     TRTC86        KPJBU
075900091127     C                   CALL      'TRTC86R1'
076000960924     C                   PARM                    KPJBA
076100051201    4C                   ENDIF
076200031118     C*  Al termine dell'esecuzione delle opzioni se è stato premuto F6 => ricarico sub-file
076300051201    4C     WSCELT        IFNE      'N'
076400031118     C                   EXSR      INZ02
076500051201    4C                   ENDIF
076600051201    3C                   ENDIF
076700051201    2C                   ENDIF
076800031110     C*
076900051201    1C                   ENDIF
077000960924     C*
077100960924     C                   ENDSR
077200960924     C*----------------------------------------------------*
077300051206     C*   CONTRTRASFE - controllo se posso fare il trasferimento
077400960924     C*----------------------------------------------------*
077500051206     C     ContrTrasfe   BEGSR
077600121022     C*
077700121022     C* Filiale obbligatoria
077800051206     c                   if        v2cfil=0
077900051206     c                   movel     err(7)        $msg
078000051206     c                   seton                                        28
078100051206     c                   goto      endctr
078200051206     c                   endif
078300121022     c*
078400141113     C* Controllo che la Filiale di conferma bolle esista
078500121022     C     V2CFIL        CHAIN     AZORG01L                           28
078600121022    1C     *IN28         IFEQ      *ON
078700121022     C     ORGFVA        ORNE      ' '
078800121022     C     ORGFAG        ORNE      'A'
078900121022     C     ORGFAG        ANDNE     'F'
079000121022     c                   movel     err(9)        $msg
079100121022     C                   GOTO      ENDCTR
079200121022    1C                   ENDIF
079300141113     C* reperisco il suo network
079400141113     C                   MOVEL     ORGDE3        OG143
079500141113     C                   EVAL      wNTW_1 = §OGNTW
079600121128
079700141114     C*** tolgo il richiamo al TNTB51R in forma di controllo perché il test è legato a
079800141114     C*** PO + cliente e non solo a PO come si vuole
079900121128     c* richiamo pgm tntb51r
080000141114     c***                clear                   tntb50ds
080100141114     c***                movel     v2cccm        b50ke2
080200141114     c***                movel     v2cccm        b50ke1
080300141114     c***                movel     'C'           b50opz
080400141114     c***                movel     v2cfil        b50poa
080500141114     c***                call      'TNTB51R'
080600141114     c***                parm                    kpjba
080700141114     c***                parm                    tntb50ds
080800141114     c***
080900141114     c***                if        b50err<>' '
081000141114     c***                movel     b50msg        $msg
081100141114     c***                seton                                        2870
081200141114     c***                goto      endctr
081300141114     c***                endif
081400141114
081500121128     c
081600051206     c
081700051206     c* Verifico se già presente il CMR nel p.o. di ricezione dati
081800051206     c     kvab3         setll     edivab3l
081900051206     c     kvab3         reade     edivab3l
082000051206     c                   dow       not %eof(edivab3l)
082100051206     c                   if        vabcmr=v2ccmr
082200051206     c                   seton                                        2870
082300051206     c                   movel     err(8)        $msg
082400051206     C                   EVAL      $MSG = %trim($MSG) + 'VAB'
082500051206     c                   goto      endctr
082600051206     c                   endif
082700051206     c
082800051206     c     kvab3         reade     edivab3l
082900051206     c                   enddo
083000051206     c
083100051206     c     kdett         setll     edivad1l
083200051206     c                   if        %equal(edivad1l)
083300051206     c                   seton                                        2870
083400051206     c                   movel     err(8)        $msg
083500051206     C                   EVAL      $MSG = %trim($MSG) + 'VAD'
083600051206     c                   goto      endctr
083700051206     c                   endif
083701171219     c     kdett         setll     edivad11l
083702171219     c                   if        %equal(edivad11l)
083703171219     c                   seton                                        2870
083704171219     c                   movel     err(8)        $msg
083705171219     C                   EVAL      $MSG = %trim($MSG) + 'VAD10'
083706171219     c                   goto      endctr
083707171219     c                   endif
083800051206     c     kdett         setll     edivat1l
083900051206     c                   if        %equal(edivat1l)
084000051206     c                   seton                                        2870
084100051206     c                   movel     err(8)        $msg
084200051206     C                   EVAL      $MSG = %trim($MSG) + 'VAT'
084300051206     c                   goto      endctr
084400051206     c                   endif
084500051206     c     kdett         setll     edivax1l
084600051206     c                   if        %equal(edivax1l)
084700051206     c                   seton                                        2870
084800051206     c                   movel     err(8)        $msg
084900051206     C                   EVAL      $MSG = %trim($MSG) + 'VAX'
085000051206     c                   goto      endctr
085100051206     c                   endif
085200141112
085300141112     C*
085400141113     C* Filiale cliente esistente
085500141112     C                   EVAL      wFilTrasf =
085600141112     C                              %dec(%subst(%editc(V2CCCM:'X'):1:3):3:0)
085700141112     C     wFilTrasf     CHAIN     AZORG01L                           28
085800141112    1C     *IN28         IFEQ      *ON
085900141112     C     ORGFVA        ORNE      ' '
086000141112     C     ORGFAG        ORNE      'A'
086100141112     C     ORGFAG        ANDNE     'F'
086200141112     c                   movel     err(9)        $msg
086300141112     C                   GOTO      ENDCTR
086400141112    1C                   ENDIF
086500141113     C* reperisco il suo network
086600141112     C                   MOVEL     ORGDE3        OG143
086700141113     C                   EVAL      wNTW_2 = §OGNTW
086800141112     C*
086900141112     C* Reperimento tabella 3TR
087000141112     C                   MOVE(P)   '3TR'         tbeCOD
087100141112     C                   EVAL      tbeKE1 = %editc(wFilTrasf:'X')
087200141112     C     K02TBE01      CHAIN     TNTBE01L
087300141113     C* se non trovata filiale su tab.3TR significa che la filiale cliente può avere le bolle
087400141113     C* confermate da qualsiasi altra filiale
087500141112     C                   IF        %found(TNTBE01L) AND
087600141112     C                             tbeATB <> 'A'
087700141112     C                   MOVEL     tbeUNI        D3TR
087800141112     C                   EVAL      wPOABI = §3TRPOABI
087900141113     C* se la filiale cliente è presente su tab. 3TR controllo:
088000141113     C* - che la filiale di conferma sia presente nella schiera di quelle abilitate
088100141113     C                   IF        %lookup(%editc(V2CFIL:'X'):SkPOABI) = 0
088200141114     C                             or SkPOABI(1) = '999'
088300141113     c                   seton                                        28
088400141113     C                   movel     err(11)       $msg
088500141112     C                   GOTO      ENDCTR
088600141112     C                   ENDIF
088700141112     C                   ENDIF
088800141112     C*
088900141113     C* controllo che se il network della filiale conferma bolle è estero
089000141113     C                   IF        wNTW_1 = 'DPD' or
089100141113     C                             wNTW_1 = 'EEX' or
089200141113     C                             wNTW_1 = 'FED'
089300141113     C* il network della filiale cliente deve essere lo stesso
089400141113     C                   IF        wNTW_2 <> wNTW_1
089500141113     c                   seton                                        28
089600141113     C                   movel     err(12)       $msg
089700141113     C                   GOTO      ENDCTR
089800141113     C                   ENDIF
089900141113     C                   ENDIF
090000141112
090100051206     c
090200051206     c     endctr        endsr
090300051206     C*----------------------------------------------------*
090400051206     C*   ESEGUITRASFE - eseguo trasferimento dati con _SQL
090500051206     C*----------------------------------------------------*
090600051206     C     EseguiTRasfe  BEGSR
090700051206     C                   EVAL      Stringasql=
090800051206     C                             'UPDATE EDIVAD1L'
090900051206     C                             +
091000051206     C                             ' SET VADFGS = ' + %editc(v2cfil:'1')
091100051206     C                             +
091200051206     C                             ' WHERE VADCMR='''+ V2CCMR+ ''' AND '
091300051206     C                             +
091400051206     C                             ' VADCCM=' + %editc(v2cccm:'4') +' AND'
091500051206     C                             +
091600051206     C                             ' VADCNT=' + %editc(v2ccnt:'3')
091700051206
091800051206     C/EXEC SQL
091900051206     C+ EXECUTE IMMEDIATE :StringaSQL
092000051206     C/END-EXEC
092001171219     C                   EVAL      Stringasql=
092002171219     C                             'UPDATE EDIVAD11L'
092003171219     C                             +
092004171219     C                             ' SET VADFGS = ' + %editc(v2cfil:'1')
092005171219     C                             +
092006171219     C                             ' WHERE VADCMR='''+ V2CCMR+ ''' AND '
092007171219     C                             +
092008171219     C                             ' VADCCM=' + %editc(v2cccm:'4') +' AND'
092009171219     C                             +
092010171219     C                             ' VADCNT=' + %editc(v2ccnt:'3')
092011171219
092012171219     C/EXEC SQL
092013171219     C+ EXECUTE IMMEDIATE :StringaSQL
092014171219     C/END-EXEC
092100051206     C                   EVAL      Stringasql=
092200051206     C                             'UPDATE EDIVAT1L'
092300051206     C                             +
092400051206     C                             ' SET VATFGS = ' + %editc(v2cfil:'1')
092500051206     C                             +
092600051206     C                             ' WHERE VATCMR='''+ V2CCMR+ ''' AND '
092700051206     C                             +
092800051206     C                             ' VATCCM=' + %editc(v2cccm:'4') +' AND'
092900051206     C                             +
093000051206     C                             ' VATCNT=' + %editc(v2ccnt:'3')
093100051206
093200051206     C/EXEC SQL
093300051206     C+ EXECUTE IMMEDIATE :StringaSQL
093400051206     C/END-EXEC
093500051206
093600051206     C                   EVAL      Stringasql=
093700051206     C                             'UPDATE EDIVAX1L'
093800051206     C                             +
093900051206     C                             ' SET VAXFGS = ' + %editc(v2cfil:'1')
094000051206     C                             +
094100051206     C                             ' WHERE VAXCMR='''+ V2CCMR+ ''' AND '
094200051206     C                             +
094300051206     C                             ' VAXCCM=' + %editc(v2cccm:'4') +' AND'
094400051206     C                             +
094500051206     C                             ' VAXCNT=' + %editc(v2ccnt:'3')
094600051206
094700051206     C/EXEC SQL
094800051206     C+ EXECUTE IMMEDIATE :StringaSQL
094900051206     C/END-EXEC
095000051206
095100051206     C                   EVAL      Stringasql=
095200051206     C                             'UPDATE EDIVAB1L'
095300051206     C                             +
095400051206     C                             ' SET VABFGS = ' + %editc(v2cfil:'1')
095500051206     C                             +
095600051206     C                             ' WHERE VABCMR='''+ V2CCMR+ ''' AND '
095700051206     C                             +
095800051206     C                             ' VABCCM=' + %editc(v2cccm:'4') +' AND'
095900051206     C                             +
096000051206     C                             ' VABCNT=' + %editc(v2ccnt:'3')
096100051206
096200051206     C/EXEC SQL
096300051206     C+ EXECUTE IMMEDIATE :StringaSQL
096400051206     C/END-EXEC
096500051206     C
096600051206     c                   seton                                        75
096700051206     C                   UPDATE    TC85S02
096800051206     c                   setoff                                       75
096900051206     c                   endsr
097000051206     C*----------------------------------------------------*
097100051206     C*   ROLLUP01: Caricamento dati da Edivab1l           *
097200051206     C*----------------------------------------------------*
097300051206     C     ROLLUP01      BEGSR
097400960924     C*
097500960930     C     *IN23         IFEQ      '0'
097600960930     C     VABCCM        ANDLE     WCCMAL
097700960930     C*
097800030617     C                   DOW       *IN23     = *OFF    AND
097900030617     C                             VABCCM   <= WCCMAL
098000030617     C* Testo che se richiesto il CMR corrisponda
098100030617     C                   IF        (V1CCMR <> *blanks  AND
098200030617     C                              VABCMR  = V1CCMR)  OR
098300030617     C                             V1CCMR   = *blanks
098400031118     C* Al primo giro incremento subito e salvo il livello d rottura
098500031118     C                   IF        wGiro > *zeros
098600960930     C* Scrivo solo a rottura di CMR - contatore
098700960930     C     VABCNT        IFNE      WSVCNT
098800960930     C     VABCMR        ORNE      WSVCMR
098900960930     C     VABCCM        ORNE      WSVCCM
099000041123     C**   VABDTS        ORNE      WSVDTS
099100041123     C**   VABHMS        ORNE      WSVHMS
099200960930     C*  Scrittura TC85S02
099300960930     C                   EXSR      WRTS02
099400031118     C*  Inizializzo variabili d appoggio NN chiave
099500031118     C                   CLEAR                   WSVSPE
099600031118     C                   CLEAR                   WSVNCL
099700031118     C                   CLEAR                   WSVPKB
099800040713     C                   CLEAR                   wTotVADCMR
099801171220     C                   CLEAR                   wTotVA1CMR
099900040713     C                   CLEAR                   wTotVATCMR
100000040713     C                   CLEAR                   wTotVAXCMR
100100031118     C*  Salvataggio nuova chiave di rottura
100200031118     C                   EXSR      SAVDAT
100300031118     C                   ELSE
100400031118     C*  Salvataggio nuova chiave di rottura
100500031118     C                   EXSR      SAVDAT
100600030617     C                   ENDIF
100700031118     C                   ELSE
100800031118     C*  Salvataggio nuova chiave di rottura
100900031118     C                   EXSR      SAVDAT
101000031118     C                   Z-ADD     1             wGiro
101100031118     C                   ENDIF
101200030617     C                   ENDIF
101300960924     C*  Lettura successiva
101400020919     c                   exsr      ReadVAB
101500030617     C                   ENDDO
101600031119     C*
101700031119     C*  Scrittura TC85S02 ultimo CMR valido
101800031119     C                   EXSR      WRTS02
101900030617     C                   ENDIF
102000040713     C*
102100040713     C*  Salvo il numero d record del sub-file caricati
102200040713     C                   Z-ADD     NRR2          wNRR2             4 0
102300040713     C*
102400960924     C                   ENDSR
102500030617     C*----------------------------------------------------*
102600041123     C*   ROLLUP04: Caricamento dati da Edivab04           *
102700030617     C*----------------------------------------------------*
102800041123     C     ROLLUP04      BEGSR
102900030617     C*
103000030617     C     *IN23         IFEQ      '0'
103100030617     C     VABCMR        ANDLE     WCMRAL
103200030617     C*
103300030617     C                   DOW       *IN23     = *OFF    AND
103400030617     C                             VABCMR   <= WCMRAL
103500030617     C* Testo che se richiesto il CMR corrisponda
103600030617     C                   IF        (V1CCCM <> *zeros  AND
103700030617     C                              VABCCM  = V1CCCM) OR
103800030617     C                             V1CCCM   = *zeros
103900031118     C* Al primo giro incremento subito e salvo il livello d rottura
104000031118     C                   IF        wGiro > *zeros
104100030617     C* Scrivo solo a rottura di CMR - contatore
104200030617     C     VABCNT        IFNE      WSVCNT
104300030617     C     VABCMR        ORNE      WSVCMR
104400030617     C     VABCCM        ORNE      WSVCCM
104500041123     C**   VABDTS        ORNE      WSVDTS
104600041123     C**   VABHMS        ORNE      WSVHMS
104700030617     C*  Scrittura TC85S02
104800030617     C                   EXSR      WRTS02
104900031118     C*  Inizializzo variabili d appoggio
105000031118     C                   CLEAR                   WSVSPE
105100031118     C                   CLEAR                   WSVNCL
105200031118     C                   CLEAR                   WSVPKB
105300040713     C                   CLEAR                   wTotVADCMR
105301171220     C                   CLEAR                   wTotVA1CMR
105400040713     C                   CLEAR                   wTotVATCMR
105500040713     C                   CLEAR                   wTotVAXCMR
105600031118     C*  Salvataggio nuova chiave di rottura
105700031118     C                   EXSR      SAVDAT
105800031118     C                   ELSE
105900031118     C*  Salvataggio nuova chiave di rottura
106000031118     C                   EXSR      SAVDAT
106100030617     C                   ENDIF
106200031118     C                   ELSE
106300031118     C*  Salvataggio nuova chiave di rottura
106400031118     C                   EXSR      SAVDAT
106500031118     C                   Z-ADD     1             wGiro
106600031118     C                   ENDIF
106700030617     C                   ENDIF
106800030617     C*  Lettura successiva
106900030617     c                   exsr      ReadVAB
107000030617     C                   ENDDO
107100031119     C*
107200031119     C*  Scrittura TC85S02 ultimo CMR valido
107300031119     C                   EXSR      WRTS02
107400030617     C                   ENDIF
107500040713     C*
107600040713     C*  Salvo il numero d record del sub-file caricati
107700040713     C                   Z-ADD     NRR2          wNRR2             4 0
107800030617     C*
107900030617     C                   ENDSR
108000960930     C*----------------------------------------------------*
108100960930     C*   SAVDAT: Salvataggio campi rottura CCM-CMR-CNT    *
108200960930     C*----------------------------------------------------*
108300960930     C     SAVDAT        BEGSR
108400960930     C*
108500960930     C*  Reimposto campi per rottura totale
108600030925     C                   Z-ADD     VABFGS        WSVFGS
108700960930     C                   Z-ADD     VABCCM        WSVCCM
108800960930     C                   Z-ADD     VABCNT        WSVCNT
108900960930     C                   MOVEL     VABCMR        WSVCMR
109000960930     C                   Z-ADD     VABHMS        WSVHMS
109100960930     C                   Z-ADD     VABDTS        WSVDTS
109200960930     C                   Z-ADD     VABDCM        WSVDCM
109300031118     C                   ADD       1             WSVSPE
109400031118     C                   ADD       VABNCL        WSVNCL
109500031118     C                   ADD       VABPKB        WSVPKB
109600040719     C*
109700040719     C* Conteggia i colli in relazione al trattamento merce
109800040719     C                   z-add     1             SkJ
109900040719     C     vabCTM        lookup    SkCTM(SkJ)                             80
110000040719     C                   if        *IN80 = *on
110100040719     C                   eval      DSSKEXT = SkEXT(SkJ)
110200040719     C                   if        FLGVAD = 'S'
110300040719     C                   add       vabNCL        wTotVADCMR
110400040719     C                   endif
110401171220     C                   if        FLGVAD10 = 'S'
110402171220     C                   add       vabNCL        wTotVA1CMR
110403171220     C                   endif
110500040719     C                   if        FLGVAT = 'S'
110600040719     C                   add       vabNCL        wTotVATCMR
110700040719     C                   endif
110800040719     C                   if        FLGVAX = 'S'
110900040719     C                   add       1             wTotVAXCMR
111000040719     C                   endif
111100040719     C                   endif
111200040719     C*
111300031118     C                   CLEAR                   TC85S02
111400960930     C*
111500960930     C                   ENDSR
111600960930     C*----------------------------------------------------*
111700030617     C*   WRTS02: Scrittura dati subfile                   *
111800960930     C*----------------------------------------------------*
111900960930     C     WRTS02        BEGSR
112000030926     C*
112100040713     C                   SETOFF                                       70
112200040713     C*
112300030709     C                   Z-ADD     WSVCCM        V2CCCM
112400030709     C                   MOVEL     WSVCMR        V2CCMR
112500030709     C                   Z-ADD     WSVCNT        V2CCNT
112600030709     C                   MOVEL     WSVHMS        V2CHMS
112700030709     C                   Z-ADD     WSVDTS        G02INV
112800030709     C                   MOVEL     '3'           G02ERR
112900030709     C                   CALL      'XSRDA8'
113000030709     C                   PARM                    WLBDA8
113100030709     C                   Z-ADD     G02DAT        V2CDTS
113200030709     C                   Z-ADD     WSVDCM        G02INV
113300030709     C                   MOVEL     '3'           G02ERR
113400030709     C                   CALL      'XSRDA8'
113500030709     C                   PARM                    WLBDA8
113600030709     C                   Z-ADD     G02DAT        V2CDCM
113700040716     C*
113800040716     C* Reperisco la Ragione Sociale del cliente
113900040716     C                   CLEAR                   BS69DS
114000040716     C                   CLEAR                   ACODS
114100040716     C                   CLEAR                   INDDS
114200040716     C                   CLEAR                   CLPDS
114300040716     C                   CLEAR                   CLSDS
114400040716     C                   Z-ADD     V2CCCM        I69KAC
114500040716     C                   CALL      'TIBS69R'
114600040716     C                   PARM                    BS69DS
114700040716     C                   PARM                    ACODS
114800040716     C                   PARM                    INDDS
114900040716     C                   PARM                    CLPDS
115000040716     C                   PARM                    CLSDS
115100040716     C     O69ERR        IFNE      '1'
115200040716     C                   EVAL      V2CRAG = ACORAG
115300040716     C                   ENDIF
115400040716     C*
115500040716     C* Effettuo considerazione particolare su cambi sommati a video
115600040713     C                   IF        (WSVSPE + V2CSPE) > 99999
115700040713     C                   EVAL      V2CSPE = 99999
115800040713     C                   SETON                                        59
115900040713     C                   ELSE
116000031118     C                   ADD       WSVSPE        V2CSPE
116100040713     C                   ENDIF
116200040713     C                   IF        (WSVNCL + V2CNCL) > 99999
116300040713     C                   EVAL      V2CNCL = 99999
116400040713     C                   SETON                                        63
116500040713     C                   ELSE
116600040713     C                   ADD       WSVNCL        V2CNCL
116700040713     C                   ENDIF
116800040713     C                   IF        (WSVPKB + V2CPKG) > 99999
116900040713     C                   EVAL      V2CPKG = 99999
117000040713     C                   SETON                                        64
117100040713     C                   ELSE
117200040713     C                   ADD       WSVPKB        V2CPKG
117300040713     C                   ENDIF
117400030926     C*
117500040713     C* Effettuo lettura x chiave bolla corrente x determinare quanti dettagli
117600040713     C* VAD/VAT/VAX sono presenti
117700040713     C                   Z-ADD     *zeros        V2CNDD
117701171220     C                   Z-ADD     *zeros        V2CNDD1
117800040713     C                   Z-ADD     *zeros        V2CNDT
117900040713     C                   Z-ADD     *zeros        V2CNDX
118000040713     C                   Z-ADD     *zeros        wV2CNDD           9 0
118001171220     C                   Z-ADD     *zeros        wV2CNDD1          9 0
118100040713     C                   Z-ADD     *zeros        wV2CNDT           9 0
118200040713     C                   Z-ADD     *zeros        wV2CNDX           9 0
118300031204     C* VAD
118400030926     C     KVAD          SETLL     EDIVAD1L                               50
118500030926     C                   IF        %equal(EDIVAD1L)
118600030926     C     KVAD          READE     EDIVAD1L
118700030926     C                   DOW       not %eof(EDIVAD1L)
118800040713     C                   ADD       1             wV2CNDD
118900030926     C     KVAD          READE     EDIVAD1L
119000030926     C                   ENDDO
119100030926     C                   ENDIF
119101171220     C* VAD10
119102171221     c                   clear                   s_vadncd
119103171220     C     KVAD          SETLL     EDIVAD11L                              50
119104171220     C                   IF        %equal(EDIVAD11L)
119105171220     C     KVAD          READE     EDIVAD11L
119106171220     C                   DOW       not %eof(EDIVAD11L)
119107171221     c                   if        vadncd<>s_vadncd
119108171220     C                   ADD       1             wV2CNDD1
119109171221     c                   eval      s_vadncd=vadncd
119110171221     c                   endif
119111171220     C     KVAD          READE     EDIVAD11L
119112171220     C                   ENDDO
119113171220     C                   ENDIF
119200031204     C* VAT
119300031204     C     KVAD          SETLL     EDIVAT1L                               50
119400031204     C                   IF        %equal(EDIVAT1L)
119500031204     C     KVAD          READE     EDIVAT1L
119600031204     C                   DOW       not %eof(EDIVAT1L)
119700040713     C                   ADD       1             wV2CNDT
119800130315     C* SE TIPO RECORD "PDF" CONTEGGIO A FINI PACKING LIST
119900130319     C                   IF        VATTRC = 'P'
120000130315     C                   ADD       1             wV2CNDX
120100130315     C                   ENDIF
120200031204     C     KVAD          READE     EDIVAT1L
120300031204     C                   ENDDO
120400031204     C                   ENDIF
120500040713     C* VAX
120600040713     C     KVAD          SETLL     EDIVAX1L                               50
120700040713     C                   IF        %equal(EDIVAX1L)
120800040713     C     KVAD          READE     EDIVAX1L
120900040713     C                   DOW       not %eof(EDIVAX1L)
121000040713     C* Conteggio a rottura spedizione
121100040713     C                   IF        DSKEYVAXROT <> DSKEYVAX
121200040713     C                   ADD       1             wV2CNDX
121300040713     C                   EVAL      DSKEYVAXROT = DSKEYVAX
121400040713     C                   ENDIF
121500040713     C     KVAD          READE     EDIVAX1L
121600040713     C                   ENDDO
121700040713     C* Conteggio a rottura spedizione
121800040713     C                   IF        DSKEYVAXROT <> DSKEYVAX
121900040713     C                   ADD       1             wV2CNDX
122000040713     C                   EVAL      DSKEYVAXROT = DSKEYVAX
122100040713     C                   ENDIF
122200040713     C                   ENDIF
122300030926     C*
122400040713     C* Verifico se i conteggi sono superiori ai valori max conenibili nei campi
122500040713     C* conteggi dettagli a video
122600040713     C                   IF        wV2CNDD > 99999
122700040713     C                   EVAL      V2CNDD = 99999
122800040713     C                   SETON                                        60
122900040713     C                   ELSE
123000040713     C                   EVAL      V2CNDD = wV2CNDD
123100040713     C                   ENDIF
123101171220     C                   IF        wV2CNDD1 > 99999
123102171220     C                   EVAL      V2CNDD1 = 99999
123103171220     C                   SETON                                        65
123104171220     C                   ELSE
123105171220     C                   EVAL      V2CNDD1 = wV2CNDD1
123106171220     C                   ENDIF
123200040713     C                   IF        wV2CNDT > 99999
123300040713     C                   EVAL      V2CNDT = 99999
123400040713     C                   SETON                                        61
123500040713     C                   ELSE
123600040713     C                   EVAL      V2CNDT = wV2CNDT
123700040713     C                   ENDIF
123800040713     C                   IF        wV2CNDX > 99999
123900040713     C                   EVAL      V2CNDX = 99999
124000040713     C                   SETON                                        62
124100040713     C                   ELSE
124200040713     C                   EVAL      V2CNDX = wV2CNDX
124300040713     C                   ENDIF
124400040713     C*
124500040713     C* Effettuo considerazioni bloccanti relative a conteggi estensioni dettaglio (VAD/VAT/VAX)
124600040713     C                   EVAL      V2CERRVAD = *blanks
124601171221     C                   EVAL      V2CERRVAD1= *blanks
124700040713     C                   EVAL      V2CERRVAT = *blanks
124800040713     C                   EVAL      V2CERRVAX = *blanks
124900040713     C* VAD
125000040713     C                   IF        wV2CNDD = *zeros AND
125100040713     C                             wTotVADCMR > *zeros
125200040713     C                   SETON                                        60
125300040713     C                   EVAL      V2CERRVAD = '1'
125400040713     C                   ENDIF
125401171220     C* VAD10
125402171220     C                   IF        wV2CNDD1 = *zeros AND
125403171220     C                             wTotVA1CMR > *zeros
125404171220     C                   SETON                                        65
125405171221     C                   EVAL      V2CERRVAD1= '1'
125406171220     C                   ENDIF
125500040713     C* VAT
125600040713     C                   IF        wV2CNDT = *zeros AND
125700040713     C                             wTotVATCMR > *zeros
125800040713     C                   SETON                                        61
125900040713     C                   EVAL      V2CERRVAT = '1'
126000040713     C                   ENDIF
126100040713     C* VAX
126200040713     C                   IF        wV2CNDX = *zeros AND
126300040713     C                             wTotVAXCMR > *zeros
126400040713     C                   SETON                                        62
126500120424
126600120502     C* controllo se esiste un bypass su questo cliente (do errore se NON c'è)
126700120502     C                   IF        %lookup(V2CCCM : CCM) = 0
126800040713     C                   EVAL      V2CERRVAX = '1'
126900120424     C                   ENDIF
127000120424
127100040713     C                   ENDIF
127200070808     C* VAX x ACCORPAMENTO
127300070808     C                   SETOFF                                       03
127400070808     C     V2CCCM        LOOKUP    KSCABDVX                               33
127500070808     C                   IF        *IN33 = *OFF AND V1CABD = 'S' AND
127600070808     C                             (wV2CNDX    > *zeros OR
127700070808     C                              wTotVAXCMR > *zeros)
127800070808     C                   SETON                                        62
127900070808     C                   SETON                                        03
128000070808     C                   EVAL      V2CERRVAX = '1'
128100070808     C                   ENDIF
128200040713     C*
128300040713     C* Salvo lo stato degli indicatori a video
128400040713     C                   EVAL      WIN59 = *IN59
128500040713     C                   EVAL      WIN60 = *IN60
128600040713     C                   EVAL      WIN61 = *IN61
128700040713     C                   EVAL      WIN62 = *IN62
128800040713     C                   EVAL      WIN63 = *IN63
128900040713     C                   EVAL      WIN64 = *IN64
128901171220     C                   EVAL      WIN65 = *IN65
129000030709     C*
129100960930     C                   ADD       1             NRR2              4 0
129200960930     C                   WRITE     TC85S02
129300040713     C                   SETOFF                                       596061
129400040713     C                   SETOFF                                       626364
129401171220     C                   SETOFF                                       65
129500960930     C*
129600960930     C                   ENDSR
129700031106     C*------------------------------------------------------------------------*
129800120620     C* WdwForce - chiamata a pgm per window per forzatura
129900031106     C*------------------------------------------------------------------------*
130000120619     C     WdwForce      BEGSR
130100031106     C*
130200120619     C                   EVAL      Forcing = *off
130300120620     C*
130400120620     C                   CALL      'TRTC85R1'
130500120620     C                   PARM                    Kpjba
130600120620     C                   PARM                    Exfmt
130700120620     C                   PARM                    Msg01
130800120620     C                   PARM                    Msg02
130900120620     C                   PARM                    Msg03
131000120620     C                   PARM                    Forcing
131100120620     C*
131200120620     C* tolgo memo che c'è stata una exfmt del sfl
131300120620     C                   EVAL      Exfmt = *off
131400120620     C*
131500120620     C                   ENDSR
131600120620     C*------------------------------------------------------------------------*
131700120620     C* EmailForza - Invio e-mail di conferma forzatura
131800120620     C*------------------------------------------------------------------------*
131900120620     C     EmailForza    BEGSR
132000120620     C*
132100120620     C* invio e-mail con richiesta forzatura
132200120620     C                   EVAL      peml = 'ced@brt.it'
132300120620     C                   EVAL      pcceml = *blanks
132400120620     C                   EVAL      pogg = 'Conferma da CMR - FORZATURA'
132500120620     C                   EVAL      pmsg =
132600120620     C                             'Codice cliente mittente: ' +
132700120622     C                             %editc(V2CCCM:'X')  +
132800120620     C                             ':/N' +
132900120622     C                             'Numero CMR: ' +
133000120620     C                             V2CCMR +
133100120620     C                             ':/N' +
133200120620     C                             'Utente forzatura: ' +
133300120620     C                             SDSusr +
133400120620     C                             ':/N'
133500120620     C                   call      'TIS701C1'
133600120620     C                   parm                    peml            253
133700120620     C                   parm                    pcceml          253
133800120620     C                   parm                    pogg             44
133900120620     C                   parm                    pmsg           5000
134000120620     C                   parm                    pesito            1
134100120619     C*
134200120619     C                   ENDSR
134300120619     C*------------------------------------------------------------------------*
134400120619     C* CARTAB - CARICA LE TABELLE OCCORRENTI
134500120619     C*------------------------------------------------------------------------*
134600120619     C     CARTAB        BEGSR
134700120619     C*
134800031106     C* TABELLA 3C - CLIENTI CON ABILITAZIONE ALL'ACCORPAMENTO BOLLE X MEDESIMO DESTINATARIO
134900031106     C                   Z-ADD     *ZEROS        I                 3 0
135000031106     C                   Z-ADD     CODUT         KTBute
135100031106     C                   MOVEL     '3C'          KTBcod
135200031106     C     KEYtabP       SETLL     TABEL00F
135300031106     C     KEYtabP       READE     TABEL00F                               97
135400031106     C     *IN97         DOWEQ     *OFF
135500031106     C     TBLFLG        IFEQ      *BLANKS
135600031106     C                   MOVEL     TBLUNI        DS3C
135700050610     C                   IF        §3CABD <> *blanks
135800031106     C                   ADD       1             I
135900031106     C                   MOVEL     TBLKEY        KSCABD(I)
136000070808     C                   IF        §3CAVX = 'S'
136100070808     C                   MOVEL     TBLKEY        KSCABDVX(I)
136200070808     C                   ENDIF
136300031106     C                   ENDIF
136400031106     C                   ENDIF
136500031106     C     KEYtabP       READE     TABEL00F                               97
136600031106     C                   ENDDO
136700040713     C*
136800040713     C* TATBELLA '1B' -  CODICI TRATTAMENTO MERCE IN FUNZIONE DELLE ESTENSIONI VAD/VAT/VAX
136900040713     C                   Z-ADD     *ZEROS        SkJ               3 0
137000040713     C                   Z-ADD     CODUT         KTBute
137100040713     C                   MOVEL     '1B'          KTBcod
137200040713     C     KEYtabP       SETLL     TABEL00F
137300040713     C     KEYtabP       READE     TABEL00F                               97
137400040713     C     *IN97         DOWEQ     *OFF
137500040713     C     TBLFLG        IFEQ      *BLANKS
137600040713     C                   MOVEL     TBLUNI        DS1B
137700040713     C                   ADD       1             SkJ
137800040713     C                   MOVEL     TBLKEY        SkCTM(SkJ)
137900040716     C                   CLEAR                   DSSKEXT
138000040713     C* VAD
138100040713     C                   IF        §1BFG7 = 'S'
138200040713     C                   EVAL      FLGVAD = 'S'
138300040713     C                   ENDIF
138301171220     C* VAD10
138302171221     C                   IF        §1BF17  = 'S'
138303171220     C                   EVAL      FLGVAD10 = 'S'
138304171220     C                   ENDIF
138400040713     C* VAT
138500040713     C                   IF        §1BF12 = 'E'
138600040713     C                   EVAL      FLGVAT = 'S'
138700040713     C                   ENDIF
138800040713     C* VAX
138900040713     C                   IF        §1BF16 = 'S'
139000040713     C                   EVAL      FLGVAX = 'S'
139100040713     C                   ENDIF
139200040713     C                   EVAL      SkEXT(SkJ) = DSSKEXT
139300040713     C*
139400040713     C                   ENDIF
139500040713     C     KEYtabP       READE     TABEL00F                               97
139600040713     C                   ENDDO
139700031106     C*
139800031106     C                   ENDSR
139900070802     C*----------------------------------------------------*
140000070802      /TITLE Gestione errore generico
140100070802     C*----------------------------------------------------*
140200070802     C     *pssr         BEGSR
140300070802     C*
140400070802     C* Se errore sconosciuto => emetto operazione d ROLLBACK
140500070802     C                   ROLBK
140600070820     C*
140700070820     C* Infine arresto il controllo sincronia
140800070820     C                   CALL      'TRTC85C'
140900070820     C                   PARM      'END'         OPZ               3
141000070802     C*
141100070802     C                   ENDSR     '*CANCL'
141200960924     C*----------------------------------------------------*
141300960924     C*   *INZSR: Operazioni iniziali                      *
141400960924     C*----------------------------------------------------*
141500960924     C     *INZSR        BEGSR
141600960924     C*
141700960924     C     *ENTRY        PLIST
141800960924     C                   PARM                    KPJBA
141900051130     c                   movel     kpjbu         param
142000051130     c* Se richiamo per trasferimento accendo indicatore 01
142100051130     c                   if        trasfe='S'
142200051130     c                   seton                                        01
142300051130     c                   endif
142400960924     C*  Richiamo XPARUT
142500960924     C                   Z-ADD     1             CODUT
142600960924     C                   CALL      'X§PARUT'
142700960924     C                   PARM                    UT§DSE
142800960924     C                   MOVEL     REC80         CNCR80
142900031106     C*
143000031106     C* VERIABILI RIFERITE AL DATABASE
143100031106     C     *LIKE         DEFINE    TBLKUT        KTBute
143200031106     C     *LIKE         DEFINE    TBLCOD        KTBcod
143300031106     C*
143400031106     C* CHIAVE DI LETTURA X TABEL00F - Parziale
143500031106     C     KEYtabP       KLIST
143600031106     C                   KFLD                    KTBute
143700031106     C                   KFLD                    KTBcod
143800041115     C     Kvab5         KLIST
143900041115     C                   KFLD                    kccm
144000041115     C                   KFLD                    kcmr
144100051206     C     Kvab3         KLIST
144200051206     C                   KFLD                    v2cfil
144300051206     C                   KFLD                    v2cccm
144400051206     C     Kdett         KLIST
144500051206     C                   KFLD                    v2cfil
144600051206     C                   KFLD                    v2cccm
144700051206     C                   KFLD                    v2ccmr
144800051206     C                   KFLD                    v2ccnt
144900141112     C* Definizione chiavi - TNTBE01L
145000141112     C     K02tbe01      KLIST
145100141112     C                   KFLD                    tbeCOD
145200141112     C                   KFLD                    tbeKE1
145300121127     C*
145400121127     C                   Z-ADD     1             CODUT
145500121127      * Reperimento dati utente/aziendali
145600121127     c                   exsr      sr_DatiJob
145700031106      *
145800031106      * CARICO LE TABELLE OCCORRENTI
145900031106     C                   EXSR      CARTAB
146000020919      *
146100020919     C                   MOVEL     kpjbu         KPJBUs
146200020919      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
146300020919     C                   CLEAR                   TRUL06DS
146400020919     C                   MOVE      '£1'          D06COD
146500020919     C                   MOVEL     SIMFEL        D06KEY
146600020919     C                   MOVEL     'L'           D06TLA
146700020919     C                   MOVEL     TRUL06DS      KPJBU
146800020919      *
146900020919     C                   CALL      'TRUL06R'
147000020919     C                   PARM                    KPJBA
147100020919     C                   MOVEL     KPJBU         TRUL06DS
147200020919     C                   MOVEL     kpjbus        KPJBU
147300020919      *
147400960924     C*  Ricerca capoconti
147500960924     C                   DO        50            X
147600960924     C                   MOVE      TCU(X)        TCUDS
147700960924     C     F56           IFEQ      'CG'
147800960924     C     F34           ANDEQ     '01'
147900960924     C                   Z-ADD     KCU(X)        KCI               4 0
148000960924     C                   END
148100960924     C                   END
148200960924     C                   MOVEL     RAGUT         RSUT
148300960924     C*  Chiavi di accesso
148400960924     C     KACO          KLIST
148500960924     C                   KFLD                    KKUT
148600960924     C                   KFLD                    KKCC
148700960924     C                   KFLD                    KKSC
148800961029     C     KTAB          KLIST
148900961029     C                   KFLD                    KKUT
149000961029     C                   KFLD                    KCOD1
149100030925     C     KVAD          KLIST
149200030925     C                   KFLD                    WSVFGS
149300030925     C                   KFLD                    WSVCCM
149400030925     C                   KFLD                    WSVCMR
149500030925     C                   KFLD                    WSVCNT
149600960924     C*  Definizione variabili
149700960924     C     *LIKE         DEFINE    ACOKUT        KKUT
149800960924     C     *LIKE         DEFINE    ACOKCC        KKCC
149900960924     C     *LIKE         DEFINE    ACOKSC        KKSC
150000960924     C     *LIKE         DEFINE    TABCOD        KCOD
150100961029     C     *LIKE         DEFINE    TBLCOD        KCOD1
150200960930     C     *LIKE         DEFINE    VABCCM        KCCM
150300030617     C     *LIKE         DEFINE    VABCMR        KCMR
150400960930     C     *LIKE         DEFINE    VABCCM        WSVCCM
150500960930     C     *LIKE         DEFINE    VABCNT        WSVCNT
150600960930     C     *LIKE         DEFINE    VABCMR        WSVCMR
150700960930     C     *LIKE         DEFINE    VABHMS        WSVHMS
150800960930     C     *LIKE         DEFINE    VABDTS        WSVDTS
150900960930     C     *LIKE         DEFINE    VABDCM        WSVDCM
151000030925     C     *LIKE         DEFINE    VABFGS        WSVFGS
151100031118     C     *LIKE         DEFINE    VABNCL        WSVNCL
151200031118     C     *LIKE         DEFINE    VABNCL        WSVSPE
151300031118     C     *LIKE         DEFINE    VABPKB        WSVPKB
151400960924     C*  Carico in schiera codici tabella - PT -
151500961029     C                   Z-ADD     0             X                 4 0
151600960924     C                   Z-ADD     0             KSC
151700960924     C                   MOVEL     'PT'          KCOD
151800960924     C     KCOD          CHAIN     EDTAB01L                           31
151900960924     C     *IN31         DOWEQ     '0'
152000960924     C     TABFLG        IFEQ      ' '
152100020919     C                   MOVEL     TABUNI        DSPT
152200020919      * solo se compresa nelle linee di partenza del terminal
152300020919     c     §ptlnp        lookup    Lin                                    32
152400020919     c                   if        *in32 = *on
152500960924     C                   ADD       1             X
152600961024     C                   MOVEL     TABKEY        CPT(X)
152700961024     C                   Z-ADD     §PTKSC        KSC(X)
152800020919     c                   end
152900960924     C                   END
153000960924     C     KCOD          READE     EDTAB01L                               31
153100960924     C                   END
153200021015      *
153300021015      * Salva il contatore
153400021015     c                   z-add     x             xx                3 0
153500000915      *
153600021015      * Carica Clienti da tabella CL
153700021015     C                   MOVEL     'CL'          TABCOD
153800021015     C     TABCOD        CHAIN     EDTAB01L                           30
153900021015     C     *IN30         DOWEQ     '0'
154000021015     C     TABFLG        IFEQ      ' '
154100021015     C                   MOVEL     TABKEY        CODCL             7 0
154200021015     C     CODCL         LOOKUP    ksc                                    32
154300021015     c                   if        *in32 = *on
154400021015     c                   movel     tabuni        lnpCL             3 0
154500021015     c     LnpCL         lookup    Lin                                    32
154600021015     c                   if        *in32 = *on
154700021015     c                   add       1             xx
154800021015     C                   MOVEL     TABUNI        KSC(xx)
154900021015     C                   endif
155000021015     c                   endif
155100021015     C                   END
155200021015     C     TABCOD        READE     EDTAB01L                               30
155300021015     C                   END
155400021015      *
155500021015      * Salva il contatore
155600021015     c                   z-add     xx            x
155700021015      *
155800021015      * Caricamento Tabella Depot DEP
155900000915     C     'DEP'         CHAIN     TNTBE01L                           31
156000000915      *
156100000915     C     *IN31         DOWEQ     '0'
156200000915      * Se il S.I. è indicato deve essere uguale al mio
156300000915     C     TBEATB        IFEQ      ' '
156400000915     C     TBEKE2        ANDEQ     'I'
156500000915     C     TBESIF        IFEQ      KNSIF
156600000915     C     TBESIF        OREQ      *BLANK
156700020919     C                   MOVEL     TBEUNI        DDEP
156800020919      * solo se compresa nelle linee di partenza del terminal
156900020919     c     §dplnp        lookup    Lin                                    32
157000020919     c                   if        *in32 = *on
157100000918     C                   ADD       1             X
157200000915     C                   Z-ADD     §DPKSC        KSC(X)
157300020919     c                   end
157400000915     C                   END
157500000915     C                   END
157600000915     C     'DEP'         READE     TNTBE01L                               31
157700000915     C                   ENDDO
157800120424      *
157900120424      * Caricamento Tabella Depot 3BC bypass ctrl
158000120424     C                   Z-ADD     0             wY
158100120424     C     '3BC'         CHAIN     TNTBE01L                           31
158200120424      *
158300120424     C     *IN31         DOWEQ     '0'
158400120424      * Se il S.I. è indicato deve essere uguale al mio
158500120424     C     TBEATB        IFEQ      ' '
158600120502     C     TBEKE2        ANDEQ     *blank
158700120424     C     TBESIF        IFEQ      KNSIF
158800120424     C     TBESIF        OREQ      *BLANK
158900120424     C                   MOVEL     TBEUNI        D3BC
159000120424      * salvo il  cliente solo se il bypass relativo al VAX per CMR è attivo
159100120424     C                   IF        §3BCCCC = 'S'
159200120424     c                   if        %lookup(%dec(%trim(%subst(TBEKe1 : 1 : 7))
159300120502     C                              : 7 : 0) : CCM) = 0
159400120424     C                   ADD       1             wY
159500120502     C                   EVAL      CCM(wY) =
159600120424     C                             %dec(%trim(%subst(TBEKe1 : 1 : 7)) : 7 : 0)
159700120424     c                   end
159800120424     C                   ENDIF
159900120424     C                   END
160000120424     C                   END
160100120502     C     '3BC'         READE     TNTBE01L                               31
160200120424     C                   ENDDO
160300000915      *
160400961024     C                   MOVEL     'PU'          TABCOD
160500961024     C     TABCOD        CHAIN     EDTAB01L                           30
160600961024     C     *IN30         DOWEQ     '0'
160700961024     C     TABFLG        IFEQ      ' '
160800961024     C                   MOVEL     TABKEY        CODPU            35
160900961024     C                   Z-ADD     1             X
161000961024     C     CODPU         LOOKUP    CPT(X)                                 32
161100961024     C     *IN32         IFEQ      '1'
161200961024     C                   MOVEL     TABUNI        DPU(X)
161300961024     C                   END
161400961024     C                   END
161500961024     C     TABCOD        READE     EDTAB01L                               30
161600961024     C                   END
161700960924     C*  Inizializzo variabili
161800960924     C                   MOVEL     'N'           WFINE             1
161900960924     C*
162000960924     C                   ENDSR
162100121127
162200121127      *---------------------------------------------------------------*
162300121127      *?Reperimento dati del job (Utente/Operativi)                  ?*
162400121127      *---------------------------------------------------------------*
162500121127     c     sr_DatiJob    BEGSR
162600121127      *
162700121127     c     *dtaara       define    §azute        AZUTEds
162800121127     c     *dtaara       define    §datiute      dDatiUte
162900121127      *
163000121127     c                   in(E)     *dtaara
163100121127     c                   IF        %Error or RSUT = *blanks
163200121127     c                   clear                   TIBS34ds
163300121127     c                   call      'TIBS34R'
163400121127     c                   parm                    TIBS34ds
163500121127     c                   in        *dtaara
163600121127     c                   ENDIF
163700121127      *
163800121127     c                   ENDSR
163900960924**
164000960924Conto cliente inesistente o annullato
164100960924Conto cliente non presente nella tabella dei partner esteri
164200960924Effettuare almeno una scelta
164300031110Il cliente selezionato non è abilitato al raggruppamento bolle ("3C")
164400031120Per raggruppamento bolle selezionare CMR di un solo cliente
164500051130Opzione errata
164600121022Inserire la Filiale che deve ricevere i dati !!
164700141113Impossibile trasferire i dati: presente il nm. CMR nella filiale di ricezione
164800121119La Filiale che deve ricevere i dati non è in organigramma
164900121126La Filiale che deve ricevere i dati non è tra i network possibili
165000141113Filiale di conferma bolle NON abilitata per il cliente, interpellare il POC
165100141113Network Filiale di conf. bolle diverso da quello cliente, interpellare il POC
165200051130**
165300051213 *** Conferma bolle per numero CMR ***
165400121022 * Trasmissione CMR ad altra filiale *

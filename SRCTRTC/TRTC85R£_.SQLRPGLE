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
003900121119     D ERR             S             70    DIM(10) CTDATA PERRCD(1)
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
006500960924     D* Ds scomposzione tipo capoconti
006600960924     D TCUDS           DS
006700960924     D  F34                    3      4
006800960924     D  F56                    5      6
006900000915     D DDEP          E DS
007000000915     D  §DPKSC       E                     EXTFLD(§DEPKSC)
007100000915     D  §DPLNP       E                     EXTFLD(§DEPLNP)
007200000915     D  §DPDES       E                     EXTFLD(§DEPDES)
007300960924     D DSPT          E DS                  EXTNAME(EDIDSPT)
007400961024     D DSPU          E DS                  EXTNAME(EDIDSPU)
007500960924     D TRTC86        E DS                  EXTNAME(TRTC86DS)
007600020919     D TRUL06DS      E DS
007700020919     D  LIN                    1     90  0 DIM(30)                              P.O. COMODO
007800960924     D*  Ds per inversione data
007900960924     D WLBDA8          DS
008000960924     D  G02DAT                 1      8  0
008100960924     D  G02INV                 9     16  0
008200960924     D  G02ERR                17     17
008300960924     D  G02TGI                18     22  0
008400960924     D                SDS
008500960924     D  NOMPGM                 1     10
008600120619     D  SDSusr               254    263
008700051130     D PARAM           DS
008800051130     D  trasfe                 1      1
008900030709     D*------------------
009000030709     D* DS REPERIMENTO DATI CLIENTE
009100030709     D*-------------------
009200030709     D BS69DS        E DS                  EXTNAME(TIBS69DS)
009300030709     D ACODS         E DS                  EXTNAME(CNACO00F)
009400030709     D INDDS         E DS                  EXTNAME(CNIND00F)
009500030709     D CLPDS         E DS                  EXTNAME(CNCLP00F)
009600030709     D CLSDS         E DS                  EXTNAME(FNCLS00F)
009700040713     D*------------------
009800040713     D* DS WRK X VERIFICA ROTTURA CHIAVE BOLLA SU VAX
009900040713     D*-------------------
010000040713     D DSKEYVAX        DS
010100040713     D  VAXFGS
010200040713     D  VAXCCM
010300040713     D  VAXCMR
010400040713     D  VAXCNT
010500040713     D  VAXAAS
010600040713     D  VAXLNP
010700040713     D  VAXNRS
010800040713     D  VAXNSP
010900040713     D*------------------
011000040713     D* DS WRK X VERIFICA ESTENSIONI RICHIESTE X OGNI CODICE TRATTAMENTO MERCE
011100040713     D*-------------------
011200040713     D DSSKEXT         DS
011300040713     D  FLGVAD                        1    INZ(*blanks)
011400040713     D  FLGVAT                        1    INZ(*blanks)
011500040713     D  FLGVAX                        1    INZ(*blanks)
011600040713     D*------------------
011700040713     D* VARIABILI D WRK
011800040713     D*-------------------
011900040713     D DSKEYVAXROT     S                   LIKE(DSKEYVAX) INZ(*blanks)
012000040713     D wTotVADCMR      S              9  0 INZ(*zeros)
012100040713     D wTotVATCMR      S              9  0 INZ(*zeros)
012200040713     D wTotVAXCMR      S              9  0 INZ(*zeros)
012300050708     D savD86CCM       S                   LIKE(D86CCM)
012400050708     D savD86CMR       S                   LIKE(D86CMR)
012500050708     D savD86CNT       S                   LIKE(D86CNT)
012600051206
012700051206     d StringaSql      s            500    Varying
012800120424
012900120424     D* Variabili di appoggio
013000120424     D wY              s              3s 0
013100120619     D Forcing         s               n
013200120620     D Exfmt           s              1
013300120620     D Msg01           s             60
013400120620     D Msg02           s             60
013500120620     D Msg03           s             60
013600121127
013700121127      * - Parametri per richiamo al pgm di controllo profilo utenti
013800121127     d TIBS34ds      e ds
013900121127      * - Ds di riferimento al file esterno AzUte00F
014000121127     d AZUTEds       e ds                  ExtName(AzUte00F)
014100121127      * - Ds per dati organigramma
014200121127     d dDatiUte      e ds
014300121127     d  simfel_dut   e                     extfld(simfel)
014400960924     C*---------------------------------------------------------------------*
014500960924     C* Ciclo principale
014600960924     C*---------------------------------------------------------------------*
014700070820     C*
014800070820     C*  Avvio sempre e cmq il controllo sincronia
014900070820     C                   CALL      'TRTC85C'
015000070820     C                   PARM      'STR'         OPZ               3
015100070820     C*
015200960924     C* Inizializzo prima videata
015300960924     C                   EXSR      INZ01
015400960924      * Loop gestione videate
015500960924     C     WFINE         DOWEQ     'N'
015600960924     C     WTPVID        CASEQ     '1D'          GESD01                         Sce.cliente
015700960924     C     WTPVID        CASEQ     '2S'          GESS02                         Subfile
015800960924     C     WTPVID        CASEQ     '2D'          GESD02                         Subfile vuoto
015900960924     C                   END
016000960924     C                   END
016100960924      * Fine Lavoro
016200960924     C     FINE          TAG
016300070802     C*
016400070802     C* Infine arresto il controllo sincronia
016500070802     C                   CALL      'TRTC85C'
016600070802     C                   PARM      'END'         OPZ               3
016700070802     C*
016800960924     C                   SETON                                        LR
016900960924     C*---------------------------------------------------------------*
017000960924     C*  INZ01: Inizializzo prima videata
017100960924     C*---------------------------------------------------------------*
017200960924     C     INZ01         BEGSR
017300960924     C*
017400960924     C                   SETOFF                                       2840
017500960924     C                   MOVEL     *BLANKS       $MSG
017600960924     C                   Z-ADD     0             V1CCCM
017700960924     C                   MOVEL     NOMPGM        V1CPGM
017800051213     C  n01              MOVE      tes(1)        V1Ctes
017900051213     C   01              MOVE      tes(2)        V1Ctes
018000960924     C                   MOVEL     '1D'          WTPVID
018100030617     C                   MOVEL     'K'           V1CORD
018200031106     C                   MOVEL     *BLANKS       V1CCMR
018300031106     C                   MOVEL     'N'           V1CABD
018400960924     C*
018500960924     C                   ENDSR
018600960924     C*---------------------------------------------------------------*
018700960924     C*  INZ02: Inizializzo seconda videata
018800960924     C*---------------------------------------------------------------*
018900960924     C     INZ02         BEGSR
019000960924     C*
019100070820     C*  Avvio sempre e cmq il controllo sincronia
019200031113     C                   CALL      'TRTC85C'
019300070820     C                   PARM      'EXE'         OPZ               3
019400031113     C*
019500960924     C                   SETOFF                                       28
019600960924     C                   MOVEL     *BLANKS       $MSG
019700051206     c                   clear                   v2cfil
019800960924     C                   MOVEL     NOMPGM        V2CPGM
019900051213     C  n01              MOVE      tes(1)        V2Ctes
020000051213     C   01              MOVE      tes(2)        V2Ctes
020100960924     C*  Pulisco subfile
020200960926     C                   Z-ADD     0             NRR2
020300960924     C                   SETOFF                                       2021
020400960924     C                   WRITE     TC85C02
020500960924     C                   SETON                                        2021
020600031118     C*  Inizializzo variabili d appoggio e d wrk
020700031106     C                   CLEAR                   WSVCNT
020800031106     C                   CLEAR                   WSVCMR
020900031106     C                   CLEAR                   WSVCCM
021000040405     C                   CLEAR                   WSVDTS
021100040405     C                   CLEAR                   WSVHMS
021200031118     C                   CLEAR                   WSVSPE
021300031118     C                   CLEAR                   WSVNCL
021400031118     C                   CLEAR                   WSVPKB
021500040713     C                   CLEAR                   wTotVADCMR
021600040713     C                   CLEAR                   wTotVATCMR
021700040713     C                   CLEAR                   wTotVAXCMR
021800031118     C                   Z-ADD     *zeros        wGiro             1 0
021900030617      *
022000030617      *  Se ordinamento x cliente
022100041115     C                   IF        *in54
022200960924      *  ... se ho selezionato un codice partner carico solo quello
022300960924     C     V1CCCM        IFNE      0
022400960926     C                   Z-ADD     V1CCCM        KCCM
022500960926     C                   Z-ADD     V1CCCM        WCCMAL
022600960924     C                   ELSE
022700960924      *  ... altrimenti comincio leggere dal primo codice
022800960924     C                   Z-ADD     0             KCCM
022900960924     C                   Z-ADD     9999999       WCCMAL            7 0
023000030617     C                   ENDIF
023100041115     c* Verifico se presente anche cmr per fare lettura con 2 campi chiave
023200041115     C     V1CCMR        IFNE      *blanks
023300041115     C                   MOVEL(P)  V1CCMR        KCMR
023400041115     C                   ELSE
023500041115     C                   MOVEL     *blanks       KCMR
023600041115     C                   ENDIF
023700030617     C                   ENDIF
023800030617      *
023900030617      *  Se ordinamento x CMR
024000041115     C                   IF        *in55
024100030617      *  ... se ho selezionato un numero CMR carico solo quello
024200030617     C     V1CCMR        IFNE      *blanks
024300030617     C                   MOVEL(P)  V1CCMR        KCMR
024400030617     C                   MOVEL(P)  V1CCMR        WCMRAL
024500030617     C                   ELSE
024600030617      *  ... altrimenti comincio leggere dal primo numero CMR
024700030617     C                   MOVEL     *blanks       KCMR
024800030617     C                   MOVEL     *hival        WCMRAL           35
024900030617     C                   ENDIF
025000030617     C                   ENDIF
025100960924     C*  Prima lettura
025200041115     c                   select
025300041115     c                   when      *iN54   and kcmr<>*BLANKS
025400041123     C     KVAB5         SETLL     Edivab1L
025500041115     C                   WHEN      *IN54
025600041123     C     KCCM          SETLL     Edivab1L
025700041115     C                   WHEN      *IN55
025800041123     C     KCMR          SETLL     Edivab4L
025900041115     C                   ENDSL
026000041115     C
026100020919     c                   exsr      ReadVAB
026200041115     C
026300960924      *  Eseguo caricamento prima pagina
026400041123     C   54              EXSR      ROLLUP01
026500041123     C   55              EXSR      ROLLUP04
026600960924      *  Se non ho dati da modificare emetto subfile vuoto
026700960924     C     NRR2          IFEQ      0
026800960924     C                   MOVEL     '2D'          WTPVID            2
026900960924     C                   ELSE
027000960924     C                   MOVEL     '2S'          WTPVID
027100960924     C                   END
027200960924     C*
027300960924     C                   ENDSR
027400960924     C*---------------------------------------------------------------*
027500020919     C*  READVAB: Legge il record di EDIVAB
027600960924     C*---------------------------------------------------------------*
027700020919     C     ReadVAB       BEGSR
027800960924     C*
027900020919     c     rileggi       tag
028000041115     c
028100041115     C                   SELECT
028200041115     C                   WHEN      *IN54 AND KCCM>0 AND KCMR<>*BLANKS
028300041123     C     KVAB5         READE     Edivab1L                               23
028400041115     C                   WHEN      *IN54 AND KCCM>0
028500041123     C     kccm          READe     Edivab1L                               23
028600041115     C                   WHEN      *IN54
028700041123     C                   READ      Edivab1L                               23
028800041115     C                   WHEN      *IN55 AND KCMR<>*BLANKS
028900041123     C     kcmr          READe     Edivab4L                               23
029000041115     C                   WHEN      *IN55
029100041123     C                   READ      Edivab4L                               23
029200041115     c                   endSL
029300020919     C*
029400020919     C*  controlla se non è compresa fra le linee gestite dal terminal
029500020919     C*   quindi legge nuovo record.
029600030617     c                   if        *in54 = *on        AND                       * ordinam. edivab1l
029700041123     c                             not %eof(Edivab1L)
029800020919     c     vabfgs        cabeq     0             rileggi
029900020919     c     vabfgs        lookup    lin                                    32
030000020919     c     *in32         cabeq     *off          rileggi
030100030617     c                   endif
030200030617     c                   if        *in55 = *on        AND                       * ordinam. edivab4l
030300041123     c                             not %eof(Edivab4L)
030400030617     c     vabfgs        cabeq     0             rileggi
030500030617     c     vabfgs        lookup    lin                                    32
030600030617     c     *in32         cabeq     *off          rileggi
030700030617     c                   endif
030800031106     C*
030900130103     C* se sono entrato mediante l'azione TC8A, non servono test sull'ABD
031000130103     C* il rcd letto verrà scritto sul sfl
031100130103     C                   IF        %subst(KPJBU : 1 : 1) <> 'S'
031200031106     C*  controlla se il cliente è abilitato in tabella 3C all'accorpamento bolle x
031300031106     C*  medesimo destinatario in relazione al relativo parametro richiesto a video
031400041115     C                   if        *in54 = *on        AND                       * ordinam. edivab5l
031500041123     C                             not %eof(Edivab1L)
031600031106     C     vabCCM        lookup    KSCABD                                 32
031700031106     C                   if        (*in32 = *off AND V1CABD = 'N') OR
031800031106     C                             (*in32 = *on  AND V1CABD = 'S')
031900031106     C                   else
032000031106     C                   goto      rileggi
032100031106     C                   endif
032200031106     C                   endif
032300041115     C                   if        *in55 = *on        AND                       * ordinam. edivab6l
032400041123     C                             not %eof(Edivab4L)
032500031106     C     vabCCM        lookup    KSCABD                                 32
032600031106     C                   if        (*in32 = *off AND V1CABD = 'N') OR
032700031106     C                             (*in32 = *on  AND V1CABD = 'S')
032800031106     C                   else
032900031106     C                   goto      rileggi
033000031106     C                   endif
033100031106     C                   endif
033200130103     C*
033300130103     C                   ENDIF
033400020919     C*
033500020919     C                   ENDSR
033600020919     C*---------------------------------------------------------------*
033700020919     C*  GESD01: Gestione prima videata
033800020919     C*---------------------------------------------------------------*
033900020919     C     GESD01        BEGSR
034000020919     C*
034100960924     C                   EXFMT     TC85D01
034200960924     C                   SETOFF                                       2840
034300960924     C*  Fine Lavoro
034400960924     C     *INKC         IFEQ      '1'
034500960924     C                   MOVEL     'S'           WFINE
034600960924     C                   GOTO      FINVD1
034700960924     C                   END
034800960924     C*  Controlli
034900960924     C                   EXSR      CTR01
035000960924     C*  Caricamento subfile
035100960924     C  N28              EXSR      INZ02
035200960924     C*
035300960924     C     FINVD1        ENDSR
035400960924     C*---------------------------------------------------------------*
035500960924     C*  GESS02: Gestione subfile pieno
035600960924     C*---------------------------------------------------------------*
035700960924     C     GESS02        BEGSR
035800960924     C*
035900031110     C     EMIVS2        TAG
036000051201     C                   WRITE     TC85Z02
036100960924     C                   EXFMT     TC85C02
036200120620     C* memo che c'è stata una exfmt del sfl
036300120620     C                   EVAL      Exfmt = *on
036400031110     C*  Spengo indicatore di errore sui record del sub-file
036500031110     C                   SETOFF                                       28
036600960924     C*  Fine Lavoro
036700960924     C     *INKC         IFEQ      '1'
036800960924     C                   MOVEL     'S'           WFINE
036900960924     C                   GOTO      FINVS2
037000960924     C                   END
037100960924     C*  Ritorno
037200960924     C     *INKL         IFEQ      '1'
037300960924     C                   EXSR      INZ01
037400960924     C                   GOTO      FINVS2
037500960924     C                   END
037600960924     C*  Controlli
037700960930     C                   EXSR      CTR02
037800031110     C   28              GOTO      EMIVS2
037900960924     C*
038000960924     C     FINVS2        ENDSR
038100960924     C*---------------------------------------------------------------*
038200960924     C*  GESD02: Gestione subfile vuoto
038300960924     C*---------------------------------------------------------------*
038400960924     C     GESD02        BEGSR
038500960924     C*
038600960924     C                   EXFMT     TC85D02
038700960924     C*  Fine Lavoro
038800961028     C     *INKC         IFEQ      '1'
038900961028     C                   MOVEL     'S'           WFINE
039000960924     C                   GOTO      FINVD2
039100960924     C                   END
039200960924     C*  Ritorno
039300960924     C     *INKL         IFEQ      '1'
039400960924     C                   EXSR      INZ01
039500960924     C                   GOTO      FINVD2
039600960924     C                   END
039700960924     C*
039800960924     C     FINVD2        ENDSR
039900960924     C*---------------------------------------------------------------*
040000960924     C*  CTR01: Controlli prima videata
040100960924     C*---------------------------------------------------------------*
040200960924     C     CTR01         BEGSR
040300031106     C*
040400031106     C* Inizializzo variabili d wrk
040500031106     C                   CLEAR                   WDTA
040600960924     C*
040700960924     C*  Se immesso il codice cliente ...
040800960924     C     V1CCCM        IFNE      0
040900960924     C*  Controllo se esiste su CNACO
041000960924     C                   Z-ADD     1             KKUT
041100960924     C                   Z-ADD     KCI           KKCC
041200960924     C                   Z-ADD     V1CCCM        KKSC
041300960924     C     KACO          CHAIN     CNACO00F                           30
041400960924     C     *IN30         IFEQ      '1'
041500960924     C     ACOFLG        ORNE      ' '
041600960924     C                   SETON                                        2840
041700960924     C                   MOVEL     ERR(1)        $MSG
041800960924     C                   GOTO      FINCT1
041900960924     C                   END
042000960924     C*  Controllo se è un partner
042100961024     C                   Z-ADD     1             XY                3 0
042200970602     C     V1CCCM        IFEQ      3200004
042300970602     C     0050743       LOOKUP    KSC(XY)                                32
042400970602     C                   ELSE
042500961024     C     V1CCCM        LOOKUP    KSC(XY)                                32
042600970602     C                   END
042700960924     C     *IN32         IFEQ      '0'
042800031106     C***                SETON                                        2840
042900031106     C***                MOVEL     ERR(2)        $MSG
043000031106     C***                GOTO      FINCT1
043100961024     C                   ELSE
043200961024     C                   MOVEL     DPU(XY)       DSPU
043300961024     C                   MOVEL     §PUDTA        WDTA              1
043400960924     C                   END
043500960924     C                   END
043600961029     C*  Reperisco codice cliente x lnp bolla
043700031107     C***                Z-ADD     §PTKSC        WKSC              7 0
043800030617     C*  Verifico il tipo d ordinamento richiesto
043900030617     C                   SETOFF                                       5455
044000041115     c                   select
044100041115     c* Se selezionato cod cliente ordinamento come se fosse per cli
044200041115     c* anche per CMR, tanto la lf dopo il cliente ha cmr
044300041115     C                   when      v1cccm>0
044400030617     C                   SETON                                        54
044500041115     c* Se selezionato CMR ordinamento come se fosse per CMR
044600041115     c* anche per cliente, tanto la lf dopo il cmr ha il cliente
044700041115     C                   when      v1ccmr<>*blanks
044800041115     C                   SETON                                        55
044900041115     c* altrimenti il tipo ordinamento
045000041115     C                   when      V1CORD = 'C'
045100030617     C                   SETON                                        55
045200041115     c                   other
045300041115     C                   SETON                                        54
045400041115     C                   ENDsl
045500030617     C*
045600960924     C     FINCT1        ENDSR
045700960924     C*---------------------------------------------------------------*
045800960924     C*  CTR02: Controlli seconda videata
045900960924     C*---------------------------------------------------------------*
046000960924     C     CTR02         BEGSR
046100040713     C*
046200040713     C                   SETOFF                                       70
046300031118     C*
046400031118     C*  Inizializzo la variabile d wrk relativa al controllo: accorpamento possibile solo su
046500031118     C*  singolo cliente
046600031118     C                   Z-ADD     *zeros        wCCM              7 0
046700960924     C*
046800960924     C*  Controllo selezioni effettuate
046900960924     C                   MOVEL     'N'           WSCELT            1
047000960930     C                   MOVEL     'N'           WSTAMP            1
047100031118     C                   Z-ADD     *ZEROS        KEYNRR2           4 0
047200051201    1C     1             DO        wNRR2         KEYNRR2
047300031118     C     KEYNRR2       CHAIN     TC85S02
047400040713     C*
047500040713     C* Reimposto gli indicatori in funzione dei campi hiddend d salvataggio
047600040713     C                   EVAL      *IN59 = WIN59
047700040713     C                   EVAL      *IN60 = WIN60
047800040713     C                   EVAL      *IN61 = WIN61
047900040713     C                   EVAL      *IN62 = WIN62
048000040713     C                   EVAL      *IN63 = WIN63
048100040713     C                   EVAL      *IN64 = WIN64
048200040713     C*
048300051201    2C     V2CSCE        IFNE      ' '
048400051130     c* controllo se opzione errata
048500051201    3c                   if        v2csce='7' and not *in01
048600051130     C                   MOVEL     ERR(6)        $MSG
048700051130     C                   SETON                                        28
048800051201    3c                   endif
048900051201    3c                   if        v2csce<>'7' and *in01
049000051130     C                   MOVEL     ERR(6)        $MSG
049100051130     C                   SETON                                        28
049200051201    3c                   endif
049300051130     c
049400051201    3c                   if        *in28
049500051130     c                   seton                                        70
049600051130     C                   UPDATE    TC85S02
049700051130     c                   setoff                                       70
049800051130     C                   LEAVE
049900051201    3c                   endif
050000051130     c
050100960924     C                   MOVEL     'S'           WSCELT
050200960924     C*  Richiamo pgm per esecuzione scrittura VAB
050300960924     C                   CLEAR                   TRTC86
050400961024     C                   MOVEL     WDTA          D86DTA
050500040713     C*
050600051201     C* X opzioni 1 e 9 d conferma bolle proseguo solo se no errori su estensioni
050700051201    3C                   IF        V2CSCE = '1' OR  v2csce='7'
050800051201     C                             or V2CSCE = '9'
050900120619
051000120619     C* se c'è un errore
051100120619    4C                   IF        (V2CERRVAD <> *blanks OR
051200120619     C                              V2CERRVAT <> *blanks OR
051300120619     C                              V2CERRVAX <> *blanks   )
051400120619     C* ma l'utente è un EDP*
051500120619     C                             and %subst(SDSusr : 1 : 3) = 'EDP'
051600120619     C* chiedo se voglio forzare
051700120622     C                   EVAL      Msg01 = 'Dettagli mancanti rispetto a CTM: +
051800120622     C                             F6 per forzare'
051900120620     C                   EVAL      Msg02 = 'Codice cliente mittente: ' +
052000120622     C                                     %editc(V2CCCM:'X')  +
052100120622     C                                     '  Numero CMR: ' +
052200120622     C                                     V2CCMR
052300120620     C                   EVAL      Msg03 = 'Utente forzatura: ' +
052400120619     C                                        SDSusr
052500120619     C                   EXSR      WdwForce
052600120619     C* se richiesta forzatura
052700120619     C                   IF        Forcing = *on
052800120619     C                   EVAL      V2CERRVAD = *blank
052900120619     C                   EVAL      V2CERRVAT = *blank
053000120619     C                   EVAL      V2CERRVAX = *blank
053100120620     C                   EVAL      S02Forced = *on
053200120620     C                   UPDATE    TC85S02
053300120620     C* mi riposiziono sul rcd di sfl letto
053400120620     C     KEYNRR2       CHAIN     TC85S02
053500120619     C                   ENDIF
053600120619     C                   ENDIF
053700120619
053800051201    4C                   IF        V2CERRVAD <> *blanks OR
053900040713     C                             V2CERRVAT <> *blanks OR
054000040713     C                             V2CERRVAX <> *blanks
054100040713     C* Emetto errori relativi a dettagli mancanti
054200040713     C                   SETON                                        2870
054300051201     C  n01              EVAL      $MSG = 'Impossibile confermare, dettagli ' +
054400040713     C                             'mancanti:'
054500051201     C   01              EVAL      $MSG = 'Impossibile TRASFERIRE, dettagli ' +
054600051201     C                             'mancanti:'
054700070808     C   03              EVAL      $MSG = 'Impossibile accorpare in presenza' +
054800070808     C                             ' di'
054900051201    5C                   IF        V2CERRVAD <> *blanks
055000040713     C                   EVAL      $MSG = %trim($MSG) + ' VAD'
055100051201    5C                   ENDIF
055200051201    5C                   IF        V2CERRVAT <> *blanks
055300040713     C                   EVAL      $MSG = %trim($MSG) + ' VAT'
055400051201    5C                   ENDIF
055500051201    5C                   IF        V2CERRVAX <> *blanks
055600040713     C                   EVAL      $MSG = %trim($MSG) + ' VAX'
055700051201    5C                   ENDIF
055800040713     C                   UPDATE    TC85S02
055900040713     C                   LEAVE
056000051201    4C                   ENDIF
056100051201    3C                   ENDIF
056200040713     C*
056300051201     c* elaboro il trasferimento: ulteriori controlli
056400051201     c                   if        v2csce='7'
056500051201     c                   exsr      contrTrasfe
056600051206     c
056700051206     c                   if        *in28
056800051206     C                   UPDATE    TC85S02
056900051206     c                   setoff                                       70
057000051206     C                   LEAVE
057100051206     c                   endif
057200051206     c
057300051206     c   kf              exsr      EseguiTrasfe
057400120620     C* se c'è stata conferma e il rcd era forzato
057500120620     C                   IF        *inKF = *on and S02Forced = *on
057600120620     C* invio email di conferma forzatura
057700120620     C                   EXSR      EmailForza
057800120620     C                   ENDIF
057900120620     C*
058000051206     c                   iter
058100051201     c                   endif
058200051201     c
058300051201    3C     V2CSCE        IFEQ      '1'
058400960924     C                   MOVEL     'E'           D86MOD
058500960927     C                   MOVEL     ' '           D86STA
058600051201    3C                   ENDIF
058700960924     C*  Richiamo pgm per stampa dati
058800091124    3C**   V2CSCE        IFEQ      '6'
058900091124     C**                 MOVEL     'S'           WSTAMP            1
059000091124     C**                 MOVEL     *BLANKS       D86MOD
059100091124     C**                 MOVEL     'S'           D86STA
059200091124    3C**                 ENDIF
059300020919     C*  Richiamo pgm per cancellazione Edivab
059400051201    3C     V2CSCE        IFEQ      '4'
059500960924     C                   MOVEL     'C'           D86MOD
059600960924     C                   MOVEL     *BLANKS       D86STA
059700051201    3C                   ENDIF
059800091124     c
059900031110     C*  Se opzione diversa da 9 => eseguo *pgm standard, altrimenti eseguo *pgm x raggruppamento
060000031113     C*  bolle e scrittura FIVAB/FIVAD
060100091124     c                   select
060200091124    3C     V2CSCE        wheneq    '9'
060300031110     C*  Verifico che x opzione 9 il cliente sia abilitato nella tabella 3C all'accorpamento
060400031113     C*  x medesimo destinatario
060500031110     C     V2CCCM        LOOKUP    KSCABD                                 32
060600051201    4C                   IF        (*IN32 = *OFF) OR
060700031201     C*  e che sia stato scelto ingresso x accorpamento bolle, altrimenti do errore
060800031201     C                             V1CABD <> 'S'                                * ridondante
060900031110     C                   MOVEL     ERR(4)        $MSG
061000040713     C                   SETON                                        2870
061100040713     C                   UPDATE    TC85S02
061200031113     C                   LEAVE
061300051201    4C                   ENDIF
061400031118     C* Verifico se impostata opzioine 9 (raggruppamento) su clienti diversi (ERRORE)
061500051201    4C                   IF        wCCM > *zeros
061600051201    5C                   IF        V2CCCM <> wCCM
061700031118     C                   MOVEL     ERR(5)        $MSG
061800040713     C                   SETON                                        28
061900051201    5C                   ENDIF
062000051201   x4C                   ELSE
062100031118     C                   Z-ADD     V2CCCM        wCCM
062200051201    4C                   ENDIF
062300091124
062400091124     c* Opzione di stampa --> richiamo con RT per poi avere la stampa di riepilogo
062500091124    3C     V2CSCE        wheneq    '6'
062600091124     C                   MOVEL     'S'           WSTAMP            1
062700091124     C                   MOVEL     *BLANKS       D86MOD
062800091124     C                   MOVEL     'S'           D86STA
062900091124     C                   MOVEL     V2CCMR        D86CMR
063000091124     C                   MOVEL     'RT'          D86CHI
063100091124     C                   Z-ADD     V2CCNT        D86CNT
063200091124     C                   Z-ADD     V2CCCM        D86CCM
063300091124     C                   MOVEL     TRTC86        KPJBU
063400091124     C   KF              CALL      'TRTC86R1'
063500091124     C                   PARM                    KPJBA
063600120620     C* se c'è stata conferma e il rcd era forzato
063700120620     C                   IF        *inKF = *on and S02Forced = *on
063800120620     C* invio email di conferma forzatura
063900120620     C                   EXSR      EmailForza
064000120620     C                   ENDIF
064100120620     C*
064200091124     c
064300091124    3C     V2CSCE        whenne    ' '
064400960924     C                   MOVEL     V2CCMR        D86CMR
064500091124     c* richiamo con LR perchè carica i dati da elaborare nella INZSR
064600030617     C                   MOVEL     'LR'          D86CHI
064700960924     C                   Z-ADD     V2CCNT        D86CNT
064800961028     C                   Z-ADD     V2CCCM        D86CCM
064900960926     C                   MOVEL     TRTC86        KPJBU
065000031118     C   KF              CALL      'TRTC86C'
065100960924     C                   PARM                    KPJBA
065200120620     C* se c'è stata conferma e il rcd era forzato
065300120620     C                   IF        *inKF = *on and S02Forced = *on
065400120620     C* invio email di conferma forzatura
065500120620     C                   EXSR      EmailForza
065600120620     C                   ENDIF
065700120620     C*
065800091124    3C                   ENDSL
065900051201    2C                   ENDIF
066000091124
066100040713     C                   UPDATE    TC85S02
066200051201    1C                   ENDDO
066300031113     C*
066400031113     C*  Se nn ho errori in opzioni sub-file gestisco...
066500051201    1C                   IF        *IN28 = *OFF
066600031113     C                   Z-ADD     *ZEROS        KEYNRR2           4 0
066700051201    2C     1             DO        wNRR2         KEYNRR2
066800031113     C     KEYNRR2       CHAIN     TC85S02
066900051201    3C                   IF        %found
067000031201     C*  Se scelto ingresso x accorpamento bolle
067100051201    4C                   IF        V1CABD = 'S'
067200031201     C*  E se richiesta opzione 9 d accorpamento bolle
067300051201    5C                   IF        V2CSCE = '9'
067400031113     C                   MOVEL     'R'           D86MOD
067500031113     C                   Z-ADD     V2CCCM        D86CCM
067600031113     C                   MOVEL     V2CCMR        D86CMR
067700031113     C                   Z-ADD     V2CCNT        D86CNT
067800050708     C                   Z-ADD     V2CCCM        savD86CCM
067900050708     C                   MOVEL     V2CCMR        savD86CMR
068000050708     C                   Z-ADD     V2CCNT        savD86CNT
068100031113     C                   MOVEL     'LR'          D86CHI
068200070802     C   KF              CALL(e)   'TRTC86R3'
068300031113     C                   PARM                    TRTC86
068400120620     C* se c'è stata conferma e il rcd era forzato
068500120620     C                   IF        *inKF = *on and S02Forced = *on
068600120620     C* invio email di conferma forzatura
068700120620     C                   EXSR      EmailForza
068800120620     C                   ENDIF
068900120620     C*
069000070802     C* Se errore => rollback
069100070802     C                   IF        %error
069200070802     C                   ROLBK
069300070802     C                   ENDIF
069400070802     C*
069500051201    5C                   ENDIF
069600051201    4C                   ENDIF
069700051201    3C                   ENDIF
069800051201    2C                   ENDDO
069900031201     C*
070000031201     C*  Se scelto ingresso x accorpamento bolle
070100051201    2C                   IF        V1CABD = 'S'
070200031113     C*  Al termine del passaggio dati dai file "buoni" a quelli d lavoro eseguo il programma d
070300031113     C*  accorpamento e scrittuta FIVAB/FIVAD vero e proprio
070400040714     C                   MOVEL     'R'           D86MOD
070500050708     C                   Z-ADD     savD86CCM     D86CCM
070600050708     C                   MOVEL     savD86CMR     D86CMR
070700050708     C                   Z-ADD     savD86CNT     D86CNT
070800040714     C                   MOVEL     'LR'          D86CHI
070900070802     C   KF              CALL(e)   'TRTC86R4'
071000070420     C                   PARM                    KPJBA
071100040714     C                   PARM                    TRTC86
071200120620     C* se c'è stata conferma e il rcd era forzato
071300120620     C                   IF        *inKF = *on and S02Forced = *on
071400120620     C* invio email di conferma forzatura
071500120620     C                   EXSR      EmailForza
071600120620     C                   ENDIF
071700120620     C*
071800070802     C* Se errore => rollback
071900070802     C                   IF        %error
072000070802     C                   ROLBK
072100070802     C                   ELSE
072200070802     C* Se ok     => committ
072300070802     C                   COMMIT
072400070802     C                   ENDIF
072500070802     C*
072600051201    2C                   ENDIF
072700031113     C*
072800960924     C*  Se non ho selezionato nulla do errore
072900051201    2C     *INKF         IFEQ      '1'
073000051201    3C     WSCELT        IFEQ      'N'
073100040713     C                   SETON                                        28
073200960924     C                   MOVEL     ERR(3)        $MSG
073300051201   x3C                   ELSE
073400960924     C*  Se ho selezionato stampa richiamo pgm x totali genarali
073500051201    4C     WSTAMP        IFEQ      'S'
073600960926     C                   MOVEL     *BLANKS       D86STA
073700960924     C                   MOVEL     *BLANKS       D86CMR
073800960926     C                   MOVEL     *BLANKS       D86MOD
073900960926     C                   MOVEL     'S'           D86RIE
074000960924     C                   MOVEL     'LR'          D86CHI
074100960924     C                   Z-ADD     0             D86CNT
074200960924     C                   Z-ADD     0             D86CCM
074300031107     C***                Z-ADD     0             D86CCN
074400960926     C                   MOVEL     TRTC86        KPJBU
074500091127     C                   CALL      'TRTC86R1'
074600960924     C                   PARM                    KPJBA
074700051201    4C                   ENDIF
074800031118     C*  Al termine dell'esecuzione delle opzioni se è stato premuto F6 => ricarico sub-file
074900051201    4C     WSCELT        IFNE      'N'
075000031118     C                   EXSR      INZ02
075100051201    4C                   ENDIF
075200051201    3C                   ENDIF
075300051201    2C                   ENDIF
075400031110     C*
075500051201    1C                   ENDIF
075600960924     C*
075700960924     C                   ENDSR
075800960924     C*----------------------------------------------------*
075900051206     C*   CONTRTRASFE - controllo se posso fare il trasferimento
076000960924     C*----------------------------------------------------*
076100051206     C     ContrTrasfe   BEGSR
076200121022     C*
076300121022     C* Filiale obbligatoria
076400051206     c                   if        v2cfil=0
076500051206     c                   movel     err(7)        $msg
076600051206     c                   seton                                        28
076700051206     c                   goto      endctr
076800051206     c                   endif
076900121022     c*
077000121022     C* Filiale esistente
077100121022     C     V2CFIL        CHAIN     AZORG01L                           28
077200121022    1C     *IN28         IFEQ      *ON
077300121022     C     ORGFVA        ORNE      ' '
077400121022     C     ORGFAG        ORNE      'A'
077500121022     C     ORGFAG        ANDNE     'F'
077600121022     c                   movel     err(9)        $msg
077700121022     C                   GOTO      ENDCTR
077800121022    1C                   ENDIF
077900121126     C*
078000121128     C*** la gestione del ctrl sul network è stata tolta (ma non si sa mai...)
078100121128     C***                MOVEL     ORGDE3        OG143
078200121128     C**il network della filiale deve essere COR oppure MES, se non lo è ma fa parte di DPD, EEX o
078300121128     C**FED controllo che si possa comunque trasferire mediante iscrizione su tab.3TR
078400121128     C***                SELECT
078500121128     C***                WHEN      §OGNTW = 'COR' or
078600121128     C***                          §OGNTW = 'MES' or
078700121128     C***                          §OGNTW = 'DPD' or
078800121128     C***                          §OGNTW = 'EEX' or
078900121128     C***                          §OGNTW = 'FED'
079000121128     C**tutto OK
079100121128     c***
079200121128     C***                OTHER
079300121128     C**errore
079400121128     c***                seton                                        2870
079500121128     c***                movel     err(10)       $msg
079600121128     C***                ENDSL
079700121128     C***
079800121128
079900121128     c* richiamo pgm tntb51r
080000121128     c                   clear                   tntb50ds
080100121128     c                   movel     v2cccm        b50ke2
080200121128     c                   movel     v2cccm        b50ke1
080300121128     c                   movel     'C'           b50opz
080400121128     c                   movel     v2cfil        b50poa
080500121128     c                   call      'TNTB51R'
080600121128     c                   parm                    kpjba
080700121128     c                   parm                    tntb50ds
080800121128     c
080900121128     c                   if        b50err<>' '
081000121128     c                   movel     b50msg        $msg
081100121128     c                   seton                                        2870
081200121128     c                   goto      endctr
081300121128     c                   endif
081400121128     c
081500051206     c
081600051206     c* Verifico se già presente il CMR nel p.o. di ricezione dati
081700051206     c     kvab3         setll     edivab3l
081800051206     c     kvab3         reade     edivab3l
081900051206     c                   dow       not %eof(edivab3l)
082000051206     c                   if        vabcmr=v2ccmr
082100051206     c                   seton                                        2870
082200051206     c                   movel     err(8)        $msg
082300051206     C                   EVAL      $MSG = %trim($MSG) + 'VAB'
082400051206     c                   goto      endctr
082500051206     c                   endif
082600051206     c
082700051206     c     kvab3         reade     edivab3l
082800051206     c                   enddo
082900051206     c
083000051206     c     kdett         setll     edivad1l
083100051206     c                   if        %equal(edivad1l)
083200051206     c                   seton                                        2870
083300051206     c                   movel     err(8)        $msg
083400051206     C                   EVAL      $MSG = %trim($MSG) + 'VAD'
083500051206     c                   goto      endctr
083600051206     c                   endif
083700051206     c     kdett         setll     edivat1l
083800051206     c                   if        %equal(edivat1l)
083900051206     c                   seton                                        2870
084000051206     c                   movel     err(8)        $msg
084100051206     C                   EVAL      $MSG = %trim($MSG) + 'VAT'
084200051206     c                   goto      endctr
084300051206     c                   endif
084400051206     c     kdett         setll     edivax1l
084500051206     c                   if        %equal(edivax1l)
084600051206     c                   seton                                        2870
084700051206     c                   movel     err(8)        $msg
084800051206     C                   EVAL      $MSG = %trim($MSG) + 'VAX'
084900051206     c                   goto      endctr
085000051206     c                   endif
085100051206     c
085200051206     c     endctr        endsr
085300051206     C*----------------------------------------------------*
085400051206     C*   ESEGUITRASFE - eseguo trasferimento dati con _SQL
085500051206     C*----------------------------------------------------*
085600051206     C     EseguiTRasfe  BEGSR
085700051206     C                   EVAL      Stringasql=
085800051206     C                             'UPDATE EDIVAD1L'
085900051206     C                             +
086000051206     C                             ' SET VADFGS = ' + %editc(v2cfil:'1')
086100051206     C                             +
086200051206     C                             ' WHERE VADCMR='''+ V2CCMR+ ''' AND '
086300051206     C                             +
086400051206     C                             ' VADCCM=' + %editc(v2cccm:'4') +' AND'
086500051206     C                             +
086600051206     C                             ' VADCNT=' + %editc(v2ccnt:'3')
086700051206
086800051206     C/EXEC SQL
086900051206     C+ EXECUTE IMMEDIATE :StringaSQL
087000051206     C/END-EXEC
087100051206     C                   EVAL      Stringasql=
087200051206     C                             'UPDATE EDIVAT1L'
087300051206     C                             +
087400051206     C                             ' SET VATFGS = ' + %editc(v2cfil:'1')
087500051206     C                             +
087600051206     C                             ' WHERE VATCMR='''+ V2CCMR+ ''' AND '
087700051206     C                             +
087800051206     C                             ' VATCCM=' + %editc(v2cccm:'4') +' AND'
087900051206     C                             +
088000051206     C                             ' VATCNT=' + %editc(v2ccnt:'3')
088100051206
088200051206     C/EXEC SQL
088300051206     C+ EXECUTE IMMEDIATE :StringaSQL
088400051206     C/END-EXEC
088500051206
088600051206     C                   EVAL      Stringasql=
088700051206     C                             'UPDATE EDIVAX1L'
088800051206     C                             +
088900051206     C                             ' SET VAXFGS = ' + %editc(v2cfil:'1')
089000051206     C                             +
089100051206     C                             ' WHERE VAXCMR='''+ V2CCMR+ ''' AND '
089200051206     C                             +
089300051206     C                             ' VAXCCM=' + %editc(v2cccm:'4') +' AND'
089400051206     C                             +
089500051206     C                             ' VAXCNT=' + %editc(v2ccnt:'3')
089600051206
089700051206     C/EXEC SQL
089800051206     C+ EXECUTE IMMEDIATE :StringaSQL
089900051206     C/END-EXEC
090000051206
090100051206     C                   EVAL      Stringasql=
090200051206     C                             'UPDATE EDIVAB1L'
090300051206     C                             +
090400051206     C                             ' SET VABFGS = ' + %editc(v2cfil:'1')
090500051206     C                             +
090600051206     C                             ' WHERE VABCMR='''+ V2CCMR+ ''' AND '
090700051206     C                             +
090800051206     C                             ' VABCCM=' + %editc(v2cccm:'4') +' AND'
090900051206     C                             +
091000051206     C                             ' VABCNT=' + %editc(v2ccnt:'3')
091100051206
091200051206     C/EXEC SQL
091300051206     C+ EXECUTE IMMEDIATE :StringaSQL
091400051206     C/END-EXEC
091500051206     C
091600051206     c                   seton                                        75
091700051206     C                   UPDATE    TC85S02
091800051206     c                   setoff                                       75
091900051206     c                   endsr
092000051206     C*----------------------------------------------------*
092100051206     C*   ROLLUP01: Caricamento dati da Edivab1l           *
092200051206     C*----------------------------------------------------*
092300051206     C     ROLLUP01      BEGSR
092400960924     C*
092500960930     C     *IN23         IFEQ      '0'
092600960930     C     VABCCM        ANDLE     WCCMAL
092700960930     C*
092800030617     C                   DOW       *IN23     = *OFF    AND
092900030617     C                             VABCCM   <= WCCMAL
093000030617     C* Testo che se richiesto il CMR corrisponda
093100030617     C                   IF        (V1CCMR <> *blanks  AND
093200030617     C                              VABCMR  = V1CCMR)  OR
093300030617     C                             V1CCMR   = *blanks
093400031118     C* Al primo giro incremento subito e salvo il livello d rottura
093500031118     C                   IF        wGiro > *zeros
093600960930     C* Scrivo solo a rottura di CMR - contatore
093700960930     C     VABCNT        IFNE      WSVCNT
093800960930     C     VABCMR        ORNE      WSVCMR
093900960930     C     VABCCM        ORNE      WSVCCM
094000041123     C**   VABDTS        ORNE      WSVDTS
094100041123     C**   VABHMS        ORNE      WSVHMS
094200960930     C*  Scrittura TC85S02
094300960930     C                   EXSR      WRTS02
094400031118     C*  Inizializzo variabili d appoggio NN chiave
094500031118     C                   CLEAR                   WSVSPE
094600031118     C                   CLEAR                   WSVNCL
094700031118     C                   CLEAR                   WSVPKB
094800040713     C                   CLEAR                   wTotVADCMR
094900040713     C                   CLEAR                   wTotVATCMR
095000040713     C                   CLEAR                   wTotVAXCMR
095100031118     C*  Salvataggio nuova chiave di rottura
095200031118     C                   EXSR      SAVDAT
095300031118     C                   ELSE
095400031118     C*  Salvataggio nuova chiave di rottura
095500031118     C                   EXSR      SAVDAT
095600030617     C                   ENDIF
095700031118     C                   ELSE
095800031118     C*  Salvataggio nuova chiave di rottura
095900031118     C                   EXSR      SAVDAT
096000031118     C                   Z-ADD     1             wGiro
096100031118     C                   ENDIF
096200030617     C                   ENDIF
096300960924     C*  Lettura successiva
096400020919     c                   exsr      ReadVAB
096500030617     C                   ENDDO
096600031119     C*
096700031119     C*  Scrittura TC85S02 ultimo CMR valido
096800031119     C                   EXSR      WRTS02
096900030617     C                   ENDIF
097000040713     C*
097100040713     C*  Salvo il numero d record del sub-file caricati
097200040713     C                   Z-ADD     NRR2          wNRR2             4 0
097300040713     C*
097400960924     C                   ENDSR
097500030617     C*----------------------------------------------------*
097600041123     C*   ROLLUP04: Caricamento dati da Edivab04           *
097700030617     C*----------------------------------------------------*
097800041123     C     ROLLUP04      BEGSR
097900030617     C*
098000030617     C     *IN23         IFEQ      '0'
098100030617     C     VABCMR        ANDLE     WCMRAL
098200030617     C*
098300030617     C                   DOW       *IN23     = *OFF    AND
098400030617     C                             VABCMR   <= WCMRAL
098500030617     C* Testo che se richiesto il CMR corrisponda
098600030617     C                   IF        (V1CCCM <> *zeros  AND
098700030617     C                              VABCCM  = V1CCCM) OR
098800030617     C                             V1CCCM   = *zeros
098900031118     C* Al primo giro incremento subito e salvo il livello d rottura
099000031118     C                   IF        wGiro > *zeros
099100030617     C* Scrivo solo a rottura di CMR - contatore
099200030617     C     VABCNT        IFNE      WSVCNT
099300030617     C     VABCMR        ORNE      WSVCMR
099400030617     C     VABCCM        ORNE      WSVCCM
099500041123     C**   VABDTS        ORNE      WSVDTS
099600041123     C**   VABHMS        ORNE      WSVHMS
099700030617     C*  Scrittura TC85S02
099800030617     C                   EXSR      WRTS02
099900031118     C*  Inizializzo variabili d appoggio
100000031118     C                   CLEAR                   WSVSPE
100100031118     C                   CLEAR                   WSVNCL
100200031118     C                   CLEAR                   WSVPKB
100300040713     C                   CLEAR                   wTotVADCMR
100400040713     C                   CLEAR                   wTotVATCMR
100500040713     C                   CLEAR                   wTotVAXCMR
100600031118     C*  Salvataggio nuova chiave di rottura
100700031118     C                   EXSR      SAVDAT
100800031118     C                   ELSE
100900031118     C*  Salvataggio nuova chiave di rottura
101000031118     C                   EXSR      SAVDAT
101100030617     C                   ENDIF
101200031118     C                   ELSE
101300031118     C*  Salvataggio nuova chiave di rottura
101400031118     C                   EXSR      SAVDAT
101500031118     C                   Z-ADD     1             wGiro
101600031118     C                   ENDIF
101700030617     C                   ENDIF
101800030617     C*  Lettura successiva
101900030617     c                   exsr      ReadVAB
102000030617     C                   ENDDO
102100031119     C*
102200031119     C*  Scrittura TC85S02 ultimo CMR valido
102300031119     C                   EXSR      WRTS02
102400030617     C                   ENDIF
102500040713     C*
102600040713     C*  Salvo il numero d record del sub-file caricati
102700040713     C                   Z-ADD     NRR2          wNRR2             4 0
102800030617     C*
102900030617     C                   ENDSR
103000960930     C*----------------------------------------------------*
103100960930     C*   SAVDAT: Salvataggio campi rottura CCM-CMR-CNT    *
103200960930     C*----------------------------------------------------*
103300960930     C     SAVDAT        BEGSR
103400960930     C*
103500960930     C*  Reimposto campi per rottura totale
103600030925     C                   Z-ADD     VABFGS        WSVFGS
103700960930     C                   Z-ADD     VABCCM        WSVCCM
103800960930     C                   Z-ADD     VABCNT        WSVCNT
103900960930     C                   MOVEL     VABCMR        WSVCMR
104000960930     C                   Z-ADD     VABHMS        WSVHMS
104100960930     C                   Z-ADD     VABDTS        WSVDTS
104200960930     C                   Z-ADD     VABDCM        WSVDCM
104300031118     C                   ADD       1             WSVSPE
104400031118     C                   ADD       VABNCL        WSVNCL
104500031118     C                   ADD       VABPKB        WSVPKB
104600040719     C*
104700040719     C* Conteggia i colli in relazione al trattamento merce
104800040719     C                   z-add     1             SkJ
104900040719     C     vabCTM        lookup    SkCTM(SkJ)                             80
105000040719     C                   if        *IN80 = *on
105100040719     C                   eval      DSSKEXT = SkEXT(SkJ)
105200040719     C                   if        FLGVAD = 'S'
105300040719     C                   add       vabNCL        wTotVADCMR
105400040719     C                   endif
105500040719     C                   if        FLGVAT = 'S'
105600040719     C                   add       vabNCL        wTotVATCMR
105700040719     C                   endif
105800040719     C                   if        FLGVAX = 'S'
105900040719     C                   add       1             wTotVAXCMR
106000040719     C                   endif
106100040719     C                   endif
106200040719     C*
106300031118     C                   CLEAR                   TC85S02
106400960930     C*
106500960930     C                   ENDSR
106600960930     C*----------------------------------------------------*
106700030617     C*   WRTS02: Scrittura dati subfile                   *
106800960930     C*----------------------------------------------------*
106900960930     C     WRTS02        BEGSR
107000030926     C*
107100040713     C                   SETOFF                                       70
107200040713     C*
107300030709     C                   Z-ADD     WSVCCM        V2CCCM
107400030709     C                   MOVEL     WSVCMR        V2CCMR
107500030709     C                   Z-ADD     WSVCNT        V2CCNT
107600030709     C                   MOVEL     WSVHMS        V2CHMS
107700030709     C                   Z-ADD     WSVDTS        G02INV
107800030709     C                   MOVEL     '3'           G02ERR
107900030709     C                   CALL      'XSRDA8'
108000030709     C                   PARM                    WLBDA8
108100030709     C                   Z-ADD     G02DAT        V2CDTS
108200030709     C                   Z-ADD     WSVDCM        G02INV
108300030709     C                   MOVEL     '3'           G02ERR
108400030709     C                   CALL      'XSRDA8'
108500030709     C                   PARM                    WLBDA8
108600030709     C                   Z-ADD     G02DAT        V2CDCM
108700040716     C*
108800040716     C* Reperisco la Ragione Sociale del cliente
108900040716     C                   CLEAR                   BS69DS
109000040716     C                   CLEAR                   ACODS
109100040716     C                   CLEAR                   INDDS
109200040716     C                   CLEAR                   CLPDS
109300040716     C                   CLEAR                   CLSDS
109400040716     C                   Z-ADD     V2CCCM        I69KAC
109500040716     C                   CALL      'TIBS69R'
109600040716     C                   PARM                    BS69DS
109700040716     C                   PARM                    ACODS
109800040716     C                   PARM                    INDDS
109900040716     C                   PARM                    CLPDS
110000040716     C                   PARM                    CLSDS
110100040716     C     O69ERR        IFNE      '1'
110200040716     C                   EVAL      V2CRAG = ACORAG
110300040716     C                   ENDIF
110400040716     C*
110500040716     C* Effettuo considerazione particolare su cambi sommati a video
110600040713     C                   IF        (WSVSPE + V2CSPE) > 99999
110700040713     C                   EVAL      V2CSPE = 99999
110800040713     C                   SETON                                        59
110900040713     C                   ELSE
111000031118     C                   ADD       WSVSPE        V2CSPE
111100040713     C                   ENDIF
111200040713     C                   IF        (WSVNCL + V2CNCL) > 99999
111300040713     C                   EVAL      V2CNCL = 99999
111400040713     C                   SETON                                        63
111500040713     C                   ELSE
111600040713     C                   ADD       WSVNCL        V2CNCL
111700040713     C                   ENDIF
111800040713     C                   IF        (WSVPKB + V2CPKG) > 99999
111900040713     C                   EVAL      V2CPKG = 99999
112000040713     C                   SETON                                        64
112100040713     C                   ELSE
112200040713     C                   ADD       WSVPKB        V2CPKG
112300040713     C                   ENDIF
112400030926     C*
112500040713     C* Effettuo lettura x chiave bolla corrente x determinare quanti dettagli
112600040713     C* VAD/VAT/VAX sono presenti
112700040713     C                   Z-ADD     *zeros        V2CNDD
112800040713     C                   Z-ADD     *zeros        V2CNDT
112900040713     C                   Z-ADD     *zeros        V2CNDX
113000040713     C                   Z-ADD     *zeros        wV2CNDD           9 0
113100040713     C                   Z-ADD     *zeros        wV2CNDT           9 0
113200040713     C                   Z-ADD     *zeros        wV2CNDX           9 0
113300031204     C* VAD
113400030926     C     KVAD          SETLL     EDIVAD1L                               50
113500030926     C                   IF        %equal(EDIVAD1L)
113600030926     C     KVAD          READE     EDIVAD1L
113700030926     C                   DOW       not %eof(EDIVAD1L)
113800040713     C                   ADD       1             wV2CNDD
113900030926     C     KVAD          READE     EDIVAD1L
114000030926     C                   ENDDO
114100030926     C                   ENDIF
114200031204     C* VAT
114300031204     C     KVAD          SETLL     EDIVAT1L                               50
114400031204     C                   IF        %equal(EDIVAT1L)
114500031204     C     KVAD          READE     EDIVAT1L
114600031204     C                   DOW       not %eof(EDIVAT1L)
114700040713     C                   ADD       1             wV2CNDT
114800130315     C* SE TIPO RECORD "PDF" CONTEGGIO A FINI PACKING LIST
114900130319     C                   IF        VATTRC = 'P'
115000130315     C                   ADD       1             wV2CNDX
115100130315     C                   ENDIF
115200031204     C     KVAD          READE     EDIVAT1L
115300031204     C                   ENDDO
115400031204     C                   ENDIF
115500040713     C* VAX
115600040713     C     KVAD          SETLL     EDIVAX1L                               50
115700040713     C                   IF        %equal(EDIVAX1L)
115800040713     C     KVAD          READE     EDIVAX1L
115900040713     C                   DOW       not %eof(EDIVAX1L)
116000040713     C* Conteggio a rottura spedizione
116100040713     C                   IF        DSKEYVAXROT <> DSKEYVAX
116200040713     C                   ADD       1             wV2CNDX
116300040713     C                   EVAL      DSKEYVAXROT = DSKEYVAX
116400040713     C                   ENDIF
116500040713     C     KVAD          READE     EDIVAX1L
116600040713     C                   ENDDO
116700040713     C* Conteggio a rottura spedizione
116800040713     C                   IF        DSKEYVAXROT <> DSKEYVAX
116900040713     C                   ADD       1             wV2CNDX
117000040713     C                   EVAL      DSKEYVAXROT = DSKEYVAX
117100040713     C                   ENDIF
117200040713     C                   ENDIF
117300030926     C*
117400040713     C* Verifico se i conteggi sono superiori ai valori max conenibili nei campi
117500040713     C* conteggi dettagli a video
117600040713     C                   IF        wV2CNDD > 99999
117700040713     C                   EVAL      V2CNDD = 99999
117800040713     C                   SETON                                        60
117900040713     C                   ELSE
118000040713     C                   EVAL      V2CNDD = wV2CNDD
118100040713     C                   ENDIF
118200040713     C                   IF        wV2CNDT > 99999
118300040713     C                   EVAL      V2CNDT = 99999
118400040713     C                   SETON                                        61
118500040713     C                   ELSE
118600040713     C                   EVAL      V2CNDT = wV2CNDT
118700040713     C                   ENDIF
118800040713     C                   IF        wV2CNDX > 99999
118900040713     C                   EVAL      V2CNDX = 99999
119000040713     C                   SETON                                        62
119100040713     C                   ELSE
119200040713     C                   EVAL      V2CNDX = wV2CNDX
119300040713     C                   ENDIF
119400040713     C*
119500040713     C* Effettuo considerazioni bloccanti relative a conteggi estensioni dettaglio (VAD/VAT/VAX)
119600040713     C                   EVAL      V2CERRVAD = *blanks
119700040713     C                   EVAL      V2CERRVAT = *blanks
119800040713     C                   EVAL      V2CERRVAX = *blanks
119900040713     C* VAD
120000040713     C                   IF        wV2CNDD = *zeros AND
120100040713     C                             wTotVADCMR > *zeros
120200040713     C                   SETON                                        60
120300040713     C                   EVAL      V2CERRVAD = '1'
120400040713     C                   ENDIF
120500040713     C* VAT
120600040713     C                   IF        wV2CNDT = *zeros AND
120700040713     C                             wTotVATCMR > *zeros
120800040713     C                   SETON                                        61
120900040713     C                   EVAL      V2CERRVAT = '1'
121000040713     C                   ENDIF
121100040713     C* VAX
121200040713     C                   IF        wV2CNDX = *zeros AND
121300040713     C                             wTotVAXCMR > *zeros
121400040713     C                   SETON                                        62
121500120424
121600120502     C* controllo se esiste un bypass su questo cliente (do errore se NON c'è)
121700120502     C                   IF        %lookup(V2CCCM : CCM) = 0
121800040713     C                   EVAL      V2CERRVAX = '1'
121900120424     C                   ENDIF
122000120424
122100040713     C                   ENDIF
122200070808     C* VAX x ACCORPAMENTO
122300070808     C                   SETOFF                                       03
122400070808     C     V2CCCM        LOOKUP    KSCABDVX                               33
122500070808     C                   IF        *IN33 = *OFF AND V1CABD = 'S' AND
122600070808     C                             (wV2CNDX    > *zeros OR
122700070808     C                              wTotVAXCMR > *zeros)
122800070808     C                   SETON                                        62
122900070808     C                   SETON                                        03
123000070808     C                   EVAL      V2CERRVAX = '1'
123100070808     C                   ENDIF
123200040713     C*
123300040713     C* Salvo lo stato degli indicatori a video
123400040713     C                   EVAL      WIN59 = *IN59
123500040713     C                   EVAL      WIN60 = *IN60
123600040713     C                   EVAL      WIN61 = *IN61
123700040713     C                   EVAL      WIN62 = *IN62
123800040713     C                   EVAL      WIN63 = *IN63
123900040713     C                   EVAL      WIN64 = *IN64
124000030709     C*
124100960930     C                   ADD       1             NRR2              4 0
124200960930     C                   WRITE     TC85S02
124300040713     C                   SETOFF                                       596061
124400040713     C                   SETOFF                                       626364
124500960930     C*
124600960930     C                   ENDSR
124700031106     C*------------------------------------------------------------------------*
124800120620     C* WdwForce - chiamata a pgm per window per forzatura
124900031106     C*------------------------------------------------------------------------*
125000120619     C     WdwForce      BEGSR
125100031106     C*
125200120619     C                   EVAL      Forcing = *off
125300120620     C*
125400120620     C                   CALL      'TRTC85R1'
125500120620     C                   PARM                    Kpjba
125600120620     C                   PARM                    Exfmt
125700120620     C                   PARM                    Msg01
125800120620     C                   PARM                    Msg02
125900120620     C                   PARM                    Msg03
126000120620     C                   PARM                    Forcing
126100120620     C*
126200120620     C* tolgo memo che c'è stata una exfmt del sfl
126300120620     C                   EVAL      Exfmt = *off
126400120620     C*
126500120620     C                   ENDSR
126600120620     C*------------------------------------------------------------------------*
126700120620     C* EmailForza - Invio e-mail di conferma forzatura
126800120620     C*------------------------------------------------------------------------*
126900120620     C     EmailForza    BEGSR
127000120620     C*
127100120620     C* invio e-mail con richiesta forzatura
127200120620     C                   EVAL      peml = 'ced@brt.it'
127300120620     C                   EVAL      pcceml = *blanks
127400120620     C                   EVAL      pogg = 'Conferma da CMR - FORZATURA'
127500120620     C                   EVAL      pmsg =
127600120620     C                             'Codice cliente mittente: ' +
127700120622     C                             %editc(V2CCCM:'X')  +
127800120620     C                             ':/N' +
127900120622     C                             'Numero CMR: ' +
128000120620     C                             V2CCMR +
128100120620     C                             ':/N' +
128200120620     C                             'Utente forzatura: ' +
128300120620     C                             SDSusr +
128400120620     C                             ':/N'
128500120620     C                   call      'TIS701C1'
128600120620     C                   parm                    peml            253
128700120620     C                   parm                    pcceml          253
128800120620     C                   parm                    pogg             44
128900120620     C                   parm                    pmsg           5000
129000120620     C                   parm                    pesito            1
129100120619     C*
129200120619     C                   ENDSR
129300120619     C*------------------------------------------------------------------------*
129400120619     C* CARTAB - CARICA LE TABELLE OCCORRENTI
129500120619     C*------------------------------------------------------------------------*
129600120619     C     CARTAB        BEGSR
129700120619     C*
129800031106     C* TABELLA 3C - CLIENTI CON ABILITAZIONE ALL'ACCORPAMENTO BOLLE X MEDESIMO DESTINATARIO
129900031106     C                   Z-ADD     *ZEROS        I                 3 0
130000031106     C                   Z-ADD     CODUT         KTBute
130100031106     C                   MOVEL     '3C'          KTBcod
130200031106     C     KEYtabP       SETLL     TABEL00F
130300031106     C     KEYtabP       READE     TABEL00F                               97
130400031106     C     *IN97         DOWEQ     *OFF
130500031106     C     TBLFLG        IFEQ      *BLANKS
130600031106     C                   MOVEL     TBLUNI        DS3C
130700050610     C                   IF        §3CABD <> *blanks
130800031106     C                   ADD       1             I
130900031106     C                   MOVEL     TBLKEY        KSCABD(I)
131000070808     C                   IF        §3CAVX = 'S'
131100070808     C                   MOVEL     TBLKEY        KSCABDVX(I)
131200070808     C                   ENDIF
131300031106     C                   ENDIF
131400031106     C                   ENDIF
131500031106     C     KEYtabP       READE     TABEL00F                               97
131600031106     C                   ENDDO
131700040713     C*
131800040713     C* TATBELLA '1B' -  CODICI TRATTAMENTO MERCE IN FUNZIONE DELLE ESTENSIONI VAD/VAT/VAX
131900040713     C                   Z-ADD     *ZEROS        SkJ               3 0
132000040713     C                   Z-ADD     CODUT         KTBute
132100040713     C                   MOVEL     '1B'          KTBcod
132200040713     C     KEYtabP       SETLL     TABEL00F
132300040713     C     KEYtabP       READE     TABEL00F                               97
132400040713     C     *IN97         DOWEQ     *OFF
132500040713     C     TBLFLG        IFEQ      *BLANKS
132600040713     C                   MOVEL     TBLUNI        DS1B
132700040713     C                   ADD       1             SkJ
132800040713     C                   MOVEL     TBLKEY        SkCTM(SkJ)
132900040716     C                   CLEAR                   DSSKEXT
133000040713     C* VAD
133100040713     C                   IF        §1BFG7 = 'S'
133200040713     C                   EVAL      FLGVAD = 'S'
133300040713     C                   ENDIF
133400040713     C* VAT
133500040713     C                   IF        §1BF12 = 'E'
133600040713     C                   EVAL      FLGVAT = 'S'
133700040713     C                   ENDIF
133800040713     C* VAX
133900040713     C                   IF        §1BF16 = 'S'
134000040713     C                   EVAL      FLGVAX = 'S'
134100040713     C                   ENDIF
134200040713     C                   EVAL      SkEXT(SkJ) = DSSKEXT
134300040713     C*
134400040713     C                   ENDIF
134500040713     C     KEYtabP       READE     TABEL00F                               97
134600040713     C                   ENDDO
134700031106     C*
134800031106     C                   ENDSR
134900070802     C*----------------------------------------------------*
135000070802      /TITLE Gestione errore generico
135100070802     C*----------------------------------------------------*
135200070802     C     *pssr         BEGSR
135300070802     C*
135400070802     C* Se errore sconosciuto => emetto operazione d ROLLBACK
135500070802     C                   ROLBK
135600070820     C*
135700070820     C* Infine arresto il controllo sincronia
135800070820     C                   CALL      'TRTC85C'
135900070820     C                   PARM      'END'         OPZ               3
136000070802     C*
136100070802     C                   ENDSR     '*CANCL'
136200960924     C*----------------------------------------------------*
136300960924     C*   *INZSR: Operazioni iniziali                      *
136400960924     C*----------------------------------------------------*
136500960924     C     *INZSR        BEGSR
136600960924     C*
136700960924     C     *ENTRY        PLIST
136800960924     C                   PARM                    KPJBA
136900051130     c                   movel     kpjbu         param
137000051130     c* Se richiamo per trasferimento accendo indicatore 01
137100051130     c                   if        trasfe='S'
137200051130     c                   seton                                        01
137300051130     c                   endif
137400960924     C*  Richiamo XPARUT
137500960924     C                   Z-ADD     1             CODUT
137600960924     C                   CALL      'X§PARUT'
137700960924     C                   PARM                    UT§DSE
137800960924     C                   MOVEL     REC80         CNCR80
137900031106     C*
138000031106     C* VERIABILI RIFERITE AL DATABASE
138100031106     C     *LIKE         DEFINE    TBLKUT        KTBute
138200031106     C     *LIKE         DEFINE    TBLCOD        KTBcod
138300031106     C*
138400031106     C* CHIAVE DI LETTURA X TABEL00F - Parziale
138500031106     C     KEYtabP       KLIST
138600031106     C                   KFLD                    KTBute
138700031106     C                   KFLD                    KTBcod
138800041115     C     Kvab5         KLIST
138900041115     C                   KFLD                    kccm
139000041115     C                   KFLD                    kcmr
139100051206     C     Kvab3         KLIST
139200051206     C                   KFLD                    v2cfil
139300051206     C                   KFLD                    v2cccm
139400051206     C     Kdett         KLIST
139500051206     C                   KFLD                    v2cfil
139600051206     C                   KFLD                    v2cccm
139700051206     C                   KFLD                    v2ccmr
139800051206     C                   KFLD                    v2ccnt
139900121127     C*
140000121127     C                   Z-ADD     1             CODUT
140100121127      * Reperimento dati utente/aziendali
140200121127     c                   exsr      sr_DatiJob
140300031106      *
140400031106      * CARICO LE TABELLE OCCORRENTI
140500031106     C                   EXSR      CARTAB
140600020919      *
140700020919     C                   MOVEL     kpjbu         KPJBUs
140800020919      * CARICO TABELLA PUNTI OPERATIVI GESTITI £1
140900020919     C                   CLEAR                   TRUL06DS
141000020919     C                   MOVE      '£1'          D06COD
141100020919     C                   MOVEL     SIMFEL        D06KEY
141200020919     C                   MOVEL     'L'           D06TLA
141300020919     C                   MOVEL     TRUL06DS      KPJBU
141400020919      *
141500020919     C                   CALL      'TRUL06R'
141600020919     C                   PARM                    KPJBA
141700020919     C                   MOVEL     KPJBU         TRUL06DS
141800020919     C                   MOVEL     kpjbus        KPJBU
141900020919      *
142000960924     C*  Ricerca capoconti
142100960924     C                   DO        50            X
142200960924     C                   MOVE      TCU(X)        TCUDS
142300960924     C     F56           IFEQ      'CG'
142400960924     C     F34           ANDEQ     '01'
142500960924     C                   Z-ADD     KCU(X)        KCI               4 0
142600960924     C                   END
142700960924     C                   END
142800960924     C                   MOVEL     RAGUT         RSUT
142900960924     C*  Chiavi di accesso
143000960924     C     KACO          KLIST
143100960924     C                   KFLD                    KKUT
143200960924     C                   KFLD                    KKCC
143300960924     C                   KFLD                    KKSC
143400961029     C     KTAB          KLIST
143500961029     C                   KFLD                    KKUT
143600961029     C                   KFLD                    KCOD1
143700030925     C     KVAD          KLIST
143800030925     C                   KFLD                    WSVFGS
143900030925     C                   KFLD                    WSVCCM
144000030925     C                   KFLD                    WSVCMR
144100030925     C                   KFLD                    WSVCNT
144200960924     C*  Definizione variabili
144300960924     C     *LIKE         DEFINE    ACOKUT        KKUT
144400960924     C     *LIKE         DEFINE    ACOKCC        KKCC
144500960924     C     *LIKE         DEFINE    ACOKSC        KKSC
144600960924     C     *LIKE         DEFINE    TABCOD        KCOD
144700961029     C     *LIKE         DEFINE    TBLCOD        KCOD1
144800960930     C     *LIKE         DEFINE    VABCCM        KCCM
144900030617     C     *LIKE         DEFINE    VABCMR        KCMR
145000960930     C     *LIKE         DEFINE    VABCCM        WSVCCM
145100960930     C     *LIKE         DEFINE    VABCNT        WSVCNT
145200960930     C     *LIKE         DEFINE    VABCMR        WSVCMR
145300960930     C     *LIKE         DEFINE    VABHMS        WSVHMS
145400960930     C     *LIKE         DEFINE    VABDTS        WSVDTS
145500960930     C     *LIKE         DEFINE    VABDCM        WSVDCM
145600030925     C     *LIKE         DEFINE    VABFGS        WSVFGS
145700031118     C     *LIKE         DEFINE    VABNCL        WSVNCL
145800031118     C     *LIKE         DEFINE    VABNCL        WSVSPE
145900031118     C     *LIKE         DEFINE    VABPKB        WSVPKB
146000960924     C*  Carico in schiera codici tabella - PT -
146100961029     C                   Z-ADD     0             X                 4 0
146200960924     C                   Z-ADD     0             KSC
146300960924     C                   MOVEL     'PT'          KCOD
146400960924     C     KCOD          CHAIN     EDTAB01L                           31
146500960924     C     *IN31         DOWEQ     '0'
146600960924     C     TABFLG        IFEQ      ' '
146700020919     C                   MOVEL     TABUNI        DSPT
146800020919      * solo se compresa nelle linee di partenza del terminal
146900020919     c     §ptlnp        lookup    Lin                                    32
147000020919     c                   if        *in32 = *on
147100960924     C                   ADD       1             X
147200961024     C                   MOVEL     TABKEY        CPT(X)
147300961024     C                   Z-ADD     §PTKSC        KSC(X)
147400020919     c                   end
147500960924     C                   END
147600960924     C     KCOD          READE     EDTAB01L                               31
147700960924     C                   END
147800021015      *
147900021015      * Salva il contatore
148000021015     c                   z-add     x             xx                3 0
148100000915      *
148200021015      * Carica Clienti da tabella CL
148300021015     C                   MOVEL     'CL'          TABCOD
148400021015     C     TABCOD        CHAIN     EDTAB01L                           30
148500021015     C     *IN30         DOWEQ     '0'
148600021015     C     TABFLG        IFEQ      ' '
148700021015     C                   MOVEL     TABKEY        CODCL             7 0
148800021015     C     CODCL         LOOKUP    ksc                                    32
148900021015     c                   if        *in32 = *on
149000021015     c                   movel     tabuni        lnpCL             3 0
149100021015     c     LnpCL         lookup    Lin                                    32
149200021015     c                   if        *in32 = *on
149300021015     c                   add       1             xx
149400021015     C                   MOVEL     TABUNI        KSC(xx)
149500021015     C                   endif
149600021015     c                   endif
149700021015     C                   END
149800021015     C     TABCOD        READE     EDTAB01L                               30
149900021015     C                   END
150000021015      *
150100021015      * Salva il contatore
150200021015     c                   z-add     xx            x
150300021015      *
150400021015      * Caricamento Tabella Depot DEP
150500000915     C     'DEP'         CHAIN     TNTBE01L                           31
150600000915      *
150700000915     C     *IN31         DOWEQ     '0'
150800000915      * Se il S.I. è indicato deve essere uguale al mio
150900000915     C     TBEATB        IFEQ      ' '
151000000915     C     TBEKE2        ANDEQ     'I'
151100000915     C     TBESIF        IFEQ      KNSIF
151200000915     C     TBESIF        OREQ      *BLANK
151300020919     C                   MOVEL     TBEUNI        DDEP
151400020919      * solo se compresa nelle linee di partenza del terminal
151500020919     c     §dplnp        lookup    Lin                                    32
151600020919     c                   if        *in32 = *on
151700000918     C                   ADD       1             X
151800000915     C                   Z-ADD     §DPKSC        KSC(X)
151900020919     c                   end
152000000915     C                   END
152100000915     C                   END
152200000915     C     'DEP'         READE     TNTBE01L                               31
152300000915     C                   ENDDO
152400120424      *
152500120424      * Caricamento Tabella Depot 3BC bypass ctrl
152600120424     C                   Z-ADD     0             wY
152700120424     C     '3BC'         CHAIN     TNTBE01L                           31
152800120424      *
152900120424     C     *IN31         DOWEQ     '0'
153000120424      * Se il S.I. è indicato deve essere uguale al mio
153100120424     C     TBEATB        IFEQ      ' '
153200120502     C     TBEKE2        ANDEQ     *blank
153300120424     C     TBESIF        IFEQ      KNSIF
153400120424     C     TBESIF        OREQ      *BLANK
153500120424     C                   MOVEL     TBEUNI        D3BC
153600120424      * salvo il  cliente solo se il bypass relativo al VAX per CMR è attivo
153700120424     C                   IF        §3BCCCC = 'S'
153800120424     c                   if        %lookup(%dec(%trim(%subst(TBEKe1 : 1 : 7))
153900120502     C                              : 7 : 0) : CCM) = 0
154000120424     C                   ADD       1             wY
154100120502     C                   EVAL      CCM(wY) =
154200120424     C                             %dec(%trim(%subst(TBEKe1 : 1 : 7)) : 7 : 0)
154300120424     c                   end
154400120424     C                   ENDIF
154500120424     C                   END
154600120424     C                   END
154700120502     C     '3BC'         READE     TNTBE01L                               31
154800120424     C                   ENDDO
154900000915      *
155000961024     C                   MOVEL     'PU'          TABCOD
155100961024     C     TABCOD        CHAIN     EDTAB01L                           30
155200961024     C     *IN30         DOWEQ     '0'
155300961024     C     TABFLG        IFEQ      ' '
155400961024     C                   MOVEL     TABKEY        CODPU            35
155500961024     C                   Z-ADD     1             X
155600961024     C     CODPU         LOOKUP    CPT(X)                                 32
155700961024     C     *IN32         IFEQ      '1'
155800961024     C                   MOVEL     TABUNI        DPU(X)
155900961024     C                   END
156000961024     C                   END
156100961024     C     TABCOD        READE     EDTAB01L                               30
156200961024     C                   END
156300960924     C*  Inizializzo variabili
156400960924     C                   MOVEL     'N'           WFINE             1
156500960924     C*
156600960924     C                   ENDSR
156700121127
156800121127      *---------------------------------------------------------------*
156900121127      *?Reperimento dati del job (Utente/Operativi)                  ?*
157000121127      *---------------------------------------------------------------*
157100121127     c     sr_DatiJob    BEGSR
157200121127      *
157300121127     c     *dtaara       define    §azute        AZUTEds
157400121127     c     *dtaara       define    §datiute      dDatiUte
157500121127      *
157600121127     c                   in(E)     *dtaara
157700121127     c                   IF        %Error or RSUT = *blanks
157800121127     c                   clear                   TIBS34ds
157900121127     c                   call      'TIBS34R'
158000121127     c                   parm                    TIBS34ds
158100121127     c                   in        *dtaara
158200121127     c                   ENDIF
158300121127      *
158400121127     c                   ENDSR
158500960924**
158600960924Conto cliente inesistente o annullato
158700960924Conto cliente non presente nella tabella dei partner esteri
158800960924Effettuare almeno una scelta
158900031110Il cliente selezionato non è abilitato al raggruppamento bolle ("3C")
159000031120Per raggruppamento bolle selezionare CMR di un solo cliente
159100051130Opzione errata
159200121022Inserire la Filiale che deve ricevere i dati !!
159300121022Impossibile trasferire i dati: presente il numero CMR nella filiale di ricezione
159400121119La Filiale che deve ricevere i dati non è in organigramma
159500121126La Filiale che deve ricevere i dati non è tra i network possibili
159600051130**
159700051213 *** Conferma bolle per numero CMR ***
159800121022 * Trasmissione CMR ad altra filiale *

000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300941031      * TNSC71R                                                      *
000400900606      *                                                              *
000500910701      *        DETTAGLIO SPEDIZIONI C/ASSEGNI                        *
000600910701      * 30 -39 INDICATORE DI PROTEZIONE                              *
000700910521      * 40 -ERRORE GENERICO VIDEO                                    *
000800910521      * 41 - 60 ERRORRI SINGOLI CAMPI                                *
000900910521      * 70 71 - GESTIONE SFL                                         *
001000910617      * 72    - GESTIONE SFL SFLNXTCHG                               *
001100941031      * 98    - RICH. DA PGM TNSC70R SPROTETTO SOLO STATO C/A        *
001200900606?     *--------------------------------------------------------------*
001300941031     FTNSC71D   CF   E             WORKSTN
001400941031     F                                     SFILE(SC71DSF:NRR)
001500941031     FTNCSB01L  UF   E           K DISK
001600120207     FTNCSm03L  uf   e           K DISK
001700941031     FTNCSV00F  O    E           K DISK
001800011031     FTNCSA01L  IF   E           K DISK
001900011031     FCNABI01L  IF   E           K DISK
002000910702     FAZORG01L  IF   E           K DISK
002100011031     Ftitas30c  IF   E           K DISK
002200970408     FTABEL00F  IF   E           K DISK
002300120223     Ftnsc71p   O    e             PRINTER OFLIND(*IN89)
002400900515      *
002500900606     D FLU             S              1    DIM(32)
002600900606     D OPZ             S              1    DIM(10)
002700120222     D nra             S             10    DIM(5000)
002800120222     D nrs             S              4  0 DIM(5000)
002900971230      *
003000941031     D CSV           E DS                  EXTNAME(TNCSV00F)
003100941031     D  CSVREC                16    229
003200941031     D CSBREC        E DS                  EXTNAME(TNCSB00F)
003300970408     D DS4S          E DS
003400900514     D KPJBA         E DS
003500900514     D  LIBSYS                92    100
003600900516      *
003700900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003800000000     D  TCU                  398    697
003900000000     D                                     DIM(50)
004000000000     D  KCU                  698    847P 0
004100000000     D                                     DIM(50)
004200000000     D                                     PACKEVEN
004300000000     D  DTU                  848    895P 0
004400000000     D                                     DIM(12)
004500000000     D                                     PACKEVEN
004600900606      *
004700971230     D FNLV55        E DS                  EXTNAME(FNLV55DS)
004800971230     D TNSC71        E DS
004900900606     D TCUDS           DS
005000900606     D  F1                     1      1
005100900606     D  F3                     3      3
005200900606     D  F2                     2      2
005300900606     D  F4                     4      4
005400900606     D  F56                    5      6
005500900515      *
005600941031     D WLBDA8          DS
005700941031     D  G08DAT                 1      8  0
005800941031     D  G08INV                 9     16  0
005900941031     D  G08ERR                17     17
006000941031     D  G08TGI                18     22  0
006100941031     D                SDS
006200941031     D  §PGM                   1     10
006300011031     d*
006400011031     D TIBS69        E DS                  EXTNAME(TIBS69DS)
006500011031     D CNACO         E DS                  EXTNAME(CNACO00F)
006600011031     D CNIND         E DS                  EXTNAME(CNIND00F)
006700011031     D CNCLP         E DS                  EXTNAME(CNCLP00F)
006800011031     D FNCLS         E DS                  EXTNAME(FNCLS00F)
006900070327     D                 DS                  INZ
007100070327     D  sLnp                          3S 0
007200070327     D  sNrs                          2S 0
007300070327     D  sNsp                          7S 0
007400120305     D  sIdBolla               1     12A
007401120305     d dtaiso          s               d
007402120305     d dtaeur          s               d   datfmt(*eur)
007500070327
007600110428     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
007700110428     D ESITO_OK...
007800110428     D                 C                   0
007900110428     ***************************************************************************
008000110428     **
008100110428     ** Dichiarazione prototipi procedure esterne.
008200110428     **
008300110428     ***************************************************************************
008400110428      /DEFINE DFTACTGRP_YES
008500110428     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
008600110428      /UNDEFINE DFTACTGRP_YES
008700110428     ***************************************************************************
008800110428     **
008900110428     ** Definizione strutture dati.
009000110428     **
009100110428      **************************************************************************
009200110428     D tibsSocI0...
009300110428     D               E DS                  QUALIFIED
009400110428     D                                     INZ
009500110428     D tibsSocO0...
009600110428     D               E DS                  QUALIFIED
009700110428     D                                     INZ
009800110428     ***************************************************************************
009900110428     **
010000110428     ** Definizione variabili modulo/programma.
010100110428     **
010200110428     ***************************************************************************
010300110428     D prmRqsOpCode...
010400110428     D                 S             10A
010500110428     D prmRpyOpCode...
010600110428     D                 S             10A
010700110428     D prmRpyIdMsg...
010800110428     D                 S             10I 0
010900110428     D prmRqsFormato...
011000110428     D                 S             10A
011100110428     D prmRqsDataSize...
011200110428     D                 S             10I 0
011300110428     D prmRpyFormato...
011400110428     D                 S             10A
011500110428     D prmRpyDataSize...
011600110428     D                 S             10I 0
011700110428
011800011031     ***********************************************************************
011900910521     C                   EXSR      DEFCAM
012000011031     c*
012100910927     C     INIZIO        TAG
012200941031     C                   WRITE     SC71D00
012300011031     c*
012400910521     C                   EXSR      PULSFL
012500910521     C                   EXSR      RIEMPI
012600910617     C* NESSUNA DISTINTA TROVATA
012700011031     c* CONTROLLO SE CI SONO RIGHE IN SFL
012800910620     C     NRR           IFEQ      1
012900910701     C                   MOVE      'E'           PARERR
013000910701     C                   GOTO      FINE
013100910617     C                   END
013200011031     c*
013300910521     C     FMTSF         TAG
013400011031     c*
013500941031     C                   WRITE     SC71D00
013600941031     C                   WRITE     SC71D02
013700941031     C                   EXFMT     SC71DCT
013800941031     C   KL              GOTO      FINE
013900910918     C*
014000910521     C                   EXSR      CTR2
014100011031     c* INTERROGAZIONE - CMD07
014200941117     C   KG              MOVE      KPJBU         KPJBUS          256
014300941108     C   KG              MOVEL     *BLANK        KPJBU
014400950720     C   KG              CALL      'TNSC40R'
014500910927     C                   PARM                    KPJBA
014600941117     C   KG              MOVE      KPJBUS        KPJBU
014700011031     c* MODIFICA C/ASS - CMD14
014800941117     C   KN              MOVE      KPJBU         KPJBUS
014900941108     C   KN              MOVEL     *BLANK        KPJBU
015000941031     C   KN              CALL      'TNSC95R'
015100910927     C                   PARM                    KPJBA
015200941117     C   KN              MOVE      KPJBUS        KPJBU
015300941031     C   KN              GOTO      INIZIO
015400011031     c* AGGIUNTA C/ASS - CMD10
015500941117     C   KJ              MOVE      KPJBU         KPJBUS
015600941117     C   KJ              MOVEL     *BLANK        KPJBU
015700941117     C   KJ              CALL      'TNSC03R'
015800941117     C                   PARM                    KPJBA
015900941117     C   KJ              MOVE      KPJBUS        KPJBU
016000941117     C   KJ              GOTO      INIZIO
016100910701     C   40
016200941031     CORNKF              GOTO      FMTSF
016300011031     c* AGGIORNAMENTO - CMD06
016400941031     C   KF              EXSR      AGGIOR
016500910521      *
016600910521     C     FINE          TAG
016700110428     ** Chiudo il programma.
016800110428     C                   CALL      'TIBSSOCR'
016900110428     C                   PARM      'FINALIZE'    prmRqsOpCode
017000110428     C                   PARM                    prmRpyOpCode
017100110428     C                   PARM                    prmRpyIdMsg
017200910521     C                   SETON                                        LR
017300011031     C**********************************************************************
017400910521     C     DEFCAM        BEGSR
017500011031     C**********************************************************************
017600000000     C     *ENTRY        PLIST
017700000000     C                   PARM                    KPJBA
017800941031     C                   MOVEL     KPJBU         TNSC71
017900910702     C                   MOVE      PARSTA        §STA
018000910918     C* SE RICH, CON CMD14  VEDO LE SPEDIZ. DELLA DISTINTA
018100910919     C* SE RICH, CON CMD13  VOGLIO VEDRE ALTRE SPEDIZ.
018200910918     C     PARCMD        COMP      '13'                                   13
018300910918     C     PARCMD        COMP      '14'                                   14
018400910918     C* SE RICHIEDO I SOSPESI (STATO=0) POSSO SOSPENDERE O ANN.INCASSO
018500910702     C* SE RICHIEDO I SOSPESI (STATO=5) VOGLIO RIATTIVARE C/A
018600910702     C*                     :POSSO MODIFIC. I DATI DELL'ASSEGNO
018700910702     C     §STA          COMP      0                                      98
018800910919     C   13              SETOFF                                       98
018900900516      *
019000900614     C                   TIME                    TIME              6 0
019100000000     C                   Z-ADD     1             CODUT
019200900517     C                   CALL      'X§PARUT'
019300900517     C                   PARM                    UT§DSE
019400000000     C     CODUT         CABEQ     -1            FINE
019500000000     C                   MOVEL     RAGUT         RSUT             20
019600110428     ** Inizializzo il programma.
019700110428     C                   CALL      'TIBSSOCR'
019800110428     C                   PARM      'INIT'        prmRqsOpCode
019900110428     C                   PARM                    prmRpyOpCode
020000110428     C                   PARM                    prmRpyIdMsg
020100971230      *  Data del giorno
020200971230     C                   TIME                    WAMGDT           14 0
020300971230     C                   MOVE      WAMGDT        G08DAT
020400971230     C                   MOVE      *ZEROS        G08INV
020500971230     C                   MOVEL     *BLANK        G08ERR
020600971230     C                   CALL      'XSRDA8'
020700971230     C                   PARM                    WLBDA8
020800971230     C                   Z-ADD     G08INV        WOGGI             8 0
020900900606      *  RICERCA CAPOCONTI
021000900606     C                   DO        50            X                 2 0
021100900606     C                   MOVE      TCU(X)        TCUDS
021200900606     C     F56           CABNE     'CG'          END1
021300900606     C     F4            COMP      '1'                                    21
021400900606     C     F4            COMP      '2'                                    22
021500900606     C     F4            COMP      '3'                                    23
021600900606     C     F4            COMP      '6'                                    27
021700900606     C     F4            COMP      '4'                                    28
021800900606      ** 1 CLIENTI   21
021900900606      ** 2 FORNITORI 22
022000900606      ** 3 AGENTI    23
022100900606     C     F3            COMP      '0'                                242425
022200900606     C     F3            COMP      'I'                                    26
022300900606      ** 0 ITALIA   25
022400900606      ** 1 ESTERO   24
022500900606      ** I CAPO CONTO IVA
022600900606     C   21
022700900606     CAN 24              Z-ADD     KCU(X)        KCE               4 0
022800900606     C   21
022900900606     CAN 25              Z-ADD     KCU(X)        KCI               4 0
023000900606     C   22
023100900606     CAN 24              Z-ADD     KCU(X)        KFE               4 0
023200900606     C   22
023300900606     CAN 25              Z-ADD     KCU(X)        KFI               4 0
023400900606     C   23
023500900606     CAN 24              Z-ADD     KCU(X)        KAE               4 0
023600900606     C   23
023700900606     CAN 25              Z-ADD     KCU(X)        KAI               4 0
023800900606     C   26              Z-ADD     KCU(X)        KIVA              4 0
023900900606     C   27              Z-ADD     KCU(X)        KBNA              4 0
024000900606     C   28              Z-ADD     KCU(X)        KCAS              4 0
024100900606     C     END1          TAG
024200900606     C                   END
024300000630     C                   Z-ADD     0             NRR               5 0
024400060628     C                   Z-ADD     0             totNRR            5 0
024500060628     C                   Z-ADD     0             totNRRe           5 0
024600110428     c* decodifico ragione sociale della società 201
024700110428     c                   exsr      Decod_societa
024800900515      *---------------------------------------------------------------*
024900900515      * CHIAVI                                                        *
025000900515      *---------------------------------------------------------------*
025100910617      * X FILIALE + NR.DIST. KEY-COMPLETA
025200910617     C     KNDTC         KLIST
025300910701     C                   KFLD                    §TPI
025400910617     C                   KFLD                    §RGF
025500910617     C                   KFLD                    §NDT
025600910617     C                   KFLD                    §DDC
025700970408     C*  TABEL
025800970408     C     KTAB          KLIST
025900970408     C                   KFLD                    TBLKUT
026000970408     C                   KFLD                    TBLCOD
026100970408     C                   KFLD                    TBLKEY
026200910617      *
026300910701     C     KNSP          KLIST
026400910701     C                   KFLD                    §TPI
026500910701     C                   KFLD                    §RGF
026600910701     C                   KFLD                    §NDT
026700910701     C                   KFLD                    §DDC
026800941031     C                   KFLD                    §AAS
026900941031     C                   KFLD                    §MGS
027000910918     C                   KFLD                    §LNP
027100910701     C                   KFLD                    §NRS
027200910701     C                   KFLD                    §NSP
027300011031      *
027400011031     C     Kbol          KLIST
027500011031     C                   KFLD                    csbaas
027600011031     C                   KFLD                    csblnp
027700011031     C                   KFLD                    csbnrs
027800011031     C                   KFLD                    csbnsp
027900910701      *
028000910701     C     KABI          KLIST
028100120224     C                   KFLD                    SABI
028200120224     C                   KFLD                    SCAB
028300120224     C     KABIp         KLIST
028400120224     C                   KFLD                    SABIp
028500120224     C                   KFLD                    SCABp
028600910701      *
028700910702     C     *LIKE         DEFINE    CSBSTA        §STA
028800910701     C     *LIKE         DEFINE    CSBTPI        §TPI
028900910617     C     *LIKE         DEFINE    CSBRGF        §RGF
029000910617     C     *LIKE         DEFINE    CSBNDT        §NDT
029100910617     C     *LIKE         DEFINE    CSBDDC        §DDC
029200910701     C     *LIKE         DEFINE    CSBLNP        §LNP
029300941031     C     *LIKE         DEFINE    CSBAAS        §AAS
029400941031     C     *LIKE         DEFINE    CSBMGS        §MGS
029500910701     C     *LIKE         DEFINE    CSBNRS        §NRS
029600910701     C     *LIKE         DEFINE    CSBNSP        §NSP
029700910701     C                   MOVEL     PARTPI        §TPI
029800910701     C                   MOVE      PARRGF        §RGF
029900910919     C   14              MOVE      PARNDT        §NDT
030000910919     C   14              MOVE      PARDDC        §DDC
030100941031     C                   TIME                    WHHUDT           14 0
030200941031     C                   MOVEL     WHHUDT        ORA               6 0
030300941031     C                   MOVE      WHHUDT        G08DAT
030400941031     C                   MOVE      *ZEROS        G08INV
030500941031     C                   MOVEL     *BLANK        G08ERR
030600941031     C                   CALL      'XSRDA8'
030700941031     C                   PARM                    WLBDA8
030800941031     C                   Z-ADD     G08INV        AMGUDT            8 0
030900941031     C                   Z-ADD     G08DAT        GMAUDT            8 0
031000910521     C                   ENDSR
031100900524      *
031200900607      *-----------------------------------------------------***********
031300910521      * PULIZIA     SFL                                     *  PULSFL *
031400900607      *-----------------------------------------------------***********
031500910521     C     PULSFL        BEGSR
031600900607     C                   Z-ADD     1             NRR
031700060628     C                   Z-ADD     0             totNRR
031800900607     C                   SETON                                        70
031900941031     C                   WRITE     SC71DCT
032000900607     C                   SETOFF                                       70
032100900607     C                   SETON                                        71
032200900607     C                   ENDSR
032300910521      *-----------------------------------------------------***********
032400910521      * COPIA CAMPI RECORD IN CAMPI SFL                     *  RIEMPI *
032500910521      *-----------------------------------------------------***********
032600910521     C     RIEMPI        BEGSR
032700120207     c                   clear                   nra
032800120222     c                   clear                   nrs
032900120207     c                   clear                   na
033000011031     c* RIEMPIO IL SFL
033100910521     C                   Z-ADD     1             NRR
033200941031     C     KNDTC         SETLL     TNCSB01L
033300910617     C     *IN21         DOUEQ     '1'
033400941031     C     KNDTC         READE     TNCSB01L                               21
033500910617     C     *IN21         IFEQ      '0'
033600910918     C     CSBSTA        ANDEQ     §STA
033700910701     C                   MOVE      CSBLNP        SLNP
033800910701     C                   MOVE      CSBAAS        SAAS
033900910701     C                   MOVE      CSBNRS        SNRS
034000910701     C                   MOVE      CSBNSP        SNSP
034100060322     C                   move      csbLNP        PLNP
034200120207     C                   move      csbAAS        PAAS
034300060322     C                   move      csbNRS        PNRS
034400060322     C                   move      csbNSP        PNSP
034500941031     C                   MOVE      CSBMGS        WDUE              2 0
034600941031     C                   MOVEL     WDUE          SGMSP
034700941031     C                   MOVEL     CSBMGS        WDUE
034800941031     C                   MOVE      WDUE          SGMSP
034900120201     C                   MOVE      sGMSP         pgmsp
035000120131     C                   MOVE      CSBTPA        STPA
035100120131     C                   MOVE      CSBSTA        SSTA
035200120207     C                   move      csbnra        Pnra
035300120131     c     ' '           scan      csbnra                                 88
035400120131     c* singolo assegno
035500120131     c                   if        *in88 or csbabi <> 0
035600120207     c                   if        not *in98
035700120207     c                   setoff                                       32
035800120207     c                   else
035900120207     c                   seton                                        32
036000120207     c                   end
036100910701     C                   MOVEL     CSBNRA        SNRA
036200941031     C                   MOVE      CSBDTE        G08INV
036300941031     C                   MOVEL     '3'           G08ERR
036400941031     C                   CALL      'XSRDA8'
036500941031     C                   PARM                    WLBDA8
036600941031     C                   MOVEL     G08DAT        SDTE
036700941031     C                   Z-ADD     CSBCAS        SASS
036800941031     C                   MOVEL     CSBVCA        SDIVIS
036900910701     C                   MOVE      CSBABI        SABI
037000910701     C                   MOVE      CSBCAB        SCAB
037100120629     c                   setoff                                       343536
037200120207     C                   MOVE      *in32         ind32
037300120207     C                   MOVE      *in34         ind34
037400120213     C                   MOVE      *in35         ind35
037401120629     C                   MOVE      *in36         ind36
037500060628     C                   ADD       1             totNRR
037600941031     C                   WRITE     SC71DSF
037700910521     C                   ADD       1             NRR
037800120131     c                   else
037900120131     c* multiassegno
038000120207     c                   seton                                        32
038100120222     c                   z-add     1             xx                4 0
038101120629     c                   setoff                                       36
038200120222     c     csbnra        lookup    nra(xx)                                33
038300120207     c                   if        not *in33
038400120207     c                   add       1             na                3 0
038500120222     c                   z-add     1             nrs(na)
038600120207     c                   eval      nra(na) = csbnra
038601120629     C* VERIFICO SE NEL MULTIASSEGNO ci sono codici clienti diversi
038602120629     c                   eval      innra = csbnra
038603120629     c                   clear                   outnrcli
038604120629     c                   call      'TNSC76R1'
038605120629     C                   PARM                    innra            10
038606120629     C                   PARM                    outnrcli          2 0
038607120629     c                   if        outnrcli > 1
038608120629     c                   seton                                        36
038609120629     c                   end
038700120222     c                   else
038800120222     c                   add       1             nrs(xx)
038900120207     c                   end
039000120207     c     csbnra        setll     tncsm03l
039100120131     c                   do        *hival
039200120207     c     csbnra        reade     tncsm03l
039300120131     c                   if        %eof(tncsm03l)
039400120131     c                   leave
039500120131     c                   end
039600120131     C                   MOVEL     CSmNRA        SNRA
039700120131     C                   MOVE      CSmDTE        G08INV
039800120131     C                   MOVEL     '3'           G08ERR
039900120131     C                   CALL      'XSRDA8'
040000120131     C                   PARM                    WLBDA8
040100120131     C                   MOVEL     G08DAT        SDTE
040200120131     C                   Z-ADD     CSmidc        SASS
040300120131     C                   MOVEL     CSmdiv        SDIVIS
040400120131     C                   MOVE      CSmABI        SABI
040500120131     C                   MOVE      CSmCAB        SCAB
040600120207     c                   if        slnp = 0
040700120207     C                   SETON                                        34
040800120213     c                   setoff                                       35
040801120629     c                   setoff                                       36
040900120207     c                   else
041000120213     C                   SETON                                        35
041100120207     c                   setoff                                       34
041200120207     C                   end
041300120207     C                   MOVE      *in32         ind32
041400120207     C                   MOVE      *in34         ind34
041500120213     C                   MOVE      *in35         ind35
041501120629     C                   MOVE      *in36         ind36
041600120131     C                   ADD       1             totNRR
041700120131     C                   WRITE     SC71DSF
041800120131     C                   ADD       1             NRR
041900120203     c* proteggo l'opzione
042000120131     C                   clear                   SLNP
042100120131     C                   clear                   SAAS
042200120131     C                   clear                   SNRS
042300120131     C                   clear                   SNSP
042400120131     C                   clear                   SGMSP
042500120131     C                   clear                   SSTA
042600120131     c                   enddo
042700120131     c                   end
042800001108     C                   END
042900910701     C                   END
043000910521     C                   ENDSR
043100900515      *-----------------------------------------------------***********
043200900524      * CONTROLLI SUBFILE                                   *  CTR2   *
043300900515      *-----------------------------------------------------***********
043400900515     C     CTR2          BEGSR
043500120207     C                   SETOFF                                       40
043600120207     c* verifico che se immessa un'opzione su una spezione legata ci sia
043700120207     c* la stessa opzione anche sulle "sorelle"
043800120207     c                   exsr      srlegame
043900900515      *
044000060628     c                   clear                   totnrre
044100900524      *
044200910614     C     *IN20         DOUEQ     '1'
044300941031     C                   READC     SC71DSF                                20
044400910620     C     *IN20         IFEQ      '0'
044500011031     C* DETTAGLIO SPEDIZIONI
044600910919     C* CONTROLLO SCELTE
044700910919     C* CMD13 ALTTRE SPEDIZ. X INSERIM. IN DISTINTA
044800910919     C   13SSCE          IFNE      '0'
044900910919     C     SSCE          ANDNE     *BLANK
045000910702     C                   SETON                                        4044
045100910702     C                   END
045200910919     C* SOLO SPEDIZ. DELLA DISTINTA
045300060516     c                   if        *in14
045400060516     C     SSCE          IFNE      *BLANK
045500120207     C                   MOVE      SSCE          SCEN              1
045600910919     C     SCEN          IFEQ      SSTA
045700910919     C                   SETON                                        4044
045800910919     C                   END
045900910919     C                   END
046000060628     c                   if        ssce = 'E'
046100060628     c* se scelta l'esclusione della spedizione ma nella distinta esiste
046200060628     c* solo quella non l'accetto perchè basta non confermare la distinta
046300060628     c                   if        totnrr = 1
046400060628     C                   SETON                                        4044
046500060628     c                   else
046600060628     c                   add       1             totnrre
046700060628     c* errore se sono state escluse tutte le spedizioni
046800060628     c                   if        totnrre = totnrr
046900060628     C                   SETON                                        4044
047000060628     c                   else
047100060516     c                   movel     9             §ndt
047200060516     c     kndtc         setll     tncsb01l                               44
047300060516     c   44              seton                                        40
047400060516     c                   move      parndt        §ndt
047500060516     C                   END
047600060628     C                   END
047700060628     C                   END
047800060516     C                   END
047900120207     c* se inserito assegno verifico che sia inserita anche l'opzione
048000120207     c                   if        not *in32 and *in13 and ssce = ' '
048100120207     c                             and (snra <> ' ' or stpa <> ' ' or
048200120207     c                             sdte <> 0 or sabi <> 0 or scab <> 0)
048300120207     C                   SETON                                        4440
048400120207     c                   end
048500120207     c*
048600910701     C  N98SNRA          IFEQ      *BLANKS
048700910701     C                   SETON                                        4140
048800910701     C                   END
048900910927     C  N98STPA          IFEQ      *BLANKS
049000910927     C                   MOVEL     'B'           STPA
049100910927     C                   END
049200941031     C                   MOVE      SDTE          G08DAT
049300941031     C                   MOVEL     *BLANK        G08ERR
049400941031     C                   CALL      'XSRDA8'
049500941031     C                   PARM                    WLBDA8
049600941031     C                   MOVE      G08DAT        SDTE
049700941031     C  N98G08ERR        IFEQ      '1'
049800910701     C                   SETON                                        4042
049900910701     C                   END
050000910701     C*
050100910701     C  N98KABI          CHAIN     CNABI01L                           21
050200910701     C  N98
050300910701     CAN 21              SETON                                        4043
050400900516      *
050500120207     c     ind32         comp      '1'                                    32
050600120207     c     ind34         comp      '1'                                    34
050700120213     c     ind35         comp      '1'                                    35
050701120629     c     ind36         comp      '1'                                    36
050800910620     C                   SETON                                        72
050900941031     C                   UPDATE    SC71DSF
051000910620     C                   END
051100910617     C                   END
051200900515     C     FICTR2        ENDSR
051300900515      *-----------------------------------------------------***********
051400120207      * verifico stessa opzione su tutte le sorelle
051500900515      *-----------------------------------------------------***********
051600120207     C     srlegame      BEGSR
051700900515      *
051800120207     c                   do        na            naa               3 0
051900120207     c                   eval      savsce = *hival
052000120207     c                   do        *hival        nr                4 0
052100120207     C     nr            chain     SC71DSF                            33
052200120207     c                   if        *in33
052300120207     c                   leave
052400120207     c                   end
052500120314     c* escludo le righe con scelta protetta
052600120207     c                   if        ind34 = '1'
052700120207     c                   iter
052800120207     c                   end
052900120207     c* per le spedizioni legate
053000120207     c                   if        pnra = nra(naa)
053100120207     c                   if        savsce <> ssce and savsce = *hival
053200120207     c                   movel     ssce          savsce            1
053300120207     c                   else
053400120207     c                   if        ssce <> savsce
053500120207     c                   seton                                        4044
053600120207     c                   end
053700120207     c                   end
053701120314     c                   end
053800120207     c     ind32         comp      '1'                                    32
053900120207     c     ind34         comp      '1'                                    34
054000120213     c     ind35         comp      '1'                                    35
054001120629     c     ind36         comp      '1'                                    36
054100120207     C                   SETON                                        72
054200120207     C                   UPDATE    SC71DSF
054400120207     c                   enddo
054500120207     c                   enddo
054600120207     C*
054700120207     c                   endsr
054800120207      *-----------------------------------------------------***********
054900120207      * AGGIORNAMENTO DATA ARRIVO DISTINTA                  *  AGGIOR *
055000120207      *-----------------------------------------------------***********
055100120207     C     AGGIOR        BEGSR
055200120207      *
055300120207     C     *IN20         DOUEQ     '1'
055400120207     C                   READC     SC71DSF                                20
055500900524      *-------------
055600910701     C     *IN20         IFEQ      '0'
055700910701     C                   MOVE      SLNP          §LNP
055800941031     C                   MOVE      SAAS          §AAS
055900941031     C                   MOVEL     SGMSP         WDUE
056000941031     C                   MOVE      WDUE          §MGS
056100941031     C                   MOVE      SGMSP         WDUE
056200941031     C                   MOVEL     WDUE          §MGS
056300910701     C                   MOVE      SNRS          §NRS
056400910701     C                   MOVE      SNSP          §NSP
056500941031     C     KNSP          CHAIN     TNCSB01L                           21
056600910701     C     *IN21         IFEQ      '0'
056700910702     C* CREO RECORD PRIMA DELLA VARIAZ.
056800910919     C     *IN13         IFEQ      '1'
056900910919     C     SSCE          ANDEQ     '0'
057000910919     C                   MOVEL     'VACA'        CSVCAV
057100910919     C                   END
057200060516     c     ssce          ifne      'E'
057300910919     C     *IN14         IFEQ      '1'
057400910919     C     SSCE          IFEQ      '9'
057500910918     C                   MOVEL     'VACA'        CSVCAV
057600910919     C                   ELSE
057700910702     C                   MOVEL     PARCAV        CSVCAV
057800910918     C                   END
057900910919     C                   END
058000910702     C                   MOVE      AMGUDT        CSVDTV
058100910702     C                   TIME                    CSVORV
058200910702     C                   Z-ADD     046           CSVFEV
058300910702     C                   MOVEL     CSBREC        CSVREC
058400910702     C* AZZERO FLAG TRASMISS.
058500910702     C                   MOVE      *ZEROS        CSVDTR
058600910702     C                   MOVEL     *BLANK        CSVFTR
058700941031     C                   WRITE     TNCSV000
058800060516     C                   END
058900910702     C*
059000941031     C  N98              MOVE      SDTE          G08DAT
059100941031     C                   MOVEL     *BLANK        G08ERR
059200941031     C                   CALL      'XSRDA8'
059300941031     C                   PARM                    WLBDA8
059400941031     C  N98              MOVE      G08INV        CSBDTE
059500950713     C  N98              MOVEL     SNRA          CSBNRA
059600910701     C  N98              MOVE      STPA          CSBTPA
059700910701     C  N98              MOVE      SABI          CSBABI
059800910701     C  N98              MOVE      SCAB          CSBCAB
059900910702     C*
060000910702     C     PARTXT        IFEQ      '01'
060100910919     C     SSCE          ANDEQ     '5'
060200120222     C* stampo solo se gli assegni smarriti sono a fronte di una sola
060300120227     c* spedizione altrimenti aggiorno lo stao delle spedizioni e la
060400120227     c* stampa la gestiscono a mano. questo perchè le LNP o CDI possono
060500120227     c* essere diversi
060600120224     c                   clear                   stampa            1
060700120223     c                   if        %subst(pnra: 10: 1)<> ' '
060800120222     c                   z-add     1             xx
060900120222     c     pnra          lookup    nra(xx)                                33
061000120222     c                   if        *in33 and nrs(xx) = 1
061100120223     c                   eval      stampa = *on
061200120223     c                   else
061300120223     c                   eval      stampa = *off
061400120223     c                   end
061500120223     c                   else
061600120223     c                   eval      stampa = *on
061700120223     c                   end
061800011031     c* reperisco campi per stampa
061900120223     c                   if        stampa = *on
062000120227     c                   exsr      srcampi
062100120227     c* faccio 5 stampe identiche eccetto la "freccia" nell'indirizzo
062200120227     c     80            do        83            xy                2 0
062300120227     c                   movea     '0000'        *in(80)
062301120301     c                   setoff                                       89
062400120227     c                   eval      *in(xy) = *on
062500120227     c                   exsr      srcampi
062600120227     c                   enddo
062700120222     C                   END
062800120223     C                   END
062900011031     c*
063000910919     C     SSCE          IFNE      '9'
063100060516     C     SSCE          andne     'E'
063200910919     C                   MOVE      SSCE          CSBSTA
063300970408     C* CONTROLLO SE DEVO TRASMETTERE VARIAZIONE STATO C/ASS IN FILIALE
063400970408     C                   Z-ADD     1             TBLKUT
063500970408     C                   MOVEL     *BLANKS       TBLUNI
063600970408     C                   MOVEL     '4S'          TBLCOD
063700970408     C                   MOVEL     *BLANKS       TBLKEY
063800970408     C                   MOVE      SSCE          TBLKEY
063900120201     c                   clear                   ds4s
064000120201     C     KTAB          CHAIN     TABEL00F
064100120201     c                   if        %found(tabel00f)
064200970408     C                   MOVEL     TBLUNI        DS4S
064300120201     c                   end
064400970408     C     §4SFIL        IFEQ      'S'
064500970408     C                   MOVEL     *BLANKS       CSBFTR
064600970408     C                   Z-ADD     0             CSBDTR
064700970408     C                   END
064800910919     C   13              MOVE      PARDDC        CSBDDC
064900910919     C   13              MOVE      PARNDT        CSBNDT
065000910919     C   13              MOVE      AMGUDT        CSBUDA
065100910919     C   13              MOVE      'M'           CSBTPI
065200950127     C   13              MOVE      'M'           CSBFUS
065300910918     C                   END
065400910919     C     SSCE          IFEQ      '9'
065500910918     C* ANNULLO  SOLO L'INCASSO
065600910918     C                   MOVEL     *BLANKS       CSBTPI
065700950127     C                   MOVEL     *BLANKS       CSBFUS
065800910918     C                   MOVE      *ZEROS        CSBNDT
065900910918     C                   MOVE      *ZEROS        CSBDDC
066000910918     C                   MOVE      *BLANKS       CSBNRA
066100910918     C                   MOVE      *ZEROS        CSBDTE
066200910918     C                   MOVE      *ZEROS        CSBABI
066300910918     C                   MOVE      *ZEROS        CSBCAB
066400910918     C                   MOVE      *ZEROS        CSBUDA
066500910918     C                   MOVE      *BLANKS       CSBTPA
066600120207     c* se legata devo annullare l'assegno
066700120207     c                   if        %subst(pnra: 5: 1) <>' '
066800120207     c     pnra          setll     tncsm03l
066900120207     c                   do        *hival
067000120207     c     pnra          reade     tncsm03l
067100120207     c                   if        %eof(tncsm03l)
067200120207     c                   leave
067300120207     c                   end
067400120207     c                   delete    tncsm000
067500120207     c                   enddo
067600910918     C                   END
067700120207     C                   END
067800060516     c* esclusione momentanea di una spedizione dalla distinta. Forzo un 9
067900060516     c* davanti al numero distinta, verrà poi eliminato dal TNSC70
068000060516     c                   if        ssce = 'E'
068100060516     c                   movel     9             csbndt
068200060516     C                   EXCEPT    AGGCSBe
068300060516     c                   else
068400910918     C                   EXCEPT    AGGCSB
068500060516     c                   end
068600910617     C                   END
068700900524      *---------
068800910617     C                   END
068900910701     C                   END
069000011031     C                   ENDSR
069100011031      *-----------------------------------------------------***********
069200011031      * Campi per stampa
069300011031      *-----------------------------------------------------***********
069400011031     C     SRCAMPI       BEGSR
069500011031     c* destinatario spedizione
069600011106     c                   clear                   prom              5
069700011031     c     kbol          chain     titas30c
069800011031     c                   if        %found(titas30c)
069900011031     C                   MOVEL     tasrsd        ragd
070000011031     C                   MOVEL     tasrsd        ragdo
070100011031     C                   MOVEL     tasind        indd
070200011031     C                   MOVEL     tascad        capd
070300011107     C                   eval      locd = %trimr(taslod)+' ('+tasprd+')'
070400011031     c                   else
070500011031     C                   MOVEL     *ALL'?'       ragd
070600011031     C                   MOVEL     *ALL'?'       ragdo
070700011031     C                   MOVEL     *all'?'       indd
070800011031     C                   MOVEL     *all'?'       capd
070900011031     C                   MOVEL     *ALL'?'       locd
071000011031     c                   end
071001120305     c* data spedizione
071002120305     c                   movel     tasaas        com8              8 0
071003120305     c                   move      tasmgs        com8
071004120305     c                   movel     com8          dtaiso
071005120305     c                   movel     dtaiso        dtaeur
071006120305     c                   movel     dtaeur        sdtaspe
071100011031     c* punto operativo di partenza
071200011031     C     CSBlnp        CHAIN     AZORG01L
071300011031     c                   if        %found(azorg01l)
071400011031     C                   MOVEL     ORGDES        pom
071500011031     c                   else
071600011031     C                   MOVEL     *ALL'?'       pom
071700011031     c                   end
071800011031     c* punto operativo di arrivo
071900011031     C     CSBlna        CHAIN     AZORG01L
072000011031     c                   if        %found(azorg01l)
072100011031     C                   MOVEL     ORGDES        pod
072200011031     c                   else
072300011031     C                   MOVEL     *ALL'?'       pod
072400011031     c                   end
072500011031     c* mittente spedizione
072600011031     C                   MOVE      csbCDI        §8899             4
072700011031     c                   if        §8899 = '8888' or §8899 = '9999'
072800011031     c     kbol          chain     tncsa01l
072900011031     c                   if        %found(tncsa01l)
073000011031     C                   MOVEL     csarsd        ragm
073100011031     C                   MOVEL     csarsd        ragmo
073200011031     C                   MOVEL     csavid        indm
073300011031     C                   MOVEL     csacad        capm
073400011107     C                   eval      locm = %trimr(csacid)+' ('+csaprm+')'
073500011031     c                   else
073600011031     C                   MOVEL     *ALL'?'       ragm
073700011031     C                   MOVEL     *ALL'?'       ragmo
073800011031     C                   MOVEL     *all'?'       indm
073900011031     C                   MOVEL     *all'?'       capm
074000011031     C                   MOVEL     *ALL'?'       locm
074100011031     c                   end
074200011031     c                   else
074300011031     C                   CLEAR                   TIBS69
074400011031     C                   CLEAR                   CNACO
074500011031     C                   cLEAR                   CNIND
074600011031     C                   CLEAR                   CNCLP
074700011031     C                   CLEAR                   FNCLS
074800011031     C                   MOVEL     ' '           I69TLA
074900011031     C                   MOVEL     KNSIF         I69SIF
075000011031     C                   Z-ADD     0             I69KCC
075100011031     C                   Z-ADD     csbcdi        I69KAC
075200011031     C                   Z-ADD     csbcdi        I69KIN
075300011031     C                   Z-ADD     0             I69KCP
075400011031     C                   CALL      'TIBS69R'
075500011031     C                   PARM                    TIBS69
075600011031     C                   PARM                    CNACO
075700011031     C                   PARM                    CNIND
075800011031     C                   PARM                    CNCLP
075900011031     C                   PARM                    FNCLS
076000011031     C                   IF        o69err = *BLANK
076100011031     C                   eval      ragm = acorag
076200011106     C                   eval      ragmo = acorag
076300011031     C                   eval      indm = indvia
076400011031     C                   MOVEL     INDCAP        CAPm
076500011107     C                   eval      locm = %trimr(indcit)+' ('+indprv+')'
076600011031     C                   END
076700011031     c                   end
076800120222     C* STAMPO LETTERA SMARRIMENTO ASSEGNO
076801120301     C                   EVAL      sassc = csbcas
076802120301     C                   EVAL      sdivis = csbvca
076900120222     C                   WRITE     TES01
077000120224     c*
077100120222     c                   if        %subst(pnra: 10: 1)= ' '
077200120224     c                   move      snra          snrap
077300120224     c                   move      sabi          sabip             5 0
077400120224     c                   move      scab          scabp             5 0
077500120224     c                   move      sdivis        sdivisp
077600120224     c                   move      sdte          sdtep
077700070326     C                   EVAL      sassp = sass
077800120224     c                   seton                                        50
077900120224     c                   exsr      srstampa
078000120222     c                   else
078100120222     c* multiassegni
078200120222     c     pnra          setll     tncsm03l
078300120224     c                   do        *hival
078400120222     c     pnra          reade     tncsm03l
078500120224     c                   if        %eof(tncsm03l)
078600120224     c                   leave
078700120224     c                   end
078800120224     c                   movel     csmnra        snrap
078900120224     c                   move      csmabi        sabip
079000120224     c                   move      csmcab        scabp
079100120224     C                   MOVE      CSmDTE        G08INV
079200120224     C                   MOVEL     '3'           G08ERR
079300120224     C                   CALL      'XSRDA8'
079400120224     C                   PARM                    WLBDA8
079500120224     C                   MOVEL     G08DAT        SDTEp
079600120224     c                   movel     csmdiv        sdivisp
079700120224     C                   EVAL      sassp = csmidc
079800120224     c                   seton                                        50
079900120224     c                   exsr      srstampa
080000120222     c                   enddo
080100120224     c                   end
080200120224     c                   seton                                        51
080300120224     c                   exsr      srstampa
080400120224     c
080500120224     C                   ENDSR
080600120224     C**************************************************************************
080700120224     C* stampa
080800120224     C************************************************************
080900120224     C     srstampa      begSR
081000120224     c*
081100120224     c                   if        *in89
081200120224     c                   write     salto
081300120224     c                   setoff                                       89
081400120224     c                   end
081500120227     c   50              exsr      srabi
081600120224     c   50              write     det01
081700120301     c   51              do        10            xxx               2 0
081800120224     c                   if        *in89
081900120224     c                   write     salto
082000120224     c                   setoff                                       89
082100120224     c                   end
082200120224     c                   select
082300120224     c                   when      xxx = 1
082400120224     c                   write     tot01
082500120224     c                   when      xxx = 2
082600120224     c                   write     tot02
082700120224     c                   when      xxx = 3
082800120224     c                   write     tot03
082900120224     c                   when      xxx = 4
083000120224     c                   write     tot04
083100120224     c                   when      xxx = 5
083200120224     c                   write     tot05
083300120224     c                   when      xxx = 6
083400120224     c                   write     tot06
083500120224     c                   when      xxx = 7
083600120224     c                   write     tot07
083700120224     c                   when      xxx = 8
083800120224     c                   write     tot08
083900120224     c                   when      xxx = 9
084000120224     c                   write     tot09
084100120224     c                   when      xxx = 10
084200120224     c                   write     tot10
084700120224     c                   endsl
084800120224     c                   enddo
084900120224     c                   setoff                                       5051
085000120224     c*
085100900515     C                   ENDSR
085200120224     C**************************************************************************
085300120224     C* decodifica la banca
085400120224     C************************************************************
085500120224     C     srabi         begSR
085600120224     c* Banca
085700120224     C     kabip         CHAIN     cnabi01l
085800120224     c                   if        %found(cnabi01l)
085900120224     C                   MOVEL     abiist        desabi
086000120224     c                   else
086100120224     C                   MOVEL     *ALL'?'       desabi
086200120224     c                   end
086300120224     C                   ENDSR
086400120224     C**************************************************************************
086500120224     C* decodifica la società
086600120224     C************************************************************
086700120224     C     Decod_societa begSR
086800110428      *
086900110428     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
087000110428     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
087100110428     c* forzo società 201
087200110428     C                   EVAL      tibsSocI0.idSocieta ='201'
087300110428     C
087400110428     C                   CALL      'TIBSSOCR'
087500110428     C                   PARM      'GETANAGRAF'  prmRqsOpCode
087600110428     C                   PARM      *BLANK        prmRpyOpCode
087700110428     C                   PARM      *ZERO         prmRpyIdMsg
087800110428     C                   PARM      'TIBSSOCI0'   prmRqsFormato
087900110428     C                   PARM                    tibsSocI0
088000110428     C                   PARM                    prmRqsDataSize
088100110428     C                   PARM      'TIBSSOCO0'   prmRpyFormato
088200110428     C                   PARM                    tibsSocO0
088300110428     C                   PARM                    prmRpyDataSize
088400110428     c                   if         PRMRPYIDMSG >= 0
088500110428     c                              and TIBSSOCO0.IDSOCIETA <> *blank
088600110428     c                   eval       ragsoc  = tibsSocO0.RAGSOCIALE
088700110428     c                   else
088800110428     c                   eval       ragsoc  = *all'?'
088900110428     c                   end
089000110428      *
089100110428     C                   ENDSR
089200941031     OTNCSB000  E            AGGCSB
089300910701     O                       CSBSTA
089400910701     O                       CSBDTE
089500910701     O                       CSBNRA
089600910701     O                       CSBTPA
089700910701     O                       CSBABI
089800910701     O                       CSBCAB
089900910918     O                       CSBTPA
090000910918     O                       CSBUDA
090100910918     O                       CSBNDT
090200910918     O                       CSBDDC
090300910918     O                       CSBTPI
090400950127     O                       CSBFUS
090500970408     O                       CSBFTR
090600970408     O                       CSBDTR
090700060516     OTNCSB000  E            AGGCSBe
090800060516     O                       CSBndt

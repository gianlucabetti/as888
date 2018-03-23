000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300900606      * CNC0B4R                                                      *
000400900606      *                                                              *
000500900606      *           GESTIONE DISTINTA CONTRASSEGNI                     *
000600900606?     *--------------------------------------------------------------*
000700900524     FCNC0B4D   CF   E             WORKSTN
000800900524     F                                     SFILE(CNCB4SF:NRR)
000900060403     Ffnarb01l  iF   E           K DISK
000901060403     Fcnass01l  iF A E           K DISK    USROPN
001200000222     F                                     COMMIT
001201060403     Fyfmdi01l  iF A E           K DISK    USROPN
001202060403     F                                     COMMIT
001203060403     FFNBCO01L  UF A E           K DISK    USROPN
001204060403     F                                     RENAME(FNBCO:FNBC1)
001205060403     F                                     INFDS(TARDS)
001206060403     F                                     COMMIT
001300991122     FFNBCO00F  UF   E             DISK    USROPN
001400991122     F                                     RENAME(FNBCO:FNBC0)
001500000222     F                                     COMMIT
001600000222     FFLNUF01L  UF   E           K DISK    USROPN
001601060526     f* ho tolto il commit dal file x rilasciare il record appena possibile
001700060526     F*                                    COMMIT
001800900524     FAZORG01L  IF   E           K DISK
001900900524     FTABEL00F  IF   E           K DISK
002000991122     FTNTBE01L  IF   E           K DISK
002100900524     FCNABI01L  IF   E           K DISK
002200900525     FCNACO00F  IF   E           K DISK
002300991123     FFNMOV00T  O    E             DISK    USROPN
002400900607      *
002500900709     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002600900709     F                                     USROPN
002700991122?     *--------------------------------------------------------------*
002800900606     D FLU             S              1    DIM(32)                              FLAG PAR.U
002900900606     D OPZ             S              1    DIM(10)                              FLAGS ANAG
003000991122     D CCV             S              3    DIM(200)                             divise
003100991123     D DEC             S              2  0 DIM(200)                             decimali
003200991123     D CMB             S              9  3 DIM(200)                             cambio
003300900515      *
003400900709     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)
003500020930     D CMDM            S             49    DIM(6) CTDATA PERRCD(1)
003600971126      *
003700020726     D £6              S              3  0 DIM(30)
003800991122?     *--------------------------------------------------------------*
003801060404     d trul33ds      e ds
003900991123     D DSCMZ         E DS
004000991123     D YEURDS        E DS                  EXTNAME(YEURCODS)
004100020726     D TRul06ds      E DS
004200020726     D  LIN                    1     90  0 dim(30)
004300020726     D fnlv50ds      E DS
004400050112     D ddatiute      e ds
004500050112     D azuteds       e ds                  extname(AZUTE00F)
004600050112     D tibs34ds      E DS                  inz
004700900608     D PARAM           DS
004800020930     D                                     INZ
004900900608     D  §DIT                   1      1
005000900608     D  §NRDIS                 2      8  0
005100020930     D  PrmFle
005200020930     D                                     LIKE(BcoFle)
005300900606      *
005400900514     D KPJBA         E DS
005500900516      *
005600900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005700000000     D  TCU                  398    697
005800000000     D                                     DIM(50)                              TAB FLAG C
005900000000     D  KCU                  698    847P 0
006000000000     D                                     DIM(50)                              TAB CAPO C
006100000000     D                                     PACKEVEN
006200000000     D  DTU                  848    895P 0
006300000000     D                                     DIM(12)                              DATE PARAM
006400000000     D                                     PACKEVEN
006500900606      *
006600900606     D TCUDS           DS
006700900606     D  F4                     4      4
006800900524      *
006900900524     D DS4A          E DS
007000020729     D DY4O          E DS
007100991124     D DS1A          E DS
007200991122     D DGEI          E DS
007300991122     D DGED          E DS
007400991122     D DSCV          E DS
007500010927     D DSCW          E DS
007600010928     D*
007700060424     D savfc0          S                   like(orgfc0)
007701060424     D PrmTpInc        S              2
007800010928     D PrmDivisa       S              3
007900010928     D PrmData         S               D
008000010928     D PrmEsito        S              1
008100011002     D PrmDS1A         S
008200011002     D                                     LIKE(DS1A)
008300011002     D PrmDSCW         S
008400011002     D                                     LIKE(DSCW)
008500011002     D Wrk1ADes        S
008600011002     D                                     LIKE(§1ADes)
008700900515      *
008800991123     D WLBDAT          DS
008900991122     D  G02DAT                 1      8  0
009000991122     D  G02INV                 9     16  0
009100991122     D  G02ERR                17     17
009200991122     D  G02TGI                18     22  0
009300900524      *
009400020930     D TIBS55        E DS                  EXTNAME(TIBS55DS)
009500991123     D TARDS           DS
009600900524     D  RRN                  397    400B 0
009700010927
009800900515      *---------------------------------------------------------------*
009900900515      * FLUSSO PRINCIPALE                                             *
010000900515      *---------------------------------------------------------------*
010100991122     C                   OPEN      FNBCO00F
010200991122     C                   OPEN      FNBCO01L
010300000222     C                   OPEN      FLNUF01L
010301060403     C                   OPEN      cnass01L
010302060403     C                   OPEN      yfmdi01L
010400900515      *          =============
010500900509     C     INIZIO        TAG
010600900515      *          =============
010700900515     C                   EXSR      PULIZ
010800900525      *
010900900515      *          =============
011000900515     C     FMT00         TAG
011100900515      *          =============
011200900525      *
011300900524     C                   EXFMT     CNCB400
011400900515     C   03              GOTO      FINE                                         F3=FINE
011500000121      *
011600000121      * se richiesto cambio filiale riemetto videata
011700000121     C  N18
011800000121     CAN 01*INKS         IFEQ      '1'
011900000121     C                   SETON                                        18
012000000121     C                   GOTO      FMT00
012100000121     C                   END
012200000121      * controllo se è stata modificata la filiale
012300000121     C     *IN18         IFEQ      '1'
012400000121     C     *IN01         ANDEQ     '1'
012500050112     C     VIDFLE        ANDNE     dutpou
012600000121     C                   EXSR      CTRFIL
012700000121     C   90              GOTO      FMT00
012800000121     C                   END
012900000121      *
013000910102     C                   MOVE      VIDDDS        AAA
013100900524     C                   EXSR      CTR1
013200900524     C   90
013300900524     COR 04              GOTO      FMT00
013400900524      *
013500900607     C                   EXSR      PUTSFL
013600900515      *          =============
013700900515     C     FMT01         TAG
013800900515      *          =============
013900900607      *
014000900524     C                   WRITE     CNCB4RC
014100900524     C                   EXFMT     CNCB4CT
014200900515     C   12              GOTO      INIZIO
014300900524     C                   EXSR      CTR2
014400900524     C   90              GOTO      FMT01
014500900516      *
014600900516      * ANNULLA
014700900516     C   16              DO
014800900607     C                   EXSR      DEL00F
014900900516     C                   GOTO      INIZIO
015000900516     C                   END
015100900524      *
015200900607      *------------------------------------------------
015300900607      * AGGIORNAMENTO
015400900524     C   06              DO
015500900607      *
015600900607      * CONTROLLO SE CI SONO RIGHE IN SFL
015700900607      *
015800900607     C     1             CHAIN     CNCB4SF                            88
015900900607     C   88              GOTO      FMT01
016000900607     C     VIDLNP        IFEQ      0
016100900607     C                   GOTO      FMT01
016200900607     C                   END
016300900607      *
016400900607     C                   EXSR      AGGIOR
016500000222     C* ESEGUO AGGIORNAMENTO NUMERATORE SOLO SE SONO IN IMMISSIONE
016600060526     C*    WTPVIS        IFEQ      'I'
016700060526     C*                  Z-ADD     6             CNU
016800060526     C*                  MOVE      VIDCOD        FIL
016900060526     C*                  MOVE      *ZEROS        NUFFL2
017000060526     C*    KNUF          CHAIN     FLNUF01L                           86
017100060526     C* N86              Z-ADD     VIDNDS        NUFNUM
017200060526     C* N86              UPDATE    FLNUF
017300060526     C*                  END
017400900525      *
017500900525      *          =============
017600900525     C     FMT02         TAG
017700900525      *          =============
017800900525      *
017900900525     C                   EXSR      RIEMP2
018000900606      *
018100900606      *          =============
018200900606     C     SU2           TAG
018300900606      *          =============
018400900606      *
018500900525     C                   WRITE     CNCB403
018600900525     C                   WRITE     CNCB402
018700900525     C                   EXFMT     CNCB401
018800900607     C   12              EXSR      PUTSFL
018900900607     C   12              GOTO      FMT01
019000041221     C                   SETOFF                                       91
019100000303     C*
019200000303     C* SE NON HO AGGANCIATO TABELLA CHIUSURA OPERATORE
019300000303     C* CONTROLLO IL CONTO IMMESSO MANUALMETE E' VALIDO
019400000303     C     *IN96         IFEQ      '1'
019500000303     C* CONTROLLI
019600000303     C     VIDSC1        IFEQ      0
019700000303     C                   SETON                                        93
019800000303     C                   GOTO      SU2
019900000303     C                   END
020000020729     C                   move      §4OCC1        CCCWWW
020100000303     C                   Z-ADD     VIDSC1        SCCWWW
020200000303     C     KACO          CHAIN     CNACO00F                           94
020300000303     C   94              GOTO      SU2
020400000303     C     ACOFLT        IFNE      VIDFLE
020500000303     C                   SETON                                        95
020600000303     C                   GOTO      SU2
020700000303     C                   END
020800000303     C                   END
020900991124     C* EFFETTUO CONTROLLO SOLO SE IMPORTO <> 0
021000991124     C     VIDIMP        IFNE      0
021100991124     C* CONTROLLI
021200991124     C                   SETOFF                                       8992
021300991124     C     VIDNP1        CABEQ     *ZEROS        SU2                      92
021400991124     C                   CLEAR                   WLBDAT
021500991124     C                   MOVE      VIDDP1        G02DAT                         *
021600991124     C                   CALL      'XSRDA8'
021700991124     C                   PARM                    WLBDAT
021800991124     C     G02ERR        IFEQ      '1'                                          *
021900991124     C                   SETON                                        89
022000991124     C                   GOTO      SU2
022100991124     C                   END
022200991124     C                   MOVE      G02DAT        VIDDP1
022300991124     C                   END
022400900606      *--------------------------------
022500900607      * STAMPA DI CONTROLLO
022600900607      *
022700900607     C   08              DO
022800900607     C                   EXSR      STAMPA
022900900607     C                   GOTO      SU2
023000900607     C                   END
023100900607      *--------------------------------
023200900607      *
023300900607      *--------------------------------
023400991122      * TRASMISSIONE FNBCO00F E
023500900607      * SCRITTURA MOVIMENTO CONTABILE
023600900607      *
023700900607     C   06              DO
023800000222     C* SE IMPORTO DA CONTABILIZZARE > 0 APRO FNMOV
023900000222     C     VIDIMP        IFNE      0
024000000222     C* SE TUTTO OK CONTROLLO SE ESISTE FNMOV
024100020930     C                   MOVEL     *BLANKS       VAR5              8
024200020930     C                   MOVE      '046) '       VAR5
024300000222     C                   MOVEL     VIDFLE        VAR5
024400000222     C                   MOVE      VAR5          CMDM(1)
024500020930     C                   MOVEL     CMDM(1)       CMDX1            49
024600020930     C                   Z-ADD     49            LUNCMD           15 5
024700000222     C                   SETOFF                                       90
024800000222     C                   CALL      'QCMDEXC'                            90
024900000222     C                   PARM                    CMDX1
025000000222     C                   PARM                    LUNCMD
025100000222     C* SE MEMBRO FNMOV NON ESISTE LO AGGIUNGO
025200000222     C     *IN90         IFEQ      '1'
025300000222     C                   MOVE      VAR5          CMDM(2)
025400000222     C                   MOVEL     CMDM(2)       CMDX1
025500020930     C                   Z-ADD     49            LUNCMD           15 5
025600000222     C                   SETOFF                                       91
025700000222     C                   CALL      'QCMDEXC'                            91
025800000222     C                   PARM                    CMDX1
025900000222     C                   PARM                    LUNCMD
026000041221     c   91              goto      su2
026100000222     C                   END
026200000222     C* SE TUTTO OK ALLOCO FNMOV
026300000222     C     *IN91         IFEQ      '0'
026400020930     C                   MOVE      '046))'       VAR5
026500000222     C                   MOVEL     VIDFLE        VAR5
026600000222     C                   MOVE      VAR5          CMDM(3)
026700000222     C                   MOVEA     CMDM(3)       CMDX1
026800020930     C                   Z-ADD     49            LUNCMD           15 5
026900000222     C                   SETOFF                                       91
027000000222     C                   CALL      'QCMDEXC'                            91
027100000222     C                   PARM                    CMDX1
027200000222     C                   PARM                    LUNCMD
027300041221     c   91              goto      su2
027400041221     C  n91              MOVEL     'S'           WACMOV            1
027500041221     C                   END
027600000222     C* Una volta allocato eseguo Ovrdbf
027700000222     C     *IN91         IFEQ      '0'
027800020930     C                   MOVE      '046) '       VAR5
027900000222     C                   MOVEL     VIDFLE        VAR5
028000000222     C                   MOVE      VAR5          CMDM(4)
028100000222     C                   MOVEA     CMDM(4)       CMDX1
028200020930     C                   Z-ADD     49            LUNCMD           15 5
028300000222     C                   SETOFF                                       91
028400000222     C                   CALL      'QCMDEXC'                            91
028500000222     C                   PARM                    CMDX1
028600000222     C                   PARM                    LUNCMD
028700041221     c   91              goto      su2
028800000222     C                   END
028900000222     C* OPEN FNMOV
029000991123     C                   MOVEL     'S'           WOPMOV            1
029100991123     C                   OPEN      FNMOV00T
029200000222     C                   END
029300041221      * CONFERMO LE VARIAZIONI
029400041221     C                   COMMIT
029500041221      *--------------------------
029600041221      *
029700041221?     ***********************
029800041221?     * TRASMISSIONE FNBCO00F
029900041221     C                   CLOSE     FNBCO00F
030000041221     C                   CLOSE     FNBCO01L
030100900606      *
030200900608     C                   Z-ADD     VIDNDS        §NRDIS
030300900608     C                   MOVEL     VIDCOD        §DIT
030400020930     C                   EVAL      PrmFle = VidFle
030500900608     C                   MOVEL     PARAM         KPJBU
030600900607     C                   CALL      'TRTR82R'
030700900606     C                   PARM                    KPJBA
030800900606      *
030900991122     C                   OPEN      FNBCO00F
031000991122     C                   OPEN      FNBCO01L
031100900618      *  REGISTR. SOLO SE IMPORTO REG. <>0
031200900618     C     VIDIMP        IFNE      0
031300900606     C                   EXSR      REGMOV
031400000310     C     WOPMOV        IFEQ      'S'
031500000310     C                   CLOSE     FNMOV00T
031600000310     C                   END
031700991123     C* Eseguo immediatamente trasmissione file movimenti ctb
031800991123     C                   EXSR      TRAMOV
031900000310     C                   EXSR      LIBMOV
032000900618     C                   END
032100900618      *
032200900606     C                   EXSR      STAMPA
032201060404     c* aggiorno file assegni e mandato d'introito
032202060404     c                   exsr      sraggio
032203060404     c*
032300900607     C                   EXSR      DEL00F
032400000224      * CONFERMO LE VARIAZIONI
032500000224     C                   COMMIT
032600900606      *
032700900606     C                   GOTO      INIZIO
032800900608      *
032900900608      *--------------------------
033000900608     C                   END
033100900606      *--------------------------------
033200900607     C                   GOTO      SU2
033300900525      *
033400900524     C                   END
033500900607      *------------------------------------------------
033600900607     C                   GOTO      FMT01
033700900515      *
033800000000     C     FINE          TAG
033900000310     C     WOPMOV        IFEQ      'S'
034000000310     C                   CLOSE     FNMOV00T
034100000310     C                   END
034200991124     C                   EXSR      LIBMOV
034300920515     C*-                                                   *
034400000222     C                   ROLBK
034500000000     C                   SETON                                        LR
034600900515      *-----------------------------------------------------***********
034700900515      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
034800900515      *-----------------------------------------------------***********
034900900515     C     PULIZ         BEGSR
035000900515      *
035100050112     C                   MOVE      dutpou        VIDFLE
035200900524     C                   MOVEL     *BLANKS       VIDDFI
035300910102     C                   MOVE      '1'           VIDCOD
035400900524     C                   MOVEL     *BLANKS       VIDDES
035500991124     C                   Z-ADD     GMAUD8        VIDDDS
035600900524     C                   MOVEL     'CNC0B4R'     VIDPGM
035700011016     C                   MOVEL     §GEDCN        V1CDIV
035800000121      *
035900050112     C                   SETOFF                                       18
036000000121      *
036100000121      * DECODFICO FILIALE ELABORATORE
036200000121      *
036300000121     C                   MOVE      VIDFLE        CODFIL            3 0
036400000121     C     KAZO          CHAIN     AZORG01L                           31
036500000121     C                   MOVEL     *BLANKS       VIDDFI
036600000121     C  N31              MOVEL     ORGDES        VIDDFI
036700000121     C   31              MOVEL     *ALL'?'       VIDDFI
036701060424     c                   eval      savfc0 = orgfc0
036800060424     c                   if        orgfc0 <> dutpou
036900060424     c                   setoff                                       01
037000060424     c                   else
037100060424     c                   seton                                        01
037200060424     c                   end
037300060424     C* Controllo se filiale che gestisce la £6
037400000121     C                   MOVEL     'S'           W£6               1
037500020726     C*                  END
037600900524      *
037700900514     C                   ENDSR
037800000121      *-----------------------------------------------------***********
037900000121      * CONTROLLI FORMATO 1                                 *  CTR1   *
038000000121      *-----------------------------------------------------***********
038100000121     C     CTRFIL        BEGSR
038200000121      *
038300020726     C                   SETOFF                                       9037
038400000121      *
038500000121      * CONTROLLO FILIALE ELABORATORE
038600000121      *
038700000121     C                   MOVE      VIDFLE        CODFIL            3 0
038800000121     C     KAZO          CHAIN     AZORG01L                           36
038900000121     C                   MOVEL     *BLANKS       VIDDFI
039000000121     C  N36              MOVEL     ORGDES        VIDDFI
039100000121     C   36              MOVEL     *ALL'?'       VIDDFI
039200000121     C   36              SETON                                        90
039300000121     C   90              GOTO      FIFLE
039400000121      *
039500060426      * controllo che la filiale scelta sia gestita dallo stesso po di
039600060426      * CONTABILITA'
039702060424     C     ORGFC0        IFNE      savfc0
039800060424     C                   SETON                                        3790
039900060424     C                   ELSE
040000000121     C                   MOVEL     'S'           W£6
040100020726      * ricarico la £6 se veniva cambiato il P.O aveva i dati errati
040200020726     c                   clear                   trul06ds
040300020726     C                   MOVEL     '£6'          D06COD
040400020726     c                   movel     'S'           d06esc
040500020726     C                   MOVEL     vidfle        D06KEY
040600020726     C                   MOVEL     TRul06ds      KPJBU
040700020726     C                   CALL      'TRUL06R'
040800020726     C                   PARM                    KPJBA
040900020726     C                   MOVEL     KPJBU         TRul06ds
041000020726     C                   MOVEA     LIN           £6
041100040921     c*                  end
041200020726     c
041300000121     C     VIDFLE        LOOKUP    £6                                     31
041400000121     C     *IN31         IFEQ      '1'
041500000121     C                   SETON                                        3790
041600000121     C                   END
041700020726     C*                  END
041800060424     C                   END
041900000121     C*
042000000121     C     FIFLE         ENDSR
042100900515      *-----------------------------------------------------***********
042200900515      * CONTROLLI FORMATO 1                                 *  CTR1   *
042300900515      *-----------------------------------------------------***********
042400900515     C     CTR1          BEGSR
042500900515      *
042600900509     C                   SETOFF                                       90
042700900515      *
042800900608     C     VIDCOD        IFEQ      ' '
042900900608     C                   SETON                                        90
043000900608     C                   END
043100900608      *
043200900524      * CONTROLLO DATA
043300910102     C*
043400900524     C                   MOVE      VIDDDS        G02DAT
043500900515     C                   MOVEL     *BLANK        G02ERR
043600991122     C                   CALL      'XSRDA8'
043700900515     C                   PARM                    WLBDAT
043800900515     C     G02ERR        IFEQ      '1'
043900900524     C                   SETON                                        32  90
044000900515     C                   END
044100910102     C     G02INV        IFLE      DTU(1)
044200991122     C     G02INV        ORGT      AMGUD8
044300910102     C                   SETON                                        32  90
044400910102     C                   END
044500900515     C   90              GOTO      FICTR1
044600900524     C                   Z-ADD     G02INV        BCODDS
044700900524      *
044800900524     C                   MOVEL     '4A'          CODTAB
044900971128     C                   MOVEL     *BLANKS       KEYTAB
045000971128     C                   MOVEL     VIDCOD        KEYTAB
045100900524     C     KTAB          CHAIN     TABEL00F                           77
045200900524     C  N77              MOVEL     TBLUNI        DS4A
045300110504     C                   MOVEL     ragut         VIDDES
045400991122     C* Controllo se immessa divisa ....
045500991122     C     V1CDIV        IFEQ      *BLANKS
045600991124     C                   SETON                                        3490
045700991122     C                   ELSE
045800991122     C                   Z-ADD     1             XY                3 0
045900991122     C     V1CDIV        LOOKUP    CCV(XY)                                35
046000991124     C  N35              SETON                                        3390
046100991122     C                   END
046200050112     c*
046300060424     c                   if        orgfc0 <> savfc0
046400060424     c                   seton                                        3690
046500060424     c                   end
046600050112     c*
046700060424     c*                  if        vidfle = dutpou
046800060421     c                   clear                   trul06ds
046900060421     C                   MOVEL     '£6'          D06COD
047000060421     c                   movel     'S'           d06esc
047100060424     C                   MOVEL     vidfle        D06KEY
047200060421     C                   MOVEL     TRul06ds      KPJBU
047300060421     C                   CALL      'TRUL06R'
047400060421     C                   PARM                    KPJBA
047500060421     C                   MOVEL     KPJBU         TRul06ds
047600060421     C                   MOVEA     LIN           £6
047700060424     c*                  endif
047800991122     C*
047900900515     C     FICTR1        ENDSR
048000900607      *-----------------------------------------------------***********
048100900607      * RIEMPIMENTO SFL                                     *  PUTSFL *
048200900607      *-----------------------------------------------------***********
048300900607     C     PUTSFL        BEGSR
048400900607      *
048500900607      * PULISCO IL SFL
048600900607      *
048700900607     C                   Z-ADD     1             NRR
048800900607     C                   SETON                                        70
048900900607     C                   WRITE     CNCB4CT
049000900607     C                   SETOFF                                       70
049100900607     C                   SETON                                        71
049200991122     C                   MOVEL     V1CDIV        V2CDIV
049300900607      *
049400900607     C                   DO        100           NRR
049500900607     C                   Z-ADD     0             VIDRRN
049600900607     C                   Z-ADD     0             VIDLNP
049700900607     C                   Z-ADD     0             VIDLNA
049800900607     C                   Z-ADD     0             VIDAAS
049900900607     C                   Z-ADD     0             VIDNRS
050000900607     C                   Z-ADD     0             VIDNSP
050100900607     C                   Z-ADD     0             VIDDTA
050200900607     C                   Z-ADD     0             VIDIMA
050300900607     C                   MOVEL     *BLANKS       VIDTIP
050400900607     C                   MOVEL     *BLANKS       VIDMIT
050500900607     C                   Z-ADD     0             VIDRIF
050600900607     C                   Z-ADD     0             VIDABI
050700900607     C                   Z-ADD     0             VIDCAB
050800900607     C                   Z-ADD     0             VIDNRA
050900900607     C                   WRITE     CNCB4SF
051000900607     C                   END
051100900607      *
051200020930     C     K02Bco01      SETLL     FNBCO01L
051300020930     C     K02Bco01      READE     FNBCO01L                               80
051400000222     C   80              MOVEL     'I'           WTPVIS            1
051500000222     C  N80              EXSR      RIEMPI
051600000222      * SE SONO IN IMMISSIONE E NON IN MODIFICA REPERISCO NUMERATORE
051601060608     c                   if        not *in12 and not *in16
051700060608     C     WTPVIS        IFEQ      'I'
051800900607     C                   Z-ADD     6             CNU
051900900607     C                   MOVE      VIDCOD        FIL
052000930226     C                   MOVE      *ZEROS        NUFFL2
052100930226     C     KNUF          CHAIN     FLNUF01L                           79
052101060526     c                   if        not *in79
052200060526     C     1             ADD       NUFNUM        VIDNDS
052300060526     C     1             ADD       NUFNUM        VIDNRE
052400060526     C                   MOVEL     VIDFLE        VIDNRE
052401060526     C                   Z-ADD     VIDNDS        NUFNUM
052402060526     C                   UPDATE    FLNUF
052500900607     C                   END
052501060526     C                   END
052502060608     C                   END
052600900607      *
052700900607     C                   ENDSR
052800900515      *-----------------------------------------------------***********
052900900524      * CONTROLLI SUBFILE                                   *  CTR2   *
053000900515      *-----------------------------------------------------***********
053100900515     C     CTR2          BEGSR
053200900515      *
053300900524     C                   SETOFF                                       90
053400900524     C                   SETOFF                                       404142
053500900524     C                   SETOFF                                       434445
053600900524     C                   SETOFF                                       464748
053601060403     C                   SETOFF                                       5859
053700900524      *
053800900524     C                   Z-ADD     1             NRR
053900900524      *--------------------
054000900524     C                   DO        100           NRR
054100900524     C     NRR           CHAIN     CNCB4SF                            82
054200900524      *-------------
054300900524     C     VIDLNP        IFNE      0
054400900524     C     VIDLNA        ORNE      0
054500991122     C* Converto anno da 2 a 4  cifre se immesso
054600991122     C                   MOVEL     VIDAAS        WCOMO2            2 0
054700991122     C     WCOMO2        IFEQ      0
054800991122     C     VIDAAS        IFGE      40
054900991122     C                   MOVEL     19            VIDAAS
055000991122     C                   ELSE
055100991122     C                   MOVEL     20            VIDAAS
055200991122     C                   END
055300991122     C                   END
055400900524      *******************************
055500900524      * CONTROLLO FILIALE DI PARTENZA
055600900524      *
055700900524     C                   MOVE      VIDLNP        CODFIL            3 0
055800900524     C     KAZO          CHAIN     AZORG01L                           40
055900900524     C   40              SETON                                        90
056000900524     C   40              UPDATE    CNCB4SF
056100900524     C   90              GOTO      FICTR2
056200900524      *****************************
056300900524      * CONTROLLO FILIALE DI ARRIVO
056400900524      *
056500900524     C                   MOVE      VIDLNA        CODFIL            3 0
056600900524     C     KAZO          CHAIN     AZORG01L                           41
056700000121     C* Controllo se lna è in £6 di VIDFLE o è uguale a VIDFLE
056800000121     C  N41VIDLNA        IFNE      VIDFLE
056900000121     C     W£6           IFEQ      'S'
057000000121     C                   Z-ADD     1             YY
057100000121     C     VIDLNA        LOOKUP    £6(YY)                                 31
057200020726     C     *IN31         IFNE      '1'
057300020726     C                   SETON                                        55
057400020726     C                   END
057500000121     C                   ELSE
057600000121     C                   SETON                                        55
057700000121     C                   END
057800000121     C                   END
057900000121     C     *IN41         IFEQ      '1'
058000000121     C     *IN55         OREQ      '1'
058100000121     C                   SETON                                        90
058200000121     C                   UPDATE    CNCB4SF
058300000121     C                   END
058400000121     C*
058500900524     C   90              GOTO      FICTR2
058600060424     C*    dutpou        IFNE      vidfle
058700060424     C*                  SETON                                        9041
058800060424     C*                  UPDATE    CNCB4SF
058900060424     C*                  END
059000971126     C   90              GOTO      FICTR2
059100900524      ***********************************
059200900524      * NUMERO DI SPEDIZIONE OBBLIGATORIO
059300900524      *
059400900524     C     VIDNSP        IFEQ      0
059500900524     C                   SETON                                        42  90
059600900524     C                   UPDATE    CNCB4SF
059700900524     C                   END
059800900524     C   90              GOTO      FICTR2
059801060403     c* controllo spedione su FNARB00F
059802060403     c     karb          chain     fnarb01l
059803060403     c                   if        %found(fnarb01l)
059804060403     c                             and (arbicc = ' ' or arbicc = 'R')
059805060404     c                   seton                                        58  90
059806060404     C                   UPDATE    CNCB4SF
059807060404     C                   GOTO      FICTR2
059808060403     c                   end
059809060403     c* controllo che spedione non ci sia su CNASS00F
059810060403     c     kass          chain     cnass01l
059811060403     c                   if        %found(cnass01l)
059812060404     c                   seton                                        59  90
059813060404     C                   UPDATE    CNCB4SF
059814060404     C                   GOTO      FICTR2
059815060403     c                   end
059900900524      ************************
060000900524      * CONTROLLO DATA ASSEGNO
060100900524      *
060200900524     C                   MOVE      VIDDTA        G02DAT
060300900524     C                   MOVEL     *BLANK        G02ERR
060400991122     C                   CALL      'XSRDA8'
060500900524     C                   PARM                    WLBDAT
060600900524     C     G02ERR        IFEQ      '1'
060700900524     C                   SETON                                        43  90
060800900524     C                   UPDATE    CNCB4SF
060900900524     C                   END
061000900524     C   90              GOTO      FICTR2
061100991124     C                   MOVE      G02DAT        VIDDTA
061200900524      *
061300991123     C     G02INV        IFGT      AMGUD8
061400900524     C                   SETON                                        44  90
061500900524     C                   UPDATE    CNCB4SF
061600900524     C                   END
061700900524     C   90              GOTO      FICTR2
061800900524      ******************************
061900900524      * IMPORTO ASSEGNO OBBLIGATORIO
062000900524      *
062100900524     C     VIDIMA        IFEQ      0
062200900524     C                   SETON                                        45  90
062300900524     C                   UPDATE    CNCB4SF
062400900524     C                   END
062500991122     C   90              GOTO      FICTR2
062600991122     C* Controllo congruenza decimali con divisa
062700991122     C                   MOVE      VIDIMA        WCOMO2            2 0
062800010927     C                   MOVE      VIDIMA        WCOMO1            1 0
062900011003     C     DEC(XY)       IFEQ      1
063000011003     C     WCOMO1        ANDNE     0
063100011003     C     DEC(XY)       OREQ      0
063200011003     C     WCOMO2        ANDNE     0
063300991122     C                   SETON                                        51  90
063400991122     C                   UPDATE    CNCB4SF
063500991122     C                   END
063600991122     C   90              GOTO      FICTR2
063700991122     C     VIDTIP        IFNE      'M'
063800011016     C     V2CDIV        ANDNE     §GEDCR
063900991122     C     V2CDIV        ANDNE     DIVEUR
064000991122     C                   SETON                                        52  90
064100991122     C                   UPDATE    CNCB4SF
064200991122     C                   END
064300900524     C   90              GOTO      FICTR2
064400900524      *************************
064500900524      * RIFERIMENTI OBBLIGATORI
064600900524      *
064700900606     C     VIDRIF        IFEQ      0
064800900524     C                   SETON                                        46  90
064900900524     C                   UPDATE    CNCB4SF
065000900524     C                   END
065100900524     C   90              GOTO      FICTR2
065200900524      ****************************************
065300900524      * SE TIPO ASSEGNO E' MITTENTE
065400900524      * ABI, CAB E N° ASSEGNO SONO OBBLIGATORI
065500900524      *
065600900524     C     VIDTIP        IFEQ      'M'
065700900524     C     VIDABI        IFEQ      0
065800900524     C     VIDCAB        OREQ      0
065900900524     C     VIDNRA        OREQ      0
066000900524     C                   SETON                                        47  90
066100900524     C                   UPDATE    CNCB4SF
066200900524     C                   END
066300900524     C                   END
066301060626     C   90              GOTO      FICTR2
066302060626     c* congruenza rif. assegno
066303060626     C                   if        (vidnra<>0 and (vidabi=0 or vidcab=0)) or
066304060626     C                             (vidabi<>0 and (vidnra=0 or vidcab=0)) or
066305060626     C                             (vidcab<>0 and (vidnra=0 or vidabi=0))
066306060626     C                   SETON                                        47  90
066307060626     C                   UPDATE    CNCB4SF
066308060626     C                   END
066400900524     C   90              GOTO      FICTR2
066500900524      *********************************************
066600900524      * ANNO SPEDIZIONE DEVE ESSERE 0 O MINORE DI 1
066700900524      * DI UYEAR
066800900524      *
066900991124     C     VIDAAS        IFNE      WANNO
067000991124     C     WANNO         SUB       VIDAAS        DELTA             5 0
067100900524     C     DELTA         IFNE      1
067200900524     C                   SETON                                        48  90
067300900524     C                   UPDATE    CNCB4SF
067400900524     C                   END
067500900524     C                   END
067600900524     C   90              GOTO      FICTR2
067700900524      **************************
067800900524      * CONTROLLO CODICE ABI/CAB
067900900524      *
068000900614     C                   MOVEL     *BLANKS       §DESBA
068100900524     C     VIDTIP        IFEQ      'M'
068200900524     C     KABI          CHAIN     CNABI01L                           49
068300900614     C  N49              MOVEL     ABIIST        §DESBA           25
068400900524     C   49              SETON                                        90
068500900524     C   49              UPDATE    CNCB4SF
068600900524     C                   END
068700900524     C   90              GOTO      FICTR2
068800900524      *********************************************
068900010927
069000010927     ***********************************************************************
069100010927     **
069200010927     ** Controllo congruenza tra tipo pagamento e divisa.
069300010927     **
069400010927     ***********************************************************************
069500010927     C                   SELECT
069600010928     C* Assegno bancario intestato a noi.
069700010927     C                   WHEN      VidNrA <> 0
069800010927     C                             AND
069900010927     C                             VidTip = *BLANK
070000010928     C                   EVAL      PrmTpInc = 'B'
070100011002     C                   EVAL      Wrk1ADes = 'assegno'
070200010928     C* Assegno bancario intestato al mittente.
070300010927     C                   WHEN      VidNrA <> 0
070400010927     C                             AND
070500010927     C                             VidTip = 'M'
070600010928     C                   EVAL      PrmTpInc = 'BM'
070700011002     C                   EVAL      Wrk1ADes = 'assegno'
070800010928     C* Contanti.
070900010928     C                   OTHER
071000010928     C                   EVAL      PrmTpInc = 'SC'
071100011002     C                   EVAL      Wrk1ADes = 'contanti'
071200010927     C                   ENDSL
071300010928     C*
071400010928     C     *EUR          MOVE      VidDta        PrmData
071500010927     C*
071600010928     C                   CALL      'X1ACW'
071700010928     C                   PARM                    PrmTpInc
071800010928     C                   PARM      V1CDiv        PrmDivisa
071900010928     C                   PARM                    PrmData
072000010928     C                   PARM      *BLANK        PrmEsito
072100011002     C     DS1A          PARM      *BLANK        PrmDS1A
072200011002     C     DSCW          PARM      *BLANK        PrmDSCW
072300010928     C*
072400011002     C                   IF        PrmEsito <> '0'
072500010928     C                   EVAL      CPF9897 =
072600010928     C                             'Il tipo incasso '
072700010928     C                             +
072800011002     C                             %TRIM(Wrk1ADes)
072900010928     C                             +
073000010928     C                             ' con la divisa '
073100010928     C                             +
073200010928     C                             V1CDiv
073300010928     C                             +
073400010928     C                             ' non è ammesso dopo il '
073500010928     C                             +
073600010928     C                             %EDITW(§CwDlc:'    -  -  ')
073700010928     C                             +
073800010928     C                             '.'
073900010928     C                   EVAL      *IN38 = *ON
074000010927     C                   EVAL      *IN90 = *ON
074100010927     C                   UPDATE    CNCB4SF
074200010927     C                   GOTO      FiCtr2
074300010927     C                   ENDIF
074400010927     ***********************************************************************
074500040107
074600040107      ****************************************
074700040107      * SE NON SPECIFICATO NUMERO ASSEGNO E DIVISA
074800040107      * EURO ACCETTO CONANTE SOLO SE FLAG ABILITATO
074900040107      *
075000040107     C     VIDTIP        IFNE      'M'
075100040107     C     V2CDIV        ANDEQ     'EUR'
075200040107     C     §1AEUR        ANDNE     'S'
075300040107     C     VIDNRA        IFEQ      0
075400040107     C     VIDABI        OREQ      0
075500040107     C     VIDCAB        OREQ      0
075600040107     C                   SETON                                        5490
075700040107     C                   UPDATE    CNCB4SF
075800040107     C                   GOTO      FICTR2
075900040107     C                   END
076000040107     C                   END
076100010927
076200900524     C                   END
076300900524      *-------------
076400900524     C                   UPDATE    CNCB4SF
076500900524     C                   END
076600900524      *--------------------
076700900516      *
076800900515     C     FICTR2        ENDSR
076900900515      *-----------------------------------------------------***********
077000900607      * AGGIORNAMENTO DISTINTA                              *  AGGIOR *
077100900515      *-----------------------------------------------------***********
077200900607     C     AGGIOR        BEGSR
077300900515      *
077400900607     C                   Z-ADD     0             VIDIMP
077500900607     C                   Z-ADD     0             VIDTAS
077600900607     C                   Z-ADD     0             VIDTAM
077700900525     C                   Z-ADD     0             VIDTDS
077800900525      *
077900900524     C                   Z-ADD     1             NRR
078000900524      *--------------------
078100900524     C                   DO        100           NRR
078200900524     C     NRR           CHAIN     CNCB4SF                            82
078300900524      *-------------
078400900524     C     VIDLNP        IFNE      0
078500900524      *---------
078600900524     C     VIDRRN        IFNE      0
078700991122     C     VIDRRN        CHAIN     FNBCO00F                           83
078800900524      *----
078900900524     C     VIDANN        IFEQ      'A'
079000991122     C                   DELETE    FNBC0
079100900524     C                   ELSE
079200900524     C                   EXSR      MUOVI
079300991122     C                   UPDATE    FNBC0
079400900524     C                   END
079500900524      *----
079600900524     C                   ELSE
079700900607     C     VIDANN        IFNE      'A'
079800900524     C                   EXSR      MUOVI
079900991122     C                   WRITE     FNBC1
080000900607     C                   END
080100900524     C                   END
080200900524      *---------
080300900524     C                   END
080400900524      *-------------
080500900524     C                   END
080600900524      *--------------------
080700900515      *
080800900515     C                   ENDSR
080900900524      *-----------------------------------------------------***********
081000900524      * STAMPA                                              *  STAMPA *
081100900524      *-----------------------------------------------------***********
081200900524     C     STAMPA        BEGSR
081300900524      *
081400900607     C                   EXCEPT    TESTA
081500900607      *
081600900607     C                   Z-ADD     1             NRR
081700900607      *--------------------
081800900607     C                   DO        100           NRR
081900900607     C     NRR           CHAIN     CNCB4SF                            82
082000900614     C   OF              EXCEPT    TESTA
082100900607      *-------------
082200900607     C     VIDLNP        IFNE      0
082300900607      *---------
082400900607     C     VIDANN        IFNE      'A'
082500900607     C                   EXCEPT    RIGA
082600900607     C                   END
082700900607      *---------
082800900607     C                   END
082900900607      *-------------
083000900607     C                   END
083100900607      *--------------------
083200900607     C   OF              EXCEPT    TESTA
083300900607     C                   EXCEPT    TOTALE
083400900524      *
083500900524     C                   ENDSR
083600900515      *-----------------------------------------------------***********
083700900524      * COPIA CAMPI RECORD IN CAMPI SFL                     *  RIEMPI *
083800900515      *-----------------------------------------------------***********
083900900515     C     RIEMPI        BEGSR
084000000222      *
084100000222      * IMPOSTO AD IMMISSIONE IL FLAG TIPO VISITA
084200000222     C                   MOVEL     'I'           WTPVIS
084300900515      *
084400900515      * RIEMPIO IL SFL
084500900515      *
084600900524     C                   Z-ADD     1             NRR
084700020930     C     K02Bco01      SETLL     FNBCO01L
084800020930     C     K02Bco01      READE     FNBCO01L                               81
084900900524      *--------------------
085000900524     C     *IN81         DOWEQ     '0'
085100900524      *
085200991122     C     BCOVCA        IFEQ      V2CDIV
085300000121     C* CARICO C/ASSEGNO SOLO SE LA LNA E' UGUALE ALLA FILIALE DELLA
085400000121     C* PRIMA VIDEATA O E' IN L6
085500000121     C                   MOVEL     'N'           WCAS              1
085600000121     C     BCOLNA        IFNE      VIDFLE
085700000121     C     W£6           IFEQ      'S'
085800000121     C                   Z-ADD     1             YY
085900000121     C     BCOLNA        LOOKUP    £6(YY)                                 31
086000000121     C     *IN31         IFEQ      '1'
086100020726     C*    F£6(YY)       ANDEQ     VIDFLE
086200000121     C                   MOVEL     'S'           WCAS
086300000121     C                   END
086400000121     C                   END
086500000121     C                   ELSE
086600000121     C                   MOVEL     'S'           WCAS
086700000121     C                   END
086800000121     C     WCAS          IFEQ      'S'
086900991122     C     NRR           CHAIN     CNCB4SF                            82
087000000222     C                   MOVEL     'M'           WTPVIS
087100900524     C                   Z-ADD     RRN           VIDRRN
087200900524      *
087300900524     C                   Z-ADD     BCONDS        VIDNDS
087400991125     C                   Z-ADD     BCONDS        VIDNRE
087500991125     C                   MOVEL     VIDFLE        VIDNRE
087600991122     C                   Z-ADD     BCOLNP        VIDLNP
087700900524     C                   Z-ADD     BCOLNA        VIDLNA
087800900524     C                   Z-ADD     BCOAAS        VIDAAS
087900900524     C                   Z-ADD     BCONRS        VIDNRS
088000900524     C                   Z-ADD     BCONSP        VIDNSP
088100900524      *
088200900524     C     BCODTA        IFNE      0
088300991124     C                   CLEAR                   WLBDAT
088400991124     C                   MOVEL     '3'           G02ERR
088500991124     C                   Z-ADD     BCODTA        G02INV
088600991124     C                   CALL      'XSRDA8'
088700991124     C                   PARM                    WLBDAT
088800991124     C                   MOVE      G02DAT        VIDDTA
088900900524     C                   END
089000900524      *
089100900524     C                   Z-ADD     BCOIMA        VIDIMA
089200900524     C                   MOVEL     BCOTIP        VIDTIP
089300900524     C                   MOVEL     BCOMIT        VIDMIT
089400900606     C                   Z-ADD     BCORIF        VIDRIF
089500900524     C                   Z-ADD     BCOABI        VIDABI
089600900524     C                   Z-ADD     BCOCAB        VIDCAB
089700900524     C                   Z-ADD     BCONRA        VIDNRA
089800900524      *
089900900524     C     BCODDS        IFNE      0
090000991124     C                   CLEAR                   WLBDAT
090100991124     C                   MOVEL     '3'           G02ERR
090200991124     C                   Z-ADD     BCODDS        G02INV
090300991124     C                   CALL      'XSRDA8'
090400991124     C                   PARM                    WLBDAT
090500991124     C                   MOVE      G02DAT        VIDDDS
090600900524     C                   END
090700900524      *
090800900524     C                   UPDATE    CNCB4SF
090900900515     C                   ADD       1             NRR
091000991122     C                   END
091100000121     C                   END
091200900524      *
091300020930     C     K02Bco01      READE     FNBCO01L                               81
091400900524     C                   END
091500900524      *--------------------
091600900524      *
091700900515     C                   ENDSR
091800900524      *-----------------------------------------------------***********
091900900524      * MUOVE CAMPI SFL IN CAMPI RECORD                     *  MUOVI  *
092000900524      *-----------------------------------------------------***********
092100900524     C     MUOVI         BEGSR
092200900524      *
092300900524     C                   Z-ADD     VIDFLE        BCOFLE
092400900524     C                   Z-ADD     VIDNDS        BCONDS
092500900524     C                   Z-ADD     VIDLNP        BCOLNP
092600151218     C                   Z-ADD     dutpou        BCOLNA
092700900524     C                   Z-ADD     VIDAAS        BCOAAS
092800900524     C                   Z-ADD     VIDNRS        BCONRS
092900900524     C                   Z-ADD     VIDNSP        BCONSP
093000900524      *
093100900524     C     VIDDTA        IFNE      0
093200991124     C                   Z-ADD     VIDDTA        G02DAT
093300991124     C                   MOVE      *BLANKS       G02ERR
093400991124     C                   CALL      'XSRDA8'
093500991124     C                   PARM                    WLBDAT
093600991124     C                   MOVE      G02INV        BCODTA
093700900524     C                   ELSE
093800900524     C                   Z-ADD     0             BCODTA
093900900524     C                   END
094000900524      *
094100900524     C                   Z-ADD     VIDIMA        BCOIMA
094200991123     C                   MOVE      V2CDIV        BCOVCA
094300900524     C                   MOVEL     VIDTIP        BCOTIP
094400900524     C                   MOVEL     VIDMIT        BCOMIT
094500900606     C                   Z-ADD     VIDRIF        BCORIF
094600900524     C                   MOVEL     VIDCOD        BCOCOD
094700900524     C                   Z-ADD     VIDABI        BCOABI
094800900524     C                   Z-ADD     VIDCAB        BCOCAB
094900900524     C                   Z-ADD     VIDNRA        BCONRA
095000900524      *
095100900524     C     VIDDDS        IFNE      0
095200991124     C                   Z-ADD     VIDDDS        G02DAT
095300991124     C                   MOVE      *BLANKS       G02ERR
095400991124     C                   CALL      'XSRDA8'
095500991124     C                   PARM                    WLBDAT
095600991124     C                   MOVE      G02INV        BCODDS
095700900524     C                   ELSE
095800900524     C                   Z-ADD     0             BCODDS
095900900524     C                   END
096000900524      *
096100900525     C     VIDTIP        IFEQ      'M'
096200900525     C                   ADD       VIDIMA        VIDTAM
096300900607     C                   ELSE
096400900607     C                   ADD       VIDIMA        VIDTAS
096500900607     C                   ADD       VIDIMA        VIDIMP
096600900525     C                   END
096700900525     C                   ADD       VIDIMA        VIDTDS
096800991124     C                   MOVE      V2CDIV        V3CDIV
096900900525      *
097000900524     C                   ENDSR
097001060403      *-----------------------------------------------------***********
097002060403      * MUOVE CAMPI SFL IN CNASS00F
097003060403      *-----------------------------------------------------***********
097004060403     C     sraggio       BEGSR
097005060404     c                   clear                   yfmdi000
097006060403      * scivo cnass00f
097007060404     C     K02Bco01      SETLL     FNBCO01L
097008060404     C                   do        *hival
097009060404     C     K02Bco01      READE     FNBCO01L
097010060404     c                   if        %eof(fnbco01l)
097011060404     c                   leave
097012060404     c                   end
097016060404     c* se assegno e no mittente
097017060404     c                   if        bconra<>0 and bcoabi<>0 and bcocab<>0
097018060404     c                             and bcotip = ' '
097019060404     c                   add       bcoima        mdiiass
097020060404     c                   clear                   cnass000
097021060404     C                   Z-ADD     bcoFLE        assFLE
097022060403     C                   movel     'C'           asstpd
097023060403     C                   movel     'B'           asstpa
097024060403     C                   movel     'V'           assfve
097025060404     C                   movel     'T'           assftr
097026060404     C                   Z-ADD     bcoLNP        assLNP
097027060404     C                   Z-ADD     bcoAAS        assAAS
097028060404     C                   Z-ADD     bcoNRS        assNRS
097029060404     C                   Z-ADD     bcoNSP        assNSP
097030060404     C                   Z-ADD     bcoIMA        asscas
097031060404     C                   MOVE      bcoVCA        assVCA
097032060404     C                   MOVEL     bcoTIP        assTPI
097033060404     C                   Z-ADD     bcoABI        assABI
097034060404     C                   Z-ADD     bcoCAB        assCAB
097035060404     C                   move      bcoNRA        comNRA            4
097036060404     C                   movel     comNRA        assNRA
097037060524     C                   CLEAR                   WLBDAT
097038060524     C                   MOVEL     '3'           G02ERR
097039060524     C                   Z-ADD     BCODTA        G02INV
097040060524     C                   CALL      'XSRDA8'
097041060524     C                   PARM                    WLBDAT
097042060524     C                   MOVE      G02DAT        comdTA            8 0
097043060524     C                   CLEAR                   WLBDAT
097044060524     C                   Z-ADD     comdta        G02DAT
097046060524     C                   CALL      'XSRDA8'
097047060524     C                   PARM                    WLBDAT
097048060524     C                   MOVE      G02INV        assdte
097050060404     c                   write     cnass000
097051060404     c                   else
097052060524     c                   if        bcotip <>'M'
097053060404     c                   add       bcoima        mdiicon
097054060404     c                   end
097055060524     c                   end
097056060404     c                   enddo
097057060404     C*  PRENDO NR. MANDATO DI INTROITO
097058060524     c                   if        mdiicon <> 0 or mdiiass <> 0
097059060404     c                   do        *hival
097060060404     C                   CLEAR                   TRUL33DS
097061060404     c                   eval      i33tla = 'L'
097062060404     C                   Z-ADD     13            I33CNU
097063060404     C                   Z-ADD     bcofle        I33PO1
097064060404     C                   Z-ADD     1             I33NUM
097065060404     c                   movel(p)  trul33ds      kpjbu
097066060404     c                   call      'TRUL33R'
097067060404     c                   parm                    kpjba
097068060404     c                   movel     kpjbu         trul33ds
097069060404     C                   IF        O33ERR = 0
097070060404     C                   Z-ADD     O33Nrf        mdinmi
097071060404     c                   leave
097072060404     C                   END
097073060404     C                   ENDdo
097074060403     c* YFMDI00F
097075060404     C                   MOVE      bcofle        mdiFLE
097076060403     C                   MOVE      dutcou        mdiCUC
097077060403     C                   MOVE      'Z'           mditmi
097078060403     C                   MOVE      AMGUD8        mdiDMI
097079060403     C                   MOVE      ora           mdiora
097080060404     C                   MOVEL     'S'           mdiFPC
097081060403     C* imposto a blanks flag versamento
097082060403     C                   MOVEL     *BLANKS       mdivIE
097083060403     C                   WRITE     yfmdi000
097084060524     c                   end
097085060403      *
097086060403     C                   ENDSR
097100900525      *-----------------------------------------------------***********
097200900525      * PREPARA CAMPI SECONDA VIDEATA                       *  RIEMP2 *
097300900525      *-----------------------------------------------------***********
097400900525     C     RIEMP2        BEGSR
097500000121      *
097600000303     C                   SETOFF                                       96
097700020729      * CARICO INCASSI D'ATTRIBUIRE DA TAB.y4O
097800020729     C                   MOVEL     'Y4O'         kcode
097900020729     C                   CLEAR                   Kkeye
098000020729     C                   MOVEL     '201'         Kkeye
098100020730     C                   MOVEL     vidfle        Kkeye2
098200020730     C     KTbe2         CHAIN     Tntbe01l                           77
098300020729     C     *IN77         IFEQ      '0'
098400020729     C                   MOVEL     TBeUNI        Dy4O
098500020729     C* SE NON SONO RIUSCITA AD AGGANCIARE LA TABELLA y4O
098600000303     C* SPROTTEGGO IL CODICE DEL SOTTOCONTO PER LASCIARLO
098700000303     C* IMMETTERE A LORO
098800000303     C                   ELSE
098900000303     C                   SETON                                        96
099000000121     C                   END
099100900525      *
099200020729     C                   move      §4OCC1        VIDCC1
099300020729     C                   move      §4OSC1        VIDSC1
099400900525      *
099500020729     C                   move      §4OCC1        CCCWWW
099600981121     C                   Z-ADD     VIDSC1        SCCWWW
099700900525     C     KACO          CHAIN     CNACO00F                           84
099800900525     C                   MOVEL     *BLANKS       DESSC1
099900900606     C  N84              MOVEL     ACORAG        DESSC1
100000900525     C   84              MOVEL     *ALL'?'       DESSC1
100100900525      *
100200900525     C                   Z-ADD     §4ACC2        VIDCC2
100300900525     C                   Z-ADD     §4ASC2        VIDSC2
100400900525      *
100500900525     C                   Z-ADD     §4ACC2        CCCWWW
100600900525     C                   Z-ADD     §4ASC2        SCCWWW
100700900525     C     KACO          CHAIN     CNACO00F                           84
100800900525     C                   MOVEL     *BLANKS       DESSC2
100900900525     C  N84              MOVEL     ACORAG        DESSC2
101000900525     C   84              MOVEL     *ALL'?'       DESSC2
101100900525      *
101200900525     C                   MOVEL     §4ACO1        VIDCA1
101300981217     C                   MOVEL     §4ACO2        VIDCA2
101400910102     C                   Z-ADD     VIDDDS        VIDDRE
101500900525     C                   Z-ADD     VIDNDS        VIDNDC
101600900525     C                   Z-ADD     VIDDDS        VIDDDO
101700900525     C                   Z-ADD     0             VIDDP1
101800900525     C                   Z-ADD     VIDDDS        VIDDP2
101900900525     C                   MOVE      'D'           DARE1
102000900525     C                   MOVE      'A'           AVER1
102100900607     C                   Z-ADD     0             VIDNP1
102200900607     C                   Z-ADD     VIDNDS        VIDNP2
102300900525      *
102400900525     C                   Z-ADD     §4ACC3        VIDCC3
102500900525     C                   Z-ADD     §4ASC3        VIDSC3
102600900525      *
102700900525     C                   Z-ADD     §4ACC3        CCCWWW
102800900525     C                   Z-ADD     §4ASC3        SCCWWW
102900900525     C     KACO          CHAIN     CNACO00F                           84
103000900525     C                   MOVEL     *BLANKS       DESSC3
103100900525     C  N84              MOVEL     ACORAG        DESSC3
103200900525     C   84              MOVEL     *ALL'?'       DESSC3
103300900525      *
103400900525     C                   MOVEL     §4ACO2        VIDCA3
103500900525     C                   MOVEL     §4ACO2        VIDCA4
103600991124     C                   Z-ADD     GMAUD8        VIDDR2
103700900525     C                   Z-ADD     VIDDDS        VIDDD2
103800900525     C                   Z-ADD     VIDDDS        VIDDP3
103900900607     C                   Z-ADD     VIDDDS        VIDDP4
104000900525     C                   MOVE      'D'           DARE2
104100900525     C                   MOVE      'A'           AVER2
104200900525     C                   Z-ADD     VIDNDS        VIDNP3
104300900607     C                   Z-ADD     VIDNDS        VIDNP4
104301060404     C                   Z-ADD     VIDfle        VIDNP1
104302060404     C                   Z-ADD     GMAUD8        VIDDp1
104400900525      *
104500900525     C                   ENDSR
104600900607      *-----------------------------------------------------***********
104700991122      * DELETE RECORDS DI FNBCO00F                          *  DEL00F *
104800900607      *-----------------------------------------------------***********
104900900607     C     DEL00F        BEGSR
105000900607      *
105100020930     C     K02Bco01      SETLL     FNBCO01L
105200900607     C     LEGDEL        TAG
105300020930     C     K02Bco01      READE     FNBCO01L                               80
105400991122     C  N80              DELETE    FNBC1
105500900607     C  N80              GOTO      LEGDEL
105600900607      *
105700900607     C                   ENDSR
105800900525      *-----------------------------------------------------***********
105900900525      * REGISTRAZIONE MOVIMENTO CONTABILE                   *  REGMOV *
106000900525      *-----------------------------------------------------***********
106100900525     C     REGMOV        BEGSR
106200900606?     *--------------------------------------------------------------*
106300900606?     * REGISTRAZIONE 1° RIGA MOVIMENTO                              *
106400991123?    C                   CLEAR                   FNMOV000
106500900606?     *
106600991123     C                   MOVE      KNMUS         MOVNMU
106700991123     C                   MOVE      KNRAZ         MOVRIF
106800991123     C                   MOVEL     VIDFLE        MOVRIF
106900991123     C                   ADD       1             SUBNUM
107000991123     C                   Z-ADD     SUBNUM        MOVSBN
107100991123     C                   ADD       1             SUBRIG            5 0
107200991123     C                   Z-ADD     SUBRIG        MOVSBR
107300991123     C                   MOVEL     'O'           MOVFGE
107400010312     C* FORZO SEMPRE LA BARTOLINI
107500010312     C                   MOVEL     '201'         MOVSOC
107600991123     C                   MOVEL     VIDFLE        MOVFPN
107700991123     C                   MOVEL     'D'           MOVTMO
107800991123     C                   MOVEL     'CG'          MOVCTB
107900991123     C                   Z-ADD     VIDNRE        MOVNRG
108000991123     C                   Z-ADD     VIDDRE        G02DAT
108100991123     C                   MOVE      *ZEROS        G02INV
108200991123     C                   MOVEL     *BLANK        G02ERR
108300991123     C                   CALL      'XSRDA8'
108400991123     C                   PARM                    WLBDAT
108500991123     C                   Z-ADD     G02INV        MOVDRG
108600991123     C                   MOVEL     'COGE'        MOVSRR
108700991123     C                   MOVEL     VIDCA1        MOVCOD
108800991123      *
108900991123     C                   MOVE      *ZEROS        MOVKCC
109000991123     C                   MOVE      *ZEROS        MOVKSC
109100991123     C                   MOVE      VIDCC1        MOVKCC
109200991123     C                   MOVE      VIDSC1        MOVKSC
109300991123     C                   Z-ADD     1             MOVDAR
109400991123     C                   Z-ADD     0             MOVAVE
109500991123     C                   Z-ADD     VIDIMP        MOVIMD
109600010702     C                   MOVEL     V3CDIV        MOVDIV
109700991123     C* ... Se divisa diversa da moneta di conto eseguo il calcolo
109800991123     C*     del controvalore
109900010702     C     V3CDIV        IFNE      §GEDMC
110000991123     C                   CLEAR                   YEURDS
110100991123     C                   MOVEL     V3CDIV        YECDVI
110200991123     C                   Z-ADD     VIDIMP        YECIMI
110300010702     C                   MOVEL     §GEDMC        YECDVO
110400991123     C                   MOVEL     'H'           YECTAR
110500991123     C                   CALL      'YEURCO'
110600991123     C                   PARM                    YEURDS
110700991123     C                   Z-ADD     YECIMO        MOVIMM
110800991124     C                   ELSE
110900991124     C                   Z-ADD     VIDIMP        MOVIMM
111000991123     C                   END
111100991123     C                   Z-ADD     AMGUD8        MOVDRC
111200900606     C                   Z-ADD     VIDNDC        MOVNDC
111300991123     C                   Z-ADD     VIDDDO        G02DAT
111400991123     C                   MOVE      *ZEROS        G02INV
111500991123     C                   MOVEL     *BLANK        G02ERR
111600991123     C                   CALL      'XSRDA8'
111700991123     C                   PARM                    WLBDAT
111800991123     C                   Z-ADD     G02INV        MOVDDC
111900991123     C                   MOVEL     *BLANKS       MOVSRD
112000900607     C                   Z-ADD     VIDNP1        MOVNRP
112100900606      *
112200991123     C                   Z-ADD     VIDDP1        G02DAT
112300991123     C                   MOVE      *ZEROS        G02INV
112400991123     C                   MOVEL     *BLANK        G02ERR
112500991123     C                   CALL      'XSRDA8'
112600991123     C                   PARM                    WLBDAT
112700991123     C                   Z-ADD     G02INV        MOVDRP
112800010702     C                   MOVEL     §GEDMC        MOVDIP
112900991123     C                   Z-ADD     MOVIMM        MOVIPM
113000991123     C                   Z-ADD     MOVIMM        MOVIPD
113100991123     C                   Z-ADD     VIDCC1        CCCWWW
113200991123     C                   Z-ADD     VIDSC1        SCCWWW
113300991123     C     KACO          CHAIN     CNACO00F                           85
113400991123     C                   MOVEA     ACOOPZ        OPZ
113500991123     C                   MOVE      OPZ(7)        HOPZ7             1            OBBL.RAPP.
113600991123     C                   MOVE      OPZ(2)        HOPZ2             1            GEST.SCADE
113700991123      *
113800991123     C                   Z-ADD     VIDCC2        CCCWWW
113900991123     C                   Z-ADD     VIDSC2        SCCWWW
114000991123     C     KACO          CHAIN     CNACO00F                           85
114100991123     C                   MOVEA     ACOOPZ        OPZ
114200991123     C                   MOVE      OPZ(7)        KOPZ7             1            OBBL.RAPP.
114300991123     C                   MOVE      OPZ(2)        KOPZ2             1            GEST.SCADE
114400991123      * SCADENZA
114500991123     C                   Z-ADD     0             MOVDSC
114600991123     C     VIDCC1        IFEQ      KBNA                                         BANCA
114700991123     C     HOPZ2         ANDEQ     '1'
114800991123     C     VIDCC1        OREQ      VIDCC2                                       CONTROPART
114900991123     C     KOPZ2         ANDEQ     '1'
115000991123     C                   Z-ADD     MOVDRG        MOVDSC
115100991123     C                   END
115200900606      *
115300900606     C                   MOVEL     *BLANKS       MOVDES
115400991123     C                   MOVE      *ZEROS        MOVCCP
115500991123     C                   MOVE      *ZEROS        MOVSCP
115600991123     C                   MOVE      VIDCC2        MOVCCP
115700991123     C                   MOVE      VIDSC2        MOVSCP
115800900606      *
115900991123     C                   Z-ADD     0             MOVDCA
116000010702      * IMPOSTO IL CAMBIO IN QUANTO NON POSSO REPERIRLO DALLA
116100010702      * TABELLA CV POICHE' SULLA DIVISA ITL DEVE RESTARE IL
116200010702      * CAMBIO 1.
116300010702      * SE LA  MONETA DI CONTO E' EURO E ....
116400010702     C     §GEDMC        IFEQ      'EUR'
116500010702      * LA REGISTRAZIONE E IN 'ITL' IMPOSTO FISSO 1936,27 COME
116600010702      * CAMBIO ....
116700010702     C     MOVDIV        IFEQ      'ITL'
116800010702     C                   Z-ADD     1936,27       MOVCMB
116900010702      * ... ALTRIMENTI (LA REGISTRAZIONE E' IN EURO) IMPOSTO FISSO
117000010702      *     IL CAMBIO A 1
117100010702     C                   ELSE
117200010702     C                   Z-ADD     1             MOVCMB
117300010702     C                   END
117400010702     C                   END
117500010702      *
117600900606      *
117700991123     C                   WRITE     FNMOV000
117800900606?     *--------------------------------------------------------------*
117900991123?     * REGISTRAZIONE 2° RIGA MOVIMENTO: IMPOSTO SOLO DATI DIVERSI   *
118000900606?     *
118100991123     C                   ADD       1             SUBRIG
118200991123     C                   Z-ADD     SUBRIG        MOVSBR
118300991123     C                   MOVEL     VIDCA2        MOVCOD
118400991123     C                   MOVE      *ZEROS        MOVKCC
118500991123     C                   MOVE      *ZEROS        MOVKSC
118600991123     C                   MOVE      VIDCC2        MOVKCC
118700991123     C                   MOVE      VIDSC2        MOVKSC
118800991123     C                   Z-ADD     0             MOVDAR
118900991123     C                   Z-ADD     1             MOVAVE
119000900606      *
119100900606     C                   Z-ADD     VIDNDS        MOVNRP
119200991123     C                   Z-ADD     VIDDP2        G02DAT
119300991123     C                   MOVE      *ZEROS        G02INV
119400991123     C                   MOVEL     *BLANK        G02ERR
119500991123     C                   CALL      'XSRDA8'
119600991123     C                   PARM                    WLBDAT
119700991123     C                   Z-ADD     G02INV        MOVDRP
119800900607      *
119900991123     C                   MOVEL     V3CDIV        MOVDIP
120000991123     C                   Z-ADD     MOVIMM        MOVIPM
120100991123     C                   Z-ADD     MOVIMD        MOVIPD
120200991123     C                   MOVE      *ZEROS        MOVCCP
120300991123     C                   MOVE      *ZEROS        MOVSCP
120400991123     C                   MOVE      VIDCC1        MOVCCP
120500991123     C                   MOVE      VIDSC1        MOVSCP
120600900606     C                   Z-ADD     0             MOVDSC
120700900606      *
120800900606     C                   Z-ADD     VIDCC2        CCCWWW
120900900606     C                   Z-ADD     VIDSC2        SCCWWW
121000900606     C     KACO          CHAIN     CNACO00F                           85
121100900606     C                   MOVEA     ACOOPZ        OPZ
121200900606     C                   MOVE      OPZ(7)        HOPZ7             1            OBBL.RAPP.
121300900606     C                   MOVE      OPZ(2)        HOPZ2             1            GEST.SCADE
121400900606      *
121500900606     C                   Z-ADD     VIDCC1        CCCWWW
121600900606     C                   Z-ADD     VIDSC1        SCCWWW
121700900606     C     KACO          CHAIN     CNACO00F                           85
121800900606     C                   MOVEA     ACOOPZ        OPZ
121900900606     C                   MOVE      OPZ(7)        KOPZ7             1            OBBL.RAPP.
122000900606     C                   MOVE      OPZ(2)        KOPZ2             1            GEST.SCADE
122100900606      * SCADENZA
122200900606     C     VIDCC2        IFEQ      KBNA                                         BANCA
122300900606     C     HOPZ2         ANDEQ     '1'
122400900606     C     VIDCC2        OREQ      VIDCC1                                       CONTROPART
122500900606     C     KOPZ2         ANDEQ     '1'
122600991123     C                   Z-ADD     MOVDRG        MOVDSC
122700900606     C                   END
122800900606      *
122900991123     C                   WRITE     FNMOV000
123000900606?     *--------------------------------------------------------------*
123100900606?     * REGISTRAZIONE 3° RIGA MOVIMENTO                              *
123200900606?     *
123300991123     C                   MOVE      KNMUS         MOVNMU
123400991123     C                   MOVE      KNRAZ         MOVRIF
123500991123     C                   MOVEL     VIDFLE        MOVRIF
123600991123     C                   ADD       1             SUBNUM
123700991123     C                   Z-ADD     SUBNUM        MOVSBN
123800991123     C                   Z-ADD     1             SUBRIG
123900991123     C                   Z-ADD     SUBRIG        MOVSBR
124000991123     C                   MOVEL     VIDFLE        MOVFPN
124100991123     C                   MOVEL     'D'           MOVTMO
124200991123     C                   MOVEL     'CG'          MOVCTB
124300991123     C                   Z-ADD     VIDNRE        MOVNRG
124400991123     C                   Z-ADD     VIDDR2        G02DAT
124500991123     C                   MOVE      *ZEROS        G02INV
124600991123     C                   MOVEL     *BLANK        G02ERR
124700991123     C                   CALL      'XSRDA8'
124800991123     C                   PARM                    WLBDAT
124900991123     C                   Z-ADD     G02INV        MOVDRG
125000991123     C                   MOVEL     'COGE'        MOVSRR
125100991123     C                   MOVEL     VIDCA3        MOVCOD
125200991123      *
125300991123     C                   MOVE      *ZEROS        MOVKCC
125400991123     C                   MOVE      *ZEROS        MOVKSC
125500991123     C                   MOVE      VIDCC2        MOVKCC
125600991123     C                   MOVE      VIDSC2        MOVKSC
125700991123     C                   Z-ADD     1             MOVDAR
125800991123     C                   Z-ADD     0             MOVAVE
125900991123     C                   Z-ADD     VIDIMP        MOVIMD
126000991123     C                   MOVEL     V3CDIV        MOVDIV
126100991123     C* ... Se divisa diversa da moneta di conto eseguo il calcolo
126200991123     C*     del controvalore
126300010702     C     V1CDIV        IFNE      §GEDMC
126400991123     C                   CLEAR                   YEURDS
126500991123     C                   MOVEL     V3CDIV        YECDVI
126600991123     C                   Z-ADD     VIDIMP        YECIMI
126700010702     C                   MOVEL     §GEDMC        YECDVO
126800991123     C                   MOVEL     'H'           YECTAR
126900991123     C                   CALL      'YEURCO'
127000991123     C                   PARM                    YEURDS
127100991123     C                   Z-ADD     YECIMO        MOVIMM
127200991124     C                   ELSE
127300991124     C                   Z-ADD     VIDIMP        MOVIMM
127400991123     C                   END
127500991123     C                   Z-ADD     AMGUD8        MOVDRC
127600991123     C                   Z-ADD     VIDNDC        MOVNDC
127700991123     C                   Z-ADD     VIDDD2        G02DAT
127800991123     C                   MOVE      *ZEROS        G02INV
127900991123     C                   MOVEL     *BLANK        G02ERR
128000991123     C                   CALL      'XSRDA8'
128100991123     C                   PARM                    WLBDAT
128200991123     C                   Z-ADD     G02INV        MOVDDC
128300991123     C                   MOVEL     *BLANKS       MOVSRD
128400991123     C                   Z-ADD     VIDNP3        MOVNRP
128500991123      *
128600991123     C                   Z-ADD     VIDDP3        G02DAT
128700991123     C                   MOVE      *ZEROS        G02INV
128800991123     C                   MOVEL     *BLANK        G02ERR
128900991123     C                   CALL      'XSRDA8'
129000991123     C                   PARM                    WLBDAT
129100991123     C                   Z-ADD     G02INV        MOVDRP
129200991123     C                   MOVEL     V3CDIV        MOVDIP
129300991123     C                   Z-ADD     MOVIMM        MOVIPM
129400991123     C                   Z-ADD     MOVIMD        MOVIPD
129500991123     C                   Z-ADD     0             MOVDSC
129600991123      *
129700991123     C                   Z-ADD     VIDCC2        CCCWWW
129800991123     C                   Z-ADD     VIDSC2        SCCWWW
129900991123     C     KACO          CHAIN     CNACO00F                           85
130000991123     C                   MOVEA     ACOOPZ        OPZ
130100991123     C                   MOVE      OPZ(7)        HOPZ7             1            OBBL.RAPP.
130200991123     C                   MOVE      OPZ(2)        HOPZ2             1            GEST.SCADE
130300991123      *
130400991123     C                   Z-ADD     VIDCC3        CCCWWW
130500991123     C                   Z-ADD     VIDSC3        SCCWWW
130600991123     C     KACO          CHAIN     CNACO00F                           85
130700991123     C                   MOVEA     ACOOPZ        OPZ
130800991123     C                   MOVE      OPZ(7)        KOPZ7             1            OBBL.RAPP.
130900991123     C                   MOVE      OPZ(2)        KOPZ2             1            GEST.SCADE
131000991123      * SCADENZA
131100991123     C     VIDCC2        IFEQ      KBNA                                         BANCA
131200991123     C     HOPZ2         ANDEQ     '1'
131300991123     C     VIDCC2        OREQ      VIDCC3                                       CONTROPART
131400991123     C     KOPZ2         ANDEQ     '1'
131500991123     C                   Z-ADD     MOVDRG        MOVDSC
131600991123     C                   END
131700991123     C                   MOVEL     *BLANKS       MOVDES
131800991123     C                   MOVE      *ZEROS        MOVCCP
131900991123     C                   MOVE      *ZEROS        MOVSCP
132000991123     C                   MOVE      VIDCC3        MOVCCP
132100991123     C                   MOVE      VIDSC3        MOVSCP
132200010702      * IMPOSTO IL CAMBIO IN QUANTO NON POSSO REPERIRLO DALLA
132300010702      * TABELLA CV POICHE' SULLA DIVISA ITL DEVE RESTARE IL
132400010702      * CAMBIO 1.
132500010702      * SE LA  MONETA DI CONTO E' EURO E ....
132600010702     C     §GEDMC        IFEQ      'EUR'
132700010702      * LA REGISTRAZIONE E IN 'ITL' IMPOSTO FISSO 1936,27 COME
132800010702      * CAMBIO ....
132900010702     C     MOVDIV        IFEQ      'ITL'
133000010702     C                   Z-ADD     1936,27       MOVCMB
133100010702      * ... ALTRIMENTI (LA REGISTRAZIONE E' IN EURO) IMPOSTO FISSO
133200010702      *     IL CAMBIO A 1
133300010702     C                   ELSE
133400010702     C                   Z-ADD     1             MOVCMB
133500010702     C                   END
133600010702     C                   END
133700010702      *
133800991123      *
133900991123     C                   WRITE     FNMOV000
134000900606?     *--------------------------------------------------------------*
134100900606?     * REGISTRAZIONE 4° RIGA MOVIMENTO                              *
134200900606?     *
134300991123     C                   ADD       1             SUBRIG
134400991123     C                   Z-ADD     SUBRIG        MOVSBR
134500991123     C                   MOVEL     VIDCA4        MOVCOD
134600991123     C                   MOVE      *ZEROS        MOVKCC
134700991123     C                   MOVE      *ZEROS        MOVKSC
134800991123     C                   MOVE      VIDCC3        MOVKCC
134900991123     C                   MOVE      VIDSC3        MOVKSC
135000991123     C                   Z-ADD     0             MOVDAR
135100991123     C                   Z-ADD     1             MOVAVE
135200900606     C                   Z-ADD     VIDNDC        MOVNDC
135300991123     C                   Z-ADD     VIDNP4        MOVNRP
135400991123     C                   Z-ADD     VIDDP4        G02DAT
135500991123     C                   MOVE      *ZEROS        G02INV
135600991123     C                   MOVEL     *BLANK        G02ERR
135700991123     C                   CALL      'XSRDA8'
135800991123     C                   PARM                    WLBDAT
135900991123     C                   Z-ADD     G02INV        MOVDRP
136000991124     C                   MOVEL     V3CDIV        MOVDIP
136100991123     C                   Z-ADD     MOVIMM        MOVIPM
136200991123     C                   Z-ADD     MOVIMD        MOVIPD
136300991123      *
136400991123     C                   Z-ADD     VIDCC3        CCCWWW
136500991123     C                   Z-ADD     VIDSC3        SCCWWW
136600991123     C     KACO          CHAIN     CNACO00F                           85
136700991123     C                   MOVEA     ACOOPZ        OPZ
136800991123     C                   MOVE      OPZ(7)        HOPZ7             1            OBBL.RAPP.
136900991123     C                   MOVE      OPZ(2)        HOPZ2             1            GEST.SCADE
137000991123      *
137100991123     C                   Z-ADD     VIDCC2        CCCWWW
137200991123     C                   Z-ADD     VIDSC2        SCCWWW
137300991123     C     KACO          CHAIN     CNACO00F                           85
137400991123     C                   MOVEA     ACOOPZ        OPZ
137500991123     C                   MOVE      OPZ(7)        KOPZ7             1            OBBL.RAPP.
137600991123     C                   MOVE      OPZ(2)        KOPZ2             1            GEST.SCADE
137700991123      * SCADENZA
137800991123     C     VIDCC3        IFEQ      KBNA                                         BANCA
137900991123     C     HOPZ2         ANDEQ     '1'
138000991123     C     VIDCC3        OREQ      VIDCC2                                       CONTROPART
138100991123     C     KOPZ2         ANDEQ     '1'
138200991123     C                   Z-ADD     MOVDRG        MOVDSC
138300991123     C                   END
138400900606      *
138500900606     C                   MOVEL     *BLANKS       MOVDES
138600991123     C                   MOVE      *ZEROS        MOVCCP
138700991123     C                   MOVE      *ZEROS        MOVSCP
138800991123     C                   MOVE      VIDCC2        MOVCCP
138900991123     C                   MOVE      VIDSC2        MOVSCP
139000900606      *
139100991123     C                   WRITE     FNMOV000
139200900525      *
139300900525     C                   ENDSR
139400991123      *-----------------------------------------------------***********
139500991123      * TRASMISSIONE  MOVIMENTI CONTABILI                   *  REGMOV *
139600991123      *-----------------------------------------------------***********
139700991123     C     TRAMOV        BEGSR
139800991123     C*
139900991123     C* GUARDO SE IL MEMBRO è VUOTO - FNMOV00T
140000991124     C                   MOVEL     O50PLA        LIBR             10
140100991123     C                   MOVEL     *BLANK        FILE             10
140200991123     C                   MOVEL     *BLANK        MBR              10
140300991123     C                   Z-ADD     0             NRRM             10 0
140400991123     C                   MOVEL     'FNMOV00T'    FILE
140500020930     C                   EVAL      Mbr = 'M' + %EDITC(VidFle:'X') + '046'
140600991123     C                   CALL      'TRUL51C'
140700991123     C                   PARM                    LIBR
140800991123     C                   PARM                    FILE
140900991123     C                   PARM                    MBR
141000991123     C                   PARM                    NRRM
141100991123     C     NRRM          IFNE      *ALL'9'
141200991122      *-----------------------------------------------------*
141300991123     C                   MOVEL     '046'         CMZDST                          DEST.:SED
141400991123     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
141500020930     C                   EVAL      CmzMbD = 'M' + %EDITC(VidFle:'X') + CmzDst
141600991123     C                   MOVEL     O50PLA        CMZLBD
141700991123     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
141800020930     C                   MOVEL     CmzMbD        CMZMBA
141900010312     C                   MOVEL     O50ALA        CMZLBA
142000021001     C                   MOVEL     'GAITRA'      CMZLBA
142100021001     C                   MOVE      O50ALA        WQUATR            4
142200021001     C     WQUATR        IFEQ      '101 '
142300021001     C                   MOVE      '201 '        CMZLBA
142400021001     C                   ENDIF
142500991123     C                   MOVE      '0000'        CMZORI                          ORA
142600991123     C                   MOVE      '0000000'     CMZDTI                          DATA
142700991123     C                   MOVE      'N'           CMZURG                          NOTTE
142800991123     C                   MOVE      'A'           CMZADD                          IN ADD
142900991123     C                   MOVE      'S'           CMZCLR                          CLEAR
143000991123     C                   MOVE      'S'           CMZFTR                          DA TRANS.
143100991123     C                   MOVE      *BLANK        CMZERR                          1= ERR.
143200991123     C                   MOVE      'Y029'        CMZAZN                          AZIONE
143300010312     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
143400021001     C                   MOVEL     'GAITRA'      CMZSYF
143500021001     C                   MOVE      O50ASI        WQUATR
143600021001     C     WQUATR        IFEQ      '101 '
143700021001     C                   MOVE      '201 '        CMZSYF
143800021001     C                   ENDIF
143900991123     C                   MOVE      NRRM          CMZNRR                          N.REC.
144000991123     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
144100991123     C                   EXSR      CALCMZ
144200991123     C                   END
144300991123     C*
144400991123     C                   ENDSR
144500991123      *----------------------------------------------------------------
144600991123     C**----------------------------------
144700991123     C** CALL AL PROGRAMMA CHE CREA FILE DI TRASMISSION
144800991123     C**----------------------------------
144900991123     C     CALCMZ        BEGSR
145000991123     C*
145100991123     C                   CLEAR                   W236
145200020930     C                   CALL      'TIBS53C'
145300991123     C                   PARM                    O50LCL
145400991123     C                   PARM                    CMZDST
145500991123     C                   PARM                    CMZFLD
145600991123     C                   PARM                    CMZMBD
145700991123     C                   PARM                    CMZLBD
145800991123     C                   PARM                    CMZFLA
145900991123     C                   PARM                    CMZMBA
146000991123     C                   PARM                    CMZLBA
146100991123     C                   PARM                    CMZORI
146200991123     C                   PARM                    CMZDTI
146300991123     C                   PARM                    CMZURG
146400991123     C                   PARM                    CMZADD
146500991123     C                   PARM                    CMZCLR
146600991123     C                   PARM                    CMZFTR
146700991123     C                   PARM                    CMZERR
146800991123     C                   PARM                    CMZAZN
146900991123     C                   PARM                    CMZSYF
147000991123     C                   PARM                    CMZNRR
147100991123     C                   PARM                    CMZPRR
147200991123     C                   PARM                    W236            236
147300991123     C*
147400991123     C                   ENDSR
147500991124     C**----------------------------------
147600991124     C** LIBERO FNMOV
147700991124     C**----------------------------------
147800991124     C     LIBMOV        BEGSR
147900991124     C* Se eseguita Open ed ovrdbf x FNMOV eseguo dltovr e close
148000991124     C     WOPMOV        IFEQ      'S'
148100991124     C* Eseguo dltovr
148200991124     C                   MOVEA     CMDM(5)       CMDX1
148300020930     C                   Z-ADD     49            LUNCMD           15 5
148400991124     C                   SETOFF                                       91
148500991124     C                   CALL      'QCMDEXC'                            91
148600991124     C                   PARM                    CMDX1
148700991124     C                   PARM                    LUNCMD
148800991124     C*
148900991124     C                   MOVEL     *BLANKS       WOPMOV
149000991124     C                   END
149100991124     C* Se eseguita Alcobj su FNmov00t eseguo Dlc
149200991124     C     WACMOV        IFEQ      'S'
149300991124     C* Eseguo dltovr
149400020930     C                   MOVE      '046))'       VAR5
149500991124     C                   MOVEL     VIDFLE        VAR5
149600991124     C                   MOVE      VAR5          CMDM(6)
149700991124     C                   MOVEA     CMDM(6)       CMDX1
149800020930     C                   Z-ADD     49            LUNCMD           15 5
149900991124     C                   SETOFF                                       91
150000991124     C                   CALL      'QCMDEXC'                            91
150100991124     C                   PARM                    CMDX1
150200991124     C                   PARM                    LUNCMD
150300991124     C*
150400991124     C                   MOVEL     *BLANKS       WACMOV
150500991124     C                   END
150600991124     C*
150700991124     C                   ENDSR
150800991123      *-----------------------------------------------------*
150900991122      * OPERAZIONI INIZIALI                                 *
151000991122      *-----------------------------------------------------*
151100991122     C     *INZSR        BEGSR
151200991122     C*
151300991122     C     *ENTRY        PLIST
151400991122     C                   PARM                    KPJBA
151500991122     C                   TIME                    TIME              6 0
151600991122     C                   Z-ADD     1             CODUT
151700991122     C                   CALL      'X§PARUT'
151800991122     C                   PARM                    UT§DSE
151900991122     C     CODUT         CABEQ     -1            FINE
152000991122     C                   MOVEL     RAGUT         RSUT             20
152100991122     C                   MOVEL     RAGUT         RCONT            17
152200991122     C                   MOVEL     RAGUT         RSUT2            25
152300991122     C                   MOVEL     RAGUT         PARDUT           30
152400050112      *
152500050112     c     *dtaara       define    §azute        azuteds
152600050112     c     *dtaara       define    §datiute      ddatiute
152700050112     C                   in(E)     *dtaara
152800050112     C                   IF        %Error  or  RSUT = *blanks
152900050112     C                   call      'TIBS34R'
153000050112     C                   parm                    Tibs34Ds
153100050112     C                   in        *dtaara
153200050112     c                   ENDIF
153300991122      *  RICERCA CAPOCONTI
153400991122     C                   DO        50            X                 2 0
153500991122     C                   MOVE      TCU(X)        TCUDS
153600991122     C     F4            COMP      '6'                                    27
153700991122     C   27              Z-ADD     KCU(X)        KBNA              4 0
153800991122     C     END1          TAG
153900991122     C                   END
154000000121      * CARICO £6
154100000121     C                   Z-ADD     0             Z                 3 0
154200000121     C                   Z-ADD     0             YY                3 0
154300020726     c                   clear                   trul06ds
154400020726     C                   MOVEL     '£6'          D06COD
154500020726     c                   movel     'S'           d06esc
154600050112     C                   MOVEL     dutpou        D06KEY
154700020726     C                   MOVEL     TRul06ds      KPJBU
154800020726     C                   CALL      'TRUL06R'
154900020726     C                   PARM                    KPJBA
155000020726     C                   MOVEL     KPJBU         TRul06ds
155100020726     C                   MOVEA     LIN           £6
155200000121      * CARICO INCASSI D'ATTRIBUIRE DA TAB. 4O
155300020729     C                   MOVEL     'Y4O'         kcode
155400020729     C                   CLEAR                   Kkeye
155500020729     C                   MOVEL     '201'         Kkeye
155600050112     C                   MOVEL     dutpou        Kkeye2
155700020730     C     KTbe2         CHAIN     Tntbe01l                           77
155800000121     C     *IN77         IFEQ      '0'
155900020729     C                   MOVEL     TBeUNI        Dy4O
156000000121     C                   END
156100991122     C*§ APRO FILE DI STAMPA
156200991122     C*
156300991122     C                   MOVEA     CMDP          CMDPX            60
156400991122     C                   Z-ADD     60            LUNCMD           15 5
156500991122     C                   CALL      'QCMDEXC'                            91
156600991122     C                   PARM                    CMDPX
156700991122     C                   PARM                    LUNCMD
156800991122     C                   OPEN      QSYSPRT
156900991122      *
157000991122     C                   MOVEL     *ALL'-'       TRATT           132
157100991122     C                   Z-ADD     0             NRR               3 0
157200991122      *---------------------------------------------------------------*
157300991122      * CHIAVI                                                        *
157400991122      *---------------------------------------------------------------*
157500991122     C     KACO          KLIST
157600991122     C                   KFLD                    CODUT
157700991122     C                   KFLD                    CCCWWW            4 0
157800991122     C                   KFLD                    SCCWWW            7 0
157900991122      *
158000060403     C     KArb          KLIST
158100060403     C                   KFLD                    vidaas
158101060403     C                   KFLD                    vidlnp
158102060403     C                   KFLD                    vidnrs
158103060403     C                   KFLD                    vidnsp
158104060403      *
158105060403     C     KAss          KLIST
158106060403     C                   KFLD                    vidfle
158108060403     C                   KFLD                    vidlnp
158109060403     C                   KFLD                    vidaas
158110060403     C                   KFLD                    vidnrs
158111060403     C                   KFLD                    vidnsp
158112060403      *
158113060403     C     KAZO          KLIST
158114060403     C                   KFLD                    CODFIL            3 0
158200991122      *
158300991122     C     KNUF          KLIST
158400991122     C                   KFLD                    AAA               2 0
158500991122     C                   KFLD                    CNU               3 0
158600991122     C                   KFLD                    FIL               3 0
158700991122     C                   KFLD                    NUFFL2
158800991122      *
158900991122     C     KTAB          KLIST
159000991122     C                   KFLD                    CODUT
159100991122     C                   KFLD                    CODTAB            2
159200991122     C                   KFLD                    KEYTAB            8
159300991122     C     KTAB1         KLIST
159400991122     C                   KFLD                    CODUT
159500991122     C                   KFLD                    CODTAB            2
159600991122     C     KTBE          KLIST
159700991122     C                   KFLD                    KCODE
159800991122     C                   KFLD                    KKEYE
159900991122      *
160000020730     C     KTBE2         KLIST
160100020730     C                   KFLD                    KCODE
160200020802     C                   KFLD                    KKEYE
160300020730     C                   KFLD                    KKEYE2
160400020730      *
160500991122     C     KABI          KLIST
160600991122     C                   KFLD                    VIDABI
160700991122     C                   KFLD                    VIDCAB
160800020930      *
160900020930     C     K02Bco01      KLIST
161000020930     C                   KFLD                    VidFle
161100020930     C                   KFLD                    VidCod
161200991122     C*----------------------------------------------------*
161300991122     C     *LIKE         DEFINE    TBECOD        KCODE
161400991122     C     *LIKE         DEFINE    TBEKE1        KKEYE
161500020730     C     *LIKE         DEFINE    TBEKE2        KKEYE2
161600991122     C*----------------------------------------------------*
161700991122     C                   Z-ADD     0             SUBNUM            9 0
161800991122     C                   Z-ADD     0             NUMPRG            9 0
161900991122     C*  Reperisco la data del giorno
162000991122     C                   TIME                    WHHDAT           14 0
162100991123     C                   MOVE      WHHDAT        G02DAT
162200991124     C                   MOVE      WHHDAT        WANNO             4 0
162201060403     C                   MOVEL     WHHdat        ORA               6 0
162300991122     C                   MOVE      *ZEROS        G02INV
162400991122     C                   MOVEL     *BLANK        G02ERR
162500991122     C                   CALL      'XSRDA8'
162600991123     C                   PARM                    WLBDAT
162700991122     C                   Z-ADD     G02INV        AMGUD8            8 0
162800991122     C                   Z-ADD     G02DAT        GMAUD8            8 0
162900991122     C                   MOVEL     G02DAT        OGGI              6 0
163000991122     C*----------------------------------------------------*
163100991122     C*  Aggancio tabella 'GED' x reperire la divisa corrente
163200991122     C                   MOVEL     'GED'         KCODE
163300991122     C                   MOVEL     *BLANKS       KKEYE
163400991122     C                   MOVEL     '1'           KKEYE
163500991122     C     KTBE          CHAIN     TNTBE01L                           13
163600991122     C     *IN13         IFEQ      '0'
163700991122     C                   MOVEL     TBEUNI        DGED
163800991122     C                   END
163900991122     C*
164000991122     C*  CARICO DATI TABELLA CV
164100991122     C                   Z-ADD     0             X
164200991122     C                   Z-ADD     1             CODUT
164300991122     C                   MOVEL     *BLANKS       DIVEUR
164400991123     C                   MOVEL     'CV'          CODTAB
164500991122     C     KTAB1         CHAIN     TABEL00F                           13
164600991122     C     *IN13         DOWEQ     '0'
164700991122     C                   ADD       1             X
164800991122     C                   MOVEL     TBLKEY        DIVCV             3
164900991122     C                   MOVEL     TBLKEY        CCV(X)
165000991122     C                   MOVEL     TBLUNI        DSCV
165100991122     C                   MOVEL     §CVDEC        DEC(X)
165200011220     C**                 MOVEL     §CVCMB        CMB(X)
165300010702     C* Se divisa diversa dalla moneta CORRENTE reperisco
165400010702     C* LA Divisa ALTERNATIVA da utilizzare x incassi
165500011016     C     §GEDCR        IFNE      DIVCV
165600991122     C     DIVEUR        ANDEQ     *BLANKS
165700991122     C     §CVDVA        ANDGE     'S'
165800991122     C                   MOVEL     DIVCV         DIVEUR            3
165900991122     C                   END
166000991122     C     KTAB1         READE     TABEL00F                               13
166100991122     C                   END
166200991122     C*  Se non ho impostato divisa alternativa ci sbatto
166300991122     C*  la divisa corrente
166400991122     C     DIVEUR        IFEQ      *BLANKS
166500011016     C                   MOVEL     §GEDCR        DIVEUR
166600991122     C                   END
166700991124     C*
166800991124     C                   MOVEL     '1A'          CODTAB
166900991124     C                   MOVEL     *BLANKS       KEYTAB
167000991124     C                   MOVEL     'SC'          KEYTAB
167100991124     C     KTAB          CHAIN     TABEL00F                           77
167200991124     C  N77              MOVEL     TBLUNI        DS1A
167300991122     C*
167400020930     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
167500991124     C* da/a cui spedire
167600020930     C                   CLEAR                   TIBS55
167700991124     C                   MOVEL     'L'           I50TLA
167800010312     C                   MOVEL     KNSIF         I50PSI
167900050112     C                   MOVEL     dutpou        I50PPO
168000991124     C                   MOVEL     '046'         I50APO
168100020930     C                   CALL      'TIBS55R'
168200020930     C                   PARM                    TIBS55
168300991124     C*
168400991122     C                   ENDSR
168500900607      *---------------------------------------------------------------*
168600900607     OQSYSPRT   E            TESTA            01
168700900607     O                       RSUT                20
168800900607     O                                           32 '* CNC0B4R *'
168900900607     O                                           70 'INCASSI CONTRASSEGNI PER'
169000971126     O                                         +  1 'CONTO'
169100971126     O                       RCONT             +  1
169200991122     O                       OGGI          Y   +  2
169300900614     O                       TIME              +  2 '  :  :  '
169400900607     O                                          128 'PAG.'
169500900607     O                       PAGE               132
169600900607      *
169700900607     O          E            TESTA            03
169800900607     O                                         +  0 'DISTINTA N°'
169900900607     O                       VIDNDS        Z   +  1
170000900614     O                                         +  2 'DEL'
170100991122     O                       VIDDDS        8   +  2
170200900607     O                       VIDCOD            + 10
170300900607     O                       VIDDES            +  2
170400961024     O                                         +  2 'PUNTO OPERATIVO'
170500900614     O                       VIDDFI            +  2
170600991122     O                                         +  2 'DIVISA:'
170700991123     O                       V1CDIV            +  2
170800900607      *
170900900607     O          E            TESTA       0
171000900607     O                                         +  0 'DISTINTA N°'
171100900607     O                       VIDNDS        Z   +  1
171200900614     O                                         +  2 'DEL'
171300991122     O                       VIDDDS        8   +  2
171400900607     O                       VIDCOD            + 10
171500900607     O                       VIDDES            +  2
171600961024     O                                         +  2 'PUNTO OPERATIVO'
171700900614     O                       VIDDFI            +  2
171800991122     O                                         +  2 'DIVISA:'
171900991123     O                       V1CDIV            +  2
172000900607      *
172100900607     O          E            TESTA            04
172200900607     O                       TRATT             +  0
172300900607      *
172400900607     O          E            TESTA            05
172500900614     O                                         +  0 'Par.'
172600991122     O                                            9 'Arr.'
172700991122     O                                           14 'Anno'
172800991122     O                                           26 'N° Spediz.'
172900991122     O                                           38 'Data assegno'
173000991122     O                                           51 'Importo'
173100991122     O                                           60 'Tipo'
173200900618     O                                         +  2 'Mittente'
173300991122     O                                           92 'Riferim.'
173400991122     O                                         +  1 'N° Assegno'
173500900618     O                                          112 'Banca'
173600900607      *
173700900607     O          E            TESTA            06
173800900607     O                       TRATT             +  0
173900900607      *
174000900607     O          E            RIGA        2
174100991122     O                       VIDLNP        Z      3
174200991122     O                       VIDLNA        Z      8
174300991122     O                       VIDAAS        Z     14
174400991122     O                       VIDNRS        Z     18
174500991122     O                       VIDNSP        Z     26
174600991122     O                       VIDDTA              38 '  /  /    '
174700991122     O                       VIDIMA        2     55
174800011220     O**             05                          55 '    '
174900991122     O                       VIDTIP              59
175000991122     O                       VIDMIT              81
175100991122     O                       VIDRIF        Z     92
175200991122     O                       VIDNRA        Z    101
175300900618     O                       §DESBA             132
175400900607      *
175500900607     O          E            TOTALE      2
175600900607     O                       TRATT             +  0
175700900607      *
175800900607     O          E            TOTALE      2
175900060404     O                                         + 70 'Totale assegni/contante'
176000060404     O                                         +  0 ' ....'
176100900607     O                       VIDTAS        2   +  2
176200011220     O**             05                         118 '    '
176300900607      *
176400900607     O          E            TOTALE      2
176500900607     O                                         + 70 'Totale assegni'
176600900607     O                                         +  0 ' Mittente ....'
176700900607     O                       VIDTAM        2   +  2
176800011220     O**             05                         118 '    '
176900900607      *
177000900607     O          E            TOTALE      2
177100900607     O                                         + 70 'Totale Distint'
177200900607     O                                         +  0 'a ............'
177300900607     O                       VIDTDS        2   +  2
177400011220     O**             05                         118 '    '
177500900607      *
177600900607     O          E            TOTALE      0
177700900607     O                                         + 70 'Totale Distint'
177800900607     O                                         +  0 'a ............'
177900900607     O                       VIDTDS        2   +  2
178000011220     O**             05                         118 '    '
178100900607      *
178200900607     O          E            TOTALE      2
178300900607     O                       TRATT             +  0
178400900606      *----------------------------------------------------------------
178500900709**  CMDP
178600900709OVRPRTF FILE(QSYSPRT) FORMTYPE(DS_CONTR) SAVE(*YES)
178700991123**   CMDM
178800020930CHKOBJ OBJ(FNMOV00T) OBJTYPE(*FILE) MBR(M000000)
178900020930ADDPFM FILE(FNMOV00T)               MBR(M000000)
179000020930ALCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
179100020930OVRDBF FILE(FNMOV00T)               MBR(M000000)
179200991123DLTOVR FILE(FNMOV00T)
179300020930DLCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))

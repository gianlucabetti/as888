000100910207     H DECEDIT('0,') DATEDIT(*DMY.)
000200900423     H* FLFG15R *----------------------------------------------------*
000300900418     H*                                                              *
000400900423     H*             - ESITAZIONE/ADDEBITO SPESE GIACENZA             *
000500900418     H*  19 IND. ERRORE GENERICO VIDEO                               *
000600900418     H*  21 IND. COMODO                                              *
000700910314     H*  30 31 DI COMODO                                             *
000800900418     H*  40 TEST SE GIACENZA GIA CARICATA O NO                       *
000900900419     H*  42 TEST SE GIACENZA RIAPERTA                                *
001000900419     H*  43 CHAIN FLBLP                                              *
001100900510     H*  50.....57 ERRORI VIDEO                                      *
001200900524     H*  58 SE CAMBIATI I GG SOSTA A VIDEO LO EVIDENZ. CON RI        *
001300900508     H*  59 ERRORE IN FASE ADDEBITO SPESE : IN SCRITTURA FLBLP OPPU -*
001400900508     H*     RE MANCA TABELLA DEFAULT (2G) OPPURE NON ESISTE O E'ALLO-*
001500900508     H*     CATO IL RECORD CON NUMERO DI SPEDIZIONE                  *
001600910319     H*  70 ERRORE                                                   *
001700900418     H*--------------------------------------------------------------*
001800941011     FFNLG10D   CF   E             WORKSTN
001900941011     F                                     SFILE(LG10S02:NRR)
002000941011     FFNBLP01L  IF   E           K DISK
002100060213     FFiar401L  IF   E           K DISK
002200990929     FFIAR601L  IF   E           K DISK
002300051115     FFiar901L  IF   E           K DISK
002400050216     Ftigcp02L  IF   E           K DISK
002500050216     Ftignp01L  IF   E           K DISK
002600050216     Ftigap01L  IF   E           K DISK
002700900418     FTABEL00F  IF   E           K DISK
002800941011     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002900900418     FCNACO00F  IF   E           K DISK
003000900418     FCNIND00F  IF   E           K DISK
003100941011     D*--------------------------------------------------------------*
003200941011     D STA             S             25    DIM(3) CTDATA PERRCD(1)
003300090520     d* tipi servizio
003400090520     D C5e             S              1    DIM(10)
003500090520     D d5e             S             15    DIM(10)
003600941011     D ERR             S             70    DIM(10) CTDATA PERRCD(1)
003700900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003800900418     D  TCU                  398    697
003900900418     D                                     DIM(50)
004000900418     D  KCU                  698    847P 0
004100900418     D                                     DIM(50)
004200900418     D                                     PACKEVEN
004300900418     D TCUDS           DS
004400900418     D  F3                     3      3
004500900418     D  F4                     4      4
004600900418     D  F56                    5      6
004700941011     D WLBDA8          DS
004800941011     D  G02DAT                 1      8  0
004900941011     D  G02INV                 9     16  0
005000941011     D  G02ERR                17     17
005100941011     D  G02TGI                18     22  0
005200941011     D DSTB69        E DS                  EXTNAME(DS7PQRS)
005300960123     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
005400941011     D FNLG82        E DS                  EXTNAME(FNLG82DS)
005500900418     D CNCR80        E DS
005600911217     D DS2D          E DS
005700911217     D DS2E          E DS
005800910314     D DS3A          E DS
005900900418     D KPJBA         E DS
006000051011     D dgcpflr       E DS
006100060809     D fnlg12ds      E DS
006200060809     D savkpjbu        s                   like(kpjbu)
006300941011     I*--------------------------------------------------------------*
006400941011     IAZORG01L  AA
006500941011     I                             P    4    5 0ORGFIL
006600941011     I                                  6    6  ORGFVA
006700941011     I                                 14   33  ORGDES
006800911108     C*---------------------------------------------------------------*
006900941011     C                   SETON                                        7690
007000911122     C                   EXSR      PULVID
007100941011     C*
007200050216     C     KGIA          CHAIN     tigcp02L                           40
007300900419     C* CONTROLLO SE SI PUO MODIFICARE O DECODIFICO STATO
007400900418     C     *IN40         IFEQ      '0'
007500941011     C     GCPFAS        ANDEQ     40
007600900418     C* PRENDO DATI DELLA BOLLA
007700941011     C     KBOL          CHAIN     FNBLP01L                           43
007800941011     C     BLPATB        IFEQ      '*'
007900941011     C     *IN43         OREQ      '1'
008000941011     C                   SETON                                        2850
008100941011     C                   MOVEL     ERR(1)        $MSG
008200941011     C                   ELSE
008300941011     C                   EXSR      BLPVID
008400941011     C                   END
008500900423     C*  CARICO I DATI DELLA GIAC. A VIDEO
008600941011     C                   EXSR      GCPVID
008700911108     C* DISPOSIZIONI SI POSSONO IMMETTERE ANCHE IN QUESTA FASE SE LA
008800911108     C* GIACENZA E' MANUALE
008900900418     C     EMFG02        TAG
009000941011     C                   SETOFF                                       04
009100060329     c                   if        d82nodsp = *blank
009200941011     C                   EXFMT     LG10D01
009300060329     c                   endif
009400941011     C*                    ------------
009500941011     C     $MSG          IFNE      ERR(1)
009600941011     C     $MSG          ANDNE     ERR(2)
009700941011     C                   SETOFF                                       28
009800941011     C                   END
009900941011     C*
010000941011     C     *INKL         CABEQ     '1'           FINE
010100941011     C*
010200941011     C     *IN50         IFEQ      '1'
010300941011     C                   GOTO      EMFG02
010400941011     C                   END
010500060809      *emetto le videate se il parametro della ds lo permette
010600060809     c                   if        d82tri <> 'D' and precedenti = *blank
010700060809     c                   movel     kpjbu         savkpjbu
010800060809     c                   clear                   fnlg12ds
010900060809     c                   z-add     gcpagc        i12agc
011000060809     c                   z-add     gcpngc        i12ngc
011100060809     c                   z-add     gcpfgc        i12fgc
011200060809     c                   move      'S'           i12nodsp
011300060809     c                   move      d82motr       i12motr
011400060809     c                   movel(p)  fnlg12ds      kpjbu
011500060809     c                   call      'FNLG12R'
011600060809     c                   parm                    kpjba
011700060809     c                   movel     kpjbu         fnlg12ds
011800060809     c                   movel     savkpjbu      kpjbu
011900060809     c                   move      'X'           precedenti        1
012000060809     c                   if        o12ok <> *blank
012100060809     c                   clear                   d82nodsp
012200060809     c                   goto      emfg02
012300060809     c                   endif
012400060809     c                   endif
012500060809     c
012600960123     C*  VISUALIZZA DESTINATARIO ORIGINALE
012700960123     C     *INKM         IFEQ      '1'
012800960123     C                   MOVEL     'N'           WRITOR            1
012900960123     C     WRITOR        DOWEQ     'N'
013000960123     C                   EXSR      GESD06
013100960123     C                   END
013200960123     C                   GOTO      EMFG02
013300960123     C                   END
013400900423     C* CONTROLLO VIDEO DISPOSIZIONI
013500941011     C     *INKN         IFEQ      '1'
013600941011     C                   EXSR      GESNOT
013700941011     C                   GOTO      EMFG02
013800941011     C                   ELSE
013900941011     C     '?'           SCAN      V1CRPG                                 32
014000941011     C     *IN32         IFEQ      '1'
014100941011     C                   MOVEL     'S'           DS7RIC
014200941011     C                   MOVEL     V1CGGA        DS7COD
014300941011     C                   MOVEL     'V'           DS7GES
014400941011     C                   MOVEL     DSTB69        KPJBU
014500941011     C                   CALL      'TRTB69R'
014600941011     C                   PARM                    KPJBA
014700941011     C                   MOVEL     ' '           V1CRPG
014800941011     C                   GOTO      EMFG02
014900911108     C                   END
015000941011     C                   END
015100941011     C* Controllo se devo gestire forzatura da apertura
015200941011     C     WFORZA        IFEQ      'S'
015300941011     C                   SETON                                        04
015400060329     c                   if        d82nodsp = *blank
015500941011     C                   EXFMT     LG10D01
015600060329     c                   endif
015700941011     C     *INKA         IFEQ      '0'
015800941011     C                   GOTO      EMFG02
015900941011     C                   END
016000941011     C                   END
016100941011     C*
016200050318     C                   MOVEL     'S'           D82comm
016300941011     C                   MOVEL     'C'           D82TRI
016400941011     C                   MOVEL     FNLG82        KPJBU
016500941011     C                   CALL      'FNLG14R'
016600911108     C                   PARM                    KPJBA
016700941011     C                   MOVEL     KPJBU         FNLG82
016800941011     C*
016900941011     C     D82F12        IFEQ      '1'
017000941011     C                   GOTO      EMFG02
017100911217     C                   END
017200941011     C*
017300941011     C                   END
017400941011     C*
017500900418     C     FINE          TAG
017600900418     C                   SETON                                        LR
017700960123     C*-----------------------------------------------------*
017800960123     C*  Gestione della sesta videata
017900960123     C*-----------------------------------------------------*
018000960123     C     GESD06        BEGSR
018100960123     C*
018200960123     C                   EXFMT     LG10D06
018300960123     C*                    ------------
018400960123     C*  Ritorno
018500960123     C     *INKL         IFEQ      '1'
018600960123     C                   MOVEL     'S'           WRITOR
018700960123     C                   END
018800960123     C*
018900960123     C                   ENDSR
019000900418     C/EJECT
019100900418     C*---------------------------------------------------------------*
019200941011     C*   OPERAZIONI INIZIALI                                         *
019300900418     C*---------------------------------------------------------------*
019400941011     C     *INZSR        BEGSR
019500941011     C*
019600900418     C     *ENTRY        PLIST
019700900418     C                   PARM                    KPJBA
019800941011     C                   MOVEL     KPJBU         FNLG82
019900941011     C                   CLEAR                   DSTB69
020000941011     C                   MOVE      D82AGC        VAGC              4 0
020100941011     C                   MOVE      D82FGC        VFGC
020200941011     C                   MOVE      D82NGC        VNGC
020300960123     C                   MOVE      D82AGC        V6CAGC
020400960123     C                   MOVE      D82FGC        V6CFGC
020500960123     C                   MOVE      D82NGC        V6CNGC
020600941011     C                   MOVEL     'IN PART'     V1CGES
020700941011     C                   MOVE      'ENZA'        V1CGES
020800941011     C*
020900900418     C                   Z-ADD     1             CODUT
021000900418     C                   CALL      'X§PARUT'
021100900418     C                   PARM                    UTEDSE
021200900418     C     CODUT         CABEQ     -1            FINE
021300900418     C                   MOVEL     RAGUT         RSUT             20
021400960123     C                   MOVEL     RAGUT         V1CRSU
021500900418     C                   MOVEL     RAGUT         RSUT2            25
021600941011     C                   MOVEL     '* FNLG15'    VPROGR
021700900423     C                   MOVE      'R *'         VPROGR
021800900427     C                   MOVEL     RAGUT         PARDUT           30
021900941011     C                   MOVEL     REC80         CNCR80
022000900418     C*--- RICERCA CAPOCONTI------------------------------------------*
022100941011     C                   DO        50            X                 3 0
022200941011     C                   MOVE      TCU(X)        TCUDS
022300941011     C     F56           IFEQ      'CG'
022400941011     C     F4            ANDEQ     '1'
022500941011     C     F3            ANDEQ     '0'
022600941011     C                   Z-ADD     KCU(X)        KCI               4 0
022700941011     C                   END
022800941011     C                   END
022900900418     C*---------------------------------------------------------------*
023000900418     C*  KLIST
023100900418     C     KGIA          KLIST
023200900420     C                   KFLD                    VAGC
023300900420     C                   KFLD                    VFGC
023400900420     C                   KFLD                    VNGC
023500911108     C*
023600900420     C     KBOL          KLIST
023700900420     C                   KFLD                    GCPAAS
023800900420     C                   KFLD                    GCPLNP
023900900420     C                   KFLD                    GCPNRS
024000900420     C                   KFLD                    GCPNSP
024100990929     C     KAR6          KLIST
024200990929     C                   KFLD                    GCPAAS
024300990929     C                   KFLD                    GCPLNP
024400990929     C                   KFLD                    GCPNRS
024500990929     C                   KFLD                    GCPNSP
024600990929     C                   KFLD                    KTRC
024700941011     C*
024800941011     C     KBOL4         KLIST
024900941011     C                   KFLD                    GCPAAS
025000941011     C                   KFLD                    GCPLNP
025100941011     C                   KFLD                    GCPNRS
025200941011     C                   KFLD                    GCPNSP
025300060213     C                   KFLD                    ar4TRC
025400900420     C*
025500900418     C     KTAB          KLIST
025600900418     C                   KFLD                    CODUT
025700900418     C                   KFLD                    TBLCOD
025800900418     C                   KFLD                    TBLKEY
025900941011     C*
026000941011     C     KTBL2         KLIST
026100941011     C                   KFLD                    KKUT
026200941011     C                   KFLD                    KCOD
026300941011     C                   Z-ADD     1             KKUT
026400900418     C*
026500911108     C     KNOT          KLIST
026600911108     C                   KFLD                    VAGC
026700911108     C                   KFLD                    VFGC
026800911108     C                   KFLD                    VNGC
026900911108     C                   KFLD                    §FRG
027000911108     C                   KFLD                    §FAS
027100900418     C*
027200900418     C     KACO          KLIST
027300900418     C                   KFLD                    CODUT
027400900418     C                   KFLD                    KCI
027500900427     C                   KFLD                    §KSC
027600941011     C*                                                    *
027700900427     C     *LIKE         DEFINE    ACOKSC        §KSC
027800941011     C     *LIKE         DEFINE    GCPFRG        §FRG
027900941011     C     *LIKE         DEFINE    GCPFAS        §FAS
028000941011     C     *LIKE         DEFINE    TBLKUT        KKUT
028100941011     C     *LIKE         DEFINE    TBLCOD        KCOD
028200990929     C     *LIKE         DEFINE    AR6TRC        KTRC
028300900427     C*
028400960208     C                   SETON                                        92
028500990929     C                   SETOFF                                       94
028600090520     C* Carico tabella 5e
028700090520     C                   Z-ADD     0             X
028800090520     C                   MOVEL     '5E'          KCOD
028900090520     C     KTBL2         CHAIN     TABEL                              31
029000090520     C     *IN31         DOWEQ     '0'
029100090520     C     X             ANDLT     10
029200090520     C     TBLFLG        IFEQ      ' '
029300090520     C                   ADD       1             X
029400090520     C                   MOVEL     TBLKEY        c5e(X)
029500090520     C                   MOVEL     TBLUNI        d5e(X)
029600090520     C                   END
029700090520     C     KTBL2         READE     TABEL                                  31
029800090520     C                   END
029900900418     C                   ENDSR
030000900426     C/SPACE 3
030100900418     C*---------------------------------------------------------------*
030200900418     C*   P U L V I D                                                 *
030300900418     C*---------------------------------------------------------------*
030400900418     C     PULVID        BEGSR
030500900423     C*
030600960123     C                   MOVEL     *ZEROS        V6CAAS
030700960123     C                   MOVEL     *ZEROS        V6CLNP
030800960123     C                   MOVEL     *ZEROS        V6CNRS
030900960123     C                   MOVEL     *ZEROS        V6CNSP
031000960123     C                   MOVEL     *BLANK        V6CRSD
031100960123     C                   MOVEL     *BLANK        V6CDE2
031200960123     C                   MOVEL     *BLANK        V6CIND
031300960123     C                   MOVEL     *BLANK        V6CLOD
031400960123     C                   MOVEL     *BLANK        V6CPRD
031500960123     C                   MOVEL     *ZEROS        V6CCAD
031600960123     C                   MOVEL     *BLANKS       V6CNZD
031700941011     C                   MOVEL     *ZEROS        VNRS
031800941011     C                   MOVEL     *ZEROS        VNSP
031900941011     C                   MOVEL     *BLANK        VMSG
032000941011     C                   MOVEL     *ZEROS        VDSP
032100941011     C                   MOVEL     *BLANK        VTBL
032200941011     C                   MOVEL     *BLANK        DTBL
032300941011     C                   MOVEL     *ZEROS        VLNP
032400941011     C                   MOVEL     *ZEROS        VLNA
032500941011     C                   MOVEL     *BLANK        DLNA
032600941011     C                   MOVEL     *BLANK        VNAS
032700941011     C                   Z-ADD     *ZEROS        VIFT
032800991119     C                   MOVEL     *BLANK        V1CDFA
032900941011     C                   Z-ADD     *ZEROS        VNCL
033000941011     C                   Z-ADD     *ZEROS        VPKG
033100941011     C                   Z-ADD     *ZEROS        VASS
033200941011     C                   Z-ADD     *ZEROS        VVOL
033300941011     C                   MOVEL     *BLANK        VRSD
033400941011     C                   MOVEL     *BLANK        VIND
033500941011     C                   MOVEL     *BLANK        VLOD
033600941011     C                   MOVEL     *BLANK        VPRD
033700941011     C                   MOVEL     *BLANKS       VCAD
033800941011     C                   MOVEL     *ZEROS        VSCM
033900941011     C                   MOVEL     *BLANK        VRSM
034000941011     C                   MOVEL     *BLANK        VINM
034100941011     C                   MOVEL     *BLANK        VLOM
034200941011     C                   MOVEL     *BLANK        VPRM
034300941011     C                   MOVEL     *BLANKS       VCAM
034400941011     C                   MOVEL     *ZEROS        VDTG
034500941011     C                   MOVEL     *ZEROS        VDIG
034600941011     C                   MOVEL     *BLANKS       DFGC
034700941011     C                   MOVEL     *ZEROS        VCMC
034800941011     C                   MOVEL     *BLANKS       DCMC
034900941011     C                   MOVEL     *BLANKS       VDMC
035000941011     C                   MOVEL     *BLANKS       VNPC
035100941011     C                   MOVEL     *BLANKS       DNPC
035200941011     C                   MOVEL     *BLANKS       V1CNDV
035300941011     C                   MOVEL     *BLANKS       V1CDIV
035400941011     C                   MOVEL     *BLANKS       V1CRD2
035500941011     C                   MOVEL     *BLANKS       V1CNZD
035600941011     C                   MOVEL     *BLANKS       V1CNZM
035700941011     C                   MOVEL     *BLANKS       V1CRMA
035800941011     C                   MOVEL     *BLANKS       V1CMSD
035900941011     C                   MOVEL     *BLANKS       V1CMSP
036000941011     C                   MOVEL     *BLANKS       V1DMSP
036100941011     C                   MOVEL     *BLANKS       V1CGGA
036200941011     C                   MOVEL     *BLANKS       V1CRPG
036300941011     C                   MOVEL     *ZEROS        VDDM
036400941011     C                   MOVEL     *ZEROS        V1CDED
036500941011     C                   MOVEL     *BLANKS       VFRI
036600941011     C*
036700900418     C                   ENDSR
036800900426     C/SPACE 3
036900900418     C*---------------------------------------------------------------*
037000900418     C*   B L P V I D                                                 *
037100900418     C*---------------------------------------------------------------*
037200900418     C     BLPVID        BEGSR
037300941011     C                   CLEAR                   WLBDA8
037400950207     C                   Z-ADD     BLPMGS        G02INV
037500941011     C                   MOVEL     BLPAAS        G02INV
037600941011     C                   Z-ADD     0             G02DAT
037700941011     C                   MOVEL     '3'           G02ERR
037800941011     C                   CALL      'XSRDA8'
037900941011     C                   PARM                    WLBDA8
038000941011     C                   MOVEL     G02DAT        VDSP
038100941011     C* TIPO SPEDIZIONE
038200941011     C                   MOVEL     *BLANKS       DTSP
038300090520     C                   Z-ADD     1             X
038400090520     C     BLPtsp        LOOKUP    C5e(X)                                 32
038500090520     C     *IN32         IFEQ      '1'
038600090520     C                   MOVEl     D5e(X)        Dtsp
038700090520     C                   END
038800941011     C* TIPO BOLLA
038900941011     C                   MOVEL     BLPCBO        VTBL
039000941011     C                   MOVEL     '3A'          TBLCOD
039100941011     C                   MOVEL     *BLANK        TBLKEY
039200941011     C                   MOVEL     VTBL          TBLKEY
039300941011     C     KTAB          CHAIN     TABEL                              21
039400941011     C   21              MOVEL     *ALL'?'       DTBL
039500941011     C  N21              MOVEL     TBLUNI        DS3A
039600941011     C  N21              MOVEL     §3ADES        DTBL
039700990929     C  N21              MOVEL     §3ATB1        WPORTO            1
039800941011     C* LINEA ARRIVO
039900941011     C                   MOVEL     BLPLNA        VLNA
040000941011     C                   MOVEL     BLPLNA        ORGFIL
040100941011     C     ORGFIL        CHAIN     AZORG01L                           21
040200941011     C  N21ORGFVA        COMP      '*'                                    21
040300941011     C   21              MOVEL     *ALL'?'       DLNA
040400941011     C  N21              MOVEL     ORGDES        DLNA
040500941011     C*
040600941011     C                   MOVEL     BLPNAS        VNAS
040700941011     C                   Z-ADD     BLPNCL        VNCL
040800941011     C                   Z-ADD     BLPPKF        VPKG
040900941011     C                   Z-ADD     BLPVLF        VVOL
041000941011     C* Reperisco importi fatture e C/Assegno
041100991005     C     WPORTO        IFEQ      'F'
041200990929     C                   MOVEL     '1'           KTRC
041300990929     C                   Z-ADD     0             AR6IFT
041400990929     C     KAR6          CHAIN     FIAR601L                           31
041500990929     C                   Z-ADD     AR6IFT        VIFT
041600991119     C                   MOVEL     AR6DIV        V1CDFA
041700990929     C                   END
041800051115     C     KBOL          CHAIN     Fiar901L                           31
041900051115     C                   Z-ADD     ar9CAS        VASS
042000051115     C                   MOVEL     ar9VCA        V1CDIV
042100941011     C* Controllo se esiste estensione descrizione destinatario
042200060213     C                   MOVEL     'D'           ar4TRC
042300060213     C                   MOVEL     *BLANKS       ar4NOT
042400060213     C     KBOL4         CHAIN     Fiar401L                           31
042500960123     C* Controllo se esiste destinatario finale
042600960123     C                   SETOFF                                       13
042700960123     C                   CLEAR                   FNLSN0
042800960123     C                   Z-ADD     BLPAAS        DN0AAS
042900960123     C                   Z-ADD     BLPLNP        DN0LNP
043000960123     C                   Z-ADD     BLPNRS        DN0NRS
043100960123     C                   Z-ADD     BLPNSP        DN0NSP
043200960123     C                   MOVEL     *BLANKS       DN0TRC
043300960125     C                   Z-ADD     GCPDUR        DN0DAG
043400960123     C                   MOVEL     KPJBU         WSVKPJ          256
043500960123     C                   MOVEL     FNLSN0        KPJBU
043600960123     C                   CALL      'FNLSN0R'
043700960123     C                   PARM                    KPJBA
043800960123     C                   MOVEL     KPJBU         FNLSN0
043900960123     C                   MOVEL     WSVKPJ        KPJBU
044000960123     C* Destinatario
044100960123     C     DN0TRV        IFEQ      'S'
044200960123     C                   SETON                                        13
044300960123     C                   MOVEL     DN0RSD        VRSD
044400960123     C                   MOVEL     DN0RS2        V1CRD2
044500960123     C                   MOVEL     DN0IND        VIND
044600960123     C                   MOVEL     DN0LOD        VLOD
044700960123     C                   MOVEL     DN0PRD        VPRD
044800960123     C                   MOVEL     DN0CAD        VCAD
044900960123     C                   MOVEL     DN0NZD        V1CNZD
045000960123     C                   MOVEL     BLPRSD        V6CRSD
045100060213     C                   MOVEL     ar4NOT        V6CDE2
045200960123     C                   MOVEL     BLPIND        V6CIND
045300960123     C                   MOVEL     BLPLOD        V6CLOD
045400960123     C                   MOVEL     BLPPRD        V6CPRD
045500960123     C                   MOVEL     BLPCAD        V6CCAD
045600960123     C                   MOVEL     BLPNZD        V6CNZD
045700960123     C                   ELSE
045800960123     C* DESTINATARIO
045900960123     C                   MOVEL     BLPRSD        VRSD
046000060213     C                   MOVEL     ar4NOT        V1CRD2
046100960123     C                   MOVEL     BLPIND        VIND
046200960123     C                   MOVEL     BLPLOD        VLOD
046300960123     C                   MOVEL     BLPPRD        VPRD
046400960123     C                   MOVEL     BLPCAD        VCAD
046500960123     C                   MOVEL     BLPNZD        V1CNZD
046600960123     C                   END
046700941011     C* MITTENTE
046800941011     C                   MOVEL     BLPCCM        VSCM
046900941011     C                   MOVEL     BLPRSM        VRSM
047000941011     C                   MOVEL     BLPINM        VINM
047100941011     C                   MOVEL     BLPLOM        VLOM
047200941011     C                   MOVEL     BLPPRM        VPRM
047300941011     C                   MOVEL     BLPCAM        VCAM
047400941011     C                   MOVEL     BLPRMN        VRIF
047500941011     C                   MOVEL     BLPRMA        V1CRMA
047600941011     C                   MOVEL     BLPNZM        V1CNZM
047700941011     C                   MOVEL     BLPGGA        V1CGGA
047800941011     C* Carico messaggio particolarità
047900941011     C                   MOVEL     'N'           WFORZA            1
048000941011     C                   SETOFF                                       0506
048100900418     C                   ENDSR
048200900426     C/SPACE 3
048300900418     C*---------------------------------------------------------------*
048400900418     C*   G C P V I D                                                 *
048500900418     C*---------------------------------------------------------------*
048600900418     C     GCPVID        BEGSR
048700941011     C*
048800960123     C                   MOVE      GCPLNP        V6CLNP
048900960123     C                   MOVE      GCPAAS        V6CAAS
049000960123     C                   MOVE      GCPNRS        V6CNRS
049100960123     C                   MOVE      GCPNSP        V6CNSP
049200941011     C     GCPDUR        COMP      *ZERO                              42
049300941011     C                   MOVE      GCPLNP        VLNP
049400941011     C                   MOVE      GCPAAS        VAAS
049500941011     C                   MOVE      GCPNRS        VNRS
049600941011     C                   MOVE      GCPNSP        VNSP
049700941011     C                   MOVE      GCPSCM        VSCM
049800941011     C                   MOVE      VSCM          §QUATR            4
049900941011     C                   MOVE      VSCM          §KSC
050000941011     C     §QUATR        IFEQ      *ALL'8'
050100941011     C     §QUATR        OREQ      *ALL'9'
050200941011     C* MITTENTE DA APPOGGIO
050300050216     C     KBOL          CHAIN     tigap01L                           21
050400941011     C     *IN21         IFEQ      '0'
050500941011     C                   MOVEL     GAPRSM        VRSM
050600941011     C                   MOVEL     GAPINM        VINM
050700941011     C                   MOVEL     GAPLOM        VLOM
050800941011     C                   MOVEL     GAPPRM        VPRM
050900941011     C                   MOVEL     GAPCAM        VCAM
051000941011     C                   MOVEL     GAPNZM        V1CNZM
051100941011     C                   END
051200941011     C                   ELSE
051300941011     C* SOLO SE SONO IN PARTENZA
051400941011     C     KACO          CHAIN     CNACO000                           21
051500941011     C  N21ACOFLG        COMP      '*'                                    21
051600941011     C  N21ACOABL        COMP      '*'                                    21
051700941011     C  N21              MOVEL     ACORAG        VRSM
051800941011     C   21              MOVE      *ALL'?'       VRSM
051900941011     C     KACO          CHAIN     CNIND000                           21
052000941011     C   21              MOVE      *ALL'?'       VINM
052100941011     C   21              MOVE      *ALL'?'       VLOM
052200941011     C   21              MOVE      *ALL'?'       VPRM
052300941011     C  N21              MOVEL     INDVIA        VINM
052400941011     C  N21              MOVEL     INDCIT        VLOM
052500941011     C  N21              MOVEL     INDPRV        VPRM
052600050223     C  N21              MOVEL     INDCAe        VCAM
052700941011     C  N21              MOVEL     INDSTA        V1CNZM
052800941011     C                   END
052900941011     C* CODICE MOTIVO GIACENZA
053000941011     C                   MOVE      GCPCMC        VCMC
053100941011     C                   MOVEL     '2A'          TBLCOD
053200941011     C                   MOVEL     *BLANK        TBLKEY
053300941011     C                   MOVEL     VCMC          TBLKEY
053400941011     C     KTAB          CHAIN     TABEL                              21
053500941011     C   21              MOVEL     *ALL'?'       DCMC
053600941011     C  N21              MOVEL     TBLUNI        DCMC
053700941011     C* ULTERIORE MOTIVO
053800941011     C                   Z-ADD     10            §FAS
053900050216     C     KNOT          CHAIN     tignp01L                           21
054000941011     C  N21              MOVEL     GNPDMC        VDMC
054100941011     C   21              MOVEL     *BLANKS       VDMC
054200941011     C* DATA APERTURA GIACENZA
054300950207     C                   Z-ADD     GCPMGC        G02INV
054400960123     C                   MOVEL     GCPAGC        G02INV
054500941011     C                   Z-ADD     0             G02DAT
054600941011     C                   MOVEL     '3'           G02ERR
054700941011     C                   CALL      'XSRDA8'
054800941011     C                   PARM                    WLBDA8
054900941011     C                   MOVEL     G02DAT        VDTG
055000941011     C                   MOVE      GCPNGC        VNGC
055100960123     C                   MOVE      GCPNGC        V6CNGC
055200941011     C* FIL. CHE HA APERTO LA GIACENZA
055300941011     C     GCPFGC        IFNE      *ZEROS
055400960123     C                   MOVE      GCPFGC        V6CFGC
055500941011     C                   MOVE      GCPFGC        VFGC
055600941011     C     VFGC          CHAIN     AZORG01L                           21
055700941011     C  N21ORGFVA        COMP      '*'                                    21
055800941011     C   21              MOVEL     *ALL'?'       DFGC
055900941011     C  N21              MOVEL     ORGDES        DFGC
056000941011     C                   END
056100941011     C*
056200941011     C* DATA ULTIMA RIAPERTURA
056300050408     c                   if        gcpriap = 'R'
056400941011     C                   Z-ADD     GCPDUR        G02INV
056500941011     C                   Z-ADD     0             G02DAT
056600941011     C                   MOVEL     '3'           G02ERR
056700941011     C                   CALL      'XSRDA8'
056800941011     C                   PARM                    WLBDA8
056900941011     C                   MOVEL     G02DAT        VDIG
057000050408     c                   endif
057100051011     C* appuntamento
057200051011     C                   MOVEL     GCPflr        dgcpflr
057300051011     c                   movel     D§GCPAPP      v1capp
057400941011     C* DATA DISPOSIZIONI MITTENTE
057500941011     C                   Z-ADD     GCPDDM        G02INV
057600941011     C                   Z-ADD     0             G02DAT
057700941011     C                   MOVEL     '3'           G02ERR
057800941011     C                   CALL      'XSRDA8'
057900941011     C                   PARM                    WLBDA8
058000941011     C                   MOVEL     G02DAT        VDDM
058100941011     C* DATA COMUNICAZIONE INTERNA = DATA DIST. PAD. SE IN ARRIVO
058200941011     C                   Z-ADD     GCPDXD        G02INV
058300941011     C                   Z-ADD     0             G02DAT
058400941011     C                   MOVEL     '3'           G02ERR
058500941011     C                   CALL      'XSRDA8'
058600941011     C                   PARM                    WLBDA8
058700941011     C                   MOVEL     G02DAT        VDXD
058800941011     C* DATA ESECUZIONE DISPOSIZIONI
058900941011     C     GCPDED        IFGT      *ZEROS
059000941011     C                   Z-ADD     GCPDED        G02INV
059100941011     C                   Z-ADD     0             G02DAT
059200941011     C                   MOVEL     '3'           G02ERR
059300941011     C                   CALL      'XSRDA8'
059400941011     C                   PARM                    WLBDA8
059500941011     C                   MOVEL     G02DAT        V1CDED
059600941011     C                   END
059700941011     C* NR. E DATA COMUNICAZ.
059800941011     C                   MOVE      GCPNRC        VNRC
059900941011     C* LUOGO DEPOSITO MERCE
060000941011     C                   MOVE      GCPCCR        VCCR
060100941011     C                   MOVEL     *BLANKS       DCCR
060200941011     C     GCPCCR        IFNE      *ZEROS
060300941011     C                   MOVE      VCCR          ORGFIL
060400941011     C     ORGFIL        CHAIN     AZORG01L                           21
060500941011     C  N21ORGFVA        COMP      '*'                                    21
060600941011     C   21              MOVEL     *ALL'?'       DCCR
060700941011     C  N21              MOVEL     *BLANK        DCCR
060800941011     C  N21              MOVEL     ORGDES        DCCR
060900941011     C     *IN21         IFEQ      '1'
061000941011     C                   MOVEL     '2B'          TBLCOD
061100941011     C                   MOVEL     *BLANK        TBLKEY
061200941011     C                   MOVEL     VCCR          TBLKEY
061300941011     C     KTAB          CHAIN     TABEL                              21
061400941011     C   21              MOVEL     *ALL'?'       DCCR
061500941011     C  N21              MOVEL     TBLUNI        DCCR
061600941011     C                   END
061700941011     C                   END
061800941011     C*
061900941011     C                   MOVEL     GCPDIS        VFRI
062000941011     C                   MOVEL     '2D'          TBLCOD
062100941011     C                   MOVEL     *BLANK        TBLKEY
062200941011     C                   MOVEL     VFRI          TBLKEY
062300941011     C     KTAB          CHAIN     TABEL                              21
062400941011     C   21              MOVEL     *ALL'?'       DFRI
062500941011     C  N21              MOVEL     TBLUNI        DS2D
062600941011     C  N21              MOVEL     §2DDES        DFRI
062700941011     C*
062800941011     C                   MOVEL     GCPASG        VASG
062900941011     C                   MOVEL     GCPPSG        VPSG
063000941011     C     VPSG          IFNE      ' '
063100941011     C                   MOVEL     '2E'          TBLCOD
063200941011     C                   MOVEL     *BLANK        TBLKEY
063300941011     C                   MOVEL     VPSG          TBLKEY
063400941011     C     KTAB          CHAIN     TABEL                              21
063500941011     C   21              MOVEL     *ALL'?'       DPSG
063600941011     C  N21              MOVEL     TBLUNI        DS2E
063700941011     C  N21              MOVEL     §2EDES        DPSG
063800941011     C                   END
063900941011     C*
064000941011     C     GCPNPC        IFNE      *BLANK
064100941011     C                   MOVEL     GCPNPC        VNPC
064200941011     C     GCPNPC        COMP      'A'                                    21
064300941011     C   21              MOVEL     'ASSEGNAT'    DNPC
064400941011     C   21              MOVE      'O '          DNPC
064500941011     C  N21              MOVEL     'FRANCO  '    DNPC
064600941011     C  N21              MOVE      '  '          DNPC
064700941011     C                   ELSE
064800941011     C                   MOVEL     *BLANKS       VNPC
064900941011     C                   MOVE      *BLANKS       DNPC
065000941011     C                   END
065100941011     C                   MOVEL     GCPVCS        VVCS
065200941011     C                   MOVEL     GCPCAS        VNCA
065300941011     C                   MOVEL     GCPVCA        V1CNDV
065400941011     C                   MOVEL     GCPAPC        VAPC
065500941011     C                   MOVEL     GCPPPC        VPPC
065600060808     C                   clear                   V1cchim
065700060808     c                   if        GCPchiaut = 'N'
065800060808     C                   MOVE      'S'           V1cchim
065900060808     c                   endif
066000941011     C     VPPC          IFNE      *BLANK
066100941011     C                   MOVEL     '2E'          TBLCOD
066200941011     C                   MOVEL     *BLANK        TBLKEY
066300941011     C                   MOVEL     VPPC          TBLKEY
066400941011     C     KTAB          CHAIN     TABEL                              21
066500941011     C   21              MOVEL     *ALL'?'       DPPC
066600941011     C  N21              MOVEL     TBLUNI        DS2E
066700941011     C  N21              MOVEL     §2EDES        DPPC
066800941011     C                   END
066900900423     C*
067000960208     C                   Z-ADD     20            §FAS
067100050216     C     KNOT          SETLL     tignp01L                               21
067200960208     C   21              SETON                                        94
067300960208     C*
067400900426     C                   ENDSR
067500911108     C*---------------*
067600911108     C* GESTIONE NOTE
067700911108     C*---------------*
067800911108     C     GESNOT        BEGSR
067900911122     C                   Z-ADD     20            §FAS
068000911108     C                   EXSR      PULIZ
068100050216     C     KNOT          SETLL     tignp01L
068200911108     C     *IN21         DOUEQ     '1'
068300050216     C     KNOT          READE     tignp01L                               21
068400911108     C     *IN21         IFEQ      '0'
068500911125     C                   MOVEL     GNPDMC        VDMC1
068600911108     C                   MOVEL     GNPTPN        VTPN
068700911108     C                   MOVEL     GNPNPR        VNPR
068800911108     C                   ADD       1             NRR               3 0
068900941011     C                   WRITE     LG10S02
069000911108     C                   ELSE
069100960208     C     NRR           DIV       10            WRESTO            2 0
069200960208     C                   MVR                     §NR               2 0
069300960208     C     10            SUB       §NR           §NR
069400911108     C                   DO        §NR
069500911125     C                   MOVEL     *BLANK        VDMC1
069600911108     C                   MOVEL     *BLANK        VTPN
069700911108     C                   ADD       1             VNPR
069800911108     C                   ADD       1             NRR
069900941011     C                   WRITE     LG10S02
070000911108     C                   END
070100911108     C                   END
070200911108     C                   END
070300960208     C                   Z-ADD     1             NR2KEY
070400911108     C*
070500941011     C     *INKL         DOWEQ     '0'
070600941011     C                   WRITE     LG10Z02
070700941011     C                   EXFMT     LG10C02
070800941011     C                   END
070900941011     C*
071000941011     C                   ENDSR
071100911108     C*----------------*
071200911108     C* PULIZIA SUBFILE
071300911108     C*----------------*
071400911108     C     PULIZ         BEGSR
071500911108     C                   SETON                                        70
071600941011     C                   WRITE     LG10C02
071700911108     C                   Z-ADD     0             NRR
071800911108     C                   SETOFF                                       70
071900911108     C                   ENDSR
072000941011** STA
072100941011PRIMA CHIUSURA   :
072200941011DOPO  CHIUSURA   :
072300941011 NON MANUTENZIONABILE !!
072400941011** ERR
072500941011Bolla inesistente                                                     01

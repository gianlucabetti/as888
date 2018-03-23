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
003300941011     D C7Q             S              2    DIM(100)
003400941011     D D7Q             S             89    DIM(100)
003500941011     D ERR             S             70    DIM(10) CTDATA PERRCD(1)
003600900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003700900418     D  TCU                  398    697
003800900418     D                                     DIM(50)
003900900418     D  KCU                  698    847P 0
004000900418     D                                     DIM(50)
004100900418     D                                     PACKEVEN
004200900418     D TCUDS           DS
004300900418     D  F3                     3      3
004400900418     D  F4                     4      4
004500900418     D  F56                    5      6
004600941011     D WLBDA8          DS
004700941011     D  G02DAT                 1      8  0
004800941011     D  G02INV                 9     16  0
004900941011     D  G02ERR                17     17
005000941011     D  G02TGI                18     22  0
005100941011     D DSTB69        E DS                  EXTNAME(DS7PQRS)
005200960123     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
005300941011     D FNLG82        E DS                  EXTNAME(FNLG82DS)
005400900418     D CNCR80        E DS
005500911217     D DS2D          E DS
005600911217     D DS2E          E DS
005700910314     D DS3A          E DS
005800941011     D DS7Q          E DS
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
028600900418     C                   ENDSR
028700900426     C/SPACE 3
028800900418     C*---------------------------------------------------------------*
028900900418     C*   P U L V I D                                                 *
029000900418     C*---------------------------------------------------------------*
029100900418     C     PULVID        BEGSR
029200900423     C*
029300960123     C                   MOVEL     *ZEROS        V6CAAS
029400960123     C                   MOVEL     *ZEROS        V6CLNP
029500960123     C                   MOVEL     *ZEROS        V6CNRS
029600960123     C                   MOVEL     *ZEROS        V6CNSP
029700960123     C                   MOVEL     *BLANK        V6CRSD
029800960123     C                   MOVEL     *BLANK        V6CDE2
029900960123     C                   MOVEL     *BLANK        V6CIND
030000960123     C                   MOVEL     *BLANK        V6CLOD
030100960123     C                   MOVEL     *BLANK        V6CPRD
030200960123     C                   MOVEL     *ZEROS        V6CCAD
030300960123     C                   MOVEL     *BLANKS       V6CNZD
030400941011     C                   MOVEL     *ZEROS        VNRS
030500941011     C                   MOVEL     *ZEROS        VNSP
030600941011     C                   MOVEL     *BLANK        VMSG
030700941011     C                   MOVEL     *ZEROS        VDSP
030800941011     C                   MOVEL     *BLANK        VTBL
030900941011     C                   MOVEL     *BLANK        DTBL
031000941011     C                   MOVEL     *ZEROS        VLNP
031100941011     C                   MOVEL     *ZEROS        VLNA
031200941011     C                   MOVEL     *BLANK        DLNA
031300941011     C                   MOVEL     *BLANK        VNAS
031400941011     C                   Z-ADD     *ZEROS        VIFT
031500991119     C                   MOVEL     *BLANK        V1CDFA
031600941011     C                   Z-ADD     *ZEROS        VNCL
031700941011     C                   Z-ADD     *ZEROS        VPKG
031800941011     C                   Z-ADD     *ZEROS        VASS
031900941011     C                   Z-ADD     *ZEROS        VVOL
032000941011     C                   MOVEL     *BLANK        VRSD
032100941011     C                   MOVEL     *BLANK        VIND
032200941011     C                   MOVEL     *BLANK        VLOD
032300941011     C                   MOVEL     *BLANK        VPRD
032400941011     C                   MOVEL     *BLANKS       VCAD
032500941011     C                   MOVEL     *ZEROS        VSCM
032600941011     C                   MOVEL     *BLANK        VRSM
032700941011     C                   MOVEL     *BLANK        VINM
032800941011     C                   MOVEL     *BLANK        VLOM
032900941011     C                   MOVEL     *BLANK        VPRM
033000941011     C                   MOVEL     *BLANKS       VCAM
033100941011     C                   MOVEL     *ZEROS        VDTG
033200941011     C                   MOVEL     *ZEROS        VDIG
033300941011     C                   MOVEL     *BLANKS       DFGC
033400941011     C                   MOVEL     *ZEROS        VCMC
033500941011     C                   MOVEL     *BLANKS       DCMC
033600941011     C                   MOVEL     *BLANKS       VDMC
033700941011     C                   MOVEL     *BLANKS       VNPC
033800941011     C                   MOVEL     *BLANKS       DNPC
033900941011     C                   MOVEL     *BLANKS       V1CNDV
034000941011     C                   MOVEL     *BLANKS       V1CDIV
034100941011     C                   MOVEL     *BLANKS       V1CRD2
034200941011     C                   MOVEL     *BLANKS       V1CNZD
034300941011     C                   MOVEL     *BLANKS       V1CNZM
034400941011     C                   MOVEL     *BLANKS       V1CRMA
034500941011     C                   MOVEL     *BLANKS       V1CMSD
034600941011     C                   MOVEL     *BLANKS       V1CMSP
034700941011     C                   MOVEL     *BLANKS       V1DMSP
034800941011     C                   MOVEL     *BLANKS       V1CGGA
034900941011     C                   MOVEL     *BLANKS       V1CRPG
035000941011     C                   MOVEL     *ZEROS        VDDM
035100941011     C                   MOVEL     *ZEROS        V1CDED
035200941011     C                   MOVEL     *BLANKS       VFRI
035300941011     C*
035400900418     C                   ENDSR
035500900426     C/SPACE 3
035600900418     C*---------------------------------------------------------------*
035700900418     C*   B L P V I D                                                 *
035800900418     C*---------------------------------------------------------------*
035900900418     C     BLPVID        BEGSR
036000941011     C                   CLEAR                   WLBDA8
036100950207     C                   Z-ADD     BLPMGS        G02INV
036200941011     C                   MOVEL     BLPAAS        G02INV
036300941011     C                   Z-ADD     0             G02DAT
036400941011     C                   MOVEL     '3'           G02ERR
036500941011     C                   CALL      'XSRDA8'
036600941011     C                   PARM                    WLBDA8
036700941011     C                   MOVEL     G02DAT        VDSP
036800941011     C* TIPO SPEDIZIONE
036900941011     C                   MOVEL     *BLANKS       DTSP
037000941011     C     BLPTSP        IFEQ      'U'
037100941011     C     BLPTSP        OREQ      'E'
037200941011     C                   MOVEL     'ESPRESSO'    DTSP
037300941011     C                   END
037400941011     C* TIPO BOLLA
037500941011     C                   MOVEL     BLPCBO        VTBL
037600941011     C                   MOVEL     '3A'          TBLCOD
037700941011     C                   MOVEL     *BLANK        TBLKEY
037800941011     C                   MOVEL     VTBL          TBLKEY
037900941011     C     KTAB          CHAIN     TABEL                              21
038000941011     C   21              MOVEL     *ALL'?'       DTBL
038100941011     C  N21              MOVEL     TBLUNI        DS3A
038200941011     C  N21              MOVEL     §3ADES        DTBL
038300990929     C  N21              MOVEL     §3ATB1        WPORTO            1
038400941011     C* LINEA ARRIVO
038500941011     C                   MOVEL     BLPLNA        VLNA
038600941011     C                   MOVEL     BLPLNA        ORGFIL
038700941011     C     ORGFIL        CHAIN     AZORG01L                           21
038800941011     C  N21ORGFVA        COMP      '*'                                    21
038900941011     C   21              MOVEL     *ALL'?'       DLNA
039000941011     C  N21              MOVEL     ORGDES        DLNA
039100941011     C*
039200941011     C                   MOVEL     BLPNAS        VNAS
039300941011     C                   Z-ADD     BLPNCL        VNCL
039400941011     C                   Z-ADD     BLPPKF        VPKG
039500941011     C                   Z-ADD     BLPVLF        VVOL
039600941011     C* Reperisco importi fatture e C/Assegno
039700991005     C     WPORTO        IFEQ      'F'
039800990929     C                   MOVEL     '1'           KTRC
039900990929     C                   Z-ADD     0             AR6IFT
040000990929     C     KAR6          CHAIN     FIAR601L                           31
040100990929     C                   Z-ADD     AR6IFT        VIFT
040200991119     C                   MOVEL     AR6DIV        V1CDFA
040300990929     C                   END
040400051115     C     KBOL          CHAIN     Fiar901L                           31
040500051115     C                   Z-ADD     ar9CAS        VASS
040600051115     C                   MOVEL     ar9VCA        V1CDIV
040700941011     C* Controllo se esiste estensione descrizione destinatario
040800060213     C                   MOVEL     'D'           ar4TRC
040900060213     C                   MOVEL     *BLANKS       ar4NOT
041000060213     C     KBOL4         CHAIN     Fiar401L                           31
041100960123     C* Controllo se esiste destinatario finale
041200960123     C                   SETOFF                                       13
041300960123     C                   CLEAR                   FNLSN0
041400960123     C                   Z-ADD     BLPAAS        DN0AAS
041500960123     C                   Z-ADD     BLPLNP        DN0LNP
041600960123     C                   Z-ADD     BLPNRS        DN0NRS
041700960123     C                   Z-ADD     BLPNSP        DN0NSP
041800960123     C                   MOVEL     *BLANKS       DN0TRC
041900960125     C                   Z-ADD     GCPDUR        DN0DAG
042000960123     C                   MOVEL     KPJBU         WSVKPJ          256
042100960123     C                   MOVEL     FNLSN0        KPJBU
042200960123     C                   CALL      'FNLSN0R'
042300960123     C                   PARM                    KPJBA
042400960123     C                   MOVEL     KPJBU         FNLSN0
042500960123     C                   MOVEL     WSVKPJ        KPJBU
042600960123     C* Destinatario
042700960123     C     DN0TRV        IFEQ      'S'
042800960123     C                   SETON                                        13
042900960123     C                   MOVEL     DN0RSD        VRSD
043000960123     C                   MOVEL     DN0RS2        V1CRD2
043100960123     C                   MOVEL     DN0IND        VIND
043200960123     C                   MOVEL     DN0LOD        VLOD
043300960123     C                   MOVEL     DN0PRD        VPRD
043400960123     C                   MOVEL     DN0CAD        VCAD
043500960123     C                   MOVEL     DN0NZD        V1CNZD
043600960123     C                   MOVEL     BLPRSD        V6CRSD
043700060213     C                   MOVEL     ar4NOT        V6CDE2
043800960123     C                   MOVEL     BLPIND        V6CIND
043900960123     C                   MOVEL     BLPLOD        V6CLOD
044000960123     C                   MOVEL     BLPPRD        V6CPRD
044100960123     C                   MOVEL     BLPCAD        V6CCAD
044200960123     C                   MOVEL     BLPNZD        V6CNZD
044300960123     C                   ELSE
044400960123     C* DESTINATARIO
044500960123     C                   MOVEL     BLPRSD        VRSD
044600060213     C                   MOVEL     ar4NOT        V1CRD2
044700960123     C                   MOVEL     BLPIND        VIND
044800960123     C                   MOVEL     BLPLOD        VLOD
044900960123     C                   MOVEL     BLPPRD        VPRD
045000960123     C                   MOVEL     BLPCAD        VCAD
045100960123     C                   MOVEL     BLPNZD        V1CNZD
045200960123     C                   END
045300941011     C* MITTENTE
045400941011     C                   MOVEL     BLPCCM        VSCM
045500941011     C                   MOVEL     BLPRSM        VRSM
045600941011     C                   MOVEL     BLPINM        VINM
045700941011     C                   MOVEL     BLPLOM        VLOM
045800941011     C                   MOVEL     BLPPRM        VPRM
045900941011     C                   MOVEL     BLPCAM        VCAM
046000941011     C                   MOVEL     BLPRMN        VRIF
046100941011     C                   MOVEL     BLPRMA        V1CRMA
046200941011     C                   MOVEL     BLPNZM        V1CNZM
046300941011     C                   MOVEL     BLPGGA        V1CGGA
046400941011     C* Carico messaggio particolarità
046500941011     C                   MOVEL     'N'           WFORZA            1
046600941011     C                   SETOFF                                       0506
046700941011     C     BLPGGA        IFNE      *BLANKS
046800941011     C                   Z-ADD     1             X
046900941011     C     BLPGGA        LOOKUP    C7Q(X)                                 32
047000941011     C     *IN32         IFEQ      '1'
047100941011     C                   MOVE      D7Q(X)        DS7Q
047200941011     C     §7Q3MF        IFEQ      'C'
047300941011     C     §7Q3MS        ANDNE     *BLANKS
047400941011     C     §7Q3MQ        IFEQ      'P'
047500941011     C                   SETON                                        05
047600941011     C                   MOVEL     STA(1)        V1DMSP
047700941011     C                   MOVEL     §7Q3MQ        V1CMSP
047800941011     C                   MOVEL     §7Q3MC        WFORZA
047900941011     C                   ELSE
048000941011     C                   SETON                                        06
048100941011     C                   MOVEL     STA(2)        V1DMSP
048200941011     C                   MOVEL     §7Q3MQ        V1CMSD
048300941011     C                   END
048400941011     C                   END
048500941011     C                   END
048600941011     C                   END
048700900418     C                   ENDSR
048800900426     C/SPACE 3
048900900418     C*---------------------------------------------------------------*
049000900418     C*   G C P V I D                                                 *
049100900418     C*---------------------------------------------------------------*
049200900418     C     GCPVID        BEGSR
049300941011     C*
049400960123     C                   MOVE      GCPLNP        V6CLNP
049500960123     C                   MOVE      GCPAAS        V6CAAS
049600960123     C                   MOVE      GCPNRS        V6CNRS
049700960123     C                   MOVE      GCPNSP        V6CNSP
049800941011     C     GCPDUR        COMP      *ZERO                              42
049900941011     C                   MOVE      GCPLNP        VLNP
050000941011     C                   MOVE      GCPAAS        VAAS
050100941011     C                   MOVE      GCPNRS        VNRS
050200941011     C                   MOVE      GCPNSP        VNSP
050300941011     C                   MOVE      GCPSCM        VSCM
050400941011     C                   MOVE      VSCM          §QUATR            4
050500941011     C                   MOVE      VSCM          §KSC
050600941011     C     §QUATR        IFEQ      *ALL'8'
050700941011     C     §QUATR        OREQ      *ALL'9'
050800941011     C* MITTENTE DA APPOGGIO
050900050216     C     KBOL          CHAIN     tigap01L                           21
051000941011     C     *IN21         IFEQ      '0'
051100941011     C                   MOVEL     GAPRSM        VRSM
051200941011     C                   MOVEL     GAPINM        VINM
051300941011     C                   MOVEL     GAPLOM        VLOM
051400941011     C                   MOVEL     GAPPRM        VPRM
051500941011     C                   MOVEL     GAPCAM        VCAM
051600941011     C                   MOVEL     GAPNZM        V1CNZM
051700941011     C                   END
051800941011     C                   ELSE
051900941011     C* SOLO SE SONO IN PARTENZA
052000941011     C     KACO          CHAIN     CNACO000                           21
052100941011     C  N21ACOFLG        COMP      '*'                                    21
052200941011     C  N21ACOABL        COMP      '*'                                    21
052300941011     C  N21              MOVEL     ACORAG        VRSM
052400941011     C   21              MOVE      *ALL'?'       VRSM
052500941011     C     KACO          CHAIN     CNIND000                           21
052600941011     C   21              MOVE      *ALL'?'       VINM
052700941011     C   21              MOVE      *ALL'?'       VLOM
052800941011     C   21              MOVE      *ALL'?'       VPRM
052900941011     C  N21              MOVEL     INDVIA        VINM
053000941011     C  N21              MOVEL     INDCIT        VLOM
053100941011     C  N21              MOVEL     INDPRV        VPRM
053200050223     C  N21              MOVEL     INDCAe        VCAM
053300941011     C  N21              MOVEL     INDSTA        V1CNZM
053400941011     C                   END
053500941011     C* CODICE MOTIVO GIACENZA
053600941011     C                   MOVE      GCPCMC        VCMC
053700941011     C                   MOVEL     '2A'          TBLCOD
053800941011     C                   MOVEL     *BLANK        TBLKEY
053900941011     C                   MOVEL     VCMC          TBLKEY
054000941011     C     KTAB          CHAIN     TABEL                              21
054100941011     C   21              MOVEL     *ALL'?'       DCMC
054200941011     C  N21              MOVEL     TBLUNI        DCMC
054300941011     C* ULTERIORE MOTIVO
054400941011     C                   Z-ADD     10            §FAS
054500050216     C     KNOT          CHAIN     tignp01L                           21
054600941011     C  N21              MOVEL     GNPDMC        VDMC
054700941011     C   21              MOVEL     *BLANKS       VDMC
054800941011     C* DATA APERTURA GIACENZA
054900950207     C                   Z-ADD     GCPMGC        G02INV
055000960123     C                   MOVEL     GCPAGC        G02INV
055100941011     C                   Z-ADD     0             G02DAT
055200941011     C                   MOVEL     '3'           G02ERR
055300941011     C                   CALL      'XSRDA8'
055400941011     C                   PARM                    WLBDA8
055500941011     C                   MOVEL     G02DAT        VDTG
055600941011     C                   MOVE      GCPNGC        VNGC
055700960123     C                   MOVE      GCPNGC        V6CNGC
055800941011     C* FIL. CHE HA APERTO LA GIACENZA
055900941011     C     GCPFGC        IFNE      *ZEROS
056000960123     C                   MOVE      GCPFGC        V6CFGC
056100941011     C                   MOVE      GCPFGC        VFGC
056200941011     C     VFGC          CHAIN     AZORG01L                           21
056300941011     C  N21ORGFVA        COMP      '*'                                    21
056400941011     C   21              MOVEL     *ALL'?'       DFGC
056500941011     C  N21              MOVEL     ORGDES        DFGC
056600941011     C                   END
056700941011     C*
056800941011     C* DATA ULTIMA RIAPERTURA
056900050408     c                   if        gcpriap = 'R'
057000941011     C                   Z-ADD     GCPDUR        G02INV
057100941011     C                   Z-ADD     0             G02DAT
057200941011     C                   MOVEL     '3'           G02ERR
057300941011     C                   CALL      'XSRDA8'
057400941011     C                   PARM                    WLBDA8
057500941011     C                   MOVEL     G02DAT        VDIG
057600050408     c                   endif
057700051011     C* appuntamento
057800051011     C                   MOVEL     GCPflr        dgcpflr
057900051011     c                   movel     D§GCPAPP      v1capp
058000941011     C* DATA DISPOSIZIONI MITTENTE
058100941011     C                   Z-ADD     GCPDDM        G02INV
058200941011     C                   Z-ADD     0             G02DAT
058300941011     C                   MOVEL     '3'           G02ERR
058400941011     C                   CALL      'XSRDA8'
058500941011     C                   PARM                    WLBDA8
058600941011     C                   MOVEL     G02DAT        VDDM
058700941011     C* DATA COMUNICAZIONE INTERNA = DATA DIST. PAD. SE IN ARRIVO
058800941011     C                   Z-ADD     GCPDXD        G02INV
058900941011     C                   Z-ADD     0             G02DAT
059000941011     C                   MOVEL     '3'           G02ERR
059100941011     C                   CALL      'XSRDA8'
059200941011     C                   PARM                    WLBDA8
059300941011     C                   MOVEL     G02DAT        VDXD
059400941011     C* DATA ESECUZIONE DISPOSIZIONI
059500941011     C     GCPDED        IFGT      *ZEROS
059600941011     C                   Z-ADD     GCPDED        G02INV
059700941011     C                   Z-ADD     0             G02DAT
059800941011     C                   MOVEL     '3'           G02ERR
059900941011     C                   CALL      'XSRDA8'
060000941011     C                   PARM                    WLBDA8
060100941011     C                   MOVEL     G02DAT        V1CDED
060200941011     C                   END
060300941011     C* NR. E DATA COMUNICAZ.
060400941011     C                   MOVE      GCPNRC        VNRC
060500941011     C* LUOGO DEPOSITO MERCE
060600941011     C                   MOVE      GCPCCR        VCCR
060700941011     C                   MOVEL     *BLANKS       DCCR
060800941011     C     GCPCCR        IFNE      *ZEROS
060900941011     C                   MOVE      VCCR          ORGFIL
061000941011     C     ORGFIL        CHAIN     AZORG01L                           21
061100941011     C  N21ORGFVA        COMP      '*'                                    21
061200941011     C   21              MOVEL     *ALL'?'       DCCR
061300941011     C  N21              MOVEL     *BLANK        DCCR
061400941011     C  N21              MOVEL     ORGDES        DCCR
061500941011     C     *IN21         IFEQ      '1'
061600941011     C                   MOVEL     '2B'          TBLCOD
061700941011     C                   MOVEL     *BLANK        TBLKEY
061800941011     C                   MOVEL     VCCR          TBLKEY
061900941011     C     KTAB          CHAIN     TABEL                              21
062000941011     C   21              MOVEL     *ALL'?'       DCCR
062100941011     C  N21              MOVEL     TBLUNI        DCCR
062200941011     C                   END
062300941011     C                   END
062400941011     C*
062500941011     C                   MOVEL     GCPDIS        VFRI
062600941011     C                   MOVEL     '2D'          TBLCOD
062700941011     C                   MOVEL     *BLANK        TBLKEY
062800941011     C                   MOVEL     VFRI          TBLKEY
062900941011     C     KTAB          CHAIN     TABEL                              21
063000941011     C   21              MOVEL     *ALL'?'       DFRI
063100941011     C  N21              MOVEL     TBLUNI        DS2D
063200941011     C  N21              MOVEL     §2DDES        DFRI
063300941011     C*
063400941011     C                   MOVEL     GCPASG        VASG
063500941011     C                   MOVEL     GCPPSG        VPSG
063600941011     C     VPSG          IFNE      ' '
063700941011     C                   MOVEL     '2E'          TBLCOD
063800941011     C                   MOVEL     *BLANK        TBLKEY
063900941011     C                   MOVEL     VPSG          TBLKEY
064000941011     C     KTAB          CHAIN     TABEL                              21
064100941011     C   21              MOVEL     *ALL'?'       DPSG
064200941011     C  N21              MOVEL     TBLUNI        DS2E
064300941011     C  N21              MOVEL     §2EDES        DPSG
064400941011     C                   END
064500941011     C*
064600941011     C     GCPNPC        IFNE      *BLANK
064700941011     C                   MOVEL     GCPNPC        VNPC
064800941011     C     GCPNPC        COMP      'A'                                    21
064900941011     C   21              MOVEL     'ASSEGNAT'    DNPC
065000941011     C   21              MOVE      'O '          DNPC
065100941011     C  N21              MOVEL     'FRANCO  '    DNPC
065200941011     C  N21              MOVE      '  '          DNPC
065300941011     C                   ELSE
065400941011     C                   MOVEL     *BLANKS       VNPC
065500941011     C                   MOVE      *BLANKS       DNPC
065600941011     C                   END
065700941011     C                   MOVEL     GCPVCS        VVCS
065800941011     C                   MOVEL     GCPCAS        VNCA
065900941011     C                   MOVEL     GCPVCA        V1CNDV
066000941011     C                   MOVEL     GCPAPC        VAPC
066100941011     C                   MOVEL     GCPPPC        VPPC
066200060808     C                   clear                   V1cchim
066300060808     c                   if        GCPchiaut = 'N'
066400060808     C                   MOVE      'S'           V1cchim
066500060808     c                   endif
066600941011     C     VPPC          IFNE      *BLANK
066700941011     C                   MOVEL     '2E'          TBLCOD
066800941011     C                   MOVEL     *BLANK        TBLKEY
066900941011     C                   MOVEL     VPPC          TBLKEY
067000941011     C     KTAB          CHAIN     TABEL                              21
067100941011     C   21              MOVEL     *ALL'?'       DPPC
067200941011     C  N21              MOVEL     TBLUNI        DS2E
067300941011     C  N21              MOVEL     §2EDES        DPPC
067400941011     C                   END
067500900423     C*
067600960208     C                   Z-ADD     20            §FAS
067700050216     C     KNOT          SETLL     tignp01L                               21
067800960208     C   21              SETON                                        94
067900960208     C*
068000900426     C                   ENDSR
068100911108     C*---------------*
068200911108     C* GESTIONE NOTE
068300911108     C*---------------*
068400911108     C     GESNOT        BEGSR
068500911122     C                   Z-ADD     20            §FAS
068600911108     C                   EXSR      PULIZ
068700050216     C     KNOT          SETLL     tignp01L
068800911108     C     *IN21         DOUEQ     '1'
068900050216     C     KNOT          READE     tignp01L                               21
069000911108     C     *IN21         IFEQ      '0'
069100911125     C                   MOVEL     GNPDMC        VDMC1
069200911108     C                   MOVEL     GNPTPN        VTPN
069300911108     C                   MOVEL     GNPNPR        VNPR
069400911108     C                   ADD       1             NRR               3 0
069500941011     C                   WRITE     LG10S02
069600911108     C                   ELSE
069700960208     C     NRR           DIV       10            WRESTO            2 0
069800960208     C                   MVR                     §NR               2 0
069900960208     C     10            SUB       §NR           §NR
070000911108     C                   DO        §NR
070100911125     C                   MOVEL     *BLANK        VDMC1
070200911108     C                   MOVEL     *BLANK        VTPN
070300911108     C                   ADD       1             VNPR
070400911108     C                   ADD       1             NRR
070500941011     C                   WRITE     LG10S02
070600911108     C                   END
070700911108     C                   END
070800911108     C                   END
070900960208     C                   Z-ADD     1             NR2KEY
071000911108     C*
071100941011     C     *INKL         DOWEQ     '0'
071200941011     C                   WRITE     LG10Z02
071300941011     C                   EXFMT     LG10C02
071400941011     C                   END
071500941011     C*
071600941011     C                   ENDSR
071700911108     C*----------------*
071800911108     C* PULIZIA SUBFILE
071900911108     C*----------------*
072000911108     C     PULIZ         BEGSR
072100911108     C                   SETON                                        70
072200941011     C                   WRITE     LG10C02
072300911108     C                   Z-ADD     0             NRR
072400911108     C                   SETOFF                                       70
072500911108     C                   ENDSR
072600941011** STA
072700941011PRIMA CHIUSURA   :
072800941011DOPO  CHIUSURA   :
072900941011 NON MANUTENZIONABILE !!
073000941011** ERR
073100941011Bolla inesistente                                                     01

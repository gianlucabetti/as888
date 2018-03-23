000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200970122     H* TNTA32R *----------------------------------------------------*
000300970117     H*   CREAZIONE DATI TIPO OFFERTA CON COPIA DA TARIFFA/OFFERTA   *
000400000000     H*--------------------------------------------------------------*
000500011002     FTEETC01L  IF A E           K DISK
000600011002     FTEETT01L  IF A E           K DISK
000700011002     FTEETS01L  IF A E           K DISK
000701061009     FTEETS11L  IF A E           K DISK
000702061006     F                                     RENAME(TEETS000:TEETS001)
000800011026     FTEETCX1L  UF A E           K DISK
000900011026     F                                     RENAME(TEETC000:TEETC111)
001000011026     FTEETTX1L  UF A E           K DISK
001100011026     F                                     RENAME(TEETT000:TEETT111)
001200011026     FTEETSX1L  UF A E           K DISK
001300011026     F                                     RENAME(TEETS000:TEETS111)
001301061009     FTEETSY1L  UF A E           K DISK
001302061006     F                                     RENAME(TEETS000:TEETSY11)
001400970117     D*
001500970117     D* PARAMETRI RICEVUTI DAL PGM CHIAMANTE        - TNTA01R2 -
001600970122     D DSTA32        E DS                  EXTNAME(TNTA32DS)
001700970409     D WLBDAT          DS                  INZ
001800970409     D  G02DAT                 1      8  0
001900970409     D  G02INV                 9     16  0
002000970409     D  G02ERR                17     17
002100970409     D  G02TGI                18     22  0
002200970117     D*
002300921013     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002400000000     D KPJBA         E DS
002500011029     D*-------------------
002600011029     D* CONVERSIONI IMPORTI
002700011029     D*-------------------
002800011029     D YEUR          E DS                  EXTNAME(YEURCODS)
002900921015     C*****************************************************************
003000921015     C* RIEPILOGO INDICATORI
003100921015     C*****************************************************************
003200970117     C* 30    - DI COMODO
003300921015     C*****************************************************************
003400000000     C     *ENTRY        PLIST
003500000000     C                   PARM                    KPJBA
003600970122     C                   MOVEL     KPJBU         DSTA32
003700970409     C                   TIME                    W0140            14 0
003800970409     C* UDATE IN GGMMAAAA
003900970409     C                   MOVE      W0140         WDTGIO            8 0
004000970409     C*
004100970409     C* UDATE IN AAAAMMGG
004200970409     C                   Z-ADD     WDTGIO        G02DAT
004300970409     C                   MOVEL     *BLANK        G02ERR
004400970409     C                   CALL      'XSRDA8'
004500970409     C                   PARM                    WLBDAT
004600970409     C                   MOVEL     G02INV        DATEU             8 0
004700970117     C*
004800000000     C*---------------------------------------------------------------*
004900970122     C* TIPO LANCIO: D32TLA = "C" -           CHIUDO CON LR
005000970122     C*              D32TLA = "L" - ELABORO E CHIUDO CON LR
005100970122     C*              D32TLA = " " - ELABORO E CHIUDO IN  RETRN
005200970417    1C     D32TLA        IFNE      'C'
005300970117     C*
005400970117     C****  C O P I A    D A    T A R I F F A  *****
005500970117     C*
005600011002     C****  DUPLICO TEETC  ****
005700011002     C     KTAR          SETLL     TEETC000
005800011002     C     KTAR          READE     TEETC000                               30
005900970115     C*
006000970117    2C     *IN30         DOWEQ     *OFF
006100970117     C* ESCLUDO ANNULLATI
006200970117    3C     ETCATB        IFEQ      ' '
006300061009     C                   MOVEL     'S'           ETCDSF
006400091127     C                   MOVEL     'O'           ETCCTO
006500970122     C                   Z-ADD     D32NRV        ETCKSC
006600970122     C                   Z-ADD     D32CT2        ETCCTR
006700970409     C                   Z-ADD     DATEU         ETCDIR
006800091125     C                   z-add     d32pr2        ETCPRG
006900970117     C                   CLEAR                   ETCFTR
007000970117     C                   CLEAR                   ETCDTR
007100970418     C                   CLEAR                   ETCDUV
007200011029     C* Verifico se occorre convertire i campi importo
007300011029     C     D32DIV        IFNE      D32DIN
007400011029     C                   EXSR      CNVETC
007500011029     C                   ENDIF
007600011029     C*
007700011026     C* Salvo la linea di partenza
007800011026     C                   EVAL      WLNP = ETCLNP
008400970122     C* SE D32LNP > 0 SIGNIFICA CHE STO COPIANDO L'OFFERTA DA TARIFFA
008500970117     C*   DI CARTELLO QUINDI COPIO SOLO LA LINEA PARTENZA INTERESSATA
008600011106     C     KTARCW        CHAIN     TEETCX1L
008700970122    4C     D32LNP        IFEQ      0
008800011026     C                   IF        %found(TEETCX1L)
008900011026     C                   UPDATE    TEETC111
009000011026     C                   ELSE
009100011026     C                   WRITE     TEETC111                             97
009200011026     C                   ENDIF
009300970117   X4C                   ELSE
009400011031    5C     D32LNP        IFEQ      WLNP
009500011026     C                   IF        %found(TEETCX1L)
009600011026     C                   UPDATE    TEETC111
009700011026     C                   ELSE
009800011026     C                   WRITE     TEETC111                             97
009900011026     C                   ENDIF
010000970117    5C                   ENDIF
010100970117    4C                   ENDIF
010200970117    3C                   ENDIF
010300970117     C*
010400011002     C     KTAR          READE     TEETC000                               30
010500970117    2C                   ENDDO
010600921016     C*
010700011002     C****  DUPLICO TEETT  ****
010800011002     C     KTAR          SETLL     TEETT000
010900011002     C     KTAR          READE     TEETT000                               30
011000970117     C*
011100970117    2C     *IN30         DOWEQ     *OFF
011200970117     C* ESCLUDO ANNULLATI
011300970117    3C     ETTATB        IFEQ      ' '
011400061009     C                   MOVEL     'S'           ETTDSF
011500980526     C                   MOVEL     'O'           ETTCTO
011600970122     C                   Z-ADD     D32NRV        ETTKSC
011700970122     C                   Z-ADD     D32CT2        ETTCTR
011800970409     C                   Z-ADD     DATEU         ETTDIR
011901091125     C                   z-add     d32pr2        ETTPRG
012000970117     C                   CLEAR                   ETTFTR
012100970117     C                   CLEAR                   ETTDTR
012200970418     C                   CLEAR                   ETTDUV
012300011106     C* Salvo la linea di partenza e gli altri campi chiave
012400011031     C                   EVAL      WLNP = ETTLNP
012500011106     C                   EVAL      WFIE = ETTFIE
012600011106     C                   EVAL      WCTS = ETTCTS
013100970117     C*
013200970122     C* SE D32LNP > 0 SIGNIFICA CHE STO COPIANDO L'OFFERTA DA TARIFFA
013300970117     C*   DI CARTELLO QUINDI COPIO SOLO LA LINEA PARTENZA INTERESSATA
013400011106     C     KTARTW        CHAIN     TEETTX1L
013500970122    4C     D32LNP        IFEQ      0
013600011026     C                   IF        %found(TEETTX1L)
013700011026     C                   UPDATE    TEETT111
013800011026     C                   ELSE
013900011026     C                   WRITE     TEETT111                             97
014000011026     C                   ENDIF
014100970117   X4C                   ELSE
014200011031    5C     D32LNP        IFEQ      WLNP
014300011026     C                   IF        %found(TEETTX1L)
014400011026     C                   UPDATE    TEETT111
014500011026     C                   ELSE
014600011026     C                   WRITE     TEETT111                             97
014700011026     C                   ENDIF
014800970117    5C                   ENDIF
014900970117    4C                   ENDIF
015000970117    3C                   ENDIF
015100970117     C*
015200011002     C     KTAR          READE     TEETT000                               30
015300970117    2C                   ENDDO
015400921012     C*
015500061006     C****  DUPLICO TEETS00****
015600011002     C     KTAR          SETLL     TEETS000
015700011002     C     KTAR          READE     TEETS000                               30
015800970117     C*
015900970117    2C     *IN30         DOWEQ     *OFF
016000970117     C* ESCLUDO ANNULLATI
016100970117    3C     ETSATB        IFEQ      ' '
016200061006     C                   MOVEL     'S'           ETSDSF
016300970117     C                   MOVEL     'O'           ETSCTO
016400970122     C                   Z-ADD     D32NRV        ETSKSC
016500970122     C                   Z-ADD     D32CT2        ETSCTR
016600970409     C                   Z-ADD     DATEU         ETSDIR
016701091125     C                   z-add     d32pr2        ETSPRG
016800970117     C                   CLEAR                   ETSFTR
016900970117     C                   CLEAR                   ETSDTR
017000970418     C                   CLEAR                   ETSDUV
017100011029     C* Verifico se occorre convertire i campi importo
017200011029     C     D32DIV        IFNE      D32DIN
017300011029     C                   EXSR      CNVETS
017400011029     C                   ENDIF
017500011029     C*
017600011106     C* Salvo la linea di partenza e gli altri campi chiave
017700011031     C                   EVAL      WLNP = ETSLNP
017800011106     C                   EVAL      WSGL = ETSSGL
018300970117     C*
018400970122     C* SE D32LNP > 0 SIGNIFICA CHE STO COPIANDO L'OFFERTA DA TARIFFA
018500970117     C*   DI CARTELLO QUINDI COPIO SOLO LA LINEA PARTENZA INTERESSATA
018600011106     C     KTARSW        CHAIN     TEETSX1L
018700970122    4C     D32LNP        IFEQ      0
018800011026     C                   IF        %found(TEETSX1L)
018900011026     C                   UPDATE    TEETS111
019000011026     C                   ELSE
019100011026     C                   WRITE     TEETS111                             97
019200011026     C                   ENDIF
019300970117   X4C                   ELSE
019400011031    5C     D32LNP        IFEQ      WLNP
019500011026     C                   IF        %found(TEETSX1L)
019600011026     C                   UPDATE    TEETS111
019700011026     C                   ELSE
019800011026     C                   WRITE     TEETS111                             97
019900011026     C                   ENDIF
020000970117    5C                   ENDIF
020100970117    4C                   ENDIF
020200970117    3C                   ENDIF
020300970117     C*
020400011002     C     KTAR          READE     TEETS000                               30
020500970117    2C                   ENDDO
020501061006     C****  DUPLICO TEETS10****
020502061006     C     KTAR          SETLL     TEETS001
020503061006     C     KTAR          READE     TEETS001                               30
020504061006     C*
020505061006    2C     *IN30         DOWEQ     *OFF
020506061006     C* ESCLUDO ANNULLATI
020507061009    3C     ETSATB        IFEQ      ' '
020508061009     C                   MOVEL     'S'           ETSDSF
020509061009     C                   MOVEL     'O'           ETSCTO
020510061009     C                   Z-ADD     D32NRV        ETSKSC
020511061009     C                   Z-ADD     D32CT2        ETSCTR
020512061009     C                   Z-ADD     DATEU         ETSDIR
020513091125     C                   z-add     d32pr2        ETSPRG
020514061009     C                   CLEAR                   ETSFTR
020515061009     C                   CLEAR                   ETSDTR
020516061009     C                   CLEAR                   ETSDUV
020517061006     C* Verifico se occorre convertire i campi importo
020518061006     C     D32DIV        IFNE      D32DIN
020519061009     C                   EXSR      CNVETS
020520061006     C                   ENDIF
020521061006     C*
020522061006     C* Salvo la linea di partenza e gli altri campi chiave
020523061009     C                   EVAL      WLNP = ETSLNP
020524061009     C                   EVAL      WSGL = ETSSGL
020529061006     C*
020530061006     C* SE D32LNP > 0 SIGNIFICA CHE STO COPIANDO L'OFFERTA DA TARIFFA
020531061006     C*   DI CARTELLO QUINDI COPIO SOLO LA LINEA PARTENZA INTERESSATA
020532061006     C     KTARSW        CHAIN     TEETSY1L
020533061006    4C     D32LNP        IFEQ      0
020534061006     C                   IF        %found(TEETSY1L)
020535061006     C                   UPDATE    TEETSY11
020536061006     C                   ELSE
020537061006     C                   WRITE     TEETSY11                             97
020538061006     C                   ENDIF
020539061006   X4C                   ELSE
020540061006    5C     D32LNP        IFEQ      WLNP
020541061006     C                   IF        %found(TEETSY1L)
020542061006     C                   UPDATE    TEETSY11
020543061006     C                   ELSE
020544061006     C                   WRITE     TEETSY11                             97
020545061006     C                   ENDIF
020546061006    5C                   ENDIF
020547061006    4C                   ENDIF
020548061006    3C                   ENDIF
020549061006     C*
020550061006     C     KTAR          READE     TEETS001                               30
020551061006    2C                   ENDDO
020600970117     C*
020700970417    1C                   ENDIF
020800970117     C*
020900970117     C* CHIUSURA PGM
021000970417    1C     D32TLA        IFEQ      ' '
021100970117     C                   RETURN
021200970417   X1C                   ELSE
021300970117     C                   SETON                                        LR
021400970417    1C                   ENDIF
021500970117     C*
021600011029     C*------------------------------------------------------------------------*
021700011029     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
021800011029     C*------------------------------------------------------------------------*
021900011029     C     CNVEUR        BEGSR
022000011029     C*
022100011029     C* AZZERA VARIABILI DI LAVORO
022200011029     C                   Z-ADD     *ZEROS        WIMO             30 9
022300011029     C                   MOVEL     '9'           NDEC              1
022400011029     C*
022500011029IF  1C     WIMI          IFNE      *ZEROS
022600011029     C                   CLEAR                   YEUR
022700011029     C                   MOVEL     D32DIV        YECDVI
022800011029     C                   Z-ADD     WIMI          YECIMI
022900011029     C                   MOVEL     D32DIN        YECDVO
023000011029     C                   MOVEL     NDEC          YECDCO
023100011029     C                   MOVEL     *BLANKS       YECTAR
023200011029     C                   CALL      'YEURCO'
023300011029     C                   PARM                    YEUR
023400011029IF  2C     YECESI        IFEQ      ' '
023500011029     C                   Z-ADD     YECIMO        WIMO
023600011029X   2C                   ELSE
023700011029     C                   Z-ADD     WIMI          WIMO
023800011029E   2C                   ENDIF
023900011029E   1C                   ENDIF
024000011029     C*
024100011029     C                   ENDSR
024200011029     C*------------------------------------------------------------------------*
024300011029     C* CNVETC - CONVERTE I CAMPI DI TEETC IN DIVISA DI CALCOLO DEL CE
024400011029     C*------------------------------------------------------------------------*
024500011029     C     CNVETC        BEGSR
024600011029     C*
024700011029     C* ARROTONDAMENTO
024800011029     C                   IF        ETCIAR > *zeros
024900011029     C                   IF        D32DIN = 'EUR'
025000011029     C                   Z-ADD     0,1           ETCIAR
025100011029     C                   ELSE
025200011029     C                   Z-ADD     100           ETCIAR
025300011029     C                   ENDIF
025400011029     C                   ENDIF
025500011029     C* CONTRASSEGNO
025600011029     C                   Z-ADD     ETCCAS        WIMI             30 9
025700011029     C                   EXSR      CNVEUR
025800011029     C                   Z-ADD     WIMO          ETCCAS
025900011029     C* ASSICURAZIONE
026000011029     C                   Z-ADD     ETCASC        WIMI             30 9
026100011029     C                   EXSR      CNVEUR
026200011029     C                   Z-ADD     WIMO          ETCASC
026300011029     C*
026400011029     C                   ENDSR
026500011029     C*------------------------------------------------------------------------*
026600011029     C* CNVETS - CONVERTE I CAMPI DI TEETS IN DIVISA DI CALCOLO DEL CE
026700011029     C*------------------------------------------------------------------------*
026800011029     C     CNVETS        BEGSR
026900011029     C*
027000011029     C* INOLTRO
027100011029     C                   Z-ADD     ETSINL        WIMI             30 9
027200011029     C                   EXSR      CNVEUR
027300011029     C                   Z-ADD     WIMO          ETSINL
027400011029     C*
027500011029     C                   ENDSR
027600011029     C*
027700970117     C*--- SR INIZIALE -----------------------------------------------*
027800970117     C     *INZSR        BEGSR
027900970117     C****
028000970117     C** KLIST
028100970117     C****
028200011026     C*
028300011026     C     *LIKE         DEFINE    ETCLNP        WLNP
028500011106     C     *LIKE         DEFINE    ETTFIE        WFIE
028600011106     C     *LIKE         DEFINE    ETTCTS        WCTS
028700011106     C     *LIKE         DEFINE    ETSSGL        WSGL
028800011026     C*
028900011002     C* ACCESSO TEETC01L / TEETT01L / TEETS01L
029000970117     C     KTAR          KLIST
029100980526     C                   KFLD                    D32DSF
029200980526     C                   KFLD                    D32CTO
029300970122     C                   KFLD                    D32KSC
029400970122     C                   KFLD                    D32CTR
029500970122     C                   KFLD                    D32PRG
029600011106     C* SCRITTURA TEETC01L
029700011106     C     KTARCW        KLIST
029800011026     C                   KFLD                    D32DSF
029900011026     C                   KFLD                    D32CTO
030000011031     C                   KFLD                    D32NRV
030100011031     C                   KFLD                    D32CT2
030200091125     C                   KFLD                    d32pr2
030300011026     C                   KFLD                    WLNP
030400011106     C* SCRITTURA TEETT01L
030500011106     C     KTARTW        KLIST
030600011106     C                   KFLD                    D32DSF
030700011106     C                   KFLD                    D32CTO
030800011106     C                   KFLD                    D32NRV
030900011106     C                   KFLD                    D32CT2
031000091125     C                   KFLD                    d32pr2
031100011106     C                   KFLD                    WLNP
031200011106     C                   KFLD                    WFIE
031300011106     C                   KFLD                    WCTS
031400011106     C* SCRITTURA TEETS01L
031500011106     C     KTARSW        KLIST
031600011106     C                   KFLD                    D32DSF
031700011106     C                   KFLD                    D32CTO
031800011106     C                   KFLD                    D32NRV
031900011106     C                   KFLD                    D32CT2
032000091125     C                   KFLD                    d32pr2
032100011106     C                   KFLD                    WLNP
032200011106     C                   KFLD                    WSGL
032300970417     C*
032400970117     C                   ENDSR

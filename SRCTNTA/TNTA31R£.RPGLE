000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200970122     H* TNTA31R *----------------------------------------------------*
000300970114     H*               - DUPLICAZIONE DATI TIPO -       BATCH         *
000400000000     H*--------------------------------------------------------------*
000500011026     FTEETC01L  UF   E           K DISK
000600011026     FTEETT01L  UF   E           K DISK
000700011026     FTEETS01L  UF   E           K DISK
000701061009     FTEETS11L  UF   E           K DISK
000702061006     F                                     RENAME(TEETS000:TEETS001)
000800011026     FTEETC00F  UF A E           K DISK
000900011026     F                                     RENAME(TEETC000:TEETCXXX)
001000011026     FTEETT00F  UF A E           K DISK
001100011026     F                                     RENAME(TEETT000:TEETTXXX)
001200011026     FTEETS00F  UF A E           K DISK
001300011026     F                                     RENAME(TEETS000:TEETSXXX)
001301061009     FTEETS10F  UF A E           K DISK
001302061006     F                                     RENAME(TEETS000:TEETSXX1)
001400011026     F*
001500970421     D WLBDAT          DS                  INZ
001600970421     D  G02DAT                 1      8  0
001700970421     D  G02INV                 9     16  0
001800970421     D  G02ERR                17     17
001900970421     D  G02TGI                18     22  0
002000940621     D*
002100970122     D* PARAMETRI RICEVUTI DAL PGM CHIAMANTE      - TNTA31R -
002200970122     D DSTA31        E DS                  EXTNAME(TNTA31DS)
002300940621     D*
002400000000     D KPJBA         E DS
002500011026     D*-------------------
002600011026     D* CONVERSIONI IMPORTI
002700011026     D*-------------------
002800011026     D YEUR          E DS                  EXTNAME(YEURCODS)
002900970115     C*****************************************************************
003000940616     C* RIEPILOGO INDICATORI
003100940616     C*****************************************************************
003200970115     C* 30/32 - DI COMODO
003300940616     C*****************************************************************
003400000000     C     *ENTRY        PLIST
003500000000     C                   PARM                    KPJBA
003600970122     C                   MOVEL     KPJBU         DSTA31
003700970115     C*
003800000000     C*---------------------------------------------------------------*
003900970122     C* TIPO LANCIO: D31TLA = "C" -           CHIUDO CON LR
004000970122     C*              D31TLA = "L" - ELABORO E CHIUDO CON LR
004100970122     C*              D31TLA = " " - ELABORO E CHIUDO IN  RETRN
004200970122    0C     D31TLA        IFNE      'C'
004300970421     C*
004400970421     C                   TIME                    W0140            14 0
004500970421     C* UDATE IN GGMMAAAA
004600970421     C                   MOVE      W0140         WDTGIO            8 0
004700970421     C*
004800970421     C* UDATE IN AAAAMMGG
004900970421     C                   Z-ADD     WDTGIO        G02DAT
005000970421     C                   MOVEL     *BLANK        G02ERR
005100970421     C                   CALL      'XSRDA8'
005200970421     C                   PARM                    WLBDAT
005300970421     C                   MOVEL     G02INV        DATEU             8 0
005400970421     C*
005500970114     C*
005600011026     C*****    D U P L I C A    D I    T E E T C    *****
005700011026     C*
005800011026     C* PRIMA DELETO PREVENTIVAMENTE IL NUOVO
005900011026     C     KETXN         SETLL     TEETC01L
006000011026     C     KETXN         READE     TEETC01L                               30
006100011026     C*
006200011026    1C     *IN30         DOWEQ     *OFF
006300011026     C                   DELETE    TEETC01L
006400011026     C     KETXN         READE     TEETC01L                               30
006500011026    1C                   ENDDO
006600970114     C*
006700011026     C* QUINDI PROCEDO CON LA DUPLICA
006800011026     C     KETXO         SETLL     TEETC01L
006900011026     C     KETXO         READE     TEETC01L                               30
007000940616     C*
007100940616    1C     *IN30         DOWEQ     *OFF
007200900423     C* ESCLUDO ANNULLATI
007300970114    2C     ETCATB        IFEQ      ' '
007400900503     C*
007500970122     C                   Z-ADD     D31KSN        ETCKSC
007600970122     C                   Z-ADD     D31CTN        ETCCTR
007700970122     C                   Z-ADD     D31PRN        ETCPRG
007800970421     C                   Z-ADD     DATEU         ETCDIR
007900970421     C                   CLEAR                   ETCDUV
008000970421     C                   CLEAR                   ETCFTR
008100970114     C                   CLEAR                   ETCDTR
008200011026     C* Verifico se occorre convertire i campi importo
008300011026     C     D31DIV        IFNE      D31DIN
008400011026     C                   EXSR      CNVETC
008500011026     C                   ENDIF
008600011026     C*
008700011026     C                   WRITE     TEETCXXX
008800940616    2C                   ENDIF
008900921102     C*
009000011026     C     KETXO         READE     TEETC01L                               30
009100940616    1C                   ENDDO
009200921029     C*
009300940616     C*
009400011026     C*****    D U P L I C A    D I    T E E T T    *****
009500011026     C*
009600011026     C* PRIMA DELETO PREVENTIVAMENTE IL NUOVO
009700011026     C     KETXN         SETLL     TEETT01L
009800011026     C     KETXN         READE     TEETT01L                               30
009900011026     C*
010000011026    1C     *IN30         DOWEQ     *OFF
010100011026     C                   DELETE    TEETT01L
010200011026     C     KETXN         READE     TEETT01L                               30
010300011026    1C                   ENDDO
010400970114     C*
010500011026     C* QUINDI PROCEDO CON LA DUPLICA
010600011026     C     KETXO         SETLL     TEETT01L
010700011026     C     KETXO         READE     TEETT01L                               30
010800970114     C*
010900970114    1C     *IN30         DOWEQ     *OFF
011000970114     C* ESCLUDO ANNULLATI
011100970114    2C     ETTATB        IFEQ      ' '
011200970114     C*
011300970122     C                   Z-ADD     D31KSN        ETTKSC
011400970122     C                   Z-ADD     D31CTN        ETTCTR
011500970122     C                   Z-ADD     D31PRN        ETTPRG
011600970421     C                   Z-ADD     DATEU         ETTDIR
011700970421     C                   CLEAR                   ETTDUV
011800970114     C                   CLEAR                   ETTFTR
011900970114     C                   CLEAR                   ETTDTR
012000011026     C                   WRITE     TEETTXXX
012100970114    2C                   ENDIF
012200970114     C*
012300011026     C     KETXO         READE     TEETT01L                               30
012400970114    1C                   ENDDO
012500970114     C*
012600970114     C*
012700011026     C*****    D U P L I C A    D I    T E E T S    *****
012800011026     C*
012900011026     C* PRIMA DELETO PREVENTIVAMENTE IL NUOVO
013000011026     C     KETXN         SETLL     TEETS01L
013100011026     C     KETXN         READE     TEETS01L                               30
013200011026     C*
013300011026    1C     *IN30         DOWEQ     *OFF
013400011026     C                   DELETE    TEETS01L
013500011026     C     KETXN         READE     TEETS01L                               30
013600011026    1C                   ENDDO
013700970114     C*
013800011026     C* QUINDI PROCEDO CON LA DUPLICA
013900011026     C     KETXO         SETLL     TEETS01L
014000011026     C     KETXO         READE     TEETS01L                               30
014100970114     C*
014200970114    1C     *IN30         DOWEQ     *OFF
014300970114     C* ESCLUDO ANNULLATI
014400970114    2C     ETSATB        IFEQ      ' '
014500970114     C*
014600970122     C                   Z-ADD     D31KSN        ETSKSC
014700970122     C                   Z-ADD     D31CTN        ETSCTR
014800970122     C                   Z-ADD     D31PRN        ETSPRG
014900970421     C                   Z-ADD     DATEU         ETSDIR
015000970421     C                   CLEAR                   ETSDUV
015100970114     C                   CLEAR                   ETSFTR
015200970114     C                   CLEAR                   ETSDTR
015300011026     C* Verifico se occorre convertire i campi importo
015400011026     C     D31DIV        IFNE      D31DIN
015500011026     C                   EXSR      CNVETS
015600011026     C                   ENDIF
015700011026     C*
015800011026     C                   WRITE     TEETSXXX
015900970114    2C                   ENDIF
016000970114     C*
016100011026     C     KETXO         READE     TEETS01L                               30
016200970114    1C                   ENDDO
016201061006     C* PRIMA DELETO PREVENTIVAMENTE IL NUOVO
016202061006     C     KETXN         SETLL     TEETS11L
016203061006     C     KETXN         READE     TEETS11L                               30
016204061006     C*
016205061006    1C     *IN30         DOWEQ     *OFF
016206061006     C                   DELETE    TEETS11L
016207061006     C     KETXN         READE     TEETS11L                               30
016208061006    1C                   ENDDO
016209061006     C*
016210061006     C* QUINDI PROCEDO CON LA DUPLICA
016211061006     C     KETXO         SETLL     TEETS11L
016212061006     C     KETXO         READE     TEETS11L                               30
016213061006     C*
016214061006    1C     *IN30         DOWEQ     *OFF
016215061006     C* ESCLUDO ANNULLATI
016216061009    2C     ETSATB        IFEQ      ' '
016217061006     C*
016218061009     C                   Z-ADD     D31KSN        ETSKSC
016219061009     C                   Z-ADD     D31CTN        ETSCTR
016220061009     C                   Z-ADD     D31PRN        ETSPRG
016221061009     C                   Z-ADD     DATEU         ETSDIR
016222061009     C                   CLEAR                   ETSDUV
016223061009     C                   CLEAR                   ETSFTR
016224061009     C                   CLEAR                   ETSDTR
016225061006     C* Verifico se occorre convertire i campi importo
016226061006     C     D31DIV        IFNE      D31DIN
016227061009     C                   EXSR      CNVETS
016228061006     C                   ENDIF
016229061006     C*
016230061006     C                   WRITE     TEETSXX1
016231061006    2C                   ENDIF
016232061006     C*
016233061006     C     KETXO         READE     TEETS11L                               30
016234061006    1C                   ENDDO
016300970114     C*
016400970115    0C                   ENDIF
016500941122     C*
016600970117     C* CHIUSURA PGM
016700970122    0C     D31TLA        IFEQ      ' '
016800970117     C                   RETURN
016900970117   X0C                   ELSE
017000970117     C                   SETON                                        LR
017100970117    0C                   ENDIF
017200011026     C*------------------------------------------------------------------------*
017300011026     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
017400011026     C*------------------------------------------------------------------------*
017500011026     C     CNVEUR        BEGSR
017600011026     C*
017700011026     C* AZZERA VARIABILI DI LAVORO
017800011026     C                   Z-ADD     *ZEROS        WIMO             30 9
017900011026     C                   MOVEL     '9'           NDEC              1
018000011026     C*
018100011026IF  1C     WIMI          IFNE      *ZEROS
018200011026     C                   CLEAR                   YEUR
018300011026     C                   MOVEL     D31DIV        YECDVI
018400011026     C                   Z-ADD     WIMI          YECIMI
018500011026     C                   MOVEL     D31DIN        YECDVO
018600011026     C                   MOVEL     NDEC          YECDCO
018700011026     C                   MOVEL     *BLANKS       YECTAR
018800011026     C                   CALL      'YEURCO'
018900011026     C                   PARM                    YEUR
019000011026IF  2C     YECESI        IFEQ      ' '
019100011026     C                   Z-ADD     YECIMO        WIMO
019200011026X   2C                   ELSE
019300011026     C                   Z-ADD     WIMI          WIMO
019400011026E   2C                   ENDIF
019500011026E   1C                   ENDIF
019600011026     C*
019700011026     C                   ENDSR
019800011026     C*------------------------------------------------------------------------*
019900011026     C* CNVETC - CONVERTE I CAMPI DI TEETC IN DIVISA DI CALCOLO DEL CE
020000011026     C*------------------------------------------------------------------------*
020100011026     C     CNVETC        BEGSR
020200011026     C*
020300011026     C* ARROTONDAMENTO
020400011029     C                   IF        ETCIAR > *zeros
020500011026     C                   IF        D31DIN = 'EUR'
020600011026     C                   Z-ADD     0,1           ETCIAR
020700011026     C                   ELSE
020800011026     C                   Z-ADD     100           ETCIAR
020900011026     C                   ENDIF
021000011029     C                   ENDIF
021100011026     C* CONTRASSEGNO
021200011026     C                   Z-ADD     ETCCAS        WIMI             30 9
021300011026     C                   EXSR      CNVEUR
021400011026     C                   Z-ADD     WIMO          ETCCAS
021500011026     C* ASSICURAZIONE
021600011026     C                   Z-ADD     ETCASC        WIMI             30 9
021700011026     C                   EXSR      CNVEUR
021800011026     C                   Z-ADD     WIMO          ETCASC
021900011026     C*
022000011026     C                   ENDSR
022100011026     C*------------------------------------------------------------------------*
022200011026     C* CNVETS - CONVERTE I CAMPI DI TEETS IN DIVISA DI CALCOLO DEL CE
022300011026     C*------------------------------------------------------------------------*
022400011026     C     CNVETS        BEGSR
022500011026     C*
022600011026     C* INOLTRO
022700011026     C                   Z-ADD     ETSINL        WIMI             30 9
022800011026     C                   EXSR      CNVEUR
022900011026     C                   Z-ADD     WIMO          ETSINL
023000011026     C*
023100011026     C                   ENDSR
023200970115     C*
023300970115     C*--- SR INIZIALE -----------------------------------------------*
023400970115     C     *INZSR        BEGSR
023500970115     C****
023600970115     C** KLIST
023700970115     C****
023800011002     C* ACCESSO TEETC01L / TEETT01L / TEETS01L
023900011026     C     KETXO         KLIST
024000980526     C                   KFLD                    WDSF
024100980526     C                   KFLD                    WCTO
024200970122     C                   KFLD                    D31KSC
024300970122     C                   KFLD                    D31CTR
024400970122     C                   KFLD                    D31PRG
024500011026     C* X DELETE PREVENTIVA DEL NUOVO
024600011026     C     KETXN         KLIST
024700011026     C                   KFLD                    WDSF
024800011026     C                   KFLD                    WCTO
024900011026     C                   KFLD                    D31KSN
025000011026     C                   KFLD                    D31CTN
025100011026     C                   KFLD                    D31PRN
025200970115     C*
025300970115     C* IMPOSTAZIONE CAMPI FISSI
025400061006     C                   MOVEL     'S'           WDSF
025500980526     C                   MOVEL     'T'           WCTO
025600011026     C*
025700011026     C* DEFINIZIONE CAMPI
025800980526     C     *LIKE         DEFINE    ETCDSF        WDSF
025900980526     C     *LIKE         DEFINE    ETCCTO        WCTO
026000970115     C*
026100970115     C                   ENDSR

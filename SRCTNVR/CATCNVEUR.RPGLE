000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*------------------------------------------------------------------------*
000300000000     F* DATA BASE
000400000000     F*------------------------------------------------------------------------*
000500011029     FTNTAM01L  IF   E           K DISK
000600011029     FTNOFM01L  IF   E           K DISK
000700011029     F                                     RENAME(TNTAM000:TNOFM000)
000800011029     FTEETC01L  UF   E           K DISK
000900011029     FTEETS01L  UF   E           K DISK
001000010808     D*------------------------------------------------------------------------*
001100011029     D* FLAG OPERATIVI -TNTAM/TNOFM--
001200011029     D*-------------------
001300011029     D DSTA01        E DS
001400011029     D*------------------
001500011029     D* CONVERSIONI IMPORTI
001600011029     D*-------------------
001700011029     D YEUR          E DS                  EXTNAME(YEURCODS)
001800011029     D*-------------------
001900011029     D* DS "XSRDA8" - CONTROLLO DATA (8)
002000011029     D*-------------------
002100011029     D WLBDA8          DS                  INZ
002200011029     D  G08DAT                 1      8  0
002300011029     D  G08INV                 9     16  0
002400011029     D  G08ERR                17     17
002500011029     D  G08TGI                18     22  0
002600000000     C*------------------------------------------------------------------------*
002700000000     C* MAIN LINE
002800000000     C*------------------------------------------------------------------------*
002900010808     C*
003000010829     C                   EXSR      EXECNV
003100010829     C*
003200010808     C                   SETON                                            LR
003300011029     C*------------------------------------------------------------------------*
003400011029     C* CNVETC - CONVERTE I CAMPI DI TEETC IN DIVISA DI CALCOLO DEL CE
003500011029     C*------------------------------------------------------------------------*
003600011029     C     CNVETC        BEGSR
003700011029     C* ARROTONDAMENTO
003800011029     C                   IF        §TADIV = 'ITL'
003900011029     C                   Z-ADD     100           ETCIAR
004000011029     C                   ELSE
004100011029     C                   Z-ADD     0,1           ETCIAR
004200011029     C                   ENDIF
004300011029     C* CONTRASSEGNO
004400011029     C                   Z-ADD     ETCCAS        WIMI             30 9
004500011029     C                   EXSR      CNVEUR
004600011029     C                   Z-ADD     WIMO          ETCCAS
004700011029     C* ASSICURAZIONE
004800011029     C                   Z-ADD     ETCASC        WIMI             30 9
004900011029     C                   EXSR      CNVEUR
005000011029     C                   Z-ADD     WIMO          ETCASC
005100011029     C*
005200011029     C                   ENDSR
005300011029     C*------------------------------------------------------------------------*
005400011029     C* CNVETS - CONVERTE I CAMPI DI TEETS IN DIVISA DI CALCOLO DEL CE
005500011029     C*------------------------------------------------------------------------*
005600011029     C     CNVETS        BEGSR
005700011029     C* INOLTRO
005800011029     C                   Z-ADD     ETSINL        WIMI             30 9
005900011029     C                   EXSR      CNVEUR
006000011029     C                   Z-ADD     WIMO          ETSINL
006100011029     C*
006200011029     C                   ENDSR
006300011029     C*------------------------------------------------------------------------*
006400011029     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
006500011029     C*------------------------------------------------------------------------*
006600011029     C     CNVEUR        BEGSR
006700011029     C*
006800011029     C* AZZERA VARIABILI DI LAVORO
006900011029     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
007000011029     C                   MOVEL     '9'           NDEC              1            *N. DEC IN OUTPUT
007100011029     C*
007200011029IF  1C     WIMI          IFNE      *ZEROS
007300011029     C                   CLEAR                   YEUR
007400011029     C                   MOVEL     'ITL'         YECDVI                         *DIVISA IMPORTO
007500011029     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
007600011029     C                   MOVEL     §TADIV        YECDVO                         *DIVISA IMP CONVERTI
007700011029     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
007800011029     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
007900011029     C                   CALL      'YEURCO'
008000011029     C                   PARM                    YEUR
008100011029IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
008200011029     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
008300011029X   2C                   ELSE                                                   *SI ERRORI
008400011029     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
008500011029E   2C                   ENDIF
008600011029E   1C                   ENDIF
008700011029     C*
008800011029     C                   ENDSR
008900010808     C*------------------------------------------------------------------------*
009000010829     C* EXECNV - ESEGUE OPERAZIONI DI CONVERSIONE
009100010808     C*------------------------------------------------------------------------*
009200010829     C     EXECNV        BEGSR
009300010808     C*
009400011029     C     *loval        SETLL     TEETC01L
009500011029     C                   READ      TEETC01L
009600010808     C*
009700011029DO  1C                   DOW       not %eof(TEETC01L)
009800011029     C*
009900011029     C                   MOVEL     '0'           flgOK             1
010000011029     C*
010100011029     C* Reperisco la divisa del dato tipo dalle offerte o tariffe
010200011029     C                   IF        etcCTO = 'T'
010300011029     C     KEYTAR        CHAIN     TNTAM01L
010400011029     C                   IF        %found(TNTAM01L)
010500011029     C                   MOVEL     '1'           flgOK             1
010600011029     C                   MOVEL     TAMFLO        DSTA01
010700011029     C                   ENDIF
010800011029     C                   ENDIF
010900011029     C*
011000011029     C                   IF        etcCTO = 'O'
011100011029     C     KEYTAR        CHAIN     TNOFM01L
011200011029     C                   IF        %found(TNOFM01L)
011300011029     C                   MOVEL     '1'           flgOK             1
011400011029     C                   MOVEL     TAMFLO        DSTA01
011500011029     C                   ENDIF
011600011029     C                   ENDIF
011700011029     C*
011800011029     C* Se  sono riuscito a reperire l'offerta/tariffa => proseguo
011900011029     C                   IF        flgOK = '1'
012000011029     C*
012100011029     C*******************
012200011029     C* T E E T C 0 0 F
012300011029     C*******************
012400011029     C*
012500011029     C* Verifico la divisa della tariffa e se diversa da quella del CAT => converto
012600011029     C                   IF        §TADIV <> 'ITL'
012700011029     C                   EXSR      CNVETC
012800011029     C                   ENDIF
012900011029     C*
013000011029     C* A questo punto effettuo la scrittura del TEETC00F
013100011029     C                   EVAL      ETCISO = 'N'
013200011029     C                   EVAL      ETCINR = 'N'
013300011029     C                   EVAL      ETCIX1 = 'N'
013400011029     C                   EVAL      ETCIX2 = 'N'
013500011029     C                   UPDATE    TEETC000
013600011029     C*
013700011029     C*******************
013800011029     C* T E E T S 0 0 F
013900011029     C*******************
014000011029     C*
014100011029     C* Per ogni record di TEETC00F scorro tutti i record correlati su TEETS00F
014200011029     C     KEYETX        CHAIN     TEETS01L                           98
014300011029     C*
014400011029DO  2C                   DOW       not *in98
014500011029     C*
014600011029     C* Verifico la divisa della tariffa e se diversa da quella del CAT => converto
014700011029     C                   IF        §TADIV <> 'ITL'
014800011029     C                   EXSR      CNVETS
014900011029     C                   ENDIF
015000011029     C*
015100011029     C* A questo punto effettuo la scrittura del TEETS00F
015200011029     C                   UPDATE    TEETS000
015300011029     C*
015400011029     C* Leggo successivo record di TEETS01L
015500011029     C     KEYETX        READE     TEETS01L                               98
015600011029     C*
015700011029E   2C                   ENDDO
015800011029     C                   ENDIF
015900011029     C*
016000011029     C                   READ      TEETC01L
016100010808     C*
016200010922E   1C                   ENDDO
016300010808     C*
016400010808     C                   ENDSR
016500010808     C*------------------------------------------------------------------------*
016600000000     C* *INZSR - OPERAZIONI INIZIALI
016700000000     C*------------------------------------------------------------------------*
016800000000     C     *INZSR        BEGSR
016900011029     C*
017000011029     C*--------------------
017100011029     C* DEFINIZIONE CHIAVI
017200011029     C*--------------------
017300011029     C*
017400011029     C* LETTURA TxETx01L - COMPLETA
017500011029     C     KEYETX        KLIST
017600011029     C                   KFLD                    ETCDSF
017700011029     C                   KFLD                    ETCCTO
017800011029     C                   KFLD                    ETCKSC
017900011029     C                   KFLD                    ETCCTR
018000011029     C                   KFLD                    ETCPRG
018100011029     C                   KFLD                    ETCLNP
018200011029     C* LETTURA TNTAM01L/TNOFM01L - COMPLETA
018300011029     C     KEYTAR        KLIST
018400011029     C                   KFLD                    ETCKSC                         *CLIENTE
018500011029     C                   KFLD                    ETCCTR                         *TARIFFA
018600011029     C                   KFLD                    ETCPRG                         *PROGRESSIVO
018700011029     C*
018800011029     C* REPERISCE LA DATA CORRENTE
018900011029     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
019000011029     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
019100011029     C                   Z-ADD     WN8           G08DAT
019200011029     C                   Z-ADD     *ZEROS        G08INV
019300011029     C                   MOVEL     '0'           G08ERR
019400011029     C                   CALL      'XSRDA8'
019500011029     C                   PARM                    WLBDA8
019600011029     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
019700010829     C*
019800010829     C                   ENDSR

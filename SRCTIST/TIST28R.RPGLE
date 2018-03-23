000100000000     H*------------------------------------------------------------------------*
000200000225     H* STATISTICA DESTINATARI - GENERA WRK FILE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000225     H*------------------------------------------------------------------------*
000600000313     FTITAS11L  IF   E           K DISK
000700000000     FTABEL00F  IF   E           K DISK
000800000225     FWFDES01L  UF A E           K DISK
000900000000     D*------------------------------------------------------------------------*
001000000000     D* SCHIERE
001100000000     D*------------------------------------------------------------------------*
001200000225     D*-------------------
001300000000     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
001400000000     D*-------------------
001500000000     D RBL             S              1    DIM(5)                               RAGGRUPPAMENTO BOLLA
001600000000     D FSP             S              1    DIM(5)                               FLAG SPEDIZIONE
001700000000     D FRI             S              1    DIM(5)                               FLAG RICAVO
001800000000     D*-------------------
001900000000     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
002000000000     D*-------------------
002100000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
002200000225     D SP              S              1    DIM(20)                              FLAG SPEDIZIONE
002300000225     D RI              S              1    DIM(20)                              FLAG RICAVO
002400000000     D*------------------------------------------------------------------------*
002500000000     D* INPUT
002600000000     D*------------------------------------------------------------------------*
002700000000     D*-------------------
002800000000     D* ARCHITETTURA
002900000000     D*-------------------
003000000000     D KPJBA         E DS
003100000000     D*-------------------
003200000000     D* DS TIPO BOLLA
003300000000     D*-------------------
003400000000     D DSTB          E DS
003500000000     D*-------------------
003600000000     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
003700000000     D*-------------------
003800000000     D DS2W          E DS
003900000225     D*-------------------
004000000225     D* DS SCOMPOSIZIONE DATE INIZ/FIN/WRK
004100000000     D*-------------------
004200991028     D                 DS                  INZ
004300991028     D  MM                     1      2  0
004400991028     D  MMGG                   1      4  0
004500000000     D                 DS                  INZ
004600000000     D  DIANN                  1      4  0
004700000000     D  DIMES                  5      6  0
004800000000     D  DIMGS                  5      8  0
004900000000     D  DIGIO                  7      8  0
005000000000     D  DIAAM                  1      6  0
005100991213     D  DIDAT                  1      8  0
005200000000     D                 DS                  INZ
005300000000     D  DFANN                  1      4  0
005400000000     D  DFMES                  5      6  0
005500000000     D  DFMGS                  5      8  0
005600000225     D  DFGIO                  7      8  0
005700000225     D  DFAAM                  1      6  0
005800000225     D  DFDAT                  1      8  0
005900000000     D*-------------------
006000000000     D* DS PARAMETRI DI LANCIO PGM
006100000000     D*-------------------
006200000000     D PARAM           DS
006300000225     D  PARDAI                 1      8  0
006400000225     D  PARDAF                 9     16  0
006500000000     C*------------------------------------------------------------------------*
006600000000     C* MAIN LINE
006700000000     C*------------------------------------------------------------------------*
006800000000     C*
006900000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
007000000000     C     FINL00        IFEQ      '0'                                          --- 1 -->
007100000000     C*
007200000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
007300000000     C                   EXSR      SETFIL
007400000000     C*
007500000000     C* CICLO FINO A FINE FILE
007600000000     C     FINL00        DOWEQ     '0'                                          --- 2 -->
007700000000     C*
007800000000     C* CREA IL RECORD SUI FILE DI LAVORO
007900000000     C                   EXSR      CREARE
008000000000     C*
008100000000     C* LEGGE IL RECORD SUCCESSIVO
008200000000     C                   EXSR      LETFIL
008300000000     C                   ENDDO                                                  <-- 2 ---
008400000000     C*
008500000000     C                   ENDIF                                                  <-- 1 ---
008600000000     C*
008700000000     C                   SETON                                        LR
008800000000     C*------------------------------------------------------------------------*
008900000000     C* SETFIL - POSIZIONAMENTO SUL FILE
009000000000     C*------------------------------------------------------------------------*
009100000000     C     SETFIL        BEGSR
009200000000     C*
009300000000     C* POSIZIONAMENTO E PRIMA LETTURA
009400000225     C                   Z-ADD     PARDAI        DIDAT
009500000225     C                   Z-ADD     PARDAF        DFDAT
009600000000     C*
009700000000     C                   Z-ADD     DIANN         KTAAAS
009800000000     C                   Z-ADD     DIMGS         KTAMGS
009900000313     C                   MOVEL     TASRSD        KTARSD
010000000313     C                   MOVEL     TASIND        KTAIND
010100000313     C                   MOVEL     TASLOD        KTALOD
010200000313     C                   MOVEL     TASPRD        KTAPRD
010300000313     C                   MOVEL     TASNZD        KTANZD
010400000313     C                   MOVEL     TASDCM        KTADCM
010500991123     C*
010600000313     C     KEYTAS        SETLL     TITAS11L                           99        *FINE FILE
010700000000     C*
010800000000     C     *IN99         IFEQ      '1'
010900000000     C                   MOVEL     '1'           FINL00
011000000000     C                   ELSE
011100000000     C                   EXSR      LETFIL
011200000000     C                   END
011300000000     C*
011400000225     C                   ENDSR
011500000000     C*------------------------------------------------------------------------*
011600000000     C* LETFIL - LETTURA PROSSIMO RECORD
011700000000     C*------------------------------------------------------------------------*
011800000000     C     LETFIL        BEGSR
011900000000     C*
012000000000     C* LEGGO FINO A:
012100000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
012200000000     C*     - TROVATO RECORD VALIDO
012300000000     C                   MOVEL     'N'           WRECOK
012400000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
012500000000     C     WRECOK        ANDEQ     'N'
012600000000     C*
012700000000     C* LETTURE SUCCESSIVE ALLA PRIMA
012800000313     C                   READ      TITAS11L                               99
012900000225     C*
013000000225     C     *IN99         IFEQ      *OFF
013100000225     C                   MOVEL     TASAAS        PERIODO           8 0
013200000225     C                   MOVE      TASMGS        PERIODO
013300000225     C                   ENDIF
013400000225     C*
013500000225     C     *IN99         IFEQ      *ON                                          *FINE FILE
013600000225     C     PERIODO       ORLT      PARDAI                                       *SUPERATO RANGE PERI
013700000225     C     PERIODO       ORGT      PARDAF                                       *SUPERATO RANGE PERI
013800000000     C                   MOVEL     '1'           FINL00
013900000000     C                   ELSE
014000000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
014100000225     C                   ENDIF
014200000000     C*
014300000225     C                   ENDDO                                                  <-- 1 ---
014400000000     C*
014500000000     C                   ENDSR
014600000000     C*------------------------------------------------------------------------*
014700000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
014800000000     C*------------------------------------------------------------------------*
014900000000     C     CHKREC        BEGSR
015000000000     C*
015100000000     C                   MOVEL     'S'           WRECOK            1
015200000000     C*
015300000000     C* ESCLUDE LE BOLLE NON VALIDE COME "SPEDIZIONE"
015400000000     C     WRECOK        IFEQ      'S'
015500000000     C                   Z-ADD     1             J
015600000000     C                   SETOFF                                           28
015700000000     C     TASTBL        LOOKUP    TB(J)                                  28
015800000000     C     SP(J)         IFEQ      'N'
015900000000     C                   MOVEL     'N'           WRECOK
016000000000     C                   ENDIF
016100000000     C                   ENDIF
016200000225     C*
016300000313     C* CONTROLLI SU "ASSEGNATO"
016400000310     C     WRECOK        IFEQ      'S'
016500000310     C                   MOVEL     TASTBL        A1                1
016600000310     C     A1            IFEQ      'A'                                          *ASSEGNATO
016700000310     C     TASCCM        IFEQ      *ZEROS                                       *CLIENTE VUOTO
016800000310     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
016900000000     C                   ENDIF
017000000000     C                   ENDIF
017100000000     C                   ENDIF
017200000000     C*
017300000000     C                   ENDSR
017400000000     C*------------------------------------------------------------------------*
017500000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
017600000000     C*------------------------------------------------------------------------*
017700000000     C     CREARE        BEGSR
017800991025     C*
017900000225     C                   MOVEL     TASRSD        KWDRSD
018000000225     C                   MOVEL     TASIND        KWDIND
018100000225     C                   MOVEL     TASLOD        KWDLOD
018200000225     C                   MOVEL     TASPRD        KWDPRD
018300000225     C                   MOVEL     TASNZD        KWDNZD
018400000000     C*
018500000000     C* SCRIVO O RISCRIVO FILE DI LAVORO PER STAMPA STATISTICA PESI
018600000000     C*
018700000225     C     KEYWFD        CHAIN     WFDES01L                           99
018800000000     C*
018900000000     C* RIEMPIO BUFFER RECORD
019000000000     C*
019100000225     C                   MOVEL     KWDRSD        WFDRSD
019200000225     C                   MOVEL     KWDIND        WFDIND
019300000225     C                   MOVEL     KWDLOD        WFDLOD
019400000225     C                   MOVEL     KWDPRD        WFDPRD
019500000225     C                   MOVEL     KWDNZD        WFDNZD
019600000225     C   99              EXSR      AZZWFD                                       *RE-INIZIALIZZA BUFF
019700000225     C                   EXSR      ADDWFD                                       *AGGIUNGE SPED.AL RE
019800000225     C   99              WRITE     WFDES000                                     *RECORD NON ESISTE,
019900000225     C  N99              UPDATE    WFDES000                                     *RECORD ESISTE, AGGI
020000991025     C*
020100000000     C                   ENDSR
020200000225     C*------------------------------------------------------------------------*
020300000225     C* AZZWFD - AZZERA CAMPI RANGE IN WFDES00F
020400000225     C*------------------------------------------------------------------------*
020500000225     C     AZZWFD        BEGSR
020600000225     C*
020700000313     C                   Z-ADD     *ZEROS        WFDDCM
020800000225     C                   Z-ADD     *ZEROS        WFDSPE
020900000225     C                   Z-ADD     *ZEROS        WFDNCL
021000000225     C                   Z-ADD     *ZEROS        WFDPKB
021100000225     C                   Z-ADD     *ZEROS        WFDVLB
021200000225     C                   Z-ADD     *ZEROS        WFDNGG
021300000225     C*
021400000225     C                   ENDSR
021500000000     C*------------------------------------------------------------------------*
021600000225     C* ADDWFD - AGGIUNGE LA SPEDIZIONE AL RECORD WFDES00F
021700000000     C*------------------------------------------------------------------------*
021800000313     C     ADDWFD        BEGSR
021900991025     C*
022000000313     C                   Z-ADD     *ZEROS        WFDDCM
022100000310     C                   Z-ADD     TASLNP        WFDLNP
022200000225     C                   ADD       1             WFDSPE
022300000225     C                   ADD       TASNCL        WFDNCL
022400000225     C                   ADD       TASPKB        WFDPKB
022500000225     C                   ADD       TASVLB        WFDVLB
022600000313     C     *IN99         IFEQ      *off
022700000313     C     TASDCM        IFNE      DEPDCM
022800000225     C                   ADD       1             WFDNGG
022900000313     C                   ELSE
023000000313     C                   Z-ADD     TASDCM        DEPDCM
023100000313     C                   ENDIF
023200000313     C                   ELSE
023300000313     C                   Z-ADD     1             WFDNGG
023400000313     C                   Z-ADD     TASDCM        DEPDCM
023500000313     C                   ENDIF
023600991025     C*
023700000000     C                   ENDSR
023800000000     C*------------------------------------------------------------------------*
023900000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
024000000000     C*------------------------------------------------------------------------*
024100000000     C     CARTBL        BEGSR
024200000000     C*--------------------
024300000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
024400000000     C*--------------------
024500000000     C                   Z-ADD     0             I
024600000000     C                   Z-ADD     CODUT         KTBUTE
024700000000     C                   MOVEL     '2W'          KTBCOD
024800000000     C                   MOVEL     *BLANKS       KTBKEY
024900000000     C     KEYTAB        SETLL     TABEL00F
025000000000     C                   READ      TABEL00F                               99
025100000000     C     *IN99         DOWEQ     *OFF
025200000000     C     TBLCOD        ANDEQ     '2W'
025300000000     C     TBLFLG        IFEQ      *BLANKS
025400000000     C                   ADD       1             I
025500000000     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
025600000000     C                   MOVEL     TBLUNI        DS2W
025700000000     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
025800000000     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
025900000000     C                   END
026000000000     C                   READ      TABEL00F                               99
026100000000     C                   END
026200000000     C*--------------------
026300000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
026400000000     C*--------------------
026500000225     C                   Z-ADD     0             I                 3 0
026600000000     C                   Z-ADD     CODUT         KTBUTE
026700000000     C                   MOVEL     'TB'          KTBCOD
026800000000     C                   MOVEL     *BLANKS       KTBKEY
026900000000     C     KEYTAB        SETLL     TABEL00F
027000000000     C                   READ      TABEL00F                               99
027100000000     C     *IN99         DOWEQ     *OFF
027200000000     C     TBLCOD        ANDEQ     'TB'
027300000000     C     TBLFLG        IFEQ      *BLANKS
027400000000     C                   ADD       1             I
027500000000     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
027600000000     C                   MOVEL     TBLUNI        DSTB
027700000000     C                   Z-ADD     1             J                 4 0
027800000000     C                   SETOFF                                       28
027900000000     C     §TBRBL        LOOKUP    RBL(J)                                 28
028000000000     C     *IN28         IFEQ      *ON
028100000000     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
028200000000     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
028300000000     C                   ELSE
028400000000     C                   MOVEL     'N'           SP(I)
028500000000     C                   MOVEL     'N'           RI(I)
028600000000     C                   ENDIF
028700000000     C                   END
028800000000     C                   READ      TABEL00F                               99
028900000000     C                   END
029000000000     C*
029100000000     C                   ENDSR
029200000000     C*------------------------------------------------------------------------*
029300000000     C* *INZSR - ROUTINE INIZIALE
029400000000     C*------------------------------------------------------------------------*
029500000000     C     *INZSR        BEGSR
029600000000     C*--------------------
029700000000     C* RICEVIMENTO PARAMETRI
029800000000     C*--------------------
029900000000     C     *ENTRY        PLIST
030000000000     C                   PARM                    KPJBA
030100000000     C                   MOVEL     KPJBU         PARAM
030200000000     C*--------------------
030300000000     C* CAMPI RIFERITI AL DATA BASE
030400000000     C*--------------------
030500000000     C     *LIKE         DEFINE    TASAAS        KTAAAS
030600000000     C     *LIKE         DEFINE    TASMGS        KTAMGS
030700000313     C     *LIKE         DEFINE    TASRSD        KTARSD
030800000313     C     *LIKE         DEFINE    TASIND        KTAIND
030900000313     C     *LIKE         DEFINE    TASLOD        KTALOD
031000000313     C     *LIKE         DEFINE    TASPRD        KTAPRD
031100000313     C     *LIKE         DEFINE    TASNZD        KTANZD
031200000313     C     *LIKE         DEFINE    TASDCM        KTADCM
031300000313     C     *LIKE         DEFINE    TASDCM        DEPDCM
031400000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
031500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
031600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
031700000225     C     *LIKE         DEFINE    WFDRSD        KWDRSD
031800000225     C     *LIKE         DEFINE    WFDIND        KWDIND
031900000225     C     *LIKE         DEFINE    WFDLOD        KWDLOD
032000000225     C     *LIKE         DEFINE    WFDPRD        KWDPRD
032100000225     C     *LIKE         DEFINE    WFDNZD        KWDNZD
032200000000     C*--------------------
032300000000     C* CHIAVI DI LETTURA
032400000000     C*--------------------
032500000313     C* CHIAVE LETTURA TITAS11C - COMPLETA
032600000000     C     KEYTAS        KLIST
032700000000     C                   KFLD                    KTAAAS
032800000000     C                   KFLD                    KTAMGS
032900000313     C                   KFLD                    KTARSD
033000000313     C                   KFLD                    KTAIND
033100000313     C                   KFLD                    KTALOD
033200000313     C                   KFLD                    KTAPRD
033300000313     C                   KFLD                    KTANZD
033400000313     C                   KFLD                    KTADCM
033500000000     C*
033600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
033700000000     C     KEYTAB        KLIST
033800000000     C                   KFLD                    KTBUTE
033900000000     C                   KFLD                    KTBCOD
034000000000     C                   KFLD                    KTBKEY
034100000000     C*
034200000225     C* CHIAVE LETTURA WFDES01L - COMPLETA
034300000225     C     KEYWFD        KLIST
034400000225     C                   KFLD                    KWDRSD
034500000225     C                   KFLD                    KWDIND
034600000225     C                   KFLD                    KWDLOD
034700000225     C                   KFLD                    KWDPRD
034800000225     C                   KFLD                    KWDNZD
034900000000     C*--------------------
035000000000     C* CAMPI DI CONTROLLO
035100000000     C*--------------------
035200000000     C                   MOVEL     '0'           FINL00            1
035300000000     C                   Z-ADD     1             CODUT             1 0
035400000313     C                   Z-ADD     *zeros        DEPDCM
035500000000     C*
035600000000     C* CARICA TIPI BOLLA
035700000000     C                   EXSR      CARTBL
035800000000     C*
035900000000     C                   ENDSR

000100000000     H*------------------------------------------------------------------------*
000200991029     H* STATISTICA VOLUMI CML - GENERA WRK FILE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     FTITAS38C  IF   E           K DISK
000600000000     FAZORG01L  IF   E           K DISK
000700000000     FTABEL00F  IF   E           K DISK
000800991104     FWFVLM01L  UF A E           K DISK
000900000000     D*------------------------------------------------------------------------*
001000000000     D* SCHIERE
001100000000     D*------------------------------------------------------------------------*
001200000216     D*------------------
001300000216     D* DECODIFICA
001400000216     D*------------------
001500000216     D DEC             S             40    DIM(5) CTDATA PERRCD(1)              *DECODIFICHE
001600000000     D*-------------------
001700000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
001800000000     D*-------------------
001900000000     D FIL             S              3  0 DIM(500)                             *FILIALE
002000000000     D ARE             S              3  0 DIM(500)                             *AREA
002100000000     D DIV             S              1    DIM(500)                             *DIVISIONE
002200000000     D DIT             S              3    DIM(500)                             *RAMO AZIENDALE
002300000000     D*-------------------
002400000000     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
002500000000     D*-------------------
002600000000     D RBL             S              1    DIM(5)                               RAGGRUPPAMENTO BOLLA
002700000000     D FSP             S              1    DIM(5)                               FLAG SPEDIZIONE
002800000000     D FRI             S              1    DIM(5)                               FLAG RICAVO
002900000000     D*-------------------
003000000000     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003100000000     D*-------------------
003200000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
003300000000     D SP              S              1    DIM(20)                              FLAG SPEDIZIONE
003400000000     D RI              S              1    DIM(20)                              FLAG RICAVO
003500000000     D*------------------------------------------------------------------------*
003600000000     D* INPUT
003700000000     D*------------------------------------------------------------------------*
003800000000     D*-------------------
003900000000     D* ARCHITETTURA
004000000000     D*-------------------
004100000000     D KPJBA         E DS
004200000000     D*-------------------
004300000000     D* DS TIPO BOLLA
004400000000     D*-------------------
004500000000     D DSTB          E DS
004600000000     D*-------------------
004700000000     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
004800000000     D*-------------------
004900000000     D DS2W          E DS
005000991029     D*------------------
005100991029     D* DS REPERIMENTO DATI UTENTE
005200991029     D*-------------------
005300991029     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
005400991029     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
005500991029     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
005600991029     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005700991029     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005800000000     D*-------------------
005900000000     D* DS SCOMPOSIZIONE DATE INIZ/FIN/WRK
006000000000     D*-------------------
006100991028     D                 DS                  INZ
006200991028     D  MM                     1      2  0
006300991028     D  MMGG                   1      4  0
006400000000     D                 DS                  INZ
006500000000     D  DIANN                  1      4  0
006600000000     D  DIMES                  5      6  0
006700000000     D  DIMGS                  5      8  0
006800000000     D  DIGIO                  7      8  0
006900000000     D  DIAAM                  1      6  0
007000000000     D                 DS                  INZ
007100000000     D  DFANN                  1      4  0
007200000000     D  DFMES                  5      6  0
007300000000     D  DFMGS                  5      8  0
007400000000     D  DFGIO                  7      8  0
007500000000     D  DFAAM                  1      6  0
007600000000     D*-------------------
007700000000     D* DS SCOMPOSIZIONE CODICE CLIENTE
007800000000     D*-------------------
007900000000     D                 DS
008000000000     D  DSCLI                  1      7  0                                      *CLIEN
008100000000     D  DSFIL                  1      3  0                                      *FIL
008200000000     D  DSCOD                  4      7  0                                      *COD
008300000000     D*-------------------
008400000000     D* DS PARAMETRI DI LANCIO PGM
008500000000     D*-------------------
008600000000     D PARAM           DS
008700991029     D  PARTLA                 1      1
008800991029     D  PARANN                 2      5  0
008900991029     D  PARMMI                 6      7  0
009000991029     D  PARMMF                 8      9  0
009100991029     D  PARSNR                10     10
009200991029     D  PARPID                27     31  2
009300991029     D  PARDIV                32     32
009400991029     D  PARARE                33     35  0
009500991029     D  PARFIL                36     38  0
009600991029     D  PARDET                39     39
009700991029     D  PARTOT                40     40
009800991029     D  PARRG1                41     44  1
009900991029     D  PARRG2                45     48  1
010000991029     D  PARPIA                49     53  2
010100991029     D  PARPSD                54     58  2
010200991029     D  PARPSA                59     63  2
010300991103     D  PAROK                 65     66
010400991229     D  PARMIN                67     71  3
010500000706     D  PARPOS                72     72
010600030910     D  PARNT1                73     75
010700030910     D  PARNT2                76     78
010800030910     D  PARNT3                79     81
010900030910     D  PARNT4                82     84
011000050124     D  PARSAL                85     85
011100000000     C*------------------------------------------------------------------------*
011200000000     C* MAIN LINE
011300000000     C*------------------------------------------------------------------------*
011400000000     C*
011500000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
011600000000     C     FINL00        IFEQ      '0'                                          --- 1 -->
011700000000     C*
011800000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
011900000000     C                   EXSR      SETFIL
012000000000     C*
012100000000     C* CICLO FINO A FINE FILE
012200000000     C     FINL00        DOWEQ     '0'                                          --- 2 -->
012300000000     C*
012400000000     C* CREA IL RECORD SUI FILE DI LAVORO
012500000000     C                   EXSR      CREARE
012600000000     C*
012700000000     C* LEGGE IL RECORD SUCCESSIVO
012800000000     C                   EXSR      LETFIL
012900000000     C                   ENDDO                                                  <-- 2 ---
013000000000     C*
013100000000     C                   ENDIF                                                  <-- 1 ---
013200991029     C*
013300991029     C* OPERAZIONI FINALI
013400991029     C                   EXSR      FINSR
013500000000     C*
013600000000     C                   SETON                                        LR
013700000000     C*------------------------------------------------------------------------*
013800000000     C* SETFIL - POSIZIONAMENTO SUL FILE
013900000000     C*------------------------------------------------------------------------*
014000000000     C     SETFIL        BEGSR
014100000000     C*
014200000000     C* POSIZIONAMENTO E PRIMA LETTURA
014300000000     C                   MOVE      PARANN        DIANN
014400000000     C                   MOVE      PARMMI        DIMES
014500000000     C                   MOVE      PARANN        DFANN
014600000000     C                   MOVE      PARMMF        DFMES
014700000000     C                   MOVE      '99'          DFGIO
014800000000     C*
014900000000     C                   Z-ADD     DIANN         KTAAAS
015000000000     C                   Z-ADD     DIMGS         KTAMGS
015100000000     C                   Z-ADD     *ZEROS        KTAKSC
015200000000     C*
015300000000     C     KEYTAS        SETLL     TITAS38C                           99        *FINE FILE
015400000000     C*
015500000000     C     *IN99         IFEQ      '1'
015600000000     C                   MOVEL     '1'           FINL00
015700000000     C                   ELSE
015800000000     C                   EXSR      LETFIL
015900000000     C                   END
016000000000     C*
016100000000     C                   ENDSR
016200000000     C*------------------------------------------------------------------------*
016300000000     C* LETFIL - LETTURA PROSSIMO RECORD
016400000000     C*------------------------------------------------------------------------*
016500000000     C     LETFIL        BEGSR
016600000000     C*
016700000000     C* LEGGO FINO A:
016800000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
016900000000     C*     - TROVATO RECORD VALIDO
017000000000     C                   MOVEL     'N'           WRECOK
017100000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
017200000000     C     WRECOK        ANDEQ     'N'
017300000000     C*
017400000000     C* LETTURE SUCCESSIVE ALLA PRIMA
017500000000     C                   READ      TITAS38C                               99
017600000000     C*
017700000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
017800000000     C     TASAAS        ORGT      PARANN                                       *SUPERATO RANGE PERI
017900000000     C     TASMGS        ORGT      DFMGS
018000000000     C                   MOVEL     '1'           FINL00
018100000000     C                   ELSE
018200000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
018300000000     C                   END
018400000000     C*
018500000000     C                   END                                                    <-- 1 ---
018600000000     C*
018700000000     C                   ENDSR
018800000000     C*------------------------------------------------------------------------*
018900000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
019000000000     C*------------------------------------------------------------------------*
019100000000     C     CHKREC        BEGSR
019200000000     C*
019300000000     C                   MOVEL     'S'           WRECOK            1
019400001011     C*
019500001011     C* NUOVO CONTROLLO ==> SE IL CAMPO TASNCR = 99999 ==> ESCLUDO COMPLETAMENTE IL RECORD
019600001011     C     WRECOK        IFEQ      'S'
019700001011     C     TASNCR        IFEQ      99999
019800001011     C                   MOVEL     'N'           WRECOK
019900001011     C                   ENDIF
020000001011     C                   ENDIF
020100000000     C*
020200000000     C* ESCLUDE LE BOLLE NON VALIDE COME "SPEDIZIONE"
020300000000     C     WRECOK        IFEQ      'S'
020400000000     C                   Z-ADD     1             J
020500000000     C                   SETOFF                                           28
020600000000     C     TASTBL        LOOKUP    TB(J)                                  28
020700000000     C     SP(J)         IFEQ      'N'
020800000000     C                   MOVEL     'N'           WRECOK
020900000000     C                   ENDIF
021000000000     C                   ENDIF
021100000000     C*
021200000000     C* SE E' ASSEGNATO PRENDE CLIENTE MITTENTE, SE FRANCO CLIENTE DESTINO
021300000000     C     WRECOK        IFEQ      'S'
021400000000     C                   Z-ADD     *ZEROS        WCLI
021500000000     C                   Z-ADD     *ZEROS        WCTR
021600000000     C                   MOVEL     TASTBL        A1                1
021700000000     C     A1            IFEQ      'A'                                          *ASSEGNATO
021800000000     C     TASCCM        IFEQ      *ZEROS                                       *CLIENTE VUOTO
021900000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
022000000000     C                   ELSE
022100000000     C                   Z-ADD     TASCCM        WCLI                           *CLIENTE MITTENTE
022200000000     C                   Z-ADD     999           WCTR                           *TARIFFA FARLOCCA
022300000000     C                   ENDIF
022400000000     C                   ELSE                                                   *FRANCO
022500000000     C                   Z-ADD     TASKSC        WCLI                           *CLIENTE DESTINO
022600000000     C                   MOVE      TASKSC        N4                4 0
022700000000     C     N4            IFEQ      8888
022800000000     C                   Z-ADD     888           WCTR                           *TARIFFA FARLOCCA
022900000000     C                   ELSE
023000000000     C                   Z-ADD     TASCTR        WCTR                           *TARIFFA CLIENTE
023100000000     C                   ENDIF
023200000000     C                   ENDIF
023300991029     C                   ENDIF
023400991029     C*
023500991029     C* DECODIFICO LA RAGIONE SOCIALE DEL CLIENTE GIA' CONVERTITO
023600991029     C                   EXSR      DECCLI
023700000000     C*
023800000000     C* RECUPERA FILIALE,DIVISIONE ED AREA IN BASE AL CODICE CLIENTE
023900000000     C     WRECOK        IFEQ      'S'
024000000000     C                   Z-ADD     WCLI          DSCLI
024100000000     C                   Z-ADD     DSFIL         WFIL              3 0
024200000000     C                   EXSR      DIVARE
024300000000     C                   MOVEL     WDIV          WDIV              1
024400000000     C                   Z-ADD     WARE          WARE              3 0
024500000000     C                   ENDIF
024600000000     C*
024700000000     C     WRECOK        IFEQ      'S'
024800000000     C*
024900000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
025000000000     C*
025100000000     C     PARDIV        IFNE      *BLANKS
025200000000     C     WDIV          IFEQ      PARDIV
025300000000     C                   ELSE
025400000000     C                   MOVEL     'N'           WRECOK
025500000000     C                   ENDIF
025600000000     C                   ENDIF
025700000000     C*
025800000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
025900000000     C     PARARE        IFNE      *ZEROS
026000000000     C     WARE          IFEQ      PARARE
026100000000     C                   ELSE
026200000000     C                   MOVEL     'N'           WRECOK
026300000000     C                   ENDIF
026400000000     C                   ENDIF
026500000000     C*
026600000000     C* SE FILIALE PARTENZA DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
026700000000     C     PARFIL        IFNE      *ZEROS
026800000000     C     TASLNP        IFNE      PARFIL
026900000000     C                   MOVEL     'N'           WRECOK
027000000000     C                   ENDIF
027100000000     C                   ENDIF
027200000000     C                   ENDIF
027300000000     C*
027400000000     C                   ENDSR
027500000000     C*------------------------------------------------------------------------*
027600000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
027700000000     C*------------------------------------------------------------------------*
027800000000     C     CREARE        BEGSR
027900991029     C*
028000991029     C* IMPOSTA IL FLAG TIPO VOLUME (REALE O NON REALE)
028100040211IF  1C*--- TASFVB        IFNE      'R'
028200040211     C*---               MOVEL     *BLANKS       TASFVB                         *' '=NON REALE
028300040211E   1C*---               ENDIF
028400991025     C*
028500000000     C                   Z-ADD     TASMGS        MMGG
028600991029     C                   Z-ADD     TASAAS        KWVANN
028700991029     C                   MOVEL     WDIV          KWVDIV
028800991029     C                   Z-ADD     WARE          KWVARE
028900991029     C                   Z-ADD     WFIL          KWVFIL
029000991029     C                   MOVEL     WFVRAG        KWVRAG
029100991029     C                   Z-ADD     WCLI          KWVKSC
029200991029     C                   Z-ADD     WCTR          KWVCTR
029300991029     C                   Z-ADD     MM            KWVMES
029400040211     C*---               MOVEL     TASFVB        KWVFVB
029500040211     C                   MOVEL     *BLANKS       KWVFVB
029600000000     C*
029700000000     C* SCRIVO O RISCRIVO FILE DI LAVORO PER STAMPA STATISTICA PESI
029800000000     C*
029900991104     C     KEYWFV        CHAIN     WFVLM01L                           99
030000000000     C*
030100000000     C* RIEMPIO BUFFER RECORD
030200000000     C*
030300991029     C                   Z-ADD     KWVANN        WFVANN
030400991029     C                   MOVEL     KWVDIV        WFVDIV
030500991029     C                   Z-ADD     KWVARE        WFVARE
030600991029     C                   Z-ADD     KWVFIL        WFVFIL
030700991029     C                   Z-ADD     KWVKSC        WFVKSC
030800991029     C                   Z-ADD     KWVCTR        WFVCTR
030900991029     C                   Z-ADD     KWVMES        WFVMES
031000991029     C                   MOVEL     KWVFVB        WFVFVB
031100991029     C   99              EXSR      AZZWFV                                       *RE-INIZIALIZZA BUFF
031200991029     C                   EXSR      ADDWFV                                       *AGGIUNGE SPED.AL RE
031300991029     C   99              WRITE     WFVOL                                        *RECORD NON ESISTE,
031400991029     C  N99              UPDATE    WFVOL                                        *RECORD ESISTE, AGGI
031500991025     C*
031600000000     C                   ENDSR
031700000000     C*------------------------------------------------------------------------*
031800991104     C* ADDWFV - AGGIUNGE LA SPEDIZIONE AL RECORD WFVLM00F
031900000000     C*------------------------------------------------------------------------*
032000991029     C     ADDWFV        BEGSR
032100991029     C*
032200991029     C                   MOVEL     'S'           ESEGUI            1
032300991025     C*
032400991029     C                   ADD       TASNCL        WFVTCL
032500991104     C*
032600991104     C* CONTROLLA SE E COSA PRENDERE COME VOL. PER IL VOL. CML
032700991104     C*
032800991104 OLD C* VOLUME TOTALE (T) -> VOLUME DA FATTURARE=VOLUME CML
032900991104  "  C* VOLUME NON TOTALE (Z, R, P, C, K) -> SOLO SE COLLI BOLLA=COLLI RILEVATI
033000991104  "  C* (CASO DI FRANCHI IN ARRIVO PASSATI DA CML MA ON SOSTITUITO IL VOLUME)
033100991104  "  C*                    Z-ADD*ZEROS    WVLC
033200991104  "  C*          TASFVF    IFEQ 'T'
033300991104  "  C*                    Z-ADDTASVLF    WVLC
033400991104  "  C*                    ELSE
033500991104  "  C*          TASNCL    IFEQ TASNCR
033600991104  "  C*          TASVLC    IFGT *ZEROS
033700991104  "  C*                    Z-ADDTASVLC    WVLC
033800991104  "  C*                    ELSE
033900991104  "  C*                    MOVEL'N'       ESEGUI
034000991104  "  C*                    ENDIF
034100991104  "  C*                    ELSE
034200991104  "  C*                    MOVEL'N'       ESEGUI
034300991104  "  C*                    ENDIF
034400991104 OLD C*                    ENDIF
034500991104     C*
034600991104     C     TASFVF        IFEQ      'T'
034700991104     C                   Z-ADD     TASVLF        WVLC
034800991104     C                   ELSE
034900991104     C     TASNCL        IFEQ      TASNCR
035000991104     C     TASVLC        IFGT      *ZEROS
035100991104     C                   Z-ADD     TASVLC        WVLC
035200991104     C                   ELSE
035300991104     C                   MOVEL     'N'           ESEGUI
035400991104     C                   ENDIF
035500991104     C                   ELSE
035600080619     C     TASVLC        IFGT      TASVLB
035700080619     C**   TASVLC        IFGT      TASVLF
035800991104     C                   Z-ADD     TASVLC        WVLC
035900991104     C                   ELSE
036000991104     C                   MOVEL     'N'           ESEGUI
036100991104     C                   ENDIF
036200991104     C                   ENDIF
036300991104     C                   ENDIF
036400991104     C*
036500991029     C     ESEGUI        IFEQ      'S'
036600991029     C                   ADD       WVLC          WFVVLC
036700991029     C                   ADD       TASVLB        WFVVLB
036800991029     C                   ADD       TASNCL        WFVNCL
036900991029     C                   ADD       TASNCR        WFVNCR
037000991029     C                   ADD       TASPKF        WFVPKB
037100991029     C                   ADD       1             WFVNSP
037200991026     C                   ENDIF
037300991025     C*
037400000000     C                   ENDSR
037500000000     C*------------------------------------------------------------------------*
037600991104     C* AZZWFV - AZZERA CAMPI IN WFVLM00F
037700000000     C*------------------------------------------------------------------------*
037800991029     C     AZZWFV        BEGSR
037900000000     C*
038000991029     C                   Z-ADD     *ZEROS        WFVTCL
038100991029     C                   Z-ADD     *ZEROS        WFVVLC
038200991029     C                   Z-ADD     *ZEROS        WFVNCL
038300991029     C                   Z-ADD     *ZEROS        WFVNCR
038400991029     C                   Z-ADD     *ZEROS        WFVNSP
038500991029     C                   Z-ADD     *ZEROS        WFVVLB
038600991029     C                   Z-ADD     *ZEROS        WFVPKB
038700000000     C*
038800000000     C                   ENDSR
038900991029     C*------------------------------------------------------------------------*
039000991029     C* DECCLI - DECODIFICA CLIENTE
039100991029     C*------------------------------------------------------------------------*
039200991029     C     DECCLI        BEGSR
039300991029     C*
039400000216     C                   Z-ADD     WCLI          DSCLI
039500000216IF  1C     DSCOD         IFEQ      8888                                         *MITTENTE NON COD
039600000216     C                   MOVEL     DEC(1)        WFVRAG
039700000216X   1C                   ELSE                                                   *CODIFICATI
039800991029     C                   CLEAR                   BS69DS
039900991029     C                   CLEAR                   ACODS
040000991029     C                   MOVEL     KNSIF         I69SIF                         *S.I.
040100991029     C                   Z-ADD     WCLI          I69KAC                         *CLIENTE X CNACO
040200991029     C                   CALL      'TIBS69R'
040300991029     C                   PARM                    BS69DS
040400991029     C                   PARM                    ACODS
040500991029     C                   PARM                    INDDS
040600991029     C                   PARM                    CLPDS
040700991029     C                   PARM                    CLSDS
040800991029     C     O69ERR        IFNE      '1'                                          *NO ERRORE
040900991029     C                   MOVEL     ACORAG        WFVRAG
041000991029     C                   ELSE
041100991029     C                   MOVEL     *ALL'*'       WFVRAG
041200991029     C                   ENDIF
041300000216E   1C                   ENDIF
041400991029     C*
041500991029     C                   ENDSR
041600000000     C*------------------------------------------------------------------------*
041700000000     C* DIVARE - RECUPERA AREA E DIVISIONE
041800000000     C*------------------------------------------------------------------------*
041900000000     C     DIVARE        BEGSR
042000000000     C*
042100000000     C                   Z-ADD     1             I
042200000000     C     WFIL          LOOKUP    FIL(I)                                 99
042300000000     C     *IN99         IFEQ      '0'                                          --- 3 -->
042400000000     C                   MOVEL     '*'           WDIV              1
042500000000     C                   Z-ADD     999           WARE              3 0
042600000000     C                   ELSE                                                   <-- 3 -->
042700000000     C                   MOVEL     DIV(I)        WDIV
042800000000     C                   Z-ADD     ARE(I)        WARE
042900000000     C                   END                                                    <-- 3 ---
043000000000     C*
043100000000     C                   ENDSR
043200000000     C*------------------------------------------------------------------------*
043300000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
043400000000     C*------------------------------------------------------------------------*
043500000000     C     CARORG        BEGSR
043600000000     C*
043700000000     C                   Z-ADD     *ZEROS        KORFIL
043800000000     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
043900000000     C  N99              READ      AZORG01L                               99
044000000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
044100000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
044200000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
044300000000     C     ORGFAG        IFNE      'V'
044400000000     C                   ADD       1             I
044500000000     C                   Z-ADD     ORGFIL        FIL(I)
044600000000     C                   Z-ADD     ORGCAR        ARE(I)
044700000000     C                   MOVEL     ORGFL3        DIV(I)
044800000000     C                   MOVEL     ORGDIT        DIT(I)
044900000000     C                   END
045000000000     C                   END                                                    <-- 2 ---
045100000000     C                   READ      AZORG01L                               99
045200000000     C                   END                                                    <-- 1 ---
045300000000     C*
045400000000     C                   ENDSR
045500000000     C*------------------------------------------------------------------------*
045600000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
045700000000     C*------------------------------------------------------------------------*
045800000000     C     CARTBL        BEGSR
045900000000     C*--------------------
046000000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
046100000000     C*--------------------
046200000000     C                   Z-ADD     0             I
046300000000     C                   Z-ADD     CODUT         KTBUTE
046400000000     C                   MOVEL     '2W'          KTBCOD
046500000000     C                   MOVEL     *BLANKS       KTBKEY
046600000000     C     KEYTAB        SETLL     TABEL00F
046700000000     C                   READ      TABEL00F                               99
046800000000     C     *IN99         DOWEQ     *OFF
046900000000     C     TBLCOD        ANDEQ     '2W'
047000000000     C     TBLFLG        IFEQ      *BLANKS
047100000000     C                   ADD       1             I
047200000000     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
047300000000     C                   MOVEL     TBLUNI        DS2W
047400000000     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
047500000000     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
047600000000     C                   END
047700000000     C                   READ      TABEL00F                               99
047800000000     C                   END
047900000000     C*--------------------
048000000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
048100000000     C*--------------------
048200000000     C                   Z-ADD     0             I
048300000000     C                   Z-ADD     CODUT         KTBUTE
048400000000     C                   MOVEL     'TB'          KTBCOD
048500000000     C                   MOVEL     *BLANKS       KTBKEY
048600000000     C     KEYTAB        SETLL     TABEL00F
048700000000     C                   READ      TABEL00F                               99
048800000000     C     *IN99         DOWEQ     *OFF
048900000000     C     TBLCOD        ANDEQ     'TB'
049000000000     C     TBLFLG        IFEQ      *BLANKS
049100000000     C                   ADD       1             I
049200000000     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
049300000000     C                   MOVEL     TBLUNI        DSTB
049400000000     C                   Z-ADD     1             J                 4 0
049500000000     C                   SETOFF                                       28
049600000000     C     §TBRBL        LOOKUP    RBL(J)                                 28
049700000000     C     *IN28         IFEQ      *ON
049800000000     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
049900000000     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
050000000000     C                   ELSE
050100000000     C                   MOVEL     'N'           SP(I)
050200000000     C                   MOVEL     'N'           RI(I)
050300000000     C                   ENDIF
050400000000     C                   END
050500000000     C                   READ      TABEL00F                               99
050600000000     C                   END
050700000000     C*
050800000000     C                   ENDSR
050900000000     C*------------------------------------------------------------------------*
051000991104     C* CANWFV - CANCELLA IL WFVLM00F TUTTI I RECORD DEI MESI RICHIESTI
051100000000     C*------------------------------------------------------------------------*
051200991029     C     CANWFV        BEGSR
051300000000     C*
051400000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
051500991029     C                   Z-ADD     PARANN        KWVANN
051600991029     C                   MOVEL     *BLANKS       KWVDIV
051700991029     C                   Z-ADD     *ZEROS        KWVARE
051800991029     C                   Z-ADD     *ZEROS        KWVFIL
051900991029     C                   MOVEL     *BLANKS       KWVRAG
052000991029     C                   Z-ADD     *ZEROS        KWVKSC
052100991029     C                   Z-ADD     *ZEROS        KWVCTR
052200991029     C                   Z-ADD     *ZEROS        KWVMES
052300991029     C                   MOVEL     *BLANKS       KWVFVB
052400991104     C     KEYWFV        SETLL     WFVLM01L
052500991104     C                   READ      WFVLM01L                               99
052600000000     C*
052700000000     C* CICLO FINO A FINE FILE OPPURE A MESE MAGGIORE MESE FINE
052800000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
052900991029     C     WFVANN        ANDEQ     PARANN
053000000000     C*
053100000000     C* ELIMINA RECORD SE ALL'INTERNO DEL RANGE DEI MESI RICHIESTI
053200991029     C     WFVMES        IFGE      PARMMI
053300991029     C     WFVMES        ANDLE     PARMMF
053400991029     C                   DELETE    WFVOL
053500000000     C                   END
053600000000     C*
053700000000     C* LEGGE IL RECORD SUCCESSIVO
053800991104     C                   READ      WFVLM01L                               99
053900000000     C*
054000000000     C                   END                                                    <-- 1 ---
054100000000     C*
054200000000     C                   ENDSR
054300991029     C*------------------------------------------------------------------------*
054400991029     C* FINSR - OPERAZIONI FINALI
054500991029     C*------------------------------------------------------------------------*
054600991029     C     FINSR         BEGSR
054700991029     C*
054800991029     C* CHIUDE I FILE DEI PGM CHIAMATI
054900991029     C                   CLEAR                   BS69DS
055000991029     C                   CLEAR                   ACODS
055100991029     C                   CLEAR                   INDDS
055200991029     C                   CLEAR                   CLPDS
055300991029     C                   CLEAR                   CLSDS
055400991029     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
055500991029     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
055600991029     C                   CALL      'TIBS69R'
055700991029     C                   PARM                    BS69DS
055800991029     C                   PARM                    ACODS
055900991029     C                   PARM                    INDDS
056000991029     C                   PARM                    CLPDS
056100991029     C                   PARM                    CLSDS
056200991029     C*
056300991029     C                   ENDSR
056400000000     C*------------------------------------------------------------------------*
056500000000     C* *INZSR - ROUTINE INIZIALE
056600000000     C*------------------------------------------------------------------------*
056700000000     C     *INZSR        BEGSR
056800000000     C*--------------------
056900000000     C* RICEVIMENTO PARAMETRI
057000000000     C*--------------------
057100000000     C     *ENTRY        PLIST
057200000000     C                   PARM                    KPJBA
057300000000     C                   MOVEL     KPJBU         PARAM
057400000000     C*--------------------
057500000000     C* CAMPI RIFERITI AL DATA BASE
057600000000     C*--------------------
057700000000     C     *LIKE         DEFINE    TASAAS        KTAAAS
057800000000     C     *LIKE         DEFINE    TASMGS        KTAMGS
057900000000     C     *LIKE         DEFINE    TASKSC        KTAKSC
058000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
058100000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
058200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
058300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
058400991029     C     *LIKE         DEFINE    WFVANN        KWVANN
058500991029     C     *LIKE         DEFINE    WFVDIV        KWVDIV
058600991029     C     *LIKE         DEFINE    WFVARE        KWVARE
058700991029     C     *LIKE         DEFINE    WFVFIL        KWVFIL
058800991029     C     *LIKE         DEFINE    WFVRAG        KWVRAG
058900991029     C     *LIKE         DEFINE    WFVKSC        KWVKSC
059000991029     C     *LIKE         DEFINE    WFVCTR        KWVCTR
059100991029     C     *LIKE         DEFINE    WFVMES        KWVMES
059200991029     C     *LIKE         DEFINE    WFVFVB        KWVFVB
059300000000     C     *LIKE         DEFINE    TASKSC        WCLI
059400000000     C     *LIKE         DEFINE    TASCTR        WCTR
059500991029     C     *LIKE         DEFINE    TASVLC        WVLC
059600000000     C*--------------------
059700000000     C* CHIAVI DI LETTURA
059800000000     C*--------------------
059900000000     C* CHIAVE LETTURA TITAS38C - COMPLETA
060000000000     C     KEYTAS        KLIST
060100000000     C                   KFLD                    KTAAAS
060200000000     C                   KFLD                    KTAMGS
060300000000     C                   KFLD                    KTAKSC
060400000000     C*
060500000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
060600000000     C     KEYORG        KLIST
060700000000     C                   KFLD                    KORFIL
060800000000     C*
060900000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
061000000000     C     KEYTAB        KLIST
061100000000     C                   KFLD                    KTBUTE
061200000000     C                   KFLD                    KTBCOD
061300000000     C                   KFLD                    KTBKEY
061400000000     C*
061500991104     C* CHIAVE LETTURA WFVLM01L - COMPLETA
061600991029     C     KEYWFV        KLIST
061700991029     C                   KFLD                    KWVANN
061800991029     C                   KFLD                    KWVDIV
061900991029     C                   KFLD                    KWVARE
062000991029     C                   KFLD                    KWVFIL
062100991029     C                   KFLD                    KWVRAG
062200991029     C                   KFLD                    KWVKSC
062300991029     C                   KFLD                    KWVCTR
062400991029     C                   KFLD                    KWVMES
062500991029     C                   KFLD                    KWVFVB
062600000000     C*--------------------
062700000000     C* CAMPI DI CONTROLLO
062800000000     C*--------------------
062900000000     C                   MOVEL     '0'           FINL00            1
063000000000     C                   Z-ADD     1             CODUT             1 0
063100991025     C                   Z-ADD     *ZEROS        MEMCLI            7 0
063200991025     C                   Z-ADD     *ZEROS        MEMCTR            3 0
063300000000     C*
063400000000     C***
063500000000     C* LANCIO PER SALDI
063600000000     C***
063700000000     C* CARICA ORGANIGRAMMA
063800000000     C                   EXSR      CARORG
063900000000     C*
064000000000     C* CARICA TIPI BOLLA
064100000000     C                   EXSR      CARTBL
064200000000     C*
064300991104     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA WFVLM00F
064400991029     C                   EXSR      CANWFV
064500000000     C*
064600000000     C                   ENDSR
064700000216**   DEC - DECODIFICHE
064800000216MITTENTE NON CODIFICATO                 1
0649000002162
0650000002163
0651000002164
0652000002165

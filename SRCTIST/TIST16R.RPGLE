000100000000     H*------------------------------------------------------------------------*
000200000000     H* STATISTICA PESO VDL - GENERA WRK FILE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     FTITAS38C  IF   E           K DISK
000600000000     FAZORG01L  IF   E           K DISK
000700000000     FTABEL00F  IF   E           K DISK
000800000000     FWFPES01L  UF A E           K DISK
000900000000     D*------------------------------------------------------------------------*
001000000000     D* SCHIERE
001100000000     D*------------------------------------------------------------------------*
001200000000     D*-------------------
001300000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
001400000000     D*-------------------
001500000000     D FIL             S              3  0 DIM(500)                             *FILIALE
001600000000     D ARE             S              3  0 DIM(500)                             *AREA
001700000000     D DIV             S              1    DIM(500)                             *DIVISIONE
001800000000     D DIT             S              3    DIM(500)                             *RAMO AZIENDALE
001900000000     D*-------------------
002000000000     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
002100000000     D*-------------------
002200000000     D RBL             S              1    DIM(5)                               RAGGRUPPAMENTO BOLLA
002300000000     D FSP             S              1    DIM(5)                               FLAG SPEDIZIONE
002400000000     D FRI             S              1    DIM(5)                               FLAG RICAVO
002500000000     D*-------------------
002600000000     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
002700000000     D*-------------------
002800000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
002900000000     D SP              S              1    DIM(20)                              FLAG SPEDIZIONE
003000000000     D RI              S              1    DIM(20)                              FLAG RICAVO
003100000000     D*------------------------------------------------------------------------*
003200000000     D* INPUT
003300000000     D*------------------------------------------------------------------------*
003400000000     D*-------------------
003500000000     D* ARCHITETTURA
003600000000     D*-------------------
003700000000     D KPJBA         E DS
003800000000     D*-------------------
003900000000     D* DS TIPO BOLLA
004000000000     D*-------------------
004100000000     D DSTB          E DS
004200000000     D*-------------------
004300000000     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
004400000000     D*-------------------
004500000000     D DS2W          E DS
004600040220     D*------------------
004700040220     D* DS REPERIMENTO DATI UTENTE
004800040220     D*-------------------
004900040220     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
005000040220     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
005100040220     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
005200040220     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
005300040220     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
005400000000     D*-------------------
005500000000     D* DS SCOMPOSIZIONE DATE INIZ/FIN/WRK
005600000000     D*-------------------
005700991028     D                 DS                  INZ
005800991028     D  MM                     1      2  0
005900991028     D  MMGG                   1      4  0
006000000000     D                 DS                  INZ
006100000000     D  DIANN                  1      4  0
006200000000     D  DIMES                  5      6  0
006300000000     D  DIMGS                  5      8  0
006400000000     D  DIGIO                  7      8  0
006500000000     D  DIAAM                  1      6  0
006600991213     D  DIDAT                  1      8  0
006700000000     D                 DS                  INZ
006800000000     D  DFANN                  1      4  0
006900000000     D  DFMES                  5      6  0
007000000000     D  DFMGS                  5      8  0
007100000000     D  DFGIO                  7      8  0
007200000000     D  DFAAM                  1      6  0
007300000000     D*-------------------
007400000000     D* DS SCOMPOSIZIONE CODICE CLIENTE
007500000000     D*-------------------
007600000000     D                 DS
007700000000     D  DSCLI                  1      7  0                                      *CLIEN
007800000000     D  DSFIL                  1      3  0                                      *FIL
007900000000     D  DSCOD                  4      7  0                                      *COD
008000000000     D*-------------------
008100000000     D* DS PARAMETRI DI LANCIO PGM
008200000000     D*-------------------
008300000000     D PARAM           DS
008400000000     D  PARTLA                 1      1
008500000000     D  PARANN                 2      5  0
008600000000     D  PARMMI                 6      7  0
008700000000     D  PARMMF                 8      9  0
008800000000     D  PARSNR                10     10
008900000000     D  PARDTI                11     18  0
009000000000     D  PARDTF                19     26  0
009100991025     D  PARPEI                27     31  2
009200000000     D  PARDIV                32     32
009300000000     D  PARARE                33     35  0
009400000000     D  PARFIL                36     38  0
009500000000     D  PARDET                39     39
009600000000     D  PARTOT                40     40
009700991026     D  PARRG1                41     44  1
009800991026     D  PARRG2                45     48  1
009900991026     D  PARTAR                49     53  3
010000991026     D  PARPES                54     58  2
010100991229     D  PARMIN                59     63  3
010200000706     D  PARPOS                64     64
010300011112     D  PARNEG                65     65
010400050124     D  PARSAL                66     66
010500000000     C*------------------------------------------------------------------------*
010600000000     C* MAIN LINE
010700000000     C*------------------------------------------------------------------------*
010800000000     C*
010900000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
011000000000     C     FINL00        IFEQ      '0'                                          --- 1 -->
011100000000     C*
011200000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
011300000000     C                   EXSR      SETFIL
011400000000     C*
011500000000     C* CICLO FINO A FINE FILE
011600000000     C     FINL00        DOWEQ     '0'                                          --- 2 -->
011700000000     C*
011800000000     C* CREA IL RECORD SUI FILE DI LAVORO
011900000000     C                   EXSR      CREARE
012000000000     C*
012100000000     C* LEGGE IL RECORD SUCCESSIVO
012200000000     C                   EXSR      LETFIL
012300000000     C                   ENDDO                                                  <-- 2 ---
012400000000     C*
012500000000     C                   ENDIF                                                  <-- 1 ---
012600000000     C*
012700000000     C                   SETON                                        LR
012800000000     C*------------------------------------------------------------------------*
012900000000     C* SETFIL - POSIZIONAMENTO SUL FILE
013000000000     C*------------------------------------------------------------------------*
013100000000     C     SETFIL        BEGSR
013200000000     C*
013300000000     C* POSIZIONAMENTO E PRIMA LETTURA
013400000000     C                   MOVE      PARANN        DIANN
013500000000     C                   MOVE      PARMMI        DIMES
013600000000     C                   MOVE      PARANN        DFANN
013700000000     C                   MOVE      PARMMF        DFMES
013800000000     C                   MOVE      '99'          DFGIO
013900991123     C*
014000991213 VED C*  MINIMO 15/11/99 : PRIMA NON PESAVA BENE
014100991213  "  C*
014200991214     C* RIPETO LA FORZATURA DELLA DATA X IMPOSTARE ANCHE IL GIORNO
014300991214     C* (QUESTO E' NECESSARIO SOLO IN FASE DI CREAZONE WRK FILE)
014400991213     C     DIAAM         IFLT      199912
014500991213     C                   Z-ADD     19991115      DIDAT
014600991213     C                   ENDIF
014700991213  "  C*          DIANN     IFLE 1999
014800991213  "  C*          DIMES     ANDLT12
014900991213  "  C*                    MOVE 15        DIMGS
015000991213 VED C*                    ENDIF
015100000000     C*
015200000000     C                   Z-ADD     DIANN         KTAAAS
015300000000     C                   Z-ADD     DIMGS         KTAMGS
015400000000     C                   Z-ADD     *ZEROS        KTAKSC
015500991123     C*
015600000000     C     KEYTAS        SETLL     TITAS38C                           99        *FINE FILE
015700000000     C*
015800000000     C     *IN99         IFEQ      '1'
015900000000     C                   MOVEL     '1'           FINL00
016000000000     C                   ELSE
016100000000     C                   EXSR      LETFIL
016200000000     C                   END
016300000000     C*
016400000000     C                   ENDSR
016500000000     C*------------------------------------------------------------------------*
016600000000     C* LETFIL - LETTURA PROSSIMO RECORD
016700000000     C*------------------------------------------------------------------------*
016800000000     C     LETFIL        BEGSR
016900000000     C*
017000000000     C* LEGGO FINO A:
017100000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
017200000000     C*     - TROVATO RECORD VALIDO
017300000000     C                   MOVEL     'N'           WRECOK
017400000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
017500000000     C     WRECOK        ANDEQ     'N'
017600000000     C*
017700000000     C* LETTURE SUCCESSIVE ALLA PRIMA
017800000000     C                   READ      TITAS38C                               99
017900000000     C*
018000000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
018100000000     C     TASAAS        ORGT      PARANN                                       *SUPERATO RANGE PERI
018200000000     C     TASMGS        ORGT      DFMGS
018300000000     C                   MOVEL     '1'           FINL00
018400000000     C                   ELSE
018500000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
018600000000     C                   END
018700000000     C*
018800000000     C                   END                                                    <-- 1 ---
018900000000     C*
019000000000     C                   ENDSR
019100000000     C*------------------------------------------------------------------------*
019200000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
019300000000     C*------------------------------------------------------------------------*
019400000000     C     CHKREC        BEGSR
019500000000     C*
019600000000     C                   MOVEL     'S'           WRECOK            1
019700000000     C*
019800040305     C     TASNCP        IFEQ      99999
019900040305     C                   MOVEL     'N'           WRECOK
020000040305     C                   ENDIF
020100040305     C*
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
023300000000     C                   ENDIF
023400040220     C*
023500040220     C* DECODIFICO LA RAGIONE SOCIALE DEL CLIENTE GIA' CONVERTITO
023600040220     C                   EXSR      DECCLI
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
027300011112     C*
027400011112     C* SE NEL LANCIO RICHIESTO SOLO SPEDIZIONI "NEGATIVE" (OVVERO CON BESO BOLLETTATO <
027500011112     C* PESO RILEVATO) => ESCUDO I RECORD CHE NON SODDISFANO LA CONDIZOINE RICHIESTA
027600011112     C     PARNEG        IFEQ      'S'
027700011112     C     TASPKB        IFGE      TASPKC
027800011112     C                   MOVEL     'N'           WRECOK
027900011112     C                   ENDIF
028000011112     C                   ENDIF
028100011112     C*
028200000000     C                   ENDSR
028300000000     C*------------------------------------------------------------------------*
028400000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
028500000000     C*------------------------------------------------------------------------*
028600000000     C     CREARE        BEGSR
028700991025     C*
028800000000     C                   Z-ADD     TASMGS        MMGG
028900991025     C*
029000000000     C                   Z-ADD     TASAAS        KWPANN
029100000000     C                   MOVEL     WDIV          KWPDIV
029200000000     C                   Z-ADD     WARE          KWPARE
029300000000     C                   Z-ADD     WFIL          KWPFIL
029400040220     C                   MOVEL     WFPRAG        KWPRAG
029500000000     C                   Z-ADD     WCLI          KWPKSC
029600000000     C                   Z-ADD     WCTR          KWPCTR
029700000000     C                   Z-ADD     MM            KWPMES
029800000000     C*
029900000000     C* SCRIVO O RISCRIVO FILE DI LAVORO PER STAMPA STATISTICA PESI
030000000000     C*
030100000000     C     KEYWFP        CHAIN     WFPES01L                           99
030200000000     C*
030300000000     C* RIEMPIO BUFFER RECORD
030400000000     C*
030500000000     C                   Z-ADD     KWPANN        WFPANN
030600000000     C                   MOVEL     KWPDIV        WFPDIV
030700000000     C                   Z-ADD     KWPARE        WFPARE
030800000000     C                   Z-ADD     KWPFIL        WFPFIL
030900000000     C                   Z-ADD     KWPKSC        WFPKSC
031000000000     C                   Z-ADD     KWPCTR        WFPCTR
031100000000     C                   Z-ADD     KWPMES        WFPMES
031200991025     C   99              EXSR      AZZWFP                                       *RE-INIZIALIZZA BUFF
031300000000     C                   EXSR      ADDWFP                                       *AGGIUNGE SPED.AL RE
031400000000     C   99              WRITE     WFPES                                        *RECORD NON ESISTE,
031500000000     C  N99              UPDATE    WFPES                                        *RECORD ESISTE, AGGI
031600991025     C*
031700000000     C                   ENDSR
031800000000     C*------------------------------------------------------------------------*
031900000000     C* ADDWFP - AGGIUNGE LA SPEDIZIONE AL RECORD WFPES00F
032000000000     C*------------------------------------------------------------------------*
032100000000     C     ADDWFP        BEGSR
032200991025     C*
032300991025     C                   ADD       1             WFPTNS
032400991025     C                   ADD       TASPKB        WFPTPB
032500991025     C                   ADD       TASNCL        WFPTCL
032600991025     C*
032700991025     C* CONTROLLO SE NUM. COLLI BOLL. = NUM. COLLI VDL
032800991025     C* SE DIVERSO RIEMPIO CAMPI SE PESO VDL COMUNQUE
032900991025     C* > PESO BOLLETTATO                                              RILEVATI
033000991025     C*
033100991025     C                   Z-ADD     0             VDLTAR            8 3
033200991025     C                   Z-ADD     0             VDLNET            9 1
033300991025     C     TASNCP        MULT      PARTAR        VDLTAR
033400991025     C     TASPKC        SUB       VDLTAR        VDLNET
033500991025     C*
033600991025     C     TASNCL        IFNE      TASNCP
033700991026     C     VDLNET        IFGT      TASPKB
033800000000     C                   ADD       VDLNET        WFPPSC
033900000000     C                   ADD       TASNCL        WFPNCL
034000000000     C                   ADD       1             WFPNSP
034100000000     C                   ADD       TASPKB        WFPPSB
034200000000     C                   ADD       TASNCP        WFPNCV
034300991026     C                   ENDIF
034400991026     C                   ELSE
034500991026     C                   ADD       VDLNET        WFPPSC
034600991026     C                   ADD       TASNCL        WFPNCL
034700991026     C                   ADD       1             WFPNSP
034800991026     C                   ADD       TASPKB        WFPPSB
034900991026     C                   ADD       TASNCP        WFPNCV
035000991026     C                   ENDIF
035100991025     C*
035200000000     C                   ENDSR
035300000000     C*------------------------------------------------------------------------*
035400000000     C* AZZWFP - AZZERA CAMPI RANGE IN WFPES00F
035500000000     C*------------------------------------------------------------------------*
035600000000     C     AZZWFP        BEGSR
035700000000     C*
035800991025     C                   Z-ADD     *ZEROS        WFPTCL
035900991025     C                   Z-ADD     *ZEROS        WFPTPB
036000991025     C                   Z-ADD     *ZEROS        WFPTNS
036100000000     C                   Z-ADD     *ZEROS        WFPPSC
036200000000     C                   Z-ADD     *ZEROS        WFPNCL
036300000000     C                   Z-ADD     *ZEROS        WFPNSP
036400000000     C                   Z-ADD     *ZEROS        WFPPSB
036500000000     C                   Z-ADD     *ZEROS        WFPNCV
036600000000     C*
036700000000     C                   ENDSR
036800040220     C*------------------------------------------------------------------------*
036900040220     C* DECCLI - DECODIFICA CLIENTE
037000040220     C*------------------------------------------------------------------------*
037100040220     C     DECCLI        BEGSR
037200040220     C*
037300040220     C                   Z-ADD     WCLI          DSCLI
037400040220IF  1C     DSCOD         IFEQ      8888                                         *MITTENTE NON COD
037500040226     c                   EVAL      WFPRAG = 'Cliente non codificato'
037600040220X   1C                   ELSE                                                   *CODIFICATI
037700040220     C                   CLEAR                   BS69DS
037800040220     C                   CLEAR                   ACODS
037900040220     C                   MOVEL     KNSIF         I69SIF                         *S.I.
038000040220     C                   Z-ADD     WCLI          I69KAC                         *CLIENTE X CNACO
038100040220     C                   CALL      'TIBS69R'
038200040220     C                   PARM                    BS69DS
038300040220     C                   PARM                    ACODS
038400040220     C                   PARM                    INDDS
038500040220     C                   PARM                    CLPDS
038600040220     C                   PARM                    CLSDS
038700040220     C     O69ERR        IFNE      '1'                                          *NO ERRORE
038800040220     C                   MOVEL     ACORAG        WFPRAG
038900040220     C                   ELSE
039000040220     C                   MOVEL     *ALL'*'       WFPRAG
039100040220     C                   ENDIF
039200040220E   1C                   ENDIF
039300040220     C*
039400040220     C                   ENDSR
039500000000     C*------------------------------------------------------------------------*
039600000000     C* DIVARE - RECUPERA AREA E DIVISIONE
039700000000     C*------------------------------------------------------------------------*
039800000000     C     DIVARE        BEGSR
039900000000     C*
040000000000     C                   Z-ADD     1             I
040100000000     C     WFIL          LOOKUP    FIL(I)                                 99
040200000000     C     *IN99         IFEQ      '0'                                          --- 3 -->
040300000000     C                   MOVEL     '*'           WDIV              1
040400000000     C                   Z-ADD     999           WARE              3 0
040500000000     C                   ELSE                                                   <-- 3 -->
040600000000     C                   MOVEL     DIV(I)        WDIV
040700000000     C                   Z-ADD     ARE(I)        WARE
040800000000     C                   END                                                    <-- 3 ---
040900000000     C*
041000000000     C                   ENDSR
041100000000     C*------------------------------------------------------------------------*
041200000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
041300000000     C*------------------------------------------------------------------------*
041400000000     C     CARORG        BEGSR
041500000000     C*
041600000000     C                   Z-ADD     *ZEROS        KORFIL
041700000000     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
041800000000     C  N99              READ      AZORG01L                               99
041900000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
042000000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
042100000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
042200000000     C     ORGFAG        IFNE      'V'
042300000000     C                   ADD       1             I
042400000000     C                   Z-ADD     ORGFIL        FIL(I)
042500000000     C                   Z-ADD     ORGCAR        ARE(I)
042600000000     C                   MOVEL     ORGFL3        DIV(I)
042700000000     C                   MOVEL     ORGDIT        DIT(I)
042800000000     C                   END
042900000000     C                   END                                                    <-- 2 ---
043000000000     C                   READ      AZORG01L                               99
043100000000     C                   END                                                    <-- 1 ---
043200000000     C*
043300000000     C                   ENDSR
043400000000     C*------------------------------------------------------------------------*
043500000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
043600000000     C*------------------------------------------------------------------------*
043700000000     C     CARTBL        BEGSR
043800000000     C*--------------------
043900000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
044000000000     C*--------------------
044100000000     C                   Z-ADD     0             I
044200000000     C                   Z-ADD     CODUT         KTBUTE
044300000000     C                   MOVEL     '2W'          KTBCOD
044400000000     C                   MOVEL     *BLANKS       KTBKEY
044500000000     C     KEYTAB        SETLL     TABEL00F
044600000000     C                   READ      TABEL00F                               99
044700000000     C     *IN99         DOWEQ     *OFF
044800000000     C     TBLCOD        ANDEQ     '2W'
044900000000     C     TBLFLG        IFEQ      *BLANKS
045000000000     C                   ADD       1             I
045100000000     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
045200000000     C                   MOVEL     TBLUNI        DS2W
045300000000     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
045400000000     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
045500000000     C                   END
045600000000     C                   READ      TABEL00F                               99
045700000000     C                   END
045800000000     C*--------------------
045900000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
046000000000     C*--------------------
046100000000     C                   Z-ADD     0             I
046200000000     C                   Z-ADD     CODUT         KTBUTE
046300000000     C                   MOVEL     'TB'          KTBCOD
046400000000     C                   MOVEL     *BLANKS       KTBKEY
046500000000     C     KEYTAB        SETLL     TABEL00F
046600000000     C                   READ      TABEL00F                               99
046700000000     C     *IN99         DOWEQ     *OFF
046800000000     C     TBLCOD        ANDEQ     'TB'
046900000000     C     TBLFLG        IFEQ      *BLANKS
047000000000     C                   ADD       1             I
047100000000     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
047200000000     C                   MOVEL     TBLUNI        DSTB
047300000000     C                   Z-ADD     1             J                 4 0
047400000000     C                   SETOFF                                       28
047500000000     C     §TBRBL        LOOKUP    RBL(J)                                 28
047600000000     C     *IN28         IFEQ      *ON
047700000000     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
047800000000     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
047900000000     C                   ELSE
048000000000     C                   MOVEL     'N'           SP(I)
048100000000     C                   MOVEL     'N'           RI(I)
048200000000     C                   ENDIF
048300000000     C                   END
048400000000     C                   READ      TABEL00F                               99
048500000000     C                   END
048600000000     C*
048700000000     C                   ENDSR
048800000000     C*------------------------------------------------------------------------*
048900000000     C* CANWFP - CANCELLA IL WFPES00F TUTTI I RECORD DEI MESI RICHIESTI
049000000000     C*------------------------------------------------------------------------*
049100000000     C     CANWFP        BEGSR
049200000000     C*
049300000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
049400000000     C                   Z-ADD     PARANN        KWPANN
049500000000     C                   MOVEL     *BLANKS       KWPDIV
049600000000     C                   Z-ADD     *ZEROS        KWPARE
049700000000     C                   Z-ADD     *ZEROS        KWPFIL
049800040220     C                   MOVEL     *BLANKS       KWPRAG
049900000000     C                   Z-ADD     *ZEROS        KWPKSC
050000000000     C                   Z-ADD     *ZEROS        KWPCTR
050100000000     C                   Z-ADD     *ZEROS        KWPMES
050200000000     C     KEYWFP        SETLL     WFPES01L
050300000000     C                   READ      WFPES01L                               99
050400000000     C*
050500000000     C* CICLO FINO A FINE FILE OPPURE A MESE MAGGIORE MESE FINE
050600000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
050700000000     C     WFPANN        ANDEQ     PARANN
050800000000     C*
050900000000     C* ELIMINA RECORD SE ALL'INTERNO DEL RANGE DEI MESI RICHIESTI
051000000000     C     WFPMES        IFGE      PARMMI
051100000000     C     WFPMES        ANDLE     PARMMF
051200000000     C                   DELETE    WFPES
051300000000     C                   END
051400000000     C*
051500000000     C* LEGGE IL RECORD SUCCESSIVO
051600000000     C                   READ      WFPES01L                               99
051700000000     C*
051800000000     C                   END                                                    <-- 1 ---
051900000000     C*
052000000000     C                   ENDSR
052100040220     C*------------------------------------------------------------------------*
052200040220     C* FINSR - OPERAZIONI FINALI
052300040220     C*------------------------------------------------------------------------*
052400040220     C     FINSR         BEGSR
052500040220     C*
052600040220     C* CHIUDE I FILE DEI PGM CHIAMATI
052700040220     C                   CLEAR                   BS69DS
052800040220     C                   CLEAR                   ACODS
052900040220     C                   CLEAR                   INDDS
053000040220     C                   CLEAR                   CLPDS
053100040220     C                   CLEAR                   CLSDS
053200040220     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
053300040220     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
053400040220     C                   CALL      'TIBS69R'
053500040220     C                   PARM                    BS69DS
053600040220     C                   PARM                    ACODS
053700040220     C                   PARM                    INDDS
053800040220     C                   PARM                    CLPDS
053900040220     C                   PARM                    CLSDS
054000040220     C*
054100040220     C                   ENDSR
054200000000     C*------------------------------------------------------------------------*
054300000000     C* *INZSR - ROUTINE INIZIALE
054400000000     C*------------------------------------------------------------------------*
054500000000     C     *INZSR        BEGSR
054600000000     C*--------------------
054700000000     C* RICEVIMENTO PARAMETRI
054800000000     C*--------------------
054900000000     C     *ENTRY        PLIST
055000000000     C                   PARM                    KPJBA
055100000000     C                   MOVEL     KPJBU         PARAM
055200000000     C*--------------------
055300000000     C* CAMPI RIFERITI AL DATA BASE
055400000000     C*--------------------
055500000000     C     *LIKE         DEFINE    TASAAS        KTAAAS
055600000000     C     *LIKE         DEFINE    TASMGS        KTAMGS
055700000000     C     *LIKE         DEFINE    TASKSC        KTAKSC
055800000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
055900000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
056000000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
056100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
056200000000     C     *LIKE         DEFINE    WFPANN        KWPANN
056300000000     C     *LIKE         DEFINE    WFPDIV        KWPDIV
056400000000     C     *LIKE         DEFINE    WFPARE        KWPARE
056500000000     C     *LIKE         DEFINE    WFPFIL        KWPFIL
056600040220     C     *LIKE         DEFINE    WFPRAG        KWPRAG
056700000000     C     *LIKE         DEFINE    WFPKSC        KWPKSC
056800000000     C     *LIKE         DEFINE    WFPCTR        KWPCTR
056900000000     C     *LIKE         DEFINE    WFPMES        KWPMES
057000000000     C     *LIKE         DEFINE    TASKSC        WCLI
057100000000     C     *LIKE         DEFINE    TASCTR        WCTR
057200000000     C*--------------------
057300000000     C* CHIAVI DI LETTURA
057400000000     C*--------------------
057500000000     C* CHIAVE LETTURA TITAS38C - COMPLETA
057600000000     C     KEYTAS        KLIST
057700000000     C                   KFLD                    KTAAAS
057800000000     C                   KFLD                    KTAMGS
057900000000     C                   KFLD                    KTAKSC
058000000000     C*
058100000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
058200000000     C     KEYORG        KLIST
058300000000     C                   KFLD                    KORFIL
058400000000     C*
058500000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
058600000000     C     KEYTAB        KLIST
058700000000     C                   KFLD                    KTBUTE
058800000000     C                   KFLD                    KTBCOD
058900000000     C                   KFLD                    KTBKEY
059000000000     C*
059100000000     C* CHIAVE LETTURA WFPES01L - COMPLETA
059200000000     C     KEYWFP        KLIST
059300000000     C                   KFLD                    KWPANN
059400000000     C                   KFLD                    KWPDIV
059500000000     C                   KFLD                    KWPARE
059600000000     C                   KFLD                    KWPFIL
059700040220     C                   KFLD                    KWPRAG
059800000000     C                   KFLD                    KWPKSC
059900000000     C                   KFLD                    KWPCTR
060000000000     C                   KFLD                    KWPMES
060100000000     C*--------------------
060200000000     C* CAMPI DI CONTROLLO
060300000000     C*--------------------
060400000000     C                   MOVEL     '0'           FINL00            1
060500000000     C                   Z-ADD     1             CODUT             1 0
060600991025     C                   Z-ADD     *ZEROS        MEMCLI            7 0
060700991025     C                   Z-ADD     *ZEROS        MEMCTR            3 0
060800000000     C*
060900000000     C***
061000000000     C* LANCIO PER SALDI
061100000000     C***
061200000000     C* CARICA ORGANIGRAMMA
061300000000     C                   EXSR      CARORG
061400000000     C*
061500000000     C* CARICA TIPI BOLLA
061600000000     C                   EXSR      CARTBL
061700000000     C*
061800000000     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA WFPES00F
061900000000     C                   EXSR      CANWFP
062000000000     C*
062100000000     C                   ENDSR

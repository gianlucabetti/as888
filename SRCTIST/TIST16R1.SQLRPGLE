000100050110     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* STATISTICA PESO VDL - GENERA WRK FILE
000400000000     H*------------------------------------------------------------------------*
000500050121     FAZORG01L  IF   E           K DISK
000600050110     FWFPES00f  iF   E             DISK
000700050107     FSIPES02L  UF A E           K DISK
000800000000     D*------------------------------------------------------------------------*
000900000000     D KPJBA         E DS
001000000000     D*-------------------
001100050110     D wfPESds       E DS                  EXTNAME(wfpes00f)
001200050121     D*-------------------
001300050121     D* LETTURA CAMPO ORGDE£ DI AZORG
001400050121     D*-------------------
001500050121     D OG143         E DS
001600050121     D*-------------------
001700050121     D* REPERIMENTO CLIENTI CON/SENZA IMPIANTO
001800050121     D*-------------------
001900050121     D OG150         E DS
002000050121     D*------------------
002100050121     D* DS REPEIMENTO TERMINALs
002200050121     D*------------------
002300050121     D FNLV55DS      E DS
002400050121     D*------------------
002500050121     D* DS CONTROLLO DATA (8)
002600050121     D*------------------
002700050121     D WLBDA8          DS                  INZ
002800050121     D  G08DAT                 1      8  0
002900050121     D  G08INV                 9     16  0
003000050121     D  G08ERR                17     17
003100050121     D  G08TGI                18     22  0
003200050121     D*-------------------
003300050121     D* SCHIERA FILIALI
003400050121     D*-------------------
003500160708     D*** FIL             S              3  0 DIM(500)                             *CODICE FILIALE
003600160708     D*** DFIL            S             20    DIM(500)                             *DESCR FILIALE
003700160708     D*** PFIL            S              3    DIM(500)                             *NETWORK POSTE
003800050110     D*-------------------
003900000000     D* DS PARAMETRI DI LANCIO PGM
004000000000     D*-------------------
004100000000     D PARAM           DS
004200000000     D  PARTLA                 1      1
004300000000     D  PARANN                 2      5  0
004400000000     D  PARMMI                 6      7  0
004500000000     D  PARMMF                 8      9  0
004600000000     D  PARSNR                10     10
004700000000     D  PARDTI                11     18  0
004800000000     D  PARDTF                19     26  0
004900991025     D  PARPEI                27     31  2
005000000000     D  PARDIV                32     32
005100000000     D  PARARE                33     35  0
005200000000     D  PARFIL                36     38  0
005300000000     D  PARDET                39     39
005400000000     D  PARTOT                40     40
005500991026     D  PARRG1                41     44  1
005600991026     D  PARRG2                45     48  1
005700991026     D  PARTAR                49     53  3
005800991026     D  PARPES                54     58  2
005900991229     D  PARMIN                59     63  3
006000000706     D  PARPOS                64     64
006100011112     D  PARNEG                65     65
006200050107     D  PARSAL                66     66
006300000000     C*------------------------------------------------------------------------*
006400000000     C* MAIN LINE
006500000000     C*------------------------------------------------------------------------*
006600050110     C     *ENTRY        PLIST
006700050110     C                   PARM                    KPJBA
006800050110     C                   MOVEL     KPJBU         PARAM
006900050121     C*--------------------
007000050121     C* REPERISCE DATA/ORA CORRENTE
007100050121     C*--------------------
007200050121     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
007300050121     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
007400050121     C                   Z-ADD     *ZEROS        G08INV
007500050121     C                   MOVEL     '0'           G08ERR
007600050121     C                   CALL      'XSRDA8'
007700050121     C                   PARM                    WLBDA8
007800050121     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
007900050121     C*
008000050121     C* CARICA ORGANIGRAMMA
008100160708     C****               EXSR      CARORG
008200050121     C*
008300050110     C*--------------------
008400050110     C* CAMPI RIFERITI AL DATA BASE
008500050110     C*--------------------
008600050110     C     *LIKE         DEFINE    WFPANN        KWPANN
008700050110     C     *LIKE         DEFINE    WFPMES        KWPMES
008800050121     C     *LIKE         DEFINE    WFPFIL        DEPFIL
008900050121     C     *LIKE         DEFINE    WFPFIL        DEPTFP
009000050121     C     *LIKE         DEFINE    ORGFIL        KORFIL
009100050110     C*--------------------
009200050110     C* CHIAVI DI LETTURA
009300050110     C*--------------------
009400050121     C* CHIAVE LETTURA AZORG01L - COMPLETA
009500050121     C     KEYORG        KLIST
009600050121     C                   KFLD                    KORFIL
009700050121     C*
009800050110     C* CHIAVE LETTURA WFPES01L - COMPLETA
009900050110     C     KeysiPes      KLIST
010000050110     C                   KFLD                    KWPANN
010100050110     C                   KFLD                    KWPMES
010200050110     C*--------------------
010300050110     C* CAMPI DI CONTROLLO
010400050110     C*--------------------
010500050110     C                   Z-ADD     1             CODUT             1 0
010600050110     C***
010700050110     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA SIPES00F
010800050110     C                   EXSR      CANsiPES
010900050110     C* LANCIO PER SALDI
011000050110     C                   EXSR      CreaSIPES
011100000000     C*
011200000000     C                   SETON                                        LR
011300050110     C*------------------------------------------------------------------------*
011400050110     C* CANsiPES  - CANCELLA IL SIPES00F TUTTI I RECORD DEI MESI RICHIESTI
011500050110     C*------------------------------------------------------------------------*
011600050110     C     CANsiPES      BEGSR
011700050110     C*
011800050110     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
011900050110     C                   Z-ADD     PARANN        KWPANN
012000050110     C                   Z-ADD     parMMi        KWPMES
012100050110     C     KeysiPes      chain     siPES02L                           99
012200050110     C*
012300050110     C* CICLO FINO A FINE FILE OPPURE A MESE MAGGIORE MESE FINE
012400050110     C     *IN99         DOWEQ     *Off                                         --- 1 -->
012500050121     C     pesMES        IFGE      PARMMI
012600050121     C     pesMES        ANDLE     PARMMF
012700050110     C* ELIMINA RECORD SE ALL'INTERNO DEL RANGE DEI MESI RICHIESTI
012800050110     C                   DELETE    siPES000
012900050121     C                   END                                                    <-- 1 ---
013000050110     C* LEGGE IL RECORD SUCCESSIVO
013100050110     C     KwpANN        reade     siPES02L                               99
013200050110     C                   ENDdo                                                  <-- 1 ---
013300050110     C*
013400050110     C                   ENDSR
013500050110     C*------------------------------------------------------------------------*
013600050110     C* CREAsiPES  - Crea i records dei Saldi nel range dei mesi richiesti
013700050110     C*------------------------------------------------------------------------*
013800050110     c     CREAsiPES     BEGSR
013900050110     C*
014000050110     C/EXEC SQL
014100050110     C+ Declare A1 Cursor for
014200050110     c+ SELECT * FROM wfPES00F
014300050110     c+ WHERE WFPANN = :kwpann AND WFPMES between :PARMMI and :PARMMF
014400050110     c+ ORDER BY WFPANN, WFPMES
014500050110     C/END-EXEC
014600050110      *          apertura cursore
014700050110     C/EXEC SQL
014800050110     C+ OPEN A1
014900050110     C/END-EXEC
015000050110     C                   DOU       SqlCod <> 0
015100050110      *          lettura cursore
015200050110     C/EXEC SQL
015300050110     C+ Fetch Next From A1 Into :wfPESds
015400050110     C/END-EXEC
015500050110     C                   SELECT
015600050110     C                   WHEN      SqlCod = 100
015700050110     c                   leave
015800050110      *
015900050110     C                   WHEN      SqlCod < 0
016000050110     C                   EXSR      SQLError
016100050110     c                   seton                                        H1
016200050110     c                   leave
016300050110      *
016400050110     C                   OTHER
016500050110      * legge e scrive
016600050110     c                   exsr      writeSIPES
016700050110      *
016800050110     C                   ENDSL
016900050110      *
017000050110     C                   ENDDO
017100050110     C/EXEC SQL
017200050110     C+ Close A1
017300050110     C/END-EXEC
017400050110     C*
017500050110     C                   ENDSR
017600040220     C*------------------------------------------------------------------------*
017700050110     C*  Errori durante l'SQL
017800050110     C*------------------------------------------------------------------------*
017900050110     c     SQLError      begsr
018000050110     C*
018100050110     c                   endsr
018200050110      *------------------------------------------------------------------------*
018300050110      * scrive il saldo dal WRkfile
018400050110      *------------------------------------------------------------------------*
018500050110     c     writeSIPES    begsr
018600050110      *
018700050121      * Controlla se il record deve essere saldificato in base ai parametri
018800050121      *  di lancio.
018900050121     c                   exsr      CHKREC
019000050121      *
019100050121     c                   if        WrecOK = 'S'
019200050110     c                   move      WFPANN        PESANN
019300050110     c                   move      WFPMES        PESMES
019400050110     c                   move      WFPKSC        PESKSC
019500050110     c                   move      WFPCTR        PESCTR
019600050110     c                   move      WFPPSC        PESPSC
019700050110     c                   move      WFPNCV        PESNCV
019800050110     c                   move      WFPPSB        PESPSB
019900050110     c                   move      WFPNCL        PESNCL
020000050110     c                   move      WFPNSP        PESNSP
020100050110     c                   move      WFPTCL        PESTCL
020200050110     c                   move      WFPTPB        PESTPB
020300050110     c                   move      WFPTNS        PESTNS
020400050110      *
020500050110     C                   WRITE     siPES000
020600050121     c                   end
020700050110      *
020800050110     c                   endsr
020900050121     C*------------------------------------------------------------------------*
021000050121     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
021100050121     C*------------------------------------------------------------------------*
021200050121     C     CHKREC        BEGSR
021300050121     C*
021400050121     C                   MOVEL     'S'           WRECOK            1
021500050121     C*
021600050121     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
021700050121     C     WFPMES        IFGE      PARMMI
021800050121     C     WFPMES        ANDLE     PARMMF
021900050121     C                   ELSE
022000050121     C                   MOVEL     'N'           WRECOK
022100050121     C                   END
022200160706     C**********
022300050121     C* SE LANCIO NO DATI POSTE, ESLCUDE I CLIENTI POSTE (E VICEVERSA)
022400160706IF  1C**** WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
022500160706     C********           Z-ADD     1             I
022600160706     C**** WFPFIL        LOOKUP    FIL(I)                                 95
022700160706IF  2C**** PARPOS        IFEQ      'N'                                          *NO DATI POSTE
022800160706IF  3C**** PFIL(I)       IFEQ      'PPT'                                        *FILIALE POSTE
022900160706     C********           MOVEL     'N'           WRECOK
023000160706E   3C********           ENDIF
023100160706X   2C********           ELSE                                                   *SI DATI POSTE
023200160706IF  3C**** PFIL(I)       IFNE      'PPT'                                        *NO FILIALE POSTE
023300160706     C********           MOVEL     'N'           WRECOK
023400160706E   3C********           ENDIF
023500160706E   2C********           ENDIF
023600160706E   1C********           ENDIF
023700160706     C********
023800160705     C*
023900160705     C*
024000050121     C* CALCOLA PERCENTUALE COLLI VDL SU COLLI BOLLETTATI
024100050121     C     WFPNCV        IFGT      *ZEROS
024200050121     C                   Z-ADD     WFPTCL        WCPNCL           12 1
024300050121     C                   Z-ADD     WFPNCV        WCPNCV           12 1
024400050121     C                   EXSR      CALPER
024500050121     C                   Z-ADD     WCPPVA        WPER              6 2
024600050121     C                   ELSE
024700160708     C***********        MOVEL     'N'           WRECOK
024800050121     C                   END
024900050121     C*
025000050121     C* CALCOLA PERCENTUALE PESO VDL SU PESO BOLLETTATO
025100050121     C     WFPPSC        IFGT      *ZEROS
025200050121     C                   Z-ADD     WFPPSC        WCPPSC           13 1
025300050121     C                   Z-ADD     WFPPSB        WCPPSB           13 1
025400050121     C                   EXSR      CALRNG
025500050121     C                   Z-ADD     WCPRVA        WRNG              6 2
025600050121     C                   ELSE
025700160708     C***********        MOVEL     'N'           WRECOK
025800050121     C                   END
025900050121     C*
026000050121     C*  Decodifica la filiale x reperire le PERCENTUALI X FILIALE
026100050121     c                   exsr      DecFIL
026200050121     C*
026300160708     C*
026400160708     C*  NON SI VUOLE PIU' TESTARE LE PERCENTUALI
026500160708     C*
026600160708     C*
026700050121     C* SE LA PERCENTUALE (B) E' INFERIORE ALLA MINIMA RICHIESTA,SCARTO
026800160708     C**** WCPPVA        IFLT      PARXXX
026900160708     C**** WCPPVA        OREQ      *ZEROS
027000160706     C**************     MOVEL     'N'           WRECOK
027100160708     C****               END
027200160708     C*******
027300050121     C* SE LA PERCENTUALE (A) NON E' COMPRESA NEL RANGE INDICATO,SCARTO
027400050121     C* PER (A) SI INTENDE LA PERCENTUALE REALE AL NETTO
027500050121     C* DELLO SCARTO MINIMO (PARMIN)
027600160708     C*******            Z-ADD     *zeros        PERC              6 2
027700160708     C*******            EVAL      PERC=%abs(WCPRVA)
027800160708     C*******
027900160708     C**** WCPRVA        IFLT      PARRG1
028000160708     C**** WCPRVA        ORGT      PARRG2
028100050121     C* Al momento tolto il filtro  sulla percentuale di scostamento minimo
028200050121     C**** WCPRVA        OREQ      *ZEROS
028300050121     C**** PERC          ORLT      PARMIN
028400160706     C**************     MOVEL     'N'           WRECOK
028500160708     C****               END
028600050121     C*
028700050121     C                   ENDSR
028800050121     C*------------------------------------------------------------------------*
028900050121     C* CALPER - CALCOLA PERCENTUALE COLLI
029000050121     C* INPUT:   - COLLI BOLLETTATI        (WCPNCL)
029100050121     C*          - COLLI VDL               (WCPNCV)
029200050121     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPPVA)
029300050121     C*------------------------------------------------------------------------*
029400050121     C     CALPER        BEGSR
029500050121     C*
029600050121     C* ===> CALCOLA PERCENTUALE COLLI VDL SU COLLI BOLL.
029700050121     C                   Z-ADD     *ZEROS        WCPPVA            6 2
029800050121     C     WCPNCL        IFNE      WCPNCV                                       --- 1 -->
029900050121     C     WCPNCL        ANDGT     *ZEROS
030000050121     C     WCPNCV        ANDGT     *ZEROS
030100050121     C     WCPNCV        MULT      100           WMULTI           17 3
030200050121     C     WMULTI        DIV       WCPNCL        WRKPER           17 3
030300050121     C     WRKPER        IFGT      9999                                         --- 2 -->
030400050121     C                   Z-ADD     9999,99       WCPPVA
030500050121     C                   ELSE
030600050121     C                   Z-ADD     WRKPER        WCPPVA
030700050121     C                   END                                                    <-- 2 ---
030800050121     C                   END                                                    <-- 1 ---
030900050121     C*
031000050121     C     WCPNCL        IFEQ      WCPNCV                                       --- 1 -->
031100050121     C     WCPNCL        ANDGT     *ZEROS
031200050121     C     WCPNCV        ANDGT     *ZEROS
031300050121     C                   Z-ADD     100           WCPPVA
031400050121     C                   ENDIF
031500050121     C*
031600050121     C     WCPPVA        IFGT      100
031700050121     C                   Z-ADD     100           WCPPVA
031800050121     C                   ENDIF
031900050121     C*
032000050121     C                   ENDSR
032100050121     C*------------------------------------------------------------------------*
032200050121     C* CALRNG - CALCOLA PERCENTUALE RANGE
032300050121     C* INPUT:   - PESO BOLLETTAT0         (WCPPSB)
032400050121     C*          - PESO VDL                (WCPPSC)
032500050121     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPRVA)
032600050121     C*------------------------------------------------------------------------*
032700050121     C     CALRNG        BEGSR
032800050121     C*
032900050121     C* ===> CALCOLA PERCENTUALE PESO VDL SU PESO BOLL.
033000050121     C                   Z-ADD     *ZEROS        WCPRVA            6 2
033100050121     C     WCPPSC        IFNE      WCPPSB                                       --- 1 -->
033200050121     C     WCPPSC        ANDGT     *ZEROS
033300050121     C     WCPPSB        ANDGT     *ZEROS
033400050121     C     WCPPSB        SUB       WCPPSC        WDIFFE           13 1
033500050121     C     WDIFFE        MULT      100           WRKPER           17 3
033600050121     C     WRKPER        DIV       WCPPSB        WRKPER
033700050121     C     WRKPER        IFGT      9999                                         --- 2 -->
033800050121     C                   Z-ADD     9999,99       WCPRVA
033900050121     C                   ELSE
034000050121     C                   Z-ADD     WRKPER        WCPRVA
034100050121     C                   END                                                    <-- 2 ---
034200050121     C                   END                                                    <-- 1 ---
034300050121     C*
034400050121     C     WCPPSC        IFEQ      WCPPSB                                       --- 1 -->
034500050121     C     WCPPSC        ANDGT     *ZEROS
034600050121     C     WCPPSB        ANDGT     *ZEROS
034700050121     C                   Z-ADD     100           WCPPVA
034800050121     C                   ENDIF
034900050121     C*
035000050121     C     WCPPVA        IFGT      100
035100050121     C                   Z-ADD     100           WCPPVA
035200050121     C                   ENDIF
035300050121     C*
035400050121     C                   ENDSR
035500050121     C*------------------------------------------------------------------------*
035600050121     C* DecFIL - OPERAZIONI PER ogni FILIALE
035700050121     C*------------------------------------------------------------------------*
035800050121     C     DecFIL        BEGSR
035900050121     C*
036000050121     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
036100050121     C                   Z-ADD     WFPFIL        DEPFIL
036200050121     C*
036300050121     C* REPERISCE IL TERMINAL PARTENZA
036400050121     C                   CLEAR                   FNLV55DS
036500050121     C                   MOVEL     'P'           D55TPT
036600050121     C                   MOVEL     DEPFIL        D55LIN
036700050121     C                   Z-ADD     DATCOR        D55DRF
036800050121     C                   CALL      'FNLV55R'
036900050121     C                   PARM                    FNLV55DS
037000050121IF  1C     D55ERR        IFNE      *BLANKS
037100050121     C                   MOVEL     DEPFIL        D55TFP
037200050121E   1C                   ENDIF
037300050121     C                   Z-ADD     D55TFP        DEPTFP
037400050121     C*
037500050121     C* REPERISCO LE PERCENTUALI X LA FILIALE CORRENTE
037600160706     C     DEPTFP        CHAIN     AZORG01L                           88
037700050121     C     *IN88         IFEQ      *OFF
037800050121     C                   MOVEL     ORGDF0        OG150
037900050121     C     §OGPES        IFNE      *BLANKS
038000080310     C     §ogPES        andNE     *all'0'
038100050121     C                   Z-ADD     PARPEI        PARXXX            5 2
038200050121     C                   ELSE
038300050121     C                   Z-ADD     PARPES        PARXXX            5 2
038400050121     C                   ENDIF
038500050121     C                   ELSE
038600050121     C                   Z-ADD     PARPES        PARXXX            5 2
038700050121     C                   ENDIF
038800050121     C*
038900050121     C                   ENDSR
039000050121     C*------------------------------------------------------------------------*
039100050121     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
039200050121     C*------------------------------------------------------------------------*
039300160708     C**** CARORG        BEGSR
039400160708     C****
039500160708     C****  POSIZIONAMENTO SUL FILE E PRIMA LETTURA
039600160708     C****               Z-ADD     *ZEROS        KORFIL
039700160708     C**** KEYORG        SETLL     AZORG01L                           98        *FINE-ARCHIV
039800160708     C* N98              READ      AZORG01L                               98
039900160708     C****               Z-ADD     0             I                 3 0          *INDICE SCHIERA
040000160708     C**** *IN98         DOWEQ     '0'                                          --- 1 -->
040100160708     C**** ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
040200160708     C**** ORGFAG        IFNE      'V'
040300160708     C****               ADD       1             I
040400160708     C****               Z-ADD     ORGFIL        FIL(I)
040500160708     C****               MOVEL     ORGDES        DFIL(I)
040600160708     C****               MOVEL     ORGDE3        OG143
040700160708     C****               MOVEL     §OGNTW        PFIL(I)
040800160708     C****               END
040900160708     C****               END                                                    <-- 2 ---
041000160708     C****               READ      AZORG01L                               98
041100160708     C****               END                                                    <-- 1 ---
041200160708     C****
041300160708     C****               ENDSR
041400050121     O*------------------------------------------------------------------------*

000100050110     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300050110     H* STATISTICA VOLUMI VDL - GENERA WRK FILE
000400000000     H*------------------------------------------------------------------------*
000500050124     FTNTAM01L  IF   E           K DISK
000600050124     FAZORG01L  IF   E           K DISK
000700050110     FWFvlm00f  iF   E             DISK
000800050110     FsiVLM02L  uF A E           K DISK
000900000000     D*------------------------------------------------------------------------*
001000000000     D KPJBA         E DS
001100000000     D*-------------------
001200050110     D wfVLMds       E DS                  EXTNAME(wfVLM00f)
001300050124     D*-------------------
001400050124     D* LETTURA CAMPO ORGDE£ DI AZORG
001500050124     D*-------------------
001600050124     D OG143         E DS
001700050124     D*-------------------
001800050124     D* REPERIMENTO CLIENTI CON/SENZA IMPIANTO
001900050124     D*-------------------
002000050124     D OG150         E DS
002100050124     D*------------------
002200050124     D* DS REPEIMENTO TERMINALs
002300050124     D*------------------
002400050124     D FNLV55DS      E DS
002500050124     D*------------------
002600050124     D* DS RIDEFINIZIONE FLAG OPERATIVI TARIFFE
002700050124     D*------------------
002800050124     D DSTA01        E DS
002900050124     D*------------------
003000050124     D* DS CONTROLLO DATA (8)
003100050124     D*------------------
003200050124     D WLBDA8          DS                  INZ
003300050124     D  G08DAT                 1      8  0
003400050124     D  G08INV                 9     16  0
003500050124     D  G08ERR                17     17
003600050124     D  G08TGI                18     22  0
003700050124     D*-------------------
003800050124     D* SCHIERA FILIALI
003900050124     D*-------------------
004000050124     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
004100050124     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
004200050124     D PFIL            S              3    DIM(500)                             *NETWORK POSTE
004300050110     D*-------------------
004400000000     D* DS PARAMETRI DI LANCIO PGM
004500000000     D*-------------------
004600050110     D PARAM           DS
004700050110     D  PARTLA                 1      1
004800050110     D  PARANN                 2      5  0
004900050110     D  PARMMI                 6      7  0
005000050110     D  PARMMF                 8      9  0
005100050110     D  PARSNR                10     10
005200050110     D  PARPID                27     31  2
005300050110     D  PARDIV                32     32
005400050110     D  PARARE                33     35  0
005500050110     D  PARFIL                36     38  0
005600050110     D  PARDET                39     39
005700050110     D  PARTOT                40     40
005800050110     D  PARRG1                41     44  1
005900050110     D  PARRG2                45     48  1
006000050110     D  PARPIA                49     53  2
006100050110     D  PARPSD                54     58  2
006200050110     D  PARPSA                59     63  2
006300050110     D  PAROK                 65     66
006400050110     D  PARMIN                67     71  3
006500050110     D  PARPOS                72     72
006600050110     D  PARNT1                73     75
006700050110     D  PARNT2                76     78
006800050110     D  PARNT3                79     81
006900050110     D  PARNT4                82     84
007000050110     D  PARSAL                85     85
007100000000     C*------------------------------------------------------------------------*
007200000000     C* MAIN LINE
007300000000     C*------------------------------------------------------------------------*
007400050110     C     *ENTRY        PLIST
007500050110     C                   PARM                    KPJBA
007600050110     C                   MOVEL     KPJBU         PARAM
007700050124     C*--------------------
007800050124     C* REPERISCE DATA/ORA CORRENTE
007900050124     C*--------------------
008000050124     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
008100050124     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
008200050124     C                   Z-ADD     *ZEROS        G08INV
008300050124     C                   MOVEL     '0'           G08ERR
008400050124     C                   CALL      'XSRDA8'
008500050124     C                   PARM                    WLBDA8
008600050124     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
008700050124     C*
008800050124     C* CARICA ORGANIGRAMMA
008900050124     C                   EXSR      CARORG
009000050124     C*
009100050110     C*--------------------
009200050110     C* CAMPI RIFERITI AL DATA BASE
009300050110     C*--------------------
009400050110     C     *LIKE         DEFINE    WFvANN        KWvANN
009500050110     C     *LIKE         DEFINE    WFvMES        KWvMES
009600050124     C     *LIKE         DEFINE    WFVFIL        DEPFIL
009700050124     C     *LIKE         DEFINE    WFVFIL        DEPTFP
009800050124     C     *LIKE         DEFINE    ORGFIL        KORFIL
009900050124     C     *LIKE         DEFINE    TAMKSC        KTFKSC
010000050124     C     *LIKE         DEFINE    TAMCTR        KTFCTR
010100050124     C     *LIKE         DEFINE    TAMPRG        KTFPRG
010200050124     C*--------------------
010300050124     C* CHIAVI DI LETTURA
010400050124     C*--------------------
010500050124     C* CHIAVE LETTURA TNTAM31C - COMPLETA
010600050124     C     KEYTAM        KLIST
010700050124     C                   KFLD                    KTFKSC
010800050124     C                   KFLD                    KTFCTR
010900050124     C                   KFLD                    KTFPRG
011000050124     C*
011100050124     C* CHIAVE LETTURA AZORG01L - COMPLETA
011200050124     C     KEYORG        KLIST
011300050124     C                   KFLD                    KORFIL
011400050124     C*
011500050110     C* CHIAVE LETTURA WFVLM01L - COMPLETA
011600050110     C     KeysiVlm      KLIST
011700050110     C                   KFLD                    KWvANN
011800050110     C                   KFLD                    KWvMES
011900050110     C*--------------------
012000050110     C* CAMPI DI CONTROLLO
012100050110     C*--------------------
012200050110     C                   Z-ADD     1             CODUT             1 0
012300050110     C***
012400050110     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA SIVLM00F
012500050110     C                   EXSR      CANsiVLM
012600050110     C* LANCIO PER SALDI
012700050110     C                   EXSR      CREAsiVLM
012800000000     C*
012900000000     C                   SETON                                        LR
013000050110     C*------------------------------------------------------------------------*
013100050110     C* CANsiVLM  - CANCELLA IL SIVLM00F TUTTI I RECORD DEI MESI RICHIESTI
013200050110     C*------------------------------------------------------------------------*
013300050110     C     CANsiVLM      BEGSR
013400050110     C*
013500050110     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
013600050110     C                   Z-ADD     PARANN        KWvANN
013700050110     C                   Z-ADD     parMMi        KWvMES
013800050110     C     KeysiVLM      chain     siVLM02L                           99
013900050110     C*
014000050110     C* CICLO FINO A FINE FILE OPPURE A MESE MAGGIORE MESE FINE
014100050110     C     *IN99         DOWEQ     *Off                                         --- 1 -->
014200050110     C     vlmMES        ANDle     PARMMf
014300050110     C* ELIMINA RECORD SE ALL'INTERNO DEL RANGE DEI MESI RICHIESTI
014400050110     C                   DELETE    siVOL000
014500050110     C* LEGGE IL RECORD SUCCESSIVO
014600050110     C     KwvANN        reade     siVLM02L                               99
014700050110     C                   ENDdo                                                  <-- 1 ---
014800050110     C*
014900050110     C                   ENDSR
015000050110     C*------------------------------------------------------------------------*
015100050110     C* CREAsiVLM  - Crea i records dei Saldi nel range dei mesi richiesti
015200050110     C*------------------------------------------------------------------------*
015300050110     c     CREAsiVLM     BEGSR
015400050110     C*
015500050110     C/EXEC SQL
015600050110     C+ Declare A1 Cursor for
015700050110     c+ SELECT * FROM wfVLM00F
015800050110     c+ WHERE WFvANN = :kwvann AND WFvMES between :PARMMI and :PARMMF
015900050110     c+ ORDER BY WFvANN, WFvMES
016000050110     C/END-EXEC
016100050110      *          apertura cursore
016200050110     C/EXEC SQL
016300050110     C+ OPEN A1
016400050110     C/END-EXEC
016500050110     C                   DOU       SqlCod <> 0
016600050110      *          lettura cursore
016700050110     C/EXEC SQL
016800050110     C+ Fetch Next From A1 Into :wfVLMds
016900050110     C/END-EXEC
017000050110     C                   SELECT
017100050110     C                   WHEN      SqlCod = 100
017200050110     c                   leave
017300050110      *
017400050110     C                   WHEN      SqlCod < 0
017500050110     C                   EXSR      SQLError
017600050110     c                   seton                                        H1
017700050110     c                   leave
017800050110      *
017900050110     C                   OTHER
018000050110      * legge e scrive
018100050110     c                   exsr      WRITEsiVLM
018200050110      *
018300050110     C                   ENDSL
018400050110      *
018500050110     C                   ENDDO
018600050110     C/EXEC SQL
018700050110     C+ Close A1
018800050110     C/END-EXEC
018900050110     C*
019000050110     C                   ENDSR
019100040220     C*------------------------------------------------------------------------*
019200050110     C*  Errori durante l'SQL
019300050110     C*------------------------------------------------------------------------*
019400050110     c     SQLError      begsr
019500050110     C*
019600050110     c                   endsr
019700050110      *------------------------------------------------------------------------*
019800050110      * scrive il saldo dal WRkfile
019900050110      *------------------------------------------------------------------------*
020000050110     c     WRITEsiVLM    begsr
020100050110      *
020200050124      * Controlla se il record deve essere saldificato in base ai parametri
020300050124      *  di lancio.
020400050124     c                   exsr      CHKREC
020500050124      *
020600050124     c                   if        WrecOK = 'S'
020700050110     c                   move      WFvANN        VLMANN
020800050110     c                   move      WFvMES        VLMMES
020900050110     c                   move      WFvKSC        VLMKSC
021000050110     c                   move      WFvCTR        VLMCTR
021100050110     c                   move      WFvFVB        VLMFVB
021200050110     c                   move      WFvVLB        VLMVLB
021300050110     c                   move      WFvVLC        VLMVLC
021400050110     c                   move      WFvNCL        VLMNCL
021500050110     c                   move      WFVPKB        VLMPKB
021600050110     c                   move      WFVNSP        VLMNSP
021700050110     c                   move      WFVNCR        VLMNCR
021800050110     c                   move      WFVTCL        VLMTCL
021900050110      *
022000050110     C                   WRITE     siVOL000
022100050124     c                   end
022200050110      *
022300050110     c                   endsr
022400050124     C*------------------------------------------------------------------------*
022500050124     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
022600050124     C*------------------------------------------------------------------------*
022700050124     C     CHKREC        BEGSR
022800050124     C*
022900050124     C                   MOVEL     'S'           WRECOK            1
023000050124     C*
023100050124     C* SE NON RIENTRA NEI MESI RICHIESTI ESCLUDO IL RECORD
023200050124     C     WFVMES        IFGE      PARMMI
023300050124     C     WFVMES        ANDLE     PARMMF
023400050124     C                   ELSE
023500050124     C                   MOVEL     'N'           WRECOK
023600050124     C                   END
023700050124     C*
023800050124     C* SE LANCIO NO DATI POSTE, ESLCUDE I CLIENTI POSTE (E VICEVERSA)
023900050124IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
024000050124     C                   Z-ADD     1             I
024100050124     C     WFVFIL        LOOKUP    FIL(I)                                 95
024200050124IF  2C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
024300050124IF  3C     PFIL(I)       IFEQ      'PPT'                                        *FILIALE POSTE
024400050124     C                   MOVEL     'N'           WRECOK
024500050124E   3C                   ENDIF
024600050124X   2C                   ELSE                                                   *SI DATI POSTE
024700050124IF  3C     PFIL(I)       IFNE      'PPT'                                        *NO FILIALE POSTE
024800050124     C                   MOVEL     'N'           WRECOK
024900050124E   3C                   ENDIF
025000050124E   2C                   ENDIF
025100050124E   1C                   ENDIF
025200050124     C*
025300050124     C* CALCOLA RAPPORTO COLLI CML SU COLLI BOLL.
025400050124     C**** WFVTCL        IFGT      *ZEROS
025500050124     C                   Z-ADD     WFVTCL        WCPNCL           15 3
025600050124     C                   Z-ADD     WFVNCR        WCPNCR           15 3
025700050124     C                   EXSR      CALPER
025800050124     C                   Z-ADD     WCPPVA        WPER              6 2
025900050124     C*****              ELSE
026000050124     C*****              MOVEL     'N'           WRECOK
026100050124     C*****              END
026200050124     C*
026300050124     C* CALCOLA PERCENTUALE VOLUME CML SU VOL.AUTOM.
026400050124     C*****WFVVLC        IFGT      *ZEROS
026500050124     C                   Z-ADD     WFVVLB        WCPVLB           15 3
026600050124     C                   Z-ADD     WFVVLC        WCPVLC           15 3
026700050124     C                   EXSR      CALRNG
026800050124     C                   Z-ADD     WCPRVA        WRNG              6 2
026900050124     C*****              ELSE
027000050124     C*****              MOVEL     'N'           WRECOK
027100050124     C*****              END
027200050124     C*
027300050124     C*  Decodifica la filiale x reperire le PERCENTUALI X FILIALE
027400050124     c                   exsr      DecFIL
027500050124     C*
027600050124     C* SE IL RAPPORTO COLLI VDL CON/SENZA IMPIANTO NO IN RANGE RICHIESTO, SCARTO
027700050124     c* CONSIDERAZIONI SOLO PER RIGA DETTAGLIO
027800050124     C     WCPPVA        IFLT      PARXDA
027900050124     C     WCPPVA        OREQ      *ZEROS
028000050124     C                   MOVEL     'N'           WRECOK
028100050124     C                   END
028200050124     C*
028300050124     C* SE LA PERCENTUALE (A) SCOST. VOL NON COMPRESA IN RANGE RICHIESTO,SCARTO
028400050124     C* PER (A) SI INTENDE LA PERCENTUALE REALE AL NETTO
028500050124     C* DELLO SCARTO MINIMO (PARMIN)
028600050124     C                   Z-ADD     *zeros        PERC              6 2
028700050124     C                   EVAL      PERC=%abs(WCPRVA)
028800050124     C     WCPRVA        IFLT      PARRG1
028900050124     C     WCPRVA        ORGT      PARRG2
029000050124     C* Al momento tolto il filtro  sulla percentuale di scostamento minimo
029100050124     C**** WCPRVA        OREQ      *ZEROS
029200050124     C**** PERC          ORLT      PARMIN
029300050124     C                   MOVEL     'N'           WRECOK
029400050124     C                   END
029500050124     C*
029600050124     C* CONTROLLO SE RICHIESTE SELEZIONI X NETWORK TARIFFE
029700050124     C                   IF        WRECOK <> 'N'
029800050124     C                   IF        PARNT1 <> *blanks OR
029900050124     C                             PARNT2 <> *blanks OR
030000050124     C                             PARNT3 <> *blanks OR
030100050124     C                             PARNT4 <> *blanks
030200050124     C* SE RICHIESTE SELEZIONI VERIFICO
030300050124     C                   MOVEL     *blanks       wNTWTAR           3
030400050124     C* NEGO I FLAG X POI EVENTUALMENTE ATTIVARLO SE RISPONDENTE AI PARAMETRI RICHIESTI
030500050124     C                   MOVEL     'N'           WRECOK
030600050124     C* REPERISCO IL NETWORK DELLA TARIFFA DEL RECORD CORRENTE SE NN TRATTASI DI TARIFFE 888 O 999
030700050124     C     WFVCTR        IFNE      888
030800050124     C     WFVCTR        ANDNE     999
030900050124     C                   Z-ADD     WFVKSC        KTFKSC
031000050124     C                   Z-ADD     WFVCTR        KTFCTR
031100050124     C                   Z-ADD     *ZEROS        KTFPRG
031200050124     C     KEYTAM        SETLL     TNTAM01L                           99
031300050124     C  N99              READ      TNTAM01L                               99
031400050124     C     *IN99         DOWEQ     '0'
031500050124     C     TAMKSC        ANDEQ     KTFKSC
031600050124     C     TAMCTR        ANDEQ     KTFCTR
031700050124     C     TAMATB        IFEQ      ' '
031800050124     C                   MOVEL(P)  TAMFLO        DSTA01
031900050124     C                   IF        TAMFIE  = 'I' AND §TADPD <> 'S'
032000050124     C                   EVAL      wNTWTAR = 'ITA'
032100050124     C                   ENDIF
032200050124     C                   IF        TAMFIE  = 'I' AND §TADPD  = 'S'
032300050124     C                   EVAL      wNTWTAR = 'DPD'
032400050124     C                   ENDIF
032500050124     C                   IF        TAMFIE  = 'E' AND §TAFED <> 'S'
032600050124     C                   EVAL      wNTWTAR = 'EEX'
032700050124     C                   ENDIF
032800050124     C                   IF        TAMFIE  = 'E' AND §TAFED  = 'S'
032900050124     C                   EVAL      wNTWTAR = 'FED'
033000050124     C                   ENDIF
033100050124     C                   ENDIF
033200050124     C                   READ      TNTAM01L                               99
033300050124     C                   ENDDO
033400050124     C                   ENDIF
033500050124     C* EFFETTUO CONSIDERAZIONI
033600050124     C                   IF        PARNT1 = 'ITA' OR
033700050124     C                             PARNT2 = 'ITA' OR
033800050124     C                             PARNT3 = 'ITA' OR
033900050124     C                             PARNT4 = 'ITA'
034000050124     C                   IF        wNTWTAR = 'ITA' OR
034100050124     C                             WFVCTR = 888    OR
034200050124     C                             WFVCTR = 999
034300050124     C                   MOVEL     'S'           WRECOK
034400050124     C                   ENDIF
034500050124     C                   ENDIF
034600050124     C                   IF        PARNT1 = 'EEX' OR
034700050124     C                             PARNT2 = 'EEX' OR
034800050124     C                             PARNT3 = 'EEX' OR
034900050124     C                             PARNT4 = 'EEX'
035000050124     C                   IF        wNTWTAR = 'EEX'
035100050124     C                   MOVEL     'S'           WRECOK
035200050124     C                   ENDIF
035300050124     C                   ENDIF
035400050124     C                   IF        PARNT1 = 'DPD' OR
035500050124     C                             PARNT2 = 'DPD' OR
035600050124     C                             PARNT3 = 'DPD' OR
035700050124     C                             PARNT4 = 'DPD'
035800050124     C                   IF        wNTWTAR = 'DPD'
035900050124     C                   MOVEL     'S'           WRECOK
036000050124     C                   ENDIF
036100050124     C                   ENDIF
036200050124     C                   IF        PARNT1 = 'FED' OR
036300050124     C                             PARNT2 = 'FED' OR
036400050124     C                             PARNT3 = 'FED' OR
036500050124     C                             PARNT4 = 'FED'
036600050124     C                   IF        wNTWTAR = 'FED'
036700050124     C                   MOVEL     'S'           WRECOK
036800050124     C                   ENDIF
036900050124     C                   ENDIF
037000050124     C*
037100050124     C                   ENDIF
037200050124     C                   ENDIF
037300050124     C*
037400050124     C                   ENDSR
037500050124     C*------------------------------------------------------------------------*
037600050124     C* CALPER - CALCOLA PERCENTUALE RAPPORTO COLLI
037700050124     C* INPUT:   - COLLI BOLLETTATI        (WCPNCL)
037800050124     C*          - COLLI CML               (WCPNCR)
037900050124     C* OUTPUT:  - PERCENTUALE DIFFERENZA  (WCPPVA)
038000050124     C*------------------------------------------------------------------------*
038100050124     C     CALPER        BEGSR
038200050124     C*
038300050124     C* ===> CALCOLA PERCENTUALE COLLI CML SU COLLI BOLL.
038400050124     C                   Z-ADD     *ZEROS        WCPPVA            6 2
038500050124     C     WCPNCL        IFNE      WCPNCR                                       --- 1 -->
038600050124     C     WCPNCL        ANDGT     *ZEROS
038700050124     C     WCPNCR        ANDGT     *ZEROS
038800050124     C     WCPNCR        MULT      100           WMULTI           18 3
038900050124     C     WMULTI        DIV       WCPNCL        WRKPER           20 3
039000050124     C     WRKPER        IFGT      9999                                         --- 2 -->
039100050124     C                   Z-ADD     9999,99       WCPPVA
039200050124     C                   ELSE
039300050124     C                   Z-ADD     WRKPER        WCPPVA
039400050124     C                   END                                                    <-- 2 ---
039500050124     C                   END                                                    <-- 1 ---
039600050124     C*
039700050124     C     WCPNCL        IFEQ      WCPNCR                                       --- 1 -->
039800050124     C     WCPNCL        ANDGT     *ZEROS
039900050124     C     WCPNCR        ANDGT     *ZEROS
040000050124     C                   Z-ADD     100           WCPPVA
040100050124     C                   ENDIF
040200050124     C*
040300050124     C                   ENDSR
040400050124     C*------------------------------------------------------------------------*
040500050124     C* CALRNG - CALCOLA PERCENTUALE SCOSTAMENTO VOLUMI
040600050124     C* INPUT:   - VOLUME BOLLETTAT0         (WCPVLB)
040700050124     C*          - VOLUME VDL                (WCPVLC)
040800050124     C* OUTPUT:  - PERCENTUALE DIFFERENZA    (WCPRVA)
040900050124     C*------------------------------------------------------------------------*
041000050124     C     CALRNG        BEGSR
041100050124     C*
041200050124     C* ===> CALCOLA PERCENTUALE VOL CML SU VOL BOLL.
041300050124     C                   Z-ADD     *ZEROS        WCPRVA            6 2
041400050124     C     WCPVLC        IFNE      WCPVLB                                       --- 1 -->
041500050124     C     WCPVLC        ANDGT     *ZEROS
041600050124     C     WCPVLB        ANDGT     *ZEROS
041700050124     C     WCPVLB        SUB       WCPVLC        WDIFFE           18 3
041800050124     C     WDIFFE        MULT      100           WRKPER           20 3
041900050124     C     WRKPER        DIV       WCPVLB        WRKPER
042000050124     C     WRKPER        IFGT      9999                                         --- 2 -->
042100050124     C                   Z-ADD     9999,99       WCPRVA
042200050124     C                   ELSE
042300050124     C                   Z-ADD     WRKPER        WCPRVA
042400050124     C                   END                                                    <-- 2 ---
042500050124     C                   END                                                    <-- 1 ---
042600050124 OLD C*
042700050124  "  C* SPECIFICA LOGICAMENTE ERRATA MA INSERITA X MANTENERE UNIFORMI
042800050124  "  C* I CRITERI DI VALUTAZIONE UTENTE RISPETTO ALLA VECCHIA
042900050124  "  C* STATISTICA VOLUMI
043000050124  "  C*                    MULT -1        WCPRVA
043100050124     C*
043200050124     C     WCPVLC        IFEQ      WCPVLB                                       --- 1 -->
043300050124     C     WCPVLC        ANDGT     *ZEROS
043400050124     C     WCPVLB        ANDGT     *ZEROS
043500050124     C                   Z-ADD     100           WCPRVA
043600050124     C                   ENDIF
043700050124     C*
043800050124     C                   ENDSR
043900050124     C*------------------------------------------------------------------------*
044000050124     C* DecFIL - OPERAZIONI PER ogni FILIALE
044100050124     C*------------------------------------------------------------------------*
044200050124     C     DecFIL        BEGSR
044300050124     C*
044400050124     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
044500050124     C                   Z-ADD     WFVFIL        DEPFIL
044600050124     C*
044700050124     C* REPERISCE IL TERMINAL PARTENZA
044800050124     C                   CLEAR                   FNLV55DS
044900050124     C                   MOVEL     'P'           D55TPT
045000050124     C                   MOVEL     DEPFIL        D55LIN
045100050124     C                   Z-ADD     DATCOR        D55DRF
045200050124     C                   CALL      'FNLV55R'
045300050124     C                   PARM                    FNLV55DS
045400050124IF  1C     D55ERR        IFNE      *BLANKS
045500050124     C                   MOVEL     DEPFIL        D55TFP
045600050124E   1C                   ENDIF
045700050124     C                   Z-ADD     D55TFP        DEPTFP
045800050124     C*
045900050124     C* REPERISCO LE PERCENTUALI X LA FILIALE CORRENTE
046000050124     C     DEPTFP        CHAIN     AZORG01L                           88
046100050124     C     *IN88         IFEQ      *OFF
046200050124     C                   MOVEL     ORGDF0        OG150
046300050124     C     §OGVOL        IFNE      *BLANKS
046301080310     C     §ogVOL        andNE     *all'0'
046400050124     C                   Z-ADD     PARPID        PARXDA            5 2
046500050124     C                   ELSE
046600050124     C                   Z-ADD     PARPSD        PARXDA            5 2
046700050124     C                   ENDIF
046800050124     C                   ELSE
046900050124     C                   Z-ADD     PARPSD        PARXDA            5 2
047000050124     C                   ENDIF
047100050124     C*
047200050124     C                   ENDSR
047300050124     C*------------------------------------------------------------------------*
047400050124     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
047500050124     C*------------------------------------------------------------------------*
047600050124     C     CARORG        BEGSR
047700050124     C*
047800050124     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
047900050124     C                   Z-ADD     *ZEROS        KORFIL
048000050124     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
048100050124     C  N99              READ      AZORG01L                               99
048200050124     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
048300050124     C     *IN99         DOWEQ     '0'                                          --- 1 -->
048400050124     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
048500050124     C     ORGFAG        IFNE      'V'
048600050124     C                   ADD       1             I
048700050124     C                   Z-ADD     ORGFIL        FIL(I)
048800050124     C                   MOVEL     ORGDES        DFIL(I)
048900050124     C                   MOVEL     ORGDE3        OG143
049000050124     C                   MOVEL     §OGNTW        PFIL(I)
049100050124     C                   END
049200050124     C                   END                                                    <-- 2 ---
049300050124     C                   READ      AZORG01L                               99
049400050124     C                   END                                                    <-- 1 ---
049500050124     C*
049600050124     C                   ENDSR
049700050124     O*------------------------------------------------------------------------*

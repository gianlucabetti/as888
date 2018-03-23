000100000000     H*------------------------------------------------------------------------*
000200000000     H* FATTURAZIONE INFRAGRUPPO MCUBI - RICHIESTA PARAMETRI                   *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DATEDIT(*YMD)
000500000000     F*------------------------------------------------------------------------*
000600170113     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800161216     Ftabel00f  iF   E           K DISK
000900161216     FTREC29D   CF   E             WORKSTN
001000000000     D*-------------------
001100000000     D* MESSAGGI VIDEO
001200000000     D*-------------------
001300000000     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001400000000     D*------------------------------------------------------------------------*
001500000000     D* INPUT
001600000000     D*------------------------------------------------------------------------*
001700170116     d AZUTEds       e ds                  extname(AZUTE00F)
001800170116     d DDatiUte      e ds
001900170116      *--------------------------
002000170116     d* Parametri x Controllo profilo utenti
002100170116     d TIBS34ds      e ds
002200000000     D*------------------
002300000000     D* DS PARAMETRI DI LANCIO
002400000000     D*------------------
002500170117     D PARAM         E DS                  extname(TREC28DS)
002600000000     D*-------------------
002700000000     D* ARCHITETTURA
002800000000     D*-------------------
002900000000     D KPJBA         E DS
003000000000     C*------------------------------------------------------------------------*
003100000000     C* MAIN LINE
003200000000     C*------------------------------------------------------------------------*
003300000000     C* IMPOSTAZIONI PRIMA VIDEATA
003400000000     C                   EXSR      IMPVD1
003500000000     C*
003600000000     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
003700000000DO  1C     $FINE         DOUEQ     'S'
003800000000     C     $TIVID        CASEQ     '1'           GESVD1
003900000000     C                   ENDCS
004000000000E   1C                   ENDDO
004100000000     C*
004200000000     C                   SETON                                        LR
004300000000     C*------------------------------------------------------------------------*
004400000000     C* IMPVD1 - IMPOSTAZIONI PRIMA VIDEATA
004500000000     C*------------------------------------------------------------------------*
004600000000     C     IMPVD1        BEGSR
004700000000     C*
004800000000     C* REIMPOSTA FORMATO VIDEO E INDICATORI
004900000000     C                   EXSR      SETVD1
005000000000     C                   MOVEL     '1'           $TIVID
005100170117     C                   MOVEL     'N'           v1cstp
005200000000     C*
005300000000     C                   ENDSR
005400000000     C*------------------------------------------------------------------------*
005500000000     C* SETVD1 - IMPOSTAZIONI INDICATORI PRIMA VIDEATA
005600000000     C*------------------------------------------------------------------------*
005700000000     C     SETVD1        BEGSR
005800000000     C*
005900990324     C                   SETOFF                                       202122
006000170117     C                   SETOFF                                       902423
006100000000     C                   MOVEL     *BLANKS       $MSG
006200000000     C*
006300000000     C                   ENDSR
006400000000     C*------------------------------------------------------------------------*
006500000000     C* GESVD1 - GESTIONE PRIMA VIDEATA
006600000000     C*------------------------------------------------------------------------*
006700000000     C     GESVD1        BEGSR
006800000000     C*
006900000000     C* EMISSIONE VIDEATA
007000170227     C                   write     EC29D00
007100161216     C                   EXFMT     EC29D01
007200000000     C                   EXSR      SETVD1
007300000000     C* F03-FINE
007400000000IF  1C     *INKC         IFEQ      *ON
007500000000     C                   EXSR      F03GES
007600000000     C                   GOTO      FINVD1
007700000000E   1C                   ENDIF
007800000000     C* F12-RITORNO
007900000000IF  1C     *INKL         IFEQ      *ON
008000000000     C                   EXSR      F12GES
008100000000     C                   GOTO      FINVD1
008200000000E   1C                   ENDIF
008300000000     C* ENTER-CONTROLLA VIDEO
008400000000     C                   EXSR      CTRVD1
008500000000IF  1C     *IN90         IFEQ      *ON
008600000000     C                   GOTO      FINVD1
008700000000E   1C                   ENDIF
008800000000     C* F06-CONFERMA
008900000000IF  1C     *INKF         IFEQ      *ON
009000000000     C                   EXSR      F06GES
009100000000     C                   GOTO      FINVD1
009200000000E   1C                   ENDIF
009300000000     C*
009400000000     C     FINVD1        ENDSR
009500000000     C*------------------------------------------------------------------------*
009600000000     C* CTRVD1 - CONTROLLA PRIMA VIDEATA
009700000000     C*------------------------------------------------------------------------*
009800000000     C     CTRVD1        BEGSR
009900000000     C*
010000000000     C* REIMPOSTA LE VARIABILI DI LAVORO
010100000000     C                   MOVEL     '0'           WERR
010200990407     C*---
010300990407     C* ANNO
010400990407     C*---
010500990407IF  1C     WERR          IFEQ      '0'
010600990407IF  2C     V1CANN        IFLE      *ZEROS
010700990407     C                   SETON                                        2090
010800990407     C                   MOVEL     MSG(1)        $MSG
010900990407     C                   MOVEL     '1'           WERR
011000990407     C                   GOTO      FINCT1
011100990407E   2C                   ENDIF
011200990407E   1C                   ENDIF
011300990415     C*---
011400990415     C* MESE
011500990415     C*---
011600990415IF  1C     WERR          IFEQ      '0'
011700990420IF  2C     V1CMES        IFGE      1
011800990415     C     V1CMES        ANDLE     12
011900990415X   2C                   ELSE
012000990415     C                   SETON                                        2190
012100990415     C                   MOVEL     MSG(2)        $MSG
012200990415     C                   MOVEL     '1'           WERR
012300990415     C                   GOTO      FINCT1
012400990415E   2C                   ENDIF
012500990415E   1C                   ENDIF
012600990324     C*---
012700990324     C* SIMULAZIONE
012800990324     C*---
012900990324IF  1C     WERR          IFEQ      '0'
013000990324IF  2C     V1CSIM        IFNE      'S'
013100990324     C     V1CSIM        ANDNE     'N'
013200990324     C                   SETON                                        2290
013300990324     C                   MOVEL     MSG(3)        $MSG
013400990324     C                   MOVEL     '1'           WERR
013500990324     C                   GOTO      FINCT1
013600990324E   2C                   ENDIF
013700990324E   1C                   ENDIF
013800161221      **
013900161221      *   se al passaggio dell'anno
014000161221     c                   move      v1cmes        mese              2
014100161221     c                   move      v1cann        anno              4
014200161221     c                   if        v1cmes - 1 = 0
014300161221     c                   move      '12'          mese
014400161221     c     v1cann        sub       1             anno1             4 0
014500161221     c                   move      anno1         anno
014600161221     c                   else
014700161221     c     v1cmes        sub       1             mese1             2 0
014800161221     c                   move      mese1         mese
014900161221     c                   end
015000161221     C*
015100170113     C* Solo se NON sono in Simulazione esegue il controllo sul mese
015200170113     C*  precedente.
015300170113IF  2C     V1CSIM        IFEQ      'N'
015400170113IF  2C     V1CANN        andGT     2016
015500170113     C*
015600161221     C*** controllo in tabella se stampato il mese precedente x tipo
015700161221     c                   if        v1ctip = 'T' or
015800161221     c                             v1ctip = *blank
015900161221     C                   Z-ADD     1             TBLKUT
016000161221     C                   MOVEL     '$5'          TBLCOD
016100161221     C                   eval      tblkey = 'T' + anno + mese
016200161221     C     KTAB          CHAIN     TABEL00F
016300161221     C                   IF        not %Found(Tabel00F)
016400161221     C                   SETON                                        2390
016500161221     C                   MOVEL     MSG(4)        $MSG
016600161221     c                   eval      $msg = %trim($msg) + ' TRAINI'
016700161221     C                   MOVEL     '1'           WERR
016800161221     C                   GOTO      FINCT1
016900161221     C                   ENDIF
017000161221     C                   endIF
017100161221     C*
017200161221     c                   if        v1ctip = 'P' or
017300161221     c                             v1ctip = *blank
017400161221     C                   Z-ADD     1             TBLKUT
017500161221     C                   MOVEL     '$5'          TBLCOD
017600161221     C                   eval      tblkey = 'P' + anno + mese
017700161221     C     KTAB          CHAIN     TABEL00F
017800161221     C                   IF        not %Found(Tabel00F)
017900161221     C                   SETON                                        2390
018000161221     C                   MOVEL     MSG(4)        $MSG
018100161221     c                   eval      $msg = %trim($msg) + ' PARTENZE'
018200161221     C                   MOVEL     '1'           WERR
018300161221     C                   GOTO      FINCT1
018400161221     C                   ENDIF
018500161221     C                   endIF
018600161221     C*
018700161221     c                   if        v1ctip = 'A' or
018800161221     c                             v1ctip = *blank
018900161221     C                   Z-ADD     1             TBLKUT
019000161221     C                   MOVEL     '$5'          TBLCOD
019100161221     C                   eval      tblkey = 'A' + anno + mese
019200161221     C     KTAB          CHAIN     TABEL00F
019300161221     C                   IF        not %Found(Tabel00F)
019400161221     C                   SETON                                        2390
019500161221     C                   MOVEL     MSG(4)        $MSG
019600161221     c                   eval      $msg = %trim($msg) + ' ARRIVI'
019700161221     C                   MOVEL     '1'           WERR
019800161221     C                   GOTO      FINCT1
019900161221     C                   ENDIF
020000161221     C                   endIF
020100161221     C*
020200170113     C                   END
020300170117     C*
020400170117IF  2C     V1CSIM        IFEQ      'N'
020500170117IF  2C     V1CSTP        andeq     'N'
020600170117     C*
020700170117     C                   SETON                                        2490
020800170117     C                   MOVEL     MSG(5)        $MSG
020900170117     C                   MOVEL     '1'           WERR
021000170117     C                   GOTO      FINCT1
021100170117     C*
021200170117     C                   END
021300170113     C*
021400000000     C     FINCT1        ENDSR
021500000000     C*------------------------------------------------------------------------*
021600000000     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
021700000000     C*------------------------------------------------------------------------*
021800000000     C     F03GES        BEGSR
021900000000     C*
022000000000     C                   MOVEL     'S'           $FINE
022100000000     C*
022200000000     C                   ENDSR
022300000000     C*------------------------------------------------------------------------*
022400000000     C* F12GES - GESTIONE TASTO FUNZIONALE F12 ==> RITORNO
022500000000     C*------------------------------------------------------------------------*
022600000000     C     F12GES        BEGSR
022700000000     C*
022800000000     C                   MOVEL     'S'           $FINE
022900000000     C*
023000000000     C                   ENDSR
023100000000     C*------------------------------------------------------------------------*
023200000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
023300000000     C*------------------------------------------------------------------------*
023400000000     C     F06GES        BEGSR
023500000000     C*
023600170116     C* CHIAMA IL PROGRAMMA CHE LANCIA LA SOTTOMISSIONE
023700990324     C                   Z-ADD     V1CANN        PARANN
023800990324     C                   Z-ADD     V1CMES        PARMES
023900000000     C                   MOVEL     V1CSIM        PARSIM
024000170116     C                   MOVEL     V1CTIP        PARTIP
024100170117     C                   MOVEL     V1CSTP        PARSTP
024200000000     C                   MOVEL     PARAM         KPJBU
024300170116     C                   CALL      'TREC29R1'
024400000000     C                   PARM                    KPJBA
024500000000     C*
024600000000     C                   ENDSR
024700000000     C*------------------------------------------------------------------------*
024800000000     C* *INZSR - OPERAZIONI INIZIALI
024900000000     C*------------------------------------------------------------------------*
025000000000     C     *INZSR        BEGSR
025100000000     C*---
025200000000     C* RICEVIMENTO PARAMETRI
025300000000     C*---
025400000000     C     *ENTRY        PLIST
025500000000     C                   PARM                    KPJBA
025600000000     C*---
025700170116     c                   exsr      DatiJOB
025800170116     C                   MOVEL     RSUT          DsFIRS
025900170116     C*---
026000000000     C* VARIABILI NON RIFERITE AL DATABASE
026100000000     C*---
026200000000     C                   MOVEL     'N'           $FINE             1
026300000000     C                   MOVEL     '1'           $TESVD            1
026400000000     C                   MOVEL     '1'           $TIVID            1
026500000000     C                   MOVEL     '0'           WERR              1
026600161216     C*---
026700161216     C     Ktab          KLIST
026800161216     C                   KFLD                    tblkut
026900161216     C                   KFLD                    tblcod
027000161216     C                   KFLD                    tblkey
027100161216      *
027200000000     C                   ENDSR
027300170116      *---------------------------------------------------------------*
027400170116      *?  Reperimento Dati del job (Utente/Operativi)                ?
027500170116      *---------------------------------------------------------------*
027600170116     c     DatiJOB       BEGSR
027700170116      *
027800170116     c     *dtaara       define    §azute        azuteds
027900170116     c     *dtaara       define    §datiute      ddatiute
028000170116      *
028100170116     c                   in(E)     *dtaara
028200170116     c                   IF        %ERROR or RSUT = *blanks
028300170116     c                   clear                   Tibs34Ds
028400170116     c                   call      'TIBS34R'
028500170116     c                   parm                    Tibs34Ds
028600170116     c                   in        *dtaara
028700170116     c                   ENDIF
028800170116      *
028900170116     c                   ENDSR
029000000000     O*------------------------------------------------------------------------*
029100990324** -MSG-
029200161216TREC29R- Anno errato                                                          1
029300161216TREC29R- Mese errato                                                          2
029400161216TREC29R- Immettere uno dei valori possibili                                   3
029500170117TREC29R- Non è stato stampato il mese precedente per il tipo                  4
029600170117TREC29R- Se scelta Archiviazione Ottica è obbligatoria la Stampa              5
029700161216TREC29R- 6
029800161216TREC29R- 7
029900161216TREC29R- 8
030000161216TREC29R- 9
030100170116TREC29R- 10

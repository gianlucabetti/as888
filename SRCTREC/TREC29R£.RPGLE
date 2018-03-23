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
002500000000     D PARAM           DS                  INZ
002600990324     D  PARANN                 1      4  0
002700990324     D  PARMES                 5      6  0
002800990420     D  PARSIM                 7      7
002900161216     D  PARtip                 8      8
003000161216     D  PARsoc                 9     11
003100161216     D*-------------------
003200161216     d ds3i          E DS
003300000000     D*-------------------
003400000000     D* ARCHITETTURA
003500000000     D*-------------------
003600000000     D KPJBA         E DS
003700000000     C*------------------------------------------------------------------------*
003800000000     C* MAIN LINE
003900000000     C*------------------------------------------------------------------------*
004000000000     C* IMPOSTAZIONI PRIMA VIDEATA
004100000000     C                   EXSR      IMPVD1
004200000000     C*
004300000000     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
004400000000DO  1C     $FINE         DOUEQ     'S'
004500000000     C     $TIVID        CASEQ     '1'           GESVD1
004600000000     C                   ENDCS
004700000000E   1C                   ENDDO
004800000000     C*
004900000000     C                   SETON                                        LR
005000000000     C*------------------------------------------------------------------------*
005100000000     C* IMPVD1 - IMPOSTAZIONI PRIMA VIDEATA
005200000000     C*------------------------------------------------------------------------*
005300000000     C     IMPVD1        BEGSR
005400000000     C*
005500000000     C* REIMPOSTA FORMATO VIDEO E INDICATORI
005600000000     C                   EXSR      SETVD1
005700000000     C                   MOVEL     '1'           $TIVID
005800000000     C*
005900000000     C                   ENDSR
006000000000     C*------------------------------------------------------------------------*
006100000000     C* SETVD1 - IMPOSTAZIONI INDICATORI PRIMA VIDEATA
006200000000     C*------------------------------------------------------------------------*
006300000000     C     SETVD1        BEGSR
006400000000     C*
006500990324     C                   SETOFF                                       202122
006600161221     C                   SETOFF                                       90  23
006700000000     C                   MOVEL     *BLANKS       $MSG
006800000000     C*
006900000000     C                   ENDSR
007000000000     C*------------------------------------------------------------------------*
007100000000     C* GESVD1 - GESTIONE PRIMA VIDEATA
007200000000     C*------------------------------------------------------------------------*
007300000000     C     GESVD1        BEGSR
007400000000     C*
007500000000     C* EMISSIONE VIDEATA
007600000000IF  1C     $TESVD        IFEQ      '1'
007700161216     C                   WRITE     EC29D00
007800000000     C                   MOVEL     '0'           $TESVD
007900000000E   1C                   ENDIF
008000161216     C                   EXFMT     EC29D01
008100000000     C                   EXSR      SETVD1
008200000000     C* F03-FINE
008300000000IF  1C     *INKC         IFEQ      *ON
008400000000     C                   EXSR      F03GES
008500000000     C                   GOTO      FINVD1
008600000000E   1C                   ENDIF
008700000000     C* F12-RITORNO
008800000000IF  1C     *INKL         IFEQ      *ON
008900000000     C                   EXSR      F12GES
009000000000     C                   GOTO      FINVD1
009100000000E   1C                   ENDIF
009200000000     C* ENTER-CONTROLLA VIDEO
009300000000     C                   EXSR      CTRVD1
009400000000IF  1C     *IN90         IFEQ      *ON
009500000000     C                   GOTO      FINVD1
009600000000E   1C                   ENDIF
009700000000     C* F06-CONFERMA
009800000000IF  1C     *INKF         IFEQ      *ON
009900000000     C                   EXSR      F06GES
010000000000     C                   GOTO      FINVD1
010100000000E   1C                   ENDIF
010200000000     C*
010300000000     C     FINVD1        ENDSR
010400000000     C*------------------------------------------------------------------------*
010500000000     C* CTRVD1 - CONTROLLA PRIMA VIDEATA
010600000000     C*------------------------------------------------------------------------*
010700000000     C     CTRVD1        BEGSR
010800000000     C*
010900000000     C* REIMPOSTA LE VARIABILI DI LAVORO
011000000000     C                   MOVEL     '0'           WERR
011100990407     C*---
011200990407     C* ANNO
011300990407     C*---
011400990407IF  1C     WERR          IFEQ      '0'
011500990407IF  2C     V1CANN        IFLE      *ZEROS
011600990407     C                   SETON                                        2090
011700990407     C                   MOVEL     MSG(1)        $MSG
011800990407     C                   MOVEL     '1'           WERR
011900990407     C                   GOTO      FINCT1
012000990407E   2C                   ENDIF
012100990407E   1C                   ENDIF
012200990415     C*---
012300990415     C* MESE
012400990415     C*---
012500990415IF  1C     WERR          IFEQ      '0'
012600990420IF  2C     V1CMES        IFGE      1
012700990415     C     V1CMES        ANDLE     12
012800990415X   2C                   ELSE
012900990415     C                   SETON                                        2190
013000990415     C                   MOVEL     MSG(2)        $MSG
013100990415     C                   MOVEL     '1'           WERR
013200990415     C                   GOTO      FINCT1
013300990415E   2C                   ENDIF
013400990415E   1C                   ENDIF
013500990324     C*---
013600990324     C* SIMULAZIONE
013700990324     C*---
013800990324IF  1C     WERR          IFEQ      '0'
013900990324IF  2C     V1CSIM        IFNE      'S'
014000990324     C     V1CSIM        ANDNE     'N'
014100990324     C                   SETON                                        2290
014200990324     C                   MOVEL     MSG(3)        $MSG
014300990324     C                   MOVEL     '1'           WERR
014400990324     C                   GOTO      FINCT1
014500990324E   2C                   ENDIF
014600990324E   1C                   ENDIF
014700161221      **
014800161221      *   se al passaggio dell'anno
014900161221     c                   move      v1cmes        mese              2
015000161221     c                   move      v1cann        anno              4
015100161221     c                   if        v1cmes - 1 = 0
015200161221     c                   move      '12'          mese
015300161221     c     v1cann        sub       1             anno1             4 0
015400161221     c                   move      anno1         anno
015500161221     c                   else
015600161221     c     v1cmes        sub       1             mese1             2 0
015700161221     c                   move      mese1         mese
015800161221     c                   end
015900161221     C*
016000170113     C* Solo se NON sono in Simulazione esegue il controllo sul mese
016100170113     C*  precedente.
016200170113IF  2C     V1CSIM        IFEQ      'N'
016300170113IF  2C     V1CANN        andGT     2016
016400170113     C*
016500161221     C*** controllo in tabella se stampato il mese precedente x tipo
016600161221     c                   if        v1ctip = 'T' or
016700161221     c                             v1ctip = *blank
016800161221     C                   Z-ADD     1             TBLKUT
016900161221     C                   MOVEL     '$5'          TBLCOD
017000161221     C                   eval      tblkey = 'T' + anno + mese
017100161221     C     KTAB          CHAIN     TABEL00F
017200161221     C                   IF        not %Found(Tabel00F)
017300161221     C                   SETON                                        2390
017400161221     C                   MOVEL     MSG(4)        $MSG
017500161221     c                   eval      $msg = %trim($msg) + ' TRAINI'
017600161221     C                   MOVEL     '1'           WERR
017700161221     C                   GOTO      FINCT1
017800161221     C                   ENDIF
017900161221     C                   endIF
018000161221     C*
018100161221     c                   if        v1ctip = 'P' or
018200161221     c                             v1ctip = *blank
018300161221     C                   Z-ADD     1             TBLKUT
018400161221     C                   MOVEL     '$5'          TBLCOD
018500161221     C                   eval      tblkey = 'P' + anno + mese
018600161221     C     KTAB          CHAIN     TABEL00F
018700161221     C                   IF        not %Found(Tabel00F)
018800161221     C                   SETON                                        2390
018900161221     C                   MOVEL     MSG(4)        $MSG
019000161221     c                   eval      $msg = %trim($msg) + ' PARTENZE'
019100161221     C                   MOVEL     '1'           WERR
019200161221     C                   GOTO      FINCT1
019300161221     C                   ENDIF
019400161221     C                   endIF
019500161221     C*
019600161221     c                   if        v1ctip = 'A' or
019700161221     c                             v1ctip = *blank
019800161221     C                   Z-ADD     1             TBLKUT
019900161221     C                   MOVEL     '$5'          TBLCOD
020000161221     C                   eval      tblkey = 'A' + anno + mese
020100161221     C     KTAB          CHAIN     TABEL00F
020200161221     C                   IF        not %Found(Tabel00F)
020300161221     C                   SETON                                        2390
020400161221     C                   MOVEL     MSG(4)        $MSG
020500161221     c                   eval      $msg = %trim($msg) + ' ARRIVI'
020600161221     C                   MOVEL     '1'           WERR
020700161221     C                   GOTO      FINCT1
020800161221     C                   ENDIF
020900161221     C                   endIF
021000161221     C*
021100170113     C                   END
021200170113     C*
021300000000     C     FINCT1        ENDSR
021400000000     C*------------------------------------------------------------------------*
021500000000     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
021600000000     C*------------------------------------------------------------------------*
021700000000     C     F03GES        BEGSR
021800000000     C*
021900000000     C                   MOVEL     'S'           $FINE
022000000000     C*
022100000000     C                   ENDSR
022200000000     C*------------------------------------------------------------------------*
022300000000     C* F12GES - GESTIONE TASTO FUNZIONALE F12 ==> RITORNO
022400000000     C*------------------------------------------------------------------------*
022500000000     C     F12GES        BEGSR
022600000000     C*
022700000000     C                   MOVEL     'S'           $FINE
022800000000     C*
022900000000     C                   ENDSR
023000000000     C*------------------------------------------------------------------------*
023100000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
023200000000     C*------------------------------------------------------------------------*
023300000000     C     F06GES        BEGSR
023400000000     C*
023500161216     c                   if        v1ctip = *blank
023600161216     c                   z-add     3             volte             3 0
023700161216     c                   else
023800161216     c                   z-add     1             volte             3 0
023900161216     c                   end
024000000000     C* LANCIA IL PROGRAMMA
024100161216     c                   do        volte         nr                3 0
024200161216     c                   if        volte =1
024300161216     C                   MOVEL     V1CTIP        PARtip
024400161216     c                   else
024500161216     c                   if        nr =1
024600161216     C                   MOVEL     'T'           PARtip
024700161216     c                   elseif    nr =2
024800161216     C                   MOVEL     'A'           PARtip
024900161216     c                   elseif    nr =3
025000161216     C                   MOVEL     'P'           PARtip
025100161216     c                   end
025200161216     c                   end
025300161216     c                   if        PARtip = 'T'
025400161216     C                   MOVEL     soctraini     PARsoc
025500161216     c                   else
025600161216     C                   MOVEL     socBRT        PARsoc
025700161216     c                   end
025800990324     C                   Z-ADD     V1CANN        PARANN
025900990324     C                   Z-ADD     V1CMES        PARMES
026000000000     C                   MOVEL     V1CSIM        PARSIM
026100161216     C                   MOVEL     'EC28'        KCOAZ
026200000000     C                   MOVEL     PARAM         KPJBU
026300000000     C                   CALL      'BCH10'
026400000000     C                   PARM                    KPJBA
026500000121     C*---                 MOVEL'S'       $FINE            *FINE PROGRAMMA
026600161216     c                   end
026700000000     C*
026800000000     C                   ENDSR
026900000000     C*------------------------------------------------------------------------*
027000000000     C* *INZSR - OPERAZIONI INIZIALI
027100000000     C*------------------------------------------------------------------------*
027200000000     C     *INZSR        BEGSR
027300000000     C*---
027400000000     C* RICEVIMENTO PARAMETRI
027500000000     C*---
027600000000     C     *ENTRY        PLIST
027700000000     C                   PARM                    KPJBA
027800000000     C*---
027900170116     c                   exsr      DatiJOB
028000170116     C                   MOVEL     RSUT          DsFIRS
028100170116     C*---
028200000000     C* VARIABILI NON RIFERITE AL DATABASE
028300000000     C*---
028400000000     C                   MOVEL     'N'           $FINE             1
028500000000     C                   MOVEL     '1'           $TESVD            1
028600000000     C                   MOVEL     '1'           $TIVID            1
028700000000     C                   MOVEL     '0'           WERR              1
028800161216     C*---
028900161216     C     Ktab          KLIST
029000161216     C                   KFLD                    tblkut
029100161216     C                   KFLD                    tblcod
029200161216     C                   KFLD                    tblkey
029300161216      *
029400161216     c                   clear                   soctraini         3
029500161216     c                   z-add     1             tblkut
029600161216     c                   move      '3I'          tblcod
029700161216     c                   movel(p)  '1'           tblkey
029800161216     c     ktab          chain     tabel00f
029900161216     c                   if        %found(tabel00f)
030000161216     c                   movel     tbluni        ds3i
030100161216     c                   eval      soctraini = %subst(§3IBST: 7: 3)
030200161216     c                   endif
030300161216     c                   move      '201'         socBRT            3
030400000000     C*
030500000000     C                   ENDSR
030600170116      *---------------------------------------------------------------*
030700170116      *?  Reperimento Dati del job (Utente/Operativi)                ?
030800170116      *---------------------------------------------------------------*
030900170116     c     DatiJOB       BEGSR
031000170116      *
031100170116     c     *dtaara       define    §azute        azuteds
031200170116     c     *dtaara       define    §datiute      ddatiute
031300170116      *
031400170116     c                   in(E)     *dtaara
031500170116     c                   IF        %ERROR or RSUT = *blanks
031600170116     c                   clear                   Tibs34Ds
031700170116     c                   call      'TIBS34R'
031800170116     c                   parm                    Tibs34Ds
031900170116     c                   in        *dtaara
032000170116     c                   ENDIF
032100170116      *
032200170116     c                   ENDSR
032300000000     O*------------------------------------------------------------------------*
032400990324** -MSG-
032500161216TREC29R- Anno errato                                                          1
032600161216TREC29R- Mese errato                                                          2
032700161216TREC29R- Immettere uno dei valori possibili                                   3
032800161221TREC29R- Non è stato stampato il mese precedente per il tipo
032900161216TREC29R- 5
033000161216TREC29R- 6
033100161216TREC29R- 7
033200161216TREC29R- 8
033300161216TREC29R- 9
033400161216TREC29R- Impossibile lanciare questa elaborazione in SDI !!!                  10

000100980223     H DECEDIT('0,') DATEDIT(*DMY.)
000200020621
000300020621     FFLTR351L  IF   E           K DISK
000400020621     FAZORG01L  IF   E           K DISK
000500980227     FFNLV68D   CF   E             WORKSTN
000600980403     F                                     SFILE(LV68S02:NRR)
000700020621
000800980224     D TES             S             35    DIM(1) CTDATA PERRCD(1)
000900020724     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
001000980223     D*
001100980223     D KPJBA         E DS
001200020621     D*
001300020621     D PARAM1          DS                  INZ
001400020621     D  PA1CTL                 1      1
001500020621     D  PA1DPL                 2      3
001600020621     D  PA1APL                 4      5
001700020621     D  PA1POG                 6      8
001800020621     D*
001900020621     D UT§DSE0F      E DS
002000020621     D CNCR80        E DS
002100020621     D FNLV50DS      E DS                  INZ
002200020621     D*
002300020621     D WRKPOG          S              3  0 INZ
002400020621     D wCTL            S                   LIKE(TR3CTL) INZ
002500020724     D*
002600020724     D* caratteri numerici consentiti
002700020724     D DigitN          C                   CONST('1234567890')
002800020621
002900980223     C*****************************************************************
003000980223     C* RIEPILOGO INDICATORI
003100980223     C*****************************************************************
003200980227     C* 31/   - DI COMODO
003300980227     C* 40/42 - ERRORI
003400980223     C*****************************************************************
003500020621      *---------------------------------------------------------------*
003600020621      * DEFINIZIONE CHIAVI
003700020621      *
003800020621     C     K03TR350V     KLIST
003900020621     C                   KFLD                    V1CPOG
004000020621     C                   KFLD                    V1CCTL
004100020621     C                   KFLD                    V1CDPL
004200020621     C     K02TR350V     KLIST
004300020621     C                   KFLD                    V1CPOG
004400020621     C                   KFLD                    V1CCTL
004500020621      *
004600020621     C     K03TR350      KLIST
004700020621     C                   KFLD                    TR3POG
004800020621     C                   KFLD                    TR3CTL
004900020621     C                   KFLD                    TR3NPL
005000020621      *---------------------------------------------------------------*
005100980223     C     *ENTRY        PLIST
005200980223     C                   PARM                    KPJBA
005300980223     C                   Z-ADD     1             CODUT
005400980223     C                   CALL      'X§PARUT'
005500020621     C                   PARM                    UT§DSE0F
005600980223     C                   MOVEL     RAGUT         VTCRSU
005700980223     C                   MOVEL     REC80         CNCR80
005800020621      *
005900980223      * TESTATA PGM
006000980224     C                   MOVEL     TES(1)        VTCTES
006100020621      *
006200020621      * Imposto il "P.O. IN GESTIONE"
006300020621     c                   move      simPOU        V1Cpog
006400020621if  1c                   IF        simTPP = '2' or simTPP = *blanks
006500020621     c                   eval      *in06  = *on
006600020621e   1c                   ENDIF
006700020618     c                   exsr      CTRFGS
006800020618     c                   movel     ORGdes        V1Dpog
006900020618     C                   SETOFF                                       9028
007000020621      *
007100020621      * Non visualizzo "P.O. IN GESTIONE" se sono in sede
007200020621if  1C                   IF        simFEL = *zeros
007300020621     C                   eval      *in06  = *on
007400020621     C                   eval      *in07  = *on
007500020621e   1C                   ENDIF
007600020621      *---------------------------------------------------------------*
007700980223     C     INIZIO        TAG
007800980223     C                   CLEAR                   V1CCTL
007900980302     C                   CLEAR                   V1CDPL
008000980302     C                   CLEAR                   V1CAPL
008100980223      *
008200980223     C     FOR01         TAG
008300980302     C                   WRITE     LV68T01
008400980302     C                   EXFMT     LV68D01
008500980223      *
008600980223      * pulizia campo messaggio e relativo indicatore  (*IN28)
008700980224     C                   CLEAR                   V1CMSG
008800980223      * azzero indicatori relativi ai messaggi di errore
008900980302     C                   SETOFF                                       404142
009000020621     C                   SETOFF                                       43
009100020618     C                   SETOFF                                       2890
009200980223      *
009300980223      ** CMD3  - FINE LAVORO
009400980223     C   KC              GOTO      FINE
009500020621      *
009600020621      * F18=Cambio P.O. in gestione
009700020621     c   KS              eval      *in05 = *on
009800020621     c   KS              goto      FOR01
009900020621      *
010000020621      * 05 on --> controllo P.O. in gestione
010100020621if  1c                   if        *in05
010200020621     C                   clear                   V1Dpog
010300020621     c                   exsr      CTRFGS
010400020621     c   90              movel     D50MSG        V1CMSG
010500020621     c   90              goto      FOR01
010600020621     c                   movel     ORGdes        V1Dpog
010700020621     c                   eval      *in05 = *off
010800020621e   1c                   endif
010900980223      *
011000980223      ** CONTROLLI FORMATO
011100980403      *
011200980403      * Se esiste un ? di ricerca piani di lavorazione richiamo l'interrogazione
011300020621    1C     V1CDPL        IFNE      *BLANKS
011400980403     C     '?'           SCAN      V1CDPL                                 90
011500020621    2C     *IN90         IFEQ      *ON
011600980403     C                   EXSR      RICNPL
011700980403     C                   MOVE      V2SNPL        V1CDPL
011800020621    3C     V1CCTL        IFEQ      ' '
011900980403     C     V1CCTL        OREQ      '0'
012000980403     C                   MOVEL     V1CDPL        V1CCTL
012100020621   -3C                   ENDIF
012200020621   -2C                   ENDIF
012300020621   -1C                   ENDIF
012400980403      *
012500020621    1C     V1CAPL        IFNE      *BLANKS
012600980403     C     '?'           SCAN      V1CAPL                                 90
012700020621    2C     *IN90         IFEQ      *ON
012800980403     C                   EXSR      RICNPL
012900980403     C                   MOVE      V2SNPL        V1CAPL
013000020621    3C     V1CCTL        IFEQ      ' '
013100980403     C     V1CCTL        OREQ      '0'
013200980403     C                   MOVEL     V1CAPL        V1CCTL
013300020621   -3C                   ENDIF
013400020621   -2C                   ENDIF
013500020621   -1C                   ENDIF
013600980403      *
013700020621    1C     V1CCTL        IFEQ      *BLANK
013800980224     C     V1CCTL        OREQ      *ZERO
013900980302     C                   MOVEL     MSG(1)        V1CMSG
014000980302     C                   SETON                                        4028
014100980224     C                   GOTO      FOR01
014200020621   -1C                   ENDIF
014300020724      * controllo effettiva numericità del campo
014400020724     C     DigitN        Check     V1CCTL                                 28
014500020724     C                   If        *in28 = *on
014600020724     C                   seton                                        40
014700020724     C                   movel     MSG(5)        V1CMSG
014800020724     C                   goto      FOR01
014900020724     C                   Endif
015000980224      *
015100020621    1C     V1CDPL        IFNE      *BLANKS
015200980305     C     V1CDPL        ANDNE     *ZEROS
015300980302     C                   MOVEL     V1CDPL        WCTL
015400980223      * il primo carattere del piano di lavorazione deve essere uguale
015500980223      * al tipo lavorazione
015600020621    2C     WCTL          IFNE      V1CCTL
015700980302     C                   MOVEL     MSG(2)        V1CMSG
015800980302     C                   SETON                                        4128
015900980223     C                   GOTO      FOR01
016000020621   -2C                   ENDIF
016100020724      * controllo effettiva numericità del campo
016200020724     C     DigitN        Check     V1CDPL                                 28
016300020724     C                   If        *in28 = *on
016400020724     C                   seton                                        41
016500020724     C                   movel     MSG(5)        V1CMSG
016600020724     C                   goto      FOR01
016700020724     C                   Endif
016800020621   -1C                   ENDIF
016900980302      *
017000020621    1C     V1CDPL        IFEQ      *BLANKS
017100980305     C     V1CDPL        OREQ      *ZEROS
017200980302     C                   MOVEL     *ZEROS        V1CDPL
017300980305     C                   MOVEL     V1CCTL        V1CDPL
017400020621   -1C                   ENDIF
017500980302      *
017600020621    1C     V1CAPL        IFNE      *BLANKS
017700980305     C     V1CAPL        ANDNE     *ZEROS
017800980302     C                   MOVEL     V1CAPL        WCTL
017900980302      * il primo carattere del piano di lavorazione deve essere uguale
018000980302      * al tipo lavorazione
018100020621    2C     WCTL          IFNE      V1CCTL
018200980302     C                   MOVEL     MSG(2)        V1CMSG
018300980302     C                   SETON                                        4228
018400980302     C                   GOTO      FOR01
018500020621   -2C                   ENDIF
018600020724      * controllo effettiva numericità del campo
018700020724     C     DigitN        Check     V1CAPL                                 28
018800020724     C                   If        *in28 = *on
018900020724     C                   seton                                        42
019000020724     C                   movel     MSG(5)        V1CMSG
019100020724     C                   goto      FOR01
019200020724     C                   Endif
019300020621   -1C                   ENDIF
019400980302      *
019500020621    1C     V1CAPL        IFEQ      *BLANKS
019600980305     C     V1CAPL        OREQ      *ZEROS
019700980302     C                   MOVEL     '99'          V1CAPL
019800980305     C                   MOVEL     V1CCTL        V1CAPL
019900020621   -1C                   ENDIF
020000980302      *
020100980302      * controllo che i limiti dal al piano di lavorazione
020200020621    1C     V1CDPL        IFGT      V1CAPL
020300980302     C                   MOVEL     MSG(3)        V1CMSG
020400980302     C                   SETON                                        4128
020500980302     C                   GOTO      FOR01
020600020621   -1C                   ENDIF
020700980302      * Controllo l'esistenza di records  per le richieste fatte
020800020621     C     K03TR350V     SETLL     FLTR3500
020900020621     C     K02TR350V     READE     FLTR3500                               41
021000020621    1C     *IN41         IFEQ      *ON
021100980302     C                   MOVEL     MSG(4)        V1CMSG
021200980302     C                   SETON                                        4128
021300980302     C                   GOTO      FOR01
021400020621   X1C                   ELSE
021500980302      *
021600020621    2C     TR3NPL        IFGT      V1CAPL
021700980302     C                   MOVEL     MSG(4)        V1CMSG
021800980302     C                   SETON                                        4128
021900980302     C                   GOTO      FOR01
022000020621   -2C                   ENDIF
022100980302      *
022200020621   -1C                   ENDIF
022300980302      *
022400980302     C  NKF              GOTO      FOR01
022500980302      *
022600980302      *
022700020621     C                   MOVE      V1CPOG        PA1POG
022800980302     C                   MOVE      V1CCTL        PA1CTL
022900980302     C                   MOVE      V1CDPL        PA1DPL
023000980302     C                   MOVE      V1CAPL        PA1APL
023100980302     C                   MOVEL     PARAM1        KPJBU
023200980305     C                   MOVEL     'LV69'        KCOAZ
023300980302     C                   CALL      'BCH10'
023400980302     C                   PARM                    KPJBA
023500980224      *
023600980224     C                   GOTO      INIZIO
023700980224      *
023800980224     C     FINE          TAG
023900980224     C                   SETON                                        LR
024000020621      *
024100020621      *------------------------------------------------------------------------
024200020621      * RICNPL RICERCA PIANO DI LAVORAZIONE
024300020621      *------------------------------------------------------------------------
024400980403     CSR   RICNPL        BEGSR
024500980403      * Pulizia ed inizializzazione subfile
024600980403     C                   SETOFF                                       2021
024700980403     C                   WRITE     LV68C02
024800980403     C                   Z-ADD     *ZEROS        NRR               4 0
024900980403      * Caricamento subfile
025000020621     C     K02TR350V     SETLL     FLTR3500
025100020621     C     V1Cpog        READE     FLTR3500                               31
025200980403    1C     *IN31         DOWEQ     *OFF
025300020621      *
025400980403      * se richiesto uno specifico tipo di lavorazione faccio vedere
025500980403      * solo i relativi piani di lavorazione
025600020621    2C     V1CCTL        IFEQ      ' '
025700980403     C     V1CCTL        ORGT      '0'
025800980403     C     TR3CTL        ANDEQ     V1CCTL
025900980403     C                   ADD       1             NRR
026000980403     C                   MOVE      TR3NPL        V2SNPL
026100980403     C                   CLEAR                   V2SSEL
026200980403      * scrivo il subfile
026300980403     C                   WRITE     LV68S02
026400980403      * mi posiziono al piano di lavorazione successivo
026500020621     C     K03TR350      SETGT     FLTR3500
026600020621   -2C                   ENDIF
026700980403      *
026800020621     C     V1Cpog        READE     FLTR3500                               31
026900020621   -1C                   ENDDO
027000980403      *
027100980403      * GESTIONE SUBFILE
027200980403     C                   WRITE     LV68T01
027300980403     C                   WRITE     LV68Z02
027400980403     C                   Z-ADD     1             REC
027500980403     C                   SETON                                        21
027600980403     C     NRR           COMP      0                                  20
027700980403      *
027800980403    1C                   DO        *HIVAL
027900020621      *
028000980403     C                   EXFMT     LV68C02
028100020621      *
028200020621      * F12 - videata precedente
028300020621    2C     *INKL         IFEQ      *ON
028400980403     C                   MOVE      *BLANKS       V2SNPL
028500980403     C                   LEAVE
028600020621   -2C                   ENDIF
028700020621      *
028800020621      * Enter - controllo dati inseriti a video
028900980403    2C     *INKL         IFEQ      *OFF
029000980403     C                   Z-ADD     1             NRR
029100980403      *
029200020621    3C                   DO        *HIVAL
029300980403      *
029400980403     C     NRR           CHAIN     LV68S02                            32
029500980403     C   32              LEAVE
029600020621    4C     V2SSEL        IFEQ      '1'
029700980403     C                   MOVEL     *ON           *INKL
029800980403     C                   LEAVE
029900020621   -4C                   ENDIF
030000980403     C                   ADD       1             NRR
030100020621      *
030200020621   -3C                   ENDDO
030300020621      *
030400020621   -2C                   ENDIF
030500980403      *
030600020621   -1C  NKL              ENDDO
030700980403      *
030800980403     C                   ENDSR
030900020621      *
031000020621      **********************************************************************
031100020621      * CONTROLLO IL P.O. IN IN GESTIONE
031200020621      **********************************************************************
031300020621     C     CTRFGS        BEGSR
031400020621      *
031500020621     C                   setoff                                       9028
031600020621     C                   clear                   FNLV50DS
031700020621     C                   movel     KNMUS         D50PRU
031800020621     C                   move      V1Cpog        D50FGS
031900020621     C                   call      'FNLV50R'
032000020621     C                   parm                    FNLV50DS
032100020621      *
032200020621if  1C                   IF        D50ERR <> *blanks
032300020621     C                   seton                                        9028
032400020621x   1C                   else
032500020621      * decodifica
032600020621     C                   move      V1Cpog        WRKpog
032700020621     C     WRKpog        chain     AZORG
032800020621if  2C                   IF        not %found(AZORG01L)
032900020621     C                   clear                   ORGdes
033000020621e   2C                   ENDIF
033100020621e   1C                   ENDIF
033200020621      *
033300020621     C                   ENDSR
033400020621      *
033500980302** TESTATA PROGRAMMA
033600980227  STAMPA  PIANI  DI  SMISTAMENTO
033700980223** MESSAGGI ERRORI
033800980302Inserire il tipo lavorazione                                                   1
033900980302Primo carattere del piano lavorazione deve essere uguale al tipo lavorazione   2
034000980302Dal piano di lavorazione deve essere minore di Al piano di lavorazione         3
034100980302Non presenti dati per la selezione richiesta                                   4
034200020724Solo ammessi solo valori numerici                                              5

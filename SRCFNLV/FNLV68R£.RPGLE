000100980223     H DECEDIT('0,') DATEDIT(*DMY.)
000200020621
000300020621     FFLTR351L  IF   E           K DISK
000400020621     FAZORG01L  IF   E           K DISK
000500980227     FFNLV68D   CF   E             WORKSTN
000600980403     F                                     SFILE(LV68S02:NRR)
000700020621
000800980224     D TES             S             35    DIM(1) CTDATA PERRCD(1)
000900980302     D MSG             S             78    DIM(4) CTDATA PERRCD(1)
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
002500020621
002600980223     C*****************************************************************
002700980223     C* RIEPILOGO INDICATORI
002800980223     C*****************************************************************
002900980227     C* 31/   - DI COMODO
003000980227     C* 40/42 - ERRORI
003100980223     C*****************************************************************
003200020621      *---------------------------------------------------------------*
003300020621      * DEFINIZIONE CHIAVI
003400020621      *
003500020621     C     K03TR350V     KLIST
003600020621     C                   KFLD                    V1CPOG
003700020621     C                   KFLD                    V1CCTL
003800020621     C                   KFLD                    V1CDPL
003900020621     C     K02TR350V     KLIST
004000020621     C                   KFLD                    V1CPOG
004100020621     C                   KFLD                    V1CCTL
004200020621      *
004300020621     C     K03TR350      KLIST
004400020621     C                   KFLD                    TR3POG
004500020621     C                   KFLD                    TR3CTL
004600020621     C                   KFLD                    TR3NPL
004700020621      *---------------------------------------------------------------*
004800980223     C     *ENTRY        PLIST
004900980223     C                   PARM                    KPJBA
005000980223     C                   Z-ADD     1             CODUT
005100980223     C                   CALL      'X§PARUT'
005200020621     C                   PARM                    UT§DSE0F
005300980223     C                   MOVEL     RAGUT         VTCRSU
005400980223     C                   MOVEL     REC80         CNCR80
005500020621      *
005600980223      * TESTATA PGM
005700980224     C                   MOVEL     TES(1)        VTCTES
005800020621      *
005900020621      * Imposto il "P.O. IN GESTIONE"
006000020621     c                   move      simPOU        V1Cpog
006100020621if  1c                   IF        simTPP = '2' or simTPP = *blanks
006200020621     c                   eval      *in06  = *on
006300020621e   1c                   ENDIF
006400020618     c                   exsr      CTRFGS
006500020618     c                   movel     ORGdes        V1Dpog
006600020618     C                   SETOFF                                       9028
006700020621      *
006800020621      * Non visualizzo "P.O. IN GESTIONE" se sono in sede
006900020621if  1C                   IF        simFEL = *zeros
007000020621     C                   eval      *in06  = *on
007100020621     C                   eval      *in07  = *on
007200020621e   1C                   ENDIF
007300020621      *---------------------------------------------------------------*
007400980223     C     INIZIO        TAG
007500980223     C                   CLEAR                   V1CCTL
007600980302     C                   CLEAR                   V1CDPL
007700980302     C                   CLEAR                   V1CAPL
007800980223      *
007900980223     C     FOR01         TAG
008000980302     C                   WRITE     LV68T01
008100980302     C                   EXFMT     LV68D01
008200980223      *
008300980223      * pulizia campo messaggio e relativo indicatore  (*IN28)
008400980224     C                   CLEAR                   V1CMSG
008500980223      * azzero indicatori relativi ai messaggi di errore
008600980302     C                   SETOFF                                       404142
008700020621     C                   SETOFF                                       43
008800020618     C                   SETOFF                                       2890
008900980223      *
009000980223      ** CMD3  - FINE LAVORO
009100980223     C   KC              GOTO      FINE
009200020621      *
009300020621      * F18=Cambio P.O. in gestione
009400020621     c   KS              eval      *in05 = *on
009500020621     c   KS              goto      FOR01
009600020621      *
009700020621      * 05 on --> controllo P.O. in gestione
009800020621if  1c                   if        *in05
009900020621     C                   clear                   V1Dpog
010000020621     c                   exsr      CTRFGS
010100020621     c   90              movel     D50MSG        V1CMSG
010200020621     c   90              goto      FOR01
010300020621     c                   movel     ORGdes        V1Dpog
010400020621     c                   eval      *in05 = *off
010500020621e   1c                   endif
010600980223      *
010700980223      ** CONTROLLI FORMATO
010800980403      *
010900980403      * Se esiste un ? di ricerca piani di lavorazione richiamo l'interrogazione
011000020621    1C     V1CDPL        IFNE      *BLANKS
011100980403     C     '?'           SCAN      V1CDPL                                 90
011200020621    2C     *IN90         IFEQ      *ON
011300980403     C                   EXSR      RICNPL
011400980403     C                   MOVE      V2SNPL        V1CDPL
011500020621    3C     V1CCTL        IFEQ      ' '
011600980403     C     V1CCTL        OREQ      '0'
011700980403     C                   MOVEL     V1CDPL        V1CCTL
011800020621   -3C                   ENDIF
011900020621   -2C                   ENDIF
012000020621   -1C                   ENDIF
012100980403      *
012200020621    1C     V1CAPL        IFNE      *BLANKS
012300980403     C     '?'           SCAN      V1CAPL                                 90
012400020621    2C     *IN90         IFEQ      *ON
012500980403     C                   EXSR      RICNPL
012600980403     C                   MOVE      V2SNPL        V1CAPL
012700020621    3C     V1CCTL        IFEQ      ' '
012800980403     C     V1CCTL        OREQ      '0'
012900980403     C                   MOVEL     V1CAPL        V1CCTL
013000020621   -3C                   ENDIF
013100020621   -2C                   ENDIF
013200020621   -1C                   ENDIF
013300980403      *
013400020621    1C     V1CCTL        IFEQ      *BLANK
013500980224     C     V1CCTL        OREQ      *ZERO
013600980302     C                   MOVEL     MSG(1)        V1CMSG
013700980302     C                   SETON                                        4028
013800980224     C                   GOTO      FOR01
013900020621   -1C                   ENDIF
014000980224      *
014100020621    1C     V1CDPL        IFNE      *BLANKS
014200980305     C     V1CDPL        ANDNE     *ZEROS
014300980302     C                   MOVEL     V1CDPL        WCTL
014400980223      * il primo carattere del piano di lavorazione deve essere uguale
014500980223      * al tipo lavorazione
014600020621    2C     WCTL          IFNE      V1CCTL
014700980302     C                   MOVEL     MSG(2)        V1CMSG
014800980302     C                   SETON                                        4128
014900980223     C                   GOTO      FOR01
015000020621   -2C                   ENDIF
015100020621   -1C                   ENDIF
015200980302      *
015300020621    1C     V1CDPL        IFEQ      *BLANKS
015400980305     C     V1CDPL        OREQ      *ZEROS
015500980302     C                   MOVEL     *ZEROS        V1CDPL
015600980305     C                   MOVEL     V1CCTL        V1CDPL
015700020621   -1C                   ENDIF
015800980302      *
015900020621    1C     V1CAPL        IFNE      *BLANKS
016000980305     C     V1CAPL        ANDNE     *ZEROS
016100980302     C                   MOVEL     V1CAPL        WCTL
016200980302      * il primo carattere del piano di lavorazione deve essere uguale
016300980302      * al tipo lavorazione
016400020621    2C     WCTL          IFNE      V1CCTL
016500980302     C                   MOVEL     MSG(2)        V1CMSG
016600980302     C                   SETON                                        4228
016700980302     C                   GOTO      FOR01
016800020621   -2C                   ENDIF
016900020621   -1C                   ENDIF
017000980302      *
017100020621    1C     V1CAPL        IFEQ      *BLANKS
017200980305     C     V1CAPL        OREQ      *ZEROS
017300980302     C                   MOVEL     '99'          V1CAPL
017400980305     C                   MOVEL     V1CCTL        V1CAPL
017500020621   -1C                   ENDIF
017600980302      *
017700980302      * controllo che i limiti dal al piano di lavorazione
017800020621    1C     V1CDPL        IFGT      V1CAPL
017900980302     C                   MOVEL     MSG(3)        V1CMSG
018000980302     C                   SETON                                        4128
018100980302     C                   GOTO      FOR01
018200020621   -1C                   ENDIF
018300980302      * Controllo l'esistenza di records  per le richieste fatte
018400020621     C     K03TR350V     SETLL     FLTR3500
018500020621     C     K02TR350V     READE     FLTR3500                               41
018600020621    1C     *IN41         IFEQ      *ON
018700980302     C                   MOVEL     MSG(4)        V1CMSG
018800980302     C                   SETON                                        4128
018900980302     C                   GOTO      FOR01
019000020621   X1C                   ELSE
019100980302      *
019200020621    2C     TR3NPL        IFGT      V1CAPL
019300980302     C                   MOVEL     MSG(4)        V1CMSG
019400980302     C                   SETON                                        4128
019500980302     C                   GOTO      FOR01
019600020621   -2C                   ENDIF
019700980302      *
019800020621   -1C                   ENDIF
019900980302      *
020000980302     C  NKF              GOTO      FOR01
020100980302      *
020200980302      *
020300020621     C                   MOVE      V1CPOG        PA1POG
020400980302     C                   MOVE      V1CCTL        PA1CTL
020500980302     C                   MOVE      V1CDPL        PA1DPL
020600980302     C                   MOVE      V1CAPL        PA1APL
020700980302     C                   MOVEL     PARAM1        KPJBU
020800980305     C                   MOVEL     'LV69'        KCOAZ
020900980302     C                   CALL      'BCH10'
021000980302     C                   PARM                    KPJBA
021100980224      *
021200980224     C                   GOTO      INIZIO
021300980224      *
021400980224     C     FINE          TAG
021500980224     C                   SETON                                        LR
021600020621      *
021700020621      *------------------------------------------------------------------------
021800020621      * RICNPL RICERCA PIANO DI LAVORAZIONE
021900020621      *------------------------------------------------------------------------
022000980403     CSR   RICNPL        BEGSR
022100980403      * Pulizia ed inizializzazione subfile
022200980403     C                   SETOFF                                       2021
022300980403     C                   WRITE     LV68C02
022400980403     C                   Z-ADD     *ZEROS        NRR               4 0
022500980403      * Caricamento subfile
022600020621     C     K02TR350V     SETLL     FLTR3500
022700020621     C     V1Cpog        READE     FLTR3500                               31
022800980403    1C     *IN31         DOWEQ     *OFF
022900020621      *
023000980403      * se richiesto uno specifico tipo di lavorazione faccio vedere
023100980403      * solo i relativi piani di lavorazione
023200020621    2C     V1CCTL        IFEQ      ' '
023300980403     C     V1CCTL        ORGT      '0'
023400980403     C     TR3CTL        ANDEQ     V1CCTL
023500980403     C                   ADD       1             NRR
023600980403     C                   MOVE      TR3NPL        V2SNPL
023700980403     C                   CLEAR                   V2SSEL
023800980403      * scrivo il subfile
023900980403     C                   WRITE     LV68S02
024000980403      * mi posiziono al piano di lavorazione successivo
024100020621     C     K03TR350      SETGT     FLTR3500
024200020621   -2C                   ENDIF
024300980403      *
024400020621     C     V1Cpog        READE     FLTR3500                               31
024500020621   -1C                   ENDDO
024600980403      *
024700980403      * GESTIONE SUBFILE
024800980403     C                   WRITE     LV68T01
024900980403     C                   WRITE     LV68Z02
025000980403     C                   Z-ADD     1             REC
025100980403     C                   SETON                                        21
025200980403     C     NRR           COMP      0                                  20
025300980403      *
025400980403    1C                   DO        *HIVAL
025500020621      *
025600980403     C                   EXFMT     LV68C02
025700020621      *
025800020621      * F12 - videata precedente
025900020621    2C     *INKL         IFEQ      *ON
026000980403     C                   MOVE      *BLANKS       V2SNPL
026100980403     C                   LEAVE
026200020621   -2C                   ENDIF
026300020621      *
026400020621      * Enter - controllo dati inseriti a video
026500980403    2C     *INKL         IFEQ      *OFF
026600980403     C                   Z-ADD     1             NRR
026700980403      *
026800020621    3C                   DO        *HIVAL
026900980403      *
027000980403     C     NRR           CHAIN     LV68S02                            32
027100980403     C   32              LEAVE
027200020621    4C     V2SSEL        IFEQ      '1'
027300980403     C                   MOVEL     *ON           *INKL
027400980403     C                   LEAVE
027500020621   -4C                   ENDIF
027600980403     C                   ADD       1             NRR
027700020621      *
027800020621   -3C                   ENDDO
027900020621      *
028000020621   -2C                   ENDIF
028100980403      *
028200020621   -1C  NKL              ENDDO
028300980403      *
028400980403     C                   ENDSR
028500020621      *
028600020621      **********************************************************************
028700020621      * CONTROLLO IL P.O. IN IN GESTIONE
028800020621      **********************************************************************
028900020621     C     CTRFGS        BEGSR
029000020621      *
029100020621     C                   setoff                                       9028
029200020621     C                   clear                   FNLV50DS
029300020621     C                   movel     KNMUS         D50PRU
029400020621     C                   move      V1Cpog        D50FGS
029500020621     C                   call      'FNLV50R'
029600020621     C                   parm                    FNLV50DS
029700020621      *
029800020621if  1C                   IF        D50ERR <> *blanks
029900020621     C                   seton                                        9028
030000020621x   1C                   else
030100020621      * decodifica
030200020621     C                   move      V1Cpog        WRKpog
030300020621     C     WRKpog        chain     AZORG
030400020621if  2C                   IF        not %found(AZORG01L)
030500020621     C                   clear                   ORGdes
030600020621e   2C                   ENDIF
030700020621e   1C                   ENDIF
030800020621      *
030900020621     C                   ENDSR
031000020621      *
031100980302** TESTATA PROGRAMMA
031200980227  STAMPA  PIANI  DI  SMISTAMENTO
031300980223** MESSAGGI ERRORI
031400980302Inserire il tipo lavorazione                                                   1
031500980302Primo carattere del piano lavorazione deve essere uguale al tipo lavorazione   2
031600980302Dal piano di lavorazione deve essere minore di Al piano di lavorazione         3
031700980302Non presenti dati per la selezione richiesta                                   4

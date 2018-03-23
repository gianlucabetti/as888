000100980828     H*------------------------------------------------------------------------*
000200980828     H* REPORT COMMERCIALE - RICHIESTA PARAMETRI                               *
000300980828     H*------------------------------------------------------------------------*
000400980828     H DATEDIT(*YMD)
000500980828     F*------------------------------------------------------------------------*
000600980828     F* DATA BASE
000700980828     F*------------------------------------------------------------------------*
000800980828     FTIST10D   CF   E             WORKSTN
000900980828     FTABEL00F  IF   E           K DISK
001000980828     D*------------------------------------------------------------------------*
001100980828     D* SCHIERE
001200980828     D*------------------------------------------------------------------------*
001300980828     D*---
001400980828     D* MESSAGGI VIDEO
001500980828     D*---
001600980828     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001700980828     D*------------------------------------------------------------------------*
001800980828     D* INPUT
001900980828     D*------------------------------------------------------------------------*
002000980828     D*---
002100980828     D* DS AVANZAMENTO TRASFERIMENTI
002200980828     D*---
002300980828     D DS$3          E DS
002400980828     D*---
002500980828     D* DS CONTROLLA DATA (8)
002600980828     D*---
002700980828     D WLBDA8          DS
002800980828     D  G08DAT                 1      8  0
002900980828     D  G08INV                 9     16  0
003000980828     D  G08ERR                17     17
003100980828     D  G08TGI                18     22  0
003200980828     D*---
003300980828     D* DS PARAMETRI LANCIO
003400980828     D*---
003500980828     D PARAM           DS                  INZ
003600980828     D  PARAMI                 1      6  0
003700980828     D  PARAMF                 7     12  0
003800051010      *
003900051010     D DS5AS1        E DS
004000980828     D*---
004100980828     D* ARCHITETTURA
004200980828     D*---
004300980828     D KPJBA         E DS
004400980828     C*------------------------------------------------------------------------*
004500980828     C* MAIN LINE
004600980828     C*------------------------------------------------------------------------*
004700980828     C*
004800980828     C* IMPOSTAZIONI PRIMA VIDEATA
004900980828     C                   EXSR      IMPVD1
005000980828     C*
005100980828     C* CICLO FINO A RICHIESTA DI FINE ELABORAZIONE
005200980828DO  1C     $FINE         DOUEQ     'S'
005300980828     C     TIPVID        CASEQ     '1'           GESVD1                         *1° VIDEO
005400980828     C                   ENDCS
005500980828E   1C                   ENDDO
005600980828     C*
005700980828     C                   SETON                                        LR
005800980828     C*------------------------------------------------------------------------*
005900980828     C* IMPVD1 - IMPOSTAZIONI PRIMA VIDEATA
006000980828     C*------------------------------------------------------------------------*
006100980828     C     IMPVD1        BEGSR
006200980828     C*
006300980828     C* REIMPOSTA FORMATO VIDEO E INDICATORI
006400980828     C                   EXSR      SETVD1
006500980828     C                   MOVEL     '1'           TIPVID                         *GESTIONE VIDEO
006600980828     C*
006700980828     C                   ENDSR
006800980828     C*------------------------------------------------------------------------*
006900980828     C* SETVD1 - IMPOSTAZIONI INDICATORI PRIMA VIDEATA
007000980828     C*------------------------------------------------------------------------*
007100980828     C     SETVD1        BEGSR
007200980828     C*
007300980828     C                   SETOFF                                       2021       ERRORI
007400980828     C                   SETOFF                                       90         GENERICO DI ERRORE
007500980828     C                   MOVEL     *BLANKS       $MSG                            MESSAGGIO
007600980828     C*
007700980828     C                   ENDSR
007800980828     C*------------------------------------------------------------------------*
007900980828     C* GESVD1 - GESTIONE PRIMA VIDEATA
008000980828     C*------------------------------------------------------------------------*
008100980828     C     GESVD1        BEGSR
008200980828     C*
008300980828     C* EMISSIONE VIDEATA
008400980828IF  1C     $TESVD        IFEQ      '1'
008500980828     C                   WRITE     ST10D00                                      *TESTATA
008600980828     C                   MOVEL     '0'           $TESVD
008700980828E   1C                   ENDIF
008800980828     C                   EXFMT     ST10D01
008900980828     C                   EXSR      SETVD1
009000980828     C* F03-FINE
009100980828IF  1C     *INKC         IFEQ      *ON
009200980828     C                   EXSR      F03GES
009300980828     C                   GOTO      FINVD1
009400980828E   1C                   ENDIF
009500980828     C* ENTER-CONTROLLA VIDEO
009600980828     C                   EXSR      CTRVD1
009700980828IF  1C     *IN90         IFEQ      *ON                                          *ERRORI
009800980828     C                   GOTO      FINVD1
009900980828E   1C                   ENDIF
010000980828     C* F06-CONFERMA
010100980828IF  1C     *INKF         IFEQ      *ON
010200980828     C                   EXSR      F06GES
010300980828     C                   GOTO      FINVD1
010400980828E   1C                   ENDIF
010500980828     C*
010600980828     C     FINVD1        ENDSR
010700980828     C*------------------------------------------------------------------------*
010800980828     C* CTRVD1 - CONTROLLA PRIMA VIDEATA
010900980828     C*------------------------------------------------------------------------*
011000980828     C     CTRVD1        BEGSR
011100980828     C*
011200980828     C* REIMPOSTA LE VARIABILI DI LAVORO
011300980828     C                   MOVEL     '0'           WERR
011400980828     C*---
011500980828     C* ANNO/MESE INIZIALE
011600980828     C*---
011700980828     C                   MOVEL     V1CAMI        DA8W
011800980828     C                   MOVE      01            DA8W
011900980828     C                   EXSR      CHKDAT
012000980828     C                   MOVEL     DA8W          V1CAMI
012100980828IF  1C     WERR          IFEQ      '1'
012200980828     C                   SETON                                        2090
012300980828     C                   MOVEL     MSG(1)        $MSG
012400980828     C                   GOTO      FINCT1
012500980828E   1C                   ENDIF
012600980828     C*
012700980828IF  1C     V1CAMI        IFLT      MINAMI
012800980828     C                   SETON                                        2090
012900980828     C                   MOVEL     MSG(3)        $MSG
013000980828     C                   GOTO      FINCT1
013100980828E   1C                   ENDIF
013200051010     c*
013300051010     c                   movel     v1cami        wanno             4 0
013400051010     c                   if        wanno<ansac0
013500051010     C                   SETON                                        2090
013600051010     C                   MOVEL     MSG(6)        $MSG
013700051010     C                   GOTO      FINCT1
013800051010E   1C                   ENDIF
013900980828     C*---
014000980828     C* ANNO/MESE FINALE
014100980828     C*---
014200980828     C                   MOVEL     V1CAMF        DA8W
014300980828     C                   MOVE      01            DA8W
014400980828     C                   EXSR      CHKDAT
014500980828     C                   MOVEL     DA8W          V1CAMF
014600980828IF  1C     WERR          IFEQ      '1'
014700980828     C                   SETON                                        2190
014800980828     C                   MOVEL     MSG(1)        $MSG
014900980828     C                   GOTO      FINCT1
015000980828E   1C                   ENDIF
015100980828     C*
015200981007IF  1C     V1CAMF        IFGT      MAXAMF
015300980828     C                   SETON                                        2190
015400980828     C                   MOVEL     MSG(4)        $MSG
015500980828     C                   GOTO      FINCT1
015600980828E   1C                   ENDIF
015700980828     C*---
015800980828     C* ANNO/MESE INIZIALE + FINALE
015900980828     C*---
016000980828IF  1C     V1CAMF        IFLT      V1CAMI
016100980828     C                   SETON                                        202190
016200980828     C                   MOVEL     MSG(2)        $MSG
016300980828     C                   GOTO      FINCT1
016400980828E   1C                   ENDIF
016500980901     C*
016600980901     C     V1CAMF        SUB       V1CAMI        N4                4 0
016700980901IF  1C     N4            IFGE      100
016800980901     C                   SETON                                        202190
016900980901     C                   MOVEL     MSG(5)        $MSG
017000980901     C                   GOTO      FINCT1
017100980901E   1C                   ENDIF
017200980828     C*
017300980828     C     FINCT1        ENDSR
017400980828     C*------------------------------------------------------------------------*
017500980828     C* CHKDAT - CONTROLLA LA DATA
017600980828     C*------------------------------------------------------------------------*
017700980828     C     CHKDAT        BEGSR
017800980828     C*
017900980828     C     *LIKE         DEFINE    G08DAT        WDA8
018000980828     C     *LIKE         DEFINE    G08DAT        DA8W
018100980828     C*
018200980828     C                   Z-ADD     DA8W          G08INV
018300980828     C                   Z-ADD     *ZEROS        G08DAT
018400980828     C                   MOVEL     '3'           G08ERR
018500980828     C                   CALL      'XSRDA8'
018600980828     C                   PARM                    WLBDA8
018700980828     C                   Z-ADD     G08INV        DA8W
018800980828     C                   Z-ADD     G08DAT        WDA8
018900980828IF  1C     G08ERR        IFEQ      '1'
019000980828     C                   MOVEL     '1'           WERR
019100980828E   1C                   ENDIF
019200980828     C*
019300980828     C                   ENDSR
019400980828     C*------------------------------------------------------------------------*
019500980828     C* F03GES - GESTIONE TASTO FUNZIONALE F03 ==> FINE
019600980828     C*------------------------------------------------------------------------*
019700980828     C     F03GES        BEGSR
019800980828     C*
019900980828     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
020000980828     C*
020100980828     C                   ENDSR
020200980828     C*------------------------------------------------------------------------*
020300980828     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA
020400980828     C*------------------------------------------------------------------------*
020500980828     C     F06GES        BEGSR
020600980828     C*
020700980828     C* LANCIA IL PROGRAMMA
020800980828     C                   Z-ADD     V1CAMI        PARAMI
020900980828     C                   Z-ADD     V1CAMF        PARAMF
021000980828     C                   MOVEL     'ST11'        KCOAZ
021100980828     C                   MOVEL     PARAM         KPJBU
021200020409     C                   CALL      'BCH10'
021300020409 MOM C*                    CALL 'TIST11C'
021400980828     C                   PARM                    KPJBA
021500981007     C                   MOVEL     'S'           $FINE                          *FINE PROGRAMMA
021600980828     C*
021700980828     C                   ENDSR
021800980828     C*------------------------------------------------------------------------*
021900980828     C* CARTAB - CARICA LE TABELLE OCCORRENTI
022000980828     C*------------------------------------------------------------------------*
022100980828     C     CARTAB        BEGSR
022200980828     C*---
022300980828     C* DATE AVANZAMENTO TRASFERIMENTI
022400980828     C*---
022500980828     C     *LIKE         DEFINE    V1CAMI        MINAMI
022600980828     C     *LIKE         DEFINE    V1CAMF        MAXAMF
022700980828     C*
022800980828     C                   CLEAR                   DS$3
022900020906     C                   Z-ADD     1             KTBKUT
023000980828     C                   MOVEL     '$3'          KTBCOD
023100980828     C     KE2TAB        CHAIN     TABEL00F                           99
023200980828IF  1C     *IN99         IFEQ      *OFF
023300980828     C                   MOVEL     TBLUNI        DS$3
023400000519     C                   MOVEL     §$3D10        MINAMI
023500000519     C                   MOVEL     §$3D00        MAXAMF
023600980828E   1C                   ENDIF
023700980828     C*
023800980828     C                   ENDSR
023900980828     C*------------------------------------------------------------------------*
024000980828     C* IMPVD0 - IMPOSTAZIONE TESTATA
024100980828     C*------------------------------------------------------------------------*
024200980828     C     IMPVD0        BEGSR
024300980828     C*
024400980828     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
024500980828     C*--------------------
024600981117     C* PERIODO -> ANNO/MESE CORRENTE - 1 MESE
024700980828     C*--------------------
024800981111     C                   MOVE      UYEAR         AAAA              4 0
024900981111IF  1C     AAAA          IFGT      60
025000981111     C                   MOVEL     19            AAAA
025100981111X   1C                   ELSE
025200981111     C                   MOVEL     20            AAAA
025300981111E   1C                   ENDIF
025400981111     C                   MOVEL     UMONTH        MMMM              2 0
025500981117     C                   SUB       1             MMMM
025600981117IF  1C     MMMM          IFEQ      *ZEROS                                       *SE GENNAIO
025700981117     C                   Z-ADD     12            MMMM                           *IMPOSTA DICEMBRE
025800981117     C                   SUB       1             AAAA                           *DELL'ANNO PRECEDENT
025900981117E   1C                   ENDIF
026000981111     C                   MOVEL     AAAA          V1CAMI
026100981111     C                   MOVE      MMMM          V1CAMI
026200981111     C                   Z-ADD     V1CAMI        V1CAMF
026300981109     C*--------------------
026400981109     C* D A T A  F I N E --> ANNO/MESE CORRENTE - 1 MESE
026500981109     C*--------------------
026600981111     C*---                 MOVE UYEAR     AAAA    40
026700981111IF  1C*---       AAAA      IFGT 60
026800981111     C*---                 MOVEL19        AAAA
026900981111X   1C*---                 ELSE
027000981111     C*---                 MOVEL20        AAAA
027100981111E   1C*---                 ENDIF
027200981111     C*---                 MOVELUMONTH    MMMM    20
027300981111     C*---                 SUB  1         MMMM
027400981111IF  1C*---       MMMM      IFEQ *ZEROS                     *SE GENNAIO
027500981111     C*---                 Z-ADD12        MMMM             *IMPOSTA DICEMBRE
027600981111     C*---                 SUB  1         AAAA             *DELL'ANNO PRECEDENTE
027700981111E   1C*---                 ENDIF
027800981111     C*---                 MOVELAAAA      V1CAMF
027900981111     C*---                 MOVE MMMM      V1CAMF
028000981109     C*--------------------
028100981111     C*-D-A T A  I N I Z I O --> INIIZO DELL'ANNO
028200981109     C*--------------------
028300981111     C*---                 Z-ADDV1CAMF    V1CAMI
028400981111     C*---                 MOVE 01        V1CAMI
028500981111     C*
028600980828     C                   ENDSR
028700980828     C*------------------------------------------------------------------------*
028800980828     C* *INZSR - OPERAZIONI INIZIALI
028900980828     C*------------------------------------------------------------------------*
029000980828     C     *INZSR        BEGSR
029100980828     C*--------------------
029200980828     C* RICEVIMENTO PARAMETRI
029300980828     C*--------------------
029400980828     C     *ENTRY        PLIST
029500980828     C                   PARM                    KPJBA
029600980828     C*--------------------
029700980828     C* VARIABILI RIFERITE AL DATABASE
029800980828     C*--------------------
029900980828     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
030000980828     C     *LIKE         DEFINE    TBLCOD        KTBCOD
030100051010     C     *LIKE         DEFINE    TBLkey        KTBkey
030200980828     C*--------------------
030300980828     C* CHIAVI DI LETTURA
030400980828     C*--------------------
030500980828     C     KE2TAB        KLIST                                                  *TABEL00F
030600980828     C                   KFLD                    KTBKUT                          -UTENTE
030700980828     C                   KFLD                    KTBCOD                          -TABELLA
030800051010     C     KTAB          KLIST
030900051010     C                   KFLD                    ktbkut
031000051010     C                   KFLD                    ktbcod
031100051010     C                   KFLD                    ktbKEY
031200051010      *
031300051010      * RECUPERO DA TABELLA 5A I PARAMETRI DI PULIZIA
031400051010      *
031500051010     C                   Z-ADD     1             KTBKUT
031600051010     C                   MOVEL     '5A'          ktbCOD
031700051010     C                   MOVEL     'SEDE1   '    ktbKEY
031800051010     C     KTAB          CHAIN     TABEL00F                           30
031900051010     C  N30              MOVEL     TBLUNI        DS5AS1
032000051010     C   30              CLEAR                   DS5AS1
032100051010      *
032200051010     C     *YEAR         SUB       §5AS12        ANSAC0            4 0
032300980828     C*--------------------
032400980828     C* VARIABILI NON RIFERITE AL DATABASE
032500980828     C*--------------------
032600980828     C                   MOVEL     'N'           $FINE             1            *FINE PROGRAMMA
032700980828     C                   MOVEL     '0'           WERR              1            *ERRORE
032800980828     C                   MOVEL     '1'           $TESVD            1            *RIEMETTERE TESTATA
032900980828     C                   MOVEL     '1'           TIPVID            1            *VIDEO DA GESTIRE
033000980828     C*
033100980828     C* IMPOSTA LA DATA CORRENTE
033200980828     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
033300980828     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
033400980828     C                   Z-ADD     N8            G08DAT
033500980828     C                   Z-ADD     *ZEROS        G08INV
033600980828     C                   MOVEL     '0'           G08ERR
033700980828     C                   CALL      'XSRDA8'
033800980828     C                   PARM                    WLBDA8
033900980828     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
034000980828     C*
034100980828     C* CARICA LE TABELLE OCCORRENTI
034200980828     C                   EXSR      CARTAB
034300980828     C*
034400980828     C* IMPOSTAZIONI TESTATA
034500980828     C                   EXSR      IMPVD0
034600980828     C*
034700980828     C                   ENDSR
034800980828     O*------------------------------------------------------------------------*
034900980901** -MSG-
035000980828TIST10R- Data errata                                                          1
035100980828TIST10R- La data iniziale non può essere maggiore della data finale           2
035200980828TIST10R- A questa data non ci sono più le bolle in archivio                   3
035300980828TIST10R- A questa data non ci sono ancora le bolle in archivio                4
035400980901TIST10R- Impossibile richiedere più di 12 mesi                                5
035500051010TIST10R-A questa data non ci sono più i saldi statistiche Attività commerciale6
035600980828TIST10R- 7
035700980828TIST10R- 8
035800980901TIST10R- 9
035900980901TIST10R- Non si possono lanciare le statistiche di gruppo in SDI !!!          10

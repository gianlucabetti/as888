000100940328     FTABEL00F  IF   E           K DISK    USROPN
000200020612     FTITASQUE  UF   E           K DISK
000300950901     D*****************************************************************
000400940328     D D$PREC          S              1    DIM(500)
000500940328     D D$DREC          S              1    DIM(500)
000600940328     D*****************************************************************
000700940328     D* FILE E RELATIVI PGM DI GESTIONE
000800940328     D FIL             S             10    DIM(20)
000900940328     D PGM             S             10    DIM(20)
001000020528     D SDA             S              1    DIM(20)
001100020528     D SDC             S              1    DIM(20)
001200020528     D SDF             S              1    DIM(20)
001300020528     D SDO             S              1    DIM(20)
001400020528     D SDP             S              1    DIM(20)
001500020528     D SDR             S              1    DIM(20)
001600020528     D SDT             S              1    DIM(20)
001601040316     D SDE             S              1    DIM(20)
001700020528     D CET             S              1    DIM(20)
001800020528     D MOV             S              1    DIM(20)
001900020531     D FIM             S              1    DIM(20)
002000020531     D FCE             S              1    DIM(20)
002100021223     D COP             S              1    DIM(20)
002200950125     D*****************************************************************
002300950125     D* SCHIERE CODICI E IMPORTI IN INPUT E OUTPUT AL PGM
002400030505     D CAN             S              2    DIM(75)
002500030505     D IAN             S              7    DIM(75)
002600991214     D*****************************************************************
002700991214     D* SCHIERA PROGRAMMI CHIAMATI
002800991214     D PGS             S             10    DIM(20)
002900950125     D*****************************************************************
003000941129     D DS£ECO        E DS
003100941129     D DS$ECO        E DS
003200020528     D DS2V          E DS
003300931029     D PARAM           DS
003400941129     D  DSECO                  1   1100
003500941129     D                                     DIM(1100)
003600950125     D*****************************************************************
003700950125     D* DS PER PASSAGGIO CAMPI PACKED -CODICE-
003800950125     D                 DS
003900950125     D  DSWCN                  1      3P 0
004000950125     D  DSWCA                  1      3
004100011213     D  DSWIN                  4     13P 4
004200011213     D  DSWIA                  4     13
004300970217     D*****************************************************************
004400990804     D* DS ESTERNA PER RIDEFINIRE IMM. PRIMA E DOPO DI TITAS10F + BOLLA ORIG.
004500970217     D TRECDS        E DS
004600970217     D  D$PDS1                 1    250
004700970217     D  D$PDS2               251    500
004800970217     D  D$DDS1               501    750
004900970217     D  D$DDS2               751   1000
005000970217     D  D$ODS1              1001   1250
005100021223     D  D$ODS2              1251   1500
005200950201     D*****************************************************************
005300950201     D* INFDS PER CONTROLLO STATO DEL PGM
005400950201     D                SDS
005500950201     D  ERRTYP                40     42
005600950201     D  ERRNUM                43     46
005700940328     I*****************************************************************
005800930617     C     *ENTRY        PLIST
005900020612     C                   PARM                    FILE             10
006000930617     C                   PARM                    LIB              10
006100020617     C                   PARM                    CHIUDI            1
006200940328     C*
006300940328     C* CHIAVE LETTURA TABEL00F - COMPLETA
006400940328     C     KEYTAB        KLIST
006500940328     C                   KFLD                    TBLKUT
006600940328     C                   KFLD                    TBLCOD
006700940328     C                   KFLD                    TBLKEY
006800941026     C*
006900991214     C* AZZERAMENTI
007000991214     C                   Z-ADD     *ZEROS        D$GPRE
007100991214     C                   Z-ADD     *ZEROS        NPGS
007200950125     C*
007300950125     C* AZZERA SCHIERE COMPETENZE/RICAVI/VOCI
007400950125     C                   EXSR      AZZSCH
007500930709     C*
007600940328     C* CARICA TABELLE
007700940328     C                   EXSR      CARTAB
007800020612     C*
007900020612     C* AZZERO INDICATORE CONDIZIONANTE IL CICLO DI REPERIMENTO
008000020612     C                   MOVEL     '0'           *IN97
008100020612     C                   MOVEL     '0'           *IN98
008200940328     C*
008300940328     C* RICEVE RECORD DA DATAQ
008400020612     C                   EXSR      RCVFILE
008500020612     C*
008600940328     C* LANCIA PGM DI AGG. SALDI E RICEVE ALTRO RECORD
008700940328     C     D$GSTS        DOWNE     '§'
008800020612     C     *IN97         ANDEQ     '0'
008900020612     C     *IN98         ANDEQ     '0'
009000950901     C*
009100940505     C     DAREG         IFEQ      'S'
009200950901     C*
009300991214     C* LANCIA PGM
009400940328     C                   CALL      D$GPGS
009500940805     C                   PARM                    DS$ECO
009600971229     C                   PARM                    DS£ECO
009700950124     C                   MOVEL     D$GSTS        WPRSTS            1
009800991214     C                   EXSR      MEMPGS
009900940505     C                   ENDIF
010000020612     C*
010100020612     C* PRIMA DEL PROSSIMO REPERIMENTO EFETTUO LA DELETE DEL RECORD GIA' ELAB.
010200020617     C* TRANNE CHE X IMMAGINI CON FLAG SU TITASQUE <> *BLANKS
010300020617     C                   IF        WQUEFLG = *BLANKS
010400020612     C                   DELETE    TITASQ00
010500020617     C                   ENDIF
010600020612     C*
010700950117     C* CONTROLLA SE E' STATA RICHIESTA LA CHIUSURA DEL SOTTOSISTEMA
010800950117     C                   SHTDN                                        98
010900991214     C     *IN98         IFEQ      '0'
011000020612     C                   EXSR      RCVFILE
011100991214     C                   ENDIF
011200930617     C                   ENDDO
011300991214     C*
011400991214     C* RILANCIA TUTTI I PGM CHIAMATI PER FINIRE OPERAZIONI IN SOSPESO
011500991214     C* E PER CHIUDERE I FILE
011600020703     C                   MOVEL     D$GPGF        DEPPGF           10
011700991214     C     *IN98         IFEQ      '1'
011800991214     C                   MOVEL     'S'           D$GSTS
011900020617     C                   MOVEL     'S'           CHIUDI
012000991214     C                   ELSE
012100991214     C                   MOVEL     'C'           D$GSTS
012200020703     C                   MOVEL     'TRECD4R'     D$GPGF
012300991214     C                   ENDIF
012400991214     C     1             DO        NPGS          I
012500991214     C                   CALL      PGS(I)
012600991214     C                   PARM                    DS$ECO
012700991214     C                   PARM                    DS£ECO
012800991214     C                   ENDDO
012900020703     C                   MOVEL     DEPPGF        D$GPGF
013000940328     C*
013100930617     C                   SETON                                        LR
013200991214     C*-------------------------------------------------------------------
013300991214     C*? MEMPGS - MEMORIZZA TUTTI I PGM CHIAMATI
013400991214     C*-------------------------------------------------------------------
013500991214     C     MEMPGS        BEGSR
013600991214     C*
013700991214     C                   Z-ADD     1             I
013800991214     C     D$GPGS        LOOKUP    PGS(I)                                 99
013900991214     C     *IN99         IFEQ      '0'
014000991214     C                   ADD       1             NPGS              6 0
014100991214     C                   Z-ADD     NPGS          I
014200991214     C                   MOVEL     D$GPGS        PGS(I)
014300991214     C                   ENDIF
014400991214     C*
014500991214     C                   ENDSR
014600940328     C*-------------------------------------------------------------------
014700020612     C*? RCVFILE - LEGGE RECORD DAL FILE TITASQUE
014800940328     C*-------------------------------------------------------------------
014900020612     C     RCVFILE       BEGSR
015000940505     C*
015100940505     C* IMPOSTA SEMPRE CHE IL RECORD E' DA REGISTRARE
015200940505     C                   MOVEL     'S'           DAREG             1
015300020617     C                   MOVEL     *BLANKS       WQUEFLG           1
015400020617     C*
015500020617     C* INIZIALIZZO A NON AGGIORNARE I FLAGS DELLA TABELLA "2V"
015600020617     C                   MOVEL     *BLANKS       D$GPGS
015700020617     C                   MOVEL     'N'           D$GSDA
015800020617     C                   MOVEL     'N'           D$GSDC
015900020617     C                   MOVEL     'N'           D$GSDF
016000020617     C                   MOVEL     'N'           D$GSDO
016100020617     C                   MOVEL     'N'           D$GSDP
016200020617     C                   MOVEL     'N'           D$GSDR
016300020617     C                   MOVEL     'N'           D$GSDT
016301040316     C                   MOVEL     'N'           D$GSDE
016400020617     C                   MOVEL     'N'           D$GCET
016500020617     C                   MOVEL     'N'           D$GWFM
016600020617     C                   MOVEL     'N'           D$GFIM
016700020617     C                   MOVEL     'N'           D$GFCE
016800021223     C                   MOVEL     'N'           D$GCOP
016900020612     C*
017000020612     C* LEGGE RECORD DAL FILE TITASQUE
017100020612     C                   READ      TITASQUE
017200020614     C                   IF        %EOF(TITASQUE)
017300020612     C                   SETON                                        97
017400020612     C                   ELSE
017500020617     C*
017600020617     C* TESTO SE IL FLAG DEL FILE TITASQUE E' A *BLANKS, OVVERO DA CONSIDERARE
017700020617     C                   IF        QUEFLG = *BLANKS
017800020617     C                   EVAL      PARAM = QUEDTA
017900020617     C                   ELSE
018000020617     C                   MOVEL     'N'           DAREG
018100020617     C                   MOVEL     QUEFLG        WQUEFLG
018200020612     C                   ENDIF
018300020617     C                   ENDIF
018400020612     C*
018500020614     C* SE LETTO RECORD OK PROCEDO CON L'ELABORAZIONE
018600020617     C                   IF        *IN97 = *OFF AND
018700020617     C                             DAREG = 'S'
018800940405     C*
018900940405     C* PORTA I PARAMETRI RICEVUTI NELLA DS DEI SALDI
019000941129     C                   MOVEA     DSECO         DS£ECO
019100020607     C*
019200020607     C* imposta i flag di aggiornamento files dalla tabella 2V
019300020607     C                   Z-ADD     1             I
019400020607     C     D£GFIL        LOOKUP    FIL(I)                                 99
019500020607     C     *IN99         IFEQ      '1'
019600020607     C                   MOVEL     PGM(I)        D$GPGS
019700020607     C                   MOVEL     SDA(I)        D$GSDA
019800020607     C                   MOVEL     SDC(I)        D$GSDC
019900020607     C                   MOVEL     SDF(I)        D$GSDF
020000020607     C                   MOVEL     SDO(I)        D$GSDO
020100020607     C                   MOVEL     SDP(I)        D$GSDP
020200020607     C                   MOVEL     SDR(I)        D$GSDR
020300020607     C                   MOVEL     SDT(I)        D$GSDT
020301040316     C                   MOVEL     SDE(I)        D$GSDE
020400020607     C                   MOVEL     CET(I)        D$GCET
020500020607     C                   MOVEL     MOV(I)        D$GWFM
020600020607     C                   MOVEL     FIM(I)        D$GFIM
020700020607     C                   MOVEL     FCE(I)        D$GFCE
020800021223     C                   MOVEL     COP(I)        D$GCOP
020900020607     C                   ENDIF
021000000302     C*=========================================================================
021100000302     C*=========================================================================
021200000302     C*
021300000302     C*           R   I   C    A    L   C   O   L    O
021400000327     C*
021500000327     C* NB: PER RICALCOLARE COME INSERIMENTO :
021600000327     C*     USARE LE SPECIFICHE "RILCE" E ASTERISCARE LA SPECIFICA "RILCX"
021700000327     C*     ALLA FINE DEL RICALCOLO ASTERISCARE LE SPECIFICHE "RILCE" E
021800000327     C*     TOGLIERE L'ASTERISCO DALLA "RILCX" IN QUESTA ROUTINE
021900000327     C*
022000000302     C*=========================================================================
022100000302     C*=========================================================================
022200000302     C*
022300970217     C* SE MODIFICA DEL PGM DI RICALCOLO C.E.
022400011213RILCEC*--- D£GPGF        IFEQ      'TRECE9R'
022500011213RILCEC*---               MOVEL     D£PRE1        D$PDS1
022600011213RILCEC*---               MOVEL     D£PRE2        D$PDS2
022700011213RILCEC*---               MOVEL     D£DRE1        D$DDS1
022800011213RILCEC*---               MOVEL     D£DRE2        D$DDS2
022900030303RILCEC*--- D$DLL2        IFEQ      'S'
023000011213RILCEC*---               EXSR      APDTAQ
023100011213RILCEC*---               MOVEL     D$PDS1        D£PRE1
023200011213RILCEC*---               MOVEL     D$PDS2        D£PRE2
023300011213RILCEC*---               MOVEL     'I'           D£GSTS
023400020528RILCEC*--- D£GFIL        IFEQ      'TITAS10F'
023500011213RILCEC*---               MOVEL     'S'           D$GSDA
023600011213RILCEC*---               MOVEL     'S'           D$GSDC
023700011213RILCEC*---               MOVEL     'S'           D$GSDF
023800011213RILCEC*---               MOVEL     'S'           D$GSDO
023900011213RILCEC*---               MOVEL     'S'           D$GSDP
024000011213RILCEC*---               MOVEL     'S'           D$GSDR
024100011213RILCEC*---               MOVEL     'S'           D$GSDT
024101040316RILCEC*---               MOVEL     'S'           D$GSDE
024200011213RILCEC*---               MOVEL     'S'           D$GCET
024300011213RILCEC*---               MOVEL     'S'           D$GWFM
024400020531RILCEC*---               MOVEL     'S'           D$GFIM
024500020531RILCEC*---               MOVEL     'S'           D$GFCE
024600021223RILCEC*---               MOVEL     'S'           D$GCOP
024700020528RILCEC*---               ENDIF
024800020528RILCEC*--- D£GFIL        IFEQ      'TITAS00F'
024900020528RILCEC*---               MOVEL     'N'           D$GSDA
025000020527RILCEC*---               MOVEL     'N'           D$GSDC
025100020527RILCEC*---               MOVEL     'N'           D$GSDF
025200020527RILCEC*---               MOVEL     'N'           D$GSDO
025300020527RILCEC*---               MOVEL     'N'           D$GSDP
025400020527RILCEC*---               MOVEL     'N'           D$GSDR
025500020527RILCEC*---               MOVEL     'N'           D$GSDT
025501040316RILCEC*---               MOVEL     'N'           D$GSDE
025600020527RILCEC*---               MOVEL     'S'           D$GCET
025700020528RILCEC*---               MOVEL     'N'           D$GWFM
025800020531RILCEC*---               MOVEL     'N'           D$GFIM
025900020531RILCEC*---               MOVEL     'S'           D$GFCE
026000021223RILCEC*---               MOVEL     'S'           D$GCOP
026100020528RILCEC*---               ENDIF
026200020528RILCEC*
026300011213RILCEC*---               ENDIF
026400011213RILCEC*---               ENDIF
026500000302     C*=========================================================================
026600000302     C*=========================================================================
026700000302     C*=========================================================================
026800000302     C*=========================================================================
026900941129     C*
027000941129     C* IMPOSTA I CAMPI DELLA DS DI GESTIONE DEL C/ECON.
027100941129     C                   MOVEL     D£GSTS        D$GSTS
027200950123     C                   MOVEL     D£GSTS        D$GSTO
027300941129     C                   Z-ADD     D£GDAT        D$GDAT
027400941129     C                   Z-ADD     D£GTIM        D$GTIM
027500941129     C                   MOVEL     D£GUSR        D$GUSR
027600941129     C                   MOVEL     D£GPGF        D$GPGF
027700941129     C                   MOVEL     D£GFIL        D$GFIL
027800941129     C                   MOVEL     D£GMBR        D$GMBR
027900941129     C                   MOVEL     D£GLIB        D$GLIB
028000941129     C                   MOVEL     *BLANKS       D$GERR
028100941129     C                   MOVEL     *BLANKS       D$GMSG
028200950119     C                   MOVEL     D£GPDE        D$GPDE
028300021223     C                   MOVEL     D£GTRD        D$GTRD
028400941129     C                   MOVEL     *BLANKS       D$GXXX
028500950119     C                   MOVEL     D£PRE1        D$PRE1
028600950119     C                   MOVEL     D£PRE2        D$PRE2
028700950119     C                   MOVEL     D£DRE1        D$DRE1
028800950119     C                   MOVEL     D£DRE2        D$DRE2
028900950123     C     D£GFIL        IFEQ      'TNCSB00F'
029000950123     C                   MOVEL     D£PRE1        D$PCO1
029100950123     C                   MOVEL     D£PRE2        D$PCO2
029200950123     C                   MOVEL     D£DRE1        D$DCO1
029300950123     C                   MOVEL     D£DRE2        D$DCO2
029400950123     C                   ENDIF
029500030801     C     D£GFIL        IFEQ      'FIAR500F'
029600021223IF  2C                   IF        D£GTRD = 'TRS'
029700021223     C                   MOVEL     D£PRE1        D$PAR5T
029800021223     C                   MOVEL     D£DRE1        D$DAR5T
029900021223E   2C                   ENDIF
030000030801IF  2C                   IF        D£GTRD = 'BAN' OR
030100030801     C                             D£GTRD = 'BNB'
030200021223     C                   MOVEL     D£PRE1        D$PAR5B
030300021223     C                   MOVEL     D£DRE1        D$DAR5B
030400021223E   2C                   ENDIF
030500021223     C                   ENDIF
030600950125     C                   MOVEA     CAN           D$DCCD
030700950125     C                   MOVEA     IAN           D$DICD
030800950125     C                   MOVEA     CAN           D$DCVD
030900950125     C                   MOVEA     CAN           D$DPVD
031000950125     C                   MOVEA     IAN           D$DIVD
031100950125     C*
031200950125     C* SE MODIFICA DI UN PGM DI AGG. SALDI -> NON CONSIDERO
031300950125     C* AGG. SALDI NON AGGIORNANO FILE.
031400950125     C     D$GPGF        IFEQ      'TRECD6R'
031500950125     C                   MOVEL     'N'           DAREG
031600950125     C                   ENDIF
031700950315     C*
031800990804     C* SE E' LA MODIFICA DEL PGM TNSB36R (TITAS10->TITASP), NON CONSIDERO
031900950315     C     D$GPGF        IFEQ      'TNSB36R'
032000020702     C     D$GPGF        OREQ      'TNSF02C'
032100020702     C     D$GPGF        OREQ      'TNSF04C'
032200011213     C*----D$GPGF        OREQ      'TRECE9R'
032300011213     C*--- D$GPGF        OREQ      'XXXXXXX'
032400950315     C                   MOVEL     'N'           DAREG
032500950315     C                   ENDIF
032600030515     C*
032700030515     C* IL TRIGGER SU FIAR500F FA PASSARE (NONOSTANTE CI SIANO DEGLI "IF" CHE LO
032800030729     C* IMPEDISCANO) DEI TIPI RECORD <> DA "TRS", "BAN", "BNB" (????????);
032900030515     C* PER EVITARE ERRORI LI BLOCCO ANCHE QUI (PEZZA).
033000030801     C*
033100030801IF  1C                   IF        D£GFIL = 'FIAR500F'
033200030801IF  2C                   IF        D£GTRD = 'TRS' OR
033300030801     C                             D£GTRD = 'BAN' OR
033400030801     C                             D£GTRD = 'BNB'
033500030515X   2C                   ELSE
033600030515     C                   MOVEL     'N'           DAREG
033700030515E   2C                   ENDIF
033800030515E   1C                   ENDIF
033900950103     C*
034000950103     C* SE LA STATO PRECEDENTE ERA 'C' (CHIUSURA FILE) E ANCHE LO STATO ATTUALE
034100950103     C* E' 'C' NON LANCIO PGM DI REGISTRAZIONE.
034200950103     C* COSI' NON APRE E CHUIDE I FILE
034300950331     C* SE PGM DIVERSO DA TRASFERIMENTO BOLLE TAS00 > TAS10
034400950331     C     D$GPGF        IFEQ      'TNSB23R'
034500950331     C                   ELSE
034600950103     C     WPRSTS        IFEQ      'C'
034700950103     C     D$GSTS        ANDEQ     'C'
034800950103     C                   MOVEL     'N'           DAREG
034900950217     C                   ENDIF
035000950331     C                   ENDIF
035100950103     C* MEMORIZZO LO STATO ATTUALE COME STATO PRECEDENTE
035200011213     C*---               MOVEL     D$GSTS        WPRSTS            1
035300991214     C*
035400991214     C* SE IL PROGRAMMA DA CHIAMARE E' UGUALE A SPAZIO NON LANCIO NIENTE
035500991214     C     D$GPGS        IFEQ      *BLANKS
035600991214     C                   MOVEL     'N'           DAREG
035700991214     C                   ENDIF
035800940328     C*
035900020612     C                   ENDIF
036000020612     C*
036100930617     C                   ENDSR
036200950125     C*-------------------------------------------------------------------
036300990804     C*? AZZSCH - AZZERA LE SCHIERE PACKED COMPET/RICAVI/VOCI
036400950125     C*-------------------------------------------------------------------
036500950125     C     AZZSCH        BEGSR
036600950125     C*
036700030505     C     1             DO        75            I
036800950125     C                   Z-ADD     *ZEROS        DSWCN
036900950125     C                   MOVE      DSWCA         CAN(I)
037000950125     C                   Z-ADD     *ZEROS        DSWIN
037100950125     C                   MOVE      DSWIA         IAN(I)
037200950125     C                   ENDDO
037300950125     C                   MOVEA     CAN           D$DCCD
037400950125     C                   MOVEA     IAN           D$DICD
037500950125     C                   MOVEA     CAN           D$DCVD
037600950125     C                   MOVEA     CAN           D$DPVD
037700950125     C                   MOVEA     IAN           D$DIVD
037800950125     C*
037900950125     C                   ENDSR
038000940328     C*-------------------------------------------------------------------
038100940328     C*? CARTAB - CARICA TABELLE
038200940328     C*-------------------------------------------------------------------
038300940328     C     CARTAB        BEGSR
038400940328     C*
038500940328     C                   OPEN      TABEL00F
038600940328     C*
038700020528     C* CARICA TABELLA DATI VARI CONTO ECONOMICO
038800940328     C                   Z-ADD     0             I                 4 0
038900940328     C                   Z-ADD     1             TBLKUT
039000940805     C                   MOVEL     '2V'          TBLCOD
039100940328     C                   MOVEL     *BLANKS       TBLKEY
039200940328     C     KEYTAB        SETLL     TABEL00F                           99
039300940328     C  N99              READ      TABEL00F                               99
039400940328     C     *IN99         DOWEQ     '0'
039500940805     C     TBLCOD        ANDEQ     '2V'
039600940328     C     TBLFLG        IFEQ      *BLANKS
039700940328     C                   ADD       1             I
039800020528     C                   MOVEL     TBLUNI        DS2V
039900940328     C                   MOVEL     TBLKEY        FIL(I)
040000020528     C                   MOVEL     §2VPGM        PGM(I)
040100020528     C                   MOVEL     §2VSDA        SDA(I)
040200020528     C                   MOVEL     §2VSDC        SDC(I)
040300020528     C                   MOVEL     §2VSDF        SDF(I)
040400020528     C                   MOVEL     §2VSDO        SDO(I)
040500020528     C                   MOVEL     §2VSDP        SDP(I)
040600020528     C                   MOVEL     §2VSDR        SDR(I)
040700020528     C                   MOVEL     §2VSDT        SDT(I)
040701040316     C                   MOVEL     §2VSDE        SDE(I)
040800020528     C                   MOVEL     §2VCET        CET(I)
040900020528     C                   MOVEL     §2VMOV        MOV(I)
041000020531     C                   MOVEL     §2VFIM        FIM(I)
041100020531     C                   MOVEL     §2VFCE        FCE(I)
041200021223     C                   MOVEL     §2VCOP        COP(I)
041300020528     C                   ENDIF
041400940328     C                   READ      TABEL00F                               99
041500940328     C                   ENDDO
041600940328     C*
041700940328     C                   CLOSE     TABEL00F
041800970219     C*
041900940328     C                   ENDSR
042000970219RILCEC*------------------------------------------------------------------------*
042100970219RILCEC*?APDTAQ - AZZERA TUTTI I CAMPI DELLA DS -PRIMA-
042200970219RILCEC*------------------------------------------------------------------------*
042300970505RILCEC     APDTAQ        BEGSR
042400970505RILCEC*
042500970505RILCEC                   Z-ADD     *ZEROS        D$PAAS
042600990804RILCEC                   Z-ADD     *ZEROS        D$PLNP
042700990804RILCEC                   Z-ADD     *ZEROS        D$PNRS
042800990804RILCEC                   Z-ADD     *ZEROS        D$PNSP
042900990804RILCEC                   MOVEL     *BLANKS       D$PTBL
043000990804RILCEC                   MOVEL     *BLANKS       D$PCBO
043100990804RILCEC                   Z-ADD     *ZEROS        D$PMGS
043200990804RILCEC                   Z-ADD     *ZEROS        D$PKSC
043300990804RILCEC                   Z-ADD     *ZEROS        D$PLNA
043400990804RILCEC                   Z-ADD     *ZEROS        D$PNCL
043500990804RILCEC                   Z-ADD     *ZEROS        D$PPKB
043600990804RILCEC                   MOVEL     *BLANKS       D$PFPB
043700990804RILCEC                   Z-ADD     *ZEROS        D$PPKC
043800990804RILCEC                   Z-ADD     *ZEROS        D$PNCP
043900990804RILCEC                   Z-ADD     *ZEROS        D$PPKF
044000990804RILCEC                   MOVEL     *BLANKS       D$PFPF
044100990804RILCEC                   Z-ADD     *ZEROS        D$PPVL
044200990804RILCEC                   MOVEL     *BLANKS       D$PFVB
044300990804RILCEC                   Z-ADD     *ZEROS        D$PVLB
044400990804RILCEC                   Z-ADD     *ZEROS        D$PVLC
044500990804RILCEC                   Z-ADD     *ZEROS        D$PNCR
044600990804RILCEC                   Z-ADD     *ZEROS        D$PVLF
044700990804RILCEC                   MOVEL     *BLANKS       D$PFVF
044800990804RILCEC                   Z-ADD     *ZEROS        D$PQFT
044900990804RILCEC                   Z-ADD     *ZEROS        D$PIAS
045000990804RILCEC                   MOVEL     *BLANKS       D$PVAS
045100990804RILCEC                   MOVEL     *BLANKS       D$PFCM
045200990804RILCEC                   Z-ADD     *ZEROS        D$PCTR
045300990804RILCEC                   MOVEL     *BLANKS       D$PTSP
045400990804RILCEC                   MOVEL     *BLANKS       D$PRSD
045500990804RILCEC                   MOVEL     *BLANKS       D$PIND
045600990804RILCEC                   MOVEL     *BLANKS       D$PCAD
045700990804RILCEC                   MOVEL     *BLANKS       D$PLOD
045800990804RILCEC                   MOVEL     *BLANKS       D$PPRD
045900990804RILCEC                   MOVEL     *BLANKS       D$PNZD
046000990804RILCEC                   MOVEL     *BLANKS       D$PCPD
046100990804RILCEC                   MOVEL     *BLANKS       D$PCTS
046200990804RILCEC                   MOVEL     *BLANKS       D$PFIN
046300990804RILCEC                   Z-ADD     *ZEROS        D$PCCM
046400990804RILCEC                   Z-ADD     *ZEROS        D$PRMN
046500990804RILCEC                   MOVEL     *BLANKS       D$PMCT
046600990804RILCEC                   MOVEL     *BLANKS       D$PFAP
046700990804RILCEC                   MOVEL     *BLANKS       D$PFTM
046800990804RILCEC                   MOVEL     *BLANKS       D$PNAS
046900990804RILCEC                   Z-ADD     *ZEROS        D$PFLB
047000990804RILCEC                   Z-ADD     *ZEROS        D$PFLP
047100990804RILCEC                   Z-ADD     *ZEROS        D$PFL2
047200990804RILCEC                   Z-ADD     *ZEROS        D$PTFA
047300990804RILCEC                   Z-ADD     *ZEROS        D$PTFP
047400990804RILCEC                   Z-ADD     *ZEROS        D$PDIM
047500990804RILCEC                   MOVEL     *BLANKS       D$PSOP
047600990804RILCEC                   Z-ADD     *ZEROS        D$PPDR
047700990804RILCEC                   Z-ADD     *ZEROS        D$PDRT
047800990804RILCEC                   Z-ADD     *ZEROS        D$PHRT
047900990804RILCEC                   Z-ADD     *ZEROS        D$PNRT
048000990804RILCEC                   MOVEL     *BLANKS       D$PFPP
048100990804RILCEC                   Z-ADD     *ZEROS        D$PSTR
048200990804RILCEC                   MOVEL     *BLANKS       D$PCTM
048300990804RILCEC                   MOVEL     *BLANKS       D$PFNS
048400990804RILCEC                   Z-ADD     *ZEROS        D$PFLS
048500990804RILCEC                   Z-ADD     *ZEROS        D$PNCD
048600990804RILCEC                   Z-ADD     *ZEROS        D$PNCA
048700990804RILCEC                   MOVEL     *BLANKS       D$PXCO
048800990804RILCEC                   Z-ADD     *ZEROS        D$PDBR
048900990804RILCEC                   Z-ADD     *ZEROS        D$PDUC
049000990804RILCEC                   Z-ADD     *ZEROS        D$PDTI
049100990804RILCEC                   Z-ADD     *ZEROS        D$PHTI
049200990804RILCEC                   Z-ADD     *ZEROS        D$PZNC
049300990804RILCEC                   Z-ADD     *ZEROS        D$PPDC
049400990804RILCEC                   Z-ADD     *ZEROS        D$PNDC
049500990804RILCEC                   Z-ADD     *ZEROS        D$PDDC
049600990804RILCEC                   Z-ADD     *ZEROS        D$PSTP
049700990804RILCEC                   Z-ADD     *ZEROS        D$PDCE
049800990804RILCEC                   Z-ADD     *ZEROS        D$PHCE
049900990804RILCEC                   Z-ADD     *ZEROS        D$PDCP
050000990804RILCEC                   Z-ADD     *ZEROS        D$PDCM
050100990804RILCEC                   Z-ADD     *ZEROS        D$PHMC
050200990804RILCEC                   MOVEL     *BLANKS       D$PCCA
050300990804RILCEC                   Z-ADD     *ZEROS        D$PNPI
050400990804RILCEC                   Z-ADD     *ZEROS        D$PNCI
050500990804RILCEC                   Z-ADD     *ZEROS        D$PNRC
050600990804RILCEC                   MOVEL     *BLANKS       D$PFDN
050700990804RILCEC                   MOVEL     *BLANKS       D$PFFD
050800990804RILCEC                   MOVEL     *BLANKS       D$PFTC
050900990804RILCEC                   MOVEL     *BLANKS       D$PTC2
051000990804RILCEC                   MOVEL     *BLANKS       D$PGC1
051100990804RILCEC                   MOVEL     *BLANKS       D$PGC2
051200990804RILCEC                   MOVEL     *BLANKS       D$PTCR
051300990804RILCEC                   Z-ADD     *ZEROS        D$PDCR
051400990804RILCEC                   Z-ADD     *ZEROS        D$PHCR
051500990804RILCEC                   MOVEL     *BLANKS       D$PFBR
051600990804RILCEC                   MOVEL     *BLANKS       D$PLL1
051700990804RILCEC                   MOVEL     *BLANKS       D$PLL2
051800990804RILCEC                   MOVEL     *BLANKS       D$PFA2
051900990804RILCEC                   MOVEL     *BLANKS       D$PFCA
052000990804RILCEC                   MOVEL     *BLANKS       D$PFA1
052100990804RILCEC                   MOVEL     *BLANKS       D$PFGC
052200990804RILCEC                   MOVEL     *BLANKS       D$PFDA
052300990804RILCEC                   Z-ADD     *ZEROS        D$PGUF
052400990804RILCEC                   MOVEL     *BLANKS       D$PFLO
052500990804RILCEC                   Z-ADD     *ZEROS        D$PGGS
052600990804RILCEC                   MOVEL     *BLANKS       D$PGGA
052700990804RILCEC                   MOVEL     *BLANKS       D$PGMA
052800990804RILCEC                   MOVEL     *BLANKS       D$PGVA
052900990804RILCEC                   MOVEL     *BLANKS       D$PNZM
053000990804RILCEC                   MOVEL     *BLANKS       D$PCAM
053100990804RILCEC                   MOVEL     *BLANKS       D$PDIV
053200990804RILCEC                   Z-ADD     *ZEROS        D$PPOR
053300990804RILCEC                   MOVEL     *BLANKS       D$PSV1
053400990804RILCEC                   Z-ADD     *ZEROS        D$PVA1
053500990804RILCEC                   MOVEL     *BLANKS       D$PSV2
053600990804RILCEC                   Z-ADD     *ZEROS        D$PVA2
053700990804RILCEC                   MOVEL     *BLANKS       D$PSV3
053800990804RILCEC                   Z-ADD     *ZEROS        D$PVA3
053900990804RILCEC                   Z-ADD     *ZEROS        D$PIMV
054000990804RILCEC                   Z-ADD     *ZEROS        D$PDFT
054100990804RILCEC                   Z-ADD     *ZEROS        D$PNFT
054200990804RILCEC                   Z-ADD     *ZEROS        D$PFIV
054300990804RILCEC                   MOVEL     *BLANKS       D$PFEI
054400990804RILCEC                   MOVEL     *BLANKS       D$PFTS
054500990804RILCEC                   MOVEL     *BLANKS       D$PFIC
054600990806RILCEC                   MOVEL     *BLANKS       D$PSCL
054700970505RILCEC*
054800970505RILCEC                   ENDSR

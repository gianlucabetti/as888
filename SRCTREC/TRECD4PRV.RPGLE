000100940328     FTABEL00F  IF   E           K DISK    USROPN
000200040130     Fecerr01l  IF   E           K DISK
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
001700040316     D SDE             S              1    DIM(20)
001800020528     D CET             S              1    DIM(20)
001900020528     D MOV             S              1    DIM(20)
002000020531     D FIM             S              1    DIM(20)
002100020531     D FCE             S              1    DIM(20)
002200021223     D COP             S              1    DIM(20)
002300950125     D*****************************************************************
002400950125     D* SCHIERE CODICI E IMPORTI IN INPUT E OUTPUT AL PGM
002500030505     D CAN             S              2    DIM(75)
002600030505     D IAN             S              7    DIM(75)
002700950125     D*****************************************************************
002800941129     D DS£ECO        E DS
002900941129     D DS$ECO        E DS
003000020528     D DS2V          E DS
003100931029     D PARAM           DS
003200941129     D  DSECO                  1   1100
003300941129     D                                     DIM(1100)
003400950125     D*****************************************************************
003500950125     D* DS PER PASSAGGIO CAMPI PACKED -CODICE-
003600950125     D                 DS
003700950125     D  DSWCN                  1      3P 0
003800950125     D  DSWCA                  1      3
003900090904     D  DSWIN                  4     13P 5
004000011213     D  DSWIA                  4     13
004100970217     D*****************************************************************
004200990804     D* DS ESTERNA PER RIDEFINIRE IMM. PRIMA E DOPO DI TITAS10F + BOLLA ORIG.
004300970217     D TRECDS        E DS
004400970217     D  D$PDS1                 1    250
004500970217     D  D$PDS2               251    500
004600970217     D  D$DDS1               501    750
004700970217     D  D$DDS2               751   1000
004800970217     D  D$ODS1              1001   1250
004900021223     D  D$ODS2              1251   1500
005000950201     D*****************************************************************
005100950201     D* INFDS PER CONTROLLO STATO DEL PGM
005200950201     D                SDS
005300950201     D  ERRTYP                40     42
005400950201     D  ERRNUM                43     46
005500940328     I*****************************************************************
005600930617     C     *ENTRY        PLIST
005700040119     C                   PARM                    ds£eco
005800040128     C                   PARM                    QUSJT04           1
005900940328     C*
006000940328     C* CHIAVE LETTURA TABEL00F - COMPLETA
006100940328     C     KEYTAB        KLIST
006200940328     C                   KFLD                    TBLKUT
006300940328     C                   KFLD                    TBLCOD
006400940328     C                   KFLD                    TBLKEY
006500040130     C* CHIAVE LETTURA ecerr01l
006600040130     C     KEYerr        KLIST
006700040130     C                   KFLD                    d$gdatc           8 0
006800040130     C                   KFLD                    d$gtim
006900941026     C*
007000991214     C* AZZERAMENTI
007100991214     C                   Z-ADD     *ZEROS        D$GPRE
007200950125     C*
007300950125     C* AZZERA SCHIERE COMPETENZE/RICAVI/VOCI
007400950125     C                   EXSR      AZZSCH
007500930709     C*
007600940328     C* CARICA TABELLE
007700940328     C                   EXSR      CARTAB
007800940328     C*
007900040119     C* RICEVE RECORD VARIATO
008000020612     C                   EXSR      RCVFILE
008100040130     c*
008200040130     c                   eval      d$gdatc = d$gdat + 20000000
008300040130     c     keyerr        setgt     ecerr01l
008400040130     c     keyerr        readpe    ecerr01l
008500040130     c                   if        not %Eof(ecerr01l)
008600040130     c                   z-add     errprg        d$gpre
008700040130     c                   end
008800950901     C*
008900991214     C* LANCIA PGM
009000940328     C                   CALL      D$GPGS
009100940805     C                   PARM                    DS$ECO
009200971229     C                   PARM                    DS£ECO
009300040119     C* CHIUDE TUTTI I PGM
009400040203     C                   MOVEL     'C'           D$GSTS
009500040203     C                   CALL      D$GPGS
009600040203     C                   PARM                    DS$ECO
009700040203     C                   PARM                    DS£ECO
009800940328     C*
009900040128     C* Infine x decidere il tipo di chiusura testo il tipo di job: se
010000040128     C* interattivo chiudo in "RT", altrimenti in "LR".
010100040203     C                   IF        QUSJT04 = 'I'
010200040203     C                   EVAL      *INLR = *ON
010300040203     C                   ELSE
010400040128     C                   EVAL      *INRT = *ON
010500040203     C                   ENDIF
010600940328     C*-------------------------------------------------------------------
010700040119     C*? RCVFILE - LEGGE RECORD VARIATO
010800940328     C*-------------------------------------------------------------------
010900020612     C     RCVFILE       BEGSR
011000020617     C* INIZIALIZZO A NON AGGIORNARE I FLAGS DELLA TABELLA "2V"
011100020617     C                   MOVEL     *BLANKS       D$GPGS
011200020617     C                   MOVEL     'N'           D$GSDA
011300020617     C                   MOVEL     'N'           D$GSDC
011400020617     C                   MOVEL     'N'           D$GSDF
011500020617     C                   MOVEL     'N'           D$GSDO
011600020617     C                   MOVEL     'N'           D$GSDP
011700020617     C                   MOVEL     'N'           D$GSDR
011800020617     C                   MOVEL     'N'           D$GSDT
011900040316     C                   MOVEL     'N'           D$GSDE
012000020617     C                   MOVEL     'N'           D$GCET
012100020617     C                   MOVEL     'N'           D$GWFM
012200020617     C                   MOVEL     'N'           D$GFIM
012300020617     C                   MOVEL     'N'           D$GFCE
012400021223     C                   MOVEL     'N'           D$GCOP
012500020612     C*
012600020607     C* imposta i flag di aggiornamento files dalla tabella 2V
012700020607     C                   Z-ADD     1             I
012800020607     C     D£GFIL        LOOKUP    FIL(I)                                 99
012900020607     C     *IN99         IFEQ      '1'
013000020607     C                   MOVEL     PGM(I)        D$GPGS
013100020607     C                   MOVEL     SDA(I)        D$GSDA
013200020607     C                   MOVEL     SDC(I)        D$GSDC
013300020607     C                   MOVEL     SDF(I)        D$GSDF
013400020607     C                   MOVEL     SDO(I)        D$GSDO
013500020607     C                   MOVEL     SDP(I)        D$GSDP
013600020607     C                   MOVEL     SDR(I)        D$GSDR
013700020607     C                   MOVEL     SDT(I)        D$GSDT
013800040316     C                   MOVEL     SDE(I)        D$GSDE
013900020607     C                   MOVEL     CET(I)        D$GCET
014000020607     C                   MOVEL     MOV(I)        D$GWFM
014100020607     C                   MOVEL     FIM(I)        D$GFIM
014200020607     C                   MOVEL     FCE(I)        D$GFCE
014300021223     C                   MOVEL     COP(I)        D$GCOP
014400040119     c                   endif
014500941129     C* IMPOSTA I CAMPI DELLA DS DI GESTIONE DEL C/ECON.
014600941129     C                   MOVEL     D£GSTS        D$GSTS
014700950123     C                   MOVEL     D£GSTS        D$GSTO
014800941129     C                   Z-ADD     D£GDAT        D$GDAT
014900941129     C                   Z-ADD     D£GTIM        D$GTIM
015000941129     C                   MOVEL     D£GUSR        D$GUSR
015100941129     C                   MOVEL     D£GPGF        D$GPGF
015200941129     C                   MOVEL     D£GFIL        D$GFIL
015300941129     C                   MOVEL     D£GMBR        D$GMBR
015400941129     C                   MOVEL     D£GLIB        D$GLIB
015500941129     C                   MOVEL     *BLANKS       D$GERR
015600941129     C                   MOVEL     *BLANKS       D$GMSG
015700950119     C                   MOVEL     D£GPDE        D$GPDE
015800021223     C                   MOVEL     D£GTRD        D$GTRD
015900941129     C                   MOVEL     *BLANKS       D$GXXX
016000950119     C                   MOVEL     D£PRE1        D$PRE1
016100950119     C                   MOVEL     D£PRE2        D$PRE2
016200950119     C                   MOVEL     D£DRE1        D$DRE1
016300950119     C                   MOVEL     D£DRE2        D$DRE2
016400950123     C     D£GFIL        IFEQ      'TNCSB00F'
016500950123     C                   MOVEL     D£PRE1        D$PCO1
016600950123     C                   MOVEL     D£PRE2        D$PCO2
016700950123     C                   MOVEL     D£DRE1        D$DCO1
016800950123     C                   MOVEL     D£DRE2        D$DCO2
016900950123     C                   ENDIF
017000030801     C     D£GFIL        IFEQ      'FIAR500F'
017100021223IF  2C                   IF        D£GTRD = 'TRS'
017200021223     C                   MOVEL     D£PRE1        D$PAR5T
017300021223     C                   MOVEL     D£DRE1        D$DAR5T
017400021223E   2C                   ENDIF
017500030801IF  2C                   IF        D£GTRD = 'BAN' OR
017600030801     C                             D£GTRD = 'BNB'
017700021223     C                   MOVEL     D£PRE1        D$PAR5B
017800021223     C                   MOVEL     D£DRE1        D$DAR5B
017900021223E   2C                   ENDIF
018000021223     C                   ENDIF
018100950125     C                   MOVEA     CAN           D$DCCD
018200950125     C                   MOVEA     IAN           D$DICD
018300950125     C                   MOVEA     CAN           D$DCVD
018400950125     C                   MOVEA     CAN           D$DPVD
018500950125     C                   MOVEA     IAN           D$DIVD
018600020612     C*
018700930617     C                   ENDSR
018800950125     C*-------------------------------------------------------------------
018900990804     C*? AZZSCH - AZZERA LE SCHIERE PACKED COMPET/RICAVI/VOCI
019000950125     C*-------------------------------------------------------------------
019100950125     C     AZZSCH        BEGSR
019200950125     C*
019300030505     C     1             DO        75            I
019400950125     C                   Z-ADD     *ZEROS        DSWCN
019500950125     C                   MOVE      DSWCA         CAN(I)
019600950125     C                   Z-ADD     *ZEROS        DSWIN
019700950125     C                   MOVE      DSWIA         IAN(I)
019800950125     C                   ENDDO
019900950125     C                   MOVEA     CAN           D$DCCD
020000950125     C                   MOVEA     IAN           D$DICD
020100950125     C                   MOVEA     CAN           D$DCVD
020200950125     C                   MOVEA     CAN           D$DPVD
020300950125     C                   MOVEA     IAN           D$DIVD
020400950125     C*
020500950125     C                   ENDSR
020600940328     C*-------------------------------------------------------------------
020700940328     C*? CARTAB - CARICA TABELLE
020800940328     C*-------------------------------------------------------------------
020900940328     C     CARTAB        BEGSR
021000940328     C*
021100940328     C                   OPEN      TABEL00F
021200940328     C*
021300020528     C* CARICA TABELLA DATI VARI CONTO ECONOMICO
021400940328     C                   Z-ADD     0             I                 4 0
021500940328     C                   Z-ADD     1             TBLKUT
021600940805     C                   MOVEL     '2V'          TBLCOD
021700940328     C                   MOVEL     *BLANKS       TBLKEY
021800940328     C     KEYTAB        SETLL     TABEL00F                           99
021900940328     C  N99              READ      TABEL00F                               99
022000940328     C     *IN99         DOWEQ     '0'
022100940805     C     TBLCOD        ANDEQ     '2V'
022200940328     C     TBLFLG        IFEQ      *BLANKS
022300940328     C                   ADD       1             I
022400020528     C                   MOVEL     TBLUNI        DS2V
022500940328     C                   MOVEL     TBLKEY        FIL(I)
022600020528     C                   MOVEL     §2VPGM        PGM(I)
022700020528     C                   MOVEL     §2VSDA        SDA(I)
022800020528     C                   MOVEL     §2VSDC        SDC(I)
022900020528     C                   MOVEL     §2VSDF        SDF(I)
023000020528     C                   MOVEL     §2VSDO        SDO(I)
023100020528     C                   MOVEL     §2VSDP        SDP(I)
023200020528     C                   MOVEL     §2VSDR        SDR(I)
023300020528     C                   MOVEL     §2VSDT        SDT(I)
023400040316     C                   MOVEL     §2VSDE        SDE(I)
023500020528     C                   MOVEL     §2VCET        CET(I)
023600020528     C                   MOVEL     §2VMOV        MOV(I)
023700020531     C                   MOVEL     §2VFIM        FIM(I)
023800020531     C                   MOVEL     §2VFCE        FCE(I)
023900021223     C                   MOVEL     §2VCOP        COP(I)
024000020528     C                   ENDIF
024100940328     C                   READ      TABEL00F                               99
024200940328     C                   ENDDO
024300940328     C*
024400940328     C                   CLOSE     TABEL00F
024500970219     C*
024600940328     C                   ENDSR

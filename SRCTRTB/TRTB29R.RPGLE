000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020418      * TRTB29R  *---------------------------------------------------*
000300020418      *-----------------*                                            *
000400020418      *   - GESTIONE TABELLA CODICI TRATTAMENTO MERCE DI DEFAULT
000500020418      *--------------------------------------------------------------*
000600900607     FTABEL00F  UF A E           K DISK
000700910328     FAZORG01L  IF   E           K DISK
000800910328     FTRTB29D   CF   E             WORKSTN
000900910328     F                                     SFILE(TB29D2SF:NRR)
001000971203     D L1              S              3  0 DIM(30)
001100910328     D SKI             S              1    DIM(5)
001200971216     D TRUL12        E DS                  EXTNAME(TRUL12DS)
001300900607     D RECORD          DS
001400910328     D  VIDLNP                 1      3
001500910328     D  VIDCTM                 4      5
001600910328     D DS1B          E DS
001700910328     D CNCR80        E DS
001800910328     D KPJBA         E DS
001900900320     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002000971203     D* DS PER TRUL06R - CARICAMENTO £X
002100971203     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002200971203     D  LIN                    1     90  0
002300971203     D                                     DIM(30)
002400020418
002500020418     D Dstnsd          DS
002600020418     D  §codnsd                       3
002700020418     D  §tipnsd                       1
002800020418     D  §desnsd                      25
002900020418     D  §pransd                       3
003000000000     C*---------------------------------------------------------------*
003100000000     C     *ENTRY        PLIST
003200000000     C                   PARM                    KPJBA
003300000000     C                   Z-ADD     1             CODUT
003400900320     C                   CALL      'X§PARUT'
003500900320     C                   PARM                    UT§DSE
003600000000     C                   MOVEL     RAGUT         RSUT             20
003700910328     C                   MOVEL     REC80         CNCR80
003800000000     C*---------------------------------------------------------------*
003900910328     C     *LIKE         DEFINE    TBLKUT        §KUT
004000910328     C     *LIKE         DEFINE    TBLCOD        §COD
004100910328     C     *LIKE         DEFINE    TBLKEY        §KEY
004200900607     C     KTAB          KLIST
004300900607     C                   KFLD                    CODUT
004400900607     C                   KFLD                    COD
004500900607     C     KTAB2         KLIST
004600900607     C                   KFLD                    CODUT
004700900607     C                   KFLD                    COD               2
004800900607     C                   KFLD                    KEY               8
004900971216     C**
005000971216     C* Richiamo pgm per controllo modificabilità tabella
005100971216     C                   CLEAR                   TRUL12
005200971216     C                   MOVEL     'L'           I12TLA
005300971216     C                   MOVEL     KNSIF         I12NSI
005400971216     C                   MOVEL     '3G'          I12COD
005500971216     C                   CALL      'TRUL12R'
005600971216     C                   PARM                    TRUL12
005700971216     C     O12ERR        IFEQ      '1'
005800971216     C                   GOTO      FINE
005900971216     C                   END
006000900607     C*
006100910328     C                   EXSR      CARTAB
006200900607     C                   DO        *HIVAL
006300900607     C                   SETOFF                                       50
006400900323     C** PULIZIA SUBFILE
006500000000     C                   EXSR      PULSF
006600900323     C** CARICO SUBFILE
006700910328     C                   MOVEL     '3G'          COD
006800900607     C     KTAB          SETLL     TABEL
006900900517     C                   Z-ADD     0             NRR               4 0
007000900516     C                   Z-ADD     1             REC
007100900517     C                   EXSR      CARSBF
007200900323     C*
007300910328     C     SU2           TAG
007400910328     C                   WRITE     TB29D201
007500900607     C*
007600000000     C     SU            TAG
007700910328     C                   EXFMT     TB29D2CT
007800900323     C** FINE LAVORO
007900900607     C  NKC              DO
008000900608     C** 10 ON - PREMUTO ROLLUP
008100900607     C   10              DO
008200900607     C                   Z-ADD     SAV           NRR
008300910328     C   50NRR           CHAIN     TB29D2SF                           32
008400910328     C* SE NON E' PIENO L'ULTIMO RECORD NON ROLLO
008500910328     C   50
008600910328     CANN32RECORD        IFNE      *BLANKS
008700900607     C                   EXSR      CARSBF
008800900607     C                   ELSE
008900900607     C                   SETON                                        08
009000900607     C                   END
009100900607     C   50
009200900607     CANN32              GOTO      SU
009300900607     C*
009400900607     C* PER GLI ALTRI CASI ROLLO
009500900607     C                   EXSR      CARSBF
009600900607     C                   GOTO      SU
009700900607     C                   END
009800900608     C**
009900900608     C** ENTER - CONTROLLI
010000910328     C                   EXSR      CONTR
010100910328     C   90              GOTO      SU
010200910328     C   91              GOTO      SU2
010300900608     C*
010400910328     C                   SETOFF                                       30
010500910328     C                   Z-ADD     1             NRR
010600910328     C     *IN30         DOWEQ     '0'
010700910328     C                   READC     TB29D2SF                               30
010800910328     C  N30RECORD        IFNE      *BLANKS
010900910328     C                   MOVEL     '3G'          COD
011000910328     C                   MOVEL     *BLANKS       KEY
011100910328     C                   MOVEL     VIDLNP        KEY
011200910328     C     KTAB2         CHAIN     TABEL                              31
011300900608     C     VIDANN        IFEQ      'A'
011400900607     C* CANCELLO
011500910328     C  N31              DELETE    TABEL
011600900607     C                   ELSE
011700900706     C                   MOVEL     CODUT         TBLKUT
011800910328     C                   MOVEL     '3G'          TBLCOD
011900910328     C                   MOVEL     *BLANKS       TBLKEY
012000910328     C                   MOVEL     VIDLNP        TBLKEY
012100910328     C                   MOVEL     *BLANKS       TBLUNI
012200910328     C                   MOVEL     VIDCTM        TBLUNI
012300900706     C                   MOVEL     *BLANKS       TBLFLG
012400980407     C                   MOVEL     '2'           TBLFTT
012500980407     C                   MOVEL     VIDLNP        TBLFLT
012600900706     C                   MOVEL     *BLANKS       TBLFTR
012700900706     C                   MOVEL     *ZEROS        TBLDTR
012800900607     C* AGGIUNGO
012900900607     C   31              WRITE     TABEL
013000900607     C* AGGIORNO
013100900607     C  N31              UPDATE    TABEL
013200900607     C                   END
013300900607     C                   END
013400910328     C*
013500910328     C                   END
013600910328     C                   END
013700900607     C  NKC              END
013800900323     C*
013900000000     C     FINE          TAG
014000000000     C                   SETON                                        LR
014100900323     C* PULIZIA SUBFILE ---------------------------------------------
014200900323     C     PULSF         BEGSR
014300000000     C                   SETON                                        84
014400910328     C                   WRITE     TB29D2CT
014500000000     C                   SETOFF                                       84
014600900319     C                   ENDSR
014700900323     C*
014800900323     C* CARICO IN AVANTI SUBFILE --------------------------------------
014900900323     C     CARSBF        BEGSR
015000900323     C*
015100900607     C                   Z-ADD     1             B                 3 0
015200910328     C                   SETOFF                                       02
015300900607     C  N50B             DOWLE     17
015400900607     C     KTAB          READE     TABEL                                  50
015500900607     C*
015600900607     C  N50              DO
015700020725      * Proseguo nel caricamento solo se tblkey è uguale ad un P.O
015800020725      * del terminal.
015900020725     C                   MOVEL     TblKey        $Cod              3 0
016000020725     C     $Cod          LOOKUP    L1                                     31
016100020725     C                   IF        *in31=*on
016200900607     C                   MOVEL     *BLANKS       VIDANN
016300910328     C                   MOVEL     TBLKEY        VIDLNP
016400910328     C                   MOVEL     TBLKEY        COMLNP
016500910328     C* DECODIFICA LINEA DI PARTENZA
016600910328     C     COMLNP        CHAIN     AZORG                              31
016700910328     C  N31              MOVEL     ORGDES        DESLNP
016800910328     C   31              MOVEL     *BLANKS       DESLNP
016900910328     C*
017000910328     C* DECODIFICA CODICE TRATTAMENTO MERCE
017100910328     C                   MOVEL     TBLUNI        VIDCTM
017200910328     C                   MOVEL     '1B'          COD
017300910328     C                   MOVEL     *BLANKS       KEY
017400910328     C                   MOVEL     VIDCTM        KEY
017500910328     C     KTAB2         CHAIN     TABEL                              31
017600910328     C  N31              MOVEL     TBLUNI        DESCTM
017700910328     C   31              MOVEL     *BLANKS       DESCTM
017800910328     C*
017900910328     C                   MOVEL     '3G'          COD
018000910328     C                   MOVEL     *BLANKS       KEY
018100910328     C                   MOVEL     VIDLNP        KEY
018200910328     C     KTAB2         CHAIN     TABEL                              31
018300900607     C                   ADD       1             NRR
018400910328     C                   ADD       1             B
018500910328     C                   WRITE     TB29D2SF
018600020725     C                   ENDIF
018700900323     C                   END
018800900607     C  N50              END
018900900607     C*
019000900607     C** FINE CARICAMENTO IL RESTO LO INIZIALIZZO
019100910328     C                   SETON                                        02
019200900607     C   50B             DOWLE     17
019300900607     C                   ADD       1             NRR
019400910328     C                   MOVEL     *BLANKS       VIDLNP
019500910328     C                   MOVEL     *BLANKS       DESLNP
019600910328     C                   MOVEL     *BLANKS       VIDCTM
019700910328     C                   MOVEL     *BLANKS       DESCTM
019800910328     C                   MOVEL     *BLANKS       VIDANN
019900910328     C                   WRITE     TB29D2SF
020000910328     C                   ADD       1             B
020100900607     C                   END
020200900607     C*
020300900607     C                   Z-ADD     NRR           REC
020400900607     C                   Z-ADD     NRR           SAV               4 0
020500900323     C*
020600900323     C                   ENDSR
020700910328     C* CARICO TABELLA FILIALI GESTITE £1 -----------------------------
020800910328     C     CARTAB        BEGSR
020900971203     C***
021000971203     C* CARICO TABELLA PUNTI OPERATIVI GESTITI £1
021100971203     C***
021200971203     C                   CLEAR                   DSUL06
021300971203     C                   MOVE      '£1'          D06COD
021400971203     C                   MOVEL     SIMFEL        D06KEY
021500971203     C                   MOVEL     'L'           D06TLA
021600971203     C                   MOVEL     DSUL06        KPJBU
021700971203     C*
021800971203     C                   CALL      'TRUL06R'
021900971203     C                   PARM                    KPJBA
022000971203     C                   MOVEL     KPJBU         DSUL06
022100971203     C                   MOVEA     LIN           L1
022200910328     C*
022300910328     C                   ENDSR
022400910328     C*
022500910328     C* CONTROLLO DATI FORMATO1 --------------------------------------
022600910328     C     CONTR         BEGSR
022700910328     C                   SETOFF                                       309091
022800910328     C                   Z-ADD     1             NRR
022900910328     C*
023000910328     C     *IN30         DOWEQ     '0'
023100910328     C                   READC     TB29D2SF                               30
023200910328     C*
023300910328     C  N30RECORD        IFNE      *BLANKS
023400910328     C* CONTROLLO SE RICERCA  LINEA PARTENZA
023500910328     C                   MOVEA     *BLANKS       SKI
023600910328     C                   MOVEA     VIDLNP        SKI
023700910328     C     '?'           LOOKUP    SKI                                    31
023800910328     C   31              DO
023900020418     C                   clear                   dstnsd
024000020418     C                   call      'TNSD24R'
024100020418     C                   parm                    §codnsd
024200020418     C                   parm                    §tipnsd
024300020418     C                   parm                    §desnsd
024400020418     C                   parm                    §pransd
024500020418     C                   movel     §codnsd       vidlnp
024600020418     C                   movel     §desnsd       deslnp
024700910328     C                   SETON                                        9130
024800910328     C                   GOTO      ENDCTR
024900910328     C                   END
025000910328     C* CONTRLLO SE RICERCA CODICI TRATTAMENTO MERCE
025100910328     C                   MOVEA     *BLANKS       SKI
025200910328     C                   MOVEA     VIDCTM        SKI
025300910328     C     '?'           LOOKUP    SKI                                    31
025400910328     C   31              DO
025500910328     C                   MOVEL     CODUT         §KUT
025600910328     C                   MOVEL     '1B'          §COD
025700910328     C                   MOVE      *BLANKS       VIDCTM
025800910328     C                   CALL      'X§TABER'
025900910328     C                   PARM                    §KUT
026000910328     C                   PARM                    §COD
026100910328     C                   PARM                    §KEY
026200910328     C                   PARM                    §DES             30
026300910328     C                   MOVEL     §KEY          VIDCTM
026400910328     C                   MOVEL     §DES          DESCTM
026500910328     C                   SETON                                        9130
026600910328     C                   GOTO      ENDCTR
026700910328     C                   END
026800910328     C*
026900910328     C* LINEA PARTENZA
027000910328     C                   TESTN                   VIDLNP               31
027100910328     C  N31              SETON                                        30
027200910328     C*
027300910328     C  N30VIDLNP        IFGT      *ZEROS
027400910328     C                   MOVEL     VIDLNP        COMLNP            3 0
027500910328     C     COMLNP        CHAIN     AZORG                              30
027600910328     C                   ELSE
027700910328     C                   SETON                                        30
027800910328     C                   END
027900910328     C* CONTROLLO SE ANNULLATO
028000910328     C  N30ORGFVA        IFNE      *BLANKS
028100910328     C                   SETON                                        30
028200910328     C                   END
028300910328     C* CONTRLLO SE E' IN £1
028400910328     C  N30COMLNP        LOOKUP    L1                                     31
028500910328     C  N30
028600910328     CANN31              SETON                                            30
028700910328     C*
028800910328     C   30              Z-ADD     NRR           REC
028900910328     C   30              SETON                                        4090
029000910328     C   30              GOTO      ENDCTR
029100910328     C                   MOVEL     ORGDES        DESLNP
029200910328     C*
029300910328     C** COD TRATTAMENTO MERCE
029400910328     C     VIDCTM        IFNE      *BLANKS
029500910328     C*
029600910328     C                   MOVEL     '1B'          COD
029700910328     C                   MOVEL     *BLANKS       KEY
029800910328     C                   MOVEL     VIDCTM        KEY
029900910328     C     KTAB2         CHAIN     TABEL                              30
030000910328     C   30              SETON                                        4190
030100910328     C   30              GOTO      ENDCTR
030200910328     C                   END
030300910328     C*
030400910328     C                   MOVEL     '3G'          COD
030500910328     C                   MOVEL     *BLANKS       KEY
030600910328     C                   MOVEL     VIDLNP        KEY
030700910328     C     KTAB2         CHAIN     TABEL                              31
030800910328     C* SE NON ESISTE E RICHIESTO ANNULLAMENTO ERRORE
030900910328     C   31VIDANN        IFEQ      'A'
031000910328     C                   SETON                                        429030
031100910328     C                   END
031200910328     C*
031300910328     C     ENDCTR        TAG
031400910328     C                   SETON                                        36
031500910328     C                   UPDATE    TB29D2SF
031600910328     C                   SETOFF                                       36
031700910328     C                   END
031800910328     C                   END
031900910328     C*
032000910328     C                   ENDSR

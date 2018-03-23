000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020807      * FNLSD5R *----------------------------------------------------*
000300020807      *                STAMPA ETICHETTA                              *
000400020807      *--------------------------------------------------------------*
000500020807     ***FFNBLP29L  IF   E           K DISK
000600020807     FFNBLP79L  IF   E           K DISK
000700900509     FAZORG01L  IF   E           K DISK
000800940919     FFNFGV01L  IF   E           K DISK
000900020807
001000941006     D* PASSAGGIO DATI DA STAMPA BORDERO'
001100900511     D PARAM           DS
001200910130     D  VIDFFV                 1      3  0
001300910130     D  VIDFLP                 4      6  0
001400910130     D  VIDTSP                 7      7
001500020807     D  VIDPOG                 8     10  0
001600971113     D* PGM DI STAMPA ETICHETTA BORDERO'
001700941006     D  PGMSE                228    237
001800910610     D  VIDLNP               238    240  0
001900910610     D  VIDNFV               241    245  0
002000941006     D  DBR                  247    254  0
002100910130     D  FLG                  255    255
002200900521     D                 DS
002300940919     D  BLPAAS                 1      4  0
002400940919     D  BLPMGS                 5      8  0
002500940919     D  BLPDSP                 1      8  0
002600930325     D                 DS
002700930325     D  AA                     1      2  0
002800930325     D  GG                     5      6  0
002900930325     D  DATA                   1      6  0
003000910521     D KPJBA         E DS
003100020807     D UT§DSE0F      E DS
003200020807     D CNCR80        E DS
003300020807     D FNLV55DS      E DS
003400020807     D DSBLB         E DS
003500940315     C*****************************************************************
003600900521     C*  RIEPILOGO INDICATORI
003700940315     C*****************************************************************
003800940919     C* 08ON  - VENGO DA RISTAMPA
003900940919     C* 08OFF - VENGO DA STAMPA
004000910610     C* 11    - ON IMMESSA LINEA DI PARTENZA DA BORDERIZZARE
004100941006     C* 13    - ON STAMPA ETICHETTA
004200900521     C* 30    - LETTURA FILE BOLLE
004300940315     C* 31    - DI COMODO RIUTILIZZABILI
004400900521     C*****************************************************************
004500000000     C     *ENTRY        PLIST
004600000000     C                   PARM                    KPJBA
004700900522     C                   MOVEL     KPJBU         PARAM
004800000000     C                   Z-ADD     1             CODUT
004900900521     C                   CALL      'X§PARUT'
005000020807     C                   PARM                    UT§DSE0F
005100000000     C                   MOVEL     RAGUT         RSUT             20
005200900521     C                   MOVEL     REC80         CNCR80
005300900511     C*---------------------------------------------------------------*
005400940919     C* ACCESSO FNBLP29L
005500020807     ***C     KBLP1         KLIST
005600020807     C     K03BLP79      klist
005700020807     C                   kfld                    VIDpog
005800940919     C                   KFLD                    KNFV
005900940919     C                   KFLD                    VIDFFV
006000020807     C* Accesso FNFGV01L
006100020807     C     K02FGV01      klist
006200020807     C                   kfld                    VIDnfv
006300020807     C                   kfld                    VIDpog
006400910704     C*
006500940315     C* DEFINIZIONE VARIABILI
006600940919     C     *LIKE         DEFINE    BLPNFV        KNFV
006700931209     C*---------------------------------------------------------------*
006800940315     C* OPERAZIONI INIZIALI
006900940315     C                   EXSR      OPINZ
007000910704     C*
007100910809     C* CHAIN SU FOGLIO VIAGGIO SE NON C'E' VADO A FINE
007200020807     ***C     VIDNFV        CHAIN     FNFGV01L                           31
007300020807     C     K02FGV01      chain     FNFGV01L                           31
007400930216     C  N08
007500930216     CAN 31              GOTO      FINE
007600910809     C*
007700940919     C   08              Z-ADD     VIDNFV        KNFV
007800940919     C  N08              Z-ADD     0             KNFV
007900900521     C*
008000940919     C* N.B.: IL CICLO DI STAMPA PER L'ETICHETTA DEVE NECESSARIAMENTE
008100940919     C*       RISPECCHIARE QUELLO PER LA STAMPA/RISTAMPA BORDERO',
008200940919     C*       QUINDI OCCORRE FAR RIFERIMENTO AL PGM FNLS21R.
008300941006     C*
008400941006     C* CERCO SE C'E' ALMENO UNA BOLLA DA STAMPARE: SE C'E' ESCO DAL
008500941006     C*  CICLO.(ADESSO NON SI CONTANO PIU' LE SPEDIZIONI DA SCRIVERE
008600941006     C*  SULLA ETICHETTA)
008700940919     C*
008800020807     ***C     KBLP1         SETLL     FNBLP29L
008900020807     ***C     KBLP1         READE     FNBLP29L                               30
009000020807     C     K03BLP79      setll     FNBLP79L
009100020807     C     K03BLP79      reade     FNBLP79L                               30
009200900521     C*
009300940322    1C     *IN30         DOWEQ     *OFF
009400940323     C                   SETOFF                                       31
009500940315     C*
009600940315     C* 08 OFF - NON VENGO DA RISTAMPA
009700941006    2C     *IN08         IFEQ      *OFF
009800940315     C*
009900920910     C* DATA SPEDIZIONE < = DATA BORDERO' IMMESSA  - PER BORDERO'
010000941006    3C     BLPDSP        IFGT      DBR
010100941006     C                   SETON                                        31
010200941006    3C                   ENDIF
010300940315     C*
010400920910     C* LINEA DI PARTENZA NON E' QUELLA IMMESSA    - PER BORDERO'
010500940919     C  N31
010600940919    3CAN 11BLPLNP        IFNE      VIDLNP
010700940322     C                   SETON                                        31
010800940322    3C                   ENDIF
010900940919     C*
011000940919     C* SE SONO IN STAMPA, DEVO CONTROLLARE SE E' STATO RICHIESTO
011100940919     C* UN TIPO SERVIZIO SPECIFICO O TUTTI
011200941006    3C     VIDTSP        IFNE      *BLANKS
011300940919     C     VIDTSP        ANDNE     BLPTSP
011400940919     C                   SETON                                        31
011500941006    3C                   ENDIF
011600940919     C*
011700940322    2C                   ENDIF
011800930325     C*
011900940322     C* SE BOLLA LETTA NON VA BENE RILEGGO
012000020807     ***C   31KBLP1         READE     FNBLP29L                               30
012100020807     C   31K03BLP79      reade     FNBLP79L                               30
012200940322     C*
012300940322     C* SE TUTTO OK ESCO E STAMPO ETICHETTA
012400940322     C  N31              SETON                                        3013
012500940322    1C                   ENDDO
012600940315     C*
012700940315     C* RISTAMPA ETICHETTA
012800940322     C* CONTROLLO ESISTENZA FILIALE DI TRANSITO
012900940322     C     *IN13         IFEQ      *ON
013000940322     C                   EXSR      CTRFLP
013100940322     C                   EXSR      STAET
013200940322     C                   ENDIF
013300940315     C*
013400940315     C     FINE          TAG
013500971202     C*
013600971202     C                   MOVEL     'C'           D55TLA
013700971202     C                   CALL      'FNLV55R'
013800020807     C                   PARM                    FNLV55DS
013900971202     C*
014000000000     C                   SETON                                        LR
014100940919     C**************************************************************************
014200940919     C*    OPERAZIONI INIZIALI
014300940919     C**************************************************************************
014400940315     C     OPINZ         BEGSR
014500930325     C*
014600930325     C* 11 ON - IMMESSA LA LINEA DI PARTENZA
014700930325     C     VIDLNP        IFGT      0
014800930325     C                   SETON                                        11
014900940315     C                   ENDIF
015000930325     C*
015100930325     C* SE FLAG ><' '  -->  VENGO DA RISTAMPA
015200941006     C     FLG           IFNE      ' '
015300900522     C                   SETON                                        08
015400940315     C                   ENDIF
015500910809     C*
015600900521     C                   ENDSR
015700940919     C**************************************************************************
015800940919     C*    CONTROLLO ESISTENZA FILIALE DI TRANSITO
015900940919     C**************************************************************************
016000910221     C     CTRFLP        BEGSR
016100940919     C*
016200910221     C* CONTROLLO SE C'E' TRANSITO
016300940107     C*
016400910221     C     VIDFLP        IFNE      0
016500940107     C*
016600940107     C* C'E' TRANSITO SE :
016700940107     C* 1) VIDFLP NON E' = ALLA LINEA DI ARRIVO
016800940107     C* 2) VIDFLP NON E' = AL TERMINAL DI ARRIVO DELLA LINEA ARRIVO
016900971202     C* 3) VIDFLP NON E' = AL TERMINAL DI PARTENZA  DELLA LINEA ARRIVO
017000971202     C* 4) TERMINAL DI PART DI VIDFLP NON E' = AL TERMINAL DI PARTENZA
017100940107     C*                                        DELLA LINEA ARRIVO
017200940315     C     BLPLNA        IFNE      VIDFLP
017300930316     C     BLPTFA        ANDNE     VIDFLP
017400940315     C     BLPFEA        ANDNE     VIDFLP
017500940919     C     VIDFLP        CHAIN     AZORG01L                           31
017600971202     C*
017700020807     C                   CLEAR                   FNLV55DS
017800971202     C                   MOVEL     'P'           D55TPT
017900971202     C                   MOVE      VIDFLP        D55LIN
018000971203     C  N08              MOVE      DBR           D55DRF
018100971203     C   08              MOVE      BLPDBR        D55DRF
018200971202     C                   CALL      'FNLV55R'
018300020807     C                   PARM                    FNLV55DS
018400940107     C*
018500971202     C     BLPFEA        IFNE      D55TFP
018600910221     C  N31              MOVEL     ORGDES        DESFLP           18
018700910221     C   31              MOVEL     *BLANKS       DESFLP           18
018800940107     C                   ENDIF
018900910221     C                   END
019000910221     C                   END
019100910221     C*
019200910221     C                   ENDSR
019300940919     C**************************************************************************
019400941006     C*    STAMPO ETICHETTA BORDERO' DA ATTACCARE SULLA BUSTA
019500940919     C**************************************************************************
019600910704     C     STAET         BEGSR
019700940919     C*
019800910521     C                   Z-ADD     VIDNFV        §LBNFV
019900910521     C*
020000940919     C* LA DATA NELLA DS DSBLB E' IN FORMATO GG/MM/AA (6 CIFRE)
020100940919     C                   MOVE      FGVDFV        DATA
020200940919     C                   MOVE      FGVDFV        §LBDAT
020300940919     C                   MOVEL     GG            §LBDAT
020400940919     C                   MOVE      AA            §LBDAT
020500910521     C*
020600941216     C                   CLEAR                   §LBNSP
020700910521     C                   MOVEL     FGVDPD        §LBVET
020800910521     C                   MOVEL     FGVDCN        §LBCON
020900910521     C                   MOVEL     FGVLNP        §LBLNP
021000940919     C     FGVLNP        CHAIN     AZORG01L                           31
021100910521     C  N31              MOVEL     ORGDES        §LBDLP
021200940314     C*
021300910521     C                   MOVEL     VIDFFV        §LBLNA
021400940919     C     VIDFFV        CHAIN     AZORG01L                           31
021500910521     C  N31              MOVEL     ORGDES        §LBDLA
021600910521     C*
021700920427     C                   MOVEL     DESFLP        §LBSCA
021800930319     C*
021900930324     C* DESFLP >< ' ' --> TRANSITO
022000930319     C     DESFLP        IFNE      *BLANKS
022100020807     C                   CLEAR                   FNLV55DS
022200971113     C                   MOVEL     'A'           D55TPT
022300971113     C                   MOVEL     FGVLNP        D55LNP
022400971113     C                   MOVEL     VIDFFV        D55LIN
022500971113     C                   MOVEL     FGVDFV        D55DRF
022600971113     C                   CALL      'FNLV55R'
022700020807     C                   PARM                    FNLV55DS
022800971113     C     D55ERR        IFNE      ' '
022900971113     C                   MOVEL     D55LIN        D55TFA
023000971113     C                   ENDIF
023100930319     C*
023200971113     C     VIDFFV        IFNE      D55TFA
023300971113     C     D55TFA        CHAIN     AZORG01L                           31
023400930319     C  N31              MOVEL     ORGDES        §LBINO
023500971113     C                   ENDIF
023600930319     C*
023700930319     C                   ELSE
023800930319     C*
023900930319     C     VIDFLP        IFNE      VIDFFV
024000940919     C     VIDFLP        CHAIN     AZORG01L                           31
024100930319     C  N31              MOVEL     ORGDES        §LBINO
024200930319     C                   END
024300930324     C                   END
024400930319     C*
024500941216     C* (IL NUMERO DI BORDERO' E LA "A" DI ARRIVI NON ESISTONO PIU')
024600941216     C                   CLEAR                   §LBARR
024700941216     C                   CLEAR                   §LBNBR
024800930112     C*
024900941006     C                   CALL      PGMSE
025000910521     C                   PARM                    DSBLB
025100940919     C*
025200900521     C                   ENDSR

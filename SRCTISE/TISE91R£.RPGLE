000100000000     H*------------------------------------------------------------------------*
000200020404     H*                     - CREA WORK FILE PER PC -                          *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FWFDEL03L  IF   E           K DISK
000900020116     FSEPCC00F  O    E           K DISK
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*------------------
001400000000     D* TOTALI RAGGRUPPAMENTO COMMERCIALE
001500000000     D*------------------
001600000000     D SPE             S             13  0 DIM(12)                              *NUMERO SPEDIZIONI
001700020116     D RIR             S             17  5 DIM(12)                              *RICAVI REALI
001800020116     D RIP             S             17  5 DIM(12)                              *RICAVI PRESUNTI
001900000000     D DEL             S              4  1 DIM(12)                              *DELTA
002000000000     D*------------------------------------------------------------------------*
002100000000     D* INPUT
002200000000     D*------------------------------------------------------------------------*
002300000000     D*------------------
002400000000     D* ARCHITETTURA
002500000000     D*------------------
002600000000     D KPJBA         E DS
002700000000     D*------------------
002800000000     D* PARAMETRI DI LANCIO
002900000000     D*------------------
003000000000     D PARAM           DS
003100000000     D  PARDAT                 1      6  0
003200000000     D  PARAAI                 1      4  0
003300000000     D  PARMMI                 5      6  0
003400000000     D  PARDA2                 7     12  0
003500000000     D  PARAAF                 7     10  0
003600000000     D  PARMMF                11     12  0
003700000000     D  PARDCK                13     13
003800000000     D  PARDCC                14     14
003900000000     D  PARDCF                15     15
004000000000     D  PARFIC                16     18  0
004100000000     D  PARARC                19     21  0
004200000000     D  PARDIC                22     22
004300000000     D  PARDFF                23     23
004400000000     D  PARCOF                24     30  0
004500000000     D  PARDIF                31     31
004600000000     D  PARSTA                32     32
004700000824     D  PARSCE                33     33
004800000000     D  PARTSP                34     34
004900000000     D  PARIOE                35     35
005000000000     D  PARCLV                36     36
005100000000     D  PARDPT                37     42  0
005200000000     D  PARDP2                43     48  0
005300000000     D  PARSPD                49     52  1
005400000000     D  PARPDD                53     56  1
005500000000     D  PARPDA                57     60  1
005600000000     D  PARPOS                61     61
005700000825     D  PARCRE                62     62
005800010618     D  PARCL1                63     63
005900010618     D  PARCL2                64     64
006000010618     D  PARSIN                65     65
006100020404     D  PARCL3                66     66
006200020404     D  PARCL4                67     67
006300020404     D  PARNTW                68     68
006400030923     D  PARDFD                69     72  1
006500030923     D  PARDFA                73     76  1
006600030923     D  PARDFDP               77     80  1
006700030923     D  PARDFAP               81     84  1
006800030923     D  PARFPC                85     85
006900030923     D  PARWPC                86     86
007000030923     D  PARWCP                87     87
007100000000     C*------------------------------------------------------------------------*
007200000000     C* MAIN LINE
007300000000     C*------------------------------------------------------------------------*
007400000000     C*
007500000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
007600000000     C                   EXSR      SETFIL
007700000000     C*
007800000000     C* CICLO FINO A FINE FILE DI LAVORO
007900000000DO  1C     $FINE         DOWEQ     '0'
008000000000     C*
008100000000     C* MEMORIZZA IL NUOVO ANNO IN UN DEPOSITO
008200000000     C                   Z-ADD     WFDANN        DEPANN
008300000000     C*
008400000000     C* CICLO FINO A ROTTURA DI ANNO
008500000000DO  2C     $FINE         DOWEQ     '0'
008600000000     C     WFDANN        ANDEQ     DEPANN
008700000000     C*
008800000000     C* OPERAZIONI PER NUOVO AGENTE
008900000000     C                   Z-ADD     WFDRGF        DEPRGF
009000000000     C                   MOVEL     WFDRGD        DEPRGD
009100000000     C                   Z-ADD     *ZEROS        NUMCLI            9 0
009200000000     C                   CLEAR                   SPE                            *NUMERO SPEDIZIONI
009300000000     C                   CLEAR                   RIR                            *RICAVI REALI
009400000000     C                   CLEAR                   RIP                            *RICAVI PRESUNTI
009500000000     C*
009600000000     C* CICLO FINO A ROTTURA DI AGENTE
009700000000DO  3C     $FINE         DOWEQ     '0'
009800000000     C     WFDANN        ANDEQ     DEPANN
009900000000     C     WFDRGF        ANDEQ     DEPRGF
010000000000     C*
010100000000     C* OPERAZIONI PER NUOVO CLIENTE
010200000825     C                   Z-ADD     WFDCLI        DEPCLI
010300000000     C*
010400000000     C* CICLO FINO A ROTTURA DI CLIENTE
010500000000DO  4C     $FINE         DOWEQ     '0'
010600000000     C     WFDANN        ANDEQ     DEPANN
010700000000     C     WFDRGF        ANDEQ     DEPRGF
010800000825     C     WFDCLI        ANDEQ     DEPCLI
010900000000     C*
011000000000     C* MEMORIZZA IN SCHIERA MESI
011100000000     C                   Z-ADD     WFDMES        MM
011200000000     C                   ADD       WFDNSP        SPE(MM)                        NUMERO SPEDIZIONI
011300000000     C                   ADD       WFDRRN        RIR(MM)                        RICAVI REALI DELTA
011400000000     C                   ADD       WFDRPN        RIP(MM)                        RICAVI PRESUNTI DELT
011500000000     C*
011600000000     C* LEGGE IL RECORD SUCCESSIVO
011700000000     C                   EXSR      LETFIL
011800000000     C*
011900000000E   4C                   ENDDO
012000000000     C*
012100000000     C* FINE CLIENTE, INCREMENTO NUMERO CLIENTI
012200000000     C                   ADD       1             NUMCLI
012300000000E   3C                   ENDDO
012400000000     C*
012500020116     C* FINE AGENTE, SCRIVE RECORD SU SEPCC00F
012600000000     C                   EXSR      SCRPCC
012700000000E   2C                   ENDDO
012800000000     C* FINE ANNO
012900000000E   1C                   ENDDO
013000000000     C*
013100000000     C                   SETON                                        LR
013200000000     C*------------------------------------------------------------------------*
013300000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL WORK FILE
013400000000     C*------------------------------------------------------------------------*
013500000000     C     SETFIL        BEGSR
013600000000     C*
013700000000     C* POSIZIONAMENTO E PRIMA LETTURA DEL WORK FILE
013800000000     C                   Z-ADD     1             KWDANN                         *ANNO
013900000000     C                   Z-ADD     *ZEROS        KWDRGF                         *RAGGRUPPAMENTO
014000000825     C                   Z-ADD     *ZEROS        KWDCLI                         *CLIENTE
014100000000     C     KEYDEL        SETLL     WFDEL03L                           99        *FINE ARCHIVIO
014200000000     C*
014300000000IF  1C     *IN99         IFEQ      *ON
014400000000     C                   MOVEL     '1'           $FINE
014500000000X   1C                   ELSE
014600000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
014700000000E   1C                   ENDIF
014800000000     C*
014900000000     C                   ENDSR
015000000000     C*------------------------------------------------------------------------*
015100000000     C* LETFIL - LETTURA PROSSIMO RECORD DEL WORK FILE
015200000000     C*------------------------------------------------------------------------*
015300000000     C     LETFIL        BEGSR
015400000000     C*
015500000000     C* LEGGO FINO A:
015600000000     C*     - FINE FILE
015700000000     C*     - TROVATO RECORD VALIDO
015800000000     C                   MOVEL     'N'           WRECOK
015900000000DO  1C     $FINE         DOWEQ     '0'
016000000000     C     WRECOK        ANDEQ     'N'
016100000000     C*
016200000000     C                   READ      WFDEL03L                               99
016300000000     C*
016400000000IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
016500000000     C                   MOVEL     '1'           $FINE
016600000000X   2C                   ELSE
016700000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
016800000000E   2C                   ENDIF
016900000000E   1C                   ENDDO
017000000000     C*
017100000000     C                   ENDSR
017200000000     C*------------------------------------------------------------------------*
017300000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD DEL WORK FILE
017400000000     C*------------------------------------------------------------------------*
017500000000     C     CHKREC        BEGSR
017600000000     C*
017700000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
017800000000     C*
017900000000     C                   ENDSR
018000000000     C*------------------------------------------------------------------------*
018100020116     C* SCRPCC - SCRIVE RECORD IN SEPCC00F
018200000000     C*------------------------------------------------------------------------*
018300000000     C     SCRPCC        BEGSR
018400000000     C*
018500020116     C* === CON L'EURO NN SI EFFETTUA + LA "MIGLIALIZZAZIONE" DEGLI IMPORTI  ====
018600020116     C*
018700000000     C* RICAVI X 1000
018800020116DO  1C*    1             DO        12            MM
018900020116     C*    RIR(MM)       DIV       1000          RIR(MM)                        *RICAVO REALE
019000020116     C*    RIP(MM)       DIV       1000          RIP(MM)                        *RICAVO PRESUNTO
019100020116E   1C*                  ENDDO
019200020116     C* =========================================================================
019300000000     C*
019400000000     C* DELTA 12 MESI
019500000000DO  1C     1             DO        12            MM
019600020116     C                   Z-ADD     RIR(MM)       WRRW             17 5          *RICAVO REALE
019700020116     C                   Z-ADD     RIP(MM)       WRPW             17 5          *RICAVO PRESUNTO
019800000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
019900000000     C                   EXSR      CALDEL
020000000000     C                   Z-ADD     WDPW          DEL(MM)                        *SCHIERA DELTA
020100000000E   1C                   ENDDO
020200000000     C*
020300000000     C* IMPOSTA RECORD DA SCRIVERE
020400000000     C                   Z-ADD     DEPANN        PCCANN                         *ANNO
020500000000     C                   Z-ADD     DEPRGF        PCCRGF                         *AGENTE UNIFICNATE
020600000000     C                   MOVEL     DEPRGD        PCCRGD                         *DES.AGE.UNIFICANTE
020700000000     C                   Z-ADD     NUMCLI        PCCNCL                         *NUMERO CLIENTI
020800000000     C                   Z-ADD     SPE(1)        PCCSP1                         *SPEDIZIONI
020900000000     C                   Z-ADD     SPE(2)        PCCSP2                         *SPEDIZIONI
021000000000     C                   Z-ADD     SPE(3)        PCCSP3                         *SPEDIZIONI
021100000000     C                   Z-ADD     SPE(4)        PCCSP4                         *SPEDIZIONI
021200000000     C                   Z-ADD     SPE(5)        PCCSP5                         *SPEDIZIONI
021300000000     C                   Z-ADD     SPE(6)        PCCSP6                         *SPEDIZIONI
021400000000     C                   Z-ADD     SPE(7)        PCCSP7                         *SPEDIZIONI
021500000000     C                   Z-ADD     SPE(8)        PCCSP8                         *SPEDIZIONI
021600000000     C                   Z-ADD     SPE(9)        PCCSP9                         *SPEDIZIONI
021700000000     C                   Z-ADD     SPE(10)       PCCSP0                         *SPEDIZIONI
021800000000     C                   Z-ADD     SPE(11)       PCCSPA                         *SPEDIZIONI
021900000000     C                   Z-ADD     SPE(12)       PCCSPB                         *SPEDIZIONI
022000000000     C                   Z-ADD     RIR(1)        PCCRI1                         *RICAVI
022100000000     C                   Z-ADD     RIR(2)        PCCRI2                         *RICAVI
022200000000     C                   Z-ADD     RIR(3)        PCCRI3                         *RICAVI
022300000000     C                   Z-ADD     RIR(4)        PCCRI4                         *RICAVI
022400000000     C                   Z-ADD     RIR(5)        PCCRI5                         *RICAVI
022500000000     C                   Z-ADD     RIR(6)        PCCRI6                         *RICAVI
022600000000     C                   Z-ADD     RIR(7)        PCCRI7                         *RICAVI
022700000000     C                   Z-ADD     RIR(8)        PCCRI8                         *RICAVI
022800000000     C                   Z-ADD     RIR(9)        PCCRI9                         *RICAVI
022900000000     C                   Z-ADD     RIR(10)       PCCRI0                         *RICAVI
023000000000     C                   Z-ADD     RIR(11)       PCCRIA                         *RICAVI
023100000000     C                   Z-ADD     RIR(12)       PCCRIB                         *RICAVI
023200000000     C                   Z-ADD     DEL(1)        PCCDE1                         *DELTA
023300000000     C                   Z-ADD     DEL(2)        PCCDE2                         *DELTA
023400000000     C                   Z-ADD     DEL(3)        PCCDE3                         *DELTA
023500000000     C                   Z-ADD     DEL(4)        PCCDE4                         *DELTA
023600000000     C                   Z-ADD     DEL(5)        PCCDE5                         *DELTA
023700000000     C                   Z-ADD     DEL(6)        PCCDE6                         *DELTA
023800000000     C                   Z-ADD     DEL(7)        PCCDE7                         *DELTA
023900000000     C                   Z-ADD     DEL(8)        PCCDE8                         *DELTA
024000000000     C                   Z-ADD     DEL(9)        PCCDE9                         *DELTA
024100000000     C                   Z-ADD     DEL(10)       PCCDE0                         *DELTA
024200000000     C                   Z-ADD     DEL(11)       PCCDEA                         *DELTA
024300000000     C                   Z-ADD     DEL(12)       PCCDEB                         *DELTA
024400000000     C*
024500020116     C                   WRITE     SEPCC000
024600000000     C*
024700000000     C                   ENDSR
024800000000     C*------------------------------------------------------------------------*
024900000000     C* CALDEL - CALCOLA IL DELTA
025000000000     C*          INPUT  --> WRRW: RICAVO REALE
025100000000     C*                     WRPW: RICAVO PRESUNTO
025200000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
025300000000     C*------------------------------------------------------------------------*
025400000000     C     CALDEL        BEGSR
025500000000     C*--------------------
025600000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
025700000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
025800000000     C*--------------------
025900000000     C*
026000000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
026100020116     C     WRRW          SUB       WRPW          GAP              17 5          (REALE - PRESUNTO)
026200000000     C     GAP           MULT      100           GAP                            (GAP x 100)
026300000000     C*
026400000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
026500000000IF  1C     WRRW          IFEQ      *ZEROS
026600000000     C                   Z-ADD     1             WRRW
026700000000E   1C                   ENDIF
026800000000     C*
026900000000     C     GAP           DIV       WRRW          GAP
027000000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
027100000000     C*
027200000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
027300000000     C     WDPW          COMP      0                                    28
027400000000     C* POSITIVO
027500000000IF  1C     *IN28         IFEQ      *OFF
027600000000IF  2C     GAP           IFGE      1000
027700000000     C                   Z-ADD     +999,9        WDPW
027800000000E   2C                   ENDIF
027900000000     C* NEGATIVO
028000000000X   1C                   ELSE
028100000000IF  2C     GAP           IFLE      -1000
028200000000     C                   Z-ADD     -999,9        WDPW
028300000000E   2C                   ENDIF
028400000000E   1C                   ENDIF
028500000000     C*
028600000000     C                   ENDSR
028700000000     C*------------------------------------------------------------------------*
028800000000     C* *INZSR - IMPOSTAZIONI INIZIALI
028900000000     C*------------------------------------------------------------------------*
029000000000     C     *INZSR        BEGSR
029100000000     C*--------------------
029200000000     C* RICEVIMENTO PARAMETRI
029300000000     C*--------------------
029400000000     C     *ENTRY        PLIST
029500000000     C                   PARM                    KPJBA
029600000000     C                   MOVEL     KPJBU         PARAM
029700000000     C*--------------------
029800000000     C* VARIABILI RIFERITE AL DATA BASE
029900000000     C*--------------------
030000000000     C     *LIKE         DEFINE    WFDANN        KWDANN                         *WFDEL03L
030100000000     C     *LIKE         DEFINE    WFDRGF        KWDRGF
030200000825     C     *LIKE         DEFINE    WFDCLI        KWDCLI
030300000000     C     *LIKE         DEFINE    WFDANN        DEPANN                         *DEPOSITI
030400000000     C     *LIKE         DEFINE    WFDRGF        DEPRGF
030500000000     C     *LIKE         DEFINE    WFDRGD        DEPRGD
030600000000     C     *LIKE         DEFINE    WFDMES        DEPMES
030700000825     C     *LIKE         DEFINE    WFDCLI        DEPCLI
030800000000     C*--------------------
030900000000     C* CHIAVI DI LETTURA
031000000000     C*--------------------
031100000000     C* LETTURA WFDEL03L - COMPLETA
031200000000     C     KEYDEL        KLIST
031300000000     C                   KFLD                    KWDANN                         *ANNO
031400000000     C                   KFLD                    KWDRGF                         *RAGGRUP.TO FILIALE
031500000825     C                   KFLD                    KWDCLI                         *CLIENTE
031600000000     C*--------------------
031700000000     C* VARIABILI NON RIFERITE AL DATA BASE
031800000000     C*--------------------
031900000000     C                   Z-ADD     *ZEROS        MM                2 0          *INDICE PER MESI
032000000000     C*--------------------
032100000000     C* VARIABILI DI CONTROLLO
032200000000     C*--------------------
032300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
032400000000     C                   MOVEL     'S'           WRECOK            1            *VALIDITA ' RECORD
032500000000     C*
032600000000     C                   ENDSR

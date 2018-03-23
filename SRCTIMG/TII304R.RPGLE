000100990127     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990127     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300870513     H* OCO301R *-OC/38-*--------------------------------------------*
000400870513     H*-----------------*                                            *
000500870513     H*             - SIMULAZIONE ASSEGNAZIONE -                     *
000600870513     H*             ----------------------------                     *
000700870513     H*                                                      AZ.O301 *
000800990127     H*         FILE :          - tmmoc08L - tmmoc09L -              *
000900990127     H*                tmmoc00F - tmamp01L - tmsmp01L -              *
001000870513     H*                                                              *
001100870513     H*--------------------------------------------------------------*
001200990127     Ftmmoc08L  UF   E           K DISK
001300990127     Ftmamp01L  IF   E           K DISK
001400990127     Ftmsmp01L  UF   E           K DISK
001500871229     FOCPRTSTD  O    F  132        PRINTER USROPN
001600870513     F*--------------------------------------------------------------*
001700871229     D MPHDT1          DS
001800871229     D*--- DATA AREA N.MOVIMENTO MAGAZZINO :
001900871229     D  NUMMOV                 1      5  0
002000871229     D  FILL1                  6      8
002100871229     D  DATDTA                 9     14  0
002200871229     D  FILL2                 15     17
002300871229     D  ANNDTA                18     19  0
002400871229     D  FIL03                 20     50
002500871229     D  DGIO                  51     56  0
002600871229     D  FIL04                 57    100
002700871229     D  FILL5                101    300
002800871229     D  GIOTOL               301    302  0
002900871229     D  FILL6                303    512
003000870513     D PARAM2          DS
003100990127     D  VIDDCP                 1      8  0
003200990127     D  VIDVAL                 9     13  2
003300990127     D  VIDFLS                14     14
003400990127     D  VIDIMP                15     29  0
003500990127     D  socpas                30     32
003600870515     D RCDS            DS
003700870515     D  MOCPCL                 1      1
003800990127     D  MOCDCP                 2      9  0
003900990127     D  MOCDOR                10     17  0
004000990127     D  MOCNOR                18     22  0
004100870515     D RCPRE           DS
004200870515     D  PREPCL                 1      1
004300990127     D  PREDCP                 2      9  0
004400990127     D  PREDOR                10     17  0
004500990127     D  PRENOR                18     22  0
004600990127     D soc001        E DS                  EXTNAME(xsoc001ds)
004700990127     D xsocds          DS          1000
004800990127     d dataiso         s               d   datfmt(*iso)
004900990127     d dataeur         s               d   datfmt(*eur)
005000870513     D KPJBA         E DS
005100870513     I*--- PASSAGGIO PARAMETRI
005200870513     C*----------------------------------------------------*
005300870513     C     *ENTRY        PLIST                                                  *
005400870513     C                   PARM                    KPJBA                          *
005500990127     c                   time                    como14           14 0
005600990127     c                   move      como14        woggi             8 0
005700990127     C*--- FILE SIMULAZIONE ASSEGNAZIONE 'tmmoc08L'        *
005800870513     C     KEYMOC        KLIST                                                  *
005900870519     C                   KFLD                    MOCPCL                         *
006000870519     C                   KFLD                    MOCDCP                         *
006100870928     C                   KFLD                    MOCDOR                         *
006200870519     C                   KFLD                    MOCNOR                         *
006300990127     C*--- FILE ANAG. MAT. PRIME "tmamp01L"                *
006400870513     C     KEYAN         KLIST                                                  *
006500870520     C                   KFLD                    MOCMAT                         *
006600990127     C*--- FILE SALDI MAGAZZINO M.P. "tmsmp01L"            *
006700870513     C     KEYSAL        KLIST                                                  *
006800990127     C                   KFLD                    ANNDTA4                        *
006900870513     C                   KFLD                    MOCMAG                         *
007000870520     C                   KFLD                    MOCMAT                         *
007100870513     C*----------------------------------------------------*
007200871229     C*PULIZIE CAMPI
007300870916     C                   MOVEL     *BLANKS       MOCPCL
007400870916     C                   Z-ADD     0             MOCDCP
007500870916     C                   Z-ADD     0             MOCDOR
007600870916     C                   Z-ADD     0             MOCNOR
007700870520     C                   MOVEL     *BLANKS       PREPCL
007800870520     C                   Z-ADD     0             PREDCP
007900870916     C                   Z-ADD     0             PREDOR
008000870520     C                   Z-ADD     0             PRENOR
008100870513     C                   Z-ADD     0             VIDDCP
008200870513     C                   Z-ADD     0             VIDVAL
008300870514     C                   MOVEL     'N'           VIDFLS
008400870513     C                   Z-ADD     0             VIDIMP
008500870519     C                   MOVEL     KPJBU         PARAM2
008600990127     C*---------- RICERCA DITTA :
008700990127     C                   MOVEL     'SOC001'      TIPXSC
008800990127     C                   MOVEL     *blank        SOCXSC
008900990127     C                   EXSR      REPSOC
009000990127     C     RTNXSC        IFNE      '1'
009100990127     C                   MOVEL     XSOCDS        SOC001
009200990127     c                   movel     xscrgs        desdit           20
009300990127     c                   end
009400870916     C*----------------------------------------------------*
009500871229     C     *DTAARA       DEFINE                  MPHDT1                         *
009600871229     C                   IN        MPHDT1
009700870916     C*ESCLUSIONE DELLE RIGHE DIVERSE DA 'OR'
009800870916     C*ESCLUSIONE DELLE RIGHE SOSPESE DA ORDINE CLIENTE
009900870916     C*ESCLUSIONE DELLE RIGHE CON ASSEGNAZIONE IN PROVA MAGGIORE DI 0
010000870916     C*ESCLUSIONE DELLE RIGHE CHE HANNO LA DATA MAGGIORE DI QUELLA
010100870916     C*RICHIESTA
010200990127     c                   z-add     anndta        anndta4           4 0
010300990127     c                   if        anndta > 40
010400990127     c                   add       1900          anndta4
010500990127     c                   else
010600990127     c                   add       2000          anndta4
010700990127     c                   end
010800990127      *
010900870513     C     SU            TAG
011000990127     C                   READ      tmmoc000                               02
011100870916     C   02              EXSR      ORDINE
011200870513     C   02              GOTO      FINE
011300870527     C     MOCFAS        CABNE     'OR'          SU
011400870630     C     MOCTRG        CABNE     '1'           SU
011500870527     C     MOCSOS        CABNE     *BLANKS       SU
011600870527     C     MOCASS        CABNE     0             SU
011700870527     C     VIDDCP        IFGT      0
011800990127     C                   move      VIDDCP        dataeur
011900990127     C                   move      dataeur       dataiso
012000990127     c                   move      dataiso       viddcpg           8 0
012100990127     C     MOCDCP        CABGT     viddcpg       SU
012200870527     C                   END
012300870916     C*SE NON TROVO IL MATERIALE SULL'ANAGRIFICO MATERIALI SEGNALO
012400870916     C*ERRORE E SALTO LA RIGA
012500990127     C     KEYAN         CHAIN     tmamp000                           03
012600870527     C   03              EXSR      ERRORE
012700870527     C   03              GOTO      SU
012800870527     C     AMPAMT        CABNE     *BLANKS       SU
012900870513     C  N99              MOVEL     RCDS          RCPRE
013000870513     C  N99              SETON                                            99
013100870916     C     RCDS          IFNE      RCPRE
013200870513     C                   EXSR      ORDINE
013300870520     C                   MOVEL     RCDS          RCPRE
013400870520     C                   Z-ADD     0             IMPMER
013500870520     C                   Z-ADD     0             VALORD
013600870513     C                   END
013700870916     C*SE NON TROVO IL MATERIALE  SULL'ANAGRAFICO DEI SALDI
013800870916     C*SEGNALO ERRORE E SALTO LA RIGA
013900990127     C     KEYSAL        CHAIN     tmsmp000                           04
014000870520     C   04              EXSR      ERRORE
014100870520     C   04              GOTO      SU
014200870520     C                   EXSR      DSPMAG
014300870917     C     MATDSP        CABEQ     0             SU
014400870928     C** ESCLUDO LE RIGHE CHE NON HANNO DISPONIBILITA'
014500870514     C     VIDFLS        COMP      'S'                                    20
014600870514     C     MATDSP        IFGE      MOCQTA
014700870514     C                   EXSR      ASSMAG
014800870514     C                   EXSR      ASSORD
014900870520     C                   GOTO      SU
015000870514     C                   END
015100870514     C  N20              DO
015200870514     C                   EXSR      DISPON
015300870514     C                   EXSR      ASSORD
015400870518     C                   END
015500870518     C                   GOTO      SU
015600870515     C     FINE          TAG
015700870515     C                   SETON                                        LR
015800870520     C**********************************************************
015900870513     C     ORDINE        BEGSR
016000870916     C**----------------------------------------
016100870916     C** SE CAMBIA L'ORDINE CONTROLLO L'IMPORTO
016200870916     C** E LA PERCENTUALE RICHIESTA
016300870916     C**----------------------------------------
016400870520     C                   SETOFF                                       70
016500870515     C     VIDIMP        IFGT      0
016600870515     C                   EXSR      VALORE
016700870522     C                   END
016800870518     C  N70              DO
016900870515     C     VIDVAL        IFGT      0
017000870518     C                   EXSR      PERC
017100870515     C                   END
017200870518     C                   END
017300870520     C                   SETOFF                                       70
017400870916     C                   Z-ADD     0             FLGNA
017500870515     C                   ENDSR
017600870514     C******************************************************
017700870514     C     DSPMAG        BEGSR
017800870916     C**----------------------------------------
017900870916     C** CALCOLO DISPONIBILITA' DI MAGAZZINO
018000870916     C**----------------------------------------
018100870916     C                   Z-ADD     0             MATDSP
018200870514     C     SMPQGF        SUB       SMPQIM        TOT              11 2
018300870514     C     TOT           SUB       SMPQIP        MATDSP           11 2
018400870514     C                   ENDSR
018500870514     C******************************************************
018600870514     C     ASSMAG        BEGSR
018700870928     C**----------------------------------------
018800870928     C** ASSEGNAZIONE A MAGAZZINO SE
018900870928     C** MATERIALE DISPONIBILE E' A SALDO
019000870928     C**----------------------------------------
019100870519     C                   Z-ADD     MOCQTA        MOCASS
019200870520     C                   MOVEL     'S'           MOCFLS
019300870514     C                   ADD       MOCASS        SMPQIP
019400990127     C                   UPDATE    tmmoc000
019500990127     C                   UPDATE    tmsmp000
019600870514     C                   ENDSR
019700870514     C******************************************************
019800870514     C     DISPON        BEGSR
019900870928     C**----------------------------------------
020000870928     C** ASSEGNAZIONE A MAGAZZINO SE
020100870928     C** MATERIALE DISPONIBILE E' A CONTO
020200870928     C**----------------------------------------
020300870514     C                   Z-ADD     MATDSP        MOCASS
020400870514     C                   ADD       MOCASS        SMPQIP
020500870520     C                   MOVEL     '1'           FLGNA             1 0
020600870915     C                   MOVEL     *BLANKS       MOCFLS
020700990127     C                   UPDATE    tmmoc000
020800990127     C                   UPDATE    tmsmp000
020900870514     C                   ENDSR
021000870514     C******************************************************
021100870514     C     ASSORD        BEGSR
021200870928     C**----------------------------------------
021300870928     C** CALCOLO E MEMORIZZO IL VALORE RIGA
021400870928     C** ASSEGNATO E IL VALORE RIGA ORDINATO
021500870928     C**----------------------------------------
021600870916     C                   Z-ADD     0             RIGASS
021700870916     C                   Z-ADD     0             RIGORD
021800870515     C                   Z-ADD     MOCPRZ        PRENET           11 2
021900870915     C     PRENET        MULT      MOCSC1        SCONTO           13 2
022000870515     C     SCONTO        DIV       100           SCONTO
022100870515     C     PRENET        SUB       SCONTO        PRENET
022200870515     C     PRENET        MULT      MOCSC2        SCONTO
022300870515     C     SCONTO        DIV       100           SCONTO
022400870515     C     PRENET        SUB       SCONTO        PRENET
022500870515     C     PRENET        MULT      MOCSC3        SCONTO
022600870515     C     SCONTO        DIV       100           SCONTO
022700870515     C     PRENET        SUB       SCONTO        PRENET
022800870916     C     PRENET        IFGT      0
022900870916     C     MOCASS        MULT(H)   PRENET        RIGASS           13 2          VALORE ASS.
023000870916     C     MOCQTA        MULT(H)   PRENET        RIGORD           13 2          VALORE VEN
023100870916     C                   END
023200870915     C                   ADD       RIGASS        IMPMER           13 2
023300870915     C                   ADD       RIGORD        VALORD           13 2
023400870515     C                   ENDSR
023500870515     C******************************************************
023600870515     C     VALORE        BEGSR
023700870928     C**----------------------------------------
023800870928     C** SE L'IMPORTO ASSEGNATO è MINORE DELL'IM-
023900870928     C** PORTO DIGITATO A VIDEO VA BENE SOLO
024000870928     C** SE E' A SALDO, SE NO DEVE ESSERE MAGGIORE
024100870928     C**----------------------------------------
024200870515     C     IMPMER        IFLT      VIDIMP
024300870518     C     FLGNA         IFEQ      1
024400870518     C                   SETON                                        70
024500870518     C   70              EXSR      ANNUL
024600870515     C                   END
024700870518     C                   END
024800870515     C                   ENDSR
024900870515     C******************************************************
025000870515     C     ANNUL         BEGSR
025100870928     C**----------------------------------------
025200870928     C** ANNULLO ASSEGNAZIONE SULLE RIGHE DELL'OR-
025300870928     C** DINE SE IL VALORE ASSEGNATO è MINORE DEL
025400870928     C** VALORE A VIDEO E SE HO ASSEGNATO DELLE
025500870928     C** RIGHE NON A SALDO
025600870928     C**----------------------------------------
025700870515     C                   MOVEL     RCPRE         RCDS
025800990127     C     KEYMOC        SETLL     tmmoc000                               11
025900870515     C     STAR          TAG
026000990127     C                   READ      tmmoc000                               10
026100870917     C   10              GOTO      END1
026200870917     C     MOCFAS        CABNE     'OR'          STAR
026300870917     C     MOCTRG        CABNE     '1'           STAR
026400870917     C     MOCSOS        CABNE     *BLANKS       STAR
026500870917     C     MOCASS        CABNE     0             STAR
026600990127     C     MOCDCP        CABGT     viddcpg       STAR
026700870917     C*SE NON TROVO IL MATERIALE SULL'ANAGRIFICO MATERIALI SEGNALO
026800870917     C*ERRORE E SALTO LA RIGA
026900990127     C     KEYAN         CHAIN     tmamp000                           03
027000870917     C   03              GOTO      STAR
027100870917     C     AMPAMT        CABNE     *BLANKS       STAR
027200870916     C     RCDS          IFNE      RCPRE
027300870515     C                   GOTO      END1
027400870515     C                   END
027500990127     C     KEYSAL        CHAIN     tmsmp000                           05
027600870515     C     SMPQIP        SUB       MOCASS        SMPQIP
027700870520     C                   Z-ADD     0             MOCASS
027800870915     C                   MOVEL     *BLANKS       MOCFLS
027900990127     C                   UPDATE    tmmoc000
028000990127     C                   UPDATE    tmsmp000
028100870515     C                   GOTO      STAR
028200870515     C     END1          ENDSR
028300870515     C******************************************************
028400870518     C     PERC          BEGSR
028500870928     C**----------------------------------------
028600870928     C** CALCOLO PERCENTUALE SUL VALORE ASSEGNATO
028700870928     C** E SULLA QUANTITA' ORDINATA E SE E' MINORE
028800870928     C** DI QUELLA RICHIESTA ANNULLO L'ASSEGNAZIONE
028900870928     C**----------------------------------------
029000870520     C                   Z-ADD     0             VALASS
029100870520     C                   Z-ADD     0             VALPER
029200870916     C     VALORD        IFGT      0
029300870915     C     IMPMER        MULT      100           VALASS           15 2
029400870915     C     VALASS        DIV(H)    VALORD        VALPER           15 2
029500870916     C                   END
029600870518     C     VALPER        IFLT      VIDVAL
029700870518     C                   EXSR      ANNUL
029800870518     C                   END
029900870518     C                   ENDSR
030000871229     C******************************************************
030100870520     C     ERRORE        BEGSR
030200871229     C**----------------------------------------
030300871229     C*SE IL CODICE MATERIALE NON LO TROVO FACCIO
030400871229     C*LA STAMPA E SALTO LA RIGA
030500871229     C**----------------------------------------
030600871229     C  N88              OPEN      OCPRTSTD
030700871229     C  N88              SETON                                        88
030800870520     C                   EXCEPT    TESTA
030900870520     C                   EXCEPT    ERR1
031000870520     C                   ENDSR
031100990127     C*----------------------------------------------------*
031200990127     C* Reperimento dati società
031300990127     C*----------------------------------------------------*
031400990127     C     REPSOC        BEGSR
031500990127     C*
031600990127     C                   CALLB     'XSOC'
031700990127     C                   PARM                    TIPXSC            6
031800990127     C                   PARM      socpas        SOCXSC            3
031900990127     C                   PARM                    CDSXSC            9 0
032000990127     C                   PARM                    MODXSC            3
032100990127     C                   PARM      *blanks       RTNXSC            1
032200990127     C                   PARM                    XSOCDS
032300990127     C                   PARM                    KPJBA
032400990127     C*
032500990127     C                   ENDSR
032600870520     O*--------------------------------------------------------------*
032700870520     OOCPRTSTD  E            TESTA            01
032800870520     O                       DESDIT              20
032900990127     O                       woggi              118 '  /  /    '
033000870520     O          E            TESTA            03
033100870520     O                                           53 'SIMULAZIONE'
033200870520     O                                           67 'ASSEGNAZIONE'
033300870520     O          E            ERR1        2
033400870520     O                       MOCCLI               8
033500870520     O                       MOCNOR        Z     15
033600990127     O                       MOCDOR              26 '    /  /  '
033700870521     O                       MOCMAG              35
033800870928     O                       MOCMAT              49
033900870928     O                     03                    74 'CODICE MATERIALE INESIST'
034000870928     O                     03                    98 'ENTE SU ANAGRAFICO MATER'
034100870928     O                     03                   106 'IE PRIME'
034200870928     O                     04                    74 'CODICE MATERIALE INESIST'
034300870928     O                     04                    98 'ENTE SU ANAGRAFICO SALDI'

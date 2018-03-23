000100981211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300951124     H*PARMS CVTOPT(*DATETIME)
000400951127     H DECEDIT('0,') DATEDIT(*DMY/)
000500951124     F*---------------------------------------------------------------*
000600951124     F*         STAMPA DI SERVIZIO MOVIMENTI ESTRATTI                 *
000700951124     F*                 RIEPILOGHI VERSAMENTI                         *
000800000000     F*---------------------------------------------------------------*
000900951124     FND77R01L  IF   E           K DISK
001000951124     F*------------
001100951124     FQSYSPRT   O    F  132        PRINTER OFLIND(*IN39)
001200951124     D*---------------------------------------------------------------*
001300951124     D KPJBA         E DS
001400951124     D*------------
001500951124     D ndcr70ds      E DS
001600951124     D*------------
001700951124     D* Dati di ambiente ottenuti da XSOC
001800951124     D SOC001        E DS                  EXTNAME(XSOC001DS)
001900951124     D*------------
002000951124     D* DS Interna per dati di output di XSOC
002100951124     D XSOCDS          S           1000
002200951124     D*------------
002300951127     D* Reperimento nome PGM
002400951127     D STATUS         SDS           333
002500951127     D  DSPGM            *PROC
002600951127     D  PARMS            *PARMS
002700951127     D*------------
002800000000     D SK              S             66    DIM(2) CTDATA PERRCD(1)              STAMPA
002900951124     D*------------
003000951124     D DatMin          C                   CONST('0001-01-01')
003100000117C1309D ZDateISO        S               D
003200000000     C*----------------------------------------------------*
003300000000     C     *ENTRY        PLIST                                                  *
003400000000     C                   PARM                    KPJBA                          *
003500951124     C*
003600951127     C                   MOVEL     KPJBU         ndcr70ds
003700951124     C*
003800000000     C     KEY77R        KLIST                                                  *
003900951124     C                   KFLD                    XSCSOC                         *
004000951124     C                   KFLD                    R77ACF                         *
004100951124     C*
004200890419     C     KEY771        KLIST                                                  *
004300951124     C                   KFLD                    XSCSOC                         *
004400951124     C                   KFLD                    R77ACF                         *
004500951124     C                   KFLD                    R77QUA                         *
004600951124     C*
004700951124     C* Chiamata a xsoc
004800951124     C                   move      socR70        SOCXSC
004900951124    >C                   MOVEL     'SOC001'      TIPXSC
005000951124     C                   CALL      'XSOC'
005100951124     C                   PARM                    TIPXSC            6
005200951124     C                   PARM                    SOCXSC            3
005300951124     C                   PARM                    CDSXSC            9 0
005400951124     C                   PARM                    MODXSC            3
005500951124     C                   PARM      *BLANKS       RTNXSC            1
005600951124     C                   PARM                    XSOCDS
005700951124     C                   PARM                    KPJBA
005800951124   1 C     RTNXSC        IFNE      '1'
005900951124    >C                   MOVEL     XSOCDS        SOC001
006000951124   1xC                   ELSE
006100951124     C                   GOTO      FINE
006200951124   1-C                   ENDIF
006300951127     C*
006400951127     C* Valorizzazione campi univoci testate
006500951127     C                   MOVEL     XSCDSI        NOMDIT           20
006600951127     C                   MOVEL     DSPGM         NOMPGM           10
006700951127     C* Reperisco nome sistema informatico
006800951127     C                   CALL      'XNETA'                              21
006900951127     C                   PARM                    NOMSYS            8
007000951124     C*
007100951124     C                   seton                                        39
007200000000     C*----------------------------------------------------*
007300951124     C                   z-add     annoR70       R77ACF
007400951124     C     KEY77R        SETLL     ND77R01L                                     *
007500951124     C     KEY77R        READE     ND77R01L                               90    *
007600890419     C   90              GOTO      FINE                                         *
007700951124     C*
007800951124     C     flagDR70      IFEQ      *on                                          *
007900040713C1770C                   MOVE      'ST'          W77QUA            2            *
008000020415     C                   MOVE      'AU'          R77QUA                         *
008100951124     C     KEY771        SETLL     ND77R01L                                     *
008200951124     C     KEY771        READE     ND77R01L                               90    *
008300890419     C  N90              EXSR      ELAB                                         *
008400951124     C                   ENDIF
008500951124     C*
008600951124     C* Stampa totali generali
008700951124     C     *in39         IFEQ      *on                                          *
008800951127     C                   time                    ORA
008900890419     C                   EXCEPT    TESTA                                        *
009000951124     C                   setoff                                       39        *
009100951124     C                   ENDIF                                                  *
009200890419     C                   EXCEPT    TOTALE                                       *
009300951124     C*
009400890419     C     FINE          TAG                                                    *
009500890419     C                   SETON                                        LR        *
009600951124     C*
009700890419     C*----------------------------------------------------*
009800890419     C*  ELABORAZIONE DETTAGLIO                            *
009900890419     C*----------------------------------------------------*
010000890419     C     ELAB          BEGSR                                                  *
010100010525     C                   Z-ADD     0             QUAIMS           15 2          *
010200010525     C                   Z-ADD     0             QUARAC           15 2          *
010300010525     C                   Z-ADD     0             QUAIMV           15 2          *
010301150514R183 C                   Z-ADD     0             QUAIRO           12 2
010400890419     C                   MOVE      R77QUA        SAVQUA            2            *
010500951124     C*
010600890419     C                   DO        *HIVAL                                       *
010700951124     C     R77DQV        IFNE      DatMin
010800951124     C                   CALLb     'XDT4000'
010900951124     C                   PARM      R77DQV        XDTAMG           10
011000951124     C     DQV           PARM                    XDTGMA            6 0
011100951124     C                   PARM      2             XDTSTA            1 0
011200951124     C                   ELSE
011300951124     C                   Z-ADD     0             DQV               6 0
011400951124     C                   ENDIF
011500160217R215 X***                Z-ADD     R77IMS        IMS              15 2
011600160217R215 X***                Z-ADD     R77RAC        RAC              15 2
011700160217R215 X***                Z-ADD     R77IMV        IMV              15 2
011701160217R215 C                   Z-ADD     R77IMS        IMS              14 2
011702160217  "  C                   Z-ADD     R77RAC        RAC              14 2
011703160217R215 C                   Z-ADD     R77IMV        IMV              14 2
011704150514R183 C                   Z-ADD     R77IRO        IRO              12 2
011800951124     C*
011900951124     C* Totalizzo importi parziali per quadro
012000010525     C                   ADD       R77IMS        QUAIMS           15 2          *
012100010525     C                   ADD       R77RAC        QUARAC           15 2          *
012200010525     C                   ADD       R77IMV        QUAIMV           15 2          *
012201150514R183 C                   ADD       R77IRO        QUAIRO           12 2          *
012300951124     C*
012400951124     C* Totalizzo importi generali
012500010525     C                   ADD       R77IMS        TOTIMS           15 2          *
012600010525     C                   ADD       R77RAC        TOTRAC           15 2          *
012700010525     C                   ADD       R77IMV        TOTIMV           15 2          *
012701150514R183 C                   ADD       R77IRO        TOTIRO           12 2
012800951124     C*
012900951124     C* Stampa dettaglio
013000951127     C     *in39         IFEQ      *on                                          *
013100951127     C                   time                    ORA               6 0
013200000000     C                   EXCEPT    TESTA                                        *
013300951124     C                   setoff                                       39        *
013400951124     C                   ENDIF                                                  *
013500000000     C                   EXCEPT    DET                                          *
013600951124     C*
013700951124     C     KEY771        READE     ND77R01L                               90    *
013800951124     C  N90              ENDDO                                                  *
013900951124     C*
014000951124     C* Stampa totali parziali
014100951124     C     *in39         IFEQ      *on                                          *
014200951127     C                   time                    ORA
014300890419     C                   EXCEPT    TESTA                                        *
014400951124     C                   setoff                                       39        *
014500951124     C                   ENDIF                                                  *
014600890419     C                   EXCEPT    TOTQUA                                       *
014700951124     C*
014800890419     C                   ENDSR                                                  *
014900000117      *----------------------------------------------------
015000000117      * routine iniziale
015100000117      *----------------------------------------------------
015200000117     C     *INZSR        Begsr
015300000117C1309C     *DMY          Move      Udate         ZDateISO
015400000117C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
015500000117      *
015600000117     C                   Endsr
015700000000     O*----------------------------------------------------*
015701160204R211 O* Trasformati i campi importi da edtcde'2'a edcde'K'
015702160204R211 O*----------------------------------------------------*
015800951124     OQSYSPRT   E            TESTA            02
015900951127     O                       NOMDIT              21
016000951127     O                                           48 '***-------------------'
016100951127     O                                           70 '----------------------'
016200951127     O                                           92 '----------------------'
016300951127     O                                           96 '-***'
016400951127     O                       KNMUS              110
016500000117C1309O*****                  UDATE         Y    120
016600000117C1309O                       ZDATE         Y    120
016700951127     O                                          126 'Pag.'
016800951127     O                       PAGE          Z    131
016900951127     O          E            TESTA       1
017000951127     O                       NOMSYS               9
017100951127     O                       KNSIF               20
017200951127     O                                           27 '*'
017300020415     O                                           53 'Stampa di servizio rie'
017400020415     O                                           75 'pilogo versamenti per '
017500040713     O                                           93 'mod.770 quadro ST '
017600951127     O                                           96 '*'
017700951127     O                       NOMPGM             110
017800951127     O                       ORA                120 '0 :  :  '
017900000000     O          E            TESTA       1
018000951127     O                                           48 '***-------------------'
018100951127     O                                           70 '----------------------'
018200951127     O                                           92 '----------------------'
018300951127     O                                           96 '-***'
018400951127     O                                          110 'gruppo PRO'
018500000000     O          E            TESTA       2
018600000000     O                       SK(1)               66
018700000000     O                       SK(2)              132
018800000000     O          E            TESTA       1
018900951127     O                                            5 'Anno'
019000150508R183 X***                                        14 'Quadro'
019100150508  "  X***                                        36 'Importo'
019200150508  "  X***                                        48 'MM/AA corr'
019300150508  "  X***                                        69 'Ritenute'
019400150508  "  X***                                        76 'D/C'
019500150508  "  X***                                        88 'Versam.'
019600150508  "  X***                                       101 'N° boll.'
019700150508  "  X***                                       113 'Tributo'
019800150508R183 X***                                       132 'Imp.versato'
019801150508R183 O                                           12 'Quadro'
019802150508  "  O                                           32 'Importo'
019803150508  "  O                                           44 'MM/AA corr'
019804150508  "  O                                           59 'Ritenute'
019805150508  "  O                                           64 'D/C'
019806150508  "  O                                           74 'Versam.'
019807150508  "  O                                           87 'N° boll.'
019808150508  "  O                                           99 'Tributo'
019809150508  "  O                                          116 'Imp.versato'
019810150514R183 O                                          132 'Int.Rav.Op.'
019900000000     O          E            TESTA       1
020000000000     O                       SK(1)               66
020100000000     O                       SK(2)              132
020200000000     O*
020300000000     O          E            DET         1
020400000000     O                       R77ACF        Z      5
020500040713C1770O***                    R77QUA              12
020601150508R183 X*** ex C1770           W77QUA              12
020700150508  "  X***                    IMS           2     36
020800150508  "  X***                    R77MAC        Y     45
020900150508  "  X***                    RAC           2     69
021000150508  "  X***                    R77TPQ              75
021100150508  "  X***                    DQV           Y     88
021200150508  "  X***                    R77NQV             105
021300150508  "  X***                    R77TRI             110
021400150508  "  X***                    R77PTR             112
021500150508R183 X***                    IMV           2    132
021501150508R183 O                       W77QUA              10
021502160204  "  O                       IMS           K     32
021503150508  "  O                       R77MAC        Y     38
021504160204  "  O                       RAC           K     59
021505150508  "  O                       R77TPQ              63
021506150508  "  O                       DQV           Y     74
021507150508  "  O                       R77NQV              91
021508150508  "  O                       R77TRI              96
021509150508  "  O                       R77PTR              98
021510160204  "  O                       IMV           K    116
021511160204R183 O                       IRO           K    132
021600890331     O*
021700890419     O          E            TOTQUA      2
021800150508R183 X***                                        13 'Totale quadro'
021900040713C1770O***                    SAVQUA              16
022000150508R183 X*** Ex C1770           W77QUA              16
022100150508  "  X***                    QUAIMS        2     36
022200150508  "  X***                    QUARAC        2     69
022300150508R183 X***                    QUAIMV        2    132
022303160217R215 X*** Ex R183                                09 'T. quadro'
022304160217R215 X*** Ex R183            W77QUA              12
022305160217R215 O                                           08 'T.quadro'
022306160217R215 O                       W77QUA              11
022307160204R183 O                       QUAIMS        K     32
022308160204  "  O                       QUARAC        K     59
022309160204  "  O                       QUAIMV        K    116
022310160204R183 O                       QUAIRO        K    132
022400890419     O          E            TOTQUA      0
022500150508R183 X***                                        13 'Totale quadro'
022600040713C1770O***                    SAVQUA              16
022700150508R183 X*** Ex C1770           W77QUA              16
022800150508  "  X***                    QUAIMS        2     36
022900150508R183 X***                    QUARAC        2     69
023000150508R183 X***                    QUAIMV        2    132
023003160217R215 X*** Ex R183                                09 'T. quadro'
023004160217R215 X*** Ex R183            W77QUA              12
023005160217R215 O                                           08 'T.quadro'
023006160217R215 O                       W77QUA              11
023007160204R183 O                       QUAIMS        K     32
023008160204  "  O                       QUARAC        K     59
023009160204  "  O                       QUAIMV        K    116
023010160204R183 O                       QUAIRO        K    132
023100890419     O          E            TOTQUA      1
023200890419     O                                            1 ' '
023300951124     O*
023400150529     O          E            TOTALE      2
023500150529R183 X***                                        15 'TOTALE GENERALE'
023600150529  "  X***                    TOTIMS        2     36
023700150529  "  X***                    TOTRAC        2     69
023800160217R183 X***                    TOTIMV        2    132
023801160217R215 X*** Ex R183                                12 'TOT.GENERALE'
023802160217R215 O                                           10 'T.GENERALE'
023803160217R183 O                       TOTIMS        K     32
023804160204  "  O                       TOTRAC        K     59
023805160204  "  O                       TOTIMV        K    116
023806160204R183 O                       TOTIRO        K    132
023900150529R183 O          E            TOTALE      0
024000150529R183 X***                                        15 'TOTALE GENERALE'
024100150508  "  X***                    TOTIMS        2     36
024200150508  "  X***                    TOTRAC        2     69
024300150508R183 X***                    TOTIMV        2    132
024302160217R215 X*** Ex R183                                12 'TOT.GENERALE'
024303160217R215 O                                           10 'T.GENERALE'
024304160217R183 O                       TOTIMS        K     32
024305160204  "  O                       TOTRAC        K     59
024306160204  "  O                       TOTIMV        K    116
024307160204R183 O                       TOTIRO        K    132
024400000000**
024500000000------------------------------------------------------------------
024600000000------------------------------------------------------------------

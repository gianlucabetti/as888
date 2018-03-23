000100990119     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990119     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F* ANG99AR  *-AN/38-*-------------------------------------------*
000400000000     F*------------------*                                           *
000500000000     F*       - ESECUZIONE STAMPA ANAGRAFICO MATERIE PRIME -         *
000600000000     F*       ----------------------------------------------         *
000700000000     F*                                                   AZN.: G99A *
000800990119     F*         FILE : tmamp02L - ANTAB00F -
000900000000     F*                                                              *
001000000000     F*         PGM. : RIC001R  - RICERCA INTESTAZIONE DITTA         *
001100000000     F*                                                              *
001200990119     F*---- COMMENTI : tmamp02L VIENE OVERRAIDATO CON = tmamp01L/    *
001300990119     F*                tmamp09L/tmamp10L/tmamp05L                    *
001400000000     F*                                                              *
001500000000     F*--------------------------------------------------------------*
001600990119     Ftmamp02L  IF   E           K DISK
001700000000     F*---------
001800000000     FANTAB00F  IF   E           K DISK
001900000000     F*---------
002000000000     FANPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
002100000000     F                                     USROPN
002200000000     D*--------------------------------------------------------------*
002300000000     D TS1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
002400000000     D TS2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
002500000000     D TS3             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 3
002600000000     D*--------------------------------------------------------------*
002700860905     D DT1             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 1
002800860905     D DTY             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 1 BIS
002900000000     D DT2             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 2
003000000000     D DT3             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 3
003100000000     D DT4             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 4
003200000000     D DT5             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 5
003300000000     D DT6             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 6
003400000000     D DT7             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 7
003500000000     D DT8             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 8
003600000000     D DT9             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 9
003700000000     D DTA             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 10
003800000000     D DTB             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 11
003900000000     D DTC             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 12
004000000000     D DTD             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 13
004100000000     D DTE             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 14
004200000000     D DTF             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 15
004300000000     D DTG             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 16
004400000000     D DTH             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 17
004500000000     D DTL             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 18
004600000000     D DTM             S             66    DIM(2) CTDATA PERRCD(1)              *DETTAGLIO 19
004700000000     D*--------------------------------------------------------------*
004800000000     D ER1             S             66    DIM(2) CTDATA PERRCD(1)              *ERRORI
004900000000     D*--------------------------------------------------------------*
005000000000     D KPJBA         E DS
005100000000     D*---- PASSAGGIO PARAMETRI
005200871013     D  ARTINF               285    296
005300871013     D  ARTSUP               297    308
005400871013     D  LIMART               285    308
005500000000     D  CLAINF               261    263
005600000000     D  CLASUP               264    266
005700000000     D  LIMCLA               261    266
005800000000     D  SOTINF               267    271
005900000000     D  SOTSUP               272    276
006000000000     D  LIMSOT               267    276
006100000000     D  CATINF               277    278
006200000000     D  CATSUP               279    280
006300000000     D  LIMCAT               277    280
006400000000     D  TIPSTA               281    281
006500000000     D  ORDINE               282    282
006600860905     D  UBICAZ               283    283
006700860924     D  ANNULL               284    284
006800860905     D                 DS
006900131030     D  AMPfil                 1     15
007000131030     D  UBI                    1     15
007100131030     D                                     DIM(15)                              *UBICAZIONE
007200990119     D soc001        E DS                  EXTNAME(xsoc001ds)
007300990119     D xsocds          DS          1000
007400990119     d dataiso         s               d   datfmt(*iso)
007500990119     d dataeur         s               d   datfmt(*eur)
007600000000     C*----------------------------------------------------*
007700000000     C     *ENTRY        PLIST                                                  *
007800000000     C                   PARM                    KPJBA                          *
007900990119     C*---------- RICERCA DITTA :
008000990119     C                   MOVEL     'SOC001'      TIPXSC
008100990119     C                   MOVEL     *blank        SOCXSC
008200990119     C                   EXSR      REPSOC
008300990119     C     RTNXSC        IFNE      '1'
008400990119     C                   MOVEL     XSOCDS        SOC001
008500990119     C                   MOVEL     xscrgs        desdit           20
008600990119     c                   else
008700990119     c                   seton                                        80
008800990119     c                   end
008900000000     C*--- CHIAVE PER FILE "ANTAB00F":                     *
009000000000     C     KEYANT        KLIST                                                  *
009100000000     C                   KFLD                    TABGRU                         *
009200000000     C                   KFLD                    TABCOD                         *
009300860924     C*--- ROVESCIA UDATE                                  *
009400990119     c                   time                    como14           14 0
009500990119     c                   move      como14        woggi             8 0
009600990119     c                   move      woggi         dataeur
009700990119     c                   move      dataeur       dataiso
009800990119     c                   move      dataiso       woggig            8 0
009900000000     C*----------------------------------------------------*
010000000000     C*--- SCELTA TIPO STAMPA :                            *
010100000000     C     LIMART        CABNE     *BLANKS       LEGGI                1010      *
010200000000     C     LIMCLA        CABNE     *BLANKS       LEGGI                2020      *
010300000000     C     LIMSOT        CABNE     *BLANKS       LEGGI                3030      *
010400000000     C     LIMCAT        CABNE     *BLANKS       LEGGI                4040      *
010500000000     C*--- IMPOSTAZIONI INIZIALI :                         *
010600000000     C     LEGGI         TAG                                                    *
010700000000     C                   Z-ADD     0             PAGE              4 0          *
010800000000     C                   Z-ADD     51            PAGE1             2 0          *
010900000000     C                   Z-ADD     4             PAGE2             1 0          *
011000000000     C*--- CONTROLLO TIPO STAMPA :                         *
011100000000     C     TIPSTA        COMP      'C'                                5050      *
011200000000     C*--- CICLO DI LETTURA :                              *
011300000000     C                   DO        *HIVAL                                       *
011400860924     C                   SETOFF                                           12    *
011500990119     C                   READ      tmamp02L                               99    *
011600000000     C  N99              DO                                                     *
011700860905     C*--- RICERCA CODICE UBICAZIONE  :                    *
011800860905     C     UBICAZ        IFNE      '9'                                          *
011900860905     C     UBICAZ        LOOKUP    UBI                                    11    *
012000860924     C  N11              GOTO      NOSTAM                                       *
012100860905     C                   END                                                    *
012200860924     C*--- CONTROLLA SE ARTICOLO NON PIU GESTITO ----------*
012300860924     C     AMPDFG        IFNE      *ZEROS                                       *
012400990119     C     AMPDFG        COMP      woggig                               12      *
012500860924     C   12ANNULL        CABEQ     'N'           NOSTAM                         *
012600860924     C                   END                                                    *
012700860925     C*----------------------------------------------------*
012800860925      /EJECT
012900860925     C*--- CONTROLLO LIMITI SUPERIORI ---------------------*
013000000000     C   10AMPMAT        CABGT     ARTSUP        FINE                           * ST. FINALI
013100000000     C   20AMPCLM        CABGT     CLASUP        FINE                           * ST. FINALI
013200000000     C   30AMPSCM        CABGT     SOTSUP        FINE                           * ST. FINALI
013300000000     C   40AMPCTF        CABGT     CATSUP        FINE                           * ST. FINALI
013400000000     C                   SETON                                        98        *
013500000000     C*--- CONTROLLO CLASSE PER SALTO A PAGINA NUOVA :     *
013600000000     C     *IN(20)       CABNE     '1'           NONCLM                         *
013700000000     C     AMPCLM        IFNE      COMCLM                                       *
013800000000     C   50              Z-ADD     51            PAGE1                          *
013900000000     C  N50              Z-ADD     4             PAGE2                          *
014000000000     C                   MOVE      AMPCLM        COMCLM            3            *
014100000000     C                   END                                                    *
014200000000     C     NONCLM        TAG                                                    *
014300000000     C*--- CONTROLLO S/CLASSE PER SALTO A PAGINA NUOVA :   *
014400000000     C     *IN(30)       CABNE     '1'           NONSCM                         *
014500000000     C     AMPSCM        IFNE      COMSCM                                       *
014600000000     C   50              Z-ADD     51            PAGE1                          *
014700000000     C  N50              Z-ADD     4             PAGE2                          *
014800000000     C                   MOVE      AMPSCM        COMSCM            5            *
014900000000     C                   END                                                    *
015000000000     C     NONSCM        TAG                                                    *
015100000000     C*--- CONTROLLO CAT.FISCALE PER SALTO A PAGINA NUOVA: *
015200000000     C     *IN(40)       CABNE     '1'           NONCTF                         *
015300000000     C     AMPCTF        IFNE      COMCTF                                       *
015400000000     C   50              Z-ADD     51            PAGE1                          *
015500000000     C  N50              Z-ADD     4             PAGE2                          *
015600000000     C                   MOVE      AMPCTF        COMCTF            2            *
015700000000     C                   END                                                    *
015800000000     C     NONCTF        TAG                                                    *
015900860905     C   50              GOTO      STAMPA                                       * FINE ST.PARZ.
016000860905     C*-                                                   *
016100000000     C*--- RICERCA DESCRIZIONE CLASSE MERC. :              *
016200000000     C                   MOVE      '155'         TABGRU                         *
016300000000     C                   MOVE      *ZEROS        TABCOD                         *
016400000000     C                   MOVE      AMPCLM        TABCOD                         *
016500000000     C                   EXSR      TABFIL                                       *
016600000000     C  N97              MOVEL     TABDES        DESCLM           25            *
016700000000     C   97              MOVEL     *ALL'_'       DESCLM                         *
016800000000     C*--- RICERCA DESCRIZIONE S/CLASSE MERC. :            *
016900000000     C                   MOVE      '156'         TABGRU                         *
017000000000     C                   MOVE      *ZEROS        TABCOD                         *
017100000000     C                   MOVE      AMPSCM        TABCOD                         *
017200000000     C                   EXSR      TABFIL                                       *
017300000000     C  N97              MOVEL     TABDES        DESSCM           25            *
017400000000     C   97              MOVEL     *ALL'_'       DESSCM                         *
017500860925     C*--- RICERCA DESCRIZIONE CATEGORIA FISCALE ----------*
017600000000     C                   MOVE      '163'         TABGRU                         *
017700000000     C                   MOVE      *ZEROS        TABCOD                         *
017800000000     C                   MOVE      AMPCTF        TABCOD                         *
017900000000     C                   EXSR      TABFIL                                       *
018000000000     C  N97              MOVEL     TABDES        DESCTF           25            *
018100000000     C   97              MOVEL     *ALL'_'       DESCTF                         *
018200000000     C  N97              MOVEL     TABDES        DESFIS           19            *
018300000000     C   97              MOVEL     *ALL'_'       DESFIS                         *
018400860924     C*--- ROVESCIA DATE INIZIO E FINE GESTIONE            *
018500990119     c                   if        ampdig > 0
018600990119     c                   move      ampdig        dataiso
018700990119     c                   move      dataiso       dataeur
018800990119     c     *dmy          move      dataeur       datdig            6 0
018900990119     c                   end
019000860924     C*-                                                   *
019100990119     c                   if        ampdfg > 0
019200990119     c                   move      ampdfg        dataiso
019300990119     c                   move      dataiso       dataeur
019400990119     c     *dmy          move      dataeur       datdfg            6 0
019500990119     c                   end
019600000000     C*--- RICERCA DESCRIZIONE UN.MIS.ANAGRAF.:            *
019700000000     C                   MOVE      '154'         TABGRU                         *
019800000000     C                   MOVE      *ZEROS        TABCOD                         *
019900000000     C                   MOVE      AMPUMA        TABCOD                         *
020000000000     C                   EXSR      TABFIL                                       *
020100000000     C  N97              MOVEL     TABDES        DESUMA           25            *
020200000000     C   97              MOVEL     *ALL'_'       DESUMA                         *
020300000000     C*--- RICERCA DESCRIZIONE UN.MIS.STOCCAGGIO :         *
020400000000     C                   MOVE      '154'         TABGRU                         *
020500000000     C                   MOVE      *ZEROS        TABCOD                         *
020600000000     C                   MOVE      AMPUMS        TABCOD                         *
020700000000     C                   EXSR      TABFIL                                       *
020800000000     C  N97              MOVEL     TABDES        DESUMS           25            *
020900000000     C   97              MOVEL     *ALL'_'       DESUMS                         *
021000000000     C*--- RICERCA DESCRIZIONE UN.MIS.ACQUISTO :           *
021100000000     C                   MOVE      '154'         TABGRU                         *
021200000000     C                   MOVE      *ZEROS        TABCOD                         *
021300000000     C                   MOVE      AMPUMQ        TABCOD                         *
021400000000     C                   EXSR      TABFIL                                       *
021500000000     C  N97              MOVEL     TABDES        DESUMQ           25            *
021600000000     C   97              MOVEL     *ALL'_'       DESUMQ                         *
021700000000     C*--- RICERCA DESCRIZIONE UN.MIS.VENDITA :            *
021800000000     C                   MOVE      '154'         TABGRU                         *
021900000000     C                   MOVE      *ZEROS        TABCOD                         *
022000000000     C                   MOVE      AMPUMV        TABCOD                         *
022100000000     C                   EXSR      TABFIL                                       *
022200000000     C  N97              MOVEL     TABDES        DESUMV           25            *
022300000000     C   97              MOVEL     *ALL'_'       DESUMV                         *
022400860925     C*--- RICERCA DESCRIZIONE LISTINO --------------------*
022500000000     C                   MOVE      '161'         TABGRU                         *
022600000000     C                   MOVE      *ZEROS        TABCOD                         *
022700000000     C                   MOVE      AMPLIS        TABCOD                         *
022800000000     C                   EXSR      TABFIL                                       *
022900000000     C  N97              MOVEL     TABDES        DESLIS           25            *
023000000000     C   97              MOVEL     *ALL'_'       DESLIS                         *
023100000000     C*--- RICERCA DESCRIZIONE IMBALLO :                   *
023200000000     C                   MOVE      '158'         TABGRU                         *
023300000000     C                   MOVE      *ZEROS        TABCOD                         *
023400000000     C                   MOVE      AMPIMB        TABCOD                         *
023500000000     C                   EXSR      TABFIL                                       *
023600000000     C  N97              MOVEL     TABDES        DESIMB           25            *
023700000000     C   97              MOVEL     *ALL'_'       DESIMB                         *
023800000000     C*--- RICERCA DESCRIZIONE MAGAZZINO :                 *
023900860905     C                   MOVE      '153'         TABGRU                         *
024000000000     C                   MOVE      *ZEROS        TABCOD                         *
024100000000     C                   MOVE      AMPMAG        TABCOD                         *
024200000000     C                   EXSR      TABFIL                                       *
024300000000     C  N97              MOVEL     TABDES        DESMAG           25            *
024400000000     C   97              MOVEL     *ALL'_'       DESMAG                         *
024500990119     c                   if        amptar > 0
024600990119     c                   move      amptar        dataiso
024700990119     c                   move      dataiso       dataeur
024800990119     c     *dmy          move      dataeur       comtar            6 0
024900990119     c                   end
025000000000     C*--- GESTIONE STAMPA :                               *
025100000000     C     STAMPA        TAG                                                    *
025200000000     C                   EXSR      RUTSTA                                       *
025300000000     C                   END                                                    *
025400860924     C     NOSTAM        TAG                                                    *
025500000000     C  N99              END                                                    *
025600000000     C*----------------------------------------------------*
025700000000     C*     USCITA PROGRAMMA                               *
025800000000     C*----------------------------------------------------*
025900000000     C     FINE          TAG                                                    *
026000000000     C                   SETON                                        LR        *
026100000000     CLRN60
026200000000     CANN80              OPEN      ANPRTSTD                                     *
026300000000     CLRN98
026400000000     CANN80              EXCEPT    TESTA                                        *
026500000000     CLRN98
026600000000     CANN80              EXCEPT    ERRORE                                       *
026700000000     CLRN50
026800000000     CAN 70              EXCEPT    TESTA                                        *
026900000000     C***------------------------------------------------***
027000000000     C**--- EMISSIONE STAMPA TESTATA/DETTAGLIO -----------**
027100000000     C***------------------------------------------------***
027200000000     CSR   RUTSTA        BEGSR                                                  *
027300000000     C  N60              OPEN      ANPRTSTD                                     *
027400000000     C  N60              SETON                                        60        *
027500000000     C   50PAGE1         CABLT     50            DETAIL                         *ST.PARZIALIZZ.
027600000000     C  N50PAGE2         CABLT     3             DETAIL                         *ST.COMPLETA
027700000000     C                   EXCEPT    TESTA                                        *
027800000000     C   50              Z-ADD     0             PAGE1                          *
027900000000     C  N50              Z-ADD     0             PAGE2                          *
028000000000     C                   SETOFF                                       70        *
028100000000     C     DETAIL        TAG                                                    *
028200000000     C   50              EXCEPT    DETPAR                                       *DET.PARZIALIZ.
028300000000     C  N50              EXCEPT    DETCOM                                       *DET.COMPLETO
028400000000     C  N50PAGE2         COMP      3                                      70    *
028500000000     C     NRARTI        ADD       1             NRARTI            5 0          *
028600000000     CSR                 ENDSR                                                  *
028700000000     C***------------------------------------------------***
028800000000     C**--- RICERCA DESCRIZIONI TABELLE ------------------**
028900000000     C***------------------------------------------------***
029000000000     CSR   TABFIL        BEGSR                                                  *
029100000000     C     KEYANT        CHAIN     ANTAB00F                           97        *
029200000000     CSR                 ENDSR                                                  *
029300000000     C***------------------------------------------------***
029400990119     C*----------------------------------------------------*
029500990119     C* Reperimento dati società
029600990119     C*----------------------------------------------------*
029700990119     C     REPSOC        BEGSR
029800990119     C*
029900990119     C                   CALLB     'XSOC'
030000990119     C                   PARM                    TIPXSC            6
030100990119     C                   PARM      '201'         SOCXSC            3
030200990119     C                   PARM                    CDSXSC            9 0
030300990119     C                   PARM                    MODXSC            3
030400990119     C                   PARM      *blanks       RTNXSC            1
030500990119     C                   PARM                    XSOCDS
030600990119     C                   PARM                    KPJBA
030700990119     C*
030800990119     C                   ENDSR
030900000000     O*--------------------------------------------------------------*
031000000000     O*     TESTATA                                                  *
031100000000     O*--------------------------------------------------------------*
031200000000     OANPRTSTD  E            TESTA          1 01
031300000000     O                       TS1                132
031400000000     O                       DESDIT              20
031500990119     O                       woggi              120 '  /  /    '
031600000000     O                       PAGE          Z    132
031700000000     O          E            TESTA       0
031800000000     O                       TS2                132
031900000000     O                  10                       92 'PRODOTTO'
032000000000     O                  20                       96 'CLASSE MERC.'
032100000000     O                  30                       97 'S/CLASS.MERC.'
032200000000     O                  40                       97 'CATEG.FISCALE'
032300000000     O               98N10                      104 '-     -'
032400000000     O               98 20   AMPCLM             102
032500000000     O               98 30   AMPSCM             103
032600000000     O               98 40   AMPCTF             102
032700000000     O          E            TESTA          1
032800000000     O                  10                       92 'PRODOTTO'
032900000000     O                  20                       96 'CLASSE MERC.'
033000000000     O                  30                       97 'S/CLASS.MERC.'
033100000000     O                  40                       97 'CATEG.FISCALE'
033200000000     O               98N10                      104 '-     -'
033300000000     O               98 20   AMPCLM             102
033400000000     O               98 30   AMPSCM             103
033500000000     O               98 40   AMPCTF             102
033600000000     O          E            TESTA          2
033700000000     O                       TS3                132
033800000000     O                       KNMUS              132
033900860905     O          E    50 98   TESTA       1
034000000000     O                       DT1                132
034100860905     O          E    50 98   TESTA       1  3
034200860905     O                       DTY                132
034300990119      *
034400000000     O*     DETTAGLIO PARZIALIZZATO                                  *
034500860924     O          E    12      DETPAR      0  0
034600860924     O                                           12 '____________'
034700860925     O          E    12      DETPAR      0  0
034800860925     O                                           12 '____________'
034900880107     O          E            DETPAR         1
035000880216     O                       PAGE1                3
035100871013     O                       AMPMAT              13
035200000000     O                       AMPDMT              44
035300861211     O                                           45 '*'
035400871013     O                       AMPCLM              48
035500871013     O                                           49 '*'
035600871013     O                       AMPSCM              54
035700871013     O                                           55 '*'
035800871013     O                       AMPUMA              58
035900871013     O                       AMPUMS              62
036000871013     O                       AMPUMQ              66
036100871013     O                       AMPUMV              70
036200871013     O                                           71 '*'
036300131030     O                       AMPfil              86
036400131030     O                                           87 '*'
036500131030     O                       AMPLRO        4    100
036600131030     O                                          101 '*'
036700131030     O                       AMPLVM        4    114
036800131030     O                                          115 '*'
036900131030     O                       AMPGAP        Z    120
037000131030     O                                          121 '*'
037100990119     O*-
037200000000     O*     DETTAGLIO COMPLETO                                       *
037300000000     O          E            DETCOM      1  0
037400880216     O                       PAGE2                2
037500000000     O                       DT2                132
037600861211     O                       AMPMAT              36
037700000000     O                       AMPDMT              68
037800000000     O                       DATDIG             101 '  /  / 0'
037900000000     O                       DATDFG             132 '  /  / 0'
038000841113     O          E            DETCOM         0
038100861211     O                       AMPMAT              36
038200000000     O                       AMPDMT              68
038300000000     O                       DATDIG             101 '  /  / 0'
038400000000     O                       DATDFG             132 '  /  / 0'
038500841113     O          E            DETCOM         1
038600861211     O                       AMPMAT              36
038700841113     O                       AMPDMT              68
038800841113     O                       DATDIG             101 '  /  / 0'
038900841113     O                       DATDFG             132 '  /  / 0'
039000000000     O          E            DETCOM         1
039100000000     O                       DT3                132
039200000000     O                       AMPCLM              29
039300000000     O                       DESCLM              63
039400000000     O                       AMPLRO             123 '   .   . 0 ,  '
039500000000     O          E            DETCOM         1
039600000000     O                       DT4                132
039700000000     O                       AMPSCM              31
039800000000     O                       DESSCM              63
039900000000     O                       AMPLVM             123 '   .   . 0 ,  '
040000000000     O          E            DETCOM         1
040100000000     O                       DT5                132
040200000000     O                       AMPUMA              29
040300000000     O                       DESUMA              63
040400000000     O                       AMPLEC             123 '   .   . 0 ,  '
040500000000     O          E            DETCOM         1
040600000000     O                       DT6                132
040700000000     O                       AMPUMS              29
040800000000     O                       DESUMS              63
040900000000     O                       AMPFCS             123 '   . 0 ,     '
041000000000     O          E            DETCOM         1
041100000000     O                       DT7                132
041200000000     O                       AMPUMQ              29
041300000000     O                       DESUMQ              63
041400000000     O                       AMPFCQ             123 '   . 0 ,     '
041500000000     O          E            DETCOM         1
041600000000     O                       DT8                132
041700000000     O                       AMPUMV              29
041800000000     O                       DESUMV              63
041900000000     O                       AMPFCV             123 '   . 0 ,     '
042000000000     O          E            DETCOM         1
042100000000     O                       DT9                132
042200000000     O                       AMPCTF              28
042300000000     O                       DESCTF              63
042400000000     O                       AMPGAP             113 ' 0 '
042500000000     O          E            DETCOM         1
042600000000     O                       DTA                132
042700000000     O                       AMPLIS              29
042800000000     O                       DESLIS              63
042900000000     O                       AMPAMT             111
043000990119     O*         E            DETCOM         1
043100990119     O*                      DTb                132
043200000000     O          E            DETCOM         1
043300000000     O                       DTC                132
043400000000     O                       AMPPDC              32 ' 0 ,  '
043500000000     O          E            DETCOM         1
043600000000     O                       DTD                132
043700000000     O                       AMPTRP              29 ' 0 '
043800000000     O          E            DETCOM         1
043900000000     O                       DTE                132
044000990119     o                       ampaiv        2    123
044100000000     O          E            DETCOM         1
044200000000     O                       DTF                132
044300000000     O                       AMPPNT              37 '   . 0 ,   '
044400990119     o                       ampesv             123
044500000000     O          E            DETCOM         1
044600000000     O                       DTG                132
044700871120     O                       COMTAR              36 '  /  /  '
044800000000     O          E            DETCOM         1
044900000000     O                       DTH                132
045000000000     O                       AMPIMB              33
045100000000     O                       DESIMB              63
045200990119     o                       ampaia        2    123
045300000000     O          E            DETCOM         1
045400000000     O                       DTL                132
045500000000     O                       AMPMAG              33
045600000000     O                       DESMAG              63
045700990119     o                       ampesa             123
045800000000     O          E            DETCOM         2
045900000000     O                       DTM                132
046000131030     O                       AMPfil              41
046100000000     O*--------------------------------------------------------------*
046200000000     O*     SEGNALAZIONE DI ERRORE                                   *
046300000000     O*--------------------------------------------------------------*
046400000000     O          E            ERRORE      3  0
046500000000     O                       ER1                132
046600000000     O                  10   ARTINF              55
046700861211     O                  10   ARTSUP              66
046800000000     O                  20   CLAINF              55
046900000000     O                  20   CLASUP              59
047000000000     O                  30   SOTINF              55
047100000000     O                  30   SOTSUP              61
047200000000     O                  40   CATINF              55
047300000000     O                  40   CATSUP              58
047400000000     O          E            ERRORE         1
047500000000     O                  10   ARTINF              55
047600861211     O                  10   ARTSUP              66
047700000000     O                  20   CLAINF              55
047800000000     O                  20   CLASUP              59
047900000000     O                  30   SOTINF              55
048000000000     O                  30   SOTSUP              61
048100000000     O                  40   CATINF              55
048200000000     O                  40   CATSUP              58
048300000000     O*--------------------------------------------------------------*
048400000000     O*     STAMPA TOTALI FINALI                                     *
048500000000     O*--------------------------------------------------------------*
048600000000     O          T    LRN80                    62
048700000000     O                                           23 'Num. Prodotti Stampati'
048800000000     O                       NRARTI              31 '  . 0 '
048900000000     O*--------------------------------------------------------------*
049000000000** TS1
049100000000*-DESDIT-----------* ***------------------------------------------ TS1
049200000000-------------------------------------*** il               Pag.     TS1
049300000000** TS2
049400000000                     **      Gestione Anagrafica : STAMPA MATERIE  TS2
049500990317PRIME E CONSUMO X                     **        Pgm...: TII013R    TS2
049600000000** TS3
049700000000                     ***------------------------------------------ TS3
049800000000-------------------------------------***        Utente:            TS3
049900000000** DT1
050000871022Articolo      Descrizione-------------------*Cl.*S/Cl.*Unita'di Mi DT1
050100131030sura*Ubicazione     *  Liv. Riord.* Liv. Minimo * GG. *            DT1
050200860905** DTY
050300871013                                            *   *     *Ana.Sto.Acq DTY
050400131030.Ven*               *             *             *     *            DTY
050500000000** DT2
050600871022Articolo................:             xxxxxxxxxxxxxxxxxxxxxxxxxxxx DT2
050700000000xx   Data Inizio Gestione: xx/xx/xx   Data Fine Gestione: xx/xx/xx DT2
050800000000** DT3
050900000000Classe Merceologica.....: xxx         xxxxxxxxxxxxxxxxxxxxxxxxx    DT3
051000000000                Livello Riordino in..UMA:  xxx.xxx.xxx,xx          DT3
051100000000** DT4
051200000000S/Classe Merceologica...: xxxxx       xxxxxxxxxxxxxxxxxxxxxxxxx    DT4
051300000000                Livello Minimo in....UMA:  xxx.xxx.xxx,xx          DT4
051400000000** DT5
051500000000Unita'Mis.Anagrafica.UMA: xxx         xxxxxxxxxxxxxxxxxxxxxxxxx    DT5
051600000000                Lotto Economico in...UMA:  xxx.xxx.xxx,xx          DT5
051700000000** DT6
051800000000Unita'Mis.Stoccaggio.UMS: xxx         xxxxxxxxxxxxxxxxxxxxxxxxx    DT6
051900000000                Fattore Convers..UMA-UMS:   xxx.xxx,xxxxx          DT6
052000000000** DT7
052100000000Unita'Mis Acquisti...UMQ: xxx         xxxxxxxxxxxxxxxxxxxxxxxxx    DT7
052200000000                Fattore Convers..UMA-UMQ:   xxx.xxx,xxxxx          DT7
052300000000** DT8
052400000000Unita'Mis.Vendita....UMV: xxx         xxxxxxxxxxxxxxxxxxxxxxxxx    DT8
052500000000                Fattore Convers..UMA-UMV:   xxx.xxx,xxxxx          DT8
052600000000** DT9
052700000000Categoria Fiscale.......: xx          xxxxxxxxxxxxxxxxxxxxxxxxx    DT9
052800000000                Giorni Medi Approvigion.:   xxx                    DT9
052900000000** DTA
053000000000Codice Listino Vendita..: xxx         xxxxxxxxxxxxxxxxxxxxxxxxx    DTA
053100000000                Codice Attivita'Prodotto:   x                      DTA
053200000000** DTB
053300000000Codice Fornitore Stand..: xxxxxxx     xxxxxxxxxxxxxxxxxxxxxxxxxxxx DTB
053400000000xx              Codice Prodotto x Fornit:   xxxxxxxxxxxxxxx        DTB
053500000000** DTC
053600000000Percent.Delta Consegna+-: xxx,xx                                   DTC
053700990119                                                                   DTC
053800000000** DTD
053900000000Tempo Riordino/Prod...gg: xxx                                      DTD
054000000000                                                                   DTD
054100000000** DTE
054200000000Coeffic.Calo Aumento....: x,xxxx                                   DTE
054300990119                Aliquota iva Vendite....:                          DTE
054400000000** DTF
054500000000Peso Netto in........UMA: xxx.xxx,xxx                              DTF
054600990119                Esenz.I.V.A.Vendite.....:                          DTF
054700000000** DTG
054800871120Data ultima uscita vendite:                                        DTG
054900871022                                                                   DTG
055000000000** DTH
055100000000Codice Imballo..........: xxxxxxx     xxxxxxxxxxxxxxxxxxxxxxxxx    DTH
055200990119                Aliquota iva Acquisti...:             xxxxx        DTH
055300000000** DTL
055400000000Magazzino di Competenza.: xxxxxxx     xxxxxxxxxxxxxxxxxxxxxxxxx    DTL
055500990119                Esenz.I.V.A.Acquisti....:               xxx        DTL
055600000000** DTM
055700000000Codice Ubicazione.......: xxxxx                                    DTM
055800871022                                                                   DTM
055900000000** ER1
056000000000Attenzione nessun PRODOTTO compreso nei limiti -       /       -!! ER1
056100000000  Riprova cambiando limiti.                                        ER1

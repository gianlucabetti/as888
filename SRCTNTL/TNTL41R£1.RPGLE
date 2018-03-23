000100951009     H*------------------------------------------------------------------------*
000200951124     H* CONTROLLO TRAINI (BATCH )                                      NO      *
000300010824
000400010824      * ATTENZIONE: questo programma era il vecchio TNTL31R
000500010824
000600951009     H*------------------------------------------------------------------------*
000700951009     H DECEDIT('0,') DATEDIT(*DMY.)
000800951009     F*------------------------------------------------------------------------*
000900951026     FTNFVS01L  IF   E           K DISK    USROPN
001000960306     FTNFVS02L  UF   E           K DISK    USROPN
001100951026     F                                     RENAME(TNFVS000:TNFVS2)
001200010823     FTNFWS01L  IF   E           K DISK    usropn
001300951031     FTNTST01L  UF   E           K DISK
001400960304     FTNTSR01L  UF   E           K DISK
001500960129     FTNTSR02L  UF   E           K DISK    USROPN
001600951020     F                                     RENAME(TNTSR000:TNTSR2)
001700960205     FTNTSR03L  UF   E           K DISK    USROPN
001800951026     F                                     RENAME(TNTSR000:TNTSR3)
001900951031     FTNTSR04L  IF   E           K DISK
002000951031     F                                     RENAME(TNTSR000:TNTSR4)
002100960229     FTNTSR05L  UF   E           K DISK    USROPN
002200960229     F                                     RENAME(TNTSR000:TNTSR5)
002300960229     FTNTSR09L  UF   E           K DISK    USROPN
002400960229     F                                     RENAME(TNTSR000:TNTSR9)
002500960201     FTNTSR07L  UF   E           K DISK    USROPN
002600960129     F                                     RENAME(TNTSR000:TNTSR7)
002700951023     FAZORG01L  IF   E           K DISK
002800951026     FTABEL00F  IF   E           K DISK
002900951108     FTNTLZ01L  IF   E           K DISK
003000960228     FFNFVD01L  IF   E           K DISK    USROPN
003100960228     FFNFV203L  IF   E           K DISK    USROPN
003200960304     FFNFV401L  IF   E           K DISK
003300951102     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
003400960304     D PAR             S             66    DIM(12) CTDATA PERRCD(1)             x stampa
003500951215     D COS             S             66    DIM(9) CTDATA PERRCD(1)              x stampa
003600960228     D FVN             S             66    DIM(11) CTDATA PERRCD(1)             x stampa
003700960229     D TRZ             S             66    DIM(12) CTDATA PERRCD(1)             x stampa
003800960304     D MSG             S             20    DIM(8) CTDATA PERRCD(1)              x stampa
003900960229     D CAR             S              3  0 DIM(5)                               ALTRE FERM.CARICO
004000960229     D PRO             S              3    DIM(5)                               ALTRE FERM.(PROV)
004100000626     D SPT             S              3  0 DIM(100)
004200010823
004300010823     d p_fwsipt        s             11  2
004400010823     d p_tstili        s             11  2
004500010823     d p_tstipt        s             11  2
004600010823     d p_tstico        s             11  2
004700010911     d p_tstdiv        s                   like(tstdiv)
004800010824     d w_delta         s                   like(fwsipt)
004900010824     d sav_tstili      s                   like(tstili)
005000010823
005100951009     D PARAM           DS
005200951215     D* Data foglio viaggio teorica dal
005300951023     D  PARDTD                 1      8  0
005400951215     D* data foglio viaggio teorica al
005500951023     D  PARDTA                 9     16  0
005600951215     D* linea partenza foglio viaggio teorico
005700951016     D  V1CLNP                17     19  0
005800951019     D* data traino
005900951016     D  PARDTN                20     27  0
006000951019     D* numero traino
006100951016     D  PARTRN                28     34  0
006200960229     D* stampa f.v. reali non abbinati
006300951019     D  V1CFVB                35     35
006400951220     D  V1CABB                36     36
006500951215     D* stampa f.v. teorici senza abbinamento a reali
006600951220     D  V1CVUO                37     37
006700951215     D* stampa ritardi/anticipi
006800951220     D  V1CRIT                38     38
006900951215     D* stampa traini non autorizzati al pagamento
007000951220     D  V1CTAR                39     39
007100951220     D  V1CCNF                40     40
007200960304     D* flag per stampa elenco traini con trazionisti fasulli
007300960304     D  V1CFAS                41     41
007400951031     D* Data decorrenza traino
007500960304     D  PARDDE                42     49  0
007600960301     D* Ora per controllo anomalie (Se = 0 il pgm assumerà la data del
007700960301     D*                             sistema)
007800960304     D  PARORA                50     53  0
007900070517     D  PARPDR                54     60  0
008000070517     D  PARffa                61     63  0
008100070517     D  PARTRN1               64     70  0
008200951019     D* DS PER PASSAGGIO PARAMETRI A PGM DI CREAZIONE GRIGLIA
008300951019     D PAR24           DS
008400951019     D* data traino dal
008500951019     D  P24DTD                 1      8  0
008600951019     D* data traino al
008700951019     D  P24DTA                 9     16  0
008800951019     D* numero traino
008900951019     D  P24TRN                17     23  0
009000951019     D* terminal di partenza
009100951019     D  P24TFP                24     26  0
009200951019     D* data decorrenza traino
009300951019     D  P24DDE                27     34  0
009400951206     D* Creazione se giorno festivo (' '=No controllo festivo
009500951206     D*                             ('S'=Controllo festivo e crea solo
009600951206     D*                                  se non festivo)
009700951206     D  P24FES                35     35
009800960229     D* DS PER TNTL35R - CALCOLO DATA TEORICA E RITARDO/ANTICIPO
009900960126     D DSTL35        E DS                  EXTNAME(TNTL35DS)
010000960229     D* DS PER TNTL36R - STAMPA RITARDI ANTICIPI
010100960229     D DSTL36        E DS                  EXTNAME(TNTL36DS)
010200960126     D*
010300951009     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010400951027     D TNTL30        E DS                  EXTNAME(TNTL30DS)
010500971204     D DSLV55        E DS                  EXTNAME(FNLV55DS)
010600951009     D KPJBA         E DS
010700960223     D OG148         E DS
010800000626     D OG143         E DS
010900010824
011000010824     D tibs02ds      E DS
011100010824     D dgei          E DS
011200010824
011300951020     D WLBDA8          DS
011400951020     D  G08DAT                 1      8  0
011500951020     D  G08INV                 9     16  0
011600951020     D  G08ERR                17     17
011700951020     D  G08TGI                18     22  0
011800951020     D WGIDAT          DS
011900951020     D  GI8DAT                 1      8  0
012000951020     D  GI8INV                 9     16  0
012100951020     D  GI8TGI                17     21  0
012200960228     D                 DS
012300960228     D  FVDVL1                 1      9  4
012400960228     D  FVDVL2                10     18  4
012500960228     D  FVDVL3                19     27  4
012600960228     D  FVDVL4                28     36  4
012700960228     D  FVDVL5                37     45  4
012800960228     D  FVDVL6                46     54  4
012900960228     D  FVD                    1     54  4
013000960228     D                                     DIM(6)                               CAMPI TOT.VOL.L
013100960229     D DSTSR         E DS                  EXTNAME(TNTSR00F)
013200960301     D WDSTSR          DS            86
013300960229     D WXDTOR          DS
013400960229     D  WXDT1                  1      8  0
013500960229     D  WXHM1                  9     12  0
013600960229     D  WXDT2                 13     20  0
013700960229     D  WXHM2                 21     24  0
013800960229     D  WXFLG                 25     25
013900960229     D  WXMIN                 26     30  0
014000951221     D TIT1            C                   CONST('NON ABBINATI        -
014100951221     D                                                    ')
014200951221     D TIT2            C                   CONST('ABBINATI MANUALMENTE-
014300951221     D                                                    ')
014400951221     D TIT3            C                   CONST('NON ABBINATI O ABBIN-
014500951221     D                                     ATI MANUALMENTE')
014600951020     I*
014700960202     ITNTSR2        02
014800960229     ITNTSR5        05
014900960229     ITNTSR9        09
015000960201     ITNTSR7        07
015100960129     ITNTSR4
015200960129     I              TSRATB                      SRATB4
015300960129     I              TSRTRN                      SRTRN4
015400960129     I              TSRDTN                      SRDTN4
015500960129     I              TSRDTT                      SRDTT4
015600960301     I              TSRTFM                      SRTFM4
015700960129     I              TSRNFP                      SRNFP4
015800960129     I              TSRFFP                      SRFFP4
015900960129     I              TSRNFA                      SRNFA4
016000960129     I              TSRFFA                      SRFFA4
016100960129     I              TSRDTP                      SRDTP4
016200960129     I              TSRHTP                      SRHTP4
016300960129     I              TSRDRP                      SRDRP4
016400960129     I              TSRHRP                      SRHRP4
016500960320     I              TSRRTP                      SRRTP4
016600960129     I              TSRDTA                      SRDTA4
016700960129     I              TSRHTA                      SRHTA4
016800960129     I              TSRDRA                      SRDRA4
016900960129     I              TSRHRA                      SRHRA4
017000960201     I              TSRRTA                      SRRTA4
017100960129     I              TSRDKP                      SRDKP4
017200960301     I              TSRFKP                      SRFKP4
017300960129     I              TSRDKA                      SRDKA4
017400960301     I              TSRFKA                      SRFKA4
017500960129     I              TSRDKI                      SRDKI4
017600960301     I              TSRFKI                      SRFKI4
017700960129     I              TSRLNP                      SRLNP4
017800960129     I              TSRNFV                      SRNFV4
017900960129     I              TSRDFV                      SRDFV4
018000951009     C****************************************************************
018100951009     C*  RIEPILOGO INDICATORI
018200951009     C***************************************************************
018300960208     C* 01    - ROTTURA NUMERO FERMATA PARTENZA
018400960201     C* 02    - TIPO RECORD TNTSR2
018500960229     C* 05    - TIPO RECORD TNTSR5
018600960229     C* 09    - TIPO RECORD TNTSR9
018700960129     C* 07    - TIPO RECORD TNTSR7
018800960304     C* 15    - CONDIZIONAMENTO STAMPA DEL TERZO MESSAGGGIO
018900960129     C* 11    - OFF = CONTROLLO PER DATA F.V. TEORICA DI TUTTE LE LNP
019000951215     C*         ON  = CONTROLLO PER DATA F.V.TEOR. DI UNA SPECIFICA LNP
019100960305     C* 13    - OFF = E' SOLO RISTAMPA
019200960129     C* 14    - STAMPA TESTATA TRAINO
019300960129     C* 15    - CONDIZIONAMENTO STAMPA DEL SECONDO MESSAGGGIO
019400960129     C* 16    - CONDIZIONAMENTO IN STAMPA DEL CODICE TRAZIONISTA
019500960131     C* 17      ON :STAMPA SOLO ELENCO FOGLI VIAGGIO NON ABBINATI
019600951215     C* 19    - LETTURA RIGHE F.V TEOR. SE CONTROLLO PER DATA F.V. TEOR
019700951009     C* 30    - COMODO
019800951009     C***************************************************************
019900960226     C* AREA DATI -- > DATA ULTIMO CONTROLLO
020000960226    1C     PARDTD        IFGT      *ZEROS
020100960226     C     *DTAARA       DEFINE                  TNTL30
020200960226     C     *LOCK         IN        TNTL30
020300960226    2C     D30BLC        IFEQ      ' '
020400960226     C                   MOVE      '2'           D30BLC
020500960226     C                   OUT       TNTL30
020600960226   X2C                   ELSE
020700960226     C* se impegnata non è possibile fare controlli e invio quindi msg
020800960226     C                   UNLOCK    TNTL30
020900960226     C                   CALL      'TNTL31C'
021000960226     C                   PARM                    KNMUS
021100960226     C                   PARM      '2'           TIPO              1
021200960226     C                   GOTO      FINE
021300960226    2C                   END
021400960226    1C                   END
021500960325     C* RICHIAMO PGM CHE RICEVE DATE/ORE ARRIVO REALI
021600960325     C                   CALL      'TNTL30C'
021700960325     C                   PARM                    KPJBA
021800960226     C* RICHIAMO CREAZIONE GRIGLIA
021900960226     C                   CLEAR                   PAR24
022000960226     C                   MOVEL     'S'           P24FES
022100960226     C* se controllo per dtt creo griglia per data traino dal/al
022200960226    1C     PARDTD        IFGT      *ZEROS
022300960226     C                   MOVE      PARDTD        P24DTD
022400960226     C                   MOVE      PARDTA        P24DTA
022500960226   X1C                   ELSE
022600960226     C* se controllo per nr.traino creo griglia per nr.traino
022700960226     C                   MOVE      PARTRN        P24TRN
022800960226     C                   MOVE      PARDTN        P24DTD
022900960226     C                   MOVE      PARDDE        P24DDE
023000960226    1C                   END
023100960226     C                   MOVEL     PAR24         KPJBU
023200960226     C                   CALL      'TNTL24R'
023300960226     C                   PARM                    KPJBA
023400960226     C* RICHIAMO ABBINAMENTO AUTOMATICO F.V. A TRAINI
023500960226     C                   CLEAR                   KPJBU
023600010822     C                   CALL      'TNTL45R'
023700960226     C                   PARM                    KPJBA
023800960226     C* RICHIAMO PGM CHE APPOGGIA DATE/ORE ARRIVO SU GRIGLIA
023900960226     C                   CALL      'TNTL26R'
024000960226     C                   PARM                    KPJBA
024100960226     C* PRIMA DI RICHIAMARE LE ROUTINE DI ELABORAZIONE
024200960315     C* STAMPO PARAMETRI DI RICHIESTA
024300960315     C     V1CFVB        IFNE      *BLANKS
024400960315     C     V1CVUO        ORNE      *BLANKS
024500960315     C     V1CTAR        ORNE      *BLANKS
024600960315     C     V1CFAS        ORNE      *BLANKS
024700960315     C                   EXCEPT    TESTA
024800960315     C                   EXCEPT    PARRIC
024900960315     C                   MOVE      '1'           WPRT              1
025000960315     C                   END
025100960226     C* ACCENDO OF PER STAMPARE L'INTESTAZIONE PER LA PRIMA VOLTA
025200960226     C                   SETON                                        OF
025300960305     C* DETERMINO SE è RISTAMPA
025400960305     C     PARDTD        IFGT      *ZEROS
025500960305    2C     PARDTA        COMP      D30DAT                             13  13
025600960305     C* Se non è ristampa imposto data per aggiornamento date controllo
025700960305     C     *IN13         IFEQ      *ON
025800960305     C     PARDTD        IFGE      D30DAT
025900960305     C                   MOVE      PARDTD        WDAT
026000960305     C                   ELSE
026100960305     C                   MOVE      D30DAT        WDAT
026200960305     C                   END
026300960305     C                   END
026400960305     C                   END
026500960306     C* STAMPA FV NON ABBINATI E AGGIORNAMENTO DATA CONTROLLO
026600960306     C     PARDTD        IFGT      *ZEROS
026700960226     C                   EXSR      FVNAB
026800960226     C                   END
026900960226     C* STAMPA ANOMALIE TRAINI+BIS/ECCEZIONALI+CONFERMA AUTOMATICA IMPO
027000960226     C* RTI
027100960226     C                   EXSR      SRTRZ
027200960229     C* SE RICHIESTI RITARDI E/O ANTICIPI RICHIAMO PGM DI STAMPA
027300960229     C     V1CRIT        IFNE      *BLANKS
027400960229     C                   CLEAR                   DSTL36
027500070517     c                   if        partrn>0
027600960229     C                   MOVE      PARTRN        D36TRN
027700070517     c                   else
027800070517     c                   z-add     partrn1       d36trn
027900070517     c                   endif
028000960229     C                   MOVE      PARDTN        D36DTD
028100960229     C                   MOVE      V1CLNP        D36LNP
028200960229     C                   MOVE      PARDTD        D36DKD
028300960305     C                   MOVE      PARDTA        D36DKA
028400960229     C                   MOVEL     V1CRIT        D36FRA
028500070517     c                   z-add     parpdr        d36pdr
028600070517     c                   z-add     parffa        d36ffa
028700960229     C                   MOVEL     DSTL36        KPJBU
028800960229     C                   CALL      'TNTL36R'
028900960229     C                   PARM                    KPJBA
029000960229     C                   END
029100960226     C***
029200960229     C*** SE CONTROLLO PER DATA F.V. TEOR. AGGIORNA AREA DATI TNTL30:
029300960226     C*
029400960226     C* - AGGIORNO LA DATA ULTIMO CONTROLLO SE IL CONTROLLO E' PER
029500960226     C*   TUTTE LE LNP E DTT > DI DATA ULTIMO CONTROLLO
029600960226     C* - SBLOCCO AREA DATI
029700960226     C* - STAMPO MESSAGGIO DI AVVERTIMENTO DI TABULATO NON ATTENDIBILE
029800960226     C*   SE FRA DATA ULTIMO CONTROLLO E DATA F.V. TEOR. DAL APPENA
029900960226     C*   CONTROLLATA ESISTE ALMENO UN GIORNO CHE NON E' STATO CONTROL-
030000960226     C*   LATO (IN QUESTO CASO NON AGGIORNO NEMMENO LA DATA ULT.CONTR.)
030100960226     C*   (Questo problema può verificarsi se fra il momento del lancio
030200960226     C*    del controllo e il momento dell'esecuzione dello stesso
030300960226     C*    è stata effettuato uno sgrigliamento che ha aggiornato la
030400960226     C*    data ultimo controllo)
030500960226     C                   CLEAR                   W30DAT
030600960226    1C     PARDTD        IFGT      *ZEROS
030700960226     C     PARDTA        ANDGT     D30DAT
030800960226     C                   CLEAR                   WLBDA8
030900960226     C                   MOVE      D30DAT        G08INV
031000960226     C                   MOVE      '3'           G08ERR
031100960226     C                   CALL      'XSRDA8'
031200960226     C                   PARM                    WLBDA8
031300960226     C                   ADD       1             G08TGI
031400960226     C                   CLEAR                   WGIDAT
031500960226     C                   MOVE      G08TGI        GI8TGI
031600960226     C                   CALL      'XSRGI8'
031700960226     C                   PARM                    WGIDAT
031800960226    2C     GI8INV        IFGE      PARDTD
031900960226     C     V1CLNP        IFEQ      *ZEROS
032000960226     C                   MOVE      PARDTA        W30DAT
032100960226     C                   END
032200960226   X2C                   ELSE
032300960315     C   OF              EXCEPT    TESTA
032400960226     C                   EXCEPT    MESSAG
032500960226    2C                   END
032600960226    1C                   END
032700960226     C*
032800960226     C     WPRT          IFEQ      '1'
032900960226     C                   EXCEPT    FINES
033000960226     C                   END
033100960226     C* SBLOCCO AREA DATI + EVENTUALE AGGIORN. DATA ULTIMO CONTROLLO
033200960226    1C     PARDTD        IFGT      *ZEROS
033300960226     C     *LOCK         IN        TNTL30
033400960226     C                   MOVE      *BLANKS       D30BLC
033500960226    2C     W30DAT        IFGT      *ZEROS
033600960226     C                   MOVE      W30DAT        D30DAT
033700960226    2C                   END
033800960226     C                   OUT       TNTL30
033900960226    1C                   END
034000960226     C     FINE          TAG
034100971204     C*
034200971204     C                   CLEAR                   DSLV55
034300971204     C                   MOVEL     'C'           D55TLA
034400971204     C                   CALL      'FNLV55R'
034500971204     C                   PARM                    DSLV55
034600971204     C**
034700960226     C                   SETON                                        LR
034800960123     C******  STAMPA F.V. NON ABBINATI E/O ABBINATI MANUALM.  *********
034900951020     C     FVNAB         BEGSR
035000951025     C                   OPEN      TNFVS02L
035100010823     c                   open      tnfws01l
035200960228     C                   OPEN      FNFVD01L
035300960228     C                   OPEN      FNFV203L
035400951103     C                   CLEAR                   COMLNP
035500960305     C                   CLEAR                   WDKF
035600960305     C                   SETOFF                                       17
035700951221     C*
035800960131    1C                   SELECT
035900951221     C     V1CABB        WHENEQ    'N'
036000960131     C                   SETON                                        17
036100951221     C                   MOVEL     TIT1          PRTTIT
036200960131     C     V1CABB        WHENEQ    'A'
036300960131     C                   MOVEL     TIT2          PRTTIT           35
036400951221     C     V1CABB        WHENEQ    'T'
036500951221     C                   MOVEL     TIT3          PRTTIT
036600960131    1C                   ENDSL
036700960305     C*
036800960305     C     *IN13         IFEQ      *OFF
036900960305     C                   MOVE      PARDTA        WDKF
037000960305     C                   ELSE
037100960305     C* Se è ristampa imposto data=pardta
037200960305     C                   MOVE      WDAT          WDKF
037300960305     C                   END
037400960306     C*
037500960305     C     KFVS          SETLL     TNFVS02L
037600960305     C     KABN          READE     TNFVS02L                               30
037700951024    1C     *IN30         DOWEQ     *OFF
037800951221     C     FVSTRS        ANDLT     9999998
037900960319     C* Escludo fv abbinati automaticamente
038000960319     C     FVSTRS        IFGT      *ZEROS
038100960319     C     FVSTRS        ANDLT     9000000
038200960319     C     FVSTRS        ANDEQ     FVSTRN
038300960319     C                   GOTO      LEGFVS
038400960319     C                   END
038500951027     C* Escludo annullati
038600960319     C     FVSATB        CABNE     ' '           LEGFVS
038700960305     C* Escludo quelli con data controllo > di data lancio
038800960319     C     FVSDKF        CABGT     WDKF          LEGFVS
038900960319     C* Escludo quelli con data foglio viaggio > di data controllo al
039000960319     C     FVSDFV        CABGT     PARDTA        LEGFVS
039100960319     C* Se è ristampa escludo anche quelli con data controllo=0
039200960319     C  N13FVSDKF        CABEQ     *ZEROS        LEGFVS
039300960131     C*
039400960306     C* Non stampo e aggiorno solo la data controllo nei seguenti casi:
039500960306     C* - non è stata richiesta la stampa
039600960306     C* - è stata richiesta la stampa dei fogli viaggio non abbinati
039700960306     C*   e questo FV è abbinato manualmente
039800960306     C* - è stata richiesta la stampa dei fogli viaggio abbinati
039900960306     C*   manualmente e questo FV non è abbinato
040000960319    2C     V1CFVB        IFEQ      *BLANKS
040100960306     C     V1CABB        OREQ      'N'
040200960305     C     FVSTRS        ANDGT     *ZEROS
040300960305     C     V1CABB        OREQ      'A'
040400960305     C     FVSTRS        ANDEQ     *ZEROS
040500960305     C                   GOTO      FVAGG
040600960319    2C                   END
040700951023     C* Gestione rottura linea di partenza f.v.
040800960319    2C     FVSLNP        IFNE      COMLNP
040900951023     C                   MOVE      FVSLNP        COMLNP
041000951023     C     FVSLNP        CHAIN     AZORG01L                           30
041100951115     C  N30              MOVEL     ORGDES        WRTLNP           10
041200951115     C   30              MOVE      *BLANKS       WRTLNP
041300951115     C                   MOVEL     WRTLNP        PRTLNP           10
041400960319    2C                   END
041500011009      *
041600011009     c                   clear                   fwsipt
041700011009     c                   clear                   p_fwsipt
041800010823      * aggancio il file delle estensioni foglio viaggio per gli importi in
041900010823      * divisa
042000010823     c     kfws          chain     tnfws01l
042100010907      * se l'importo dell'estensione è > 0 prendo quello che trovo
042200010907      * perchè vuol dire che stanno già girando i programmi nuovi modificati per l'euro
042300010907     c                   if        fwsipt > 0
042400010907     c                   z-add     fwsipt        p_fwsipt
042500010907     c                   endif
042600010907      * se l'importo dell'estensione è = 0 e l'importo sul file
042700010907      * principale è > 0, vuol dire che è un foglio viaggio vecchio e quindi
042800010907      * la divisa è lire
042900010907     c                   if        fwsipt = *zeros and fvsipt <> 0
043000010907     c                   eval      fwsvip = 'ITL'
043100010907     c                   z-add     fvsipt        p_fwsipt
043200010907     c                   endif
043300010907      * se l'importo dell'estensione è = 0 e l'importo sul file
043400010907      * principale è = 0, imposto la divisa a blank così non viene stampata
043500010907     c                   if        fwsipt = *zeros and fvsipt = 0
043600010907     c                   z-add     fvsipt        p_fwsipt
043700010907     c                   clear                   fwsvip
043800010907     c                   endif
043900951023     C* Decodifiche  per campi di stampa
044000951030     C                   CLEAR                   WLBDA8
044100951030     C                   MOVE      FVSDFV        G08INV
044200951030     C                   MOVEL     '3'           G08ERR
044300951030     C                   CALL      'XSRDA8'
044400951030     C                   PARM                    WLBDA8
044500951030     C                   MOVE      G08DAT        PRTDF8
044600951023     C     FVSLNA        CHAIN     AZORG01L                           30
044700951103     C  N30              MOVEL     ORGDES        PRTLNA           10
044800951027     C   30              MOVE      *BLANKS       PRTLNA
044900951023     C                   MOVEL     'TU'          KCOD
045000951023     C                   MOVEL(P)  FVSTMZ        KKEY
045100951027     C     KTAB          CHAIN     TABEL00F                           30
045200951027     C  N30              MOVEL     TBLUNI        PRTTMZ           15
045300951027     C   30              MOVE      *BLANKS       PRTTMZ
045400951103     C                   MOVEL     FVSDPD        PRTPDR           10
045500960129     C     FVSPDR        COMP      *ZEROS                             1616
045600960131     C* Se f.v. abbinato manualmente chaino storico traini per reperire
045700960131     C* campi per la stampa
045800960319    2C     *IN17         IFEQ      *OFF
045900960131     C                   CLEAR                   TNTSR4
046000960131     C                   CLEAR                   PRTDTR
046100960319    3C     FVSTRS        IFGT      *ZEROS
046200960131     C     FVSTRS        ANDNE     FVSTRN
046300960319     C     FVSTRS        ORGE      9000000
046400960131     C     KTSR4B        CHAIN     TNTSR04L                           30
046500960131     C  N30              MOVE      SRTRN4        KTRN
046600960131     C  N30              MOVE      SRDTN4        KDTN
046700960131     C  N30KTST          CHAIN     TNTST01L                           30
046800960319    4C     *IN30         IFEQ      *OFF
046900960131     C* Rovescio data traino
047000960131     C                   CLEAR                   WLBDA8
047100960131     C                   MOVE      TSTDTN        G08INV
047200960131     C                   MOVEL     '3'           G08ERR
047300960131     C                   CALL      'XSRDA8'
047400960131     C                   PARM                    WLBDA8
047500960131     C                   MOVE      G08DAT        PRTDTR
047600960319    4C                   END
047700960319    3C                   END
047800960319    2C                   END
047900960228     C* ROVESCIO DATA PARTENZA
048000960228     C                   CLEAR                   WLBDA8
048100960228     C                   MOVE      FVSDRP        G08INV
048200960228     C                   MOVEL     '3'           G08ERR
048300960228     C                   CALL      'XSRDA8'
048400960228     C                   PARM                    WLBDA8
048500960228     C                   MOVEL     G08DAT        PRTDRP            4 0
048600960228     C* LEGGO DATA DI ARRIVO
048700960228     C                   CLEAR                   PRTDPA
048800960228     C     KFV2          CHAIN     FNFV203L                           30
048900960319    2C     *IN30         IFEQ      *OFF
049000960228     C* ROVESCIO DATA ARRIVO
049100960228     C                   CLEAR                   WLBDA8
049200960228     C                   MOVE      FV2DPA        G08INV
049300960228     C                   MOVEL     '3'           G08ERR
049400960228     C                   CALL      'XSRDA8'
049500960228     C                   PARM                    WLBDA8
049600960228     C                   MOVEL     G08DAT        PRTDPA            4 0
049700960319    2C                   END
049800960228     C* DETERMINO METRI CUBI
049900960228     C                   MOVE      *ZEROS        PRTMTC            7 4
050000960228     C     KFVD          SETLL     FNFVD01L
050100960228     C     KFVD          READE     FNFVD01L                               30
050200960319    2C     *IN30         DOWEQ     *OFF
050300960228     C                   XFOOT     FVD           W0074             7 4
050400960228     C                   ADD       W0074         PRTMTC
050500960228     C     KFVD          READE     FNFVD01L                               30
050600960319    2C                   ENDDO
050700960228     C* Evidenzio se si tratta di traino bis o eccezionale
050800960228     C                   MOVE      *BLANKS       PRTBIS            3
050900960319    2C     FVSBIS        IFEQ      'B'
051000960228     C                   MOVEL     'BIS'         PRTBIS
051100960228     C                   ELSE
051200960319    3C     TSTBIS        IFEQ      'E'
051300960228     C                   MOVEL     'ECC'         PRTBIS
051400960319    3C                   END
051500960319    2C                   END
051600960228     C*
051700960319    2C     *INOF         IFEQ      *ON
051800951026     C                   EXCEPT    TESTA
051900951026     C                   EXCEPT    TESFV
052000951115     C* WRTLNP = Decodifica linea partenza che stampo solo quando
052100951115     C* varia e a salto pagina
052200951115     C                   MOVEL     WRTLNP        PRTLNP           10
052300951102     C                   SETOFF                                       OF
052400960319    2C                   END
052500960319    2C     PRTDPA        IFGT      *ZEROS
052600960228     C                   EXCEPT    RIGARR
052700960319    2C                   END
052800951024     C                   EXCEPT    RIGFV
052900960228     C* CONTINUO A LEGGERE E STAMPARE LE DATE DI ARRIVO SE CE NE SONO
053000960228     C     KFV2          READE     FNFV203L                               30
053100960319    2C     *IN30         DOWEQ     *OFF
053200960228     C* ROVESCIO DATA ARRIVO
053300960228     C                   CLEAR                   WLBDA8
053400960228     C                   MOVE      FV2DPA        G08INV
053500960228     C                   MOVEL     '3'           G08ERR
053600960228     C                   CALL      'XSRDA8'
053700960228     C                   PARM                    WLBDA8
053800960228     C                   MOVEL     G08DAT        PRTDPA            4 0
053900960319    3C     *INOF         IFEQ      *ON
054000960228     C                   EXCEPT    TESTA
054100960228     C                   EXCEPT    TESFV
054200960228     C                   MOVEL     WRTLNP        PRTLNP           10
054300960319    3C                   END
054400960228     C                   EXCEPT    RIGARR
054500960319    3C     *INOF         IFEQ      *ON
054600960304     C                   EXCEPT    RIGFV
054700960304     C                   ELSE
054800960304     C                   EXCEPT    VUOTO
054900960319    3C                   END
055000960228     C                   SETOFF                                       OF
055100960228     C     KFV2          READE     FNFV203L                               30
055200960319    2C                   ENDDO
055300960304     C* STAMPO MOTIVAZIONE DI NON ABBINAMENTO
055400960304     C                   CLEAR                   PRTNT1
055500960304     C                   CLEAR                   PRTNT2
055600960304     C                   MOVEL     'P'           KEPA
055700960304     C                   CLEAR                   KLAI
055800960304     C                   MOVEL     'C'           KTRC
055900960304     C                   MOVE      ' '           WCONT             1
056000960304     C     KFV4          SETLL     FNFV401L
056100960304     C     KFV4          READE     FNFV401L                               30
056200960319    2C     *IN30         DOWEQ     *OFF
056300960319    3C     PRTNT1        IFEQ      *BLANKS
056400960304     C                   MOVEL     FV4NOT        PRTNT1
056500960304     C                   MOVE      '1'           WCONT
056600960319   X3C                   ELSE
056700960304     C                   MOVEL     FV4NOT        PRTNT2
056800960304     C                   MOVE      '2'           WCONT
056900960304     C                   MOVE      ' '           WCONT
057000960304     C* STAMPO MOTIVAZIONE
057100960319    4C     *INOF         IFEQ      *ON
057200960304     C                   EXCEPT    TESTA
057300960304     C                   EXCEPT    TESFV
057400960319    4C                   END
057500960304     C                   EXCEPT    RIGMOT
057600960304     C                   CLEAR                   PRTNT1
057700960304     C                   CLEAR                   PRTNT2
057800960319    3C                   END
057900960304     C     KFV4          READE     FNFV401L                               30
058000960319    2C                   ENDDO
058100960319    2C     WCONT         IFNE      ' '
058200960305     C* STAMPO MOTIVAZIONE
058300960319    3C     *INOF         IFEQ      *ON
058400960304     C                   EXCEPT    TESTA
058500960304     C                   EXCEPT    TESFV
058600960319    3C                   END
058700960304     C                   EXCEPT    RIGMOT
058800960319    2C                   END
058900960305     C     FVAGG         TAG
059000960305     C* AGGIORNO DATA CONTROLLO (FVSDKF)
059100960319    2C     FVSDKF        IFEQ      *ZEROS
059200960305     C                   MOVE      WDAT          FVSDKF
059300960319    2C                   END
059400960228     C*
059500960319    2C     PARORA        IFEQ      *ZEROS
059600960306     C                   UPDATE    TNFVS2
059700960319     C                   ELSE
059800960319     C                   UNLOCK    TNFVS02L
059900960319    2C                   END
060000960319     C     LEGFVS        TAG
060100960131     C* LETTURA
060200960305     C     KABN          READE     TNFVS02L                               30
060300951024    1C                   ENDDO
060400951025     C                   CLOSE     TNFVS02L
060500010823     c                   close     tnfws01l
060600960228     C                   CLOSE     FNFVD01L
060700960228     C                   CLOSE     FNFV203L
060800951020     C                   ENDSR
060900951030     C********* STAMPA BUCHI + SCOSTAMENTI + BIS/ECCEZIONALI  *********
061000951023     C     SRTRZ         BEGSR
061100960129     C* OPERAZIONI PRELIMINARI
061200960130     C                   CLEAR                   COMDTN
061300960130     C                   CLEAR                   COMTRN
061400951031     C                   OPEN      TNFVS01L
061500960307     C                   SETON                                        OF
061600960307     C*§Imposto WPRT1 per stampa riga di separazione fra elenco fogli
061700960307     C*§viaggio non abbinati ed elenco traini
061800960307    1C*§         V1CFVB    IFEQ 'S'
061900960307     C*§                   MOVE ' '       WPRT1   1
062000960307     C*§                   ELSE
062100960307     C*§                   MOVE '1'       WPRT1
062200960307    1C*§                   END
062300951024     C* C O N T R O L L O   P E R  N./ D A T A   T R A I N O
062400951023    1C     PARDTD        IFEQ      *ZEROS
062500960129     C                   SETOFF                                       11
062600960202     C                   OPEN      TNTSR03L
062700960129     C                   MOVE      PARTRN        TSRTRN
062800960129     C                   MOVE      PARDTN        TSRDTN
062900960129     C                   EXSR      SRBRK
063000960229     C* RICHIAMO ROUTINE DI CARICAMENTO SCHIERA DELLE FERMATE DI
063100960229     C* CARICO DEL TRAINO
063200960229     C                   EXSR      RIECAR
063300960129     C* LETTURA RIGHE GRIGLIA
063400960129     C     KTSR1         SETLL     TNTSR01L
063500960129     C     KTSR1         READE     TNTSR01L                               30
063600960226    2C     *IN30         DOWEQ     *OFF
063700960226    3C     TSRATB        IFEQ      *BLANKS
063800960208     C* CONTROLLO ROTTURA NUMERO FERMATA PARTENZA
063900960208     C                   SETOFF                                       01
064000960226    4C     TSRNFP        IFNE      COMNFP
064100960208     C                   SETON                                        01
064200960208     C                   MOVE      TSRNFP        COMNFP
064300960226    4C                   END
064400960226     C                   MOVE      TSRDTT        WDAT
064500960129     C                   EXSR      SRRIG
064600960819    4C     TSRNFP        IFEQ      TSTNFE
064700960819     C                   EXSR      SRTES
064800960819     C  N01              CLEAR                   WCONF
064900960819     C  N01              CLEAR                   WFAS
065000960819     C     WCONF         IFNE      *BLANKS
065100960819     C     WFAS          ORNE      *BLANKS
065200960819     C                   SETON                                        14
065300960819     C                   END
065400960819   X4C                   ELSE
065500960819     C                   MOVE      *HIVAL        TSRDKI
065600960819     C                   MOVEL     'S'           TSRFKI
065700960819    4C                   END
065800960819     C*
065900960819    4C     *IN14         IFEQ      *ON
066000960819     C     WOKPRT        OREQ      'S'
066100960129     C                   EXSR      PRTRIG
066200960226    4C                   END
066300960819     C*
066400960319     C     PARORA        IFEQ      *ZEROS
066500960226     C                   UPDATE    TNTSR000
066600960319     C                   ELSE
066700960319     C                   UNLOCK    TNTSR01L
066800960319     C                   END
066900960226    3C                   END
067000960129     C     KTSR1         READE     TNTSR01L                               30
067100960226    2C                   ENDDO
067200951023   X1C                   ELSE
067300951215     C* C O N T R O L L O   P E R   L N P / D A T A  F . V .  TEORICA
067400960129     C     V1CLNP        COMP      *ZEROS                             1111
067500960229     C* T N T S R 0 5 L
067600960229     C   13              MOVE      *ZEROS        TSRDKP
067700960229     C  N13              Z-ADD     1             TSRDKP
067800960229     C                   OPEN      TNTSR05L
067900960229     C*
068000960229    2C     *IN11         IFEQ      *ON
068100960229     C                   MOVE      V1CLNP        TSRFFP
068200960229     C     KTSR5         SETLL     TNTSR05L
068300960229     C                   ELSE
068400960229     C     TSRDKP        SETLL     TNTSR05L
068500960229    2C                   END
068600960229     C                   READ      TNTSR05L                               19
068700960229    2C     *IN19         DOWEQ     *OFF
068800960229     C*
068900960229    3C     TSRDKP        IFGT      *ZEROS
069000960229    4C     TSRDKP        IFLT      PARDTD
069100960229     C                   MOVE      PARDTD        TSRDKP
069200960229     C   11              MOVE      V1CLNP        TSRFFP
069300960229     C  N11              MOVE      *ZEROS        TSRFFP
069400960229     C     KTSR5         SETLL     TNTSR05L
069500960229     C                   GOTO      LEGG05
069600960229   X4C                   ELSE
069700960229     C     TSRDKP        CABGT     PARDTA        LEGG05               19
069800960229    4C                   END
069900960229    3C                   END
070000960229     C*
070100960229    3C     *IN11         IFEQ      *ON
070200960229    4C     TSRFFP        IFGT      V1CLNP
070300960229    5C     TSRDKP        IFEQ      *ZEROS
070400960229     C                   MOVE      PARDTD        TSRDKP
070500960229     C                   MOVE      V1CLNP        TSRFFP
070600960229     C     KTSR5         SETLL     TNTSR05L
070700960229   X5C                   ELSE
070800960229     C     TSRDKP        CABEQ     PARDTA        LEGG05                   19
070900960229     C     TSRDKP        SETGT     TNTSR05L
071000960229    5C                   END
071100960229     C                   GOTO      LEGG05
071200960229    4C                   END
071300960229    3C                   END
071400960229     C*
071500960229    3C     TSRDTT        IFGE      PARDTD
071600960229    4C     *IN11         IFEQ      *OFF
071700960229     C     KTSR5         SETGT     TNTSR05L
071800960229     C                   GOTO      LEGG05
071900960229   X4C                   ELSE
072000960229    5C     TSRDKP        IFEQ      *ZEROS
072100960229     C                   MOVE      PARDTD        TSRDKP
072200960229     C                   MOVE      V1CLNP        TSRFFP
072300960229     C     KTSR5         SETLL     TNTSR05L
072400960229   X5C                   ELSE
072500960229     C     TSRDKP        CABEQ     PARDTA        LEGG05                   19
072600960229     C     TSRDKP        SETGT     TNTSR05L
072700960229    5C                   END
072800960229     C                   GOTO      LEGG05
072900960229    4C                   END
073000960229    3C                   END
073100960229     C                   EXSR      ELAB
073200960229     C     LEGG05        TAG
073300960229     C  N19              READ      TNTSR05L                               19
073400960229    2C                   ENDDO
073500960229     C                   CLOSE     TNTSR05L
073600960229     C                   SETOFF                                       05
073700960129     C*
073800960229     C* T N T S R 0 9 L
073900960229     C* ENTRO NEL CICLO LETTURA PER DKA (SOLO SE NON è RISTAMPA)
074000960229     C* PER AGGIORNAMENTO DI TSRDKA (SE è RISTAMPA DEVO SOLO
074100960229     C* RICHIAMARE IL PROGRAMMA CHE STAMPA I RITARDI)
074200960229     C     *IN13         IFEQ      *ON
074300960229     C                   MOVE      *ZEROS        WDKA
074400960229     C                   OPEN      TNTSR09L
074500960229     C   11KTSR9         SETLL     TNTSR09L
074600960229     C  N11WDKA          SETLL     TNTSR09L
074700960229     C*
074800960229     C   11KTSR9         READE     TNTSR09L                               19
074900960229     C  N11WDKA          READE     TNTSR09L                               19
075000960129    2C     *IN19         DOWEQ     *OFF
075100960313     C     TSRDTT        IFGE      PARDTD
075200960313     C     *IN11         IFEQ      *OFF
075300960313     C     KTSR9S        SETGT     TNTSR09L
075400960313     C                   GOTO      LEGG09
075500960313     C                   END
075600960313     C     *IN11         CABEQ     *ON           LEGG09                   19
075700960313     C                   END
075800960201     C* Elaboro il record solo se non l'ho già elaborato durante la
075900960229     C* lettura del tntsr05l
076000960229     C     TSRDKP        IFNE      WDAT
076100960201     C     TSRDKP        ANDGT     0
076200960229     C     TSRDKP        IFLT      PARDTD
076300960229     C     TSRDKP        ORGT      PARDTA
076400960201     C                   EXSR      ELAB
076500960229     C                   END
076600960229     C                   END
076700960313     C     LEGG09        TAG
076800960313     C   11
076900960313     CANN19KTSR9         READE     TNTSR09L                               19
077000960313     C  N11
077100960313     CANN19WDKA          READE     TNTSR09L                               19
077200960129    2C                   ENDDO
077300960229     C                   CLOSE     TNTSR09L
077400960229     C                   SETOFF                                       09
077500960229     C*
077600960229     C                   END
077700960129     C*
077800960129     C* T N T S R 0 7 L
077900960201     C   13              MOVE      *ZEROS        TSRDKI
078000960201     C  N13              Z-ADD     1             TSRDKI
078100960201     C*
078200960201     C                   OPEN      TNTSR07L
078300960129    2C     *IN11         IFEQ      *ON
078400960129     C                   MOVE      V1CLNP        TSRFFP
078500960129     C     KTSR7         SETLL     TNTSR07L
078600960129     C                   ELSE
078700960209     C     TSRDKI        SETLL     TNTSR07L
078800960129    2C                   END
078900960129     C                   READ      TNTSR07L                               19
079000960129    2C     *IN19         DOWEQ     *OFF
079100960129     C*
079200960129    3C     TSRDKI        IFGT      *ZEROS
079300960129    4C     TSRDKI        IFLT      PARDTD
079400960129     C                   MOVE      PARDTD        TSRDKI
079500960129     C   11              MOVE      V1CLNP        TSRFFP
079600960129     C  N11              MOVE      *ZEROS        TSRFFP
079700960129     C     KTSR7         SETLL     TNTSR07L
079800960129     C                   GOTO      LEGG07
079900960129   X4C                   ELSE
080000960129     C     TSRDKI        CABGT     PARDTA        LEGG07               19
080100960129    4C                   END
080200960129    3C                   END
080300960129     C*
080400960129    3C     *IN11         IFEQ      *ON
080500960129    4C     TSRFFP        IFGT      V1CLNP
080600960129    5C     TSRDKI        IFEQ      *ZEROS
080700960129     C                   MOVE      PARDTD        TSRDKI
080800960129     C                   MOVE      V1CLNP        TSRFFP
080900960129     C     KTSR7         SETLL     TNTSR07L
081000960129   X5C                   ELSE
081100960129     C     TSRDKI        CABEQ     PARDTA        LEGG07                   19
081200960129     C     TSRDKI        SETGT     TNTSR07L
081300960129    5C                   END
081400960129     C                   GOTO      LEGG07
081500960129    4C                   END
081600960129    3C                   END
081700960129     C*
081800960129    3C     TSRDTT        IFGE      PARDTD
081900960129    4C     *IN11         IFEQ      *OFF
082000960129     C     KTSR7         SETGT     TNTSR07L
082100960209     C                   GOTO      LEGG07
082200960129   X4C                   ELSE
082300960129    5C     TSRDKI        IFEQ      *ZEROS
082400960129     C                   MOVE      PARDTD        TSRDKI
082500960129     C                   MOVE      V1CLNP        TSRFFP
082600960129     C     KTSR7         SETLL     TNTSR07L
082700960129   X5C                   ELSE
082800960129     C     TSRDKI        CABEQ     PARDTA        LEGG07                   19
082900960129     C     TSRDKI        SETGT     TNTSR07L
083000960129    5C                   END
083100960129     C                   GOTO      LEGG07
083200960129    4C                   END
083300960129    3C                   END
083400960201     C* elaboro il record solo se non l'ho già elaborato durante le
083500960201     C* letture precedenti
083600960229     C                   SELECT
083700960229     C     *IN13         WHENEQ    *ON
083800960229     C     TSRDKP        ANDNE     WDAT
083900960229     C     TSRDKP        ANDGT     0
084000960229     C     TSRDKA        ANDNE     WDAT
084100960229     C     TSRDKA        ANDGT     0
084200960229     C     TSRDKP        IFLT      PARDTD
084300960229     C     TSRDKP        ORGT      PARDTA
084400960229     C                   EXSR      ELAB
084500960229     C                   END
084600960229     C     *IN13         WHENEQ    *OFF
084700960229     C     TSRDKP        IFLT      PARDTD
084800960229     C     TSRDKP        ORGT      PARDTA
084900960229     C                   EXSR      ELAB
085000960229     C                   END
085100960229     C                   ENDSL
085200960129     C     LEGG07        TAG
085300960129     C  N19              READ      TNTSR07L                               19
085400960129    2C                   ENDDO
085500960129     C                   CLOSE     TNTSR07L
085600960207     C                   SETOFF                                       07
085700960129     C*
085800960129     C* T N T S R 0 2 L
085900951023     C* Richiesta una LNP in particolare:posizionam. per LNP/DTT
086000951024     C* Richieste tutte le LNP: posizionam.sul primo record
086100960201     C                   OPEN      TNTSR02L
086200960129    2C     *IN11         IFEQ      *ON
086300951031     C                   MOVE      V1CLNP        TSRFFP
086400951024     C                   MOVE      PARDTD        TSRDTT
086500960129     C     KTSR2         SETLL     TNTSR02L                               19
086600951103     C     V1CLNP        READE     TNTSR02L                               19
086700951025   X2C                   ELSE
086800951024     C     *LOVAL        SETLL     TNTSR02L
086900951103     C                   READ      TNTSR02L                               19
087000951025    2C                   END
087100951023     C*
087200951103    2C     *IN19         DOWEQ     *OFF
087300960325     C     TSRATB        CABNE     *BLANKS       LEGGI
087400951215     C* Verifico superamento data f.v. teorica al
087500951023    3C     TSRDTT        IFGT      PARDTA
087600960129     C   11              SETON                                        19
087700960129     C  N11TSRFFP        SETGT     TNTSR02L
087800951023     C                   GOTO      LEGGI
087900951023    3C                   END
088000951215     C* Se lettura per tutte le lnp verifico che data f.v. teor. letta
088100951215     C* sia > o = a data f.v. teorica dal
088200960129    3C  N11TSRDTT        IFLT      PARDTD
088300951103     C     KTSR2         SETGT     TNTSR02L
088400951023     C                   GOTO      LEGGI
088500951023    3C                   END
088600960129     C                   EXSR      ELAB
088700951023     C     LEGGI         TAG
088800960129     C  N11
088900960129     CANN19              READ      TNTSR02L                               19
089000960129     C   11
089100960129     CANN19V1CLNP        READE     TNTSR02L                               19
089200951023    2C                   ENDDO
089300960201     C                   CLOSE     TNTSR02L
089400960129     C*
089500951023    1C                   END
089600960201     C* RICHIAMO PER CHIUDERE FILES
089700960201     C                   MOVEL     'C'           D35TLA
089800960201     C                   MOVEL     DSTL35        KPJBU
089900960201     C                   CALL      'TNTL35R'
090000960201     C                   PARM                    KPJBA
090100960201     C*
090200951023     C                   ENDSR
090300960129     C**************   ELABORAZIONE RIGA GRIGLIA   ********************
090400960129     C     ELAB          BEGSR
090500960202     C*
090600960202     C                   CLEAR                   WPART
090700960202     C                   CLEAR                   WCONF
090800960304     C                   CLEAR                   WFAS
090900960227     C* GESTIONE ROTTURA TRAINO  PER ELABORAZIONI
091000960129     C                   EXSR      SRBRK
091100960208     C* VERIFICO ROTTURA DI FILIALE FERMATA PARTENZA
091200960208     C                   SETOFF                                       01
091300960208     C     WBRKTN        IFEQ      '1'
091400960208     C     TSRDTT        ORNE      COMDTT
091500960208     C     TSRNFP        ORNE      COMNFP
091600960208     C                   SETON                                        01
091700960208     C                   MOVE      TSRDTT        COMDTT
091800960208     C                   MOVE      TSRNFP        COMNFP
091900960208     C                   END
092000960202     C* AGGIORNAMENTO DATE CONTROLLO
092100960202     C   02              MOVE      TSRDTT        WDAT
092200960202     C     *IN13         IFEQ      *ON
092300960202     C     *IN02         OREQ      *ON
092400960205     C                   EXSR      SRRIG
092500960205     C                   EXSR      SRTES
092600960208     C  N01              CLEAR                   WCONF
092700960304     C  N01              CLEAR                   WFAS
092800960319     C     PARORA        IFEQ      *ZEROS
092900960205     C   02              UPDATE    TNTSR2
093000960229     C   05              UPDATE    TNTSR5
093100960229     C   09              UPDATE    TNTSR9
093200960205     C   07              UPDATE    TNTSR7
093300960319     C                   ELSE
093400960319     C   02              UNLOCK    TNTSR02L
093500960319     C   05              UNLOCK    TNTSR05L
093600960319     C   09              UNLOCK    TNTSR09L
093700960319     C   07              UNLOCK    TNTSR07L
093800960319     C                   END
093900960202     C                   END
094000960202     C* IN BASE ALLE OPZIONI DI STAMPA VERIFICO SE CI SONO ERRORI DA
094100960205     C* STAMPARE CHE NON SONO STATI RILEVATI IN ROUTINE SRRIG/SRTES
094200960202     C*
094300960229     C* - FOGLIO VIAGGIO TEORICO NON ABBINATO
094400960202    1C     WPART         IFEQ      *BLANKS
094500960209     C*   Se è cambiata la filiale fermata partenza  (*IN01 ON)
094600960208     C     *IN01         ANDEQ     *ON
094700960226     C*   Se Sono nel giro dei controlli per DKP o DTT
094800960229    2C     *IN05         IFEQ      *ON
094900960202     C     *IN02         OREQ      *ON
095000960202    3C     TSRDKP        IFGT      0
095100960202     C     TSRDKP        ANDNE     99999999
095200960202    4C     TSRNFV        IFEQ      *ZEROS
095300960229     C     V1CVUO        ANDEQ     'S'
095400960202     C                   MOVE      'B'           WPART
095500960202    4C                   END
095600960202    3C                   END
095700960202    2C                   END
095800960202    1C                   END
095900960202     C* IMPORTO NON CONFERMATO O CONFERMATO MANUALMENTE
096000960202    1C     WCONF         IFEQ      *BLANKS
096100960209     C*   Se è cambiata la filiale fermata partenza  (*IN01 ON)
096200960208     C     *IN01         ANDEQ     *ON
096300960202     C     V1CTAR        ANDEQ     'S'
096400960209     c*   Se sono nel giro dei controlli per errori partenza o arrivo
096500960229    2C     *IN05         IFEQ      *ON
096600960229     C     *IN09         OREQ      *ON
096700960202    3C     TSRDKI        IFGE      PARDTD
096800960202     C     TSRDKI        ANDLE     PARDTA
096900960202    4C     TSTCNF        IFEQ      *BLANKS
097000960202    5C     V1CCNF        IFEQ      'T'
097100960202     C     V1CCNF        OREQ      'N'
097200960202     C                   MOVE      'N'           WCONF
097300960202    5C                   END
097400960202   X4C                   ELSE
097500960202    5C     V1CCNF        IFEQ      'T'
097600960202     C     V1CCNF        OREQ      'A'
097700960202     C                   MOVE      'M'           WCONF
097800960202    5C                   END
097900960202    4C                   END
098000960202    3C                   END
098100960202   X2C                   ELSE
098200960209     C*  Se sono nel giro di controlli errori per importi non conf. o
098300960209     C*  per DTT
098400960202    3C     *IN07         IFEQ      *ON
098500960202     C     *IN02         OREQ      *ON
098600960202    4C     TSRDKI        IFGT      0
098700960202     C     TSRDKI        ANDNE     99999999
098800960202    5C     TSTCNF        IFEQ      *BLANKS
098900960202    6C     V1CCNF        IFEQ      'T'
099000960202     C     V1CCNF        OREQ      'N'
099100960202     C                   MOVE      'N'           WCONF
099200960202    6C                   END
099300960202   X5C                   ELSE
099400960202    6C     V1CCNF        IFEQ      'T'
099500960202     C     V1CCNF        OREQ      'A'
099600960202     C                   MOVE      'M'           WCONF
099700960202    6C                   END
099800960202    5C                   END
099900960202    4C                   END
100000960202    3C                   END
100100960202    2C                   END
100200960202    1C                   END
100300960202     C* ACCENDO INDICATORE PER STAMPA DATI DI TESTATA
100400960202     C     WCONF         COMP      *BLANKS                            1414
100500960304     C  N14WFAS          COMP      *BLANKS                            1414
100600960202     C*
100700960229     C* STAMPO ERRORI RICHIESTI SE CE NE SONO
100800960202     C     WCONF         IFNE      *BLANKS
100900960202     C     WPART         ORNE      *BLANKS
101000960304     C     WFAS          ORNE      *BLANKS
101100960202     C                   EXSR      PRTRIG
101200960202     C                   END
101300960130     C*
101400960130     C                   ENDSR
101500960130     C**************  GESTIONE ROTTURA TRAINO  ************************
101600960130     C     SRBRK         BEGSR
101700960227     C                   MOVE      ' '           WBRKTN            1
101800960130     C*
101900960130    1C     TSRTRN        IFNE      COMTRN
102000960130     C     TSRDTN        ORNE      COMDTN
102100960227     C                   MOVE      '1'           WBRKTN            1
102200960130     C* Chain su testata griglia NUOVA
102300960130     C                   CLEAR                   TNTST000
102400960130     C                   MOVE      TSRTRN        KTRN
102500960130     C                   MOVE      TSRDTN        KDTN
102600960130     C     KTST          CHAIN     TNTST01L                           30
102700010824
102800010824      * aggancio la tabella 'GEI' per il campo della tolleranza
102900010824     c                   clear                   dgei
103000010824     c                   clear                   tibs02ds
103100010824     c                   movel     'L'           t02tla
103200010824     c                   movel     'C'           t02mod
103300010824     c                   movel     knsif         t02sif
103400010824     c                   movel     'GEI'         t02cod
103500010824     c                   movel     tstdiv        t02ke1
103600010824     c                   call      'TIBS02R'
103700010824     c                   parm                    kpjba
103800010824     c                   parm                    tibs02ds
103900010824    5c                   movel     t02uni        dgei
104000010824      * calcolo il delta e controllo la tolleranza
104100010824     c                   eval      sav_tstili = tstili
104200010824     c                   if        tstipt <> tstili
104300010824     c                   eval      w_delta = (tstipt - tstili)
104400010910      * il delta deve essere come valore assoluto
104500010910     c                   mllzo     1             w_delta
104600010824     c                   if        w_delta <= §gettr
104700010824     c                   eval      sav_tstili = tstipt
104800010824     c                   endif
104900010824     c                   endif
105000960202     C* Richiamo routine conferma automatica importo se importo listino
105100960202     C* diverso da 0 e = importo f.v., se non ci sono buchi di f.v.
105200960213     C* se ho la causale contabilizzazione  e il cod.trazionista
105300960202    2C     TSTCNF        IFEQ      *BLANKS
105400960202     C     TSTIPT        ANDNE     *ZEROS
105500010824     C     TSTIPT        ANDEQ     sav_TSTILI
105600960202     C     TSTCAU        ANDNE     *BLANKS
105700960213     C     TSTPDR        ANDGT     *ZEROS
105800960319     C     PARORA        IFEQ      *ZEROS
105900960202     C                   EXSR      CONFIM
106000960319     C                   ELSE
106100960319     C                   UNLOCK    TNTST01L
106200960319     C                   END
106300960202    2C                   END
106400960202     C                   MOVE      TSRTRN        COMTRN
106500960202     C                   MOVE      TSRDTN        COMDTN
106600960130     C* Verifico se esistono buchi in attesa di essere chiusi
106700960130     C                   EXSR      BUCHI
106800960130    1C                   END
106900960130     C                   ENDSR
107000960130     C*****  DETERMINA ESISTENZA BUCHI IN ATTESA DI ESSERE CHIUSI  ****
107100960130     C     BUCHI         BEGSR
107200960130     C                   CLEAR                   WBUCHI
107300960130     C     KTSR4         SETLL     TNTSR04L
107400960130     C     KTSR4         READE     TNTSR04L                               30
107500960130     C     *IN30         DOWEQ     *OFF
107600960130     C     SRDKP4        IFEQ      *ZEROS
107700960130     C     SRDTP4        IFGT      WDATEU
107800960130     C     SRDTP4        OREQ      WDATEU
107900960307     C     SRHTP4        ANDGT     WORAC
108000960130     C                   MOVE      '1'           WBUCHI            1
108100960130     C                   END
108200960130     C                   END
108300960130     C     WBUCHI        IFEQ      *BLANKS
108400960130     C     KTSR4         READE     TNTSR04L                               30
108500960130     C                   ELSE
108600960130     C                   SETON                                        30
108700960130     C                   END
108800960130     C                   ENDDO
108900960130     C                   ENDSR
109000960130     C****************  CONFERMA AUTOMATICA IMPORTO  ******************
109100960130     C     CONFIM        BEGSR
109200960130     C     KTSR4         SETLL     TNTSR04L                               30
109300960130     C     *IN30         IFEQ      *OFF
109400960130     C                   MOVE      '1'           TSTCNF
109500960130     C                   Z-ADD     TSTILI        TSTICO
109600960130     C                   EXCEPT    AGGTST
109700960209     C* Se sono nei controlli per n.traino e ho confermato importo
109800960209     C* aggiorno DKI su tutte le righe
109900960205     C     PARDTD        IFEQ      *ZEROS
110000960205     C     KTSR3         SETLL     TNTSR03L
110100960205     C     KTSR3         READE     TNTSR03L                               30
110200960205     C     *IN30         DOWEQ     *OFF
110300960205     C                   MOVE      99999999      TSRDKI
110400960229     C                   MOVEL     'S'           TSRFKI
110500960205     C                   UPDATE    TNTSR3
110600960205     C     KTSR3         READE     TNTSR03L                               30
110700960205     C                   ENDDO
110800960205     C                   END
110900960130     C                   ELSE
111000960130     C                   UNLOCK    TNTST01L
111100960130     C                   END
111200960130     C                   ENDSR
111300960205     C*****************  ELABORAZIONE ERRORI DI RIGA  *****************
111400960205     C     SRRIG         BEGSR
111500960205     C                   MOVE      ' '           WPART             1
111600960205     C                   MOVE      ' '           WOKPRT            1
111700960205     C* DATA CONTROLLO BUCO/RITARDO PARTENZA (Data in cui il buco o il
111800960205     C* ritardo si è manifestato --> Per i FV teorici con data apertu-
111900960205     C* ra = data partenza, il buco o il ritardo si manifesta per forza
112000960205     C* nello stesso giorno. il problema sussiste per i FV teorici con
112100960205     C* data apertura < data partenza. In questo caso il buco o ritardo
112200960205     C* si potrà rilevare solo a partire dalla data/ora partenza in
112300960205     C* poi. Ragionamento analogo per determinazione data controllo
112400960205     C* buco/ritardo arrivo e data controllo importo non confermato)
112500960205    1C     TSRDKP        IFEQ      *ZEROS
112600960205     C     PARDTD        OREQ      *ZEROS
112700960205    2C     TSRNFV        IFEQ      *ZEROS
112800960205    3C     TSRDTP        IFLT      WDATEU
112900960205     C     TSRDTP        OREQ      WDATEU
113000960307     C     TSRHTP        ANDLE     WORAC
113100960826     C     TSRDKP        IFEQ      *ZEROS
113200960205     C                   Z-ADD     WDAT          TSRDKP
113300960229     C                   MOVEL     'S'           TSRFKP
113400960205     C                   END
113500960208     C   01V1CVUO        IFEQ      'S'
113600960205     C                   MOVE      'B'           WPART
113700960205     C                   MOVE      'S'           WOKPRT
113800960205     C                   END
113900960205    3C                   END
114000960205   X2C                   ELSE
114100970327     C* Se ho la causale ritardo il ritardo c'è sicuramente
114200970327   3AC     TSRRTP        IFNE      *BLANKS
114300970327     C                   Z-ADD     WDAT          TSRDKP
114400970327     C                   MOVEL     'S'           TSRFKP
114500970327  X3AC                   ELSE
114600960205    3C     TSRDTP        IFNE      TSRDRP
114700960205     C     TSRHTP        ORNE      TSRHRP
114800960205     C                   CLEAR                   DSTL35
114900960205     C                   MOVEL     'C'           D35TUP
115000960205     C                   MOVEL     'P'           D35EPA
115100960205     C                   MOVE      TSRDTP        D35DTE
115200960205     C                   MOVE      TSRHTP        D35HTE
115300960205     C                   MOVE      TSRDRP        D35DRE
115400960205     C                   MOVE      TSRHRP        D35HRE
115500960205     C                   MOVEL     DSTL35        KPJBU
115600960205     C                   CALL      'TNTL35R'
115700960205     C                   PARM                    KPJBA
115800960205     C                   MOVEL     KPJBU         DSTL35
115900960205    4C     D35FLG        IFNE      *BLANKS
116000960826     C     TSRDKP        IFEQ      *ZEROS
116100960205     C                   Z-ADD     WDAT          TSRDKP
116200960229     C                   MOVEL     'S'           TSRFKP
116300960205     C                   END
116400960205   X4C                   ELSE
116500960826     C     TSRDKP        IFEQ      *ZEROS
116600960205     C                   MOVE      99999999      TSRDKP
116700960229     C                   MOVEL     'S'           TSRFKP
116800960205     C                   END
116900960205    4C                   END
117000960205   X3C                   ELSE
117100960826     C     TSRDKP        IFEQ      *ZEROS
117200960205     C                   MOVE      99999999      TSRDKP
117300960229     C                   MOVEL     'S'           TSRFKP
117400960205     C                   END
117500960205    3C                   END
117600970327   3AC                   END
117700960205    2C                   END
117800960205    1C                   END
117900960228     C* DATA CONTROLLO BUCO/RITARDO ARRIVO (SOLO SE FV ABBINATO)
118000960228   1AC     TSRNFV        IFGT      *ZEROS
118100960319     C* Aggiorno DKA ed FKA solo se richiesta la stampa dei ritardi
118200960319     C* (Per evitare che la stampa delle variazioni di date/ore arrivo
118300960319     C* di fv già controllati venga fatta anche se il manca data o
118400960319     C* il ritardo non era ancora stato stampato - Per quanto riguarda
118500960319     C* l'aggiornamento di tsrdkp l'effettuo a prescindere altrimenti
118600960319     C* non potrei effettuare sbinamenti con la manutenzione griglia ed
118700960319     C* in ogni caso è molto più raro che ci siano dei ritardi partenza
118800000626     C* che in arrivo)
118900960319     C     V1CRIT        ANDNE     *BLANKS
119000960205    1C     TSRDKA        IFEQ      *ZEROS
119100960205     C     PARDTD        OREQ      *ZEROS
119200960205    2C     TSRDRA        IFEQ      *ZEROS
119300960205    3C     TSRDTA        IFLT      WDATEU
119400960205     C     TSRDTA        OREQ      WDATEU
119500960307     C     TSRHTA        ANDLE     WORAC
119600960227     C                   CLEAR                   WLOC
119700960228    4C     TSRDKA        IFEQ      *ZEROS
119800960223     C* VERIFICO SE IL FV E LOCALE PERCHE' IN QUESTO CASO LA DATA DI
119900960223     C* ARRIVO NON ARRIVERA' E QUINDI NON DEVO SEGNALARE ERRORE
120000960227     C                   MOVE      TSRFFA        W003A             3
120100971204     C                   CLEAR                   DSLV55
120200971204     C                   MOVEL     'P'           D55TPT
120300971204     C                   MOVEL     TSRFFA        D55LIN
120400971204     C                   MOVEL     TSRDTN        D55DRF
120500971204     C                   CALL      'FNLV55R'
120600971204     C                   PARM                    DSLV55
120700971204     C     D55ERR        IFNE      ' '
120800971204     C                   MOVEL     D55LIN        D55TFP
120900971204     C                   ENDIF
121000000626     C                   Z-ADD     D55TFP        TFPFFA
121100971204     C**
121200000626     C     TSRLNP        LOOKUP    SPT                                    31
121300000626    5C     *IN31         IFEQ      *ON
121400000626     C                   CLEAR                   DSLV55
121500000626     C                   MOVEL     'P'           D55TPT
121600000626     C                   MOVEL     TSRLNP        D55LIN
121700000626     C                   MOVEL     TSRDTN        D55DRF
121800000626     C                   CALL      'FNLV55R'
121900000626     C                   PARM                    DSLV55
122000000626    6C     D55ERR        IFNE      ' '
122100000626     C                   MOVEL     D55LIN        D55TFP
122200000626    6C                   ENDIF
122300000626     C                   Z-ADD     D55TFP        LNPFFA
122400000626   X5C                   ELSE
122500000626     C                   Z-ADD     TSRLNP        LNPFFA
122600000626    5C                   ENDIF
122700000626    5C     TFPFFA        IFEQ      LNPFFA
122800960227     C                   MOVE      'L'           WLOC              1
122900960227     C                   MOVE      *HIVAL        TSRDKA
123000960229     C                   MOVEL     'S'           TSRFKA
123100960227     C                   MOVE      TSRDTA        TSRDRA
123200960227     C                   MOVE      TSRHTA        TSRHRA
123300960227     C                   ELSE
123400960227     C                   Z-ADD     WDAT          TSRDKA
123500960229     C                   MOVEL     'S'           TSRFKA
123600960228    5C                   END
123700960223    4C                   END
123800960205    3C                   END
123900960205   X2C                   ELSE
124000970327     C* Se ho la causale ritardo, il ritardo c'è sicuramente
124100970327   3AC     TSRRTA        IFNE      *BLANKS
124200970327     C                   MOVE      WDAT          TSRDKA
124300970327     C                   MOVEL     'S'           TSRFKA
124400970327  X3AC                   ELSE
124500960205     C* Verifico se c'è ritardo/anticipo
124600960205    3C     TSRDRA        IFNE      TSRDTA
124700960205     C     TSRHRA        ORNE      TSRHTA
124800960205     C                   CLEAR                   DSTL35
124900960205     C                   MOVEL     'C'           D35TUP
125000960205     C                   MOVEL     'A'           D35EPA
125100960205     C                   MOVE      TSRDTA        D35DTE
125200960205     C                   MOVE      TSRHTA        D35HTE
125300960205     C                   MOVE      TSRDRA        D35DRE
125400960205     C                   MOVE      TSRHRA        D35HRE
125500960205     C                   MOVEL     DSTL35        KPJBU
125600960205     C                   CALL      'TNTL35R'
125700960205     C                   PARM                    KPJBA
125800960205     C                   MOVEL     KPJBU         DSTL35
125900960205    4C     D35FLG        IFNE      *BLANKS
126000960228    5C     TSRDKA        IFEQ      *ZEROS
126100960205     C                   MOVE      WDAT          TSRDKA
126200960229     C                   MOVEL     'S'           TSRFKA
126300960228    5C                   END
126400960228   X4C                   ELSE
126500960228    5C     TSRDKA        IFEQ      *ZEROS
126600960205     C                   MOVE      99999999      TSRDKA
126700960229     C                   MOVEL     'S'           TSRFKA
126800960228    5C                   END
126900960205    4C                   END
127000960228   X3C                   ELSE
127100960228    4C     TSRDKA        IFEQ      *ZEROS
127200960205     C                   MOVE      99999999      TSRDKA
127300960229     C                   MOVEL     'S'           TSRFKA
127400960228    4C                   END
127500960205    3C                   END
127600970327   3AC                   END
127700960205    2C                   END
127800960205    1C                   END
127900960228   1AC                   END
128000960205     C                   ENDSR
128100960205     C************** ELABORAZIONE ERRORI DI TESTATA *******************
128200960205     C     SRTES         BEGSR
128300960129     C*
128400960205     C                   MOVE      ' '           WCONF             1
128500960304     C                   MOVE      ' '           WFAS              1
128600960126     C* DATA CONTROLLO IMPORTO NON CONFERMATO
128700960126    1C     TSRNFP        IFEQ      TSTNFE
128800960208     C     TSRDKI        ANDEQ     *ZEROS
128900960226     C     TSRDKP        ANDNE     *ZEROS
129000960208     C     PARDTD        OREQ      *ZEROS
129100960205    2C     TSTCNF        IFEQ      *BLANKS
129200960129     C* WBUCHI=blanks --> non esistono buchi in attesa di essere chiusi
129300960129    3C     WBUCHI        IFEQ      *BLANKS
129400960205     C     PARDTD        IFGT      *ZEROS
129500960819     C     TSRDKI        OREQ      *ZEROS
129600960126     C                   MOVE      WDAT          TSRDKI
129700960229     C                   MOVEL     'S'           TSRFKI
129800960205     C                   END
129900960208     C     V1CCNF        IFEQ      'N'
130000960202     C     V1CCNF        OREQ      'T'
130100960202     C                   MOVE      'N'           WCONF
130200960205     C                   MOVE      'S'           WOKPRT
130300960202     C                   END
130400960126    3C                   END
130500960126   X2C                   ELSE
130600960130    3C     TSTCNF        IFEQ      '2'
130700960205     C     PARDTD        IFGT      *ZEROS
130800960819     C     TSRDKI        OREQ      *ZEROS
130900960130     C                   MOVE      WDAT          TSRDKI
131000960229     C                   MOVEL     'S'           TSRFKI
131100960205     C                   END
131200960208     C     V1CCNF        IFEQ      'T'
131300960202     C     V1CCNF        OREQ      'A'
131400960202     C                   MOVE      'M'           WCONF
131500960205     C                   MOVE      'S'           WOKPRT
131600960202     C                   END
131700960130   X3C                   ELSE
131800960205     C     PARDTD        IFGT      *ZEROS
131900960819     C     TSRDKI        OREQ      *ZEROS
132000960126     C                   MOVE      99999999      TSRDKI
132100960229     C                   MOVEL     'S'           TSRFKI
132200960205     C                   END
132300960130    3C                   END
132400960130    2C                   END
132500960130   X1C                   ELSE
132600960205     C     PARDTD        IFGT      *ZEROS
132700960208     C     TSRNFP        ANDNE     TSTNFE
132800960130     C                   MOVE      99999999      TSRDKI
132900960229     C                   MOVEL     'S'           TSRFKI
133000960205     C                   END
133100960126    1C                   END
133200960304     C* ELENCO TRAINI CON TRAZIONISTA FASULLO
133300960304    1C     TSRNFP        IFEQ      TSTNFE
133400960304     C     TSTPDR        ANDEQ     0465000
133500960304     C     V1CFAS        ANDEQ     'S'
133600960304     C                   MOVE      '1'           WFAS
133700960304     C                   END
133800960202     C*
133900960125     C                   ENDSR
134000960130     C****************  STAMPA RIGHE DI ROTTURA  **********************
134100960130     C     PRTBRK        BEGSR
134200960307     C*§Se è la prima volta che stampo dopo la stampa dei f.v. non
134300960307     c*§abbinati stampo riga di separazione+intestazione
134400960307    1C*§NOF      WPRT1     IFEQ *BLANKS
134500960307     C*§                   EXCPTSEPAR
134600960307     C*§                   MOVE '1'       WPRT1
134700960307     C*§                   EXCPTTEST1
134800960307    1C*§                   END
134900960130     C* A ROTTURA DI TRAINO DETERMINO CAMPI DI STAMPA
135000960227    1C***        WBRKT1    IFEQ '1'
135100960227     C     TSRTRN        IFNE      COMTR1
135200960227     C     TSRDTN        ORNE      COMDT1
135300960130     C* Rovescio data traino
135400960130     C                   CLEAR                   WLBDA8
135500960130     C                   MOVE      TSTDTN        G08INV
135600960130     C                   MOVEL     '3'           G08ERR
135700960130     C                   CALL      'XSRDA8'
135800960130     C                   PARM                    WLBDA8
135900960130     C                   MOVE      G08DAT        W0020             2 0
136000960130     C                   MOVEL     G08DAT        PRTDTN            6 0
136100960130     C                   MOVE      W0020         PRTDTN
136200960130     C* Evidenzio se si tratta di traino bis o eccezionale
136300960130     C                   MOVE      *BLANKS       PRTBIS            3
136400960130    2C     TSTBIS        IFEQ      'B'
136500960130     C                   MOVEL     'BIS'         PRTBIS
136600960130     C                   ELSE
136700960130    3C     TSTBIS        IFEQ      'E'
136800960130     C                   MOVEL     'ECC'         PRTBIS
136900960130    3C                   END
137000960130    2C                   END
137100960229     C* RICHIAMO ROUTINE DI CARICAMENTO SCHIERA FERMATE DI CARICO DEL
137200960229     C* TRAINO PER STAMPARLE
137300960319     C     PARDTD        IFGT      *ZEROS
137400960229     C                   MOVEL     DSTSR         WDSTSR
137500960229     C                   EXSR      RIECAR
137600960229     C                   MOVEL     WDSTSR        DSTSR
137700960319     C                   END
137800960130     C*
137900960208     C*          PARTRN    IFNE *ZEROS
138000960130     C     *INOF         IFEQ      *ON
138100960130     C                   EXCEPT    TESTA
138200960130     C                   EXCEPT    TEST1
138300960130     C                   SETOFF                                       OF
138400960130    2C                   END
138500960130     C                   EXCEPT    BRKTRN
138600960208     C*                    END
138700960227     C*****                MOVE *BLANKS   WBRKT1  1
138800960227     C                   MOVE      TSRTRN        COMTR1
138900960227     C                   MOVE      TSRDTN        COMDT1
139000960130    1C                   END
139100960130     C                   ENDSR
139200951103     C**************  DECODIFICHE CAMPI DI TESTATA  *******************
139300960129     C     PRTTES        BEGSR
139400951026     C* Pulizia campi di stampa
139500951031     C                   CLEAR                   PRTPDR
139600951103     C                   CLEAR                   PRTAUT
139700951027     C                   CLEAR                   PRTDFT
139800951026     C* Decodifico trazionista
139900960129     C     TSTPDR        COMP      *ZEROS                             1616
140000960129    1C     *IN16         IFEQ      *ON
140100960304     C     TSTPDR        ANDNE     0465000
140200000807     C     KTNTLZ        CHAIN     TNTLZ01L                           30
140300951108    2C     *IN30         IFEQ      *OFF
140400951108     C     TLZATB        ANDEQ     *BLANKS
140500951108     C                   MOVEL     TLZRSC        PRTPDR
140600951108    2C                   END
140700951108   X1C                   ELSE
140800951026     C* (Visto che la decodifica del trazionista è sul f.v. della
140900951026     C*  filiale che espone l'importo, posso decodificarlo solo se
141000951026     C*  l'importo del f.v. è diverso da 0 ( --> f.v. già ingrigliato))
141100951221    2C*          TSTIPT    IFNE *ZEROS
141200951026     C     KFVS1         CHAIN     TNFVS01L                           30
141300951103     C  N30              MOVEL     FVSDPD        PRTPDR           10
141400951221    2C*                    END
141500951108    1C                   END
141600951108     C*
141700951221    1C*          TSTILI    IFNE TSTIPT
141800951027     C* Calcolo delta importo
141900951221     C*          TSTILI    SUB  TSTIPT    PRTDLT
142000951221    1C*                    END
142100951026     C* Decodifico tipo automezzo
142200951221     C     TSTTMZ        IFNE      *BLANKS
142300951026     C                   MOVEL     'TU'          KCOD
142400951026     C                   MOVEL(P)  TSTTMZ        KKEY
142500951027     C     KTAB          CHAIN     TABEL00F                           30
142600951103     C  N30              MOVEL     TBLUNI        PRTAUT           10
142700951103     C   30              MOVE      *BLANKS       PRTAUT
142800951221     C                   END
142900951026     C* Rovescio data fattura
143000951026     C     TSTDFT        IFNE      *ZEROS
143100951026     C                   CLEAR                   WLBDA8
143200951026     C                   MOVE      TSTDFT        G08INV
143300951026     C                   MOVE      '3'           G08ERR
143400951026     C                   CALL      'XSRDA8'
143500951026     C                   PARM                    WLBDA8
143600951103     C                   MOVEL     G08DAT        PRTDFT            4 0
143700951026     C                   END
143800951026     C                   ENDSR
143900951026     C****************  STAMPA RIGA STORICO  **************************
144000951026     C     PRTRIG        BEGSR
144100951106     C                   MOVE      *BLANKS       PRTMSG           20
144200960126     C                   MOVE      *BLANKS       PRTMS1           20
144300960304     C                   MOVE      *BLANKS       PRTMS2           20
144400960304     C                   SETOFF                                       1510
144500960125     C                   CLEAR                   PRTFFP
144600960125     C                   CLEAR                   PRTDT6
144700960125     C                   CLEAR                   PRTDFV
144800951026     C* RICHIAMO ROUTINE DI STAMPA RIGHE DI ROTTURA
144900951026     C                   EXSR      PRTBRK
145000960223     C*
145100960223    1C     WPART         IFNE      *BLANKS
145200960223     C     *IN14         OREQ      *ON
145300951215     C* D a t i  f . v .  t e o r i c o
145400951027     C* decodifico filiale fermata partenza
145500951027     C     TSRFFP        CHAIN     AZORG01L                           30
145600951027     C  N30              MOVEL     ORGDES        PRTFFP           10
145700951215     C* rovescio data f.v. teorica
145800960223     C                   CLEAR                   WLBDA8
145900960223     C                   MOVE      TSRDTT        G08INV
146000951027     C                   MOVEL     '3'           G08ERR
146100951027     C                   CALL      'XSRDA8'
146200951027     C                   PARM                    WLBDA8
146300951102     C                   MOVE      G08DAT        W0020             2 0
146400951027     C                   MOVEL     G08DAT        PRTDT6            6 0
146500951102     C                   MOVE      W0020         PRTDT6
146600951215     C* D a t i   f o g l i o   v i a g g i o   r e a l e
146700951027     C* rovescio data f.v. se diversa da 0
146800960223    2C     TSRDFV        IFNE      *ZEROS
146900951027     C                   CLEAR                   WLBDA8
147000951027     C                   MOVE      TSRDFV        G08INV
147100951027     C                   MOVEL     '3'           G08ERR
147200951027     C                   CALL      'XSRDA8'
147300951027     C                   PARM                    WLBDA8
147400951103     C                   MOVEL     G08DAT        PRTDFV            4 0
147500960223    2C                   END
147600960201     C* Messaggio di manca abbinamento f.v.
147700960223    2C     WPART         IFEQ      'B'
147800951106     C                   MOVEL     MSG(1)        PRTMSG
147900960223    2C                   END
148000960126     C* GESTIONE STAMPA TESTATA TRAINO
148100960223    2C     *IN14         IFEQ      *ON
148200960129     C                   EXSR      PRTTES
148300960201     C* Messaggio di importo non confermato
148400960304    3C                   SELECT
148500960304     C     WCONF         WHENEQ    'N'
148600960223    4C     PRTMSG        IFNE      *BLANKS
148700960129     C                   SETON                                        15
148800960126     C                   MOVEL     MSG(2)        PRTMS1
148900960126     C                   ELSE
149000951107     C                   MOVEL     MSG(2)        PRTMSG
149100960223    4C                   END
149200960201     C* Messaggio di importo confermato manualmente
149300960304     C     WCONF         WHENEQ    'M'
149400960304    4C     PRTMSG        IFNE      *BLANKS
149500960129     C                   SETON                                        15
149600960126     C                   MOVEL     MSG(7)        PRTMS1
149700960126     C                   ELSE
149800951215     C                   MOVEL     MSG(7)        PRTMSG
149900960304    4C                   END
150000960304    3C                   ENDSL
150100960304     C* Messaggio di trazionista fasullo
150200960304    3C     WFAS          IFEQ      '1'
150300960304    4C                   SELECT
150400960304     C     PRTMSG        WHENNE    *BLANKS
150500960304     C     PRTMS1        ANDNE     *BLANKS
150600960304     C                   SETON                                        10
150700960304     C                   MOVEL     MSG(8)        PRTMS2
150800960304     C     PRTMSG        WHENNE    *BLANKS
150900960304     C     PRTMS1        ANDEQ     *BLANKS
151000960304     C                   SETON                                        15
151100960304     C                   MOVEL     MSG(8)        PRTMS1
151200960304     C                   OTHER
151300960304     C                   MOVEL     MSG(8)        PRTMSG
151400960304    4C                   ENDSL
151500960304    3C                   END
151600960223    2C                   END
151700010823      * sposto i campi degli importi in campi di comodo per la stampa
151800010823     c                   z-add     tstili        p_tstili
151900010823     c                   z-add     tstipt        p_tstipt
152000010823     c                   z-add     tstico        p_tstico
152100010911      * se gli importi sono tutti e 3 a 0 non devo stampare la divisa
152200010911     c                   if        tstili = 0 and tstipt = 0
152300010911     c                             and tstico = 0
152400010911     c                   clear                   p_tstdiv
152500010911     c                   else
152600010911     c                   eval      p_tstdiv = tstdiv
152700010911     c                   endif
152800951103     C* STAMPO RIGA
152900960304     C     *INOF         IFEQ      *ON
153000960304     C                   EXCEPT    TESTA
153100960304     C                   EXCEPT    TEST1
153200960304     C                   EXCEPT    BRKTRN
153300951103     C                   SETOFF                                       OF
153400960304     C                   END
153500951103     C                   EXCEPT    RIGTTT
153600960226     C                   SETOFF                                       14
153700960223    1C                   END
153800951106     C                   EXCEPT    RIGVUO
153900951026     C                   ENDSR
154000960229     C*********  CARICAMENTO FILIALI DI CARICO DEL TRAINO  ************
154100960229     C     RIECAR        BEGSR
154200960229     C                   CLEAR                   CAR
154300960229     C                   CLEAR                   PRO
154400960305     C                   MOVE      *ZEROS        X                 3 0
154500960229     C     KTSR1         SETLL     TNTSR01L
154600960229     C     KTSR1         READE(N)  TNTSR01L                               30
154700960229    1C     *IN30         DOWEQ     *OFF
154800960305     C     X             ANDLE     6
154900960229     C* NON CARICO LA PRIMA FILIALE DI CARICO PERCHè è ANCHE TERMINAL
155000960229     C* DI PARTENZA DEL TRAINO
155100960229    2C     TSRNFP        IFNE      1
155200960819     C     TSRATB        ANDEQ     *BLANKS
155300960229     C     TSRFFP        LOOKUP    CAR                                    30
155400960229    3C     *IN30         IFEQ      *OFF
155500960229     C* REPERISCO CODICE PROVINCIA
155600960229     C     TSRFFP        CHAIN     AZORG01L                           30
155700960229    4C     *IN30         IFEQ      *OFF
155800960229     C                   ADD       1             X
155900960305    5C     X             IFGT      5
156000960305     C                   MOVE      '.. '         PRO(5)
156100960305   X5C                   ELSE
156200960229     C                   MOVEL     ORGPRO        PRO(X)
156300960229     C                   MOVE      '/'           PRO(X)
156400960229     C                   MOVEL     TSRFFP        CAR(X)
156500960305    5C                   END
156600960229    4C                   END
156700960229    3C                   END
156800960229    2C                   END
156900960229     C     KTSR1         READE(N)  TNTSR01L                               30
157000960229    1C                   ENDDO
157100960229     C                   ENDSR
157200951009     C****************  ROUTINE INIZIALE  *****************************
157300951009     C     *INZSR        BEGSR
157400951009     C     *ENTRY        PLIST
157500951009     C                   PARM                    KPJBA
157600951031     C                   MOVEL     KPJBU         PARAM
157700951009     C*
157800951009     C                   Z-ADD     1             CODUT
157900951009     C                   CALL      'X§PARUT'
158000951009     C                   PARM                    UT§DSE
158100951027     C                   MOVEL     RAGUT         RSUT             20
158200951102     C*
158300951102     C     PARDTD        IFNE      *ZEROS
158400951102     C     PARDTA        ANDEQ     *ZEROS
158500951102     C                   Z-ADD     PARDTD        PARDTA
158600951102     C                   END
158700960125     C* RILEVO LA DATA DEL GIORNO E LA ROVESCIO
158800951027     C                   TIME                    WTIME            14 0          ORA E DATA
158900951027     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
159000960125     C                   CLEAR                   WLBDA8
159100960125     C                   MOVE      WDATE         G08DAT
159200960125     C                   CALL      'XSRDA8'
159300960125     C                   PARM                    WLBDA8
159400960126     C                   MOVE      G08INV        WDATEU
159500960125     C* RILEVO ORA DEL GIORNO
159600960307     C                   MOVEL     WTIME         WORA              4 0
159700960301     C* ORA PER CONTROLLI: ASSUME QUELLA RICEVUTA SE > 0
159800960301     C     PARORA        IFGT      *ZEROS
159900960301     C                   MOVE      PARORA        WORA
160000960301     C                   END
160100960307     C                   MOVE      WORA          WORAC             4 0
160200960229     C* ALLA DATA/ORA DEL GIORNO SOTTRAGGO UN'ORA
160300960229     C* LA DATA/ORA COSì CALCOLATA VERRà UTILIZZATA PER CONTROLLARE
160400960229     C* ERRORI DI BUCHI O IMPORTI NON CONFERMATI O RITARDI/ANTICIPI
160500960229     C* ED AGGIORNARE LE DATE CONTROLLO SU GRIGLIA TRAINI
160600960229     C                   CLEAR                   WXDTOR
160700960229     C                   MOVE      WDATEU        WXDT1
160800960307     C                   MOVE      WORAC         WXHM1
160900960229     C                   MOVE      '-'           WXFLG
161000960229     C                   MOVE      60            WXMIN
161100960229     C                   CALL      'XDTORA'
161200960229     C                   PARM                    WXDTOR
161300960229     C                   Z-ADD     WXDT2         WDATEU
161400960307     C                   Z-ADD     WXHM2         WORAC             4 0
161500951031     C* PREPARO PARAMETRI DI RICHIESTA PER LA STAMPA
161600951031     C                   MOVE      *ZEROS        PRMDTD
161700951031     C                   MOVE      *ZEROS        PRMDTA
161800951031     C                   MOVE      *ZEROS        PRMDTN
161900951102     C                   MOVE      *BLANKS       PRMLNP           20
162000951031     C     PARDTD        IFNE      *ZEROS
162100951031     C                   CLEAR                   WLBDA8
162200951031     C                   MOVE      PARDTD        G08INV
162300951031     C                   MOVEL     '3'           G08ERR
162400951031     C                   CALL      'XSRDA8'
162500951031     C                   PARM                    WLBDA8
162600951031     C                   MOVE      G08DAT        PRMDTD
162700951031     C     PARDTA        IFNE      PARDTD
162800951031     C                   CLEAR                   WLBDA8
162900951031     C                   MOVE      PARDTA        G08INV
163000951031     C                   MOVEL     '3'           G08ERR
163100951031     C                   CALL      'XSRDA8'
163200951031     C                   PARM                    WLBDA8
163300951031     C                   MOVE      G08DAT        PRMDTA
163400951031     C                   ELSE
163500951031     C                   MOVE      PRMDTD        PRMDTA
163600951031     C                   END
163700951031     C                   END
163800951031     C     PARDTN        IFNE      *ZEROS
163900951031     C                   CLEAR                   WLBDA8
164000951031     C                   MOVE      PARDTN        G08INV
164100951031     C                   MOVEL     '3'           G08ERR
164200951031     C                   CALL      'XSRDA8'
164300951031     C                   PARM                    WLBDA8
164400951031     C                   MOVE      G08DAT        PRMDTN
164500951031     C                   END
164600951031     C     V1CLNP        IFGT      *ZEROS
164700951031     C     V1CLNP        CHAIN     AZORG01L                           30
164800951031     C     *IN30         IFEQ      *OFF
164900951031     C                   MOVEL     ORGDES        PRMLNP
165000951031     C                   END
165100951031     C                   END
165200951102     C*
165300960307     C*§                   MOVE *ALL'-'   LIN198198
165400951009     C*
165500000626     C* CARICO IN SCHIERA ELENCO PO POSTE
165600000626     C                   CLEAR                   SPT
165700000626     C                   Z-ADD     1             II                3 0
165800000626     C     *LOVAL        SETLL     AZORG01L
165900000626     C                   READ      AZORG01L                               31
166000000626     C     *IN31         DOWEQ     *OFF
166100000626     C     ORGFVA        IFEQ      *BLANKS
166200000626     C     ORGFAG        ANDEQ     'F'
166300000626     C     ORGFVA        OREQ      *BLANKS
166400000626     C     ORGFAG        ANDEQ     'A'
166500000626     C                   MOVEL     ORGDE3        OG143
166600000626     C     §OGPT         IFEQ      'S'
166700000626     C                   Z-ADD     ORGFIL        SPT(II)
166800000626     C                   ADD       1             II
166900000626     C                   ENDIF
167000000626     C                   ENDIF
167100000626     C                   READ      AZORG01L                               31
167200000626     C                   ENDDO
167300000626     C*
167400000807     C     KTNTLZ        KLIST
167500000807     C                   KFLD                    KTLTIP            2
167600000807     C                   KFLD                    TSTPDR
167700000807     C                   MOVEL     'T '          KTLTIP
167800951020     C     KFVS          KLIST
167900951020     C                   KFLD                    KABN
168000951020     C                   KFLD                    KTRS
168100951020     C                   MOVE      'S'           KABN
168200951020     C                   CLEAR                   KTRS
168300951026     C     KFVS1         KLIST
168400951103     C                   KFLD                    TSRLNP
168500951103     C                   KFLD                    TSRNFV
168600010823     C     KFWS          KLIST
168700010823     C                   KFLD                    fvsLNP
168800010823     C                   KFLD                    fvsNFV
168900951023     C     KTAB          KLIST
169000951023     C                   KFLD                    CODUT
169100951023     C                   KFLD                    KCOD
169200951023     C                   KFLD                    KKEY
169300951031     C     KTST          KLIST
169400951031     C                   KFLD                    KTRN
169500951031     C                   KFLD                    KDTN
169600951024     C     KTSR1         KLIST
169700951024     C                   KFLD                    TSTTRN
169800951024     C                   KFLD                    TSTDTN
169900951024     C     KTSR2         KLIST
170000951031     C                   KFLD                    TSRFFP
170100951024     C                   KFLD                    TSRDTT
170200951026     C     KTSR3         KLIST
170300951026     C                   KFLD                    TSTTRN
170400951026     C                   KFLD                    TSTDTN
170500951026     C                   KFLD                    TSTFES
170600951127     C                   KFLD                    TSTNFE
170700951031     C     KTSR4         KLIST
170800951109     C                   KFLD                    KLNP
170900951109     C                   KFLD                    KNFV
171000951031     C                   KFLD                    TSTTRN
171100951031     C                   KFLD                    TSTDTN
171200951031     C                   MOVE      *ZEROS        KLNP
171300951109     C                   MOVE      *ZEROS        KNFV
171400951221     C     KTSR4B        KLIST
171500951221     C                   KFLD                    FVSLNP
171600951221     C                   KFLD                    FVSNFV
171700960229     C* ACCESSO A TNTSR05L
171800960229     C     KTSR5         KLIST
171900960229     C                   KFLD                    TSRDKP
172000960229     C                   KFLD                    TSRFFP
172100960229     C* ACCESSO A TNTSR09L
172200960229     C     KTSR9         KLIST
172300960229     C                   KFLD                    WDKA
172400960313     C                   KFLD                    V1CLNP
172500960313     C     KTSR9S        KLIST
172600960313     C                   KFLD                    TSRDKA
172700960313     C                   KFLD                    TSRFFP
172800960129     C* ACCESSO A TNTSR07L
172900960129     C     KTSR7         KLIST
173000960129     C                   KFLD                    TSRDKI
173100960129     C                   KFLD                    TSRFFP
173200960129     C*
173300960228     C     KFVD          KLIST
173400960228     C                   KFLD                    FVSLNP
173500960228     C                   KFLD                    FVSNFV
173600960228     C     KFV2          KLIST
173700960228     C                   KFLD                    FVSLNP
173800960228     C                   KFLD                    FVSNFV
173900960228     C                   KFLD                    WEPA
174000960228     C                   KFLD                    WTDH
174100960228     C                   MOVEL     'A'           WEPA
174200960228     C                   MOVEL     'R'           WTDH
174300960304     C     KFV4          KLIST
174400960304     C                   KFLD                    FVSLNP
174500960304     C                   KFLD                    FVSNFV
174600960304     C                   KFLD                    KEPA
174700960304     C                   KFLD                    KLAI
174800960304     C                   KFLD                    KTRC
174900951020     C*
175000951020     C     *LIKE         DEFINE    FVSABN        KABN
175100951020     C     *LIKE         DEFINE    FVSTRS        KTRS
175200951023     C     *LIKE         DEFINE    TBLCOD        KCOD
175300951023     C     *LIKE         DEFINE    TBLKEY        KKEY
175400951031     C     *LIKE         DEFINE    TSRFFP        COMLNP
175500951024     C     *LIKE         DEFINE    TSRDTN        COMDTN
175600951024     C     *LIKE         DEFINE    TSRTRN        COMTRN
175700960227     C     *LIKE         DEFINE    TSRDTN        COMDT1
175800960227     C     *LIKE         DEFINE    TSRTRN        COMTR1
175900951031     C     *LIKE         DEFINE    TSRRTP        PRTRIT
176000000626     C     *LIKE         DEFINE    TSRFFA        TFPFFA
176100000626     C     *LIKE         DEFINE    TSRLNP        LNPFFA
176200951031     C     *LIKE         DEFINE    TSTTRN        KTRN
176300951031     C     *LIKE         DEFINE    TSTDTN        KDTN
176400951221     C     *LIKE         DEFINE    TSTDTN        PRTDTR
176500951031     C     *LIKE         DEFINE    PARDTD        PRMDTD
176600951031     C     *LIKE         DEFINE    PARDTA        PRMDTA
176700951031     C     *LIKE         DEFINE    PARDTN        PRMDTN
176800951031     C     *LIKE         DEFINE    FVSDFV        PRTDF8
176900951031     C     *LIKE         DEFINE    TSRLNP        KLNP
177000951109     C     *LIKE         DEFINE    TSRNFV        KNFV
177100951102     C     *LIKE         DEFINE    D30DAT        W30DAT
177200960126     C     *LIKE         DEFINE    G08INV        WDATEU
177300960126     C     *LIKE         DEFINE    G08INV        WDAT
177400960305     C     *LIKE         DEFINE    G08INV        WDKF
177500960229     C     *LIKE         DEFINE    TSRDTT        COMDTT
177600960229     C     *LIKE         DEFINE    TSRNFP        COMNFP
177700960228     C     *LIKE         DEFINE    FV2EPA        WEPA
177800960228     C     *LIKE         DEFINE    FV2TDH        WTDH
177900960229     C     *LIKE         DEFINE    TSRDKA        WDKA
178000960304     C     *LIKE         DEFINE    FV4NOT        PRTNT1
178100960304     C     *LIKE         DEFINE    FV4NOT        PRTNT2
178200960304     C     *LIKE         DEFINE    FV4EPA        KEPA
178300960304     C     *LIKE         DEFINE    FV4TRC        KTRC
178400960304     C     *LIKE         DEFINE    FV4LAI        KLAI
178500951009     C                   ENDSR
178600951102     OPRTF198   E            TESTA          1  1
178700951031     O                       COS(1)              66
178800951031     O                       COS(2)             132
178900951031     O                       COS(3)             198
179000951027     O                       RSUT                20
179100960305     O                       WDATE              168 '  /  /    '
179200951030     O                       PAGE          Z    198
179300951030     O          E            TESTA                5
179400951030     O                       KNSIF               10
179500951030     O                       KNMUS               21
179600960229     O                       WORA               168 '  :  '
179700951102     O* PARAMETRI DI RICHIESTA
179800951102     O          E            PARRIC         1 10
179900951030     O                       PAR(1)             132
180000960305     O                       PRMDTD             113 '  /  /    '
180100960305     O                       PRMDTA             127 '  /  /    '
180200951031     O          E            PARRIC         1
180300951030     O                       PAR(2)             132
180400951215     O                       V1CLNP        Z    106
180500951215     O                       PRMLNP             127
180600951220     O          E            PARRIC         3
180700951030     O                       PAR(3)             132
180800960305     O                       PRMDTN              94 '  /  /    '
180900951221     O                       PARTRN        Z    121
181000951031     O          E            PARRIC         2
181100951030     O                       PAR(5)             132
181200951031     O          E            PARRIC         1
181300951030     O                       PAR(6)             132
181400951215     O                       V1CFVB             116
181500951220     O          E            PARRIC         1
181600951220     O                       PAR(7)             132
181700951220     O                       V1CABB             116
181800951031     O          E            PARRIC         1
181900951220     O                       PAR(8)             132
182000951215     O                       V1CVUO             116
182100951031     O          E            PARRIC         1
182200951220     O                       PAR(9)             132
182300951215     O                       V1CRIT             116
182400951031     O          E            PARRIC         1
182500951220     O                       PAR(10)            132
182600951215     O                       V1CTAR             116
182700951220     O          E            PARRIC         1
182800951220     O                       PAR(11)            132
182900951220     O                       V1CCNF             116
183000960304     O          E            PARRIC         1
183100960304     O                       PAR(12)            132
183200960304     O                       V1CFAS             116
183300951215     O* ELENCO FOGLI VIAGGIO REALI NON ABBINATI
183400951102     O          E            TESFV          2
183500951221     O                       FVN(1)             132
183600951221     O                       PRTTIT             130
183700951220     O          E            TESFV          1
183800951030     O                       FVN(3)              66
183900960228     O                       FVN(6)             132
184000960228     O                       FVN(9)             198
184100951220     O          E            TESFV          2
184200960228     O                       FVN(4)              66
184300960228     O                       FVN(7)             132
184400960228     O                       FVN(10)            198
184500960228     O          E            RIGARR      0  0
184600010823     O                       FV2LAI        Z    135
184700010823     O                       PRTDPA             141 '  /  '
184800010823     O                       FV2HPA             147 '  :  '
184900010823     O                       FV2RTC             149
185000960123     O          E            RIGFV          1
185100951103     O                       FVSLNP        Z      3
185200951115     O                       PRTLNP         B    14
185300951103     O                       FVSNFV        Z     21
185400960228     O                       PRTBIS              25
185500960305     O                       PRTDF8              36 '  /  /    '
185600960228     O                       FVSLNA        Z     41
185700960228     O                       PRTLNA              52
185800960228     O                       FVSTTR              56
185900960228     O               16      FVSPDR              65
186000960228     O                       PRTPDR              76
186100960228     O                       FVSTMZ              80
186200960228     O                       PRTTMZ              96
186300010823     O**!!!                  FVSIPT        2    109
186400010907     O                       fwsvip         b   100
186500010907     O                       p_fwsipt      2b   115
186600010823     O                       PRTDRP             122 '  /  '
186700010823     O                       FVSHRP             128 '  :  '
186800010823     O                       FVSRTP             130
186900010823     O                       PRTMTC        2    159
187000010823     O                       FVSTRN        Z    168
187100010823     O                       PRTDTR             181 '  /  /    '
187200010823     O              N17      TSTTRN        Z    189
187300010823     O              N17      TSTTFP        Z    194
187400010823     O              N17      TSTTFA        Z    198
187500960228     O          E            VUOTO          1
187600960304     O          E            RIGMOT         1
187700960305     O                       PRTNT1              77
187800960305     O                       PRTNT2             112
187900960307     O*§      E 13             SEPAR
188000960307     O*§                       LIN198   198
188100951124     O* STAMPA ANOMALIE SU TRAINI E/O BIS/ECCEZIONALI
188200951106     O          E            TEST1       0  0
188300951103     O                       TRZ(2)              66
188400951103     O                       TRZ(5)             132
188500951103     O                       TRZ(8)             198
188600951106     O          E            TEST1       0  1
188700960229     O                       TRZ(11)            132
188800951106     O          E            TEST1       0  0
188900951103     O                       TRZ(3)              66
189000951103     O                       TRZ(6)             132
189100951103     O                       TRZ(9)             198
189200960129     O          E            TEST1       0  2
189300960304     O                       TRZ(12)            132
189400951106     O*       E  2             BRKLNP
189500951106     O*                        TRZ,1     66
189600951106     O*                        TSRFFPZ   13
189700951106     O*                        PRTLNP    24
189800951106     O*                        PRTDTT8   39
189900951030     O          E            BRKTRN      0  0
190000951102     O                       PRTDTN               8 '  /  /  '
190100951103     O                       TSTTRN        Z     17
190200951103     O                       TSTTTR              20
190300951103     O                       TSTTFP        Z     25
190400951103     O                                           26 '/'
190500951103     O                       TSTTFA        Z     29
190600960229     O                       PRO(1)              33
190700960229     O                       PRO(2)              36
190800960229     O                       PRO(3)              39
190900960229     O                       PRO(4)              42
191000960229     O                       PRO(5)              45
191100960229     O                       PRTBIS              48
191200951103     O          E            BRKTRN      0  0
191300960229     O                       PRTBIS              48
191400960227     O          E            RIGTTT      0  1
191500960229     O                       TSRFFP        Z     53
191600960229     O                                           54 '-'
191700960229     O                       PRTFFP              64
191800960229     O                       PRTDT6              74 '  /  /  '
191900960229     O                       TSRNFV        Z     81
192000960229     O                       PRTDFV              89 '  /  '
192100960129     O               14 16   TSTPDR              98
192200960129     O               14      PRTPDR             109
192300960129     O               14      TSTTMZ             113
192400960129     O               14      PRTAUT             124
192500010823     O**!!!          14      TSTILI        2    137
192600010823     O**!!!          14      TSTIPT        2    150
192700960129     O*                14      PRTDLT2  162
192800010823     O**!!!          14      TSTICO        2    162
192900010911     o               14      p_tstdiv           129
193000010823     o               14      p_tstili      2    144
193100010823     o               14      p_tstipt      2    160
193200010823     o               14      p_tstico      2    176
193300010823     O               14      TSTNFT        Z    184
193400010823     O               14      PRTDFT             191 '  /  '
193500951106     O                       PRTMSG             198
193600960129     O          E    15      RIGTTT      0  1
193700960126     O                       PRTMS1             198
193800960304     O          E    10      RIGTTT      0  1
193900960304     O                       PRTMS2             198
194000951106     O          E            RIGVUO         1
194100951031     O          E            MESSAG      1  1
194200951031     O                       COS(6)              66
194300951031     O                       COS(7)             132
194400951215     O                       COS(8)             198
194500960305     O                       GI8DAT             107 '  /  /    '
194600951031     O          E            FINES       1  1
194700951215     O                       COS(9)             132
194800951102     O*
194900951031     OTNTST000  E            AGGTST
195000951031     O                       TSTICO
195100951031     O                       TSTCNF
195200960304**  PAR - PARAMETRI DI RICHIESTA
195300951218   Data Foglio viaggio teorica:  Dal            Al                   1
195400951218   Linea di partenza foglio viaggio:                                 2
195500951221   Data traino..:                Numero traino:                      3
195600951220                                                                     4
195700951220                    OPZIONI DI STAMPA                                5
195800960229   Fogli viaggio reali non abbinati............:   (S/)              6
195900951221   . . . . Abbinati manualm./Non abbinati/Tutti:   (A/N/T)           7
196000951221   Fogli viaggio teorici senza abbinam. a reali:   (S/ )             8
196100960229   Ritardi/anticipi ...........................:   (R/A/E/ )         9
196200951221   Traini non autorizzati al pagamento ........:   (S/ )            10
196300951221   . . . . Confermati/Non confermati/Tutti.....:   (C/N/T)          11
196400960304   Traini con trazionista "fasullo"............:   (S/ )            12
196500951031**  COS - INTESTAZIONE + PIEDI PAGINA
196600951020XXXXXXXXXXXXXXXXXXXX                                                 1
196700951124                 ***    CONTROLLO   TRAINI    ***                    2
196800010823TNTL41R                   XX/XX/XXXX                     PAG. XXXX   3
196900951020XXXXXXXXXX XXXXXXXXXX                                                4
197000960212                               XX:XX                                 5
197100951031ATTENZIONE! La presente stampa non risulta attendibile. Occorre co   6
197200951215ntrollare da data f.v. teorica XX/XX/XXXX di tutte le linee di par   7
197300951215tenza                                                                8
197400951215              ***   FINE   STAMPA   ***                              9
197500951103**  FVN  *************************************************************
197600960123  ELENCO FOGLI VIAGGIO REALI XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX     1
197700951103                                                                     2
197800960228LINEA PARTENZA  NUMERO     DATA       LINEA ARRIVO    TP  TRAZIONI   3
197900960228                                                      TR             4
198000960228XXX DDDDDDDDDD  XXXXX XXX XX/XX/XXXX  XXX DDDDDDDDDD   X  XXXXXXX    5
198100010823STA         TIPO AUTOMEZZO     DIV   IMPORTO F.V.  -- PARTENZA -     6
198200010823                                                    DATA  ORA  C     7
198300010823DDDDDDDDDD  XX DDDDDDDDDDDDDDD XXX XXX.XXX.XXX,XX  XX/XX XX:XX X     8
198400010823---- ARRIVO -----  METRI C. TRAINO ABB.  TRAINO ABBINATO IN SEDE     9
198500010823LNA  DATA  ORA  C            NEL P.O.    DATA      NUMERO  PAR/ARR  10
198600010823XXX XX/XX XX:XX X  XXX,XXXX  XXXXXXX   XX/XX/XXXX XXXXXXX  XXX/XXX  11
198700960228**  TRZ  *************************************************************
198800951106                                                                     1
198900960229--------------  T  R  A  I  N  O  --------------|FOGLIO VIAGGIO TE   2
199000961022DATA       NUMERO TIP PAR/ARR ALTRE FERM.CAR    | P.O. PARTENZA      3
199100960229XX/XX/XX  1234567  X  XXX/XXX XX/XX/XX/XX/.. BIS| XXX-1234567890     4
199200010823ORICO    |FOGLIO VIAGGIO|                                 | ------   5
199300010823  DATA   |   ABBINATO   |TRAZIONISTA        TIPO AUTOMEZZO|          6
199400010823XX/XX/XX | XXXXX  XX/XX |1234567 1234567890  XX 1234567890  XXX XX   7
199500010823--- I M P O R T O   P A T T U I T O --------|                        8
199600010823     LISTINO  FOGLIO VIAGGIO      CONFERMATO|    FATTURA             9
199700010823X.XXX.XXX,XX  XXX.XXX.XXX,XX  XXX.XXX.XXX,XX  XXXXXX  XX/XX         10
199800960229         |FOGLIO VIAGGIO|                                           11
199900960229         |   ABBINATO   |                                           12
200000951106** MSG
200100951106MANCA ABBIN. A F.V.                                                  1
200200951215IMP.NON CONFERMATO                                                   2
200300951122RITARDO                                                              3
200400951122ANTICIPO                                                             4
200500951215MANCA PARTENZA REALE                                                 5
200600951122MANCA ARRIVO REALE                                                   6
200700951215IMP.CONFERM.MANUALM.                                                 7
200800960304TRAZIONISTA FASULLO                                                  8

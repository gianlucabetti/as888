000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940720      * FNLR37R *----------------------------------------------------*
000300940720      *      VISUALIZZAZIONE DETTAGLIO SEGNACOLLI BOLLE ARRIVI
000400940720      *--------------------------------------------------------------*
000500940720     FFNLR37D   CF   E             WORKSTN
000600940720     F                                     SFILE(LR37S01:NRR1)
000700911024     FTABEL00F  IF   E           K DISK
000800940720     FFNART01L  IF   E           K DISK
000900051013     FFIARS01L  IF   E           K DISK
001000980312     FAZORG01L  IF   E           K DISK
001100970805      *--------------------------------------------------------------*
001200970805     D* Schiera codici consegne anomale + relativi flag
001300970805     D C7O             S              1    DIM(200)                             cod.cons.anom
001400970805     D F7O             S              1    DIM(200)                             flg cons.anom
001500970805      *--------------------------------------------------------------*
001600911024     D PARAM           DS
001700940720     D  PARAAS                14     17  0
001800940720     D  PARLNP                18     20  0
001900940720     D  PARNRS                21     22  0
002000940720     D  PARNSP                23     29  0
002100940720     D  PARF03                30     30
002200940720     D  PARFLG                31     31
002300940720     D  PARPGM                32     41
002400940720     D  PARDSA                42     61
002500940809     D  PARCML                62     62
002600940720     D  PARDSP                63     70  0
002700940720     D  PARRMN                71     85  0
002800940720     D  PARLNA                86     88  0
002900940720     D  PADLNA                89    108
003000940802     D  PARRSD               109    143
003100950126     D  PARGIA               144    144
003200950126     D  PARFEL               145    147  0
003300950629     D  PARTBO               148    148
003400980312     D                 DS
003500980312     D  NUM                    1      7
003600980312     D                                     DIM(7)                               DI COMODO
003700980312     D  WNUM                   1      7
003800980312     D                 DS
003900980312     D  NUE                    1      8
004000980312     D                                     DIM(8)                               DI COMODO
004100980312     D  WNUME                  1      8
004200980312     D                 DS
004300980312     D  VAR                    1     35
004400980312     D                                     DIM(35)                              DI COMODO
004500980312     D  VSCVAR                 1     35
004600941230     D PARAM1          DS                  INZ
004700920803     D  VI1NFV                 1      5  0
004800920803     D  VI1LNP                 6      8  0
004900920803     D  VI1LNA                 9     11  0
005000920803     D  VI1NRS                12     13  0
005100920803     D  VI1NCD                14     20  0
005200920803     D  VI1NCA                21     27  0
005300920803     D  VI1NPS                28     29  0
005400920803     D  VI1NPG                30     30  0
005500920813     D  VI1FLG                31     31
005600920803     D  VI1PGM                32     38
005700941230     D  VI1FGS                39     41  0
005800101202     d* bolla
005900101202     d  vi1bolp               42     44  0
006000101202     d  vi1bors               45     46  0
006100101202     d  vi1bosp               47     53  0
006200101202     d  vi1boan               54     57  0
006300101202     D  PA1ERR                58     58
006400940720     D WLBDAT          DS                  INZ
006500940720     D  G02DAT                 1      8  0
006600940720     D  G02INV                 9     16  0
006700940720     D  G02ERR                17     17
006800940720     D  G02TGI                18     22  0
006900961122     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
007000961122     D DSLR45        E DS                  EXTNAME(FNLR45DS)
007100900517     D KPJBA         E DS
007200120711     D DS3E          E DS
007300120711     D DS7O          E DS
007400000911     D DS7H          E DS
007500020121     D OG143         E DS
007600980312     D*
007700020227     D FUNF11          S             38    DIM(8) CTDATA PERRCD(1)
007800120620     D chkart          S             35    DIM(1) CTDATA PERRCD(1)
007900940720     C*-------------------------------------------------------------------------
008000941110     C     *ENTRY        PLIST
008100941110     C                   PARM                    KPJBA
008200941110     C*
008300910219     C                   MOVEL     KPJBU         PARAM
008400940720     C                   MOVEL     PARDSA        V1CDSA
008500940720     C                   MOVEL     PADLNA        VCDLNA                         DECODIFICA FILIALE
008600940720     C                   MOVEL     PARRSD        VCDRSD
008700930113     C*
008800000211     C* 15 ON  - SI TRATTA DI BOLLA EXPORT EURO? o EXPORT DPD
008900000211     C     PARLNA        CHAIN     AZORG01L
009000020121     C                   if        %found
009100020121     C                   movel     ORGde3        og143
009200020226     C**                 eval      *in15 = (ORGfl1 = 'E' or §OGDPD = 'S')
009300020226     C                   eval      *in15 = (§OgNtw = 'EEX' or §OgNtw ='EUP' or
009400020226     C                                     §OgNtw ='FED' or §OgNtw = 'DPD')
009500000211     C                   endif
009600961112     C*
009700920803     C* IMPOSTA CAMPI PER INT SPUNTE
009800920803     C                   Z-ADD     0             VI1NFV
009900950116     C                   MOVEL     *ZEROS        VI1LNP
010000920803     C                   MOVEL     PARLNA        VI1LNA
010100920803     C                   MOVEL     PARNRS        VI1NRS
010200101202     C                   MOVEL     PARNRS        VI1bors
010300101202     C                   MOVEL     PARlnp        VI1bolp
010400101202     C                   MOVEL     PARnsp        VI1bosp
010500101202     C                   MOVEL     PARaas        VI1boan
010600920803     C                   MOVEL     9             VI1NPG
010700920803     C                   Z-ADD     0             VI1NCD
010800920803     C                   Z-ADD     0             VI1NCA
010900920803     C                   MOVEL     99            VI1NPS
011000101202     C                   MOVEL     parpgm        VI1PGM
011100920813     C                   MOVEL     '1'           VI1FLG
011200910219     C*
011300910806     C* CARICO RIGHE DI DETTAGLIO SEGNACOLLI FLBLT
011400910806     C                   EXSR      CARBLT
011500910219     C*
011600020226     C* Gestisco abilitazione e descrizione tasto funzione F11
011700020226     C* solo se ho una delle due righe in alternanza : Fedex/EuroExpress
011800020226     C* o Chi sono/CML
011900020226     C     WESTERO1      IFNE      *BLANKS
012000020226     C     WESTERO2      ANDNE     *BLANKS
012100020226     C     *IN16         OREQ      *ON
012200020226     C                   EXSR      ABIF11
012300020226     C                   endif
012400020226      *
012500900509     C     FOR01         TAG
012600940720     C                   WRITE     LR37T01                                      TESTATA
012700940720     C                   WRITE     LR37Z01                                      TASTI FUNZIONALI
012800940720     C                   EXFMT     LR37C01                                      SFLCTL
012900940802     C*
013000980316     C                   SETOFF                                       11
013100950220     C                   CLEAR                   PARF03                         NO PREMUTO F3=FINE
013200940802     C   KC              MOVEL     '1'           PARF03                         PREMUTO F3=FINE
013300920803     C*
013400920804     C* CMD19 - INTERROGAZIONE SPUNTE
013500950125     C   KT              DO
013600950125     C                   MOVEL     PARAM1        KPJBU
013700950125     C                   CALL      'FNLR04R'                            99
013800920803     C                   PARM                    KPJBA
013900950125     C                   GOTO      FOR01
014000950125     C                   ENDDO
014100910219     C*
014200961122     C* CMD8  - INTERROGAZIONE EVENTI SEGNACOLLI
014300961122     C     *INKH         IFEQ      *ON
014400961122     C                   CLEAR                   DSLR45
014500961122     C                   MOVE      PARAAS        D45AAS
014600961122     C                   MOVE      PARLNP        D45FIL
014700961122     C                   MOVE      PARNRS        D45NRS
014800961122     C                   MOVE      PARNSP        D45NSP
014900961122     C                   MOVEL     DSLR45        KPJBU
015000961122     C                   CALL      'FNLR45R'
015100961122     C                   PARM                    KPJBA
015200961122     C                   GOTO      FOR01
015300961122     C                   ENDIF
015400020226      *
015500980312     C* CMD11 - INTERROGAZIONE VOLUME/SEGNACOLLO EUROEXPRESS
015600980312     C     *INKK         IFEQ      *ON
015700020226      *
015800980316     C                   SETON                                        11
015900980312     C                   EXSR      UPDS01
016000020226      *
016100020226     C                   EXSR      ABIF11POST
016200020226      *
016300020226      *
016400980312     C                   GOTO      FOR01
016500980312     C                   END
016600120620     c*
016700120620     c* F21 - nuovo dett colli
016800120620     c                   if        *inkv
016900120620     c                   movel     param         kpjbu
017000120620     c                   call      'FNLR38R'
017100120620     c                   parm                    kpjba
017200120620     c
017300120620     c                   goto      for01
017400120620     C                   END
017500961122     C*
017600000000     C     FINE          TAG
017700950125     C                   MOVEL     PARAM         KPJBU
017800000000     C                   SETON                                        LR
017900940720     C**************************************************************************
018000940720     C* CARICO DETTAGLIO SEGNACOLLI
018100940720     C**************************************************************************
018200910806     C     CARBLT        BEGSR
018300940720     C*
018400910205     C                   EXSR      PULSF
018500940720     C                   Z-ADD     0             NRR1              4 0
018600910806     C                   SETOFF                                       30
018700940720     C     KART1         SETLL     FNART01L
018800940720     C     KART1         READE     FNART01L                               30
018900940720     C  N30              MOVEL     ARTNSC        VI1NCD
019000950116     C  N30              MOVEL     ARTFLS        VI1LNP
019100910220     C*
019200910806     C     *IN30         DOWEQ     '0'
019300920813     C*
019400941219     C                   Z-ADD     ARTFLS        VSCFLS                         FIL. SEGNACOLLO
019500940720     C                   MOVEL     ARTNSC        VSCNSC                         NUMERO SEGNACOLLO
019600940720     C                   MOVEL     ARTFLP        VSCFLP                         FILIALE TRANSITO
019700940720     C*
019800940720     C                   MOVEL     *BLANKS       VSCCAN
019900920813     C     ARTCAN        IFNE      ' '
020000940720     C                   MOVEL     ' -'          VSCCAN
020100940720     C                   MOVEL     ARTCAN        VSCCAN
020200940720     C                   MOVEL     '3E'          KCOD
020300940720     C                   MOVEL     *BLANKS       KKEY
020400940720     C                   MOVEL     ARTCAN        KKEY
020500940720     C     KTAB1         CHAIN     TABEL00F                           31
020600120711     c  n31              movel     tbluni        ds3e
020700120711     c   31              clear                   ds3e
020800120711     C                   MOVEL(P)  §3ede12       ALF13            13
020900980312     C                   MOVE      ALF13         VSCCAN
021000920813     C                   END
021100910912     C* GIRO DATE:
021200910912     C* PARTENZA MERCE
021300940720     C                   MOVEL     ARTDFV        G02INV
021400940720     C                   Z-ADD     *ZEROS        G02DAT
021500940720     C                   MOVEL     '3'           G02ERR
021600940720     C                   CALL      'XSRDA8'
021700940720     C                   PARM                    WLBDAT
021800940720     C                   MOVEL     G02DAT        VSCDFV
021900950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
022000950116     C                   MOVE      DUE           VSCDFV
022100921231     C     ARTDFV        IFGT      0
022200940720     C                   MOVEL     ARTNPP        VSCNPP
022300921231     C                   ELSE
022400940720     C                   MOVEL     '  '          VSCNPP
022500921231     C                   END
022600921230     C* ENTRATA AL TRANSITO
022700941219     C                   MOVEL     ARTDET        G02INV                         AAAA/MM/GG
022800940720     C                   Z-ADD     *ZEROS        G02DAT
022900940720     C                   MOVEL     '3'           G02ERR
023000940720     C                   CALL      'XSRDA8'
023100940720     C                   PARM                    WLBDAT
023200941219     C                   MOVEL     G02DAT        VSCDET                         GG/MM/AA (6)
023300941219     C                   MOVE      G02DAT        DUE               2 0          ANNO 2 CIFRE
023400941219     C                   MOVE      DUE           VSCDET
023500921231     C     ARTDET        IFGT      0
023600940720     C                   MOVEL     ARTPET        VSCPET
023700921231     C                   ELSE
023800940720     C                   MOVEL     '  '          VSCPET
023900921231     C                   END
024000910912     C* USCITA DAL TRANSITO
024100940720     C                   MOVEL     ARTDUT        G02INV
024200940720     C                   Z-ADD     *ZEROS        G02DAT
024300940720     C                   MOVEL     '3'           G02ERR
024400940720     C                   CALL      'XSRDA8'
024500940720     C                   PARM                    WLBDAT
024600940720     C                   MOVEL     G02DAT        VSCDUT
024700941219     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
024800941219     C                   MOVE      DUE           VSCDUT
024900921231     C     ARTDUT        IFGT      0
025000940720     C                   MOVEL     ARTPUT        VSCPUT
025100921231     C                   ELSE
025200940720     C                   MOVEL     '  '          VSCPUT
025300921231     C                   END
025400910912     C* ARRIVO MERCE ALLA FIL ARRIVO
025500940720     C                   MOVEL     ARTDAM        G02INV
025600940720     C                   Z-ADD     *ZEROS        G02DAT
025700940720     C                   MOVEL     '3'           G02ERR
025800940720     C                   CALL      'XSRDA8'
025900940720     C                   PARM                    WLBDAT
026000940720     C                   MOVEL     G02DAT        VSCDAM
026100950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
026200950116     C                   MOVE      DUE           VSCDAM
026300921231     C     ARTDAM        IFGT      0
026400940720     C                   MOVEL     ARTNAP        VSCNAP
026500921231     C                   ELSE
026600940720     C                   MOVEL     '  '          VSCNAP
026700921231     C                   END
026800910912     C* CONSEGNA MERCE REALE
026900940720     C                   MOVEL     ARTDCM        G02INV
027000940720     C                   Z-ADD     *ZEROS        G02DAT
027100940720     C                   MOVEL     '3'           G02ERR
027200940720     C                   CALL      'XSRDA8'
027300940720     C                   PARM                    WLBDAT
027400940720     C                   MOVEL     G02DAT        VSCDCM
027500950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
027600950116     C                   MOVE      DUE           VSCDCM
027700980312     C**ARTDAB = 'S' COLLO DISABILITATO
027800980312     C**         ARTDCM    IFEQ 0
027900980312     C**         ARTDAB    ANDEQ'S'                        'S'=DISABILITATO
028000980312     C**                   SETON                     01
028100980312     C**                   ELSE
028200980312     C**                   SETOF                     01
028300980312     C**                   ENDIF
028400910912     C*
028500961112     C* SOLO PER LE BOLLE EXPORT (15 ON) VISUALIZZO LA DATA INVIO COLLO
028600961112     C     *IN15         IFEQ      *ON
028700060322     C***  §OGntw        andne     'DPD'
028800961112     C                   MOVEL     ARTDTR        G02INV
028900961112     C                   Z-ADD     *ZEROS        G02DAT
029000961112     C                   MOVEL     '3'           G02ERR
029100961112     C                   CALL      'XSRDA8'
029200961112     C                   PARM                    WLBDAT
029300961112     C                   MOVEL     G02DAT        VSCDTR
029400961112     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
029500961112     C                   MOVE      DUE           VSCDTR
029600961112     C                   ENDIF
029700000211      *
029800000211      * Aggancio estensione dettaglio segnacolli
029900020226      * A seconda della linea di arrivo aggancio prima tipo righe E o F
030000020226     C                   MOVE      *BLANKS       WESTERO1          1
030100020226     C                   MOVE      *BLANKS       WESTERO2          1
030200020226     C     §OGNTW        IFEQ      'FED'
030300020226     C                   MOVE      'F'           WTRC
030400020226     C                   ELSE
030500020226     C                   MOVE      'E'           WTRC
030600020226     C                   ENDIF
030700051013     C     KARS1         CHAIN     FIARS01L                           31
030800020226   1aC     *IN31         IFEQ      *OFF
030900000911      *
031000020226      * VERIFICO PRESENZA SEGNACOLLO EUROEXPRESS/FEDEX
031100000911     C     ARSNOT        IFNE      *BLANKS
031200020226     C                   MOVE      WTRC          WESTERO1          1
031300020227     C                   MOVE      WTRC          WINFO1
031400000911     C                   MOVEL     ARSNOT        VSCVA2
031500000911     C                   MOVE      '7H'          KCOD
031600000911     C                   MOVEL(P)  WTRC          KKEY
031700000911     C     KTAB1         CHAIN     TABEL00F                           38
031800020226     C     *in38         ifeq      *off
031900020226     C                   MOVEL     TBLUNI        DS7H
032000020226     C                   MOVEL     §7HDEB        VSC7H2
032100020226     C                   else
032200020226     C                   clear                   VSC7H2
032300020226     C                   endif
032400000911     C                   ELSE
032500000911     C                   CLEAR                   VSCVA2
032600000911     C                   CLEAR                   VSC7H2
032700000911     C                   ENDIF
032800011031      *
032900011031      **** riportato da sotto.
033000011031      * VISUALIZZO ANCHE LA DATA DI ARRIVO DEL COLLO AL PARTNER
033100011031     C                   MOVEL     ARSDAT        G02INV            8 0
033200011031     C                   Z-ADD     *ZEROS        G02DAT
033300011031     C                   MOVEL     '3'           G02ERR
033400011031     C                   CALL      'XSRDA8'
033500011031     C                   PARM                    WLBDAT
033600011031     C                   MOVEL     G02DAT        VSCDTA
033700011031     C* VISUALIZZO ANCHE EVENTUALE ANOMALIA DA PARTE DEL PARTNER
033800011031     C                   MOVEL     ARSCAN        VSCCAE
033900011031      *
034000000911     C                   ELSE
034100000911     C                   CLEAR                   VSCVA2
034200000911     C                   CLEAR                   VSC7H2
034300000911     C                   ENDIF
034400020226      *
034500020226      * Aggancio ora estensione dettaglio segnacolli del tipo E o F
034600020226      * che non ho agganciato prima
034700020226     C     §OGNTW        IFEQ      'FED'
034800020226     C                   MOVE      'E'           WTRC
034900020226     C                   ELSE
035000020226     C                   MOVE      'F'           WTRC
035100020226     C                   ENDIF
035200051013     C     KARS1         CHAIN     FIARS01L                           31
035300020226    1C     *IN31         IFEQ      *OFF
035400020226      *
035500020226      * VERIFICO PRESENZA SEGNACOLLO EUROEXPRESS/FEDEX
035600020226    2C     ARSNOT        IFNE      *BLANKS
035700020226      *
035800020226    3C     WESTERO1      IFNE      *BLANKS
035900020227     C                   MOVEL     ARSNOT        VSHEEZ
036000020226     C                   MOVE      WTRC          WESTERO2          1
036100020226   3eC                   ELSE
036200020226     C                   MOVEL     ARSNOT        VSCVA2
036300020226     C                   MOVE      WTRC          WESTERO1          1
036400020227     C                   MOVE      WTRC          WINFO1
036500020226   3-C                   ENDIF
036600020226      *
036700020226     C                   MOVE      '7H'          KCOD
036800020226     C                   MOVEL(P)  WTRC          KKEY
036900020226     C     KTAB1         CHAIN     TABEL00F                           38
037000020226    3C     *IN38         IFEQ      *OFF
037100020226     C                   MOVEL     TBLUNI        DS7H
037200020226    4C     WESTERO2      IFEQ      *BLANKS
037300020226     C                   MOVEL     §7HDEB        VSC7H2
037400020226   4eC                   ELSE
037500020227     C                   MOVEL     §7HDEB        VSHEEK
037600020226   4-C                   ENDIF
037700020226   3eC                   ELSE
037800020226    4C     WESTERO1      IFNE      *BLANKS
037900020226     C                   CLEAR                   VSC7H2
038000020226   4-C                   ENDIF
038100020226    4C     WESTERO2      IFNE      *BLANKS
038200020227     C                   CLEAR                   VSHEEK
038300020226   4-C                   ENDIF
038400020226   3-C                   ENDIF
038500020226   2eC                   ELSE
038600020226    3C     WESTERO1      IFNE      *BLANKS
038700020226     C                   CLEAR                   VSCVA2
038800020226     C                   CLEAR                   VSC7H2
038900020226   3-C                   ENDIF
039000020226    3C     WESTERO2      IFNE      *BLANKS
039100020226     C                   CLEAR                   VSHEEK
039200020226     C                   CLEAR                   VSHEEZ
039300020226   3-C                   ENDIF
039400020226   2-C                   ENDIF
039500020226      *
039600020226      **** riportato da sotto.
039700020226      * VISUALIZZO ANCHE LA DATA DI ARRIVO DEL COLLO AL PARTNER
039800020226     C                   MOVEL     ARSDAT        G02INV            8 0
039900020226     C                   Z-ADD     *ZEROS        G02DAT
040000020226     C                   MOVEL     '3'           G02ERR
040100020226     C                   CALL      'XSRDA8'
040200020226     C                   PARM                    WLBDAT
040300020226     C                   MOVEL     G02DAT        VSCDTA
040400020226     C* VISUALIZZO ANCHE EVENTUALE ANOMALIA DA PARTE DEL PARTNER
040500020226     C                   MOVEL     ARSCAN        VSCCAE
040600020226      *
040700020226   1eC                   ELSE
040800020227     C     WESTERO1      IFNE      *BLANKS
040900020226     C                   CLEAR                   VSHEEK
041000020226     C                   CLEAR                   VSHEEZ
041100020227     C                   ELSE
041200020226     C                   CLEAR                   VSCVA2
041300020226     C                   CLEAR                   VSC7H2
041400020227     C                   ENDIF
041500020226   1-C                   ENDIF
041600000911      *
041700000911     C                   MOVEL     'C'           WTRC
041800051013     C     KARS1         CHAIN     FIARS01L                           31
041900980312     C     *IN31         IFEQ      *OFF
042000000211      *
042100000911      * VERIFICO PRESENZA CODICE "CHI SONO"
042200980316     C     ARSNOT        IFNE      *BLANKS
042300000211     C                   eval      *in16 = *on
042400980312     C                   MOVEL     ARSNOT        VSCVAR
042500000911     C                   MOVE      '7H'          KCOD
042600000911     C                   MOVEL(P)  WTRC          KKEY
042700000911     C     KTAB1         CHAIN     TABEL00F                           38
042800000911     C  N38              MOVEL     TBLUNI        DS7H
042900000911     C  N38              MOVEL     §7HDEB        VSC7H1
043000000911     C   38              CLEAR                   VSC7H1
043100000911     C                   MOVEL     ARSNOT        VSHEEX                         "CHI SONO"
043200000911     C                   MOVEL     VSC7H1        VSHEEY                         DESCRIZIONE
043300000211      *
043400000911     C                   ELSE
043500000911     C                   CLEAR                   VSC7H1
043600000211     C                   ENDIF
043700000211      *
043800011031      ****** spostato tutto questo pezzo sotto l'aggancio al record
043900011031      ****** precedente per il tipo "E" (e non "C")
044000000211      * VISUALIZZO ANCHE LA DATA DI ARRIVO DEL COLLO AL PARTNER
044100011031     C******             MOVEL     ARSDAT        G02INV            8 0
044200011031     C******             Z-ADD     *ZEROS        G02DAT
044300011031     C******             MOVEL     '3'           G02ERR
044400011031     C******             CALL      'XSRDA8'
044500011031     C******             PARM                    WLBDAT
044600011031     C******             MOVEL     G02DAT        VSCDTA
044700980331     C* VISUALIZZO ANCHE EVENTUALE ANOMALIA DA PARTE DEL PARTNER
044800011031     C******             MOVEL     ARSCAN        VSCCAE
044900000211      *
045000980312     C                   ELSE
045100000911     C                   CLEAR                   VSC7H1
045200000211      *
045300051013      * SE NON C'È FIARS VISUALIZZO IL VOLUME CML
045400980312     C                   CLEAR                   VAR
045500980312     C                   MOVEA     'Vol'         VAR(22)
045600980312     C     ARTVUC        IFGT      0
045700980316     C                   Z-ADD     1             I                 1 0
045800980312     C                   Z-ADD     6             D                 1 0
045900980312     C                   MOVE      ARTVUC        WNUM              7
046000980312     C                   EXSR      EDITA
046100980312     C                   MOVEA     WNUME         VAR(26)
046200980312     C                   MOVEA     ARTFVC        VAR(35)
046300980312     C                   END
046400990913     C                   MOVEA     'Peso'        VAR(2)
046500990909     C     ARTPUC        IFGT      0
046600990909     C                   Z-ADD     4             I                 1 0
046700990909     C                   Z-ADD     3             D                 1 0
046800990909     C                   MOVE      ARTPUC        WNUM              7
046900990909     C                   EXSR      EDITA
047000990913     C                   MOVEA     WNUME         VAR(7)
047100990913     C                   MOVEA     ARTFPC        VAR(17)
047200990909     C                   END
047300980312     C                   END
047400000920      *
047500000920     C* memorizzo in ogni caso il volume cml E IL PESO
047600000920     C                   clear                   VSHFVC
047700000920     C                   clear                   VSHFPC
047800000920      *
047900000920     C     ARTVUC        IFGT      0
048000000920     C                   MOVEL     ARTFVC        VSHFVC
048100000920     C                   END
048200000920     C                   MOVEL     ARTVUC        VSHVUC
048300000920      *
048400000920     C     ARTPUC        IFGT      0
048500000920     C                   MOVEL     ARTFPC        VSHFPC
048600000920     C                   END
048700000920     C                   MOVEL     ARTPUC        VSHPUC
048800000211      *
048900000211      * SEGNALO SE SEGNACOLLO IN IDD
049000970805     C                   MOVEL     *BLANKS       V1CIDD
049100970805     C                   Z-ADD     1             X
049200970805     C     ARTFL1        LOOKUP    C7O(X)                                 34
049300970805     C     *IN34         IFEQ      '1'
049400970805     C     F7O(X)        ANDNE     *BLANKS
049500970805     C                   MOVEL     'IDD'         V1CIDD
049600970805     C                   END
049700961112     C*
049800910205     C* SCRIVO SUBFILE
049900940720     C                   ADD       1             NRR1
050000940720     C                   WRITE     LR37S01
050100910912     C*
050200940720     C     KART1         READE     FNART01L                               30
050300910205     C                   END
050400920803     C*
050500920803     C* SE PIU' DI UN COLLO MEMORIZZO ANCHE AL
050600940720     C     ARTNSC        IFNE      VI1NCD
050700940720     C                   MOVEL     ARTNSC        VI1NCA
050800920803     C                   END
050900910205     C*
051000940720     C                   Z-ADD     NRR1          SAVNRR            4 0
051100921104     C*
051200940720     C     NRR1          IFEQ      0
051300940720     C                   Z-ADD     0             VSCNSC
051400940720     C                   Z-ADD     0             VSCDFV
051500940720     C                   MOVEL     '  '          VSCNPP
051600940720     C                   Z-ADD     0             VSCDET
051700940720     C                   MOVEL     '  '          VSCPET
051800940720     C                   MOVEL     000           VSCFLP
051900940720     C                   Z-ADD     0             VSCDUT
052000940720     C                   MOVEL     '  '          VSCPUT
052100940720     C                   Z-ADD     0             VSCDAM
052200940720     C                   MOVEL     '  '          VSCNAP
052300940720     C                   Z-ADD     0             VSCDCM
052400940720     C                   MOVEL     *BLANKS       VSCCAN
052500961112     C                   CLEAR                   VSCDTR
052600980312     C                   CLEAR                   VSCDTA
052700980312     C                   CLEAR                   VSCVAR
052800000911     C                   CLEAR                   VSCVA2
052900980312     C                   CLEAR                   VSHEEX
053000000911     C                   CLEAR                   VSHEEY
053100980312     C                   CLEAR                   VSHVUC
053200980312     C                   CLEAR                   VSHFVC
053300990913     C                   CLEAR                   VSHFPC
053400990909     C                   CLEAR                   VSHPUC
053500000911     C                   CLEAR                   VSC7H1
053600000911     C                   CLEAR                   VSC7H2
053700940720     C                   ADD       1             NRR1
053800940720     C                   WRITE     LR37S01
053900921104     C                   END
054000910205     C*
054100910205     C                   ENDSR
054200020226     C**************************************************************************
054300020226     C* GESTIONE ABILITAZIONE TASTO FUNZIONE F11
054400020226     C**************************************************************************
054500020226     C     ABIF11        BEGSR
054600020226     C                   seton                                        17
054700020226      * Vari casi per valorizzazre tasto funzione
054800020226     C                   SELECT
054900020226     C
055000020226     C     *in16         wheneq    *on
055100020226     C     WEstero2      andeq     *blanks
055200020226     C                   movel     FunF11(1)     VZCF11
055300020226      *
055400020226     C     *in16         wheneq    *off
055500020226     C     WEstero1      andne     *blanks
055600020226     C     WEstero2      andne     *blanks
055700020226      * Scelgo la descrizione del tasto F11 a seconda che abia caricato
055800020226      * prima tipo record 'F' o tipo record 'E'
055900020227     C     WInfo1        ifeq      'F'
056000020226     C                   movel     FunF11(3)     VZCF11
056100020227     C                   move      'E'           WInfo1            1
056200020226     C                   else
056300020226     C                   movel     FunF11(4)     VZCF11
056400020227     C                   move      'F'           WInfo1            1
056500020226     C                   endif
056600020226      *
056700020226     C     *in16         wheneq    *on
056800020226     C     WEstero1      andne     *blanks
056900020226     C     WEstero2      andne     *blanks
057000020227     C     WInfo1        ifeq      'F'
057100020226     C                   movel     FunF11(5)     VZCF11
057200020227     C                   move      'E'           WInfo1            1
057300020226     C                   else
057400020226     C                   movel     FunF11(6)     VZCF11
057500020227     C                   move      'F'           WInfo1            1
057600020226     C                   endif
057700020226     C                   ENDSL
057800020226      *
057900020227      * La prima volta imposto la richiesta CML su F11 perchè è
058000020227      * caricato a sfl il CHI SONO.
058100020226     C     *IN16         IFEQ      *ON
058200020227     C                   SELECT
058300020227     C     WInfo         wheneq    *blanks
058400020226     C                   MOVEL     'S'           WINFO             1
058500020227     C     WInfo         wheneq    'S'
058600020227     C                   MOVEL     'V'           WINFO             1
058700020227     C     WInfo         wheneq    'V'
058800020227     C                   MOVEL     'S'           WINFO             1
058900020227     C                   ENDSL
059000020227     C                   ENDIF
059100020226      *
059200020226     C                   ENDSR
059300020226     C**************************************************************************
059400020226     C* GESTIONE ABILITAZIONE TASTO FUNZIONE F11 DOPO AGGIORNAMENTO SFL
059500020226     C**************************************************************************
059600020226     C     ABIF11POST    BEGSR
059700020227      *
059800020226      * Modifico la descrizione tasto funzione
059900020226     C                   SELECT
060000020226     C
060100020226     C     WInfo         wheneq    'V'
060200020226     C     WEstero2      andeq     *blanks
060300020227     C                   movel     FunF11(1)     VZCF11
060400020226      *
060500020226     C     WInfo         wheneq    'S'
060600020226     C     WEstero2      andeq     *blanks
060700020227     C                   movel     FunF11(2)     VZCF11
060800020226      *
060900020226     C     WInfo         wheneq    *blanks
061000020226     C     WEstero1      andne     *blanks
061100020226     C     WEstero2      andne     *blanks
061200020226     C     WInfo1        ifeq      'F'
061300020226     C                   movel     FunF11(3)     VZCF11
061400020226     C                   else
061500020226     C                   movel     FunF11(4)     VZCF11
061600020226     C                   endif
061700020226      *
061800020226     C     WInfo         whenne    *blanks
061900020226     C     WEstero1      andne     *blanks
062000020226     C     WEstero2      andne     *blanks
062100020226      *
062200020226     C     WInfo         ifeq      'V'
062300020227     C     WInfo1        ifeq      'F'
062400020227     C                   movel     FunF11(5)     VZCF11
062500020227     C                   else
062600020227     C                   movel     FunF11(6)     VZCF11
062700020227     C                   endif
062800020227      *
062900020226     C                   else
063000020227     C     WInfo1        ifeq      'F'
063100020227     C                   movel     FunF11(7)     VZCF11
063200020227     C                   else
063300020227     C                   movel     FunF11(8)     VZCF11
063400020227     C                   endif
063500020226     C                   endif
063600020226     C                   ENDSL
063700020227      *
063800020227      * Imposto per il cambio di visualizzazione successivo
063900020227      * CHI SONO/CML
064000020227     C                   SELECT
064100020227     C     WINFO         WHENEQ    'S'
064200020227     C                   MOVE      'V'           WINFO
064300020227     C     WINFO         WHENEQ    'V'
064400020227     C                   MOVE      'S'           WINFO
064500020227     C                   ENDSL
064600020227      *
064700020227      * Imposto per il cambio di visualizzazione successivo
064800020227      * EuroExpress/FedEx
064900020227     C                   SELECT
065000020227     C     WINFO1        WHENEQ    'F'
065100020227     C                   MOVE      'E'           WINFO1
065200020227     C     WINFO1        WHENEQ    'E'
065300020227     C                   MOVE      'F'           WINFO1
065400020227     C                   ENDSL
065500020226      *
065600020226     C                   ENDSR
065700980312     C**************************************************************************
065800980312     C* EDITAZIONE CAMPO NUMERICO
065900980312     C**************************************************************************
066000980312     C     EDITA         BEGSR
066100980312     C* MUOVO PRIMA I DECIMALI
066200980312     C     D             IFGT      0
066300980312     C     I             ADD       1             II                1 0
066400980316     C     I             ADD       2             DD                1 0
066500980316     C                   MOVEA     NUM(II)       NUE(DD)
066600980316     C                   MOVE      ','           NUE(II)
066700980312     C                   END
066800980312     C* POI GLI INTERI
066900980312     C* PRIMA ABBLENCO GLI ZERI NON SIGNIFICATIVI
067000980312     C                   MOVE      *BLANKS       WFINE             1
067100980312     C                   Z-ADD     1             II
067200980316     C     I             SUB       1             IM                1 0
067300980312     C     WFINE         DOWEQ     *BLANKS
067400980312     C     II            ANDLE     IM
067500980312     C     NUM(II)       IFEQ      '0'
067600980312     C                   MOVE      *BLANKS       NUM(II)
067700980312     C                   ELSE
067800980312     C                   MOVE      '1'           WFINE
067900980312     C                   END
068000980312     C                   ADD       1             II
068100980312     C                   ENDDO
068200980312     C* POI SCRIVO LA PARTE INTERA NEL CAMPO DI OUTPUT
068300980316     C                   SUB       2             DD
068400980312     C                   Z-ADD     I             II
068500980312     C                   DO        I
068600980312     C                   MOVE      NUM(II)       NUE(DD)
068700980312     C                   SUB       1             DD
068800980312     C                   SUB       1             II
068900980312     C                   ENDDO
069000980312     C*
069100980312     C                   ENDSR
069200980312     C**************************************************************************
069300980312     C* AGGIORNAMENTO SUBFILE PER INTERROGAZIONE ALTRI DATI
069400980312     C**************************************************************************
069500980312     C     UPDS01        BEGSR
069600980312     C                   Z-ADD     1             NRR1
069700980312     C     NRR1          CHAIN     LR37S01                            31
069800980312     C     *IN31         DOWEQ     *OFF
069900980312     C* VISUALIZZO VOLUME
070000980312     C     WINFO         IFEQ      'S'
070100980312     C                   CLEAR                   VAR
070200000911     C                   CLEAR                   VSC7H1
070300980312     C                   MOVEA     'Vol'         VAR(22)
070400980312     C     VSHVUC        IFGT      0
070500980316     C                   Z-ADD     1             I                 1 0
070600980312     C                   Z-ADD     6             D                 1 0
070700980312     C                   MOVE      VSHVUC        WNUM              7
070800980312     C                   EXSR      EDITA
070900980312     C                   MOVEA     WNUME         VAR(26)
071000980312     C                   MOVEA     VSHFVC        VAR(35)
071100980312     C                   END
071200990913     C                   MOVEA     'Peso'        VAR(2)
071300990909     C     VSHPUC        IFGT      0
071400990909     C                   Z-ADD     4             I                 1 0
071500990909     C                   Z-ADD     3             D                 1 0
071600990909     C                   MOVE      VSHPUC        WNUM              7
071700990909     C                   EXSR      EDITA
071800990913     C                   MOVEA     WNUME         VAR(7)
071900990913     C                   MOVEA     VSHFPC        VAR(17)
072000990909     C                   END
072100980312     C                   ELSE
072200000911     C* VISUALIZZO "CHI SONO"
072300980312     C                   MOVEL     VSHEEX        VSCVAR
072400000911     C                   MOVEL     VSHEEY        VSC7H1
072500980312     C                   END
072600020226     C* Visualizzo in alterannza Fedex/EuroExpress
072700020226     C     WEstero1      ifne      *blanks
072800020226     C     WEstero2      andne     *blanks
072900020226     C                   movel(P)  vsc7h2        wsc7h2           35
073000020226     C                   movel(P)  vscva2        wscva2           11
073100020226     C                   movel(P)  vsheek        vsc7h2
073200020226     C                   movel(P)  vsheez        vscva2
073300020226     C                   movel(P)  wsc7h2        vsheek
073400020227     C                   movel(P)  wscva2        vsheez
073500020226     C                   endif
073600020226      *
073700980312     C                   UPDATE    LR37S01
073800980312     C                   ADD       1             NRR1
073900980312     C     NRR1          CHAIN     LR37S01                            31
074000980312     C                   ENDDO
074100980312     C                   ENDSR
074200940720     C**************************************************************************
074300940720     C* PULIZIA SUBFILE
074400940720     C**************************************************************************
074500910205     C     PULSF         BEGSR
074600940720     C*
074700910205     C                   SETON                                        35
074800940720     C                   WRITE     LR37C01                                      SFLCTL
074900910205     C                   SETOFF                                       35
075000940720     C*
075100910205     C                   ENDSR
075200940720     C**************************************************************************
075300940720     C*  ROUTINE  INIZIALE
075400940720     C**************************************************************************
075500940720     C     *INZSR        BEGSR
075600940720     C*
075700940720     C     KART1         KLIST                                                  FNART01L
075800940720     C                   KFLD                    PARAAS                         ..ANNO
075900940720     C                   KFLD                    PARLNP                         ..LINEA PARTENZA
076000940720     C                   KFLD                    PARNRS                         ..NUMERO SERIE
076100940720     C                   KFLD                    PARNSP                         ..NUMERO SPEDIZIONE
076200940720     C     KTAB1         KLIST                                                  TABEL00F
076300940720     C                   KFLD                    KKUT                           ..CODICE UTENTE
076400940720     C                   KFLD                    KCOD                           ..CODICE TABELLA
076500940720     C                   KFLD                    KKEY                           ..CODICE ELEMENTO
076600970805     C     KTAB2         KLIST                                                  TABEL00F
076700970805     C                   KFLD                    KKUT                           ..CODICE UTENTE
076800970805     C                   KFLD                    KCOD                           ..CODICE TABELLA
076900980312     C     KARS1         KLIST                                                  FNART01L
077000980312     C                   KFLD                    ARTFLS
077100980312     C                   KFLD                    ARTLNA                                     ENZA
077200980312     C                   KFLD                    ARTNRS                                     IE
077300980312     C                   KFLD                    ARTNSC                                     DIZIONE
077400980312     C                   KFLD                    WTRC                                       DIZIONE
077500940720     C*
077600940720     C     *LIKE         DEFINE    TBLKUT        KKUT                           CODICE UTENTE
077700940720     C     *LIKE         DEFINE    TBLCOD        KCOD                           CODICE TABELLA
077800940720     C     *LIKE         DEFINE    TBLKEY        KKEY                           CODICE ELEMENTO
077900980312     C     *LIKE         DEFINE    ARSTRC        WTRC                                       NTO
078000940720     C*
078100940720     C                   Z-ADD     1             KKUT
078200970805     C*
078300970805     C* Eseguo caricamento della tabella dei codici consegna anomala
078400970805     C                   Z-ADD     0             X                 3 0
078500970805     C                   MOVEL     '7O'          KCOD
078600970805     C     KTAB2         CHAIN     TABEL                              34
078700970805     C     *IN34         DOWEQ     '0'
078800970805     C     X             ANDLT     100
078900970805     C     TBLFLG        IFEQ      *BLANKS
079000970805     C                   ADD       1             X
079100970805     C                   MOVEL     TBLKEY        C7O(X)
079200970805     C                   MOVEL     TBLUNI        DS7O
079300970805     C                   MOVEL     §7OIDD        F7O(X)
079400970805     C                   END
079500970805     C     KTAB2         READE     TABEL                                  34
079600970805     C                   END
079700120620     c* controllo se esiste nuovo file FIART
079800120620     c                   eval      comman=chkart(1)
079900120620     c                   eval      lengh=35
080000120620     c                   call      'QCMDEXC'                            99
080100120620     c                   parm                    comman           80
080200120620     c                   parm                    lengh            15 5
080300120620     c
080400120620     c                   if        not *in99  and %subst(knmus:1:3)='EDP'
080500120620     c                   seton                                        18
080600120620     c                   endif
080700940720     C*
080800940720     C                   ENDSR
080900020225**
081000051129F11=Dati VDL
081100020226F11=CHI SONO
081200020226F11=Tipo record EuroExpress
081300020226F11=Tipo record AWB FEDEX
081400051129F11=Dati VDL + Tipo record EuroExpress
081500051129F11=Dati VDL + Tipo record AWB FEDEX
081600020226F11=CHI SONO + Tipo record EuroExpress
081700020226F11=CHI SONO + Tipo record AWB FEDEX
081800120620**
081900120620CHKOBJ OBJ(FIART01L) OBJTYPE(*FILE)

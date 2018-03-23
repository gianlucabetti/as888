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
016600961122     C*
016700000000     C     FINE          TAG
016800950125     C                   MOVEL     PARAM         KPJBU
016900000000     C                   SETON                                        LR
017000940720     C**************************************************************************
017100940720     C* CARICO DETTAGLIO SEGNACOLLI
017200940720     C**************************************************************************
017300910806     C     CARBLT        BEGSR
017400940720     C*
017500910205     C                   EXSR      PULSF
017600940720     C                   Z-ADD     0             NRR1              4 0
017700910806     C                   SETOFF                                       30
017800940720     C     KART1         SETLL     FNART01L
017900940720     C     KART1         READE     FNART01L                               30
018000940720     C  N30              MOVEL     ARTNSC        VI1NCD
018100950116     C  N30              MOVEL     ARTFLS        VI1LNP
018200910220     C*
018300910806     C     *IN30         DOWEQ     '0'
018400920813     C*
018500941219     C                   Z-ADD     ARTFLS        VSCFLS                         FIL. SEGNACOLLO
018600940720     C                   MOVEL     ARTNSC        VSCNSC                         NUMERO SEGNACOLLO
018700940720     C                   MOVEL     ARTFLP        VSCFLP                         FILIALE TRANSITO
018800940720     C*
018900940720     C                   MOVEL     *BLANKS       VSCCAN
019000920813     C     ARTCAN        IFNE      ' '
019100940720     C                   MOVEL     ' -'          VSCCAN
019200940720     C                   MOVEL     ARTCAN        VSCCAN
019300940720     C                   MOVEL     '3E'          KCOD
019400940720     C                   MOVEL     *BLANKS       KKEY
019500940720     C                   MOVEL     ARTCAN        KKEY
019600940720     C     KTAB1         CHAIN     TABEL00F                           31
019700120711     c  n31              movel     tbluni        ds3e
019800120711     c   31              clear                   ds3e
019900120711     C                   MOVEL(P)  §3ede12       ALF13            13
020000980312     C                   MOVE      ALF13         VSCCAN
020100920813     C                   END
020200910912     C* GIRO DATE:
020300910912     C* PARTENZA MERCE
020400940720     C                   MOVEL     ARTDFV        G02INV
020500940720     C                   Z-ADD     *ZEROS        G02DAT
020600940720     C                   MOVEL     '3'           G02ERR
020700940720     C                   CALL      'XSRDA8'
020800940720     C                   PARM                    WLBDAT
020900940720     C                   MOVEL     G02DAT        VSCDFV
021000950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
021100950116     C                   MOVE      DUE           VSCDFV
021200921231     C     ARTDFV        IFGT      0
021300940720     C                   MOVEL     ARTNPP        VSCNPP
021400921231     C                   ELSE
021500940720     C                   MOVEL     '  '          VSCNPP
021600921231     C                   END
021700921230     C* ENTRATA AL TRANSITO
021800941219     C                   MOVEL     ARTDET        G02INV                         AAAA/MM/GG
021900940720     C                   Z-ADD     *ZEROS        G02DAT
022000940720     C                   MOVEL     '3'           G02ERR
022100940720     C                   CALL      'XSRDA8'
022200940720     C                   PARM                    WLBDAT
022300941219     C                   MOVEL     G02DAT        VSCDET                         GG/MM/AA (6)
022400941219     C                   MOVE      G02DAT        DUE               2 0          ANNO 2 CIFRE
022500941219     C                   MOVE      DUE           VSCDET
022600921231     C     ARTDET        IFGT      0
022700940720     C                   MOVEL     ARTPET        VSCPET
022800921231     C                   ELSE
022900940720     C                   MOVEL     '  '          VSCPET
023000921231     C                   END
023100910912     C* USCITA DAL TRANSITO
023200940720     C                   MOVEL     ARTDUT        G02INV
023300940720     C                   Z-ADD     *ZEROS        G02DAT
023400940720     C                   MOVEL     '3'           G02ERR
023500940720     C                   CALL      'XSRDA8'
023600940720     C                   PARM                    WLBDAT
023700940720     C                   MOVEL     G02DAT        VSCDUT
023800941219     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
023900941219     C                   MOVE      DUE           VSCDUT
024000921231     C     ARTDUT        IFGT      0
024100940720     C                   MOVEL     ARTPUT        VSCPUT
024200921231     C                   ELSE
024300940720     C                   MOVEL     '  '          VSCPUT
024400921231     C                   END
024500910912     C* ARRIVO MERCE ALLA FIL ARRIVO
024600940720     C                   MOVEL     ARTDAM        G02INV
024700940720     C                   Z-ADD     *ZEROS        G02DAT
024800940720     C                   MOVEL     '3'           G02ERR
024900940720     C                   CALL      'XSRDA8'
025000940720     C                   PARM                    WLBDAT
025100940720     C                   MOVEL     G02DAT        VSCDAM
025200950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
025300950116     C                   MOVE      DUE           VSCDAM
025400921231     C     ARTDAM        IFGT      0
025500940720     C                   MOVEL     ARTNAP        VSCNAP
025600921231     C                   ELSE
025700940720     C                   MOVEL     '  '          VSCNAP
025800921231     C                   END
025900910912     C* CONSEGNA MERCE REALE
026000940720     C                   MOVEL     ARTDCM        G02INV
026100940720     C                   Z-ADD     *ZEROS        G02DAT
026200940720     C                   MOVEL     '3'           G02ERR
026300940720     C                   CALL      'XSRDA8'
026400940720     C                   PARM                    WLBDAT
026500940720     C                   MOVEL     G02DAT        VSCDCM
026600950116     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
026700950116     C                   MOVE      DUE           VSCDCM
026800980312     C**ARTDAB = 'S' COLLO DISABILITATO
026900980312     C**         ARTDCM    IFEQ 0
027000980312     C**         ARTDAB    ANDEQ'S'                        'S'=DISABILITATO
027100980312     C**                   SETON                     01
027200980312     C**                   ELSE
027300980312     C**                   SETOF                     01
027400980312     C**                   ENDIF
027500910912     C*
027600961112     C* SOLO PER LE BOLLE EXPORT (15 ON) VISUALIZZO LA DATA INVIO COLLO
027700961112     C     *IN15         IFEQ      *ON
027800060322     C***  §OGntw        andne     'DPD'
027900961112     C                   MOVEL     ARTDTR        G02INV
028000961112     C                   Z-ADD     *ZEROS        G02DAT
028100961112     C                   MOVEL     '3'           G02ERR
028200961112     C                   CALL      'XSRDA8'
028300961112     C                   PARM                    WLBDAT
028400961112     C                   MOVEL     G02DAT        VSCDTR
028500961112     C                   MOVE      G02DAT        DUE                            ANNO 2 CIFRE
028600961112     C                   MOVE      DUE           VSCDTR
028700961112     C                   ENDIF
028800000211      *
028900000211      * Aggancio estensione dettaglio segnacolli
029000020226      * A seconda della linea di arrivo aggancio prima tipo righe E o F
029100020226     C                   MOVE      *BLANKS       WESTERO1          1
029200020226     C                   MOVE      *BLANKS       WESTERO2          1
029300020226     C     §OGNTW        IFEQ      'FED'
029400020226     C                   MOVE      'F'           WTRC
029500020226     C                   ELSE
029600020226     C                   MOVE      'E'           WTRC
029700020226     C                   ENDIF
029800051013     C     KARS1         CHAIN     FIARS01L                           31
029900020226   1aC     *IN31         IFEQ      *OFF
030000000911      *
030100020226      * VERIFICO PRESENZA SEGNACOLLO EUROEXPRESS/FEDEX
030200000911     C     ARSNOT        IFNE      *BLANKS
030300020226     C                   MOVE      WTRC          WESTERO1          1
030400020227     C                   MOVE      WTRC          WINFO1
030500000911     C                   MOVEL     ARSNOT        VSCVA2
030600000911     C                   MOVE      '7H'          KCOD
030700000911     C                   MOVEL(P)  WTRC          KKEY
030800000911     C     KTAB1         CHAIN     TABEL00F                           38
030900020226     C     *in38         ifeq      *off
031000020226     C                   MOVEL     TBLUNI        DS7H
031100020226     C                   MOVEL     §7HDEB        VSC7H2
031200020226     C                   else
031300020226     C                   clear                   VSC7H2
031400020226     C                   endif
031500000911     C                   ELSE
031600000911     C                   CLEAR                   VSCVA2
031700000911     C                   CLEAR                   VSC7H2
031800000911     C                   ENDIF
031900011031      *
032000011031      **** riportato da sotto.
032100011031      * VISUALIZZO ANCHE LA DATA DI ARRIVO DEL COLLO AL PARTNER
032200011031     C                   MOVEL     ARSDAT        G02INV            8 0
032300011031     C                   Z-ADD     *ZEROS        G02DAT
032400011031     C                   MOVEL     '3'           G02ERR
032500011031     C                   CALL      'XSRDA8'
032600011031     C                   PARM                    WLBDAT
032700011031     C                   MOVEL     G02DAT        VSCDTA
032800011031     C* VISUALIZZO ANCHE EVENTUALE ANOMALIA DA PARTE DEL PARTNER
032900011031     C                   MOVEL     ARSCAN        VSCCAE
033000011031      *
033100000911     C                   ELSE
033200000911     C                   CLEAR                   VSCVA2
033300000911     C                   CLEAR                   VSC7H2
033400000911     C                   ENDIF
033500020226      *
033600020226      * Aggancio ora estensione dettaglio segnacolli del tipo E o F
033700020226      * che non ho agganciato prima
033800020226     C     §OGNTW        IFEQ      'FED'
033900020226     C                   MOVE      'E'           WTRC
034000020226     C                   ELSE
034100020226     C                   MOVE      'F'           WTRC
034200020226     C                   ENDIF
034300051013     C     KARS1         CHAIN     FIARS01L                           31
034400020226    1C     *IN31         IFEQ      *OFF
034500020226      *
034600020226      * VERIFICO PRESENZA SEGNACOLLO EUROEXPRESS/FEDEX
034700020226    2C     ARSNOT        IFNE      *BLANKS
034800020226      *
034900020226    3C     WESTERO1      IFNE      *BLANKS
035000020227     C                   MOVEL     ARSNOT        VSHEEZ
035100020226     C                   MOVE      WTRC          WESTERO2          1
035200020226   3eC                   ELSE
035300020226     C                   MOVEL     ARSNOT        VSCVA2
035400020226     C                   MOVE      WTRC          WESTERO1          1
035500020227     C                   MOVE      WTRC          WINFO1
035600020226   3-C                   ENDIF
035700020226      *
035800020226     C                   MOVE      '7H'          KCOD
035900020226     C                   MOVEL(P)  WTRC          KKEY
036000020226     C     KTAB1         CHAIN     TABEL00F                           38
036100020226    3C     *IN38         IFEQ      *OFF
036200020226     C                   MOVEL     TBLUNI        DS7H
036300020226    4C     WESTERO2      IFEQ      *BLANKS
036400020226     C                   MOVEL     §7HDEB        VSC7H2
036500020226   4eC                   ELSE
036600020227     C                   MOVEL     §7HDEB        VSHEEK
036700020226   4-C                   ENDIF
036800020226   3eC                   ELSE
036900020226    4C     WESTERO1      IFNE      *BLANKS
037000020226     C                   CLEAR                   VSC7H2
037100020226   4-C                   ENDIF
037200020226    4C     WESTERO2      IFNE      *BLANKS
037300020227     C                   CLEAR                   VSHEEK
037400020226   4-C                   ENDIF
037500020226   3-C                   ENDIF
037600020226   2eC                   ELSE
037700020226    3C     WESTERO1      IFNE      *BLANKS
037800020226     C                   CLEAR                   VSCVA2
037900020226     C                   CLEAR                   VSC7H2
038000020226   3-C                   ENDIF
038100020226    3C     WESTERO2      IFNE      *BLANKS
038200020226     C                   CLEAR                   VSHEEK
038300020226     C                   CLEAR                   VSHEEZ
038400020226   3-C                   ENDIF
038500020226   2-C                   ENDIF
038600020226      *
038700020226      **** riportato da sotto.
038800020226      * VISUALIZZO ANCHE LA DATA DI ARRIVO DEL COLLO AL PARTNER
038900020226     C                   MOVEL     ARSDAT        G02INV            8 0
039000020226     C                   Z-ADD     *ZEROS        G02DAT
039100020226     C                   MOVEL     '3'           G02ERR
039200020226     C                   CALL      'XSRDA8'
039300020226     C                   PARM                    WLBDAT
039400020226     C                   MOVEL     G02DAT        VSCDTA
039500020226     C* VISUALIZZO ANCHE EVENTUALE ANOMALIA DA PARTE DEL PARTNER
039600020226     C                   MOVEL     ARSCAN        VSCCAE
039700020226      *
039800020226   1eC                   ELSE
039900020227     C     WESTERO1      IFNE      *BLANKS
040000020226     C                   CLEAR                   VSHEEK
040100020226     C                   CLEAR                   VSHEEZ
040200020227     C                   ELSE
040300020226     C                   CLEAR                   VSCVA2
040400020226     C                   CLEAR                   VSC7H2
040500020227     C                   ENDIF
040600020226   1-C                   ENDIF
040700000911      *
040800000911     C                   MOVEL     'C'           WTRC
040900051013     C     KARS1         CHAIN     FIARS01L                           31
041000980312     C     *IN31         IFEQ      *OFF
041100000211      *
041200000911      * VERIFICO PRESENZA CODICE "CHI SONO"
041300980316     C     ARSNOT        IFNE      *BLANKS
041400000211     C                   eval      *in16 = *on
041500980312     C                   MOVEL     ARSNOT        VSCVAR
041600000911     C                   MOVE      '7H'          KCOD
041700000911     C                   MOVEL(P)  WTRC          KKEY
041800000911     C     KTAB1         CHAIN     TABEL00F                           38
041900000911     C  N38              MOVEL     TBLUNI        DS7H
042000000911     C  N38              MOVEL     §7HDEB        VSC7H1
042100000911     C   38              CLEAR                   VSC7H1
042200000911     C                   MOVEL     ARSNOT        VSHEEX                         "CHI SONO"
042300000911     C                   MOVEL     VSC7H1        VSHEEY                         DESCRIZIONE
042400000211      *
042500000911     C                   ELSE
042600000911     C                   CLEAR                   VSC7H1
042700000211     C                   ENDIF
042800000211      *
042900011031      ****** spostato tutto questo pezzo sotto l'aggancio al record
043000011031      ****** precedente per il tipo "E" (e non "C")
043100000211      * VISUALIZZO ANCHE LA DATA DI ARRIVO DEL COLLO AL PARTNER
043200011031     C******             MOVEL     ARSDAT        G02INV            8 0
043300011031     C******             Z-ADD     *ZEROS        G02DAT
043400011031     C******             MOVEL     '3'           G02ERR
043500011031     C******             CALL      'XSRDA8'
043600011031     C******             PARM                    WLBDAT
043700011031     C******             MOVEL     G02DAT        VSCDTA
043800980331     C* VISUALIZZO ANCHE EVENTUALE ANOMALIA DA PARTE DEL PARTNER
043900011031     C******             MOVEL     ARSCAN        VSCCAE
044000000211      *
044100980312     C                   ELSE
044200000911     C                   CLEAR                   VSC7H1
044300000211      *
044400051013      * SE NON C'È FIARS VISUALIZZO IL VOLUME CML
044500980312     C                   CLEAR                   VAR
044600980312     C                   MOVEA     'Vol'         VAR(22)
044700980312     C     ARTVUC        IFGT      0
044800980316     C                   Z-ADD     1             I                 1 0
044900980312     C                   Z-ADD     6             D                 1 0
045000980312     C                   MOVE      ARTVUC        WNUM              7
045100980312     C                   EXSR      EDITA
045200980312     C                   MOVEA     WNUME         VAR(26)
045300980312     C                   MOVEA     ARTFVC        VAR(35)
045400980312     C                   END
045500990913     C                   MOVEA     'Peso'        VAR(2)
045600990909     C     ARTPUC        IFGT      0
045700990909     C                   Z-ADD     4             I                 1 0
045800990909     C                   Z-ADD     3             D                 1 0
045900990909     C                   MOVE      ARTPUC        WNUM              7
046000990909     C                   EXSR      EDITA
046100990913     C                   MOVEA     WNUME         VAR(7)
046200990913     C                   MOVEA     ARTFPC        VAR(17)
046300990909     C                   END
046400980312     C                   END
046500000920      *
046600000920     C* memorizzo in ogni caso il volume cml E IL PESO
046700000920     C                   clear                   VSHFVC
046800000920     C                   clear                   VSHFPC
046900000920      *
047000000920     C     ARTVUC        IFGT      0
047100000920     C                   MOVEL     ARTFVC        VSHFVC
047200000920     C                   END
047300000920     C                   MOVEL     ARTVUC        VSHVUC
047400000920      *
047500000920     C     ARTPUC        IFGT      0
047600000920     C                   MOVEL     ARTFPC        VSHFPC
047700000920     C                   END
047800000920     C                   MOVEL     ARTPUC        VSHPUC
047900000211      *
048000000211      * SEGNALO SE SEGNACOLLO IN IDD
048100970805     C                   MOVEL     *BLANKS       V1CIDD
048200970805     C                   Z-ADD     1             X
048300970805     C     ARTFL1        LOOKUP    C7O(X)                                 34
048400970805     C     *IN34         IFEQ      '1'
048500970805     C     F7O(X)        ANDNE     *BLANKS
048600970805     C                   MOVEL     'IDD'         V1CIDD
048700970805     C                   END
048800961112     C*
048900910205     C* SCRIVO SUBFILE
049000940720     C                   ADD       1             NRR1
049100940720     C                   WRITE     LR37S01
049200910912     C*
049300940720     C     KART1         READE     FNART01L                               30
049400910205     C                   END
049500920803     C*
049600920803     C* SE PIU' DI UN COLLO MEMORIZZO ANCHE AL
049700940720     C     ARTNSC        IFNE      VI1NCD
049800940720     C                   MOVEL     ARTNSC        VI1NCA
049900920803     C                   END
050000910205     C*
050100940720     C                   Z-ADD     NRR1          SAVNRR            4 0
050200921104     C*
050300940720     C     NRR1          IFEQ      0
050400940720     C                   Z-ADD     0             VSCNSC
050500940720     C                   Z-ADD     0             VSCDFV
050600940720     C                   MOVEL     '  '          VSCNPP
050700940720     C                   Z-ADD     0             VSCDET
050800940720     C                   MOVEL     '  '          VSCPET
050900940720     C                   MOVEL     000           VSCFLP
051000940720     C                   Z-ADD     0             VSCDUT
051100940720     C                   MOVEL     '  '          VSCPUT
051200940720     C                   Z-ADD     0             VSCDAM
051300940720     C                   MOVEL     '  '          VSCNAP
051400940720     C                   Z-ADD     0             VSCDCM
051500940720     C                   MOVEL     *BLANKS       VSCCAN
051600961112     C                   CLEAR                   VSCDTR
051700980312     C                   CLEAR                   VSCDTA
051800980312     C                   CLEAR                   VSCVAR
051900000911     C                   CLEAR                   VSCVA2
052000980312     C                   CLEAR                   VSHEEX
052100000911     C                   CLEAR                   VSHEEY
052200980312     C                   CLEAR                   VSHVUC
052300980312     C                   CLEAR                   VSHFVC
052400990913     C                   CLEAR                   VSHFPC
052500990909     C                   CLEAR                   VSHPUC
052600000911     C                   CLEAR                   VSC7H1
052700000911     C                   CLEAR                   VSC7H2
052800940720     C                   ADD       1             NRR1
052900940720     C                   WRITE     LR37S01
053000921104     C                   END
053100910205     C*
053200910205     C                   ENDSR
053300020226     C**************************************************************************
053400020226     C* GESTIONE ABILITAZIONE TASTO FUNZIONE F11
053500020226     C**************************************************************************
053600020226     C     ABIF11        BEGSR
053700020226     C                   seton                                        17
053800020226      * Vari casi per valorizzazre tasto funzione
053900020226     C                   SELECT
054000020226     C
054100020226     C     *in16         wheneq    *on
054200020226     C     WEstero2      andeq     *blanks
054300020226     C                   movel     FunF11(1)     VZCF11
054400020226      *
054500020226     C     *in16         wheneq    *off
054600020226     C     WEstero1      andne     *blanks
054700020226     C     WEstero2      andne     *blanks
054800020226      * Scelgo la descrizione del tasto F11 a seconda che abia caricato
054900020226      * prima tipo record 'F' o tipo record 'E'
055000020227     C     WInfo1        ifeq      'F'
055100020226     C                   movel     FunF11(3)     VZCF11
055200020227     C                   move      'E'           WInfo1            1
055300020226     C                   else
055400020226     C                   movel     FunF11(4)     VZCF11
055500020227     C                   move      'F'           WInfo1            1
055600020226     C                   endif
055700020226      *
055800020226     C     *in16         wheneq    *on
055900020226     C     WEstero1      andne     *blanks
056000020226     C     WEstero2      andne     *blanks
056100020227     C     WInfo1        ifeq      'F'
056200020226     C                   movel     FunF11(5)     VZCF11
056300020227     C                   move      'E'           WInfo1            1
056400020226     C                   else
056500020226     C                   movel     FunF11(6)     VZCF11
056600020227     C                   move      'F'           WInfo1            1
056700020226     C                   endif
056800020226     C                   ENDSL
056900020226      *
057000020227      * La prima volta imposto la richiesta CML su F11 perchè è
057100020227      * caricato a sfl il CHI SONO.
057200020226     C     *IN16         IFEQ      *ON
057300020227     C                   SELECT
057400020227     C     WInfo         wheneq    *blanks
057500020226     C                   MOVEL     'S'           WINFO             1
057600020227     C     WInfo         wheneq    'S'
057700020227     C                   MOVEL     'V'           WINFO             1
057800020227     C     WInfo         wheneq    'V'
057900020227     C                   MOVEL     'S'           WINFO             1
058000020227     C                   ENDSL
058100020227     C                   ENDIF
058200020226      *
058300020226     C                   ENDSR
058400020226     C**************************************************************************
058500020226     C* GESTIONE ABILITAZIONE TASTO FUNZIONE F11 DOPO AGGIORNAMENTO SFL
058600020226     C**************************************************************************
058700020226     C     ABIF11POST    BEGSR
058800020227      *
058900020226      * Modifico la descrizione tasto funzione
059000020226     C                   SELECT
059100020226     C
059200020226     C     WInfo         wheneq    'V'
059300020226     C     WEstero2      andeq     *blanks
059400020227     C                   movel     FunF11(1)     VZCF11
059500020226      *
059600020226     C     WInfo         wheneq    'S'
059700020226     C     WEstero2      andeq     *blanks
059800020227     C                   movel     FunF11(2)     VZCF11
059900020226      *
060000020226     C     WInfo         wheneq    *blanks
060100020226     C     WEstero1      andne     *blanks
060200020226     C     WEstero2      andne     *blanks
060300020226     C     WInfo1        ifeq      'F'
060400020226     C                   movel     FunF11(3)     VZCF11
060500020226     C                   else
060600020226     C                   movel     FunF11(4)     VZCF11
060700020226     C                   endif
060800020226      *
060900020226     C     WInfo         whenne    *blanks
061000020226     C     WEstero1      andne     *blanks
061100020226     C     WEstero2      andne     *blanks
061200020226      *
061300020226     C     WInfo         ifeq      'V'
061400020227     C     WInfo1        ifeq      'F'
061500020227     C                   movel     FunF11(5)     VZCF11
061600020227     C                   else
061700020227     C                   movel     FunF11(6)     VZCF11
061800020227     C                   endif
061900020227      *
062000020226     C                   else
062100020227     C     WInfo1        ifeq      'F'
062200020227     C                   movel     FunF11(7)     VZCF11
062300020227     C                   else
062400020227     C                   movel     FunF11(8)     VZCF11
062500020227     C                   endif
062600020226     C                   endif
062700020226     C                   ENDSL
062800020227      *
062900020227      * Imposto per il cambio di visualizzazione successivo
063000020227      * CHI SONO/CML
063100020227     C                   SELECT
063200020227     C     WINFO         WHENEQ    'S'
063300020227     C                   MOVE      'V'           WINFO
063400020227     C     WINFO         WHENEQ    'V'
063500020227     C                   MOVE      'S'           WINFO
063600020227     C                   ENDSL
063700020227      *
063800020227      * Imposto per il cambio di visualizzazione successivo
063900020227      * EuroExpress/FedEx
064000020227     C                   SELECT
064100020227     C     WINFO1        WHENEQ    'F'
064200020227     C                   MOVE      'E'           WINFO1
064300020227     C     WINFO1        WHENEQ    'E'
064400020227     C                   MOVE      'F'           WINFO1
064500020227     C                   ENDSL
064600020226      *
064700020226     C                   ENDSR
064800980312     C**************************************************************************
064900980312     C* EDITAZIONE CAMPO NUMERICO
065000980312     C**************************************************************************
065100980312     C     EDITA         BEGSR
065200980312     C* MUOVO PRIMA I DECIMALI
065300980312     C     D             IFGT      0
065400980312     C     I             ADD       1             II                1 0
065500980316     C     I             ADD       2             DD                1 0
065600980316     C                   MOVEA     NUM(II)       NUE(DD)
065700980316     C                   MOVE      ','           NUE(II)
065800980312     C                   END
065900980312     C* POI GLI INTERI
066000980312     C* PRIMA ABBLENCO GLI ZERI NON SIGNIFICATIVI
066100980312     C                   MOVE      *BLANKS       WFINE             1
066200980312     C                   Z-ADD     1             II
066300980316     C     I             SUB       1             IM                1 0
066400980312     C     WFINE         DOWEQ     *BLANKS
066500980312     C     II            ANDLE     IM
066600980312     C     NUM(II)       IFEQ      '0'
066700980312     C                   MOVE      *BLANKS       NUM(II)
066800980312     C                   ELSE
066900980312     C                   MOVE      '1'           WFINE
067000980312     C                   END
067100980312     C                   ADD       1             II
067200980312     C                   ENDDO
067300980312     C* POI SCRIVO LA PARTE INTERA NEL CAMPO DI OUTPUT
067400980316     C                   SUB       2             DD
067500980312     C                   Z-ADD     I             II
067600980312     C                   DO        I
067700980312     C                   MOVE      NUM(II)       NUE(DD)
067800980312     C                   SUB       1             DD
067900980312     C                   SUB       1             II
068000980312     C                   ENDDO
068100980312     C*
068200980312     C                   ENDSR
068300980312     C**************************************************************************
068400980312     C* AGGIORNAMENTO SUBFILE PER INTERROGAZIONE ALTRI DATI
068500980312     C**************************************************************************
068600980312     C     UPDS01        BEGSR
068700980312     C                   Z-ADD     1             NRR1
068800980312     C     NRR1          CHAIN     LR37S01                            31
068900980312     C     *IN31         DOWEQ     *OFF
069000980312     C* VISUALIZZO VOLUME
069100980312     C     WINFO         IFEQ      'S'
069200980312     C                   CLEAR                   VAR
069300000911     C                   CLEAR                   VSC7H1
069400980312     C                   MOVEA     'Vol'         VAR(22)
069500980312     C     VSHVUC        IFGT      0
069600980316     C                   Z-ADD     1             I                 1 0
069700980312     C                   Z-ADD     6             D                 1 0
069800980312     C                   MOVE      VSHVUC        WNUM              7
069900980312     C                   EXSR      EDITA
070000980312     C                   MOVEA     WNUME         VAR(26)
070100980312     C                   MOVEA     VSHFVC        VAR(35)
070200980312     C                   END
070300990913     C                   MOVEA     'Peso'        VAR(2)
070400990909     C     VSHPUC        IFGT      0
070500990909     C                   Z-ADD     4             I                 1 0
070600990909     C                   Z-ADD     3             D                 1 0
070700990909     C                   MOVE      VSHPUC        WNUM              7
070800990909     C                   EXSR      EDITA
070900990913     C                   MOVEA     WNUME         VAR(7)
071000990913     C                   MOVEA     VSHFPC        VAR(17)
071100990909     C                   END
071200980312     C                   ELSE
071300000911     C* VISUALIZZO "CHI SONO"
071400980312     C                   MOVEL     VSHEEX        VSCVAR
071500000911     C                   MOVEL     VSHEEY        VSC7H1
071600980312     C                   END
071700020226     C* Visualizzo in alterannza Fedex/EuroExpress
071800020226     C     WEstero1      ifne      *blanks
071900020226     C     WEstero2      andne     *blanks
072000020226     C                   movel(P)  vsc7h2        wsc7h2           35
072100020226     C                   movel(P)  vscva2        wscva2           11
072200020226     C                   movel(P)  vsheek        vsc7h2
072300020226     C                   movel(P)  vsheez        vscva2
072400020226     C                   movel(P)  wsc7h2        vsheek
072500020227     C                   movel(P)  wscva2        vsheez
072600020226     C                   endif
072700020226      *
072800980312     C                   UPDATE    LR37S01
072900980312     C                   ADD       1             NRR1
073000980312     C     NRR1          CHAIN     LR37S01                            31
073100980312     C                   ENDDO
073200980312     C                   ENDSR
073300940720     C**************************************************************************
073400940720     C* PULIZIA SUBFILE
073500940720     C**************************************************************************
073600910205     C     PULSF         BEGSR
073700940720     C*
073800910205     C                   SETON                                        35
073900940720     C                   WRITE     LR37C01                                      SFLCTL
074000910205     C                   SETOFF                                       35
074100940720     C*
074200910205     C                   ENDSR
074300940720     C**************************************************************************
074400940720     C*  ROUTINE  INIZIALE
074500940720     C**************************************************************************
074600940720     C     *INZSR        BEGSR
074700940720     C*
074800940720     C     KART1         KLIST                                                  FNART01L
074900940720     C                   KFLD                    PARAAS                         ..ANNO
075000940720     C                   KFLD                    PARLNP                         ..LINEA PARTENZA
075100940720     C                   KFLD                    PARNRS                         ..NUMERO SERIE
075200940720     C                   KFLD                    PARNSP                         ..NUMERO SPEDIZIONE
075300940720     C     KTAB1         KLIST                                                  TABEL00F
075400940720     C                   KFLD                    KKUT                           ..CODICE UTENTE
075500940720     C                   KFLD                    KCOD                           ..CODICE TABELLA
075600940720     C                   KFLD                    KKEY                           ..CODICE ELEMENTO
075700970805     C     KTAB2         KLIST                                                  TABEL00F
075800970805     C                   KFLD                    KKUT                           ..CODICE UTENTE
075900970805     C                   KFLD                    KCOD                           ..CODICE TABELLA
076000980312     C     KARS1         KLIST                                                  FNART01L
076100980312     C                   KFLD                    ARTFLS
076200980312     C                   KFLD                    ARTLNA                                     ENZA
076300980312     C                   KFLD                    ARTNRS                                     IE
076400980312     C                   KFLD                    ARTNSC                                     DIZIONE
076500980312     C                   KFLD                    WTRC                                       DIZIONE
076600940720     C*
076700940720     C     *LIKE         DEFINE    TBLKUT        KKUT                           CODICE UTENTE
076800940720     C     *LIKE         DEFINE    TBLCOD        KCOD                           CODICE TABELLA
076900940720     C     *LIKE         DEFINE    TBLKEY        KKEY                           CODICE ELEMENTO
077000980312     C     *LIKE         DEFINE    ARSTRC        WTRC                                       NTO
077100940720     C*
077200940720     C                   Z-ADD     1             KKUT
077300970805     C*
077400970805     C* Eseguo caricamento della tabella dei codici consegna anomala
077500970805     C                   Z-ADD     0             X                 3 0
077600970805     C                   MOVEL     '7O'          KCOD
077700970805     C     KTAB2         CHAIN     TABEL                              34
077800970805     C     *IN34         DOWEQ     '0'
077900970805     C     X             ANDLT     100
078000970805     C     TBLFLG        IFEQ      *BLANKS
078100970805     C                   ADD       1             X
078200970805     C                   MOVEL     TBLKEY        C7O(X)
078300970805     C                   MOVEL     TBLUNI        DS7O
078400970805     C                   MOVEL     §7OIDD        F7O(X)
078500970805     C                   END
078600970805     C     KTAB2         READE     TABEL                                  34
078700970805     C                   END
078800120620     c* controllo se esiste nuovo file FIART
078900120620     c                   eval      comman=chkart(1)
079000120620     c                   eval      lengh=35
079100120620     c                   call      'QCMDEXC'                            99
079200120620     c                   parm                    comman           80
079300120620     c                   parm                    lengh            15 5
079400120620     c
079500120620     c                   if        not *in99  and %subst(knmus:1:3)='EDP'
079600120620     c                   seton                                        18
079700120620     c                   endif
079800940720     C*
079900940720     C                   ENDSR
080000020225**
080100051129F11=Dati VDL
080200020226F11=CHI SONO
080300020226F11=Tipo record EuroExpress
080400020226F11=Tipo record AWB FEDEX
080500051129F11=Dati VDL + Tipo record EuroExpress
080600051129F11=Dati VDL + Tipo record AWB FEDEX
080700020226F11=CHI SONO + Tipo record EuroExpress
080800020226F11=CHI SONO + Tipo record AWB FEDEX
080900120620**
081000120620CHKOBJ OBJ(FIART01L) OBJTYPE(*FILE)

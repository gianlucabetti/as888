000100980325     H DECEDIT('0,') DATEDIT(*DMY.)
000200980410      *--------------------------------------------------------------*
000300980410      *         - GESTIONE DISTINTA ARRIVI ESTERO -                  *
000400980410      *          - GESTIONE DETTAGLIO SEGNACOLLI -                   *
000500980325      *--------------------------------------------------------------*
000600980410     FFNLRK1D   CF   E             WORKSTN
000700980410     F                                     SFILE(LRK1S06:NRR6)
000800980410     F                                     SFILE(LRK1S07:NRR7)
000900980417     F                                     SFILE(LRK1S09:NRR9)
001000980410     FFNCDA03L  UF   E           K DISK
001100980331     F                                     COMMIT
001200980416     FEDRDA02L  IF   E           K DISK
001300980420     FFNCDAW1L  UF A E           K DISK
001400980417     FFNART01L  IF   E           K DISK
001500051017     FFIARS01L  IF   E           K DISK
001600980420     FEDTAB01L  IF   E           K DISK
001700980325      *--------------------------------------------------------------*
001800980330      *  SCHIERE
001900980325      *--------------------------------------------------------------*
002000980424     D ERR             S             70    DIM(2) CTDATA PERRCD(1)              ERRORI
002100980420      * Tabella codici eventi
002200980420     D CEV             S              6    DIM(200)
002300980420     D DEV             S             90    DIM(200)
002400980330      *--------------------------------------------------------------*
002500980330      *  DS
002600980330      *--------------------------------------------------------------*
002700980325     D KPJBA         E DS
002800020913     D Ddatiute      E DS
002900980410     D FNLRK1        E DS                  EXTNAME(FNLRK1DS)
003000980420     D DSCE          E DS                  EXTNAME(EDIDSCE)
003100980410      *  DS per inversione data
003200980331     D WLBDA8          DS
003300980331     D  G02DAT                 1      8  0
003400980331     D  G02INV                 9     16  0
003500980331     D  G02ERR                17     17
003600980331     D  G02TGI                18     22  0
003700980330      *
003800980330     D                 DS
003900980416     D  DATA                   1      6
004000980416     D  STS                    7      9
004100980416     D  COD                   10     12
004200980416     D  SCHI                   1     12
004300980325      *--------------------------------------------------------------*
004400980325      *  C I C L O        P R I N C I P A L E
004500980325      *--------------------------------------------------------------*
004600980325     C     *ENTRY        PLIST
004700980325     C                   PARM                    KPJBA
004800980325      *
004900980410     C                   MOVEL     KPJBU         FNLRK1
005000980330      *
005100980417      *  Carico subfile hidden + file di lavoro con dettaglio segnacolli
005200980330     C                   EXSR      INZS07
005300980417     C     NRR7          IFEQ      *ZEROS
005400980417     C                   GOTO      FINE
005500980417     C                   END
005600980417      *
005700980417      *  Imposto dati testata subfiles
005800980417     C                   MOVEL     DK1AAS        V6CAAS
005900980417     C                   Z-ADD     DK1LNP        V6CLNP
006000980417     C                   Z-ADD     DK1NRS        V6CNRS
006100980417     C                   Z-ADD     DK1NSP        V6CNSP
006200980417     C                   Z-ADD     DK1NCL        V6CNCS
006300980417      *
006400980417      *  Carico file di lavoro con dettaglio segnacolli
006500980417     C                   EXSR      CARCDW
006600980331      *
006700980331      *  Caricamento subfile emissione da subfile hidden
006800980330     C                   EXSR      INZS06
006900980331      *
007000980331      *  Loop gestione errori
007100980330     C     WFINE         DOUEQ     'S'
007200980331     C     WTPVID        CASEQ     '6'           GESS06
007300980330     C                   END
007400980330     C                   END
007500980330      *
007600980330     C     FINE          TAG
007700980410     C                   MOVEL     FNLRK1        KPJBU
007800980330     C                   COMMIT
007900980330     C                   SETON                                        LR
008000980325      *--------------------------------------------------------------*
008100980330      *  Carico subfile hidden
008200980325      *--------------------------------------------------------------*
008300980330     C     INZS07        BEGSR
008400980330      *
008500980330      * Pulisco subfile
008600980330     C                   SETON                                        23
008700980410     C                   WRITE     LRK1C07
008800980330     C                   SETOFF                                       23
008900980330      *
009000980416     C                   Z-ADD     *ZEROS        NRR7
009100980416     C                   MOVEL     *BLANKS       WSVSCE
009200980416     C                   Z-ADD     *ZEROS        WSVDAM
009300980416     C                   Z-ADD     *ZEROS        WSVHRA
009400980416     C                   MOVEL     *BLANKS       WSVSTS
009500980416     C                   MOVEL     *BLANKS       WSVCOD
009600980416      *
009700980410     C                   Z-ADD     DK1LNA        KLNA
009800980410     C                   Z-ADD     DK1AAS        KAAS
009900980410     C                   Z-ADD     DK1LNP        KLNP
010000980410     C                   Z-ADD     DK1NRS        KNRS
010100980410     C                   Z-ADD     DK1NSP        KNSP
010200980410     C     KCDA          CHAIN     FNCDA000                           30
010300980330      *
010400980330     C     *IN30         DOWEQ     '0'
010500980410     C     DK1NDT        IFEQ      CDANDC
010600980330      *
010700980428      *  Se evento già elaborato e segnac. <> blanks *IN04 =*ON e aggiorno FNCDA
010800980416     C     WSVDAM        IFEQ      CDADAM
010900980416     C     WSVHRA        ANDEQ     CDAHRA
011000980416     C     WSVSTS        ANDEQ     CDASTS
011100980416     C     WSVCOD        ANDEQ     CDACOD
011200980428     C     CDANOT        ANDNE     *BLANKS
011300980416     C                   MOVEL     '1'           V7CIN4
011400980422     C                   MOVEL     '1'           CDACCA
011500980422     C                   EXCEPT    AGGCDA
011600980416     C                   ELSE
011700980416     C                   MOVEL     '0'           V7CIN4
011800980416     C                   Z-ADD     CDADAM        WSVDAM            8 0
011900980416     C                   Z-ADD     CDAHRA        WSVHRA            4 0
012000980416     C                   MOVEL     CDASTS        WSVSTS            3
012100980416     C                   MOVEL     CDACOD        WSVCOD            3
012200980416     C                   END
012300980416      *
012400980416     C                   MOVEL     CDASTS        V7CSTS
012500980410     C                   MOVEL     CDACOD        V7CCOD
012600980410     C                   MOVEL     CDANOT        V7CSEE
012700980410     C                   Z-ADD     CDAHRA        V7CHRA
012800980416     C                   Z-ADD     CDANCL        V7CNCL
012900980331      *
013000980416     C                   EXSR      SUBNOT
013100980416      *
013200980331     C                   CLEAR                   WLBDA8
013300980410     C                   Z-ADD     CDADAM        G02INV
013400980331     C                   MOVEL     '3'           G02ERR
013500980331     C                   CALL      'XSRDA8'
013600980331     C                   PARM                    WLBDA8
013700980416     C                   MOVEL     G02DAT        DATA4             4 0
013800980416     C                   MOVE      G02DAT        DATA2             2 0
013900980416     C                   Z-ADD     DATA2         V7CDAM
014000980416     C                   MOVEL     DATA4         V7CDAM
014100980410     C                   Z-ADD     CDADAM        V7XDAM
014200980331      *
014300980330     C                   ADD       1             NRR7
014400980330      *
014500980410     C                   WRITE     LRK1S07
014600980410     C                   END
014700980410      *
014800980410     C     KCDA          READE     FNCDA000                               30
014900980330     C                   END
015000980330      *
015100980330     C                   ENDSR
015200980330      *---------------------------------------------------------------*
015300980417      * Carico file di work                                           *
015400980330      *---------------------------------------------------------------*
015500980417     C     CARCDW        BEGSR
015600980330      *
015700980417     C                   Z-ADD     DK1AAS        KAAS
015800980417     C                   Z-ADD     DK1LNP        KLNP
015900980417     C                   Z-ADD     DK1NRS        KNRS
016000980417     C                   Z-ADD     DK1NSP        KNSP
016100980417      *
016200980417     C     KCDW          CHAIN     FNCDW000                           30
016300980417      *
016400980417     C     *IN30         IFEQ      '1'
016500980417     C                   Z-ADD     DK1AAS        CDWAAS
016600980417     C                   Z-ADD     DK1LNP        CDWLNP
016700980417     C                   Z-ADD     DK1NRS        CDWNRS
016800980417     C                   Z-ADD     DK1NSP        CDWNSP
016900980417      *
017000980417     C     KCDW          CHAIN     FNART000                           30
017100980417     C     *IN30         DOWEQ     '0'
017200980417     C                   Z-ADD     ARTNSC        CDWNSC
017300980417     C                   Z-ADD     ARTFLS        CDWFLS
017400980417      *
017500980417     C                   MOVEL     'E'           KTRC
017600980417     C                   Z-ADD     ARTNSC        KNSC
017700980417     C                   Z-ADD     ARTFLS        KFLS
017800980417     C                   Z-ADD     ARTLNA        KLNA
017900051017     C     KARS          CHAIN     FIARS000                           31
018000980422     C  N31              MOVEL     ARSNOT        CDWSEE
018100980422     C  N31              MOVEL     ARSCAN        CDWDAN
018200980417      *
018300980417     C                   WRITE     FNCDW000
018400980417      *
018500980417     C     KCDW          READE     FNART000                               30
018600980417     C                   END
018700980417      *
018800980417     C                   END
018900980417      *
019000980417     C                   ENDSR
019100980417      *---------------------------------------------------------------*
019200980417      * Gestione videata subfile errori                               *
019300980417      *---------------------------------------------------------------*
019400980417     C     GESS06        BEGSR
019500980417      *
019600980330     C  N28              Z-ADD     1             NR6KEY
019700980330      *
019800980331      *  Se errore emetto finestra con messaggio
019900980421     C   28              WRITE     LRK1Z06
020000980410     C   28              EXFMT     LRK1W08
020100980330      *
020200980410     C                   WRITE     LRK1Z06
020300980410     C                   EXFMT     LRK1C06
020400980330      *
020500980330     C                   MOVEL     *BLANKS       WINMSG
020600980330     C                   Z-ADD     1             NR6KEY
020700980330     C                   SETOFF                                       28
020800091029      * legenda status
020900091029     C     *INKD         IFEQ      '1'
021000091029     c                   call      'FNLRK1RH'
021100091029     C                   END
021200980331      *
021300980330      *  Fine Lavoro
021400980331     C     *INKC         IFEQ      '1'
021500980330     C                   MOVEL     'S'           WFINE
021600980421     C                   EXSR      DELCDW
021700980330     C                   GOTO      FINVD3
021800980330     C                   END
021900980331      *  Controlli
022000980330     C                   EXSR      CTR06
022100980416      *  Se non confermo o ci sono errori ricarico il subfile
022200980416     C     *INKF         IFEQ      '0'
022300980416     C     *IN28         OREQ      '1'
022400980416     C                   EXSR      INZS06
022500980416     C                   END
022600980331      *  Se non ho errori elaboro dati per conferma
022700980330     C  N28
022800980330     CAN KF              EXSR      CONF06
022900980330      *
023000980330     C     FINVD3        ENDSR
023100980330      *---------------------------------------------------------------*
023200980331      * Gestione videata subfile emissione                            *
023300980330      *---------------------------------------------------------------*
023400980330     C     INZS06        BEGSR
023500980330      *
023600980330     C                   SETON                                        63
023700980410     C                   WRITE     LRK1C06
023800980330     C                   SETOFF                                       63
023900980330      *
024000980330     C                   Z-ADD     1             NRR7              4 0
024100980330     C                   Z-ADD     0             NRR6              4 0
024200980330     C                   MOVEL     'N'           WFINE
024300980330      *
024400980410     C     NRR7          CHAIN     LRK1S07                            31
024500980330      *
024600980330     C     *IN31         DOWEQ     '0'
024700980330      *
024800980331     C                   SETOFF                                       22
024900980330      *
025000980331      *  Scrivo subfile emissione da subfile hidden
025100980416     C                   MOVEL     V7CIN4        V6CIN4
025200980416     C                   MOVEL     V7CIN4        *IN04
025300980331     C                   MOVEL     V7CSTS        V6CSTS
025400980331     C                   MOVEL     V7CCOD        V6CCOD
025500980331     C                   MOVEL     V7CSEE        V6CSEE
025600980410     C                   Z-ADD     V7CDAM        V6CDAM
025700980421     C                   Z-ADD     V7XDAM        V6XDAM
025800980410     C                   Z-ADD     V7CHRA        V6CHRA
025900980416     C                   Z-ADD     V7CNCL        V6CNCL
026000980416     C                   MOVEL     V7CNOE        V6CNOE
026100980330     C                   MOVEL     V7CSCE        V6CSCE
026200980421     C                   Z-ADD     V7CDSE        V6CDSE
026300980421     C                   MOVEL     V7CDSE        *IN05
026400980330     C                   Z-ADD     NRR7          V6CNR7
026500980330     C                   ADD       1             NRR6
026600980410     C                   WRITE     LRK1S06
026700980330      *
026800980330     C                   ADD       1             NRR7
026900980410     C     NRR7          CHAIN     LRK1S07                            31
027000980330     C                   END
027100980330      *
027200980330     C                   ENDSR
027300980330      *---------------------------------------------------------------*
027400980331      *  Controllo errori
027500980330      *---------------------------------------------------------------*
027600980330     C     CTR06         BEGSR
027700980330      *
027800980421     C                   SETOFF                                       28
027900980422     C                   MOVEL     *BLANKS       WSVSCE            1
028000980424      *
028100980331     C                   Z-ADD     1             NRR6
028200980421     C     NRR6          CHAIN     LRK1S06                            32
028300980330      *
028400980330     C     *IN32         DOWEQ     '0'
028500980330      *
028600980416      *  Se evento aggiuntivo prendo ex scelta
028700980422     C     V6CIN4        IFEQ      '1'
028800980416     C                   MOVEL     WSVSCE        V6CSCE
028900980416     C                   ELSE
029000980422     C                   MOVEL     V6CSCE        WSVSCE
029100980416     C                   END
029200980416      *
029300980331     C                   SETOFF                                       31
029400980421      *
029500980421      *  - Forzo record -
029600980424      *  Se spedizione = 0 non consento la forzatura.
029700980424      *  Se colli evento <>  colli sped. elaboro i segnacolli, solo 1 volta.
029800980421      *  Se segn. EUROEXPRESS = blanks la selezione dei segnacolli da aggiornare
029900980421      *  è interattiva. Se segn. EUROEXPRESS <> blanks provvedo io.
030000980421      *
030100980421     C  N28V6CSCE        IFEQ      'F'
030200980424      *
030300980424     C     V6CNSP        IFEQ      *ZEROS
030400980424     C                   Z-ADD     NRR6          NR6KEY
030500980424     C                   MOVEL     ERR(2)        WINMSG
030600980424     C                   ELSE
030700980424      *
030800980424     C     V6CNCL        IFNE      V6CNCS
030900980420     C     V6CDSE        ANDEQ     *ZEROS
031000980421      *
031100980421     C     V6CSEE        IFEQ      *BLANKS
031200980417     C                   EXSR      SUBDSE
031300980421     C     NRR6          CHAIN     LRK1S06                            32
031400980421     C                   MOVEL     V6§SCE        V6CSCE
031500980421     C                   Z-ADD     V6§DSE        V6CDSE
031600980421     C                   ELSE
031700980421     C                   EXSR      CRTDSE
031800980421     C                   Z-ADD     1             V6CDSE
031900980417     C                   END
032000980421      *
032100980330     C                   END
032200980424     C                   END
032300980421     C                   END
032400980416      *
032500980416      *  Nessuna scelta
032600980416      *
032700980416     C     V6CSCE        IFEQ      *BLANKS
032800980416     C                   SETON                                        28
032900980416     C                   END
033000980330      *
033100980401      * Imposto messaggio d'errore
033200980331     C     *IN28         IFEQ      '1'
033300980331     C                   Z-ADD     NRR6          NR6KEY
033400980330     C                   MOVEL     ERR(1)        WINMSG
033500980330     C                   END
033600980330      *
033700980330      *  Aggiorno subfile
033800980330     C                   SETON                                        22
033900980410     C                   UPDATE    LRK1S06
034000980330     C                   SETOFF                                       22
034100980330      *
034200980330      *  Aggiorno subfile nascosto
034300980410     C     V6CNR7        CHAIN     LRK1S07                            31
034400980330     C                   MOVEL     V6CSCE        V7CSCE
034500980421     C                   Z-ADD     V6CDSE        V7CDSE
034600980410     C  N31              UPDATE    LRK1S07
034700980330      *
034800980401     C                   ADD       1             NRR6
034900980410     C     NRR6          CHAIN     LRK1S06                            32
035000980330     C                   END
035100980401      *
035200980401      *  Se ho rilevato almeno un errore accendo *IN28
035300980401     C     WINMSG        COMP      *BLANKS                            2828
035400980401      *
035500980330     C                   ENDSR
035600980331      *---------------------------------------------------------------*
035700980331      * No errori conferma scelte
035800980331      *---------------------------------------------------------------*
035900980331     C     CONF06        BEGSR
036000980331      *
036100980331     C                   Z-ADD     1             NRR7
036200980331      *
036300980410     C     NRR7          CHAIN     LRK1S07                            31
036400980331      *
036500980331     C     *IN31         DOWEQ     '0'
036600980331      *
036700980331      *  Annulla record
036800980331      *
036900980331     C     V7CSCE        IFEQ      'A'
037000980429     C     KCDA          CHAIN     FNCDA000                           32
037100980331      *
037200980429     C     *IN32         DOWEQ     '0'
037300980331     C                   EXSR      VERRCD
037400980429     C   32              DELETE    FNCDA000
037500980429     C  N32KCDA          READE     FNCDA000                               32
037600980331     C                   END
037700980331      *
037800980331     C                   END
037900980331      *
038000980331      *  Escludi record
038100980331      *
038200980331     C     V7CSCE        IFEQ      'E'
038300980429     C     KCDA          CHAIN     FNCDA000                           32
038400980429      *
038500980429     C     *IN32         DOWEQ     '0'
038600980331     C                   EXSR      VERRCD
038700980422     C   32              MOVEL     *BLANKS       CDACCA
038800980422     C   32              Z-ADD     *ZEROS        CDANDC
038900980410     C   32              EXCEPT    AGGCDA
039000980429     C  N32KCDA          READE     FNCDA000                               32
039100980331     C                   END
039200980331      *
039300980331     C                   END
039400980422      *
039500980422      *  Forzo record - Imposto tests se esiste dettaglio segnacolli.
039600980422      *
039700980422     C     V7CSCE        IFEQ      'F'
039800980422     C     V7CDSE        ANDEQ     1
039900980429      *
040000980429     C     KCDA          CHAIN     FNCDA000                           32
040100980429      *
040200980429     C     *IN32         DOWEQ     '0'
040300980422     C                   EXSR      VERRCD
040400980422     C   32              MOVEL     '1'           CDAFL3
040500980422     C   32              EXCEPT    AGGCDA
040600980429     C  N32KCDA          READE     FNCDA000                               32
040700980422     C                   END
040800980422      *
040900980422     C                   END
041000980331      *
041100980331      * Leggo
041200980331     C                   ADD       1             NRR7
041300980410     C     NRR7          CHAIN     LRK1S07                            31
041400980331     C                   END
041500980331      *
041600980422      * Imposto fine PGM e test di dettaglio eventi elaborati
041700980331     C                   MOVEL     'S'           WFINE
041800980410     C                   Z-ADD     1             DK1DSE
041900980331      *
042000980401     C                   COMMIT
042100980401      *
042200980331     C                   ENDSR
042300980331      *---------------------------------------------------------------*
042400980331      *  Verifica record
042500980331      *---------------------------------------------------------------*
042600980331     C     VERRCD        BEGSR
042700980331      *
042800980410     C     CDANDC        IFEQ      DK1NDT                                       numero distinta
042900980410     C     CDADAM        ANDEQ     V7XDAM                                       data evento
043000980410     C     CDAHRA        ANDEQ     V7CHRA                                       ora evento
043100980410     C     CDASTS        ANDEQ     V7CSTS                                       stato
043200980410     C     CDACOD        ANDEQ     V7CCOD                                       codice
043300050504      ***
043400050504      ***   Se NON sono gestiti i Segnacolli ricevuti come PCI in dettaglio
043500050504     c                   IF        cdanot = *blank
043600050504      ***
043700980331     C                   SETON                                        32
043800050504      ***
043900050504     c                   eLSe
044000050504      *** SE INVECE DEVE CONTROLLARE CON I SEGNACOLLI:
044100050504      * se il segnacollo corrisponde allora abilita l'aggionramento del record
044200050504     c                   IF        cdanot = v7csee
044300050504     C                   Seton                                        32
044400050504      * se il segnacollo non corrisponde deve leggere il successivo
044500050504     c                   else
044600050504     C                   Setoff                                       32
044700050504     c                   endIF
044800050504     c                   endIF
044900050504      ***
045000980331     C                   ELSE
045100980331     C                   SETOFF                                       32
045200980331     C                   END
045300980331      *
045400980331     C                   ENDSR
045500980416      *---------------------------------------------------------------*
045600980416      *  REPERISCO LE NOTE EDI
045700980416      *---------------------------------------------------------------*
045800980416     C     SUBNOT        BEGSR
045900980416      *
046000980416     C                   MOVEL     'DD4440'      KNMC
046100980416     C                   Z-ADD     CDADAM        KDAM
046200980416     C                   Z-ADD     CDAHRA        KHRA
046300980416     C                   MOVEL     CDASTS        KSTS
046400980416     C                   MOVEL     CDACOD        KCOD
046500980416      *
046600980416     C                   MOVEL     *BLANKS       NOTE70           70
046700980416      *
046800980416     C     KRDA          CHAIN     EDRDA000                           31
046900980416     C     *IN31         IFEQ      '0'
047000980416     C                   MOVEL     RDAVAL        NOTE70
047100980416     C     KRDA          READE     EDRDA000                               31
047200980416     C  N31              MOVE      RDAVAL        NOTE70
047300980416     C                   END
047400980416      *
047500980416     C                   MOVEL     NOTE70        V7CNOE
047600980417      *
047700980417     C                   ENDSR
047800980421      *---------------------------------------------------------------*
047900980421      *  SE ESCO CON F3 CANCELLO EVENTUALE FILE DI LAVORO
048000980421      *---------------------------------------------------------------*
048100980421     C     DELCDW        BEGSR
048200980421      *
048300980421     C                   Z-ADD     DK1AAS        KAAS
048400980421     C                   Z-ADD     DK1LNP        KLNP
048500980421     C                   Z-ADD     DK1NRS        KNRS
048600980421     C                   Z-ADD     DK1NSP        KNSP
048700980421      *
048800980421     C     KCDW          SETLL     FNCDW000                               30
048900980421      *
049000980421     C   30              DO        *HIVAL
049100980421     C     KCDW          DELETE    FNCDW000                           31
049200980421     C  N31              END
049300980421      *
049400980421     C                   ENDSR
049500980417      *---------------------------------------------------------------*
049600980417      * DETTAGLIO SEGNACOLLI
049700980417      *---------------------------------------------------------------*
049800980417     C     SUBDSE        BEGSR
049900980420      *
050000980420     C                   MOVEL     *IN28         MEMO28            1
050100980420     C                   MOVEL     'N'           WFINE9            1
050200980421     C                   MOVEL     V6CSCE        V6§SCE
050300980421     C                   Z-ADD     V6CDSE        V6§DSE
050400980417      *
050500980420      *  Carico subfile
050600980417     C                   EXSR      DSECAS
050700980420      *
050800980420      *  Decodifico evento
050900980420     C                   Z-ADD     1             XX                2 0
051000980420     C                   MOVEL     V9CSTS        WCEV
051100980420     C                   MOVE      V9CCOD        WCEV              6
051200980421     C     WCEV          LOOKUP    CEV(XX)                                33
051300980421     C   33              MOVEL     DEV(XX)       DSCE
051400980421     C  N33              MOVEL     'S'           WFINE9
051500980417      *
051600980420      *  Gestione subfile
051700980421     C     WFINE9        DOUEQ     'S'
051800980417      *
051900980417     C  N28              Z-ADD     1             NR9KEY
052000980417      *
052100980417     C   28              EXFMT     LRK1W08
052200980417     C                   WRITE     LRK1Z09
052300980417     C                   EXFMT     LRK1C09
052400980417      *
052500980417     C                   MOVEL     *BLANKS       WINMSG
052600980417     C                   Z-ADD     1             NR9KEY
052700980417     C                   SETOFF                                       28
052800091029      * legenda status
052900091029     C     *INKD         IFEQ      '1'
053000091029     c                   call      'FNLRK1RH'
053100091029     C                   END
053200980417      *
053300980417      *  Fine Lavoro
053400980417     C     *INKC         IFEQ      '1'
053500980417     C                   MOVEL     'S'           WFINE9
053600980421     C                   MOVEL     *BLANKS       V6§SCE            1
053700980417     C                   ELSE
053800980417      *  Controlli
053900980417     C                   EXSR      DSECTR
054000980417      *  Se premo F6 e non ho errori elaboro dati per conferma
054100980417     C  N28
054200980417     CAN KF              EXSR      DSECNF
054300980421     C  N28
054400980421     CAN KF              Z-ADD     1             V6§DSE            1 0
054500980417     C                   END
054600980417      *
054700980417     C                   END
054800980417      *
054900980417     C                   MOVEL     MEMO28        *IN28
055000980417      *
055100980416     C                   ENDSR
055200980417      *--------------------------------------------------------------*
055300980417      *  DETTAGLIO SEGNACOLLI -- CARICAMENTO SUBFILE
055400980417      *--------------------------------------------------------------*
055500980417     C     DSECAS        BEGSR
055600980417      *
055700980417     C                   SETON                                        64
055800980421     C                   WRITE     LRK1C09
055900980417     C                   SETOFF                                       64
056000980417      *
056100980417     C                   Z-ADD     V6CAAS        V9CAAS
056200980417     C                   Z-ADD     V6CLNP        V9CLNP
056300980417     C                   Z-ADD     V6CNRS        V9CNRS
056400980417     C                   Z-ADD     V6CNSP        V9CNSP
056500980417     C                   Z-ADD     V6CNCL        V9CNCL
056600980420     C                   Z-ADD     V6CDAM        V9CDAE
056700980417     C                   MOVEL     V6CSTS        V9CSTS
056800980417     C                   MOVEL     V6CCOD        V9CCOD
056900980417      *
057000980417     C                   Z-ADD     1             NRR9              4 0
057100980417     C     KCDW          CHAIN     FNCDW000                           33
057200980417     C     *IN33         DOWEQ     '0'
057300980417      *
057400980417     C                   CLEAR                   WLBDA8
057500980417     C                   Z-ADD     CDWDAM        G02INV
057600980417     C                   MOVEL     '3'           G02ERR
057700980417     C                   CALL      'XSRDA8'
057800980417     C                   PARM                    WLBDA8
057900980420     C                   MOVEL     G02DAT        V9CDAM
058000980420     C                   Z-ADD     CDWDAM        V9XDAM
058100980417      *
058200980417     C                   CLEAR                   WLBDA8
058300980417     C                   Z-ADD     CDWDMA        G02INV
058400980417     C                   MOVEL     '3'           G02ERR
058500980417     C                   CALL      'XSRDA8'
058600980417     C                   PARM                    WLBDA8
058700980420     C                   MOVEL     G02DAT        V9CDMA
058800980420     C                   Z-ADD     CDWDMA        V9XDMA
058900980417      *
059000980417     C                   Z-ADD     CDWNSC        V9CNSC
059100980417     C                   MOVEL     CDWDAN        V9CDAN
059200980417     C                   MOVEL     CDWSEE        V9CSEE
059300980417     C                   Z-ADD     NRR9          V9CNRR
059400980417     C                   WRITE     LRK1S09
059500980421     C                   ADD       1             NRR9
059600980417      *
059700980417     C     KCDW          READE     FNCDW000                               33
059800980417     C                   END
059900980417      *
060000980417     C                   ENDSR
060100980420      *---------------------------------------------------------------*
060200980420      *  Controllo errori
060300980420      *---------------------------------------------------------------*
060400980420     C     DSECTR        BEGSR
060500980420      *
060600980420     C                   Z-ADD     *ZEROS        SELEV9            2 0
060700980421     C                   Z-ADD     1             NRR9
060800980420      *
060900980421     C     NRR9          CHAIN     LRK1S09                            33
061000980420      *
061100980420     C     *IN33         DOWEQ     '0'
061200980420     C     *IN28         ANDEQ     '0'
061300980420      *
061400980420     C     V9CSCE        IFEQ      '1'
061500980420     C                   ADD       1             SELEV9
061600980420      *
061700980421      * Segnalo errore se: i colli selezionati superano i colli evento;
061800980421      *    esiste data arrivo; evento mancanza ed esiste data mancanza.
061900980420     C     SELEV9        IFGT      V9CNCL
062000980421     C     V9CDAM        ORGT      *ZEROS
062100980421     C     §CETEA        OREQ      'M'
062200980421     C     V9CDMA        ANDGT     *ZEROS
062300980420     C                   SETON                                        28
062400980420     C                   END
062500980421      *
062600980421     C                   END
062700980420      *
062800980420      * Imposto messaggio d'errore
062900980420     C     *IN28         IFEQ      '1'
063000980420     C                   Z-ADD     NRR9          NR9KEY
063100980420     C                   MOVEL     ERR(1)        WINMSG
063200980420     C                   END
063300980420      *
063400980420      *  Aggiorno subfile
063500980420     C                   SETON                                        22
063600980420     C                   UPDATE    LRK1S09
063700980420     C                   SETOFF                                       22
063800980420      *
063900980421     C  N28              ADD       1             NRR9
064000980421     C  N28NRR9          CHAIN     LRK1S09                            33
064100980420     C                   END
064200980420      *
064300980420      * Se colli selezionati non corrispondono ai colli evento errore
064400980420     C  N28SELEV9        IFNE      V9CNCL
064500980420     C                   SETON                                        28
064600980420     C                   Z-ADD     1             NR9KEY
064700980420     C                   MOVEL     ERR(1)        WINMSG
064800980420     C                   END
064900980420      *
065000980420     C                   ENDSR
065100980420      *---------------------------------------------------------------*
065200980420      *  Conferma dettaglio segnacolli
065300980420      *---------------------------------------------------------------*
065400980420     C     DSECNF        BEGSR
065500980420      *
065600980420     C                   CLEAR                   WLBDA8
065700980420     C                   Z-ADD     V9CDAE        G02DAT
065800980420     C                   MOVEL     *BLANKS       G02ERR
065900980420     C                   CALL      'XSRDA8'
066000980420     C                   PARM                    WLBDA8
066100980420     C                   Z-ADD     G02INV        DATEV9            8 0
066200980420      *
066300980420     C                   Z-ADD     1             NRR9
066400980420      *
066500980420     C     NRR9          CHAIN     LRK1S09                            33
066600980420      *
066700980420     C     *IN33         DOWEQ     '0'
066800980420      *
066900980420     C                   Z-ADD     V9CAAS        KAAS
067000980420     C                   Z-ADD     V9CLNP        KLNP
067100980420     C                   Z-ADD     V9CNRS        KNRS
067200980420     C                   Z-ADD     V9CNSP        KNSP
067300980420     C                   Z-ADD     V9CNSC        KNSC
067400980421     C     KCDW1         CHAIN     FNCDW000                           33
067500980421      *
067600980420      * Se evento mancanza aggiorno data mancanza per i colli selezionati,
067700980420      * per i non selezionati aggiorno la data arrivo
067800980422      *
067900980420     C     §CETEA        IFEQ      'M'
068000980420      *
068100980420     C     V9CSCE        IFEQ      '1'
068200980420     C     CDWDMA        ANDLT     DATEV9
068300980420     C                   Z-ADD     DATEV9        CDWDMA
068400980422     C                   MOVEL     'M'           CDWIMM
068500980420     C                   ELSE
068600980420     C     V9CSCE        IFEQ      *BLANKS
068700980420     C     CDWDAM        ANDEQ     *ZEROS
068800980420     C                   Z-ADD     DATEV9        CDWDAM
068900980422     C                   MOVEL     'A'           CDWIMA
069000980420     C                   END
069100980420     C                   END
069200980420      *
069300980420     C                   ELSE
069400980420      *
069500980420     C     V9CSCE        IFEQ      '1'
069600980420      *
069700980420     C     §CETEA        IFEQ      'D'
069800980420     C                   MOVEL     'D'           CDWDAN
069900980422     C                   MOVEL     'D'           CDWIMA
070000980420     C                   END
070100980420      *
070200980420     C     CDWDAM        IFEQ      *ZEROS
070300980420     C                   Z-ADD     DATEV9        CDWDAM
070400980422     C     CDWIMA        IFEQ      *BLANKS
070500980422     C                   MOVEL     'A'           CDWIMA
070600980420     C                   END
070700980422     C                   END
070800980420      *
070900980420     C                   END
071000980420      *
071100980420     C                   END
071200980420      *
071300980420      *  Aggiorno File
071400980420     C                   UPDATE    FNCDW000
071500980420      *
071600980420     C                   ADD       1             NRR9
071700980420     C     NRR9          CHAIN     LRK1S09                            33
071800980420     C                   END
071900980420      *
072000980421     C                   MOVEL     'S'           WFINE9
072100980421      *
072200980420     C                   ENDSR
072300980421      *---------------------------------------------------------------*
072400980421      *  Aggiorno dettaglio segnacolli "batch"
072500980421      *---------------------------------------------------------------*
072600980421     C     CRTDSE        BEGSR
072700980421      *
072800980421      *  Decodifico l'evento
072900980421     C                   Z-ADD     1             XX                2 0
073000980421     C                   MOVEL     V6CSTS        WCEV
073100980421     C                   MOVE      V6CCOD        WCEV              6
073200980421     C     WCEV          LOOKUP    CEV(XX)                                33
073300980421     C   33              MOVEL     DEV(XX)       DSCE
073400980421      *
073500980421     C                   Z-ADD     V6CAAS        KAAS
073600980421     C                   Z-ADD     V6CLNP        KLNP
073700980421     C                   Z-ADD     V6CNRS        KNRS
073800980421     C                   Z-ADD     V6CNSP        KNSP
073900980421     C     KCDW          CHAIN     FNCDW000                           33
074000980421      *
074100980421     C     *IN33         DOWEQ     '0'
074200980421      *
074300980421      * Se evento mancanza aggiorno data mancanza del collo selezionato, per
074400980421      * i non selezionati aggiorno data arrivo. Eccezioni per eventi aggiuntivi.
074500980421     C     §CETEA        IFEQ      'M'
074600980421      *
074700980421     C     CDWSEE        IFEQ      V6CSEE
074800980421     C     CDWDMA        ANDLT     V6XDAM
074900980421     C                   Z-ADD     V6XDAM        CDWDMA
075000980422     C                   MOVEL     'M'           CDWIMM
075100980421      * Se evento aggiuntivo cancello data arrivo
075200980422     C     V6CIN4        IFEQ      '1'
075300980421     C     CDWDAM        ANDGT     *ZEROS
075400980421     C                   Z-ADD     *ZEROS        CDWDAM
075500980422     C                   MOVEL     *BLANKS       CDWIMA
075600980421     C                   END
075700980421      *
075800980421      * Imposto arrivati gli altri segnacolli tranne se evento aggiuntivo
075900980421     C                   ELSE
076000980421     C     CDWSEE        IFNE      V6CSEE
076100980421     C     CDWDAM        ANDEQ     *ZEROS
076200980422     C     V6CIN4        ANDEQ     '0'
076300980421     C                   Z-ADD     V6XDAM        CDWDAM
076400980422     C                   MOVEL     'A'           CDWIMA
076500980421     C                   END
076600980421     C                   END
076700980421      *
076800980421     C                   ELSE
076900980421      *
077000980421     C     CDWSEE        IFEQ      V6CSEE
077100980421      *
077200980421     C     §CETEA        IFEQ      'D'
077300980421     C                   MOVEL     'D'           CDWDAN
077400980422     C                   MOVEL     'D'           CDWIMA
077500980421     C                   END
077600980421     C     CDWDAM        IFEQ      *ZEROS
077700980421     C                   Z-ADD     V6XDAM        CDWDAM
077800980422     C     CDWIMA        IFEQ      *BLANKS
077900980422     C                   MOVEL     'A'           CDWIMA
078000980421     C                   END
078100980422     C                   END
078200980421     C                   END
078300980421      *
078400980421     C                   END
078500980421     C                   UPDATE    FNCDW000
078600980421      *
078700980421     C     KCDW          READE     FNCDW000                               33
078800980421     C                   END
078900980421      *
079000980421     C                   ENDSR
079100980330      *--------------------------------------------------------------*
079200980330      *  SUBROUTINE INIZIALE
079300980330      *--------------------------------------------------------------*
079400980330     C     *INZSR        BEGSR
079500980325      *
079600980325      * Definisco chiave di accesso
079700980417     C     KCDW          KLIST
079800980417     C                   KFLD                    KAAS
079900980417     C                   KFLD                    KLNP
080000980417     C                   KFLD                    KNRS
080100980417     C                   KFLD                    KNSP
080200980421      *
080300980421     C     KCDW1         KLIST
080400980421     C                   KFLD                    KAAS
080500980421     C                   KFLD                    KLNP
080600980421     C                   KFLD                    KNRS
080700980421     C                   KFLD                    KNSP
080800980421     C                   KFLD                    KNSC
080900980417      *
081000980417     C     KARS          KLIST
081100980417     C                   KFLD                    KFLS
081200980417     C                   KFLD                    KLNA
081300980417     C                   KFLD                    KNRS
081400980417     C                   KFLD                    KNSC
081500980417     C                   KFLD                    KTRC
081600980417      *
081700980410     C     KCDA          KLIST
081800980410     C                   KFLD                    KLNA
081900980325     C                   KFLD                    KAAS
082000980325     C                   KFLD                    KLNP
082100980325     C                   KFLD                    KNRS
082200980325     C                   KFLD                    KNSP
082300980325      *
082400980416     C     KRDA          KLIST
082500980416     C                   KFLD                    KAAS
082600980416     C                   KFLD                    KLNP
082700980416     C                   KFLD                    KNRS
082800980416     C                   KFLD                    KNSP
082900980416     C                   KFLD                    KNMC
083000980416     C                   KFLD                    KDAM
083100980416     C                   KFLD                    KHRA
083200980416     C                   KFLD                    KSTS
083300980416     C                   KFLD                    KCOD
083400980416      *
083500980325      * Definisco variabili
083600980410     C     *LIKE         DEFINE    CDALNA        KLNA
083700980410     C     *LIKE         DEFINE    CDAAAS        KAAS
083800980410     C     *LIKE         DEFINE    CDALNP        KLNP
083900980410     C     *LIKE         DEFINE    CDANRS        KNRS
084000980410     C     *LIKE         DEFINE    CDANSP        KNSP
084100980416     C     *LIKE         DEFINE    RDANMC        KNMC
084200980416     C     *LIKE         DEFINE    RDADAM        KDAM
084300980416     C     *LIKE         DEFINE    RDAHRA        KHRA
084400980416     C     *LIKE         DEFINE    RDASTS        KSTS
084500980416     C     *LIKE         DEFINE    RDACOD        KCOD
084600980417     C     *LIKE         DEFINE    ARTFLS        KFLS
084700980417     C     *LIKE         DEFINE    ARTNSC        KNSC
084800980417     C     *LIKE         DEFINE    ARSTRC        KTRC
084900020913      *
085000020913      * Decodifica Elaboratore
085100020913      *  TIBS34R richiamato dal FNLRK1R ha impostato i dati
085200020913      *   che servono anche in questo programma.
085300020913     c     *Dtaara       Define    §datiute      DDatiUte
085400020913     c                   In        *Dtaara
085500980325      *
085600980330     C                   MOVEL     'N'           WFINE             1
085700980331     C                   MOVEL     '6'           WTPVID            1
085800980330      *
085900980420      * Caricamento Tabella codici eventi
086000980420     C                   Z-ADD     0             X                 3 0
086100980420     C                   MOVEL     'CE'          TABCOD
086200980420     C     TABCOD        CHAIN     EDTAB01L                           30
086300980420     C     *IN30         DOWEQ     '0'
086400980420     C     TABFLG        IFEQ      *BLANKS
086500980420     C                   ADD       1             X
086600980420     C                   MOVEL     TABKEY        CEV(X)
086700980420     C                   MOVEL     TABUNI        DEV(X)
086800980420     C                   MOVEL     DEV(X)        DSCE
086900980420     C                   END
087000980420     C     TABCOD        READE     EDTAB01L                               30
087100980420     C                   END
087200980420      *
087300980325     C                   ENDSR
087400980330      *--------------------------------------------------------------*
087500980410     OFNCDA000  E            AGGCDA
087600980410     O                       CDANDC
087700980422     O                       CDAFL3
087800980422     O                       CDACCA
087900980330      *--------------------------------------------------------------*
088000980424**
088100980330Selezioni non congruenti. Controlla.
088200980424Evento non forzabile !!

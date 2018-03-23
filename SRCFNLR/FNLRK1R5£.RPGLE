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
020800980331      *
020900980330      *  Fine Lavoro
021000980331     C     *INKC         IFEQ      '1'
021100980330     C                   MOVEL     'S'           WFINE
021200980421     C                   EXSR      DELCDW
021300980330     C                   GOTO      FINVD3
021400980330     C                   END
021500980331      *  Controlli
021600980330     C                   EXSR      CTR06
021700980416      *  Se non confermo o ci sono errori ricarico il subfile
021800980416     C     *INKF         IFEQ      '0'
021900980416     C     *IN28         OREQ      '1'
022000980416     C                   EXSR      INZS06
022100980416     C                   END
022200980331      *  Se non ho errori elaboro dati per conferma
022300980330     C  N28
022400980330     CAN KF              EXSR      CONF06
022500980330      *
022600980330     C     FINVD3        ENDSR
022700980330      *---------------------------------------------------------------*
022800980331      * Gestione videata subfile emissione                            *
022900980330      *---------------------------------------------------------------*
023000980330     C     INZS06        BEGSR
023100980330      *
023200980330     C                   SETON                                        63
023300980410     C                   WRITE     LRK1C06
023400980330     C                   SETOFF                                       63
023500980330      *
023600980330     C                   Z-ADD     1             NRR7              4 0
023700980330     C                   Z-ADD     0             NRR6              4 0
023800980330     C                   MOVEL     'N'           WFINE
023900980330      *
024000980410     C     NRR7          CHAIN     LRK1S07                            31
024100980330      *
024200980330     C     *IN31         DOWEQ     '0'
024300980330      *
024400980331     C                   SETOFF                                       22
024500980330      *
024600980331      *  Scrivo subfile emissione da subfile hidden
024700980416     C                   MOVEL     V7CIN4        V6CIN4
024800980416     C                   MOVEL     V7CIN4        *IN04
024900980331     C                   MOVEL     V7CSTS        V6CSTS
025000980331     C                   MOVEL     V7CCOD        V6CCOD
025100980331     C                   MOVEL     V7CSEE        V6CSEE
025200980410     C                   Z-ADD     V7CDAM        V6CDAM
025300980421     C                   Z-ADD     V7XDAM        V6XDAM
025400980410     C                   Z-ADD     V7CHRA        V6CHRA
025500980416     C                   Z-ADD     V7CNCL        V6CNCL
025600980416     C                   MOVEL     V7CNOE        V6CNOE
025700980330     C                   MOVEL     V7CSCE        V6CSCE
025800980421     C                   Z-ADD     V7CDSE        V6CDSE
025900980421     C                   MOVEL     V7CDSE        *IN05
026000980330     C                   Z-ADD     NRR7          V6CNR7
026100980330     C                   ADD       1             NRR6
026200980410     C                   WRITE     LRK1S06
026300980330      *
026400980330     C                   ADD       1             NRR7
026500980410     C     NRR7          CHAIN     LRK1S07                            31
026600980330     C                   END
026700980330      *
026800980330     C                   ENDSR
026900980330      *---------------------------------------------------------------*
027000980331      *  Controllo errori
027100980330      *---------------------------------------------------------------*
027200980330     C     CTR06         BEGSR
027300980330      *
027400980421     C                   SETOFF                                       28
027500980422     C                   MOVEL     *BLANKS       WSVSCE            1
027600980424      *
027700980331     C                   Z-ADD     1             NRR6
027800980421     C     NRR6          CHAIN     LRK1S06                            32
027900980330      *
028000980330     C     *IN32         DOWEQ     '0'
028100980330      *
028200980416      *  Se evento aggiuntivo prendo ex scelta
028300980422     C     V6CIN4        IFEQ      '1'
028400980416     C                   MOVEL     WSVSCE        V6CSCE
028500980416     C                   ELSE
028600980422     C                   MOVEL     V6CSCE        WSVSCE
028700980416     C                   END
028800980416      *
028900980331     C                   SETOFF                                       31
029000980421      *
029100980421      *  - Forzo record -
029200980424      *  Se spedizione = 0 non consento la forzatura.
029300980424      *  Se colli evento <>  colli sped. elaboro i segnacolli, solo 1 volta.
029400980421      *  Se segn. EUROEXPRESS = blanks la selezione dei segnacolli da aggiornare
029500980421      *  è interattiva. Se segn. EUROEXPRESS <> blanks provvedo io.
029600980421      *
029700980421     C  N28V6CSCE        IFEQ      'F'
029800980424      *
029900980424     C     V6CNSP        IFEQ      *ZEROS
030000980424     C                   Z-ADD     NRR6          NR6KEY
030100980424     C                   MOVEL     ERR(2)        WINMSG
030200980424     C                   ELSE
030300980424      *
030400980424     C     V6CNCL        IFNE      V6CNCS
030500980420     C     V6CDSE        ANDEQ     *ZEROS
030600980421      *
030700980421     C     V6CSEE        IFEQ      *BLANKS
030800980417     C                   EXSR      SUBDSE
030900980421     C     NRR6          CHAIN     LRK1S06                            32
031000980421     C                   MOVEL     V6§SCE        V6CSCE
031100980421     C                   Z-ADD     V6§DSE        V6CDSE
031200980421     C                   ELSE
031300980421     C                   EXSR      CRTDSE
031400980421     C                   Z-ADD     1             V6CDSE
031500980417     C                   END
031600980421      *
031700980330     C                   END
031800980424     C                   END
031900980421     C                   END
032000980416      *
032100980416      *  Nessuna scelta
032200980416      *
032300980416     C     V6CSCE        IFEQ      *BLANKS
032400980416     C                   SETON                                        28
032500980416     C                   END
032600980330      *
032700980401      * Imposto messaggio d'errore
032800980331     C     *IN28         IFEQ      '1'
032900980331     C                   Z-ADD     NRR6          NR6KEY
033000980330     C                   MOVEL     ERR(1)        WINMSG
033100980330     C                   END
033200980330      *
033300980330      *  Aggiorno subfile
033400980330     C                   SETON                                        22
033500980410     C                   UPDATE    LRK1S06
033600980330     C                   SETOFF                                       22
033700980330      *
033800980330      *  Aggiorno subfile nascosto
033900980410     C     V6CNR7        CHAIN     LRK1S07                            31
034000980330     C                   MOVEL     V6CSCE        V7CSCE
034100980421     C                   Z-ADD     V6CDSE        V7CDSE
034200980410     C  N31              UPDATE    LRK1S07
034300980330      *
034400980401     C                   ADD       1             NRR6
034500980410     C     NRR6          CHAIN     LRK1S06                            32
034600980330     C                   END
034700980401      *
034800980401      *  Se ho rilevato almeno un errore accendo *IN28
034900980401     C     WINMSG        COMP      *BLANKS                            2828
035000980401      *
035100980330     C                   ENDSR
035200980331      *---------------------------------------------------------------*
035300980331      * No errori conferma scelte
035400980331      *---------------------------------------------------------------*
035500980331     C     CONF06        BEGSR
035600980331      *
035700980331     C                   Z-ADD     1             NRR7
035800980331      *
035900980410     C     NRR7          CHAIN     LRK1S07                            31
036000980331      *
036100980331     C     *IN31         DOWEQ     '0'
036200980331      *
036300980331      *  Annulla record
036400980331      *
036500980331     C     V7CSCE        IFEQ      'A'
036600980429     C     KCDA          CHAIN     FNCDA000                           32
036700980331      *
036800980429     C     *IN32         DOWEQ     '0'
036900980331     C                   EXSR      VERRCD
037000980429     C   32              DELETE    FNCDA000
037100980429     C  N32KCDA          READE     FNCDA000                               32
037200980331     C                   END
037300980331      *
037400980331     C                   END
037500980331      *
037600980331      *  Escludi record
037700980331      *
037800980331     C     V7CSCE        IFEQ      'E'
037900980429     C     KCDA          CHAIN     FNCDA000                           32
038000980429      *
038100980429     C     *IN32         DOWEQ     '0'
038200980331     C                   EXSR      VERRCD
038300980422     C   32              MOVEL     *BLANKS       CDACCA
038400980422     C   32              Z-ADD     *ZEROS        CDANDC
038500980410     C   32              EXCEPT    AGGCDA
038600980429     C  N32KCDA          READE     FNCDA000                               32
038700980331     C                   END
038800980331      *
038900980331     C                   END
039000980422      *
039100980422      *  Forzo record - Imposto tests se esiste dettaglio segnacolli.
039200980422      *
039300980422     C     V7CSCE        IFEQ      'F'
039400980422     C     V7CDSE        ANDEQ     1
039500980429      *
039600980429     C     KCDA          CHAIN     FNCDA000                           32
039700980429      *
039800980429     C     *IN32         DOWEQ     '0'
039900980422     C                   EXSR      VERRCD
040000980422     C   32              MOVEL     '1'           CDAFL3
040100980422     C   32              EXCEPT    AGGCDA
040200980429     C  N32KCDA          READE     FNCDA000                               32
040300980422     C                   END
040400980422      *
040500980422     C                   END
040600980331      *
040700980331      * Leggo
040800980331     C                   ADD       1             NRR7
040900980410     C     NRR7          CHAIN     LRK1S07                            31
041000980331     C                   END
041100980331      *
041200980422      * Imposto fine PGM e test di dettaglio eventi elaborati
041300980331     C                   MOVEL     'S'           WFINE
041400980410     C                   Z-ADD     1             DK1DSE
041500980331      *
041600980401     C                   COMMIT
041700980401      *
041800980331     C                   ENDSR
041900980331      *---------------------------------------------------------------*
042000980331      *  Verifica record
042100980331      *---------------------------------------------------------------*
042200980331     C     VERRCD        BEGSR
042300980331      *
042400980410     C     CDANDC        IFEQ      DK1NDT                                       numero distinta
042500980410     C     CDADAM        ANDEQ     V7XDAM                                       data evento
042600980410     C     CDAHRA        ANDEQ     V7CHRA                                       ora evento
042700980410     C     CDASTS        ANDEQ     V7CSTS                                       stato
042800980410     C     CDACOD        ANDEQ     V7CCOD                                       codice
042900050504      ***
043000050504      ***   Se NON sono gestiti i Segnacolli ricevuti come PCI in dettaglio
043100050504     c                   IF        cdanot = *blank
043200050504      ***
043300980331     C                   SETON                                        32
043400050504      ***
043500050504     c                   eLSe
043600050504      *** SE INVECE DEVE CONTROLLARE CON I SEGNACOLLI:
043700050504      * se il segnacollo corrisponde allora abilita l'aggionramento del record
043800050504     c                   IF        cdanot = v7csee
043900050504     C                   Seton                                        32
044000050504      * se il segnacollo non corrisponde deve leggere il successivo
044100050504     c                   else
044200050504     C                   Setoff                                       32
044300050504     c                   endIF
044400050504     c                   endIF
044500050504      ***
044600980331     C                   ELSE
044700980331     C                   SETOFF                                       32
044800980331     C                   END
044900980331      *
045000980331     C                   ENDSR
045100980416      *---------------------------------------------------------------*
045200980416      *  REPERISCO LE NOTE EDI
045300980416      *---------------------------------------------------------------*
045400980416     C     SUBNOT        BEGSR
045500980416      *
045600980416     C                   MOVEL     'DD4440'      KNMC
045700980416     C                   Z-ADD     CDADAM        KDAM
045800980416     C                   Z-ADD     CDAHRA        KHRA
045900980416     C                   MOVEL     CDASTS        KSTS
046000980416     C                   MOVEL     CDACOD        KCOD
046100980416      *
046200980416     C                   MOVEL     *BLANKS       NOTE70           70
046300980416      *
046400980416     C     KRDA          CHAIN     EDRDA000                           31
046500980416     C     *IN31         IFEQ      '0'
046600980416     C                   MOVEL     RDAVAL        NOTE70
046700980416     C     KRDA          READE     EDRDA000                               31
046800980416     C  N31              MOVE      RDAVAL        NOTE70
046900980416     C                   END
047000980416      *
047100980416     C                   MOVEL     NOTE70        V7CNOE
047200980417      *
047300980417     C                   ENDSR
047400980421      *---------------------------------------------------------------*
047500980421      *  SE ESCO CON F3 CANCELLO EVENTUALE FILE DI LAVORO
047600980421      *---------------------------------------------------------------*
047700980421     C     DELCDW        BEGSR
047800980421      *
047900980421     C                   Z-ADD     DK1AAS        KAAS
048000980421     C                   Z-ADD     DK1LNP        KLNP
048100980421     C                   Z-ADD     DK1NRS        KNRS
048200980421     C                   Z-ADD     DK1NSP        KNSP
048300980421      *
048400980421     C     KCDW          SETLL     FNCDW000                               30
048500980421      *
048600980421     C   30              DO        *HIVAL
048700980421     C     KCDW          DELETE    FNCDW000                           31
048800980421     C  N31              END
048900980421      *
049000980421     C                   ENDSR
049100980417      *---------------------------------------------------------------*
049200980417      * DETTAGLIO SEGNACOLLI
049300980417      *---------------------------------------------------------------*
049400980417     C     SUBDSE        BEGSR
049500980420      *
049600980420     C                   MOVEL     *IN28         MEMO28            1
049700980420     C                   MOVEL     'N'           WFINE9            1
049800980421     C                   MOVEL     V6CSCE        V6§SCE
049900980421     C                   Z-ADD     V6CDSE        V6§DSE
050000980417      *
050100980420      *  Carico subfile
050200980417     C                   EXSR      DSECAS
050300980420      *
050400980420      *  Decodifico evento
050500980420     C                   Z-ADD     1             XX                2 0
050600980420     C                   MOVEL     V9CSTS        WCEV
050700980420     C                   MOVE      V9CCOD        WCEV              6
050800980421     C     WCEV          LOOKUP    CEV(XX)                                33
050900980421     C   33              MOVEL     DEV(XX)       DSCE
051000980421     C  N33              MOVEL     'S'           WFINE9
051100980417      *
051200980420      *  Gestione subfile
051300980421     C     WFINE9        DOUEQ     'S'
051400980417      *
051500980417     C  N28              Z-ADD     1             NR9KEY
051600980417      *
051700980417     C   28              EXFMT     LRK1W08
051800980417     C                   WRITE     LRK1Z09
051900980417     C                   EXFMT     LRK1C09
052000980417      *
052100980417     C                   MOVEL     *BLANKS       WINMSG
052200980417     C                   Z-ADD     1             NR9KEY
052300980417     C                   SETOFF                                       28
052400980417      *
052500980417      *  Fine Lavoro
052600980417     C     *INKC         IFEQ      '1'
052700980417     C                   MOVEL     'S'           WFINE9
052800980421     C                   MOVEL     *BLANKS       V6§SCE            1
052900980417     C                   ELSE
053000980417      *  Controlli
053100980417     C                   EXSR      DSECTR
053200980417      *  Se premo F6 e non ho errori elaboro dati per conferma
053300980417     C  N28
053400980417     CAN KF              EXSR      DSECNF
053500980421     C  N28
053600980421     CAN KF              Z-ADD     1             V6§DSE            1 0
053700980417     C                   END
053800980417      *
053900980417     C                   END
054000980417      *
054100980417     C                   MOVEL     MEMO28        *IN28
054200980417      *
054300980416     C                   ENDSR
054400980417      *--------------------------------------------------------------*
054500980417      *  DETTAGLIO SEGNACOLLI -- CARICAMENTO SUBFILE
054600980417      *--------------------------------------------------------------*
054700980417     C     DSECAS        BEGSR
054800980417      *
054900980417     C                   SETON                                        64
055000980421     C                   WRITE     LRK1C09
055100980417     C                   SETOFF                                       64
055200980417      *
055300980417     C                   Z-ADD     V6CAAS        V9CAAS
055400980417     C                   Z-ADD     V6CLNP        V9CLNP
055500980417     C                   Z-ADD     V6CNRS        V9CNRS
055600980417     C                   Z-ADD     V6CNSP        V9CNSP
055700980417     C                   Z-ADD     V6CNCL        V9CNCL
055800980420     C                   Z-ADD     V6CDAM        V9CDAE
055900980417     C                   MOVEL     V6CSTS        V9CSTS
056000980417     C                   MOVEL     V6CCOD        V9CCOD
056100980417      *
056200980417     C                   Z-ADD     1             NRR9              4 0
056300980417     C     KCDW          CHAIN     FNCDW000                           33
056400980417     C     *IN33         DOWEQ     '0'
056500980417      *
056600980417     C                   CLEAR                   WLBDA8
056700980417     C                   Z-ADD     CDWDAM        G02INV
056800980417     C                   MOVEL     '3'           G02ERR
056900980417     C                   CALL      'XSRDA8'
057000980417     C                   PARM                    WLBDA8
057100980420     C                   MOVEL     G02DAT        V9CDAM
057200980420     C                   Z-ADD     CDWDAM        V9XDAM
057300980417      *
057400980417     C                   CLEAR                   WLBDA8
057500980417     C                   Z-ADD     CDWDMA        G02INV
057600980417     C                   MOVEL     '3'           G02ERR
057700980417     C                   CALL      'XSRDA8'
057800980417     C                   PARM                    WLBDA8
057900980420     C                   MOVEL     G02DAT        V9CDMA
058000980420     C                   Z-ADD     CDWDMA        V9XDMA
058100980417      *
058200980417     C                   Z-ADD     CDWNSC        V9CNSC
058300980417     C                   MOVEL     CDWDAN        V9CDAN
058400980417     C                   MOVEL     CDWSEE        V9CSEE
058500980417     C                   Z-ADD     NRR9          V9CNRR
058600980417     C                   WRITE     LRK1S09
058700980421     C                   ADD       1             NRR9
058800980417      *
058900980417     C     KCDW          READE     FNCDW000                               33
059000980417     C                   END
059100980417      *
059200980417     C                   ENDSR
059300980420      *---------------------------------------------------------------*
059400980420      *  Controllo errori
059500980420      *---------------------------------------------------------------*
059600980420     C     DSECTR        BEGSR
059700980420      *
059800980420     C                   Z-ADD     *ZEROS        SELEV9            2 0
059900980421     C                   Z-ADD     1             NRR9
060000980420      *
060100980421     C     NRR9          CHAIN     LRK1S09                            33
060200980420      *
060300980420     C     *IN33         DOWEQ     '0'
060400980420     C     *IN28         ANDEQ     '0'
060500980420      *
060600980420     C     V9CSCE        IFEQ      '1'
060700980420     C                   ADD       1             SELEV9
060800980420      *
060900980421      * Segnalo errore se: i colli selezionati superano i colli evento;
061000980421      *    esiste data arrivo; evento mancanza ed esiste data mancanza.
061100980420     C     SELEV9        IFGT      V9CNCL
061200980421     C     V9CDAM        ORGT      *ZEROS
061300980421     C     §CETEA        OREQ      'M'
061400980421     C     V9CDMA        ANDGT     *ZEROS
061500980420     C                   SETON                                        28
061600980420     C                   END
061700980421      *
061800980421     C                   END
061900980420      *
062000980420      * Imposto messaggio d'errore
062100980420     C     *IN28         IFEQ      '1'
062200980420     C                   Z-ADD     NRR9          NR9KEY
062300980420     C                   MOVEL     ERR(1)        WINMSG
062400980420     C                   END
062500980420      *
062600980420      *  Aggiorno subfile
062700980420     C                   SETON                                        22
062800980420     C                   UPDATE    LRK1S09
062900980420     C                   SETOFF                                       22
063000980420      *
063100980421     C  N28              ADD       1             NRR9
063200980421     C  N28NRR9          CHAIN     LRK1S09                            33
063300980420     C                   END
063400980420      *
063500980420      * Se colli selezionati non corrispondono ai colli evento errore
063600980420     C  N28SELEV9        IFNE      V9CNCL
063700980420     C                   SETON                                        28
063800980420     C                   Z-ADD     1             NR9KEY
063900980420     C                   MOVEL     ERR(1)        WINMSG
064000980420     C                   END
064100980420      *
064200980420     C                   ENDSR
064300980420      *---------------------------------------------------------------*
064400980420      *  Conferma dettaglio segnacolli
064500980420      *---------------------------------------------------------------*
064600980420     C     DSECNF        BEGSR
064700980420      *
064800980420     C                   CLEAR                   WLBDA8
064900980420     C                   Z-ADD     V9CDAE        G02DAT
065000980420     C                   MOVEL     *BLANKS       G02ERR
065100980420     C                   CALL      'XSRDA8'
065200980420     C                   PARM                    WLBDA8
065300980420     C                   Z-ADD     G02INV        DATEV9            8 0
065400980420      *
065500980420     C                   Z-ADD     1             NRR9
065600980420      *
065700980420     C     NRR9          CHAIN     LRK1S09                            33
065800980420      *
065900980420     C     *IN33         DOWEQ     '0'
066000980420      *
066100980420     C                   Z-ADD     V9CAAS        KAAS
066200980420     C                   Z-ADD     V9CLNP        KLNP
066300980420     C                   Z-ADD     V9CNRS        KNRS
066400980420     C                   Z-ADD     V9CNSP        KNSP
066500980420     C                   Z-ADD     V9CNSC        KNSC
066600980421     C     KCDW1         CHAIN     FNCDW000                           33
066700980421      *
066800980420      * Se evento mancanza aggiorno data mancanza per i colli selezionati,
066900980420      * per i non selezionati aggiorno la data arrivo
067000980422      *
067100980420     C     §CETEA        IFEQ      'M'
067200980420      *
067300980420     C     V9CSCE        IFEQ      '1'
067400980420     C     CDWDMA        ANDLT     DATEV9
067500980420     C                   Z-ADD     DATEV9        CDWDMA
067600980422     C                   MOVEL     'M'           CDWIMM
067700980420     C                   ELSE
067800980420     C     V9CSCE        IFEQ      *BLANKS
067900980420     C     CDWDAM        ANDEQ     *ZEROS
068000980420     C                   Z-ADD     DATEV9        CDWDAM
068100980422     C                   MOVEL     'A'           CDWIMA
068200980420     C                   END
068300980420     C                   END
068400980420      *
068500980420     C                   ELSE
068600980420      *
068700980420     C     V9CSCE        IFEQ      '1'
068800980420      *
068900980420     C     §CETEA        IFEQ      'D'
069000980420     C                   MOVEL     'D'           CDWDAN
069100980422     C                   MOVEL     'D'           CDWIMA
069200980420     C                   END
069300980420      *
069400980420     C     CDWDAM        IFEQ      *ZEROS
069500980420     C                   Z-ADD     DATEV9        CDWDAM
069600980422     C     CDWIMA        IFEQ      *BLANKS
069700980422     C                   MOVEL     'A'           CDWIMA
069800980420     C                   END
069900980422     C                   END
070000980420      *
070100980420     C                   END
070200980420      *
070300980420     C                   END
070400980420      *
070500980420      *  Aggiorno File
070600980420     C                   UPDATE    FNCDW000
070700980420      *
070800980420     C                   ADD       1             NRR9
070900980420     C     NRR9          CHAIN     LRK1S09                            33
071000980420     C                   END
071100980420      *
071200980421     C                   MOVEL     'S'           WFINE9
071300980421      *
071400980420     C                   ENDSR
071500980421      *---------------------------------------------------------------*
071600980421      *  Aggiorno dettaglio segnacolli "batch"
071700980421      *---------------------------------------------------------------*
071800980421     C     CRTDSE        BEGSR
071900980421      *
072000980421      *  Decodifico l'evento
072100980421     C                   Z-ADD     1             XX                2 0
072200980421     C                   MOVEL     V6CSTS        WCEV
072300980421     C                   MOVE      V6CCOD        WCEV              6
072400980421     C     WCEV          LOOKUP    CEV(XX)                                33
072500980421     C   33              MOVEL     DEV(XX)       DSCE
072600980421      *
072700980421     C                   Z-ADD     V6CAAS        KAAS
072800980421     C                   Z-ADD     V6CLNP        KLNP
072900980421     C                   Z-ADD     V6CNRS        KNRS
073000980421     C                   Z-ADD     V6CNSP        KNSP
073100980421     C     KCDW          CHAIN     FNCDW000                           33
073200980421      *
073300980421     C     *IN33         DOWEQ     '0'
073400980421      *
073500980421      * Se evento mancanza aggiorno data mancanza del collo selezionato, per
073600980421      * i non selezionati aggiorno data arrivo. Eccezioni per eventi aggiuntivi.
073700980421     C     §CETEA        IFEQ      'M'
073800980421      *
073900980421     C     CDWSEE        IFEQ      V6CSEE
074000980421     C     CDWDMA        ANDLT     V6XDAM
074100980421     C                   Z-ADD     V6XDAM        CDWDMA
074200980422     C                   MOVEL     'M'           CDWIMM
074300980421      * Se evento aggiuntivo cancello data arrivo
074400980422     C     V6CIN4        IFEQ      '1'
074500980421     C     CDWDAM        ANDGT     *ZEROS
074600980421     C                   Z-ADD     *ZEROS        CDWDAM
074700980422     C                   MOVEL     *BLANKS       CDWIMA
074800980421     C                   END
074900980421      *
075000980421      * Imposto arrivati gli altri segnacolli tranne se evento aggiuntivo
075100980421     C                   ELSE
075200980421     C     CDWSEE        IFNE      V6CSEE
075300980421     C     CDWDAM        ANDEQ     *ZEROS
075400980422     C     V6CIN4        ANDEQ     '0'
075500980421     C                   Z-ADD     V6XDAM        CDWDAM
075600980422     C                   MOVEL     'A'           CDWIMA
075700980421     C                   END
075800980421     C                   END
075900980421      *
076000980421     C                   ELSE
076100980421      *
076200980421     C     CDWSEE        IFEQ      V6CSEE
076300980421      *
076400980421     C     §CETEA        IFEQ      'D'
076500980421     C                   MOVEL     'D'           CDWDAN
076600980422     C                   MOVEL     'D'           CDWIMA
076700980421     C                   END
076800980421     C     CDWDAM        IFEQ      *ZEROS
076900980421     C                   Z-ADD     V6XDAM        CDWDAM
077000980422     C     CDWIMA        IFEQ      *BLANKS
077100980422     C                   MOVEL     'A'           CDWIMA
077200980421     C                   END
077300980422     C                   END
077400980421     C                   END
077500980421      *
077600980421     C                   END
077700980421     C                   UPDATE    FNCDW000
077800980421      *
077900980421     C     KCDW          READE     FNCDW000                               33
078000980421     C                   END
078100980421      *
078200980421     C                   ENDSR
078300980330      *--------------------------------------------------------------*
078400980330      *  SUBROUTINE INIZIALE
078500980330      *--------------------------------------------------------------*
078600980330     C     *INZSR        BEGSR
078700980325      *
078800980325      * Definisco chiave di accesso
078900980417     C     KCDW          KLIST
079000980417     C                   KFLD                    KAAS
079100980417     C                   KFLD                    KLNP
079200980417     C                   KFLD                    KNRS
079300980417     C                   KFLD                    KNSP
079400980421      *
079500980421     C     KCDW1         KLIST
079600980421     C                   KFLD                    KAAS
079700980421     C                   KFLD                    KLNP
079800980421     C                   KFLD                    KNRS
079900980421     C                   KFLD                    KNSP
080000980421     C                   KFLD                    KNSC
080100980417      *
080200980417     C     KARS          KLIST
080300980417     C                   KFLD                    KFLS
080400980417     C                   KFLD                    KLNA
080500980417     C                   KFLD                    KNRS
080600980417     C                   KFLD                    KNSC
080700980417     C                   KFLD                    KTRC
080800980417      *
080900980410     C     KCDA          KLIST
081000980410     C                   KFLD                    KLNA
081100980325     C                   KFLD                    KAAS
081200980325     C                   KFLD                    KLNP
081300980325     C                   KFLD                    KNRS
081400980325     C                   KFLD                    KNSP
081500980325      *
081600980416     C     KRDA          KLIST
081700980416     C                   KFLD                    KAAS
081800980416     C                   KFLD                    KLNP
081900980416     C                   KFLD                    KNRS
082000980416     C                   KFLD                    KNSP
082100980416     C                   KFLD                    KNMC
082200980416     C                   KFLD                    KDAM
082300980416     C                   KFLD                    KHRA
082400980416     C                   KFLD                    KSTS
082500980416     C                   KFLD                    KCOD
082600980416      *
082700980325      * Definisco variabili
082800980410     C     *LIKE         DEFINE    CDALNA        KLNA
082900980410     C     *LIKE         DEFINE    CDAAAS        KAAS
083000980410     C     *LIKE         DEFINE    CDALNP        KLNP
083100980410     C     *LIKE         DEFINE    CDANRS        KNRS
083200980410     C     *LIKE         DEFINE    CDANSP        KNSP
083300980416     C     *LIKE         DEFINE    RDANMC        KNMC
083400980416     C     *LIKE         DEFINE    RDADAM        KDAM
083500980416     C     *LIKE         DEFINE    RDAHRA        KHRA
083600980416     C     *LIKE         DEFINE    RDASTS        KSTS
083700980416     C     *LIKE         DEFINE    RDACOD        KCOD
083800980417     C     *LIKE         DEFINE    ARTFLS        KFLS
083900980417     C     *LIKE         DEFINE    ARTNSC        KNSC
084000980417     C     *LIKE         DEFINE    ARSTRC        KTRC
084100020913      *
084200020913      * Decodifica Elaboratore
084300020913      *  TIBS34R richiamato dal FNLRK1R ha impostato i dati
084400020913      *   che servono anche in questo programma.
084500020913     c     *Dtaara       Define    §datiute      DDatiUte
084600020913     c                   In        *Dtaara
084700980325      *
084800980330     C                   MOVEL     'N'           WFINE             1
084900980331     C                   MOVEL     '6'           WTPVID            1
085000980330      *
085100980420      * Caricamento Tabella codici eventi
085200980420     C                   Z-ADD     0             X                 3 0
085300980420     C                   MOVEL     'CE'          TABCOD
085400980420     C     TABCOD        CHAIN     EDTAB01L                           30
085500980420     C     *IN30         DOWEQ     '0'
085600980420     C     TABFLG        IFEQ      *BLANKS
085700980420     C                   ADD       1             X
085800980420     C                   MOVEL     TABKEY        CEV(X)
085900980420     C                   MOVEL     TABUNI        DEV(X)
086000980420     C                   MOVEL     DEV(X)        DSCE
086100980420     C                   END
086200980420     C     TABCOD        READE     EDTAB01L                               30
086300980420     C                   END
086400980420      *
086500980325     C                   ENDSR
086600980330      *--------------------------------------------------------------*
086700980410     OFNCDA000  E            AGGCDA
086800980410     O                       CDANDC
086900980422     O                       CDAFL3
087000980422     O                       CDACCA
087100980330      *--------------------------------------------------------------*
087200980424**
087300980330Selezioni non congruenti. Controlla.
087400980424Evento non forzabile !!

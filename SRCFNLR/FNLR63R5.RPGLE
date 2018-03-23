000100980325     H DECEDIT('0,') DATEDIT(*DMY.)
000200980325      *---------*----------------------------------------------------*
000300980325      * FNLR63R5*                                                    *
000400980325      *---------*                                                    *
000500980325      *         - GESTIONE CHIUSURA DISTINTA ESTERO -                *
000600980325      *           -- GESTIONE DETTAGLIO SEGNACOLLI --                *
000700980325      *--------------------------------------------------------------*
000800980325     FFNLR63D   CF   E             WORKSTN
000900980330     F                                     SFILE(LR63S06:NRR6)
001000980330     F                                     SFILE(LR63S07:NRR7)
001100980325      *---------
001200980325     FFNCDS02L  UF   E           K DISK
001300980331     F                                     COMMIT
001400980325      *--------------------------------------------------------------*
001500980330      *  SCHIERE
001600980325      *--------------------------------------------------------------*
001700980330     D ERR             S             70    DIM(1) CTDATA PERRCD(1)              ERRORI
001800980330     D SCA             S             14    DIM(99)                              ANNULLATI
001900980330     D SCE             S             14    DIM(99)                              ELIMINATI
002000980330     D SCF             S             14    DIM(99)                              FORZATI
002100980330      *--------------------------------------------------------------*
002200980330      *  DS
002300980330      *--------------------------------------------------------------*
002400980325     D KPJBA         E DS
002500980325     D FNLR63        E DS                  EXTNAME(FNLR63DS)
002600980331      *  DS per inverisone data
002700980331     D WLBDA8          DS
002800980331     D  G02DAT                 1      8  0
002900980331     D  G02INV                 9     16  0
003000980331     D  G02ERR                17     17
003100980331     D  G02TGI                18     22  0
003200980330      *
003300980330     D                 DS
003400980330     D  DATA                   1      8
003500980330     D  STS                    9     11
003600980330     D  COD                   12     14
003700980330     D  SCHI                   1     14
003800980325      *--------------------------------------------------------------*
003900980325      *  C I C L O        P R I N C I P A L E
004000980325      *--------------------------------------------------------------*
004100980325     C     *ENTRY        PLIST
004200980325     C                   PARM                    KPJBA
004300980325      *
004400980325     C                   MOVEL     KPJBU         FNLR63
004500980330      *
004600980330      *  Carico subfile hidden
004700980330     C                   EXSR      INZS07
004800980330     C     NRR7          IFEQ      *ZEROS
004900980330     C                   GOTO      FINE
005000980330     C                   END
005100980331      *
005200980331      *  Caricamento subfile emissione da subfile hidden
005300980330     C                   EXSR      INZS06
005400980331      *
005500980331      *  Loop gestione errori
005600980330     C     WFINE         DOUEQ     'S'
005700980331     C     WTPVID        CASEQ     '6'           GESS06
005800980330     C                   END
005900980330     C                   END
006000980330      *
006100980330     C     FINE          TAG
006200980330     C                   MOVEL     FNLR63        KPJBU
006300980330     C                   COMMIT
006400980330     C                   SETON                                        LR
006500980325      *--------------------------------------------------------------*
006600980330      *  Carico subfile hidden
006700980325      *--------------------------------------------------------------*
006800980330     C     INZS07        BEGSR
006900980330      *
007000980330      * Pulisco subfile
007100980330     C                   SETON                                        23
007200980330     C                   WRITE     LR63C07
007300980330     C                   SETOFF                                       23
007400980330     C                   Z-ADD     *ZEROS        NRR7
007500980330      *
007600980330     C                   Z-ADD     D63NDT        KNDC
007700980330     C                   Z-ADD     D63AAS        KAAS
007800980330     C                   Z-ADD     D63LNP        KLNP
007900980330     C                   Z-ADD     D63NRS        KNRS
008000980330     C                   Z-ADD     D63NSP        KNSP
008100980330     C     KCDS          CHAIN     FNCDS000                           30
008200980330      *
008300980330     C     *IN30         DOWEQ     '0'
008400980330      *
008500980330     C                   MOVEL     CDSSTS        V7CSTS
008600980330     C                   MOVEL     CDSCOD        V7CCOD
008700980330     C                   MOVEL     CDSMOT        V7DCOD
008800980330     C                   MOVEL     CDSSEE        V7CSEE
008900980331      *
009000980331     C                   CLEAR                   WLBDA8
009100980331      *
009200980331     C                   Z-ADD     CDSDCM        G02INV
009300980331     C                   MOVEL     '3'           G02ERR
009400980331     C                   CALL      'XSRDA8'
009500980331     C                   PARM                    WLBDA8
009600980331     C                   Z-ADD     G02DAT        V7CDCM
009700980401     C                   Z-ADD     CDSDCM        V7XDCM
009800980331      *
009900980331     C                   Z-ADD     CDSDRC        G02INV
010000980331     C                   MOVEL     '3'           G02ERR
010100980331     C                   CALL      'XSRDA8'
010200980331     C                   PARM                    WLBDA8
010300980331     C                   Z-ADD     G02DAT        V7CDRC
010400980401     C                   Z-ADD     CDSDRC        V7XDRC
010500980331      *
010600980331     C                   Z-ADD     CDSDLA        G02INV
010700980331     C                   MOVEL     '3'           G02ERR
010800980331     C                   CALL      'XSRDA8'
010900980331     C                   PARM                    WLBDA8
011000980331     C                   Z-ADD     G02DAT        V7CDLA
011100980401     C                   Z-ADD     CDSDLA        V7XDLA
011200980331      *
011300980331     C                   Z-ADD     CDSDAG        G02INV
011400980331     C                   MOVEL     '3'           G02ERR
011500980331     C                   CALL      'XSRDA8'
011600980331     C                   PARM                    WLBDA8
011700980331     C                   Z-ADD     G02DAT        V7CGIA
011800980401     C                   Z-ADD     CDSDAG        V7XGIA
011900980331      *
012000980330     C                   ADD       1             NRR7
012100980330      *
012200980330     C                   WRITE     LR63S07
012300980330     C     KCDS          READE     FNCDS000                               30
012400980330     C                   END
012500980330      *
012600980330     C                   ENDSR
012700980330      *---------------------------------------------------------------*
012800980330      * Gestione videata subfile errori                               *
012900980330      *---------------------------------------------------------------*
013000980330     C     GESS06        BEGSR
013100980330      *
013200980330     C  N28              Z-ADD     1             NR6KEY
013300980330      *
013400980331      *  Se errore emetto finestra con messaggio
013500980330     C   28              EXFMT     LR63W08
013600980330      *
013700980330     C                   WRITE     LR63Z06
013800980330     C                   EXFMT     LR63C06
013900980330      *
014000980330     C                   MOVEL     *BLANKS       WINMSG
014100980330     C                   Z-ADD     1             NR6KEY
014200980330     C                   SETOFF                                       28
014300980331      *
014400980330      *  Fine Lavoro
014500980331     C     *INKC         IFEQ      '1'
014600980330     C                   MOVEL     'S'           WFINE
014700980330     C                   GOTO      FINVD3
014800980330     C                   END
014900980331      *  Controlli
015000980330     C                   EXSR      CTR06
015100980330      *  Se ho errori ricarico il subfile
015200980330     C   28              EXSR      INZS06
015300980331      *  Se non ho errori elaboro dati per conferma
015400980330     C  N28
015500980330     CAN KF              EXSR      CONF06
015600980330      *
015700980330     C     FINVD3        ENDSR
015800980330      *---------------------------------------------------------------*
015900980331      * Gestione videata subfile emissione                            *
016000980330      *---------------------------------------------------------------*
016100980330     C     INZS06        BEGSR
016200980330      *
016300980330     C                   SETON                                        63
016400980330     C                   WRITE     LR63C06
016500980330     C                   SETOFF                                       63
016600980330      *
016700980330     C                   MOVEL     D63AAS        V6CAAS
016800980330     C                   Z-ADD     D63LNP        V6CLNP
016900980330     C                   Z-ADD     D63NRS        V6CNRS
017000980330     C                   Z-ADD     D63NSP        V6CNSP
017100980330     C                   Z-ADD     1             NRR7              4 0
017200980330     C                   Z-ADD     0             NRR6              4 0
017300980330     C                   MOVEL     'N'           WFINE
017400980330     C                   CLEAR                   SCE
017500980330     C                   CLEAR                   SCA
017600980330     C                   CLEAR                   SCF
017700980330      *
017800980330     C     NRR7          CHAIN     LR63S07                            31
017900980330      *
018000980330     C     *IN31         DOWEQ     '0'
018100980330      *
018200980331     C                   SETOFF                                       22
018300980330      *
018400980331      *  Scrivo subfile emissione da subfile hidden
018500980331     C                   MOVEL     V7CSTS        V6CSTS
018600980331     C                   MOVEL     V7CCOD        V6CCOD
018700980331     C                   MOVEL     V7DCOD        V6DCOD
018800980331     C                   MOVEL     V7CSEE        V6CSEE
018900980331     C                   Z-ADD     V7CDCM        V6CDCM
019000980331     C                   Z-ADD     V7CDRC        V6CDRC
019100980331     C                   Z-ADD     V7CDLA        V6CDLA
019200980331     C                   Z-ADD     V7CGIA        V6CGIA
019300980330     C                   MOVEL     V7CSCE        V6CSCE
019400980330     C                   Z-ADD     NRR7          V6CNR7
019500980330     C                   ADD       1             NRR6
019600980331     C                   WRITE     LR63S06
019700980330      *
019800980330     C                   ADD       1             NRR7
019900980330     C     NRR7          CHAIN     LR63S07                            31
020000980330     C                   END
020100980330      *
020200980330     C                   ENDSR
020300980330      *---------------------------------------------------------------*
020400980331      *  Controllo errori
020500980330      *---------------------------------------------------------------*
020600980330     C     CTR06         BEGSR
020700980330      *
020800980331     C                   Z-ADD     1             NRR6
020900980331     C     NRR6          CHAIN     LR63S06                            32
021000980330      *
021100980330     C     *IN32         DOWEQ     '0'
021200980330      *
021300980331     C                   SETOFF                                       31
021400980330     C                   MOVEL     *BLANKS       SCHI
021500980330      *
021600980330      *  Creo DS per schiera
021700980330      *
021800980331     C                   MOVEL     V6CSTS        STS
021900980331     C                   MOVEL     V6CCOD        COD
022000980331     C     V6CDCM        IFGT      *ZEROS
022100980331     C                   MOVEL     V6CDCM        DATA
022200980330     C                   ELSE
022300980331     C     V6CDRC        IFGT      *ZEROS
022400980331     C                   MOVEL     V6CDRC        DATA
022500980330     C                   ELSE
022600980331     C     V6CDLA        IFGT      *ZEROS
022700980331     C                   MOVEL     V6CDLA        DATA
022800980330     C                   ELSE
022900980331     C                   MOVEL     V6CGIA        DATA
023000980330     C                   END
023100980331     C                   END
023200980330     C                   END
023300980330      *
023400980330      *  Annulla record
023500980330      *
023600980331     C     V6CSCE        IFEQ      'A'
023700980330      *
023800980330     C     SCHI          LOOKUP    SCE                                    28      escluse
023900980330     C  N28SCHI          LOOKUP    SCF                                    28      forzate
024000980330      *
024100980331     C     SCHI          LOOKUP    SCA                                    31
024200980331     C     *IN31         IFEQ      '0'
024300980330     C                   Z-ADD     1             XX                3 0
024400980331     C     *BLANKS       LOOKUP    SCA(XX)                                31
024500980331     C   31              MOVEL     SCHI          SCA(XX)
024600980330     C                   END
024700980330     C                   END
024800980330      *
024900980330      *  Escludi record
025000980330      *
025100980331     C     V6CSCE        IFEQ      'E'
025200980330      *
025300980330     C     SCHI          LOOKUP    SCA                                    28      annullate
025400980330     C  N28SCHI          LOOKUP    SCF                                    28      forzate
025500980330      *
025600980331     C     SCHI          LOOKUP    SCE                                    31
025700980331     C     *IN31         IFEQ      '0'
025800980331     C                   Z-ADD     1             XX
025900980331     C     *BLANKS       LOOKUP    SCE(XX)                                31
026000980331     C   31              MOVEL     SCHI          SCE(XX)
026100980330     C                   END
026200980330     C                   END
026300980330      *
026400980330      *  Forzo record
026500980330      *
026600980331     C     V6CSCE        IFEQ      *BLANKS
026700980330      *
026800980330     C     SCHI          LOOKUP    SCA                                    28      annullate
026900980330     C  N28SCHI          LOOKUP    SCE                                    28      escluse
027000980330      *
027100980331     C     SCHI          LOOKUP    SCF                                    31
027200980331     C     *IN31         IFEQ      '0'
027300980331     C                   Z-ADD     1             XX
027400980331     C     *BLANKS       LOOKUP    SCF(XX)                                31
027500980331     C   31              MOVEL     SCHI          SCF(XX)
027600980330     C                   END
027700980330     C                   END
027800980330      *
027900980401      * Imposto messaggio d'errore
028000980331     C     *IN28         IFEQ      '1'
028100980331     C                   Z-ADD     NRR6          NR6KEY
028200980330     C                   MOVEL     ERR(1)        WINMSG
028300980330     C                   END
028400980330      *
028500980330      *  Aggiorno subfile
028600980330     C                   SETON                                        22
028700980330     C                   UPDATE    LR63S06
028800980330     C                   SETOFF                                       22
028900980330      *
029000980330      *  Aggiorno subfile nascosto
029100980330     C     V6CNR7        CHAIN     LR63S07                            31
029200980330     C                   MOVEL     V6CSCE        V7CSCE
029300980330     C  N31              UPDATE    LR63S07
029400980330      *
029500980401     C                   ADD       1             NRR6
029600980401     C     NRR6          CHAIN     LR63S06                            32
029700980330     C                   END
029800980401      *
029900980401      *  Se ho rilevato almeno un errore accendo *IN28
030000980401     C     WINMSG        COMP      *BLANKS                            2828
030100980401      *
030200980401      *  Può esistere 1 solo evento forzato
030300980401     C  N28SCF(1)        IFNE      *BLANKS
030400980401     C     SCF(2)        ANDNE     *BLANKS
030500980401     C                   SETON                                        28
030600980401     C                   Z-ADD     1             NR6KEY
030700980401     C                   MOVEL     ERR(1)        WINMSG
030800980401     C                   END
030900980330      *
031000980330     C                   ENDSR
031100980331      *---------------------------------------------------------------*
031200980331      * No errori conferma scelte
031300980331      *---------------------------------------------------------------*
031400980331     C     CONF06        BEGSR
031500980331      *
031600980331     C                   Z-ADD     1             NRR7
031700980331      *
031800980331     C     NRR7          CHAIN     LR63S07                            31
031900980331      *
032000980331     C     *IN31         DOWEQ     '0'
032100980331      *
032200980331      *  Annulla record
032300980331      *
032400980331     C     V7CSCE        IFEQ      'A'
032500980331     C     KCDS          CHAIN     FNCDS000                           30
032600980331      *
032700980331     C     *IN30         DOWEQ     '0'
032800980331     C                   EXSR      VERRCD
032900980331     C   32KCDS          DELETE    FNCDS000                           30
033000980331     C     KCDS          READE     FNCDS000                               30
033100980331     C                   END
033200980331      *
033300980331     C                   END
033400980331      *
033500980331      *  Escludi record
033600980331      *
033700980331     C     V7CSCE        IFEQ      'E'
033800980331     C     KCDS          CHAIN     FNCDS000                           30
033900980331      *
034000980331     C     *IN30         DOWEQ     '0'
034100980331     C                   EXSR      VERRCD
034200980331     C   32              Z-ADD     *ZEROS        CDSNDC
034300980331     C   32              EXCEPT    AGGCDS
034400980331     C     KCDS          READE     FNCDS000                               30
034500980331     C                   END
034600980331      *
034700980331     C                   END
034800980331      *
034900980331      * Leggo
035000980331     C                   ADD       1             NRR7
035100980331     C     NRR7          CHAIN     LR63S07                            31
035200980331     C                   END
035300980331      *
035400980331      * Imposto fine PGM e test di dettaglio segnacolli univoco per evento
035500980331     C                   MOVEL     'S'           WFINE
035600980331     C                   Z-ADD     1             D63DSE
035700980331      *
035800980401     C                   COMMIT
035900980401      *
036000980331     C                   ENDSR
036100980331      *---------------------------------------------------------------*
036200980331      *  Verifica record
036300980331      *---------------------------------------------------------------*
036400980331     C     VERRCD        BEGSR
036500980331      *
036600980401     C     CDSDCM        IFEQ      V7XDCM                                       data consegna
036700980401     C     CDSDLA        ANDEQ     V7XDLA                                       data lasciato avviso
036800980401     C     CDSDAG        ANDEQ     V7XGIA                                       data giacenza
036900980401     C     CDSDRC        ANDEQ     V7XDRC                                       data riserva
037000980331     C     CDSSTS        ANDEQ     V7CSTS                                       stato
037100980331     C     CDSCOD        ANDEQ     V7CCOD                                       codice
037200980331     C                   SETON                                        32
037300980331     C                   ELSE
037400980331     C                   SETOFF                                       32
037500980331     C                   END
037600980331      *
037700980331     C                   ENDSR
037800980330      *--------------------------------------------------------------*
037900980330      *  SUBROUTINE INIZIALE
038000980330      *--------------------------------------------------------------*
038100980330     C     *INZSR        BEGSR
038200980325      *
038300980325      * Definisco chiave di accesso
038400980325     C     KCDS          KLIST
038500980325     C                   KFLD                    KNDC
038600980325     C                   KFLD                    KAAS
038700980325     C                   KFLD                    KLNP
038800980325     C                   KFLD                    KNRS
038900980325     C                   KFLD                    KNSP
039000980325      *
039100980325      * Definisco variabili
039200980325     C     *LIKE         DEFINE    CDSNDC        KNDC
039300980325     C     *LIKE         DEFINE    CDSAAS        KAAS
039400980325     C     *LIKE         DEFINE    CDSLNP        KLNP
039500980325     C     *LIKE         DEFINE    CDSNRS        KNRS
039600980325     C     *LIKE         DEFINE    CDSNSP        KNSP
039700980325      *
039800980330     C                   MOVEL     'N'           WFINE             1
039900980331     C                   MOVEL     '6'           WTPVID            1
040000980330      *
040100980325     C                   ENDSR
040200980330      *--------------------------------------------------------------*
040300980330      *  O U T P U T
040400980330      *--------------------------------------------------------------*
040500980330     OFNCDS000  E            AGGCDS
040600980330     O                       CDSNDC
040700980330      *--------------------------------------------------------------*
040800980330**
040900980330Selezioni non congruenti. Controlla.

000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200980410      *--------------------------------------------------------------*
000300980410      *         - GESTIONE DISTINTA ARRIVI ESTERO -                  *
000400980410      *                -- DISTINTA ERRORI --                         *
000500980410      *--------------------------------------------------------------*
000600980410     FFNLRK1D   CF   E             WORKSTN
000700980410     F                                     SFILE(LRK1S03:NRR3)
000800980410     F                                     SFILE(LRK1S05:NRR5)
000900980410     FFNCDA04L  UF   E           K DISK
001000950315     F                                     COMMIT
001100980415     FFNARB01L  IF   E           K DISK
001200980415     F                                     COMMIT
001300980410     FFNCDA03L  UF   E           K DISK
001400980325     F                                     COMMIT
001500980410     F                                     RENAME(FNCDA000:FNCDA003)
001600950320      *----------------------------------------------------------------*
001700980325     D ERR             S             70    DIM(7) CTDATA PERRCD(1)              ERRORI
001800980410      *
001900950315      * DS per richiamo pgm FNLR36R
002000950315     D DSLR36          DS
002100950315     D  P36AAS                14     17  0
002200950315     D  P36LNP                18     20  0
002300950315     D  P36NRS                21     22  0
002400950315     D  P36NSP                23     29  0
002500950315     D  P36F03                30     30
002600950315     D  P36FLG                31     31
002700980410     D FNLRK1        E DS                  EXTNAME(FNLRK1DS)
002800020913     D Ddatiute      E DS
002900980410      *
003000950315     D KPJBA         E DS
003100980410      *
003200950315     D                SDS
003300950315     D  NOMPGM                 1     10
003400950315      *--------------------------------------------------------------*
003500980410     IFNCDA003
003600980410     I              CDATRC                      CD$TRC
003700980410     I              CDADAM                      CD$DAM
003800980410     I              CDAHRA                      CD$HRA
003900980410     I              CDACMC                      CD$CMC
004000980410     I              CDANCL                      CD$NCL
004100980410     I              CDACCA                      CD$CCA
004200980410     I              CDANO1                      CD$NO1
004300980410     I              CDANOT                      CD$NOT
004400980410     I              CDAERR                      CD$ERR
004500980410     I              CDASTS                      CD$STS
004600980410     I              CDACOD                      CD$COD
004700980410     I              CDAMOT                      CD$MOT
004800980410     I              CDAFTX                      CD$FTX
004900980410     I              CDAFL1                      CD$FL1
005000980410     I              CDAFL2                      CD$FL2
005100980410     I              CDAFL3                      CD$FL3
005200980410     I              CDANDC                      CD$NDC
005300950315      *---------------------------------------------------------------*
005400950317     C     *ENTRY        PLIST
005500950317     C                   PARM                    KPJBA
005600980410      *
005700980410     C                   MOVEL     KPJBU         FNLRK1
005800980410     C                   Z-ADD     DK1LNA        KLNA
005900980410      *
006000980410     C     DK1TRI        IFNE      'U'
006100980410      *  Caricamento subfile errori da subfile hidden
006200950317     C                   EXSR      INZS03
006300950315      *  Loop gestione subfile degli errori
006400950315     C     WFINE         DOUEQ     'S'
006500980410      *  Gestione subfile errori
006600950317     C     WTPVID        CASEQ     '3'           GESS03
006700950315     C                   END
006800950317     C                   END
006900980410      *
007000950315     C     FINE          TAG
007100980424     C                   Z-ADD     TSTFRZ        DK1NRR
007200980410     C                   MOVEL     FNLRK1        KPJBU
007300950315     C                   SETON                                        RT
007400980403     C                   Z-ADD     *ZEROS        TSTFRZ
007500950320     C                   ELSE
007600980410      *
007700950320     C                   COMMIT
007800950320     C                   SETON                                        LR
007900950320     C                   END
008000950317      *---------------------------------------------------------------*
008100950317      * Gestione videata subfile errori                               *
008200950317      *---------------------------------------------------------------*
008300950317     C     INZS03        BEGSR
008400950317      *
008500950320     C                   SETOFF                                       202128
008600980410     C                   WRITE     LRK1C03
008700950317     C                   SETON                                        2021
008800980410      *
008900980410     C                   MOVEL     DK1DLA        V3DLNA
009000980410     C                   Z-ADD     DK1LNA        V3CLNA
009100980410     C                   Z-ADD     DK1NDT        V3CNDT
009200950317     C                   Z-ADD     1             NRR5              4 0
009300950317     C                   Z-ADD     0             NRR3              4 0
009400970304     C                   MOVEL     'S'           WNOREC            1
009500950320     C                   Z-ADD     1             NR3KEY
009600950321     C                   MOVEL     'N'           WFINE
009700980324      *
009800980410     C     NRR5          CHAIN     LRK1S05                            31
009900980324      *
010000950317     C     *IN31         DOWEQ     '0'
010100980410     C                   Z-ADD     DK1NDT        KNDC
010200950606     C                   Z-ADD     V5CAAS        KAAS
010300950606     C                   Z-ADD     V5CLNP        KLNP
010400950606     C                   Z-ADD     V5CNSP        KNSP
010500950606     C                   Z-ADD     V5CNRS        KNRS
010600980410      *
010700980324      *  se spedizione = vecchia e *IN03 on prendo ex scelta
010800961122     C     WSVAAS        IFEQ      V5CAAS
010900961122     C     WSVLNP        ANDEQ     V5CLNP
011000961122     C     WSVNRS        ANDEQ     V5CNRS
011100961122     C     WSVNSP        ANDEQ     V5CNSP
011200961122     C     V5CIN3        ANDEQ     '1'
011300961122     C                   MOVEL     WSVSCE        V5CSCE
011400961122     C                   ELSE
011500980410      *  salvo vecchia scelta e vecchio numero spedizione
011600961122     C                   MOVEL     V5CSCE        WSVSCE            1
011700961122     C                   Z-ADD     V5CAAS        WSVAAS            4 0
011800961122     C                   Z-ADD     V5CLNP        WSVLNP            3 0
011900961122     C                   Z-ADD     V5CNRS        WSVNRS            2 0
012000961122     C                   Z-ADD     V5CNSP        WSVNSP            7 0
012100961122     C                   END
012200980410      *
012300961120     C                   SETOFF                                       405022
012400961121     C                   SETOFF                                       41
012500961120     C                   MOVEL     *BLANKS       V3CSCE
012600980325      *
012700980325      *  Spedizione con note EDI
012800961120     C     V5CFTX        IFEQ      'S'
012900961211     C                   SETON                                        415022
013000961120     C                   END
013100980331      *
013200980410      *  Gestione dettaglio
013300980331      *
013400980410     C     V5CSCE        IFEQ      'D'
013500980331      *
013600980410     C                   Z-ADD     V5CAAS        DK1AAS
013700980410     C                   Z-ADD     V5CLNP        DK1LNP
013800980410     C                   Z-ADD     V5CNSP        DK1NSP
013900980410     C                   Z-ADD     V5CNRS        DK1NRS
014000980417     C                   Z-ADD     V5CDEV        DK1DSE
014100980415      *
014200980415     C     KARB          CHAIN     FNARB000                           01
014300980424     C                   Z-ADD     *ZEROS        DK1NCL
014400980424     C  N01              Z-ADD     ARBNCL        DK1NCL
014500980401      *
014600980410     C                   MOVEL     FNLRK1        KPJBU
014700980410     C                   CALL      'FNLRK1R5'
014800980331     C                   PARM                    KPJBA
014900980410     C                   MOVEL     KPJBU         FNLRK1
015000980331      *
015100980410      *  Se ho elaborato il dettaglio eventi determino la scelta sulla testata.
015200980410      *  se non ci sono rcd di FNCDA dettaglio ==> A
015300980331      *  se ci sono rcd con distinta uguale a quella in elaborazione ==> F
015400980331      *  se non ci sono rcd con distinta uguale a quella in elaborazione ==> E
015500980331      *
015600980331     C                   MOVEL     *BLANKS       V5CSCE
015700980417     C     V5CDEV        IFNE      DK1DSE
015800980331      *
015900980410     C     KCDAD         CHAIN     FNCDA003                           32
016000980410     C     *IN32         IFEQ      '1'
016100980331     C                   MOVEL     'A'           V5CSCE
016200980410     C                   ELSE
016300980331      *
016400980410     C     *IN32         DOWEQ     '0'
016500980331     C                   MOVEL     'E'           V5CSCE
016600980410     C     DK1NDT        IFEQ      CD$NDC
016700980331     C                   MOVEL     'F'           V5CSCE
016800980331     C                   SETON                                        32
016900980403     C                   EXCEPT    NOAGG
017000980410     C                   END
017100980410     C  N32KCDAD         READE     FNCDA003                               32
017200980410     C                   END
017300980331      *
017400980331     C                   END
017500980401     C                   END
017600980331      *
017700980422      * Aggiorno subfile
017800980417     C                   Z-ADD     DK1DSE        V5CDEV
017900980410     C                   UPDATE    LRK1S05
018000980331     C                   END
018100980325      *
018200980325      *  Annulla record
018300980325      *
018400950606     C     V5CSCE        IFEQ      'A'
018500980325      *
018600980410     C     KCDA          DELETE    FNCDA000                           31
018700980331     C     *IN31         DOWEQ     '0'
018800980410     C     KCDAD         DELETE    FNCDA003                           31
018900980325     C                   END
019000980331     C                   SETOFF                                       31
019100980410      *  Se l'errore era forzabile sottraggo 1 al numero delle bolle in distinta
019200961121     C     V5CBLO        IFNE      'S'
019300980410     C                   SUB       1             DK1NRR
019400961211     C                   END
019500950606     C                   GOTO      NOWRT
019600950606     C                   END
019700980325      *
019800980325      *  Escludi record
019900980325      *
020000961120     C     V5CSCE        IFEQ      'E'
020100980410     C     KCDA          CHAIN     FNCDA000                           31
020200980325      *
020300980325     C     *IN31         IFEQ      '0'
020400980410     C                   Z-ADD     CDANDC        §NDC
020500980410     C                   Z-ADD     *ZEROS        CDANDC
020600980410     C                   EXCEPT    AGGCDA
020700980325      *
020800980410     C     KCDAD         CHAIN     FNCDA003                           32
020900980331     C     *IN32         DOWEQ     '0'
021000980410     C     CD$NDC        IFEQ      §NDC
021100980410     C                   Z-ADD     0             CD$NDC
021200980410     C                   EXCEPT    AGGCD$
021300980325     C                   END
021400980410     C     KCDAD         READE     FNCDA003                               32
021500980325     C                   END
021600980325     C                   END
021700980325      *
021800980410      *  Se l'errore era forzabile sottraggo 1 al numero delle bolle in distinta
021900961211     C     V5CBLO        IFNE      'S'
022000980410     C                   SUB       1             DK1NRR
022100961120     C                   END
022200961120     C                   GOTO      NOWRT
022300961120     C                   END
022400980325      *
022500980325      *  Forzo immissione in distinta
022600980325      *
022700961120     C     V5CSCE        IFEQ      'F'
022800980403     C                   Z-ADD     1             TSTFRZ            1 0
022900961120     C                   GOTO      NOWRT
023000961120     C                   END
023100980325      *
023200980325      *  Riscrivo subfile errori da subfile nascosto
023300950606     C                   MOVEL     *BLANKS       V3CTPD
023400961122     C                   MOVEL     V5CSCE        V3CSCE
023500961122     C                   MOVEL     V5CIN3        V3CIN3
023600961122     C                   MOVEL     V5CIN3        *IN03
023700950317     C                   MOVEL     V5CERR        V3CERR
023800980417     C                   Z-ADD     V5CDEV        V3CDEV
023900980325     C                   Z-ADD     V5CAAS        V3CAAS
024000950317     C                   Z-ADD     V5CLNP        V3CLNP
024100950317     C                   Z-ADD     V5CNRS        V3CNRS
024200950317     C                   Z-ADD     V5CNSP        V3CNSP
024300980410     C                   Z-ADD     V5CDAM        V3CDAM
024400950317     C                   Z-ADD     V5CRMN        V3CRMN
024500961120     C                   MOVEL     V5CSTS        V3CSTS
024600961120     C                   MOVEL     V5CCOD        V3CCOD
024700961120     C                   MOVEL     V5DCOD        V3DCOD
024800961120     C                   MOVEL     V5CBLO        V3CBLO
024900961120     C                   MOVEL     V5CFTX        V3CFTX
025000950317     C                   Z-ADD     NRR5          V3CNR5
025100961120     C                   ADD       1             NRR3
025200970304     C                   MOVEL     'N'           WNOREC            1
025300980410     C                   WRITE     LRK1S03
025400980325      *
025500950606     C     NOWRT         TAG
025600950317     C                   ADD       1             NRR5
025700980410     C     NRR5          CHAIN     LRK1S05                            31
025800950317     C                   END
025900980325      *
026000980325      *  Se il subfile degli errori è vuoto esco da programma
026100950317     C     NRR3          IFEQ      0
026200980424      *?*        DK1NRR    ANDNE0
026300950317     C                   MOVEL     'S'           WFINE
026400950317     C                   END
026500980325      *
026600950317     C                   ENDSR
026700980401      *---------------------------------------------------------------*
026800980401      * Gestione videata subfile errori                               *
026900980401      *---------------------------------------------------------------*
027000980401     C     GESS03        BEGSR
027100980401      *
027200961120      * Controllo se devo inviare messaggio che non ci sono bolle
027300961120      * buone in distinta
027400950315     C                   SETON                                        2021
027500970304     C     WNOREC        IFEQ      'S'
027600970207     C                   SETOFF                                       20
027700970207     C                   END
027800980403     C     TSTFRZ        COMP      *ZEROS                                 02
027900980410     C   02DK1NRR        COMP      *ZEROS                                 02
028000970213     C   02              Z-ADD     1             NRR3
028100970214     C  N28              Z-ADD     1             NR3KEY
028200980325      *
028300980410     C                   WRITE     LRK1Z03
028400980410     C                   EXFMT     LRK1C03
028500980325      *
028600950320     C                   Z-ADD     1             NR3KEY
028700950320     C                   SETOFF                                       28
028800980325      *  Fine Lavoro
028900950315     C     *INKC         IFEQ      '1'
029000950315     C                   MOVEL     'S'           WFINE
029100980410     C                   MOVEL     '1'           DK1F03
029200950315     C                   GOTO      FINVD3
029300950315     C                   END
029400980325      *  Escludi tutte
029500961127     C   KA              EXSR      TUTTE
029600980325      *  Loop di controlli
029700950315     C                   EXSR      CTR03
029800980325      *  Se ci sono ancora dei record ricarico il subfile
029900950606     C  N28
030000950606     CAN KF              EXSR      INZS03
030100980325      *
030200950317     C     FINVD3        ENDSR
030300961127      *---------------------------------------------------------------*
030400961127      *  Escludi tutte                                                *
030500961127      *---------------------------------------------------------------*
030600961127     C     TUTTE         BEGSR
030700961127      *
030800961127     C                   MOVEL     'N'           WINTER            1
030900961127     C                   Z-ADD     1             NRR3
031000980410     C     NRR3          CHAIN     LRK1S03                            32
031100980325      *
031200961127     C     *IN32         DOWEQ     '0'
031300961127     C                   SETOFF                                       414050
031400980325      *
031500980410      *  Se esistono note EDI visualizzo il campo
031600980410     C     V3CFTX        IFEQ      'S'
031700961127     C                   SETON                                        50
031800961127     C                   END
031900980325      *
032000980325      *  Aggiorno subfile forzando 'E' nella scelta
032100961127     C                   SETON                                        22
032200961127     C                   MOVEL     V3CIN3        *IN03
032300961127     C                   MOVEL     'E'           V3CSCE
032400980410     C                   UPDATE    LRK1S03
032500961127     C                   SETOFF                                       22
032600980325      *
032700980325      *  Aggiorno subfile nascosto
032800980410     C     V3CNR5        CHAIN     LRK1S05                            31
032900961127     C                   MOVEL     V3CSCE        V5CSCE
033000961127     C                   MOVEL     V3CTPD        V5CTPD
033100980410     C  N31              UPDATE    LRK1S05
033200980325      *
033300980325      * Leggo RCD successivo
033400961127     C                   ADD       1             NRR3
033500980410     C     NRR3          CHAIN     LRK1S03                            32
033600961127     C                   END
033700980325      *
033800961127     C                   COMMIT
033900980325      *
034000961127     C                   ENDSR
034100950315      *---------------------------------------------------------------*
034200950315      *  Loop di errori  subfile errori                               *
034300950315      *---------------------------------------------------------------*
034400950315     C     CTR03         BEGSR
034500950315      *
034600950317     C                   MOVEL     'N'           WINTER            1
034700980410     C                   READC     LRK1S03                                32
034800980325      *
034900950317     C     *IN32         DOWEQ     '0'
035000950320     C     *IN28         ANDEQ     '0'
035100980325      *
035200961121     C                   SETOFF                                       414050
035300980410     C     V3CFTX        IFEQ      'S'
035400950606     C                   SETON                                        50
035500950606     C                   END
035600980325      *  Controllo se devo richiamare la visualizzazione delle note EDI
035700961120     C     V3CTPD        IFEQ      'E'
035800961120     C     V3CFTX        IFEQ      'S'
035900980410     C                   Z-ADD     V3CAAS        DK1AAS
036000980410     C                   Z-ADD     V3CLNP        DK1LNP
036100980410     C                   Z-ADD     V3CNSP        DK1NSP
036200980410     C                   Z-ADD     V3CNRS        DK1NRS
036300980410     C                   MOVEL     FNLRK1        KPJBU
036400980410     C                   CALL      'FNLRK1R4'
036500961120     C                   PARM                    KPJBA
036600961120     C                   MOVEL     *BLANKS       V3CTPD
036700980325      *
036800961120     C                   ELSE
036900961120     C                   SETON                                        4128
037000961120     C                   Z-ADD     NRR3          NR3KEY
037100961120     C                   MOVEL     ERR(4)        $MSG
037200961120     C                   END
037300961120     C                   END
037400980325      *
037500980325      *  Interrogazione bolle
037600950317     C     V3CSCE        IFEQ      'V'
037700950317     C                   MOVEL     'S'           WINTER
037800950317     C                   MOVEL     '1'           P36FLG
037900950317     C                   Z-ADD     V3CAAS        P36AAS
038000950317     C                   Z-ADD     V3CLNP        P36LNP
038100950317     C                   Z-ADD     V3CNSP        P36NSP
038200950317     C                   Z-ADD     V3CNRS        P36NRS
038300950317     C                   MOVEL     DSLR36        KPJBU
038400950317     C                   CALL      'FNLR36R'
038500950317     C                   PARM                    KPJBA
038600950606     C                   MOVEL     ' '           V3CSCE
038700980410      *
038800950317     C                   ELSE
038900980325      *
039000980325      *  Controllo se l'errore è forzabile oppure no !
039100961120     C     V3CBLO        IFEQ      'S'
039200961120     C     V3CSCE        ANDEQ     'F'
039300961120     C                   SETON                                        4028
039400961120     C                   Z-ADD     NRR3          NR3KEY
039500961120     C                   MOVEL     ERR(3)        $MSG
039600961120     C                   END
039700980325      *
039800980410      *  Se record con dettaglio da analizzare non posso forzare
039900980417     C     V3CDEV        IFGE      2
040000980325     C     V3CSCE        ANDEQ     'F'
040100980325     C                   SETON                                        4028
040200980325     C                   Z-ADD     NRR3          NR3KEY
040300980325     C                   MOVEL     ERR(3)        $MSG
040400980325     C                   END
040500980325      *
040600980410      *  Gestione dettaglio solo se esiste
040700980410     C     V3CSCE        IFEQ      'D'
040800980417     C     V3CDEV        ANDLT     2
040900980325     C                   SETON                                        4028
041000980325     C                   Z-ADD     NRR3          NR3KEY
041100980325     C                   MOVEL     ERR(7)        $MSG
041200980325     C                   END
041300980325      *
041400950320     C                   END
041500980325      *
041600980325      *  Aggiorno subfile
041700950320     C                   SETON                                        22
041800980325     C                   MOVEL     V3CIN3        *IN03
041900980410     C                   UPDATE    LRK1S03
042000950320     C                   SETOFF                                       22
042100980325      *
042200980325      *  Aggiorno subfile nascosto
042300980410     C     V3CNR5        CHAIN     LRK1S05                            31
042400961122     C                   MOVEL     V3CSCE        V5CSCE
042500950606     C                   MOVEL     V3CTPD        V5CTPD
042600980410     C  N31              UPDATE    LRK1S05
042700980325      *
042800980410     C  N28              READC     LRK1S03                                32
042900950317     C                   END
043000980325      *
043100950317     C                   COMMIT
043200980410      *
043300980410      *  Se richiesto interogazione bolle senza selezione richiamo il pgm.
043400950317     C     *INKG         IFEQ      '1'
043500950317     C     WINTER        ANDEQ     'N'
043600950317     C                   MOVEL     ' '           P36FLG
043700950317     C                   Z-ADD     V3CAAS        P36AAS
043800950317     C                   Z-ADD     V3CLNP        P36LNP
043900950317     C                   Z-ADD     V3CNSP        P36NSP
044000950317     C                   Z-ADD     V3CNRS        P36NRS
044100950317     C                   MOVEL     DSLR36        KPJBU
044200950317     C                   CALL      'FNLR36R'
044300950317     C                   PARM                    KPJBA
044400950317     C                   END
044500980410      *
044600950317     C                   ENDSR
044700980410      *-----------------------------------------------------*
044800980410      *  IMPOSTAZIONI INIZIALI                              *
044900980410      *-----------------------------------------------------*
045000950317     C     *INZSR        BEGSR
045100980325      *
045200980325      *  Definizione chiavi
045300980410     C     KCDA          KLIST
045400950317     C                   KFLD                    KNDC
045500950317     C                   KFLD                    KAAS
045600950317     C                   KFLD                    KLNP
045700950317     C                   KFLD                    KNRS
045800950317     C                   KFLD                    KNSP
045900980410      *
046000980410     C     KCDAD         KLIST
046100980410     C                   KFLD                    KLNA
046200980410     C                   KFLD                    KAAS
046300980410     C                   KFLD                    KLNP
046400980410     C                   KFLD                    KNRS
046500980410     C                   KFLD                    KNSP
046600980415      *
046700980415     C     KARB          KLIST
046800980415     C                   KFLD                    KAAS
046900980415     C                   KFLD                    KLNP
047000980415     C                   KFLD                    KNRS
047100980415     C                   KFLD                    KNSP
047200980325      *
047300980325      *  Definizione delle variabili
047400980410     C     *LIKE         DEFINE    CDAAAS        KAAS
047500980410     C     *LIKE         DEFINE    CDALNP        KLNP
047600980410     C     *LIKE         DEFINE    CDALNA        KLNA
047700980410     C     *LIKE         DEFINE    CDANRS        KNRS
047800980410     C     *LIKE         DEFINE    CDANSP        KNSP
047900980410     C     *LIKE         DEFINE    CDANDC        KNDC
048000980410     C     *LIKE         DEFINE    CDANDC        §NDC
048100980325      *
048200020913      *
048300020913      * Decodifica Elaboratore
048400020913      *  TIBS34R richiamato dal FNLRK1R ha impostato i dati
048500020913      *   che servono anche in questo programma.
048600020913     c     *Dtaara       Define    §datiute      DDatiUte
048700020913     c                   In        *Dtaara
048800020913      *
048900980325      *  Inizializzazione variabili
049000950317     C                   MOVEL     'N'           WFINE             1
049100950317     C                   MOVEL     '3'           WTPVID            1
049200980325      *
049300950317     C                   ENDSR
049400980325      *----------------------------------------------------*
049500980410     OFNCDA000  E            AGGCDA
049600980410     O                       CDANDC
049700980410     OFNCDA003  E            AGGCD$
049800980410     O                       CD$NDC
049900980410     OFNCDA003  E            NOAGG
050000980410      *----------------------------------------------------*
050100980410**
050200950320Opzione non valida
050300961121Immetti G=sped.giac./C=Conse./A=L.Avv.
050400961120Non è possibile forzare l'immissione della spedizione in distinta
050500961120Non ci sono note EDI da visualizzare
050600961121E' possibile visualizzare solo le note EDI
050700961212Obbligatorio immettere C/G/A
050800980410Non esiste dettaglio da visualizzare

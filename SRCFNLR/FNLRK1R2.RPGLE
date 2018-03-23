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
011300050506     C****               MOVEL     WSVSCE        V5CSCE
011400050506      ****  Serve per memorizzarsi di cancellare il record se eseguita
011500050506      ****   un operazione sul record precedente.
011600050506     C                   if        WSVSCE <> *blank
011700050506     C                   MOVEL     'X'           V5CSCE
011800050506     c                   end
011900050506      ***
012000961122     C                   ELSE
012100980410      *  salvo vecchia scelta e vecchio numero spedizione
012200961122     C                   MOVEL     V5CSCE        WSVSCE            1
012300961122     C                   Z-ADD     V5CAAS        WSVAAS            4 0
012400961122     C                   Z-ADD     V5CLNP        WSVLNP            3 0
012500961122     C                   Z-ADD     V5CNRS        WSVNRS            2 0
012600961122     C                   Z-ADD     V5CNSP        WSVNSP            7 0
012700961122     C                   END
012800980410      *
012900961120     C                   SETOFF                                       405022
013000961121     C                   SETOFF                                       41
013100961120     C                   MOVEL     *BLANKS       V3CSCE
013200980325      *
013300980325      *  Spedizione con note EDI
013400961120     C     V5CFTX        IFEQ      'S'
013500961211     C                   SETON                                        415022
013600961120     C                   END
013700980331      *
013800980410      *  Gestione dettaglio
013900980331      *
014000980410     C     V5CSCE        IFEQ      'D'
014100980331      *
014200980410     C                   Z-ADD     V5CAAS        DK1AAS
014300980410     C                   Z-ADD     V5CLNP        DK1LNP
014400980410     C                   Z-ADD     V5CNSP        DK1NSP
014500980410     C                   Z-ADD     V5CNRS        DK1NRS
014600980417     C                   Z-ADD     V5CDEV        DK1DSE
014700980415      *
014800980415     C     KARB          CHAIN     FNARB000                           01
014900980424     C                   Z-ADD     *ZEROS        DK1NCL
015000980424     C  N01              Z-ADD     ARBNCL        DK1NCL
015100980401      *
015200980410     C                   MOVEL     FNLRK1        KPJBU
015300980410     C                   CALL      'FNLRK1R5'
015400980331     C                   PARM                    KPJBA
015500980410     C                   MOVEL     KPJBU         FNLRK1
015600980331      *
015700980410      *  Se ho elaborato il dettaglio eventi determino la scelta sulla testata.
015800980410      *  se non ci sono rcd di FNCDA dettaglio ==> A
015900980331      *  se ci sono rcd con distinta uguale a quella in elaborazione ==> F
016000980331      *  se non ci sono rcd con distinta uguale a quella in elaborazione ==> E
016100980331      *
016200980331     C                   MOVEL     *BLANKS       V5CSCE
016300980417     C     V5CDEV        IFNE      DK1DSE
016400980331      *
016500980410     C     KCDAD         CHAIN     FNCDA003                           32
016600980410     C     *IN32         IFEQ      '1'
016700980331     C                   MOVEL     'A'           V5CSCE
016800980410     C                   ELSE
016900980331      *
017000980410     C     *IN32         DOWEQ     '0'
017100980331     C                   MOVEL     'E'           V5CSCE
017200980410     C     DK1NDT        IFEQ      CD$NDC
017300980331     C                   MOVEL     'F'           V5CSCE
017400980331     C                   SETON                                        32
017500980403     C                   EXCEPT    NOAGG
017600980410     C                   END
017700980410     C  N32KCDAD         READE     FNCDA003                               32
017800980410     C                   END
017900980331      *
018000980331     C                   END
018100980401     C                   END
018200980331      *
018300980422      * Aggiorno subfile
018400980417     C                   Z-ADD     DK1DSE        V5CDEV
018500980410     C                   UPDATE    LRK1S05
018600980331     C                   END
018700980325      *
018800980325      *  Annulla record
018900980325      *
019000950606     C     V5CSCE        IFEQ      'A'
019100980325      *
019200980410     C     KCDA          DELETE    FNCDA000                           31
019300980331     C     *IN31         DOWEQ     '0'
019400980410     C     KCDAD         DELETE    FNCDA003                           31
019500980325     C                   END
019600980331     C                   SETOFF                                       31
019700980410      *  Se l'errore era forzabile sottraggo 1 al numero delle bolle in distinta
019800961121     C     V5CBLO        IFNE      'S'
019900980410     C                   SUB       1             DK1NRR
020000961211     C                   END
020100950606     C                   GOTO      NOWRT
020200950606     C                   END
020300980325      *
020400980325      *  Escludi record
020500980325      *
020600961120     C     V5CSCE        IFEQ      'E'
020700980410     C     KCDA          CHAIN     FNCDA000                           31
020800980325      *
020900980325     C     *IN31         IFEQ      '0'
021000980410     C                   Z-ADD     CDANDC        §NDC
021100980410     C                   Z-ADD     *ZEROS        CDANDC
021200980410     C                   EXCEPT    AGGCDA
021300980325      *
021400980410     C     KCDAD         CHAIN     FNCDA003                           32
021500980331     C     *IN32         DOWEQ     '0'
021600980410     C     CD$NDC        IFEQ      §NDC
021700980410     C                   Z-ADD     0             CD$NDC
021800980410     C                   EXCEPT    AGGCD$
021900980325     C                   END
022000980410     C     KCDAD         READE     FNCDA003                               32
022100980325     C                   END
022200980325     C                   END
022300980325      *
022400980410      *  Se l'errore era forzabile sottraggo 1 al numero delle bolle in distinta
022500961211     C     V5CBLO        IFNE      'S'
022600980410     C                   SUB       1             DK1NRR
022700961120     C                   END
022800961120     C                   GOTO      NOWRT
022900961120     C                   END
023000980325      *
023100980325      *  Forzo immissione in distinta
023200980325      *
023300961120     C     V5CSCE        IFEQ      'F'
023400980403     C                   Z-ADD     1             TSTFRZ            1 0
023500961120     C                   GOTO      NOWRT
023600961120     C                   END
023700980325      *
023800980325      *  Riscrivo subfile errori da subfile nascosto
023900050506     c                   if        V5CSCE <> 'X'
024000950606     C                   MOVEL     *BLANKS       V3CTPD
024100961122     C                   MOVEL     V5CSCE        V3CSCE
024200961122     C                   MOVEL     V5CIN3        V3CIN3
024300961122     C                   MOVEL     V5CIN3        *IN03
024400950317     C                   MOVEL     V5CERR        V3CERR
024500980417     C                   Z-ADD     V5CDEV        V3CDEV
024600980325     C                   Z-ADD     V5CAAS        V3CAAS
024700950317     C                   Z-ADD     V5CLNP        V3CLNP
024800950317     C                   Z-ADD     V5CNRS        V3CNRS
024900950317     C                   Z-ADD     V5CNSP        V3CNSP
025000980410     C                   Z-ADD     V5CDAM        V3CDAM
025100950317     C                   Z-ADD     V5CRMN        V3CRMN
025200961120     C                   MOVEL     V5CSTS        V3CSTS
025300961120     C                   MOVEL     V5CCOD        V3CCOD
025400961120     C                   MOVEL     V5DCOD        V3DCOD
025500961120     C                   MOVEL     V5CBLO        V3CBLO
025600961120     C                   MOVEL     V5CFTX        V3CFTX
025700950317     C                   Z-ADD     NRR5          V3CNR5
025800961120     C                   ADD       1             NRR3
025900970304     C                   MOVEL     'N'           WNOREC            1
026000980410     C                   WRITE     LRK1S03
026100050506     c                   end
026200980325      *
026300950606     C     NOWRT         TAG
026400950317     C                   ADD       1             NRR5
026500980410     C     NRR5          CHAIN     LRK1S05                            31
026600950317     C                   END
026700980325      *
026800980325      *  Se il subfile degli errori è vuoto esco da programma
026900950317     C     NRR3          IFEQ      0
027000980424      *?*        DK1NRR    ANDNE0
027100950317     C                   MOVEL     'S'           WFINE
027200950317     C                   END
027300980325      *
027400950317     C                   ENDSR
027500980401      *---------------------------------------------------------------*
027600980401      * Gestione videata subfile errori                               *
027700980401      *---------------------------------------------------------------*
027800980401     C     GESS03        BEGSR
027900980401      *
028000961120      * Controllo se devo inviare messaggio che non ci sono bolle
028100961120      * buone in distinta
028200950315     C                   SETON                                        2021
028300970304     C     WNOREC        IFEQ      'S'
028400970207     C                   SETOFF                                       20
028500970207     C                   END
028600980403     C     TSTFRZ        COMP      *ZEROS                                 02
028700980410     C   02DK1NRR        COMP      *ZEROS                                 02
028800970213     C   02              Z-ADD     1             NRR3
028900970214     C  N28              Z-ADD     1             NR3KEY
029000980325      *
029100980410     C                   WRITE     LRK1Z03
029200980410     C                   EXFMT     LRK1C03
029300980325      *
029400950320     C                   Z-ADD     1             NR3KEY
029500950320     C                   SETOFF                                       28
029600980325      *  Fine Lavoro
029700950315     C     *INKC         IFEQ      '1'
029800950315     C                   MOVEL     'S'           WFINE
029900980410     C                   MOVEL     '1'           DK1F03
030000950315     C                   GOTO      FINVD3
030100950315     C                   END
030200980325      *  Escludi tutte
030300961127     C   KA              EXSR      TUTTE
030400980325      *  Loop di controlli
030500950315     C                   EXSR      CTR03
030600980325      *  Se ci sono ancora dei record ricarico il subfile
030700950606     C  N28
030800950606     CAN KF              EXSR      INZS03
030900980325      *
031000950317     C     FINVD3        ENDSR
031100961127      *---------------------------------------------------------------*
031200961127      *  Escludi tutte                                                *
031300961127      *---------------------------------------------------------------*
031400961127     C     TUTTE         BEGSR
031500961127      *
031600961127     C                   MOVEL     'N'           WINTER            1
031700961127     C                   Z-ADD     1             NRR3
031800980410     C     NRR3          CHAIN     LRK1S03                            32
031900980325      *
032000961127     C     *IN32         DOWEQ     '0'
032100961127     C                   SETOFF                                       414050
032200980325      *
032300980410      *  Se esistono note EDI visualizzo il campo
032400980410     C     V3CFTX        IFEQ      'S'
032500961127     C                   SETON                                        50
032600961127     C                   END
032700980325      *
032800980325      *  Aggiorno subfile forzando 'E' nella scelta
032900961127     C                   SETON                                        22
033000961127     C                   MOVEL     V3CIN3        *IN03
033100961127     C                   MOVEL     'E'           V3CSCE
033200980410     C                   UPDATE    LRK1S03
033300961127     C                   SETOFF                                       22
033400980325      *
033500980325      *  Aggiorno subfile nascosto
033600980410     C     V3CNR5        CHAIN     LRK1S05                            31
033700961127     C                   MOVEL     V3CSCE        V5CSCE
033800961127     C                   MOVEL     V3CTPD        V5CTPD
033900980410     C  N31              UPDATE    LRK1S05
034000980325      *
034100980325      * Leggo RCD successivo
034200961127     C                   ADD       1             NRR3
034300980410     C     NRR3          CHAIN     LRK1S03                            32
034400961127     C                   END
034500980325      *
034600961127     C                   COMMIT
034700980325      *
034800961127     C                   ENDSR
034900950315      *---------------------------------------------------------------*
035000950315      *  Loop di errori  subfile errori                               *
035100950315      *---------------------------------------------------------------*
035200950315     C     CTR03         BEGSR
035300950315      *
035400950317     C                   MOVEL     'N'           WINTER            1
035500980410     C                   READC     LRK1S03                                32
035600980325      *
035700950317     C     *IN32         DOWEQ     '0'
035800950320     C     *IN28         ANDEQ     '0'
035900980325      *
036000961121     C                   SETOFF                                       414050
036100980410     C     V3CFTX        IFEQ      'S'
036200950606     C                   SETON                                        50
036300950606     C                   END
036400980325      *  Controllo se devo richiamare la visualizzazione delle note EDI
036500961120     C     V3CTPD        IFEQ      'E'
036600961120     C     V3CFTX        IFEQ      'S'
036700980410     C                   Z-ADD     V3CAAS        DK1AAS
036800980410     C                   Z-ADD     V3CLNP        DK1LNP
036900980410     C                   Z-ADD     V3CNSP        DK1NSP
037000980410     C                   Z-ADD     V3CNRS        DK1NRS
037100980410     C                   MOVEL     FNLRK1        KPJBU
037200980410     C                   CALL      'FNLRK1R4'
037300961120     C                   PARM                    KPJBA
037400961120     C                   MOVEL     *BLANKS       V3CTPD
037500980325      *
037600961120     C                   ELSE
037700961120     C                   SETON                                        4128
037800961120     C                   Z-ADD     NRR3          NR3KEY
037900961120     C                   MOVEL     ERR(4)        $MSG
038000961120     C                   END
038100961120     C                   END
038200980325      *
038300980325      *  Interrogazione bolle
038400950317     C     V3CSCE        IFEQ      'V'
038500950317     C                   MOVEL     'S'           WINTER
038600950317     C                   MOVEL     '1'           P36FLG
038700950317     C                   Z-ADD     V3CAAS        P36AAS
038800950317     C                   Z-ADD     V3CLNP        P36LNP
038900950317     C                   Z-ADD     V3CNSP        P36NSP
039000950317     C                   Z-ADD     V3CNRS        P36NRS
039100950317     C                   MOVEL     DSLR36        KPJBU
039200950317     C                   CALL      'FNLR36R'
039300950317     C                   PARM                    KPJBA
039400950606     C                   MOVEL     ' '           V3CSCE
039500980410      *
039600950317     C                   ELSE
039700980325      *
039800980325      *  Controllo se l'errore è forzabile oppure no !
039900961120     C     V3CBLO        IFEQ      'S'
040000961120     C     V3CSCE        ANDEQ     'F'
040100961120     C                   SETON                                        4028
040200961120     C                   Z-ADD     NRR3          NR3KEY
040300961120     C                   MOVEL     ERR(3)        $MSG
040400961120     C                   END
040500980325      *
040600980410      *  Se record con dettaglio da analizzare non posso forzare
040700980417     C     V3CDEV        IFGE      2
040800980325     C     V3CSCE        ANDEQ     'F'
040900980325     C                   SETON                                        4028
041000980325     C                   Z-ADD     NRR3          NR3KEY
041100980325     C                   MOVEL     ERR(3)        $MSG
041200980325     C                   END
041300980325      *
041400980410      *  Gestione dettaglio solo se esiste
041500980410     C     V3CSCE        IFEQ      'D'
041600980417     C     V3CDEV        ANDLT     2
041700980325     C                   SETON                                        4028
041800980325     C                   Z-ADD     NRR3          NR3KEY
041900980325     C                   MOVEL     ERR(7)        $MSG
042000980325     C                   END
042100980325      *
042200950320     C                   END
042300980325      *
042400980325      *  Aggiorno subfile
042500950320     C                   SETON                                        22
042600980325     C                   MOVEL     V3CIN3        *IN03
042700980410     C                   UPDATE    LRK1S03
042800950320     C                   SETOFF                                       22
042900980325      *
043000980325      *  Aggiorno subfile nascosto
043100980410     C     V3CNR5        CHAIN     LRK1S05                            31
043200961122     C                   MOVEL     V3CSCE        V5CSCE
043300950606     C                   MOVEL     V3CTPD        V5CTPD
043400980410     C  N31              UPDATE    LRK1S05
043500980325      *
043600980410     C  N28              READC     LRK1S03                                32
043700950317     C                   END
043800980325      *
043900950317     C                   COMMIT
044000980410      *
044100980410      *  Se richiesto interogazione bolle senza selezione richiamo il pgm.
044200950317     C     *INKG         IFEQ      '1'
044300950317     C     WINTER        ANDEQ     'N'
044400950317     C                   MOVEL     ' '           P36FLG
044500950317     C                   Z-ADD     V3CAAS        P36AAS
044600950317     C                   Z-ADD     V3CLNP        P36LNP
044700950317     C                   Z-ADD     V3CNSP        P36NSP
044800950317     C                   Z-ADD     V3CNRS        P36NRS
044900950317     C                   MOVEL     DSLR36        KPJBU
045000950317     C                   CALL      'FNLR36R'
045100950317     C                   PARM                    KPJBA
045200950317     C                   END
045300980410      *
045400950317     C                   ENDSR
045500980410      *-----------------------------------------------------*
045600980410      *  IMPOSTAZIONI INIZIALI                              *
045700980410      *-----------------------------------------------------*
045800950317     C     *INZSR        BEGSR
045900980325      *
046000980325      *  Definizione chiavi
046100980410     C     KCDA          KLIST
046200950317     C                   KFLD                    KNDC
046300950317     C                   KFLD                    KAAS
046400950317     C                   KFLD                    KLNP
046500950317     C                   KFLD                    KNRS
046600950317     C                   KFLD                    KNSP
046700980410      *
046800980410     C     KCDAD         KLIST
046900980410     C                   KFLD                    KLNA
047000980410     C                   KFLD                    KAAS
047100980410     C                   KFLD                    KLNP
047200980410     C                   KFLD                    KNRS
047300980410     C                   KFLD                    KNSP
047400980415      *
047500980415     C     KARB          KLIST
047600980415     C                   KFLD                    KAAS
047700980415     C                   KFLD                    KLNP
047800980415     C                   KFLD                    KNRS
047900980415     C                   KFLD                    KNSP
048000980325      *
048100980325      *  Definizione delle variabili
048200980410     C     *LIKE         DEFINE    CDAAAS        KAAS
048300980410     C     *LIKE         DEFINE    CDALNP        KLNP
048400980410     C     *LIKE         DEFINE    CDALNA        KLNA
048500980410     C     *LIKE         DEFINE    CDANRS        KNRS
048600980410     C     *LIKE         DEFINE    CDANSP        KNSP
048700980410     C     *LIKE         DEFINE    CDANDC        KNDC
048800980410     C     *LIKE         DEFINE    CDANDC        §NDC
048900980325      *
049000020913      *
049100020913      * Decodifica Elaboratore
049200020913      *  TIBS34R richiamato dal FNLRK1R ha impostato i dati
049300020913      *   che servono anche in questo programma.
049400020913     c     *Dtaara       Define    §datiute      DDatiUte
049500020913     c                   In        *Dtaara
049600020913      *
049700980325      *  Inizializzazione variabili
049800950317     C                   MOVEL     'N'           WFINE             1
049900950317     C                   MOVEL     '3'           WTPVID            1
050000980325      *
050100950317     C                   ENDSR
050200980325      *----------------------------------------------------*
050300980410     OFNCDA000  E            AGGCDA
050400980410     O                       CDANDC
050500980410     OFNCDA003  E            AGGCD$
050600980410     O                       CD$NDC
050700980410     OFNCDA003  E            NOAGG
050800980410      *----------------------------------------------------*
050900980410**
051000950320Opzione non valida
051100961121Immetti G=sped.giac./C=Conse./A=L.Avv.
051200961120Non è possibile forzare l'immissione della spedizione in distinta
051300961120Non ci sono note EDI da visualizzare
051400961121E' possibile visualizzare solo le note EDI
051500961212Obbligatorio immettere C/G/A
051600980410Non esiste dettaglio da visualizzare

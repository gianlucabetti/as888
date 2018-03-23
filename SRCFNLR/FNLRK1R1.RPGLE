000100950313     H DECEDIT('0,') DATEDIT(*DMY.)
000200980409      *--------------------------------------------------------------*
000300980409      *         - GESTIONE DISTINTA ARRIVI ESTERO -                  *
000400980409      *          -- DATI DISTINTA + CARICAMENTO --                   *
000500950313      *--------------------------------------------------------------*
000600980409     FFNLRK1D   CF   E             WORKSTN
000700980409     F                                     SFILE(LRK1S05:NRR5)
000800980409     FFNCDA01L  UF   E           K DISK    USROPN
000900950313     F                                     COMMIT
001000980409     FFNCDA03L  UF   E           K DISK    USROPN
001100980409     F                                     COMMIT
001200980409     F                                     RENAME(FNCDA000:FNCDA003)
001300980409     FFNARB01L  IF   E           K DISK    USROPN
001400950313     F                                     COMMIT
001500980428      *?*FNGCA01LIF  E           K        DISK
001600980423     FFNCDAW1L  IF   E           K DISK
001700051017     FFIARS01L  UF   E           K DISK
001800051017     FFIARS10L  UF   E           K DISK
001900051017     F                                     RENAME(FIARS000:FIARS010)
002000980423     FFNART01L  IF   E           K DISK
002100990809     FFNEVB00F  O    E             DISK
002200980422     FEDTAB01L  IF   E           K DISK    USROPN
002300980424     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002400980424     F                                     USROPN
002500980424     FSYSPRT    O    F  132        PRINTER OFLIND(*INOA)
002600980424     F                                     USROPN
002700950313      *----------------------------------------------------------------*
002800980415     D ERR             S             70    DIM(11) CTDATA PERRCD(1)             ERRORI
002900980422      * Tabella codici eventi
003000980422     D CEV             S              6    DIM(200)
003100980422     D DEV             S             90    DIM(200)
003200980409      *
003300950313     D KPJBA         E DS
003400950313     D  LIBSYS                92    100
003500950313     D  MSGERR               453    502
003600950313      *
003700020913     D Ddatiute      E DS
003800980422     D DSCE          E DS                  EXTNAME(EDIDSCE)
003900980422     D FNLRK1        E DS                  EXTNAME(FNLRK1DS)
004000950315      *
004100980415      *  DS per inversione data
004200950313     D WLBDA8          DS
004300950313     D  G02DAT                 1      8  0
004400950313     D  G02INV                 9     16  0
004500950313     D  G02ERR                17     17
004600950313     D  G02TGI                18     22  0
004700980414      *
004800950315     D                SDS
004900950315     D  NOMPGM                 1     10
005000950313      *----------------------------------------------------------------
005100980409     IFNCDA003
005200980409     I              CDATRC                      CD$TRC
005300980409     I              CDADAM                      CD$DAM
005400980409     I              CDAHRA                      CD$HRA
005500980409     I              CDACMC                      CD$CMC
005600980409     I              CDANCL                      CD$NCL
005700980409     I              CDACCA                      CD$CCA
005800980409     I              CDANO1                      CD$NO1
005900980409     I              CDANOT                      CD$NOT
006000980409     I              CDAERR                      CD$ERR
006100980409     I              CDASTS                      CD$STS
006200980409     I              CDACOD                      CD$COD
006300980409     I              CDAMOT                      CD$MOT
006400980409     I              CDAFTX                      CD$FTX
006500980409     I              CDAFL1                      CD$FL1
006600980409     I              CDAFL2                      CD$FL2
006700980409     I              CDAFL3                      CD$FL3
006800980409     I              CDANDC                      CD$NDC
006900950313      *---------------------------------------------------------------*
007000950313      *  Operazioni iniziali
007100950313     C     *ENTRY        PLIST
007200950313     C                   PARM                    KPJBA
007300980409      *
007400980409     C                   MOVEL     KPJBU         FNLRK1
007500980409     C                   MOVEL     ' '           WFINE
007600980409      *
007700950313      *  Se pgm richiamato per l'ultima volta chiudo in LR
007800980409     C     DK1TRI        IFNE      'U'
007900980409      *
008000950313      *  Impostazione campi distinta
008100950313     C                   EXSR      INZD02
008200980409      *
008300950313      *  Loop gestione videate
008400950313     C     WFINE         DOUEQ     'S'
008500950313     C     WTPVID        CASEQ     '2'           GESD02
008600950313     C                   END
008700950313     C                   END
008800980409      *
008900950313     C     FINE          TAG
009000980409     C                   MOVEL     FNLRK1        KPJBU
009100950313     C                   SETON                                        RT
009200980409      *
009300950313     C                   ELSE
009400980409      *
009500950320     C     WCHIUS        IFEQ      'S'
009600980421     C                   EXSR      CHIUDI
009700950320     C                   END
009800980414      *
009900950320     C     WSBLOC        IFEQ      'S'
010000980409     C                   MOVEL     FNLRK1        KPJBU
010100980409     C                   CALL      'FNLRK1R3'
010200950320     C                   PARM                    KPJBA
010300950320     C                   END
010400980414      *
010500950426     C     WPGMER        IFEQ      'S'
010600980409     C                   MOVEL     FNLRK1        KPJBU
010700980409     C                   CALL      'FNLRK1R2'
010800950320     C                   PARM                    KPJBA
010900980424      *  Se sono uscito con F3 richiamo il pgm per sbloccare le distinte
011000980424     C     DK1F03        IFEQ      '1'
011100980424     C                   MOVEL     FNLRK1        KPJBU
011200980424     C                   CALL      'FNLRK1R3'
011300980424     C                   PARM                    KPJBA
011400980424     C                   MOVEL     KPJBU         FNLRK1
011500980424     C                   MOVEL     'S'           WFINE
011600980424     C                   MOVEL     'S'           WSBLOC
011700980424     C                   END
011800950320     C                   END
011900980409      *
012000950320     C                   COMMIT
012100950313     C                   SETON                                        LR
012200950313     C                   END
012300950313      *---------------------------------------------------------*
012400950313      *  Inizializzo campi della prima videata                  *
012500950313      *---------------------------------------------------------*
012600950313     C     INZD02        BEGSR
012700980415      *
012800950313     C                   SETOFF                                       404142
012900950313     C                   SETOFF                                       2801
013000980414      *
013100980409     C                   Z-ADD     DK1LNA        V2CLNA
013200980409     C                   MOVEL     DK1NDT        V2CNDC
013300980414     C                   Z-ADD     DK1LNA        KLNA
013400980414     C                   Z-ADD     0             KNDC
013500980414      *
013600980409      * Controllo se esistono record di FNCDA con il numero distinta impostato
013700980409     C     KCDA          SETGT     FNCDA01L
013800980409     C     V2CLNA        READE     FNCDA01L                               31
013900950313     C     *IN31         IFEQ      '0'
014000950320     C                   EXCEPT    NOAGG
014100950313     C                   SETON                                            01
014200950313     C                   END
014300980409      *
014400950313     C                   ENDSR
014500950313      *---------------------------------------------------------*
014600950313      *  Gestione della seconda videata                         *
014700950313      *---------------------------------------------------------*
014800950313     C     GESD02        BEGSR
014900950313      *
015000980409     C                   EXFMT     LRK1D02
015100950320     C                   SETOFF                                       404142
015200950320     C                   SETOFF                                       28
015300980409      *  Fine Lavoro
015400950313     C     *INKC         IFEQ      '1'
015500980409     C                   MOVEL     '1'           DK1F03
015600950313     C                   MOVEL     'S'           WFINE
015700950313     C                   GOTO      FINVD2
015800950313     C                   END
015900980409      *  Ritorno
016000950313     C     *INKL         IFEQ      '1'
016100950320     C                   MOVEL     'S'           WFINE
016200950320     C                   GOTO      FINVD2
016300950313     C                   END
016400980409      *  Sblocco distinta
016500950313     C     *INKK         IFEQ      '1'
016600980409     C                   MOVEL     FNLRK1        KPJBU
016700980409     C                   CALL      'FNLRK1R3'
016800950313     C                   PARM                    KPJBA
016900980409     C                   MOVEL     KPJBU         FNLRK1
017000950320     C                   MOVEL     'S'           WSBLOC
017100950317     C                   SETOFF                                       01
017200950313     C                   GOTO      FINVD2
017300950313     C                   END
017400980409      *
017500950313     C  N01*IN28         IFEQ      '0'
017600980415      *
017700980415      *  Esamino record di FNCDA ed impostare il numero distinta
017800980424     C                   OPEN      QSYSPRT
017900980424     C                   SETON                                        OF
018000980424     C                   EXSR      UPDCDA
018100980424     C                   CLOSE     QSYSPRT
018200980409      *
018300980415      *  Se ho trovato degli errori chiamo pgm x la gestione
018400950313     C     WERROR        IFEQ      'S'
018500950313     C                   EXSR      GESERR
018600950313     C                   END
018700980415      *
018800980421      *  Chiudo distinta
018900950315     C     WFINE         IFNE      'S'
019000980424     C     DK1NRR        IFGT      *ZEROS
019100980424     C     TSTFRZ        ORGT      *ZEROS
019200980421     C                   EXSR      CHIUDI
019300980415     C                   ELSE
019400980415     C                   MOVEL     ERR(11)       $MSG
019500980415     C                   SETON                                        28
019600980415     C                   END
019700950313     C                   END
019800980409      *
019900950315     C                   END
020000980409      *
020100950315     C     FINVD2        ENDSR
020200980409      *-----------------------------------------------------*
020300980409      *  Aggiorno archivio FNCDA con il numero distinta
020400980409      *-----------------------------------------------------*
020500980409     C     UPDCDA        BEGSR
020600980323      *
020700980409     C                   MOVEL     'N'           WERROR            1
020800980409     C                   Z-ADD     0             DK1NRR
020900950322     C                   Z-ADD     0             KNDC
021000011031     C******  PORTATO ALL'INTERON DEL CICLO DI LETTURA
021100011031     C******               Z-ADD*ZEROS    WDSE    10
021200980423     C                   MOVEL     *BLANKS       PSTS
021300980423     C                   MOVEL     *BLANKS       PCOD
021400980423     C                   Z-ADD     *ZEROS        PDAM
021500980423     C                   Z-ADD     *ZEROS        PHRA
021600980424     C                   Z-ADD     *ZEROS        PNCL
021700980424     C                   MOVEL     *BLANKS       PCMC
021800980424     C                   MOVEL     *BLANKS       PSEE
021900980424     C                   MOVEL     *BLANKS       PDEC             30
022000980423     C                   CLEAR                   DSCE
022100980409      *
022200980409     C     KCDA          CHAIN     FNCDA01L                           31
022300950317     C     *IN31         DOWEQ     '0'
022400980409      *
022500961220     C                   MOVEL     'S'           WERRBL
022600980409     C                   Z-ADD     *ZEROS        WNRERR            5 0
022700011031     C**** AZZERA AD OGNI LETTURA IL WDSE
022800011031     C                   Z-ADD     *ZEROS        WDSE              1 0
022900980409     C                   Z-ADD     CDAAAS        KAAS
023000980409     C                   Z-ADD     CDALNP        KLNP
023100980409     C                   Z-ADD     CDANRS        KNRS
023200980409     C                   Z-ADD     CDANSP        KNSP
023300980409      *
023400980424      *  Se key sped. = 0 (Eccedenza) evito i controlli sulla spedizione
023500980409     C     CDANSP        IFEQ      *ZEROS
023600980409     C                   GOTO      NOCTL
023700980409     C                   END
023800980409      *  Bolla non trovata su FNARB
023900950314     C     KARB          CHAIN     FNARB01L                           32
024000950314     C     *IN32         IFEQ      '1'
024100980409     C                   Z-ADD     1             I
024200950314     C                   EXSR      WRTERR
024300980409     C                   GOTO      FINCTL
024400950314     C                   END
024500980323      *  Linea di arrivo archivio P.C. diversa da bolle arrivi
024600980409     C     ARBLNA        IFNE      CDALNA
024700980409     C                   Z-ADD     2             I
024800950314     C                   EXSR      WRTERR
024900980409     C                   GOTO      FINCTL
025000950314     C                   END
025100980323      *  Rif.numerico mittente diverso da bolle arrivi
025200980409     C     ARBRMN        IFNE      CDARMN
025300980409     C                   Z-ADD     3             I
025400950314     C                   EXSR      WRTERR
025500980409     C                   GOTO      FINCTL
025600950314     C                   END
025700980323      *  Errore bloccante
025800980409     C     CDAFL1        IFEQ      'B'
025900980409     C                   Z-ADD     10            I
026000961129     C                   EXSR      WRTERR
026100980409     C                   GOTO      FINCTL
026200961129     C                   END
026300980409      *  Spedizione già consegnata
026400980428      *?*        ARBDCM    IFNE 0
026500980428      *?*                  Z-ADD4         I
026600980428      *?*                  MOVEL'N'       WERRBL
026700980428      *?*                  EXSR WRTERR
026800980428      *?*                  END
026900980409      *  Spedizione in giacenza
027000980428      *?*        KARB      CHAINFNGCA01L             33
027100980428      *?*        *IN33     IFEQ '0'
027200980428      *?*                  MOVEL'N'       WERRBL
027300980428      *?*                  Z-ADD7         I
027400980428      *?*                  EXSR WRTERR
027500980428      *?*                  END
027600980323      *  Motivo di consegna non gestibile
027700980409     C     CDAERR        IFEQ      'S'
027800980409     C                   Z-ADD     8             I
027900961118     C                   MOVEL     'N'           WERRBL
028000961118     C                   EXSR      WRTERR
028100961118     C                   END
028200980409      *
028300980409     C     NOCTL         TAG
028400980409      *
028500980409      *  - Se data a zero e non esiste dettaglio errore bloccante
028600980409      *  - Se data a zero ed esiste dettaglio scrivo messaggio di
028700980417      *     analizzare dettaglio  V5CDEV = 2
028800980402      *
028900980409     C     CDADAM        IFEQ      *ZEROS
029000980323      *
029100980409     C                   Z-ADD     CDALNA        KLNA
029200980409     C                   Z-ADD     CDAAAS        KAAS
029300980409     C                   Z-ADD     CDALNP        KLNP
029400980409     C                   Z-ADD     CDANRS        KNRS
029500980409     C                   Z-ADD     CDANSP        KNSP
029600050505      *
029700050506     c     KCDAD         CHAIN     FNCDA003                           33
029800980324      *
029900980324     C     *IN33         IFEQ      '1'
030000980409     C                   Z-ADD     5             I
030100980324     C                   ELSE
030200980415     C                   Z-ADD     6             I
030300980409     C                   Z-ADD     2             WDSE
030400980409     C                   END
030500980324      *
030600970107     C                   MOVEL     'N'           WERRBL
030700961120     C                   EXSR      WRTERR
030800961120     C                   END
030900980406      *  Se 2° evento mancanza scrivo messaggio
031000980409     C     CDAFL1        IFEQ      'S'
031100980409     C                   Z-ADD     10            I                 4 0
031200961120     C                   MOVEL     'N'           WERRBL            1
031300961120     C                   EXSR      WRTERR
031400961120     C                   END
031500980323      *
031600980414     C                   ADD       1             DK1NRR
031700980409     C     FINCTL        TAG
031800980424      *
031900980424      *  Aggiorno test eventi validi per distinta
032000980424     C     I             IFEQ      *ZEROS
032100980424     C                   Z-ADD     1             TSTFRZ            1 0
032200980424     C                   END
032300980323      *
032400980409      *  Aggiorno FNCDA
032500980415     C                   Z-ADD     V2CNDC        CDANDC
032600980409     C                   EXCEPT    AGGCDA
032700980423      *
032800980424      *  Se spedizione = 0 (eccedenza) azzero numero colli
032900980424     C     CDAAAS        IFEQ      *ZEROS
033000980424     C                   Z-ADD     *ZEROS        ARBNCL
033100980424     C                   END
033200980424      *
033300980424      *  Stampa solo se la spedizione entra in predistinta
033400980424     C     I             IFNE      *ZEROS
033500980423     C   OF              EXCEPT    TEST1
033600980424     C                   EXCEPT    DATSP1
033700980423      *  se CDACMC <> blanks stampo dati evento
033800980423     C     CDACMC        IFNE      *BLANKS
033900980424     C                   SETOFF                                       34
034000980423     C                   MOVEL     CDASTS        PSTS
034100980423     C                   MOVEL     CDACOD        PCOD
034200980423     C                   Z-ADD     CDADAM        PDAM
034300980423     C                   Z-ADD     CDAHRA        PHRA
034400980424     C                   Z-ADD     CDANCL        PNCL
034500980423     C                   MOVEL     CDACMC        PCMC
034600980423     C                   EXSR      DECEVE
034700980423     C                   MOVEL     CDANOT        PSEE
034800980424     C                   MOVEL     §CEDEC        PDEC
034900980424     C   OF              EXCEPT    TEST1
035000980424     C                   EXCEPT    DATEV1
035100980423     C                   END
035200980424     C                   END
035300980323      *
035400980428     C                   MOVEL     *BLANKS       PSTS
035500980428     C                   MOVEL     *BLANKS       PCOD
035600980428     C                   Z-ADD     *ZEROS        PDAM
035700980428     C                   Z-ADD     *ZEROS        PHRA
035800980428     C                   MOVEL     *BLANKS       PCMC
035900980428     C                   MOVEL     *BLANKS       PSEE
036000980428      *
036100980428      *  Aggiorno il dettaglio di FNCDA se esiste
036200980409     C                   Z-ADD     CDALNA        KLNA
036300980409     C                   Z-ADD     CDAAAS        KAAS
036400980409     C                   Z-ADD     CDALNP        KLNP
036500980409     C                   Z-ADD     CDANRS        KNRS
036600980409     C                   Z-ADD     CDANSP        KNSP
036700980409     C     KCDAD         SETLL     FNCDA003                               33
036800980428     C   33              DO        *HIVAL
036900980409     C     KCDAD         READE     FNCDA003                               33
037000980423     C  N33              Z-ADD     V2CNDC        CD$NDC
037100980409     C  N33              EXCEPT    AGCDA3
037200980424      * Se esiste dettaglio l'evento entra in predistinta quindi stampo
037300980424     C  N33              EXSR      PRTEVE
037400980323     C  N33              END
037500980323      *
037600980409      *  Leggo prossimo record di FNCDA con numero distinta a 0
037700980424     C                   Z-ADD     *ZEROS        I
037800980409     C     KCDA          READE     FNCDA01L                               31
037900950315     C                   END
038000980409      *
038100950317     C                   COMMIT
038200980409      *
038300950314     C                   ENDSR
038400980409      *-----------------------------------------------------*
038500980423      *  Scrittura eventi
038600980409      *-----------------------------------------------------*
038700980423     C     PRTEVE        BEGSR
038800980409      *
038900980423     C     CD$STS        IFEQ      PSTS
039000980423     C     CD$COD        ANDEQ     PCOD
039100980423     C     CD$DAM        ANDEQ     PDAM
039200980423     C     CD$HRA        ANDEQ     PHRA
039300980423     C     CD$CMC        ANDEQ     PCMC
039400980428     C     CD$NOT        ANDNE     PSEE
039500980423     C                   SETON                                        34
039600980424     C                   MOVEL     CD$NOT        PSEE
039700980423     C                   ELSE
039800980424     C                   SETOFF                                       34
039900980423     C                   MOVEL     CD$STS        PSTS
040000980423     C                   MOVEL     CD$COD        PCOD
040100980423     C                   Z-ADD     CD$DAM        PDAM
040200980423     C                   Z-ADD     CD$HRA        PHRA
040300980424     C                   Z-ADD     CD$NCL        PNCL
040400980423     C                   MOVEL     CD$CMC        PCMC
040500980423     C                   EXSR      DECEV$
040600980424     C                   MOVEL     §CEDEC        PDEC
040700980423     C                   MOVEL     CD$NOT        PSEE
040800980424     C                   END
040900980424     C   OF              EXCEPT    TEST1
041000980424     C                   EXCEPT    DATEV1
041100980423      *
041200980423     C                   ENDSR
041300980423      *-----------------------------------------------------*
041400980423      *  Scrittura degli errori
041500980423      *-----------------------------------------------------*
041600980423     C     WRTERR        BEGSR
041700980423      *
041800980409      *  Se è la prima volta azzero subfile degli errori
041900950314     C     WERROR        IFEQ      'N'
042000980323     C                   Z-ADD     0             NRR5              4 0
042100950314     C                   MOVEL     'S'           WERROR
042200950315     C                   SETOFF                                       2021
042300980409     C                   WRITE     LRK1C05
042400950314     C                   SETON                                        2021
042500010209     C                   ELSE
042600010209      *  PER OVRFLOW SFL
042700010209     C     NRR5          CABGE     9999          ENDERR
042800010209     C                   END
042900980409      *
043000980409      *  Aggiorno numeratore errori
043100961122     C                   ADD       1             WNRERR
043200980409     C                   ADD       1             NRR5
043300980409      *
043400961122     C     WNRERR        IFGT      1
043500961122     C                   MOVEL     '1'           V5CIN3
043600961122     C                   ELSE
043700961122     C                   MOVEL     '0'           V5CIN3
043800961122     C                   END
043900980409      *
044000961122     C                   MOVEL     *BLANKS       V5CSCE
044100950606     C                   MOVEL     *BLANKS       V5CTPD
044200980417     C                   MOVEL     WDSE          V5CDEV
044300961118     C                   MOVEL     WERRBL        V5CBLO
044400980409     C                   Z-ADD     CDAAAS        V5CAAS
044500980409     C                   Z-ADD     CDALNP        V5CLNP
044600980409     C                   Z-ADD     CDANRS        V5CNRS
044700980409     C                   Z-ADD     CDANSP        V5CNSP
044800980409     C                   Z-ADD     CDARMN        V5CRMN
044900980409     C                   MOVEL     CDASTS        V5CSTS
045000980409     C                   MOVEL     CDACOD        V5CCOD
045100980409     C                   MOVEL     CDAMOT        V5DCOD
045200980409     C                   MOVEL     CDAFTX        V5CFTX
045300980409     C                   MOVEL     CDAFL1        V5CFL1
045400950320     C                   CLEAR                   WLBDA8
045500980409     C                   Z-ADD     CDADAM        G02INV
045600950320     C                   MOVEL     '3'           G02ERR
045700950320     C                   CALL      'XSRDA8'
045800950320     C                   PARM                    WLBDA8
045900980409     C                   Z-ADD     G02DAT        V5CDAM
046000980409     C     I             IFEQ      8
046100961125     C                   MOVEL     *BLANKS       V5CERR
046200980409     C                   MOVEL     CDAMOT        V5CERR
046300961125     C                   ELSE
046400950317     C                   MOVEL     ERR(I)        V5CERR
046500961125     C                   END
046600980409      *
046700980409     C                   WRITE     LRK1S05
046800980409      *
046900010209     C     ENDERR        ENDSR
047000980409      *-----------------------------------------------------*
047100980409      *  Gestione degli errori
047200980409      *-----------------------------------------------------*
047300950315     C     GESERR        BEGSR
047400980409      *
047500980409      *  Richiamo pgm subfile gestione errori
047600980409     C                   MOVEL     FNLRK1        KPJBU
047700980409     C                   CALL      'FNLRK1R2'
047800950315     C                   PARM                    KPJBA
047900980409     C                   MOVEL     KPJBU         FNLRK1
048000950426     C                   MOVEL     'S'           WPGMER
048100980415      *
048200980415      *  Se sono uscito con F3 richiamo il pgm per sbloccare le distinte
048300980409     C     DK1F03        IFEQ      '1'
048400980409     C                   MOVEL     FNLRK1        KPJBU
048500980409     C                   CALL      'FNLRK1R3'
048600950315     C                   PARM                    KPJBA
048700980409     C                   MOVEL     KPJBU         FNLRK1
048800950315     C                   MOVEL     'S'           WFINE
048900950320     C                   MOVEL     'S'           WSBLOC
049000950315     C                   END
049100980323      *
049200950315     C                   ENDSR
049300950315      *---------------------------------------------------------*
049400980423      *  CHIUSURA DISTINTA                                      *
049500950315      *---------------------------------------------------------*
049600980421     C     CHIUDI        BEGSR
049700980409      *
049800980416     C                   MOVEL     *BLANKS       V10PRT
049900980416     C                   MOVEL     'N'           FINW10            1
050000980415      *
050100980416     C     FINW10        DOWNE     'S'
050200980416     C                   EXFMT     LRK1W10
050300980415     C     *INKC         IFEQ      '1'
050400980415     C     *INKF         OREQ      '1'
050500980416     C                   MOVEL     'S'           FINW10
050600980415     C                   END
050700980415     C                   END
050800980424      *
050900980424      *  Esco con F6: aggiorno segnacolli, scrivo eventi e stampo a richiesta
051000980424     C     *INKF         IFEQ      '1'
051100980424     C                   OPEN      SYSPRT
051200980424     C                   SETON                                        OA
051300980424     C                   EXSR      CONFER
051400980424     C                   CLOSE     SYSPRT
051500980424     C                   END
051600980421      *
051700980424      *  Ogni uscita: sblocco la distinta e termino
051800980421     C                   MOVEL     FNLRK1        KPJBU
051900980421     C                   CALL      'FNLRK1R3'
052000980421     C                   PARM                    KPJBA
052100980421     C                   MOVEL     KPJBU         FNLRK1
052200980421     C                   MOVEL     'S'           WFINE
052300980422     C                   MOVEL     'S'           WSBLOC
052400980409      *
052500950317     C                   COMMIT
052600980415      *
052700950315     C                   ENDSR
052800980409      *-----------------------------------------------------*
052900980421      *  Aggiorno file segnacolli e scrivo eventi
053000980409      *-----------------------------------------------------*
053100950322     C     CONFER        BEGSR
053200980424      *
053300980422     C                   MOVEL     'S'           WFINE
053400980323      *
053500980409     C                   Z-ADD     DK1NDT        KNDC
053600980409     C     KCDA          CHAIN     FNCDA01L                           31
053700980323      *
053800090806    1C     *IN31         DOWEQ     '0'
053900980422      *
054000090806    2C     CDACMC        IFNE      *BLANKS                                      TESTATA
054100980424      *
054200980423     C                   EXSR      DECEVE
054300980423     C                   EXSR      WRTEVE
054400090806    3C     §CETEA        IFNE      'M'
054500980423     C                   EXSR      WRTDTA
054600090806    3C                   END
054700980424      *
054800980424     C                   EXSR      SUBPR1
054900980424     C                   DELETE    FNCDA000
055000980422      *
055100090806   x2C                   ELSE                                                   DETTAGLIO
055200980422      *
055300980427     C                   MOVEL     *BLANKS       WPRTSP            1
055400980427     C                   Z-ADD     *ZEROS        TSTDET            2 0
055500980424     C                   Z-ADD     CDALNA        KLNA
055600980422     C                   Z-ADD     CDAAAS        KAAS
055700980422     C                   Z-ADD     CDALNP        KLNP
055800980422     C                   Z-ADD     CDANRS        KNRS
055900980422     C                   Z-ADD     CDANSP        KNSP
056000980423     C     KCDAD         CHAIN     FNCDA003                           32
056100980422      *
056200090806    3C     *IN32         DOWEQ     '0'
056300980427      *
056400980423     C                   EXSR      DECEV$
056500980422      *
056600090806    4C     CD$NDC        IFNE      DK1NDT
056700980424     C                   ADD       1             TSTDET
056800980424      *
056900090806   x4C                   ELSE
057000980424      *
057100090806    5C     CD$FL3        IFEQ      *BLANKS
057200980423     C                   EXSR      WRTEV$
057300980422      *
057400090806    6C     §CETEA        IFNE      'M'
057500980423     C     CD$NOT        ANDNE     *BLANKS
057600980422     C     §CETEA        ORNE      'M'
057700980423     C     CD$NOT        ANDEQ     *BLANKS
057800980423     C     CD$CCA        ANDEQ     *BLANKS
057900980423     C                   EXSR      WRTDT$
058000090806    6C                   END
058100980422      *
058200090806   x5C                   ELSE
058300980423     C                   EXSR      WRTEV$
058400980422     C                   EXSR      WRTDTW
058500090806    5C                   END
058600980427      *
058700980427     C                   EXSR      SUBPR2
058800980427     C                   DELETE    FNCDA003
058900090806    4C                   END
059000980422      *
059100980423     C     KCDAD         READE     FNCDA003                               32
059200090806    3C                   END
059300980422      *
059400090806    3C     TSTDET        IFEQ      *ZEROS
059500980427     C                   DELETE    FNCDA000
059600090806    3C                   END
059700090806    2C                   END
059800980424      *
059900980422     C     KCDA          READE     FNCDA01L                               31
060000090806    1C                   END
060100980422      *
060200980422     C                   ENDSR
060300980422      *-------------------------------------------------------------*
060400980423      *  DECEVE - DECODIFICO EVENTO FNCDA01L
060500980422      *----------------------------------------------------------------
060600980423     C     DECEVE        BEGSR
060700980422      *
060800980423     C                   Z-ADD     1             JJ
060900980423     C                   MOVEL     CDASTS        WCEV
061000980423     C                   MOVE      CDACOD        WCEV              6
061100980423     C     WCEV          LOOKUP    CEV(JJ)                                32
061200980423     C   32              MOVEL     DEV(JJ)       DSCE
061300980423      *
061400980423     C                   ENDSR
061500980423      *-------------------------------------------------------------*
061600980423      *  WRTEVE - SCRIVO EVENTO
061700980423      *----------------------------------------------------------------
061800980423     C     WRTEVE        BEGSR
061900980423      *
062000980422      * Scrivo solo se non si tratta di evento aggiuntivo
062100980422     C     CDACCA        IFNE      '1'
062200980422     C     §CECAR        ANDEQ     'E'
062300980422      *
062400990809     C                   Z-ADD     DATA8G        EVBDTV
062500980422     C                   Z-ADD     WORA          EVBORV
062600020913     C*****              Z-ADD     DK1LNA        EVBFLE
062700020913     C                   Z-ADD     DUTPOU        EVBFLE
062800020913     C                   Z-ADD     DUTCOU        EVBCDU
062900980422     C                   Z-ADD     CDAAAS        EVBAAS
063000980422     C                   Z-ADD     CDALNP        EVBLNP
063100980422     C                   Z-ADD     CDANRS        EVBNRS
063200980422     C                   Z-ADD     CDANSP        EVBNSP
063300990809     C                   Z-ADD     CDADAM        EVBDEV
063400980422     C                   MOVEL     §CECEV        EVBCEV
063500980422      *
063600980422     C     CDAHRA        IFGT      *ZEROS
063700980422     C                   Z-ADD     CDAHRA        EVBHEV
063800980422     C                   ELSE
063900980422     C                   Z-ADD     1200          EVBHEV
064000980422     C                   END
064100980422      *
064200990809     C                   WRITE     FNEVB000
064300980422     C                   END
064400980422      *
064500980422     C                   ENDSR
064600980422      *-----------------------------------------------------*
064700980423      * AGGIORNO DATA ARRIVO SU DETTAGLIO SEGNACOLLI
064800980423      *-----------------------------------------------------*
064900980423     C     WRTDTA        BEGSR
065000980423      *
065100980423     C     CDANOT        IFNE      *BLANKS
065200980423      *
065300021029      *  Ora occorre controllare il TRC poichè la vista logica seleziona
065400021029      *  oltre al tipo "E"= Euroexpress and "F" segnacollo normale
065500021029      *   ovviamente qui interessa solo il tipo Euroexpress
065600051017     C     CDANOT        setll     FIARS010
065700051017     C     CDANOT        reade     FIARS010                               33
065800021029     C     *IN33         doweq     *Off
065900021029     c                   if        ArsTrc ='E'
066000021029     c                   leave
066100021029     c                   end
066200051017     C     CDANOT        reade     FIARS010                               33
066300021029     c                   enddo
066400021029      *
066500980423     C  N33ARSDAT        IFEQ      *ZEROS
066600980423     C                   Z-ADD     CDADAM        ARSDAT
066700980423     C     §CETEA        IFEQ      'D'
066800980423     C                   MOVEL     'D'           ARSCAN
066900980423     C                   END
067000980423     C                   EXCEPT    AGGAR1
067100090826     c                   else
067200090826     c                   unlock    FIARS10l
067300980423     C                   END
067400980423      *
067500980423     C                   ELSE
067600980423      *
067700980423     C                   Z-ADD     CDAAAS        KAAS
067800980423     C                   Z-ADD     CDALNP        KLNP
067900980423     C                   Z-ADD     CDANRS        KNRS
068000980423     C                   Z-ADD     CDANSP        KNSP
068100980423     C     KARB          CHAIN     FNART000                           33
068200980423      *
068300980423     C     *IN33         DOWEQ     '0'
068400980423      *
068500980423     C                   Z-ADD     ARTFLS        KFLS
068600980423     C                   Z-ADD     ARTLNA        KLNA
068700980423     C                   Z-ADD     ARTNRS        KNRS
068800980423     C                   Z-ADD     ARTNSC        KNSC
068900980423     C                   MOVEL     'E'           KTRC
069000051017     C     KARS          CHAIN     FIARS000                           34
069100021029      *
069200980423     C  N34ARSDAT        IFEQ      *ZEROS
069300980423     C                   Z-ADD     CDADAM        ARSDAT
069400980423     C     §CETEA        IFEQ      'D'
069500980423     C                   MOVEL     'D'           ARSCAN
069600980423     C                   END
069700980423     C                   EXCEPT    AGGARS
069800090826     c                   else
069900090826     c                   unlock    FIARS01l
070000980423     C                   END
070100980423      *
070200980424     C     KARB          READE     FNART000                               33
070300980423     C                   END
070400980423      *
070500980423     C                   END
070600980423      *
070700980423     C                   ENDSR
070800980423      *-------------------------------------------------------------*
070900980423      *  DECEV$ - DECODIFICO EVENTO FNCDA03L
071000980423      *----------------------------------------------------------------
071100980423     C     DECEV$        BEGSR
071200980423      *
071300980423     C                   Z-ADD     1             JJ
071400980423     C                   MOVEL     CD$STS        WCEV
071500980423     C                   MOVE      CD$COD        WCEV              6
071600980423     C     WCEV          LOOKUP    CEV(JJ)                                32
071700980423     C   32              MOVEL     DEV(JJ)       DSCE
071800980423      *
071900980423     C                   ENDSR
072000980423      *-------------------------------------------------------------*
072100980423      *  WRTEV$ - SCRIVO EVENTO DA FNCDA03L
072200980423      *----------------------------------------------------------------
072300980423     C     WRTEV$        BEGSR
072400980423      *
072500980423      * Scrivo solo se non si tratta di evento aggiuntivo
072600980423     C     CD$CCA        IFNE      '1'
072700980423     C     §CECAR        ANDEQ     'E'
072800980423      *
072900990809     C                   Z-ADD     DATA8G        EVBDTV
073000980423     C                   Z-ADD     WORA          EVBORV
073100020913     C*****              Z-ADD     DK1LNA        EVBFLE
073200020913     C                   Z-ADD     DUTPOU        EVBFLE
073300020913     C                   Z-ADD     DUTCOU        EVBCDU
073400980423     C                   Z-ADD     CDAAAS        EVBAAS
073500980423     C                   Z-ADD     CDALNP        EVBLNP
073600980423     C                   Z-ADD     CDANRS        EVBNRS
073700980423     C                   Z-ADD     CDANSP        EVBNSP
073800990809     C                   Z-ADD     CD$DAM        EVBDEV
073900980423     C                   MOVEL     §CECEV        EVBCEV
074000980423      *
074100980423     C     CD$HRA        IFGT      *ZEROS
074200980423     C                   Z-ADD     CD$HRA        EVBHEV
074300980423     C                   ELSE
074400980423     C                   Z-ADD     1200          EVBHEV
074500980423     C                   END
074600980423      *
074700990809     C                   WRITE     FNEVB000
074800980423     C                   END
074900980423      *
075000980423     C                   ENDSR
075100980423      *-----------------------------------------------------*
075200980423      * AGGIORNO DATA ARRIVO SU SEGNACOLLI DA FNCDA03L
075300980423      *-----------------------------------------------------*
075400980423     C     WRTDT$        BEGSR
075500980423      *
075600980423     C     CD$NOT        IFNE      *BLANKS
075700980423      *
075800021029      *  Ora occorre controllare il TRC poichè la vista logica seleziona
075900021029      *  oltre al tipo "E"= Euroexpress and "F" segnacollo normale
076000021029      *   ovviamente qui interessa solo il tipo Euroexpress
076100051017     C     CD$NOT        setll     FIARS010
076200051017     C     CD$NOT        reade     FIARS010                               33
076300021029     C     *IN33         doweq     *Off
076400021029     c                   if        ArsTrc ='E'
076500021029     c                   leave
076600021029     c                   end
076700051017     C     CD$NOT        reade     FIARS010                               33
076800021029     c                   enddo
076900021029      *
077000980423     C  N33ARSDAT        IFEQ      *ZEROS
077100980423     C                   Z-ADD     CD$DAM        ARSDAT
077200980423     C     §CETEA        IFEQ      'D'
077300980423     C                   MOVEL     'D'           ARSCAN
077400980423     C                   END
077500980423     C                   EXCEPT    AGGAR1
077600090826     c                   else
077700090826     c                   unlock    FIARS10l
077800980423     C                   END
077900980423      *
078000980423     C                   ELSE
078100980423      *
078200980423     C                   Z-ADD     CDAAAS        KAAS
078300980423     C                   Z-ADD     CDALNP        KLNP
078400980423     C                   Z-ADD     CDANRS        KNRS
078500980423     C                   Z-ADD     CDANSP        KNSP
078600980423     C     KARB          CHAIN     FNART000                           33
078700980423      *
078800980423     C     *IN33         DOWEQ     '0'
078900980423      *
079000980423     C                   Z-ADD     ARTFLS        KFLS
079100980423     C                   Z-ADD     ARTLNA        KLNA
079200980423     C                   Z-ADD     ARTNRS        KNRS
079300980423     C                   Z-ADD     ARTNSC        KNSC
079400980423     C                   MOVEL     'E'           KTRC
079500051017     C     KARS          CHAIN     FIARS000                           34
079600980423     C  N34ARSDAT        IFEQ      *ZEROS
079700980423     C                   Z-ADD     CD$DAM        ARSDAT
079800980423     C     §CETEA        IFEQ      'D'
079900980423     C                   MOVEL     'D'           ARSCAN
080000980423     C                   END
080100980423     C                   EXCEPT    AGGARS
080200090826     c                   else
080300090826     c                   unlock    FIARS01l
080400980423     C                   END
080500980423      *
080600980429     C     KARB          READE     FNART000                               33
080700980423     C                   END
080800980423      *
080900980423     C                   END
081000980423      *
081100980423     C                   ENDSR
081200980423      *----------------------------------------------------------*
081300980423      * AGGIORNO DATA ARRIVO SU DETTAGLIO SEGNACOLLI DA FNCDW
081400980423      *----------------------------------------------------------*
081500980423     C     WRTDTW        BEGSR
081600980423      *
081700980423     C                   Z-ADD     CDAAAS        KAAS
081800980423     C                   Z-ADD     CDALNP        KLNP
081900980423     C                   Z-ADD     CDANRS        KNRS
082000980423     C                   Z-ADD     CDANSP        KNSP
082100980423     C     KARB          CHAIN     FNCDW000                           33
082200980423      *
082300980423     C     *IN33         DOWEQ     '0'
082400980423      *
082500980423     C                   Z-ADD     CDWFLS        KFLS
082600980423     C                   Z-ADD     DK1LNA        KLNA
082700980423     C                   Z-ADD     CDWNRS        KNRS
082800980423     C                   Z-ADD     CDWNSC        KNSC
082900980423     C                   MOVEL     'E'           KTRC
083000051017     C     KARS          CHAIN     FIARS000                           34
083100980423     C  N34ARSDAT        IFEQ      *ZEROS
083200980423     C     CDWDAM        ANDGT     *ZEROS
083300980423     C                   Z-ADD     CDWDAM        ARSDAT
083400980423     C     CDWDAN        IFEQ      'D'
083500980423     C                   MOVEL     'D'           ARSCAN
083600980423     C                   END
083700980423     C                   EXCEPT    AGGARS
083800090826     c                   else
083900090826     c                   unlock    FIARS01l
084000980423     C                   END
084100980423      *
084200980423     C     KARB          READE     FNCDW000                               33
084300980423     C                   END
084400980423      *
084500980423     C                   ENDSR
084600980401      *-----------------------------------------------------*
084700980424      *  STAMPO EVENTO DISTINTA - SOLO TIPO RCD "T"
084800980401      *-----------------------------------------------------*
084900980424     C     SUBPR1        BEGSR
085000980401      *
085100980424      *  Aggancio FNARB per colli spedizione
085200980424     C                   Z-ADD     CDAAAS        KAAS
085300980424     C                   Z-ADD     CDALNP        KLNP
085400980424     C                   Z-ADD     CDANRS        KNRS
085500980424     C                   Z-ADD     CDANSP        KNSP
085600980424     C     KARB          CHAIN     FNARB01L                           32
085700980424      *
085800980424     C   OA              EXCEPT    TEST2
085900980424     C                   EXCEPT    DATSP2
086000980424      *
086100980424     C                   SETOFF                                       35
086200980424     C                   MOVEL     CDASTS        PSTS
086300980424     C                   MOVEL     CDACOD        PCOD
086400980424     C                   Z-ADD     CDADAM        PDAM
086500980424     C                   Z-ADD     CDAHRA        PHRA
086600980424     C                   Z-ADD     CDANCL        PNCL
086700980424     C                   MOVEL     CDACMC        PCMC
086800980424     C                   MOVEL     CDANOT        PSEE
086900980424     C                   MOVEL     §CEDEC        PDEC
087000980424      *
087100980424     C   OA              EXCEPT    TEST2
087200980424     C                   EXCEPT    DATEV2
087300980401      *
087400980401     C                   ENDSR
087500980424      *-----------------------------------------------------*
087600980424      *  STAMPO EVENTO DISTINTA - SOLO TIPO RCD "D"
087700980424      *-----------------------------------------------------*
087800980424     C     SUBPR2        BEGSR
087900980424      *
088000980424      *  Stampo dati bolla solo 1° giro
088100980424     C     WPRTSP        IFEQ      *BLANKS
088200980424     C                   MOVEL     'N'           WPRTSP
088300980424      *
088400980424      *  Aggancio FNARB per colli spedizione
088500980424     C                   Z-ADD     CDAAAS        KAAS
088600980424     C                   Z-ADD     CDALNP        KLNP
088700980424     C                   Z-ADD     CDANRS        KNRS
088800980424     C                   Z-ADD     CDANSP        KNSP
088900980424     C     KARB          CHAIN     FNARB01L                           32
089000980424      *
089100980424     C   OA              EXCEPT    TEST2
089200980424     C                   EXCEPT    DATSP2
089300980424     C                   END
089400980424      *
089500980424     C     CD$STS        IFEQ      PSTS
089600980424     C     CD$COD        ANDEQ     PCOD
089700980424     C     CD$DAM        ANDEQ     PDAM
089800980424     C     CD$HRA        ANDEQ     PHRA
089900980424     C     CD$CMC        ANDEQ     PCMC
090000980424     C                   SETON                                        35
090100980424     C                   MOVEL     CD$NOT        PSEE
090200980424     C                   ELSE
090300980424     C                   SETOFF                                       35
090400980424     C                   MOVEL     CD$STS        PSTS
090500980424     C                   MOVEL     CD$COD        PCOD
090600980424     C                   Z-ADD     CD$DAM        PDAM
090700980424     C                   Z-ADD     CD$HRA        PHRA
090800980424     C                   Z-ADD     CD$NCL        PNCL
090900980424     C                   MOVEL     CD$CMC        PCMC
091000980424     C                   MOVEL     §CEDEC        PDEC
091100980424     C                   MOVEL     CD$NOT        PSEE
091200980424     C                   END
091300980424      *
091400980424     C   OA              EXCEPT    TEST2
091500980424     C                   EXCEPT    DATEV2
091600980424      *
091700980424     C                   ENDSR
091800980409      *-----------------------------------------------------*
091900980409      *  Operazioni iniziali
092000980409      *-----------------------------------------------------*
092100980324     C     *INZSR        BEGSR
092200020913      *
092300020913      * Decodifica Elaboratore
092400020913      *  TIBS34R richiamato dal FNLRK1R ha impostato i dati
092500020913      *   che servono anche in questo programma.
092600020913     c     *Dtaara       Define    §datiute      DDatiUte
092700020913     c                   In        *Dtaara
092800980324      *
092900980324      * Apertura controllata dei files
093000980409     C                   OPEN      FNCDA01L                             16
093100950313     C  N16              OPEN      FNARB01L                             16
093200980409     C  N16              OPEN      FNCDA03L                             16
093300950313     C   16              GOTO      FINE                                         OPEN vado a fine
093400980324      *
093500980409      *  Definizione chiavi
093600950315     C     KARB          KLIST
093700950313     C                   KFLD                    KAAS
093800950313     C                   KFLD                    KLNP
093900950313     C                   KFLD                    KNRS
094000950313     C                   KFLD                    KNSP
094100980409      *
094200980409     C     KCDAD         KLIST
094300980409     C                   KFLD                    KLNA
094400980409     C                   KFLD                    KAAS
094500980409     C                   KFLD                    KLNP
094600980409     C                   KFLD                    KNRS
094700980409     C                   KFLD                    KNSP
094800980409      *
094900980409     C     KCDA          KLIST
095000950315     C                   KFLD                    KLNA
095100950315     C                   KFLD                    KNDC
095200980423      *
095300980423     C     KARS          KLIST
095400980423     C                   KFLD                    KFLS
095500980423     C                   KFLD                    KLNA
095600980423     C                   KFLD                    KNRS
095700980423     C                   KFLD                    KNSC
095800980423     C                   KFLD                    KTRC
095900980421      *
096000980410      *  Definizione delle variabili
096100950313     C     *LIKE         DEFINE    ARBAAS        KAAS
096200950313     C     *LIKE         DEFINE    ARBLNP        KLNP
096300950313     C     *LIKE         DEFINE    ARBNRS        KNRS
096400950313     C     *LIKE         DEFINE    ARBNSP        KNSP
096500980409     C     *LIKE         DEFINE    CDALNA        KLNA
096600980409     C     *LIKE         DEFINE    CDANDC        KNDC
096700980423     C     *LIKE         DEFINE    ARSFLS        KFLS
096800980423     C     *LIKE         DEFINE    ARSNSC        KNSC
096900980423     C     *LIKE         DEFINE    ARSTRC        KTRC
097000980424     C     *LIKE         DEFINE    CDACMC        PCMC
097100980424     C     *LIKE         DEFINE    CDACOD        PCOD
097200980424     C     *LIKE         DEFINE    CDADAM        PDAM
097300980424     C     *LIKE         DEFINE    CDAHRA        PHRA
097400980424     C     *LIKE         DEFINE    CDANCL        PNCL
097500980424     C     *LIKE         DEFINE    CDANOT        PSEE
097600980424     C     *LIKE         DEFINE    CDASTS        PSTS
097700980409      *
097800980409      * Giro data del giorno
097900950313     C                   TIME                    WHHDAT           14 0
098000980422     C                   TIME                    WHHD12           12 0
098100980422     C                   MOVE      WHHDAT        DATA8             8 0
098200980422     C                   MOVE      WHHD12        DATA6             6 0
098300980422     C                   MOVEL     DATA6         DATA              6 0
098400980422     C                   TIME                    WORA              6 0
098500980422      * Giro la data
098600980422     C                   CLEAR                   WLBDA8
098700990809     C                   Z-ADD     DATA8         G02DAT
098800980422     C                   MOVEL     *BLANKS       G02ERR
098900980422     C                   CALL      'XSRDA8'
099000980422     C                   PARM                    WLBDA8
099100990809     C                   Z-ADD     G02INV        DATA8G            8 0
099200980409      *
099300950313     C                   MOVEL     ' '           WFINE             1
099400950313     C                   MOVEL     '2'           WTPVID            1
099500950320     C                   MOVEL     'N'           WCHIUS            1
099600950426     C                   MOVEL     'N'           WPGMER            1
099700950320     C                   MOVEL     'N'           WSBLOC            1
099800980423     C                   MOVEL     *ALL'-'       TRAT            132
099900980409      *
100000980422      * Caricamento Tabella codici eventi
100100980422     C                   OPEN      EDTAB01L
100200980422     C                   Z-ADD     *ZEROS        JJ                3 0
100300980422     C                   MOVEL     'CE'          TABCOD
100400980422      *
100500980422     C     TABCOD        CHAIN     EDTAB01L                           30
100600980422      *
100700980422     C     *IN30         DOWEQ     '0'
100800980422     C     TABFLG        IFEQ      *BLANKS
100900980422     C                   ADD       1             JJ
101000980422     C                   MOVEL     TABKEY        CEV(JJ)
101100980422     C                   MOVEL     TABUNI        DEV(JJ)
101200980422     C                   MOVEL     DEV(JJ)       DSCE
101300980422     C                   END
101400980422     C     TABCOD        READE     EDTAB01L                               30
101500980422     C                   END
101600980422      *
101700980422     C                   CLOSE     EDTAB01L
101800980422      *
101900950313     C                   ENDSR
102000980409      *----------------------------------------------------*
102100980409     OFNCDA000  E            AGGCDA
102200980409     O                       CDANDC
102300980409     OFNCDA000  E            NOAGG
102400051017     OFIARS000  E            AGGARS
102500980423     O                       ARSDAT
102600980423     O                       ARSCAN
102700051017     OFIARS010  E            AGGAR1
102800980423     O                       ARSDAT
102900980423     O                       ARSCAN
103000980409     OFNCDA003  E            AGCDA3
103100980409     O                       CD$NDC
103200980323      *----------------------------------------------------*
103300980423     OQSYSPRT   E            TEST1            01
103400980423     O                       RSUT                20
103500980427     O                                           60 '** DISTINTA ARRIVI EXPOR'
103600980427     O                                         +  0 'T ** EVENTI ANOMALI **'
103700980423     O                       DATA6         Y    110
103800980424     O                       WORA               120 '  :  :  '
103900980423     O                                          127 'Pag.:'
104000980423     O                       PAGE1         Z    132
104100980424     O          E            TEST1            03
104200980423     O                                           18 'LINEA D''ARRIVO'
104300980423     O                       DK1LNA        4   +  1
104400980423     O                       DK1DLA            +  2
104500980424     O          E            TEST1       0  1
104600980423     O                                           18 'LINEA D''ARRIVO'
104700980423     O                       DK1LNA        4   +  1
104800980423     O                       DK1DLA            +  2
104900980423      *  Spedizione
105000980424     O          E            DATSP1      1  1
105100980423     O                       TRAT               132
105200980424     O          E            DATSP1      1
105300980423     O                                           14 'NR.SPEDIZIONE:'
105400980424     O                       CDAAAS        Z   +  1
105500980423     O                       CDALNP        Z   +  1
105600980423     O                       CDANRS        Z   +  1
105700980423     O                       CDANSP        Z   +  1
105800980424     O                                         +  6 'COLLI SPEDIZIONE:'
105900980424     O                       ARBNCL        Z   +  0
106000980423      *  Evento
106100980424     O          E            DATEV1      1
106200980424     O              N34                           6 'EVENTO'
106300980423     O              N34      PSTS              +  1
106400980423     O              N34      PCOD              +  1
106500980424     O              N34      PCMC              +  1
106600980424     O              N34      PDEC              +  1
106700980424     O              N34                        +  2 'DATA'
106800980423     O              N34      PDAM              +  1
106900980423     O              N34                        +  1 'ORA'
107000980423     O              N34      PHRA              +  1
107100980424     O              N34                        +  2 'COLLI EVENTO'
107200980424     O              N34      PNCL          4   +  0
107300980424     O                       PSEE               132
107400980424      *----------------------------------------------------*
107500980424     OSYSPRT    E            TEST2            01
107600980424     O                       RSUT                20
107700980427     O                                           60 '** DISTINTA ARRIVI EXPOR'
107800980427     O                                         +  0 'T ** EVENTI CONFERMATI *'
107900980427     O                                         +  0 '*'
108000980424     O                       DATA6         Y    110
108100980424     O                       WORA               120 '  :  :  '
108200980424     O                                          127 'Pag.:'
108300980424     O                       PAGE1         Z    132
108400980424     O          E            TEST2            03
108500980424     O                                           18 'LINEA D''ARRIVO'
108600980424     O                       DK1LNA        4   +  1
108700980424     O                       DK1DLA            +  2
108800980424     O          E            TEST2       0  1
108900980424     O                                           18 'LINEA D''ARRIVO'
109000980424     O                       DK1LNA        4   +  1
109100980424     O                       DK1DLA            +  2
109200980424      *  Spedizione
109300980424     O          E            DATSP2      1  1
109400980424     O                       TRAT               132
109500980424     O          E            DATSP2      1
109600980424     O                                           14 'NR.SPEDIZIONE:'
109700980424     O                       CDAAAS        Z   +  1
109800980424     O                       CDALNP        Z   +  1
109900980424     O                       CDANRS        Z   +  1
110000980424     O                       CDANSP        Z   +  1
110100980424     O                                         +  6 'COLLI SPEDIZIONE:'
110200980424     O                       ARBNCL        Z   +  0
110300980424      *  Evento
110400980424     O          E            DATEV2      1
110500980424     O              N35                           6 'EVENTO'
110600980424     O              N35      PSTS              +  1
110700980424     O              N35      PCOD              +  1
110800980424     O              N35      PCMC              +  1
110900980424     O              N35      PDEC              +  1
111000980424     O              N35                        +  2 'DATA'
111100980424     O              N35      PDAM              +  1
111200980424     O              N35                        +  1 'ORA'
111300980424     O              N35      PHRA              +  1
111400980424     O              N35                        +  2 'COLLI EVENTO'
111500980424     O              N35      PNCL          4   +  0
111600980424     O                       PSEE               132
111700980423      *----------------------------------------------------*
111800980409**
111900980409Numero di spedizione inesistente                                       01
112000980409Linea di arrivo diversa dalla bolla                                    02
112100980409Riferimento mittente numerico diverso dalla bolla                      03
112200980409Spedizione già consegnata                                              04
112300980409Evento ricevuto senza data                                             05
112400980415Analizzare dettaglio eventi !!                                         06
112500980409Spedizione in giacenza                                                 07
112600980409Evento da analizzare !!                                                08
112700980409Immetti E se vuoi visualizzare note EDI                                09
112800980409Ricevuto secondo evento colli mancanti                                 10
112900980409Non ci sono spedizioni valide da chiudere in distinta                  11

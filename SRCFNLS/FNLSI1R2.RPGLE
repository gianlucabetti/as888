000100970612     H DECEDIT('0,') DATEDIT(*DMY.)
000200970613     H* FNLSI1R2 *---------------------------------------------------*
000300970612     H*                                                              *
000400970612     H*       - CHIUSURA BOLLA IN PARTENZA CON PRATICA IDD      -    *
000500970612     H*--------------------------------------------------------------*
000600970701     FTABEL00F  IF   E           K DISK    USROPN
000700970701     F*--------
000800970616     FFNBLT01L  UF   E           K DISK
000900970613     F*--------
001000070123     FFNBRV07L  IF   E           K DISK
001100970613     F*--------
001200970612     FFNLSI1D   CF   E             WORKSTN
001300970613     F                                     SFILE(LSI1S04:NRR4)
001400970612     D*--------------------------------------------------------------*
001500970612     D*  SCHIERE
001600970612     D*--------------------------------------------------------------*
001700970701     D* Schiera codici pistole fittizie: 7J
001800970701     D C7J             S              2  0 DIM(500)
001900970612     D* Errori
002000980203     D ERR             S             78    DIM(2) CTDATA PERRCD(1)
002100970612     D*--------------------------------------------------------------*
002200970612     D* DS
002300970612     D*--------------------------------------------------------------*
002400970612     D WLBDA8          DS
002500970612     D  G02DAT                 1      8  0
002600970612     D  G02INV                 9     16  0
002700970612     D  G02ERR                17     17
002800970612     D  G02TGI                18     22  0
002900970612     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003000970612     D CNCR80        E DS
003100970612     D KPJBA         E DS
003200970701     D DS7J          E DS
003300970612     D FNLSI1        E DS                  EXTNAME(FNLSI1DS)
003400970613     D EDDS2A        E DS                  EXTNAME(EDIDS2A)
003500970612     D                SDS
003600970612     D  NOMPGM                 1     10
003700970612     C*--------------------------------------------------------------*
003800970612     C*  C I C L O     P R I N C I P A L E
003900970612     C*--------------------------------------------------------------*
004000970613     C     *ENTRY        PLIST
004100970613     C                   PARM                    KPJBA
004200970613     C                   MOVEL     KPJBU         FNLSI1
004300970612     C*  Inizializzo terza videata
004400970613     C                   EXSR      CARS04
004500970612     C*  Gestione della prima videata
004600970612     C     WFINE         DOWEQ     'N'
004700970613     C     WTPVID        CASEQ     '4'           GESD04
004800970612     C                   END
004900970612     C                   END
005000970612     C*  Fine pgm
005100970613     C                   MOVEL     FNLSI1        KPJBU
005200970613     C                   SETON                                        RT
005300970612     C*--------------------------------------------------------------*
005400970613     C* CARS04: Caricamento subfile dettaglio segnacolli
005500970612     C*--------------------------------------------------------------*
005600970613     C     CARS04        BEGSR
005700970612     C*
005800970617     C                   Z-ADD     0             NRR4
005900970613     C* Clear del subfile
006000970613     C                   SETOFF                                       2021
006100970613     C                   WRITE     LSI1C04
006200970613     C                   SETON                                        2021
006300970613     C* Inizio caricamento
006400970613     C                   SETOFF                                       28
006500970613     C                   MOVEL     'N'           WFINE             1
006600970613     C                   MOVEL     '4'           WTPVID            1
006700970613     C                   MOVEL     NOMPGM        V4CPGM
006800970613     C                   MOVEL     DI1KSC        V4CKSC
006900970613     C                   MOVEL     DI1DEC        V4DKSC
007000980203     C                   MOVEL     DI1STS        V4CSTS
007100980203     C                   MOVEL     DI1DEV        V4DCOD
007200980203     C                   MOVEL     DI1CEV        V4CCOD
007300970613     C                   Z-ADD     DI1AAS        V4CAAS
007400970613     C                   Z-ADD     DI1LNP        V4CLNP
007500970613     C                   Z-ADD     DI1NRS        V4CNRS
007600970613     C                   Z-ADD     DI1NSP        V4CNSP
007700970613     C* eseguo caricamento subfile
007800970613     C                   Z-ADD     DI1AAS        KAAS
007900970613     C                   Z-ADD     DI1LNP        KLNP
008000970613     C                   Z-ADD     DI1NRS        KNRS
008100970613     C                   Z-ADD     DI1NSP        KNSP
008200970616     C     KBLT1         CHAIN     FNBLT01L                           31
008300970613     C     *IN31         DOWEQ     '0'
008400970613     C                   Z-ADD     BLTFLS        V4CFLS
008500970613     C                   Z-ADD     BLTNSC        V4CNSC
008600970619     C                   MOVEL     BLTFL1        V4CFL1
008700970613     C* Controllo se ci sono spunte per quel collo
008800970613     C                   Z-ADD     BLTFLS        KLNP
008900970613     C                   Z-ADD     BLTLNA        KLNA
009000970619     C                   Z-ADD     BLTNSC        KNSC
009100970613     C                   Z-ADD     SIMFEL        KFLE
009200970619     C     DI1FOR        IFEQ      ' '
009300970630     C                   MOVEL     'N'           WESCI             1
009400970630     C                   MOVEL     'N'           WTROVA            1
009500070123     C     KBRV          CHAIN     FNBRV07L                           33
009600970630     C     WESCI         DOUEQ     'S'
009700970630     C*
009800970701     C  N33BRVNPS        LOOKUP    C7J                                    34
009900970630     C     *IN33         IFEQ      '0'
010000970701     C     *IN34         ANDEQ     '0'
010100970630     C                   MOVEL     'S'           WESCI
010200970630     C                   MOVEL     'S'           WTROVA
010300970630     C                   ELSE
010400970630     C     *IN33         IFNE      '0'
010500970630     C                   MOVEL     'S'           WESCI
010600970630     C                   ELSE
010700070123     C     KBRV          READE     FNBRV07L                               33
010800970630     C                   END
010900970630     C                   END
011000970630     C*
011100970630     C                   END
011200970630     C*
011300970630     C     WTROVA        IFEQ      'N'
011400970613     C                   MOVEL     '7'           V4CSCE
011500970613     C                   SETON                                        22
011600970613     C                   ELSE
011700970613     C                   SETOFF                                       22
011800970613     C                   MOVEL     *BLANKS       V4CSCE
011900970613     C                   END
012000970619     C* Altrimenti se evento di mancanza è già stato immesso imposto
012100970619     C* flag da FL1
012200970619     C                   ELSE
012300970619     C                   MOVEL     BLTFL1        V4CSCE
012400970619     C                   END
012500970613     C* Scrittura record
012600970619     C                   MOVEL     V4CSCE        V4CEXS
012700970613     C                   SETON                                        22
012800970613     C                   ADD       1             NRR4              4 0
012900970613     C                   WRITE     LSI1S04
013000970613     C* Lettura successiva
013100970616     C     KBLT1         READE     FNBLT01L                               31
013200970613     C                   END
013300970617     C*
013400970617     C                   Z-ADD     NRR4          WNRCOL            6 0
013500970612     C*
013600970612     C                   ENDSR
013700970613     C*--------------------------------------------------------------*
013800970613     C* GESD04: Gestione videata dettaglio segnacolli
013900970613     C*--------------------------------------------------------------*
014000970613     C     GESD04        BEGSR
014100970613     C*
014200970613     C* Emetto videata
014300970617     C                   WRITE     LSI1Z04
014400970613     C                   EXFMT     LSI1C04
014500970617     C                   SETOFF                                       0528
014600970613     C* Fine lavoro
014700970613     C     *INKC         IFEQ      '1'
014800970613     C                   MOVEL     'S'           WFINE             1
014900970613     C                   MOVEL     '03'          DI1CMD
015000970613     C                   GOTO      FINVD4
015100970613     C                   END
015200970613     C* Ritorno
015300970613     C     *INKL         IFEQ      '1'
015400970613     C                   MOVEL     'S'           WFINE
015500970613     C                   MOVEL     '12'          DI1CMD
015600970613     C                   GOTO      FINVD4
015700970613     C                   END
015800970613     C*
015900980203     C* TUTTI
016000980203     C     *INKB         IFEQ      '1'
016100980203     C                   EXSR      TUTTI
016200980204     C                   GOTO      FINVD4
016300980203     C                   END
016400970613     C* Controlli
016500970613     C                   EXSR      CTR04
016600970613     C* Per errore vado a fine
016700970613     C   28              GOTO      FINVD4
016800970613     C* Se conferma F06 esco x aggiornamento
016900970613     C     *INKF         IFEQ      '1'
017000970613     C                   MOVEL     'S'           WFINE
017100970613     C                   Z-ADD     WNRMAN        DI1NCL
017200970613     C                   MOVEL     '06'          DI1CMD
017300970616     C* Eseguo aggiornamento dettaglio segnacolli
017400970616     C                   EXSR      AGGBLT
017500970613     C                   END
017600970613     C*
017700970613     C     FINVD4        ENDSR
017800980203     C*--------------------------------------------------------------*
017900980203     C* TUTTI: Seleziono tutti i colli
018000980203     C*--------------------------------------------------------------*
018100980203     C     TUTTI         BEGSR
018200980203     C*
018300980203     C                   SETON                                        22
018400980203     C                   Z-ADD     1             NRR4
018500980203     C     NRR4          CHAIN     LSI1S04                            29
018600980203     C     *IN29         DOWEQ     '0'
018700980203     C                   MOVEL     '7'           V4CSCE
018800980203     C                   UPDATE    LSI1S04
018900980203     C                   ADD       1             NRR4
019000980203     C     NRR4          CHAIN     LSI1S04                            29
019100980203     C                   END
019200980204     C                   Z-ADD     1             NRR4
019300980203     C*
019400980203     C                   SETOFF                                       22
019500980203     C*
019600980203     C                   ENDSR
019700970613     C*--------------------------------------------------------------*
019800970616     C* CTR04: CONTROLLO segnacolli persi
019900970613     C*--------------------------------------------------------------*
020000970616     C     CTR04         BEGSR
020100970616     C*  Conto i colli chiusi con mancanza
020200970616     C                   Z-ADD     0             WNRMAN            6 0
020300970619     C                   MOVEL     'N'           WCOLDV            1
020400970616     C                   READC     LSI1S04                                29
020500970616     C     *IN29         DOWEQ     '0'
020600970616     C* Aggiorno totale colli mancanti
020700970616     C     V4CSCE        IFEQ      '7'
020800970616     C                   ADD       1             WNRMAN
020900970616     C                   END
021000970619     C* Colli chiusi con IDD diversi da quelli della precedente chiusura
021100970619     C     DI1FOR        IFEQ      'S'
021200970619     C     V4CSCE        ANDNE     V4CFL1
021300970619     C     V4CSCE        ANDNE     V4CEXS
021400970619     C                   MOVEL     'S'           WCOLDV
021500970619     C                   MOVEL     V4CSCE        V4CEXS
021600970619     C                   END
021700970616     C* Aggiornamento
021800970616     C                   SETON                                        22
021900970616     C                   UPDATE    LSI1S04
022000970616     C                   SETOFF                                       22
022100970616     C* Lettura successiva
022200970616     C                   READC     LSI1S04                                29
022300970616     C                   END
022400970616     C*
022500970619     C* Colli chiusi diversi
022600970619     C     WCOLDV        IFEQ      'S'
022700970619     C                   SETON                                        28
022800980203     C                   MOVEL     ERR(2)        $MSG
022900970619     C                   GOTO      FINCT3
023000970619     C                   END
023100970616     C* Selezionare almeno un collo mancante
023200970616     C     WNRMAN        IFEQ      0
023300970616     C                   SETON                                        28
023400970616     C                   MOVEL     ERR(1)        $MSG
023500970616     C                   END
023600970613     C*
023700970619     C     FINCT3        ENDSR
023800970616     C*--------------------------------------------------------------*
023900970616     C* AGGBLT: Aggiorno segnacolli persi
024000970616     C*--------------------------------------------------------------*
024100970616     C     AGGBLT        BEGSR
024200970616     C*
024300970619     C*  Se avevo già inviato evento di mancanza azzero BLT
024400970619     C*  prima di ritrasmetterlo
024500970619     C     DI1FOR        IFEQ      'S'
024600980203     C     DI1BLT        ANDEQ     'S'
024700970619     C     KBLT1         CHAIN     FNBLT01L                           31
024800970619     C     *IN31         DOWEQ     '0'
024900970619     C                   Z-ADD     0             BLTDFV
025000970619     C                   Z-ADD     0             BLTNFV
025100970619     C                   Z-ADD     0             BLTNPP
025200970619     C                   Z-ADD     0             BLTDCM
025300970619     C                   Z-ADD     0             BLTDTR
025400970619     C                   MOVEL     *BLANKS       BLTFTR
025500970619     C                   MOVEL     *BLANKS       BLTFL1
025600970619     C                   UPDATE    FNBLT000
025700970619     C* Lettura successiva
025800970619     C     KBLT1         READE     FNBLT01L                               31
025900970619     C                   END
026000970619     C                   END
026100970619     C*
026200970616     C*  Conto i colli chiusi con mancanza
026300970616     C                   Z-ADD     0             WNRMAN            6 0
026400970616     C                   READC     LSI1S04                                29
026500970616     C     *IN29         DOWEQ     '0'
026600970616     C* Aggiorno totale colli mancanti
026700970616     C     V4CSCE        IFEQ      '7'
026800970616     C                   ADD       1             WNRMAN
026900970616     C                   Z-ADD     V4CNSC        KNSC
027000970616     C     KBLT2         CHAIN     FNBLT01L                           32
027100970616     C                   MOVEL     '7'           BLTFL1
027200970616     C                   UPDATE    FNBLT000
027300970616     C                   END
027400970616     C* Lettura successiva
027500970616     C                   READC     LSI1S04                                29
027600970616     C                   END
027700970616     C*
027800970616     C                   ENDSR
027900970613     C*--------------------------------------------------------------*
028000970613     C* *INZSR: Operazioni iniziali
028100970613     C*--------------------------------------------------------------*
028200970613     C     *INZSR        BEGSR
028300970613     C*
028400970613     C* Richiamo xparut
028500970613     C                   Z-ADD     1             CODUT
028600970613     C                   CALL      'X§PARUT'
028700970613     C                   PARM                    UTEDSE
028800970613     C                   MOVEL     REC80         CNCR80
028900970613     C                   MOVEL     RAGUT         RSUT
029000970613      *  Definizione chiavi
029100970616     C     KBLT1         KLIST
029200970613     C                   KFLD                    KAAS
029300970613     C                   KFLD                    KLNP
029400970613     C                   KFLD                    KNRS
029500970613     C                   KFLD                    KNSP
029600970616     C     KBLT2         KLIST
029700970616     C                   KFLD                    KAAS
029800970616     C                   KFLD                    KLNP
029900970616     C                   KFLD                    KNRS
030000970616     C                   KFLD                    KNSP
030100970616     C                   KFLD                    KNSC
030200970613     C     KBRV          KLIST
030300970613     C                   KFLD                    KLNP
030400970613     C                   KFLD                    KLNA
030500970613     C                   KFLD                    KNRS
030600970613     C                   KFLD                    KNSC
030700970613     C                   KFLD                    KFLE
030800970701     C*
030900970701     C     KTAB1         KLIST
031000970701     C                   KFLD                    KKUT
031100970701     C                   KFLD                    KCOD
031200970613      *
031300970613      *  Definizione variabili
031400970613     C     *LIKE         DEFINE    BLTAAS        KAAS
031500970613     C     *LIKE         DEFINE    BLTNSP        KNSP
031600970613     C     *LIKE         DEFINE    BRVLNA        KLNA
031700970613     C     *LIKE         DEFINE    BRVNSC        KNSC
031800970613     C     *LIKE         DEFINE    BRVFLE        KFLE
031900970613     C     *LIKE         DEFINE    BRVLNP        KLNP
032000970613     C     *LIKE         DEFINE    BRVNRS        KNRS
032100970701     C     *LIKE         DEFINE    TBLCOD        KCOD
032200970701     C     *LIKE         DEFINE    TBLKUT        KKUT
032300970616      *
032400970701     C* Eseguo caricamento da tabella 7J codici pistole fittizie
032500970701     C                   OPEN      TABEL00F
032600970701     C                   MOVEA     *HIVAL        C7J
032700970701     C                   Z-ADD     0             X                 4 0
032800970701     C                   Z-ADD     1             KKUT
032900970701     C                   MOVEL     '7J'          KCOD
033000970701     C     KTAB1         CHAIN     TABEL                              30
033100970701     C     *IN30         DOWEQ     '0'
033200970701     C     X             ANDLT     100
033300970701     C     TBLFLG        IFEQ      *BLANKS
033400970701     C                   MOVEL     TBLUNI        DS7J
033500970701     C     §7JFSL        IFEQ      'F'
033600970701     C                   ADD       1             X
033700970701     C                   MOVEL     TBLKEY        C7J(X)
033800970701     C                   END
033900970701     C                   END
034000970701     C     KTAB1         READE     TABEL                                  30
034100970701     C                   END
034200970701     C                   CLOSE     TABEL00F
034300970616      *  Reperisco la data del giorno
034400970616     C                   TIME                    WHHDAT           14 0
034500970616     C                   MOVE      WHHDAT        G02DAT
034600970616     C                   MOVEL     *BLANK        G02ERR
034700970616     C                   CALL      'XSRDA8'
034800970616     C                   PARM                    WLBDA8
034900970616     C                   Z-ADD     G02INV        OGGI              8 0
035000970613     C*
035100970613     C                   ENDSR
035200970619**
035300980203Selezionare almeno un collo
035400970619Attenzione ! Colli indicati in IDD diversi da quelli della precedente chiusura

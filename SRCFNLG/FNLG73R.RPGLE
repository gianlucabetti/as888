000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200940923     H* FNLG73R *----------------------------------------------------*
000300900418     H*                                                              *
000400920522     H*    - STAMPA ELENCO GIACENZE APERTE PER CLIENTE  -BATCH-      *
000500900423     H*                                                              *
000600900418     H*--------------------------------------------------------------*
000700050224     FTIGCP07L  IF   E           K DISK
000800050224     FTIGNP01L  IF   E           K DISK
000900060213     FFiar401L  IF   E           K DISK
001000051115     FFiar901L  IF   E           K DISK
001100920522     FAZORG01L  IF   E           K DISK
001200920522     FTABEL00F  IF   E           K DISK
001300940923     FFNBLP01L  IF   E           K DISK
001400920522     FCNACO00F  IF   E           K DISK
001500920522     FCNIND00F  IF   E           K DISK
001600940926     FLET132    O    F  132        PRINTER INFDS(STP1)
001700940923     F*--------------------------------------------------------------*
001800900430     D DS3A          E DS
001900900607     D DS2A          E DS
002000900607     D DS2B          E DS
002100940923     D FNLG73        E DS                  EXTNAME(FNLG73DS)
002200960124     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
002300900502     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002400900502     D CNCR80        E DS
002500900608     D CNCR42        E DS
002600900418     D KPJBA         E DS
002700940923     D WLBDA8          DS
002800940923     D  G02DAT                 1      8  0
002900940923     D  G02INV                 9     16  0
003000940923     D  G02ERR                17     17
003100940923     D  G02TGI                18     22  0
003200940926     D STP1            DS
003300940926     D  LIN                  367    368B 0
003400900423     C*---------------------------------------------------------------*
003500020822     c     kgcp          klist
003600020822     c                   kfld                    simpou
003700020822     c                   kfld                    codcli
003800050224     C     kgcp          SETLL     TIGCP07L
003900050224     C     kgcp          READE     TIGCP07L                             1510
004000911018     C     *IN10         DOWEQ     '0'
004100940923     C** SE DATA APERTURA DIVERSA NON FACCIO NULLA
004200940926     C  N15GCPDMM        IFLE      D73DTA
004300940926     C     GCPDMM        ANDGE     D73DTD
004400920522     C     GCPFRG        ANDEQ     0
004500920522     C                   EXSR      STAMPA
004600920522     C                   END
004700050224     C     kgcp          READE     TIGCP000                             1510
004800920522     C                   END
004900920525     C   90              EXCEPT    FINE
005000920522     C*
005100920522     C                   SETON                                          LR
005200940923     C*---------------------------------------------------------------*
005300920522     C     STAMPA        BEGSR
005400920522     C* DECODIFICO LA PRIMA VOLTA IL CLIENTE
005500920522     C  N90              DO
005600920522     C     KACO          CHAIN     CNACO000                           20
005700920522     C  N20              MOVEL     ACORAG        PRAG             30
005800920522     C   20              MOVEL     *ALL'?'       PRAG
005900920522     C  N20KACO          CHAIN     CNIND000                           20
006000920522     C                   END
006100920522     C*
006200900423     C** AGGANCIO FILE STORICO
006300940926     C     KBLP          CHAIN     FNBLP01L                           22
006400911018     C  N22              DO
006500900430     C** CODICE BOLLA
006600900430     C                   MOVEL     '3A'          COD
006700900430     C                   MOVEL     *BLANKS       KEY
006800940923     C                   MOVEL     BLPCBO        KEY
006900900430     C     KTAB          CHAIN     TABEL                              70
007000900509     C                   MOVEL     *BLANKS       DESCBL           10
007100900509     C  N70              DO
007200900509     C                   MOVEL     TBLUNI        DS3A
007300900509     C                   MOVEL     §3ADES        DESCBL
007400900509     C                   END
007500900423     C** RICERCO DESCRIZIONE DELLA FILIALE DI PARTENZA
007600900430     C     GCPLNP        CHAIN     AZORG                              23
007700911018     C  N23              DO
007800900423     C                   MOVEL     *BLANKS       DESLNP           20
007900900423     C                   MOVEL     *BLANKS       INDLNP           20
008000900423     C                   Z-ADD     0             CAPLNP            5 0
008100900423     C                   MOVEL     *BLANKS       LOCLNP           20
008200900423     C                   MOVEL     *BLANKS       PROLNP            2
008300900423     C                   MOVEL     *BLANKS       FAXLNP           13
008400900423     C                   MOVEL     *BLANKS       TLXLNP           13
008500900423     C                   MOVEL     *BLANKS       TELLNP           13
008600900423     C                   MOVEL     ORGDES        DESLNP
008700900423     C                   MOVEL     ORGIND        INDLNP
008800900430     C                   Z-ADD     ORGCPF        CAPLNP
008900900423     C                   MOVEL     ORGLOC        LOCLNP
009000900423     C                   MOVEL     ORGPRO        PROLNP
009100900423     C                   MOVEL     ORGTEL        TELLNP
009200900423     C                   MOVEL     ORGFAX        FAXLNP
009300900423     C                   MOVEL     ORGTLX        TLXLNP
009400900423     C                   END
009500900511     C** RICERCO DESCRIZIONE DEL COD.ELABORATORE
009600920522     C     BLPLNP        CHAIN     AZORG                              70
009700900511     C                   MOVEL     *BLANKS       DESELA           20
009800900511     C                   MOVEL     *BLANKS       INDELA           20
009900900511     C                   Z-ADD     0             CAPELA            5 0
010000900511     C                   MOVEL     *BLANKS       LOCELA           20
010100900511     C                   MOVEL     *BLANKS       PROELA            2
010200900511     C  N70              DO
010300900511     C                   MOVEL     ORGDES        DESELA
010400900511     C                   MOVEL     ORGIND        INDELA
010500900511     C                   Z-ADD     ORGCPF        CAPELA
010600900511     C                   MOVEL     ORGLOC        LOCELA
010700900511     C                   MOVEL     ORGPRO        PROELA
010800900511     C                   END
010900900607     C** RICERCO DESCRIZIONE LUOGO DEPOSITO MERCE SULLA TABELLA
011000900607     C** SE NON LO TROVO LO RICERCO SULL'ORGANIGRAMMA
011100900502     C                   MOVEL     '2B'          COD
011200900502     C                   MOVEL     *BLANKS       KEY
011300900607     C                   MOVEL     GCPCCR        KEY
011400900502     C     KTAB          CHAIN     TABEL                              70
011500900607     C  N70              MOVEL     TBLUNI        DS2B
011600900607     C   70              DO
011700900607     C     GCPCCR        CHAIN     AZORG                              70
011800900607     C                   MOVEL     *BLANKS       §2BDES
011900900607     C                   MOVEL     *BLANKS       §2BIND
012000900607     C                   MOVEL     *BLANKS       §2BLOC
012100900607     C                   MOVEL     *ZEROS        §2BCAP
012200900607     C                   MOVEL     *BLANKS       §2BPRO
012300900607     C  N70              DO
012400900607     C                   MOVEL     ORGDES        §2BDES
012500900607     C                   MOVEL     ORGIND        §2BIND
012600900607     C                   MOVEL     ORGLOC        §2BLOC
012700900607     C                   Z-ADD     ORGCPF        §2BCAP
012800900607     C                   MOVEL     ORGPRO        §2BPRO
012900900502     C                   END
013000900607     C                   END
013100900607     C** RICERCO COD.MOTIVO GIACENZA SULLA TABELLA
013200900607     C                   MOVEL     '2A'          COD
013300900607     C                   MOVEL     *BLANKS       KEY
013400900607     C                   MOVEL     GCPCMC        KEY
013500900607     C     KTAB          CHAIN     TABEL                              70
013600900607     C  N70              MOVEL     TBLUNI        DS2A
013700920525     C                   MOVEL     §2ADES        PDES             50
013800911018     C* PRELEVO EVENTUALE ULTERIORE DESCRIZIONE
013900911018     C                   Z-ADD     10            FASE
014000911205     C                   Z-ADD     1             NPR
014100050224     C     KNOT          CHAIN     tignp01L                           69
014200940923     C  N69              MOVEL     GNPDMC        ULTDMC
014300911018     C   69              MOVEL     *BLANKS       ULTDMC           50
014400940926     C** RIBALTAMENTO DATA SPEDIZIONE
014500940926     C                   MOVEL     BLPAAS        G02INV
014600940926     C                   MOVE      BLPMGS        G02INV
014700940926     C                   Z-ADD     0             G02DAT
014800940926     C                   MOVEL     '3'           G02ERR
014900940926     C                   CALL      'XSRDA8'
015000940926     C                   PARM                    WLBDA8
015100940926     C                   Z-ADD     G02DAT        DATSPE            8 0
015200910314     C* PRENDO ULTIME 3 CIFRE DEL SEGNSCOLLO AL
015300910314     C                   MOVE      BLPNCA        STANCA            3 0
015400900423     C*
015500940926     C* Controllo se esiste estensione descrizione destinatario
015600060213     C                   MOVEL     '4'           ar4TRC
015700940926     C                   SETOFF                                       05
015800060213     C                   MOVEL     *BLANKS       ar4NOT
015900060213     C     Kar4          CHAIN     Fiar401L                           31
016000940926     C  N31              SETON                                        05
016100060213     C                   MOVEL     ar4NOT        RAGDE2           35
016200960124     C*
016300960124     C*  CONTROLLO SE ESISTE DESTINATARIO FINALE
016400960124     C                   CLEAR                   FNLSN0
016500960124     C                   Z-ADD     GCPAAS        DN0AAS
016600960124     C                   Z-ADD     GCPLNP        DN0LNP
016700960124     C                   Z-ADD     GCPNRS        DN0NRS
016800960124     C                   Z-ADD     GCPNSP        DN0NSP
016900960124     C                   MOVEL     *BLANKS       DN0TRC
017000960125     C                   Z-ADD     GCPDUR        DN0DAG
017100960124     C                   MOVEL     KPJBU         WSVKPJ          256
017200960124     C                   MOVEL     FNLSN0        KPJBU
017300960124     C                   CALL      'FNLSN0R'
017400960124     C                   PARM                    KPJBA
017500960124     C                   MOVEL     KPJBU         FNLSN0
017600960124     C                   MOVEL     WSVKPJ        KPJBU
017700960124     C* Destinatario
017800960124     C     DN0TRV        IFEQ      'S'
017900960124     C                   MOVEL     DN0RSD        STPRSD
018000960124     C                   MOVEL     DN0RS2        RAGDE2
018100960124     C                   MOVEL     DN0IND        STPIND
018200960124     C                   MOVEL     DN0LOD        STPLOD
018300960124     C                   MOVEL     DN0PRD        STPPRD
018400960124     C                   MOVEL     DN0CAD        STPCAD
018500960124     C                   MOVEL     DN0NZD        STPNZD
018600960124     C                   ELSE
018700960124     C                   MOVEL     BLPRSD        STPRSD
018800960124     C                   MOVEL     BLPIND        STPIND
018900960124     C                   MOVEL     BLPLOD        STPLOD
019000960124     C                   MOVEL     BLPPRD        STPPRD
019100960124     C                   MOVEL     BLPCAD        STPCAD
019200960124     C                   MOVEL     BLPNZD        STPNZD
019300960124     C                   END
019400940926     C* Note su bolla
019500060213     C                   MOVEL     '8'           ar4TRC
019600060213     C                   MOVEL     *BLANKS       ar4NOT
019700060213     C     Kar4          CHAIN     Fiar401L                           31
019800060213     C                   MOVEL     ar4NOT        NOTE1            50
019900060213     C                   MOVEL     '9'           ar4TRC
020000060213     C                   MOVEL     *BLANKS       ar4NOT
020100060213     C     Kar4          CHAIN     Fiar401L                           31
020200060213     C                   MOVEL     ar4NOT        NOTE2            50
020300940926     C  N31              SETON                                        06
020400940926     C* Dati C/Assegno
020500051115     C                   Z-ADD     0             ar9CAS
020600051115     C                   MOVEL     *BLANKS       ar9VCA
020700051115     C     KBLP          CHAIN     Fiar901L                           31
020800940926     C*
020900920522     C     *IN90         IFEQ      '0'
021000940926     C     LIN           ORGE      58
021100920522     C                   EXCEPT    TESTA
021200920522     C                   SETON                                        90
021300920522     C                   END
021400920522     C                   EXCEPT    RIGA
021500970623     C                   END
021600940926     C*
021700920522     C                   ENDSR
021800940923     C*---------------------------------------------------------------*
021900940923     C*  Operazioni iniziali                                         -*
022000940923     C*---------------------------------------------------------------*
022100940923     C     *INZSR        BEGSR
022200940923     C*
022300940923     C     *ENTRY        PLIST
022400940923     C                   PARM                    KPJBA
022500940923     C                   Z-ADD     1             CODUT
022600940923     C                   CALL      'X§PARUT'
022700940923     C                   PARM                    UTEDSE
022800940923     C*          CODUT     CABEQ-1        FINE
022900940923     C                   MOVEL     RAGUT         RSUT             20
023000940923     C*---------------------------------------------------------------*
023100940923     C                   MOVEL     REC80         CNCR80
023200940923     C                   MOVEL     REC42         CNCR42
023300940923     C                   MOVEL     *BLANKS       RAGSOC
023400940923     C                   MOVEL     SIMRGS        RAGSOC           30
023500940923     C** TABEL
023600940923     C     KTAB          KLIST
023700940923     C                   KFLD                    CODUT
023800940923     C                   KFLD                    COD               2
023900940923     C                   KFLD                    KEY               8
024000940923     C* CNACO
024100940923     C     KACO          KLIST
024200940923     C                   KFLD                    CODUT
024300940926     C                   KFLD                    D73KCC
024400940923     C                   KFLD                    GCPSCM
024500940923     C* FLBLP
024600940923     C     KBLP          KLIST
024700940923     C                   KFLD                    GCPAAS
024800940923     C                   KFLD                    GCPLNP
024900940923     C                   KFLD                    GCPNRS
025000940923     C                   KFLD                    GCPNSP
025100940926     C*
025200060213     C     Kar4          KLIST
025300940926     C                   KFLD                    GCPAAS
025400940926     C                   KFLD                    GCPLNP
025500940926     C                   KFLD                    GCPNRS
025600940926     C                   KFLD                    GCPNSP
025700060213     C                   KFLD                    ar4TRC
025800940923     C*
025900940923     C     KNOT          KLIST
026000940923     C                   KFLD                    GCPAGC
026100940923     C                   KFLD                    GCPFGC
026200940923     C                   KFLD                    GCPNGC
026300940923     C                   KFLD                    GCPFRG
026400940923     C                   KFLD                    FASE              3 0
026500940923     C                   KFLD                    NPR               3 0
026600960124     C*----------------------------------------------------*
026700960124     C*  DEFINIZIONE CAMPI DI STAMPA
026800960124     C     *LIKE         DEFINE    DN0RSD        STPRSD
026900960124     C     *LIKE         DEFINE    DN0IND        STPIND
027000960124     C     *LIKE         DEFINE    DN0LOD        STPLOD
027100960124     C     *LIKE         DEFINE    DN0PRD        STPPRD
027200960124     C     *LIKE         DEFINE    DN0CAD        STPCAD
027300960124     C     *LIKE         DEFINE    DN0NZD        STPNZD
027400940923     C*----------------------------------------------------*
027500940923     C                   CLEAR                   FNLG73
027600940923     C                   MOVEL     KPJBU         FNLG73
027700940926     C                   Z-ADD     D73KSC        CODCLI            7 0
027800940923     C                   SETOFF                                       90
027900940923     C*
028000940923     C*  GIRO LA DATA LIMITE INIZIALE
028100940923     C                   Z-ADD     D73DTD        G02INV
028200940923     C                   Z-ADD     0             G02DAT
028300940923     C                   MOVEL     '3'           G02ERR
028400940923     C                   CALL      'XSRDA8'
028500940926     C                   PARM                    WLBDA8
028600940923     C                   Z-ADD     G02DAT        DATAD             8 0
028700940923     C*
028800940923     C*  GIRO LA DATA LIMITE FINALE
028900940923     C                   Z-ADD     D73DTA        G02INV
029000940923     C                   Z-ADD     0             G02DAT
029100940923     C                   MOVEL     '3'           G02ERR
029200940923     C                   CALL      'XSRDA8'
029300940926     C                   PARM                    WLBDA8
029400940923     C                   Z-ADD     G02DAT        DATAA             8 0
029500940923     C*
029600940923     C                   ENDSR
029700940923     C*----------------------------------------------------*
029800900608     O** STAMPA LETTERA AL MITTENTE MODULO NUOVO
029900961115     OLET132    E            TESTA            10
030000920525     O                                           70 'Pagina'
030100920525     O                       PAGE          Z   +  1
030200961115     O          E            TESTA            11
030300961001     O                                         +  1 'Punto Operativo di'
030400961001     O          E            TESTA            12
030500961115     O                       DESLNP            +  1
030600920525     O                                           60 'Spettabile'
030700961115     O          E            TESTA            13
030800961115     O                       INDLNP              21
030900961001     O          E            TESTA            14
031000961115     O                       CAPLNP               6
031100961115     O                       LOCLNP            +  1
031200961115     O                       PROLNP            +  1
031300920525     O                       ACORAG              98
031400961001     O          E            TESTA            15
031500961115     O                                            4 'FAX'
031600961115     O                       FAXLNP            +  1
031700961115     O                                         +  1 'TLX'
031800961115     O                       TLXLNP            +  1
031900961115     O                       INDVIA              80
032000961001     O          E            TESTA            16
032100961115     O                                            4 'TEL'
032200961115     O                       TELLNP            +  1
032300050415     O                       INDCAe              59
032400930331     O                       INDCIT            +  1
032500961001     O          E            TESTA            19
032600940926     O                                           24 'Con la presente Vi comun'
032700940926     O                                         +  0 'ichiamo le spedizioni gi'
032800940926     O                                         +  0 'acenti dal'
032900940926     O                       DATAD             +  1 '  /  /    '
033000940926     O                                         +  1 'al'
033100940926     O                       DATAA             +  1 '  /  /    '
033200920525     O          E            RIGA        1
033300920525     O                                           11 'Giacenza '
033400940926     O                                         +  1 'Destinatario'
033500920525     O          E            RIGA        0  1
033600920525     O                                           11 'Giacenza '
033700940926     O                                         +  1 'Destinatario'
033800920525     O          E            RIGA           0
033900920525     O                       GCPFGC        Z      3
034000920525     O                                         +  0 '/'
034100920525     O                       GCPNGC        Z   +  0
034200960124     O                       STPRSD            +  1
034300940926     O                                         +  1 'Sped.'
034400940926     O                       GCPAAS        Z   +  0
034500920522     O                       GCPLNP        Z   +  1
034600920522     O                       GCPNRS        Z   +  1
034700920522     O                       GCPNSP        Z   +  1
034800940926     O                       DATSPE            +  1 '  /  /    '
034900940926     O          E    05      RIGA        1
035000940926     O                       RAGDE2              47
035100920522     O          E            RIGA        1
035200960124     O                       STPIND              47
035300940926     O                                         +  1 'Rif:'
035400940926     O                       BLPRMN        Z   +  0
035500940926     O                       BLPRMA            +  1
035600920522     O          E            RIGA        1
035700960124     O                       STPCAD              21
035800960124     O                       STPLOD            +  1
035900960124     O                       STPPRD            +  1
036000960124     O                       STPNZD            +  1
036100920525     O          E            RIGA        1
036200940926     O                                           18 'Colli:'
036300940926     O                       BLPNCL        Z   +  0
036400940926     O                                         +  1 'Peso:'
036500940926     O                       BLPPKF        2   +  0
036600940926     O                                         +  1 'Vol:'
036700940926     O                       BLPVLF        2   +  0
036800940926     O                                         +  1 'C/assegno:'
036900051115     O                       ar9CAS        2   +  0
037000051115     O                       ar9VCA            +  1
037100920525     O          E            RIGA        1
037200940926     O                                           17 'NOTE.:'
037300940926     O                       NOTE1             +  1
037400940926     O          E    06      RIGA        1
037500940926     O                       NOTE2               69
037600940926     O          E            RIGA        1
037700940926     O                                           24 'Motivo giac:'
037800920522     O                       PDES              +  1
037900920522     O          E            RIGA        1
038000940926     O                       ULTDMC              75
038100920525     O          E            FINE        1
038200920525     O                                           24 'In attesa di Vs. disposi'
038300920525     O                                         +  0 'zioni porgiamo distinti'
038400920525     O                                         +  1 'saluti.'
038500971113     O          E            FINE        3
038600971113     O                       RSUT                73

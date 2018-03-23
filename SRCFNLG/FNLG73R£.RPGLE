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
007500900430     C** TIPO SPEDIZIONE
007600900423     C                   MOVEL     *BLANKS       DESTSP            8
007700900430     C     BLPTSP        IFEQ      'U'
007800940926     C     BLPTSP        OREQ      'E'
007900900423     C                   MOVEL     'URGENTE'     DESTSP
008000900423     C                   END
008100900423     C** RICERCO DESCRIZIONE DELLA FILIALE DI PARTENZA
008200900430     C     GCPLNP        CHAIN     AZORG                              23
008300911018     C  N23              DO
008400900423     C                   MOVEL     *BLANKS       DESLNP           20
008500900423     C                   MOVEL     *BLANKS       INDLNP           20
008600900423     C                   Z-ADD     0             CAPLNP            5 0
008700900423     C                   MOVEL     *BLANKS       LOCLNP           20
008800900423     C                   MOVEL     *BLANKS       PROLNP            2
008900900423     C                   MOVEL     *BLANKS       FAXLNP           13
009000900423     C                   MOVEL     *BLANKS       TLXLNP           13
009100900423     C                   MOVEL     *BLANKS       TELLNP           13
009200900423     C                   MOVEL     ORGDES        DESLNP
009300900423     C                   MOVEL     ORGIND        INDLNP
009400900430     C                   Z-ADD     ORGCPF        CAPLNP
009500900423     C                   MOVEL     ORGLOC        LOCLNP
009600900423     C                   MOVEL     ORGPRO        PROLNP
009700900423     C                   MOVEL     ORGTEL        TELLNP
009800900423     C                   MOVEL     ORGFAX        FAXLNP
009900900423     C                   MOVEL     ORGTLX        TLXLNP
010000900423     C                   END
010100900511     C** RICERCO DESCRIZIONE DEL COD.ELABORATORE
010200920522     C     BLPLNP        CHAIN     AZORG                              70
010300900511     C                   MOVEL     *BLANKS       DESELA           20
010400900511     C                   MOVEL     *BLANKS       INDELA           20
010500900511     C                   Z-ADD     0             CAPELA            5 0
010600900511     C                   MOVEL     *BLANKS       LOCELA           20
010700900511     C                   MOVEL     *BLANKS       PROELA            2
010800900511     C  N70              DO
010900900511     C                   MOVEL     ORGDES        DESELA
011000900511     C                   MOVEL     ORGIND        INDELA
011100900511     C                   Z-ADD     ORGCPF        CAPELA
011200900511     C                   MOVEL     ORGLOC        LOCELA
011300900511     C                   MOVEL     ORGPRO        PROELA
011400900511     C                   END
011500900607     C** RICERCO DESCRIZIONE LUOGO DEPOSITO MERCE SULLA TABELLA
011600900607     C** SE NON LO TROVO LO RICERCO SULL'ORGANIGRAMMA
011700900502     C                   MOVEL     '2B'          COD
011800900502     C                   MOVEL     *BLANKS       KEY
011900900607     C                   MOVEL     GCPCCR        KEY
012000900502     C     KTAB          CHAIN     TABEL                              70
012100900607     C  N70              MOVEL     TBLUNI        DS2B
012200900607     C   70              DO
012300900607     C     GCPCCR        CHAIN     AZORG                              70
012400900607     C                   MOVEL     *BLANKS       §2BDES
012500900607     C                   MOVEL     *BLANKS       §2BIND
012600900607     C                   MOVEL     *BLANKS       §2BLOC
012700900607     C                   MOVEL     *ZEROS        §2BCAP
012800900607     C                   MOVEL     *BLANKS       §2BPRO
012900900607     C  N70              DO
013000900607     C                   MOVEL     ORGDES        §2BDES
013100900607     C                   MOVEL     ORGIND        §2BIND
013200900607     C                   MOVEL     ORGLOC        §2BLOC
013300900607     C                   Z-ADD     ORGCPF        §2BCAP
013400900607     C                   MOVEL     ORGPRO        §2BPRO
013500900502     C                   END
013600900607     C                   END
013700900607     C** RICERCO COD.MOTIVO GIACENZA SULLA TABELLA
013800900607     C                   MOVEL     '2A'          COD
013900900607     C                   MOVEL     *BLANKS       KEY
014000900607     C                   MOVEL     GCPCMC        KEY
014100900607     C     KTAB          CHAIN     TABEL                              70
014200900607     C  N70              MOVEL     TBLUNI        DS2A
014300920525     C                   MOVEL     §2ADES        PDES             50
014400911018     C* PRELEVO EVENTUALE ULTERIORE DESCRIZIONE
014500911018     C                   Z-ADD     10            FASE
014600911205     C                   Z-ADD     1             NPR
014700050224     C     KNOT          CHAIN     tignp01L                           69
014800940923     C  N69              MOVEL     GNPDMC        ULTDMC
014900911018     C   69              MOVEL     *BLANKS       ULTDMC           50
015000940926     C** RIBALTAMENTO DATA SPEDIZIONE
015100940926     C                   MOVEL     BLPAAS        G02INV
015200940926     C                   MOVE      BLPMGS        G02INV
015300940926     C                   Z-ADD     0             G02DAT
015400940926     C                   MOVEL     '3'           G02ERR
015500940926     C                   CALL      'XSRDA8'
015600940926     C                   PARM                    WLBDA8
015700940926     C                   Z-ADD     G02DAT        DATSPE            8 0
015800910314     C* PRENDO ULTIME 3 CIFRE DEL SEGNSCOLLO AL
015900910314     C                   MOVE      BLPNCA        STANCA            3 0
016000900423     C*
016100940926     C* Controllo se esiste estensione descrizione destinatario
016200060213     C                   MOVEL     '4'           ar4TRC
016300940926     C                   SETOFF                                       05
016400060213     C                   MOVEL     *BLANKS       ar4NOT
016500060213     C     Kar4          CHAIN     Fiar401L                           31
016600940926     C  N31              SETON                                        05
016700060213     C                   MOVEL     ar4NOT        RAGDE2           35
016800960124     C*
016900960124     C*  CONTROLLO SE ESISTE DESTINATARIO FINALE
017000960124     C                   CLEAR                   FNLSN0
017100960124     C                   Z-ADD     GCPAAS        DN0AAS
017200960124     C                   Z-ADD     GCPLNP        DN0LNP
017300960124     C                   Z-ADD     GCPNRS        DN0NRS
017400960124     C                   Z-ADD     GCPNSP        DN0NSP
017500960124     C                   MOVEL     *BLANKS       DN0TRC
017600960125     C                   Z-ADD     GCPDUR        DN0DAG
017700960124     C                   MOVEL     KPJBU         WSVKPJ          256
017800960124     C                   MOVEL     FNLSN0        KPJBU
017900960124     C                   CALL      'FNLSN0R'
018000960124     C                   PARM                    KPJBA
018100960124     C                   MOVEL     KPJBU         FNLSN0
018200960124     C                   MOVEL     WSVKPJ        KPJBU
018300960124     C* Destinatario
018400960124     C     DN0TRV        IFEQ      'S'
018500960124     C                   MOVEL     DN0RSD        STPRSD
018600960124     C                   MOVEL     DN0RS2        RAGDE2
018700960124     C                   MOVEL     DN0IND        STPIND
018800960124     C                   MOVEL     DN0LOD        STPLOD
018900960124     C                   MOVEL     DN0PRD        STPPRD
019000960124     C                   MOVEL     DN0CAD        STPCAD
019100960124     C                   MOVEL     DN0NZD        STPNZD
019200960124     C                   ELSE
019300960124     C                   MOVEL     BLPRSD        STPRSD
019400960124     C                   MOVEL     BLPIND        STPIND
019500960124     C                   MOVEL     BLPLOD        STPLOD
019600960124     C                   MOVEL     BLPPRD        STPPRD
019700960124     C                   MOVEL     BLPCAD        STPCAD
019800960124     C                   MOVEL     BLPNZD        STPNZD
019900960124     C                   END
020000940926     C* Note su bolla
020100060213     C                   MOVEL     '8'           ar4TRC
020200060213     C                   MOVEL     *BLANKS       ar4NOT
020300060213     C     Kar4          CHAIN     Fiar401L                           31
020400060213     C                   MOVEL     ar4NOT        NOTE1            50
020500060213     C                   MOVEL     '9'           ar4TRC
020600060213     C                   MOVEL     *BLANKS       ar4NOT
020700060213     C     Kar4          CHAIN     Fiar401L                           31
020800060213     C                   MOVEL     ar4NOT        NOTE2            50
020900940926     C  N31              SETON                                        06
021000940926     C* Dati C/Assegno
021100051115     C                   Z-ADD     0             ar9CAS
021200051115     C                   MOVEL     *BLANKS       ar9VCA
021300051115     C     KBLP          CHAIN     Fiar901L                           31
021400940926     C*
021500920522     C     *IN90         IFEQ      '0'
021600940926     C     LIN           ORGE      58
021700920522     C                   EXCEPT    TESTA
021800920522     C                   SETON                                        90
021900920522     C                   END
022000920522     C                   EXCEPT    RIGA
022100970623     C                   END
022200940926     C*
022300920522     C                   ENDSR
022400940923     C*---------------------------------------------------------------*
022500940923     C*  Operazioni iniziali                                         -*
022600940923     C*---------------------------------------------------------------*
022700940923     C     *INZSR        BEGSR
022800940923     C*
022900940923     C     *ENTRY        PLIST
023000940923     C                   PARM                    KPJBA
023100940923     C                   Z-ADD     1             CODUT
023200940923     C                   CALL      'X§PARUT'
023300940923     C                   PARM                    UTEDSE
023400940923     C*          CODUT     CABEQ-1        FINE
023500940923     C                   MOVEL     RAGUT         RSUT             20
023600940923     C*---------------------------------------------------------------*
023700940923     C                   MOVEL     REC80         CNCR80
023800940923     C                   MOVEL     REC42         CNCR42
023900940923     C                   MOVEL     *BLANKS       RAGSOC
024000940923     C                   MOVEL     SIMRGS        RAGSOC           30
024100940923     C** TABEL
024200940923     C     KTAB          KLIST
024300940923     C                   KFLD                    CODUT
024400940923     C                   KFLD                    COD               2
024500940923     C                   KFLD                    KEY               8
024600940923     C* CNACO
024700940923     C     KACO          KLIST
024800940923     C                   KFLD                    CODUT
024900940926     C                   KFLD                    D73KCC
025000940923     C                   KFLD                    GCPSCM
025100940923     C* FLBLP
025200940923     C     KBLP          KLIST
025300940923     C                   KFLD                    GCPAAS
025400940923     C                   KFLD                    GCPLNP
025500940923     C                   KFLD                    GCPNRS
025600940923     C                   KFLD                    GCPNSP
025700940926     C*
025800060213     C     Kar4          KLIST
025900940926     C                   KFLD                    GCPAAS
026000940926     C                   KFLD                    GCPLNP
026100940926     C                   KFLD                    GCPNRS
026200940926     C                   KFLD                    GCPNSP
026300060213     C                   KFLD                    ar4TRC
026400940923     C*
026500940923     C     KNOT          KLIST
026600940923     C                   KFLD                    GCPAGC
026700940923     C                   KFLD                    GCPFGC
026800940923     C                   KFLD                    GCPNGC
026900940923     C                   KFLD                    GCPFRG
027000940923     C                   KFLD                    FASE              3 0
027100940923     C                   KFLD                    NPR               3 0
027200960124     C*----------------------------------------------------*
027300960124     C*  DEFINIZIONE CAMPI DI STAMPA
027400960124     C     *LIKE         DEFINE    DN0RSD        STPRSD
027500960124     C     *LIKE         DEFINE    DN0IND        STPIND
027600960124     C     *LIKE         DEFINE    DN0LOD        STPLOD
027700960124     C     *LIKE         DEFINE    DN0PRD        STPPRD
027800960124     C     *LIKE         DEFINE    DN0CAD        STPCAD
027900960124     C     *LIKE         DEFINE    DN0NZD        STPNZD
028000940923     C*----------------------------------------------------*
028100940923     C                   CLEAR                   FNLG73
028200940923     C                   MOVEL     KPJBU         FNLG73
028300940926     C                   Z-ADD     D73KSC        CODCLI            7 0
028400940923     C                   SETOFF                                       90
028500940923     C*
028600940923     C*  GIRO LA DATA LIMITE INIZIALE
028700940923     C                   Z-ADD     D73DTD        G02INV
028800940923     C                   Z-ADD     0             G02DAT
028900940923     C                   MOVEL     '3'           G02ERR
029000940923     C                   CALL      'XSRDA8'
029100940926     C                   PARM                    WLBDA8
029200940923     C                   Z-ADD     G02DAT        DATAD             8 0
029300940923     C*
029400940923     C*  GIRO LA DATA LIMITE FINALE
029500940923     C                   Z-ADD     D73DTA        G02INV
029600940923     C                   Z-ADD     0             G02DAT
029700940923     C                   MOVEL     '3'           G02ERR
029800940923     C                   CALL      'XSRDA8'
029900940926     C                   PARM                    WLBDA8
030000940923     C                   Z-ADD     G02DAT        DATAA             8 0
030100940923     C*
030200940923     C                   ENDSR
030300940923     C*----------------------------------------------------*
030400900608     O** STAMPA LETTERA AL MITTENTE MODULO NUOVO
030500961115     OLET132    E            TESTA            10
030600920525     O                                           70 'Pagina'
030700920525     O                       PAGE          Z   +  1
030800961115     O          E            TESTA            11
030900961001     O                                         +  1 'Punto Operativo di'
031000961001     O          E            TESTA            12
031100961115     O                       DESLNP            +  1
031200920525     O                                           60 'Spettabile'
031300961115     O          E            TESTA            13
031400961115     O                       INDLNP              21
031500961001     O          E            TESTA            14
031600961115     O                       CAPLNP               6
031700961115     O                       LOCLNP            +  1
031800961115     O                       PROLNP            +  1
031900920525     O                       ACORAG              98
032000961001     O          E            TESTA            15
032100961115     O                                            4 'FAX'
032200961115     O                       FAXLNP            +  1
032300961115     O                                         +  1 'TLX'
032400961115     O                       TLXLNP            +  1
032500961115     O                       INDVIA              80
032600961001     O          E            TESTA            16
032700961115     O                                            4 'TEL'
032800961115     O                       TELLNP            +  1
032900050415     O                       INDCAe              59
033000930331     O                       INDCIT            +  1
033100961001     O          E            TESTA            19
033200940926     O                                           24 'Con la presente Vi comun'
033300940926     O                                         +  0 'ichiamo le spedizioni gi'
033400940926     O                                         +  0 'acenti dal'
033500940926     O                       DATAD             +  1 '  /  /    '
033600940926     O                                         +  1 'al'
033700940926     O                       DATAA             +  1 '  /  /    '
033800920525     O          E            RIGA        1
033900920525     O                                           11 'Giacenza '
034000940926     O                                         +  1 'Destinatario'
034100920525     O          E            RIGA        0  1
034200920525     O                                           11 'Giacenza '
034300940926     O                                         +  1 'Destinatario'
034400920525     O          E            RIGA           0
034500920525     O                       GCPFGC        Z      3
034600920525     O                                         +  0 '/'
034700920525     O                       GCPNGC        Z   +  0
034800960124     O                       STPRSD            +  1
034900940926     O                                         +  1 'Sped.'
035000940926     O                       GCPAAS        Z   +  0
035100920522     O                       GCPLNP        Z   +  1
035200920522     O                       GCPNRS        Z   +  1
035300920522     O                       GCPNSP        Z   +  1
035400940926     O                       DATSPE            +  1 '  /  /    '
035500940926     O          E    05      RIGA        1
035600940926     O                       RAGDE2              47
035700920522     O          E            RIGA        1
035800960124     O                       STPIND              47
035900940926     O                                         +  1 'Rif:'
036000940926     O                       BLPRMN        Z   +  0
036100940926     O                       BLPRMA            +  1
036200920522     O          E            RIGA        1
036300960124     O                       STPCAD              21
036400960124     O                       STPLOD            +  1
036500960124     O                       STPPRD            +  1
036600960124     O                       STPNZD            +  1
036700920525     O          E            RIGA        1
036800940926     O                                           18 'Colli:'
036900940926     O                       BLPNCL        Z   +  0
037000940926     O                                         +  1 'Peso:'
037100940926     O                       BLPPKF        2   +  0
037200940926     O                                         +  1 'Vol:'
037300940926     O                       BLPVLF        2   +  0
037400940926     O                                         +  1 'C/assegno:'
037500051115     O                       ar9CAS        2   +  0
037600051115     O                       ar9VCA            +  1
037700920525     O          E            RIGA        1
037800940926     O                                           17 'NOTE.:'
037900940926     O                       NOTE1             +  1
038000940926     O          E    06      RIGA        1
038100940926     O                       NOTE2               69
038200940926     O          E            RIGA        1
038300940926     O                                           24 'Motivo giac:'
038400920522     O                       PDES              +  1
038500920522     O          E            RIGA        1
038600940926     O                       ULTDMC              75
038700920525     O          E            FINE        1
038800920525     O                                           24 'In attesa di Vs. disposi'
038900920525     O                                         +  0 'zioni porgiamo distinti'
039000920525     O                                         +  1 'saluti.'
039100971113     O          E            FINE        3
039200971113     O                       RSUT                73

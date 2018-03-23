000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200940921     H* FNLG81R *----------------------------------------------------*
000300900418     H*                                                              *
000400911017     H*          - CONFERMA GIACENZE APPERTE IN ARRIVO -             *
000500940921     H*  KC FINE LAVORO                                              *
000600900418     H*  19 IND. ERRORE GENERICO VIDEO                               *
000700900418     H*  21 IND. COMODO                                              *
000800900418     H*                                                              *
000900900418     H*--------------------------------------------------------------*
001000940921     FFNLG81D   CF   E             WORKSTN
001100940921     F                                     SFILE(LG81S01:NRR)
001200940921     FFNBLP01L  IF   E           K DISK
001300050214     Ftigcp06L  IF   E           K DISK
001400911010     FTABEL00F  IF   E           K DISK
001500940921     F*--------------------------------------------------------------*
001600940921     D FL£7            S              3    DIM(29)
001700020503     D £1              S              3  0 DIM(30)
001800940921     D £7              S              3  0 DIM(29)
001900950105     D ERR             S             70    DIM(2) CTDATA PERRCD(1)
002000940921     D*--------------------------------------------------------------*
002100940921     D MULT            DS            89    OCCURS(50)
002200900418     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002300900418     D CNCR80        E DS
002400020503     d trul06ds      e ds
002500020503     d  lin                    1     90  0 dim(30)
002600900418     D KPJBA         E DS
002700020730     D §FLE            S              3  0
002800960124     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
002900960124     D FNLG05        E DS                  EXTNAME(FNLG05DS)
003000920110     D                 DS
003100920110     D  GCPMGC                 1      4  0
003200920110     D  MESE                   1      2  0
003300920110     D  GIOR                   3      4  0
003400950105     D WLBDA8          DS
003500950105     D  G02DAT                 1      8  0
003600950105     D  G02INV                 9     16  0
003700950105     D  G02ERR                17     17
003800950105     D  G02TGI                18     22  0
003900950105      *
004000950105     D DATPAR          DS
004100950105     D  GIODAT                 1      8  0
004200950105     D  GIOINV                 9     16  0
004300950105     D  GIOTGI                17     21  0
004400950105     D*
004500950105     D GFES8           DS
004600950105     D  DATADA                 1      8  0
004700950105     D  DATAA                  9     16  0
004800950105     D  GGF                   17     21  0
004900940921     D*---------------------------------------------------------------*
005000940921     D*  Costanti per campo messaggio
005100940921     D COST1           C                   CONST('MESSAG')
005200911017     C*---------------------------------------------------------------*
005300940921     C*  Caricamento videata subfile iniziale
005400940921     C                   EXSR      INZ01
005500940921     C*
005600940921     C     *INKC         DOWEQ     '0'
005700940921     C                   EXSR      GESS01
005800940921     C                   END
005900940921     C*
006000940921     C     FINE          TAG
006100940921     C                   SETON                                        LR
006200940921     C*------------------------------------------------------------*
006300940921     C*  Gestione prima videata
006400940921     C*------------------------------------------------------------*
006500940921     C     GESS01        BEGSR
006600940921     C*
006700940921     C                   WRITE     LG81Z01
006800940921     C                   EXFMT     LG81C01
006900940921     C                   SETOFF                                       28
007000940921     C                   MOVEL     'N'           WSELEZ
007100940921     C* Fine Lavoro
007200940921     C     *INKC         IFEQ      '1'
007300941003     C* Richiamo l'ultima volta la apertura/variaz. giacenze x eseguire
007400941003     C* DLTOVR
007500941003     C                   CLEAR                   FNLG05
007600941003     C                   MOVEL     'U'           D05TRI
007700941003     C                   MOVEL     'S'           D05RIC
007800941003     C                   MOVEL     FNLG05        KPJBU
007900050214     C                   CALL      'FNLG05R'
008000941003     C                   PARM                    KPJBA
008100940921     C                   GOTO      FINVD1
008200940921     C                   END
008300940921     C* Cambio Filiale
008400940921     C     *INKS         IFEQ      '1'
008500940921     C                   Z-ADD     V1CFLE        V2CFLE
008600940921     C                   SETOFF                                       402802
008700940921     C                   EXSR      GESD02
008800950105     C                   MOVEL     'S'           WGCAPE            1
008900960124     C                   EXSR      INZ01
009000940921     C                   GOTO      FINVD1
009100940921     C                   END
009200940921     C* Controlli
009300940921     C     WNOGIA        IFEQ      'N'
009400940921     C                   EXSR      CTRD1
009500940921     C                   END
009600940921     C* Se ho selezionato una o più spedizioni ricarico il subfile
009700940921     C     WSELEZ        IFEQ      'S'
009800940921     C                   EXSR      INZ01
009900940921     C                   END
010000940921     C*
010100940921     C     FINVD1        ENDSR
010200940921     C*------------------------------------------------------------*
010300940921     C*  Gestione seconda videata
010400940921     C*------------------------------------------------------------*
010500940921     C     GESD02        BEGSR
010600940921     C*
010700940921     C*  Fino a che non do CMD12 o cod.filiale esatto loop seconda videata
010800940921     C     *INKL         DOUEQ     '1'
010900940921     C     *IN28         OREQ      '0'
011000940921     C                   EXFMT     LG81D02
011100940921     C                   SETOFF                                       2840
011200940921     C* Ritorno
011300940921     C     *INKL         IFEQ      '0'
011400940921     C                   SETOFF                                       32
011500940921     C     V2CFLE        IFNE      0
011600940921     C     V2CFLE        LOOKUP    £1                                     32
011700940921     C                   END
011800940921     C     *IN32         IFEQ      '0'
011900940921     C                   SETON                                        2840
012000940921     C                   MOVEL     ERR(2)        $MSG
012100940921     C                   ELSE
012200940921     C                   Z-ADD     V2CFLE        V1CFLE
012300940921     C                   EXSR      CAR£7
012400940921     C                   END
012500940921     C                   END
012600940921     C*
012700940921     C                   END
012800940921     C*
012900940921     C                   ENDSR
013000940921     C*------------------------------------------------------------*
013100940921     C* CARICAMENTO SUBFILE
013200940921     C*---------------------------------------------------------------*
013300940921     C     INZ01         BEGSR
013400940921     C                   MOVEL     'N'           WNOGIA
013500940921     C* Pulizia Subfile
013600940921     C                   EXSR      PULIZ
013700940921     C* Verifico se la filiale ne controlla altre
013800940921     C                   DO        X£7           I                 2 0
013900940921     C                   MOVE      £7(I)         KLNP
014000050214     C     Kgcp          CHAIN     tigcp06L                           31
014100940921     C     *IN31         DOWEQ     '0'
014200950105     C                   MOVEL     'N'           WTREVE            1
014300940921     C* Considero le giac. in essere con fase = 10
014400950105     C                   Z-ADD     GCPAAS        KAAS
014500950105     C                   Z-ADD     GCPNRS        KNRS
014600950105     C                   Z-ADD     GCPNSP        KNSP
014700950105     C     KBLP          CHAIN     FNBLP01L                           32
014800940921     C                   EXSR      WTR
014900050214     C     Kgcp          READE     tigcp06L                               31
015000940921     C                   END
015100940921     C                   END
015200940921     C*
015300940921     C     NRR           IFEQ      0
015400940921     C                   MOVEL     'S'           WNOGIA            1
015500940921     C                   ADD       1             NRR
015600940921     C                   WRITE     LG81S01
015700940921     C                   SETON                                        28
015800940921     C                   MOVEL     ERR(1)        $MSG
015900940921     C                   ELSE
016000940921     C                   SETOFF                                       28
016100940921     C                   SETON                                        20
016200940921     C                   END
016300940921     C*
016400940921     C                   ENDSR
016500950105     C*----------------------------------------------------------*
016600940921     C*  Controlli prima videata
016700940921     C*------------------------------------------------------------*
016800940921     C     CTRD1         BEGSR
016900911010     C*
017000940921     C                   READC     LG81S01                                30
017100940921     C     *IN30         DOWEQ     '0'
017200940921     C     V1CSEL        IFEQ      '1'
017300940921     C                   MOVEL     'S'           WSELEZ            1
017400940921     C                   Z-ADD     V1CAAS        D05AAS
017500940921     C                   Z-ADD     V1CLNP        D05LNP
017600940921     C                   Z-ADD     V1CNRS        D05NRS
017700940921     C                   Z-ADD     V1CNSP        D05NSP
017800940921     C                   Z-ADD     V1CAGC        D05AGC
017900940921     C                   Z-ADD     V1CFGC        D05FGC
018000940921     C                   Z-ADD     V1CNGC        D05NGC
018100940921     C                   Z-ADD     0             D05FRG
018200050316     C                   MOVEL     'S'           D05RIC
018300050214     C                   MOVEL     'P'           D05ges
018400940921     C                   MOVEL     FNLG05        KPJBU
018500050214     C                   CALL      'FNLG05R'
018600911010     C                   PARM                    KPJBA
018700940926     C                   MOVEL     ' '           D05TRI
018800911010     C                   END
018900940921     C                   READC     LG81S01                                30
019000911010     C                   END
019100940921     C*
019200940921     C                   ENDSR
019300911017     C*-----------------------------*
019400911017     C* SCRITTURA RECORD NEL SUBFILE
019500911017     C*-----------------------------*
019600911010     C     WTR           BEGSR
019700940921     C                   SETOFF                                       03
019800940921     C*  Controllo se devo visualizzare messaggio o gestire esecuz.disp.
019900940921     C                   Z-ADD     GCPFGC        V1CFGC
020000940921     C                   Z-ADD     GCPAGC        V1CAGC
020100940921     C                   Z-ADD     GCPNGC        V1CNGC
020200940921     C                   MOVEL     GIOR          V1CMGC
020300940921     C                   MOVE      MESE          V1CMGC
020400940921     C                   MOVEL     GCPCMC        V1CCMC
020500940921     C                   Z-ADD     GCPAAS        V1CAAS
020600940921     C                   Z-ADD     GCPLNP        V1CLNP
020700940921     C                   Z-ADD     GCPNRS        V1CNRS
020800940921     C                   Z-ADD     GCPNSP        V1CNSP
020900940921     C* Prelevo mitt. e destinatario dalla bolla
021000940921     C                   MOVEL     BLPRSM        V1CRSM
021100960124     C                   CLEAR                   FNLSN0
021200960124     C                   Z-ADD     BLPAAS        DN0AAS
021300960124     C                   Z-ADD     BLPLNP        DN0LNP
021400960124     C                   Z-ADD     BLPNRS        DN0NRS
021500960124     C                   Z-ADD     BLPNSP        DN0NSP
021600960124     C                   MOVEL     'R'           DN0TRC
021700960124     C                   MOVEL     KPJBU         WSVKPJ          256
021800960124     C                   MOVEL     FNLSN0        KPJBU
021900960124     C                   CALL      'FNLSN0R'
022000960124     C                   PARM                    KPJBA
022100960124     C                   MOVEL     KPJBU         FNLSN0
022200960124     C                   MOVEL     WSVKPJ        KPJBU
022300960124     C     DN0TRV        IFEQ      'S'
022400960124     C                   MOVEL     DN0RSD        V1CRSD
022500960124     C                   ELSE
022600940921     C                   MOVEL     BLPRSD        V1CRSD
022700960124     C                   END
022800911011     C                   ADD       1             NRR               4 0
022900940921     C                   WRITE     LG81S01
023000900509     C                   ENDSR
023100911017     C*----------------*
023200911010     C* PULIZIA SUBFILE
023300911017     C*----------------*
023400911010     C     PULIZ         BEGSR
023500940921     C                   SETOFF                                       2021
023600940921     C                   WRITE     LG81C01
023700911010     C                   Z-ADD     0             NRR
023800940921     C                   SETON                                        21
023900940921     C                   MOVEL     *BLANKS       V1CSEL
024000940921     C                   MOVE      *ZEROS        V1CFGC
024100940921     C                   MOVE      *ZEROS        V1CAGC
024200940921     C                   MOVE      *ZEROS        V1CMGC
024300940921     C                   MOVE      *ZEROS        V1CNGC
024400940921     C                   MOVE      *BLANKS       V1CCMC
024500940921     C                   MOVE      *ZEROS        V1CAAS
024600940921     C                   MOVE      *ZEROS        V1CLNP
024700940921     C                   MOVE      *ZEROS        V1CNRS
024800940921     C                   MOVE      *ZEROS        V1CNSP
024900940921     C                   MOVE      *BLANKS       V1CRSM
025000940921     C                   MOVE      *ZEROS        V1CRSD
025100940921     C                   MOVE      *BLANKS       V1CMSG
025200911017     C                   ENDSR
025300940921     C*----------------------------------------------------------*
025400940921     C* Caricamento tabella £7
025500940921     C*----------------------------------------------------------*
025600940921     C     CAR£7         BEGSR
025700940921     C* Caricamento £7
025800940921     C                   MOVEA     *ZEROS        FL£7
025900940921     C                   MOVEL     *BLANKS       KKEY
026000940921     C                   MOVEL     V1CFLE        KKEY
026100940921     C                   MOVEL     '£7'          KCOD
026200940921     C     KTBL2         CHAIN     TABEL                              31
026300940921     C     *IN31         IFEQ      '0'
026400940921     C     TBLFLG        ANDEQ     ' '
026500940921     C                   MOVEA     TBLUNI        FL£7
026600940921     C                   ELSE
026700940921     C                   MOVE      V1CFLE        FL£7(1)
026800940921     C                   END
026900940921     C*
027000940921     C                   Z-ADD     0             £7
027100940921     C                   Z-ADD     0             X£7               2 0
027200960124     C                   DO        29            X                 3 0
027300940921     C     FL£7(X)       IFNE      *BLANKS
027400940921     C                   ADD       1             X£7               2 0
027500940921     C                   MOVE      FL£7(X)       £7(X£7)
027600940921     C                   END
027700940921     C                   END
027800940921     C*
027900940921     C                   ENDSR
028000940921     C*----------------------------------------------------*
028100940921     C* Operazioni Iniziali
028200940921     C*----------------------------------------------------*
028300940921     C     *INZSR        BEGSR
028400940921     C*
028500940921     C     *ENTRY        PLIST
028600940921     C                   PARM                    KPJBA
028700940921     C                   Z-ADD     1             CODUT
028800940921     C                   CALL      'X§PARUT'
028900940921     C                   PARM                    UTEDSE
029000940921     C     CODUT         CABEQ     -1            FINE
029100940921     C                   MOVEL     RAGUT         V1CRSU
029200940921     C                   MOVEL     RAGUT         RSUT2            25
029300940921     C                   MOVEL     RAGUT         PARDUT           30
029400940921     C                   MOVEL     REC80         CNCR80
029500020730     C     SimTpP        COMP      '2'                                    01
029600020812     C                   Z-ADD     SIMPOU        §FLE
029700940921     C*---------------------------------------------------------------*
029800940921     C*  KLIST
029900940921     C*
030000940921     C     KTBL2         KLIST
030100940921     C                   KFLD                    KKUT
030200940921     C                   KFLD                    KCOD
030300940921     C                   KFLD                    KKEY
030400940921     C*
030500050214     C     Kgcp          KLIST
030600050214     C                   KFLD                    Klnp
030700050214     C                   KFLD                    Kfas
030800050214     c                   eval      kfas = 10
030900940921     C*
031000940921     C     KBLP          KLIST
031100940921     C                   KFLD                    KAAS
031200940921     C                   KFLD                    KLNP
031300940921     C                   KFLD                    KNRS
031400940921     C                   KFLD                    KNSP
031500940921     C*---------------------------------------------------------------*
031600940921     C*  Definizioni variabili
031700940921     C     *LIKE         DEFINE    GCPLNP        KLNP
031800940921     C     *LIKE         DEFINE    TBLKUT        KKUT
031900940921     C     *LIKE         DEFINE    TBLCOD        KCOD
032000940921     C     *LIKE         DEFINE    TBLKEY        KKEY
032100940921     C     *LIKE         DEFINE    BLPAAS        KAAS
032200940921     C     *LIKE         DEFINE    BLPNRS        KNRS
032300940921     C     *LIKE         DEFINE    BLPNSP        KNSP
032400050214     C     *LIKE         DEFINE    gcpfas        Kfas
032500940921     C*---------------------------------------------------------------*
032600940921     C* Inizializzazione variabili
032700940921     C                   CLEAR                   FNLG05
032800940921     C                   Z-ADD     1             KKUT
032900940921     C                   Z-ADD     §FLE          V1CFLE
033000940926     C                   MOVEL     'P'           D05TRI
033100950105     C                   CLEAR                   WLBDA8
033200950105     C                   CLEAR                   DATPAR
033300950105     C                   Z-ADD     1             KKUT
033400950105     C                   TIME                    WHHDAT           14 0
033500950105     C                   MOVE      WHHDAT        G02DAT
033600950105     C                   MOVEL     ' '           G02ERR
033700950105     C                   CALL      'XSRDA8'
033800950105     C                   PARM                    WLBDA8
033900950105     C                   Z-ADD     G02INV        OGGI              8 0
034000950105     C                   MOVEL     'S'           WGCAPE
034100940921     C*---------------------------------------------------------------*
034200940921     C* Caricamento £1
034300020503     c                   clear                   trul06ds
034400020503     c                   eval      d06cod = '£1'
034500020503     c                   movel     v1cfle        d06key
034600020503     c                   movel     trul06ds      kpjbu
034700020503     c                   call      'TRUL06R'
034800020503     c                   parm                    kpjba
034900020503     c                   movel     kpjbu         trul06ds
035000020503     c                   movea     lin           £1
035100020503     C*                  Z-ADD     0             Y                 2 0
035200020503     C*                  MOVEL     '£1'          KCOD
035300020503     C*                  MOVEL     V1CFLE        KKEY
035400020503     C*    KTBL2         CHAIN     TABEL                              31
035500020503     C*    *IN31         IFEQ      '0'
035600020503     C*    TBLFLG        ANDEQ     ' '
035700020503     C*                  MOVEA     TBLUNI        FL£1
035800020503     C*                  DO        29            X
035900020503     C*    FL£1(X)       IFNE      *BLANKS
036000020503     C*                  ADD       1             Y
036100020503     C*                  MOVE      FL£1(X)       £1(Y)
036200020503     C*                  END
036300020503     C*                  END
036400020503     C*                  ELSE
036500020503     C*                  MOVE      V1CFLE        £1(1)
036600020503     C*                  END
036700940921     C*---------------------------------------------------------------*
036800940921     C*  Caricamento tabella £7
036900940921     C                   EXSR      CAR£7
037000940921     C*
037100940921     C                   ENDSR
037200961114**
037300911106Mancano giacenze aperte in arrivo da confermare !!
037400961114Punto operativo errato

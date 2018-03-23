000100900509     H* FLFG19R *----------------------------------------------------*
000200000000     H*-----------------*                                            *
000300900508     H*         - RISTAMPA MODULO AL MITTENTE -FILTRO-               *
000400900423     H*         ---------------------------------------              *
000500000000     H*--------------------------------------------------------------*
000600940922     FFNLG19D   CF   E             WORKSTN
000700940922     FTABEL00F  IF   E           K DISK
000800990628     FFIGCP02L  IF   E           K DISK
000900010528     FAZORG01L  IF   E           K DISK
001000940922     H*--------------------------------------------------------------*
001100940922     D C2F             S              1    DIM(50)
001200940922     D F2F             S              1    DIM(50)
001300940922     D T2F             S              1    DIM(50)
001400940922     D FL£7            S              3    DIM(29)
001500940922     D £7              S              3  0 DIM(29)
001600020430     D £1              S              3  0 DIM(30)
001700961212     D ERR             S             70    DIM(14) CTDATA PERRCD(1)
001800940922     D*--------------------------------------------------------------*
001900940922     D FNLG07        E DS                  EXTNAME(FNLG07DS)
002000940922     D DS2F          E DS
002100940922     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002200940922     D CNCR80        E DS
002300020430     D TR06DS        E DS                  EXTNAME(TRUL06DS)
002400020430     D  LIN                    1     90  0 dim(30)
002500900131     D KPJBA         E DS
002600940922     D WLBDA8          DS
002700940922     D  G02DAT                 1      8  0
002800940922     D  G02INV                 9     16  0
002900940922     D  G02ERR                17     17
003000940922     D  G02TGI                18     22  0
003100020926      *parametri per reperimento coda
003200020926     d finep           s              1
003300020926     d codpo           s              3s 0
003400020926     d tpcoda          s              1
003500020926     d nomcoda         s             10
003600020926     d libcoda         s             10
003700020926     d esito           s              1
003800900131     C*---------------------------------------------------------------*
003900940922     C*  Inizializzazione prima videata
004000940922     C                   EXSR      INZ01
004100940922     C*  Loop gestione videata
004200940922     C     *INKC         DOWEQ     '0'
004300940922     C                   EXSR      GESD01
004400940922     C                   END
004500940922     C*
004600940922     C     FINE          TAG
004700940922     C                   SETON                                        LR
004800940922     C*-----------------------------------------------------*
004900940922     C*  Gestione della prima videata
005000940922     C*------------------------------------------------------*
005100940922     C     GESD01        BEGSR
005200940922     C*
005300920311     C                   Z-ADD     0             DATA
005400940922     C                   EXFMT     LG19D01
005500940922     C                   SETOFF                                       404142
005600940922     C                   SETOFF                                       434445
005700961212     C                   SETOFF                                       464728
005800961212     C   02              SETOFF                                       02
005900900423     C   KC              GOTO      FINE
006000961212     C*   SE RICHIESTO CAMBIO FILIALE VADO A FINE VIDEATA
006100961212     C     *INKS         IFEQ      '1'
006200961212     C                   SETON                                        1802
006300961212     C                   GOTO      FINVD1
006400961212     C                   END
006500900508     C** CONTROLLI
006600940922     C                   EXSR      CTR01
006700920311     C* PASSAGGIO PARAMETRI IN KPJBU E LANCIO BATCH DI STAMPA LETTERE
006800940922     C     *INKF         IFEQ      '1'
006900940922     C     *IN28         ANDEQ     '0'
007000961212     C                   MOVE      V1CFIL        D07FIL
007100961212     C                   MOVE      VRACC         D07RAC
007200940922     C                   MOVE      VTIP          D07TIP
007300940922     C                   Z-ADD     VAGCI         D07AGC
007400940922     C                   Z-ADD     VFGCI         D07FGC
007500940922     C                   Z-ADD     VNGCI         D07NGC
007600940922     C                   Z-ADD     DATA          D07DAT
007700020926     C                   MOVE      nomcoda       D07PRF
007800940922     C                   MOVEL     FNLG07        KPJBU
007900940922     C                   MOVEL     'LG07'        KCOAZ
008000900131     C     KNMUS         IFNE      *ALL'1'
008100900131     C                   CALL      'BCH10'
008200000000     C                   PARM                    KPJBA
008300900131     C                   ELSE
008400940922     C                   CALL      'FNLG07R'
008500000000     C                   PARM                    KPJBA
008600890905     C                   END
008700940922     C                   EXSR      INZ01
008800000000     C                   END
008900940922     C*
009000961212     C     FINVD1        ENDSR
009100940922     C*-----------------------------------------------------*
009200940922     C*  Controlli sulla prima videata
009300940922     C*------------------------------------------------------*
009400940922     C     CTR01         BEGSR
009500961212     C*
009600961212     C* CONTROLLO FILIALE
009700020730     C     V1CFIL        IFNE      SimPOU
009800961212     C     V1CFIL        LOOKUP    £1                                     32
009900961212     C     *IN32         IFEQ      '1'
010000961212     C                   Z-ADD     1             KKUT
010100961212     C                   MOVEA     *ZEROS        FL£7
010200961212     C                   MOVEL     *BLANKS       KKEY
010300020730     C                   MOVEL     SimPOU        KKEY
010400961212     C                   MOVEL     '£7'          KCOD
010500961212     C     KTBL2         CHAIN     TABEL                              31
010600961212     C     *IN31         IFEQ      '0'
010700961212     C     TBLFLG        ANDEQ     ' '
010800961212     C                   MOVEA     TBLUNI        FL£7
010900961212     C                   ELSE
011000020730     C                   MOVE      SimPOU        FL£7(1)
011100961212     C                   END
011200961212     C*
011300961212     C                   Z-ADD     0             £7
011400961212     C                   Z-ADD     0             X£7               2 0
011500961212     C                   DO        29            X
011600961212     C     FL£7(X)       IFNE      *BLANKS
011700961212     C                   ADD       1             X£7               2 0
011800961212     C                   MOVE      FL£7(X)       £7(X£7)
011900961212     C                   END
012000961212     C                   END
012100961212     C                   ELSE
012200961212     C                   SETON                                        2847
012300961212     C                   MOVEL     ERR(13)       $MSG
012400961212     C                   GOTO      FINCT1
012500961212     C                   END
012600961212     C                   END
012700940922     C*
012800940922     C* SE SELEZIONATE ENTRAMBE LE OPZIONI ERRORE
012900940922     C     VSEL          IFNE      *BLANKS
013000940922     C     VSEL2         ANDNE     *BLANKS
013100940922     C                   SETON                                        284044
013200940922     C                   MOVEL     ERR(1)        $MSG
013300940922     C                   GOTO      FINCT1
013400940922     C                   END
013500940922     C* SE NON SELEZIONATA UNA OPZIONE ERRORE
013600940922     C     VSEL          IFEQ      *BLANKS
013700940922     C     VSEL2         ANDEQ     *BLANKS
013800940922     C                   SETON                                        284044
013900940922     C                   MOVEL     ERR(2)        $MSG
014000940922     C                   GOTO      FINCT1
014100940922     C                   END
014200940922     C*
014300940923     C     VSEL          IFEQ      '1'
014400940922     C     VAGCI         IFEQ      0
014500940922     C                   SETON                                        2841
014600940922     C                   MOVEL     ERR(3)        $MSG
014700940922     C                   GOTO      FINCT1
014800940922     C                   END
014900940922     C* Se anno a 2 lo porto a  4
015000940922     C                   MOVEL     VAGCI         WCOM2             2 0
015100940922     C     WCOM2         IFEQ      0
015200940922     C     VAGCI         IFGT      60
015300940922     C                   ADD       1900          VAGCI
015400940922     C                   ELSE
015500940922     C                   ADD       2000          VAGCI
015600940922     C                   END
015700940922     C                   END
015800940922     C     VFGCI         IFEQ      0
015900940922     C                   SETON                                        2842
016000940922     C                   MOVEL     ERR(4)        $MSG
016100940922     C                   GOTO      FINCT1
016200940922     C                   END
016300940922     C     VNGCI         IFEQ      0
016400940922     C                   SETON                                        2843
016500940922     C                   MOVEL     ERR(6)        $MSG
016600940922     C                   GOTO      FINCT1
016700940922     C                   END
016800940922     C*  Controllo se esiste giacenza
016900940922     C                   Z-ADD     VAGCI         KAGC
017000940922     C                   Z-ADD     VFGCI         KFGC
017100940922     C                   Z-ADD     VNGCI         KNGC
017200940922     C                   Z-ADD     0             KFRG
017300990628     C     KGCP          CHAIN     FIGCP02L                           31
017400940922     C     *IN31         IFEQ      '1'
017500940922     C                   SETON                                        284142
017600940922     C                   SETON                                        43
017700940922     C                   MOVEL     ERR(7)        $MSG
017800940922     C                   GOTO      FINCT1
017900940922     C                   END
018000940922     C     GCPFAS        IFNE      20
018100940922     C                   SETON                                        284142
018200940922     C                   SETON                                        43
018300940922     C                   MOVEL     ERR(8)        $MSG
018400940922     C                   GOTO      FINCT1
018500940922     C                   END
018600940922     C                   END
018700940922     C*
018800940923     C     VSEL2         IFEQ      '1'
018900940922     C* CONTROLLO SE LA DATA IMMESSA E' VALIDA
019000940922     C                   MOVE      VDATA         G02DAT
019100940922     C                   MOVE      *BLANKS       G02ERR
019200940922     C                   CALL      'XSRDA8'
019300940922     C                   PARM                    WLBDA8
019400940922     C     G02ERR        IFEQ      '1'
019500940922     C                   SETON                                        2845
019600940922     C                   MOVEL     ERR(9)        $MSG
019700940922     C                   GOTO      FINCT1
019800940922     C                   ELSE
019900940922     C                   MOVEL     G02INV        DATA              8 0
020000940922     C                   MOVEL     G02DAT        VDATA
020100940922     C                   END
020200940922     C                   END
020300940922     C* Controllo tipo comunicazione richiesto
020400940922     C*§  Ricerca in tabel00f
020500940922     C     VTIP          IFEQ      '?'
020600940922     C                   MOVEL     '2F'          §COD              2
020700940922     C                   MOVEL     *BLANK        §KEY              8
020800940922     C                   MOVEL     *BLANK        §DES             35
020900940922     C                   MOVE      *BLANK        VTIP
021000940922     C                   CALL      'X§TABER'
021100940922     C                   PARM                    CODUT
021200940922     C                   PARM                    §COD
021300940922     C                   PARM                    §KEY
021400940922     C                   PARM                    §DES
021500940922     C                   MOVEL     §KEY          VTIP
021600940922     C                   END
021700940922     C     VTIP          IFNE      *BLANKS
021800940922     C                   Z-ADD     1             X
021900940922     C     VTIP          LOOKUP    C2F(X)                                 32
022000940922     C     *IN32         IFEQ      '0'
022100940922     C                   SETON                                        2846
022200940922     C                   MOVEL     ERR(10)       $MSG
022300940922     C                   ELSE
022400940922     C                   MOVEL     TBLUNI        DS2F
022500940922     C     F2F(X)        IFEQ      'N'
022600940922     C                   SETON                                        2846
022700940922     C                   MOVEL     ERR(12)       $MSG
022800940922     C                   GOTO      FINCT1
022900940922     C                   END
023000020730     C     SimTpP        IFEQ      '2'
023100940922     C     T2F(X)        ANDEQ     'P'
023200940922     C                   SETON                                        2846
023300940922     C                   MOVEL     ERR(11)       $MSG
023400961212     C                   GOTO      FINCT1
023500940922     C                   END
023600940922     C                   END
023700940922     C                   END
023800961212     C*
023900961212     C     VSEL2         IFEQ      '1'
024000961212     C     VTIP          ANDEQ     *BLANKS
024100961212     C                   SETON                                        2846
024200961212     C                   MOVEL     ERR(14)       $MSG
024300961212     C                   GOTO      FINCT1
024400961212     C                   END
024500940922     C*
024600940922     C     FINCT1        ENDSR
024700940922     C*------------------------------------------------------*
024800940922     C*  Inizializzazione della prima videata
024900940922     C*------------------------------------------------------*
025000940922     C     INZ01         BEGSR
025100940922     C*
025200940922     C                   MOVEL     'S'           VRACC
025300020730     C     SimTpP        IFEQ      '2'
025400940923     C                   MOVEL     'N'           VRACC
025500940923     C                   END
025600940922     C                   Z-ADD     0             VAGCI
025700940922     C                   Z-ADD     0             VFGCI
025800940922     C                   Z-ADD     0             VNGCI
025900940922     C                   MOVEL     *BLANKS       VSEL
026000940922     C                   MOVEL     *BLANKS       VSEL2
026100940922     C                   Z-ADD     0             VDATA
026200940922     C                   SETOFF                                       404142
026300940922     C                   SETOFF                                       434445
026400961212     C                   SETOFF                                       462818
026500020730     C                   MOVE      SimPOU        V1CFIL
026600940922     C*
026700940922     C                   ENDSR
026800940922     C*-----------------------------------------------------*
026900940922     C*  Operazioni iniziali
027000940922     C*------------------------------------------------------*
027100940922     C     *INZSR        BEGSR
027200940922     C*
027300940922     C     *ENTRY        PLIST
027400940922     C                   PARM                    KPJBA
027500940922     C                   Z-ADD     1             CODUT
027600940922     C                   CALL      'X§PARUT'
027700940922     C                   PARM                    UTEDSE
027800940922     C     CODUT         CABEQ     -1            FINE
027900940922     C                   MOVEL     RAGUT         RSUT             20
028000940922     C                   MOVEL     REC80         CNCR80
028100020730     C     SimTpP        COMP      '2'                                0101
028200940922     C*---------------------------------------------------------------*
028300940922     C*  Like KLIST
028400940922     C     KTBL1         KLIST
028500940922     C                   KFLD                    KKUT
028600940922     C                   KFLD                    KCOD
028700940922     C*
028800940922     C     KTBL2         KLIST
028900940922     C                   KFLD                    KKUT
029000940922     C                   KFLD                    KCOD
029100940922     C                   KFLD                    KKEY
029200940922     C*
029300940922     C     KGCP          KLIST
029400940922     C                   KFLD                    KAGC
029500940922     C                   KFLD                    KFGC
029600940922     C                   KFLD                    KNGC
029700940922     C                   KFLD                    KFRG
029800940922     C*---------------------------------------------------------------*
029900940922     C*  Like DEFN
030000940922     C     *LIKE         DEFINE    TBLKUT        KKUT
030100940922     C     *LIKE         DEFINE    TBLCOD        KCOD
030200940922     C     *LIKE         DEFINE    TBLKEY        KKEY
030300940922     C     *LIKE         DEFINE    GCPAGC        KAGC
030400940922     C     *LIKE         DEFINE    GCPFGC        KFGC
030500940922     C     *LIKE         DEFINE    GCPNGC        KNGC
030600940922     C     *LIKE         DEFINE    GCPFRG        KFRG
030700940922     C*----------------------------------------------------*
030800940922     C* Pulizia dati DS
030900940922     C                   CLEAR                   FNLG07
031000940922     C                   MOVEL     'S'           D07RIS
031100940922     C*-------------------------------------------------------*
031200940922     C* Caricamento £7
031300940922     C                   Z-ADD     1             KKUT
031400940922     C                   MOVEA     *ZEROS        FL£7
031500940922     C                   MOVEL     *BLANKS       KKEY
031600020730     C                   MOVEL     SimPOU        KKEY
031700940922     C                   MOVEL     '£7'          KCOD
031800940922     C     KTBL2         CHAIN     TABEL                              31
031900940922     C     *IN31         IFEQ      '0'
032000940922     C     TBLFLG        ANDEQ     ' '
032100940922     C                   MOVEA     TBLUNI        FL£7
032200940922     C                   ELSE
032300020730     C                   MOVE      SimPOU        FL£7(1)
032400940922     C                   END
032500940922     C*
032600940922     C                   Z-ADD     0             £7
032700940922     C                   Z-ADD     0             X£7               2 0
032800940922     C                   DO        29            X
032900940922     C     FL£7(X)       IFNE      *BLANKS
033000940922     C                   ADD       1             X£7               2 0
033100940922     C                   MOVE      FL£7(X)       £7(X£7)
033200940922     C                   END
033300940922     C                   END
033400961212     C*-------------------------------------------------------*
033500971202     C                   Z-ADD     1             KKUT
033600020926      *reperimento coda fax sostituisce tabella 3i
033700020926     c                   move      'L'           finep
033800020926     c                   z-add     simpou        codpo
033900020926     c                   move      '2'           tpcoda
034000020926     c                   clear                   nomcoda
034100020926     c                   clear                   libcoda
034200020926     c                   clear                   esito
034300020926     c                   call      'TRULOUTQ'
034400020926     c                   parm                    finep
034500020926     c                   parm                    codpo
034600020926     c                   parm                    tpcoda
034700020926     c                   parm                    nomcoda
034800020926     c                   parm                    libcoda
034900020926     c                   parm                    esito
035000020927     c                   if        esito <> '0' or nomcoda = *blank
035100020927     c                   eval      nomcoda = 'QDKT'
035200020927     c                   endif
035300020926     c
035400961212     C* Caricamento £1
035500020430     C                   CLEAR                   TR06DS
035600020430     C                   MOVEL     '£1'          D06COD
035700020430     C                   MOVEL     simfel        D06KEY
035800020430     C                   MOVEL     TR06DS        KPJBU
035900020430     C                   CALL      'TRUL06R'
036000020430     C                   PARM                    KPJBA
036100020430     C                   MOVEL     KPJBU         TR06DS
036200020430     C                   MOVEA     LIN           £1
036300940922     C*-------------------------------------------------------*
036400940922     C* Caricamento 2F
036500940922     C                   Z-ADD     0             X                 2 0
036600940922     C                   MOVEL     '2F'          KCOD
036700940922     C     KTBL1         CHAIN     TABEL                              31
036800940922     C     *IN31         DOWEQ     '0'
036900940922     C     X             ANDLT     50
037000940922     C     TBLFLG        IFEQ      ' '
037100031027     C                   MOVEL     TBLUNI        DS2F
037200031027      * solo se non è riservato al CED lo carica
037300031027     c                   if        §2Fris = *blank
037400940922     C                   ADD       1             X
037500940922     C                   MOVEL     §2FFTM        F2F(X)
037600940922     C                   MOVEL     §2FFTP        T2F(X)
037700940922     C                   MOVEL     TBLKEY        C2F(X)
037800031027     c                   end
037900940922     C                   END
038000940922     C     KTBL1         READE     TABEL                                  31
038100940922     C                   END
038200940922     C*
038300940922     C                   ENDSR
038400961212**
038500940922Selezionare una sola delle opzioni                                     01
038600940922Obbligatoria una scelta                                                02
038700940922Anno obbligatorio                                                      03
038800961113Punto operativo di apertura giacenza obbligatoria                      04
038900961113Punto operativo di apertura giacenza non in gestione                   05
039000940922Numero giacenza obbligatorio                                           06
039100940922Giacenza inesistente                                                   07
039200940922Impossibile eseguire ristampa: fase giacenza errata                    08
039300940922Data ristampa errata                                                   09
039400940922Tipo comunicazione non presente in tabella                             10
039500961113Tipo comunicazione non ammesso per punto operativo di secondo livello  11
039600940922Tipo di comunicazione richiesto non prevede la stampa del mod.Mittente 12
039700961212Punto operativo non in gestione su questo as                           13
039800961212Se selez. ristampa di un giorno obbligatorio specificare tipo modulo   14

000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200160301     H* FNLG41R *----------------------------------------------------*
000300900418     H*                                                              *
000400931119     H*       - LISTE CONTROLLO GIACENZE IN PARTENZA                 *
000500900423     H*                                                              *
000600900418     H*--------------------------------------------------------------*
000700050218     Ftigcp07L  IF   E           K DISK
000800050406     Ftigcp09L  IF   E           K DISK
000900050218     F                                     RENAME(TIGCP000:TIGCP00§)
001000050218     Ftignp01L  IF   E           K DISK
001100900514     FCNACO00F  IF   E           K DISK
001200950110     FCNIND00F  IF   E           K DISK
001300950110     FFNBLP01L  IF   E           K DISK
001400990929     FFIAR601L  IF   E           K DISK
001500051115     FFiar901L  IF   E           K DISK
001600050218     Ftigap01L  IF   E           K DISK
001700900515     FTABEL00F  IF   E           K DISK
001800990730     FTITGC01L  IF   E           K DISK
001900950110     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002000950110     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002100950110      *----------------------------------------------------------------
002200020228     E* TESTO LETTERA
002300961209     D ST              S             66    DIM(20) CTDATA PERRCD(1)
002400020228     E* COD. NAZIONE
002500011217     D CNZ             S              3    DIM(500)
002600020228     E* FLAG ITALIA/ESTERO
002700011217     D ITA             S              1    DIM(500)
002800020228     E* COD. TAB. 7Q
002900950111     D C7Q             S              2    DIM(100)
003000020228     E* FLAG TAB. 7Q
003100950111     D D7Q             S             89    DIM(100)
003200020228     E* COD. TAB. 3A
003300990929     D C3A             S              2    DIM(200)
003400020228     E* TIPO 1° BOLLA
003500990929     D T3A             S              1    DIM(200)
003600950110     D*--------------------------------------------------------------------*
003700950111     D DS7Q          E DS
003800950111     D DS2A          E DS
003900990929     D DS3A          E DS
004000950110     D DS15          E DS
004100911220     D DS2G          E DS
004200050210     D DS20          E DS
004300000202     D OG143         E DS
004400160301     D trulc7ds      e ds
004500160301     D trul27ds1     e ds
004600020305     D FNLG40        E DS                  EXTNAME(FNLG40DS)
004700960124     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
004800150514     D fnlv59ds      E DS
004900160301     d  KS_CT                 74    118    dim(15)
005000160301     d  KS_pr                119    163    dim(15)
005100160301     d  KS_ie                164    178    dim(15)
005200160301     d
005300950116     D TRUL52        E DS                  EXTNAME(TRUL52DS)
005400950110     D*--------------------------------------------------------------------*
005500900423     D TCUDS           DS
005600900423     D  F3                     3      3
005700900423     D  F4                     4      4
005800900423     D  F56                    5      6
005900950110     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
006000900423     D  TCU                  398    697
006100900418     D                                     DIM(50)
006200900423     D  KCU                  698    847P 0
006300900418     D                                     DIM(50)
006400900418     D                                     PACKEVEN
006500950110     D CNCR80        E DS
006600950110     D WLBDA8          DS
006700950110     D  G02DAT                 1      8  0
006800950110     D  G02INV                 9     16  0
006900950110     D  G02ERR                17     17
007000950110     D  G02TGI                18     22  0
007100940329     D*-------------
007200990906     D* DS PER ROUTINE 'XGGFEST'
007300940329     D*-------------
007400950110     D WDAT8           DS
007500950110     D  DATADA                 1      8  0
007600950110     D  DATAA                  9     16  0
007700950110     D  GGF                   17     21  0
007800900418     D KPJBA         E DS
007900950110      *----------------------------------------------------------------
008000950110     IAZORG01L  AA
008100950110     I                             P    4    5 0ORGFIL
008200950110     I                                  6    6  ORGFVA
008300971113     I                                 11   11  ORGFL1
008400950110     I                                 14   34  ORGDES
008500000202     I                               3776 3800  ORGDE3
008600900514     C*---------------------------------------------------------------*
008700950110     C*  Controllo il tipo di stampa scelta
008800950110     C     D40SC1        CASEQ     '1'           LISTA1
008900950110     C     D40SC2        CASEQ     '1'           LISTA2
009000950110     C     D40SC3        CASEQ     '1'           LISTA3
009100950110     C     D40SC4        CASEQ     '1'           LISTA4
009200950110     C     D40SC5        CASEQ     '1'           LISTA5
009300900514     C                   END
009400950110     C*
009500900514     C     FINE          TAG
009600160301     C                   CLEAR                   TRULc7ds
009700160301     C                   MOVE      'C'           Ic7tla
009800160301     C                   CALL      'TRULC7R'
009900160301     C                   PARM                    TRULc7ds
010000160301
010100160301     C                   CLEAR                   TRUL27ds1
010200160301     C                   MOVE      'C'           I27tla
010300160301     C                   CALL      'TRUL27R1'
010400160301     C                   PARM                    TRUL27ds1
010500900514     C                   SETON                                        LR
010600900514     C*---------------------------------------------------------------*
010700950110     C*   OPERAZIONI INIZIALI                                         *
010800900514     C*---------------------------------------------------------------*
010900950110     C     *INZSR        BEGSR
011000950110     C*
011100900514     C     *ENTRY        PLIST
011200900514     C                   PARM                    KPJBA
011300950110     C                   MOVEL     KPJBU         FNLG40
011400950110     C*---------------------------------------------------------------*
011500900514     C                   Z-ADD     1             CODUT
011600950110     C                   CALL      'X§PARUT'
011700950110     C                   PARM                    UT§DSE
011800950110     C                   MOVEL     REC80         CNCR80
011900900514     C                   MOVEL     RAGUT         RSUT             20
012000950110     C                   Z-ADD     0             SAVCCR
012100950110     C*--- RICERCA CAPOCONTI------------------------------------------*
012200950110     C                   DO        50            X                 2 0
012300950110     C                   MOVE      TCU(X)        TCUDS
012400950110     C     F56           IFEQ      'CG'
012500950110     C     F4            ANDEQ     '1'
012600950110     C     F3            ANDEQ     '0'
012700950110     C                   Z-ADD     KCU(X)        KCI               4 0
012800950110     C                   END
012900950110     C                   END
013000950110     C*---------------------------------------------------------------*
013100950110     C*  Definizione chiavi
013200950110     C     KACO          KLIST
013300950110     C                   KFLD                    CODUT
013400950110     C                   KFLD                    KCI
013500950110     C                   KFLD                    GCPSCM
013600950110     C*
013700950110     C     KTGC          KLIST
013800950110     C                   KFLD                    KKSC
013900950110     C                   KFLD                    KCTR
014000950110     C                   KFLD                    KPRG
014100950110     C*
014200950110     C     KGAP          KLIST
014300950110     C                   KFLD                    GCPAAS
014400950110     C                   KFLD                    GCPLNP
014500950110     C                   KFLD                    GCPNRS
014600950110     C                   KFLD                    GCPNSP
014700990929     C     KAR6          KLIST
014800990929     C                   KFLD                    GCPAAS
014900990929     C                   KFLD                    GCPLNP
015000990929     C                   KFLD                    GCPNRS
015100990929     C                   KFLD                    GCPNSP
015200990929     C                   KFLD                    KTRC
015300950110     C*
015400950110     C     KTAB          KLIST
015500950110     C                   KFLD                    CODUT
015600950110     C                   KFLD                    COD               2
015700950110     C                   KFLD                    KEY               8
015800950110     C*
015900950110     C     KTAB1         KLIST
016000950110     C                   KFLD                    CODUT
016100950110     C                   KFLD                    COD               2
016200950110     C*
016300950110     C     KNOT          KLIST
016400950110     C                   KFLD                    GCPAGC
016500950110     C                   KFLD                    GCPFGC
016600950110     C                   KFLD                    GCPNGC
016700950110     C                   KFLD                    GCPFRG
016800050218     C                   KFLD                    §FAS              3 0
016900020822     C*
017000020822     C     Kgcp          KLIST
017100020822     C                   KFLD                    simpou
017200020822     C                   KFLD                    d40md1
017300020822     C     Kgcp5         KLIST
017400020822     C                   KFLD                    simpou
017500020822     C                   KFLD                    §FAS
017600950110     C*---------------------------------------------------------------*
017700950110     C     *LIKE         DEFINE    TGCKSC        KKSC
017800961209     C     *LIKE         DEFINE    GCPSCM        SCMPRE
017900990929     C     *LIKE         DEFINE    AR6TRC        KTRC
018000990929     C     *LIKE         DEFINE    TGCCTR        KCTR
018100950110     C     *LIKE         DEFINE    TGCPRG        KPRG
018200950110     C     *LIKE         DEFINE    G02TGI        §GGD
018300950110     C     *LIKE         DEFINE    G02TGI        §GGA
018400950110     C     *LIKE         DEFINE    G02TGI        §GGDI
018500950110     C*---------------------------------------------------------------*
018600960216     C                   CLEAR                   FNLSN0
018700950110     C                   SETOFF                                       404142
018800950110     C                   SETOFF                                       43
018900950111     C                   SETON                                        OF
019000950110     C                   TIME                    WTIME             6 0
019100950110     C                   TIME                    WHHDA1           12 0
019200950110     C                   TIME                    WHHDAT           14 0
019300950110     C                   MOVE      WHHDAT        OGGI              8 0
019400950110     C                   MOVE      *ALL'-'       CMP1            198
019500950110     C                   MOVE      *ALL'-'       CMP2            198
019600990929     C*
019700990929     C* Carico DATI TABELLA CODICI BOLLA
019800990929     C                   Z-ADD     0             §F                3 0
019900990929     C                   MOVEL     '3A'          COD
020000990929     C                   MOVEL     *BLANK        KEY
020100990929     C     KTAB1         CHAIN     TABEL                              21
020200990929     C     *IN21         DOWEQ     '0'
020300990929     C     §F            ANDLE     200
020400990929     C     TBLFLG        IFEQ      *BLANK
020500990929     C                   ADD       1             §F
020600990929     C                   MOVEL     TBLUNI        DS3A
020700990929     C                   MOVEL     §3ATB1        T3A(§F)
020800990929     C                   MOVEL     TBLKEY        C3A(§F)
020900990929     C                   END
021000990929     C     KTAB1         READE     TABEL                                  21
021100990929     C                   END
021200950110     C*---------------------------------------------------------------*
021300950110     C                   ENDSR
021400950110     C*---------------------------------------------------------------*
021500950110     C*   Giacenze in attesa di disposizioni mittente                 *
021600950110     C*---------------------------------------------------------------*
021700950110     C     LISTA1        BEGSR
021800961209     C                   CLEAR                   CLIEST
021900950110     C*
022000950116     C* Carico DATI TABELLA PARTICOLARITA'
022100950116     C                   Z-ADD     0             §F                3 0
022200950116     C                   MOVEL     '7Q'          COD
022300950116     C                   MOVEL     *BLANK        KEY
022400950116     C     KTAB1         CHAIN     TABEL                              21
022500950116     C     *IN21         DOWEQ     '0'
022600950116     C     §F            ANDLE     100
022700950116     C     TBLFLG        IFEQ      *BLANK
022800950116     C                   ADD       1             §F
022900950116     C                   MOVEL     TBLUNI        D7Q(§F)
023000950116     C                   MOVEL     TBLKEY        C7Q(§F)
023100950116     C                   END
023200950116     C     KTAB1         READE     TABEL                                  21
023300950116     C                   END
023400950116     C*
023500950110     C* Carico NAZIONI
023600950110     C                   Z-ADD     0             §F                3 0
023700950110     C                   MOVEL     '15'          COD
023800950110     C                   MOVEL     *BLANK        KEY
023900950110     C     KTAB1         CHAIN     TABEL                              21
024000950110     C     *IN21         DOWEQ     '0'
024100011221     C     §F            ANDLE     500
024200950110     C     TBLFLG        IFEQ      *BLANK
024300950110     C                   ADD       1             §F
024400950110     C                   MOVEL     TBLUNI        DS15
024500950110     C                   MOVEL     TBLKEY        CNZ(§F)
024600950110     C                   MOVEL     §15ITA        ITA(§F)
024700950110     C                   END
024800950110     C     KTAB1         READE     TABEL                                  21
024900950110     C                   END
025000950110     C*  Cerco gg. rientro su tabella
025100950110     C                   SETON                                        40
025200950110     C                   MOVEL     '2G'          COD
025300950110     C                   MOVEL     *BLANKS       KEY
025400950110     C                   MOVEL     1             KEY
025500950110     C                   CLEAR                   DS2G
025600950110     C     KTAB          CHAIN     TABEL00F                           70
025700950110     C  N70              MOVEL     TBLUNI        DS2G
025800950110     C*
025900950110     C*  Giro la data del giorno
026000950110     C                   MOVE      OGGI          G02DAT
026100950110     C                   MOVEL     *BLANK        G02ERR
026200950110     C                   CALL      'XSRDA8'
026300950110     C                   PARM                    WLBDA8
026400950110     C                   Z-ADD     G02TGI        §GGD
026500950110     C                   Z-ADD     G02INV        DATAA
026600950110     C*---------------------------------------------------------------*
026700950110     C*  Esamino giacenze fino a fine lista o codice mittente superiore
026800950110     C*  a quello selezionato
026900050218     C     kgcp          SETLL     tigcp07L
027000050218     C     simpou        READe     tigcp07L                               21
027100950110     C     *IN21         DOWEQ     '0'
027200950111     C     GCPSCM        ANDLE     D40MA1
027300020228     c* disp. da immettere e giac. in essere e non annullata
027400950110     C     GCPDDM        IFEQ      0
027500950110     C     GCPFRG        ANDEQ     0
027600950110     C     GCPATB        ANDEQ     ' '
027700950110     C*
027800950110     C     GCPFAS        IFEQ      20
027900950110     C     GCPFAS        OREQ      26
028000950110     C                   EXSR      STPL01
028100950110     C                   END
028200950110     C                   END
028300050218     C     simpou        READe     tigcp07L                               21
028400950110     C                   END
028500950110     C*
028600950110     C                   ENDSR
028700950110     C*---------------------------------------------------------------*
028800950110     C*   Giacenze in attesa di chiusura                              *
028900950110     C*---------------------------------------------------------------*
029000950110     C     LISTA2        BEGSR
029100961209     C                   CLEAR                   CLIEST
029200950110     C*
029300950110     C                   SETON                                        41
029400950110     C                   MOVE      OGGI          G02DAT
029500950110     C                   MOVEL     *BLANK        G02ERR
029600950110     C                   CALL      'XSRDA8'
029700950110     C                   PARM                    WLBDA8
029800950110     C                   Z-ADD     G02TGI        §GGD
029900950110     C                   Z-ADD     G02INV        DATAA
030000950110     C*---------------------------------------------------------------*
030100950110     C*  Esamino giacenze fino a fine lista da dispos. trasmesse
030200950110     C                   Z-ADD     30            §FAS
030300050406     C     Kgcp5         SETLL     tigcp09L
030400050406     C     simpou        READe     tigcp09L                               21
030500950110     C     *IN21         DOWEQ     '0'
030600020228     c* ricevuta chiusura
030700950111     C     GCPFAS        ANDLT     40
030800020228     c* luogo dep. >= da
030900950110     C     GCPCCR        IFGE      D40FD2
031000020228     c* luogo dep. <= da
031100950110     C     GCPCCR        ANDLE     D40FA2
031200020228     c* trasmesse disp.
031300950110     C     GCPDLD        ANDGT     0
031400020228     c* giacenze da chiudere
031500050406     C*****GCPDCG        ANDEQ     0
031600020228     c* giacenze in essere
031700950110     C     GCPFRG        ANDEQ     0
031800020228     c* giacenze non annullata
031900950110     C     GCPATB        ANDEQ     ' '
032000950110     C                   EXSR      STPL02
032100950110     C                   END
032200050406     C     simpou        READe     tigcp09l                               21
032300950110     C                   END
032400950110     C*
032500950110     C                   ENDSR
032600950110     C*---------------------------------------------------------------*
032700950110     C*   Giacenze in attesa di rientro                               *
032800950110     C*---------------------------------------------------------------*
032900950110     C     LISTA3        BEGSR
033000961209     C                   CLEAR                   CLIEST
033100950110     C*
033200950110     C*  Giro la data del giorno
033300950110     C                   MOVE      OGGI          G02DAT
033400950110     C                   MOVEL     *BLANK        G02ERR
033500950110     C                   CALL      'XSRDA8'
033600950110     C                   PARM                    WLBDA8
033700950110     C                   Z-ADD     G02TGI        §GGD
033800950110     C                   Z-ADD     G02INV        AMGUDT            8 0
033900950110     C*
034000950110     C                   SETON                                        42
034100950111     C                   Z-ADD     30            §FAS
034200050406     C     kgcp5         SETLL     tigcp09l
034300050406     C     simpou        READe     tigcp09l                               21
034400950110     C     *IN21         DOWEQ     '0'
034500020228     c* ricevuta chiusura
034600950111     C     GCPFAS        ANDLT     40
034700020228     c* giacenza da chiudere
034800950110     C     GCPDCG        IFLE      0
034900020228     c* flag di rientrato
035000950110     C     GCPDIS        ANDEQ     'R'
035100020228     C*Trasmesse disp.
035200950111     C     GCPDLD        ANDGT     0
035300020228     C* Giac. in essere
035400950110     C     GCPFRG        ANDEQ     0
035500020228     C*  Giac. annullata
035600950110     C     GCPATB        ANDEQ     ' '
035700020228     C*  Dispos. non eseg.
035800950110     C     GCPDED        ANDLE     AMGUDT
035900950110     C                   EXSR      STPL03
036000950110     C                   END
036100050406     C     simpou        READe     tigcp09l                               21
036200950110     C                   END
036300950110     C*
036400950110     C                   ENDSR
036500950110     C*---------------------------------------------------------------*
036600950110     C*   Giacenze aperte in ritardo                                  *
036700950110     C*---------------------------------------------------------------*
036800950110     C     LISTA4        BEGSR
036900961209     C                   CLEAR                   CLIEST
037000950110     C*
037100950111     C* Carico DATI TABELLA PARTICOLARITA'
037200950111     C                   Z-ADD     0             §F                3 0
037300950111     C                   MOVEL     '7Q'          COD
037400950111     C                   MOVEL     *BLANK        KEY
037500950111     C     KTAB1         CHAIN     TABEL                              21
037600950111     C     *IN21         DOWEQ     '0'
037700950111     C     §F            ANDLE     100
037800950111     C     TBLFLG        IFEQ      *BLANK
037900950111     C                   ADD       1             §F
038000950111     C                   MOVEL     TBLUNI        D7Q(§F)
038100950112     C                   MOVEL     TBLKEY        C7Q(§F)
038200950111     C                   END
038300950111     C     KTAB1         READE     TABEL                                  21
038400950111     C                   END
038500950111     C*  GIRO DATE LIMITI
038600950110     C                   Z-ADD     D40DD4        G02INV
038700950110     C                   MOVEL     '3'           G02ERR
038800950110     C                   CALL      'XSRDA8'
038900950110     C                   PARM                    WLBDA8
039000950110     C                   Z-ADD     G02DAT        DRD4              8 0
039100950110     C                   Z-ADD     D40DA4        G02INV
039200950110     C                   MOVEL     '3'           G02ERR
039300950110     C                   CALL      'XSRDA8'
039400950110     C                   PARM                    WLBDA8
039500950110     C                   Z-ADD     G02DAT        DRA4              8 0
039600950110     C*
039700950110     C                   SETON                                        43
039800050406     c     simpou        setll     tigcp09l
039900050406     C     simpou        READe     tigcp09L                               21
040000950110     C     *IN21         DOWEQ     '0'
040100020228     C* Luogo dep. >= da
040200950110     C     GCPCCR        IFGE      D40FD4
040300020228     C* Luogo dep. <= a
040400950110     C     GCPCCR        ANDLE     D40FA4
040500020228     C* Giac.da chiudere
040600950110     C     GCPDCG        ANDEQ     0
040700020228     C* Giac.in essere
040800950110     C     GCPFRG        ANDEQ     0
040900020228     C* Giac.non annul.
041000950110     C     GCPATB        ANDEQ     ' '
041100950110     C                   EXSR      STPL04
041200950110     C                   END
041300050406     C     simpou        READe     tigcp09l                               21
041400950110     C                   END
041500950110     C*
041600950110     C                   ENDSR
041700950110     C*---------------------------------------------------------------*
041800950110     C*   Giacenze chiuse in ritardo dalla partenza                   *
041900950110     C*---------------------------------------------------------------*
042000950110     C     LISTA5        BEGSR
042100961209     C                   CLEAR                   CLIEST
042200950110     C*
042300950110     C*  GIRO DATE LIMITI
042400950110     C                   Z-ADD     D40DD5        G02INV
042500950110     C                   MOVEL     '3'           G02ERR
042600950110     C                   CALL      'XSRDA8'
042700950110     C                   PARM                    WLBDA8
042800950110     C                   Z-ADD     G02DAT        DRD5              8 0
042900950110     C                   Z-ADD     D40DA5        G02INV
043000950110     C                   MOVEL     '3'           G02ERR
043100950110     C                   CALL      'XSRDA8'
043200950110     C                   PARM                    WLBDA8
043300950110     C                   Z-ADD     G02DAT        DRA5              8 0
043400950110     C*
043500950110     C                   SETON                                        44
043600950110     C                   MOVE      40            §FAS
043700050406     C     kgcp5         SETLL     tigcp09L
043800050406     C     simpou        READe     tigcp09L                               21
043900950110     C     *IN21         DOWEQ     '0'
044000020228     C* Linea part. da
044100950110     C     GCPLNP        IFGE      D40FD5
044200020228     C* Linea part. a
044300950110     C     GCPLNP        ANDLE     D40DA5
044400020228     C* Data chius.giac da
044500950110     C     GCPDCG        ANDGE     D40DD5
044600020228     C* Data chius.giac a
044700950110     C     GCPDCG        ANDLE     D40DA5
044800020228     C* no annullata
044900950110     C     GCPATB        ANDEQ     ' '
045000950110     C                   EXSR      STPL05
045100950110     C                   END
045200050406     C     simpou        READe     tigcp09L                               21
045300950110     C                   END
045400950110     C*
045500950110     C                   ENDSR
045600900514     C*---------------------------------------------------------------*
045700950110     C*   Esegui stampa lista giacenze in attesa disposiz.            *
045800900514     C*---------------------------------------------------------------*
045900950110     C     STPL01        BEGSR
046000900515     C*
046100050218     C                   Z-ADD     0             DATDUR            8 0
046200050218     C* DATA ULT.RIAP-
046300950110     C                   MOVEL     GCPDUR        G02INV
046400050406     c                   if        gcpriap = 'R'
046500950116     C                   MOVEL     '3'           G02ERR
046600950116     C                   CALL      'XSRDA8'
046700950116     C                   PARM                    WLBDA8
046800950116     C                   Z-ADD     G02DAT        DATDUR
046900050218     c                   end
047000900515     C*
047100950110     C* Calcolo giorni di rientro su tariffa cliente
047200990929     C                   Z-ADD     0             AR6IFT
047300051115     C                   Z-ADD     0             ar9CAS
047400051115     C                   MOVEL     *BLANKS       ar9VCA
047500991119     C                   MOVEL     *BLANKS       AR6DIV
047600051115     C     KGAP          CHAIN     Fiar901L                           70
047700051115     C                   MOVE      ar9CAS        COM3              3 0
047800950111     C     COM3          COMP      000                                    06
047900950110     C     KGAP          CHAIN     FNBLP01L                           70
048000990929     C* Verifico se esiste la spedizione è in porto FRANCO
048100990929     C* per reperire eventuali dati di tassazione
048200990929     C     *IN70         IFEQ      '0'
048300020305     C                   Z-ADD     1             Y                 3 0
048400990929     C     BLPCBO        LOOKUP    C3A(Y)                                 70
048500990929     C     T3A(Y)        IFEQ      'F'
048600990929     C     *IN70         ANDEQ     '1'
048700990929     C                   MOVEL     '1'           KTRC
048800990929     C     KAR6          CHAIN     FIAR601L                           70
048900990929     C                   END
049000990929     C                   END
049100991119     C                   Z-ADD     AR6IFT        STPIFT           11 2
049200991119     C                   MOVE      STPIFT        COM2              2 0
049300991119     C     COM2          COMP      000                                    07
049400950116     C*  Controllo se partenza doveva attendere X gg. prima conferma giac.
049500950116     C     BLPGGA        IFNE      *BLANKS
049600950116     C                   Z-ADD     1             Y
049700950116     C     BLPGGA        LOOKUP    C7Q(Y)                                 40
049800950116     C     *IN40         IFEQ      '1'
049900950116     C                   MOVEL     D7Q(Y)        DS7Q
050000950116     C     §7Q3EV        IFNE      *BLANKS
050100950116     C     §7Q3MF        ANDEQ     'A'
050200950116     C                   MOVEL     '2A'          COD
050300950116     C                   MOVEL     *BLANK        KEY
050400950116     C                   MOVEL     §7Q3EV        KEY
050500950116     C     KTAB          CHAIN     TABEL                              23
050600950116     C     *IN23         IFEQ      '0'
050700950116     C                   MOVEL     TBLUNI        DS2A
050800950116     C     §2AGLA        IFGT      0
050900950116     C                   CLEAR                   TRUL52
051000950116     C                   Z-ADD     §2AGLA        D52GIO
051100950116     C                   Z-ADD     G02INV        D52DIN
051200950116     C                   CALL      'TRUL52R'
051300950116     C                   PARM                    TRUL52
051400950116     C     D52ERR        IFEQ      *BLANKS
051500950116     C                   Z-ADD     D52DFI        G02INV            8 0
051600950116     C                   END
051700950116     C                   END
051800950116     C                   END
051900950116     C                   END
052000950116     C*
052100950116     C                   END
052200950116     C                   END
052300950116     C*
052400950116     C                   MOVEL     '3'           G02ERR
052500950116     C                   CALL      'XSRDA8'
052600950116     C                   PARM                    WLBDA8
052700950116     C                   Z-ADD     G02TGI        §GGA
052800950116     C                   Z-ADD     G02INV        DATADA
052900950111     C* Ricerco decodifica cliente mittente
053000950110     C                   EXSR      DECODI
053100961209      * SE CLIENTE ESTERO  STAMPO IN LINGUA
053200961209     C                   CLEAR                   CLIEST
053300961209     C                   Z-ADD     1             Y
053400961209     C     BLPNZM        LOOKUP    CNZ(Y)                                 40
053500971113     C* Aggancio azorg lnp x sapere se lnp Estera
053600971113     C                   MOVEL     *BLANKS       ORGFL1
053700971113     C     BLPLNP        CHAIN     AZORG01L                           30
053800020717     C  n30              MOVEL     ORGDE3        OG143
053900961209     C     ITA(Y)        IFNE      'I'
054000020717     C     §OgNTW        oreq      'FED'
054100020717     C     §OgNTW        oreq      'EEX'
054200020717     C     §OgNTW        oreq      'EUP'
054300961209     C                   MOVEL     'S'           CLIEST            1
054400961209     C                   ENDIF
054500950111     C*
054600150514     C                   CLEAR                   fnlv59ds
054700950110     C                   MOVEL     BLPAAS        DATDSP            8 0
054800950110     C                   MOVE      BLPMGS        DATDSP
054900950110     C                   MOVEL     ' '           WTROVA            1
055000950110     C                   MOVE      GCPSCM        §QUATR            4 0
055100160301     C* chiamo trul27r per reperire il fie
055200160301     C                   CLEAR                   TRUL27ds1
055300021118     C                   MOVEL     BLPctr        I27ctb
055400020304     C                   MOVEL     BLPTSP        I27TSP
055500020304     C                   MOVE      BLPLNA        I27LNA
055600020304     C                   MOVE      BLPLNP        I27LNP
055700020304     C                   MOVE      gcpscm        I27CLI
055800160301     C                   CALL      'TRUL27R1'
055900160301     C                   PARM                    TRUL27ds1
056000020304     c*
056100950110     C     §QUATR        IFNE      *ALL'8'
056200950110     C     §QUATR        ANDNE     *ALL'9'
056300950110     C     §QUATR        ANDNE     *ZEROS
056400150514     C                   MOVEL     ' '           ilv59TBO
056500150514     C                   Z-ADD     DATDSP        ilv59DSP
056600150514     C                   MOVE      BLPCTR        ilv59CTR
056700950116     C                   MOVE      GCPSCM        KKSC
056800150514     C                   MOVE      KKSC          ilv59KSC
056900150514     C                   MOVE      KCI           ilv59KCI
057000150514     C                   MOVEL     o27fie        ilv59FIE
057100150514     C                   MOVEL     BLPTSP        ilv59TSP
057200150514     C                   MOVEL     'S'           ilv59CON
057300150514     c                   eval      ilv59ta2='B'
057400150514     c                   eval      ilv59blo='S'
057500150514     C                   CALL      'FNLV59R'
057600150514     C                   PARM                    fnlv59ds
057700950110     C*
057800150514     C     olv59ERR      IFEQ      *BLANKS
057900150514     C     olv59tks      ANDEQ     'S'
058000950110     C*  Se ho trovato tariffa in vigore mi posiziono sul file
058100950110     C*  delle tariffe giacenze con il proggressivo giusto
058200150514     C                   MOVE      olv59prg      KPRG
058300150514     C                   MOVE      ilv59ksc      KKSC
058400150514     C                   MOVE      olv59ctr      KCTR
058500990730     C     KTGC          CHAIN     TITGC01L                           31
058600950110     C     *IN31         IFEQ      '0'
058700950116     C     TGCGGR        ANDNE     *ZEROS
058800950110     C                   MOVE      TGCGGR        WGGR              2 0
058900950110     C                   MOVEL     'S'           WTROVA
059000950110     C                   END
059100950110     C                   END
059200950110     C                   END
059300160301     c
059400950110     C*  Se non trovato gg. rientro su tariffa cliente li cerco
059500950110     C*  sulla tariffa di cartello
059600950110     C     WTROVA        IFNE      'S'
059700160301     c* Carico la cartello :
059800160301     c*  - per 8888 col fie della bolla
059900160301     c*  - per tariffe cliente esistente col fie della tariffa
060000160301     c                   clear                   trulc7ds
060100160301    2C     olv59ERR      ifeq      *blanks
060200160301     C     olv59tks      andeq     'S'
060300160301     c                   z-add     1             indx              3 0
060400160301     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
060500160301    3c                   if        indx>0 and %editc(olv59prg:'X')=
060600160301     c                             ks_pr(indx)
060700160301     c                   eval      ic7tfie=ks_ie(indx)
060800160301   x3c                   else
060900160302     c                   eval      ic7tntw=o27ntw
061000160301    3c                   endif
061100160301     c
061200160301   x2c                   else
061300160302     c*
061400160302     c                   eval      ic7tntw=o27ntw
061500160301    2c                   endif
061600160301     c
061700160301     C                   move      blpctr        Ic7ctb
061800160301     C                   MOVE      datdsp        Ic7DTA
061900160301     c                   call      'TRULC7R'
062000160301     c                   parm                    trulc7ds
062100160301     c
062200160301     C                   MOVE      oc7PRGc       KPRG
062300160301     C                   MOVE      oc7KSCc       KKSC
062400160301     C                   MOVE      oc7CTRc       KCTR
062500990730     C     KTGC          CHAIN     TITGC01L                           31
062600950110     C     *IN31         IFEQ      '0'
062700950116     C     TGCGGR        ANDNE     *ZEROS
062800950110     C                   MOVE      TGCGGR        WGGR
062900950110     C                   MOVEL     'S'           WTROVA
063000950110     C                   END
063100950110     C                   END
063200970618     C*  Se non ho impostato i gg rientro li prendo dalla tabella 2G
063300970618     C*
063400950110     C     WTROVA        IFNE      'S'
063500950110     C                   MOVE      §2GGGL        WGGR
063600950110     C                   END
063700961204     C*
063800020410     C     O27FIE        IFNE      'I'
063900020304     C                   SELECT
064000020304      * Se è da DPD
064100050210     C     o27ntw        WHENEQ    'DPD'
064200020304     C     WGGR          IFGT      §2GGDP
064300020304     C                   MOVE      §2GGDP        WGGR
064400020304     C                   END
064500020304      * Se è FEDEX
064600050210     C     o27ntw        WHENEQ    'FED'
064700020304     C     WGGR          IFGT      §2GGFE
064800020304     C                   MOVE      §2GGFE        WGGR
064900020304     C                   END
065000020304      * Se è estero
065100050210     C     o27ntw        WHENEQ    'EEX'
065200050210     C     o27ntw        OREQ      'EUP'
065300020304     C     WGGR          IFGT      §2GGRE
065400020304     C                   MOVE      §2GGRE        WGGR
065500020304     C                   END
065600040112      *
065700020304     C                   ENDSL
065800010921     C                   ENDIF
065900040217      *
066000050210     C*  Se ci sono eccezioni ai resi x decorsi termini sul cliente
066100050210     C*   sui giorni da prendere
066200050210      * Se è un estero Euroexpress
066300050210     C     O27FIE        IFNE      'I'
066400050210     C     o27ntw        andeq     'EEX'
066500050210     C                   MOVEL     '20'          COD
066600050210     C                   MOVEL     *blank        KEY
066700050210     C                   MOVEL     gcpSCM        KEY
066800050210     c     ktab          chain     tabel00f
066900050210     c                   if        %found(tabel00f)
067000050210     c                   movel     tbluni        ds20
067100050210     C                   Z-ADD     §20GRE        wGGR
067200050210     c                   endif
067300050210     c                   END
067400050210     C*
067500900515     C     §GGD          SUB       §GGA          §GGDI
067600950110     C     §GGDI         IFGT      WGGR
067700940329     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
067800940329     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
067900950110     C** Aggancio file storico
068000950110     C                   MOVEL     BLPRSD        DESTI            15
068100960124     C                   MOVE      BLPRMN        RIFNUM           15 0
068200960124     C*  Controllo se esiste destinatario finale  e se si lo sostit.
068300960124     C                   Z-ADD     GCPAAS        DN0AAS
068400960124     C                   Z-ADD     GCPLNP        DN0LNP
068500960124     C                   Z-ADD     GCPNRS        DN0NRS
068600960124     C                   Z-ADD     GCPNSP        DN0NSP
068700960124     C                   MOVEL     'R'           DN0TRC
068800960124     C                   MOVEL     FNLSN0        KPJBU
068900960124     C                   CALL      'FNLSN0R'
069000960124     C                   PARM                    KPJBA
069100960124     C                   MOVEL     KPJBU         FNLSN0
069200960124     C     DN0TRV        IFEQ      'S'
069300960124     C                   MOVEL     DN0RSD        DESTI
069400960124     C                   END
069500900515     C*
069600950111     C                   EXSR      MOTAPE
069700900515     C                   EXSR      RIBDAT
069800900515     C                   EXSR      STAMP
069900950110     C                   END
070000900515     C*
070100950110     C                   ENDSR
070200950110     C*---------------------------------------------------------------*
070300950110     C*   Esegui stampa lista giacenze in attesa di chiusura          *
070400950110     C*---------------------------------------------------------------*
070500950110     C     STPL02        BEGSR
070600950110     C*
070700050218     C*  Se la data di eseguibilità è superiore a quella di immissione disp.
070800950111     C*  parto da quella
070900950111     C     GCPDED        IFGT      GCPDLD
071000020228     C* DATA LT.FIL.ARR
071100950111     C                   MOVE      GCPDED        G02INV
071200950111     C                   MOVEL     '3'           G02ERR
071300950111     C                   CALL      'XSRDA8'
071400950111     C                   PARM                    WLBDA8
071500950111     C                   Z-ADD     G02TGI        §GGA
071600950111     C                   Z-ADD     G02INV        DATADA
071700950111     C                   ELSE
071800020228     C*DATA LT.FIL.ARR
071900950111     C                   MOVE      GCPDLD        G02INV
072000950111     C                   MOVEL     '3'           G02ERR
072100950111     C                   CALL      'XSRDA8'
072200950111     C                   PARM                    WLBDA8
072300950111     C                   Z-ADD     G02TGI        §GGA
072400950111     C                   Z-ADD     G02INV        DATADA
072500950111     C                   END
072600900515     C*
072700900515     C     §GGD          SUB       §GGA          §GGDI
072800950110     C     §GGDI         IFGT      D40GG2
072900940329     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
073000940329     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
073100940329     C                   EXSR      FEST
073200940330     C                   SUB       GGF           §GGDI
073300950110     C     §GGDI         IFGT      D40GG2
073400050218     C*
073500950110     C     KGAP          CHAIN     FNBLP01L                           22
073600920330     C  N22              MOVEL     BLPRSD        DESTI            15
073700950110     C   22              MOVEL     *BLANKS       DESTI
073800960124     C  N22              MOVE      BLPRMN        RIFNUM
073900960124     C   22              MOVE      *ZEROS        RIFNUM
074000960124     C*  Controllo se esiste destinatario finale  e se si lo sostit.
074100960124     C                   Z-ADD     GCPAAS        DN0AAS
074200960124     C                   Z-ADD     GCPLNP        DN0LNP
074300960124     C                   Z-ADD     GCPNRS        DN0NRS
074400960124     C                   Z-ADD     GCPNSP        DN0NSP
074500960124     C                   MOVEL     'R'           DN0TRC
074600960124     C                   MOVEL     FNLSN0        KPJBU
074700960124     C                   CALL      'FNLSN0R'
074800960124     C                   PARM                    KPJBA
074900960124     C                   MOVEL     KPJBU         FNLSN0
075000960124     C     DN0TRV        IFEQ      'S'
075100960124     C                   MOVEL     DN0RSD        DESTI
075200960124     C                   END
075300900515     C*
075400900515     C                   EXSR      DECODI
075500911209     C                   EXSR      RICNOT
075600900515     C                   EXSR      RIBDAT
075700900515     C                   EXSR      STAMP1
075800900515     C                   END
075900900515     C                   END
076000950110     C*
076100950110     C                   ENDSR
076200950110     C*---------------------------------------------------------------*
076300950110     C*   Esegui stampa lista giacenze in attesa di rientro           *
076400950110     C*---------------------------------------------------------------*
076500950110     C     STPL03        BEGSR
076600950110     C*
076700990929     C                   Z-ADD     0             AR6IFT
076800051115     C                   Z-ADD     0             ar9CAS
076900051115     C                   MOVEL     *BLANKS       ar9VCA
077000991119     C                   MOVEL     *BLANKS       AR6DIV
077100051115     C     KGAP          CHAIN     Fiar901L                           22
077200051115     C                   MOVE      ar9CAS        COM3              3 0
077300950111     C     COM3          COMP      000                                    06
077400950110     C     KGAP          CHAIN     FNBLP01L                           22
077500990929     C  N22              MOVEL     BLPRSD        DESTI            15
077600990929     C   22              MOVEL     *BLANKS       DESTI
077700990929     C  N22              MOVE      BLPRMN        RIFNUM
077800990929     C   22              MOVE      *ZEROS        RIFNUM
077900990929     C* Verifico se esiste la spedizione è in porto FRANCO
078000990929     C* per reperire eventuali dati di tassazione
078100990929     C     *IN22         IFEQ      '0'
078200990929     C                   Z-ADD     1             Y
078300990929     C     BLPCBO        LOOKUP    C3A(Y)                                 22
078400990929     C     T3A(Y)        IFEQ      'F'
078500990929     C     *IN22         ANDEQ     '1'
078600990929     C                   MOVEL     '1'           KTRC
078700990929     C     KAR6          CHAIN     FIAR601L                           22
078800990929     C                   END
078900990929     C                   END
079000991119     C                   Z-ADD     AR6IFT        STPIFT           11 2
079100991119     C                   MOVE      STPIFT        COM2              2 0
079200991119     C     COM2          COMP      000                                    07
079300960124     C*  Controllo se esiste destinatario finale  e se si lo sostit.
079400960124     C                   Z-ADD     GCPAAS        DN0AAS
079500960124     C                   Z-ADD     GCPLNP        DN0LNP
079600960124     C                   Z-ADD     GCPNRS        DN0NRS
079700960124     C                   Z-ADD     GCPNSP        DN0NSP
079800960124     C                   MOVEL     'R'           DN0TRC
079900960124     C                   MOVEL     FNLSN0        KPJBU
080000960124     C                   CALL      'FNLSN0R'
080100960124     C                   PARM                    KPJBA
080200960124     C                   MOVEL     KPJBU         FNLSN0
080300960124     C     DN0TRV        IFEQ      'S'
080400960124     C                   MOVEL     DN0RSD        DESTI
080500960124     C                   END
080600900515     C*
080700900515     C                   EXSR      DECODI
080800900515     C                   EXSR      RIBDAT
080900900515     C                   EXSR      STAMP1
081000950110     C*
081100950110     C                   ENDSR
081200950110     C*---------------------------------------------------------------*
081300950110     C*   Esegui stampa lista giacenze aperte in ritardo              *
081400950110     C*---------------------------------------------------------------*
081500950110     C     STPL04        BEGSR
081600950110     C*
081700950111     C                   Z-ADD     0             DATDUR
081800050218     C                   MOVE      GCPDUR        datagc            8 0
081900950111     C                   MOVE      GCPDUR        G02INV
082000050406     C     GCPriap       IFEQ      'R'
082100950116     C                   MOVEL     '3'           G02ERR
082200950116     C                   CALL      'XSRDA8'
082300950116     C                   PARM                    WLBDA8
082400950116     C                   Z-ADD     G02DAT        DATDUR
082500950111     C                   END
082600050218     C*
082700990929     C                   Z-ADD     0             AR6IFT
082800051115     C                   Z-ADD     0             ar9CAS
082900051115     C                   MOVEL     *BLANKS       ar9VCA
083000991119     C                   MOVEL     *BLANKS       AR6DIV
083100051115     C     KGAP          CHAIN     Fiar901L                           22
083200051115     C                   MOVE      ar9CAS        COM3              3 0
083300950116     C     COM3          COMP      000                                    06
083400950116     C     KGAP          CHAIN     FNBLP01L                           22
083500950116     C  N22              MOVEL     BLPRSD        DESTI            15
083600950116     C   22              MOVEL     *BLANKS       DESTI
083700950116     C  N22              MOVE      BLPRMN        RIFNUM
083800950116     C   22              MOVE      *ZEROS        RIFNUM
083900990929     C* Verifico se esiste la spedizione è in porto FRANCO
084000990929     C* per reperire eventuali dati di tassazione
084100990929     C     *IN22         IFEQ      '0'
084200990929     C                   Z-ADD     1             Y
084300990929     C     BLPCBO        LOOKUP    C3A(Y)                                 22
084400990929     C     T3A(Y)        IFEQ      'F'
084500990929     C     *IN22         ANDEQ     '1'
084600990929     C                   MOVEL     '1'           KTRC
084700990929     C     KAR6          CHAIN     FIAR601L                           22
084800990929     C                   END
084900990929     C                   END
085000991119     C                   Z-ADD     AR6IFT        STPIFT
085100991119     C                   MOVE      STPIFT        COM2              2 0
085200991119     C     COM2          COMP      000                                    07
085300960124     C*  Controllo se esiste destinatario finale  e se si lo sostit.
085400960124     C                   Z-ADD     GCPAAS        DN0AAS
085500960124     C                   Z-ADD     GCPLNP        DN0LNP
085600960124     C                   Z-ADD     GCPNRS        DN0NRS
085700960124     C                   Z-ADD     GCPNSP        DN0NSP
085800960124     C                   MOVEL     'R'           DN0TRC
085900960124     C                   MOVEL     FNLSN0        KPJBU
086000960124     C                   CALL      'FNLSN0R'
086100960124     C                   PARM                    KPJBA
086200960124     C                   MOVEL     KPJBU         FNLSN0
086300960124     C     DN0TRV        IFEQ      'S'
086400960124     C                   MOVEL     DN0RSD        DESTI
086500960124     C                   END
086600950116     C*
086700950116     C*  Controllo se partenza doveva attendere X gg. prima conferma giac.
086800950116     C     BLPGGA        IFNE      *BLANKS
086900950116     C                   Z-ADD     1             Y
087000950116     C     BLPGGA        LOOKUP    C7Q(Y)                                 40
087100950116     C     *IN40         IFEQ      '1'
087200950116     C                   MOVEL     D7Q(Y)        DS7Q
087300950116     C     §7Q3EV        IFNE      *BLANKS
087400950116     C     §7Q3MF        ANDEQ     'A'
087500950116     C                   MOVEL     '2A'          COD
087600950116     C                   MOVEL     *BLANK        KEY
087700950116     C                   MOVEL     §7Q3EV        KEY
087800950116     C     KTAB          CHAIN     TABEL                              23
087900950116     C     *IN23         IFEQ      '0'
088000950116     C                   MOVEL     TBLUNI        DS2A
088100950116     C     §2AGLA        IFGT      0
088200950116     C                   CLEAR                   TRUL52
088300950116     C                   Z-ADD     §2AGLA        D52GIO
088400950116     C                   Z-ADD     G02INV        D52DIN
088500950116     C                   CALL      'TRUL52R'
088600950116     C                   PARM                    TRUL52
088700950116     C     D52ERR        IFEQ      *BLANKS
088800950116     C                   Z-ADD     D52DFI        G02INV            8 0
088900950116     C                   END
089000950116     C                   END
089100950116     C                   END
089200950116     C                   END
089300950111     C*
089400950116     C                   END
089500950116     C                   END
089600950116     C*
089700950111     C                   MOVEL     '3'           G02ERR
089800950111     C                   CALL      'XSRDA8'
089900950111     C                   PARM                    WLBDA8
090000950111     C                   Z-ADD     G02TGI        §GGD
090100950111     C                   Z-ADD     G02INV        DATADA
090200950111     C*
090300950111     C     G02INV        IFGE      D40DD4
090400950111     C     G02INV        ANDLE     D40DA4
090500900515     C*
090600020228     C* DATA STAMPA MOD.MITT
090700920915     C                   MOVEL     GCPDMM        G02INV
090800900517     C                   MOVEL     '3'           G02ERR
090900950111     C                   CALL      'XSRDA8'
091000950110     C                   PARM                    WLBDA8
091100900515     C                   Z-ADD     G02TGI        §GGA
091200940330     C                   Z-ADD     G02INV        DATAA
091300900515     C*
091400940330     C     §GGA          SUB       §GGD          §GGDI
091500950110     C     §GGDI         IFGT      D40GG4
091600940329     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
091700940329     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
091800940329     C                   EXSR      FEST
091900950110     C                   SUB       GGF           §GGDI
092000950111     C     §GGDI         IFGT      D40GG4
092100950111     C*  Ricerco codice motivo apertura giacenza
092200950111     C                   EXSR      MOTAPE
092300900515     C                   EXSR      DECODI
092400900515     C                   EXSR      RIBDAT
092500950111     C                   EXSR      STAMP
092600900515     C                   END
092700900515     C                   END
092800950110     C*
092900950110     C                   END
093000950110     C*
093100950110     C                   ENDSR
093200950110     C*---------------------------------------------------------------*
093300950110     C*   Esegui stampa lista giacenze aperte in ritardo              *
093400950110     C*---------------------------------------------------------------*
093500950110     C     STPL05        BEGSR
093600950110     C*
093700950110     C     KGAP          CHAIN     FNBLP01L                           22
093800921013     C  N22              MOVEL     BLPRSD        DESTI            15
093900921013     C   22              MOVEL     *BLANKS       DESTI
094000950110     C  N22              MOVE      BLPRMN        RIFNUM
094100950110     C   22              MOVE      *ZEROS        RIFNUM
094200960124     C*  Controllo se esiste destinatario finale  e se si lo sostit.
094300960124     C                   Z-ADD     GCPAAS        DN0AAS
094400960124     C                   Z-ADD     GCPLNP        DN0LNP
094500960124     C                   Z-ADD     GCPNRS        DN0NRS
094600960124     C                   Z-ADD     GCPNSP        DN0NSP
094700960124     C                   MOVEL     'R'           DN0TRC
094800960124     C                   MOVEL     FNLSN0        KPJBU
094900960124     C                   CALL      'FNLSN0R'
095000960124     C                   PARM                    KPJBA
095100960124     C                   MOVEL     KPJBU         FNLSN0
095200960124     C     DN0TRV        IFEQ      'S'
095300960124     C                   MOVEL     DN0RSD        DESTI
095400960124     C                   END
095500050218     C*   CHIUSURA IN PART.
095600930316     C                   MOVE      GCPDBG        G02INV
095700930316     C                   MOVE      *ZEROS        G02DAT
095800921014     C                   MOVEL     3             G02ERR
095900950110     C                   CALL      'XSRDA8'
096000950110     C                   PARM                    WLBDA8
096100950110     C                   Z-ADD     G02DAT        DATDLM            8 0
096200020228     C*   CHIUSURA IN arr.
096300930316     C                   MOVE      GCPDCG        G02INV
096400930316     C                   MOVEL     '3'           G02ERR
096500950111     C                   CALL      'XSRDA8'
096600950110     C                   PARM                    WLBDA8
096700921013     C                   Z-ADD     G02TGI        §GGD
096800940329     C                   Z-ADD     G02INV        DATADA
096900950110     C                   Z-ADD     G02DAT        DATDCG            8 0
097000921013     C*
097100930316     C     GCPDBG        IFEQ      0
097200921014     C                   SETON                                        88
097300921014     C                   MOVE      UDATE         G02DAT
097400921014     C                   MOVEL     *BLANK        G02ERR
097500950110     C                   CALL      'XSRDA8'
097600950110     C                   PARM                    WLBDA8
097700020228     C* UDATE É POSTO IN G02INV IN FORMATO AA/MM/GG.
097800921013     C                   ELSE
097900020228     C* DATA CONFERMA CHIUS. in partenza
098000930316     C                   MOVEL     GCPDBG        G02INV
098100930316     C                   END
098200921013     C                   MOVEL     '3'           G02ERR
098300950110     C                   CALL      'XSRDA8'
098400950110     C                   PARM                    WLBDA8
098500921013     C                   Z-ADD     G02TGI        §GGA
098600940329     C                   Z-ADD     G02INV        DATAA
098700921013     C*
098800921013     C     §GGA          SUB       §GGD          §GGDI
098900950110     C     §GGDI         IFGT      D40GG5
099000940329     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
099100940329     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
099200940329     C                   EXSR      FEST
099300940330     C                   SUB       GGF           §GGDI
099400950110     C     §GGDI         IFGT      D40GG5
099500921013     C                   EXSR      DECODI
099600921013     C                   EXSR      RIBDAT
099700950110     C                   EXSR      STAMP
099800921014     C                   SETOFF                                       88
099900921013     C                   END
100000921013     C                   END
100100950110     C*
100200950110     C                   ENDSR
100300900514     C*---------------------------------------------------------------*
100400900514     C*   DECODIFICA MITTENTE
100500900514     C*---------------------------------------------------------------*
100600900514     C     DECODI        BEGSR
100700900514     C** SE IL CODICE è CODIFICATO PRENDO I DATI DELLA RAG.SOC.
100800900514     C** DAL PDC SE è CODICE 88 O 99 PRENDO I DATI O DALL'ANAGRAFICA
100900900514     C** D'APPOGGIO SE ESISTE O DALLO STORICO BOLLE
101000900514     C                   MOVE      GCPSCM        CODMIT            4 0
101100950111     C                   MOVEL     *BLANKS       RAGMIT           25
101200950110     C                   MOVEL     BLPNZM        RAGNZM
101300900514     C     CODMIT        IFNE      8888
101400900514     C     CODMIT        ANDNE     9999
101500900514     C     KACO          CHAIN     CNACO000                           25
101600900514     C  N25              DO
101700950110     C                   MOVEL     ACORAG        RAGMIT
101800950110     C     KACO          CHAIN     CNIND000                           25
101900950110     C  N25              MOVEL     INDSTA        RAGNZM            3
102000900514     C                   END
102100900514     C                   ELSE
102200050218     C     KGAP          CHAIN     tigap000                           70
102300900515     C  N70              DO
102400950110     C                   MOVEL     GAPRSM        RAGMIT
102500950110     C                   MOVEL     GAPNZM        RAGNZM
102600900515     C                   END
102700900515     C   70              DO
102800950110     C                   MOVEL     BLPRSM        RAGMIT
102900900514     C                   END
103000900515     C                   END
103100950110     C*
103200900514     C                   ENDSR
103300900515     C*---------------------------------------------------------------*
103400900515     C*   RIBALTAMENTO DATE
103500900515     C*---------------------------------------------------------------*
103600900515     C     RIBDAT        BEGSR
103700900515     C* DATA GIACENZA
103800900515     C                   MOVEL     GCPAGC        DATAGC
103900900515     C                   MOVE      GCPMGC        DATAGC
104000950110     C                   Z-ADD     DATAGC        G02INV
104100950110     C                   MOVEL     '3'           G02ERR
104200950110     C                   CALL      'XSRDA8'
104300950110     C                   PARM                    WLBDA8
104400950110     C                   MOVE      G02DAT        DATAGC
104500900515     C* DATA SPEDIZIONE
104600950110     C                   MOVEL     BLPAAS        DATSPE            8 0
104700900515     C                   MOVE      BLPMGS        DATSPE
104800950110     C                   Z-ADD     DATSPE        G02INV
104900950110     C                   MOVEL     '3'           G02ERR
105000950110     C                   CALL      'XSRDA8'
105100950110     C                   PARM                    WLBDA8
105200950110     C                   Z-ADD     G02DAT        DATSPE
105300900516     C* DATA COMUNICAZIONE INTERNA
105400950110     C                   Z-ADD     GCPDXD        G02INV
105500950110     C                   MOVEL     '3'           G02ERR
105600950110     C                   CALL      'XSRDA8'
105700950110     C                   PARM                    WLBDA8
105800950110     C                   Z-ADD     G02DAT        DATDXD            8 0
105900950110     C* DATA immissione DISPOSIZIONI
106000950110     C                   Z-ADD     GCPDDM        G02INV
106100950110     C                   MOVEL     '3'           G02ERR
106200950110     C                   CALL      'XSRDA8'
106300950110     C                   PARM                    WLBDA8
106400950110     C                   Z-ADD     G02DAT        DATDDM            8 0
106500950110     C* DATA esecuzione DISPOSIZIONI
106600950110     C                   Z-ADD     GCPDED        G02INV
106700950110     C                   MOVEL     '3'           G02ERR
106800950110     C                   CALL      'XSRDA8'
106900950110     C                   PARM                    WLBDA8
107000950110     C                   Z-ADD     G02DAT        DATDED            8 0
107100950110     C*
107200900515     C                   ENDSR
107300911209     C*---------------------------------------------------------------*
107400911209     C*   RICERCA NOTE
107500911209     C*---------------------------------------------------------------*
107600911209     C     RICNOT        BEGSR
107700920626     C* RICERCO TIPO DISPOSIZIONE SE NON TROVATO PRELEVO DALLE NOTE
107800950110     C     GCPDIS        IFNE      *BLANKS
107900920626     C                   MOVEL     '2D'          COD
108000920626     C                   MOVEL     *BLANKS       KEY
108100950110     C                   MOVEL     GCPDIS        KEY
108200920626     C     KTAB          CHAIN     TABEL                              50
108300920626     C  N50              MOVEL     TBLUNI        DS1
108400920626     C   50              MOVEL     *BLANKS       DS1
108500920626     C                   ELSE
108600911209     C                   Z-ADD     20            §FAS
108700050218     C     KNOT          CHAIN     tignp01L                           50
108800911209     C  N50              MOVEL     GNPDMC        DS1              40
108900920604     C   50              MOVEL     *BLANKS       DS1
109000920626     C                   END
109100920626     C*
109200911209     C                   ENDSR
109300900515     C*---------------------------------------------------------------*
109400900515     C*   STAMPA LISTA 1
109500900515     C*---------------------------------------------------------------*
109600900515     C     STAMP         BEGSR
109700961209     C* CLIENTE ESTERO: STAMPO IN INGLESE
109800961209     C     CLIEST        IFEQ      'S'
109900961209     C                   SETON                                        90
110000961209      * GESTISCO IL CAMBIO DEL CODICE CLIENTE X SALTO PAGINA SE ESTERO
110100961209     C     GCPSCM        IFNE      SCMPRE
110200961209     C                   SETON                                        OF
110300961209     C                   MOVE      GCPSCM        SCMPRE
110400961209     C                   ENDIF
110500961209     C                   ELSE
110600961209     C   90              SETON                                        OF
110700961209     C   90              SETOFF                                       90
110800961209     C                   ENDIF
110900961209      *
111000961209     C   OF
111100961209     CANN90              EXCEPT    TESTA
111200961209     C  N90              EXCEPT    DET
111300961209     C   OF
111400961209     CAN 90              EXCEPT    TESTAI
111500961209     C   90              EXCEPT    DETI
111600950110     C*
111700900515     C                   ENDSR
111800900515     C*---------------------------------------------------------------*
111900921014     C*   STAMPA LISTA 2 3 4 5
112000900515     C*---------------------------------------------------------------*
112100900515     C     STAMP1        BEGSR
112200950110     C*
112300900515     C     GCPCCR        IFNE      SAVCCR
112400950111     C                   EXSR      RICDES
112500900515     C                   EXCEPT    TESTA
112600900515     C                   Z-ADD     GCPCCR        SAVCCR            3 0
112700900515     C                   END
112800950110     C                   EXSR      STAMP
112900950110     C*
113000950110     C                   ENDSR
113100900515     C*---------------------------------------------------------------*
113200900515     C*   RICERCA DESCRIZIONI
113300900515     C*---------------------------------------------------------------*
113400900515     C     RICDES        BEGSR
113500950111     C*
113600921014     C* LUOGO DEPOSITO MERCE 'CCR'
113700950110     C                   MOVE      GCPCCR        ORGFIL
113800950110     C     ORGFIL        CHAIN     AZORG01L                           23
113900900515     C                   MOVEL     *BLANKS       DCCR             20
114000900515     C  N23ORGFVA        COMP      '*'                                    23
114100900515     C   23              MOVEL     *ALL'?'       DCCR
114200900515     C  N23              MOVEL     ORGDES        DCCR
114300900515     C* SE NON LO TROVO NELL'ORGANIGR. LO CERCO IN TAB. CORRISP.
114400900515     C     *IN23         IFEQ      '1'
114500900516     C                   MOVEL     '2B'          COD
114600900516     C                   MOVEL     *BLANK        KEY
114700921014     C                   MOVEL     GCPCCR        KEY
114800900515     C     KTAB          CHAIN     TABEL                              23
114900900515     C   23              MOVEL     *ALL'?'       DCCR
115000900515     C  N23              MOVEL     TBLUNI        DCCR
115100950111     C                   END
115200950111     C*
115300950111     C                   ENDSR
115400950111     C*--------------------------------------------------***
115500950111     C* Ricerco codice motivo apertura  giacenza
115600950111     C*--------------------------------------------------***
115700950111     C     MOTAPE        BEGSR
115800961209     C*  CLIENTE ITALIA
115900961209     C     CLIEST        IFNE      'S'
116000950111     C                   CLEAR                   DS2A
116100950111     C                   SETOFF                                       24
116200900516     C                   MOVEL     '2A'          COD
116300900516     C                   MOVEL     *BLANK        KEY
116400900516     C                   MOVEL     GCPCMC        KEY
116500900516     C     KTAB          CHAIN     TABEL                              23
116600950111     C     *IN23         IFEQ      '0'
116700950111     C     GCPCMC        ANDNE     'GEN'
116800950111     C                   MOVEL     TBLUNI        DS2A
116900950111     C                   ELSE
117000950111     C                   SETON                                        24
117100050218     c                   if        gcpapeaut = *blanks
117200950111     C                   Z-ADD     10            §FAS
117300050218     c                   else
117400050218     c* autoconfermate
117500050218     C                   Z-ADD     15            §FAS
117600050218     c                   end
117700050218     C     KNOT          CHAIN     tiGNP01L                           50
117800950111     C  N50              MOVEL     GNPDMC        DMC              50
117900950111     C   50              MOVEL     *BLANKS       DMC
118000950111     C                   END
118100961209     C                   ELSE
118200961209     C*  CLIENTE ESTERO
118300961209     C                   SETOFF                                       24
118400961209     C                   MOVEL     '2X'          COD
118500961209     C                   MOVEL     *BLANK        KEY
118600961209     C                   MOVEL     GCPCMC        KEY
118700961209     C     KTAB          CHAIN     TABEL                              23
118800961209     C     *IN23         IFEQ      '0'
118900961209     C     GCPCMC        ANDNE     'GEN'
119000961209     C                   MOVEL     TBLUNI        DMC
119100961209     C                   ELSE
119200961209     C                   Z-ADD     10            §FAS
119300050218     C     KNOT          CHAIN     tignp01L                           50
119400961209     C  N50              MOVEL     GNPDMC        DMC
119500961209     C   50              MOVEL     *BLANKS       DMC
119600961209     C                   END
119700961209     C                   ENDIF
119800950111     C*
119900900515     C                   ENDSR
120000900515     C***------------------------------------------------***
120100940329     C* CALCOLO GIORNI FESTIVI INTERCORRENTE TRA LE DUE DATE IN ESAME
120200940329     C*-----------
120300940329     C     FEST          BEGSR
120400990906     C                   CALL      'XGGFEST'
120500950110     C                   PARM                    WDAT8
120600940329     C                   ENDSR
120700940329     C***------------------------------------------------***
120800950110     OPRTF198   E            TESTA            01
120900950110     O               40      ST(1)              132
121000950110     O               40      D40GG1        Z    132
121100950110     O               41      ST(2)              132
121200950110     O               41      D40GG2        Z    125
121300950110     O               42      ST(3)              132
121400950110     O               43      ST(4)              128
121500950110     O               43      D40GG4        Z    108
121600950110     O               43      DRD4               124 '  /  /    '
121700950110     O               43      DRA4               138 '  /  /    '
121800950110     O               44      ST(5)              122
121900950110     O               44      ST(6)              188
122000950110     O               44      D40GG5        Z    111
122100950111     O               44      DRD5               126 '  /  /    '
122200950110     O               44      DRA5               141 '  /  /    '
122300950110     O                       RSUT                20
122400950110     O                                           21 '/'
122500950110     O                       SIMFEL              24
122600961001     O                                          177 'FNLG41R'
122700961001     O                       OGGI               188 '  /  /    '
122800950110     O                                          193 'PAG.'
122900950110     O                       PAGE          Z    198
123000950110     O          E            TESTA            02
123100950110     O                       KNSIF               10
123200950110     O                       KNMUS               21
123300961001     O                       WTIME              186 '  :  :  '
123400950111     O          E   N40N43N44TESTA            03
123500961001     O                                              'PUNTO OPERATIVO :'
123600961001     O                       GCPCCR              22
123700961001     O                       DCCR                48
123800950111     O          E            TESTA       2  1
123900950110     O                       CMP1               198
124000950111     O          E            TESTA          1
124100950110     O                       ST(7)               66
124200950110     O               40      ST(8)              132
124300950116     O               40      ST(11)             198
124400950111     O               41      ST(9)              132
124500950116     O               41      ST(13)             198
124600950110     O               42      ST(8)              132
124700950116     O               42      ST(12)             198
124800950110     O               43      ST(8)              132
124900950111     O               43      ST(11)             198
125000950110     O               44      ST(10)             132
125100950111     O          E            TESTA
125200950110     O                       CMP2               198
125300950110     O          E            DET         1
125400950110     O                       GCPFGC        Z      3
125500950110     O                                            4 '-'
125600950110     O                       GCPNGC        Z     11
125700950110     O                       DATAGC              22 '  /  /    '
125800950110     O                       DATDXD              33 '  /  /    '
125900950110     O                       RAGMIT              59
126000950110     O                       RIFNUM        Z     75
126100950110     O                       GCPCMC              79
126200950110     O                       GCPLNP              83
126300950110     O                                           84 '-'
126400950110     O                       GCPNRS        Z     86
126500950110     O                       GCPNSP        Z     94
126600950110     O                       DATSPE             105 '  /  /    '
126700950110     O              N44      BLPNCL        Z    111
126800950110     O              N44      BLPPKF             120 '      , '
126900950110     O              N41N44   DESTI              136
127000051115     O              N41N44   ar9CAS        4    150
127100991119     O              N41N44 06                   150 '    '
127200991119     O              N41N44   STPIFT        4    166
127300991119     O              N41N44 07                   166 '   '
127400051115     O              N41N44   ar9VCA             154
127500991119     O              N41N44   AR6DIV             170
127600950111     O              N41N42N44DATDUR             181 '  /  /    '
127700950110     O               41      DS1                161
127800950111     O              N40N43N44DATDDM             181 '  /  /    '
127900950111     O              N40N43N44DATDED             192 '  /  /    '
128000950116     O              N41N42N44BLPGGA             190
128100950111     O               44      DATDCG             118 '  /  /    '
128200950111     O               44      DATDLM             132 '  /  /    '
128300950110     O               44 88   ST(15)             198
128400950110     O          E    44 88   DET            1
128500950116     O                       ST(14)             198
128600950110     O          E   N41N42N44DET         1
128700950111     O                    N24§2ADES             151
128800950111     O                     24DMC                126
128900961209      * TESTO IN INGLESE
129000961209     O          E            TESTAI           01
129100961209     O                       ST(16)             132
129200961209     O                       RSUT                20
129300961209     O                                           21 '/'
129400961209     O                       SIMFEL              24
129500961209     O                                          177 'FNLG41R'
129600961209     O                       OGGI               188 '  /  /    '
129700961209     O                                          193 'PAG.'
129800961209     O                       PAGE          Z    198
129900961209     O          E            TESTAI           02
130000961209     O                       ST(17)             132
130100961209     O                       KNSIF               10
130200961209     O                       KNMUS               21
130300961209     O                       WTIME              186 '  :  :  '
130400961209     O          E            TESTAI      2  1
130500961209     O                       CMP1               198
130600961209     O          E            TESTAI         1
130700961209     O                       ST(18)              66
130800961209     O                       ST(19)             132
130900961209     O                       ST(20)             198
131000961209     O          E            TESTAI
131100961209     O                       CMP2               198
131200961209     O          E            DETI        1
131300961209     O                       GCPFGC        Z      6
131400961209     O                                            8 '-'
131500961209     O                       GCPNGC        Z     15
131600961209     O                       DATAGC              33 '  /  /    '
131700961209     O                       RAGMIT              59
131800961209     O                       RIFNUM        Z     75
131900961209     O                       GCPLNP              83
132000961209     O                                           84 '-'
132100980205     O                       GCPNRS              86
132200980205     O                       GCPNSP              94
132300961209     O                       DATSPE             105 '  /  /    '
132400961209     O                       BLPNCL        Z    111
132500961209     O                       BLPPKF             120 '      , '
132600961209     O                       DESTI              136
132700051115     O                       ar9CAS        2    154
132800991119     O                     06                   154 '    '
132900051115     O                       ar9VCA             158
133000961209     O                       DATDUR             170 '  /  /    '
133100961209     O          E            DETI        1
133200961209     O                       DMC                130
133300961209**   ST
133400950110LISTA GIACENZE IN ATTESA DI DISPOSIZIONI MITTENTE DA OLTRE GG. XXX  1
133500950110      LISTA GIACENZE IN ATTESA DI CHIUSURA DA OLTRE GG. XXX         2
133600950110               LISTA GIACENZE IN ATTESA DI RIENTRO                  3
133700950110LISTA GIACENZE APERTE IN RITARDO OLTRE GG. XXX  DAL            AL   4
133800950110LISTA GIACENZE CHIUSE IN RITARDO DALLA PARTENZA GG. XXX DAL         5
133900950110     AL                                                             6
134000950111NR.GIACENZA APERTA IL  DATA COMUN MITTENTE                     RIF  7
134100950111.NUMERICO MOT  NUMERO E DATA SPEDIZIONE COLLI   PESO   DESTINATARI  8
134200950111.NUMERICO MOT  NUMERO E DATA SPEDIZIONE COLLI   PESO   D I S P O S  9
134300950111.NUMERICO MOT  NUMERO E DATA SPEDIZIONE  CHIU.ARRIVO CHIU.PARTENZA 10
134400991119O  CONTRASSEGNO        TOT.FATTURA     RIAPERTURA  PARTICOLARITA'  11
134500991119O  CONTRASSEGNO        TOT.FATTURA     IMM. DISP. ESEC.DISP.       12
134600950116 I Z I O N I    M I T T E N T E        IMM. DISP. ESEC.DISP.       13
134700950116   GIACENZA NON CHIUSA DALLA PARTENZA !                            14
134800961209                                                                   15
134900970618       LIST  OF  U N D E L I V E R A B L E   SHIPMENTS             16
135000961209            -  WAITING FOR YOUR INSTRUCTION  -                     17
135100961209PROTOCOLLE NUMBER       OPEN  ON     SENDER                    CLI 18
135200961209ENT REF.NR.      NR. DATE OF SHIPMENT   COLLI    WGT   CONSIGNEE   19
135300961209       C.O.D.               NEW UNDEL.                             20

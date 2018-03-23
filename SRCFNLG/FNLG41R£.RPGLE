000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200900514     H* FLFG41R *----------------------------------------------------*
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
004400020305     D trul27ds      e ds
004500020305     D FNLG40        E DS                  EXTNAME(FNLG40DS)
004600960124     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
004700150514     D fnlv59ds      E DS
004800950116     D TRUL52        E DS                  EXTNAME(TRUL52DS)
004900950110     D*--------------------------------------------------------------------*
005000900423     D TCUDS           DS
005100900423     D  F3                     3      3
005200900423     D  F4                     4      4
005300900423     D  F56                    5      6
005400950110     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
005500900423     D  TCU                  398    697
005600900418     D                                     DIM(50)
005700900423     D  KCU                  698    847P 0
005800900418     D                                     DIM(50)
005900900418     D                                     PACKEVEN
006000950110     D CNCR80        E DS
006100950110     D WLBDA8          DS
006200950110     D  G02DAT                 1      8  0
006300950110     D  G02INV                 9     16  0
006400950110     D  G02ERR                17     17
006500950110     D  G02TGI                18     22  0
006600940329     D*-------------
006700990906     D* DS PER ROUTINE 'XGGFEST'
006800940329     D*-------------
006900950110     D WDAT8           DS
007000950110     D  DATADA                 1      8  0
007100950110     D  DATAA                  9     16  0
007200950110     D  GGF                   17     21  0
007300900418     D KPJBA         E DS
007400950110      *----------------------------------------------------------------
007500950110     IAZORG01L  AA
007600950110     I                             P    4    5 0ORGFIL
007700950110     I                                  6    6  ORGFVA
007800971113     I                                 11   11  ORGFL1
007900950110     I                                 14   34  ORGDES
008000000202     I                               3776 3800  ORGDE3
008100900514     C*---------------------------------------------------------------*
008200950110     C*  Controllo il tipo di stampa scelta
008300950110     C     D40SC1        CASEQ     '1'           LISTA1
008400950110     C     D40SC2        CASEQ     '1'           LISTA2
008500950110     C     D40SC3        CASEQ     '1'           LISTA3
008600950110     C     D40SC4        CASEQ     '1'           LISTA4
008700950110     C     D40SC5        CASEQ     '1'           LISTA5
008800900514     C                   END
008900950110     C*
009000900514     C     FINE          TAG
009100020306     C                   CLEAR                   TRUL27ds
009200020306     C                   MOVE      'C'           I27tla
009300020306     C                   CALL      'TRUL27R'
009400020306     C                   PARM                    TRUL27ds
009500900514     C                   SETON                                        LR
009600900514     C*---------------------------------------------------------------*
009700950110     C*   OPERAZIONI INIZIALI                                         *
009800900514     C*---------------------------------------------------------------*
009900950110     C     *INZSR        BEGSR
010000950110     C*
010100900514     C     *ENTRY        PLIST
010200900514     C                   PARM                    KPJBA
010300950110     C                   MOVEL     KPJBU         FNLG40
010400950110     C*---------------------------------------------------------------*
010500900514     C                   Z-ADD     1             CODUT
010600950110     C                   CALL      'X§PARUT'
010700950110     C                   PARM                    UT§DSE
010800950110     C                   MOVEL     REC80         CNCR80
010900900514     C     CODUT         CABEQ     -1            FINE
011000900514     C                   MOVEL     RAGUT         RSUT             20
011100900514     C                   MOVEL     RAGUT         RSUT2            25
011200950110     C                   Z-ADD     0             SAVCCR
011300950110     C*--- RICERCA CAPOCONTI------------------------------------------*
011400950110     C                   DO        50            X                 2 0
011500950110     C                   MOVE      TCU(X)        TCUDS
011600950110     C     F56           IFEQ      'CG'
011700950110     C     F4            ANDEQ     '1'
011800950110     C     F3            ANDEQ     '0'
011900950110     C                   Z-ADD     KCU(X)        KCI               4 0
012000950110     C                   END
012100950110     C                   END
012200950110     C*---------------------------------------------------------------*
012300950110     C*  Definizione chiavi
012400950110     C     KACO          KLIST
012500950110     C                   KFLD                    CODUT
012600950110     C                   KFLD                    KCI
012700950110     C                   KFLD                    GCPSCM
012800950110     C*
012900950110     C     KTGC          KLIST
013000950110     C                   KFLD                    KKSC
013100950110     C                   KFLD                    KCTR
013200950110     C                   KFLD                    KPRG
013300950110     C*
013400950110     C     KGAP          KLIST
013500950110     C                   KFLD                    GCPAAS
013600950110     C                   KFLD                    GCPLNP
013700950110     C                   KFLD                    GCPNRS
013800950110     C                   KFLD                    GCPNSP
013900990929     C     KAR6          KLIST
014000990929     C                   KFLD                    GCPAAS
014100990929     C                   KFLD                    GCPLNP
014200990929     C                   KFLD                    GCPNRS
014300990929     C                   KFLD                    GCPNSP
014400990929     C                   KFLD                    KTRC
014500950110     C*
014600950110     C     KTAB          KLIST
014700950110     C                   KFLD                    CODUT
014800950110     C                   KFLD                    COD               2
014900950110     C                   KFLD                    KEY               8
015000950110     C*
015100950110     C     KTAB1         KLIST
015200950110     C                   KFLD                    CODUT
015300950110     C                   KFLD                    COD               2
015400950110     C*
015500950110     C     KNOT          KLIST
015600950110     C                   KFLD                    GCPAGC
015700950110     C                   KFLD                    GCPFGC
015800950110     C                   KFLD                    GCPNGC
015900950110     C                   KFLD                    GCPFRG
016000050218     C                   KFLD                    §FAS              3 0
016100020822     C*
016200020822     C     Kgcp          KLIST
016300020822     C                   KFLD                    simpou
016400020822     C                   KFLD                    d40md1
016500020822     C     Kgcp5         KLIST
016600020822     C                   KFLD                    simpou
016700020822     C                   KFLD                    §FAS
016800950110     C*---------------------------------------------------------------*
016900950110     C     *LIKE         DEFINE    TGCKSC        KKSC
017000961209     C     *LIKE         DEFINE    GCPSCM        SCMPRE
017100990929     C     *LIKE         DEFINE    AR6TRC        KTRC
017200990929     C     *LIKE         DEFINE    TGCCTR        KCTR
017300950110     C     *LIKE         DEFINE    TGCPRG        KPRG
017400950110     C     *LIKE         DEFINE    G02TGI        §GGD
017500950110     C     *LIKE         DEFINE    G02TGI        §GGA
017600950110     C     *LIKE         DEFINE    G02TGI        §GGDI
017700950110     C*---------------------------------------------------------------*
017800960216     C                   CLEAR                   FNLSN0
017900950110     C                   SETOFF                                       404142
018000950110     C                   SETOFF                                       43
018100950111     C                   SETON                                        OF
018200950110     C                   TIME                    WTIME             6 0
018300950110     C                   TIME                    WHHDA1           12 0
018400950110     C                   TIME                    WHHDAT           14 0
018500950110     C                   MOVE      WHHDAT        OGGI              8 0
018600950110     C                   MOVE      *ALL'-'       CMP1            198
018700950110     C                   MOVE      *ALL'-'       CMP2            198
018800990929     C*
018900990929     C* Carico DATI TABELLA CODICI BOLLA
019000990929     C                   Z-ADD     0             §F                3 0
019100990929     C                   MOVEL     '3A'          COD
019200990929     C                   MOVEL     *BLANK        KEY
019300990929     C     KTAB1         CHAIN     TABEL                              21
019400990929     C     *IN21         DOWEQ     '0'
019500990929     C     §F            ANDLE     200
019600990929     C     TBLFLG        IFEQ      *BLANK
019700990929     C                   ADD       1             §F
019800990929     C                   MOVEL     TBLUNI        DS3A
019900990929     C                   MOVEL     §3ATB1        T3A(§F)
020000990929     C                   MOVEL     TBLKEY        C3A(§F)
020100990929     C                   END
020200990929     C     KTAB1         READE     TABEL                                  21
020300990929     C                   END
020400950110     C*---------------------------------------------------------------*
020500950110     C                   ENDSR
020600950110     C*---------------------------------------------------------------*
020700950110     C*   Giacenze in attesa di disposizioni mittente                 *
020800950110     C*---------------------------------------------------------------*
020900950110     C     LISTA1        BEGSR
021000961209     C                   CLEAR                   CLIEST
021100950110     C*
021200950116     C* Carico DATI TABELLA PARTICOLARITA'
021300950116     C                   Z-ADD     0             §F                3 0
021400950116     C                   MOVEL     '7Q'          COD
021500950116     C                   MOVEL     *BLANK        KEY
021600950116     C     KTAB1         CHAIN     TABEL                              21
021700950116     C     *IN21         DOWEQ     '0'
021800950116     C     §F            ANDLE     100
021900950116     C     TBLFLG        IFEQ      *BLANK
022000950116     C                   ADD       1             §F
022100950116     C                   MOVEL     TBLUNI        D7Q(§F)
022200950116     C                   MOVEL     TBLKEY        C7Q(§F)
022300950116     C                   END
022400950116     C     KTAB1         READE     TABEL                                  21
022500950116     C                   END
022600950116     C*
022700950110     C* Carico NAZIONI
022800950110     C                   Z-ADD     0             §F                3 0
022900950110     C                   MOVEL     '15'          COD
023000950110     C                   MOVEL     *BLANK        KEY
023100950110     C     KTAB1         CHAIN     TABEL                              21
023200950110     C     *IN21         DOWEQ     '0'
023300011221     C     §F            ANDLE     500
023400950110     C     TBLFLG        IFEQ      *BLANK
023500950110     C                   ADD       1             §F
023600950110     C                   MOVEL     TBLUNI        DS15
023700950110     C                   MOVEL     TBLKEY        CNZ(§F)
023800950110     C                   MOVEL     §15ITA        ITA(§F)
023900950110     C                   END
024000950110     C     KTAB1         READE     TABEL                                  21
024100950110     C                   END
024200950110     C*  Cerco gg. rientro su tabella
024300950110     C                   SETON                                        40
024400950110     C                   MOVEL     '2G'          COD
024500950110     C                   MOVEL     *BLANKS       KEY
024600950110     C                   MOVEL     1             KEY
024700950110     C                   CLEAR                   DS2G
024800950110     C     KTAB          CHAIN     TABEL00F                           70
024900950110     C  N70              MOVEL     TBLUNI        DS2G
025000950110     C*
025100950110     C*  Giro la data del giorno
025200950110     C                   MOVE      OGGI          G02DAT
025300950110     C                   MOVEL     *BLANK        G02ERR
025400950110     C                   CALL      'XSRDA8'
025500950110     C                   PARM                    WLBDA8
025600950110     C                   Z-ADD     G02TGI        §GGD
025700950110     C                   Z-ADD     G02INV        DATAA
025800950110     C*---------------------------------------------------------------*
025900950110     C*  Esamino giacenze fino a fine lista o codice mittente superiore
026000950110     C*  a quello selezionato
026100050218     C     kgcp          SETLL     tigcp07L
026200050218     C     simpou        READe     tigcp07L                               21
026300950110     C     *IN21         DOWEQ     '0'
026400950111     C     GCPSCM        ANDLE     D40MA1
026500020228     c* disp. da immettere e giac. in essere e non annullata
026600950110     C     GCPDDM        IFEQ      0
026700950110     C     GCPFRG        ANDEQ     0
026800950110     C     GCPATB        ANDEQ     ' '
026900950110     C*
027000950110     C     GCPFAS        IFEQ      20
027100950110     C     GCPFAS        OREQ      26
027200950110     C                   EXSR      STPL01
027300950110     C                   END
027400950110     C                   END
027500050218     C     simpou        READe     tigcp07L                               21
027600950110     C                   END
027700950110     C*
027800950110     C                   ENDSR
027900950110     C*---------------------------------------------------------------*
028000950110     C*   Giacenze in attesa di chiusura                              *
028100950110     C*---------------------------------------------------------------*
028200950110     C     LISTA2        BEGSR
028300961209     C                   CLEAR                   CLIEST
028400950110     C*
028500950110     C                   SETON                                        41
028600950110     C                   MOVE      OGGI          G02DAT
028700950110     C                   MOVEL     *BLANK        G02ERR
028800950110     C                   CALL      'XSRDA8'
028900950110     C                   PARM                    WLBDA8
029000950110     C                   Z-ADD     G02TGI        §GGD
029100950110     C                   Z-ADD     G02INV        DATAA
029200950110     C*---------------------------------------------------------------*
029300950110     C*  Esamino giacenze fino a fine lista da dispos. trasmesse
029400950110     C                   Z-ADD     30            §FAS
029500050406     C     Kgcp5         SETLL     tigcp09L
029600050406     C     simpou        READe     tigcp09L                               21
029700950110     C     *IN21         DOWEQ     '0'
029800020228     c* ricevuta chiusura
029900950111     C     GCPFAS        ANDLT     40
030000020228     c* luogo dep. >= da
030100950110     C     GCPCCR        IFGE      D40FD2
030200020228     c* luogo dep. <= da
030300950110     C     GCPCCR        ANDLE     D40FA2
030400020228     c* trasmesse disp.
030500950110     C     GCPDLD        ANDGT     0
030600020228     c* giacenze da chiudere
030700050406     C*****GCPDCG        ANDEQ     0
030800020228     c* giacenze in essere
030900950110     C     GCPFRG        ANDEQ     0
031000020228     c* giacenze non annullata
031100950110     C     GCPATB        ANDEQ     ' '
031200950110     C                   EXSR      STPL02
031300950110     C                   END
031400050406     C     simpou        READe     tigcp09l                               21
031500950110     C                   END
031600950110     C*
031700950110     C                   ENDSR
031800950110     C*---------------------------------------------------------------*
031900950110     C*   Giacenze in attesa di rientro                               *
032000950110     C*---------------------------------------------------------------*
032100950110     C     LISTA3        BEGSR
032200961209     C                   CLEAR                   CLIEST
032300950110     C*
032400950110     C*  Giro la data del giorno
032500950110     C                   MOVE      OGGI          G02DAT
032600950110     C                   MOVEL     *BLANK        G02ERR
032700950110     C                   CALL      'XSRDA8'
032800950110     C                   PARM                    WLBDA8
032900950110     C                   Z-ADD     G02TGI        §GGD
033000950110     C                   Z-ADD     G02INV        AMGUDT            8 0
033100950110     C*
033200950110     C                   SETON                                        42
033300950111     C                   Z-ADD     30            §FAS
033400050406     C     kgcp5         SETLL     tigcp09l
033500050406     C     simpou        READe     tigcp09l                               21
033600950110     C     *IN21         DOWEQ     '0'
033700020228     c* ricevuta chiusura
033800950111     C     GCPFAS        ANDLT     40
033900020228     c* giacenza da chiudere
034000950110     C     GCPDCG        IFLE      0
034100020228     c* flag di rientrato
034200950110     C     GCPDIS        ANDEQ     'R'
034300020228     C*Trasmesse disp.
034400950111     C     GCPDLD        ANDGT     0
034500020228     C* Giac. in essere
034600950110     C     GCPFRG        ANDEQ     0
034700020228     C*  Giac. annullata
034800950110     C     GCPATB        ANDEQ     ' '
034900020228     C*  Dispos. non eseg.
035000950110     C     GCPDED        ANDLE     AMGUDT
035100950110     C                   EXSR      STPL03
035200950110     C                   END
035300050406     C     simpou        READe     tigcp09l                               21
035400950110     C                   END
035500950110     C*
035600950110     C                   ENDSR
035700950110     C*---------------------------------------------------------------*
035800950110     C*   Giacenze aperte in ritardo                                  *
035900950110     C*---------------------------------------------------------------*
036000950110     C     LISTA4        BEGSR
036100961209     C                   CLEAR                   CLIEST
036200950110     C*
036300950111     C* Carico DATI TABELLA PARTICOLARITA'
036400950111     C                   Z-ADD     0             §F                3 0
036500950111     C                   MOVEL     '7Q'          COD
036600950111     C                   MOVEL     *BLANK        KEY
036700950111     C     KTAB1         CHAIN     TABEL                              21
036800950111     C     *IN21         DOWEQ     '0'
036900950111     C     §F            ANDLE     100
037000950111     C     TBLFLG        IFEQ      *BLANK
037100950111     C                   ADD       1             §F
037200950111     C                   MOVEL     TBLUNI        D7Q(§F)
037300950112     C                   MOVEL     TBLKEY        C7Q(§F)
037400950111     C                   END
037500950111     C     KTAB1         READE     TABEL                                  21
037600950111     C                   END
037700950111     C*  GIRO DATE LIMITI
037800950110     C                   Z-ADD     D40DD4        G02INV
037900950110     C                   MOVEL     '3'           G02ERR
038000950110     C                   CALL      'XSRDA8'
038100950110     C                   PARM                    WLBDA8
038200950110     C                   Z-ADD     G02DAT        DRD4              8 0
038300950110     C                   Z-ADD     D40DA4        G02INV
038400950110     C                   MOVEL     '3'           G02ERR
038500950110     C                   CALL      'XSRDA8'
038600950110     C                   PARM                    WLBDA8
038700950110     C                   Z-ADD     G02DAT        DRA4              8 0
038800950110     C*
038900950110     C                   SETON                                        43
039000050406     c     simpou        setll     tigcp09l
039100050406     C     simpou        READe     tigcp09L                               21
039200950110     C     *IN21         DOWEQ     '0'
039300020228     C* Luogo dep. >= da
039400950110     C     GCPCCR        IFGE      D40FD4
039500020228     C* Luogo dep. <= a
039600950110     C     GCPCCR        ANDLE     D40FA4
039700020228     C* Giac.da chiudere
039800950110     C     GCPDCG        ANDEQ     0
039900020228     C* Giac.in essere
040000950110     C     GCPFRG        ANDEQ     0
040100020228     C* Giac.non annul.
040200950110     C     GCPATB        ANDEQ     ' '
040300950110     C                   EXSR      STPL04
040400950110     C                   END
040500050406     C     simpou        READe     tigcp09l                               21
040600950110     C                   END
040700950110     C*
040800950110     C                   ENDSR
040900950110     C*---------------------------------------------------------------*
041000950110     C*   Giacenze chiuse in ritardo dalla partenza                   *
041100950110     C*---------------------------------------------------------------*
041200950110     C     LISTA5        BEGSR
041300961209     C                   CLEAR                   CLIEST
041400950110     C*
041500950110     C*  GIRO DATE LIMITI
041600950110     C                   Z-ADD     D40DD5        G02INV
041700950110     C                   MOVEL     '3'           G02ERR
041800950110     C                   CALL      'XSRDA8'
041900950110     C                   PARM                    WLBDA8
042000950110     C                   Z-ADD     G02DAT        DRD5              8 0
042100950110     C                   Z-ADD     D40DA5        G02INV
042200950110     C                   MOVEL     '3'           G02ERR
042300950110     C                   CALL      'XSRDA8'
042400950110     C                   PARM                    WLBDA8
042500950110     C                   Z-ADD     G02DAT        DRA5              8 0
042600950110     C*
042700950110     C                   SETON                                        44
042800950110     C                   MOVE      40            §FAS
042900050406     C     kgcp5         SETLL     tigcp09L
043000050406     C     simpou        READe     tigcp09L                               21
043100950110     C     *IN21         DOWEQ     '0'
043200020228     C* Linea part. da
043300950110     C     GCPLNP        IFGE      D40FD5
043400020228     C* Linea part. a
043500950110     C     GCPLNP        ANDLE     D40DA5
043600020228     C* Data chius.giac da
043700950110     C     GCPDCG        ANDGE     D40DD5
043800020228     C* Data chius.giac a
043900950110     C     GCPDCG        ANDLE     D40DA5
044000020228     C* no annullata
044100950110     C     GCPATB        ANDEQ     ' '
044200950110     C                   EXSR      STPL05
044300950110     C                   END
044400050406     C     simpou        READe     tigcp09L                               21
044500950110     C                   END
044600950110     C*
044700950110     C                   ENDSR
044800900514     C*---------------------------------------------------------------*
044900950110     C*   Esegui stampa lista giacenze in attesa disposiz.            *
045000900514     C*---------------------------------------------------------------*
045100950110     C     STPL01        BEGSR
045200900515     C*
045300050218     C                   Z-ADD     0             DATDUR            8 0
045400050218     C* DATA ULT.RIAP-
045500950110     C                   MOVEL     GCPDUR        G02INV
045600050406     c                   if        gcpriap = 'R'
045700950116     C                   MOVEL     '3'           G02ERR
045800950116     C                   CALL      'XSRDA8'
045900950116     C                   PARM                    WLBDA8
046000950116     C                   Z-ADD     G02DAT        DATDUR
046100050218     c                   end
046200900515     C*
046300950110     C* Calcolo giorni di rientro su tariffa cliente
046400990929     C                   Z-ADD     0             AR6IFT
046500051115     C                   Z-ADD     0             ar9CAS
046600051115     C                   MOVEL     *BLANKS       ar9VCA
046700991119     C                   MOVEL     *BLANKS       AR6DIV
046800051115     C     KGAP          CHAIN     Fiar901L                           70
046900051115     C                   MOVE      ar9CAS        COM3              3 0
047000950111     C     COM3          COMP      000                                    06
047100950110     C     KGAP          CHAIN     FNBLP01L                           70
047200990929     C* Verifico se esiste la spedizione è in porto FRANCO
047300990929     C* per reperire eventuali dati di tassazione
047400990929     C     *IN70         IFEQ      '0'
047500020305     C                   Z-ADD     1             Y                 3 0
047600990929     C     BLPCBO        LOOKUP    C3A(Y)                                 70
047700990929     C     T3A(Y)        IFEQ      'F'
047800990929     C     *IN70         ANDEQ     '1'
047900990929     C                   MOVEL     '1'           KTRC
048000990929     C     KAR6          CHAIN     FIAR601L                           70
048100990929     C                   END
048200990929     C                   END
048300991119     C                   Z-ADD     AR6IFT        STPIFT           11 2
048400991119     C                   MOVE      STPIFT        COM2              2 0
048500991119     C     COM2          COMP      000                                    07
048600950116     C*  Controllo se partenza doveva attendere X gg. prima conferma giac.
048700950116     C     BLPGGA        IFNE      *BLANKS
048800950116     C                   Z-ADD     1             Y
048900950116     C     BLPGGA        LOOKUP    C7Q(Y)                                 40
049000950116     C     *IN40         IFEQ      '1'
049100950116     C                   MOVEL     D7Q(Y)        DS7Q
049200950116     C     §7Q3EV        IFNE      *BLANKS
049300950116     C     §7Q3MF        ANDEQ     'A'
049400950116     C                   MOVEL     '2A'          COD
049500950116     C                   MOVEL     *BLANK        KEY
049600950116     C                   MOVEL     §7Q3EV        KEY
049700950116     C     KTAB          CHAIN     TABEL                              23
049800950116     C     *IN23         IFEQ      '0'
049900950116     C                   MOVEL     TBLUNI        DS2A
050000950116     C     §2AGLA        IFGT      0
050100950116     C                   CLEAR                   TRUL52
050200950116     C                   Z-ADD     §2AGLA        D52GIO
050300950116     C                   Z-ADD     G02INV        D52DIN
050400950116     C                   CALL      'TRUL52R'
050500950116     C                   PARM                    TRUL52
050600950116     C     D52ERR        IFEQ      *BLANKS
050700950116     C                   Z-ADD     D52DFI        G02INV            8 0
050800950116     C                   END
050900950116     C                   END
051000950116     C                   END
051100950116     C                   END
051200950116     C*
051300950116     C                   END
051400950116     C                   END
051500950116     C*
051600950116     C                   MOVEL     '3'           G02ERR
051700950116     C                   CALL      'XSRDA8'
051800950116     C                   PARM                    WLBDA8
051900950116     C                   Z-ADD     G02TGI        §GGA
052000950116     C                   Z-ADD     G02INV        DATADA
052100950111     C* Ricerco decodifica cliente mittente
052200950110     C                   EXSR      DECODI
052300961209      * SE CLIENTE ESTERO  STAMPO IN LINGUA
052400961209     C                   CLEAR                   CLIEST
052500961209     C                   Z-ADD     1             Y
052600961209     C     BLPNZM        LOOKUP    CNZ(Y)                                 40
052700971113     C* Aggancio azorg lnp x sapere se lnp Estera
052800971113     C                   MOVEL     *BLANKS       ORGFL1
052900971113     C     BLPLNP        CHAIN     AZORG01L                           30
053000020717     C  n30              MOVEL     ORGDE3        OG143
053100961209     C     ITA(Y)        IFNE      'I'
053200020717     C     §OgNTW        oreq      'FED'
053300020717     C     §OgNTW        oreq      'EEX'
053400020717     C     §OgNTW        oreq      'EUP'
053500961209     C                   MOVEL     'S'           CLIEST            1
053600961209     C                   ENDIF
053700950111     C*
053800150514     C                   CLEAR                   fnlv59ds
053900950110     C                   MOVEL     BLPAAS        DATDSP            8 0
054000950110     C                   MOVE      BLPMGS        DATDSP
054100950110     C                   MOVEL     ' '           WTROVA            1
054200950110     C                   MOVE      GCPSCM        §QUATR            4 0
054300020304     C* chiamo trul27r per reperire il fie e la tariffa di cartello
054400020305     C                   CLEAR                   TRUL27ds
054500021118     C                   MOVEL     BLPctr        I27ctb
054600020304     C                   MOVEL     BLPTSP        I27TSP
054700020304     C                   MOVE      BLPLNA        I27LNA
054800020304     C                   MOVE      BLPLNP        I27LNP
054900020304     C                   MOVE      gcpscm        I27CLI
055000020304     C                   MOVE      datdsp        I27DTA
055100020304     C                   CALL      'TRUL27R'
055200020305     C                   PARM                    TRUL27ds
055300020304     c*
055400950110     C     §QUATR        IFNE      *ALL'8'
055500950110     C     §QUATR        ANDNE     *ALL'9'
055600950110     C     §QUATR        ANDNE     *ZEROS
055700150514     C                   MOVEL     ' '           ilv59TBO
055800150514     C                   Z-ADD     DATDSP        ilv59DSP
055900150514     C                   MOVE      BLPCTR        ilv59CTR
056000950116     C                   MOVE      GCPSCM        KKSC
056100150514     C                   MOVE      KKSC          ilv59KSC
056200150514     C                   MOVE      KCI           ilv59KCI
056300150514     C                   MOVEL     o27fie        ilv59FIE
056400150514     C                   MOVEL     BLPTSP        ilv59TSP
056500150514     C                   MOVEL     'S'           ilv59CON
056600150514     c                   eval      ilv59ta2='B'
056700150514     c                   eval      ilv59blo='S'
056800150514     C                   CALL      'FNLV59R'
056900150514     C                   PARM                    fnlv59ds
057000950110     C*
057100150514     C     olv59ERR      IFEQ      *BLANKS
057200150514     C     olv59tks      ANDEQ     'S'
057300950110     C*  Se ho trovato tariffa in vigore mi posiziono sul file
057400950110     C*  delle tariffe giacenze con il proggressivo giusto
057500150514     C                   MOVE      olv59prg      KPRG
057600150514     C                   MOVE      ilv59ksc      KKSC
057700150514     C                   MOVE      olv59ctr      KCTR
057800990730     C     KTGC          CHAIN     TITGC01L                           31
057900950110     C     *IN31         IFEQ      '0'
058000950116     C     TGCGGR        ANDNE     *ZEROS
058100950110     C                   MOVE      TGCGGR        WGGR              2 0
058200950110     C                   MOVEL     'S'           WTROVA
058300950110     C                   END
058400950110     C                   END
058500950110     C                   END
058600950110     C*  Se non trovato gg. rientro su tariffa cliente li cerco
058700950110     C*  sulla tariffa di cartello
058800950110     C     WTROVA        IFNE      'S'
058900020304     C                   MOVE      o27PRG        KPRG
059000020304     C                   MOVE      o27KSC        KKSC
059100020304     C                   MOVE      o27CTR        KCTR
059200990730     C     KTGC          CHAIN     TITGC01L                           31
059300950110     C     *IN31         IFEQ      '0'
059400950116     C     TGCGGR        ANDNE     *ZEROS
059500950110     C                   MOVE      TGCGGR        WGGR
059600950110     C                   MOVEL     'S'           WTROVA
059700950110     C                   END
059800950110     C                   END
059900970618     C*  Se non ho impostato i gg rientro li prendo dalla tabella 2G
060000970618     C*
060100950110     C     WTROVA        IFNE      'S'
060200950110     C                   MOVE      §2GGGL        WGGR
060300950110     C                   END
060400961204     C*
060500020410     C     O27FIE        IFNE      'I'
060600020304     C                   SELECT
060700020304      * Se è da DPD
060800050210     C     o27ntw        WHENEQ    'DPD'
060900020304     C     WGGR          IFGT      §2GGDP
061000020304     C                   MOVE      §2GGDP        WGGR
061100020304     C                   END
061200020304      * Se è FEDEX
061300050210     C     o27ntw        WHENEQ    'FED'
061400020304     C     WGGR          IFGT      §2GGFE
061500020304     C                   MOVE      §2GGFE        WGGR
061600020304     C                   END
061700020304      * Se è estero
061800050210     C     o27ntw        WHENEQ    'EEX'
061900050210     C     o27ntw        OREQ      'EUP'
062000020304     C     WGGR          IFGT      §2GGRE
062100020304     C                   MOVE      §2GGRE        WGGR
062200020304     C                   END
062300040112      *
062400020304     C                   ENDSL
062500010921     C                   ENDIF
062600040217      *
062700050210     C*  Se ci sono eccezioni ai resi x decorsi termini sul cliente
062800050210     C*   sui giorni da prendere
062900050210      * Se è un estero Euroexpress
063000050210     C     O27FIE        IFNE      'I'
063100050210     C     o27ntw        andeq     'EEX'
063200050210     C                   MOVEL     '20'          COD
063300050210     C                   MOVEL     *blank        KEY
063400050210     C                   MOVEL     gcpSCM        KEY
063500050210     c     ktab          chain     tabel00f
063600050210     c                   if        %found(tabel00f)
063700050210     c                   movel     tbluni        ds20
063800050210     C                   Z-ADD     §20GRE        wGGR
063900050210     c                   endif
064000050210     c                   END
064100050210     C*
064200900515     C     §GGD          SUB       §GGA          §GGDI
064300950110     C     §GGDI         IFGT      WGGR
064400940329     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
064500940329     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
064600950110     C** Aggancio file storico
064700950110     C                   MOVEL     BLPRSD        DESTI            15
064800960124     C                   MOVE      BLPRMN        RIFNUM           15 0
064900960124     C*  Controllo se esiste destinatario finale  e se si lo sostit.
065000960124     C                   Z-ADD     GCPAAS        DN0AAS
065100960124     C                   Z-ADD     GCPLNP        DN0LNP
065200960124     C                   Z-ADD     GCPNRS        DN0NRS
065300960124     C                   Z-ADD     GCPNSP        DN0NSP
065400960124     C                   MOVEL     'R'           DN0TRC
065500960124     C                   MOVEL     FNLSN0        KPJBU
065600960124     C                   CALL      'FNLSN0R'
065700960124     C                   PARM                    KPJBA
065800960124     C                   MOVEL     KPJBU         FNLSN0
065900960124     C     DN0TRV        IFEQ      'S'
066000960124     C                   MOVEL     DN0RSD        DESTI
066100960124     C                   END
066200900515     C*
066300950111     C                   EXSR      MOTAPE
066400900515     C                   EXSR      RIBDAT
066500900515     C                   EXSR      STAMP
066600950110     C                   END
066700900515     C*
066800950110     C                   ENDSR
066900950110     C*---------------------------------------------------------------*
067000950110     C*   Esegui stampa lista giacenze in attesa di chiusura          *
067100950110     C*---------------------------------------------------------------*
067200950110     C     STPL02        BEGSR
067300950110     C*
067400050218     C*  Se la data di eseguibilità è superiore a quella di immissione disp.
067500950111     C*  parto da quella
067600950111     C     GCPDED        IFGT      GCPDLD
067700020228     C* DATA LT.FIL.ARR
067800950111     C                   MOVE      GCPDED        G02INV
067900950111     C                   MOVEL     '3'           G02ERR
068000950111     C                   CALL      'XSRDA8'
068100950111     C                   PARM                    WLBDA8
068200950111     C                   Z-ADD     G02TGI        §GGA
068300950111     C                   Z-ADD     G02INV        DATADA
068400950111     C                   ELSE
068500020228     C*DATA LT.FIL.ARR
068600950111     C                   MOVE      GCPDLD        G02INV
068700950111     C                   MOVEL     '3'           G02ERR
068800950111     C                   CALL      'XSRDA8'
068900950111     C                   PARM                    WLBDA8
069000950111     C                   Z-ADD     G02TGI        §GGA
069100950111     C                   Z-ADD     G02INV        DATADA
069200950111     C                   END
069300900515     C*
069400900515     C     §GGD          SUB       §GGA          §GGDI
069500950110     C     §GGDI         IFGT      D40GG2
069600940329     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
069700940329     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
069800940329     C                   EXSR      FEST
069900940330     C                   SUB       GGF           §GGDI
070000950110     C     §GGDI         IFGT      D40GG2
070100050218     C*
070200950110     C     KGAP          CHAIN     FNBLP01L                           22
070300920330     C  N22              MOVEL     BLPRSD        DESTI            15
070400950110     C   22              MOVEL     *BLANKS       DESTI
070500960124     C  N22              MOVE      BLPRMN        RIFNUM
070600960124     C   22              MOVE      *ZEROS        RIFNUM
070700960124     C*  Controllo se esiste destinatario finale  e se si lo sostit.
070800960124     C                   Z-ADD     GCPAAS        DN0AAS
070900960124     C                   Z-ADD     GCPLNP        DN0LNP
071000960124     C                   Z-ADD     GCPNRS        DN0NRS
071100960124     C                   Z-ADD     GCPNSP        DN0NSP
071200960124     C                   MOVEL     'R'           DN0TRC
071300960124     C                   MOVEL     FNLSN0        KPJBU
071400960124     C                   CALL      'FNLSN0R'
071500960124     C                   PARM                    KPJBA
071600960124     C                   MOVEL     KPJBU         FNLSN0
071700960124     C     DN0TRV        IFEQ      'S'
071800960124     C                   MOVEL     DN0RSD        DESTI
071900960124     C                   END
072000900515     C*
072100900515     C                   EXSR      DECODI
072200911209     C                   EXSR      RICNOT
072300900515     C                   EXSR      RIBDAT
072400900515     C                   EXSR      STAMP1
072500900515     C                   END
072600900515     C                   END
072700950110     C*
072800950110     C                   ENDSR
072900950110     C*---------------------------------------------------------------*
073000950110     C*   Esegui stampa lista giacenze in attesa di rientro           *
073100950110     C*---------------------------------------------------------------*
073200950110     C     STPL03        BEGSR
073300950110     C*
073400990929     C                   Z-ADD     0             AR6IFT
073500051115     C                   Z-ADD     0             ar9CAS
073600051115     C                   MOVEL     *BLANKS       ar9VCA
073700991119     C                   MOVEL     *BLANKS       AR6DIV
073800051115     C     KGAP          CHAIN     Fiar901L                           22
073900051115     C                   MOVE      ar9CAS        COM3              3 0
074000950111     C     COM3          COMP      000                                    06
074100950110     C     KGAP          CHAIN     FNBLP01L                           22
074200990929     C  N22              MOVEL     BLPRSD        DESTI            15
074300990929     C   22              MOVEL     *BLANKS       DESTI
074400990929     C  N22              MOVE      BLPRMN        RIFNUM
074500990929     C   22              MOVE      *ZEROS        RIFNUM
074600990929     C* Verifico se esiste la spedizione è in porto FRANCO
074700990929     C* per reperire eventuali dati di tassazione
074800990929     C     *IN22         IFEQ      '0'
074900990929     C                   Z-ADD     1             Y
075000990929     C     BLPCBO        LOOKUP    C3A(Y)                                 22
075100990929     C     T3A(Y)        IFEQ      'F'
075200990929     C     *IN22         ANDEQ     '1'
075300990929     C                   MOVEL     '1'           KTRC
075400990929     C     KAR6          CHAIN     FIAR601L                           22
075500990929     C                   END
075600990929     C                   END
075700991119     C                   Z-ADD     AR6IFT        STPIFT           11 2
075800991119     C                   MOVE      STPIFT        COM2              2 0
075900991119     C     COM2          COMP      000                                    07
076000960124     C*  Controllo se esiste destinatario finale  e se si lo sostit.
076100960124     C                   Z-ADD     GCPAAS        DN0AAS
076200960124     C                   Z-ADD     GCPLNP        DN0LNP
076300960124     C                   Z-ADD     GCPNRS        DN0NRS
076400960124     C                   Z-ADD     GCPNSP        DN0NSP
076500960124     C                   MOVEL     'R'           DN0TRC
076600960124     C                   MOVEL     FNLSN0        KPJBU
076700960124     C                   CALL      'FNLSN0R'
076800960124     C                   PARM                    KPJBA
076900960124     C                   MOVEL     KPJBU         FNLSN0
077000960124     C     DN0TRV        IFEQ      'S'
077100960124     C                   MOVEL     DN0RSD        DESTI
077200960124     C                   END
077300900515     C*
077400900515     C                   EXSR      DECODI
077500900515     C                   EXSR      RIBDAT
077600900515     C                   EXSR      STAMP1
077700950110     C*
077800950110     C                   ENDSR
077900950110     C*---------------------------------------------------------------*
078000950110     C*   Esegui stampa lista giacenze aperte in ritardo              *
078100950110     C*---------------------------------------------------------------*
078200950110     C     STPL04        BEGSR
078300950110     C*
078400950111     C                   Z-ADD     0             DATDUR
078500050218     C                   MOVE      GCPDUR        datagc            8 0
078600950111     C                   MOVE      GCPDUR        G02INV
078700050406     C     GCPriap       IFEQ      'R'
078800950116     C                   MOVEL     '3'           G02ERR
078900950116     C                   CALL      'XSRDA8'
079000950116     C                   PARM                    WLBDA8
079100950116     C                   Z-ADD     G02DAT        DATDUR
079200950111     C                   END
079300050218     C*
079400990929     C                   Z-ADD     0             AR6IFT
079500051115     C                   Z-ADD     0             ar9CAS
079600051115     C                   MOVEL     *BLANKS       ar9VCA
079700991119     C                   MOVEL     *BLANKS       AR6DIV
079800051115     C     KGAP          CHAIN     Fiar901L                           22
079900051115     C                   MOVE      ar9CAS        COM3              3 0
080000950116     C     COM3          COMP      000                                    06
080100950116     C     KGAP          CHAIN     FNBLP01L                           22
080200950116     C  N22              MOVEL     BLPRSD        DESTI            15
080300950116     C   22              MOVEL     *BLANKS       DESTI
080400950116     C  N22              MOVE      BLPRMN        RIFNUM
080500950116     C   22              MOVE      *ZEROS        RIFNUM
080600990929     C* Verifico se esiste la spedizione è in porto FRANCO
080700990929     C* per reperire eventuali dati di tassazione
080800990929     C     *IN22         IFEQ      '0'
080900990929     C                   Z-ADD     1             Y
081000990929     C     BLPCBO        LOOKUP    C3A(Y)                                 22
081100990929     C     T3A(Y)        IFEQ      'F'
081200990929     C     *IN22         ANDEQ     '1'
081300990929     C                   MOVEL     '1'           KTRC
081400990929     C     KAR6          CHAIN     FIAR601L                           22
081500990929     C                   END
081600990929     C                   END
081700991119     C                   Z-ADD     AR6IFT        STPIFT
081800991119     C                   MOVE      STPIFT        COM2              2 0
081900991119     C     COM2          COMP      000                                    07
082000960124     C*  Controllo se esiste destinatario finale  e se si lo sostit.
082100960124     C                   Z-ADD     GCPAAS        DN0AAS
082200960124     C                   Z-ADD     GCPLNP        DN0LNP
082300960124     C                   Z-ADD     GCPNRS        DN0NRS
082400960124     C                   Z-ADD     GCPNSP        DN0NSP
082500960124     C                   MOVEL     'R'           DN0TRC
082600960124     C                   MOVEL     FNLSN0        KPJBU
082700960124     C                   CALL      'FNLSN0R'
082800960124     C                   PARM                    KPJBA
082900960124     C                   MOVEL     KPJBU         FNLSN0
083000960124     C     DN0TRV        IFEQ      'S'
083100960124     C                   MOVEL     DN0RSD        DESTI
083200960124     C                   END
083300950116     C*
083400950116     C*  Controllo se partenza doveva attendere X gg. prima conferma giac.
083500950116     C     BLPGGA        IFNE      *BLANKS
083600950116     C                   Z-ADD     1             Y
083700950116     C     BLPGGA        LOOKUP    C7Q(Y)                                 40
083800950116     C     *IN40         IFEQ      '1'
083900950116     C                   MOVEL     D7Q(Y)        DS7Q
084000950116     C     §7Q3EV        IFNE      *BLANKS
084100950116     C     §7Q3MF        ANDEQ     'A'
084200950116     C                   MOVEL     '2A'          COD
084300950116     C                   MOVEL     *BLANK        KEY
084400950116     C                   MOVEL     §7Q3EV        KEY
084500950116     C     KTAB          CHAIN     TABEL                              23
084600950116     C     *IN23         IFEQ      '0'
084700950116     C                   MOVEL     TBLUNI        DS2A
084800950116     C     §2AGLA        IFGT      0
084900950116     C                   CLEAR                   TRUL52
085000950116     C                   Z-ADD     §2AGLA        D52GIO
085100950116     C                   Z-ADD     G02INV        D52DIN
085200950116     C                   CALL      'TRUL52R'
085300950116     C                   PARM                    TRUL52
085400950116     C     D52ERR        IFEQ      *BLANKS
085500950116     C                   Z-ADD     D52DFI        G02INV            8 0
085600950116     C                   END
085700950116     C                   END
085800950116     C                   END
085900950116     C                   END
086000950111     C*
086100950116     C                   END
086200950116     C                   END
086300950116     C*
086400950111     C                   MOVEL     '3'           G02ERR
086500950111     C                   CALL      'XSRDA8'
086600950111     C                   PARM                    WLBDA8
086700950111     C                   Z-ADD     G02TGI        §GGD
086800950111     C                   Z-ADD     G02INV        DATADA
086900950111     C*
087000950111     C     G02INV        IFGE      D40DD4
087100950111     C     G02INV        ANDLE     D40DA4
087200900515     C*
087300020228     C* DATA STAMPA MOD.MITT
087400920915     C                   MOVEL     GCPDMM        G02INV
087500900517     C                   MOVEL     '3'           G02ERR
087600950111     C                   CALL      'XSRDA8'
087700950110     C                   PARM                    WLBDA8
087800900515     C                   Z-ADD     G02TGI        §GGA
087900940330     C                   Z-ADD     G02INV        DATAA
088000900515     C*
088100940330     C     §GGA          SUB       §GGD          §GGDI
088200950110     C     §GGDI         IFGT      D40GG4
088300940329     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
088400940329     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
088500940329     C                   EXSR      FEST
088600950110     C                   SUB       GGF           §GGDI
088700950111     C     §GGDI         IFGT      D40GG4
088800950111     C*  Ricerco codice motivo apertura giacenza
088900950111     C                   EXSR      MOTAPE
089000900515     C                   EXSR      DECODI
089100900515     C                   EXSR      RIBDAT
089200950111     C                   EXSR      STAMP
089300900515     C                   END
089400900515     C                   END
089500950110     C*
089600950110     C                   END
089700950110     C*
089800950110     C                   ENDSR
089900950110     C*---------------------------------------------------------------*
090000950110     C*   Esegui stampa lista giacenze aperte in ritardo              *
090100950110     C*---------------------------------------------------------------*
090200950110     C     STPL05        BEGSR
090300950110     C*
090400950110     C     KGAP          CHAIN     FNBLP01L                           22
090500921013     C  N22              MOVEL     BLPRSD        DESTI            15
090600921013     C   22              MOVEL     *BLANKS       DESTI
090700950110     C  N22              MOVE      BLPRMN        RIFNUM
090800950110     C   22              MOVE      *ZEROS        RIFNUM
090900960124     C*  Controllo se esiste destinatario finale  e se si lo sostit.
091000960124     C                   Z-ADD     GCPAAS        DN0AAS
091100960124     C                   Z-ADD     GCPLNP        DN0LNP
091200960124     C                   Z-ADD     GCPNRS        DN0NRS
091300960124     C                   Z-ADD     GCPNSP        DN0NSP
091400960124     C                   MOVEL     'R'           DN0TRC
091500960124     C                   MOVEL     FNLSN0        KPJBU
091600960124     C                   CALL      'FNLSN0R'
091700960124     C                   PARM                    KPJBA
091800960124     C                   MOVEL     KPJBU         FNLSN0
091900960124     C     DN0TRV        IFEQ      'S'
092000960124     C                   MOVEL     DN0RSD        DESTI
092100960124     C                   END
092200050218     C*   CHIUSURA IN PART.
092300930316     C                   MOVE      GCPDBG        G02INV
092400930316     C                   MOVE      *ZEROS        G02DAT
092500921014     C                   MOVEL     3             G02ERR
092600950110     C                   CALL      'XSRDA8'
092700950110     C                   PARM                    WLBDA8
092800950110     C                   Z-ADD     G02DAT        DATDLM            8 0
092900020228     C*   CHIUSURA IN arr.
093000930316     C                   MOVE      GCPDCG        G02INV
093100930316     C                   MOVEL     '3'           G02ERR
093200950111     C                   CALL      'XSRDA8'
093300950110     C                   PARM                    WLBDA8
093400921013     C                   Z-ADD     G02TGI        §GGD
093500940329     C                   Z-ADD     G02INV        DATADA
093600950110     C                   Z-ADD     G02DAT        DATDCG            8 0
093700921013     C*
093800930316     C     GCPDBG        IFEQ      0
093900921014     C                   SETON                                        88
094000921014     C                   MOVE      UDATE         G02DAT
094100921014     C                   MOVEL     *BLANK        G02ERR
094200950110     C                   CALL      'XSRDA8'
094300950110     C                   PARM                    WLBDA8
094400020228     C* UDATE É POSTO IN G02INV IN FORMATO AA/MM/GG.
094500921013     C                   ELSE
094600020228     C* DATA CONFERMA CHIUS. in partenza
094700930316     C                   MOVEL     GCPDBG        G02INV
094800930316     C                   END
094900921013     C                   MOVEL     '3'           G02ERR
095000950110     C                   CALL      'XSRDA8'
095100950110     C                   PARM                    WLBDA8
095200921013     C                   Z-ADD     G02TGI        §GGA
095300940329     C                   Z-ADD     G02INV        DATAA
095400921013     C*
095500921013     C     §GGA          SUB       §GGD          §GGDI
095600950110     C     §GGDI         IFGT      D40GG5
095700940329     C** CALCOLO GIORNI FESTIVI INTERCORRENTI TRA LE DUE DATE E LI SOTTRAGGO
095800940329     C** DAI GIORNI DI RITARDO PER VEDERE SE SONO ANCORA IN RITARDO
095900940329     C                   EXSR      FEST
096000940330     C                   SUB       GGF           §GGDI
096100950110     C     §GGDI         IFGT      D40GG5
096200921013     C                   EXSR      DECODI
096300921013     C                   EXSR      RIBDAT
096400950110     C                   EXSR      STAMP
096500921014     C                   SETOFF                                       88
096600921013     C                   END
096700921013     C                   END
096800950110     C*
096900950110     C                   ENDSR
097000900514     C*---------------------------------------------------------------*
097100900514     C*   DECODIFICA MITTENTE
097200900514     C*---------------------------------------------------------------*
097300900514     C     DECODI        BEGSR
097400900514     C** SE IL CODICE è CODIFICATO PRENDO I DATI DELLA RAG.SOC.
097500900514     C** DAL PDC SE è CODICE 88 O 99 PRENDO I DATI O DALL'ANAGRAFICA
097600900514     C** D'APPOGGIO SE ESISTE O DALLO STORICO BOLLE
097700900514     C                   MOVE      GCPSCM        CODMIT            4 0
097800950111     C                   MOVEL     *BLANKS       RAGMIT           25
097900950110     C                   MOVEL     BLPNZM        RAGNZM
098000900514     C     CODMIT        IFNE      8888
098100900514     C     CODMIT        ANDNE     9999
098200900514     C     KACO          CHAIN     CNACO000                           25
098300900514     C  N25              DO
098400950110     C                   MOVEL     ACORAG        RAGMIT
098500950110     C     KACO          CHAIN     CNIND000                           25
098600950110     C  N25              MOVEL     INDSTA        RAGNZM            3
098700900514     C                   END
098800900514     C                   ELSE
098900050218     C     KGAP          CHAIN     tigap000                           70
099000900515     C  N70              DO
099100950110     C                   MOVEL     GAPRSM        RAGMIT
099200950110     C                   MOVEL     GAPNZM        RAGNZM
099300900515     C                   END
099400900515     C   70              DO
099500950110     C                   MOVEL     BLPRSM        RAGMIT
099600900514     C                   END
099700900515     C                   END
099800950110     C*
099900900514     C                   ENDSR
100000900515     C*---------------------------------------------------------------*
100100900515     C*   RIBALTAMENTO DATE
100200900515     C*---------------------------------------------------------------*
100300900515     C     RIBDAT        BEGSR
100400900515     C* DATA GIACENZA
100500900515     C                   MOVEL     GCPAGC        DATAGC
100600900515     C                   MOVE      GCPMGC        DATAGC
100700950110     C                   Z-ADD     DATAGC        G02INV
100800950110     C                   MOVEL     '3'           G02ERR
100900950110     C                   CALL      'XSRDA8'
101000950110     C                   PARM                    WLBDA8
101100950110     C                   MOVE      G02DAT        DATAGC
101200900515     C* DATA SPEDIZIONE
101300950110     C                   MOVEL     BLPAAS        DATSPE            8 0
101400900515     C                   MOVE      BLPMGS        DATSPE
101500950110     C                   Z-ADD     DATSPE        G02INV
101600950110     C                   MOVEL     '3'           G02ERR
101700950110     C                   CALL      'XSRDA8'
101800950110     C                   PARM                    WLBDA8
101900950110     C                   Z-ADD     G02DAT        DATSPE
102000900516     C* DATA COMUNICAZIONE INTERNA
102100950110     C                   Z-ADD     GCPDXD        G02INV
102200950110     C                   MOVEL     '3'           G02ERR
102300950110     C                   CALL      'XSRDA8'
102400950110     C                   PARM                    WLBDA8
102500950110     C                   Z-ADD     G02DAT        DATDXD            8 0
102600950110     C* DATA immissione DISPOSIZIONI
102700950110     C                   Z-ADD     GCPDDM        G02INV
102800950110     C                   MOVEL     '3'           G02ERR
102900950110     C                   CALL      'XSRDA8'
103000950110     C                   PARM                    WLBDA8
103100950110     C                   Z-ADD     G02DAT        DATDDM            8 0
103200950110     C* DATA esecuzione DISPOSIZIONI
103300950110     C                   Z-ADD     GCPDED        G02INV
103400950110     C                   MOVEL     '3'           G02ERR
103500950110     C                   CALL      'XSRDA8'
103600950110     C                   PARM                    WLBDA8
103700950110     C                   Z-ADD     G02DAT        DATDED            8 0
103800950110     C*
103900900515     C                   ENDSR
104000911209     C*---------------------------------------------------------------*
104100911209     C*   RICERCA NOTE
104200911209     C*---------------------------------------------------------------*
104300911209     C     RICNOT        BEGSR
104400920626     C* RICERCO TIPO DISPOSIZIONE SE NON TROVATO PRELEVO DALLE NOTE
104500950110     C     GCPDIS        IFNE      *BLANKS
104600920626     C                   MOVEL     '2D'          COD
104700920626     C                   MOVEL     *BLANKS       KEY
104800950110     C                   MOVEL     GCPDIS        KEY
104900920626     C     KTAB          CHAIN     TABEL                              50
105000920626     C  N50              MOVEL     TBLUNI        DS1
105100920626     C   50              MOVEL     *BLANKS       DS1
105200920626     C                   ELSE
105300911209     C                   Z-ADD     20            §FAS
105400050218     C     KNOT          CHAIN     tignp01L                           50
105500911209     C  N50              MOVEL     GNPDMC        DS1              40
105600920604     C   50              MOVEL     *BLANKS       DS1
105700920626     C                   END
105800920626     C*
105900911209     C                   ENDSR
106000900515     C*---------------------------------------------------------------*
106100900515     C*   STAMPA LISTA 1
106200900515     C*---------------------------------------------------------------*
106300900515     C     STAMP         BEGSR
106400961209     C* CLIENTE ESTERO: STAMPO IN INGLESE
106500961209     C     CLIEST        IFEQ      'S'
106600961209     C                   SETON                                        90
106700961209      * GESTISCO IL CAMBIO DEL CODICE CLIENTE X SALTO PAGINA SE ESTERO
106800961209     C     GCPSCM        IFNE      SCMPRE
106900961209     C                   SETON                                        OF
107000961209     C                   MOVE      GCPSCM        SCMPRE
107100961209     C                   ENDIF
107200961209     C                   ELSE
107300961209     C   90              SETON                                        OF
107400961209     C   90              SETOFF                                       90
107500961209     C                   ENDIF
107600961209      *
107700961209     C   OF
107800961209     CANN90              EXCEPT    TESTA
107900961209     C  N90              EXCEPT    DET
108000961209     C   OF
108100961209     CAN 90              EXCEPT    TESTAI
108200961209     C   90              EXCEPT    DETI
108300950110     C*
108400900515     C                   ENDSR
108500900515     C*---------------------------------------------------------------*
108600921014     C*   STAMPA LISTA 2 3 4 5
108700900515     C*---------------------------------------------------------------*
108800900515     C     STAMP1        BEGSR
108900950110     C*
109000900515     C     GCPCCR        IFNE      SAVCCR
109100950111     C                   EXSR      RICDES
109200900515     C                   EXCEPT    TESTA
109300900515     C                   Z-ADD     GCPCCR        SAVCCR            3 0
109400900515     C                   END
109500950110     C                   EXSR      STAMP
109600950110     C*
109700950110     C                   ENDSR
109800900515     C*---------------------------------------------------------------*
109900900515     C*   RICERCA DESCRIZIONI
110000900515     C*---------------------------------------------------------------*
110100900515     C     RICDES        BEGSR
110200950111     C*
110300921014     C* LUOGO DEPOSITO MERCE 'CCR'
110400950110     C                   MOVE      GCPCCR        ORGFIL
110500950110     C     ORGFIL        CHAIN     AZORG01L                           23
110600900515     C                   MOVEL     *BLANKS       DCCR             20
110700900515     C  N23ORGFVA        COMP      '*'                                    23
110800900515     C   23              MOVEL     *ALL'?'       DCCR
110900900515     C  N23              MOVEL     ORGDES        DCCR
111000900515     C* SE NON LO TROVO NELL'ORGANIGR. LO CERCO IN TAB. CORRISP.
111100900515     C     *IN23         IFEQ      '1'
111200900516     C                   MOVEL     '2B'          COD
111300900516     C                   MOVEL     *BLANK        KEY
111400921014     C                   MOVEL     GCPCCR        KEY
111500900515     C     KTAB          CHAIN     TABEL                              23
111600900515     C   23              MOVEL     *ALL'?'       DCCR
111700900515     C  N23              MOVEL     TBLUNI        DCCR
111800950111     C                   END
111900950111     C*
112000950111     C                   ENDSR
112100950111     C*--------------------------------------------------***
112200950111     C* Ricerco codice motivo apertura  giacenza
112300950111     C*--------------------------------------------------***
112400950111     C     MOTAPE        BEGSR
112500961209     C*  CLIENTE ITALIA
112600961209     C     CLIEST        IFNE      'S'
112700950111     C                   CLEAR                   DS2A
112800950111     C                   SETOFF                                       24
112900900516     C                   MOVEL     '2A'          COD
113000900516     C                   MOVEL     *BLANK        KEY
113100900516     C                   MOVEL     GCPCMC        KEY
113200900516     C     KTAB          CHAIN     TABEL                              23
113300950111     C     *IN23         IFEQ      '0'
113400950111     C     GCPCMC        ANDNE     'GEN'
113500950111     C                   MOVEL     TBLUNI        DS2A
113600950111     C                   ELSE
113700950111     C                   SETON                                        24
113800050218     c                   if        gcpapeaut = *blanks
113900950111     C                   Z-ADD     10            §FAS
114000050218     c                   else
114100050218     c* autoconfermate
114200050218     C                   Z-ADD     15            §FAS
114300050218     c                   end
114400050218     C     KNOT          CHAIN     tiGNP01L                           50
114500950111     C  N50              MOVEL     GNPDMC        DMC              50
114600950111     C   50              MOVEL     *BLANKS       DMC
114700950111     C                   END
114800961209     C                   ELSE
114900961209     C*  CLIENTE ESTERO
115000961209     C                   SETOFF                                       24
115100961209     C                   MOVEL     '2X'          COD
115200961209     C                   MOVEL     *BLANK        KEY
115300961209     C                   MOVEL     GCPCMC        KEY
115400961209     C     KTAB          CHAIN     TABEL                              23
115500961209     C     *IN23         IFEQ      '0'
115600961209     C     GCPCMC        ANDNE     'GEN'
115700961209     C                   MOVEL     TBLUNI        DMC
115800961209     C                   ELSE
115900961209     C                   Z-ADD     10            §FAS
116000050218     C     KNOT          CHAIN     tignp01L                           50
116100961209     C  N50              MOVEL     GNPDMC        DMC
116200961209     C   50              MOVEL     *BLANKS       DMC
116300961209     C                   END
116400961209     C                   ENDIF
116500950111     C*
116600900515     C                   ENDSR
116700900515     C***------------------------------------------------***
116800940329     C* CALCOLO GIORNI FESTIVI INTERCORRENTE TRA LE DUE DATE IN ESAME
116900940329     C*-----------
117000940329     C     FEST          BEGSR
117100990906     C                   CALL      'XGGFEST'
117200950110     C                   PARM                    WDAT8
117300940329     C                   ENDSR
117400940329     C***------------------------------------------------***
117500950110     OPRTF198   E            TESTA            01
117600950110     O               40      ST(1)              132
117700950110     O               40      D40GG1        Z    132
117800950110     O               41      ST(2)              132
117900950110     O               41      D40GG2        Z    125
118000950110     O               42      ST(3)              132
118100950110     O               43      ST(4)              128
118200950110     O               43      D40GG4        Z    108
118300950110     O               43      DRD4               124 '  /  /    '
118400950110     O               43      DRA4               138 '  /  /    '
118500950110     O               44      ST(5)              122
118600950110     O               44      ST(6)              188
118700950110     O               44      D40GG5        Z    111
118800950111     O               44      DRD5               126 '  /  /    '
118900950110     O               44      DRA5               141 '  /  /    '
119000950110     O                       RSUT                20
119100950110     O                                           21 '/'
119200950110     O                       SIMFEL              24
119300961001     O                                          177 'FNLG41R'
119400961001     O                       OGGI               188 '  /  /    '
119500950110     O                                          193 'PAG.'
119600950110     O                       PAGE          Z    198
119700950110     O          E            TESTA            02
119800950110     O                       KNSIF               10
119900950110     O                       KNMUS               21
120000961001     O                       WTIME              186 '  :  :  '
120100950111     O          E   N40N43N44TESTA            03
120200961001     O                                              'PUNTO OPERATIVO :'
120300961001     O                       GCPCCR              22
120400961001     O                       DCCR                48
120500950111     O          E            TESTA       2  1
120600950110     O                       CMP1               198
120700950111     O          E            TESTA          1
120800950110     O                       ST(7)               66
120900950110     O               40      ST(8)              132
121000950116     O               40      ST(11)             198
121100950111     O               41      ST(9)              132
121200950116     O               41      ST(13)             198
121300950110     O               42      ST(8)              132
121400950116     O               42      ST(12)             198
121500950110     O               43      ST(8)              132
121600950111     O               43      ST(11)             198
121700950110     O               44      ST(10)             132
121800950111     O          E            TESTA
121900950110     O                       CMP2               198
122000950110     O          E            DET         1
122100950110     O                       GCPFGC        Z      3
122200950110     O                                            4 '-'
122300950110     O                       GCPNGC        Z     11
122400950110     O                       DATAGC              22 '  /  /    '
122500950110     O                       DATDXD              33 '  /  /    '
122600950110     O                       RAGMIT              59
122700950110     O                       RIFNUM        Z     75
122800950110     O                       GCPCMC              79
122900950110     O                       GCPLNP              83
123000950110     O                                           84 '-'
123100950110     O                       GCPNRS        Z     86
123200950110     O                       GCPNSP        Z     94
123300950110     O                       DATSPE             105 '  /  /    '
123400950110     O              N44      BLPNCL        Z    111
123500950110     O              N44      BLPPKF             120 '      , '
123600950110     O              N41N44   DESTI              136
123700051115     O              N41N44   ar9CAS        4    150
123800991119     O              N41N44 06                   150 '    '
123900991119     O              N41N44   STPIFT        4    166
124000991119     O              N41N44 07                   166 '   '
124100051115     O              N41N44   ar9VCA             154
124200991119     O              N41N44   AR6DIV             170
124300950111     O              N41N42N44DATDUR             181 '  /  /    '
124400950110     O               41      DS1                161
124500950111     O              N40N43N44DATDDM             181 '  /  /    '
124600950111     O              N40N43N44DATDED             192 '  /  /    '
124700950116     O              N41N42N44BLPGGA             190
124800950111     O               44      DATDCG             118 '  /  /    '
124900950111     O               44      DATDLM             132 '  /  /    '
125000950110     O               44 88   ST(15)             198
125100950110     O          E    44 88   DET            1
125200950116     O                       ST(14)             198
125300950110     O          E   N41N42N44DET         1
125400950111     O                    N24§2ADES             151
125500950111     O                     24DMC                126
125600961209      * TESTO IN INGLESE
125700961209     O          E            TESTAI           01
125800961209     O                       ST(16)             132
125900961209     O                       RSUT                20
126000961209     O                                           21 '/'
126100961209     O                       SIMFEL              24
126200961209     O                                          177 'FNLG41R'
126300961209     O                       OGGI               188 '  /  /    '
126400961209     O                                          193 'PAG.'
126500961209     O                       PAGE          Z    198
126600961209     O          E            TESTAI           02
126700961209     O                       ST(17)             132
126800961209     O                       KNSIF               10
126900961209     O                       KNMUS               21
127000961209     O                       WTIME              186 '  :  :  '
127100961209     O          E            TESTAI      2  1
127200961209     O                       CMP1               198
127300961209     O          E            TESTAI         1
127400961209     O                       ST(18)              66
127500961209     O                       ST(19)             132
127600961209     O                       ST(20)             198
127700961209     O          E            TESTAI
127800961209     O                       CMP2               198
127900961209     O          E            DETI        1
128000961209     O                       GCPFGC        Z      6
128100961209     O                                            8 '-'
128200961209     O                       GCPNGC        Z     15
128300961209     O                       DATAGC              33 '  /  /    '
128400961209     O                       RAGMIT              59
128500961209     O                       RIFNUM        Z     75
128600961209     O                       GCPLNP              83
128700961209     O                                           84 '-'
128800980205     O                       GCPNRS              86
128900980205     O                       GCPNSP              94
129000961209     O                       DATSPE             105 '  /  /    '
129100961209     O                       BLPNCL        Z    111
129200961209     O                       BLPPKF             120 '      , '
129300961209     O                       DESTI              136
129400051115     O                       ar9CAS        2    154
129500991119     O                     06                   154 '    '
129600051115     O                       ar9VCA             158
129700961209     O                       DATDUR             170 '  /  /    '
129800961209     O          E            DETI        1
129900961209     O                       DMC                130
130000961209**   ST
130100950110LISTA GIACENZE IN ATTESA DI DISPOSIZIONI MITTENTE DA OLTRE GG. XXX  1
130200950110      LISTA GIACENZE IN ATTESA DI CHIUSURA DA OLTRE GG. XXX         2
130300950110               LISTA GIACENZE IN ATTESA DI RIENTRO                  3
130400950110LISTA GIACENZE APERTE IN RITARDO OLTRE GG. XXX  DAL            AL   4
130500950110LISTA GIACENZE CHIUSE IN RITARDO DALLA PARTENZA GG. XXX DAL         5
130600950110     AL                                                             6
130700950111NR.GIACENZA APERTA IL  DATA COMUN MITTENTE                     RIF  7
130800950111.NUMERICO MOT  NUMERO E DATA SPEDIZIONE COLLI   PESO   DESTINATARI  8
130900950111.NUMERICO MOT  NUMERO E DATA SPEDIZIONE COLLI   PESO   D I S P O S  9
131000950111.NUMERICO MOT  NUMERO E DATA SPEDIZIONE  CHIU.ARRIVO CHIU.PARTENZA 10
131100991119O  CONTRASSEGNO        TOT.FATTURA     RIAPERTURA  PARTICOLARITA'  11
131200991119O  CONTRASSEGNO        TOT.FATTURA     IMM. DISP. ESEC.DISP.       12
131300950116 I Z I O N I    M I T T E N T E        IMM. DISP. ESEC.DISP.       13
131400950116   GIACENZA NON CHIUSA DALLA PARTENZA !                            14
131500961209                                                                   15
131600970618       LIST  OF  U N D E L I V E R A B L E   SHIPMENTS             16
131700961209            -  WAITING FOR YOUR INSTRUCTION  -                     17
131800961209PROTOCOLLE NUMBER       OPEN  ON     SENDER                    CLI 18
131900961209ENT REF.NR.      NR. DATE OF SHIPMENT   COLLI    WGT   CONSIGNEE   19
132000961209       C.O.D.               NEW UNDEL.                             20

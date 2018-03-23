000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200940921     H* FNLG07R *----------------------------------------------------*
000300900418     H*                                                              *
000400921012     H* STAMPA LETTERE AL MITTENTE + ELENCO RACC.TE + SCARICO DKT    *
000500900423     H*                                                              *
000600940927     H*  02 - ON CLOSE FNVAG                                         *
000700940224     H*  10 - LETTURA ARCHIVIO GIACENZE IN PARTENZA                  *
000800940224     H*  11 - NON UTILIZZATO                                         *
000900940224     H*  12 - INVIO PER POSTA                                        *
001000940224     H*  13 - OFF --> LINEA ESTERA                                   *
001100940224     H*  15 - INDICATORE DI COMODO                                   *
001200040630     H*  16 - ON invio mail                                          *
001300940224     H*  18 - LOKUP SU £7                                            *
001400940224     H*  21 - CLIENTI                                                *
001500940224     H*  22 - CHAIN SU FLBLP25C                                      *
001600940224     H*  23 - CHAIN SU AZORG01L                                      *
001700940224     H*  24 - CIENTI ESTERO                                          *
001800940224     H*  25 - CLIENTI ITALIA                                         *
001900940224     H*  30 - ON --> NON STAMPO NULLA/ERRORE                         *
002000940224     H*  40 - NON UTILIZZATO                                         *
002100940224     H*  49 - ON  --> STAMPA UNA GIACENZA PER PAGINA                 *
002200940224     H*  52 - ON  SECONDO AVVISO                                     *
002300940224     H*  60 - CALL QCMDEXEC                                          *
002400940224     H*  65 - CHAIN SU TABELLA 2F                                    *
002500940224     H*  67 - CHAIN SU TABELLA 3K                                    *
002600940224     H*  69 - CHAIN SU FLGNP                                         *
002700940224     H*  70 - INDICATORE DI COMODO                                   *
002800940224     H*  80 - RISTAMPA                                               *
002900940224     H*  81 - RISTAMPA X DATA STAMPA MODULO                          *
003000940224     H*  87 - PRIMA VOLTA                                            *
003100160523     H*  88 - SE DPD IN STAMPA LETEST                                *
003200940224     H*  90 - PRIMA VOLTA                                            *
003300900418     H*--------------------------------------------------------------*
003400050321     Ftigcp08L  UF   E           K DISK    commit
003500050321     Ftigcp02L  iF   E           K DISK
003600050214     F                                     RENAME(tigcp000:tigcp002)
003700050214     Ftigcp21L  iF   E           K DISK
003800050214     F                                     RENAME(tigcp000:tigcp21) prefix(x)
003900050214     Ftignp01L  IF   E           K DISK
004000940922     FFNBLP01L  IF   E           K DISK
004100051115     FFiar901L  IF   E           K DISK
004200060213     FFiar401L  IF   E           K DISK
004300050214     Ftigap01L  IF   E           K DISK
004400990730     FTITGC01L  IF   E           K DISK
004500900418     FCNACO00F  IF   E           K DISK
004600900608     FCNIND00F  IF   E           K DISK
004700940922     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
004800900424     FTABEL00F  IF   E           K DISK
004900040630     FTntbe01l  IF   E           K DISK
005000040630     FTfntc01l  IF   E           K DISK
005100900423     FQSYSPRT   O    F  132        PRINTER
005200931122     FLET132    O    F  132        PRINTER USROPN
005300931122     FLETGIA    O    F  132        PRINTER USROPN
005400961203     FLETEST    O    E             PRINTER USROPN
005500940922     F*--------------------------------------------------------------*
005600911112     D FIL£7           S              3    DIM(29)
005700120420     D CMD1            S             47    DIM(1) CTDATA PERRCD(1)
005800120420     D CMD2            S             47    DIM(1) CTDATA PERRCD(1)
005900931122     D CMD4            S             63    DIM(1) CTDATA PERRCD(1)
006000931122     D CMD5            S             41    DIM(1) CTDATA PERRCD(1)
006100960507     D CMD6            S             40    DIM(1) CTDATA PERRCD(1)
006200961203     D CMD7            S             41    DIM(1) CTDATA PERRCD(1)
006300961203     D CMD8            S             40    DIM(1) CTDATA PERRCD(1)
006400040906     D CMD9            S             46    DIM(2) CTDATA PERRCD(1)
006500921029     D RIG             S             35    DIM(2) CTDATA PERRCD(1)
006600930726     D CDB             S              2    DIM(99)
006700930726     D DES             S             15    DIM(99)
006800040929     D SK              S             66    DIM(41) CTDATA PERRCD(1)
006900940923     D SCH             S              1    DIM(32)
007000940922     D C2F             S              1    DIM(50)
007100960205     D D2F             S             69    DIM(50)
007200940922     D UNIORG        E DS          5000    EXTNAME(AZORG00F)
007300041111     D Est_UNIORG    E DS          5000    EXTNAME(AZORG00F) prefix(Est_)
007400900430     D DS3A          E DS
007500900510     D DS2G          E DS
007600050210     D DS20          E DS
007700900607     D DS2A          E DS
007800900607     D DS2B          E DS
007900911021     D DS2F          E DS
008000921013     D DS3K          E DS
008100950113     D DS15          E DS
008200000202     D OG143         E DS
008300000202     D DSTB          E DS
008400940923     D DSQT1         E DS
008500990929     D DBLP          E DS
008600990929     D DTASV         E DS
008700040119     D DS2z          E DS
008800040426     D DS7q          E DS
008900090609     D DS5e          E DS
009000030717     D TRUL90DS      E DS
009100960201     D FNLSN0        E DS                  EXTNAME(FNLSN0DS)
009200960201     D TRUL42        E DS                  EXTNAME(TRUL42DS)
009300960322     D TRUL43        E DS                  EXTNAME(TRUL43DS)
009400171122     D TRUL43_1      E DS                  EXTNAME(TRUL43DS1)
009500960201     D FNLG07        E DS                  EXTNAME(FNLG07DS)
009600050630     D FNLG09ds      E DS
009700160219     d fnlg24ds      e ds
009800170925      *ds per richiamo monitor variazioni
009900170925     d tigcp         e ds                  extname(tigcp00f)
010000170925     d fnlg33ds      e ds                  inz
010100170925     d tigcpa        e ds                  extname(tigcp00f)
010200170925     d                                     prefix(a)
010300170925     d tigcpb        e ds                  extname(tigcp00f)
010400170925     d                                     prefix(b)
010500970709     D* PARAMETRI PASSATI AL PGM TISI95R X CONTROLLO CAP
010600970709     D TISI95        E DS                  EXTNAME(TISI95DS)
010700941010     D DSUL41        E DS                  EXTNAME(TRUL41DS)
010800160311     D TR27DS        E DS                  EXTNAME(TRUL27DS1)
010900170116     d trulc7ds      E DS
011000150514     D fnlv59ds      E DS
011100160311     d  KS_CT                 74    118    dim(15)
011200160311     d  KS_pr                119    163    dim(15)
011300160311     d  KS_ie                164    178    dim(15)
011400040906     d fnlv55ds      e ds
011500040629     d trtcm1ds      e ds
011600040914     D dmra          e DS
011700110503     D dvpobrt       e DS
011800960205     D                 DS
011900960205     D  F2F                    1      1
012000960205     D  T2F                    2      2
012100960205     D  R2F                    3      3
012200960205     D  G2F                    4      4
012300960205     D  DFT                    5      5
012400961203     D  EST                    6      6
012500000913     D  VAG                    7      7
012600960205     D  UNI                    1     69
012700900423     D TCUDS           DS
012800940922     D  F3                     3      3
012900900423     D  F4                     4      4
013000900423     D  F56                    5      6
013100900502     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
013200900423     D  TCU                  398    697
013300900418     D                                     DIM(50)
013400900423     D  KCU                  698    847P 0
013500900418     D                                     DIM(50)
013600900418     D                                     PACKEVEN
013700931122     D*---- DS DI APPOGGIO PER OVRPRTF
013800931122     D APPDS           DS
013900931122     D  LIN                   37     39  0
014000931122     D  OVRAPP                 1     41
014100900502     D CNCR80        E DS
014200900608     D CNCR42        E DS
014300050607     d fnlg08ds      e ds
014400170116     d fnlg071ds     E DS
014500020826     d trul06ds      e ds
014600020826     d  lin£1                  1     90  0 dim(30)
014700900418     D KPJBA         E DS
014800020918     d savkpjbu        s                   like(kpjbu)
014900041111      **
015000041111     d est_Lnp         s                   like(GcpLnp)
015100041111     d est_DesLnp      s                   like(DesLnp)
015200041111     d est_DesTel      s                   like(TelLnp)
015300041111     d est_DesFax      s                   like(FaxLnp)
015400041111      **
015500940926     D FNLG73        E DS                  EXTNAME(FNLG73DS)
015600900423     D                 DS
015700940922     D                 DS
015800940922     D  GG                     1      2  0
015900940922     D  MM                     3      4  0
016000940922     D  AA                     5      8  0
016100940922     D  WDATA8                 1      8  0
016200940922     D                 DS
016300940923     D  GX                     1      2
016400940923     D  MX                     3      4
016500940923     D  AX                     5      8
016600940922     D  DATAU                  1      8  0
016700940922     D WLBDA8          DS
016800940922     D  G02DAT                 1      8  0
016900940922     D  G02INV                 9     16  0
017000940922     D  G02ERR                17     17
017100940922     D  G02TGI                18     22  0
017200021107     d data8           s              8  0
017300021107     d dataiso         s               d   datfmt(*iso)
017400021107     d dataeur         s               d   datfmt(*eur)
017500040329     d Wdatalav        DS
017600040329     d  dadata                 1      8  0
017700040329     d  adata                  9     16  0
017800040329     d  GioLav                17     21  0
017900120321     d MITTERR         c                   '@brt.it'
018000041022     D chkc            C                   const('''/§')
018100041022     D chkcs           C                   const('   ')
018200040721     d cost1           s            105
018300040721     d cost2           s            105
018400040721     d cost3           s            105
018500040721     d cost4           s            105
018600040721     d cost5           s            105
018700040721     d cost6           s            105
018800040721     d cost7           s            105
018900040721     d cost8           s            105
019000040721     d cost9           s            105
019100040721     d cost10          s            105
019200040721     d cost11          s            105
019300040721     d cost12          s            105
019400040721     d cost13          s            105
019500040721     d cost14          s            105
019600061214      *
019700061214     D TxtInOut        S           2048
019800061214     D ElencoChar      S            256
019900061214     D TipoElenco      S              1
020000061214     D CharSost        S              1
020100061214     D UpperCase       S              1
020200061214     D ChkNull         S              1
020300061214     D CharNull        S              1
020400061214     D EsitoBon        S              1
020500940922     I*--------------------------------------------------------------*
020600940922     IAZORG01L  AA
020700900423     C*---------------------------------------------------------------*
020800931217     C*  80 ON ---> RISTAMPA
020900931217     C*  81 ON ---> RICHIESTA RISTAMPA PER DATA
021000931217     C*
021100020826     C                   do        30            ii                3 0
021200020826     c                   setoff                                       10
021300020826     c                   if        lin£1(ii) = 0
021400020826     c                   leave
021500020826     c                   endif
021600990625     C   80
021700050214     CANN81KGIA          SETLL     tigcp02L
021800020826     c                   if        *in80 and *in81 or not *in80
021900050214     c     lin£1(ii)     setll     tigcp08l
022000020826     c                   endif
022100911018     C     *IN10         DOWEQ     '0'
022200920311     C   80
022300920311     CAN 81
022400050321     CORN80lin£1(ii)     READe     tigcp000                             1510
022500050321     C   15lin£1(ii)     READe(n)  tigcp000                               10
022600050321     c   15              iter
022700950214     C     *IN80         IFEQ      '1'
022800950214     C     *IN81         ANDEQ     '0'
022900050214     C     KGIA          READE     tigcp002                             1510
023000020826     c   10              goto      fine
023100950214     C  N15
023200950214     CANN10              DO
023300950214     C                   Z-ADD     GCPDMM        G02INV
023400950214     C                   Z-ADD     0             G02DAT
023500950214     C                   MOVEL     '3'           G02ERR
023600950214     C                   CALL      'XSRDA8'
023700950214     C                   PARM                    WLBDA8
023800950214     C                   Z-ADD     G02DAT        DATAU             8 0
023900950214     C                   END
024000950214     C                   END
024100950214     C*
024200940922     C     *IN15         IFEQ      '0'
024300940922     C     *IN10         ANDEQ     '0'
024400940923     C                   SETOFF                                       5230
024500041028      *   ma....
024600041028      *   devo prendere però in esame se sto facendo una ristampa di una
024700041028      *   mail e la voglio stampare in stampa normale per poi inviarla
024800041028      *   manualmente
024900041028     c                   move      'SI'          esegui_mail       2
025000160523     c                   clear                   parcel14         14
025100041028     **
025200041111     c                   if        d07ris = 'S' and d07tip = *blank and
025300041028     c                             (gcptcm = 'V' or gcptcm = 'W')
025400041028     **
025500041028     c                   move      'NO'          esegui_mail
025600041028     **
025700041028     c                   else
025800041028     ** altrimenti.....
025900041028     **   in tutti gli altri casi.......  controllo solo le E-mail xchè
026000040827      * le mail le invio solo con tipo specifico
026100040906     c                   if        gcptcm = 'V' and d07tip <> 'V' or
026200040906     c                             gcptcm = 'W' and d07tip <> 'W'
026300040827     c                   iter
026400040827     c                   endif
026500041028     **
026600041028     c                   endif
026700940922     C*-----------------------------------------------------------------*
026800940922     C* Verifico se la fil. apertura è presente in £7
026900940922     C  N30              DO
027000940922     C                   MOVE      GCPLNP        LNP§              3
027100940922     C     LNP§          LOOKUP    FIL£7                                  18
027200940922     C* Reperisco dati relativo al tipo modulo mitt.
027300940923     C                   Z-ADD     1             XX                3 0
027400940922     C     GCPTCM        LOOKUP    C2F(XX)                                32
027500961212     C     D07TIP        IFEQ      *BLANKS
027600961212     C     D07TIP        OREQ      GCPTCM
027700960205     C                   MOVEL     D2F(XX)       UNI
027800970205     C* Se tipo invio manuale, sono una primo livello e
027900970205     C* non ho specificato il tipo comunicazione non elab.
028000020730     C     SimTpP        IFNE      '2'
028100970205     C     T2F           ANDEQ     'B'
028200970205     C     D07TIP        ANDEQ     *BLANKS
028300970205     C                   SETOFF                                       32
028400970205     C                   END
028500970203     C                   ELSE
028600970203     C                   SETOFF                                       32
028700961212     C                   END
028800160219      *
028900960205     C  N87              MOVEL     G2F           WSVG2F            1
029000960205     C  N87              MOVEL     C2F(XX)       WSVC2F            1
029100160219      *
029200941222     C* Non considero la giacenza se
029300941222     C*     non è previsto l'invio della comunicazione al mittente
029400941222     C*     sono una 2° livello e il tipo invio comun. è per posta
029500941222     C*     ho un invio manuale e la filiale non è gestita in £7
029600160219      *
029700940922     C     *IN32         IFEQ      '0'
029800941222     C                   SETON                                        30
029900970206     C                   MOVEL     T2F           WT2F
030000941222     C                   ELSE
030100160219      *
030200160219      * controlla se si tratta di una bolla DPD con tanto di PARCEL
030300160219      *   da cui prendere il DEPOT
030400160219     c                   clear                   fnlg24ds
030500160523     c                   clear                   parcel14         14
030600160219     c                   z-add     GCPAAS        i24aas
030700160219     c                   z-add     GCPLNP        i24lnp
030800160219     c                   z-add     GCPNRS        i24nrs
030900160219     c                   z-add     GCPNSP        i24nsp
031000160219     c                   call      'FNLG24R'
031100160219     C                   parm                    fnlg24ds
031200160219      *
031300960306     C*  Controllo se esiste un luogo di invio comunicazione di giac.
031400960306     C                   CLEAR                   TRUL43
031500171122     C                   CLEAR                   TRUL43_1
031600160219     c*
031700160219     c*  se c'è il parcel allora deve passare il Depot e verificare
031800160219     c*  se presente l'indirizzo mail del DEPOT mittente per inviare
031900160219     c*  l'informazione di apertura giacenza.
032000160219     c                   if          o24npr <> *blank
032100160303     c                   if        %subst(o24npr:1:4)<>'0844' and
032200160303     c                             %subst(o24npr:1:4)<>'0845'
032300171122     c                   movel     o24dep4       D43DEPOT
032400171122     c                   if          D43DEPOT = *blank
032500160219     c                   movel     o24npr        D43DEPOT
032600171122     c                   end
032700160523     c                   movel     o24npr        parcel14
032800171122     c                   movel     o24dep7       D43DEP7
032900160303     c                   end
033000160219     c                   end
033100160219     c*
033200960306     C                   Z-ADD     GCPSCM        D43KSC
033300960306     C                   CALL      'TRUL43R'
033400960306     C                   PARM                    TRUL43
033500171122     C                   PARM                    TRUL43_1
033600160219      *
033700960618     C     D43PER        COMP      *BLANKS                            0404
033800970819     C                   MOVEL     D43PER        ATTPER           35
033900160219      *
034000960201     C*  Reperisco dati cliente
034100960201     C                   EXSR      CHNIND
034200961205     C*------------------------------------------------------------*
034300961205     C*  No stampo la giacenza se il tipo invio non prevede stampa
034400961205     C     F2F           IFEQ      'N'
034500961205     C*------------------------------------------------------------*
034600961205     C*  No stampo la giacenza se il tipo invio è per posta e sono
034700961205     C*  una secondo livello
034800961205     C     WT2F          OREQ      'P'
034900020730     C     SimTpP        ANDEQ     '2'
035000961205     C*------------------------------------------------------------*
035100961205     C*  Se ho lanciato l'elaborazione con il profilo della primo
035200961205     C*  livello ma specificando una filiale di secondo stampo le
035300961205     C*  raccomandate solo se la lnp e nella £7 della filiale rich.
035400961205     C     WT2F          OREQ      'P'
035500020730     C     SimTpP        ANDNE     '2'
035600020809     C     D07FIL        ANDNE     SimFEl
035700961205     C     *IN18         ANDEQ     '0'
035800961205     C*------------------------------------------------------------*
035900961205     C*  No stampo la giacenza se il tipo invio è per fax e sono
036000961205     C*  una secondo livello
036100960201     C     WT2F          OREQ      'F'
036200020730     C     SimTpP        ANDEQ     '2'
036300961205     C*------------------------------------------------------------*
036400961205     C*  Se ho lanciato l'elaborazione con il profilo della primo
036500961205     C*  livello ma specificando una filiale di secondo stampo i
036600961205     C*  fax solo se la lnp e nella £7 della filiale rich.
036700961205     C     WT2F          OREQ      'F'
036800020730     C     SimTpP        ANDNE     '2'
036900020809     C     D07FIL        ANDNE     SimFEl
037000961205     C     *IN18         ANDEQ     '0'
037100961205     C*------------------------------------------------------------*
037200961205     C*  Stampo gli invii a mano solo se la lnp è in £7
037300960201     C     WT2F          OREQ      'B'
037400940922     C     *IN18         ANDEQ     '0'
037500961205     C*------------------------------------------------------------*
037600961205     C*  I tipi invii in lingua straniera vengono inviati solo dalla
037700961205     C*  filiale che gestisce in £7 la lnp estera
037800961205     C     WT2F          OREQ      'F'
037900961205     C     EST           ANDEQ     'E'
038000961205     C     GCPLNP        ANDNE     D07FIL
038100961211     C*
038200961211     C     WT2F          OREQ      'P'
038300961211     C     EST           ANDEQ     'E'
038400961211     C     GCPLNP        ANDNE     D07FIL
038500040629     C*
038600961205     C*------------------------------------------------------------*
038700940922     C                   SETON                                        30
038800940922     C                   END
038900950323     C*
039000950323     C                   END
039100941222     C*
039200940922     C                   END
039300170214     C*  No stampo la giacenza se il tipo invio non prevede stampa
039400170214     C*  ma ad ogni modo verifico se devo avvisare il destinatario
039500170221     c*m                 if        gcpriap = *blank and
039600170221     c*m                           f2f = 'N'
039700170221     c*m                 exsr      AvvisaDest
039800170221     c*m                 endif
039900960205     C*-----------------------------------------------------------------*
040000960205     C* Se specificato un tipo comunicazione controllo se corrisponde
040100960205     C  N30D07TIP        IFNE      *BLANKS
040200960205     C     GCPTCM        COMP      D07TIP                             3030
040300960205     C                   END
040400940922     C*-----------------------------------------------------------------*
040500900423     C** SE SI è IN FASE DI RISTAMPA SI ELABORANO SOLO LE GIACENZE
040600920309     C** CON FASE = 20 ALTRIMENTI QUELLE = 15
040700900509     C** E ENTRO I LIMITI DI ELABORAZIONE
040800911018     C   80
040900911018     CANN30GCPFAS        COMP      20                                 3030
041000940922     C   80
041100940922     CANN30
041200940922     CAN 81GCPDMM        COMP      D07DAT                             3030
041300911205     C  N30GCPFRG        COMP      0                                  3030
041400911111     C  N80
041500911111     CANN30GCPDMM        IFNE      0
041600911018     C                   SETON                                        3010
041700911018     C                   END
041800920310     C  N80
041900920310     CANN30
042000920310     CANN10GCPFAS        IFEQ      15
042100920310     C                   SETOFF                                       30
042200920310     C                   ELSE
042300920310     C                   SETON                                        30
042400900509     C                   END
042500911018     C  N30              DO
042600900423     C** AGGANCIO FILE STORICO
042700051115     C     KGAP          CHAIN     Fiar901L                           22
042800940923     C* Note su bolla
042900060213     C                   MOVEL     '8'           ar4TRC
043000060213     C                   MOVEL     *BLANKS       ar4NOT
043100060213     C     Kar4          CHAIN     Fiar401L                           22
043200940923     C     KGAP          CHAIN     FNBLP01L                           22
043300911018     C   22              EXCEPT    ERR
043400911018     C  N22              DO
043500931220     C     PREKSC        IFEQ      0
043600961218     C                   Z-ADD     BLPAAS        WSVAAS
043700961218     C                   Z-ADD     BLPMGS        WSVMGS
043800961218     C                   MOVEL     BLPNZD        WSVNZD
043900961218     C                   MOVEL     BLPTSP        WSVTSP
044000961218     C                   Z-ADD     BLPLNP        WSVLNP
044100961218     C                   Z-ADD     BLPCTR        WSVCTR
044200931220     C                   MOVE      GCPSCM        PREKSC
044300960412     C                   MOVE      GCPLNP        PRELNP            3 0
044400961218     C                   MOVE      GCPLNA        WSVLNA
044500940202     C                   END
044600931220     C* VERIFICO IL TIPO DI INVIO
044700931220     C                   MOVE      PREKSC        C8899             4
044800931220     C     PREKSC        IFNE      GCPSCM
044900931220     C     SAVFTP        ANDEQ     'B'
045000931220     C     C8899         OREQ      '8888'
045100931220     C     SAVFTP        ANDEQ     'B'
045200931220     C     C8899         OREQ      '9999'
045300931220     C     SAVFTP        ANDEQ     'B'
045400931220     C*
045500931220     C  N80              EXSR      STBOL
045600931220     C                   Z-ADD     0             GIANGC
045700931220     C                   END
045800961203     C*
045900961203     C** TESTO COMUNICAZIONE NON IN INGLESE
046000961203     C     EST           IFNE      'E'
046100961203     C** CODICE BOLLA
046200930726     C                   MOVEL     '3A'          COD
046300930726     C                   MOVEL     *BLANKS       KEY
046400940923     C                   MOVEL     BLPCBO        KEY
046500930726     C     KTAB          CHAIN     TABEL                              70
046600930726     C  N70              DO
046700930727     C                   Z-ADD     1             Y                 2 0
046800930726     C                   MOVEL     TBLUNI        DS3A
046900930726     C     §3ATB1        LOOKUP    CDB(Y)                                 25
047000930727     C   25              MOVEL     DES(Y)        PORTO            15
047100930726     C  N25              MOVEL     *BLANKS       PORTO
047200930726     C                   END
047300030911     c                   movel     §3atb1        mitdes            1
047400030911     c                   clear                   mitdesd          12
047500030911     c                   if        mitdes = 'F'
047600030911     c                   eval      mitdesd = 'Mittente    '
047700030911     c                   else
047800030911     c                   eval      mitdesd = 'Destinatario'
047900030911     c                   endif
048000900430     C** TIPO SPEDIZIONE
048100090520     C                   MOVEL     *BLANKS       DESTSP           15
048200090520     C                   MOVEL     '5E'          COD
048300090520     C                   MOVEL     *BLANKS       KEY
048400090520     C                   MOVEL     blptsp        KEY
048500090520     C     KTAB          CHAIN     TABEL                              70
048600090609     C  N70              MOVEL     TBLUNI        ds5e
048700090609     C  N70              MOVEL     §5EDES        destsp
048800900423     C** RICERCO DESCRIZIONE DELLA FILIALE DI PARTENZA
048900930318     C* MI SALVO IL RECORD VECCHIO
049000940923     C     GCPLNP        CHAIN     AZORG01L      UNIORG               23
049100911018     C   23              EXCEPT    ERR
049200911018     C  N23              DO
049300900423     C                   MOVEL     *BLANKS       DESLNP           20
049400900423     C                   MOVEL     *BLANKS       INDLNP           20
049500900423     C                   Z-ADD     0             CAPLNP            5 0
049600900423     C                   MOVEL     *BLANKS       LOCLNP           20
049700900423     C                   MOVEL     *BLANKS       PROLNP            2
049800900423     C                   MOVEL     *BLANKS       FAXLNP           13
049900900423     C                   MOVEL     *BLANKS       TLXLNP           13
050000900423     C                   MOVEL     *BLANKS       TELLNP           13
050100900423     C                   MOVEL     ORGDES        DESLNP
050200900423     C                   MOVEL     ORGIND        INDLNP
050300900430     C                   Z-ADD     ORGCPF        CAPLNP
050400900423     C                   MOVEL     ORGLOC        LOCLNP
050500900423     C                   MOVEL     ORGPRO        PROLNP
050600900423     C                   MOVEL     ORGTEL        TELLNP
050700900423     C                   MOVEL     ORGFAX        FAXLNP
050800900423     C                   MOVEL     ORGTLX        TLXLNP
050900041111      *
051000041111     C                   MOVEL     ORGDE3        OG143
051100041111     c                   clear                   est_DesLnp
051200041111     c                   clear                   est_DesTel
051300041111     c                   clear                   est_DesFax
051400041111     C**** sostituito con il campo Network la gestione dei vecchi flags
051500041111     C     §OgNTW        ifeq      'FED'
051600041111     C     §OgNTW        oreq      'EEX'
051700041111     C     §OgNTW        oreq      'EUP'
051800041111     c                   move      gcpLNP        est_Lnp
051900041111     C                   exsr      Hub_Estero
052000041111     c                   if        est_DesLnp <> *blank
052100041111     c                   movel     est_DesLnp    desLNP
052200041111     c                   movel     est_DesTel    telLNP
052300041111     c                   movel     est_DesFax    faxLNP
052400041111     C                   end
052500041111     C                   end
052600041111      *
052700900423     C                   END
052800961211     C                   END
052900900511     C** RICERCO DESCRIZIONE DEL COD.ELABORATORE
053000961202     C                   Z-ADD     D07FIL        ORGFIL
053100940923     C     ORGFIL        CHAIN     AZORG01L      UNIORG               70
053200900511     C                   MOVEL     *BLANKS       DESELA           20
053300940922     C  N70              DO
053400950123     C                   MOVEL     ORGDES        MITRGS
053500950123     C                   MOVEL     ORGIND        MITIND
053600950123     C                   MOVEL     ORGCPF        MITCPF
053700950123     C                   MOVEL     ORGLOC        MITLOC
053800950123     C                   MOVEL     ORGPRO        MITPRO
053900900511     C                   MOVEL     ORGDES        DESELA
054000900511     C                   END
054100961211     C*
054200961211     C** TESTO COMUNICAZIONE NON IN INGLESE
054300961211     C     EST           IFNE      'E'
054400900607     C** RICERCO DESCRIZIONE LUOGO DEPOSITO MERCE SULLA TABELLA
054500900607     C** SE NON LO TROVO LO RICERCO SULL'ORGANIGRAMMA
054600900502     C                   MOVEL     '2B'          COD
054700900502     C                   MOVEL     *BLANKS       KEY
054800900607     C                   MOVEL     GCPCCR        KEY
054900900502     C     KTAB          CHAIN     TABEL                              70
055000900607     C  N70              MOVEL     TBLUNI        DS2B
055100900607     C   70              DO
055200940923     C     GCPCCR        CHAIN     AZORG01L      UNIORG               70
055300900607     C                   MOVEL     *BLANKS       §2BDES
055400900607     C                   MOVEL     *BLANKS       §2BIND
055500900607     C                   MOVEL     *BLANKS       §2BLOC
055600900607     C                   MOVEL     *ZEROS        §2BCAP
055700900607     C                   MOVEL     *BLANKS       §2BPRO
055800900607     C  N70              DO
055900900607     C                   MOVEL     ORGDES        §2BDES
056000900607     C                   MOVEL     ORGIND        §2BIND
056100900607     C                   MOVEL     ORGLOC        §2BLOC
056200900607     C                   Z-ADD     ORGCPF        §2BCAP
056300900607     C                   MOVEL     ORGPRO        §2BPRO
056400900502     C                   END
056500900607     C                   END
056600900607     C** RICERCO COD.MOTIVO GIACENZA SULLA TABELLA
056700900607     C                   MOVEL     '2A'          COD
056800900607     C                   MOVEL     *BLANKS       KEY
056900900607     C                   MOVEL     GCPCMC        KEY
057000900607     C     KTAB          CHAIN     TABEL                              70
057100900607     C  N70              MOVEL     TBLUNI        DS2A
057200961203     C*
057300961203     C** TESTO COMUNICAZIONE IN INGLESE
057400961203     C                   ELSE
057500961203     C** RICERCO COD.MOTIVO GIACENZA SULLA TABELLA
057600961203     C                   MOVEL     '2X'          COD
057700961203     C                   MOVEL     *BLANKS       KEY
057800961203     C                   MOVEL     GCPCMC        KEY
057900961203     C     KTAB          CHAIN     TABEL                              70
058000961203     C  N70              MOVEL     TBLUNI        WMOTEV           75
058100961203     C                   END
058200961203     C*
058300911018     C* PRELEVO EVENTUALE ULTERIORE DESCRIZIONE
058400911018     C                   Z-ADD     10            FASE
058500911205     C                   Z-ADD     1             NPR
058600050214     C     KNOT          CHAIN     tignp01L                           69
058700911018     C  N69              MOVEL     GNPDMC        ULTDMC
058800911018     C   69              MOVEL     *BLANKS       ULTDMC           50
058900961203     C*
059000961203     C***  NO ESTERO
059100961203     C     EST           IFNE      'E'
059200961203     C*
059300930728     C* SE IL MOTIVO GIACENZA E' GENERICO GLI MUOVO L'ULT. MOTIVO
059400930728     C     §2ADES        IFEQ      *BLANKS
059500930728     C                   MOVEL     ULTDMC        §2ADES
059600930728     C                   MOVEL     *BLANKS       ULTDMC
059700930728     C                   END
059800900423     C** SE IL CODICE è CODIFICATO PRENDO I DATI DELLA RAG.SOC.
059900900423     C** DAL PDC SE è CODICE 88 O 99 PRENDO I DATI O DALL'ANAGRAFICA
060000900423     C** D'APPOGGIO SE ESISTE O DALLO STORICO BOLLE
060100940922     C                   MOVEL     *BLANKS       RAG              35
060200931217     C                   MOVEL     *BLANKS       VIA
060300931217     C                   MOVEL     *BLANKS       CIT
060400931217     C                   MOVEL     *BLANKS       PRV               2
060500940923     C                   MOVEL     *BLANKS       CAP               9
060600941007     C                   MOVEL     *BLANKS       NZM               3
060700960306     C     D43TRV        IFNE      'S'
060800960306     C*
060900900423     C     CODMIT        IFNE      8888
061000900423     C     CODMIT        ANDNE     9999
061100960201     C     KACO          CHAIN     CNACO000                           26
061200960201     C   26              EXCEPT    ERR
061300960201     C  N26              DO
061400900423     C                   MOVEL     ACORAG        RAG
061500940922     C                   MOVEL     INDVIA        VIA              35
061600940923     C                   MOVEL     INDCIT        CIT              35
061700921026     C                   MOVEL     INDPRV        PRV
061800050223     C                   MOVEL     INDCAe        CAP
061900941007     C                   MOVEL     INDSTA        NZM
062000900423     C                   END
062100900423     C                   ELSE
062200050214     C     KGAP          CHAIN     tigap000                           70
062300931217     C     *IN70         IFEQ      '0'
062400931217     C                   MOVEL     GAPRSM        RAG
062500940922     C                   MOVEL     GAPINM        VIA
062600940922     C                   MOVEL     GAPLOM        CIT
062700931217     C                   MOVEL     GAPPRM        PRV
062800940923     C                   MOVEL     GAPCAM        CAP
062900940923     C                   MOVEL     GAPNZM        NZM
063000931217     C                   ELSE
063100931217     C                   MOVEL     BLPRSM        RAG
063200940922     C                   MOVEL     BLPINM        VIA
063300940922     C                   MOVEL     BLPLOM        CIT
063400931217     C                   MOVEL     BLPPRM        PRV
063500940923     C                   MOVEL     BLPCAM        CAP
063600940923     C                   MOVEL     BLPNZM        NZM
063700900423     C                   END
063800931217     C                   END
063900960306     C*  Controllo se devo reimpostare i dati dal luogo
064000960306     C                   ELSE
064100960306     C                   MOVEL     D43RAG        RAG
064200960306     C                   MOVEL     D43IND        VIA
064300960306     C                   MOVEL     D43LOC        CIT
064400960306     C                   MOVEL     D43PRO        PRV
064500960306     C                   MOVEL     D43CAP        CAP
064600960306     C                   MOVEL     D43NAZ        NZM
064700960306     C                   END
064800961203     C**
064900040426     C**ricerca limite massimo riaperture per avviso in stampa se
065000040426      * esiste la particolarità altrimenti lascia 2
065100040426     C                   clear                   progmax           2 0
065200040426     C                   MOVEL     *BLANKS       KEY
065300040426     C                   MOVEL     '7Q'          COD
065400040426     c                   if        blpgga <> *blank
065500040426     C                   MOVEL     blpgga        KEY
065600040426     c                   else
065700040426     C                   MOVEL     'GR'          KEY
065800040426     c                   endif
065900040426     c
066000040426     C     KTAB          CHAIN     TABEL                              70
066100040426     C  N70              MOVEL     TBLUNI        DS7q
066200040426     C  N70              z-add     §7q5rp        progmax
066300040426     C** SE è UNA GIACENZA RIAPERTA STAMPA AVVISO progressivo
066400040426     c                   z-add     0             xprog             2 0
066500050214     C     GCPriap       IFne      *blank
066600050630     c                   clear                   fnlg09ds
066700050630     c                   eval      d09tla = 'R'
066800050630     c                   eval      d09aas = GCPAASa
066900050630     c                   eval      d09lnp = GCPLNPa
067000050630     c                   eval      d09nrs = GCPNRSa
067100050630     c                   eval      d09nsp = GCPNSPa
067200050630     c                   movel(p)  fnlg09ds      kpjbu
067300050630     C                   CALL      'FNLG09R'
067400050630     C                   PARM                    kpjba
067500050630     c                   movel(p)  kpjbu         fnlg09ds
067600050630     c     d09cnt        sub       d09ced        xprog
067700961203     C                   SETON                                        52
067800961203     C                   END
067900961203     C** RIBALTAMENTO DATA SPEDIZIONE
068000961203     C                   MOVEL     BLPAAS        G02INV
068100961203     C                   MOVE      BLPMGS        G02INV
068200961203     C                   Z-ADD     0             G02DAT
068300961203     C                   MOVEL     '3'           G02ERR
068400961203     C                   CALL      'XSRDA8'
068500961203     C                   PARM                    WLBDA8
068600961203     C                   Z-ADD     G02DAT        DATSPE            8 0
068700961203     C* PRENDO ULTIME 3 CIFRE DEL SEGNACOLLO AL
068800961203     C                   MOVE      BLPNCA        STANCA            3 0
068900961203     C*
069000961203     C***  SI ESTERO
069100961203     C                   ELSE
069200961203     C*  Reperisco i dati mittente per la stampa della lettera
069300961203     C*  comunicazione in inglese
069400961203     C                   EXSR      DATMIT
069500961203     C                   END
069600921029     C*
069700931123     C   87PREKSC        IFNE      GCPSCM
069800930727     C     C8899         OREQ      '8888'
069900961203     C     C8899         OREQ      '9999'
070000961203     C*** ESTERO
070100961203     C     EST           IFNE      'E'
070200961203     C     CONT          IFGT      50
070300930729     C                   EXCEPT    RIGA1
070400930729     C                   END
070500961203     C*
070600961203     C                   ELSE
070700961203     C     CONT          IFGT      50
070800961203     C     WT2F          IFEQ      'F'
070900961203     C                   ADD       1             STPPAG
071000961203     C                   WRITE     TESFAX
071100961203     C                   END
071200961203     C                   WRITE     TESGEN
071300961203     C                   END
071400961203     C                   END
071500961203     C*
071600940627     C** RICERCO SU TARIFFA I GG DA STAMPARE SU MODULO
071700950901     C  N49              Z-ADD     PREKSC        KSC
071800961203     C     EST           IFNE      'E'
071900961218     C                   Z-ADD     WSVAAS        WAAS
072000961218     C                   Z-ADD     WSVMGS        WMGS
072100961218     C                   MOVEL     WSVNZD        WNZD
072200961218     C                   MOVEL     WSVTSP        WTSP
072300961218     C                   Z-ADD     WSVLNP        WLNP
072400961218     C                   Z-ADD     WSVCTR        WCTR
072500961218     C                   MOVE      WSVLNA        WLNA
072600961203     C  N49              EXSR      CHKTFG
072700961203     C  N49              EXSR      CHKPOP
072800130319      *raccomandata
072900130319     c                   if        not *in16
073000130319     C  n49              EXCEPT    FINES
073100130412     c                   if        cont >= 55
073200130319     c                   except    testa
073300130319     c                   endif
073400130319     c  n49              except    fines2
073500130319     c                   else
073600130319      *mail
073700130319     C  N49              EXCEPT    FINESm
073800130319     c                   endif
073900961203     C                   ELSE
074000040906     c  n16
074100040906     CanN49              WRITE     FINAPE
074200040906     c   16
074300040906     CanN49              WRITE     finmail
074400961203     C  N49              WRITE     PIEDE
074500961203     C                   END
074600961203     C*
074700960205     C                   MOVEL     G2F           WSVG2F            1
074800960205     C                   MOVEL     C2F(XX)       WSVC2F            1
074900930727     C                   Z-ADD     0             PAGE
075000961203     C                   Z-ADD     0             STPPAG
075100940202     C                   SETOFF                                       12
075200960201     C     WT2F          IFEQ      'P'
075300940202     C                   SETON                                        12
075400120112     c*mm                exsr      ovrracc
075500940202     C                   END
075600940202     C                   SETON                                        13
075700971112     C     GCPLNP        CHAIN     AZORG01L      UNIORG               23
075800020717     C  N23              MOVEL     ORGDE3        OG143
075900020717     C**** sostituito con il campo Network la gestione dei vecchi flags
076000020717     C  N23§OgNTW        ifeq      'FED'
076100020717     C     §OgNTW        oreq      'EEX'
076200020717     C     §OgNTW        oreq      'EUP'
076300940202     C                   SETOFF                                       13
076400940202     C                   END
076500930728     C* PRIMA DI STAMPARE LA TESTATA MI COMPONGO LUOGO E DATA
076600960205     C     G2F           IFNE      'S'
076700961203     C*
076800040906     C                   EXSR      LUOGO
076900961203     C     EST           IFNE      'E'
077000040629      * per invio via mail overraida il file correttamente
077100040701     c                   setoff                                       16
077200040629     c                   if        wt2f = 'M'
077300041028     c                   if        esegui_mail = 'SI'
077400040701     c                   exsr      invxmail
077500040629     c                   exsr      preparaovr
077600041028     c                   end
077700040629     c                   endif
077800040708     C  n16              EXCEPT    TESTA
077900040708     C   16              EXCEPT    TESTAm
078000961203     C                   Z-ADD     18            CONT              2 0
078100961203     C                   ELSE
078200040906      * per invio via mail overraida il file correttamente
078300040906     c                   setoff                                       16
078400040906     c                   if        wt2f = 'M'
078500041028     c                   if        esegui_mail = 'SI'
078600040914     c                   exsr      invxmail
078700040906     c                   exsr      preparaovre
078800041028     c                   end
078900040906     c                   endif
079000961203     C     WT2F          IFEQ      'F'
079100961204     C                   Z-ADD     1             STPPAG
079200961203     C                   WRITE     TESFAX
079300961203     C                   END
079400961203     C                   WRITE     TESGEN
079500961203     C                   Z-ADD     13            CONT
079600961203     C                   END
079700961203     C*
079800930727     C                   END
079900931122     C                   END
080000930728     C*
080100930728     C  N87              SETON                                        87
080200950901     C                   SETOFF                                       49
080300960205     C     C2F(XX)       IFNE      WSVC2F
080400950901     C     CONT          ANDLE     50
080500960205     C     G2F           ANDNE     'S'
080600950901     C     WSVG2F        IFEQ      'S'
080700950901     C     *IN48         OREQ      '1'
080800950901     C                   SETOFF                                       12
080900960201     C     WT2F          IFEQ      'P'
081000950901     C                   SETON                                        12
081100120112     c*mm                exsr      ovrracc
081200950901     C                   END
081300950901     C                   SETON                                        13
081400971112     C     GCPLNP        CHAIN     AZORG01L      UNIORG               23
081500020717     C  N23              MOVEL     ORGDE3        OG143
081600020717     C**** sostituito con il campo Network la gestione dei vecchi flags
081700020717     C  N23§OgNTW        ifeq      'FED'
081800020717     C     §OgNTW        oreq      'EEX'
081900020717     C     §OgNTW        oreq      'EUP'
082000971112     C                   SETOFF                                       13
082100971112     C                   END
082200961203     C** ESTERO
082300040906     C                   EXSR      LUOGO
082400961203     C     EST           IFNE      'E'
082500040630      * per invio via mail overraida il file correttamente
082600040701     c                   setoff                                       16
082700040630     c                   if        wt2f = 'M'
082800041028     c                   if        esegui_mail = 'SI'
082900040701     c                   exsr      invxmail
083000040630     c                   exsr      preparaovr
083100041028     c                   end
083200040630     c                   endif
083300040708     C  n16              EXCEPT    TESTA
083400040708     C   16              EXCEPT    TESTAm
083500950901     C                   Z-ADD     18            CONT
083600961203     C                   ELSE
083700040906     c                   if        wt2f = 'M'
083800041028     c                   if        esegui_mail = 'SI'
083900040914     c                   exsr      invxmail
084000040906     c                   exsr      preparaovre
084100041028     c                   end
084200040906     c                   endif
084300961203     C     WT2F          IFEQ      'F'
084400961204     C                   Z-ADD     1             STPPAG
084500961203     C                   WRITE     TESFAX
084600961203     C                   END
084700961203     C                   WRITE     TESGEN
084800961203     C                   Z-ADD     13            CONT
084900961203     C                   END
085000961203     C*
085100950901     C                   END
085200950901     C                   END
085300950901     C*
085400950901     C                   SETOFF                                       48
085500960205     C     C2F(XX)       IFNE      WSVC2F
085600950901     C     WSVG2F        ANDNE     'S'
085700950901     C     CONT          IFGT      50
085800961203     C     EST           IFNE      'E'
085900950901     C                   EXCEPT    RIGA1
086000961203     C*
086100961203     C                   ELSE
086200961203     C     WT2F          IFEQ      'F'
086300961203     C                   ADD       1             STPPAG
086400961203     C                   WRITE     TESFAX
086500961203     C                   END
086600961203     C                   WRITE     TESGEN
086700961203     C                   END
086800961203     C*
086900950901     C                   END
087000961203     C*
087100950901     C                   SETON                                        48
087200950901     C** RICERCO SU TARIFFA I GG DA STAMPARE SU MODULO
087300961218     C                   Z-ADD     WSVAAS        WAAS
087400961218     C                   Z-ADD     WSVMGS        WMGS
087500961218     C                   MOVEL     WSVNZD        WNZD
087600961218     C                   MOVEL     WSVTSP        WTSP
087700961218     C                   Z-ADD     WSVLNP        WLNP
087800961218     C                   Z-ADD     WSVCTR        WCTR
087900040707     C                   MOVE      WSVLNA        WLNA
088000040707     C                   Z-ADD     GCPSCM        KSC               7 0
088100040707     C     EST           IFNE      'E'
088200040707     C                   EXSR      CHKTFG
088300040707     C                   EXSR      CHKPOP
088400130319      *raccomandata 16 off
088500130319     c                   if        not *in16
088600130319     C                   EXCEPT    FINES
088700130412     c                   if        cont >= 55
088800130319     c                   except    testa
088900130319     c                   endif
089000130319     c                   except    fines2
089100130319     c                   else
089200130319      *mail
089300130319     C                   EXCEPT    FINESm
089400130319     c                   endif
089500040707     C                   ELSE
089600040906     C  n16              WRITE     FINAPE
089700040906     C   16              WRITE     finmail
089800961203     C                   WRITE     PIEDE
089900961203     C                   END
090000961203     C*
090100950901     C                   END
090200960205     C                   MOVEL     G2F           WSVG2F
090300960205     C                   MOVEL     C2F(XX)       WSVC2F
090400930728     C*
090500950901     C     *IN90         IFEQ      *OFF
090600950901     C     CONT          ORGT      50
090700960205     C     G2F           OREQ      'S'
090800950901     C                   SETOFF                                       12
090900960201     C     WT2F          IFEQ      'P'
091000950901     C                   SETON                                        12
091100120112     c*mm                exsr      ovrracc
091200950901     C                   END
091300950901     C                   SETON                                        13
091400971112     C     GCPLNP        CHAIN     AZORG01L      UNIORG               23
091500020717     C  N23              MOVEL     ORGDE3        OG143
091600020717     C**** sostituito con il campo Network la gestione dei vecchi flags
091700020717     C  N23§OgNTW        ifeq      'FED'
091800020717     C     §OgNTW        oreq      'EEX'
091900020717     C     §OgNTW        oreq      'EUP'
092000950901     C                   SETOFF                                       13
092100950901     C                   END
092200961203     C*  NO ESTERO
092300040906     C                   EXSR      LUOGO
092400961203     C     EST           IFNE      'E'
092500040630      * per invio diverso da mail overraida il file correttamente
092600040701     c                   setoff                                       16
092700040701     c                   if        wt2f = 'M'
092800041028     c                   if        esegui_mail = 'SI'
092900040701     c                   exsr      invxmail
093000040701     c                   exsr      preparaovr
093100041028     c                   end
093200040701     c                   endif
093300040708     C  n16              EXCEPT    TESTA
093400040708     C   16              EXCEPT    TESTAm
093500961203     C                   Z-ADD     18            CONT
093600961203     C                   ELSE
093700961203     C*  ESTERO
093800040906     c                   if        wt2f = 'M'
093900041028     c                   if        esegui_mail = 'SI'
094000040914     c                   exsr      invxmail
094100040906     c                   exsr      preparaovre
094200041028     c                   end
094300040906     c                   endif
094400961203     C     WT2F          IFEQ      'F'
094500961204     C                   Z-ADD     1             STPPAG
094600961203     C                   WRITE     TESFAX
094700961203     C                   END
094800961203     C                   WRITE     TESGEN
094900961203     C                   Z-ADD     13            CONT
095000961203     C                   END
095100950901     C  N90              SETON                                        90
095200950901     C                   END
095300960201     C*--------------------------------------------------------*
095400960201     C*  Se non è cambiato destinatario lo imposto con i dati
095500960201     C*  della bolla
095600960201     C                   CLEAR                   FNLSN0
095700960201     C                   Z-ADD     BLPAAS        DN0AAS
095800960201     C                   Z-ADD     BLPLNP        DN0LNP
095900960201     C                   Z-ADD     BLPNRS        DN0NRS
096000960201     C                   Z-ADD     BLPNSP        DN0NSP
096100960201     C                   MOVEL     KPJBU         WSVKPJ          256
096200960201     C                   MOVEL     FNLSN0        KPJBU
096300960201     C                   CALL      'FNLSN0R'
096400960202     C                   PARM                    KPJBA
096500960201     C                   MOVEL     KPJBU         FNLSN0
096600960201     C                   MOVEL     WSVKPJ        KPJBU
096700961203     C*
096800961203     C** NO ESTERO  ---
096900961203     C     EST           IFNE      'E'
097000961203     C*
097100960201     C     DN0TRV        IFNE      'S'
097200940923     C                   MOVEL     BLPLOD        LOD              20
097300940923     C                   MOVEL     BLPPRD        PRD               2
097400941007     C                   MOVEL     BLPNZD        NZD               3
097500940923     C                   MOVEL     BLPRSD        RSD              25
097600940923     C                   MOVEL     BLPIND        IND              25
097700131122     C                   MOVEL     BLPIND        INDm             35
097800960201     C                   MOVEL     BLPCAD        CAD               9
097900960201     C                   ELSE
098000960201     C                   MOVEL     DN0LOD        LOD              20
098100960201     C                   MOVEL     DN0PRD        PRD               2
098200960201     C                   MOVEL     DN0NZD        NZD               3
098300960201     C                   MOVEL     DN0RSD        RSD              25
098400960201     C                   MOVEL     DN0IND        IND              25
098500131122     C                   MOVEL     DN0IND        INDm
098600960201     C                   MOVEL     DN0CAD        CAD               9
098700960201     C                   END
098800960201     C*--------------------------------------------------------*
098900021107     c                   movel     gcpagc        data8
099000021107     c                   move      gcpmgc        data8
099100021107     c                   move      data8         dataiso
099200021107     c                   move      dataiso       dataeur
099300021107     c                   move      dataeur       data8
099400040304     c                   setoff                                       55
099500040304     c                   clear                   datadcr           8 0
099600040304     c                   if        gcpcmc = '016' or gcpcmc = '023' or
099700040304     c                             gcpcmc = '100' or gcpcmc = '101'
099800040304     c                   if        gcpded > 0
099900040329     c                   move      gcpded        dadata
100000040329     c                   move      gcpded        adata
100100040329     c                   CALL      'XSRLAV8'
100200040329     c                   PARM                    Wdatalav
100300040329     c                   z-add     giolav        giolavs           1 0
100400040304     c                   move      gcpded        dataiso
100500040329     c                   do        *hival
100600040304     c     dataiso       adddur    1:*d          dataiso
100700040329     c                   move      gcpded        dadata
100800040329     c                   move      dataiso       adata
100900040329     c                   CALL      'XSRLAV8'
101000040329     c                   PARM                    Wdatalav
101100040329     c                   if        giolav = 0 or giolav > giolavs
101200040329     c                   leave
101300040329     c                   endif
101400040329     c                   enddo
101500040304     c                   move      dataiso       dataeur
101600040304     c                   move      dataeur       datadcr
101700040304     c                   seton                                        55
101800040304     c                   endif
101900040304     c                   endif
102000040929      *reperisce mittente mail da specificare a fine fax riferimento
102100040929     C  n16              EXsr      Repmitt
102200040708     C  n16              EXCEPT    RIGAS
102300040708     C   16              EXCEPT    RIGASm
102400931123     C                   ADD       10            CONT
102500961203     C*  Reperisco dati spedizione x estero
102600961203     C                   ELSE
102700961203     C                   EXSR      DATASP
102800040330     c*----------
102900040330     c                   setoff                                       56
103000040330     c                   clear                   stpDCR            8 0
103100040330     c                   if        gcpcmc = '016' or gcpcmc = '100' or
103200040330     c                             gcpcmc = '101'
103300040330     c**********                   or gcpcmc = '023'
103400040330     c                   if        gcpded > 0
103500040330     c                   move      gcpded        dadata
103600040330     c                   move      gcpded        adata
103700040330     c                   CALL      'XSRLAV8'
103800040330     c                   PARM                    Wdatalav
103900040330     c                   z-add     giolav        giolavs           1 0
104000040330     c                   move      gcpded        dataiso
104100040330     c                   do        *hival
104200040330     c     dataiso       adddur    1:*d          dataiso
104300040330     c                   move      gcpded        dadata
104400040330     c                   move      dataiso       adata
104500040330     c                   CALL      'XSRLAV8'
104600040330     c                   PARM                    Wdatalav
104700040330     c                   if        giolav = 0 or giolav > giolavs
104800040330     c                   leave
104900040330     c                   endif
105000040330     c                   enddo
105100040330     c                   move      dataiso       dataeur
105200040330     c                   move      dataeur       stpDCR
105300040330     c                   seton                                        56
105400040330     c                   endif
105500040330     c                   endif
105600040330     c*----------
105700961203     C                   END
105800961203     C*
105900960205     C     G2F           IFEQ      'S'
106000931123     C                   SETON                                        49
106100050609     c                   eval      d07ok = 'S'
106200940627     C** RICERCO SU TARIFFA I GG DA STAMPARE SU MODULO
106300940627     C                   Z-ADD     GCPSCM        KSC               7 0
106400961218     C                   Z-ADD     BLPAAS        WAAS
106500961218     C                   Z-ADD     BLPMGS        WMGS
106600961218     C                   MOVEL     BLPNZD        WNZD
106700961218     C                   MOVEL     BLPTSP        WTSP
106800961218     C                   Z-ADD     GCPLNP        WLNP
106900961218     C                   Z-ADD     BLPCTR        WCTR
107000961218     C                   MOVE      BLPLNA        WLNA
107100961203     C** NO ESTERO
107200961203     C     EST           IFNE      'E'
107300961203     C                   EXSR      CHKTFG
107400961203     C                   EXSR      CHKPOP
107500130319      *raccomandata 16 off
107600130319     c                   if        not *in16
107700130319     C                   EXCEPT    FINES
107800130412     c                   if        cont >= 55
107900130319     c                   except    testa
108000130319     c                   endif
108100130319     c                   except    fines2
108200130319     c                   else
108300130319      *mail
108400040708     C   16              EXCEPT    FINESm
108500130319     c                   endif
108600961203     C                   ELSE
108700961203     C** ESTERO
108800040906     c                   if        not *in16
108900040330     c                   if        *in56 = *on and stpDCR >0
109000040330     C                   WRITE     FINDCR
109100040330     c                   else
109200961203     C                   WRITE     FINAPE
109300040330     c                   end
109400040906     c                   else
109500040906     C                   WRITE     finmail
109600040906     c                   end
109700040330     C*
109800961203     C                   WRITE     PIEDE
109900961203     C                   END
110000961203     C*
110100930726     C                   END
110200950901     C*
110300930726     C*
110400960201     C     WT2F          IFEQ      'B'
110500921028     C                   ADD       1             GIANGC            3 0
110600911021     C                   END
110700921029     C*
110800911021     C** STAMPA RACC.TE SE IN TAB. TIPO COMUNICAZ. E' PREVISTA
110900911021     C** OPPURE STAMPA BOLLA PER PADRONCINO
111000911210     C                   MOVE      GCPSCM        QUATR             4 0
111100940922     C     D07RAC        IFEQ      'S'
111200960201     C     WT2F          IFEQ      'P'
111300940202     C     GCPSCM        ANDNE     SAVKSC
111400960201     C     WT2F          OREQ      'P'
111500911210     C     QUATR         ANDEQ     8888
111600960201     C     WT2F          OREQ      'P'
111700911210     C     QUATR         ANDEQ     9999
111800120113     C*mm                EXSR      STARAC
111900900423     C                   END
112000931217     C                   END
112100920521     C* MEMORIZZO CODICE CLIENTE PER STAMPA DISTINTA GIAC. APERTE
112200921029     C     SAVFRP        IFEQ      'S'
112300920521     C     GCPSCM        ANDNE     PREKSC
112400940927     C                   CLEAR                   FNLG73
112500940923     C                   MOVE      D07DAT        D73DTA
112600940923     C                   MOVE      D07DAT        D73DTD
112700940923     C                   MOVE      PREKSC        D73KSC
112800940923     C                   MOVE      KCI           D73KCC
112900940923     C                   MOVEL     FNLG73        KPJBU
113000940923     C                   MOVEL     'LG73'        KCOAZ
113100920525     C                   CALL      'BCH10'
113200920525     C                   PARM                    KPJBA
113300920525     C*
113400920521     C                   END
113500920521     C*
113600920518     C                   Z-ADD     GCPAAS        SAVAAS            2 0
113700920518     C                   Z-ADD     GCPLNP        SAVLNP            3 0
113800920518     C                   Z-ADD     GCPNRS        SAVNRS            2 0
113900920518     C                   Z-ADD     GCPNSP        SAVNSP            7 0
114000960201     C                   MOVE      WT2F          SAVFTP            1
114100960205     C                   MOVE      R2F           SAVFRP            1
114200911204     C                   Z-ADD     GCPSCM        PREKSC            7 0
114300961218     C                   Z-ADD     BLPAAS        WSVAAS
114400961218     C                   Z-ADD     BLPMGS        WSVMGS
114500961218     C                   MOVEL     BLPNZD        WSVNZD
114600961218     C                   MOVEL     BLPTSP        WSVTSP
114700961218     C                   Z-ADD     BLPLNP        WSVLNP
114800961218     C                   Z-ADD     BLPCTR        WSVCTR
114900960412     C                   MOVE      GCPLNP        PRELNP            3 0
115000961218     C                   MOVE      GCPLNA        WSVLNA
115100900423     C*
115200911018     C                   END
115300911018     C                   END
115400950323     C* SOLO SE NON SI è IN RISTAMPA...
115500950323     C     D07RIS        IFNE      'S'
115600950323     C     GCPFAS        ANDEQ     15
115700950323     C     GCPFRG        ANDEQ     0
115800950323     C     GCPDMM        ANDEQ     0
115900950329     C*
116000950329     C     D07TIP        IFEQ      *BLANKS
116100950329     C     GCPTCM        OREQ      D07TIP
116200961205     C*------------------------------------------------------*
116300961205     C*  Aggiorno giacenza se devo stampare le raccomandate  *
116400961205     C*  e sto lavorando come primo livello tranne le estere
116500960201     C     WT2F          IFEQ      'P'
116600961205     C     D07FIL        ANDEQ     SIMFEL
116700961205     C     EST           ANDNE     'E'
116800961205     C*------------------------------------------------------*
116900961205     C*  Aggiorno giacenza se devo stampare le raccomandate  *
117000961205     C*  delle filiali che gestisco in partenza se non mi    *
117100961205     C*  sono collegata come una secondo livello             *
117200961211     C     WT2F          OREQ      'P'
117300020730     C     SimTpP        ANDNE     '2'
117400961211     C     *IN18         ANDEQ     '1'
117500961205     C*------------------------------------------------------*
117600961205     C*  Aggiorno giacenza se devo stampare i fax e sto      *
117700961205     C*  lavorando come primo livello tranne i fax esteri    *
117800961203     C     WT2F          OREQ      'F'
117900020809     C     D07FIL        ANDEQ     SimFEl
118000961218     C     EST           ANDNE     'E'
118100961205     C*------------------------------------------------------*
118200961205     C*  Aggiorno giacenza se devo stampare i fax delle fil. *
118300961205     C*  che gestisco in partenza se non mi sono collegata   *
118400961205     C*  sono collegata come una secondo livello             *
118500961205     C     WT2F          OREQ      'F'
118600020730     C     SimTpP        ANDNE     '2'
118700961211     C     *IN18         ANDEQ     '1'
118800961205     C*------------------------------------------------------*
118900970210     C*  Se non devo stampare BOLLA aggiorno la giacenza
119000970210     C* se lnp è fra quelle che devo gestire e sono in iterr.
119100970210     C     WT2F          OREQ      'B'
119200950329     C     *IN18         ANDEQ     '1'
119300970206     C     D07TPL        ANDNE     'B'
119400970210     C*  Se non devo stampare nulla aggiorno la giacenza
119500970210     C*  se lnp è fra quelle che devo gestire
119600970210     C     WT2F          OREQ      ' '
119700970210     C     *IN18         ANDEQ     '1'
119800961205     C*------------------------------------------------------*
119900040707      * aggiorno se invio per e-mail
120000040629     C     WT2F          OREQ      'M'
120100950327     C                   EXSR      AGGGCP
120200950323     C* VERIFICO SE PER IL CLIENTE MITTENTE E' PREVISTO LO SCARICO
120300950323     C                   Z-ADD     49            LUNG
120400950323     C                   MOVEL     '3K'          COD
120500950323     C                   MOVEL     *BLANKS       KEY
120600950323     C                   MOVEL     GCPSCM        KEY
120700950323     C     KTAB          CHAIN     TABEL                              67
120800950323     C  N67              DO
120900950323     C                   MOVEL     TBLUNI        DS3K
121000950323     C     §3KGQI        IFEQ      'A'
121100950323     C     §3KGQI        OREQ      'E'
121200950323     C                   EXSR      SCAR
121300950323     C                   END
121400950329     C*
121500950329     C                   END
121600950323     C                   END
121700950323     C                   END
121800950327     C*
121900950327     C                   END
122000950323     C*
122100950323     C                   END
122200911018     C                   END
122300020826     c                   enddo
122400900423     C     FINE          TAG
122500931220     C*
122600940923     C     SAVFTP        IFEQ      'B'
122700931220     C  N80              EXSR      STBOL
122800931220     C                   Z-ADD     0             GIANGC
122900931220     C                   END
123000930727     C* STAMPO ULTIMO PIEDE DELLA PAGINA
123100931123     C     CONT          IFGT      50
123200970213     C     EST           IFNE      'E'
123300960202     C                   EXCEPT    RIGA1
123400961203     C* ESTERO
123500961203     C                   ELSE
123600961203     C     WT2F          IFEQ      'F'
123700961203     C                   ADD       1             STPPAG
123800961203     C                   WRITE     TESFAX
123900961203     C                   END
124000961203     C                   WRITE     TESGEN
124100961203     C                   END
124200961203     C*
124300930729     C                   END
124400961203     C*
124500940706     C     *IN87         IFEQ      '1'
124600940706     C     *IN49         ANDEQ     '0'
124700940706     C                   Z-ADD     PREKSC        KSC
124800961218     C                   Z-ADD     WSVAAS        WAAS
124900961218     C                   Z-ADD     WSVMGS        WMGS
125000961218     C                   MOVEL     WSVNZD        WNZD
125100961218     C                   MOVEL     WSVTSP        WTSP
125200961218     C                   Z-ADD     WSVLNP        WLNP
125300961218     C                   Z-ADD     WSVCTR        WCTR
125400961218     C                   MOVE      WSVLNA        WLNA
125500961203     C** NO ESTERO
125600961203     C     EST           IFNE      'E'
125700961203     C                   EXSR      CHKTFG
125800961203     C                   EXSR      CHKPOP
125900130319      *raccomandata 16 off
126000130319     c                   if        not *in16
126100130319     C                   EXCEPT    FINES
126200130412     c                   if        cont >= 55
126300130319     c                   except    testa
126400130319     c                   endif
126500130319     c                   except    fines2
126600130319     c                   else
126700130319      *mail
126800040708     C   16              EXCEPT    FINESm
126900130319     c                   endif
127000961203     C                   ELSE
127100961203     C** ESTERO
127200040906     C  n16              WRITE     FINAPE
127300040906     C   16              WRITE     finmail
127400961203     C                   WRITE     PIEDE
127500961203     C                   END
127600961203     C*
127700940706     C                   END
127800911031     C* STAMPA ULTIMA RACCOMANDATA O BOLLA
127900940923     C     SAVFRP        IFEQ      'S'
128000940927     C                   CLEAR                   FNLG73
128100940923     C                   MOVE      D07DAT        D73DTA
128200940923     C                   MOVE      D07DAT        D73DTD
128300940923     C                   MOVE      PREKSC        D73KSC
128400940923     C                   MOVE      KCI           D73KCC
128500940923     C                   MOVEL     FNLG73        KPJBU
128600940923     C                   MOVEL     'LG73'        KCOAZ
128700931123     C                   CALL      'BCH10'
128800931123     C                   PARM                    KPJBA
128900931217     C                   END
129000921016     C*
129100960507     C                   CLOSE     LETGIA
129200961203     C                   CLOSE     LETEST
129300960507     C* GUARDO CHE UTENTE HA FATTO LA RICHIESTA
129400960507     C                   SETOFF                                       60
129500960507     C                   MOVEL     *BLANKS       COMMAN
129600960507     C                   MOVEA     CMD6(1)       COMMAN           80
129700960507     C                   CALL      'QCMDEXC'                            60
129800960507     C                   PARM                    COMMAN
129900960507     C                   PARM                    LUNG             15 5
130000961203     C* GUARDO CHE UTENTE HA FATTO LA RICHIESTA
130100961203     C                   SETOFF                                       60
130200961203     C                   MOVEL     *BLANKS       COMMAN
130300961203     C                   MOVEA     CMD8(1)       COMMAN           80
130400961203     C                   CALL      'QCMDEXC'                            60
130500961203     C                   PARM                    COMMAN
130600961203     C                   PARM                    LUNG             15 5
130700160311     c
130800020222      *RICHIAMO PROGRAMMA PER CHIUSURA FILE
130900020222     C                   CLEAR                   TR27DS
131000020222     C                   MOVE      'C'           I27TLA
131100160311     C                   CALL      'TRUL27R1'
131200020222     C                   PARM                    TR27DS
131300160311     c*
131400160311     C                   CLEAR                   trulc7ds
131500160311     C                   MOVEL     'C'           Ic7TLA
131600160311     C                   CALL      'TRULC7R'
131700160311     C                   PARM                    TRULc7DS
131800160311     c
131900050630     C                   CLEAR                   fnlg09ds
132000050630     C                   MOVEL     'C'           d09TLA
132100050630     c                   movel     fnlg09ds      kpjbu
132200050630     C                   CALL      'FNLG09R'
132300050630     C                   PARM                    kpjba
132400050516     c                   movel     fnlg07        kpjbu
132500900423     C                   SETON                                          LR
132600961203     C*------------------------------------------------------*
132700961203     C*  DATMIT: Reperisco dati x stampa in inglese
132800961203     C*------------------------------------------------------*
132900961203     C     DATMIT        BEGSR
133000961203     C*
133100961203     C*  Dati anagrafici mittente
133200961203     C** SE IL CODICE è CODIFICATO PRENDO I DATI DELLA RAG.SOC.
133300961203     C** DAL PDC SE è CODICE 88 O 99 PRENDO I DATI O DALL'ANAGRAFICA
133400961203     C** D'APPOGGIO SE ESISTE O DALLO STORICO BOLLE
133500961203     C                   MOVEL     *BLANKS       STPRSM
133600961203     C                   MOVEL     *BLANKS       STPINM
133700961203     C                   MOVEL     *BLANKS       STPCAM
133800961203     C                   MOVEL     *BLANKS       STPCIM
133900961203     C                   MOVEL     *BLANKS       STPPRM
134000961203     C                   MOVEL     *BLANKS       STPNZM
134100961203     C     D43TRV        IFNE      'S'
134200961203     C*
134300961203     C     CODMIT        IFNE      8888
134400961203     C     CODMIT        ANDNE     9999
134500961203     C     KACO          CHAIN     CNACO000                           26
134600961203     C   26              EXCEPT    ERR
134700961203     C  N26              DO
134800961203     C                   MOVEL     ACORAG        STPRSM
134900961203     C                   MOVEL     ACORAG        STPPAT
135000961203     C                   MOVEL     INDVIA        STPINM
135100961203     C                   MOVEL     INDCIT        STPCIM
135200961203     C                   MOVEL     INDPRV        STPPRM
135300050223     C                   MOVEL     INDCAe        STPCAM
135400961203     C                   MOVEL     INDSTA        STPNZM
135500961203     C                   END
135600961203     C                   ELSE
135700050214     C     KGAP          CHAIN     tigap000                           70
135800961203     C     *IN70         IFEQ      '0'
135900961203     C                   MOVEL     GAPRSM        STPRSM
136000961203     C                   MOVEL     GAPRSM        STPPAT
136100961203     C                   MOVEL     GAPINM        STPINM
136200961203     C                   MOVEL     GAPLOM        STPCIM
136300961203     C                   MOVEL     GAPPRM        STPPRM
136400961203     C                   MOVEL     GAPCAM        STPCAM
136500961203     C                   MOVEL     GAPNZM        STPNZM
136600961203     C                   ELSE
136700961203     C                   MOVEL     BLPRSM        STPRSM
136800961203     C                   MOVEL     BLPRSM        STPPAT
136900961203     C                   MOVEL     BLPINM        STPINM
137000961203     C                   MOVEL     BLPLOM        STPCIM
137100961203     C                   MOVEL     BLPPRM        STPPRM
137200961203     C                   MOVEL     BLPCAM        STPCAM
137300961203     C                   MOVEL     BLPNZM        STPNZM
137400961203     C                   END
137500961203     C                   END
137600961203     C*  Controllo se devo reimpostare i dati dal luogo
137700961203     C                   ELSE
137800961203     C                   MOVEL     D43RAG        STPPAT
137900961203     C                   MOVEL     D43RAG        STPRSM
138000961203     C                   MOVEL     D43IND        STPINM
138100961203     C                   MOVEL     D43LOC        STPCIM
138200961203     C                   MOVEL     D43PRO        STPPRM
138300961203     C                   MOVEL     D43CAP        STPCAM
138400961203     C                   MOVEL     D43NAZ        STPNZM
138500961203     C                   END
138600961203     C*
138700961203     C                   ENDSR
138800961203     C*------------------------------------------------------*
138900961203     C*  DATASP: Reperisco dati x stampa in inglese
139000961203     C*------------------------------------------------------*
139100961203     C     DATASP        BEGSR
139200961203     C*
139300961203     C** RIBALTAMENTO DATA SPEDIZIONE
139400961203     C                   MOVEL     BLPAAS        G02INV
139500961203     C                   MOVE      BLPMGS        G02INV
139600961203     C                   Z-ADD     0             G02DAT
139700961203     C                   MOVEL     '3'           G02ERR
139800961203     C                   CALL      'XSRDA8'
139900961203     C                   PARM                    WLBDA8
140000961203     C                   Z-ADD     G02DAT        STPDAT            8 0
140100961203     C*  Imposto altri dati della spedizione
140200080220     C****               Z-ADD     BLPAAS        STPAA1
140300080220     C                   move      BLPAAS        STPAA1
140400961203     C                   Z-ADD     BLPLNP        STPLP1
140500961203     C                   Z-ADD     BLPNRS        STPNR1
140600961203     C                   Z-ADD     BLPNSP        STPNS1
140700961203     C                   Z-ADD     GCPFGC        STPFGC
140800961203     C                   Z-ADD     GCPNGC        STPNGC
140900961203     C                   MOVEL     GCPAGC        G02INV
141000961203     C                   MOVE      GCPMGC        G02INV
141100961203     C                   Z-ADD     0             G02DAT
141200961203     C                   MOVEL     '3'           G02ERR
141300961203     C                   CALL      'XSRDA8'
141400961203     C                   PARM                    WLBDA8
141500961203     C                   Z-ADD     G02DAT        STPDGC            8 0
141600961203     C*  Reperisco loro riferimento
141700060213     C                   MOVEL     'E'           ar4TRC
141800060213     C                   MOVEL     *BLANKS       ar4NOT
141900000121     C                   MOVE      BLPRMN        STPRMN
142000160524     C                   MOVE      BLPRMN        STPDPDRMN
142100160523     C                   SETOFF                                       88
142200090121      * aggiunto per esporlo a fianco a quello numerico vedi caso TOF
142300090121      * che aveva basato il riferimento partner sul numerico poi comunque
142400090121      * voleva il riferimento ALFA come tutti gli altri partner EDI.
142500090121      * .....aggiunto in stampa :
142600090121     C*******            MOVE      BLPRMA        STPRMA
142700090121     C*** Vogliamo il dato allineato a destra
142800160523      *
142900090121     C                   evalR     STPRMA = %TrimR(BLPRMA)
143000160525      **
143100160525      ** se DPD il Parcel è carico
143200160525     c                   if        parcel14 <> *blank
143300160525      * se DPD deve occultare il primo byte del rif.Numerico
143400160525     C                   SETON                                        88
143500160525     C                   MOVEL(p)  Parcel14      STPNT4
143600160525     c                   else
143700090121      *
143800160525      ** x DPD non aggancia ar4 (lo ha già fatto comunque FNLG24R)
143900160525      *  questo rimane solo per EEX
144000060213     C     Kar4          CHAIN     Fiar401L                           22
144100961211     C     *IN22         IFEQ      '1'
144200961211     C     BLPRMA        IFNE      *BLANKS
144300961211     C                   MOVEL     BLPRMA        STPNT4
144400961211     C                   ELSE
144500961211     C                   MOVEL     BLPRMN        STPNT4
144600961211     C                   END
144700961211     C*
144800961211     C                   ELSE
144900060213     C                   MOVEL     ar4NOT        STPNT4
145000961211     C                   END
145100160525      **
145200160525     c                   endIF
145300160525      **
145400961203     C*  Dati destinatario
145500110930     c                   z-add     blppkf        stppkg
145600110930     c                   z-add     blpncl        stpncl
145700961203     C     DN0TRV        IFNE      'S'
145800961203     C                   MOVEL     BLPLOD        STPCID
145900961203     C                   MOVEL     BLPPRD        STPPRD
146000961203     C                   MOVEL     BLPNZD        STPNZD
146100961203     C                   MOVEL     BLPRSD        STPRSD
146200961203     C                   MOVEL     BLPIND        STPIND
146300961203     C                   MOVEL     BLPCAD        STPCAD
146400961203     C                   ELSE
146500961203     C                   MOVEL     DN0LOD        STPCID
146600961203     C                   MOVEL     DN0PRD        STPPRD
146700961203     C                   MOVEL     DN0NZD        STPNZD
146800961203     C                   MOVEL     DN0RSD        STPRSD
146900961203     C                   MOVEL     DN0IND        STPIND
147000961203     C                   MOVEL     DN0CAD        STPCAD
147100961203     C                   END
147200961203     C*
147300961203     C                   MOVEL     *ALL'_'       CMP60
147400961203     C                   WRITE     PATSPE
147500961203     C                   WRITE     SOGSPE
147600961203     C                   WRITE     TESGIA
147700961203     C                   WRITE     INIAPE
147800961203     C                   MOVEL     WMOTEV        NOTE
147900971021     C                   MOVE      WMOTEV        WCOM25           25
148000961203     C                   WRITE     TESLIB
148100971016     C* SE MOTIVAZIONE + LUNGA DI 25 CHR SCRIVO ALTRA RIGA
148200971016     C     WCOM25        IFNE      *BLANKS
148300971104     C                   MOVEL     *BLANKS       NOTE
148400971104     C                   MOVEL     WCOM25        NOTE
148500971016     C                   WRITE     TESLIB
148600971016     C                   END
148700961203     C     ULTDMC        IFNE      *BLANKS
148800961203     C                   MOVEL     ULTDMC        NOTE
148900961203     C                   WRITE     TESLIB
149000961203     C                   END
149100961203     C*
149200961203     C                   ENDSR
149300961105     C*------------------------------------------------------*
149400961105     C*  DECOD. P.TO OPERAT. DA STAMPARE
149500961105     C*------------------------------------------------------*
149600961105     C     CHKPOP        BEGSR
149700961105     C                   CLEAR                   FAXPOP
149800961105     C                   CLEAR                   DESPOP
149900970115     C     WSVG2F        IFEQ      'S'
150000970115     C     GCPLNP        CHAIN     AZORG01L      UNIORG               23
150100970115     C                   ELSE
150200970115     C     PRELNP        CHAIN     AZORG01L      UNIORG               23
150300970115     C                   END
150400961105     C  N23              MOVEL     ORGFAX        FAXPOP           13
150500961105     C  N23              MOVEL     ORGDES        DESPOP           20
150600041111      *
150700041111      *----
150800041111     C  N23              do
150900041111     C                   MOVEL     ORGDE3        OG143
151000041111     c                   clear                   est_DesLnp
151100041111     c                   clear                   est_DesTel
151200041111     c                   clear                   est_DesFax
151300041111     C***
151400041111     C     §OgNTW        ifeq      'FED'
151500041111     C     §OgNTW        oreq      'EEX'
151600041111     C     §OgNTW        oreq      'EUP'
151700041111      *
151800041111     C     WSVG2F        ifEQ      'S'
151900041111     c                   move      gcpLNP        est_Lnp
152000041111     c                   else
152100041111     c                   move      preLNP        est_Lnp
152200041111     c                   end
152300041111      *
152400041111     C                   exsr      Hub_Estero
152500041111     c                   if        est_DesLnp <> *blank
152600041111     c                   movel     est_DesLnp    desPOP
152700041111     c                   movel     est_DesFax    faxPOP
152800041111     C                   end
152900041111     C                   end
153000041111     C                   endDo
153100041111      *----
153200041111      *
153300961105     C                   ENDSR
153400960201     C*------------------------------------------------------*
153500960201     C*  Posizionam. su anagrafica indirizzi x fax
153600960201     C*------------------------------------------------------*
153700960201     C     CHNIND        BEGSR
153800960306     C*
153900170925     c                   eval      tigcpb = tigcp
154000960201     C                   SETOFF                                       14
154100960205     C                   MOVEL     T2F           WT2F
154200170221 1   C     D07RIS        IFNE      'S'
154300960205     C                   MOVE      GCPSCM        CODMIT            4 0
154400170221 2   C     CODMIT        IFNE      8888
154500960201     C     CODMIT        ANDNE     9999
154600960201     C     KACO          CHAIN     CNIND000                           26
154700960201      *  Se invio tramite fax richiamo pgm x controllo validità
154800170221 3   C     T2F           IFEQ      'F'
154900960201     C                   CLEAR                   TRUL42
155000960306     C                   MOVEL     INDTLF        D42FAX
155100170221 4   C     D43TRV        IFEQ      'S'
155200960306     C                   MOVEL     D43FAX        D42FAX
155300170221 e4  C                   END
155400960201     C                   CALL      'TRUL42R'
155500960201     C                   PARM                    TRUL42
155600170221 4   C     D42ERR        IFEQ      '1'
155700960205     C                   MOVEL     DFT           WT2F              1
155800960205     C                   Z-ADD     1             XX
155900960205     C                   MOVEL     WT2F          T2F
156000960205     C                   MOVEL     *BLANKS       DFT
156100960205     C     UNI           LOOKUP    D2F(XX)                                32
156200170221 5   C     *IN32         IFEQ      '1'
156300960205     C                   MOVEL     C2F(XX)       WSVC2F            1
156400960205     C                   MOVEL     C2F(XX)       GCPTCM
156500960205     C                   EXCEPT    AGG1
156600170925     c                   exsr      RegVariazioni
156700050321      * verifica se dare commit
156800170221 6   c                   if        d07comm = 'S'
156900050321     c                   commit
157000170221e6   c                   endif
157100050321
157200170221 e5  C                   END
157300170221 x4  C                   ELSE
157400961128     C  N80              SETON                                        14
157500170221     C                   MOVEL     D42FAX        STPFAX           16
157600170221 e4  C                   END
157700170221 x3  C                   ELSE
157800960205     C     T2F           IFEQ      'F'
157900960205     C                   MOVEL     DFT           WT2F
158000960205     C                   END
158100170221 e3  C                   END
158200170221 x2  C                   ELSE
158300170221 3   C     T2F           IFEQ      'F'
158400960205     C                   MOVEL     DFT           WT2F
158500960205     C                   Z-ADD     1             XX
158600960205     C                   MOVEL     WT2F          T2F
158700960205     C                   MOVEL     *BLANKS       DFT
158800960205     C     UNI           LOOKUP    D2F(XX)                                32
1589001702214    C     *IN32         IFEQ      '1'
159000960205     C                   MOVEL     C2F(XX)       GCPTCM
159100960205     C                   MOVEL     C2F(XX)       WSVC2F            1
159200960205     C                   EXCEPT    AGG1
159300170925     c                   exsr      RegVariazioni
159400050321      * verifica se dare commit
159500050321     c                   if        d07comm = 'S'
159600050321     c                   commit
159700050321     c                   endif
159800050321
159900170221e4   C                   END
160000170221e3   C                   END
160100170221e2   C                   END
160200960202     C*
160300170221x1   C                   ELSE
160400960304     C                   MOVE      GCPSCM        CODMIT            4 0
160500960213     C     KACO          CHAIN     CNIND000                           26
160600960213     C     T2F           IFEQ      'F'
160700960213     C                   MOVEL     INDTLF        STPFAX
160800960306     C     D43TRV        IFEQ      'S'
160900960306     C                   MOVEL     D43FAX        STPFAX
161000960306     C                   END
161100961128     C  N80              SETON                                        14
161200960202     C                   END
161300170221e1   C                   END
161400960201     C*
161500960201     C                   ENDSR
161600900423     C******************************************************
161700930728     C     LUOGO         BEGSR
161800930729     C                   CLEAR                   SCH
161900930728     C     '  '          SCAN      DESLNP:1      H                 2 0
162000980127     C     H             IFEQ      0
162100980127     C                   Z-ADD     21            H
162200980127     C                   END
162300930728     C                   MOVEA     DESLNP        SCH
162400930728     C                   MOVEL     ','           SCH(H)
162500930728     C                   ADD       2             H
162600940923     C                   MOVEA     GX            SCH(H)
162700930728     C                   ADD       2             H
162800940923     C                   MOVEA     '/'           SCH(H)
162900930728     C                   ADD       1             H
163000940923     C                   MOVEA     MX            SCH(H)
163100930728     C                   ADD       2             H
163200930728     C                   MOVEL     '/'           SCH(H)
163300930728     C                   ADD       1             H
163400940923     C                   MOVEA     AX            SCH(H)
163500930728     C                   ENDSR
163600930728     C*
163700900423     C**----------------------
163800900423     C** STAMPA RACCOMANDATE
163900900423     C**----------------------
164000120113     C     STARAC        BEGSR
164100900423     C     CODMIT        CABEQ     8888          STAR
164200900423     C     CODMIT        CABEQ     9999          STAR
164300900423     C     STAR          TAG
164400120111     c                   exsr      ovrraccel
164500900423     C     CONRIG        IFGT      64
164600900423     C                   EXCEPT    TESRAC
164700900502     C                   Z-ADD     14            CONRIG
164800900423     C                   END
164900900423     C                   ADD       1             CONTA
165000040707     C
165100961211     C     EST           IFNE      'E'
165200900423     C                   EXCEPT    DETRAC
165300961211     C                   ELSE
165400961211     C                   EXCEPT    DETRAE
165500961211     C                   END
165600900502     C                   ADD       4             CONRIG
165700940202     C                   Z-ADD     GCPSCM        SAVKSC            7 0
165800911204     C*                    END
165900900423     C                   ENDSR
166000911021     C*
166100911021     C     STBOL         BEGSR
166200911021     C**--------------
166300911021     C** STAMPA BOLLA
166400911021     C**--------------
166500911021     C* PRELEVO I DATI DEL DESTINATARIO
166600921028     C* PREPARO DS X CREAZIONE BOLLA
166700990929     C                   CLEAR                   DBLP
166800990929     C                   CLEAR                   DTASV
166900941007     C                   MOVEL     'I'           §LPFPR
167000941011     C                   MOVEL     '£'           §LPSOP
167100960412     C                   MOVEL     PRELNP        §LPLNP
167200921028     C                   Z-ADD     AA            §LPAAS
167300921029     C                   Z-ADD     0             §LPMGS
167400921028     C                   MOVEL     MM            §LPMGS
167500921028     C                   MOVE      GG            §LPMGS
167600921028     C                   Z-ADD     0             §LPNRS
167700921029     C                   Z-ADD     0             §LPNSP
167800941007     C                   MOVEL     CODBOL        §LPCBO
167900921028     C                   MOVEL     *BLANKS       §LPFBR
168000960412     C                   MOVEL     PRELNP        §LPLNA
168100921028     C                   Z-ADD     0             §LPZNC
168200941011     C                   Z-ADD     8888          §LPCCN
168300941011     C                   MOVEL     §LPLNP        §LPCCN
168400941011     C                   Z-ADD     8888          §LPCCM
168500941011     C                   MOVEL     §LPLNP        §LPCCM
168600921029     C*
168700941007     C                   MOVEL     DESLNP        RAGIO            27
168800071018     C                   MOVEL     'Filiale '    §LPRSM
168900071018     C*****              MOVEL     'P.Oper. '    §LPRSM
169000921029     C                   MOVE      RAGIO         §LPRSM
169100921029     C*
169200931222     C                   MOVEL     INDLNP        §LPINM
169300941007     C                   MOVEL     CAPLNP        §LPCAM
169400931222     C                   MOVEL     LOCLNP        §LPLOM
169500931222     C                   MOVEL     PROLNP        §LPPRM
169600930208     C                   MOVEL     *BLANKS       §LPCPI
169700931217     C                   MOVEL     RAG           §LPRSD
169800941007     C                   MOVEL     *BLANKS       §LPRD2
169900931217     C                   MOVEL     VIA           §LPIND
170000940923     C                   MOVEL     CAP           §LPCAD
170100931217     C                   MOVEL     CIT           §LPLOD
170200931217     C                   MOVEL     PRV           §LPPRD
170300941221     C                   MOVEL     NZM           §LPNZD
170400921028     C                   MOVEL     *BLANKS       §LPGC1
170500921028     C                   MOVEL     *BLANKS       §LPGC2
170600921028     C                   Z-ADD     0             §LPCTR
170700921028     C                   MOVEL     *BLANKS       §LPCTS
170800921028     C                   MOVEL     *BLANKS       §LPFTM
170900921028     C                   MOVEL     *BLANKS       §LPTSP
171000941007     C                   MOVEL     *BLANKS       §LPVAS
171100941007     C                   MOVEL     *BLANKS       §LPVAD
171200941007     C                   Z-ADD     0             §LPVMD
171300941007     C                   Z-ADD     0             §LPIAS
171400921028     C                   MOVEL     'BUSTA'       §LPNAS
171500921028     C                   Z-ADD     1             §LPNCL
171600941007     C                   Z-ADD     0,1           §LPPKB
171700941007     C                   Z-ADD     0             §LPVLB
171800921028     C                   Z-ADD     0             §LPPOR
171900941007     C                   Z-ADD     0             §LPCAS
172000941007     C                   Z-ADD     0             §LPCMB
172100941007     C                   MOVEL     *BLANKS       §LPVCA
172200941007     C                   MOVEL     *BLANKS       §LPTIC
172300941007     C                   MOVEL     'P'           §LPFPP
172400921028     C                   Z-ADD     0             §LPIMV
172500921028     C                   MOVEL     *BLANKS       §LPCEI
172600921028     C                   Z-ADD     0             §LPDFT
172700921028     C                   Z-ADD     0             §LPNFT
172800921028     C                   Z-ADD     0             §LPFIV
172900921028     C                   Z-ADD     1             §LPRMN
173000941007     C                   MOVEL     *BLANKS       §LPRMA
173100921028     C                   MOVEL     *BLANKS       §LPRMO
173200921028     C                   Z-ADD     0             §LPNCD
173300921028     C                   Z-ADD     0             §LPNCA
173400921028     C                   MOVEL     *BLANKS       §LPXCO
173500921029     C*
173600921029     C     SAVFRP        IFEQ      'N'
173700921028     C                   MOVEL     'NR.'         RIGA              7
173800921028     C                   MOVE      GIANGC        RIGA
173900921028     C                   MOVE      RIG(1)        §LPNOT
174000921029     C                   MOVEL     RIGA          §LPNOT
174100921029     C                   ELSE
174200921029     C                   MOVEL     *BLANKS       §LPNOT
174300921029     C                   END
174400921029     C     SAVFRP        IFEQ      'S'
174500921029     C                   MOVEL     RIG(2)        §LPNOT
174600921029     C                   END
174700941007     C                   MOVEL     *BLANKS       §LPNT2
174800921029     C*
174900921029     C                   MOVEL     '4 '          §LPCTM
175000921028     C                   MOVEL     *BLANKS       §LPFFD
175100941007     C                   Z-ADD     999           §LPPDR
175200941011     C                   MOVEL     §LPLNP        §LPPDR
175300940922     C                   Z-ADD     D07DAT        §LPDRT
175400941007     C                   Z-ADD     0             §LPDCR
175500941007     C                   Z-ADD     0             §LPHCR
175600921028     C                   MOVEL     *BLANKS       §LPTCR
175700970327     C                   MOVEL     'S'           §LPBAM
175800970407     C                   MOVEL     'G'           §LPRIC
175900990929     C* IMPOSTO NUOVI CAMPI DBLP
176000990929     C                   MOVEL     *BLANKS       §LPFLG
176100990929     C                   MOVEL     *ZEROS        §LPLPA
176200990929     C                   MOVEL     *BLANKS       §LPFLO
176300990929     C                   MOVEL     *BLANKS       §LPDIV
176400921029     C*
176500941221     C* FORZO IL PGM STAMPA SEGNACOLLI IN KPJBA
176600941010     C                   CALL      'FNLS01R'
176700921028     C                   PARM                    KPJBA
176800990929     C                   PARM                    DBLP
176900990929     C                   PARM                    DTASV
177000030717     C                   PARM                    TRUL90DS
177100941010      *
177200960322      * RICH. PGM X SCRITTURA BOLLA IN ARRIVO solo se sped.locale
177300960322     C     §LPLNP        IFEQ      §LPLNA
177400941010     C                   CLEAR                   DSUL41
177500941010     C                   MOVEL     §LPAAS        D41AAS
177600941010     C                   MOVEL     §LPLNP        D41LNP
177700941010     C                   MOVEL     §LPNRS        D41NRS
177800941010     C                   MOVEL     §LPNSP        D41NSP
177900941010     C                   CALL      'TRUL41R'
178000941010     C                   PARM                    DSUL41
178100960322     C                   END
178200921029     C*
178300911021     C                   ENDSR
178400900423     C******************************************************
178500900423     C     AGGGCP        BEGSR
178600900423     C**----------------------
178700900423     C** AGGIORNAMENTO GIACENZE
178800900423     C**----------------------
178900170925     c                   eval      tigcpb = tigcp
179000960306     C     D43TRV        IFEQ      'S'
179100960306     C                   MOVEL     'S'           GCPFST
179200960306     C                   END
179300040119     c                   clear                   ds2z
179400040119      * verifico se cliente giacenza è un 8888 controllo se il motivo è
179500040119      * uno di quelli con data richiesta consegna se è così forzo la fase
179600040119      * e abblenco la data disposizioni
179700040830     c                   if        quatr =  8888 or
179800040830     c                             quatr =  9999
179900040119     C                   MOVEL     '2Z'          COD
180000040119     C                   MOVEL     *BLANKS       KEY
180100040119     C                   MOVEL     GCPCMC        KEY
180200040119     c     ktab          chain     tabel00f
180300041019     c                   if        %found(tabel00f)
180400040119     c                   movel     tbluni        ds2z
180500040119     c                   endif
180600040119     c                   endif
180700050412
180800040119     c                   if        §2zdcr = 'S'
180900040119     C                   Z-ADD     0             GCPDdM
181000040119     C                   Z-ADD     25            GCPFAS
181100040119     c                   else
181200050412     c                   if        gcpdis <> *blank and gcpdis <> 'R'
181300050412     c                   z-add     30            gcpfas
181400050412     c                   else
181500900423     C                   Z-ADD     20            GCPFAS
181600170117     c*
181700050412     c                   endif
181800040119     c                   endif
181900050412
182000040119     C                   Z-ADD     D07DAT        GCPDMM
182100040119
182200960202     C                   EXCEPT    AGG0
182300170925     c                   exsr      RegVariazioni
182400170221      * verifica se inviare avviso al destinatario
182500170221      * solo se prima apertura e filiale italia
182600050321     c                   if        d07comm = 'S'
182700050321     c                   commit
182800050321     c                   endif
182900170221     c                   if        gcpriap = *blank
183000170221     c                   exsr      AvvisaDest
183100170221     c                   endif
183200050321
183300900423     C                   ENDSR
183400170925     C*__________________________________________________________
183500170925     c     RegVariazioni begsr
183600170925     C*__________________________________________________________
183700170925     c                   eval      ILG33CTA = 'M'
183800170925     c                   eval      ILG33PRU = knmus
183900170925     c                   eval      ILG33NOJ = knmeb
184000170925     c                   eval      ILG33PGM = 'FNLG07R'
184100170925     c                   eval      tigcpa = tigcp
184200170925     c                   call      'FNLG33R'
184300170925     c                   parm                    fnlg33ds
184400170925     c                   parm                    tigcpb
184500170925     c                   parm                    tigcpa
184600170925     c                   endsr
184700040119     C******************************************************
184800921013     C     SCAR          BEGSR
184900040119     C**----------------------
185000050607     c                   clear                   fnlg08ds
185100050607     c                   eval      D08AGC= gcpagc
185200050607     c                   eval      D08FGC= gcpfgc
185300050607     c                   eval      D08NGC= gcpngc
185400050607     c                   eval      D08uni= §3kgks
185500050607     c                   eval      D08GSD= §3kGSD
185600050607     c                   eval      D08est= EST
185700050608     c                   eval      D08pgm = 'FNLG07R'
185800050607     c                   eval      D08commit= 'S'
185900050607     c                   movel(p)  fnlg08ds      kpjbu
186000050607     c                   call      'FNLG08R'                            28
186100050607     c                   parm                    kpjba
186200050607     c                   movel     kpjbu         fnlg08ds
186300921013     C*
186400921013     C                   ENDSR
186500940627      *--------------------------------------
186600940627     C*- CERCO I GIORNI DI RIENTRO SU TARIFFA
186700940627      *--------------------------------------
186800940627     C     CHKTFG        BEGSR
186900940627     C                   Z-ADD     0             §2GGGL
187000940923     C                   MOVE      KSC           KKSC
187100961218     C                   MOVE      WCTR          KCTR
187200961218     C                   MOVEL     WAAS          DATDSP            8 0
187300961218     C                   MOVE      WMGS          DATDSP
187400950113     C                   MOVEL     ' '           WTROVA            1
187500950116     C                   MOVE      KKSC          §QUATR            4 0
187600020222      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
187700020222     C                   CLEAR                   TR27DS
187800160311     C                   CLEAR                   fnlv59ds
187900021118     C                   MOVE      Wctr          I27ctb
188000020222     C                   MOVE      WTSP          I27TSP
188100020222     C                   MOVE      WLNA          I27LNA
188200020222     C                   MOVE      WLNP          I27LNP
188300020222     C                   MOVE      KKSC          I27CLI
188400160311     C                   CALL      'TRUL27R1'
188500020222     C                   PARM                    TR27DS
188600950113     C     §QUATR        IFNE      *ALL'8'
188700950113     C     §QUATR        ANDNE     *ALL'9'
188800950113     C     §QUATR        ANDNE     *ZEROS
188900150514     C                   MOVEL     ' '           ilv59tbo
189000150514     C                   Z-ADD     DATDSP        ilv59dsp
189100150514     C                   MOVE      WCTR          ilv59CTR
189200150514     C                   MOVE      KKSC          ilv59KSC
189300150514     C                   MOVE      KCI           ilv59kci
189400000202     C                   MOVEL     KKSC          W0030             3 0
189500150514     C                   MOVEL     O27FIE        ilv59FIE
189600150514     C                   MOVEL     WTSP          ilv59tsp
189700150514     C                   MOVEL     'S'           ilv59CON
189800150514     c                   eval      ilv59ta2='B'
189900150514     c                   eval      ilv59blo='S'
190000150514     C                   CALL      'FNLV59R'
190100150514     C                   PARM                    fnlv59ds
190200950113     C*
190300150514     C     olv59ERR      IFEQ      *BLANKS
190400150514     C     olv59tks      ANDEQ     'S'
190500950113     C*  Se ho trovato tariffa in vigore mi posiziono sul file
190600950113     C*  delle tariffe giacenze con il proggressivo giusto
190700150514     C                   MOVE      olv59ctr      KCTR
190800150514     C                   MOVE      olv59prg      KPRG
190900150514     C                   MOVE      ilv59ksc      KKSC
191000990730     C     KTGC          CHAIN     TITGC01L                           31
191100950113     C     *IN31         IFEQ      '0'
191200950116     C     TGCGGR        ANDNE     *ZEROS
191300950113     C                   MOVE      TGCGGR        §2GGGL
191400950113     C                   MOVEL     'S'           WTROVA
191500950113     C                   END
191600950113     C                   END
191700950113     C                   END
191800950113     C*  Se non trovato gg. rientro su tariffa cliente li cerco
191900950113     C*  sulla tariffa di cartello
192000950113     C     WTROVA        IFNE      'S'
192100160311     c* Carico la cartello :
192200160311     c*  - per 8888 col fie della bolla
192300160311     c*  - per tariffe cliente esistente col fie della tariffa
192400160311     c                   clear                   trulc7ds
192500160311    2C     olv59ERR      ifeq      *blanks
192600160311     C     olv59tks      andeq     'S'
192700160311     c                   z-add     1             indx              3 0
192800160311     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
192900160311    3c                   if        indx>0 and %editc(olv59prg:'X')=
193000160311     c                             ks_pr(indx)
193100160311     c                   eval      ic7tfie=ks_ie(indx)
193200160311   x3c                   else
193300160311     c                   eval      ic7tntw=o27ntw
193400160311    3c                   endif
193500160311     c
193600160311   x2c                   else
193700160311     c                   eval      ic7tntw=o27ntw
193800160311    2c                   endif
193900160311     c
194000160311     C                   moveL     datdsp        Ic7dta
194100160311     C                   move      wctr          Ic7ctb
194200160311     c                   call      'TRULC7R'
194300160311     c                   parm                    trulc7ds
194400160311     c
194500160311     C                   MOVE      Oc7CTRc       KCTR
194600160311     C                   MOVE      Oc7PRGc       KPRG
194700160311     C                   MOVE      Oc7KSCc       KKSC
194800990730     C     KTGC          CHAIN     TITGC01L                           31
194900950113     C     *IN31         IFEQ      '0'
195000950116     C     TGCGGR        ANDNE     *ZEROS
195100950113     C                   MOVE      TGCGGR        §2GGGL
195200950113     C                   MOVEL     'S'           WTROVA
195300950113     C                   END
195400950113     C                   END
195500021014     c                   if        wtrova <> 'S' or
195600021014     c                             o27ntw = 'DPD' or
195700021014     c                             o27ntw = 'FED' or
195800021014     c                             o27ntw = 'EEX' or
195900021014     c                             o27ntw = 'EUP'
196000021014
196100020222     C                   MOVEL     '2G'          COD
196200020222     C                   MOVEL     *BLANKS       KEY
196300020222     C                   MOVEL     1             KEY
196400961204     C                   Z-ADD     §2GGGL        WGGR              3 0
196500961204     C     KTAB          CHAIN     TABEL                              70
196600961204     C  N70              MOVEL     TBLUNI        DS2G
196700010921      * Se è da DPD
196800020222     C                   SELECT
196900020222      *
197000020222     C     O27NTW        WHENEQ    'DPD'
197100010921     C     §2GGDP        IFGT      WGGR
197200010925     C                   Z-ADD     WGGR          §2GGGL
197300010921     C                   ELSE
197400010921     C                   Z-ADD     §2GGDP        §2GGGL
197500010921     C                   END
197600020222      *
197700020222     C     O27NTW        WHENEQ    'FED'
197800160523     C     §2GGFE        IFGT      WGGR
197900020222     C                   Z-ADD     WGGR          §2GGGL
198000020222     C                   ELSE
198100160523     C                   Z-ADD     §2GGFE        §2GGGL
198200020222     C                   END
198300010921      * Se è un estero
198400020222     C     O27NTW        WHENEQ    'EEX'
198500020222     C     O27NTW        OREQ      'EUP'
198600961204     C     §2GGRE        IFGT      WGGR
198700961204     C                   Z-ADD     WGGR          §2GGGL
198800961204     C                   ELSE
198900961204     C                   Z-ADD     §2GGRE        §2GGGL
199000961204     C                   END
199100020222     C                   ENDSL
199200021014     c                   endif
199300961204     C*
199400160523     C* Con la tabella "20"
199500050210     C*  Se ci sono eccezioni ai resi x decorsi termini sul cliente
199600050210     C*   sui giorni da prendere
199700050210      * Se è un estero
199800050210     C     O27fie        ifne      'I'
199900050210     C     O27NTW        andeq     'EEX'
200000050210     C                   MOVEL     '20'          COD
200100050210     C                   MOVEL     *blank        KEY
200200050210     C                   MOVEL     Kksc          KEY
200300050210     c     ktab          chain     tabel00f
200400050210     c                   if        %found(tabel00f)
200500050210     c                   movel     tbluni        ds20
200600050210     C                   Z-ADD     §20GRE        §2GGGL
200700050210     c                   endif
200800050210     c                   end
200900040112     C*
201000940627     C                   ENDSR
201100040701     C*------------------------------------------------------*
201200040701     C*  reperisce dati necessari all'invio tramite mail
201300040701     C*------------------------------------------------------*
201400040701     C     invxmail      begsr
201500040701      * verifico se l'invio è tramite e-mail reperisco i dati necessari
201600040701      * se esiste il luogo l'indirizzo e-mail deve essere obbligatoriamente
201700040701      * associato al luogo altrimenti è un errore
201800040701      * *in16=*on mail
201900040701     c                   seton                                        16
202000040707     c                   clear                   trtcm1ds
202100040707     c                   if        d43trv = 'S'
202200040707     c                   if        d43trvm = 'S'
202300040707     c                   movel     d43mail       §cm1dst
202400040707     c                   end
202500040701      * se non esiste il luogo reperisco e-mail da TFNTC nota '88'
202600040707     c                   else
202700040707     c                   move      gcpscm        kcnk1
202800040707     c     kntc          chain     tfntc01l
202900041019     c                   if        %found(tfntc01l)
203000040707     c                   movel     ntcrnt        §cm1dst
203100040707     c                   end
203200040707     c                   end
203300040712     c                   move      gcplnp        §cm1po
203400040929     c                   move      '0'           §cm1sts
203500040914     c                   movel     §mrareg       §cm1tips
203600040914     c                   movel     §mraidpro     §cm1idp
203700040712     c                   movel     gcpagc        data8
203800040712     c                   move      gcpmgc        data8
203900040712     c                   move      data8         dataiso
204000040712     c                   move      dataiso       dataeur
204100040712     c                   move      dataeur       data8
204200041014     c                   movel     blprsd        rsd20            20
204300041022     c     chkc:chkcs    xlate     rsd20         rsd20
204400040721     c                   move      gcpfgc        fgcalfa           3
204500160523      ***
204600160523     c                   if        Parcel14 <> *blank
204700160523     c                   eval      §cm1var = '*OBJM*' + parcel14 + '-'
204800160523     c                   else
204900160523     c                   eval      §cm1var = '*OBJM*'
205000160523     c                   end
205100160523      ***
205200160523     c                   eval      §cm1var = %trim(§cm1var) +
205300160523     c                             'Giacenza Nr. ' + fgcalfa + '-' +
205400041014     c                             %trim(%editc(gcpngc:'Z')) + ' ' +
205500041005     c                             %editw(data8:'  -  -    ') +
205600041014     c                             ' Cli: '+  %editc(gcpscm:'Z') + ' ' +
205700041014     c                             rsd20 + '-' + blpprd
205800061214     C* Imposto i parametri x il *pgm d "bonifica"
205900061214     C                   EVAL      ElencoChar = *blanks
206000061214     C                   EVAL      TipoElenco = *blanks
206100061214     C                   EVAL      CharSost   = *blanks
206200061214     C                   EVAL      UpperCase  = ' '
206300061214     C                   EVAL      ChkNull    = '1'
206400061214     C                   EVAL      CharNull   = *blanks
206500061214     C*
206600061214     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
206700061214     C                   EVAL      TxtInOut   = §cm1var
206800061214     C                   EVAL      EsitoBon   = *blanks
206900061214     C*
207000061214     C                   CALL      'XCHKCHAR'
207100061214     C                   PARM                    TxtInOut
207200061214     C                   PARM                    ElencoChar
207300061214     C                   PARM                    TipoElenco
207400061214     C                   PARM                    CharSost
207500061214     C                   PARM                    UpperCase
207600061214     C                   PARM                    ChkNull
207700061214     C                   PARM                    CharNull
207800061214     C                   PARM                    EsitoBon
207900061214     C*
208000061214     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
208100061214     C                   IF        EsitoBon = '1'
208200061214     C                   EVAL      §cm1var = TxtInOut
208300061214     C                   ENDIF
208400040929
208500040929      * richiamo per reperimento mittente della mail
208600040929     c                   exsr      repmitt
208700040929
208800040707      *non trovato indirizzo mail destinatario invio la mail allo stesso
208900040707      * utente che sta inviando
209000040707     c                   if        §cm1dst = *blank
209100040914     c                   eval      §cm1dst = %trim(§cm1mitt)  + mitterr
209200040707     c                   endif
209300040701     c
209400040707     C                   endsr
209500170116     C*------------------------------------------------------*
209600170116     C*  reperisce dati necessari all'invio avviso destinatario
209700170116     C*------------------------------------------------------*
209800170116     C     AvvisaDest    begsr
209900170116     c                   clear                   fnlg071ds
210000170207     c                   movel     gcpAAS        i071AAS
210100170207     c                   movel     gcpLNP        i071LNP
210200170207     c                   movel     gcpNRS        i071NRS
210300170207     c                   move      gcpNSP        i071NSP
210400170207     c                   movel     gcpLNa        i071LNa
210500170207     c                   move      'A'           i071CHI
210600170207     c                   movel     gcpcmc        i071cmc
210700170116     c                   call      'FNLG07R1'
210800170207     c                   parm                    kpjba
210900170116     c                   parm                    fnlg071ds
211000170116      *
211100170116     C                   endsr
211200040629     C*------------------------------------------------------*
211300040629     C*  Prepara ovrprtf in caso di invio via e-mail
211400040629     C*------------------------------------------------------*
211500040629     C     preparaovr    begsr
211600040629     c                   close     letgia
211700040629     C*  Ovrprtf  LETgia
211800040630     c                   eval      commanl = %trim(Cmd9(1))+ trtcm1ds + ''')'
211900040630     c                   eval      lung = %len(%trim(commanl))
212000040629     C                   CALL      'QCMDEXC'                            60
212100040630     C                   PARM                    COMMANl         500
212200040629     C                   PARM                    LUNG             15 5
212300040629     C                   OPEN      LETGIA
212400040629     C                   endsr
212500040906     C*------------------------------------------------------*
212600040906     C*  Prepara ovrprtf in caso di invio via e-mail ESTERA
212700040906     C*------------------------------------------------------*
212800040906     C     preparaovre   begsr
212900040906     c                   close     letest
213000040906     C*  Ovrprtf  LETest
213100040906     c                   eval      commanl = %trim(Cmd9(2))+ trtcm1ds + ''')'
213200040906     c                   eval      lung = %len(%trim(commanl))
213300040906     C                   CALL      'QCMDEXC'                            60
213400040906     C                   PARM                    COMMANl         500
213500040906     C                   PARM                    LUNG             15 5
213600040906     C                   OPEN      LETest
213700040906      *prepara link in una stringa per printer file
213800040906     c                   exsr      strlink
213900040906     C                   endsr
214000120111     C*------------------------------------------------------*
214100120111     C*  Prepara ovrprtf per raccomandate
214200120111     C*------------------------------------------------------*
214300120111     C     ovrracc       begsr
214400120111     c                   close     letgia
214500120111     C*  Ovrprtf  LETgia
214600120111     c                   eval      commanl = Cmd1(1)
214700120111     c                   eval      lung = %len(%trim(commanl))
214800120111     C                   CALL      'QCMDEXC'                            60
214900120111     C                   PARM                    COMMANl         500
215000120111     C                   PARM                    LUNG             15 5
215100120111     C                   OPEN      LETGIA
215200120111     C                   endsr
215300120111     C*------------------------------------------------------*
215400120111     C*  Prepara ovrprtf per raccomandate elenco
215500120111     C*------------------------------------------------------*
215600120111     C     ovrraccel     begsr
215700120111     c                   close     let132
215800120111     C*  Ovrprtf  LETgia
215900120111     c                   eval      commanl = Cmd2(1)
216000120111     c                   eval      lung = %len(%trim(commanl))
216100120111     C                   CALL      'QCMDEXC'                            60
216200120111     C                   PARM                    COMMANl         500
216300120111     C                   PARM                    LUNG             15 5
216400120111     C                   OPEN      LET132
216500120111     C                   endsr
216600040906     C*------------------------------------------------------*
216700040906     C*  Prepara stringa link per printer file       ESTERA
216800040906     C*------------------------------------------------------*
216900040906     C     strlink       begsr
217000040910     c                   z-add     §2gggl        stgggl
217100040906     c                   clear                   linkst
217200040907     c                   eval      linkst = 'B#?nspediz=' +
217300040906     c                             %trim(%editc(gcplnp:'X')) +
217400040906     c                             %trim(%editc(gcpnrs:'X')) +
217500040906     c                             %trim(%editc(gcpnsp:'X')) + '&keygiac=' +
217600040906     c                             %trim(%editc(gcpaas:'X')) +
217700040906     c                             %trim(%editc(gcplnp:'X')) +
217800040906     c                             %trim(%editc(gcpnrs:'X')) +
217900040906     c                             %trim(%editc(gcpnsp:'X')) +
218000040906     c                             %trim(%editc(blpaas:'X')) +
218100040906     c                             %trim(%editc(blpmgs:'X')) +
218200040906     c                             %trim(%editc(gcpagc:'X')) +
218300040906     c                             %trim(%editc(gcpfgc:'X')) +
218400040906     c                             %trim(%editc(gcpngc:'X')) +
218500040906     c                             %trim(%editc(gcpagc:'X')) +
218600040907     c                             %trim(%editc(gcpmgc:'X')) + '#E'
218700040906     C                   endsr
218800040929     C*------------------------------------------------------*
218900040929     C*  Prepara stringa mittente per invio mail o riferimento per FAX
219000040929     C*------------------------------------------------------*
219100040929     C     repmitt       begsr
219200040929      * P.O. primo o secondo livello
219300040929     c                   movel     orgde8        fillevel          1
219400040929     c                   move      gcplnp        lnpalfa           3
219500040929      * campi di comodo per stampa fine fax
219600040929     c                   move      gcpscm        gcpscms           7
219700041020      * non deve stampare il piede del fax per clienti non codificati
219800041020     c                   setoff                                       17
219900041020     c                   if        %subst(gcpscms:4:4) = '8888' or
220000041020     c                             %subst(gcpscms:4:4) = '9999'
220100041020     c                   seton                                        17
220200041020     c                   endif
220300040929     c                   clear                   mittfax          21
220400040929      * gestione network x sapere come impostare il mittente
220500040929     c                   select
220600040929     c                   when      §ogntw = 'DPD' or §ogntw = 'EEX' or
220700040929     c                             §ogntw = 'FED' or §ogntw = 'EUP'
220800040929     c                   clear                   fnlv55ds
220900040929     c* Imposto i dati in LV55 x reperire terminal partenza fil. estera
221000040929     c                   movel     'P'           d55tpt
221100040929     c                   movel     amgdtu        d55drf
221200040929     c                   movel     gcplnp        d55lin
221300040929     c                   move      gcplnp        d55lnp
221400040929     c                   call      'FNLV55R'
221500040929     c                   parm                    fnlv55ds
221600040929      * P.O. estero va inviato come 'INTyyy' yyy=terminal partenza
221700040929     c                   move      d55tfp        terminalp         3
221800040929     c                   eval      §cm1mitt = %trim(§mramites) + terminalp
221900040929
222000040929     c                   other
222100040929      * se network italia trasmette come INFO +xxx=linea di partenza
222200040929     c                   if        fillevel = '1'
222300040929     c                   eval      §cm1mitt = %trim(§mramit1l) + lnpalfa
222400040929     c                   else
222500040929     c                   eval      §cm1mitt = %trim(§mramit2l) + lnpalfa
222600040929     c                   endif
222700040929     c                   endsl
222800040929     c                   eval      Mittfax = %trim(§cm1mitt)  + mitterr
222900040929     C                   endsr
223000931217     C******************************************************
223100931217     C     *INZSR        BEGSR
223200931217     C*
223300931217     C     *ENTRY        PLIST
223400931217     C                   PARM                    KPJBA
223500931217     C                   Z-ADD     1             CODUT
223600931217     C                   CALL      'X§PARUT'
223700931217     C                   PARM                    UTEDSE
223800931217     C     CODUT         CABEQ     -1            FINE
223900971112     C                   MOVEL     RAGUT         RSUT             30
224000931217     C*--- RICERCA CAPOCONTI------------------------------------------*
224100931217     C                   DO        50            X                 2 0
224200931217     C                   MOVE      TCU(X)        TCUDS
224300940922     C     F56           IFEQ      'CG'
224400940922     C     F4            ANDEQ     '1'
224500940922     C     F3            ANDEQ     '0'
224600940922     C                   Z-ADD     KCU(X)        KCI               4 0
224700940922     C                   END
224800931217     C                   END
224900931217     C*---------------------------------------------------------------*
225000931217     C                   MOVEL     REC80         CNCR80
225100931217     C                   MOVEL     REC42         CNCR42
225200120210     C                   MOVEL     simrgs        srsut            16
225300110606     C                   MOVEL     simrgs        STPRUT
225400931217     C** TABEL
225500931217     C     KTAB          KLIST
225600931217     C                   KFLD                    CODUT
225700931217     C                   KFLD                    COD               2
225800931217     C                   KFLD                    KEY               8
225900931217     C*
226000931217     C     KTAB1         KLIST
226100931217     C                   KFLD                    CODUT
226200931217     C                   KFLD                    COD
226300931217     C* CNACO
226400931217     C     KACO          KLIST
226500931217     C                   KFLD                    CODUT
226600931217     C                   KFLD                    KCI
226700931217     C                   KFLD                    GCPSCM
226800931217     C* FLBLP
226900931217     C     KGAP          KLIST
227000931217     C                   KFLD                    GCPAAS
227100931217     C                   KFLD                    GCPLNP
227200931217     C                   KFLD                    GCPNRS
227300931217     C                   KFLD                    GCPNSP
227400060213     C     Kar4          KLIST
227500940923     C                   KFLD                    GCPAAS
227600940923     C                   KFLD                    GCPLNP
227700940923     C                   KFLD                    GCPNRS
227800940923     C                   KFLD                    GCPNSP
227900060213     C                   KFLD                    ar4TRC
228000931217     C*
228100931217     C     KGIA          KLIST
228200940922     C                   KFLD                    D07AGC
228300940922     C                   KFLD                    D07FGC
228400940922     C                   KFLD                    D07NGC
228500990625     C                   KFLD                    PRG               2 0
228600931217     C                   Z-ADD     0             PRG
228700931217     C*
228800931217     C     KNOT          KLIST
228900931217     C                   KFLD                    GCPAGC
229000931217     C                   KFLD                    GCPFGC
229100931217     C                   KFLD                    GCPNGC
229200931217     C                   KFLD                    GCPFRG
229300931217     C                   KFLD                    FASE              3 0
229400931217     C                   KFLD                    NPR               3 0
229500940923     C*
229600940923     C     KTGC          KLIST
229700940923     C                   KFLD                    KKSC
229800940923     C                   KFLD                    KCTR
229900940923     C                   KFLD                    KPRG
230000050214     C     Kgia21        KLIST
230100050214     C                   KFLD                    GCPAASa
230200050214     C                   KFLD                    GCPLNPa
230300050214     C                   KFLD                    GCPNRSa
230400050214     C                   KFLD                    GCPNSPa
230500040629     C     Kntc          KLIST
230600040629     C                   KFLD                    kcapl
230700040629     C                   KFLD                    kcnk1
230800040629     C                   KFLD                    kcnk2
230900040629     C                   KFLD                    kctnt
231000040629     c                   move      'C'           kcapl
231100040629     c                   movel     '0151'        kcnk1
231200040629     c                   move      '88'          kctnt
231300040630     C     ktbe          klist
231400040630     C                   kfld                    tbecod
231500040630     C                   kfld                    tbeke1
231600040630     C                   kfld                    tbeke2
231700040708     c*inizializza costanti per invio mail
231800040708     c                   eval      cost1 = 'Vi informiamo inoltre che:'
231900040708     c                   eval      cost2 = '- la merce e''custodita presso la -
232000040708     c                             Ns. di:'
232100040708     c                   eval      cost3 = '- sino alla ricezione di Vs. istruz-
232200040708     c                             ioni saremo tenuti ad aderire alla richiesta-
232300040721     c                              di riconsegna da parte'
232400040721     c                   eval      cost4 = '  del destinatario'
232500040721     c                   eval      cost5 = '- trascorsi    giorni dalla present-
232600040721     c                             e senza che ci abbiate fornito istruzioni co-
232700040721     c                             ncrete, complete e'
232800040721     c                   eval      cost6 = '  attuabili, Vi ritorneremo la merc-
232900040721     c                             e a Vs. rischio e con addebito di ogni spesa-
233000040721     c                             . Trascorsi ulteriori 30'
233100040721     c                   eval      cost7 = '  giorni senza che la giacenza sia -
233200040721     c                             risolta provvederemo al recupero dei ns. cre-
233300040721     c                             diti mediante richiesta'
233400040721     c                   eval      cost8 = '  al giudice dell''esecuzione di ve-
233500040721     c                             ndita o assegnazione della merce ai sensi de-
233600040721     c                             gli art. 2756, 2761,'
233700040721     c                   eval      cost9 = '  2797 e 2798 del C.C. salvo e impr-
233800040721     c                             egiudicato ogni altro diritto per spese e co-
233900040721     c                             mpetenze non recuperate.'
234000040721     c                   eval      cost10= '- le spese di giacenza, dalla richi-
234100040721     c                             esta all''esecuzione delle Vs. istruzioni, c-
234200040721     c                             omprese eventuali '
234300040721     c                   eval      cost11= '  assicurazioni verranno addebitate-
234400040721     c                              al               nella misura prevista dall-
234500040721     c                             '' art. 17/p delle'
234600040721     c                   eval      cost12= '  ultime Condizioni Generali CONFET-
234700040721     c                             RA, depositate presso tutte le Camere di Com-
234800040721     c                             mercio.'
234900040722     c                   eval      cost13= '- Dopo il   ° AVVISO, in caso di ma-
235000040721     c                             ncata risoluzione positiva della giacenza, V-
235100040721     c                             i ritorneremo'
235200040721     c                   eval      cost14= '  la merce a Vostro rischio e con a-
235300040721     c                             ddebito di ogni spesa.'
235400040708     c
235500120209     c                   move      *all'-'       trat130         130
235600040708     c
235700940923     C*---------------------------------------------------------------*
235800940923     C*  LIKE DEFN
235900040629     C     *LIKE         DEFINE    NTCAPL        kcapl
236000040629     C     *LIKE         DEFINE    NTCNK1        kcnk1
236100040629     C     *LIKE         DEFINE    NTCNK2        kcnk2
236200040629     C     *LIKE         DEFINE    NTCTNT        kctnt
236300950113     C     *LIKE         DEFINE    TGCKSC        KKSC
236400950113     C     *LIKE         DEFINE    TGCCTR        KCTR
236500950113     C     *LIKE         DEFINE    TGCPRG        KPRG
236600950123     C     *LIKE         DEFINE    SIMRGS        MITRGS
236700950123     C     *LIKE         DEFINE    ORGIND        MITIND
236800950123     C     *LIKE         DEFINE    ORGCPF        MITCPF
236900950123     C     *LIKE         DEFINE    ORGLOC        MITLOC
237000950123     C     *LIKE         DEFINE    ORGPRO        MITPRO
237100961218     C     *LIKE         DEFINE    GCPLNA        WSVLNA
237200961218     C     *LIKE         DEFINE    BLPAAS        WSVAAS
237300961218     C     *LIKE         DEFINE    BLPMGS        WSVMGS
237400961218     C     *LIKE         DEFINE    BLPNZD        WSVNZD
237500961218     C     *LIKE         DEFINE    BLPTSP        WSVTSP
237600961218     C     *LIKE         DEFINE    BLPLNP        WSVLNP
237700961218     C     *LIKE         DEFINE    BLPCTR        WSVCTR
237800961218     C     *LIKE         DEFINE    GCPLNA        WLNA
237900961218     C     *LIKE         DEFINE    BLPAAS        WAAS
238000961218     C     *LIKE         DEFINE    BLPMGS        WMGS
238100961218     C     *LIKE         DEFINE    BLPNZD        WNZD
238200961218     C     *LIKE         DEFINE    BLPTSP        WTSP
238300961218     C     *LIKE         DEFINE    BLPLNP        WLNP
238400961218     C     *LIKE         DEFINE    BLPCTR        WCTR
238500931217     C*-----------
238600931217     C* CARICO TABELLA CON DECODIFICA TIPI BOLLA
238700931217     C*-----------
238800940202     C                   Z-ADD     0             Y
238900931217     C                   MOVEL     'TB'          COD
239000931217     C     KTAB1         SETLL     TABEL00F                           15
239100931217     C     *IN15         DOWEQ     *OFF
239200931217     C     KTAB1         READE     TABEL                                  15
239300931217     C  N15              DO
239400931217     C                   ADD       1             Y
239500931217     C                   MOVEL     TBLKEY        CDB(Y)
239600931217     C                   MOVEL     TBLUNI        DSTB
239700931217     C                   MOVEL     §TBDES        DES(Y)
239800931217     C                   ENDDO
239900931217     C                   ENDDO
240000931217     C*----------------------------------------------------*
240100940922     C                   TIME                    WHHDAT           14 0
240200940922     C                   MOVE      WHHDAT        WDATA8            8 0
240300940923     C                   Z-ADD     WDATA8        G02DAT
240400961203     C                   Z-ADD     WDATA8        STPOGG
240500931217     C                   Z-ADD     66            CONRIG            3 0
240600931217     C                   Z-ADD     0             CONTA             3 0
240700961211     C                   Z-ADD     0             CONTAE            3 0
240800040906     C* udate in amg
240900040906     C                   Z-ADD     0             g02inv
241000040906     C                   Z-ADD     wdata8        G02DAT
241100040906     C                   MOVEL     ' '           G02ERR
241200040906     C                   CALL      'XSRDA8'
241300040906     C                   PARM                    WLBDA8
241400040906     C                   Z-ADD     G02inv        amgdtu            8 0
241500110503      *
241600110503     c                   move      'VPO'         tbecod
241700110503     c                   movel     'BRT'         tbeke1
241800110503     c                   clear                   tbeke2
241900110503     c     ktbe          chain     tntbe01l
242000110503     c                   if        %found(tntbe01l)
242100110503     c                   movel     tbeuni        dvpobrt
242200110503     c                   if        amgdtu <= §VPOBRTDTA
242300110503     c                   seton                                        33
242400110503     c                   endif
242500110503     c                   endif
242600040906
242700940922     C                   MOVEL     KPJBU         FNLG07
242800050516     c                   eval      d07ok = 'N'
242900050321      * si autoimposta il flag per gestire il commit
243000050321     c                   movel     'S'           d07comm
243100030717     C* MI COMPONGO TRUL90DS X STAMPA BOLLE
243200030717     C                   EXSR      SRTRUL90
243300931217     C* CREO DATA DIRITTA
243400940922     C                   Z-ADD     D07DAT        G02INV
243500940922     C                   Z-ADD     0             G02DAT
243600940922     C                   MOVEL     '3'           G02ERR
243700940923     C                   CALL      'XSRDA8'
243800940922     C                   PARM                    WLBDA8
243900940922     C                   Z-ADD     G02DAT        DATAU             8 0
244000931217     C* GUARDO se stampare capitale sociale
244100931217     C                   MOVEL     *BLANKS       KEY
244200931217     C                   MOVEL     1             KEY
244300931217     C                   MOVEL     '5 '          COD
244400931217     C     KTAB          CHAIN     TABEL                              15
244500931217     C  N15              MOVEL     TBLUNI        SI                1
244600931217     C*
244700931217     C* GUARDO CHE UTENTE HA FATTO LA RICHIESTA
244800931217     C                   MOVEL     CMD4(1)       OVRAPP
244900020730     C*
245000931217     C*  Ovrprtf  LET132
245100961202     C                   MOVEL     D07FIL        LIN
245200931217     C                   MOVEL     OVRAPP        CMD4
245300931217     C                   Z-ADD     63            LUNG
245400931217     C                   SETOFF                                       60
245500931217     C                   MOVEL     *BLANKS       COMMAN
245600931217     C                   MOVEA     CMD4(1)       COMMAN           80
245700931217     C                   CALL      'QCMDEXC'                            60
245800931217     C                   PARM                    COMMAN
245900931217     C                   PARM                    LUNG             15 5
246000931217     C                   OPEN      LET132
246100931217     C*
246200931217     C*  Ovrprtf  LETGIA
246300931217     C                   MOVEL     CMD5(1)       OVRAPP
246400961202     C                   MOVEL     D07FIL        LIN
246500931217     C                   MOVEL     OVRAPP        CMD5
246600931217     C                   Z-ADD     41            LUNG
246700931217     C                   SETOFF                                       60
246800931217     C                   MOVEL     *BLANKS       COMMAN
246900931217     C                   MOVEA     CMD5(1)       COMMAN           80
247000931217     C                   CALL      'QCMDEXC'                            60
247100931217     C                   PARM                    COMMAN
247200931217     C                   PARM                    LUNG             15 5
247300961203     C*  Ovrprtf  LETEST
247400961203     C                   MOVEL     CMD7(1)       OVRAPP
247500961203     C                   MOVEL     D07FIL        LIN
247600961203     C                   MOVEL     OVRAPP        CMD5
247700961203     C                   Z-ADD     41            LUNG
247800961203     C                   SETOFF                                       60
247900961203     C                   MOVEL     *BLANKS       COMMAN
248000961203     C                   MOVEA     CMD5(1)       COMMAN           80
248100961203     C                   CALL      'QCMDEXC'                            60
248200961203     C                   PARM                    COMMAN
248300961203     C                   PARM                    LUNG             15 5
248400931217     C                   OPEN      LETGIA
248500961203     C                   OPEN      LETEST
248600931217     C*
248700940922     C     D07RIS        COMP      'S'                                    80
248800940922     C     D07NGC        COMP      0                                      81
248900931217     C*
249000940923     C* VERIFICO SE LA FILIALE COLLEGATA NE GESTISCE ALTRE
249100931217     C                   MOVEA     *ZEROS        FIL£7
249200931217     C                   MOVEL     *BLANKS       KEY
249300961202     C                   MOVEL     D07FIL        KEY
249400931217     C                   MOVEL     '£7'          COD
249500931217     C     KTAB          CHAIN     TABEL                              15
249600931217     C  N15              MOVEA     TBLUNI        FIL£7
249700961202     C   15              MOVE      D07FIL        LNP§
249800931217     C   15              MOVEA     LNP§          FIL£7
249900940202     C                   Z-ADD     *HIVAL        SAVKSC
250000940923     C*
250100940923     C* Carico dati tariffe
250200940923     C                   MOVEL     *BLANKS       KEY
250300940923     C                   CLEAR                   DSQT1
250400940923     C                   MOVEL     '1'           KEY
250500940923     C                   MOVEL     'QT'          COD
250600940923     C     KTAB          CHAIN     TABEL                              15
250700940923     C  N15              MOVEL     TBLUNI        DSQT1
250800940922     C* Caricamento 2F
250900970110     C                   MOVEA     *HIVAL        C2F
251000940922     C                   Z-ADD     0             X                 2 0
251100940922     C                   MOVEL     '2F'          COD
251200940922     C     KTAB1         CHAIN     TABEL                              31
251300940922     C     *IN31         DOWEQ     '0'
251400940922     C     X             ANDLT     50
251500940922     C     TBLFLG        IFEQ      ' '
251600031027     C                   MOVE      TBLUNI        Ds2f
251700031027      * non carica la tabella riservata
251800031027     c                   if        §2Fris =*blank
251900940922     C                   ADD       1             X
252000960205     C                   MOVE      TBLUNI        D2F(X)
252100940923     C                   MOVEL     TBLKEY        C2F(X)
252200031027     c                   endIF
252300940922     C                   END
252400940922     C     KTAB1         READE     TABEL                                  31
252500940922     C                   END
252600941007     C*
252700941007     C                   MOVEL     '2G'          COD
252800941007     C                   MOVEL     *BLANKS       KEY
252900941007     C                   MOVEL     1             KEY
253000941007     C     KTAB          CHAIN     TABEL                              70
253100941007     C  N70              MOVEL     TBLUNI        DS2G
253200941007     C                   MOVEL     §2GCCO        CODBOL            2
253300941007     C*
253400020826     C* Carico £1 x effettuare letture solo della mia area
253500020918     c                   movel     kpjbu         savkpjbu
253600020826     c                   clear                   trul06ds
253700020826     c                   eval      d06cod = '£1'
253800020826     c                   movel     simfel        d06key
253900020826     c                   movel     trul06ds      kpjbu
254000020826     c                   call      'TRUL06R'
254100020826     c                   parm                    kpjba
254200020826     c                   movel     kpjbu         trul06ds
254300020918     c                   movel     savkpjbu      kpjbu
254400040929      * reperimento tabella MRA definizioni invio posta elettronica
254500040929     c                   clear                   dmra
254600040929     c                   move      'MRA'         tbecod
254700040929     c                   movel(p)  'FNLG07R'     tbeke1
254800040929     c                   move      *blank        tbeke2
254900040929     c     ktbe          chain     tntbe01l
255000041019     c                   if        %found(tntbe01l)
255100040929     c                   movel     tbeuni        dmra
255200040929     c                   endif
255300931217     C                   ENDSR
255400030717     C*---------------------------------------------------------------*
255500030717     C*   R O U T I N E   INIZIALE  x trul90ds                        *
255600030717     C*---------------------------------------------------------------*
255700030717     C     srtrul90      BEGSR
255800030717     c                   clear                   trul90ds
255900030717     c                   eval      d90mde = d07moe
256000030717     c                   eval      d90mdb = d07mob
256100030717     c                   eval      d90prb = d07prb
256200030717     c                   eval      d90pre = d07pre
256300030717     c                   eval      d90pss = d07pss
256400030717     c                   eval      d90mdb4 = d07mdb4
256500030717     c                   eval      d90mdb5 = d07mdb5
256600030717     c                   eval      d90psl = d07psl
256700030717     c                   eval      d90bsl = d07bsl
256800030717     c                   eval      d90prb4 = d07prb4
256900030717     c                   eval      d90prb5 = d07prb5
257000030717     c                   endsr
257100041111     C***------------------------------------------------***
257200041111      *  reperisce informazioni Hub di partenza Estero
257300041111     C***------------------------------------------------***
257400041111     C     Hub_Estero    Begsr
257500041111      *
257600041111     C                   CLEAR                   FNLV55DS
257700041111     C                   MOVEL     'P'           D55TPT
257800041111     C                   MOVE      est_Lnp       D55LIN
257900041111     C                   MOVE      est_Lnp       D55LNP
258000041111     C                   MOVE      amgdtu        d55drf
258100041111     C                   CALL      'FNLV55R'
258200041111     C                   PARM                    FNLV55DS
258300041111     c                   if        d55err = *blanks
258400041111     c     d55tfp        chain     azorg01l      Est_UNIORG
258500041111     c                   if        %found(azorg01l)
258600041111     c                   movel     est_orgdes    est_DesLnp
258700041111     c                   movel     est_orgtel    est_DesTel
258800041111     c                   movel     est_orgfax    est_DesFax
258900041111     c                   endif
259000041111     c                   endif
259100041111      *
259200041111     C                   ENDSR
259300931217     C******************************************************
259400921013     C***------------------------------------------------***
259500050214     Otigcp000  E            AGG0
259600920529     O                       GCPFAS
259700960205     O                       GCPDMM
259800960306     O                       GCPFST
259900050214     Otigcp000  E            AGG1
260000960205     O                       GCPTCM
260100050321     O*igcp002  E            FINTO
260200960131     C***------------------------------------------------***
260300930726     O** STAMPA LETTERA AL MITTENTE MODULO NUOVO
260400120321     Oletgia    E    12      TESTA            04
260500120321     O                       gcplnp               7
260600120321     O*ETGIA    E    14      TESTA            05
260700120321     O          E    14      TESTA            05
260800960202     O                                           73 'FAX:'
260900960202     O                       STPFAX              90
261000961114     O          E    14      TESTA            06
261100120113     O*mm            12                          80 'RACCOMANDATA'
261200930728     O                                          110 'Pagina'
261300930726     O                       PAGE          Z   +  1
261400961114     O          E    14      TESTA            08
261500071018     O****                                       15 'P.Operativo di'
261600071018     O                                           11 'Filiale di'
261700071018     O                       DESLNP              32
261800961114     O          E    14      TESTA            09
261900940202     O               13      INDLNP              21
262000961114     O          E    14      TESTA            10
262100961114     O                                           77 'Spettabile'
262200940202     O               13      CAPLNP               6
262300940202     O               13      LOCLNP            +  1
262400940202     O               13      PROLNP            +  1
262500961114     O          E   N14      TESTA            10
262600071018     O******                                     15 'P.Operativo di'
262700071018     O                                           11 'Filiale di'
262800071018     O                       DESLNP              32
262900120113     O*mm            12                          53 'RACCOMANDATA'
263000120321     o*                      srsut               53
263100961114     O                                           77 'Spettabile'
263200961114     O                                          110 'Pagina'
263300961114     O                       PAGE          Z   +  1
263400961114     O          E   N14      TESTA            11
263500961114     O               13      INDLNP              21
263600961114     O          E   N14      TESTA            12
263700961114     O               13      CAPLNP               6
263800961114     O               13      LOCLNP            +  1
263900961114     O               13      PROLNP            +  1
264000961114     O                       RAG                102
264100961114     O          E    14      TESTA            12
264200930728     O                                            4 'FAX'
264300930728     O                       FAXLNP            +  1
264400940923     O                       RAG                102
264500961114     O          E   N14      TESTA            13
264600961114     O                                            4 'FAX'
264700961114     O                       FAXLNP            +  1
264800961114     O                       VIA                102
264900961114     O          E    14      TESTA            13
265000930728     O                                            4 'TEL'
265100930728     O                       TELLNP            +  1
265200940923     O                       VIA                102
265300930728     O          E            TESTA            14
265400961114     O              N14                           4 'TEL'
265500961114     O              N14      TELLNP            +  1
265600940923     O                       CAP                 76
265700931217     O                       CIT               +  1
265800931217     O                       PRV               +  1
265900940923     O                       NZM               +  1
266000930729     O          E            TESTA            15
266100940923     O                       SCH                 32
266200960618     O               04                          77 'Att.ne di '
266300960618     O               04      ATTPER            +  1
266400120321     O*         E            TESTA            17
266500120321     O          E            TESTA            20
266600040708     O                                           24 'Con la presente siamo a '
266700040708     O                                         +  0 'comunicarVi le spedizion'
266800040708     O                                         +  0 'i giacenti del'
266900940923     O                       DATAU             +  1 '  /  /    '
267000120321     O          E            TESTA            21
267100930726     O                                           11 'Giacenza '
267200930726     O                                         +  3 'Destinatario'
267300120321     O          E            TESTA          1 21
267400930726     O                                           11 'Giacenza '
267500930726     O                                         +  3 'Destinatario'
267600930729     O          E            RIGAS          0
267700930726     O                       GCPFGC        Z      3
267800930726     O                                         +  0 '/'
267900930726     O                       GCPNGC        Z   +  0
268000940923     O                       RSD               +  3
268100940923     O                                         +  1 'Spedizione:'
268200930726     O                       GCPAAS        Z   +  2
268300930726     O                       GCPLNP        Z   +  1
268400930726     O                       GCPNRS        Z   +  1
268500930726     O                       GCPNSP        Z   +  1
268600930729     O                                         +  1 'del:'
268700940923     O                       DATSPE            +  1 '  /  /    '
268800950214     O                                         +  1 'M.or.'
268900950214     O                       BLPRMO            +  0
269000931203     O          E            RIGAS       1
269100030929     O              n52                          12 '1° Avviso '
269200040426     O               52      xprog         z      3
269300040426     O               52                          12 '° Avviso '
269400940923     O                       IND                 39
269500940923     O                                         +  1 'Vs. Rif.ti:'
269600940923     O                       BLPRMN        Z   +  1
269700940923     O                       BLPRMA            +  1
269800940923     O                       DESTSP            +  1
269900940923     O                                         +  2 'Nat.merce:'
270000940923     O                       BLPNAS            +  0
270100931203     O          E            RIGAS       0  0
270200030929     O              n52                          12 '1° Avviso '
270300040426     O               52      xprog         z      3
270400040426     O               52                          12 '° Avviso '
270500930727     O          E            RIGAS       1
270600960201     O                       CAD                 23
270700940923     O                       LOD               +  1
270800940923     O                       PRD               +  1
270900941007     O                       NZD               +  1
271000940317     O                                         +  2 'Note:'
271100060213     O                       ar4NOT            +  1
271200950214     O                                         +  1 'cod.cli.:'
271300950214     O                       GCPSCM        Z   +  1
271400930727     O          E            RIGAS       1
271500930726     O                                           20 'Colli:'
271600950116     O                       BLPNCL        Z   +  0
271700950116     O                                         +  1 'Peso:'
271800950116     O                       BLPPKF        2   +  0
271900950116     O                                         +  1 'Porto:'
272000950116     O                       PORTO             +  1
272100021107     O                                         +  1 'Aperta/Riap.'
272200021107     O                       data8             +  1 '  /  /    '
272300950116     O                                         +  1 'C/Ass:'
272400051115     O                       ar9VCA         B  +  0
272500051115     O                       ar9CAS        2B  +  1
272600950116     O          E            RIGAS       1
272700950116     O                                           26 'Motivo giac:'
272800961010     O                       GCPCMC            +  1
272900930727     O                       §2ADES            +  1
273000120507     O*mm                    ULTDMC             132
273100120507     O          E            RIGAS       1
273200120507     O                       ULTDMC              81
273300040304     O          E   n55      RIGAS       1
273400930726     O                       SK(23)              66
273500120210     O                       SK(24)             130
273600040304     O          E    55      RIGAS       1
273700040304     O                       SK(36)              66
273800040304     O                       datadcr             70 '  /  /    '
273900930727     O          E            RIGAS       1
274000930726     O                       SK(25)              66
274100120210     O                       SK(26)             130
274200930727     O          E            RIGAS       1
274300930726     O                       SK(25)              66
274400120210     O                       SK(26)             130
274500040928     O          E            RIGAS       1  1
274600930726     O                       SK(27)              66
274700930729     O                       §2BDES              59
274800120210     O                       SK(28)             130
274900030911     o                       mitdesd            110
275000930729     O*
275100930729     O          E            RIGA1          2 06
275200930729     O                                          110 'Pagina'
275300930729     O                       PAGE          Z   +  1
275400930729     O*
275500930727     O          E            FINES       1
275600930726     O                       SK(1)               66
275700120210     O                       SK(30)             130
275800120210     O                       FAXPOP              78
275900030911     O          E            FINES       1
276000030911     O                       SK(31)              66
276100030911     O          E            FINES       0
276200030911     O                       SK(31)              66
276300030911     O          E            FINES       0
276400030911     O                       SK(31)              66
276500030912     O                       SK(32)             122
276600930727     O          E            FINES       1
276700930726     O                       SK(2)               66
276800120210     O                       SK(12)             130
276900930727     O          E            FINES       1
277000930726     O                       SK(3)               66
277100120210     O                       SK(13)             130
277200930727     O          E            FINES       1
277300930726     O                       SK(4)               66
277400120210     O                       SK(14)             130
277500930727     O          E            FINES       1
277600930726     O                       SK(5)               66
277700120210     O*                      SK(15)             131
277800030911     O*         E            FINES       1
277900030911     O*                      SK(6)               66
278000120210     O*                      SK(16)             131
278100930727     O          E            FINES       1
278200930726     O                       SK(7)               66
278300120210     O                       §2GGGL              13
278400120210     O                       SK(17)             130
278500930727     O          E            FINES       1
278600930726     O                       SK(8)               66
278700120210     O                       SK(18)             130
278800930727     O          E            FINES       1
278900930726     O                       SK(9)               66
279000120210     O                       SK(19)             130
279100930727     O          E            FINES       1
279200930726     O                       SK(10)              66
279300120210     O                       SK(20)             130
279400930727     O          E            FINES       1
279500930727     O                       SK(11)              66
279600030929     O          E            FINES       1
279700030929     O                       SK(33)              66
279800120210     o                       progmax       z      9
279900120210     O                       SK(34)             130
280000030929     O          E            FINES       1
280100030929     O                       SK(35)              66
280200130319     O          E            FINES2      1
280300930726     O                       SK(21)              66
280400971112     O                       RSUT               110
280500130319     O          E            FINES2      1
280600120210     O                       SK(29)             130
280700120210     O                       DESPOP             109
280800130319     O          E   n17      FINES2      1
280900120210     O                       trat130            130
281000130319     O          E   n17      FINES2      1
281100041014     O                       SK(37)              66
281200120210     O                       SK(38)             130
281300130319     O          E   n17      FINES2      1
281400041014     O                       SK(39)              66
281500120210     o                       mittfax             87
281600130319     O          E   n17      FINES2      1
281700120209     O                       SK(40)              66
281800120209     O                       SK(41)             100
281900120210     o                       gcpscms             57
282000130319     O          E   n17      FINES2      1
282100120210     O                       trat130            130
282200040708      *---------------------------------------------------------------------
282300040708      *INVIO TRAMITE MAIL
282400040721     O          E            TESTAM           02
282500071018     O*****                                      15 'P.Operativo di'
282600071018     O                                           11 'Filiale di'
282700071018     O                       DESLNP              32
282800040721     O                                           66 'Spettabile'
282900040721     O          E            TESTAM           03
283000040708     O               13      INDLNP              21
283100040721     O          E            TESTAM           04
283200040708     O               13      CAPLNP               6
283300040708     O               13      LOCLNP            +  1
283400040708     O               13      PROLNP            +  1
283500040721     O                       RAG                 91
283600040721     O          E            TESTAM           04
283700040721     O                       RAG                 91
283800040721     O          E            TESTAM           05
283900040708     O                                            4 'FAX'
284000040708     O                       FAXLNP            +  1
284100040720     O                                         +  2 'TEL'
284200040720     O                       TELLNP            +  1
284300040721     O                       VIA                 91
284400040721     O          E            TESTAM           06
284500040721     O                       CAP                 65
284600040708     O                       CIT               +  1
284700040708     O                       PRV               +  1
284800040708     O                       NZM               +  1
284900040721     O          E            TESTAM           07
285000040722     O                       SCH                 33
285100040721     O               04                          66 'Att.ne di '
285200040708     O               04      ATTPER            +  1
285300040721     O          E            TESTAM           08
285400040708     O                                           24 'Oggetto: COMUNICAZIONE '
285500040708     O                                         +  0 'DI APERTURA GIACENZA - '
285600040708     O*                      DATAU             +  1 '  /  /    '
285700040708     O          E            RIGASM      0  0
285800040708     O              n52                          65 '1° Avviso '
285900040708     O               52      xprog         z     65
286000040708     O               52                        +  1 '° Avviso '
286100040721     O          E            RIGASM           10
286200040708     O                                           25 'A fronte della spedizione'
286300040708     O                       gcpaas        z     30
286400040827     O                       gcplnp        z     34
286500040827     O                       gcpnrs        z     37
286600040708     O                       gcpnsp        z     46
286700040708     O                                           51 'del:'
286800040708     O                       DATSPE              62 '  /  /    '
286900040708     o                                           63 ','
287000040708     o                                           82 'abbiamo provveduto'
287100040708     o              n52                          93 'ad aprire '
287200040708     o               52                          93 'a riaprire'
287300040721     O          E            RIGASM           11
287400040721     o                                         +  0 'pratica di giacenza'
287500040708     o                                         +  1 'N.'
287600040708     o                       gcpfgc        z   +  0
287700040708     o                                         +  0 '/'
287800040708     o                       gcpngc        z   +  0
287900040708     O          E            RIGASM      1
288000040708     O                                            8 'in data '
288100040708     O                       data8             +  1 '  /  /    '
288200040708     o                                         +  1 'per il seguente motivo:'
288300040708     o                       gcpcmc            +  1
288400040708     o                       §2ades            +  1
288500040708     O          E            RIGASM      1
288600040708     o                       ultdmc            +  1
288700040708     O          E            RIGASM      1
288800040708     O                                           26 'Gli estremi della spedizio'
288900040708     O                                         +  0 'ne sono i seguenti :'
289000040708     O          E            RIGASM      1
289100040708     o                                           22 'Riferimenti mittente :'
289200040708     O                       blprmn        z   +  1
289300040708     O                       blprma            +  2
289400040708     O          E            RIGASM      1
289500040708     o                                           22 'Destinatario         :'
289600040708     O                       rsd               +  1
289700040708     O          E            RIGASM      1
289800131122     O                       indm              + 23
289900040708     O          E            RIGASM      1
290000040708     O                       cad               + 23
290100040708     O                       lod               +  1
290200040708     O                       prd               +  1
290300040708     O                       nzd               +  1
290400040708     O          E            RIGASM      1
290500040708     o                                           22 'Mittente originale   :'
290600040708     O                       blprmo            +  1
290700040721     o                                           75 'Codice cliente :'
290800040708     O                       gcpscm            +  1
290900040708     O          E            RIGASM      1
291000040708     o                                           22 'Colli                :'
291100040708     O                       blpncl        z   +  1
291200040721     o                                           75 'Porto          :'
291300040708     O                       porto             +  1
291400040708     O          E            RIGASM      1
291500040708     o                                           22 'Peso                 :'
291600040708     O                       blppkf        2   +  1
291700040721     o                                           75 'Contrassegno   :'
291800051115     O                       ar9VCA         B  +  1
291900051115     O                       ar9CAS        2B  +  1
292000040708     O          E            RIGASM      1
292100040708     o                                           22 'Natura della merce   :'
292200040708     O                       blpnas            +  1
292300040721     o                                           75 'Note           :'
292400060213     O                       ar4not         B  +  1
292500040802     O          E   n55      FINESm      2
292600040708     o                                           26 'Restiamo in attesa di vost'
292700040708     o                                         +  0 're disposizioni che '
292800040708     o                                         +  0 'potrete comunicarci '
292900040708     o                                         +  0 'direttamente cliccando qui'
293000040802     O          E   n55      FINESm      0
293100040708     o                                           26 'Restiamo in attesa di vost'
293200040708     o                                         +  0 're disposizioni che '
293300040708     o                                         +  0 'potrete comunicarci '
293400040708     o                                         +  0 'direttamente cliccando qui'
293500040802     O          E   n55      FINESm      0
293600040708     o                                           26 'Restiamo in attesa di vost'
293700040708     o                                         +  0 're disposizioni che '
293800040708     o                                         +  0 'potrete comunicarci '
293900040708     o                                         +  0 'direttamente cliccando qui'
294000040802     O          E    55      FINESm      2
294100040802     o                                           26 'Salvo diversa indicazione '
294200040802     o                                         +  0 'provvederemo alla riconseg'
294300040802     o                                         +  0 'na dal '
294400040802     O                       datadcr           +  1 '  /  /    '
294500040802     O          E    55      FINESm      1
294600040802     o                                           26 'Potete visualizzare le dis'
294700040802     o                                         +  0 'posizioni già inserite '
294800040802     o                                         +  0 'cliccando qui'
294900040708     O          E            FINESm      2
295000040907     O                                            4 'B#'
295100040701     o                                         +  0 '?nspediz='
295200040701     O                       gcplnp            +  0
295300040701     O                       gcpnrs            +  0
295400040701     O                       gcpnsp            +  0
295500040701     o                                         +  0 '&keygiac='
295600040630     O                       gcpaas            +  0
295700040630     O                       gcplnp            +  0
295800040630     O                       gcpnrs            +  0
295900040630     O                       gcpnsp            +  0
296000040630     O                       blpaas            +  0
296100040630     O                       blpmgs            +  0
296200040630     o                       gcpagc            +  0
296300040630     o                       gcpfgc            +  0
296400040630     o                       gcpngc            +  0
296500040630     o                       gcpagc            +  0
296600040630     o                       gcpmgc            +  0
296700040907     O                                         +  0 '#E'
296800040722     O          E            FINESm      2
296900040721     o                       cost1              105
297000040708     O          E            FINESm      1
297100040721     o                       cost2              105
297200040708     o                       §2bdes              63
297300040708     O          E            FINESm      1
297400040721     o                       cost3              105
297500040708     O          E            FINESm      1
297600040721     o                       cost4              105
297700040708     O          E            FINESm      1
297800040721     o                       cost5              105
297900040721     o                       §2gggl              14
298000040708     O          E            FINESm      1
298100040721     o                       cost6              105
298200040708     O          E            FINESm      1
298300040721     o                       cost7              105
298400040708     O          E            FINESm      1
298500040721     o                       cost8              105
298600040708     O          E            FINESm      1
298700040721     o                       cost9              105
298800040708     O          E            FINESm      1
298900040721     o                       cost10             105
299000040708     O          E            FINESm      1
299100040721     o                       cost11             105
299200040721     o                       mitdesd             52
299300040708     O          E            FINESm      1
299400040721     o                       cost12             105
299500040708     O          E            FINESm      1
299600040721     o                       cost13             105
299700040721     o                       progmax       z     12
299800040721     O          E            FINESm      1
299900040721     o                       cost14             105
300000040708     O          E            FINESm      2
300100040721     o                                           30 'Distinti saluti.'
300200040708     O          E            FINESm      1
300300120321     o               33                          67 'BRT S.P.A.'
300400120321     o**            n33      srsut               67
300500110506     o*                                        +  1 '('
300600110506     o*                      gcplnp            +  0
300700110506     o*                                        +  0 ')'
300800040708     O          E            FINESm      1
300900040721     O                       SK(29)             105
301000040721     O                       DESPOP              82
301100040708     O*****************************************************************
301200940923     OLET132    E            TESRAC           12
301300940923     O                                           45 '* ELENCO RACCOMANDATE *'
301400940923     O                                           50 'DEL'
301500940923     O                       WDATA8              63 '  /  /    '
301600940923     O          E            TESRAC           14
301700940923     O                                            8 'MITTENTE'
301800950123     O                       MITRGS              57
301900940923     O          E            TESRAC           15
302000950123     O                       MITIND              29
302100940923     O          E            TESRAC           16
302200950123     O                       MITCPF              14
302300950123     O                       MITLOC              35
302400950123     O                       MITPRO              38
302500940923     O          E            TESRAC           18
302600940923     O                                            6 'NUMERO'
302700940923     O                                           32 'D E S T I N A T A R I O'
302800940923     O          E            DETRAC      2
302900940923     O                       CONTA         Z      6
303000940923     O                       RAG                 44
303100940923     O          E            DETRAC      1
303200940923     O                       VIA                 44
303300940923     O          E            DETRAC      1
303400940923     O                       CAP                 18
303500940923     O                       CIT                 54
303600940923     O                       PRV                 57
303700940923     O                       NZM                 61
303800961211     O*------------------------------------------------*
303900961211     O*  Dettaglio raccomandate estero
304000961211     O          E            DETRAE      2
304100961211     O                       CONTAE        Z      6
304200961211     O                       STPRSM              44
304300961211     O          E            DETRAE      1
304400961211     O                       STPINM              44
304500961211     O          E            DETRAE      1
304600961211     O                       STPCAM              18
304700961211     O                       STPCIM              54
304800961211     O                       STPPRM              57
304900961211     O                       STPNZM              61
305000900430     OQSYSPRT   E            ERR         1
305100940923     O               22      GCPAAS               4
305200900430     O               22      GCPLNP            +  1
305300900430     O               22      GCPNRS            +  1
305400900430     O               22      GCPNSP            +  1
305500900430     O               22                        +  1 'BOLLA INESISTENTE NEL FI'
305600900430     O               22                        +  0 'LE STORICO'
305700900430     O               23      GCPLNP               4
305800900430     O               23                        +  1 'FILIALE INESISTENTE NELL'
305900900430     O               23                        +  1 'ORGANIGRAMMA'
306000900430     O               25      GCPSCM               8
306100900430     O               25                        +  1 'COD.CLIENTE INESISTENTE '
306200900430     O               25                        +  0 'NEL PIANO DEI CONTI'
306300120111**         CMD1
306400120420OVRPRTF FILE(LETGIA) OUTQ(piccionedb/barpicstm)
306500120111**         CMD2
306600120420OVRPRTF FILE(LET132) OUTQ(piccionedb/barpicstm)
306700961203**         CMD4
306800931122OVRPRTF FILE(LET132) FORMTYPE('*STD-000') HOLD(*YES) SAVE(*YES)
306900961203**         CMD5
307000931122OVRPRTF FILE(LETGIA) FORMTYPE('*STD-000')
307100961203**         CMD6
307200960507DLTOVR  FILE(LETGIA)
307300961203**         CMD7
307400961203OVRPRTF FILE(LETEST) FORMTYPE('*STD-000')
307500961203**         CMD8
307600961203DLTOVR  FILE(LETEST)
307700040629**         CMD9
307800040630OVRPRTF FILE(LETGIA) OUTQ(EMAILIN) USRDFNDTA('
307900040906OVRPRTF FILE(LETEST) OUTQ(EMAILIN) USRDFNDTA('
308000921028**         RIG
308100921029        COMUNIC. APERTURA GIACENZA
308200921029DISTINTA RIEPILOGO APERTURA GIAC.
308300930726**
308400961009          Restiamo in attesa di Vs. disposizioni  al numero  FAX    01
308500120210-sino alla ricezione di Vs.istruzioni saremo tenuti ad aderire a    02
308600120210-le spese di giacenza, dalla richiesta all'esecuzione delle Vs.i    03
308700120210 addebitate nella misura prevista dall'art.17/p delle ultime Con    04
308800120210 Camere di Commercio.                                               05
308900120210 canza di Vs. precisazioni saranno considerate a carico del desti   06
309000120210-trascorsi XX giorni dalla presente senza che  abbiate  fornito     07
309100120210 li, Vi ritorneremo la merce a Vs. rischio e con addebito di ogn    08
309200120210 giacenza sia risolta provvederemo al recupero di ns.crediti med    09
309300120210 o assegnazione della merce ai sensi degli art. 2756, 2761, 2797    10
309400120210 diritto per spese e competenze non recuperate.                     11
309500930726lla richiesta di riconsegna da parte del destinatario               12
309600030912struzioni,comprese eventuali assicurazioni Vi saranno               13
309700930726dizioni Generali CONFETRA, depositate presso tutte le               14
309800930728anno essere  addebitate le spese di giacenza. In man-               15
309900960507natario, se la spedizione e' in "assegnato".                        16
310000930726per iscritto istruzioni concrete, complete e attuabi-               17
310100930728i spesa. Trascorsi ulteriori  30  giorni senza che la               18
310200930727iante richiesta al giudice dell'esecuzione di vendita               19
310300120210 e 2798  del C.C. salvo e impregiudicato ogni  altro                20
310400930727          Distinti saluti.                                          21
310500110503                      S.p.a.                                        22
310600930726Vs. Disposizioni .................................................  23
310700930726.....................................................               24
310800930726..................................................................  25
310900930726.....................................................               26
311000960507La merce e' custodita presso la Ns. di:                             27
311100030911Addebito spese giacenza a carico:                                   28
311200120209            Filiale di      xxxxxxxx                                29
311300120210  nr.fax      o preferibilmente tramite il nostro                   30
311400120507sito internet www.brt.it   alla sezione  Area Clienti               31
311500030912(se in possesso di password di accesso) informandoVi che:           32
311600120209Dopo il  ° avviso, in caso di mancata risoluzione positiva della    33
311700120210 giacenza, Vi ritorneremo la merce a Vostro rischio                 34
311800120209e con addebito di ogni spesa.                                       35
311900040304Salvo diversa indicazione provvederemo alla riconsegna dal          36
312000040927Per ricevere le prossime comunicazioni di giacenze delle Vs. spedi  37
312100120507zioni via e-mail, Vi invitiamo a compilare la presente              38
312200120507inviando la stessa via fax oppure dando segnalazione a:             39
312300040929indicando i seguenti dati:                                          40
312400040929Codice cliente          e-mail __________________________________   41

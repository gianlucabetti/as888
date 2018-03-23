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
009400960201     D FNLG07        E DS                  EXTNAME(FNLG07DS)
009500050630     D FNLG09ds      E DS
009600160219     d fnlg24ds      e ds
009601170925      *ds per richiamo monitor variazioni
009602170925     d tigcp         e ds                  extname(tigcp00f)
009603170925     d fnlg33ds      e ds                  inz
009604170925     d tigcpa        e ds                  extname(tigcp00f)
009605170925     d                                     prefix(a)
009606170925     d tigcpb        e ds                  extname(tigcp00f)
009607170925     d                                     prefix(b)
009700970709     D* PARAMETRI PASSATI AL PGM TISI95R X CONTROLLO CAP
009800970709     D TISI95        E DS                  EXTNAME(TISI95DS)
009900941010     D DSUL41        E DS                  EXTNAME(TRUL41DS)
010000160311     D TR27DS        E DS                  EXTNAME(TRUL27DS1)
010100170116     d trulc7ds      E DS
010200150514     D fnlv59ds      E DS
010300160311     d  KS_CT                 74    118    dim(15)
010400160311     d  KS_pr                119    163    dim(15)
010500160311     d  KS_ie                164    178    dim(15)
010600040906     d fnlv55ds      e ds
010700040629     d trtcm1ds      e ds
010800040914     D dmra          e DS
010900110503     D dvpobrt       e DS
011000960205     D                 DS
011100960205     D  F2F                    1      1
011200960205     D  T2F                    2      2
011300960205     D  R2F                    3      3
011400960205     D  G2F                    4      4
011500960205     D  DFT                    5      5
011600961203     D  EST                    6      6
011700000913     D  VAG                    7      7
011800960205     D  UNI                    1     69
011900900423     D TCUDS           DS
012000940922     D  F3                     3      3
012100900423     D  F4                     4      4
012200900423     D  F56                    5      6
012300900502     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
012400900423     D  TCU                  398    697
012500900418     D                                     DIM(50)
012600900423     D  KCU                  698    847P 0
012700900418     D                                     DIM(50)
012800900418     D                                     PACKEVEN
012900931122     D*---- DS DI APPOGGIO PER OVRPRTF
013000931122     D APPDS           DS
013100931122     D  LIN                   37     39  0
013200931122     D  OVRAPP                 1     41
013300900502     D CNCR80        E DS
013400900608     D CNCR42        E DS
013500050607     d fnlg08ds      e ds
013600170116     d fnlg071ds     E DS
013700020826     d trul06ds      e ds
013800020826     d  lin£1                  1     90  0 dim(30)
013900900418     D KPJBA         E DS
014000020918     d savkpjbu        s                   like(kpjbu)
014100041111      **
014200041111     d est_Lnp         s                   like(GcpLnp)
014300041111     d est_DesLnp      s                   like(DesLnp)
014400041111     d est_DesTel      s                   like(TelLnp)
014500041111     d est_DesFax      s                   like(FaxLnp)
014600041111      **
014700940926     D FNLG73        E DS                  EXTNAME(FNLG73DS)
014800900423     D                 DS
014900940922     D                 DS
015000940922     D  GG                     1      2  0
015100940922     D  MM                     3      4  0
015200940922     D  AA                     5      8  0
015300940922     D  WDATA8                 1      8  0
015400940922     D                 DS
015500940923     D  GX                     1      2
015600940923     D  MX                     3      4
015700940923     D  AX                     5      8
015800940922     D  DATAU                  1      8  0
015900940922     D WLBDA8          DS
016000940922     D  G02DAT                 1      8  0
016100940922     D  G02INV                 9     16  0
016200940922     D  G02ERR                17     17
016300940922     D  G02TGI                18     22  0
016400021107     d data8           s              8  0
016500021107     d dataiso         s               d   datfmt(*iso)
016600021107     d dataeur         s               d   datfmt(*eur)
016700040329     d Wdatalav        DS
016800040329     d  dadata                 1      8  0
016900040329     d  adata                  9     16  0
017000040329     d  GioLav                17     21  0
017100120321     d MITTERR         c                   '@brt.it'
017200041022     D chkc            C                   const('''/§')
017300041022     D chkcs           C                   const('   ')
017400040721     d cost1           s            105
017500040721     d cost2           s            105
017600040721     d cost3           s            105
017700040721     d cost4           s            105
017800040721     d cost5           s            105
017900040721     d cost6           s            105
018000040721     d cost7           s            105
018100040721     d cost8           s            105
018200040721     d cost9           s            105
018300040721     d cost10          s            105
018400040721     d cost11          s            105
018500040721     d cost12          s            105
018600040721     d cost13          s            105
018700040721     d cost14          s            105
018800061214      *
018900061214     D TxtInOut        S           2048
019000061214     D ElencoChar      S            256
019100061214     D TipoElenco      S              1
019200061214     D CharSost        S              1
019300061214     D UpperCase       S              1
019400061214     D ChkNull         S              1
019500061214     D CharNull        S              1
019600061214     D EsitoBon        S              1
019700940922     I*--------------------------------------------------------------*
019800940922     IAZORG01L  AA
019900900423     C*---------------------------------------------------------------*
020000931217     C*  80 ON ---> RISTAMPA
020100931217     C*  81 ON ---> RICHIESTA RISTAMPA PER DATA
020200931217     C*
020300020826     C                   do        30            ii                3 0
020400020826     c                   setoff                                       10
020500020826     c                   if        lin£1(ii) = 0
020600020826     c                   leave
020700020826     c                   endif
020800990625     C   80
020900050214     CANN81KGIA          SETLL     tigcp02L
021000020826     c                   if        *in80 and *in81 or not *in80
021100050214     c     lin£1(ii)     setll     tigcp08l
021200020826     c                   endif
021300911018     C     *IN10         DOWEQ     '0'
021400920311     C   80
021500920311     CAN 81
021600050321     CORN80lin£1(ii)     READe     tigcp000                             1510
021700050321     C   15lin£1(ii)     READe(n)  tigcp000                               10
021800050321     c   15              iter
021900950214     C     *IN80         IFEQ      '1'
022000950214     C     *IN81         ANDEQ     '0'
022100050214     C     KGIA          READE     tigcp002                             1510
022200020826     c   10              goto      fine
022300950214     C  N15
022400950214     CANN10              DO
022500950214     C                   Z-ADD     GCPDMM        G02INV
022600950214     C                   Z-ADD     0             G02DAT
022700950214     C                   MOVEL     '3'           G02ERR
022800950214     C                   CALL      'XSRDA8'
022900950214     C                   PARM                    WLBDA8
023000950214     C                   Z-ADD     G02DAT        DATAU             8 0
023100950214     C                   END
023200950214     C                   END
023300950214     C*
023400940922     C     *IN15         IFEQ      '0'
023500940922     C     *IN10         ANDEQ     '0'
023600940923     C                   SETOFF                                       5230
023700041028      *   ma....
023800041028      *   devo prendere però in esame se sto facendo una ristampa di una
023900041028      *   mail e la voglio stampare in stampa normale per poi inviarla
024000041028      *   manualmente
024100041028     c                   move      'SI'          esegui_mail       2
024200160523     c                   clear                   parcel14         14
024300041028     **
024400041111     c                   if        d07ris = 'S' and d07tip = *blank and
024500041028     c                             (gcptcm = 'V' or gcptcm = 'W')
024600041028     **
024700041028     c                   move      'NO'          esegui_mail
024800041028     **
024900041028     c                   else
025000041028     ** altrimenti.....
025100041028     **   in tutti gli altri casi.......  controllo solo le E-mail xchè
025200040827      * le mail le invio solo con tipo specifico
025300040906     c                   if        gcptcm = 'V' and d07tip <> 'V' or
025400040906     c                             gcptcm = 'W' and d07tip <> 'W'
025500040827     c                   iter
025600040827     c                   endif
025700041028     **
025800041028     c                   endif
025900940922     C*-----------------------------------------------------------------*
026000940922     C* Verifico se la fil. apertura è presente in £7
026100940922     C  N30              DO
026200940922     C                   MOVE      GCPLNP        LNP§              3
026300940922     C     LNP§          LOOKUP    FIL£7                                  18
026400940922     C* Reperisco dati relativo al tipo modulo mitt.
026500940923     C                   Z-ADD     1             XX                3 0
026600940922     C     GCPTCM        LOOKUP    C2F(XX)                                32
026700961212     C     D07TIP        IFEQ      *BLANKS
026800961212     C     D07TIP        OREQ      GCPTCM
026900960205     C                   MOVEL     D2F(XX)       UNI
027000970205     C* Se tipo invio manuale, sono una primo livello e
027100970205     C* non ho specificato il tipo comunicazione non elab.
027200020730     C     SimTpP        IFNE      '2'
027300970205     C     T2F           ANDEQ     'B'
027400970205     C     D07TIP        ANDEQ     *BLANKS
027500970205     C                   SETOFF                                       32
027600970205     C                   END
027700970203     C                   ELSE
027800970203     C                   SETOFF                                       32
027900961212     C                   END
028000160219      *
028100960205     C  N87              MOVEL     G2F           WSVG2F            1
028200960205     C  N87              MOVEL     C2F(XX)       WSVC2F            1
028300160219      *
028400941222     C* Non considero la giacenza se
028500941222     C*     non è previsto l'invio della comunicazione al mittente
028600941222     C*     sono una 2° livello e il tipo invio comun. è per posta
028700941222     C*     ho un invio manuale e la filiale non è gestita in £7
028800160219      *
028900940922     C     *IN32         IFEQ      '0'
029000941222     C                   SETON                                        30
029100970206     C                   MOVEL     T2F           WT2F
029200941222     C                   ELSE
029300160219      *
029400160219      * controlla se si tratta di una bolla DPD con tanto di PARCEL
029500160219      *   da cui prendere il DEPOT
029600160219     c                   clear                   fnlg24ds
029700160523     c                   clear                   parcel14         14
029800160219     c                   z-add     GCPAAS        i24aas
029900160219     c                   z-add     GCPLNP        i24lnp
030000160219     c                   z-add     GCPNRS        i24nrs
030100160219     c                   z-add     GCPNSP        i24nsp
030200160219     c                   call      'FNLG24R'
030300160219     C                   parm                    fnlg24ds
030400160219      *
030500960306     C*  Controllo se esiste un luogo di invio comunicazione di giac.
030600960306     C                   CLEAR                   TRUL43
030700160219     c*
030800160219     c*  se c'è il parcel allora deve passare il Depot e verificare
030900160219     c*  se presente l'indirizzo mail del DEPOT mittente per inviare
031000160219     c*  l'informazione di apertura giacenza.
031100160219     c                   if          o24npr <> *blank
031200160303     c                   if        %subst(o24npr:1:4)<>'0844' and
031300160303     c                             %subst(o24npr:1:4)<>'0845'
031400160219     c                   movel     o24npr        D43DEPOT
031500160523     c                   movel     o24npr        parcel14
031600160303     c                   end
031700160219     c                   end
031800160219     c*
031900960306     C                   Z-ADD     GCPSCM        D43KSC
032000960306     C                   CALL      'TRUL43R'
032100960306     C                   PARM                    TRUL43
032200160219      *
032300960618     C     D43PER        COMP      *BLANKS                            0404
032400970819     C                   MOVEL     D43PER        ATTPER           35
032500160219      *
032600960201     C*  Reperisco dati cliente
032700960201     C                   EXSR      CHNIND
032800961205     C*------------------------------------------------------------*
032900961205     C*  No stampo la giacenza se il tipo invio non prevede stampa
033000961205     C     F2F           IFEQ      'N'
033100961205     C*------------------------------------------------------------*
033200961205     C*  No stampo la giacenza se il tipo invio è per posta e sono
033300961205     C*  una secondo livello
033400961205     C     WT2F          OREQ      'P'
033500020730     C     SimTpP        ANDEQ     '2'
033600961205     C*------------------------------------------------------------*
033700961205     C*  Se ho lanciato l'elaborazione con il profilo della primo
033800961205     C*  livello ma specificando una filiale di secondo stampo le
033900961205     C*  raccomandate solo se la lnp e nella £7 della filiale rich.
034000961205     C     WT2F          OREQ      'P'
034100020730     C     SimTpP        ANDNE     '2'
034200020809     C     D07FIL        ANDNE     SimFEl
034300961205     C     *IN18         ANDEQ     '0'
034400961205     C*------------------------------------------------------------*
034500961205     C*  No stampo la giacenza se il tipo invio è per fax e sono
034600961205     C*  una secondo livello
034700960201     C     WT2F          OREQ      'F'
034800020730     C     SimTpP        ANDEQ     '2'
034900961205     C*------------------------------------------------------------*
035000961205     C*  Se ho lanciato l'elaborazione con il profilo della primo
035100961205     C*  livello ma specificando una filiale di secondo stampo i
035200961205     C*  fax solo se la lnp e nella £7 della filiale rich.
035300961205     C     WT2F          OREQ      'F'
035400020730     C     SimTpP        ANDNE     '2'
035500020809     C     D07FIL        ANDNE     SimFEl
035600961205     C     *IN18         ANDEQ     '0'
035700961205     C*------------------------------------------------------------*
035800961205     C*  Stampo gli invii a mano solo se la lnp è in £7
035900960201     C     WT2F          OREQ      'B'
036000940922     C     *IN18         ANDEQ     '0'
036100961205     C*------------------------------------------------------------*
036200961205     C*  I tipi invii in lingua straniera vengono inviati solo dalla
036300961205     C*  filiale che gestisce in £7 la lnp estera
036400961205     C     WT2F          OREQ      'F'
036500961205     C     EST           ANDEQ     'E'
036600961205     C     GCPLNP        ANDNE     D07FIL
036700961211     C*
036800961211     C     WT2F          OREQ      'P'
036900961211     C     EST           ANDEQ     'E'
037000961211     C     GCPLNP        ANDNE     D07FIL
037100040629     C*
037200961205     C*------------------------------------------------------------*
037300940922     C                   SETON                                        30
037400940922     C                   END
037500950323     C*
037600950323     C                   END
037700941222     C*
037800940922     C                   END
037900170214     C*  No stampo la giacenza se il tipo invio non prevede stampa
038000170214     C*  ma ad ogni modo verifico se devo avvisare il destinatario
038100170221     c*m                 if        gcpriap = *blank and
038200170221     c*m                           f2f = 'N'
038300170221     c*m                 exsr      AvvisaDest
038400170221     c*m                 endif
038500960205     C*-----------------------------------------------------------------*
038600960205     C* Se specificato un tipo comunicazione controllo se corrisponde
038700960205     C  N30D07TIP        IFNE      *BLANKS
038800960205     C     GCPTCM        COMP      D07TIP                             3030
038900960205     C                   END
039000940922     C*-----------------------------------------------------------------*
039100900423     C** SE SI è IN FASE DI RISTAMPA SI ELABORANO SOLO LE GIACENZE
039200920309     C** CON FASE = 20 ALTRIMENTI QUELLE = 15
039300900509     C** E ENTRO I LIMITI DI ELABORAZIONE
039400911018     C   80
039500911018     CANN30GCPFAS        COMP      20                                 3030
039600940922     C   80
039700940922     CANN30
039800940922     CAN 81GCPDMM        COMP      D07DAT                             3030
039900911205     C  N30GCPFRG        COMP      0                                  3030
040000911111     C  N80
040100911111     CANN30GCPDMM        IFNE      0
040200911018     C                   SETON                                        3010
040300911018     C                   END
040400920310     C  N80
040500920310     CANN30
040600920310     CANN10GCPFAS        IFEQ      15
040700920310     C                   SETOFF                                       30
040800920310     C                   ELSE
040900920310     C                   SETON                                        30
041000900509     C                   END
041100911018     C  N30              DO
041200900423     C** AGGANCIO FILE STORICO
041300051115     C     KGAP          CHAIN     Fiar901L                           22
041400940923     C* Note su bolla
041500060213     C                   MOVEL     '8'           ar4TRC
041600060213     C                   MOVEL     *BLANKS       ar4NOT
041700060213     C     Kar4          CHAIN     Fiar401L                           22
041800940923     C     KGAP          CHAIN     FNBLP01L                           22
041900911018     C   22              EXCEPT    ERR
042000911018     C  N22              DO
042100931220     C     PREKSC        IFEQ      0
042200961218     C                   Z-ADD     BLPAAS        WSVAAS
042300961218     C                   Z-ADD     BLPMGS        WSVMGS
042400961218     C                   MOVEL     BLPNZD        WSVNZD
042500961218     C                   MOVEL     BLPTSP        WSVTSP
042600961218     C                   Z-ADD     BLPLNP        WSVLNP
042700961218     C                   Z-ADD     BLPCTR        WSVCTR
042800931220     C                   MOVE      GCPSCM        PREKSC
042900960412     C                   MOVE      GCPLNP        PRELNP            3 0
043000961218     C                   MOVE      GCPLNA        WSVLNA
043100940202     C                   END
043200931220     C* VERIFICO IL TIPO DI INVIO
043300931220     C                   MOVE      PREKSC        C8899             4
043400931220     C     PREKSC        IFNE      GCPSCM
043500931220     C     SAVFTP        ANDEQ     'B'
043600931220     C     C8899         OREQ      '8888'
043700931220     C     SAVFTP        ANDEQ     'B'
043800931220     C     C8899         OREQ      '9999'
043900931220     C     SAVFTP        ANDEQ     'B'
044000931220     C*
044100931220     C  N80              EXSR      STBOL
044200931220     C                   Z-ADD     0             GIANGC
044300931220     C                   END
044400961203     C*
044500961203     C** TESTO COMUNICAZIONE NON IN INGLESE
044600961203     C     EST           IFNE      'E'
044700961203     C** CODICE BOLLA
044800930726     C                   MOVEL     '3A'          COD
044900930726     C                   MOVEL     *BLANKS       KEY
045000940923     C                   MOVEL     BLPCBO        KEY
045100930726     C     KTAB          CHAIN     TABEL                              70
045200930726     C  N70              DO
045300930727     C                   Z-ADD     1             Y                 2 0
045400930726     C                   MOVEL     TBLUNI        DS3A
045500930726     C     §3ATB1        LOOKUP    CDB(Y)                                 25
045600930727     C   25              MOVEL     DES(Y)        PORTO            15
045700930726     C  N25              MOVEL     *BLANKS       PORTO
045800930726     C                   END
045900030911     c                   movel     §3atb1        mitdes            1
046000030911     c                   clear                   mitdesd          12
046100030911     c                   if        mitdes = 'F'
046200030911     c                   eval      mitdesd = 'Mittente    '
046300030911     c                   else
046400030911     c                   eval      mitdesd = 'Destinatario'
046500030911     c                   endif
046600900430     C** TIPO SPEDIZIONE
046700090520     C                   MOVEL     *BLANKS       DESTSP           15
046800090520     C                   MOVEL     '5E'          COD
046900090520     C                   MOVEL     *BLANKS       KEY
047000090520     C                   MOVEL     blptsp        KEY
047100090520     C     KTAB          CHAIN     TABEL                              70
047200090609     C  N70              MOVEL     TBLUNI        ds5e
047300090609     C  N70              MOVEL     §5EDES        destsp
047400900423     C** RICERCO DESCRIZIONE DELLA FILIALE DI PARTENZA
047500930318     C* MI SALVO IL RECORD VECCHIO
047600940923     C     GCPLNP        CHAIN     AZORG01L      UNIORG               23
047700911018     C   23              EXCEPT    ERR
047800911018     C  N23              DO
047900900423     C                   MOVEL     *BLANKS       DESLNP           20
048000900423     C                   MOVEL     *BLANKS       INDLNP           20
048100900423     C                   Z-ADD     0             CAPLNP            5 0
048200900423     C                   MOVEL     *BLANKS       LOCLNP           20
048300900423     C                   MOVEL     *BLANKS       PROLNP            2
048400900423     C                   MOVEL     *BLANKS       FAXLNP           13
048500900423     C                   MOVEL     *BLANKS       TLXLNP           13
048600900423     C                   MOVEL     *BLANKS       TELLNP           13
048700900423     C                   MOVEL     ORGDES        DESLNP
048800900423     C                   MOVEL     ORGIND        INDLNP
048900900430     C                   Z-ADD     ORGCPF        CAPLNP
049000900423     C                   MOVEL     ORGLOC        LOCLNP
049100900423     C                   MOVEL     ORGPRO        PROLNP
049200900423     C                   MOVEL     ORGTEL        TELLNP
049300900423     C                   MOVEL     ORGFAX        FAXLNP
049400900423     C                   MOVEL     ORGTLX        TLXLNP
049500041111      *
049600041111     C                   MOVEL     ORGDE3        OG143
049700041111     c                   clear                   est_DesLnp
049800041111     c                   clear                   est_DesTel
049900041111     c                   clear                   est_DesFax
050000041111     C**** sostituito con il campo Network la gestione dei vecchi flags
050100041111     C     §OgNTW        ifeq      'FED'
050200041111     C     §OgNTW        oreq      'EEX'
050300041111     C     §OgNTW        oreq      'EUP'
050400041111     c                   move      gcpLNP        est_Lnp
050500041111     C                   exsr      Hub_Estero
050600041111     c                   if        est_DesLnp <> *blank
050700041111     c                   movel     est_DesLnp    desLNP
050800041111     c                   movel     est_DesTel    telLNP
050900041111     c                   movel     est_DesFax    faxLNP
051000041111     C                   end
051100041111     C                   end
051200041111      *
051300900423     C                   END
051400961211     C                   END
051500900511     C** RICERCO DESCRIZIONE DEL COD.ELABORATORE
051600961202     C                   Z-ADD     D07FIL        ORGFIL
051700940923     C     ORGFIL        CHAIN     AZORG01L      UNIORG               70
051800900511     C                   MOVEL     *BLANKS       DESELA           20
051900940922     C  N70              DO
052000950123     C                   MOVEL     ORGDES        MITRGS
052100950123     C                   MOVEL     ORGIND        MITIND
052200950123     C                   MOVEL     ORGCPF        MITCPF
052300950123     C                   MOVEL     ORGLOC        MITLOC
052400950123     C                   MOVEL     ORGPRO        MITPRO
052500900511     C                   MOVEL     ORGDES        DESELA
052600900511     C                   END
052700961211     C*
052800961211     C** TESTO COMUNICAZIONE NON IN INGLESE
052900961211     C     EST           IFNE      'E'
053000900607     C** RICERCO DESCRIZIONE LUOGO DEPOSITO MERCE SULLA TABELLA
053100900607     C** SE NON LO TROVO LO RICERCO SULL'ORGANIGRAMMA
053200900502     C                   MOVEL     '2B'          COD
053300900502     C                   MOVEL     *BLANKS       KEY
053400900607     C                   MOVEL     GCPCCR        KEY
053500900502     C     KTAB          CHAIN     TABEL                              70
053600900607     C  N70              MOVEL     TBLUNI        DS2B
053700900607     C   70              DO
053800940923     C     GCPCCR        CHAIN     AZORG01L      UNIORG               70
053900900607     C                   MOVEL     *BLANKS       §2BDES
054000900607     C                   MOVEL     *BLANKS       §2BIND
054100900607     C                   MOVEL     *BLANKS       §2BLOC
054200900607     C                   MOVEL     *ZEROS        §2BCAP
054300900607     C                   MOVEL     *BLANKS       §2BPRO
054400900607     C  N70              DO
054500900607     C                   MOVEL     ORGDES        §2BDES
054600900607     C                   MOVEL     ORGIND        §2BIND
054700900607     C                   MOVEL     ORGLOC        §2BLOC
054800900607     C                   Z-ADD     ORGCPF        §2BCAP
054900900607     C                   MOVEL     ORGPRO        §2BPRO
055000900502     C                   END
055100900607     C                   END
055200900607     C** RICERCO COD.MOTIVO GIACENZA SULLA TABELLA
055300900607     C                   MOVEL     '2A'          COD
055400900607     C                   MOVEL     *BLANKS       KEY
055500900607     C                   MOVEL     GCPCMC        KEY
055600900607     C     KTAB          CHAIN     TABEL                              70
055700900607     C  N70              MOVEL     TBLUNI        DS2A
055800961203     C*
055900961203     C** TESTO COMUNICAZIONE IN INGLESE
056000961203     C                   ELSE
056100961203     C** RICERCO COD.MOTIVO GIACENZA SULLA TABELLA
056200961203     C                   MOVEL     '2X'          COD
056300961203     C                   MOVEL     *BLANKS       KEY
056400961203     C                   MOVEL     GCPCMC        KEY
056500961203     C     KTAB          CHAIN     TABEL                              70
056600961203     C  N70              MOVEL     TBLUNI        WMOTEV           75
056700961203     C                   END
056800961203     C*
056900911018     C* PRELEVO EVENTUALE ULTERIORE DESCRIZIONE
057000911018     C                   Z-ADD     10            FASE
057100911205     C                   Z-ADD     1             NPR
057200050214     C     KNOT          CHAIN     tignp01L                           69
057300911018     C  N69              MOVEL     GNPDMC        ULTDMC
057400911018     C   69              MOVEL     *BLANKS       ULTDMC           50
057500961203     C*
057600961203     C***  NO ESTERO
057700961203     C     EST           IFNE      'E'
057800961203     C*
057900930728     C* SE IL MOTIVO GIACENZA E' GENERICO GLI MUOVO L'ULT. MOTIVO
058000930728     C     §2ADES        IFEQ      *BLANKS
058100930728     C                   MOVEL     ULTDMC        §2ADES
058200930728     C                   MOVEL     *BLANKS       ULTDMC
058300930728     C                   END
058400900423     C** SE IL CODICE è CODIFICATO PRENDO I DATI DELLA RAG.SOC.
058500900423     C** DAL PDC SE è CODICE 88 O 99 PRENDO I DATI O DALL'ANAGRAFICA
058600900423     C** D'APPOGGIO SE ESISTE O DALLO STORICO BOLLE
058700940922     C                   MOVEL     *BLANKS       RAG              35
058800931217     C                   MOVEL     *BLANKS       VIA
058900931217     C                   MOVEL     *BLANKS       CIT
059000931217     C                   MOVEL     *BLANKS       PRV               2
059100940923     C                   MOVEL     *BLANKS       CAP               9
059200941007     C                   MOVEL     *BLANKS       NZM               3
059300960306     C     D43TRV        IFNE      'S'
059400960306     C*
059500900423     C     CODMIT        IFNE      8888
059600900423     C     CODMIT        ANDNE     9999
059700960201     C     KACO          CHAIN     CNACO000                           26
059800960201     C   26              EXCEPT    ERR
059900960201     C  N26              DO
060000900423     C                   MOVEL     ACORAG        RAG
060100940922     C                   MOVEL     INDVIA        VIA              35
060200940923     C                   MOVEL     INDCIT        CIT              35
060300921026     C                   MOVEL     INDPRV        PRV
060400050223     C                   MOVEL     INDCAe        CAP
060500941007     C                   MOVEL     INDSTA        NZM
060600900423     C                   END
060700900423     C                   ELSE
060800050214     C     KGAP          CHAIN     tigap000                           70
060900931217     C     *IN70         IFEQ      '0'
061000931217     C                   MOVEL     GAPRSM        RAG
061100940922     C                   MOVEL     GAPINM        VIA
061200940922     C                   MOVEL     GAPLOM        CIT
061300931217     C                   MOVEL     GAPPRM        PRV
061400940923     C                   MOVEL     GAPCAM        CAP
061500940923     C                   MOVEL     GAPNZM        NZM
061600931217     C                   ELSE
061700931217     C                   MOVEL     BLPRSM        RAG
061800940922     C                   MOVEL     BLPINM        VIA
061900940922     C                   MOVEL     BLPLOM        CIT
062000931217     C                   MOVEL     BLPPRM        PRV
062100940923     C                   MOVEL     BLPCAM        CAP
062200940923     C                   MOVEL     BLPNZM        NZM
062300900423     C                   END
062400931217     C                   END
062500960306     C*  Controllo se devo reimpostare i dati dal luogo
062600960306     C                   ELSE
062700960306     C                   MOVEL     D43RAG        RAG
062800960306     C                   MOVEL     D43IND        VIA
062900960306     C                   MOVEL     D43LOC        CIT
063000960306     C                   MOVEL     D43PRO        PRV
063100960306     C                   MOVEL     D43CAP        CAP
063200960306     C                   MOVEL     D43NAZ        NZM
063300960306     C                   END
063400961203     C**
063500040426     C**ricerca limite massimo riaperture per avviso in stampa se
063600040426      * esiste la particolarità altrimenti lascia 2
063700040426     C                   clear                   progmax           2 0
063800040426     C                   MOVEL     *BLANKS       KEY
063900040426     C                   MOVEL     '7Q'          COD
064000040426     c                   if        blpgga <> *blank
064100040426     C                   MOVEL     blpgga        KEY
064200040426     c                   else
064300040426     C                   MOVEL     'GR'          KEY
064400040426     c                   endif
064500040426     c
064600040426     C     KTAB          CHAIN     TABEL                              70
064700040426     C  N70              MOVEL     TBLUNI        DS7q
064800040426     C  N70              z-add     §7q5rp        progmax
064900040426     C** SE è UNA GIACENZA RIAPERTA STAMPA AVVISO progressivo
065000040426     c                   z-add     0             xprog             2 0
065100050214     C     GCPriap       IFne      *blank
065200050630     c                   clear                   fnlg09ds
065300050630     c                   eval      d09tla = 'R'
065400050630     c                   eval      d09aas = GCPAASa
065500050630     c                   eval      d09lnp = GCPLNPa
065600050630     c                   eval      d09nrs = GCPNRSa
065700050630     c                   eval      d09nsp = GCPNSPa
065800050630     c                   movel(p)  fnlg09ds      kpjbu
065900050630     C                   CALL      'FNLG09R'
066000050630     C                   PARM                    kpjba
066100050630     c                   movel(p)  kpjbu         fnlg09ds
066200050630     c     d09cnt        sub       d09ced        xprog
066300961203     C                   SETON                                        52
066400961203     C                   END
066500961203     C** RIBALTAMENTO DATA SPEDIZIONE
066600961203     C                   MOVEL     BLPAAS        G02INV
066700961203     C                   MOVE      BLPMGS        G02INV
066800961203     C                   Z-ADD     0             G02DAT
066900961203     C                   MOVEL     '3'           G02ERR
067000961203     C                   CALL      'XSRDA8'
067100961203     C                   PARM                    WLBDA8
067200961203     C                   Z-ADD     G02DAT        DATSPE            8 0
067300961203     C* PRENDO ULTIME 3 CIFRE DEL SEGNACOLLO AL
067400961203     C                   MOVE      BLPNCA        STANCA            3 0
067500961203     C*
067600961203     C***  SI ESTERO
067700961203     C                   ELSE
067800961203     C*  Reperisco i dati mittente per la stampa della lettera
067900961203     C*  comunicazione in inglese
068000961203     C                   EXSR      DATMIT
068100961203     C                   END
068200921029     C*
068300931123     C   87PREKSC        IFNE      GCPSCM
068400930727     C     C8899         OREQ      '8888'
068500961203     C     C8899         OREQ      '9999'
068600961203     C*** ESTERO
068700961203     C     EST           IFNE      'E'
068800961203     C     CONT          IFGT      50
068900930729     C                   EXCEPT    RIGA1
069000930729     C                   END
069100961203     C*
069200961203     C                   ELSE
069300961203     C     CONT          IFGT      50
069400961203     C     WT2F          IFEQ      'F'
069500961203     C                   ADD       1             STPPAG
069600961203     C                   WRITE     TESFAX
069700961203     C                   END
069800961203     C                   WRITE     TESGEN
069900961203     C                   END
070000961203     C                   END
070100961203     C*
070200940627     C** RICERCO SU TARIFFA I GG DA STAMPARE SU MODULO
070300950901     C  N49              Z-ADD     PREKSC        KSC
070400961203     C     EST           IFNE      'E'
070500961218     C                   Z-ADD     WSVAAS        WAAS
070600961218     C                   Z-ADD     WSVMGS        WMGS
070700961218     C                   MOVEL     WSVNZD        WNZD
070800961218     C                   MOVEL     WSVTSP        WTSP
070900961218     C                   Z-ADD     WSVLNP        WLNP
071000961218     C                   Z-ADD     WSVCTR        WCTR
071100961218     C                   MOVE      WSVLNA        WLNA
071200961203     C  N49              EXSR      CHKTFG
071300961203     C  N49              EXSR      CHKPOP
071400130319      *raccomandata
071500130319     c                   if        not *in16
071600130319     C  n49              EXCEPT    FINES
071700130412     c                   if        cont >= 55
071800130319     c                   except    testa
071900130319     c                   endif
072000130319     c  n49              except    fines2
072100130319     c                   else
072200130319      *mail
072300130319     C  N49              EXCEPT    FINESm
072400130319     c                   endif
072500961203     C                   ELSE
072600040906     c  n16
072700040906     CanN49              WRITE     FINAPE
072800040906     c   16
072900040906     CanN49              WRITE     finmail
073000961203     C  N49              WRITE     PIEDE
073100961203     C                   END
073200961203     C*
073300960205     C                   MOVEL     G2F           WSVG2F            1
073400960205     C                   MOVEL     C2F(XX)       WSVC2F            1
073500930727     C                   Z-ADD     0             PAGE
073600961203     C                   Z-ADD     0             STPPAG
073700940202     C                   SETOFF                                       12
073800960201     C     WT2F          IFEQ      'P'
073900940202     C                   SETON                                        12
074000120112     c*mm                exsr      ovrracc
074100940202     C                   END
074200940202     C                   SETON                                        13
074300971112     C     GCPLNP        CHAIN     AZORG01L      UNIORG               23
074400020717     C  N23              MOVEL     ORGDE3        OG143
074500020717     C**** sostituito con il campo Network la gestione dei vecchi flags
074600020717     C  N23§OgNTW        ifeq      'FED'
074700020717     C     §OgNTW        oreq      'EEX'
074800020717     C     §OgNTW        oreq      'EUP'
074900940202     C                   SETOFF                                       13
075000940202     C                   END
075100930728     C* PRIMA DI STAMPARE LA TESTATA MI COMPONGO LUOGO E DATA
075200960205     C     G2F           IFNE      'S'
075300961203     C*
075400040906     C                   EXSR      LUOGO
075500961203     C     EST           IFNE      'E'
075600040629      * per invio via mail overraida il file correttamente
075700040701     c                   setoff                                       16
075800040629     c                   if        wt2f = 'M'
075900041028     c                   if        esegui_mail = 'SI'
076000040701     c                   exsr      invxmail
076100040629     c                   exsr      preparaovr
076200041028     c                   end
076300040629     c                   endif
076400040708     C  n16              EXCEPT    TESTA
076500040708     C   16              EXCEPT    TESTAm
076600961203     C                   Z-ADD     18            CONT              2 0
076700961203     C                   ELSE
076800040906      * per invio via mail overraida il file correttamente
076900040906     c                   setoff                                       16
077000040906     c                   if        wt2f = 'M'
077100041028     c                   if        esegui_mail = 'SI'
077200040914     c                   exsr      invxmail
077300040906     c                   exsr      preparaovre
077400041028     c                   end
077500040906     c                   endif
077600961203     C     WT2F          IFEQ      'F'
077700961204     C                   Z-ADD     1             STPPAG
077800961203     C                   WRITE     TESFAX
077900961203     C                   END
078000961203     C                   WRITE     TESGEN
078100961203     C                   Z-ADD     13            CONT
078200961203     C                   END
078300961203     C*
078400930727     C                   END
078500931122     C                   END
078600930728     C*
078700930728     C  N87              SETON                                        87
078800950901     C                   SETOFF                                       49
078900960205     C     C2F(XX)       IFNE      WSVC2F
079000950901     C     CONT          ANDLE     50
079100960205     C     G2F           ANDNE     'S'
079200950901     C     WSVG2F        IFEQ      'S'
079300950901     C     *IN48         OREQ      '1'
079400950901     C                   SETOFF                                       12
079500960201     C     WT2F          IFEQ      'P'
079600950901     C                   SETON                                        12
079700120112     c*mm                exsr      ovrracc
079800950901     C                   END
079900950901     C                   SETON                                        13
080000971112     C     GCPLNP        CHAIN     AZORG01L      UNIORG               23
080100020717     C  N23              MOVEL     ORGDE3        OG143
080200020717     C**** sostituito con il campo Network la gestione dei vecchi flags
080300020717     C  N23§OgNTW        ifeq      'FED'
080400020717     C     §OgNTW        oreq      'EEX'
080500020717     C     §OgNTW        oreq      'EUP'
080600971112     C                   SETOFF                                       13
080700971112     C                   END
080800961203     C** ESTERO
080900040906     C                   EXSR      LUOGO
081000961203     C     EST           IFNE      'E'
081100040630      * per invio via mail overraida il file correttamente
081200040701     c                   setoff                                       16
081300040630     c                   if        wt2f = 'M'
081400041028     c                   if        esegui_mail = 'SI'
081500040701     c                   exsr      invxmail
081600040630     c                   exsr      preparaovr
081700041028     c                   end
081800040630     c                   endif
081900040708     C  n16              EXCEPT    TESTA
082000040708     C   16              EXCEPT    TESTAm
082100950901     C                   Z-ADD     18            CONT
082200961203     C                   ELSE
082300040906     c                   if        wt2f = 'M'
082400041028     c                   if        esegui_mail = 'SI'
082500040914     c                   exsr      invxmail
082600040906     c                   exsr      preparaovre
082700041028     c                   end
082800040906     c                   endif
082900961203     C     WT2F          IFEQ      'F'
083000961204     C                   Z-ADD     1             STPPAG
083100961203     C                   WRITE     TESFAX
083200961203     C                   END
083300961203     C                   WRITE     TESGEN
083400961203     C                   Z-ADD     13            CONT
083500961203     C                   END
083600961203     C*
083700950901     C                   END
083800950901     C                   END
083900950901     C*
084000950901     C                   SETOFF                                       48
084100960205     C     C2F(XX)       IFNE      WSVC2F
084200950901     C     WSVG2F        ANDNE     'S'
084300950901     C     CONT          IFGT      50
084400961203     C     EST           IFNE      'E'
084500950901     C                   EXCEPT    RIGA1
084600961203     C*
084700961203     C                   ELSE
084800961203     C     WT2F          IFEQ      'F'
084900961203     C                   ADD       1             STPPAG
085000961203     C                   WRITE     TESFAX
085100961203     C                   END
085200961203     C                   WRITE     TESGEN
085300961203     C                   END
085400961203     C*
085500950901     C                   END
085600961203     C*
085700950901     C                   SETON                                        48
085800950901     C** RICERCO SU TARIFFA I GG DA STAMPARE SU MODULO
085900961218     C                   Z-ADD     WSVAAS        WAAS
086000961218     C                   Z-ADD     WSVMGS        WMGS
086100961218     C                   MOVEL     WSVNZD        WNZD
086200961218     C                   MOVEL     WSVTSP        WTSP
086300961218     C                   Z-ADD     WSVLNP        WLNP
086400961218     C                   Z-ADD     WSVCTR        WCTR
086500040707     C                   MOVE      WSVLNA        WLNA
086600040707     C                   Z-ADD     GCPSCM        KSC               7 0
086700040707     C     EST           IFNE      'E'
086800040707     C                   EXSR      CHKTFG
086900040707     C                   EXSR      CHKPOP
087000130319      *raccomandata 16 off
087100130319     c                   if        not *in16
087200130319     C                   EXCEPT    FINES
087300130412     c                   if        cont >= 55
087400130319     c                   except    testa
087500130319     c                   endif
087600130319     c                   except    fines2
087700130319     c                   else
087800130319      *mail
087900130319     C                   EXCEPT    FINESm
088000130319     c                   endif
088100040707     C                   ELSE
088200040906     C  n16              WRITE     FINAPE
088300040906     C   16              WRITE     finmail
088400961203     C                   WRITE     PIEDE
088500961203     C                   END
088600961203     C*
088700950901     C                   END
088800960205     C                   MOVEL     G2F           WSVG2F
088900960205     C                   MOVEL     C2F(XX)       WSVC2F
089000930728     C*
089100950901     C     *IN90         IFEQ      *OFF
089200950901     C     CONT          ORGT      50
089300960205     C     G2F           OREQ      'S'
089400950901     C                   SETOFF                                       12
089500960201     C     WT2F          IFEQ      'P'
089600950901     C                   SETON                                        12
089700120112     c*mm                exsr      ovrracc
089800950901     C                   END
089900950901     C                   SETON                                        13
090000971112     C     GCPLNP        CHAIN     AZORG01L      UNIORG               23
090100020717     C  N23              MOVEL     ORGDE3        OG143
090200020717     C**** sostituito con il campo Network la gestione dei vecchi flags
090300020717     C  N23§OgNTW        ifeq      'FED'
090400020717     C     §OgNTW        oreq      'EEX'
090500020717     C     §OgNTW        oreq      'EUP'
090600950901     C                   SETOFF                                       13
090700950901     C                   END
090800961203     C*  NO ESTERO
090900040906     C                   EXSR      LUOGO
091000961203     C     EST           IFNE      'E'
091100040630      * per invio diverso da mail overraida il file correttamente
091200040701     c                   setoff                                       16
091300040701     c                   if        wt2f = 'M'
091400041028     c                   if        esegui_mail = 'SI'
091500040701     c                   exsr      invxmail
091600040701     c                   exsr      preparaovr
091700041028     c                   end
091800040701     c                   endif
091900040708     C  n16              EXCEPT    TESTA
092000040708     C   16              EXCEPT    TESTAm
092100961203     C                   Z-ADD     18            CONT
092200961203     C                   ELSE
092300961203     C*  ESTERO
092400040906     c                   if        wt2f = 'M'
092500041028     c                   if        esegui_mail = 'SI'
092600040914     c                   exsr      invxmail
092700040906     c                   exsr      preparaovre
092800041028     c                   end
092900040906     c                   endif
093000961203     C     WT2F          IFEQ      'F'
093100961204     C                   Z-ADD     1             STPPAG
093200961203     C                   WRITE     TESFAX
093300961203     C                   END
093400961203     C                   WRITE     TESGEN
093500961203     C                   Z-ADD     13            CONT
093600961203     C                   END
093700950901     C  N90              SETON                                        90
093800950901     C                   END
093900960201     C*--------------------------------------------------------*
094000960201     C*  Se non è cambiato destinatario lo imposto con i dati
094100960201     C*  della bolla
094200960201     C                   CLEAR                   FNLSN0
094300960201     C                   Z-ADD     BLPAAS        DN0AAS
094400960201     C                   Z-ADD     BLPLNP        DN0LNP
094500960201     C                   Z-ADD     BLPNRS        DN0NRS
094600960201     C                   Z-ADD     BLPNSP        DN0NSP
094700960201     C                   MOVEL     KPJBU         WSVKPJ          256
094800960201     C                   MOVEL     FNLSN0        KPJBU
094900960201     C                   CALL      'FNLSN0R'
095000960202     C                   PARM                    KPJBA
095100960201     C                   MOVEL     KPJBU         FNLSN0
095200960201     C                   MOVEL     WSVKPJ        KPJBU
095300961203     C*
095400961203     C** NO ESTERO  ---
095500961203     C     EST           IFNE      'E'
095600961203     C*
095700960201     C     DN0TRV        IFNE      'S'
095800940923     C                   MOVEL     BLPLOD        LOD              20
095900940923     C                   MOVEL     BLPPRD        PRD               2
096000941007     C                   MOVEL     BLPNZD        NZD               3
096100940923     C                   MOVEL     BLPRSD        RSD              25
096200940923     C                   MOVEL     BLPIND        IND              25
096300131122     C                   MOVEL     BLPIND        INDm             35
096400960201     C                   MOVEL     BLPCAD        CAD               9
096500960201     C                   ELSE
096600960201     C                   MOVEL     DN0LOD        LOD              20
096700960201     C                   MOVEL     DN0PRD        PRD               2
096800960201     C                   MOVEL     DN0NZD        NZD               3
096900960201     C                   MOVEL     DN0RSD        RSD              25
097000960201     C                   MOVEL     DN0IND        IND              25
097100131122     C                   MOVEL     DN0IND        INDm
097200960201     C                   MOVEL     DN0CAD        CAD               9
097300960201     C                   END
097400960201     C*--------------------------------------------------------*
097500021107     c                   movel     gcpagc        data8
097600021107     c                   move      gcpmgc        data8
097700021107     c                   move      data8         dataiso
097800021107     c                   move      dataiso       dataeur
097900021107     c                   move      dataeur       data8
098000040304     c                   setoff                                       55
098100040304     c                   clear                   datadcr           8 0
098200040304     c                   if        gcpcmc = '016' or gcpcmc = '023' or
098300040304     c                             gcpcmc = '100' or gcpcmc = '101'
098400040304     c                   if        gcpded > 0
098500040329     c                   move      gcpded        dadata
098600040329     c                   move      gcpded        adata
098700040329     c                   CALL      'XSRLAV8'
098800040329     c                   PARM                    Wdatalav
098900040329     c                   z-add     giolav        giolavs           1 0
099000040304     c                   move      gcpded        dataiso
099100040329     c                   do        *hival
099200040304     c     dataiso       adddur    1:*d          dataiso
099300040329     c                   move      gcpded        dadata
099400040329     c                   move      dataiso       adata
099500040329     c                   CALL      'XSRLAV8'
099600040329     c                   PARM                    Wdatalav
099700040329     c                   if        giolav = 0 or giolav > giolavs
099800040329     c                   leave
099900040329     c                   endif
100000040329     c                   enddo
100100040304     c                   move      dataiso       dataeur
100200040304     c                   move      dataeur       datadcr
100300040304     c                   seton                                        55
100400040304     c                   endif
100500040304     c                   endif
100600040929      *reperisce mittente mail da specificare a fine fax riferimento
100700040929     C  n16              EXsr      Repmitt
100800040708     C  n16              EXCEPT    RIGAS
100900040708     C   16              EXCEPT    RIGASm
101000931123     C                   ADD       10            CONT
101100961203     C*  Reperisco dati spedizione x estero
101200961203     C                   ELSE
101300961203     C                   EXSR      DATASP
101400040330     c*----------
101500040330     c                   setoff                                       56
101600040330     c                   clear                   stpDCR            8 0
101700040330     c                   if        gcpcmc = '016' or gcpcmc = '100' or
101800040330     c                             gcpcmc = '101'
101900040330     c**********                   or gcpcmc = '023'
102000040330     c                   if        gcpded > 0
102100040330     c                   move      gcpded        dadata
102200040330     c                   move      gcpded        adata
102300040330     c                   CALL      'XSRLAV8'
102400040330     c                   PARM                    Wdatalav
102500040330     c                   z-add     giolav        giolavs           1 0
102600040330     c                   move      gcpded        dataiso
102700040330     c                   do        *hival
102800040330     c     dataiso       adddur    1:*d          dataiso
102900040330     c                   move      gcpded        dadata
103000040330     c                   move      dataiso       adata
103100040330     c                   CALL      'XSRLAV8'
103200040330     c                   PARM                    Wdatalav
103300040330     c                   if        giolav = 0 or giolav > giolavs
103400040330     c                   leave
103500040330     c                   endif
103600040330     c                   enddo
103700040330     c                   move      dataiso       dataeur
103800040330     c                   move      dataeur       stpDCR
103900040330     c                   seton                                        56
104000040330     c                   endif
104100040330     c                   endif
104200040330     c*----------
104300961203     C                   END
104400961203     C*
104500960205     C     G2F           IFEQ      'S'
104600931123     C                   SETON                                        49
104700050609     c                   eval      d07ok = 'S'
104800940627     C** RICERCO SU TARIFFA I GG DA STAMPARE SU MODULO
104900940627     C                   Z-ADD     GCPSCM        KSC               7 0
105000961218     C                   Z-ADD     BLPAAS        WAAS
105100961218     C                   Z-ADD     BLPMGS        WMGS
105200961218     C                   MOVEL     BLPNZD        WNZD
105300961218     C                   MOVEL     BLPTSP        WTSP
105400961218     C                   Z-ADD     GCPLNP        WLNP
105500961218     C                   Z-ADD     BLPCTR        WCTR
105600961218     C                   MOVE      BLPLNA        WLNA
105700961203     C** NO ESTERO
105800961203     C     EST           IFNE      'E'
105900961203     C                   EXSR      CHKTFG
106000961203     C                   EXSR      CHKPOP
106100130319      *raccomandata 16 off
106200130319     c                   if        not *in16
106300130319     C                   EXCEPT    FINES
106400130412     c                   if        cont >= 55
106500130319     c                   except    testa
106600130319     c                   endif
106700130319     c                   except    fines2
106800130319     c                   else
106900130319      *mail
107000040708     C   16              EXCEPT    FINESm
107100130319     c                   endif
107200961203     C                   ELSE
107300961203     C** ESTERO
107400040906     c                   if        not *in16
107500040330     c                   if        *in56 = *on and stpDCR >0
107600040330     C                   WRITE     FINDCR
107700040330     c                   else
107800961203     C                   WRITE     FINAPE
107900040330     c                   end
108000040906     c                   else
108100040906     C                   WRITE     finmail
108200040906     c                   end
108300040330     C*
108400961203     C                   WRITE     PIEDE
108500961203     C                   END
108600961203     C*
108700930726     C                   END
108800950901     C*
108900930726     C*
109000960201     C     WT2F          IFEQ      'B'
109100921028     C                   ADD       1             GIANGC            3 0
109200911021     C                   END
109300921029     C*
109400911021     C** STAMPA RACC.TE SE IN TAB. TIPO COMUNICAZ. E' PREVISTA
109500911021     C** OPPURE STAMPA BOLLA PER PADRONCINO
109600911210     C                   MOVE      GCPSCM        QUATR             4 0
109700940922     C     D07RAC        IFEQ      'S'
109800960201     C     WT2F          IFEQ      'P'
109900940202     C     GCPSCM        ANDNE     SAVKSC
110000960201     C     WT2F          OREQ      'P'
110100911210     C     QUATR         ANDEQ     8888
110200960201     C     WT2F          OREQ      'P'
110300911210     C     QUATR         ANDEQ     9999
110400120113     C*mm                EXSR      STARAC
110500900423     C                   END
110600931217     C                   END
110700920521     C* MEMORIZZO CODICE CLIENTE PER STAMPA DISTINTA GIAC. APERTE
110800921029     C     SAVFRP        IFEQ      'S'
110900920521     C     GCPSCM        ANDNE     PREKSC
111000940927     C                   CLEAR                   FNLG73
111100940923     C                   MOVE      D07DAT        D73DTA
111200940923     C                   MOVE      D07DAT        D73DTD
111300940923     C                   MOVE      PREKSC        D73KSC
111400940923     C                   MOVE      KCI           D73KCC
111500940923     C                   MOVEL     FNLG73        KPJBU
111600940923     C                   MOVEL     'LG73'        KCOAZ
111700920525     C                   CALL      'BCH10'
111800920525     C                   PARM                    KPJBA
111900920525     C*
112000920521     C                   END
112100920521     C*
112200920518     C                   Z-ADD     GCPAAS        SAVAAS            2 0
112300920518     C                   Z-ADD     GCPLNP        SAVLNP            3 0
112400920518     C                   Z-ADD     GCPNRS        SAVNRS            2 0
112500920518     C                   Z-ADD     GCPNSP        SAVNSP            7 0
112600960201     C                   MOVE      WT2F          SAVFTP            1
112700960205     C                   MOVE      R2F           SAVFRP            1
112800911204     C                   Z-ADD     GCPSCM        PREKSC            7 0
112900961218     C                   Z-ADD     BLPAAS        WSVAAS
113000961218     C                   Z-ADD     BLPMGS        WSVMGS
113100961218     C                   MOVEL     BLPNZD        WSVNZD
113200961218     C                   MOVEL     BLPTSP        WSVTSP
113300961218     C                   Z-ADD     BLPLNP        WSVLNP
113400961218     C                   Z-ADD     BLPCTR        WSVCTR
113500960412     C                   MOVE      GCPLNP        PRELNP            3 0
113600961218     C                   MOVE      GCPLNA        WSVLNA
113700900423     C*
113800911018     C                   END
113900911018     C                   END
114000950323     C* SOLO SE NON SI è IN RISTAMPA...
114100950323     C     D07RIS        IFNE      'S'
114200950323     C     GCPFAS        ANDEQ     15
114300950323     C     GCPFRG        ANDEQ     0
114400950323     C     GCPDMM        ANDEQ     0
114500950329     C*
114600950329     C     D07TIP        IFEQ      *BLANKS
114700950329     C     GCPTCM        OREQ      D07TIP
114800961205     C*------------------------------------------------------*
114900961205     C*  Aggiorno giacenza se devo stampare le raccomandate  *
115000961205     C*  e sto lavorando come primo livello tranne le estere
115100960201     C     WT2F          IFEQ      'P'
115200961205     C     D07FIL        ANDEQ     SIMFEL
115300961205     C     EST           ANDNE     'E'
115400961205     C*------------------------------------------------------*
115500961205     C*  Aggiorno giacenza se devo stampare le raccomandate  *
115600961205     C*  delle filiali che gestisco in partenza se non mi    *
115700961205     C*  sono collegata come una secondo livello             *
115800961211     C     WT2F          OREQ      'P'
115900020730     C     SimTpP        ANDNE     '2'
116000961211     C     *IN18         ANDEQ     '1'
116100961205     C*------------------------------------------------------*
116200961205     C*  Aggiorno giacenza se devo stampare i fax e sto      *
116300961205     C*  lavorando come primo livello tranne i fax esteri    *
116400961203     C     WT2F          OREQ      'F'
116500020809     C     D07FIL        ANDEQ     SimFEl
116600961218     C     EST           ANDNE     'E'
116700961205     C*------------------------------------------------------*
116800961205     C*  Aggiorno giacenza se devo stampare i fax delle fil. *
116900961205     C*  che gestisco in partenza se non mi sono collegata   *
117000961205     C*  sono collegata come una secondo livello             *
117100961205     C     WT2F          OREQ      'F'
117200020730     C     SimTpP        ANDNE     '2'
117300961211     C     *IN18         ANDEQ     '1'
117400961205     C*------------------------------------------------------*
117500970210     C*  Se non devo stampare BOLLA aggiorno la giacenza
117600970210     C* se lnp è fra quelle che devo gestire e sono in iterr.
117700970210     C     WT2F          OREQ      'B'
117800950329     C     *IN18         ANDEQ     '1'
117900970206     C     D07TPL        ANDNE     'B'
118000970210     C*  Se non devo stampare nulla aggiorno la giacenza
118100970210     C*  se lnp è fra quelle che devo gestire
118200970210     C     WT2F          OREQ      ' '
118300970210     C     *IN18         ANDEQ     '1'
118400961205     C*------------------------------------------------------*
118500040707      * aggiorno se invio per e-mail
118600040629     C     WT2F          OREQ      'M'
118700950327     C                   EXSR      AGGGCP
118800950323     C* VERIFICO SE PER IL CLIENTE MITTENTE E' PREVISTO LO SCARICO
118900950323     C                   Z-ADD     49            LUNG
119000950323     C                   MOVEL     '3K'          COD
119100950323     C                   MOVEL     *BLANKS       KEY
119200950323     C                   MOVEL     GCPSCM        KEY
119300950323     C     KTAB          CHAIN     TABEL                              67
119400950323     C  N67              DO
119500950323     C                   MOVEL     TBLUNI        DS3K
119600950323     C     §3KGQI        IFEQ      'A'
119700950323     C     §3KGQI        OREQ      'E'
119800950323     C                   EXSR      SCAR
119900950323     C                   END
120000950329     C*
120100950329     C                   END
120200950323     C                   END
120300950323     C                   END
120400950327     C*
120500950327     C                   END
120600950323     C*
120700950323     C                   END
120800911018     C                   END
120900020826     c                   enddo
121000900423     C     FINE          TAG
121100931220     C*
121200940923     C     SAVFTP        IFEQ      'B'
121300931220     C  N80              EXSR      STBOL
121400931220     C                   Z-ADD     0             GIANGC
121500931220     C                   END
121600930727     C* STAMPO ULTIMO PIEDE DELLA PAGINA
121700931123     C     CONT          IFGT      50
121800970213     C     EST           IFNE      'E'
121900960202     C                   EXCEPT    RIGA1
122000961203     C* ESTERO
122100961203     C                   ELSE
122200961203     C     WT2F          IFEQ      'F'
122300961203     C                   ADD       1             STPPAG
122400961203     C                   WRITE     TESFAX
122500961203     C                   END
122600961203     C                   WRITE     TESGEN
122700961203     C                   END
122800961203     C*
122900930729     C                   END
123000961203     C*
123100940706     C     *IN87         IFEQ      '1'
123200940706     C     *IN49         ANDEQ     '0'
123300940706     C                   Z-ADD     PREKSC        KSC
123400961218     C                   Z-ADD     WSVAAS        WAAS
123500961218     C                   Z-ADD     WSVMGS        WMGS
123600961218     C                   MOVEL     WSVNZD        WNZD
123700961218     C                   MOVEL     WSVTSP        WTSP
123800961218     C                   Z-ADD     WSVLNP        WLNP
123900961218     C                   Z-ADD     WSVCTR        WCTR
124000961218     C                   MOVE      WSVLNA        WLNA
124100961203     C** NO ESTERO
124200961203     C     EST           IFNE      'E'
124300961203     C                   EXSR      CHKTFG
124400961203     C                   EXSR      CHKPOP
124500130319      *raccomandata 16 off
124600130319     c                   if        not *in16
124700130319     C                   EXCEPT    FINES
124800130412     c                   if        cont >= 55
124900130319     c                   except    testa
125000130319     c                   endif
125100130319     c                   except    fines2
125200130319     c                   else
125300130319      *mail
125400040708     C   16              EXCEPT    FINESm
125500130319     c                   endif
125600961203     C                   ELSE
125700961203     C** ESTERO
125800040906     C  n16              WRITE     FINAPE
125900040906     C   16              WRITE     finmail
126000961203     C                   WRITE     PIEDE
126100961203     C                   END
126200961203     C*
126300940706     C                   END
126400911031     C* STAMPA ULTIMA RACCOMANDATA O BOLLA
126500940923     C     SAVFRP        IFEQ      'S'
126600940927     C                   CLEAR                   FNLG73
126700940923     C                   MOVE      D07DAT        D73DTA
126800940923     C                   MOVE      D07DAT        D73DTD
126900940923     C                   MOVE      PREKSC        D73KSC
127000940923     C                   MOVE      KCI           D73KCC
127100940923     C                   MOVEL     FNLG73        KPJBU
127200940923     C                   MOVEL     'LG73'        KCOAZ
127300931123     C                   CALL      'BCH10'
127400931123     C                   PARM                    KPJBA
127500931217     C                   END
127600921016     C*
127700960507     C                   CLOSE     LETGIA
127800961203     C                   CLOSE     LETEST
127900960507     C* GUARDO CHE UTENTE HA FATTO LA RICHIESTA
128000960507     C                   SETOFF                                       60
128100960507     C                   MOVEL     *BLANKS       COMMAN
128200960507     C                   MOVEA     CMD6(1)       COMMAN           80
128300960507     C                   CALL      'QCMDEXC'                            60
128400960507     C                   PARM                    COMMAN
128500960507     C                   PARM                    LUNG             15 5
128600961203     C* GUARDO CHE UTENTE HA FATTO LA RICHIESTA
128700961203     C                   SETOFF                                       60
128800961203     C                   MOVEL     *BLANKS       COMMAN
128900961203     C                   MOVEA     CMD8(1)       COMMAN           80
129000961203     C                   CALL      'QCMDEXC'                            60
129100961203     C                   PARM                    COMMAN
129200961203     C                   PARM                    LUNG             15 5
129300160311     c
129400020222      *RICHIAMO PROGRAMMA PER CHIUSURA FILE
129500020222     C                   CLEAR                   TR27DS
129600020222     C                   MOVE      'C'           I27TLA
129700160311     C                   CALL      'TRUL27R1'
129800020222     C                   PARM                    TR27DS
129900160311     c*
130000160311     C                   CLEAR                   trulc7ds
130100160311     C                   MOVEL     'C'           Ic7TLA
130200160311     C                   CALL      'TRULC7R'
130300160311     C                   PARM                    TRULc7DS
130400160311     c
130500050630     C                   CLEAR                   fnlg09ds
130600050630     C                   MOVEL     'C'           d09TLA
130700050630     c                   movel     fnlg09ds      kpjbu
130800050630     C                   CALL      'FNLG09R'
130900050630     C                   PARM                    kpjba
131000050516     c                   movel     fnlg07        kpjbu
131100900423     C                   SETON                                          LR
131200961203     C*------------------------------------------------------*
131300961203     C*  DATMIT: Reperisco dati x stampa in inglese
131400961203     C*------------------------------------------------------*
131500961203     C     DATMIT        BEGSR
131600961203     C*
131700961203     C*  Dati anagrafici mittente
131800961203     C** SE IL CODICE è CODIFICATO PRENDO I DATI DELLA RAG.SOC.
131900961203     C** DAL PDC SE è CODICE 88 O 99 PRENDO I DATI O DALL'ANAGRAFICA
132000961203     C** D'APPOGGIO SE ESISTE O DALLO STORICO BOLLE
132100961203     C                   MOVEL     *BLANKS       STPRSM
132200961203     C                   MOVEL     *BLANKS       STPINM
132300961203     C                   MOVEL     *BLANKS       STPCAM
132400961203     C                   MOVEL     *BLANKS       STPCIM
132500961203     C                   MOVEL     *BLANKS       STPPRM
132600961203     C                   MOVEL     *BLANKS       STPNZM
132700961203     C     D43TRV        IFNE      'S'
132800961203     C*
132900961203     C     CODMIT        IFNE      8888
133000961203     C     CODMIT        ANDNE     9999
133100961203     C     KACO          CHAIN     CNACO000                           26
133200961203     C   26              EXCEPT    ERR
133300961203     C  N26              DO
133400961203     C                   MOVEL     ACORAG        STPRSM
133500961203     C                   MOVEL     ACORAG        STPPAT
133600961203     C                   MOVEL     INDVIA        STPINM
133700961203     C                   MOVEL     INDCIT        STPCIM
133800961203     C                   MOVEL     INDPRV        STPPRM
133900050223     C                   MOVEL     INDCAe        STPCAM
134000961203     C                   MOVEL     INDSTA        STPNZM
134100961203     C                   END
134200961203     C                   ELSE
134300050214     C     KGAP          CHAIN     tigap000                           70
134400961203     C     *IN70         IFEQ      '0'
134500961203     C                   MOVEL     GAPRSM        STPRSM
134600961203     C                   MOVEL     GAPRSM        STPPAT
134700961203     C                   MOVEL     GAPINM        STPINM
134800961203     C                   MOVEL     GAPLOM        STPCIM
134900961203     C                   MOVEL     GAPPRM        STPPRM
135000961203     C                   MOVEL     GAPCAM        STPCAM
135100961203     C                   MOVEL     GAPNZM        STPNZM
135200961203     C                   ELSE
135300961203     C                   MOVEL     BLPRSM        STPRSM
135400961203     C                   MOVEL     BLPRSM        STPPAT
135500961203     C                   MOVEL     BLPINM        STPINM
135600961203     C                   MOVEL     BLPLOM        STPCIM
135700961203     C                   MOVEL     BLPPRM        STPPRM
135800961203     C                   MOVEL     BLPCAM        STPCAM
135900961203     C                   MOVEL     BLPNZM        STPNZM
136000961203     C                   END
136100961203     C                   END
136200961203     C*  Controllo se devo reimpostare i dati dal luogo
136300961203     C                   ELSE
136400961203     C                   MOVEL     D43RAG        STPPAT
136500961203     C                   MOVEL     D43RAG        STPRSM
136600961203     C                   MOVEL     D43IND        STPINM
136700961203     C                   MOVEL     D43LOC        STPCIM
136800961203     C                   MOVEL     D43PRO        STPPRM
136900961203     C                   MOVEL     D43CAP        STPCAM
137000961203     C                   MOVEL     D43NAZ        STPNZM
137100961203     C                   END
137200961203     C*
137300961203     C                   ENDSR
137400961203     C*------------------------------------------------------*
137500961203     C*  DATASP: Reperisco dati x stampa in inglese
137600961203     C*------------------------------------------------------*
137700961203     C     DATASP        BEGSR
137800961203     C*
137900961203     C** RIBALTAMENTO DATA SPEDIZIONE
138000961203     C                   MOVEL     BLPAAS        G02INV
138100961203     C                   MOVE      BLPMGS        G02INV
138200961203     C                   Z-ADD     0             G02DAT
138300961203     C                   MOVEL     '3'           G02ERR
138400961203     C                   CALL      'XSRDA8'
138500961203     C                   PARM                    WLBDA8
138600961203     C                   Z-ADD     G02DAT        STPDAT            8 0
138700961203     C*  Imposto altri dati della spedizione
138800080220     C****               Z-ADD     BLPAAS        STPAA1
138900080220     C                   move      BLPAAS        STPAA1
139000961203     C                   Z-ADD     BLPLNP        STPLP1
139100961203     C                   Z-ADD     BLPNRS        STPNR1
139200961203     C                   Z-ADD     BLPNSP        STPNS1
139300961203     C                   Z-ADD     GCPFGC        STPFGC
139400961203     C                   Z-ADD     GCPNGC        STPNGC
139500961203     C                   MOVEL     GCPAGC        G02INV
139600961203     C                   MOVE      GCPMGC        G02INV
139700961203     C                   Z-ADD     0             G02DAT
139800961203     C                   MOVEL     '3'           G02ERR
139900961203     C                   CALL      'XSRDA8'
140000961203     C                   PARM                    WLBDA8
140100961203     C                   Z-ADD     G02DAT        STPDGC            8 0
140200961203     C*  Reperisco loro riferimento
140300060213     C                   MOVEL     'E'           ar4TRC
140400060213     C                   MOVEL     *BLANKS       ar4NOT
140500000121     C                   MOVE      BLPRMN        STPRMN
140600160524     C                   MOVE      BLPRMN        STPDPDRMN
140700160523     C                   SETOFF                                       88
140800090121      * aggiunto per esporlo a fianco a quello numerico vedi caso TOF
140900090121      * che aveva basato il riferimento partner sul numerico poi comunque
141000090121      * voleva il riferimento ALFA come tutti gli altri partner EDI.
141100090121      * .....aggiunto in stampa :
141200090121     C*******            MOVE      BLPRMA        STPRMA
141300090121     C*** Vogliamo il dato allineato a destra
141400160523      *
141500090121     C                   evalR     STPRMA = %TrimR(BLPRMA)
141600160525      **
141700160525      ** se DPD il Parcel è carico
141800160525     c                   if        parcel14 <> *blank
141900160525      * se DPD deve occultare il primo byte del rif.Numerico
142000160525     C                   SETON                                        88
142100160525     C                   MOVEL(p)  Parcel14      STPNT4
142200160525     c                   else
142300090121      *
142400160525      ** x DPD non aggancia ar4 (lo ha già fatto comunque FNLG24R)
142500160525      *  questo rimane solo per EEX
142600060213     C     Kar4          CHAIN     Fiar401L                           22
142700961211     C     *IN22         IFEQ      '1'
142800961211     C     BLPRMA        IFNE      *BLANKS
142900961211     C                   MOVEL     BLPRMA        STPNT4
143000961211     C                   ELSE
143100961211     C                   MOVEL     BLPRMN        STPNT4
143200961211     C                   END
143300961211     C*
143400961211     C                   ELSE
143500060213     C                   MOVEL     ar4NOT        STPNT4
143600961211     C                   END
143700160525      **
143800160525     c                   endIF
143900160525      **
144000961203     C*  Dati destinatario
144100110930     c                   z-add     blppkf        stppkg
144200110930     c                   z-add     blpncl        stpncl
144300961203     C     DN0TRV        IFNE      'S'
144400961203     C                   MOVEL     BLPLOD        STPCID
144500961203     C                   MOVEL     BLPPRD        STPPRD
144600961203     C                   MOVEL     BLPNZD        STPNZD
144700961203     C                   MOVEL     BLPRSD        STPRSD
144800961203     C                   MOVEL     BLPIND        STPIND
144900961203     C                   MOVEL     BLPCAD        STPCAD
145000961203     C                   ELSE
145100961203     C                   MOVEL     DN0LOD        STPCID
145200961203     C                   MOVEL     DN0PRD        STPPRD
145300961203     C                   MOVEL     DN0NZD        STPNZD
145400961203     C                   MOVEL     DN0RSD        STPRSD
145500961203     C                   MOVEL     DN0IND        STPIND
145600961203     C                   MOVEL     DN0CAD        STPCAD
145700961203     C                   END
145800961203     C*
145900961203     C                   MOVEL     *ALL'_'       CMP60
146000961203     C                   WRITE     PATSPE
146100961203     C                   WRITE     SOGSPE
146200961203     C                   WRITE     TESGIA
146300961203     C                   WRITE     INIAPE
146400961203     C                   MOVEL     WMOTEV        NOTE
146500971021     C                   MOVE      WMOTEV        WCOM25           25
146600961203     C                   WRITE     TESLIB
146700971016     C* SE MOTIVAZIONE + LUNGA DI 25 CHR SCRIVO ALTRA RIGA
146800971016     C     WCOM25        IFNE      *BLANKS
146900971104     C                   MOVEL     *BLANKS       NOTE
147000971104     C                   MOVEL     WCOM25        NOTE
147100971016     C                   WRITE     TESLIB
147200971016     C                   END
147300961203     C     ULTDMC        IFNE      *BLANKS
147400961203     C                   MOVEL     ULTDMC        NOTE
147500961203     C                   WRITE     TESLIB
147600961203     C                   END
147700961203     C*
147800961203     C                   ENDSR
147900961105     C*------------------------------------------------------*
148000961105     C*  DECOD. P.TO OPERAT. DA STAMPARE
148100961105     C*------------------------------------------------------*
148200961105     C     CHKPOP        BEGSR
148300961105     C                   CLEAR                   FAXPOP
148400961105     C                   CLEAR                   DESPOP
148500970115     C     WSVG2F        IFEQ      'S'
148600970115     C     GCPLNP        CHAIN     AZORG01L      UNIORG               23
148700970115     C                   ELSE
148800970115     C     PRELNP        CHAIN     AZORG01L      UNIORG               23
148900970115     C                   END
149000961105     C  N23              MOVEL     ORGFAX        FAXPOP           13
149100961105     C  N23              MOVEL     ORGDES        DESPOP           20
149200041111      *
149300041111      *----
149400041111     C  N23              do
149500041111     C                   MOVEL     ORGDE3        OG143
149600041111     c                   clear                   est_DesLnp
149700041111     c                   clear                   est_DesTel
149800041111     c                   clear                   est_DesFax
149900041111     C***
150000041111     C     §OgNTW        ifeq      'FED'
150100041111     C     §OgNTW        oreq      'EEX'
150200041111     C     §OgNTW        oreq      'EUP'
150300041111      *
150400041111     C     WSVG2F        ifEQ      'S'
150500041111     c                   move      gcpLNP        est_Lnp
150600041111     c                   else
150700041111     c                   move      preLNP        est_Lnp
150800041111     c                   end
150900041111      *
151000041111     C                   exsr      Hub_Estero
151100041111     c                   if        est_DesLnp <> *blank
151200041111     c                   movel     est_DesLnp    desPOP
151300041111     c                   movel     est_DesFax    faxPOP
151400041111     C                   end
151500041111     C                   end
151600041111     C                   endDo
151700041111      *----
151800041111      *
151900961105     C                   ENDSR
152000960201     C*------------------------------------------------------*
152100960201     C*  Posizionam. su anagrafica indirizzi x fax
152200960201     C*------------------------------------------------------*
152300960201     C     CHNIND        BEGSR
152400960306     C*
152401170925     c                   eval      tigcpb = tigcp
152500960201     C                   SETOFF                                       14
152600960205     C                   MOVEL     T2F           WT2F
152700170221 1   C     D07RIS        IFNE      'S'
152800960205     C                   MOVE      GCPSCM        CODMIT            4 0
152900170221 2   C     CODMIT        IFNE      8888
153000960201     C     CODMIT        ANDNE     9999
153100960201     C     KACO          CHAIN     CNIND000                           26
153200960201      *  Se invio tramite fax richiamo pgm x controllo validità
153300170221 3   C     T2F           IFEQ      'F'
153400960201     C                   CLEAR                   TRUL42
153500960306     C                   MOVEL     INDTLF        D42FAX
153600170221 4   C     D43TRV        IFEQ      'S'
153700960306     C                   MOVEL     D43FAX        D42FAX
153800170221 e4  C                   END
153900960201     C                   CALL      'TRUL42R'
154000960201     C                   PARM                    TRUL42
154100170221 4   C     D42ERR        IFEQ      '1'
154200960205     C                   MOVEL     DFT           WT2F              1
154300960205     C                   Z-ADD     1             XX
154400960205     C                   MOVEL     WT2F          T2F
154500960205     C                   MOVEL     *BLANKS       DFT
154600960205     C     UNI           LOOKUP    D2F(XX)                                32
154700170221 5   C     *IN32         IFEQ      '1'
154800960205     C                   MOVEL     C2F(XX)       WSVC2F            1
154900960205     C                   MOVEL     C2F(XX)       GCPTCM
155000960205     C                   EXCEPT    AGG1
155001170925     c                   exsr      RegVariazioni
155100050321      * verifica se dare commit
155200170221 6   c                   if        d07comm = 'S'
155300050321     c                   commit
155400170221e6   c                   endif
155500050321
155600170221 e5  C                   END
155700170221 x4  C                   ELSE
155800961128     C  N80              SETON                                        14
155900170221     C                   MOVEL     D42FAX        STPFAX           16
156000170221 e4  C                   END
156100170221 x3  C                   ELSE
156200960205     C     T2F           IFEQ      'F'
156300960205     C                   MOVEL     DFT           WT2F
156400960205     C                   END
156500170221 e3  C                   END
156600170221 x2  C                   ELSE
156700170221 3   C     T2F           IFEQ      'F'
156800960205     C                   MOVEL     DFT           WT2F
156900960205     C                   Z-ADD     1             XX
157000960205     C                   MOVEL     WT2F          T2F
157100960205     C                   MOVEL     *BLANKS       DFT
157200960205     C     UNI           LOOKUP    D2F(XX)                                32
1573001702214    C     *IN32         IFEQ      '1'
157400960205     C                   MOVEL     C2F(XX)       GCPTCM
157500960205     C                   MOVEL     C2F(XX)       WSVC2F            1
157600960205     C                   EXCEPT    AGG1
157601170925     c                   exsr      RegVariazioni
157700050321      * verifica se dare commit
157800050321     c                   if        d07comm = 'S'
157900050321     c                   commit
158000050321     c                   endif
158100050321
158200170221e4   C                   END
158300170221e3   C                   END
158400170221e2   C                   END
158500960202     C*
158600170221x1   C                   ELSE
158700960304     C                   MOVE      GCPSCM        CODMIT            4 0
158800960213     C     KACO          CHAIN     CNIND000                           26
158900960213     C     T2F           IFEQ      'F'
159000960213     C                   MOVEL     INDTLF        STPFAX
159100960306     C     D43TRV        IFEQ      'S'
159200960306     C                   MOVEL     D43FAX        STPFAX
159300960306     C                   END
159400961128     C  N80              SETON                                        14
159500960202     C                   END
159600170221e1   C                   END
159700960201     C*
159800960201     C                   ENDSR
159900900423     C******************************************************
160000930728     C     LUOGO         BEGSR
160100930729     C                   CLEAR                   SCH
160200930728     C     '  '          SCAN      DESLNP:1      H                 2 0
160300980127     C     H             IFEQ      0
160400980127     C                   Z-ADD     21            H
160500980127     C                   END
160600930728     C                   MOVEA     DESLNP        SCH
160700930728     C                   MOVEL     ','           SCH(H)
160800930728     C                   ADD       2             H
160900940923     C                   MOVEA     GX            SCH(H)
161000930728     C                   ADD       2             H
161100940923     C                   MOVEA     '/'           SCH(H)
161200930728     C                   ADD       1             H
161300940923     C                   MOVEA     MX            SCH(H)
161400930728     C                   ADD       2             H
161500930728     C                   MOVEL     '/'           SCH(H)
161600930728     C                   ADD       1             H
161700940923     C                   MOVEA     AX            SCH(H)
161800930728     C                   ENDSR
161900930728     C*
162000900423     C**----------------------
162100900423     C** STAMPA RACCOMANDATE
162200900423     C**----------------------
162300120113     C     STARAC        BEGSR
162400900423     C     CODMIT        CABEQ     8888          STAR
162500900423     C     CODMIT        CABEQ     9999          STAR
162600900423     C     STAR          TAG
162700120111     c                   exsr      ovrraccel
162800900423     C     CONRIG        IFGT      64
162900900423     C                   EXCEPT    TESRAC
163000900502     C                   Z-ADD     14            CONRIG
163100900423     C                   END
163200900423     C                   ADD       1             CONTA
163300040707     C
163400961211     C     EST           IFNE      'E'
163500900423     C                   EXCEPT    DETRAC
163600961211     C                   ELSE
163700961211     C                   EXCEPT    DETRAE
163800961211     C                   END
163900900502     C                   ADD       4             CONRIG
164000940202     C                   Z-ADD     GCPSCM        SAVKSC            7 0
164100911204     C*                    END
164200900423     C                   ENDSR
164300911021     C*
164400911021     C     STBOL         BEGSR
164500911021     C**--------------
164600911021     C** STAMPA BOLLA
164700911021     C**--------------
164800911021     C* PRELEVO I DATI DEL DESTINATARIO
164900921028     C* PREPARO DS X CREAZIONE BOLLA
165000990929     C                   CLEAR                   DBLP
165100990929     C                   CLEAR                   DTASV
165200941007     C                   MOVEL     'I'           §LPFPR
165300941011     C                   MOVEL     '£'           §LPSOP
165400960412     C                   MOVEL     PRELNP        §LPLNP
165500921028     C                   Z-ADD     AA            §LPAAS
165600921029     C                   Z-ADD     0             §LPMGS
165700921028     C                   MOVEL     MM            §LPMGS
165800921028     C                   MOVE      GG            §LPMGS
165900921028     C                   Z-ADD     0             §LPNRS
166000921029     C                   Z-ADD     0             §LPNSP
166100941007     C                   MOVEL     CODBOL        §LPCBO
166200921028     C                   MOVEL     *BLANKS       §LPFBR
166300960412     C                   MOVEL     PRELNP        §LPLNA
166400921028     C                   Z-ADD     0             §LPZNC
166500941011     C                   Z-ADD     8888          §LPCCN
166600941011     C                   MOVEL     §LPLNP        §LPCCN
166700941011     C                   Z-ADD     8888          §LPCCM
166800941011     C                   MOVEL     §LPLNP        §LPCCM
166900921029     C*
167000941007     C                   MOVEL     DESLNP        RAGIO            27
167100071018     C                   MOVEL     'Filiale '    §LPRSM
167200071018     C*****              MOVEL     'P.Oper. '    §LPRSM
167300921029     C                   MOVE      RAGIO         §LPRSM
167400921029     C*
167500931222     C                   MOVEL     INDLNP        §LPINM
167600941007     C                   MOVEL     CAPLNP        §LPCAM
167700931222     C                   MOVEL     LOCLNP        §LPLOM
167800931222     C                   MOVEL     PROLNP        §LPPRM
167900930208     C                   MOVEL     *BLANKS       §LPCPI
168000931217     C                   MOVEL     RAG           §LPRSD
168100941007     C                   MOVEL     *BLANKS       §LPRD2
168200931217     C                   MOVEL     VIA           §LPIND
168300940923     C                   MOVEL     CAP           §LPCAD
168400931217     C                   MOVEL     CIT           §LPLOD
168500931217     C                   MOVEL     PRV           §LPPRD
168600941221     C                   MOVEL     NZM           §LPNZD
168700921028     C                   MOVEL     *BLANKS       §LPGC1
168800921028     C                   MOVEL     *BLANKS       §LPGC2
168900921028     C                   Z-ADD     0             §LPCTR
169000921028     C                   MOVEL     *BLANKS       §LPCTS
169100921028     C                   MOVEL     *BLANKS       §LPFTM
169200921028     C                   MOVEL     *BLANKS       §LPTSP
169300941007     C                   MOVEL     *BLANKS       §LPVAS
169400941007     C                   MOVEL     *BLANKS       §LPVAD
169500941007     C                   Z-ADD     0             §LPVMD
169600941007     C                   Z-ADD     0             §LPIAS
169700921028     C                   MOVEL     'BUSTA'       §LPNAS
169800921028     C                   Z-ADD     1             §LPNCL
169900941007     C                   Z-ADD     0,1           §LPPKB
170000941007     C                   Z-ADD     0             §LPVLB
170100921028     C                   Z-ADD     0             §LPPOR
170200941007     C                   Z-ADD     0             §LPCAS
170300941007     C                   Z-ADD     0             §LPCMB
170400941007     C                   MOVEL     *BLANKS       §LPVCA
170500941007     C                   MOVEL     *BLANKS       §LPTIC
170600941007     C                   MOVEL     'P'           §LPFPP
170700921028     C                   Z-ADD     0             §LPIMV
170800921028     C                   MOVEL     *BLANKS       §LPCEI
170900921028     C                   Z-ADD     0             §LPDFT
171000921028     C                   Z-ADD     0             §LPNFT
171100921028     C                   Z-ADD     0             §LPFIV
171200921028     C                   Z-ADD     1             §LPRMN
171300941007     C                   MOVEL     *BLANKS       §LPRMA
171400921028     C                   MOVEL     *BLANKS       §LPRMO
171500921028     C                   Z-ADD     0             §LPNCD
171600921028     C                   Z-ADD     0             §LPNCA
171700921028     C                   MOVEL     *BLANKS       §LPXCO
171800921029     C*
171900921029     C     SAVFRP        IFEQ      'N'
172000921028     C                   MOVEL     'NR.'         RIGA              7
172100921028     C                   MOVE      GIANGC        RIGA
172200921028     C                   MOVE      RIG(1)        §LPNOT
172300921029     C                   MOVEL     RIGA          §LPNOT
172400921029     C                   ELSE
172500921029     C                   MOVEL     *BLANKS       §LPNOT
172600921029     C                   END
172700921029     C     SAVFRP        IFEQ      'S'
172800921029     C                   MOVEL     RIG(2)        §LPNOT
172900921029     C                   END
173000941007     C                   MOVEL     *BLANKS       §LPNT2
173100921029     C*
173200921029     C                   MOVEL     '4 '          §LPCTM
173300921028     C                   MOVEL     *BLANKS       §LPFFD
173400941007     C                   Z-ADD     999           §LPPDR
173500941011     C                   MOVEL     §LPLNP        §LPPDR
173600940922     C                   Z-ADD     D07DAT        §LPDRT
173700941007     C                   Z-ADD     0             §LPDCR
173800941007     C                   Z-ADD     0             §LPHCR
173900921028     C                   MOVEL     *BLANKS       §LPTCR
174000970327     C                   MOVEL     'S'           §LPBAM
174100970407     C                   MOVEL     'G'           §LPRIC
174200990929     C* IMPOSTO NUOVI CAMPI DBLP
174300990929     C                   MOVEL     *BLANKS       §LPFLG
174400990929     C                   MOVEL     *ZEROS        §LPLPA
174500990929     C                   MOVEL     *BLANKS       §LPFLO
174600990929     C                   MOVEL     *BLANKS       §LPDIV
174700921029     C*
174800941221     C* FORZO IL PGM STAMPA SEGNACOLLI IN KPJBA
174900941010     C                   CALL      'FNLS01R'
175000921028     C                   PARM                    KPJBA
175100990929     C                   PARM                    DBLP
175200990929     C                   PARM                    DTASV
175300030717     C                   PARM                    TRUL90DS
175400941010      *
175500960322      * RICH. PGM X SCRITTURA BOLLA IN ARRIVO solo se sped.locale
175600960322     C     §LPLNP        IFEQ      §LPLNA
175700941010     C                   CLEAR                   DSUL41
175800941010     C                   MOVEL     §LPAAS        D41AAS
175900941010     C                   MOVEL     §LPLNP        D41LNP
176000941010     C                   MOVEL     §LPNRS        D41NRS
176100941010     C                   MOVEL     §LPNSP        D41NSP
176200941010     C                   CALL      'TRUL41R'
176300941010     C                   PARM                    DSUL41
176400960322     C                   END
176500921029     C*
176600911021     C                   ENDSR
176700900423     C******************************************************
176800900423     C     AGGGCP        BEGSR
176900900423     C**----------------------
177000900423     C** AGGIORNAMENTO GIACENZE
177100900423     C**----------------------
177101170925     c                   eval      tigcpb = tigcp
177200960306     C     D43TRV        IFEQ      'S'
177300960306     C                   MOVEL     'S'           GCPFST
177400960306     C                   END
177500040119     c                   clear                   ds2z
177600040119      * verifico se cliente giacenza è un 8888 controllo se il motivo è
177700040119      * uno di quelli con data richiesta consegna se è così forzo la fase
177800040119      * e abblenco la data disposizioni
177900040830     c                   if        quatr =  8888 or
178000040830     c                             quatr =  9999
178100040119     C                   MOVEL     '2Z'          COD
178200040119     C                   MOVEL     *BLANKS       KEY
178300040119     C                   MOVEL     GCPCMC        KEY
178400040119     c     ktab          chain     tabel00f
178500041019     c                   if        %found(tabel00f)
178600040119     c                   movel     tbluni        ds2z
178700040119     c                   endif
178800040119     c                   endif
178900050412
179000040119     c                   if        §2zdcr = 'S'
179100040119     C                   Z-ADD     0             GCPDdM
179200040119     C                   Z-ADD     25            GCPFAS
179300040119     c                   else
179400050412     c                   if        gcpdis <> *blank and gcpdis <> 'R'
179500050412     c                   z-add     30            gcpfas
179600050412     c                   else
179700900423     C                   Z-ADD     20            GCPFAS
179800170117     c*
179900050412     c                   endif
180000040119     c                   endif
180100050412
180200040119     C                   Z-ADD     D07DAT        GCPDMM
180300040119
180400960202     C                   EXCEPT    AGG0
180401170925     c                   exsr      RegVariazioni
180500170221      * verifica se inviare avviso al destinatario
180600170221      * solo se prima apertura e filiale italia
180700050321     c                   if        d07comm = 'S'
180800050321     c                   commit
180900050321     c                   endif
181000170221     c                   if        gcpriap = *blank
181100170221     c                   exsr      AvvisaDest
181200170221     c                   endif
181300050321
181400900423     C                   ENDSR
181401170925     C*__________________________________________________________
181402170925     c     RegVariazioni begsr
181403170925     C*__________________________________________________________
181404170925     c                   eval      ILG33CTA = 'M'
181405170925     c                   eval      ILG33PRU = knmus
181406170925     c                   eval      ILG33NOJ = knmeb
181407170925     c                   eval      ILG33PGM = 'FNLG07R'
181408170925     c                   eval      tigcpa = tigcp
181409170925     c                   call      'FNLG33R'
181410170925     c                   parm                    fnlg33ds
181411170925     c                   parm                    tigcpb
181412170925     c                   parm                    tigcpa
181413170925     c                   endsr
181500040119     C******************************************************
181600921013     C     SCAR          BEGSR
181700040119     C**----------------------
181800050607     c                   clear                   fnlg08ds
181900050607     c                   eval      D08AGC= gcpagc
182000050607     c                   eval      D08FGC= gcpfgc
182100050607     c                   eval      D08NGC= gcpngc
182200050607     c                   eval      D08uni= §3kgks
182300050607     c                   eval      D08GSD= §3kGSD
182400050607     c                   eval      D08est= EST
182500050608     c                   eval      D08pgm = 'FNLG07R'
182600050607     c                   eval      D08commit= 'S'
182700050607     c                   movel(p)  fnlg08ds      kpjbu
182800050607     c                   call      'FNLG08R'                            28
182900050607     c                   parm                    kpjba
183000050607     c                   movel     kpjbu         fnlg08ds
183100921013     C*
183200921013     C                   ENDSR
183300940627      *--------------------------------------
183400940627     C*- CERCO I GIORNI DI RIENTRO SU TARIFFA
183500940627      *--------------------------------------
183600940627     C     CHKTFG        BEGSR
183700940627     C                   Z-ADD     0             §2GGGL
183800940923     C                   MOVE      KSC           KKSC
183900961218     C                   MOVE      WCTR          KCTR
184000961218     C                   MOVEL     WAAS          DATDSP            8 0
184100961218     C                   MOVE      WMGS          DATDSP
184200950113     C                   MOVEL     ' '           WTROVA            1
184300950116     C                   MOVE      KKSC          §QUATR            4 0
184400020222      *RICHIAMO PROGRAMMA PER REPERIMENTO INFORMAZIONI TARIFFA
184500020222     C                   CLEAR                   TR27DS
184600160311     C                   CLEAR                   fnlv59ds
184700021118     C                   MOVE      Wctr          I27ctb
184800020222     C                   MOVE      WTSP          I27TSP
184900020222     C                   MOVE      WLNA          I27LNA
185000020222     C                   MOVE      WLNP          I27LNP
185100020222     C                   MOVE      KKSC          I27CLI
185200160311     C                   CALL      'TRUL27R1'
185300020222     C                   PARM                    TR27DS
185400950113     C     §QUATR        IFNE      *ALL'8'
185500950113     C     §QUATR        ANDNE     *ALL'9'
185600950113     C     §QUATR        ANDNE     *ZEROS
185700150514     C                   MOVEL     ' '           ilv59tbo
185800150514     C                   Z-ADD     DATDSP        ilv59dsp
185900150514     C                   MOVE      WCTR          ilv59CTR
186000150514     C                   MOVE      KKSC          ilv59KSC
186100150514     C                   MOVE      KCI           ilv59kci
186200000202     C                   MOVEL     KKSC          W0030             3 0
186300150514     C                   MOVEL     O27FIE        ilv59FIE
186400150514     C                   MOVEL     WTSP          ilv59tsp
186500150514     C                   MOVEL     'S'           ilv59CON
186600150514     c                   eval      ilv59ta2='B'
186700150514     c                   eval      ilv59blo='S'
186800150514     C                   CALL      'FNLV59R'
186900150514     C                   PARM                    fnlv59ds
187000950113     C*
187100150514     C     olv59ERR      IFEQ      *BLANKS
187200150514     C     olv59tks      ANDEQ     'S'
187300950113     C*  Se ho trovato tariffa in vigore mi posiziono sul file
187400950113     C*  delle tariffe giacenze con il proggressivo giusto
187500150514     C                   MOVE      olv59ctr      KCTR
187600150514     C                   MOVE      olv59prg      KPRG
187700150514     C                   MOVE      ilv59ksc      KKSC
187800990730     C     KTGC          CHAIN     TITGC01L                           31
187900950113     C     *IN31         IFEQ      '0'
188000950116     C     TGCGGR        ANDNE     *ZEROS
188100950113     C                   MOVE      TGCGGR        §2GGGL
188200950113     C                   MOVEL     'S'           WTROVA
188300950113     C                   END
188400950113     C                   END
188500950113     C                   END
188600950113     C*  Se non trovato gg. rientro su tariffa cliente li cerco
188700950113     C*  sulla tariffa di cartello
188800950113     C     WTROVA        IFNE      'S'
188900160311     c* Carico la cartello :
189000160311     c*  - per 8888 col fie della bolla
189100160311     c*  - per tariffe cliente esistente col fie della tariffa
189200160311     c                   clear                   trulc7ds
189300160311    2C     olv59ERR      ifeq      *blanks
189400160311     C     olv59tks      andeq     'S'
189500160311     c                   z-add     1             indx              3 0
189600160311     c                   eval      indx=%lookup(ilv59ctr:ks_ct)
189700160311    3c                   if        indx>0 and %editc(olv59prg:'X')=
189800160311     c                             ks_pr(indx)
189900160311     c                   eval      ic7tfie=ks_ie(indx)
190000160311   x3c                   else
190100160311     c                   eval      ic7tntw=o27ntw
190200160311    3c                   endif
190300160311     c
190400160311   x2c                   else
190500160311     c                   eval      ic7tntw=o27ntw
190600160311    2c                   endif
190700160311     c
190800160311     C                   moveL     datdsp        Ic7dta
190900160311     C                   move      wctr          Ic7ctb
191000160311     c                   call      'TRULC7R'
191100160311     c                   parm                    trulc7ds
191200160311     c
191300160311     C                   MOVE      Oc7CTRc       KCTR
191400160311     C                   MOVE      Oc7PRGc       KPRG
191500160311     C                   MOVE      Oc7KSCc       KKSC
191600990730     C     KTGC          CHAIN     TITGC01L                           31
191700950113     C     *IN31         IFEQ      '0'
191800950116     C     TGCGGR        ANDNE     *ZEROS
191900950113     C                   MOVE      TGCGGR        §2GGGL
192000950113     C                   MOVEL     'S'           WTROVA
192100950113     C                   END
192200950113     C                   END
192300021014     c                   if        wtrova <> 'S' or
192400021014     c                             o27ntw = 'DPD' or
192500021014     c                             o27ntw = 'FED' or
192600021014     c                             o27ntw = 'EEX' or
192700021014     c                             o27ntw = 'EUP'
192800021014
192900020222     C                   MOVEL     '2G'          COD
193000020222     C                   MOVEL     *BLANKS       KEY
193100020222     C                   MOVEL     1             KEY
193200961204     C                   Z-ADD     §2GGGL        WGGR              3 0
193300961204     C     KTAB          CHAIN     TABEL                              70
193400961204     C  N70              MOVEL     TBLUNI        DS2G
193500010921      * Se è da DPD
193600020222     C                   SELECT
193700020222      *
193800020222     C     O27NTW        WHENEQ    'DPD'
193900010921     C     §2GGDP        IFGT      WGGR
194000010925     C                   Z-ADD     WGGR          §2GGGL
194100010921     C                   ELSE
194200010921     C                   Z-ADD     §2GGDP        §2GGGL
194300010921     C                   END
194400020222      *
194500020222     C     O27NTW        WHENEQ    'FED'
194600160523     C     §2GGFE        IFGT      WGGR
194700020222     C                   Z-ADD     WGGR          §2GGGL
194800020222     C                   ELSE
194900160523     C                   Z-ADD     §2GGFE        §2GGGL
195000020222     C                   END
195100010921      * Se è un estero
195200020222     C     O27NTW        WHENEQ    'EEX'
195300020222     C     O27NTW        OREQ      'EUP'
195400961204     C     §2GGRE        IFGT      WGGR
195500961204     C                   Z-ADD     WGGR          §2GGGL
195600961204     C                   ELSE
195700961204     C                   Z-ADD     §2GGRE        §2GGGL
195800961204     C                   END
195900020222     C                   ENDSL
196000021014     c                   endif
196100961204     C*
196200160523     C* Con la tabella "20"
196300050210     C*  Se ci sono eccezioni ai resi x decorsi termini sul cliente
196400050210     C*   sui giorni da prendere
196500050210      * Se è un estero
196600050210     C     O27fie        ifne      'I'
196700050210     C     O27NTW        andeq     'EEX'
196800050210     C                   MOVEL     '20'          COD
196900050210     C                   MOVEL     *blank        KEY
197000050210     C                   MOVEL     Kksc          KEY
197100050210     c     ktab          chain     tabel00f
197200050210     c                   if        %found(tabel00f)
197300050210     c                   movel     tbluni        ds20
197400050210     C                   Z-ADD     §20GRE        §2GGGL
197500050210     c                   endif
197600050210     c                   end
197700040112     C*
197800940627     C                   ENDSR
197900040701     C*------------------------------------------------------*
198000040701     C*  reperisce dati necessari all'invio tramite mail
198100040701     C*------------------------------------------------------*
198200040701     C     invxmail      begsr
198300040701      * verifico se l'invio è tramite e-mail reperisco i dati necessari
198400040701      * se esiste il luogo l'indirizzo e-mail deve essere obbligatoriamente
198500040701      * associato al luogo altrimenti è un errore
198600040701      * *in16=*on mail
198700040701     c                   seton                                        16
198800040707     c                   clear                   trtcm1ds
198900040707     c                   if        d43trv = 'S'
199000040707     c                   if        d43trvm = 'S'
199100040707     c                   movel     d43mail       §cm1dst
199200040707     c                   end
199300040701      * se non esiste il luogo reperisco e-mail da TFNTC nota '88'
199400040707     c                   else
199500040707     c                   move      gcpscm        kcnk1
199600040707     c     kntc          chain     tfntc01l
199700041019     c                   if        %found(tfntc01l)
199800040707     c                   movel     ntcrnt        §cm1dst
199900040707     c                   end
200000040707     c                   end
200100040712     c                   move      gcplnp        §cm1po
200200040929     c                   move      '0'           §cm1sts
200300040914     c                   movel     §mrareg       §cm1tips
200400040914     c                   movel     §mraidpro     §cm1idp
200500040712     c                   movel     gcpagc        data8
200600040712     c                   move      gcpmgc        data8
200700040712     c                   move      data8         dataiso
200800040712     c                   move      dataiso       dataeur
200900040712     c                   move      dataeur       data8
201000041014     c                   movel     blprsd        rsd20            20
201100041022     c     chkc:chkcs    xlate     rsd20         rsd20
201200040721     c                   move      gcpfgc        fgcalfa           3
201300160523      ***
201400160523     c                   if        Parcel14 <> *blank
201500160523     c                   eval      §cm1var = '*OBJM*' + parcel14 + '-'
201600160523     c                   else
201700160523     c                   eval      §cm1var = '*OBJM*'
201800160523     c                   end
201900160523      ***
202000160523     c                   eval      §cm1var = %trim(§cm1var) +
202100160523     c                             'Giacenza Nr. ' + fgcalfa + '-' +
202200041014     c                             %trim(%editc(gcpngc:'Z')) + ' ' +
202300041005     c                             %editw(data8:'  -  -    ') +
202400041014     c                             ' Cli: '+  %editc(gcpscm:'Z') + ' ' +
202500041014     c                             rsd20 + '-' + blpprd
202600061214     C* Imposto i parametri x il *pgm d "bonifica"
202700061214     C                   EVAL      ElencoChar = *blanks
202800061214     C                   EVAL      TipoElenco = *blanks
202900061214     C                   EVAL      CharSost   = *blanks
203000061214     C                   EVAL      UpperCase  = ' '
203100061214     C                   EVAL      ChkNull    = '1'
203200061214     C                   EVAL      CharNull   = *blanks
203300061214     C*
203400061214     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
203500061214     C                   EVAL      TxtInOut   = §cm1var
203600061214     C                   EVAL      EsitoBon   = *blanks
203700061214     C*
203800061214     C                   CALL      'XCHKCHAR'
203900061214     C                   PARM                    TxtInOut
204000061214     C                   PARM                    ElencoChar
204100061214     C                   PARM                    TipoElenco
204200061214     C                   PARM                    CharSost
204300061214     C                   PARM                    UpperCase
204400061214     C                   PARM                    ChkNull
204500061214     C                   PARM                    CharNull
204600061214     C                   PARM                    EsitoBon
204700061214     C*
204800061214     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
204900061214     C                   IF        EsitoBon = '1'
205000061214     C                   EVAL      §cm1var = TxtInOut
205100061214     C                   ENDIF
205200040929
205300040929      * richiamo per reperimento mittente della mail
205400040929     c                   exsr      repmitt
205500040929
205600040707      *non trovato indirizzo mail destinatario invio la mail allo stesso
205700040707      * utente che sta inviando
205800040707     c                   if        §cm1dst = *blank
205900040914     c                   eval      §cm1dst = %trim(§cm1mitt)  + mitterr
206000040707     c                   endif
206100040701     c
206200040707     C                   endsr
206300170116     C*------------------------------------------------------*
206400170116     C*  reperisce dati necessari all'invio avviso destinatario
206500170116     C*------------------------------------------------------*
206600170116     C     AvvisaDest    begsr
206700170116     c                   clear                   fnlg071ds
206800170207     c                   movel     gcpAAS        i071AAS
206900170207     c                   movel     gcpLNP        i071LNP
207000170207     c                   movel     gcpNRS        i071NRS
207100170207     c                   move      gcpNSP        i071NSP
207200170207     c                   movel     gcpLNa        i071LNa
207300170207     c                   move      'A'           i071CHI
207400170207     c                   movel     gcpcmc        i071cmc
207500170116     c                   call      'FNLG07R1'
207600170207     c                   parm                    kpjba
207700170116     c                   parm                    fnlg071ds
207800170116      *
207900170116     C                   endsr
208000040629     C*------------------------------------------------------*
208100040629     C*  Prepara ovrprtf in caso di invio via e-mail
208200040629     C*------------------------------------------------------*
208300040629     C     preparaovr    begsr
208400040629     c                   close     letgia
208500040629     C*  Ovrprtf  LETgia
208600040630     c                   eval      commanl = %trim(Cmd9(1))+ trtcm1ds + ''')'
208700040630     c                   eval      lung = %len(%trim(commanl))
208800040629     C                   CALL      'QCMDEXC'                            60
208900040630     C                   PARM                    COMMANl         500
209000040629     C                   PARM                    LUNG             15 5
209100040629     C                   OPEN      LETGIA
209200040629     C                   endsr
209300040906     C*------------------------------------------------------*
209400040906     C*  Prepara ovrprtf in caso di invio via e-mail ESTERA
209500040906     C*------------------------------------------------------*
209600040906     C     preparaovre   begsr
209700040906     c                   close     letest
209800040906     C*  Ovrprtf  LETest
209900040906     c                   eval      commanl = %trim(Cmd9(2))+ trtcm1ds + ''')'
210000040906     c                   eval      lung = %len(%trim(commanl))
210100040906     C                   CALL      'QCMDEXC'                            60
210200040906     C                   PARM                    COMMANl         500
210300040906     C                   PARM                    LUNG             15 5
210400040906     C                   OPEN      LETest
210500040906      *prepara link in una stringa per printer file
210600040906     c                   exsr      strlink
210700040906     C                   endsr
210800120111     C*------------------------------------------------------*
210900120111     C*  Prepara ovrprtf per raccomandate
211000120111     C*------------------------------------------------------*
211100120111     C     ovrracc       begsr
211200120111     c                   close     letgia
211300120111     C*  Ovrprtf  LETgia
211400120111     c                   eval      commanl = Cmd1(1)
211500120111     c                   eval      lung = %len(%trim(commanl))
211600120111     C                   CALL      'QCMDEXC'                            60
211700120111     C                   PARM                    COMMANl         500
211800120111     C                   PARM                    LUNG             15 5
211900120111     C                   OPEN      LETGIA
212000120111     C                   endsr
212100120111     C*------------------------------------------------------*
212200120111     C*  Prepara ovrprtf per raccomandate elenco
212300120111     C*------------------------------------------------------*
212400120111     C     ovrraccel     begsr
212500120111     c                   close     let132
212600120111     C*  Ovrprtf  LETgia
212700120111     c                   eval      commanl = Cmd2(1)
212800120111     c                   eval      lung = %len(%trim(commanl))
212900120111     C                   CALL      'QCMDEXC'                            60
213000120111     C                   PARM                    COMMANl         500
213100120111     C                   PARM                    LUNG             15 5
213200120111     C                   OPEN      LET132
213300120111     C                   endsr
213400040906     C*------------------------------------------------------*
213500040906     C*  Prepara stringa link per printer file       ESTERA
213600040906     C*------------------------------------------------------*
213700040906     C     strlink       begsr
213800040910     c                   z-add     §2gggl        stgggl
213900040906     c                   clear                   linkst
214000040907     c                   eval      linkst = 'B#?nspediz=' +
214100040906     c                             %trim(%editc(gcplnp:'X')) +
214200040906     c                             %trim(%editc(gcpnrs:'X')) +
214300040906     c                             %trim(%editc(gcpnsp:'X')) + '&keygiac=' +
214400040906     c                             %trim(%editc(gcpaas:'X')) +
214500040906     c                             %trim(%editc(gcplnp:'X')) +
214600040906     c                             %trim(%editc(gcpnrs:'X')) +
214700040906     c                             %trim(%editc(gcpnsp:'X')) +
214800040906     c                             %trim(%editc(blpaas:'X')) +
214900040906     c                             %trim(%editc(blpmgs:'X')) +
215000040906     c                             %trim(%editc(gcpagc:'X')) +
215100040906     c                             %trim(%editc(gcpfgc:'X')) +
215200040906     c                             %trim(%editc(gcpngc:'X')) +
215300040906     c                             %trim(%editc(gcpagc:'X')) +
215400040907     c                             %trim(%editc(gcpmgc:'X')) + '#E'
215500040906     C                   endsr
215600040929     C*------------------------------------------------------*
215700040929     C*  Prepara stringa mittente per invio mail o riferimento per FAX
215800040929     C*------------------------------------------------------*
215900040929     C     repmitt       begsr
216000040929      * P.O. primo o secondo livello
216100040929     c                   movel     orgde8        fillevel          1
216200040929     c                   move      gcplnp        lnpalfa           3
216300040929      * campi di comodo per stampa fine fax
216400040929     c                   move      gcpscm        gcpscms           7
216500041020      * non deve stampare il piede del fax per clienti non codificati
216600041020     c                   setoff                                       17
216700041020     c                   if        %subst(gcpscms:4:4) = '8888' or
216800041020     c                             %subst(gcpscms:4:4) = '9999'
216900041020     c                   seton                                        17
217000041020     c                   endif
217100040929     c                   clear                   mittfax          21
217200040929      * gestione network x sapere come impostare il mittente
217300040929     c                   select
217400040929     c                   when      §ogntw = 'DPD' or §ogntw = 'EEX' or
217500040929     c                             §ogntw = 'FED' or §ogntw = 'EUP'
217600040929     c                   clear                   fnlv55ds
217700040929     c* Imposto i dati in LV55 x reperire terminal partenza fil. estera
217800040929     c                   movel     'P'           d55tpt
217900040929     c                   movel     amgdtu        d55drf
218000040929     c                   movel     gcplnp        d55lin
218100040929     c                   move      gcplnp        d55lnp
218200040929     c                   call      'FNLV55R'
218300040929     c                   parm                    fnlv55ds
218400040929      * P.O. estero va inviato come 'INTyyy' yyy=terminal partenza
218500040929     c                   move      d55tfp        terminalp         3
218600040929     c                   eval      §cm1mitt = %trim(§mramites) + terminalp
218700040929
218800040929     c                   other
218900040929      * se network italia trasmette come INFO +xxx=linea di partenza
219000040929     c                   if        fillevel = '1'
219100040929     c                   eval      §cm1mitt = %trim(§mramit1l) + lnpalfa
219200040929     c                   else
219300040929     c                   eval      §cm1mitt = %trim(§mramit2l) + lnpalfa
219400040929     c                   endif
219500040929     c                   endsl
219600040929     c                   eval      Mittfax = %trim(§cm1mitt)  + mitterr
219700040929     C                   endsr
219800931217     C******************************************************
219900931217     C     *INZSR        BEGSR
220000931217     C*
220100931217     C     *ENTRY        PLIST
220200931217     C                   PARM                    KPJBA
220300931217     C                   Z-ADD     1             CODUT
220400931217     C                   CALL      'X§PARUT'
220500931217     C                   PARM                    UTEDSE
220600931217     C     CODUT         CABEQ     -1            FINE
220700971112     C                   MOVEL     RAGUT         RSUT             30
220800931217     C*--- RICERCA CAPOCONTI------------------------------------------*
220900931217     C                   DO        50            X                 2 0
221000931217     C                   MOVE      TCU(X)        TCUDS
221100940922     C     F56           IFEQ      'CG'
221200940922     C     F4            ANDEQ     '1'
221300940922     C     F3            ANDEQ     '0'
221400940922     C                   Z-ADD     KCU(X)        KCI               4 0
221500940922     C                   END
221600931217     C                   END
221700931217     C*---------------------------------------------------------------*
221800931217     C                   MOVEL     REC80         CNCR80
221900931217     C                   MOVEL     REC42         CNCR42
222000120210     C                   MOVEL     simrgs        srsut            16
222100110606     C                   MOVEL     simrgs        STPRUT
222200931217     C** TABEL
222300931217     C     KTAB          KLIST
222400931217     C                   KFLD                    CODUT
222500931217     C                   KFLD                    COD               2
222600931217     C                   KFLD                    KEY               8
222700931217     C*
222800931217     C     KTAB1         KLIST
222900931217     C                   KFLD                    CODUT
223000931217     C                   KFLD                    COD
223100931217     C* CNACO
223200931217     C     KACO          KLIST
223300931217     C                   KFLD                    CODUT
223400931217     C                   KFLD                    KCI
223500931217     C                   KFLD                    GCPSCM
223600931217     C* FLBLP
223700931217     C     KGAP          KLIST
223800931217     C                   KFLD                    GCPAAS
223900931217     C                   KFLD                    GCPLNP
224000931217     C                   KFLD                    GCPNRS
224100931217     C                   KFLD                    GCPNSP
224200060213     C     Kar4          KLIST
224300940923     C                   KFLD                    GCPAAS
224400940923     C                   KFLD                    GCPLNP
224500940923     C                   KFLD                    GCPNRS
224600940923     C                   KFLD                    GCPNSP
224700060213     C                   KFLD                    ar4TRC
224800931217     C*
224900931217     C     KGIA          KLIST
225000940922     C                   KFLD                    D07AGC
225100940922     C                   KFLD                    D07FGC
225200940922     C                   KFLD                    D07NGC
225300990625     C                   KFLD                    PRG               2 0
225400931217     C                   Z-ADD     0             PRG
225500931217     C*
225600931217     C     KNOT          KLIST
225700931217     C                   KFLD                    GCPAGC
225800931217     C                   KFLD                    GCPFGC
225900931217     C                   KFLD                    GCPNGC
226000931217     C                   KFLD                    GCPFRG
226100931217     C                   KFLD                    FASE              3 0
226200931217     C                   KFLD                    NPR               3 0
226300940923     C*
226400940923     C     KTGC          KLIST
226500940923     C                   KFLD                    KKSC
226600940923     C                   KFLD                    KCTR
226700940923     C                   KFLD                    KPRG
226800050214     C     Kgia21        KLIST
226900050214     C                   KFLD                    GCPAASa
227000050214     C                   KFLD                    GCPLNPa
227100050214     C                   KFLD                    GCPNRSa
227200050214     C                   KFLD                    GCPNSPa
227300040629     C     Kntc          KLIST
227400040629     C                   KFLD                    kcapl
227500040629     C                   KFLD                    kcnk1
227600040629     C                   KFLD                    kcnk2
227700040629     C                   KFLD                    kctnt
227800040629     c                   move      'C'           kcapl
227900040629     c                   movel     '0151'        kcnk1
228000040629     c                   move      '88'          kctnt
228100040630     C     ktbe          klist
228200040630     C                   kfld                    tbecod
228300040630     C                   kfld                    tbeke1
228400040630     C                   kfld                    tbeke2
228500040708     c*inizializza costanti per invio mail
228600040708     c                   eval      cost1 = 'Vi informiamo inoltre che:'
228700040708     c                   eval      cost2 = '- la merce e''custodita presso la -
228800040708     c                             Ns. di:'
228900040708     c                   eval      cost3 = '- sino alla ricezione di Vs. istruz-
229000040708     c                             ioni saremo tenuti ad aderire alla richiesta-
229100040721     c                              di riconsegna da parte'
229200040721     c                   eval      cost4 = '  del destinatario'
229300040721     c                   eval      cost5 = '- trascorsi    giorni dalla present-
229400040721     c                             e senza che ci abbiate fornito istruzioni co-
229500040721     c                             ncrete, complete e'
229600040721     c                   eval      cost6 = '  attuabili, Vi ritorneremo la merc-
229700040721     c                             e a Vs. rischio e con addebito di ogni spesa-
229800040721     c                             . Trascorsi ulteriori 30'
229900040721     c                   eval      cost7 = '  giorni senza che la giacenza sia -
230000040721     c                             risolta provvederemo al recupero dei ns. cre-
230100040721     c                             diti mediante richiesta'
230200040721     c                   eval      cost8 = '  al giudice dell''esecuzione di ve-
230300040721     c                             ndita o assegnazione della merce ai sensi de-
230400040721     c                             gli art. 2756, 2761,'
230500040721     c                   eval      cost9 = '  2797 e 2798 del C.C. salvo e impr-
230600040721     c                             egiudicato ogni altro diritto per spese e co-
230700040721     c                             mpetenze non recuperate.'
230800040721     c                   eval      cost10= '- le spese di giacenza, dalla richi-
230900040721     c                             esta all''esecuzione delle Vs. istruzioni, c-
231000040721     c                             omprese eventuali '
231100040721     c                   eval      cost11= '  assicurazioni verranno addebitate-
231200040721     c                              al               nella misura prevista dall-
231300040721     c                             '' art. 17/p delle'
231400040721     c                   eval      cost12= '  ultime Condizioni Generali CONFET-
231500040721     c                             RA, depositate presso tutte le Camere di Com-
231600040721     c                             mercio.'
231700040722     c                   eval      cost13= '- Dopo il   ° AVVISO, in caso di ma-
231800040721     c                             ncata risoluzione positiva della giacenza, V-
231900040721     c                             i ritorneremo'
232000040721     c                   eval      cost14= '  la merce a Vostro rischio e con a-
232100040721     c                             ddebito di ogni spesa.'
232200040708     c
232300120209     c                   move      *all'-'       trat130         130
232400040708     c
232500940923     C*---------------------------------------------------------------*
232600940923     C*  LIKE DEFN
232700040629     C     *LIKE         DEFINE    NTCAPL        kcapl
232800040629     C     *LIKE         DEFINE    NTCNK1        kcnk1
232900040629     C     *LIKE         DEFINE    NTCNK2        kcnk2
233000040629     C     *LIKE         DEFINE    NTCTNT        kctnt
233100950113     C     *LIKE         DEFINE    TGCKSC        KKSC
233200950113     C     *LIKE         DEFINE    TGCCTR        KCTR
233300950113     C     *LIKE         DEFINE    TGCPRG        KPRG
233400950123     C     *LIKE         DEFINE    SIMRGS        MITRGS
233500950123     C     *LIKE         DEFINE    ORGIND        MITIND
233600950123     C     *LIKE         DEFINE    ORGCPF        MITCPF
233700950123     C     *LIKE         DEFINE    ORGLOC        MITLOC
233800950123     C     *LIKE         DEFINE    ORGPRO        MITPRO
233900961218     C     *LIKE         DEFINE    GCPLNA        WSVLNA
234000961218     C     *LIKE         DEFINE    BLPAAS        WSVAAS
234100961218     C     *LIKE         DEFINE    BLPMGS        WSVMGS
234200961218     C     *LIKE         DEFINE    BLPNZD        WSVNZD
234300961218     C     *LIKE         DEFINE    BLPTSP        WSVTSP
234400961218     C     *LIKE         DEFINE    BLPLNP        WSVLNP
234500961218     C     *LIKE         DEFINE    BLPCTR        WSVCTR
234600961218     C     *LIKE         DEFINE    GCPLNA        WLNA
234700961218     C     *LIKE         DEFINE    BLPAAS        WAAS
234800961218     C     *LIKE         DEFINE    BLPMGS        WMGS
234900961218     C     *LIKE         DEFINE    BLPNZD        WNZD
235000961218     C     *LIKE         DEFINE    BLPTSP        WTSP
235100961218     C     *LIKE         DEFINE    BLPLNP        WLNP
235200961218     C     *LIKE         DEFINE    BLPCTR        WCTR
235300931217     C*-----------
235400931217     C* CARICO TABELLA CON DECODIFICA TIPI BOLLA
235500931217     C*-----------
235600940202     C                   Z-ADD     0             Y
235700931217     C                   MOVEL     'TB'          COD
235800931217     C     KTAB1         SETLL     TABEL00F                           15
235900931217     C     *IN15         DOWEQ     *OFF
236000931217     C     KTAB1         READE     TABEL                                  15
236100931217     C  N15              DO
236200931217     C                   ADD       1             Y
236300931217     C                   MOVEL     TBLKEY        CDB(Y)
236400931217     C                   MOVEL     TBLUNI        DSTB
236500931217     C                   MOVEL     §TBDES        DES(Y)
236600931217     C                   ENDDO
236700931217     C                   ENDDO
236800931217     C*----------------------------------------------------*
236900940922     C                   TIME                    WHHDAT           14 0
237000940922     C                   MOVE      WHHDAT        WDATA8            8 0
237100940923     C                   Z-ADD     WDATA8        G02DAT
237200961203     C                   Z-ADD     WDATA8        STPOGG
237300931217     C                   Z-ADD     66            CONRIG            3 0
237400931217     C                   Z-ADD     0             CONTA             3 0
237500961211     C                   Z-ADD     0             CONTAE            3 0
237600040906     C* udate in amg
237700040906     C                   Z-ADD     0             g02inv
237800040906     C                   Z-ADD     wdata8        G02DAT
237900040906     C                   MOVEL     ' '           G02ERR
238000040906     C                   CALL      'XSRDA8'
238100040906     C                   PARM                    WLBDA8
238200040906     C                   Z-ADD     G02inv        amgdtu            8 0
238300110503      *
238400110503     c                   move      'VPO'         tbecod
238500110503     c                   movel     'BRT'         tbeke1
238600110503     c                   clear                   tbeke2
238700110503     c     ktbe          chain     tntbe01l
238800110503     c                   if        %found(tntbe01l)
238900110503     c                   movel     tbeuni        dvpobrt
239000110503     c                   if        amgdtu <= §VPOBRTDTA
239100110503     c                   seton                                        33
239200110503     c                   endif
239300110503     c                   endif
239400040906
239500940922     C                   MOVEL     KPJBU         FNLG07
239600050516     c                   eval      d07ok = 'N'
239700050321      * si autoimposta il flag per gestire il commit
239800050321     c                   movel     'S'           d07comm
239900030717     C* MI COMPONGO TRUL90DS X STAMPA BOLLE
240000030717     C                   EXSR      SRTRUL90
240100931217     C* CREO DATA DIRITTA
240200940922     C                   Z-ADD     D07DAT        G02INV
240300940922     C                   Z-ADD     0             G02DAT
240400940922     C                   MOVEL     '3'           G02ERR
240500940923     C                   CALL      'XSRDA8'
240600940922     C                   PARM                    WLBDA8
240700940922     C                   Z-ADD     G02DAT        DATAU             8 0
240800931217     C* GUARDO se stampare capitale sociale
240900931217     C                   MOVEL     *BLANKS       KEY
241000931217     C                   MOVEL     1             KEY
241100931217     C                   MOVEL     '5 '          COD
241200931217     C     KTAB          CHAIN     TABEL                              15
241300931217     C  N15              MOVEL     TBLUNI        SI                1
241400931217     C*
241500931217     C* GUARDO CHE UTENTE HA FATTO LA RICHIESTA
241600931217     C                   MOVEL     CMD4(1)       OVRAPP
241700020730     C*
241800931217     C*  Ovrprtf  LET132
241900961202     C                   MOVEL     D07FIL        LIN
242000931217     C                   MOVEL     OVRAPP        CMD4
242100931217     C                   Z-ADD     63            LUNG
242200931217     C                   SETOFF                                       60
242300931217     C                   MOVEL     *BLANKS       COMMAN
242400931217     C                   MOVEA     CMD4(1)       COMMAN           80
242500931217     C                   CALL      'QCMDEXC'                            60
242600931217     C                   PARM                    COMMAN
242700931217     C                   PARM                    LUNG             15 5
242800931217     C                   OPEN      LET132
242900931217     C*
243000931217     C*  Ovrprtf  LETGIA
243100931217     C                   MOVEL     CMD5(1)       OVRAPP
243200961202     C                   MOVEL     D07FIL        LIN
243300931217     C                   MOVEL     OVRAPP        CMD5
243400931217     C                   Z-ADD     41            LUNG
243500931217     C                   SETOFF                                       60
243600931217     C                   MOVEL     *BLANKS       COMMAN
243700931217     C                   MOVEA     CMD5(1)       COMMAN           80
243800931217     C                   CALL      'QCMDEXC'                            60
243900931217     C                   PARM                    COMMAN
244000931217     C                   PARM                    LUNG             15 5
244100961203     C*  Ovrprtf  LETEST
244200961203     C                   MOVEL     CMD7(1)       OVRAPP
244300961203     C                   MOVEL     D07FIL        LIN
244400961203     C                   MOVEL     OVRAPP        CMD5
244500961203     C                   Z-ADD     41            LUNG
244600961203     C                   SETOFF                                       60
244700961203     C                   MOVEL     *BLANKS       COMMAN
244800961203     C                   MOVEA     CMD5(1)       COMMAN           80
244900961203     C                   CALL      'QCMDEXC'                            60
245000961203     C                   PARM                    COMMAN
245100961203     C                   PARM                    LUNG             15 5
245200931217     C                   OPEN      LETGIA
245300961203     C                   OPEN      LETEST
245400931217     C*
245500940922     C     D07RIS        COMP      'S'                                    80
245600940922     C     D07NGC        COMP      0                                      81
245700931217     C*
245800940923     C* VERIFICO SE LA FILIALE COLLEGATA NE GESTISCE ALTRE
245900931217     C                   MOVEA     *ZEROS        FIL£7
246000931217     C                   MOVEL     *BLANKS       KEY
246100961202     C                   MOVEL     D07FIL        KEY
246200931217     C                   MOVEL     '£7'          COD
246300931217     C     KTAB          CHAIN     TABEL                              15
246400931217     C  N15              MOVEA     TBLUNI        FIL£7
246500961202     C   15              MOVE      D07FIL        LNP§
246600931217     C   15              MOVEA     LNP§          FIL£7
246700940202     C                   Z-ADD     *HIVAL        SAVKSC
246800940923     C*
246900940923     C* Carico dati tariffe
247000940923     C                   MOVEL     *BLANKS       KEY
247100940923     C                   CLEAR                   DSQT1
247200940923     C                   MOVEL     '1'           KEY
247300940923     C                   MOVEL     'QT'          COD
247400940923     C     KTAB          CHAIN     TABEL                              15
247500940923     C  N15              MOVEL     TBLUNI        DSQT1
247600940922     C* Caricamento 2F
247700970110     C                   MOVEA     *HIVAL        C2F
247800940922     C                   Z-ADD     0             X                 2 0
247900940922     C                   MOVEL     '2F'          COD
248000940922     C     KTAB1         CHAIN     TABEL                              31
248100940922     C     *IN31         DOWEQ     '0'
248200940922     C     X             ANDLT     50
248300940922     C     TBLFLG        IFEQ      ' '
248400031027     C                   MOVE      TBLUNI        Ds2f
248500031027      * non carica la tabella riservata
248600031027     c                   if        §2Fris =*blank
248700940922     C                   ADD       1             X
248800960205     C                   MOVE      TBLUNI        D2F(X)
248900940923     C                   MOVEL     TBLKEY        C2F(X)
249000031027     c                   endIF
249100940922     C                   END
249200940922     C     KTAB1         READE     TABEL                                  31
249300940922     C                   END
249400941007     C*
249500941007     C                   MOVEL     '2G'          COD
249600941007     C                   MOVEL     *BLANKS       KEY
249700941007     C                   MOVEL     1             KEY
249800941007     C     KTAB          CHAIN     TABEL                              70
249900941007     C  N70              MOVEL     TBLUNI        DS2G
250000941007     C                   MOVEL     §2GCCO        CODBOL            2
250100941007     C*
250200020826     C* Carico £1 x effettuare letture solo della mia area
250300020918     c                   movel     kpjbu         savkpjbu
250400020826     c                   clear                   trul06ds
250500020826     c                   eval      d06cod = '£1'
250600020826     c                   movel     simfel        d06key
250700020826     c                   movel     trul06ds      kpjbu
250800020826     c                   call      'TRUL06R'
250900020826     c                   parm                    kpjba
251000020826     c                   movel     kpjbu         trul06ds
251100020918     c                   movel     savkpjbu      kpjbu
251200040929      * reperimento tabella MRA definizioni invio posta elettronica
251300040929     c                   clear                   dmra
251400040929     c                   move      'MRA'         tbecod
251500040929     c                   movel(p)  'FNLG07R'     tbeke1
251600040929     c                   move      *blank        tbeke2
251700040929     c     ktbe          chain     tntbe01l
251800041019     c                   if        %found(tntbe01l)
251900040929     c                   movel     tbeuni        dmra
252000040929     c                   endif
252100931217     C                   ENDSR
252200030717     C*---------------------------------------------------------------*
252300030717     C*   R O U T I N E   INIZIALE  x trul90ds                        *
252400030717     C*---------------------------------------------------------------*
252500030717     C     srtrul90      BEGSR
252600030717     c                   clear                   trul90ds
252700030717     c                   eval      d90mde = d07moe
252800030717     c                   eval      d90mdb = d07mob
252900030717     c                   eval      d90prb = d07prb
253000030717     c                   eval      d90pre = d07pre
253100030717     c                   eval      d90pss = d07pss
253200030717     c                   eval      d90mdb4 = d07mdb4
253300030717     c                   eval      d90mdb5 = d07mdb5
253400030717     c                   eval      d90psl = d07psl
253500030717     c                   eval      d90bsl = d07bsl
253600030717     c                   eval      d90prb4 = d07prb4
253700030717     c                   eval      d90prb5 = d07prb5
253800030717     c                   endsr
253900041111     C***------------------------------------------------***
254000041111      *  reperisce informazioni Hub di partenza Estero
254100041111     C***------------------------------------------------***
254200041111     C     Hub_Estero    Begsr
254300041111      *
254400041111     C                   CLEAR                   FNLV55DS
254500041111     C                   MOVEL     'P'           D55TPT
254600041111     C                   MOVE      est_Lnp       D55LIN
254700041111     C                   MOVE      est_Lnp       D55LNP
254800041111     C                   MOVE      amgdtu        d55drf
254900041111     C                   CALL      'FNLV55R'
255000041111     C                   PARM                    FNLV55DS
255100041111     c                   if        d55err = *blanks
255200041111     c     d55tfp        chain     azorg01l      Est_UNIORG
255300041111     c                   if        %found(azorg01l)
255400041111     c                   movel     est_orgdes    est_DesLnp
255500041111     c                   movel     est_orgtel    est_DesTel
255600041111     c                   movel     est_orgfax    est_DesFax
255700041111     c                   endif
255800041111     c                   endif
255900041111      *
256000041111     C                   ENDSR
256100931217     C******************************************************
256200921013     C***------------------------------------------------***
256300050214     Otigcp000  E            AGG0
256400920529     O                       GCPFAS
256500960205     O                       GCPDMM
256600960306     O                       GCPFST
256700050214     Otigcp000  E            AGG1
256800960205     O                       GCPTCM
256900050321     O*igcp002  E            FINTO
257000960131     C***------------------------------------------------***
257100930726     O** STAMPA LETTERA AL MITTENTE MODULO NUOVO
257200120321     Oletgia    E    12      TESTA            04
257300120321     O                       gcplnp               7
257400120321     O*ETGIA    E    14      TESTA            05
257500120321     O          E    14      TESTA            05
257600960202     O                                           73 'FAX:'
257700960202     O                       STPFAX              90
257800961114     O          E    14      TESTA            06
257900120113     O*mm            12                          80 'RACCOMANDATA'
258000930728     O                                          110 'Pagina'
258100930726     O                       PAGE          Z   +  1
258200961114     O          E    14      TESTA            08
258300071018     O****                                       15 'P.Operativo di'
258400071018     O                                           11 'Filiale di'
258500071018     O                       DESLNP              32
258600961114     O          E    14      TESTA            09
258700940202     O               13      INDLNP              21
258800961114     O          E    14      TESTA            10
258900961114     O                                           77 'Spettabile'
259000940202     O               13      CAPLNP               6
259100940202     O               13      LOCLNP            +  1
259200940202     O               13      PROLNP            +  1
259300961114     O          E   N14      TESTA            10
259400071018     O******                                     15 'P.Operativo di'
259500071018     O                                           11 'Filiale di'
259600071018     O                       DESLNP              32
259700120113     O*mm            12                          53 'RACCOMANDATA'
259800120321     o*                      srsut               53
259900961114     O                                           77 'Spettabile'
260000961114     O                                          110 'Pagina'
260100961114     O                       PAGE          Z   +  1
260200961114     O          E   N14      TESTA            11
260300961114     O               13      INDLNP              21
260400961114     O          E   N14      TESTA            12
260500961114     O               13      CAPLNP               6
260600961114     O               13      LOCLNP            +  1
260700961114     O               13      PROLNP            +  1
260800961114     O                       RAG                102
260900961114     O          E    14      TESTA            12
261000930728     O                                            4 'FAX'
261100930728     O                       FAXLNP            +  1
261200940923     O                       RAG                102
261300961114     O          E   N14      TESTA            13
261400961114     O                                            4 'FAX'
261500961114     O                       FAXLNP            +  1
261600961114     O                       VIA                102
261700961114     O          E    14      TESTA            13
261800930728     O                                            4 'TEL'
261900930728     O                       TELLNP            +  1
262000940923     O                       VIA                102
262100930728     O          E            TESTA            14
262200961114     O              N14                           4 'TEL'
262300961114     O              N14      TELLNP            +  1
262400940923     O                       CAP                 76
262500931217     O                       CIT               +  1
262600931217     O                       PRV               +  1
262700940923     O                       NZM               +  1
262800930729     O          E            TESTA            15
262900940923     O                       SCH                 32
263000960618     O               04                          77 'Att.ne di '
263100960618     O               04      ATTPER            +  1
263200120321     O*         E            TESTA            17
263300120321     O          E            TESTA            20
263400040708     O                                           24 'Con la presente siamo a '
263500040708     O                                         +  0 'comunicarVi le spedizion'
263600040708     O                                         +  0 'i giacenti del'
263700940923     O                       DATAU             +  1 '  /  /    '
263800120321     O          E            TESTA            21
263900930726     O                                           11 'Giacenza '
264000930726     O                                         +  3 'Destinatario'
264100120321     O          E            TESTA          1 21
264200930726     O                                           11 'Giacenza '
264300930726     O                                         +  3 'Destinatario'
264400930729     O          E            RIGAS          0
264500930726     O                       GCPFGC        Z      3
264600930726     O                                         +  0 '/'
264700930726     O                       GCPNGC        Z   +  0
264800940923     O                       RSD               +  3
264900940923     O                                         +  1 'Spedizione:'
265000930726     O                       GCPAAS        Z   +  2
265100930726     O                       GCPLNP        Z   +  1
265200930726     O                       GCPNRS        Z   +  1
265300930726     O                       GCPNSP        Z   +  1
265400930729     O                                         +  1 'del:'
265500940923     O                       DATSPE            +  1 '  /  /    '
265600950214     O                                         +  1 'M.or.'
265700950214     O                       BLPRMO            +  0
265800931203     O          E            RIGAS       1
265900030929     O              n52                          12 '1° Avviso '
266000040426     O               52      xprog         z      3
266100040426     O               52                          12 '° Avviso '
266200940923     O                       IND                 39
266300940923     O                                         +  1 'Vs. Rif.ti:'
266400940923     O                       BLPRMN        Z   +  1
266500940923     O                       BLPRMA            +  1
266600940923     O                       DESTSP            +  1
266700940923     O                                         +  2 'Nat.merce:'
266800940923     O                       BLPNAS            +  0
266900931203     O          E            RIGAS       0  0
267000030929     O              n52                          12 '1° Avviso '
267100040426     O               52      xprog         z      3
267200040426     O               52                          12 '° Avviso '
267300930727     O          E            RIGAS       1
267400960201     O                       CAD                 23
267500940923     O                       LOD               +  1
267600940923     O                       PRD               +  1
267700941007     O                       NZD               +  1
267800940317     O                                         +  2 'Note:'
267900060213     O                       ar4NOT            +  1
268000950214     O                                         +  1 'cod.cli.:'
268100950214     O                       GCPSCM        Z   +  1
268200930727     O          E            RIGAS       1
268300930726     O                                           20 'Colli:'
268400950116     O                       BLPNCL        Z   +  0
268500950116     O                                         +  1 'Peso:'
268600950116     O                       BLPPKF        2   +  0
268700950116     O                                         +  1 'Porto:'
268800950116     O                       PORTO             +  1
268900021107     O                                         +  1 'Aperta/Riap.'
269000021107     O                       data8             +  1 '  /  /    '
269100950116     O                                         +  1 'C/Ass:'
269200051115     O                       ar9VCA         B  +  0
269300051115     O                       ar9CAS        2B  +  1
269400950116     O          E            RIGAS       1
269500950116     O                                           26 'Motivo giac:'
269600961010     O                       GCPCMC            +  1
269700930727     O                       §2ADES            +  1
269800120507     O*mm                    ULTDMC             132
269900120507     O          E            RIGAS       1
270000120507     O                       ULTDMC              81
270100040304     O          E   n55      RIGAS       1
270200930726     O                       SK(23)              66
270300120210     O                       SK(24)             130
270400040304     O          E    55      RIGAS       1
270500040304     O                       SK(36)              66
270600040304     O                       datadcr             70 '  /  /    '
270700930727     O          E            RIGAS       1
270800930726     O                       SK(25)              66
270900120210     O                       SK(26)             130
271000930727     O          E            RIGAS       1
271100930726     O                       SK(25)              66
271200120210     O                       SK(26)             130
271300040928     O          E            RIGAS       1  1
271400930726     O                       SK(27)              66
271500930729     O                       §2BDES              59
271600120210     O                       SK(28)             130
271700030911     o                       mitdesd            110
271800930729     O*
271900930729     O          E            RIGA1          2 06
272000930729     O                                          110 'Pagina'
272100930729     O                       PAGE          Z   +  1
272200930729     O*
272300930727     O          E            FINES       1
272400930726     O                       SK(1)               66
272500120210     O                       SK(30)             130
272600120210     O                       FAXPOP              78
272700030911     O          E            FINES       1
272800030911     O                       SK(31)              66
272900030911     O          E            FINES       0
273000030911     O                       SK(31)              66
273100030911     O          E            FINES       0
273200030911     O                       SK(31)              66
273300030912     O                       SK(32)             122
273400930727     O          E            FINES       1
273500930726     O                       SK(2)               66
273600120210     O                       SK(12)             130
273700930727     O          E            FINES       1
273800930726     O                       SK(3)               66
273900120210     O                       SK(13)             130
274000930727     O          E            FINES       1
274100930726     O                       SK(4)               66
274200120210     O                       SK(14)             130
274300930727     O          E            FINES       1
274400930726     O                       SK(5)               66
274500120210     O*                      SK(15)             131
274600030911     O*         E            FINES       1
274700030911     O*                      SK(6)               66
274800120210     O*                      SK(16)             131
274900930727     O          E            FINES       1
275000930726     O                       SK(7)               66
275100120210     O                       §2GGGL              13
275200120210     O                       SK(17)             130
275300930727     O          E            FINES       1
275400930726     O                       SK(8)               66
275500120210     O                       SK(18)             130
275600930727     O          E            FINES       1
275700930726     O                       SK(9)               66
275800120210     O                       SK(19)             130
275900930727     O          E            FINES       1
276000930726     O                       SK(10)              66
276100120210     O                       SK(20)             130
276200930727     O          E            FINES       1
276300930727     O                       SK(11)              66
276400030929     O          E            FINES       1
276500030929     O                       SK(33)              66
276600120210     o                       progmax       z      9
276700120210     O                       SK(34)             130
276800030929     O          E            FINES       1
276900030929     O                       SK(35)              66
277000130319     O          E            FINES2      1
277100930726     O                       SK(21)              66
277200971112     O                       RSUT               110
277300130319     O          E            FINES2      1
277400120210     O                       SK(29)             130
277500120210     O                       DESPOP             109
277600130319     O          E   n17      FINES2      1
277700120210     O                       trat130            130
277800130319     O          E   n17      FINES2      1
277900041014     O                       SK(37)              66
278000120210     O                       SK(38)             130
278100130319     O          E   n17      FINES2      1
278200041014     O                       SK(39)              66
278300120210     o                       mittfax             87
278400130319     O          E   n17      FINES2      1
278500120209     O                       SK(40)              66
278600120209     O                       SK(41)             100
278700120210     o                       gcpscms             57
278800130319     O          E   n17      FINES2      1
278900120210     O                       trat130            130
279000040708      *---------------------------------------------------------------------
279100040708      *INVIO TRAMITE MAIL
279200040721     O          E            TESTAM           02
279300071018     O*****                                      15 'P.Operativo di'
279400071018     O                                           11 'Filiale di'
279500071018     O                       DESLNP              32
279600040721     O                                           66 'Spettabile'
279700040721     O          E            TESTAM           03
279800040708     O               13      INDLNP              21
279900040721     O          E            TESTAM           04
280000040708     O               13      CAPLNP               6
280100040708     O               13      LOCLNP            +  1
280200040708     O               13      PROLNP            +  1
280300040721     O                       RAG                 91
280400040721     O          E            TESTAM           04
280500040721     O                       RAG                 91
280600040721     O          E            TESTAM           05
280700040708     O                                            4 'FAX'
280800040708     O                       FAXLNP            +  1
280900040720     O                                         +  2 'TEL'
281000040720     O                       TELLNP            +  1
281100040721     O                       VIA                 91
281200040721     O          E            TESTAM           06
281300040721     O                       CAP                 65
281400040708     O                       CIT               +  1
281500040708     O                       PRV               +  1
281600040708     O                       NZM               +  1
281700040721     O          E            TESTAM           07
281800040722     O                       SCH                 33
281900040721     O               04                          66 'Att.ne di '
282000040708     O               04      ATTPER            +  1
282100040721     O          E            TESTAM           08
282200040708     O                                           24 'Oggetto: COMUNICAZIONE '
282300040708     O                                         +  0 'DI APERTURA GIACENZA - '
282400040708     O*                      DATAU             +  1 '  /  /    '
282500040708     O          E            RIGASM      0  0
282600040708     O              n52                          65 '1° Avviso '
282700040708     O               52      xprog         z     65
282800040708     O               52                        +  1 '° Avviso '
282900040721     O          E            RIGASM           10
283000040708     O                                           25 'A fronte della spedizione'
283100040708     O                       gcpaas        z     30
283200040827     O                       gcplnp        z     34
283300040827     O                       gcpnrs        z     37
283400040708     O                       gcpnsp        z     46
283500040708     O                                           51 'del:'
283600040708     O                       DATSPE              62 '  /  /    '
283700040708     o                                           63 ','
283800040708     o                                           82 'abbiamo provveduto'
283900040708     o              n52                          93 'ad aprire '
284000040708     o               52                          93 'a riaprire'
284100040721     O          E            RIGASM           11
284200040721     o                                         +  0 'pratica di giacenza'
284300040708     o                                         +  1 'N.'
284400040708     o                       gcpfgc        z   +  0
284500040708     o                                         +  0 '/'
284600040708     o                       gcpngc        z   +  0
284700040708     O          E            RIGASM      1
284800040708     O                                            8 'in data '
284900040708     O                       data8             +  1 '  /  /    '
285000040708     o                                         +  1 'per il seguente motivo:'
285100040708     o                       gcpcmc            +  1
285200040708     o                       §2ades            +  1
285300040708     O          E            RIGASM      1
285400040708     o                       ultdmc            +  1
285500040708     O          E            RIGASM      1
285600040708     O                                           26 'Gli estremi della spedizio'
285700040708     O                                         +  0 'ne sono i seguenti :'
285800040708     O          E            RIGASM      1
285900040708     o                                           22 'Riferimenti mittente :'
286000040708     O                       blprmn        z   +  1
286100040708     O                       blprma            +  2
286200040708     O          E            RIGASM      1
286300040708     o                                           22 'Destinatario         :'
286400040708     O                       rsd               +  1
286500040708     O          E            RIGASM      1
286600131122     O                       indm              + 23
286700040708     O          E            RIGASM      1
286800040708     O                       cad               + 23
286900040708     O                       lod               +  1
287000040708     O                       prd               +  1
287100040708     O                       nzd               +  1
287200040708     O          E            RIGASM      1
287300040708     o                                           22 'Mittente originale   :'
287400040708     O                       blprmo            +  1
287500040721     o                                           75 'Codice cliente :'
287600040708     O                       gcpscm            +  1
287700040708     O          E            RIGASM      1
287800040708     o                                           22 'Colli                :'
287900040708     O                       blpncl        z   +  1
288000040721     o                                           75 'Porto          :'
288100040708     O                       porto             +  1
288200040708     O          E            RIGASM      1
288300040708     o                                           22 'Peso                 :'
288400040708     O                       blppkf        2   +  1
288500040721     o                                           75 'Contrassegno   :'
288600051115     O                       ar9VCA         B  +  1
288700051115     O                       ar9CAS        2B  +  1
288800040708     O          E            RIGASM      1
288900040708     o                                           22 'Natura della merce   :'
289000040708     O                       blpnas            +  1
289100040721     o                                           75 'Note           :'
289200060213     O                       ar4not         B  +  1
289300040802     O          E   n55      FINESm      2
289400040708     o                                           26 'Restiamo in attesa di vost'
289500040708     o                                         +  0 're disposizioni che '
289600040708     o                                         +  0 'potrete comunicarci '
289700040708     o                                         +  0 'direttamente cliccando qui'
289800040802     O          E   n55      FINESm      0
289900040708     o                                           26 'Restiamo in attesa di vost'
290000040708     o                                         +  0 're disposizioni che '
290100040708     o                                         +  0 'potrete comunicarci '
290200040708     o                                         +  0 'direttamente cliccando qui'
290300040802     O          E   n55      FINESm      0
290400040708     o                                           26 'Restiamo in attesa di vost'
290500040708     o                                         +  0 're disposizioni che '
290600040708     o                                         +  0 'potrete comunicarci '
290700040708     o                                         +  0 'direttamente cliccando qui'
290800040802     O          E    55      FINESm      2
290900040802     o                                           26 'Salvo diversa indicazione '
291000040802     o                                         +  0 'provvederemo alla riconseg'
291100040802     o                                         +  0 'na dal '
291200040802     O                       datadcr           +  1 '  /  /    '
291300040802     O          E    55      FINESm      1
291400040802     o                                           26 'Potete visualizzare le dis'
291500040802     o                                         +  0 'posizioni già inserite '
291600040802     o                                         +  0 'cliccando qui'
291700040708     O          E            FINESm      2
291800040907     O                                            4 'B#'
291900040701     o                                         +  0 '?nspediz='
292000040701     O                       gcplnp            +  0
292100040701     O                       gcpnrs            +  0
292200040701     O                       gcpnsp            +  0
292300040701     o                                         +  0 '&keygiac='
292400040630     O                       gcpaas            +  0
292500040630     O                       gcplnp            +  0
292600040630     O                       gcpnrs            +  0
292700040630     O                       gcpnsp            +  0
292800040630     O                       blpaas            +  0
292900040630     O                       blpmgs            +  0
293000040630     o                       gcpagc            +  0
293100040630     o                       gcpfgc            +  0
293200040630     o                       gcpngc            +  0
293300040630     o                       gcpagc            +  0
293400040630     o                       gcpmgc            +  0
293500040907     O                                         +  0 '#E'
293600040722     O          E            FINESm      2
293700040721     o                       cost1              105
293800040708     O          E            FINESm      1
293900040721     o                       cost2              105
294000040708     o                       §2bdes              63
294100040708     O          E            FINESm      1
294200040721     o                       cost3              105
294300040708     O          E            FINESm      1
294400040721     o                       cost4              105
294500040708     O          E            FINESm      1
294600040721     o                       cost5              105
294700040721     o                       §2gggl              14
294800040708     O          E            FINESm      1
294900040721     o                       cost6              105
295000040708     O          E            FINESm      1
295100040721     o                       cost7              105
295200040708     O          E            FINESm      1
295300040721     o                       cost8              105
295400040708     O          E            FINESm      1
295500040721     o                       cost9              105
295600040708     O          E            FINESm      1
295700040721     o                       cost10             105
295800040708     O          E            FINESm      1
295900040721     o                       cost11             105
296000040721     o                       mitdesd             52
296100040708     O          E            FINESm      1
296200040721     o                       cost12             105
296300040708     O          E            FINESm      1
296400040721     o                       cost13             105
296500040721     o                       progmax       z     12
296600040721     O          E            FINESm      1
296700040721     o                       cost14             105
296800040708     O          E            FINESm      2
296900040721     o                                           30 'Distinti saluti.'
297000040708     O          E            FINESm      1
297100120321     o               33                          67 'BRT S.P.A.'
297200120321     o**            n33      srsut               67
297300110506     o*                                        +  1 '('
297400110506     o*                      gcplnp            +  0
297500110506     o*                                        +  0 ')'
297600040708     O          E            FINESm      1
297700040721     O                       SK(29)             105
297800040721     O                       DESPOP              82
297900040708     O*****************************************************************
298000940923     OLET132    E            TESRAC           12
298100940923     O                                           45 '* ELENCO RACCOMANDATE *'
298200940923     O                                           50 'DEL'
298300940923     O                       WDATA8              63 '  /  /    '
298400940923     O          E            TESRAC           14
298500940923     O                                            8 'MITTENTE'
298600950123     O                       MITRGS              57
298700940923     O          E            TESRAC           15
298800950123     O                       MITIND              29
298900940923     O          E            TESRAC           16
299000950123     O                       MITCPF              14
299100950123     O                       MITLOC              35
299200950123     O                       MITPRO              38
299300940923     O          E            TESRAC           18
299400940923     O                                            6 'NUMERO'
299500940923     O                                           32 'D E S T I N A T A R I O'
299600940923     O          E            DETRAC      2
299700940923     O                       CONTA         Z      6
299800940923     O                       RAG                 44
299900940923     O          E            DETRAC      1
300000940923     O                       VIA                 44
300100940923     O          E            DETRAC      1
300200940923     O                       CAP                 18
300300940923     O                       CIT                 54
300400940923     O                       PRV                 57
300500940923     O                       NZM                 61
300600961211     O*------------------------------------------------*
300700961211     O*  Dettaglio raccomandate estero
300800961211     O          E            DETRAE      2
300900961211     O                       CONTAE        Z      6
301000961211     O                       STPRSM              44
301100961211     O          E            DETRAE      1
301200961211     O                       STPINM              44
301300961211     O          E            DETRAE      1
301400961211     O                       STPCAM              18
301500961211     O                       STPCIM              54
301600961211     O                       STPPRM              57
301700961211     O                       STPNZM              61
301800900430     OQSYSPRT   E            ERR         1
301900940923     O               22      GCPAAS               4
302000900430     O               22      GCPLNP            +  1
302100900430     O               22      GCPNRS            +  1
302200900430     O               22      GCPNSP            +  1
302300900430     O               22                        +  1 'BOLLA INESISTENTE NEL FI'
302400900430     O               22                        +  0 'LE STORICO'
302500900430     O               23      GCPLNP               4
302600900430     O               23                        +  1 'FILIALE INESISTENTE NELL'
302700900430     O               23                        +  1 'ORGANIGRAMMA'
302800900430     O               25      GCPSCM               8
302900900430     O               25                        +  1 'COD.CLIENTE INESISTENTE '
303000900430     O               25                        +  0 'NEL PIANO DEI CONTI'
303100120111**         CMD1
303200120420OVRPRTF FILE(LETGIA) OUTQ(piccionedb/barpicstm)
303300120111**         CMD2
303400120420OVRPRTF FILE(LET132) OUTQ(piccionedb/barpicstm)
303500961203**         CMD4
303600931122OVRPRTF FILE(LET132) FORMTYPE('*STD-000') HOLD(*YES) SAVE(*YES)
303700961203**         CMD5
303800931122OVRPRTF FILE(LETGIA) FORMTYPE('*STD-000')
303900961203**         CMD6
304000960507DLTOVR  FILE(LETGIA)
304100961203**         CMD7
304200961203OVRPRTF FILE(LETEST) FORMTYPE('*STD-000')
304300961203**         CMD8
304400961203DLTOVR  FILE(LETEST)
304500040629**         CMD9
304600040630OVRPRTF FILE(LETGIA) OUTQ(EMAILIN) USRDFNDTA('
304700040906OVRPRTF FILE(LETEST) OUTQ(EMAILIN) USRDFNDTA('
304800921028**         RIG
304900921029        COMUNIC. APERTURA GIACENZA
305000921029DISTINTA RIEPILOGO APERTURA GIAC.
305100930726**
305200961009          Restiamo in attesa di Vs. disposizioni  al numero  FAX    01
305300120210-sino alla ricezione di Vs.istruzioni saremo tenuti ad aderire a    02
305400120210-le spese di giacenza, dalla richiesta all'esecuzione delle Vs.i    03
305500120210 addebitate nella misura prevista dall'art.17/p delle ultime Con    04
305600120210 Camere di Commercio.                                               05
305700120210 canza di Vs. precisazioni saranno considerate a carico del desti   06
305800120210-trascorsi XX giorni dalla presente senza che  abbiate  fornito     07
305900120210 li, Vi ritorneremo la merce a Vs. rischio e con addebito di ogn    08
306000120210 giacenza sia risolta provvederemo al recupero di ns.crediti med    09
306100120210 o assegnazione della merce ai sensi degli art. 2756, 2761, 2797    10
306200120210 diritto per spese e competenze non recuperate.                     11
306300930726lla richiesta di riconsegna da parte del destinatario               12
306400030912struzioni,comprese eventuali assicurazioni Vi saranno               13
306500930726dizioni Generali CONFETRA, depositate presso tutte le               14
306600930728anno essere  addebitate le spese di giacenza. In man-               15
306700960507natario, se la spedizione e' in "assegnato".                        16
306800930726per iscritto istruzioni concrete, complete e attuabi-               17
306900930728i spesa. Trascorsi ulteriori  30  giorni senza che la               18
307000930727iante richiesta al giudice dell'esecuzione di vendita               19
307100120210 e 2798  del C.C. salvo e impregiudicato ogni  altro                20
307200930727          Distinti saluti.                                          21
307300110503                      S.p.a.                                        22
307400930726Vs. Disposizioni .................................................  23
307500930726.....................................................               24
307600930726..................................................................  25
307700930726.....................................................               26
307800960507La merce e' custodita presso la Ns. di:                             27
307900030911Addebito spese giacenza a carico:                                   28
308000120209            Filiale di      xxxxxxxx                                29
308100120210  nr.fax      o preferibilmente tramite il nostro                   30
308200120507sito internet www.brt.it   alla sezione  Area Clienti               31
308300030912(se in possesso di password di accesso) informandoVi che:           32
308400120209Dopo il  ° avviso, in caso di mancata risoluzione positiva della    33
308500120210 giacenza, Vi ritorneremo la merce a Vostro rischio                 34
308600120209e con addebito di ogni spesa.                                       35
308700040304Salvo diversa indicazione provvederemo alla riconsegna dal          36
308800040927Per ricevere le prossime comunicazioni di giacenze delle Vs. spedi  37
308900120507zioni via e-mail, Vi invitiamo a compilare la presente              38
309000120507inviando la stessa via fax oppure dando segnalazione a:             39
309100040929indicando i seguenti dati:                                          40
309200040929Codice cliente          e-mail __________________________________   41

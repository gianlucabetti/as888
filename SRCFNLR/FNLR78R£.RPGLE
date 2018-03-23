000100040527     H DECEDIT('0,') DATEDIT(*yMd.)
000200950123     H* FNLR78R *----------------------------------------------------*
000300930910     H*            STATISTICHE CONSEGNE      - BATCH -
000400000000     H*--------------------------------------------------------------*
000500000630     F* BOLLE ARRIVI
000600970130     FAZCLN01L  IF   E           K DISK
000700040527     FFNfvv01L  IF   E           K DISK
000800040908     FFNfvv08L  IF   E           K DISK
000900040908     F                                     RENAME(FNfvv000:FNfvv008)
001000970130     FFNARB54L  IF   E           K DISK
001100950123     FFNART01L  IF   E           K DISK
001200060216     FFiAR401L  IF   E           K DISK
001300991104     FFIAR601L  IF   E           K DISK
001400051115     FFiAR901L  IF   E           K DISK
001500961108     FFNARB01L  IF   E           K DISK
001600961108     F                                     RENAME(FNARB000:FNARB001)
001700050331     FTigcp21L  IF   E           K DISK
001800961108     FFNLBL02L  IF   E           K DISK
001900930812     F* EVENTI
002000990805     FFNEVB01L  IF   E           K DISK
002100930910     F*
002200900523     FAZORG01L  IF   E           K DISK
002300000703     FFNSCO04L  UF A E           K DISK
002400000703     FFNSCO03L  IF   E           K DISK
002500000703     F                                     RENAME(FNSCO000:FNSCO003)
002600970124     FTABEL00F  UF   E           K DISK
002700021017     fFista01l  uf   e           k Disk
002800990329     FFNDCT01L  IF   E           K DISK
002900990329     F                                     RENAME(FNDCT000:FNDCT001)
003000990326     FFNDCD02L  IF   E           K DISK
003100940324     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
003200940324     F                                     USROPN
003300940210     F                                     FORMLEN(66)
003400940210     F                                     FORMOFL(63)
003500920706     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOE)
003600940210     F                                     FORMLEN(66)
003700940210     F                                     FORMOFL(65)
003800940601     FPRTF198X  O    F  198        PRINTER OFLIND(*INOD)
003900940601     F                                     USROPN
004000940601     F                                     FORMLEN(66)
004100940601     F                                     FORMOFL(64)
004200090918     D TE              S             66    DIM(50) CTDATA PERRCD(1)             RIGHE STATISTICA
004300940530     D CMD             S             43    DIM(1) CTDATA PERRCD(1)              OVR PRTF198
004400940601     D CM2             S             43    DIM(1) CTDATA PERRCD(1)              OVR PRTF198X
004500040608     D ELE             S             66    DIM(15) CTDATA PERRCD(1)             RIGHE ELENCO
004600940530     D*
004700950123     D REC             S              2    DIM(30)                              BOLLE RECUP
004800091112     D CEP             S              3    DIM(150)
004900940210     D*
005000930907     D ORE             S              3  0 DIM(13)                              ORE ANT/RIT
005100930908     D*
005200090917     D ESP             S              9  0 DIM(13)                              SKI COM "E"
005300090917     D H10             S              9  0 DIM(13)                              SKI COM "H"
005400090917     D NOR             S              9  0 DIM(13)                              SKI COM "C+D"
005500970121     D TOT             S              9  0 DIM(13)                              SKI COM TOT
005600970121     D*
005700970121     D SKE             S              9  0 DIM(13)                              SKI COM ESPR
005800090918     D SKC             S              9  0 DIM(13)                              SKI COM "C"
005900970121     D SKN             S              9  0 DIM(13)                              SKI COM DIST
006000090918     D SKh             S              9  0 DIM(13)                              SKI COM "H"
006100930907     D*
006200090917     D CNE             S              9  0 DIM(13)                              CONS.TE "E"
006300090917     D CNC             S              9  0 DIM(13)                              CONS.TE "C"
006400090917     D CNN             S              9  0 DIM(13)                              CONS.TE "D"
006500090917     D CNh             S              9  0 DIM(13)                              CONS.TE "H"
006600930907     D*
006700090917     D ICE             S              9  0 DIM(13)                              IN CONS "E"
006800090917     D ICC             S              9  0 DIM(13)                              IN CONS "C"
006900090917     D ICN             S              9  0 DIM(13)                              IN CONS "D"
007000090917     D ICh             S              9  0 DIM(13)                              CONS.TE "H"
007100930908     D*
007200970121     D NCE             S              9  0 DIM(13)                              NO IN C ESPR
007300970121     D NCC             S              9  0 DIM(13)                              NO IN C DIST
007400970121     D NCN             S              9  0 DIM(13)                              NO IN C NORM
007500090916     D NCH             S              9  0 DIM(13)                              NO IN C NORM
007600930907     D*
007700970121     D GAE             S              9  0 DIM(13)                              GIAC AP ESPR
007800970121     D GAC             S              9  0 DIM(13)                              GIAC AP CORR
007900970121     D GAN             S              9  0 DIM(13)                              GIAC AP NORM
008000090916     D GAh             S              9  0 DIM(13)                              GIAC AP NORM
008100930907     D*
008200970121     D LAE             S              9  0 DIM(13)                              LASC AV ESPR
008300970121     D LAC             S              9  0 DIM(13)                              LASC AV CORR
008400970121     D LAN             S              9  0 DIM(13)                              LASC AV NORM
008500090916     D LAh             S              9  0 DIM(13)                              LASC AV NORM
008600930907     D*
008700970121     D NFE             S              9  0 DIM(13)                              NON FAT ESPR
008800970121     D NFC             S              9  0 DIM(13)                              NON FAT CORR
008900970121     D NFN             S              9  0 DIM(13)                              NON FAT NORM
009000090916     D NFh             S              9  0 DIM(13)                              NON FAT NORM
009100930907     D*
009200970121     D CTE             S              9  0 DIM(13)                              CHIU TU ESPR
009300970121     D CTC             S              9  0 DIM(13)                              CHIU TU CORR
009400970121     D CTN             S              9  0 DIM(13)                              CHIU TU NORM
009500090916     D CTh             S              9  0 DIM(13)                              CHIU TU NORM
009600930907     D*
009700970121     D IDE             S              9  0 DIM(13)                              IN DIST ESPR
009800970121     D IDC             S              9  0 DIM(13)                              IN DIST CORR
009900970121     D IDN             S              9  0 DIM(13)                              IN DIST NORM
010000090916     D IDh             S              9  0 DIM(13)                              IN DIST NORM
010100940208     D*
010200970121     D RSE             S              9  0 DIM(1)                               RIS CON ESPR
010300970121     D RSC             S              9  0 DIM(1)                               RIS CON CORR
010400970121     D RSN             S              9  0 DIM(1)                               RIS CON NORM
010500090916     D RSh             S              9  0 DIM(1)                               RIS CON NORM
010600940208     D*
010700970121     D PRE             S              9  0 DIM(1)                               CON PAR ESPR
010800970121     D PRC             S              9  0 DIM(1)                               CON PAR CORR
010900970121     D PRN             S              9  0 DIM(1)                               CON PAR NORM
011000090916     D PRh             S              9  0 DIM(1)                               CON PAR NORM
011100930907     D*
011200970121     D COE             S              9  0 DIM(3)                               CONS.LI ESPR
011300970121     D COC             S              9  0 DIM(3)                               CONS.LI CORR
011400970121     D CON             S              9  0 DIM(3)                               CONS.LI NORM
011500090916     D COh             S              9  0 DIM(3)                               CONS.LI NORM
011600950314RM*  D*
011700950314RM*  D L6              S              3  0 DIM(30)                              FIL GEST.ARRIVI
011800000703     D L1              S              3  0 DIM(30)                              FIL GESTITE
011900930907     D*
012000040908     D knpg            S                   LIKE(fvvnpg) inz(2)
012100040908     D Wtstdec         S                   LIKE(§CVfdc)
012200040527     D Wartdam         S                   LIKE(artdam)
012300060314     d Dataiso         s               d   datfmt(*iso)
012400060314     d Dataisofine     s               d   datfmt(*iso)
012500050706     D wtipolancio     s              2
012600050706     D W013A           s             13
012700991130     D IMPeditato      s             13
012800991130     D w0130           s             13  0
012900991130     D w0122           s             12  2
013000021021     d Wfil            s                   Like(StaFil)
013100040812     d wesca           s              1
013200060314     d wfest           s              1
013300100222     d p0dpart         s             54
013400970130     D*
013500961112     D ARBDS         E DS                  EXTNAME(FNARB00F)
013600961112     D*
013700961112     D WSAVDS          DS           700
013800961112     D*
013900970207     D                 DS
014000970207     D  KDATA                  1      8  0
014100970207     D  KAAA                   1      4  0
014200970207     D  KMMM                   5      6  0
014300970207     D  KGG                    7      8  0
014400940530     D                 DS
014500950124     D  DATA                   1      8  0
014600950124     D  AA                     3      4  0
014700950124     D  MM                     5      6  0
014800950124     D  GG                     7      8  0
014900950124     D  MMGG                   5      8  0
015000970130     D CLNMAT          DS
015100970130     D  MAT                    1     31
015200970130     D                                     DIM(31)                              GG CALEND MATT
015300970130     D CLNPOM          DS
015400970130     D  POM                    1     31
015500970130     D                                     DIM(31)                              GG CALEND POMEI
015600930812     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
015700970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
015800920611     D DS2A          E DS
015900091112     D DS2Z          E DS
016000940210     D DS3A          E DS
016100040922     D*DS5C          E DS
016200930907     D* VARIABILI ARRIVI
016300930907     D DS7A          E DS
016400930907     D  ANT                   58     72  0
016500930907     D                                     DIM(5)                               ORE ANTICIPO
016600930907     D  RIT                   73     87  0
016700930907     D                                     DIM(5)                               ORE RITARDO
016800970124     D DS7AF           DS
016900970124     D  INUTIL                 1     67
017000970124     D  ALFDPE                68     75
017100970124     D  §7ACPE                68     75  0
017200970124     D  ALFDUE                76     83
017300970124     D  §7ACUE                76     83  0
017400950127     D WLBDAT          DS
017500950127     D  G02DAT                 1      8  0
017600950127     D  G02INV                 9     16  0
017700950127     D  G02ERR                17     17
017800950127     D  G02TGI                18     22  0
017900970124     D WGIDAT          DS
018000970124     D  GIODAT                 1      8  0
018100970124     D  GIOINV                 9     16  0
018200970124     D  GIOTGI                17     21  0
018300990329     D                 DS
018400990329     D  DCTDCA                 1      8  0
018500990329     D  DCTAAC                 1      4  0
018600990329     D  DCTMGC                 5      8  0
018700990326     D DTAD            DS
018800990326     D  §TARGR                26     26
018900990329     D DCCH            DS
019000990329     D  §CHDES                 1     50
019100990329     D  §CHIMA                53     53
019200970207     D* PASSAGGIO DATI DA FILTRO
019300970207     D DSLS01        E DS
019400970225     D* DS1RIS= S - ELABORA E STAMPA QUELLO INDICATO DA DS1FL1
019500970225     D*       = R -         RISTAMPA QUELLO INDICATO DA DS1FL1
019600970225     D*       =   - ELABORA E  IGNORA  IL  CAMPO        DS1FL1
019700970225     D* DS1FL1= S - STAMPA LA STATISTICA
019800970225     D*       = L - STAMPA LA LISTA SPEDIZIONI NON IN CONSEGNA
019900970225     D*       = E - STAMPA LA STATISTICA E LA LISTA
020000970225     D* DS1FL2= S - STAMPA LA LISTA DELLE SPEDIZIONI PREVISTE IN STATIS
020100970225     D*             PER UN CONTROLLO
020200100311     D* DS1FL3='L'- devo stampare la lista spedizioni non in consegna
020300040608     d*             anche se ds1ris=*blanks
020400040608     D* DS1FL4='E'- non si tratta di una vera ristampa ma di stampa
020500040608     D*             dopo aver elaborato i dati
020600040608     D* DS1FL5= S - considera spunta partenza
020700050706     D* DS1FL6= S - escludi supermercati e appuntamenti del giorno
020800100218     D* DS1FL7= S - escludi partic.consegna ATTESA se non raggiunta data
020900100311     D* DS1FL8= N - Non aggiornare la tabella ultima elaborazione
021000970207     D*
021100970207     D CNCR80        E DS
021200900511     D KPJBA         E DS
021300950314RM*  D* DS PER TRUL06R - CARICAMENTO £X
021400950314RM*  D DSUL06        E DS                  EXTNAME(TRUL06DS)
021500950314RM*  D  LIN                    1     90  0
021600950314     D                                     DIM(30)                              FIL. COMODO
021700940118     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
021800990329     D DSBS02        E DS                  EXTNAME(TIBS02DS)
021900991130     D DSCV          E DS
022000000531     D OG143         E DS
022100070419     D OG148         E DS
022200040526     D FNLV53DS      E DS
022300051128     D fnlv80ds      E DS
022400070419     D FNLRG6DS      E DS
022500090917     d dvpoDECO      e ds
022600090604     d ds5e          e ds
022700100218     d ds7R          e ds
022800110926     d fnlr78ds1     e ds
022900141119     d fnlry09ds2    e ds
023000110926     d
023100040812      * ds per il controllo della presenza di CA per la spedizione richiesta
023200040812     d FIDN12DS      e ds
023300040812     d  skKey                 26    305    dim(20)
023400040812     d  skAnn                306    325    dim(20)
023500040812     d  skDca                326    485  0 dim(20)
023600040812     d  skFca                486    545  0 dim(20)
023700040812     d  skDch                546    705  0 dim(20)
023800040812     d  skCch                706    745    dim(20)
023900040812
024000040812     d dsKey           ds
024100040812     d  dsaac                         4  0
024200040812     d  dsfil                         3  0
024300040812     d  dsnca                         7  0
024400040812      *
024500060905     D Cindistinta     C                   CONST('In Distinta n.xxxxx-
024600060905     D                                     xx  del xx/xx/xx')
024700100222     D CEscludiPar     C                   CONST('Escludi partConsegna"AT-
024800100222     D                                     TESA"per ConsRich.non raggiunta')
024900900523     C****************************************************************
025000900523     C*  RIEPILOGO INDICATORI
025100920306     C****************************************************************
025200940207     C* 01    - CONTROLLO LA STAMPA DELLE % NEL TOTALE CONSEGNABILE
025300940323     C* 02    - IMMESSA DATA AL
025400940329     C* 03    - STAMPO LA % SUL TOT.CONSEGNATO SOLO PER LE CONSEGNATE
025500040608     C* 04    -
025600040608     C* 05    - EVENTO RISERVA CONSEGNA
025700990329     C* 06-07 - PER STAMPARE LE C.A.
025800040608     C* 08    - lista non in consegna con spunte partenza
025900090916     C* 10    - SPEDIZIONE "E"- priority
026000090916     C* 11    - SPEDIZIONE "C" - corriere
026100090916     C* 12    -
026200090916     C* 13    -
026300970226     C* 15    - SI TRATTA DI UNA RISTAMPA
026400970226     C* 16    - LA SPEDIZIONE E' NON CONSEGNATA
026500940329     C* 17    - LA SPEDIZIONE NON E' COMPLETAMENTE ARRIVATA
026600070419     C* 18    - GEO attiva
026700110923     C* 19    - Perfect order
026800090916     C* 20    - SPEDIZIONE "D" distribuzione
026900090916     C* 21    - SPEDIZIONE "H" H1030
027000090917     C* 25    - servizio h1030 attivo
027100090918     C* 26    - non stampo la % finale
027200110923     C* 28    - Stampo le abilitazioni
027300920605     C* 30    - COMODO
027400950314RM*  C* 36    - FILIALE NON GESTITA DA SECONDO LIVELLO
027500950123     C* 34    - LETTURA FNARB54L
027600141119     C* 70    - Presente dipo di dirottamento
027700940117     C* 99    - STAMPA LA LISTA DI CONTROLLO
027800900523     C*****************************************************************
027900000000     C     *ENTRY        PLIST
028000000000     C                   PARM                    KPJBA
028100970207     C                   MOVEL     KPJBU         DSLS01
028200970225     C*
028300990805     C*DATA  PER CONFRONTARE CON GLI EVENTI
028400990805     C                   Z-ADD     DS1AAL        CO1DAA            8 0
028500970225     C*DATA DA SUDDIVIDERE IN AAAA MM GG
028600970207     C                   MOVEL     DS1ADA        KDATA
028700970225     C* SE DS1RIS = ' 'IGNORO IL CAMPO DS1FL1
028800970225     C     DS1RIS        IFEQ      ' '
028900970225     C                   CLEAR                   DS1FL1
029000970225     C                   ENDIF
029100040608     c*
029200040608     c* lista sped non in consegna con spunte partenza
029300040608     c                   if        ds1fl5='S'
029400040608     c                   seton                                        08
029500040608     c                   endif
029600970207     C*
029700000000     C                   Z-ADD     1             CODUT
029800900523     C                   CALL      'X§PARUT'
029900900523     C                   PARM                    UT§DSE
030000000000     C                   MOVEL     RAGUT         RSUT             20
030100920415     C                   MOVEL     REC80         CNCR80
030200000703      * CARICO L1
030300900511     C*---------------------------------------------------------------*
030400930812     C     KARB          KLIST
030500970225     C                   KFLD                    DS1LNA
030600950123     C                   KFLD                    NUM6              8 0
030700961108     C     KARB1         KLIST
030800961108     C                   KFLD                    LBLAAN
030900961108     C                   KFLD                    LBLLPN
031000961108     C                   KFLD                    LBLNRN
031100961108     C                   KFLD                    LBLNSN
031200930812     C     KARB2         KLIST
031300990805     C                   KFLD                    AR§AAS
031400930812     C                   KFLD                    ARBLNP
031500930812     C                   KFLD                    ARBNRS
031600930812     C                   KFLD                    ARBNSP
031700990805     C                   KFLD                    NUM6A
031800970417     C     KARB22        KLIST
031900990805     C                   KFLD                    AR§AAS            4 0
032000970417     C                   KFLD                    ARBLNP
032100970417     C                   KFLD                    ARBNRS
032200970417     C                   KFLD                    ARBNSP
032300990805     C                   KFLD                    NUM6A             8 0
032400970417     C                   KFLD                    NUMORA
032500940324     C     KARB3         KLIST
032600940324     C                   KFLD                    ARBAAS
032700940324     C                   KFLD                    ARBLNP
032800940324     C                   KFLD                    ARBNRS
032900940324     C                   KFLD                    ARBNSP
033000991104     C     KARB6         KLIST
033100991104     C                   KFLD                    ARBAAS
033200991104     C                   KFLD                    ARBLNP
033300991104     C                   KFLD                    ARBNRS
033400991104     C                   KFLD                    ARBNSP
033500991104     C                   KFLD                    KTRC
033600990326     C     KART          KLIST
033700990326     C                   KFLD                    ARTFLS
033800990326     C                   KFLD                    ARTLNA
033900990326     C                   KFLD                    ARTNRS
034000990326     C                   KFLD                    ARTNSC
034100950127     C     KAR41         KLIST
034200950127     C                   KFLD                    ARBAAS
034300950127     C                   KFLD                    ARBLNP
034400950127     C                   KFLD                    ARBNRS
034500950127     C                   KFLD                    ARBNSP
034600950127     C                   KFLD                    AR4TRC
034700950123     C     KARB4         KLIST
034800950123     C                   KFLD                    AR§AAS
034900950123     C                   KFLD                    ARBLNP
035000950123     C                   KFLD                    ARBNRS
035100950123     C                   KFLD                    ARBNSP
035200920611     C     KTAB          KLIST
035300920611     C                   KFLD                    CODUT
035400920611     C                   KFLD                    COD               2
035500920611     C                   KFLD                    KEY               8
035600940210     C     KTAB2         KLIST
035700940210     C                   KFLD                    CODUT
035800940210     C                   KFLD                    COD               2
035900970122     C     KSCO1         KLIST
036000000703     C************       KFLD                    KFLE
036100970121     C                   KFLD                    KLNA
036200970121     C                   KFLD                    KDCM
036300970122     C     KSCO2         KLIST
036400000703     C************       KFLD                    KFLE
036500970122     C                   KFLD                    KLNA
036600970122     C                   KFLD                    KSCO
036700970122     C                   KFLD                    KGBO
036800970122     C                   KFLD                    KDCM
036900970122     C                   KFLD                    KOLT
037000970122     C     KSCO21        KLIST
037100000703     C************       KFLD                    KFLE
037200970122     C                   KFLD                    KLNA
037300970122     C                   KFLD                    KSCO
037400970122     C                   KFLD                    KGBO
037500970122     C     KSCO22        KLIST
037600000703     C************       KFLD                    KFLE
037700970122     C                   KFLD                    KLNA
037800970122     C                   KFLD                    KSCO
037900970122     C                   KFLD                    KGBO
038000970122     C                   KFLD                    KDCM
038100970130     C     KCLN          KLIST
038200970130     C                   KFLD                    KTFP
038300970130     C                   KFLD                    KTFA
038400970130     C                   KFLD                    KAAA
038500970130     C                   KFLD                    KMMM              2 0
038600990329     C     KDCT          KLIST
038700990329     C                   KFLD                    DCTAAC
038800990329     C                   KFLD                    DCTFIL
038900990329     C                   KFLD                    DCTNCA
039000021017
039100021017     c     Kfista        Klist
039200021017     c                   Kfld                    KTRC
039300021017     c                   Kfld                    Simfel
039400021017     c                   Kfld                    Wfil
039500021017     c                   Kfld                    KDTS
039600040527     C     Kfvv          KLIST
039700040527     C                   KFLD                    D53NPA
039800040527     C                   KFLD                    D53NFA
039900040527     C                   KFLD                    D53LAI
040000040908     C     Kfvv8         KLIST
040100040908     C                   KFLD                    arbtfa
040200040908     C                   KFLD                    knpg
040300070420     C                   KFLD                    kdataloc
040400040908     C     Kfvv81        KLIST
040500040908     C                   KFLD                    arbtfa
040600040908     C                   KFLD                    knpg
040700971128     C*
040800020211     C     *LIKE         DEFINE    arblnp        savlnp
040900020211     C     *LIKE         DEFINE    EVBHEV        NUMORA
041000970417     C     *LIKE         DEFINE    ARBLNA        KTFA
041100970130     C     *LIKE         DEFINE    ARBLNA        KTFP
041200970130     C     *LIKE         DEFINE    STATRC        KTRC
041300970124     C     *LIKE         DEFINE    STADTS        KDTS
041400970124     C     *LIKE         DEFINE    STANCL        KNCL
041500970124     C     *LIKE         DEFINE    SCOOLT        KOLT
041600970122     C     *LIKE         DEFINE    SCOSCO        KSCO
041700970122     C     *LIKE         DEFINE    SCOGBO        KGBO
041800970122     C     *LIKE         DEFINE    ARBDCM        KDCM
041900970121     C     *LIKE         DEFINE    ARBLNA        KLNA
042000070420     C     *LIKE         DEFINE    fvvdfv        Kdataloc
042100000703     C**** *LIKE         DEFINE    DS1FEL        KFLE
042200991130     C     *LIKE         DEFINE    AR6DIV        SAVDIV
042300991130     C     *LIKE         DEFINE    AR6DIV        comdiv
042400961112     C*
042500930812     C*---------------------------------------------------------------*
042600930812     C* 1°ASSIOMA: SE LA SPEDIZIONE E' CONSEGNATA
042700930812     C*            COME DATA DISTINTA PRENDO LA DATA CONSEGNA
042800930812     C* 2°ASSIOMA: C'E' SEMPRE UN EVENTO CREATO A FRONTE DI OGNI
042900930812     C*            MANCATA CONSEGNA
043000950123     C* 3°ASSIOMA: ESCLUDO LE BOLLE CON DATA CONSEGNA MA IN FNARB000
043100940208     C*            PERCHE' SONO CONSEGNE ANOMALE CHE ASPETTATO LA DEFI-
043200940208     C*            NIZIONE DELLA FIGLIA
043300930812     C*---------------------------------------------------------------*
043400940117     C* LISTA SPEDIZIONI
043500970207     C     DS1FL2        IFEQ      'S'
043600940117     C                   SETON                                        99
043700940117     C                   EXCEPT    SPED1
043800940117     C                   ENDIF
043900930907     C* OPERAZIONI INIZIALI
044000930910     C                   EXSR      INIZIO
044100060110     c
044200060111     C***  DS1LNA        IFLE      0
044300060111     C***                CLEAR                   DSUL06
044400060111     C**+                MOVE      '£1'          D06COD
044500060111     C***                MOVEL     SIMFEL        D06KEY
044600060111     C***                MOVEL     DSUL06        KPJBU
044700060111     C***                CALL      'TRUL06R'
044800060111     C***                PARM                    KPJBA
044900060111     C***                MOVEL     KPJBU         DSUL06
045000060111     C***                MOVEA     LIN           L1                             TUTTE
045100060110      *                  --------------------------
045200060111     C***                ELSE
045300060110     C***                Z-ADD     DS1LNA        L1(1)                          UNA A VIDEO
045400060110     C                   movea     l6            L1                             UNA A VIDEO
045500060110      *                  -----------------------------
045600060111     C***                ENDIF
045700970122     C*
045800940329     C     *IN98         IFEQ      *OFF
045900920428     C** ELABORAZIONE
046000920430     C*
046100000703    1C     1             DO        30            WX                3 0
046200000703    2C     L1(WX)        IFNE      0
046300970122RM*  C*
046400970122RM*  C                   SETOFF                                       36
046500970122RM*  C*
046600060111     c* Impossibile lanciare pgm senza la linea di arrivo impostata
046700060111     c*  aseterisco le specifiche con 09 = *off
046800060111RM*  C**   *IN09         IFEQ      *OFF
046900060111     c**   simtpp        andeq     '2'
047000060111     c**   *in09         oreq      *off
047100060111     c**   simtpp        andeq     *blanks
047200060111RM*  C**   L1(WX)        LOOKUP    L6                                     30
047300060111RM*  C**N30              SETON                                        36
047400060111RM*  C**                 END
047500970122RM*  C*
047600970122RM*2AC     *IN36         IFEQ      *OFF
047700920430     C*
047800920604     C* MEMORIZZO FILIALE DI ARRIVO
047900000703     C                   Z-ADD     L1(WX)        DS1LNA
048000971128     C     DS1LNA        CHAIN     AZORG                              33
048100940208     C                   MOVEL     ORGDES        DESLNA           19
048200970122     C*
048300970122   2AC                   SELECT
048400970207     C     DS1RIS        WHENEQ    'R'
048500980513     C* SE E' IMPOSTATO DS1FL4 VUOL DIRE CHE NON SI TRATTA DI
048600980513     C*  RISTAMPA VERA E PROPRIA MA DI UNA RISTAMPOA SUCCESSIVA
048700980513     C*  ALLA ELABORAZIONE DEI DATI PURA
048800980513     C     DS1FL4        IFEQ      ' '
048900970226     C                   SETON                                        15
049000980513     C                   ENDIF
049100980513     C**
049200970122     C                   EXSR      ELASAL
049300970122  X2AC                   OTHER
049400920604     C*
049500950124     C* ?  S P E D I Z I O N I    C O N S E G N A T E  ?
049600930908     C*
049700970207     C                   Z-ADD     DS1ADA        NUM6
049800950123     C     KARB          SETLL     FNARB54L
049900950123     C  N02KARB          READE     FNARB54L                               34
050000970225     C   02DS1LNA        READE     FNARB54L                               34
050100000602      *
050200940324    3C     *IN34         DOWEQ     *OFF
050300000602     C*
050400940210     C* ESCLUDO I TIPI BOLLA RECUPERO
050500000609     C     ARBCBO        LOOKUP    REC                                    35
050600000609    4C     *IN35         IFEQ      *OFF
050700940324     C* SOLO SE DATA DAL AL CONTROLLO SE SUPERA LIMITE MAGGIORE
050800940324    5C     *IN02         IFEQ      *ON
050900970207     C     ARBDCM        ANDGT     DS1AAL
051000950124     C                   SETON                                        34
051100940323     C                   ELSE
051200090917     C                   EXSR      CONT1
051300940324    5C                   ENDIF
051400940324    4C                   ENDIF
051500920604     C*
051600000609     C  N02KARB          READE     FNARB54L                               34
051700970225     C  N34
051800970225     CAN 02DS1LNA        READE     FNARB54L                               34
051900940324    3C                   ENDDO
052000970130     C* SE NON HO TROVATO NESSUNA SPEDIZIONE CONSEGNATA
052100970130     C*  VERIFICO SE E' UN GIORNO FESTIVO: IN QUESTO CASO
052200970130     C*  NON ELABORO IL RESTO DELLA STATISTICA E SCRIVO UN RECORD
052300970130     C*  VUOTO CON F DI FESTIVO
052400970130     C     WCONS         IFEQ      ' '
052500060314     C                   MOVEL     DS1ADA        KDATA
052600060314     c                   exsr      CTRFesta
052700970130     C                   ENDIF
052800970130     C*
052900920605     C*
053000950124     C* ?  S P E D I Z I O N I   N O N   A N C O R A   C O N S E G N A T E  ?
053100930908     C*
053200970130     C* ELABORO SOLTANTO SE HO TROVATO ALMENO UNA SPEDIZIONE CONSEGNATA
053300970130     C*  E SE NON CE NE SONO MA SI TRATTA DI UN GIORNO LAVORATIVO
053400970130   2BC     WFEST         IFEQ      ' '
053500970130     C*
053600920605     C                   Z-ADD     0             NUM6
053700950123     C     KARB          SETLL     FNARB54L
053800950123     C     KARB          READE     FNARB54L                               34
053900920605     C*
054000940324    3C     *IN34         DOWEQ     *OFF
054100000609     C*
054200940210     C* ESCLUDO I TIPI BOLLA RECUPERO
054300950123     C     ARBCBO        LOOKUP    REC                                    35
054400940324    4C     *IN35         IFEQ      *OFF
054500940324     C* 02 OFF
054600940324     C* SE DATA ARRIVO > DATA DAL --> E' PER FORZA NON CONSEGNABILE
054700940324     C* CONTO LE NON CONSEGNATE E LE CONSEGNABILI
054800970207    5C  N02ARBDAM        IFLE      DS1ADA
054900090917     C                   EXSR      CONT4
055000940324    5C                   ENDIF
055100940324     C* 02 ON
055200940324     C* SE DATA ARRIVO > DATA AL  --> E' PER FORZA NON CONSEGNABILE
055300940324     C* CONTO SOLO LE RISERVE E LE CONSEGNE PARZIALI
055400970207    5C   02ARBDAM        IFLE      DS1AAL
055500090917     C                   EXSR      CONT3P
055600940324    5C                   ENDIF
055700940324    4C                   ENDIF
055800920605     C*
055900950123     C  N34KARB          READE     FNARB54L                               34
056000940324    3C                   ENDDO
056100920605     C*
056200950124     C* ?  S P E D I Z I O N I    C O N S E G N A T E    D O P O  ?
056300930908     C*
056400970207     C  N02              Z-ADD     DS1ADA        NUM6
056500970207     C   02              Z-ADD     DS1AAL        NUM6
056600940324     C*
056700950124     C     KARB          SETGT     FNARB54L
056800970225     C     DS1LNA        READE     FNARB54L                               34
056900920605     C*
057000970130    3C     *IN34         DOWEQ     *OFF
057100940210     C*
057200940210     C* ESCLUDO I TIPI BOLLA RECUPERO
057300950123     C     ARBCBO        LOOKUP    REC                                    35
057400940324    4C     *IN35         IFEQ      *OFF
057500940324     C* 02 OFF
057600970207    5C  N02ARBDAM        IFLE      DS1ADA
057700090917     C                   EXSR      CONT4
057800940324    5C                   ENDIF
057900940324     C* 02 ON
058000970207    5C   02ARBDAM        IFLE      DS1AAL
058100090917     C                   EXSR      CONT3P
058200940324    5C                   ENDIF
058300940324    4C                   ENDIF
058400920605     C*
058500970225     C     DS1LNA        READE     FNARB54L                               34
058600970130    3C                   ENDDO
058700970130   2BC                   ENDIF
058800940323     C*
058900920605     C*
059000970121     C* ?  S C R I V O    L A   S T A T I S T I C A  ?
059100930908     C*
059200970305     C* SOLO SE HO CHIESTO ANCHE LA STATISTICA
059300970305   2BC     DS1FL1        IFNE      'L'
059400970403     C     DS1RIS        ANDNE     'R'
059500970305     C**
059600970122     C* CANCELLO TUTTI I RECORD GIA' PRESENTI NEL FILE
059700970225     C                   MOVEL     DS1LNA        KLNA
059800970207     C                   Z-ADD     DS1ADA        KDCM
059900000703     C     KSCO1         CHAIN     FNSCO04L                           30
060000970122    3C     *IN30         DOWEQ     *OFF
060100970121     C                   DELETE    FNSCO000
060200000703     C     KSCO1         READE     FNSCO04L                               30
060300970122    3C                   ENDDO
060400970121     C**
060500970121     C                   EXSR      WRTSCO
060600970124     C**
060700100311     c** Se richiesto non aggiorno la stampa
060800100311     c
060900100311   3ac                   if        ds1fl8<>'N'
061000100311     c
061100970124     C** VEDO SE AGGIORNARE LA DATA PRIMA E ULTIMA ELABORAZIONE
061200970124     C                   MOVEL     '7A'          COD
061300970225     C                   MOVEL(P)  DS1LNA        KEY
061400970124     C     KTAB          CHAIN     TABEL                              30
061500100311     C* LA TABELLA CI DEVE ESSERE PER FORZA PERCHE' LA CREA SE MANCA
061600970124     C*  IL FILTRO
061700970124    3C     *IN30         IFEQ      *OFF
061800970124     C                   MOVEL     TBLUNI        DS7AF
061900970124    4C     ALFDUE        IFLT      *ZEROS
062000970124     C                   MOVEL     *ZEROS        ALFDUE
062100970124    4C                   ENDIF
062200970124    4C     ALFDPE        IFLT      *ZEROS
062300970124     C                   MOVEL     *ZEROS        ALFDPE
062400970124    4C                   ENDIF
062500970124     C*
062600021021     C* DATA RIELABORATA: DEVO AGGIORNARE FISTA
062700970207    5C     DS1ADA        IFGE      §7ACPE
062800970207    5C     DS1ADA        ANDLE     §7ACUE
062900970124     C                   MOVEL     'C'           KTRC
063000970207     C                   MOVEL     DS1ADA        KDTS
063100021017     c                   Movel     Ds1Lna        Wfil
063200021022     c     Kfista        Chain     Fista01l                           30
063300021022     c     *In30         DowEq     *Off
063400021022     c                   Delete    Fista01l
063500021022     c     Kfista        Reade     Fista01l                               30
063600021022     c                   EndDo
063700970124     C                   ENDIF
063800970124     C*
063900970124     C* SE MANCA LA PRIMA DATA ELABORAZIONE: AGGIORNO
064000970124    4C     §7ACPE        IFEQ      0
064100970207     C                   MOVEL     DS1ADA        ALFDPE
064200970124   X4C                   ELSE
064300970124     C* SE E' MINORE DI 1 GIORNO DELLA DATA PRIMA ELABORAZIONE
064400970124     C*  AGGIORNO ALTRIMENTI NO
064500970207    5C     DS1ADA        IFLT      §7ACPE
064600970207     C                   Z-ADD     DS1ADA        G02INV
064700970124     C                   MOVEL     '3'           G02ERR
064800970124     C                   CALL      'XSRDA8'
064900970124     C                   PARM                    WLBDAT
065000970124     C* TRASFORMO LA DATA IN GIORNI E ADDIZIONO 1
065100970124     C     G02TGI        ADD       1             GIOTGI
065200970124     C                   CALL      'XSRGI8'
065300970124     C                   PARM                    WGIDAT
065400970124    6C     GIOINV        IFEQ      §7ACPE
065500970207     C                   Z-ADD     DS1ADA        §7ACPE
065600970124    6C                   ENDIF
065700970124    5C                   ENDIF
065800970124    4C                   ENDIF
065900970124     C*
066000970124     C* SE MANCA L'ULTIMA DATA ELABORAZIONE: AGGIORNO
066100970124    4C     §7ACUE        IFEQ      0
066200970207     C                   MOVEL     DS1ADA        ALFDUE
066300970124   X4C                   ELSE
066400970124     C* SE E' MAGGIORE DI 1 GIORNO DELLA DATA ULTIMA ELABORAZIONE
066500970124     C*  AGGIORNO ALTRIMENTI NO
066600970207    5C     DS1ADA        IFGT      §7ACUE
066700970207     C                   Z-ADD     DS1ADA        G02INV
066800970124     C                   MOVEL     '3'           G02ERR
066900970124     C                   CALL      'XSRDA8'
067000970124     C                   PARM                    WLBDAT
067100970124     C* TRASFORMO LA DATA IN GIORNI E SOTTRAGGO 1
067200970124     C     G02TGI        SUB       1             GIOTGI
067300970124     C                   CALL      'XSRGI8'
067400970124     C                   PARM                    WGIDAT
067500970124    6C     GIOINV        IFEQ      §7ACUE
067600970207     C                   Z-ADD     DS1ADA        §7ACUE
067700970124    6C                   ENDIF
067800970124     C*
067900970124    5C                   ENDIF
068000970124    4C                   ENDIF
068100970124     C*
068200970124     C                   MOVEL     DS7AF         TBLUNI
068300970124     C                   UPDATE    TABEL
068400970124    3C                   ENDIF
068500100311   3aC                   ENDIF
068600970403   2BC                   ENDIF
068700970124     C*
068800970122   2AC                   ENDSL
068900970121     C*
069000970121     C* ?  S T A M P A    S T A T I S T I C A  ?
069100970225    3C     DS1FL1        IFEQ      'S'
069200970225    3C     DS1FL1        OREQ      'E'
069300920605     C                   EXSR      STAMPA
069400970122    3C                   ENDIF
069500060118     C*
069600060118     C* AZZERO CAMPI PER NUOVA LINEA DI ARRIVO
069700060118     C                   SETOFF                                       0103
069800060118     C                   CLEAR                   CNE
069900060118     C                   CLEAR                   CNN
070000060118     C                   CLEAR                   CNC
070100090918     C                   CLEAR                   CNh
070200090918     C                   CLEAR                   H10
070300090918     C                   CLEAR                   ESP
070400060118     C                   CLEAR                   NOR
070500060118     C                   CLEAR                   TOT
070600060118     C                   CLEAR                   ICE
070700060118     C                   CLEAR                   ICN
070800060118     C                   CLEAR                   ICC
070900090918     C                   CLEAR                   ICh
071000060118     C                   CLEAR                   GAE
071100060118     C                   CLEAR                   GAN
071200060118     C                   CLEAR                   GAC
071300090918     C                   CLEAR                   GAh
071400060118     C                   CLEAR                   LAE
071500060118     C                   CLEAR                   LAN
071600060118     C                   CLEAR                   LAC
071700090918     C                   CLEAR                   LAh
071800060118     C                   CLEAR                   NFE
071900060118     C                   CLEAR                   NFN
072000060118     C                   CLEAR                   NFC
072100090918     C                   CLEAR                   NFh
072200060118     C                   CLEAR                   CTE
072300060118     C                   CLEAR                   CTN
072400060118     C                   CLEAR                   CTC
072500090918     C                   CLEAR                   CTh
072600060118     C                   CLEAR                   IDE
072700060118     C                   CLEAR                   IDN
072800060118     C                   CLEAR                   IDC
072900090918     C                   CLEAR                   IDh
073000060118     C                   CLEAR                   COE
073100060118     C                   CLEAR                   CON
073200060118     C                   CLEAR                   COC
073300090918     C                   CLEAR                   COh
073400060118     C                   CLEAR                   PRE
073500060118     C                   CLEAR                   PRN
073600060118     C                   CLEAR                   PRC
073700090918     C                   CLEAR                   PRh
073800060118     C                   CLEAR                   RSE
073900060118     C                   CLEAR                   RSN
074000060118     C                   CLEAR                   RSC
074100090918     C                   CLEAR                   RSh
074200060118     C                   CLEAR                   NCE
074300060118     C                   CLEAR                   NCN
074400060118     C                   CLEAR                   NCC
074500090918     C                   CLEAR                   NCh
074600060118     C                   CLEAR                   GENNOR
074700060118     C                   CLEAR                   GENESP
074800090918     C                   CLEAR                   GENh10
074900060118     C                   CLEAR                   GENTOT
075000970122     C*
075100970122RM*2AC                   END
075200000703    2C                   ENDIF
075300980707    1C                   ENDDO
075400940203     C*
075500940324     C   99              EXCEPT    SPEDT
075600050805     C* STAMPO dicitura * fine stampa * in LISTA SPEDIZIONI NON IN CONSEGNA
075700050805     C  N02DS1FL1        IFEQ      'L'
075800050805     C     DS1FL1        OREQ      'E'
075900050805     C     DS1FL3        OREQ      'L'
076000050805     c                   if        *inod = *on
076100050805     c                   except    tesele
076200050805     c                   endif
076300050805     c                   except    fines
076400050805     C                   ENDIF
076500940324     C                   ENDIF
076600920504     C*
076700990329     C                   CLEAR                   DSBS02
076800990329     C                   MOVEL     'C'           T02TLA
076900990329     C                   CALL      'TIBS02R'
077000990329     C                   PARM                    KPJBA
077100990329     C                   PARM                    DSBS02
077200040526     C*
077300040526     C                   CLEAR                   FNLV53DS
077400040526     C                   MOVEL     'C'           D53TLA
077500040526     C                   CALL      'FNLV53R'
077600040526     C                   PARM                    FNLV53DS
077700141119     c
077800141119     c                   clear                   fnlry09ds2
077900141119     c                   eval      ilry09tla= 'C'
078000141119     c                   eval      kpjbu=fnlry09ds2
078100141119     c                   call      'FNLRY09C'
078200141119     c                   parm                    kpjba
078300141119
078400040526     c
078500900523     C                   SETON                                        LR
078600991130      **********************************************************************
078700991130      * EDITO IMPORTI PER CAMPI in stampa
078800991130      **********************************************************************
078900991130     C     EDTIMPORTI    BEGSR
079000991130
079100991130      * -----------------------------------------------------------------------------------
079200991130      * W0122      contiene il numero da editare
079300991130      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il num
079400991130      *            ero ha decimali
079500991130      * IMPEDITATO ritorna il valore editato per la stampa lungo13 (troncate
079600991130      *            cifre oltre 10 mld)
079700991130      * -----------------------------------------------------------------------------------
079800991130
079900991130     c                   clear                   IMPeditato
080000991130     c                   clear                   W013A
080100991130     c                   if        Wtstdec = 'S'
080200991130     C                   eval      W013a = %EDITC(W0122:'4')
080300991130     c                   else
080400991130     c                   z-add     W0122         W0130
080500991130     C                   eval      W013a = %EDITC(W0130:'4')
080600991130     c                   endif
080700991130     c
080800991130     c                   move      W013a         IMPeditato
080900991130     c                   ENDSR
081000950124     C**************************************************************************
081100950124     C* OPERAZIONI INIZIALI
081200950124     C**************************************************************************
081300930910     C     INIZIO        BEGSR
081400950123     C*
081500950123     C                   TIME                    WTIME            14 0          ORA & DATA
081600950123     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
081700950124     C                   MOVEL     WTIME         UTIME             6 0          ORARIO
081800950127     C*
081900950127     C* INVERTO LA DATA DEL GIORNO
082000950127     C                   Z-ADD     WDATE         G02DAT
082100950127     C                   Z-ADD     *ZERO         G02INV
082200950127     C                   MOVEL     *BLANKS       G02ERR
082300950127     C                   CALL      'XSRDA8'
082400950127     C                   PARM                    WLBDAT
082500950127     C                   Z-ADD     G02INV        WDATE1            8 0          AAAA/MM/GG
082600930910     C*
082700940323     C* 02 ON - IMMESSA DATA AL
082800970207     C     DS1AAL        IFNE      0
082900970207     C     DS1ADA        IFNE      DS1AAL
083000940323     C                   SETON                                        02
083100940323     C                   END
083200940323     C                   ELSE
083300970207     C                   Z-ADD     DS1ADA        DS1AAL
083400940323     C                   END
083500930907     C*
083600930907     C* 09 ON - LINEA ARRIVO IMMESSA
083700060111     C***  DS1LNA        COMP      0                                  09
083800930907     C*
083900930910     C* REPERISCO VARIABILI ARRIVI
084000930907     C                   MOVEL     '7A'          COD
084100930907     C                   MOVEL     *BLANKS       KEY
084200930907     C                   MOVEL     '1'           KEY
084300970124     C     KTAB          CHAIN(N)  TABEL                              31
084400930907     C  N31              MOVEL     TBLUNI        DS7A
084500930907     C   31              CLEAR                   DS7A
084600930907     C* CARICO UNICA SCHIERA DEI RITARDI E CONSEGNE
084700930907     C                   Z-ADD     5             X                 3 0
084800930907     C                   Z-ADD     2             Y                 3 0
084900930907     C                   Z-ADD     -999          ORE(1)
085000930907     C*
085100930907     C                   DO        5
085200930907     C     ANT(X)        IFGT      0
085300930907     C     0             SUB       ANT(X)        ORE(Y)
085400930907     C                   ADD       1             Y
085500930907     C                   ENDIF
085600930907     C                   SUB       1             X
085700930907     C                   ENDDO
085800930907     C*
085900930907     C                   Z-ADD     1             X                 3 0
086000930907     C*
086100930907     C                   DO        5
086200930907     C     RIT(X)        IFGT      0
086300930907     C                   ADD       1             Y
086400930907     C                   Z-ADD     RIT(X)        ORE(Y)
086500930907     C                   ENDIF
086600930907     C                   ADD       1             X
086700930907     C                   ENDDO
086800930907     C*
086900930907     C                   ADD       1             Y
087000930907     C                   Z-ADD     999           ORE(Y)
087100940210     C* CARICO I TIPI BOLLA RECUPERO CHE DEVO SCARTARE
087200940210     C                   Z-ADD     1             X
087300940210     C                   MOVEL     '3A'          COD
087400970124     C     KTAB2         CHAIN(N)  TABEL                              30
087500940210     C*
087600940210     C     *IN30         DOWEQ     *OFF
087700940210     C     TBLFLG        IFEQ      ' '
087800940210     C                   MOVEL     TBLUNI        DS3A
087900940210     C     §3ARBL        IFEQ      'R'
088000940210     C                   MOVEL     TBLKEY        REC(X)
088100940210     C                   ADD       1             X
088200940210     C                   ENDIF
088300940210     C                   ENDIF
088400940210     C*
088500970124     C     KTAB2         READE(N)  TABEL                                  30
088600940210     C                   ENDDO
088700091112     c
088800091112     C* CARICO gli eventi particolari
088900091112     C                   Z-ADD     1             X
089000091112     C                   MOVEL     '2Z'          COD
089100091112     C     KTAB2         CHAIN(N)  TABEL                              30
089200091112     C*
089300091112     C     *IN30         DOWEQ     *OFF
089400091112     C     TBLFLG        IFEQ      ' '
089500091112     C                   MOVEL     TBLUNI        DS2Z
089600091112     C     §2zcep        IFne      ' '
089700091112     C                   MOVEL     TBLKEY        CEP(X)
089800091112     C                   ADD       1             X
089900091112     C                   ENDIF
090000091112     C                   ENDIF
090100091112     C*
090200091112     C     KTAB2         READE(N)  TABEL                                  30
090300091112     C                   ENDDO
090400930907     C*
090500091112     c*
090600940530     C* OVRPRTF SU PRTF198
090700091112     C                   Z-ADD     43            LUNG             15 5
090800970225     C     DS1FL1        IFEQ      'S'
090900970225     C     DS1FL1        OREQ      'E'
091000940324     C                   MOVEA     CMD           COMMAN           80
091100940324     C*
091200940329     C                   CALL      'QCMDEXC'                              98                A
091300940324     C                   PARM                    COMMAN
091400940324     C                   PARM                    LUNG
091500940329     C  N98              OPEN      PRTF198
091600950410     C                   END
091700940601     C* OVRPRTF SU PRTF198X
091800970225     C  N02
091900970225     CANN98DS1FL1        IFEQ      'L'
092000970225     C     DS1FL1        OREQ      'E'
092100970403     C     DS1FL3        OREQ      'L'
092200940530     C                   MOVEA     CM2           COMMAN           80
092300940530     C*
092400940530     C                   CALL      'QCMDEXC'                              98                A
092500940530     C                   PARM                    COMMAN
092600940530     C                   PARM                    LUNG
092700940601     C  N98              OPEN      PRTF198X
092800940530     C                   ENDIF
092900020429     C* SE RICHIESTA STATISTICA PER TUTTE LE FILIALI GESTITE
093000020429     C* E SE SONO UN SECONDO LIVELLO CARICO LA £6
093100060110     c***  simtpp        ifeq      '2'
093200060110     c***  simtpp        oreq      *blanks
093300060110     C***  DS1LNA        ifeq      *ZEROS
093400950314RM*  C                   CLEAR                   DSUL06
093500950314RM*  C                   MOVE      '£6'          D06COD
093600060111     C***                movel     simpou        D06KEY
093700060111     C                   movel     ds1lna        D06KEY
093800950314RM*  C                   MOVEL     'L'           D06TLA
093900060118 RM* C                   MOVEL     ds1aal        D06drf
094000060118 RM* C                   MOVE      'S'           D06key
094100950314RM*  C                   MOVEL     DSUL06        KPJBU
094200950314RM*  C                   CALL      'TRUL06R'
094300950314RM*  C                   PARM                    KPJBA
094400950314RM*  C                   MOVEL     KPJBU         DSUL06
094500950314RM*  C                   MOVEA     LIN           L6
094600060110RM*  C***                END
094700060110     C***                END
094800970128     C*
094900970130     C                   CLEAR                   WCONS
095000970130     C                   CLEAR                   WFEST
095100100222     c*
095200100222     C**                 CLEAR                   dsbs02
095300100222     c**                 clear                   dvpodeco
095400100222     C**                 MOVEL     'C'           T02MOD
095500100222     C**                 MOVEL     KNSIF         T02SIF
095600100222     C**                 MOVEL     'VPO'         T02COD
095700100222     c**                 movel(P)  'DECO'        t02ke1
095800100222     C**                 CALL      'TIBS02R'
095900100222     C**                 PARM                    KPJBA
096000100222     C**                 PARM                    dsbs02
096100100222    2C**                 IF        T02ERR = *BLANKS
096200100222     C**                 MOVEL     T02uni        dvpodeco
096300100222    2C**                 ENDIF
096400090917    c
096500090917     c* Accendo indicatore 25 de devo stampare anche tipo servizio H
096600100222     c**                 if        ds1aal>=§vpotsph
096700090917     c                   seton                                        25
096800100222     c**                 endif
096900070419     c**
097000070419     c* Verifico se ha GEO attiva
097100070419     C     DS1LNA        CHAIN     AZORG                              33
097200070419     c                   movel     orgde8        og148
097300070419     c                   if        §oggeot='S'
097400070419     c                   seton                                        18
097500070419     c                   endif
097600090604
097700090604      * carico le descrizioni fisse dei tipi servizio
097800090604      * chiodo la descrizione per ogni tipo servizio visto che la descrizione
097900090604      * è da stampare nella testata ed è fissa
098000090604      * le colonne dei tipi servizio da stampare sono
098100090917      * 'C + D' 'E' "H"
098200090604     c                   clear                   ds5e
098300090604     c                   clear                   wdtspc            5
098400090604     c                   clear                   wdtspd            5
098500090604     c                   clear                   wdtspe            5
098600090917     c                   clear                   wdtsph            5
098700090604     c                   eval      cod = '5E'
098800090604     c     ktab2         setll     tabel00f
098900090604     c                   do        *hival
099000090604     c     ktab2         reade(n)  tabel00f
099100090604     c                   if        %eof(tabel00f)
099200090604     c                   leave
099300090604     c                   endif
099400090604     c                   eval      ds5e = tbluni
099500090604     c                   if        tblkey = 'C'
099600090604     c                   eval      wdtspc = §5ed05
099700090604     c                   endif
099800090604     c                   if        tblkey = 'D'
099900090604     c                   eval      wdtspd = §5ed05
100000090604     c                   endif
100100090604     c                   if        tblkey = 'E'
100200090604     c                   eval      wdtspe = §5ed05
100300090604     c                   endif
100400090917     c                   if        tblkey = 'H'
100500090917     c                   eval      wdtsph = §5ed05
100600090917     c                   endif
100700090604     c                   enddo
100800971117     C***
100900971117     C                   SETON                                        OEOFOD
101000950124     C*
101100100222     c* Verifico se devo escludere le partic.consegna di attesa non raggiunte
101200100222     c                   if        ds1fl7='S'
101300100222     c                   eval      p0dpart=CEscludiPar
101400100222     c                   endif
101500930907     C                   ENDSR
101600970122     C**************************************************************************
101700970122     C* ELABORO LA STATISTICA PARTENDO DAI SALDI
101800970122     C**************************************************************************
101900970122     C     ELASAL        BEGSR
102000970122     C* S P E D I Z I O N I   C O N S E G N A T E
102100000703     C*********          MOVEL     DS1FEL        KFLE
102200970225     C                   MOVEL     DS1LNA        KLNA
102300970207     C                   MOVEL     DS1ADA        KDCM
102400970122     C*
102500970122     C                   MOVEL     'C'           KSCO
102600970122     C                   MOVEL     'A'           KOLT
102700970122     C                   MOVEL     ' '           KGBO
102800970122     C*
102900970122     C                   EXSR      RIEMPI
103000970122     C*
103100970122     C* C O N S E G N E   P A R Z I A L I
103200970122     C                   MOVEL     'C'           KSCO
103300970122     C                   MOVEL     ' '           KOLT
103400970122     C                   MOVEL     'P'           KGBO
103500970122     C                   EXSR      RIEMPI
103600970122     C* R I S E R V E   C O N S E G N E
103700970122     C                   MOVEL     'C'           KSCO
103800970122     C                   MOVEL     ' '           KOLT
103900970122     C                   MOVEL     'R'           KGBO
104000970122     C                   EXSR      RIEMPI
104100970122     C* SOLO SE RICHEISTA UNA SOLA DATA, CONTINUO A MEMORIZZATE LE SKI
104200970122     C     *IN02         IFEQ      *OFF
104300970122     C*
104400970122     C*  N O N   C O N S E G N A T E
104500970122     C*          DI CUI GIACENZE APERTE
104600970122     C                   MOVEL     'N'           KSCO
104700970122     C                   MOVEL     'A'           KOLT
104800970122     C                   MOVEL     'G'           KGBO
104900970122     C                   EXSR      RIEMPI
105000970122     C*          DI CUI LASCIATI AVVISI
105100970122     C                   MOVEL     'N'           KSCO
105200970122     C                   MOVEL     'A'           KOLT
105300970122     C                   MOVEL     'L'           KGBO
105400970122     C                   EXSR      RIEMPI
105500970122     C*          DI CUI CHIUSE PER TURNO
105600970122     C                   MOVEL     'N'           KSCO
105700970122     C                   MOVEL     'A'           KOLT
105800970122     C                   MOVEL     'C'           KGBO
105900970122     C                   EXSR      RIEMPI
106000970122     C*          DI CUI NON FATTE
106100970122     C                   MOVEL     'N'           KSCO
106200970122     C                   MOVEL     'A'           KOLT
106300970122     C                   MOVEL     'N'           KGBO
106400970122     C                   EXSR      RIEMPI
106500970122     C*  I N     D I S T I N T A
106600970122     C                   MOVEL     'D'           KSCO
106700970122     C                   MOVEL     'A'           KOLT
106800970122     C                   MOVEL     ' '           KGBO
106900970122     C                   EXSR      RIEMPI
107000970122     C* I N   C O N S E G N A
107100970122     C                   MOVEL     'I'           KSCO
107200970122     C                   MOVEL     'A'           KOLT
107300970122     C                   MOVEL     ' '           KGBO
107400970122     C                   EXSR      RIEMPI
107500970122     C* NON   I N   C O N S E G N A
107600970124     C                   MOVEL     'O'           KSCO
107700970122     C                   MOVEL     'A'           KOLT
107800970122     C                   MOVEL     ' '           KGBO
107900970122     C                   EXSR      RIEMPI
108000970122     C*  C O N S E G N A B I L I
108100970122     C                   MOVEL     'B'           KSCO
108200970122     C                   MOVEL     ' '           KOLT
108300970122     C                   MOVEL     ' '           KGBO
108400970122     C                   EXSR      RIEMPI
108500970122     C                   ENDIF
108600970122     C*
108700970122     C                   ENDSR
108800970122     C**************************************************************************
108900970122     C* RIEMPO LE SCHIERE DAL FILE SALDI FNSCO00F
109000970122     C**************************************************************************
109100970122     C     RIEMPI        BEGSR
109200000703     C     KSCO2         SETLL     FNSCO03L
109300000703     C  N02KSCO22        READE     FNSCO03L                               30
109400000703     C   02KSCO21        READE     FNSCO03L                               30
109500970122    1C     *IN30         DOWEQ     *OFF
109600970122     C*
109700970122     C* E' DENTRO LE DATE DI CONSEGNA IMMESSE
109800970122    2C     *IN02         IFEQ      *OFF
109900970207     C     SCODCM        ORLE      DS1AAL
110000970122     C* ESCUDO I RECORD DI TOTALE
110100970122    3C     SCOOLT        IFNE      'T'
110200970122     C     SCOOLT        ANDNE     'F'
110300970122     C* IMPOSTO PER CERCARE IL POSTO NELLA SKIERA DELLE ORE
110400970122    4C                   SELECT
110500970122     C     SCOOLT        WHENEQ    'A'
110600970122     C                   Z-SUB     999           NUM3
110700970122     C     SCOOLT        WHENEQ    'O'
110800970122     C                   Z-ADD     999           NUM3
110900970122     C     SCOOLT        WHENEQ    'D'
111000970122     C                   Z-ADD     SCOORE        NUM3
111100970122     C                   OTHER
111200970122     C* NON CI SONO ORE PER L'AFFIDABILITA'
111300970122     C                   Z-ADD     1             B
111400970122    4C                   ENDSL
111500970122     C*
111600970122     C     SCOOLT        IFNE      ' '
111700970122     C                   EXSR      CALRIT
111800970122     C                   ENDIF
111900970122     C*
112000970122     C                   SELECT
112100970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
112200970122     C     SCOGBO        ANDEQ     ' '
112300970310     C                   ADD       SCOSS1        CNE(B)
112400970310     C                   ADD       SCOSS2        CNC(B)
112500970310     C                   ADD       SCOSS3        CNN(B)
112600090916     C                   ADD       SCOSS4        CNh(B)
112700970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
112800970122     C     SCOGBO        ANDEQ     'P'                                          CONS PARZIAL
112900970310     C                   ADD       SCOSS1        PRE(B)
113000970310     C                   ADD       SCOSS2        PRC(B)
113100970310     C                   ADD       SCOSS3        PRN(B)
113200090916     C                   ADD       SCOSS4        PRh(B)
113300970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
113400970122     C     SCOGBO        ANDEQ     'R'                                          RISERVE CONS
113500970310     C                   ADD       SCOSS1        RSE(B)
113600970310     C                   ADD       SCOSS2        RSC(B)
113700970310     C                   ADD       SCOSS3        RSN(B)
113800090916     C                   ADD       SCOSS4        RSh(B)
113900970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
114000970122     C     SCOGBO        ANDEQ     'G'                                          GIACENZE AP
114100970310     C                   ADD       SCOSS1        GAE(B)
114200970310     C                   ADD       SCOSS2        GAC(B)
114300970310     C                   ADD       SCOSS3        GAN(B)
114400090916     C                   ADD       SCOSS4        GAh(B)
114500970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
114600970122     C     SCOGBO        ANDEQ     'L'                                          LASCIATI AVV
114700970310     C                   ADD       SCOSS1        LAE(B)
114800970310     C                   ADD       SCOSS2        LAC(B)
114900970310     C                   ADD       SCOSS3        LAN(B)
115000090916     C                   ADD       SCOSS4        LAh(B)
115100970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
115200970122     C     SCOGBO        ANDEQ     'C'                                          CHIUSE TURNO
115300970310     C                   ADD       SCOSS1        CTE(B)
115400970310     C                   ADD       SCOSS2        CTC(B)
115500970310     C                   ADD       SCOSS3        CTN(B)
115600090916     C                   ADD       SCOSS4        CTh(B)
115700970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
115800970122     C     SCOGBO        ANDEQ     'N'                                          NON FATTE
115900970310     C                   ADD       SCOSS1        NFE(B)
116000970310     C                   ADD       SCOSS2        NFC(B)
116100970310     C                   ADD       SCOSS3        NFN(B)
116200090916     C                   ADD       SCOSS4        NFh(B)
116300970122     C     SCOSCO        WHENEQ    'D'                                          IN DISTINTA
116400970122     C     SCOGBO        ANDEQ     ' '
116500970310     C                   ADD       SCOSS1        IDE(B)
116600970310     C                   ADD       SCOSS2        IDC(B)
116700970310     C                   ADD       SCOSS3        IDN(B)
116800090916     C                   ADD       SCOSS4        IDh(B)
116900970122     C     SCOSCO        WHENEQ    'I'                                          IN CONSEGNA
117000970122     C     SCOGBO        ANDEQ     ' '
117100970310     C                   ADD       SCOSS1        ICE(B)
117200970310     C                   ADD       SCOSS2        ICC(B)
117300970310     C                   ADD       SCOSS3        ICN(B)
117400090916     C                   ADD       SCOSS4        ICh(B)
117500970122     C     SCOSCO        WHENEQ    'O'                                          NOINCONSEGNA
117600970122     C     SCOGBO        ANDEQ     ' '
117700970310     C                   ADD       SCOSS1        NCE(B)
117800970310     C                   ADD       SCOSS2        NCC(B)
117900970310     C                   ADD       SCOSS3        NCN(B)
118000090916     C                   ADD       SCOSS4        NCh(B)
118100970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
118200970122     C     SCOORE        ANDEQ     -999                                         IN ANTICIPO
118300970310     C                   ADD       SCOSS1        COE(1)
118400970310     C                   ADD       SCOSS2        COC(1)
118500970310     C                   ADD       SCOSS3        CON(1)
118600090916     C                   ADD       SCOSS4        COh(1)
118700970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
118800970122     C     SCOORE        ANDEQ     0                                            IN CONFORMIT
118900970310     C                   ADD       SCOSS1        COE(2)
119000970310     C                   ADD       SCOSS2        COC(2)
119100970310     C                   ADD       SCOSS3        CON(2)
119200090916     C                   ADD       SCOSS4        COh(2)
119300970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
119400970122     C     SCOORE        ANDEQ     999                                          IN RITARDO
119500970310     C                   ADD       SCOSS1        COE(3)
119600970310     C                   ADD       SCOSS2        COC(3)
119700970310     C                   ADD       SCOSS3        CON(3)
119800090918     C                   ADD       SCOSS4        COh(3)
119900970122     C                   ENDSL
120000970122     C*
120100970122    3C                   ENDIF
120200970122     C*
120300000703     C  N02KSCO22        READE     FNSCO03L                               30
120400000703     C   02KSCO21        READE     FNSCO03L                               30
120500970122   X2C                   ELSE
120600970122     C                   SETON                                        30
120700970122    2C                   ENDIF
120800970122    1C                   ENDDO
120900970122     C                   ENDSR
121000950124     C**************************************************************************
121100950124     C*    CONTO LE SPEDIZIONI CONSEGNATE
121200950124     C**************************************************************************
121300920604     C     CONT1         BEGSR
121400950124     C*
121500090917     C* Tipo servizio --> imposta indicatore
121600090917     c                   exsr      IndtipoServ
121700930907     C*
121800950123     C* RICHIAMO PGM DI CALCOLO AFFIDABILITA'
121900950227     C* SE MANCA LA DATA TEORICA LA CALCOLO
122000950227    1C     ARBDCE        IFEQ      0
122100970925     C                   CLEAR                   DS99
122200930908     C                   EXSR      CALDAT
122300950227   X1C                   ELSE
122400950227     C                   Z-ADD     ARBNCI        NUM3                            NON CONF.CONSEGNA
122500950227    1C                   ENDIF
122600930907     C*
122700930908     C* CERCO LO SCAGLIONE DEL RITARDO
122800930907     C                   EXSR      CALRIT
122900930907     C*
123000930812     C* C O N S E G N A T E
123100090916     C   10              ADD       1             CNE(B)                          "E"
123200090916     C   11              ADD       1             CNC(B)                          "C"
123300090916     C
123400090916     C   20              ADD       1             CNN(B)                          "D"
123500090916     C   21              ADD       1             CNh(B)                          "H"
123600970130     C*
123700970130     C* TROVATA ALMENO UNA SPEDIZIONE CONSEGNATA
123800970130     C                   MOVEL     'S'           WCONS             1
123900940117     C*
124000940117     C   99              MOVEL     'C'           STAFLG
124100940203     C   99              ADD       1             STAC              7 0
124200940117     C   99              EXCEPT    SPED                                          S P E D I Z
124300930907     C*
124400930907     C* IN    C O N S E G N A
124500090916     C   10              ADD       1             ICE(B)                          "E"
124600090916     C   11              ADD       1             ICC(B)                          "C"
124700090916     C
124800090916     C   20              ADD       1             ICN(B)                          "D"
124900090916     C   21              ADD       1             ICh(B)                          "H"
125000920604     C*
125100930812     C* C O N S E G N A B I L I
125200930907     C                   EXSR      CALCON
125300940208     C*
125400940208     C*  VEDO SE C'E' ANCHE QUALCHE CONSEGNA NON FATTA LO STESSO GIORNO
125500970417     C***********          EXSR CONT3
125600950124     C*
125700930907     C                   ENDSR
125800090917     C**************************************************************************
125900090917     C* Accendi indicatore a seconda del tipo servizio della bolla
126000090917     C**************************************************************************
126100090917     c     IndTipoSERV   BEGSR
126200090917     C                   SETOFF                                       1011
126300090917     C                   SETOFF                                       2021
126400090917
126500090917     C***  ARBTSP        COMP      'E'                                    10
126600090917     C***  ARBTSP        COMP      'C'                                    11
126700090917
126800090917     c                   select
126900090917     c                   when      arbtsp='E'
127000090917     c                   seton                                        10
127100090917     c                   when      arbtsp='C'
127200090917     c                   seton                                        11
127300090917     c                   when      arbtsp='H'
127400090917     c                   seton                                        21
127500090917     c                   other
127600090917     c                   seton                                        20
127700090917     c                   endsl
127800090917     c                   ENDSR
127900950124     C**************************************************************************
128000950124     C* CALCOLO POSIZIONE SCHIERA DELL'ANTICIPO/RITARDO
128100950124     C**************************************************************************
128200930907     C     CALRIT        BEGSR
128300950124     C*
128400940117     C                   Z-ADD     1             B                 3 0
128500930907     C     NUM3          LOOKUP    ORE(B)                                 32
128600930907     C*
128700930907    1C     *IN32         IFEQ      *OFF
128800930907     C                   Z-ADD     1             B
128900930907     C*
129000930907    2C     NUM3          IFGT      0
129100930907     C     999           LOOKUP    ORE(B)                                 32
129200930907     C*
129300930907   X2C                   ELSE
129400930907     C*
129500930907     C     -999          LOOKUP    ORE(B)                                 32
129600930907    2C                   ENDIF
129700930907    1C                   ENDIF
129800950124     C*
129900930907     C                   ENDSR
130000950124     C**************************************************************************
130100950124     C* CALCOLO LE SPEDIZIONI CONSEGNABILI
130200950124     C**************************************************************************
130300930907     C     CALCON        BEGSR
130400950124     C*
130500930907     C                   SELECT
130600930907     C* NEL PERIODO
130700930907     C     NUM3          WHENEQ    0
130800090916     C   10              ADD       1             COE(2)                          "E"
130900090916     C   11              ADD       1             COC(2)                          "C"
131000090916     C   20              ADD       1             CON(2)                          "D"
131100090916     C   21              ADD       1             COh(2)                          "H"
131200930907     C     NUM3          WHENGT    0
131300090916     C   10              ADD       1             COE(3)                          "E"
131400090916     C   11              ADD       1             COC(3)                          "C"
131500090916     C   20              ADD       1             CON(3)                          "D"
131600090917     C   21              ADD       1             COh(3)                          "H"
131700930907     C                   OTHER
131800090916     C   10              ADD       1             COE(1)                          "E"
131900090916     C   11              ADD       1             COC(1)                          "C"
132000090916     C   20              ADD       1             CON(1)                          "D"
132100090916     C   21              ADD       1             COh(1)                          "H"
132200930907     C                   END
132300930907     C*
132400930907     C                   ENDSR
132500950124     C**************************************************************************
132600950124     C*    CONTO IN ARB00F QUELLE CONSEGNABILI O IN DISTINTA
132700950124     C**************************************************************************
132800930812     C     CONT4         BEGSR
132900930908     C*
133000930908     C* NORMALE/ESPRESSO
133100090917     C***                SETOFF                                       1011
133200090917     C***  ARBTSP        COMP      'E'                                    10
133300090917     C***  ARBTSP        COMP      'C'                                    11
133400090917     C*
133500090917     C* Tipo servizio --> imposta indicatore
133600090917     c                   exsr      IndtipoServ
133700930908     C**
133800970606     C* SE C'E' NUMERO DISTINTA E'    "I N    D I S T I N T A "
133900930908     C**
134000970606     C     ARBNDC        IFGT      0
134100970606     C     ARBDDC        ANDGT     0
134200970606     C     ARBDDC        ANDLE     DS1ADA
134300090917     c
134400930908     C* DATA E ORA CALCOLO
134500970606     C                   Z-ADD     DS1ADA        D98DFC
134600970606     C                   Z-ADD     2359          D98OFC
134700970606     C* SE IL GIORNO E' OGGI, METTO L'ORARIO ESATTO
134800970606     C     DS1ADA        IFEQ      WDATE1
134900970606     C                   MOVEL     UTIME         D98OFC                         HH:MM
135000970606     C                   ENDIF
135100930908     C*
135200930908     C                   EXSR      CALDAT
135300930908     C* CERCO SCAGLIONE DEL RITARDO
135400930908     C                   EXSR      CALRIT
135500930908     C*
135600090916     C   10              ADD       1             IDE(B)                          "E"
135700090916     C   11              ADD       1             IDC(B)                          "C"
135800090916     C   20              ADD       1             IDN(B)                          "D"
135900090916     C   21              ADD       1             IDh(B)                          "H"
136000940117     C*
136100940117     C   99              MOVEL     'I'           STAFLG
136200940203     C   99              ADD       1             STAI              7 0
136300940117     C   99              EXCEPT    SPED                                          S P E D I Z
136400930908     C*
136500930908     C* IN    C O N S E G N A
136600090916     C   10              ADD       1             ICE(B)                          "E"
136700090916     C   11              ADD       1             ICC(B)                          "C"
136800090916     C   20              ADD       1             ICN(B)                          "D"
136900090916     C   21              ADD       1             ICh(B)                          "H"
137000930908     C* C O N S E G N A B I L I
137100930908     C                   EXSR      CALCON
137200930908     C                   GOTO      ENDCT4
137300930908    1C                   ENDIF
137400930908     C**
137500930908     C* CONTROLLO SE E' "N O N   C O N S E G N A T A"
137600930908     C**
137700930908     C                   EXSR      CONT3
137800940117     C*
137900940117     C*
138000930910     C*  16 ON - E' UNA NON CONSEGNATA O CONSEGNA PARZIALE ESCO
138100930908     C   16              GOTO      ENDCT4
138200990211     C**
138300990211     C     ARBLNP        CHAIN     AZORG01L                           30
138400020207     C  N30              MOVEL     orgde3        og143
138500020207     C  N30              MOVEL     §ogntw        LNPntw            3
138600020211     c   30              clear                   lnpntw
138700930908     C**
138800930908     C* CONTROLLO LE    "N O N   I N   C O N S E G N A"
138900930908     C**
139000930812     C* ESCLUDO SE:
139100930812     C* 1) E' BLOCCATA
139200930908    1C     ARBFBC        IFEQ      'B'
139300970818     C     ARBFBC        OREQ      '*'
139400930812     C* 2) C'E' CONTRASSEGNO E NON E' ABILITATO
139500000703     C**** ARBACC        OREQ      ' '
139600930907     C* 3) E' IN DISTINTA E DATA DISTINTA MINORE DATA DAL
139700970606     C****       ARBDDC    ORLT DS1ADA
139800970606     C****       ARBNDC    ANDGT0
139900940324     C* 4) SE E' UN FERMO DEPOSITO
140000940324     C     ARBFFD        ORNE      *BLANKS
140100950616     C* 5) SE DATA ARRIVO ULTIMO COLLO > DATA CONSEGNABILITA'
140200970818     C*    (BOLLA NON IMPORT)
140300970207     C     ARBDTI        ORGT      DS1ADA
140400020207     C     LNPntw        ANDEQ     'COR'
140500020207     C     ARBDTI        ORGT      DS1ADA
140600020207     C     LNPntw        ANDEQ     'MES'
140700930812     C                   GOTO      ENDCT4
140800950616    1C                   ENDIF
140900990326     C**
141000990326     C** 6) ESCLUDO SE C'E' C.A. APERTA SULLA SPEDIZIONE
141100990326     C                   CLEAR                   WESCA
141200040812      *
141300040812     c                   clear                   fidn12ds
141400040812     c                   eval      i12aas = arbaas
141500040812     c                   eval      i12lnp = arblnp
141600040812     c                   eval      i12nrs = arbnrs
141700040812     c                   eval      i12nsp = arbnsp
141800040812     c                   eval      i12fel = 'E'
141900040812     c                   eval      i12fan = 'E'
142000040812      *
142100040812     c                   call      'FIDN12R'
142200040812     c                   parm                    fidn12ds
142300040812      *
142400040812      * se non ci sono errori
142500040812     c                   if        o12err = ' '
142600040812      * se numero di CA trovate maggiore di zero
142700040812     c                   if        o12nca > 0
142800040812     c
142900040812     c                   z-add     *zeros        ii                2 0
143000040812     c     1             do        o12nca        ii
143100040812     c                   movea     skkey(ii)     dskey
143200040812     C                   Z-ADD     dsfil         dctfil
143300040812     C                   Z-ADD     dsnca         dctnca
143400040812     C                   Z-ADD     skdca(ii)     dctdca
143500040812     C                   Z-ADD     skdch(ii)     dctdch
143600040812     C                   movea     skcch(ii)     dctcch
143700040812      * se CA aperta imposto il flag e vado a fine
143800040812     c                   If        dctdch = 0
143900040812     c                   eval      wesca = 'S'
144000040812     c                   leave
144100040812     c                   else
144200040812     c                   eval      wesca = 'C'
144300040812     c                   endif
144400040812     C                   ENDDO
144500040812      *
144600040812     c                   endif
144700040812     c                   endif
144800990326     C*
144900990326     C* -SE ESISTE C.A. APERTA  --> ESCO
145000990326    1C     WESCA         IFEQ      'S'
145100990326     C                   GOTO      ENDCT4
145200990326    1C                   ENDIF
145300970818     C*
145400970818     C* DECODIFICA CODICE BOLLA
145500970818     C                   MOVEL     '3A'          COD
145600970818     C                   MOVEL     *BLANKS       KEY
145700970818     C                   MOVEL     ARBCBO        KEY
145800970818     C     KTAB          CHAIN(N)  TABEL                              30
145900970818     C  N30              MOVEL     TBLUNI        DS3A
146000970818     C   30              CLEAR                   DS3A
146100970818     C                   MOVEL     §3ADES        P0DCBO            5
146200990211     C* 6) BOLLA IMPORT:
146300970818     C* ESCLUDO SE DATA ARRIVO ULTIMO COLLO > DATA CONSEGNABILITA
146400970818     C* SOLO SE SI TRATTA DI BOLLA ASSEGNATO CON FATTURA IMMEDIATA
146500020207    1C     LNPntw        IFEQ      'EEX'
146600020207    1C     LNPntw        oreq      'EUP'
146700020207    1C     LNPntw        oreq      'DPD'
146800020207    1C     LNPntw        oreq      'FED'
146900970818     C**
147000970818    2C     ARBDTI        IFGT      DS1ADA
147100970818     C     ARBDTI        OREQ      0
147200970818     C*
147300970818    3C     ARBDTI        IFGT      DS1ADA
147400970818     C     §3AFCA        ANDGT     0
147500970818     C                   GOTO      ENDCT4
147600970818    3C                   ENDIF
147700970818     C*
147800970818     C                   MOVE      ARBKSC        W0040             4 0
147900970818    3C     W0040         IFEQ      9999
148000991104     C                   MOVEL     '1'           KTRC
148100991104     C     KARB6         CHAIN     FIAR601L                           13
148200970818     C** PER ORA ESCO SOLO SE DTI E' CALCOLATO E > DELLA DATA ELABORAZ
148300970818     C*  PERCHE' DTI POTREBBE ESSERE A 0 MA TUTTE LE DATE DI ARRIVO
148400990326     C*  PRESENTI PERCHE' ANCORA NON E' STATO CALCOLATO
148500970818    4C     ARBDTI        IFGT      DS1ADA
148600970818     C     *IN13         ANDEQ     *OFF
148700970818     C     AR6DFT        ANDGT     0
148800970818     C                   GOTO      ENDCT4
148900970818    4C                   ENDIF
149000970818    3C                   ENDIF
149100970818    2C                   ENDIF
149200970818    1C                   END
149300970818     C**
149400990326     C* 7) SE NON E' COMPLETAMENTE ARRIVATA:
149500970818     C*    O E' ARRIVATA DOPO  --> LEGGO IL DETTAGLIO COLLI
149600990329     C* O SE HA IL C/A DEVO CONTROLLARE IL DETT.COLLI
149700970818     C                   CLEAR                   WCONS1
149800970818    1C     ARBDTI        IFEQ      0
149900970818     C     ARBDTI        ORGT      DS1ADA
150000990326     C     WESCA         OREQ      'C'
150100940329     C                   SETOFF                                       17
150200970818     C                   CLEAR                   KDAM1
150300950123     C     KARB3         SETLL     FNART01L
150400950123     C     KARB3         READE     FNART01L                               30
150500940329     C*
150600940329    2C     *IN30         DOWEQ     *OFF
150700040527     c                   clear                   Wartdam
150800040527     c
150900040527    3c                   if        artdam>0
151000040527     c                   eval      Wartdam=artdam
151100040527   x3c                   else
151200040527     c
151300040527     c* Memorizzo la data di arrivo o l'ipotetico arrivo se collo partito
151400040527     c* su FGV abbinato di ida non ancora chiuso: come data di arrivo
151500040527     c*  prendo la data foglio arrivi, solo se richiesto
151600040608    4c                   if        artdam=0  and artdfv>0  and ds1fl5='S'
151700040908     c* Se si tratta di bolla LOCALE, non esiste abbinamento quindi
151800040908     c* cerco di desumere la data del foglio arrivi per vedere se è
151900040908     c*  chiuso o perto
152000040908    5c                   if        arbtfp=arbtfa
152100050706     c                   clear                   wtipolancio
152200070420     c                   eval      kdataloc=artdfv
152300050706     c                   exsr      BOLLALOC
152400040908   X5c                   else
152500050706     C
152600040527     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
152700040527     C                   CLEAR                   fnlv53ds
152800040527     C                   MOVEL     'A'           D53TFO
152900040527     C                   MOVE      artNFV        D53NFV
153000040608     C                   MOVE      arbtfp        D53FGS
153100040527     C                   MOVEL     'B'           D53ABB
153200040527     C                   MOVE      ARBLNA        D53LNA
153300040527     C                   MOVE      ARBTFA        D53TFA
153400040527     C                   CALL      'FNLV53R'
153500040527     C                   PARM                    fnlv53ds
153600040527     c*
153700040908    6c                   if        d53err=*blanks and d53nfa>0
153800040527     c* verifico se fogli arrivi di abbinamento già chiuso
153900040527     C     KFVV          CHAIN     FNFVV000
154000040527     C                   IF        %FOUND(FNFVV01L)
154100040527     C                   IF        FVVFCF=' '
154200040527     C                   EVAL      Wartdam=D53DFA
154300040527     C                   ENDIF
154400040527     C                   ENDIF
154500040908    6c                   endif
154600040908    5c                   endif
154700040527     c
154800040527    4c                   endif
154900040527    3c                   endif
155000040527     c
155100040526     C* SE NON ARRIVATO O ARRIVATO CON DATA > (E'COME SE FOSSE A 0 QUEL
155200970818     C*  GIORNO)
155300040527    3C     Wartdam       IFEQ      0
155400040527     C     Wartdam       ORGT      DS1ADA
155500990329     C* CONTROLLO SE E' SU C.A. MANCANZA O AVARIA RESA O DISTRUTTA
155600990329     C*  NEL QUAL CASO IGNORO PERCHE' IL COLLI NON CI SARA' MAI
155700990329     C                   EXSR      CTRCA
155800990329     C*
155900990329     C     WMANCA        IFEQ      ' '
156000970818     C                   SETON                                        17
156100020207     C     LNPntw        IFEQ      'COR'
156200020207     C     LNPntw        oreq      'MES'
156300020207     C     LNPntw        oreq      'PPT'
156400970818     C                   SETON                                        30
156500970818     C                   END
156600990329     C                   END
156700990329     C*
156800940329   X3C                   ELSE
156900990329     C* MEMORIZZO CHE ALMENO UN COLLO E' ARRIVATO PRIMA O NEL GIORNO
157000970818     C*  DELLA DATA DI ELABORAZIONE
157100990326    4C     ARTDCM        IFEQ      0
157200970819     C     KDAM1         ANDEQ     *ZEROS
157300990326     C     ARTDCM        ORGT      DS1ADA
157400970818     C     KDAM1         ANDEQ     *ZEROS
157500990326     C*
157600990326     C** NON MEMORIZZO SE IL COLLO NON CONSEGNATO E' RELATIVO AD
157700990326     C**  AVARIE RESE O DISTRUTTE
157800990329     C                   EXSR      CTRCA
157900990329    5C     WMANCA        IFEQ      ' '
158000990326     C                   SETON                                        33
158100990326    5C                   ENDIF
158200990326     C**
158300040527     C   33              Z-ADD     Wartdam       KDAM1
158400970818     C     *LIKE         DEFINE    ARTDAM        KDAM1
158500990329    4C                   END
158600940329    3C                   ENDIF
158700970818     C**
158800970818     C  N30KARB3         READE     FNART01L                               30
158900940329    2C                   ENDDO
159000990326     C*
159100970818     C* SE BOLLA IMPORT NON LA STAMPO SOLO SE ENTRO LA DATA ELABORA-
159200970818     C* ZIONE NON E' ARRIVATO NEMMENO UN COLLO (KDAM1=0)
159300970818     C* STAMPO INVECE LE SPEDIZIONI ARRIVATE PARZIALMENTE
159400990329     C* SE PRESENTE C.A. CHIUSA: ESCO SE I SOLI COLLI NON CONSEGNATI
159500990329     C*  SONO QUELLI RELATIVI A MANCANZE O AVARIE RESE/DISTRUTTE
159600970818    3C     KDAM1         IFEQ      *ZEROS
159700970818     C                   GOTO      ENDCT4
159800040526    3C                   ENDIF
159900990326     C**
160000970818     C* E' UNA ARRIVO PARZIALE: STAMPO SOLO SE NON E' FATTURA IMMEDIATA
160100970818     C*  E SE NON HA IN CONTRASSEGNO
160200020207    2C     LNPntw        IFEQ      'EEX'
160300020207    2C     LNPntw        oreq      'EUP'
160400020207    2C     LNPntw        oreq      'DPD'
160500020207    2C     LNPntw        oreq      'FED'
160600970818    3C     *IN17         IFEQ      *ON
160700970818     C     ARBDTI        ANDEQ     0
160800970818     C*
160900970818    4C     §3AFCA        IFGT      0
161000970818     C     W0040         OREQ      9999
161100970818     C     *IN13         ANDEQ     *OFF
161200970818     C     AR6DFT        ANDGT     0
161300970818     C                   GOTO      ENDCT4
161400970818    4C                   ENDIF
161500970818    3C                   ENDIF
161600970818     C* MEMORIZZO CHE SI TRATTA DI SPEDIZIONE CONSEGNABILE ANCHE SE
161700970818     C* E' UN ARRIVO PARZIALE
161800970818     C   17              MOVE      '1'           WCONS1            1
161900970818     C**
162000970818   X2C                   ELSE
162100940329     C*
162200940329     C   17              GOTO      ENDCT4
162300970818    2C                   END
162400970818    1C                   ENDIF
162500930907     C*
162600961108     C* SE LA BOLLA E' GIACENTE ED HA UNA FIGLIA CON LNP = LNA IN
162700970925     C*   CONSEGNA NON RICHIAMO PGM TNSD99R PERCHE' NON E' CONSEGNABILE
162800961108    1C     ARBFBC        IFEQ      'G'
162900961108     C     ARBFBC        OREQ      '*'
163000050331     C     KARB3         SETLL     Tigcp21L                               33
163100961108    2C     *IN33         IFEQ      *ON
163200961112     C* SALVO I CAMPI DELLA BOLLA
163300961112     C                   MOVE      ARBDS         WSAVDS
163400961108     C                   CLEAR                   WTROV
163500961108     C*
163600961108     C     KARB3         SETLL     FNLBL02L
163700961108     C     KARB3         READE     FNLBL02L                               37
163800961108    3C     *IN37         DOWEQ     *OFF
163900961108    4C     LBLLPN        IFEQ      LBLLAN
164000961108     C     KARB1         CHAIN     FNARB01L                           38
164100961108    5C     *IN38         IFEQ      *OFF
164200961108     C     ARBNDC        ANDGT     0
164300961108     C                   SETON                                        37
164400961108     C                   MOVEL     'S'           WTROV             1
164500961108    5C                   ENDIF
164600961108    4C                   ENDIF
164700961108     C  N37KARB3         READE     FNLBL02L                               37
164800961108    3C                   ENDDO
164900961108     C*
165000961112     C* RIMETTO I CAMPI SALVATI IN PRECEDENZA
165100961112     C                   MOVE      WSAVDS        ARBDS
165200961112     C*
165300961108    3C     WTROV         IFEQ      'S'
165400961108     C                   GOTO      ENDCT4
165500961108    3C                   ENDIF
165600961108    2C                   ENDIF
165700961108    1C                   ENDIF
165800050706     c* Se richiesto, escludo supermercati e appuntamenti arrivati nella dat
165900050706     c*  a richiesta  se privi della cons. richiesta
166000141217    1c                   if        ds1fl6='S' and arbdcr=0
166100141217    2c                   if        arbtc1='S' or arbtc1='A' or arbtc2='S' or
166200050706     c                             arbtc2='A'
166300141217     c* Solo se non ha una dispo di dirottamento escludo
166400141217     c                   exsr      dispoDIR
166500141217    3c                   if        OLRY09ESIT<>'1'
166600050706     c* Per bolla locale, desumo
166700050706     c                   if        arbtfp=arbtfa
166800050706     c                   eval      wtipolancio='SA'
166900070420     c                   eval      kdataloc=arbdbr
167000070420     c
167100050706     c                   exsr      BOLLALOC
167200050706     c                   clear                   d53err
167300050706     c                   eval      d53dfa=wartdam
167400050706     c                   else
167500050706     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
167600050706     C                   CLEAR                   fnlv53ds
167700050706     C                   MOVEL     'A'           D53TFO
167800050706     C                   MOVE      arbNFV        D53NFV
167900050706     C                   MOVE      arbtfp        D53FGS
168000050706     C                   MOVEL     'B'           D53ABB
168100050706     C                   MOVE      ARBLNA        D53LNA
168200050706     C                   MOVE      ARBTFA        D53TFA
168300050706     C                   CALL      'FNLV53R'
168400050706     C                   PARM                    fnlv53ds
168500050706     c                   endif
168600050706     c*
168700141217    4c                   select
168800060314    6c                   when      d53err=*blanks and d53dfa=ds1ada
168900050706     C                   GOTO      ENDCT4
169000060314     c
169100060315    6c                   when      d53err=*blanks and d53dfa<ds1ada  and
169200060315     c                             d53dfa>0
169300060314     c* controllo se giorni festivi da d53dfa al giorno prima di ds1ada
169400060314     c     *iso          movel     d53dfa        dataiso
169500060314     c     *iso          movel     ds1ada        dataisofine
169600060314     c                   eval      wfest='S'
169700060314     c                   dow       dataiso<dataisofine and WFEST='S'
169800060314     c                   movel     dataiso       kdata             8 0
169900060314     c                   exsr      CTRFESTA
170000060314     c                   adddur    1:*d          dataiso
170100060314     c                   enddo
170200060314     c
170300060314     c                   if        wfest='S'
170400060314     c                   eval      wfest=' '
170500060314     C                   GOTO      ENDCT4
170600060314     c                   endif
170700060314     c
170800141217    4C                   ENDSL
170900961108     C*
171000050706    3C                   ENDIF
171100141217    2C                   ENDIF
171200141217    1C                   ENDIF
171300050706     C*
171400970207     C                   Z-ADD     DS1ADA        D98DFC
171500950127     C                   Z-ADD     2359          D98OFC
171600950127     C* SE IL GIORNO E' OGGI, METTO L'ORARIO ESATTO
171700970207     C     DS1ADA        IFEQ      WDATE1
171800950127     C                   MOVEL     UTIME         D98OFC                         HH:MM
171900950127     C                   ENDIF
172000930908     C*
172100930908     C                   EXSR      CALDAT
172200940117     C*
172300940117     C                   EXSR      CALRIT
172400100218     c
172500100218     c* Se bolla con partic.consegna ATTESA verifico se da stampare a meno
172600100218     c*  in base all'anticipo che ha.
172700100218     c                   if        ds1fl7='S' and arbgma<>*blanks
172800100218     C                   MOVEL     '7R'          COD
172900100218     C                   MOVEL(P)  arbgma        KEY
173000100218     C     KTAB          CHAIN(N)  TABEL                              31
173100100218     C  N31              MOVEL     TBLUNI        DS7R
173200100218     C   31              CLEAR                   DS7R
173300100218     c                   if        §7RATT='S' and d98nci <=-24
173400100218     C                   GOTO      ENDCT4
173500100218     c                   endif
173600100218     c                   endif
173700110927     c*
173800110927     c* Se c'e' Perfect Order --> lo stampo e vedo se è abilitato
173900110927     c                   setoff                                       1928
174000110927     c                   clear                   StPorder         16
174100130221     c                   if        arbxco >*zeros
174200110927     c                   seton                                        19
174300110927     c                   exsr      POrder
174400110927     c                   if        olr78conso='S'
174500110927     c                   eval      stPorder='  Consegnabile  '
174600110927     c                   else
174700110927     c                   eval      stPorder='NON Consegnabile'
174800110927     c
174900110927     c* se non consegnabile tolgo 24 ore alla delivery per andare in anticipo
175000110927     c                   eval      num3=num3-24
175100110927     c                   exsr      CALRIT
175200110927     c                   endif
175300110927     c
175400110927     c                   endif
175500110927     c
175600930908     C*
175700940117    1C     D98OCI        IFGT      0
175800930908     C*
175900940117     C* N O N    IN    C O N S E G N A
176000090916     C   10              ADD       1             NCE(B)                          "E"
176100090916     C   11              ADD       1             NCC(B)                          "C"
176200090916     C   20              ADD       1             NCN(B)                          "D"
176300090916     C   21              ADD       1             NCh(B)                          "H"
176400940117     C*
176500940117     C   99              MOVEL     'D'           STAFLG            1
176600940203     C   99              ADD       1             STAD              7 0
176700940117     C   99              EXCEPT    SPED                                          S P E D I Z
176800940530     C*
176900940530     C* STAMPO LISTA SPEDIZIONI NON IN CONSEGNA
177000970225     C  N02DS1FL1        IFEQ      'L'
177100970225     C     DS1FL1        OREQ      'E'
177200970403     C     DS1FL3        OREQ      'L'
177300940530     C                   EXSR      STASNC
177400940530     C                   ENDIF
177500940530     C*
177600930908     C* C O N S E G N A B I L I
177700930908     C                   EXSR      CALCON
177800940117    1C                   ENDIF
177900930812     C*
178000990329     C     ENDCT4        ENDSR
178100990329     C**************************************************************************
178200990329     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
178300990329     C**************************************************************************
178400990329     C     CTRCA         BEGSR
178500990329     C                   CLEAR                   WMANCA
178600990329    1C     WESCA         IFEQ      'C'
178700990329     C**
178800990329     C     KART          CHAIN     FNDCD02L                           33
178900990329    2C     *IN33         IFEQ      *OFF
179000990330     C     DCTATB        ANDEQ     ' '
179100990329     C     KDCT          CHAIN     FNDCT01L                           33
179200050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
179300050221     c                   If        Not *In33 and dctdt2 > 0
179400050221     c                   Eval      *In33 = *On
179500050221     c                   EndIf
179600990329     C*
179700990329    3C     *IN33         IFEQ      *OFF
179800990329     C     DCTDCH        ANDEQ     DCDDCH
179900990329     C     DCDDCH        OREQ      0
180000990329     C* TESTO PRIMA LA CAUSALE DI CHIUSURA PER VEDERE SE MI INDICA LA
180100990329     C*  PRESENZA DELLA MERCE
180200990329     C                   CLEAR                   DSBS02
180300990329     C                   MOVEL     'C'           T02MOD
180400990329     C                   MOVEL     KNSIF         T02SIF
180500990329     C                   MOVEL     'CCH'         T02COD
180600990329     C                   MOVEL     DCTCCH        T02KE1
180700990329     C                   CALL      'TIBS02R'
180800990329     C                   PARM                    KPJBA
180900990329     C                   PARM                    DSBS02
181000990329     C**
181100990329    4C     T02ERR        IFEQ      ' '
181200990330     C                   MOVEL     T02UNI        DCCH
181300990329     C                   ELSE
181400990329     C                   MOVEL     'S'           §CHIMA
181500990329    4C                   ENDIF
181600990329     C*
181700990329    4C     §CHIMA        IFEQ      ' '
181800990329     C                   CLEAR                   DSBS02
181900990329     C                   MOVEL     'C'           T02MOD
182000990329     C                   MOVEL     KNSIF         T02SIF
182100990329     C                   MOVEL     'TAD'         T02COD
182200990329     C                   MOVEL     DCTTAD        T02KE1
182300990329     C                   CALL      'TIBS02R'
182400990329     C                   PARM                    KPJBA
182500990329     C                   PARM                    DSBS02
182600990329     C**
182700990329    5C     T02ERR        IFEQ      ' '
182800990329     C                   MOVEL     T02UNI        DTAD
182900990329     C                   ELSE
183000990329     C                   MOVEL     'M'           §TARGR
183100990329    5C                   ENDIF
183200990329     C*
183300990329     C* AVARIA RESA O DISTRUTTA
183400990329    5C     §TARGR        IFEQ      'V'
183500990329    6C     DCTDSD        IFEQ      'D'
183600990330     C     DCTDSD        OREQ      'R'
183700990329     C                   MOVEL     'V'           WMANCA            1
183800990329    6C                   ENDIF
183900990329    5C                   ENDIF
184000990329     C* MANCANZA
184100990329    5C     §TARGR        IFEQ      'M'
184200990329     C                   MOVEL     'M'           WMANCA            1
184300990329    5C                   ENDIF
184400990329     C*
184500990329    4C                   ENDIF
184600990329    3C                   ENDIF
184700990329    2C                   ENDIF
184800990329    1C                   ENDIF
184900990329     C*
185000990329     C                   ENDSR
185100950124     C**************************************************************************
185200950124     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
185300950124     C**************************************************************************
185400930908     C     CALDAT        BEGSR
185500950123     C*
185600930908     C                   MOVEL     ' '           D98TLA                         *TIPO LANCIO
185700930908     C                   MOVEL     'A'           D98TBO                         *TIPO BOLLA
185800950123     C                   Z-ADD     ARBAAS        D98AAS
185900950123     C                   Z-ADD     ARBLNP        D98LNP
186000950123     C                   Z-ADD     ARBNRS        D98NRS
186100950123     C                   Z-ADD     ARBNSP        D98NSP
186200970925     C                   CALL      'TNSD99R'
186300970925     C                   PARM                    DS99
186400930908     C*
186500950123     C                   Z-ADD     D98NCI        NUM3              3 0
186600950124     C*
186700930908     C                   ENDSR
186800950124     C**************************************************************************
186900950124     C* CONTROLLO LE NON CONSEGNATE DAGLI EVENTI
187000950124     C**************************************************************************
187100930908     C     CONT3         BEGSR
187200950124     C*
187300970417     C* PER OGNI BOLLA LEGGO SOLTANTO L'ULTIMO EVENTO
187400940208     C                   SETOFF                                       16
187500970207     C                   Z-ADD     DS1ADA        NUM6A
187600950123     C                   Z-ADD     ARBAAS        AR§AAS
187700970417     C                   MOVEL     *HIVAL        NUMORA
187800970417     C                   CLEAR                   WWRT
187900990805     C     KARB22        SETLL     FNEVB000                           32
188000990805     C     KARB2         READPE    FNEVB000                               32
188100091112     c                   seton                                        35
188200091112     c* Escludo gli eventi particolari
188300091112     c                   dow       not *in32 and *in35
188400091112     c     evbcev        lookup    cep                                    35
188500091112     c
188600091112     C   35KARB2         READPE    FNEVB000                               32
188700091112     c                   enddo
188800930908     C*
188900970417    1C     *IN32         IFEQ      *OFF
189000940208     C                   SETOFF                                       05
189100940208     C* DATA E ORA CALCOLO
189200950124     C                   Z-ADD     EVBDEV        D98DFC                          DATA EVENTO
189300950124     C                   Z-ADD     EVBHEV        D98OFC                          ORA  EVENTO
189400940208     C                   EXSR      CALDAT
189500940208     C*
189600940208     C* CERCO SCAGLIONE DEL RITARDO
189700940208     C                   EXSR      CALRIT
189800930908     C*
189900930908     C                   MOVEL     '2A'          COD
190000930908     C                   MOVEL     *BLANKS       KEY
190100930908     C                   MOVEL     EVBCEV        KEY
190200970124     C     KTAB          CHAIN(N)  TABEL                              30
190300930908     C   30              CLEAR                   DS2A
190400930908     C  N30              MOVEL     TBLUNI        DS2A
190500940208     C*
190600930908     C* TIPO CONSEGNA
190700970417    2C     §2AFTC        IFNE      *BLANKS
190800930908     C*
190900930908     C* SOLO SE RICHIESTA UNA DATA SCRIVO LE NON CONSEGNATE
191000930908     C                   SELECT
191100970417    3C     §2AFTC        WHENEQ    'G'
191200940208     C   99              MOVEL     'G'           STAFLG
191300940208     C   99              ADD       1             STAG              7 0
191400970121     C   10              ADD       1             GAE(B)                         GIAC APERTE
191500970121     C   11              ADD       1             GAC(B)
191600090916     C   20              ADD       1             GAN(B)
191700090916     C   21              ADD       1             GAh(B)
191800970417     C                   MOVEL     'S'           WWRT              1
191900970417    3C     §2AFTC        WHENEQ    'A'                                          LASC AVVISO
192000940208     C   99              MOVEL     'A'           STAFLG
192100940208     C   99              ADD       1             STAA              7 0
192200970121     C   10              ADD       1             LAE(B)
192300970121     C   11              ADD       1             LAC(B)
192400090916     C   20              ADD       1             LAN(B)
192500090916     C   21              ADD       1             LAh(B)
192600970417     C                   MOVEL     'S'           WWRT              1
192700930908     C*
192800970417   X3C                   OTHER
192900970417    4C     §2ACPT        IFEQ      'S'                                          CHIUSO TURNO
193000940208     C   99              MOVEL     'T'           STAFLG
193100940208     C   99              ADD       1             STAT              7 0
193200970121     C   10              ADD       1             CTE(B)
193300970121     C   11              ADD       1             CTC(B)
193400090916     C   20              ADD       1             CTN(B)
193500090916     C   21              ADD       1             CTh(B)
193600970417     C                   MOVEL     'S'           WWRT              1
193700970417   X4C                   ELSE
193800940208     C   99              MOVEL     'N'           STAFLG
193900940208     C   99              ADD       1             STAN              7 0
194000970121     C   10              ADD       1             NFE(B)                         NON FATTE
194100970121     C   11              ADD       1             NFC(B)
194200090916     C   20              ADD       1             NFN(B)
194300090916     C   21              ADD       1             NFh(B)
194400970417     C                   MOVEL     'S'           WWRT              1
194500970417    4C                   ENDIF
194600970417    3C                   ENDSL
194700940117     C*
194800970417   X2C                   ELSE
194900940208     C*
195000940208     C* SE CONSEGNA PARZIALE COME SE FOSSE CONSEGNATA
195100940208     C                   SELECT
195200970417    3C     §2ATIP        WHENEQ    'P'
195300930910     C*
195400930910     C* C O N S E G N A T E
195500090916     C   10              ADD       1             CNE(B)
195600090916     C   11              ADD       1             CNC(B)
195700090916     C   20              ADD       1             CNN(B)
195800090916     C   21              ADD       1             CNh(B)
195900940208     C* TOTALE A PARTE COME C.PARZIALE
196000970121     C   10              ADD       1             PRE(1)                          CONS PARZ
196100970121     C   11              ADD       1             PRC(1)
196200090916     C   20              ADD       1             PRN(1)
196300090916     C   21              ADD       1             PRh(1)
196400970417     C                   MOVEL     'S'           WWRT              1
196500940117     C*
196600940117     C   99              MOVEL     'P'           STAFLG
196700940203     C   99              ADD       1             STAP              7 0
196800940208     C*
196900940208     C* RISERVA CONSEGNA
197000970417    3C     §2ATIP        WHENEQ    'R'
197100940208     C   99              MOVEL     'R'           STAFLG
197200940208     C   99              ADD       1             STAR              7 0
197300970121     C   10              ADD       1             RSE(1)                         RISERVA CONS
197400970121     C   11              ADD       1             RSC(1)
197500090916     C   20              ADD       1             RSN(1)
197600090916     C   21              ADD       1             RSh(1)
197700970417     C                   MOVEL     'S'           WWRT              1
197800940208     C* 05 ON - NON DEVO CONTEGGIARLA COME IN CONSEGNA (GIA' FATTO)
197900940208     C                   SETON                                        05
198000091117     c
198100091117     C* Spedizioni Ripristinate --> tra  le "Non fatte"
198200091117    3C     §2ATIP        WHENEQ    'S'
198300091117     C   99              MOVEL     'N'           STAFLG
198400091117     C   99              ADD       1             STAN
198500091117     C   10              ADD       1             NFE(B)                         NON FATTE
198600091117     C   11              ADD       1             NFC(B)
198700091117     C   20              ADD       1             NFN(B)
198800091117     C   21              ADD       1             NFh(B)
198900091117     C                   MOVEL     'S'           WWRT
199000970417    3C                   ENDSL
199100970417    2C                   ENDIF
199200091117     c
199300970417     C* EVENTO DA CONTEGGIARE
199400970417    2C     WWRT          IFEQ      'S'
199500940208     C   99              EXCEPT    SPED
199600940208     C     *IN05         IFEQ      *OFF
199700940208     C* IN    C O N S E G N A
199800090916     C   10              ADD       1             ICE(B)
199900090916     C   11              ADD       1             ICC(B)
200000090916     C   20              ADD       1             ICN(B)
200100090916     C   21              ADD       1             ICh(B)
200200940208     C*
200300940208     C* C O N S E G N A B I L I
200400940208     C                   EXSR      CALCON
200500940208     C                   SETON                                          16
200600940208     C                   ENDIF
200700930910     C*
200800970417    2C                   ENDIF
200900970417    1C                   ENDIF
201000930908     C*
201100930908     C                   ENDSR
201200950124     C**************************************************************************
201300970417     C* CONTROLLO LE CONSEGNE PARZIALI E RISERVE SE DATA DAL AL
201400950124     C**************************************************************************
201500940324     C     CONT3P        BEGSR
201600090917     c
201700090917     C* Tipo servizio --> imposta indicatore
201800090917     c                   exsr      IndtipoServ
201900940324     C*
202000970207     C                   Z-ADD     DS1ADA        NUM6A
202100950123     C                   Z-ADD     ARBAAS        AR§AAS
202200990805     C     KARB2         SETLL     FNEVB000
202300990805     C     KARB4         READE     FNEVB000                               32
202400940324     C*
202500940324    1C     *IN32         DOWEQ     *OFF
202600950124    2C     EVBDEV        IFGT      CO1DAA
202700940324     C                   SETON                                        32
202800940324   X2C                   ELSE
202900940324     C*
203000940324     C                   MOVEL     '2A'          COD
203100940324     C                   MOVEL     *BLANKS       KEY
203200940324     C                   MOVEL     EVBCEV        KEY
203300970124     C     KTAB          CHAIN(N)  TABEL                              30
203400940324     C   30              CLEAR                   DS2A
203500940324     C  N30              MOVEL     TBLUNI        DS2A
203600940324     C*
203700940324     C* TIPO CONSEGNA
203800970417     C* CONSIDERO SOLO CONSEGNE PARZIALI E RISERVE
203900970417    3C     §2ATIP        IFEQ      'P'
204000970417     C     §2ATIP        OREQ      'R'
204100940324     C* DATA E ORA CALCOLO
204200950406     C                   Z-ADD     EVBDEV        D98DFC                          DATA EVENTO
204300950410     C                   MOVEL     EVBHEV        D98OFC                          ORA  EVENTO
204400940324     C                   EXSR      CALDAT
204500940324     C*
204600940324     C* CERCO SCAGLIONE DEL RITARDO
204700940324     C                   EXSR      CALRIT
204800940324     C*
204900940324     C* SE CONSEGNA PARZIALE COME SE FOSSE CONSEGNATA
205000940324    4C     §2ATIP        IFEQ      'P'
205100940324     C*
205200940324     C* C O N S E G N A T E
205300090916     C   10              ADD       1             CNE(B)                          "E"
205400090916     C   11              ADD       1             CNC(B)                          "C"
205500090916     C   20              ADD       1             CNN(B)                          "D"
205600090916     C   21              ADD       1             CNh(B)                          "H"
205700940324     C* TOTALE A PARTE COME C.PARZIALE
205800970121     C   10              ADD       1             PRE(1)                          CONS PARZ
205900970121     C   11              ADD       1             PRC(1)
206000090916     C   20              ADD       1             PRN(1)
206100090916     C   21              ADD       1             PRh(1)
206200940324     C*
206300940324     C   99              MOVEL     'P'           STAFLG
206400940324     C   99              ADD       1             STAP              7 0
206500940324     C*
206600940324   X4C                   ELSE
206700940324     C* RISERVA CONSEGNA
206800940324     C   99              MOVEL     'R'           STAFLG
206900940324     C   99              ADD       1             STAR              7 0
207000970121     C   10              ADD       1             RSE(1)                         RISERVA CONS
207100970121     C   11              ADD       1             RSC(1)
207200090916     C   20              ADD       1             RSN(1)
207300090916     C   21              ADD       1             RSh(1)
207400940324    4C                   ENDIF
207500940324    3C                   ENDIF
207600940324     C*
207700940324     C   99              EXCEPT    SPED
207800940324    2C                   ENDIF
207900940324     C*
208000990805     C  N32KARB4         READE     FNEVB000                               32
208100940324    1C                   ENDDO
208200940324     C*
208300940324     C                   ENDSR
208400950124     C**************************************************************************
208500950124     C*    STAMPA STATISTICA
208600950124     C**************************************************************************
208700920605     C     STAMPA        BEGSR
208800950124     C*
208900940117     C   OF              EXCEPT    TESTA
209000940117     C   OF              SETOFF                                       OF
209100940117     C*
209200930908     C                   CLEAR                   CONP1
209300930908     C                   CLEAR                   CONP2
209400930908     C*
209500930908     C* SPEDIZIONI IN CONSEGNA
209600090916     C                   XFOOT     ICE           GENICE            6 0           "E"
209700090917     C                   XFOOT     ICh           GENICH            6 0           "H"
209800090916     C                   XFOOT     ICN           GENICN            6 0           "D"
209900090916     C                   XFOOT     ICC           W0090                           "C"
210000970121     C                   ADD       W0090         GENICN
210100930908     C     GENICN        ADD       GENICE        GENIC             7 0           TOT CONSEG
210200090917     C                   ADD       GENICH        GENIC             7 0           TOT CONSEG
210300930908     C* SPEDIZIONI CONSEGNABILI
210400090916     C                   XFOOT     COE           GENCOE            6 0           "E"
210500090917     C                   XFOOT     COh           GENCOH            6 0           "H"
210600090916     C                   XFOOT     CON           GENCON            6 0           "D"
210700090916     C                   XFOOT     COC           W0090                           "C"
210800090916     C                   ADD       W0090         GENCON                          D+C
210900930908     C     GENCON        ADD       GENCOE        GENCO             7 0           TOT CONSEG
211000090917     C                   ADD       GENCOh        GENCO             7 0           TOT CONSEG
211100930908     C**
211200930908     C* SPEDIZIONI   C O N S E G N A T E
211300930908     C**
211400090916     C                   XFOOT     CNE           GENCNE            6 0           "E"
211500090917     C                   XFOOT     CNh           GENCNH            6 0           "H"
211600090916     C                   XFOOT     CNN           GENCNN            6 0           "D"
211700090916     C                   XFOOT     CNC           W0090                           "C"
211800090916     C                   ADD       W0090         GENCNN                          D+C
211900930812     C     GENCNN        ADD       GENCNE        GENCN             7 0           TOT CONSEG
212000090917     C                   ADD       GENCNh        GENCN             7 0           TOT CONSEG
212100930908     C*
212200930908     C* % CONSEGNATE        SU TOT IN CONSEGNA
212300930908    1C     GENCN         IFGT      0
212400930908     C*
212500930908     C     GENCN         MULT      100           NUM9              9 0
212600950123     C     GENIC         IFNE      *ZERO
212700940526     C     NUM9          DIV(H)    GENIC         W0052             5 2
212800950123     C                   ELSE
212900950123     C                   Z-ADD     *ZERO         W0052
213000950123     C                   ENDIF
213100940526     C                   EXSR      CTRP1
213200930908     C*
213300930908     C* % CONSEGNATE        SU TOT CONSEGNABILE
213400950123     C     GENCO         IFNE      *ZERO
213500940526     C     NUM9          DIV(H)    GENCO         W0052
213600950123     C                   ELSE
213700950123     C                   Z-ADD     *ZERO         W0052
213800950123     C                   ENDIF
213900940526     C                   EXSR      CTRP2
214000930908    1C                   ENDIF
214100930908     C*
214200940117     C                   MOVEL     TE(18)        RIGA             66
214300940202     C                   Z-ADD     GENCNN        GENNOR
214400940202     C                   Z-ADD     GENCNE        GENESP
214500090917     C                   Z-ADD     GENCNH        GENH10
214600940202     C                   Z-ADD     GENCN         GENTOT
214700930908     C                   EXCEPT    INTE1
214800940208     C*
214900940208     C* ESEGUO STAMPA DELLE CONSEGNE PARZIALI E RISERVE
215000940208     C                   EXSR      STAPR
215100930908     C*
215200930908     C* STAMPA DETTAGLIO
215300930908    1C     GENCN         IFGT      0
215400930908     C                   MOVEA     CNE           ESP
215500090917     C                   MOVEA     CNh           h10
215600970121     C     CNC           ADD       CNN           NOR
215700930908     C                   EXSR      STADCN
215800930908    1C                   ENDIF
215900940324     C**
216000940324     C     *IN02         IFEQ      *OFF
216100940329     C* LA 1 PERCENTUALE SUL TOTALE CONSEGNATO NON CI VUOLE PIU'
216200940329     C                   SETON                                        03
216300940324     C**
216400930908     C**
216500930908     C* SPEDIZIONI   N O N    C O N S E G N A T E
216600930908     C**
216700930908     C* GIACENZE APERTE
216800090916     C                   XFOOT     GAE           GENGAE            6 0           "E"
216900090918     C                   XFOOT     GAh           GENGAh            6 0           "H"
217000090916     C                   XFOOT     GAN           GENGAN            6 0           "D"
217100090916     C                   XFOOT     GAC           W0090                           "C"
217200090916     C                   ADD       W0090         GENGAN                          D+C
217300930908     C     GENGAN        ADD       GENGAE        GENGA             7 0           TOT CONSEG
217400090918     C                   ADD       GENGAH        GENGA             7 0           TOT CONSEG
217500930908     C* LASCIATI AVVISI
217600090916     C                   XFOOT     LAE           GENLAE            6 0           "E"
217700090918     C                   XFOOT     LAh           GENLAH            6 0           "H"
217800090916     C                   XFOOT     LAN           GENLAN            6 0           "D"
217900090916     C                   XFOOT     LAC           W0090                           "C"
218000090916     C                   ADD       W0090         GENLAN                          D+C
218100930908     C     GENLAN        ADD       GENLAE        GENLA             7 0           TOT CONSEG
218200090918     C                   ADD       GENLAh        GENLA             7 0           TOT CONSEG
218300930908     C* CHIUSE PER TURNO
218400090916     C                   XFOOT     CTE           GENCTE            6 0           "E"
218500090918     C                   XFOOT     CTh           GENCTH            6 0           "H"
218600090916     C                   XFOOT     CTN           GENCTN            6 0           "D"
218700090916     C                   XFOOT     CTC           W0090                           "C"
218800090916     C                   ADD       W0090         GENCTN                          D+C
218900930908     C     GENCTN        ADD       GENCTE        GENCT             7 0           TOT CONSEG
219000090918     C                   ADD       GENCTh        GENCT             7 0           TOT CONSEG
219100930908     C* NON FATTE
219200090916     C                   XFOOT     NFE           GENNFE            6 0           "E"
219300090918     C                   XFOOT     NFh           GENNFH            6 0           "H"
219400090916     C                   XFOOT     NFN           GENNFN            6 0           "D"
219500090916     C                   XFOOT     NFC           W0090                           "C"
219600090916     C                   ADD       W0090         GENNFN                          D+C
219700930908     C     GENNFN        ADD       GENNFE        GENNF             7 0           TOT CONSEG
219800090918     C                   ADD       GENNFH        GENNF             7 0           TOT CONSEG
219900930908     C*
220000930908    2C     GENGA         IFGT      0
220100930908     C     GENLA         ORGT      0
220200930908     C     GENCT         ORGT      0
220300930908     C     GENNF         ORGT      0
220400930908     C                   CLEAR                   CONP1
220500930908     C                   CLEAR                   CONP2
220600090918     C*
220700930908     C     GENGAN        ADD       GENLAN        GENNOR
220800930908     C                   ADD       GENCTN        GENNOR
220900930908     C                   ADD       GENNFN        GENNOR
221000090918     C*
221100930908     C     GENGAE        ADD       GENLAE        GENESP
221200930908     C                   ADD       GENCTE        GENESP
221300930908     C                   ADD       GENNFE        GENESP
221400090918     C*
221500090918     C     GENGAh        ADD       GENLAh        GENh10
221600090918     C                   ADD       GENCTh        GENh10
221700090918     C                   ADD       GENNFh        GENh10
221800090918     C* TOTALE
221900930908     C     GENNOR        ADD       GENESP        GENTOT
222000090918     C                   ADD       GENh10        GENTOT
222100930908     C*
222200930908     C* % NON CONSEGNATE    SU TOT IN CONSEGNA
222300930908     C*
222400930908     C     GENTOT        MULT      100           NUM9              9 0
222500950123     C     GENIC         IFNE      *ZERO
222600940526     C     NUM9          DIV(H)    GENIC         W0052
222700950123     C                   ELSE
222800950123     C                   Z-ADD     *ZERO         W0052
222900950123     C                   ENDIF
223000940526     C                   EXSR      CTRP1
223100930908     C*
223200930908     C* % NON CONSEGNATE    SU TOT CONSEGNABILE
223300950123     C     GENCO         IFNE      *ZERO
223400940526     C     NUM9          DIV(H)    GENCO         W0052
223500950123     C                   ELSE
223600950123     C                   Z-ADD     *ZERO         W0052
223700950123     C                   ENDIF
223800940526     C                   EXSR      CTRP2
223900930908     C*
224000940117     C                   MOVEL     TE(24)        RIGA
224100940117     C*
224200940117     C   OF              EXCEPT    TESTA
224300940117     C   OF              SETOFF                                       OF
224400930908     C                   EXCEPT    INTE1
224500930908     C**
224600930908     C* GIACENZE APERTE
224700930908     C**
224800930908     C                   Z-ADD     GENGA         GENTOT
224900930908     C                   Z-ADD     GENGAN        GENNOR
225000930908     C                   Z-ADD     GENGAE        GENESP
225100090918     C                   Z-ADD     GENGAh        GENh10
225200090918     C                   MOVEA     GAE           ESP
225300090918     C                   MOVEA     GAh           h10
225400090916     C     GAC           ADD       GAN           NOR
225500940202     C                   MOVEL     TE(25)        RIGA
225600930908     C*
225700930908     C                   EXSR      CTRNCO
225800930908     C**
225900930908     C* LASCIATI AVVISI
226000930908     C**
226100930908     C                   Z-ADD     GENLA         GENTOT
226200930908     C                   Z-ADD     GENLAN        GENNOR
226300930908     C                   Z-ADD     GENLAE        GENESP
226400090918     C                   Z-ADD     GENLAh        GENh10
226500090918     C                   MOVEA     LAE           ESP
226600090918     C                   MOVEA     LAh           h10
226700090916     C     LAC           ADD       LAN           NOR
226800940202     C                   MOVEL     TE(26)        RIGA
226900930908     C*
227000930908     C                   EXSR      CTRNCO
227100930908     C**
227200930908     C* CHIUSO PER TURNO
227300930908     C**
227400930908     C                   Z-ADD     GENCT         GENTOT
227500930908     C                   Z-ADD     GENCTN        GENNOR
227600930908     C                   Z-ADD     GENCTE        GENESP
227700090918     C                   Z-ADD     GENCTH        GENh10
227800090918     C                   MOVEA     CTE           ESP
227900090918     C                   MOVEA     CTh           h10
228000090916     C     CTC           ADD       CTN           NOR
228100940207     C                   MOVEL     TE(28)        RIGA
228200930908     C*
228300930908     C                   EXSR      CTRNCO
228400930908     C**
228500930908     C* NON FATTE
228600930908     C**
228700930908     C                   Z-ADD     GENNF         GENTOT
228800930908     C                   Z-ADD     GENNFN        GENNOR
228900930908     C                   Z-ADD     GENNFE        GENESP
229000090918     C                   Z-ADD     GENNFh        GENh10
229100090918     C                   MOVEA     NFE           ESP
229200090918     C                   MOVEA     NFh           h10
229300090916     C     NFC           ADD       NFN           NOR
229400940207     C                   MOVEL     TE(27)        RIGA
229500930908     C*
229600930908     C                   EXSR      CTRNCO
229700930908    2C                   ENDIF
229800930908     C**
229900930908     C* SPEDIZIONI   A N C O R A    I N    D I S T I N T A
230000930908     C**
230100090916     C                   XFOOT     IDE           GENIDE            6 0           "E"
230200090918     C                   XFOOT     IDh           GENIDH            6 0           "H"
230300090916     C                   XFOOT     IDN           GENIDN            6 0           "D"
230400090916     C                   XFOOT     IDC           W0090                           "C"
230500090916     C                   ADD       W0090         GENIDN                          D+c
230600930908     C     GENIDN        ADD       GENIDE        GENID             7 0           TOT CONSEG
230700090918     C                   ADD       GENIDh        GENID             7 0           TOT CONSEG
230800930908     C*
230900930908     C                   CLEAR                   CONP1
231000930908     C                   CLEAR                   CONP2
231100930908     C*
231200930908     C* % IN DISTINTA       SU TOT IN CONSEGNA
231300930908    1C     GENID         IFGT      0
231400930908     C*
231500930908     C     GENID         MULT      100           NUM9              9 0
231600950123     C     GENIC         IFNE      *ZERO
231700940526     C     NUM9          DIV(H)    GENIC         W0052
231800950123     C                   ELSE
231900950123     C                   Z-ADD     *ZERO         W0052
232000950123     C                   ENDIF
232100940526     C                   EXSR      CTRP1
232200930908     C*
232300930908     C* % IN DISTINTA       SU TOT CONSEGNABILE
232400950123     C     GENCO         IFNE      *ZERO
232500940526     C     NUM9          DIV(H)    GENCO         W0052
232600950123     C                   ELSE
232700950123     C                   Z-ADD     *ZERO         W0052
232800950123     C                   ENDIF
232900940526     C                   EXSR      CTRP2
233000930908     C*
233100940117     C                   MOVEL     TE(29)        RIGA
233200940202     C                   Z-ADD     GENIDN        GENNOR
233300940202     C                   Z-ADD     GENIDE        GENESP
233400090918     C                   Z-ADD     GENIDh        GENh10
233500940202     C                   Z-ADD     GENID         GENTOT
233600930908     C*
233700940117     C   OF              EXCEPT    TESTA
233800940117     C   OF              SETOFF                                       OF
233900930908     C                   EXCEPT    INTE1
234000930908     C*
234100930908     C* STAMPA DETTAGLIO
234200090918     C                   MOVEA     IDE           ESP
234300090918     C                   MOVEA     IDh           h10
234400090916     C     IDC           ADD       IDN           NOR
234500930908     C                   EXSR      STADCN
234600930908    1C                   ENDIF
234700930908     C**
234800930908     C* SPEDIZIONI   I N    C O N S E G N A
234900930908     C**
235000930908     C                   CLEAR                   CONP2
235100930908     C*
235200930908     C* % IN DISTINTA       SU TOT CONSEGNABILE
235300930908    2C     GENIC         IFGT      0
235400930908     C     GENIC         MULT      100           NUM9              9 0
235500950123     C     GENCO         IFNE      *ZERO
235600940526     C     NUM9          DIV(H)    GENCO         W0052
235700950123     C                   ELSE
235800950123     C                   Z-ADD     *ZERO         W0052
235900950123     C                   ENDIF
236000940526     C                   EXSR      CTRP2
236100930908    2C                   ENDIF
236200930908     C*
236300940117     C                   MOVEL     TE(30)        RIGA
236400940202     C                   Z-ADD     GENICN        GENNOR
236500940202     C                   Z-ADD     GENICE        GENESP
236600090918     C                   Z-ADD     GENICh        GENh10
236700940202     C                   Z-ADD     GENIC         GENTOT
236800930908     C*
236900940117     C   OF              EXCEPT    TESTA
237000940117     C   OF              SETOFF                                       OF
237100930908     C                   EXCEPT    INTE2
237200930908     C*
237300930908     C* STAMPA DETTAGLIO
237400930908    2C     GENIC         IFGT      0
237500090918     C                   MOVEA     ICE           ESP
237600090918     C                   MOVEA     ICh           h10
237700090916     C     ICC           ADD       ICN           NOR
237800930908     C                   EXSR      STADCN
237900930908    2C                   ENDIF
238000930908     C**
238100930908     C* SPEDIZIONI   N O N    I N    C O N S E G N A
238200930908     C**
238300090916     C                   XFOOT     NCE           GENNCE            6 0           "E"
238400090918     C                   XFOOT     NCh           GENNCH            6 0           "H"
238500090916     C                   XFOOT     NCN           GENNCN            6 0           "D"
238600090916     C                   XFOOT     NCC           W0090                           "C"
238700090916     C                   ADD       W0090         GENNCN                          D+C
238800940117     C     GENNCN        ADD       GENNCE        GENNC             7 0           TOT CONSEG
238900090918     C                   ADD       GENNCh        GENNC             7 0           TOT CONSEG
239000930908     C*
239100930908     C* % NON IN CONSEGNA   SU TOT CONSEGNABILE
239200930908    2C     GENNC         IFGT      0
239300940526     C                   SETON                                        04
239400940117     C                   CLEAR                   CONP2
239500940117     C*
239600930908     C     GENNC         MULT      100           NUM9              9 0
239700950123     C     GENCO         IFNE      *ZERO
239800940526     C     NUM9          DIV(H)    GENCO         W0052
239900950123     C                   ELSE
240000950123     C                   Z-ADD     *ZERO         W0052
240100950123     C                   ENDIF
240200940526     C                   EXSR      CTRP2
240300930908     C*
240400940117     C                   MOVEL     TE(31)        RIGA
240500940202     C                   Z-ADD     GENNCN        GENNOR
240600940202     C                   Z-ADD     GENNCE        GENESP
240700090918     C                   Z-ADD     GENNCh        GENh10
240800940202     C                   Z-ADD     GENNC         GENTOT
240900930908     C*
241000940117     C   OF              EXCEPT    TESTA
241100940117     C   OF              SETOFF                                       OF
241200930908     C                   EXCEPT    INTE2
241300940117     C*
241400940117     C* STAMPA DETTAGLIO
241500940526    3C     GENNC         IFGT      0
241600090918     C                   MOVEA     NCE           ESP
241700090918     C                   MOVEA     NCh           h10
241800090916     C     NCC           ADD       NCN           NOR
241900940117     C                   EXSR      STADCN
242000940526    3C                   ENDIF
242100940526     C                   SETOFF                                       04
242200940526    2C                   ENDIF
242300930908     C*
242400930908     C**
242500930908     C* SPEDIZIONI   C O N S E G N A B I L I
242600930908     C**
242700930908     C*
242800940117     C                   MOVEL     TE(32)        RIGA
242900940202     C                   Z-ADD     GENCON        GENNOR            8 0
243000940202     C                   Z-ADD     GENCOE        GENESP            8 0
243100090918     C                   Z-ADD     GENCOh        GENh10            8 0
243200940202     C                   Z-ADD     GENCO         GENTOT            8 0
243300930908     C*
243400940117     C   OF              EXCEPT    TESTA
243500940117     C   OF              SETOFF                                       OF
243600090918     c
243700090918     c                   if        not *in25
243800090918     C                   EXCEPT    INTE3
243900090918     c                   else
244000090918     c* con tipo servizio H
244100090918     c                   seton                                        26
244200090918     C                   EXCEPT    INTE2
244300090918     c                   endif
244400930909     C*
244500090918     c                   setoff                                       26
244600930909     C* STAMPO DETTAGLIO
244700930909     C                   Z-ADD     33            Y
244800930909     C*
244900940207     C                   SETON                                        01
245000930909     C                   DO        3             B
245100940207     C                   CLEAR                   CONP3
245200940117     C                   MOVEL     TE(Y)         RIGA
245300930909     C                   ADD       1             Y
245400090918     C                   Z-ADD     COE(B)        GENESP
245500090918     C                   Z-ADD     COh(B)        GENh10
245600090916     C     COC(B)        ADD       CON(B)        GENNOR
245700970121     C     GENESP        ADD       GENNOR        GENTOT
245800090918     C                   ADD       GENh10        GENTOT
245900090918     c
246000930909     C* % NON IN CONSEGNA   SU TOT CONSEGNABILE
246100930909    2C     GENTOT        IFGT      0
246200930909     C     GENTOT        MULT      100           NUM9              9 0
246300950123     C     GENCO         IFNE      *ZERO
246400940526     C     NUM9          DIV(H)    GENCO         W0052
246500950123     C                   ELSE
246600950123     C                   Z-ADD     *ZERO         W0052
246700950123     C                   ENDIF
246800940526     C                   EXSR      CTRP3
246900930909     C                   ENDIF
247000930909     C*
247100940117     C   OF              EXCEPT    TESTA
247200940117     C   OF              SETOFF                                       OF
247300930909     C                   EXCEPT    DET2
247400930909     C                   ENDDO
247500940324     C*
247600940324     C                   ENDIF
247700940117     C* RIGA FINALE
247800940117     C                   EXCEPT    DET3
247900940117     C                   SETON                                        OF
248000950124     C*
248100920617     C                   ENDSR
248200950124     C**************************************************************************
248300950124     C* ESEGUO STAMPA DELLE CONSEGNE PARZIALI E RISERVE
248400950124     C**************************************************************************
248500940208     C     STAPR         BEGSR
248600940208     C*
248700940329     C* S T A M P O   C  O N S E G N E   P A R Z I A L I
248800940208     C     PRE(1)        ADD       PRN(1)        GENTOT
248900970121     C                   ADD       PRC(1)        GENTOT
249000090916     C                   ADD       PRh(1)        GENTOT
249100940208     C     GENTOT        IFGT      0
249200970121     C     PRC(1)        ADD       PRN(1)        GENNOR
249300090918     C                   Z-ADD     PRE(1)        GENESP
249400090918     C                   Z-ADD     PRh(1)        GENh10
249500940208     C                   MOVEL     TE(38)        RIGA             66
249600940208     C* % CONSEGNATE        SU TOT CONSEGNATO
249700940208     C*
249800940208     C     GENTOT        MULT      100           NUM9              9 0
249900950123     C     GENCN         IFNE      *ZERO
250000940526     C     NUM9          DIV(H)    GENCN         W0052
250100950123     C                   ELSE
250200950123     C                   Z-ADD     *ZERO         W0052
250300950123     C                   ENDIF
250400940526     C                   EXSR      CTRP1
250500940208     C* % CONSEGNATE        SU TOT IN CONSEGNA
250600950123     C     GENIC         IFNE      *ZERO
250700940526     C     NUM9          DIV(H)    GENIC         W0052
250800950123     C                   ELSE
250900950123     C                   Z-ADD     *ZERO         W0052
251000950123     C                   ENDIF
251100940526     C                   EXSR      CTRP2
251200940208     C*
251300940208     C* % CONSEGNATE        SU TOT CONSEGNABILE
251400950123     C     GENCO         IFNE      *ZERO
251500940526     C     NUM9          DIV(H)    GENCO         W0052
251600950123     C                   ELSE
251700950123     C                   Z-ADD     *ZERO         W0052
251800950123     C                   ENDIF
251900940526     C                   EXSR      CTRP3
252000940526     C*
252100940208     C                   EXCEPT    DET2
252200940208     C                   ENDIF
252300940208     C*
252400940329     C* S T A M P O   L E   R I S E R V E
252500940208     C     RSE(1)        ADD       RSN(1)        GENTOT
252600970121     C                   ADD       RSC(1)        GENTOT
252700090916     C                   ADD       RSh(1)        GENTOT
252800940208     C     GENTOT        IFGT      0
252900970121     C     RSC(1)        ADD       RSN(1)        GENNOR
253000090918     C                   Z-ADD     RSE(1)        GENESP
253100090918     C                   Z-ADD     RSh(1)        GENh10
253200940208     C                   MOVEL     TE(39)        RIGA             66
253300940208     C* % CONSEGNATE        SU TOT CONSEGNATO
253400940208     C*
253500940208     C     GENTOT        MULT      100           NUM9              9 0
253600950123     C     GENCN         IFNE      *ZERO
253700940526     C     NUM9          DIV(H)    GENCN         W0052
253800950123     C                   ELSE
253900950123     C                   Z-ADD     *ZERO         W0052
254000950123     C                   ENDIF
254100940526     C                   EXSR      CTRP1
254200940208     C* % CONSEGNATE        SU TOT IN CONSEGNA
254300950123     C     GENIC         IFNE      *ZERO
254400940526     C     NUM9          DIV(H)    GENIC         W0052
254500950123     C                   ELSE
254600950123     C                   Z-ADD     *ZERO         W0052
254700950123     C                   ENDIF
254800940526     C                   EXSR      CTRP2
254900940208     C*
255000940208     C* % CONSEGNATE        SU TOT CONSEGNABILE
255100950123     C     GENCO         IFNE      *ZERO
255200940526     C     NUM9          DIV(H)    GENCO         W0052
255300950123     C                   ELSE
255400950123     C                   Z-ADD     *ZERO         W0052
255500950123     C                   ENDIF
255600940526     C                   EXSR      CTRP3
255700940526     C*
255800940208     C                   EXCEPT    DET2
255900940208     C                   ENDIF
256000940208     C*
256100940208     C                   ENDSR
256200950124     C**************************************************************************
256300950124     C* STAMPA DETTAGLIO DELLE CONSEGNATE
256400950124     C**************************************************************************
256500930908     C     STADCN        BEGSR
256600950124     C*
256700930908    1C                   DO        13            B
256800930908     C     ESP(B)        ADD       NOR(B)        TOT(B)
256900090918     C                   ADD       h10(B)        TOT(B)
257000930908     C* SCAGLIONE PIENO
257100930908    2C     TOT(B)        IFGT      0
257200930908     C*
257300930908     C                   SELECT
257400930908     C* RITARDO
257500930908    3C     ORE(B)        WHENGT    0
257600930908    4C     ORE(B)        IFEQ      999
257700940117     C                   MOVEL     TE(23)        RIGA
257800930908     C     B             SUB       1             Y
257900930908   X4C                   ELSE
258000940117     C                   MOVEL     TE(22)        RIGA
258100930908     C                   Z-ADD     B             Y
258200930908    4C                   ENDIF
258300930908     C* ANTICIPO
258400930908     C     ORE(B)        WHENLT    0
258500930908    4C     ORE(B)        IFEQ      -999
258600940117     C                   MOVEL     TE(19)        RIGA
258700930908     C     B             ADD       1             Y
258800930908   X4C                   ELSE
258900940117     C                   MOVEL     TE(20)        RIGA
259000930908     C                   Z-ADD     B             Y
259100930908    4C                   ENDIF
259200930908     C*
259300930908   X3C                   OTHER
259400930908     C* CONFORMITA'
259500940117     C                   MOVEL     TE(21)        RIGA
259600940202     C                   Z-ADD     B             Y
259700930908    3C                   ENDSL
259800930908     C*
259900930908     C                   CLEAR                   CONP1
260000930908     C                   CLEAR                   CONP2
260100940329     C                   CLEAR                   CONP3
260200930908     C*
260300940526     C     TOT(B)        MULT      100           NUM9              9 0
260400940526     C*
260500930908     C* % SUL TOTALE IN CONSEGNA
260600940526     C* (NON ELABORO PER LE NON IN CONSEGNA)
260700940526     C     *IN04         IFEQ      *OFF
260800950123     C     GENIC         IFNE      *ZERO
260900940526     C     NUM9          DIV(H)    GENIC         W0052
261000950123     C                   ELSE
261100950123     C                   Z-ADD     *ZERO         W0052
261200950123     C                   ENDIF
261300940526     C                   EXSR      CTRP2
261400940526     C                   ENDIF
261500930908     C*
261600930908     C* % SUL TOTALE CONSEGNABILE
261700950123     C     GENCO         IFNE      *ZERO
261800940526     C     NUM9          DIV(H)    GENCO         W0052
261900950123     C                   ELSE
262000950123     C                   Z-ADD     *ZERO         W0052
262100950123     C                   ENDIF
262200940526     C                   EXSR      CTRP3
262300940203     C* % SUL TOTALE CONSEGNATO
262400940329     C* (SOLO QUANDO ELABORO LE SPEDIZIONI CONSEGNATE)
262500940526     C     *IN03         IFEQ      *OFF
262600950123     C     GENCN         IFNE      *ZERO
262700940526     C     NUM9          DIV(H)    GENCN         W0052
262800950123     C                   ELSE
262900950123     C                   Z-ADD     *ZERO         W0052
263000950123     C                   ENDIF
263100940526     C                   EXSR      CTRP1
263200940526     C                   ENDIF
263300970121     C*
263400970121     C                   Z-ADD     NOR(B)        PNORM             7 0
263500970121     C                   Z-ADD     ESP(B)        PESPR             7 0
263600090918     C                   Z-ADD     h10(B)        Ph1030            7 0
263700970121     C                   Z-ADD     TOT(B)        PTOTAL            8 0
263800930908     C*
263900940117     C   OF              EXCEPT    TESTA
264000940117     C   OF              SETOFF                                       OF
264100940117     C                   EXCEPT    DET1
264200930908     C*
264300930908    2C                   ENDIF
264400930908    1C                   ENDDO
264500950124     C*
264600930908     C                   ENDSR
264700950124     C**************************************************************************
264800950124     C* CONTROLLO LE SPEDIZIONI NON CONSEGNATE DA STAMPARE
264900950124     C**************************************************************************
265000930908     C     CTRNCO        BEGSR
265100950124     C*
265200930908    1C     GENTOT        IFGT      0
265300930908     C* %    SU TOT IN CONSEGNA
265400930908     C     GENTOT        MULT      100           NUM9              9 0
265500950123     C     GENIC         IFNE      *ZERO
265600940526     C     NUM9          DIV(H)    GENIC         W0052
265700950123     C                   ELSE
265800950123     C                   Z-ADD     *ZERO         W0052
265900950123     C                   ENDIF
266000940526     C                   EXSR      CTRP2
266100930908     C*
266200930908     C* %    SU TOT CONSEGNABILE
266300950123     C     GENCO         IFNE      *ZERO
266400940526     C     NUM9          DIV(H)    GENCO         W0052
266500950123     C                   ELSE
266600950123     C                   Z-ADD     *ZERO         W0052
266700950123     C                   ENDIF
266800940526     C                   EXSR      CTRP3
266900940207     C*
267000940203     C                   EXCEPT    DET4
267100940203     C                   EXCEPT    DET2
267200930908     C*
267300930908     C                   EXSR      STADCN
267400930908    1C                   ENDIF
267500950124     C*
267600930908     C                   ENDSR
267700950124     C**************************************************************************
267800950124     C* CONTROLLO SE 1% E' > 99,99
267900950124     C**************************************************************************
268000940526     C     CTRP1         BEGSR
268100950124     C*
268200940526     C     W0052         IFGT      99,99
268300940526     C                   Z-ADD     99,99         CONP1             4 2
268400940526     C                   ELSE
268500940526     C                   Z-ADD     W0052         CONP1
268600940526     C                   ENDIF
268700950124     C*
268800940526     C                   ENDSR
268900950124     C**************************************************************************
269000950124     C* CONTROLLO SE 2% E' > 99,99
269100950124     C**************************************************************************
269200940526     C     CTRP2         BEGSR
269300950124     C*
269400940526     C     W0052         IFGT      99,99
269500940526     C                   Z-ADD     99,99         CONP2             4 2
269600940526     C                   ELSE
269700940526     C                   Z-ADD     W0052         CONP2
269800940526     C                   ENDIF
269900950124     C*
270000940526     C                   ENDSR
270100950124     C**************************************************************************
270200950124     C* CONTROLLO SE 3% E' > 99,99
270300950124     C**************************************************************************
270400940526     C     CTRP3         BEGSR
270500950124     C*
270600940526     C     W0052         IFGT      99,99
270700940526     C                   Z-ADD     99,99         CONP3             4 2
270800940526     C                   ELSE
270900940526     C                   Z-ADD     W0052         CONP3
271000940526     C                   ENDIF
271100950124     C*
271200940526     C                   ENDSR
271300950124     C**************************************************************************
271400950124     C* STAMPE SPEDIZIONI NON IN CONSEGNA
271500950124     C**************************************************************************
271600940530     C     STASNC        BEGSR
271700141119     c                   setoff                                       70
271800950124     C*
271900950127     C                   Z-ADD     ARBAAS        P0CAAS            2 0
272000940530     C* MESE GIORNO SPEDIZIONE
272100940530     C                   MOVEL     ARBMGS        MMGG
272200940530     C                   Z-ADD     MM            P0CMGS            4 0
272300940530     C                   MOVEL     GG            P0CMGS
272400940601     C* DESTINATARIO
272500940601     C                   MOVEL     ARBRSD        P0CRSD           15
272600950127     C                   MOVEL     ARBIND        P0CIND           24
272700940601     C                   MOVEL     ARBLOD        P0CLOD           15
272800940601     C* DECODIFICA CONSEGNA PARTICOLARE
272900950127     C                   MOVEL     *BLANKS       P0DTC1
273000950123     C     ARBTC1        IFNE      ' '
273100940601     C                   MOVEL     '1P'          COD
273200940601     C                   MOVEL     *BLANKS       KEY
273300950123     C                   MOVEL     ARBTC1        KEY
273400970124     C     KTAB          CHAIN(N)  TABEL                              30
273500950127     C  N30              MOVEL     TBLUNI        P0DTC1            5
273600940601     C                   ENDIF
273700950127     C*
273800950127     C                   MOVEL     *BLANKS       P0DTC2
273900950127     C     ARBTC2        IFNE      *BLANKS
274000950127     C                   MOVEL     '1P'          COD
274100950127     C                   MOVEL     *BLANKS       KEY
274200950127     C                   MOVEL     ARBTC2        KEY
274300970124     C     KTAB          CHAIN(N)  TABEL                              30
274400950127     C  N30              MOVEL     TBLUNI        P0DTC2           10
274500950127     C                   ENDIF
274600051128     C* DATA CONSEGNA RICHIESTA richiamo pgm per sapere se tassativa
274700051128     c                   clear                   p0cdcr            6 0
274800051128     c                   clear                   p0ctcr
274900051128     c                   if        arbdcr>0 or arbhcr>0
275000051128     c                   clear                   fnlv80ds
275100051128     c                   eval      ilv80aas=arbaas
275200051128     c                   eval      ilv80lnp=arblnp
275300051128     c                   eval      ilv80nrs=arbnrs
275400051128     c                   eval      ilv80nsp=arbnsp
275500051128     c                   eval      ilv80mgs=arbmgs
275600051128     c                   eval      ilv80tc1=arbtc1
275700051128     c                   eval      ilv80tc2=arbtc2
275800051128     c                   eval      ilv80dcr=arbdcr
275900051128     c                   eval      ilv80hcr=arbhcr
276000051128     c                   eval      ilv80tcr=arbtcr
276100051128     c                   call      'FNLV80R'
276200051128     c                   parm                    kpjba
276300051128     c                   parm                    fnlv80ds
276400051128     c
276500051128     c                   eval      p0cdcr=olv80dcr6
276600051128     c*
276700051128     c                   if        olv80tat='S'
276800051128     c                   eval      p0ctcr='TA'
276900051128     c                   else
277000051128     c                   clear                   p0ctcr            2
277100051128     c                   move      arbtcr        p0ctcr
277200051128     c                   endif
277300051128     c                   endif
277400141119     c
277500141119     c* Verifico se presente dispo di dirottamento
277600141217     c                   exsr      dispoDIR
277700141119     c*
277800141119     c                   if        OLRY09ESIT='1'
277900141119     c                   seton                                        70
278000141119     c                   endif
278100940530     C* FERMO DEPOSITO
278200051107     C**   ARBFFD        IFEQ      'S'
278300051107     C**                 MOVEL     'SI'          P0CFFD            2
278400051107     C**                 ELSE
278500051107     C**                 MOVEL     '  '          P0CFFD
278600051107     C**                 ENDIF
278700940530     C*
278800940530     C     *INOD         IFEQ      *ON
278900940530     C                   EXCEPT    TESELE
279000940530     C                   SETOFF                                       OD
279100940530     C                   ENDIF
279200950123     C*
279300950123     C* TENTO DI REPERIRE IL CONTRASEGNO
279400950123     C                   CLEAR                   AR9CAS
279500950123     C                   CLEAR                   AR9VCA
279600051115     C     KARB3         SETLL     FiAR901L                               30     ON=TROVATO
279700051115     C   30KARB3         CHAIN     FiAR901L                           30
279800950127     C*
279900950127     C* TENTO DI REPERIRE L'IMPORTO FATTURA
280000950127     C                   CLEAR                   AR6IFT
280100991130     C                   CLEAR                   AR6DIV
280200991128     C                   CLEAR                   COMIFT
280300991130     C                   CLEAR                   COMdiv
280400991104     C                   MOVEL     '1'           KTRC
280500991104     C     KARB6         SETLL     FIAR601L                               30     ON=TROVATO
280600991128     C     *IN30         IFEQ      *ON
280700991130     C     KARB6         CHAIN     FIAR601L                           30
280800940530     C*
280900991130     C     *IN30         IFEQ      *OFF
281000991130     C* Vado su tabella divise per sapere se accetta i decimali
281100991130     C     AR6DIV        IFNE      SAVDIV
281200991130     C                   MOVEL     'CV'          COD
281300991130     C                   MOVEL(P)  AR6DIV        KEY
281400991130     C     KTAB          CHAIN(N)  TABEL                              37
281500991130     C  N37              MOVEL     TBLUNI        DSCV
281600991130     C   37              CLEAR                   DSCV
281700991130     C                   MOVEL     AR6DIV        SAVDIV
281800991130     C                   ENDIF
281900991130     C*
282000991130     C* Edito importo fattura per la stampa
282100991130     c     ar6ift        ifgt      *zeros
282200991130     C                   Z-ADD     AR6IFT        W0122
282300991130     C                   MOVEL     §CVFDC        WTSTDEC
282400991130     c                   exsr      edtimporti
282500991130     c                   move      impeditato    comift            9
282600991130     c                   movel     ar6div        comdiv
282700991130     c                   endif
282800991128     C                   ENDIF
282900991130     C                   ENDIF
283000940530     C                   EXCEPT    DETELE
283100110923     c*
283200070419     C* STAMPO C/ASSEGNO NON ABILITATO SU SECONDA RIGA se GEO non attiva
283300070419     c                   if        not *in18
283400070419     c
283500070419     C     ARBAcc        IFEQ      ' '
283600070419     C     AR9CAS        ANDNE     0
283700110923     c                   seton                                        28
283800110923     c                   endif
283900110923     c
284000110923     c                   if        *in28 or *in19
284100070419     C                   EXCEPT    DETCAA
284200070419     C                   ENDIF
284300070419     c
284400070419     c* Stampo tutte le abilitazioni bolle che ci sono
284500070419     c                   else
284600070419     c                   clear                   fnlrg6ds
284700070419     c                   eval      ilrg6aas=arbaas
284800070419     c                   eval      ilrg6lnp=arblnp
284900070419     c                   eval      ilrg6nrs=arbnrs
285000070419     c                   eval      ilrg6nsp=arbnsp
285100070419     c                   call      'FNLRG6R'
285200070419     c                   parm                    fnlrg6ds
285300070419     c
285400070419     c                   if        olrg6vssin<>*blanks
285500110923     c                   seton                                        28
285600110923     c                   endif
285700110923     c
285800110923     c                   if        *in28 or *in19
285900070419     C                   EXCEPT    DETCAA
286000070419     c                   endif
286100070419     c                   endif
286200070419     c
286300950127     C* VEDO SE SONO PRESENTI DELLE NOTE
286400950127     C     *LIKE         DEFINE    AR4NOT        P0CNO1
286500950127     C     *LIKE         DEFINE    AR4NOT        P0CNO2
286600060905     C     *LIKE         DEFINE    AR4NOT        wstadis
286700950127     C                   CLEAR                   P0CNO1
286800950127     C                   CLEAR                   P0CNO2
286900950127     C                   MOVEL     '8'           AR4TRC
287000060216     C     KAR41         SETLL     FiAR401L                               30     ON=TROVATO
287100950127     C   30              DO
287200060216     C     KAR41         CHAIN     FiAR401L                           30
287300950127     C  N30              MOVEL     AR4NOT        P0CNO1
287400950127     C                   MOVEL     '9'           AR4TRC
287500060216     C     KAR41         SETLL     FiAR401L                               30     ON=TROVATO
287600950127     C   30              DO
287700060216     C     KAR41         CHAIN     FiAR401L                           30
287800950127     C  N30              MOVEL     AR4NOT        P0CNO2
287900950127     C                   ENDDO
288000950127     C                   ENDDO
288100060905     c
288200060905     c* Vedo se spedizioni in distinta con data > di quella elaborata
288300060905     c                   setoff                                       12
288400060905     c                   clear                   wstadis
288500060905     c                   if        arbndc>0 and arbddc>0
288600060905     c                   eval      wstadis=Cindistinta
288700060905     c                   eval      %subst(wstadis:15:7)= %editc(arbndc: 'Z')
288800060905     c* giro la data
288900060905     C                   Z-ADD     arbddc        G02INV                         AAAA/MM/GG
289000060905     C                   Z-ADD     *ZERO         G02DAT
289100060905     C                   MOVEL     '3'           G02ERR
289200060905     C                   CALL      'XSRDA8'
289300060905     C                   PARM                    WLBDAT
289400060905     C                   MOVE      G02DAT        W0020             2 0
289500060905     C                   MOVEL     G02DAT        STAddc            6 0          GG/MM/AA
289600060905     C                   MOVE      W0020         STAddc            6 0          GG/MM/AA
289700060905     c                   eval      %subst(wstadis:28:8)= %editc(staddc: 'Y')
289800060905     c
289900060905     c* se non c'e' la seconda nota la riempo
290000060905     c* altrimenti scrivo 3°riga
290100060905     c                   if        p0cno2=*blanks
290200060905     c                   eval      p0cno2=wstadis
290300060905     c                   else
290400060905     c                   seton                                        12
290500060905     c                   endif
290600060905     c                   endif
290700990329     C*
290800990329     C* VEDO SE E' PRESENTE UNA C.A.
290900090922     C                   SETOFF                                       07
291000040812      * Sostituisco la lettura del file FNDTC02 con
291100040812      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
291200040812      * legate alla spedizione che passo
291300040812      *
291400040812     c                   clear                   fidn12ds
291500040812     c                   eval      i12aas = arbaas
291600040812     c                   eval      i12lnp = arblnp
291700040812     c                   eval      i12nrs = arbnrs
291800040812     c                   eval      i12nsp = arbnsp
291900040812     c                   eval      i12fel = 'E'
292000040812     c                   eval      i12fan = 'E'
292100040812      *
292200040812     c                   call      'FIDN12R'
292300040812     c                   parm                    fidn12ds
292400040812      *
292500040812      * se non ci sono errori
292600040812     c                   if        o12err = ' '
292700040812      * se numero di CA trovate maggiore di zero
292800040812     c                   if        o12nca > 0
292900040812     c* prendo la prima CA
293000040812     c                   z-add     1             ii                2 0
293100040812     c                   movea     skkey(ii)     dskey
293200040812     C                   Z-ADD     dsfil         dctfil
293300040812     C                   Z-ADD     dsnca         dctnca
293400040812     C                   Z-ADD     skdca(ii)     dctdca
293500040812     C                   Z-ADD     skdch(ii)     dctdch
293600040812     C                   movea     skcch(ii)     dctcch
293700040812     c                   seton                                        07
293800040812     c                   exsr      preca
293900040812      *
294000040812     c                   endif
294100040812     c                   endif
294200040812      * stampo le note
294300040812     C* NOTE
294400040812     C     P0CNO1        IFNE      *BLANKS
294500040812     C     P0CNO2        ORNE      *BLANKS
294600040812     C     ARBTC2        ORNE      *BLANKS
294700040812     C     *IN07         OREQ      *ON
294800040812     C*
294900040812     C                   EXCEPT    DE2ELE
295000040812     C*
295100040812     c* se ci sono altre CA stampo ancora
295200040812      * se non ci sono errori
295300040812     c                   if        o12err = ' '  and o12nca > 0
295400040812     c                   add       1             ii
295500040812     c                   dow       ii <= o12nca
295600040812     c                   movea     skkey(ii)     dskey
295700040812     C                   Z-ADD     dsfil         dctfil
295800040812     C                   Z-ADD     dsnca         dctnca
295900040812     C                   Z-ADD     skdca(ii)     dctdca
296000040812     C                   Z-ADD     skdch(ii)     dctdch
296100040812     C                   movea     skcch(ii)     dctcch
296200040812      *
296300040812     C                   EXSR      PRECA
296400040812     C** STAMPO TESTATA E DETT C.A.
296500040812     C   OD              EXCEPT    TESELE
296600040812     C                   EXCEPT    DETCA
296700040812      *
296800040812     c                   add       1             ii
296900040812     C                   ENDDO
297000040812      *
297100040812     c                   endif
297200040812      *
297300040812     c                   endif
297400060905     c
297500060905     C** STAMPO se in distinta
297600060905     c                   if        *in12
297700060905     C   OD              EXCEPT    TESELE
297800060905     C                   EXCEPT    DETdis
297900060905     c                   endif
298000950123     C*
298100940530     C                   ENDSR
298200141217     C**************************************************************************
298300141217     c     DispoDIR      BEGSR
298400141217     C**************************************************************************
298500141217     c                   clear                   fnlry09ds2
298600141217     c                   eval      ILRY09TCH='T'
298700141217     c                   eval      ILRY09TDIS='D'
298800141217     c                   eval      ILRY09AAS=arbaas
298900141217     c                   eval      ILRY09LNP=arblnp
299000141217     c                   eval      ILRY09NRS=arbnrs
299100141217     c                   eval      ILRY09NSP=arbnsp
299200141217     c                   eval      kpjbu=fnlry09ds2
299300141217     c                   call      'FNLRY09C'
299400141217     c                   parm                    kpjba
299500141217     c                   eval      fnlry09ds2=kpjbu
299600141217     c                   ENDSR
299700110923     C**************************************************************************
299800110923     C* controllo Perfect Order
299900110923     C**************************************************************************
300000110923     C     POrder        BEGSR
300100110926
300200110926     c                   eval      ilr78dtela=ds1ada
300300111012     c                   eval      ilr78sppar=ds1fl5
300400110926     c                   eval      kpjbu=fnlr78ds1
300500110926     c
300600110923     c                   call      'FNLR78R1'
300700110926     c                   parm                    kpjba
300800110926     c                   parm                    arbds
300900110923     c
301000110926     c                   eval      fnlr78ds1=kpjbu
301100110926     c
301200110923     C                   ENDSR
301300990329     C**************************************************************************
301400990329     C* PREPARO I DATI DELLA C.A.
301500990329     C**************************************************************************
301600990329     C     PRECA         BEGSR
301700990329     C                   SETOFF                                       06
301800990329     C                   CLEAR                   STACCH
301900990329     C* DATA APERTURA C.A.
302000990329     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
302100990329     C                   Z-ADD     *ZERO         G02DAT
302200990329     C                   MOVEL     '3'           G02ERR
302300990329     C                   CALL      'XSRDA8'
302400990329     C                   PARM                    WLBDAT
302500990329     C                   MOVE      G02DAT        W0020             2 0
302600990329     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
302700990329     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
302800990329     C** DATA CNHIUSUA C.A.
302900990329     C                   CLEAR                   STACHC
303000990329     C     DCTDCH        IFGT      0
303100990329     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
303200990329     C                   Z-ADD     *ZERO         G02DAT
303300990329     C                   MOVEL     '3'           G02ERR
303400990329     C                   CALL      'XSRDA8'
303500990329     C                   PARM                    WLBDAT
303600990329     C                   MOVE      G02DAT        W0020             2 0
303700990329     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
303800990329     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
303900990329     C                   SETON                                        06
304000990329     C* CAUSALE CHIUSURA
304100990329    2C     DCTCCH        IFNE      *BLANKS
304200990329     C                   CLEAR                   DCCH
304300990329     C                   CLEAR                   DSBS02
304400990329     C                   MOVEL     'C'           T02MOD
304500990329     C                   MOVEL     KNSIF         T02SIF
304600990329     C                   MOVEL     'CCH'         T02COD
304700990329     C                   MOVEL     DCTCCH        T02KE1
304800990329     C                   CALL      'TIBS02R'
304900990329     C                   PARM                    KPJBA
305000990329     C                   PARM                    DSBS02
305100990329     C*
305200990329    3C     T02ERR        IFEQ      ' '
305300990329     C                   MOVEL     T02UNI        DCCH
305400990329    3C                   ENDIF
305500990329     C                   MOVEL     §CHDES        STACCH           25
305600990329    2C                   ENDIF
305700990329     C                   ENDIF
305800990329     C                   ENDSR
305900970121     C**************************************************************************
306000970121     C* SCRIVO I RECORD DI TOTALE IN FNSCO
306100970121     C**************************************************************************
306200970121     C     WRTSCO        BEGSR
306300970130     C                   CLEAR                   FNSCO000
306400970130     C*
306500970130      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
306600970130      * riempo il "flag di trasmissione" (SPAFTR)
306700040922     C***  §5CSCO        IFNE      'S'
306800040922     C***                MOVE      'N'           SCOFTR                         FLAG TRASMISS.
306900040922     C***                MOVEL     *HIVAL        SCODTR
307000040922     C***                ENDIF
307100040922     c                   z-add     wdate1        scodtr
307200040922     c
307300970207     C                   MOVEL     DS1FEL        SCOFLE
307400970225     C                   MOVEL     DS1LNA        SCOLNA
307500970207     C                   MOVEL     DS1ADA        SCODCM
307600970130     C**
307700970130     C* Se si tratta di un giorno festivo scrivo soltanto un record
307800970130     C*  indicando che e' festa con "F" in SCOSCO
307900970225    0C     WFEST         IFEQ      'S'
308000970130     C                   MOVEL     'F'           SCOSCO
308100970130     C                   WRITE     FNSCO000
308200970130     C                   ELSE
308300970130     C**
308400970130     C**
308500970121     C* LID: identifica in a che livello di deve stampare (livello
308600970121     c*      orrizontale, essendoci in stampa 4 scomparti-livelli)
308700970121     c* ORD: e' l'oridne di stampa posto in verticale
308800970122     c* OLT: =A se e' oltre un certo anticipo
308900970122     c*      =D se e' in ritardo/anticipi di un tot di ore
309000970122     c*      =O se e' oltre un certo ritardo
309100970122     c*      =T record di totale
309200970122     c*      =' '  se non c'e' affidabilita'
309300970128     C**
309400970121     C*  C O N S E G N A T E
309500970121     C                   MOVEL     1             SCOLIV
309600970121     C                   MOVEL     '2'           WTOTLI
309700970121     C                   Z-ADD     2             SCOORD
309800970121     C                   MOVEL     'C'           SCOSCO
309900970121     C                   MOVEL     'S'           WANRIT            1
310000970121     C                   MOVEL     ' '           WGBO              1
310100970121     C                   MOVEA     CNE           SKE
310200970121     C                   MOVEA     CNC           SKC
310300970121     C                   MOVEA     CNN           SKN
310400090918     C                   MOVEA     CNh           SKh
310500970121     C                   MOVEL     '2'           WTOTLI            1
310600970121     C                   EXSR      SCRIVI
310700970121     C*
310800970121     C*  C O N S E G N E   P A R Z I A L I
310900970121     C                   MOVEL     1             SCOLIV
311000970121     C                   Z-ADD     3             SCOORD
311100970121     C                   MOVEL     'N'           WANRIT            1
311200970121     C                   MOVEL     'P'           WGBO              1
311300970121     C                   CLEAR                   SKE
311400970121     C                   CLEAR                   SKC
311500970121     C                   CLEAR                   SKN
311600090918     C                   CLEAR                   SKh
311700970121     C                   Z-ADD     PRE(1)        SKE(1)
311800970121     C                   MOVEA     PRC(1)        SKC(1)
311900970121     C                   MOVEA     PRN(1)        SKN(1)
312000090918     C                   MOVEA     PRh(1)        SKh(1)
312100970121     C                   EXSR      SCRIVI
312200970121     C*  R I S E R V E     C O N S E G N E
312300970121     C                   MOVEL     1             SCOLIV
312400970121     C                   Z-ADD     4             SCOORD
312500970121     C                   MOVEL     'N'           WANRIT            1
312600970121     C                   MOVEL     'R'           WGBO              1
312700970121     C                   CLEAR                   SKE
312800970121     C                   CLEAR                   SKC
312900970121     C                   CLEAR                   SKN
313000090918     C                   CLEAR                   SKh
313100970121     C                   Z-ADD     RSE(1)        SKE(1)
313200970121     C                   MOVEA     RSC(1)        SKC(1)
313300970121     C                   MOVEA     RSN(1)        SKN(1)
313400090918     C                   MOVEA     RSh(1)        SKh(1)
313500970121     C                   EXSR      SCRIVI
313600970121     C*  N O N   C O N S E G N A T E
313700970121     C*          DI CUI GIACENZE APERTE
313800970121     C                   MOVEL     1             SCOLIV
313900970121     C                   MOVEL     '1'           WTOTLI            1
314000970121     C                   Z-ADD     7             SCOORD
314100970121     C                   MOVEL     'N'           SCOSCO
314200970121     C                   MOVEL     'S'           WANRIT            1
314300970121     C                   MOVEL     'G'           WGBO              1
314400970121     C                   MOVEA     GAE           SKE
314500970121     C                   MOVEA     GAC           SKC
314600970121     C                   MOVEA     GAN           SKN
314700090918     C                   MOVEA     GAh           SKh
314800970121     C                   EXSR      SCRIVI
314900970121     C*          DI CUI LASCIATI AVVISI
315000970121     C                   MOVEL     1             SCOLIV
315100970121     C                   Z-ADD     9             SCOORD
315200970121     C                   MOVEL     'L'           WGBO              1
315300970121     C                   MOVEA     LAE           SKE
315400970121     C                   MOVEA     LAC           SKC
315500970121     C                   MOVEA     LAN           SKN
315600090918     C                   MOVEA     LAh           SKh
315700970121     C                   EXSR      SCRIVI
315800970121     C*          DI CUI CHIUSE PER TURNO
315900970121     C                   MOVEL     1             SCOLIV
316000970121     C                   Z-ADD     11            SCOORD
316100970121     C                   MOVEL     'C'           WGBO              1
316200970121     C                   MOVEA     CTE           SKE
316300970121     C                   MOVEA     CTC           SKC
316400970121     C                   MOVEA     CTN           SKN
316500090918     C                   MOVEA     CTh           SKh
316600970121     C                   EXSR      SCRIVI
316700970121     C*          DI CUI NON FATTE
316800970121     C                   MOVEL     1             SCOLIV
316900970121     C                   Z-ADD     13            SCOORD
317000970121     C                   MOVEL     'N'           WGBO              1
317100970121     C                   MOVEA     NFE           SKE
317200970121     C                   MOVEA     NFC           SKC
317300970121     C                   MOVEA     NFN           SKN
317400090918     C                   MOVEA     NFh           SKh
317500970121     C                   EXSR      SCRIVI
317600090918
317700970121     C*          TOTALE DELLE NON CONSEGNATE
317800970121     C                   MOVEL     2             SCOLIV
317900970121     C                   Z-ADD     5             SCOORD
318000970121     C                   MOVEL     'N'           WANRIT            1
318100970121     C                   MOVEL     'T'           SCOOLT
318200970121     C                   CLEAR                   WGBO
318300970121     C                   CLEAR                   SKE
318400970121     C                   CLEAR                   SKC
318500970121     C                   CLEAR                   SKN
318600090918     C                   CLEAR                   SKh
318700970121     C                   XFOOT     GAE           W0090             9 0
318800970121     C                   ADD       W0090         SKE(1)
318900970121     C                   XFOOT     LAE           W0090             9 0
319000970121     C                   ADD       W0090         SKE(1)
319100970121     C                   XFOOT     NFE           W0090             9 0
319200970121     C                   ADD       W0090         SKE(1)
319300970121     C                   XFOOT     CTE           W0090             9 0
319400970121     C                   ADD       W0090         SKE(1)
319500970121     C*
319600970121     C                   XFOOT     GAC           W0090             9 0
319700970121     C                   ADD       W0090         SKC(1)
319800970121     C                   XFOOT     LAC           W0090             9 0
319900970121     C                   ADD       W0090         SKC(1)
320000970121     C                   XFOOT     NFC           W0090             9 0
320100970121     C                   ADD       W0090         SKC(1)
320200970121     C                   XFOOT     CTC           W0090             9 0
320300970121     C                   ADD       W0090         SKC(1)
320400970121     C*
320500970121     C                   XFOOT     GAN           W0090             9 0
320600970121     C                   ADD       W0090         SKN(1)
320700970121     C                   XFOOT     LAN           W0090             9 0
320800970121     C                   ADD       W0090         SKN(1)
320900970121     C                   XFOOT     NFN           W0090             9 0
321000970121     C                   ADD       W0090         SKN(1)
321100970121     C                   XFOOT     CTN           W0090             9 0
321200970121     C                   ADD       W0090         SKN(1)
321300090918     C*
321400090918     C                   XFOOT     GAh           W0090             9 0
321500090918     C                   ADD       W0090         SKh(1)
321600090918     C                   XFOOT     LAh           W0090             9 0
321700090918     C                   ADD       W0090         SKh(1)
321800090918     C                   XFOOT     NFh           W0090             9 0
321900090918     C                   ADD       W0090         SKh(1)
322000090918     C                   XFOOT     CTh           W0090             9 0
322100090918     C                   ADD       W0090         SKh(1)
322200090918
322300970121     C                   EXSR      SCRIVI
322400090918
322500970121     C*  I N     D I S T I N T A
322600970121     C                   MOVEL     1             SCOLIV
322700970121     C                   MOVEL     '2'           WTOTLI
322800970121     C                   Z-ADD     15            SCOORD
322900970121     C                   MOVEL     'D'           SCOSCO
323000970121     C                   MOVEL     'S'           WANRIT            1
323100970121     C                   MOVEL     ' '           WGBO              1
323200970121     C                   MOVEA     IDE           SKE
323300970121     C                   MOVEA     IDC           SKC
323400970121     C                   MOVEA     IDN           SKN
323500090918     C                   MOVEA     IDh           SKh
323600970121     C                   EXSR      SCRIVI
323700970121     C*  I N     C O N S E G N A
323800970121     C                   MOVEL     1             SCOLIV
323900970121     C                   MOVEL     '3'           WTOTLI
324000970121     C                   Z-ADD     17            SCOORD
324100970121     C                   MOVEL     'I'           SCOSCO
324200970121     C                   MOVEL     'S'           WANRIT            1
324300970121     C                   MOVEL     ' '           WGBO              1
324400970121     C                   MOVEA     ICE           SKE
324500970121     C                   MOVEA     ICC           SKC
324600970121     C                   MOVEA     ICN           SKN
324700090918     C                   MOVEA     ICh           SKh
324800970121     C                   EXSR      SCRIVI
324900970121     C*  NON    I N     C O N S E G N A
325000970121     C                   MOVEL     1             SCOLIV
325100970121     C                   MOVEL     '3'           WTOTLI
325200970121     C                   Z-ADD     19            SCOORD
325300970121     C                   MOVEL     'O'           SCOSCO
325400970121     C                   MOVEL     'S'           WANRIT            1
325500970121     C                   MOVEL     ' '           WGBO              1
325600970121     C                   MOVEA     NCE           SKE
325700970121     C                   MOVEA     NCC           SKC
325800970121     C                   MOVEA     NCN           SKN
325900090918     C                   MOVEA     NCh           SKh
326000970121     C                   EXSR      SCRIVI
326100970121     C*  C O N S E G N A B I L I
326200970121     C                   MOVEL     4             SCOLIV
326300970121     C                   Z-ADD     20            SCOORD
326400970121     C                   MOVEL     'T'           SCOOLT
326500970121     C                   MOVEL     'B'           SCOSCO
326600970121     C                   MOVEL     'N'           WANRIT            1
326700970121     C                   MOVEL     ' '           WGBO              1
326800970121     C                   CLEAR                   SKE
326900970121     C                   CLEAR                   SKC
327000970121     C                   CLEAR                   SKN
327100090918     C                   CLEAR                   SKh
327200970121     C                   XFOOT     COE           SKE(1)
327300970121     C                   XFOOT     COC           SKC(1)
327400970121     C                   XFOOT     CON           SKN(1)
327500090918     C                   XFOOT     COh           SKh(1)
327600970121     C                   EXSR      SCRIVI
327700970121     C*
327800970121     C     COE(1)        IFGT      0
327900970121     C     COC(1)        ORGT      0
328000970121     C     CON(1)        ORGT      0
328100090918     C     COh(1)        ORGT      0
328200970121     C                   MOVEL     1             SCOLIV
328300970121     C                   Z-ADD     21            SCOORD
328400970121     C                   MOVEL     'B'           SCOSCO
328500970121     C                   MOVEL     ' '           SCOOLT
328600970121     C                   Z-SUB     999           SCOORE
328700970121     C                   MOVEL     ' '           SCOGBO
328800970121     C                   Z-ADD     COE(1)        SCOSS1
328900970121     C                   Z-ADD     COC(1)        SCOSS2
329000970121     C                   Z-ADD     CON(1)        SCOSS3
329100090918     C                   Z-ADD     COh(1)        SCOSS4
329200970121     C                   WRITE     FNSCO000
329300970121     C                   ENDIF
329400970121     C     COE(2)        IFGT      0
329500970121     C     COC(2)        ORGT      0
329600970121     C     CON(2)        ORGT      0
329700090918     C     COh(2)        ORGT      0
329800970121     C                   MOVEL     1             SCOLIV
329900970121     C                   Z-ADD     22            SCOORD
330000970121     C                   MOVEL     'B'           SCOSCO
330100970121     C                   MOVEL     ' '           SCOOLT
330200970121     C                   Z-ADD     0             SCOORE
330300970121     C                   MOVEL     ' '           SCOGBO
330400970121     C                   Z-ADD     COE(2)        SCOSS1
330500970121     C                   Z-ADD     COC(2)        SCOSS2
330600970121     C                   Z-ADD     CON(2)        SCOSS3
330700090918     C                   Z-ADD     COh(2)        SCOSS4
330800970121     C                   WRITE     FNSCO000
330900970121     C                   ENDIF
331000970121     C     COE(3)        IFGT      0
331100970121     C     COC(3)        ORGT      0
331200970121     C     CON(3)        ORGT      0
331300090918     C     COh(3)        ORGT      0
331400970121     C                   MOVEL     1             SCOLIV
331500970121     C                   Z-ADD     23            SCOORD
331600970121     C                   MOVEL     'B'           SCOSCO
331700970121     C                   MOVEL     ' '           SCOOLT
331800970121     C                   Z-ADD     999           SCOORE
331900970121     C                   MOVEL     ' '           SCOGBO
332000970121     C                   Z-ADD     COE(3)        SCOSS1
332100970121     C                   Z-ADD     COC(3)        SCOSS2
332200970121     C                   Z-ADD     CON(3)        SCOSS3
332300090918     C                   Z-ADD     COh(3)        SCOSS4
332400970121     C                   WRITE     FNSCO000
332500970121     C                   ENDIF
332600970130     C*
332700970130     C                   ENDIF
332800970121     C*
332900970121     C                   ENDSR
333000970121     C**************************************************************************
333100970121     C* SCRIVO I RECORD DI TOTALE IN FNSCO
333200970121     C**************************************************************************
333300970121     C     SCRIVI        BEGSR
333400970121     C*
333500970121    1C     WANRIT        IFEQ      'S'
333600970121     C                   CLEAR                   WREC
333700970121     C*
333800970121    2C                   DO        13            X                 3 0
333900970121    3C     SKE(X)        IFGT      0
334000970121     C     SKC(X)        ORGT      0
334100970121     C     SKN(X)        ORGT      0
334200090918     C     SKh(X)        ORGT      0
334300970121     C*
334400970121     c* se il campo e' -999 o + 999 --> e' l'OLTRE
334500970121     c*  determino di quanto oltre e'
334600970121     C                   SELECT
334700970121    4C     ORE(X)        WHENEQ    -999                                         OLTRE
334800970121     C     X             ADD       1             Y                 3 0
334900970122     C                   MOVEL     'A'           SCOOLT
335000970121     C                   Z-ADD     ORE(Y)        SCOORE
335100970121    4C     ORE(X)        WHENEQ    999                                          OLTRE
335200970121     C     X             SUB       1             Y                 3 0
335300970121     C                   MOVEL     'O'           SCOOLT
335400970121     C                   Z-ADD     ORE(Y)        SCOORE
335500970121     C                   OTHER
335600970122     C                   MOVEL     'D'           SCOOLT
335700970122     C                   Z-ADD     ORE(X)        SCOORE
335800970121    4C                   ENDSL
335900970121     C*
336000970121     C                   MOVEL     WGBO          SCOGBO
336100970121     C                   Z-ADD     SKE(X)        SCOSS1
336200970121     C                   Z-ADD     SKC(X)        SCOSS2
336300970121     C                   Z-ADD     SKN(X)        SCOSS3
336400090918     C                   Z-ADD     SKh(X)        SCOSS4
336500970121     C                   WRITE     FNSCO000
336600970121     C                   CLEAR                   SCOOLT
336700970121     C                   MOVEL     'S'           WREC              1
336800970121    3C                   ENDIF
336900970121    2C                   ENDDO
337000970121     c**
337100970121     C* SCRIVO ANCHE IL RECORD DI TOTALE se richiesto
337200970121    2C     WTOTLI        IFNE      'N'
337300970121     c* solo se ho gia' scritto almeno un record di affidabilita'
337400970121     C     WREC          ANDEQ     'S'
337500970121     C                   CLEAR                   SCOORE
337600970121     C* l'ordine  di stampa del record di totale e' sempre 1 in meno
337700970121     C*  del dettaglio suddiviso per affidabilita'
337800970121     C                   SUB       1             SCOORD
337900970121     C* il livello della stampa del totale e' spesso diverso dal livell
338000970121     C*  del relativo dettaglio affidabilita', ecco perche' c'e' un
338100970121     C*  campo a parte che viene passato in questa rourine
338200970121     C                   MOVEL     WTOTLI        SCOLIV
338300970121     C* per identificare il record di totale metto un "T" in SCOOLT
338400970121     C                   MOVEL     'T'           SCOOLT
338500970121     C                   XFOOT     SKE           SCOSS1
338600970121     C                   XFOOT     SKC           SCOSS2
338700970121     C                   XFOOT     SKN           SCOSS3
338800090918     C                   XFOOT     SKh           SCOSS4
338900970121     C                   WRITE     FNSCO000
339000970121     C                   CLEAR                   SCOOLT
339100970121     C                   ENDIF
339200090918     C* SOLO 1 RECORD se non c'e' suddivisione di affidabilita'
339300970121   X1C                   ELSE
339400970121    2C     SKE(1)        IFGT      0
339500970121     C     SKC(1)        ORGT      0
339600970121     C     SKN(1)        ORGT      0
339700090918     C     SKh(1)        ORGT      0
339800970121     C*
339900970121     C                   CLEAR                   SCOORE
340000970121     C                   MOVEL     WGBO          SCOGBO
340100970121     C                   Z-ADD     SKE(1)        SCOSS1
340200970121     C                   Z-ADD     SKC(1)        SCOSS2
340300970121     C                   Z-ADD     SKN(1)        SCOSS3
340400090918     C                   Z-ADD     SKh(1)        SCOSS4
340500970121     C                   WRITE     FNSCO000
340600970121     C                   CLEAR                   SCOOLT
340700970121    2C                   ENDIF
340800970121    1C                   ENDIF
340900970121     C*
341000050706     C                   ENDSR
341100050706     C**************************************************************************
341200050706     C* Per bolla locale desumo la data dell'arrivo bolla
341300050706     C**************************************************************************
341400050706     c     BOLLALOC      begsr
341500050706     c     kfvv8         SETGT     FNFVV08L
341600050706     c     kfvv81        READe     FNFVV08L
341700070420    6C                   IF        not %eoF(FNFVV08L) AND FVVDFV>=kdataloc
341800050706    7C                   IF        FVVFCF=' '  or wtipolancio='SA'
341900050706     C                   EVAL      Wartdam=FVVDFV
342000050706    7C                   ENDIF
342100050706     C*
342200050706   X6C                   ELSE
342300070420     C* SE NON TROVATO LE STAMPO SEMPRE CON DATA  + 1
342400070420     C                   Z-ADD     kdataloc      G02INV
342500050706     C                   MOVEL     '3'           G02ERR
342600050706     C                   CALL      'XSRDA8'
342700050706     C                   PARM                    WLBDAT
342800050706     C*
342900050706     C     G02TGI        ADD       1             GIOTGI
343000050706     C                   CALL      'XSRGI8'
343100050706     C                   PARM                    WGIDAT
343200050706     C*
343300050706     C                   Z-ADD     GIOINV        WARTDAM
343400050706    6C                   ENDIF
343500050706     c                   ENDSR
343600060314     C**************************************************************************
343700060314     C* Verifico se giorno festivo
343800060314     C**************************************************************************
343900060314     c     CTRFESTA      begsr
344000060314     c                   clear                   ktfp
344100060314     c                   movel     ds1lna        ktfa
344200060314     C     KCLN          CHAIN     AZCLN01L                           34
344300060314     C     *IN34         IFEQ      *OFF
344400060314     c
344500060314     C     MAT(KGG)      IFEQ      'F'
344600060314     C     POM(KGG)      ANDEQ     'F'
344700060314     c                   eval      wfest='S'
344800060314     c                   else
344900060314     c                   eval      wfest=' '
345000060314     c                   endif
345100060314     c
345200060314     c                   endif
345300060314     c                   ENDSR
345400950123     C**************************************************************************
345500950123     O*  ?  S T A T I S T I C A    C O N S E G N E  ?
345600920605     OPRTF198   E            TESTA            02
345700920605     O                       TE(1)               66
345800940324     O                 N02   TE(2)              132
345900940324     O                  02   TE(3)              132
346000920605     O                       TE(4)              198
346100900523     O                       RSUT                20
346200970226     O               15                          47 '- RISTAMPA -'
346300970207     O                       DS1GDA             119 '  /  /    '
346400970207     O                  02   DS1GAL             136 '  /  /    '
346500950123     O                       WDATE              180 '  /  /    '
346600920605     O                       PAGE          Z    198
346700920605     O          E            TESTA       1
346800950123     O                       KNSIF               10
346900950123     O                       KNMUS               22
347000950123     O                       UTIME              179 '  :  :  '
347100940208     O          E            TESTA            04
347200920605     O                       TE(6)               66
347300920605     O                       TE(6)              132
347400920605     O                       TE(6)              198
347500090918     o* Senza tipo servizio H
347600090918     O          E   n25      TESTA            05
347700920605     O                       TE(7)               66
347800920605     O                       TE(10)             132
347900930908     O                       TE(14)             198
348000090604     o                       wdtspc              62
348100090604     o                       wdtspe              70
348200090604     o                       wdtspc             106
348300090604     o                       wdtspe             114
348400090604     o                       wdtspc             145
348500090604     o                       wdtspe             154
348600090604     o                       wdtspc             178
348700090604     o                       wdtspe             187
348800090918     O          E   n25      TESTA            06
348900920605     O                       TE(8)               66
349000920608     O                       TE(11)             132
349100930908     O                       TE(15)             198
349200970225     O                       DS1LNA              34
349300940323     O                       DESLNA              54
349400090918     O          E   n25      TESTA            07
349500940323     O                       TE(5)               66
349600930812     O                       TE(12)             132
349700930908     O                       TE(16)             198
349800090604     o                       wdtspd              62
349900090604     o                       wdtspd             106
350000090604     o                       wdtspd             145
350100090604     o                       wdtspd             178
350200090918     o* con tipo servizio H
350300090918     O          E    25      TESTA            05
350400090918     O                       TE(7)               66
350500090918     O                       TE(41)             132
350600090918     O                       TE(45)             198
350700090918     o                       wdtspc              62
350800090918     o                       wdtspe              70
350900090918     o                       wdtsph              78
351000100114     o                       wdtspc             112
351100100114     o                       wdtspe             121
351200100114     o                       wdtsph             130
351300100114     o                       wdtspc             157
351400100114     o                       wdtspe             166
351500100114     o                       wdtsph             175
351600090918     O          E    25      TESTA            06
351700090918     O                       TE(8)               66
351800090918     O                       TE(42)             132
351900090918     O                       TE(46)             198
352000090918     O                       DS1LNA              34
352100090918     O                       DESLNA              54
352200090918     O          E    25      TESTA            07
352300090918     O                       TE(5)               66
352400090918     O                       TE(43)             132
352500090918     O                       TE(47)             198
352600090918     o                       wdtspd              62
352700100114     o                       wdtspd             112
352800100114     o                       wdtspd             157
352900940208     O          E            TESTA            08
353000920608     O                       TE(6)               66
353100920608     O                       TE(6)              132
353200920608     O                       TE(6)              198
353300950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
353400930908     O* 1 INTESTAZIONE
353500090918     O          E   n25      INTE1       2
353600930908     O                       RIGA                66
353700930908     O                       TE(36)             132
353800930908     O                       TE(37)             198
353900090918     O                       GENNOR        Z    106
354000940203     O                                          107 '+'
354100940203     O                       GENESP        Z    115
354200940203     O                                          116 '='
354300940203     O                       GENTOT        Z    124
354400940324     O                 N02   CONP1              130 '0 ,  '
354500940324     O                 N02   CONP2              136 '0 ,  '
354600930909     O*
354700090918     O          E   n25      INTE1       0  0
354800930909     O                       RIGA                66
354900930909     O                       TE(36)             132
355000930909     O                       TE(37)             198
355100940203     O                       GENNOR        Z    106
355200940203     O                                          107 '+'
355300940203     O                       GENESP        Z    115
355400940203     O                                          116 '='
355500940203     O                       GENTOT        Z    124
355600940324     O                 N02   CONP1              130 '0 ,  '
355700940324     O                 N02   CONP2              136 '0 ,  '
355800090918     c* con tipo servizio H
355900090918     O          E    25      INTE1       2
356000090918     O                       RIGA                66
356100090918     O                       TE(49)             132
356200090918     O                       TE(50)             198
356300100114     O                       GENNOR        Z    112
356400100114     O                                         +  0 '+'
356500090918     O                       GENESP        Z   +  0
356600090918     O                                         +  0 '+'
356700090918     O                       GENh10        Z   +  0
356800090918     O                                         +  0 '='
356900100114     O                       GENTOT        Z   +  1
357000090918     O                 N02   CONP2             +  1 '0 ,  '
357100090918     O*
357200090918     O          E    25      INTE1       0  0
357300090918     O                       RIGA                66
357400090918     O                       TE(49)             132
357500090918     O                       TE(50)             198
357600100114     O                       GENNOR        Z    112
357700100114     O                                         +  0 '+'
357800090918     O                       GENESP        Z   +  0
357900090918     O                                         +  0 '+'
358000090918     O                       GENh10        Z   +  0
358100090918     O                                         +  0 '='
358200100114     O                       GENTOT        Z   +  1
358300090918     O                 N02   CONP2             +  1 '0 ,  '
358400950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
358500090918     O* 2 INTESTAZIONE
358600090918     O          E   n25      INTE2       2
358700930908     O                       RIGA                66
358800930908     O                       TE(36)             132
358900930908     O                       TE(37)             198
359000940203     O                       GENNOR        Z    145
359100940203     O                                          146 '+'
359200940203     O                       GENESP        Z    154
359300940203     O                                          155 '='
359400940203     O                       GENTOT        Z    163
359500940203     O                       CONP2              169 '0 ,  '
359600090918     O          E   n25      INTE2       0  0
359700930909     O                       RIGA                66
359800930909     O                       TE(36)             132
359900930909     O                       TE(37)             198
360000940203     O                       GENNOR        Z    145
360100090918     O                                          146 '+'
360200940203     O                       GENESP        Z    154
360300940203     O                                          155 '='
360400940203     O                       GENTOT        Z    163
360500940203     O                       CONP2              169 '0 ,  '
360600090918     o* con tipo servizio H
360700090918     O          E    25      INTE2       2
360800090918     O                       RIGA                66
360900090918     O                       TE(49)             132
361000090918     O                       TE(50)             198
361100100114     O                       GENNOR        Z    157
361200090918     O                                         +  0 '+'
361300090918     O                       GENESP        Z   +  0
361400090918     O                                         +  0 '+'
361500090918     O                       GENh10        Z   +  0
361600090918     O                                         +  0 '='
361700090918     O                       GENTOT        Z   +  0
361800100114     O              n26      CONP2              190 '0 ,  '
361900090918     O          E    25      INTE2       0  0
362000090918     O                       RIGA                66
362100090918     O                       TE(49)             132
362200090918     O                       TE(50)             198
362300100114     O                       GENNOR        Z    157
362400090918     O                                         +  0 '+'
362500090918     O                       GENESP        Z   +  0
362600090918     O                                         +  0 '+'
362700090918     O                       GENh10        Z   +  0
362800090918     O                                         +  0 '='
362900090918     O                       GENTOT        Z   +  0
363000100114     O              n26      CONP2              190 '0 ,  '
363100950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
363200090918     O* 3 INTESTAZIONE  solo senza tipo servizio H
363300940118     O          E            INTE3       2
363400930909     O                       RIGA                66
363500930909     O                       TE(36)             132
363600930909     O                       TE(37)             198
363700940203     O                       GENNOR        Z    178
363800940203     O                                          179 '+'
363900940203     O                       GENESP        Z    187
364000940203     O                                          188 '='
364100940203     O                       GENTOT        Z    196
364200930909     O          E            INTE3       0  0
364300930909     O                       RIGA                66
364400930909     O                       TE(36)             132
364500930909     O                       TE(37)             198
364600940203     O                       GENNOR        Z    178
364700940203     O                                          179 '+'
364800940203     O                       GENESP        Z    187
364900940203     O                                          188 '='
365000940203     O                       GENTOT        Z    196
365100950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
365200930908     O* 1 DETTAGLIO
365300090918     O          E   n25      DET1        1
365400930812     O                       RIGA                66
365500930908     O                       TE(36)             132
365600930908     O                       TE(37)             198
365700930908     O                       ORE(Y)        Z     48
365800970121     O                       PNORM         Z     62
365900930908     O                                           63 '+'
366000970121     O                       PESPR         Z     70
366100930908     O                                           71 '='
366200970121     O                       PTOTAL        Z     79
366300940526     O              N03      CONP1               85 '0 ,  '
366400940526     O              N04N02   CONP2               91 '0 ,  '
366500940324     O                 N02   CONP3               97 '0 ,  '
366600090918     o* con tipo servizio H
366700090918     O          E    25      DET1        1
366800090918     O                       RIGA                66
366900090918     O                       TE(49)             132
367000090918     O                       TE(50)             198
367100090918     O                       ORE(Y)        Z     48
367200090918     O                       PNORM         Z     62
367300090918     O                                         +  0 '+'
367400090918     O                       PESPR         Z   +  0
367500090918     O                                         +  0 '+'
367600090918     O                       ph1030        Z   +  0
367700090918     O                                         +  0 '='
367800100114     O                       PTOTAL        Z   +  0
367900100114     O              N03      CONP1               94 '0 ,  '
368000100114     O                 N02   CONP3              101 '0 ,  '
368100950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
368200930908     O* 2 DETTAGLIO
368300940203     O          E            DET4        1
368400940203     O                                              ' '
368500950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
368600090918     O          E      n25   DET2        1
368700930908     O                       RIGA                66
368800930908     O                       TE(36)             132
368900930908     O                       TE(37)             198
369000930908     O                       GENNOR        Z     62
369100930908     O                       GENESP        Z     70
369200940203     O                                           55 ''''
369300940203     O                                           63 '+'
369400930908     O                                           71 '='
369500930908     O                       GENTOT        Z     79
369600940329     O              N01   N03CONP1               85 '0 ,  '
369700940324     O              N01N02   CONP2               91 '0 ,  '
369800940324     O                 N02   CONP3               97 '0 ,  '
369900090918     O          E   N01n25   DET2        0  0
370000940207     O                       RIGA                66
370100940207     O                       GENNOR        Z     62
370200940207     O                       GENESP        Z     70
370300940207     O                                           63 '+'
370400940207     O                                           71 '='
370500940207     O                       GENTOT        Z     79
370600090918     O
370700940324     O                 N02   CONP2               91 '0 ,  '
370800940324     O                 N02   CONP3               97 '0 ,  '
370900090918     o* con tipo servizio H
371000090918     O          E       25   DET2        1
371100090918     O                       RIGA                66
371200090918     O                       TE(49)             132
371300090918     O                       TE(50)             198
371400090918     O                       GENNOR        Z     62
371500090918     O                                           55 ''''
371600090918     O                       GENESP        Z     70
371700090918     O                       GENh10        Z     78
371800090918     O                       GENTOT        Z     87
371900090918     O                                           63 '+'
372000090918     O                                           71 '+'
372100090918     O                                           79 '='
372200100114     O              N01   N03CONP1               94 '0 ,  '
372300100114     O                 N02   CONP3              101 '0 ,  '
372400090918     O          E   N01 25   DET2        0  0
372500090918     O                       RIGA                66
372600090918     O                       GENNOR        Z     62
372700090918     O                       GENESP        Z     70
372800090918     O                       GENh10        Z     78
372900090918     O                       GENTOT        Z     87
373000090918     O                                           63 '+'
373100090918     O                                           71 '+'
373200090918     O                                           79 '='
373300100114     O              N01   N03CONP1               94 '0 ,  '
373400100114     O                 N02   CONP3              101 '0 ,  '
373500950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
373600920609     O          E            DET3        2
373700920608     O                       TE(6)               66
373800920608     O                       TE(6)              132
373900920608     O                       TE(6)              198
374000950123     C**************************************************************************
374100950124     O*  ?  L I S T A  ?
374200920706     OQSYSPRT   E            SPED1          1 02
374300950124     O                                         +  3 'ANNO'
374400920706     O                                         +  2 'LNP'
374500920706     O                                         +  2 'SER'
374600920706     O                                         +  2 'SPEDIZ'
374700950124     O                                         +  1 'DATA SPED'
374800920706     O                                         +  2 'COLLI'
374900950124     O                                         +  3 'DATA ARR'
375000950124     O                                         +  2 'ABIL'
375100920706     O                                         +  1 'NUM DIST'
375200920706     O                                         +  2 'DAT DIST'
375300950124     O                                         +  2 'ULT CON'
375400950124     O***************                  +  1 'L.AVVISO'
375500920706     O                                         +  1 'CONSEGNA'
375600950124     O                                         +  3 'CON PARZ'
375700950124     O                                         +  2 'BLOCCO SP'
375800950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
375900920706     OQSYSPRT   E            SPED        1
376000940208     O                       STAFLG
376100940208     O                       ARBAAS            +  2
376200940208     O                       ARBLNP        Z   +  2
376300920706     O                       ARBNRS        Z   +  2
376400920706     O                       ARBNSP        Z   +  2
376500920706     O                       ARBMGS            +  2 '  /  '
376600920706     O                       ARBTSP            +  2
376700920706     O                       ARBNCL        Z   +  2
376800950124     O                       ARBDAM            +  2 '    /  /  '
376900950124     O                       ARBAMA            +  3
377000920706     O                       ARBNDC        Z   +  3
377100950124     O                       ARBDDC            +  2 '    /  /  '
377200950124     O                       ARBTMC            +  3
377300950124     O***************          ARBDLA  +  4 '  /  /  '
377400950124     O                       ARBDCM            +  3 '    /  /  '
377500950124     O                       ARBDCP            +  2 '    /  /  '
377600950123     O                       ARBFBC            +  3
377700950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
377800940203     O          E            SPEDT       1
377900940207     O                                              'CONSEGNATE'
378000940203     O                       STAC          Z   +  1
378100940208     O                                         +  3 'CONSEGNE PARZ'
378200940208     O                       STAP          Z   +  1
378300940208     O                                         +  3 'RISERVA CONS'
378400940208     O                       STAR          Z   +  1
378500940207     O                                         +  4 'IN DISTINTA'
378600940203     O                       STAI          Z   +  1
378700940207     O                                         +  4 'NON CONSEGNATE'
378800940203     O                       STAN          Z   +  1
378900940208     O          E            SPEDT       1
379000940208     O                                              'GIACENZE'
379100940208     O                       STAG          Z   +  1
379200950124     O                                         +  4 'L.AVVISI'
379300950124     O                       STAA          Z   +  1
379400940208     O                                         +  4 'CHIUSE X TURNO'
379500940208     O                       STAT          Z   +  1
379600940208     O                                         +  4 'DA CONSEGNARE'
379700940208     O                       STAD          Z   +  1
379800950123     O**************************************************************************
379900950123     O*  ?  ELENCO  SPEDIZIONI  NON  IN  CONSEGNA  ?
380000940601     OPRTF198X  E            TESELE           02
380100040608     O              n08      ELE(2)             132
380200040608     O              n08      ELE(3)             198
380300040608     O               08      ELE(14)            132
380400040608     O               08      ELE(15)            198
380500940530     O                       RSUT                20
380600950123     O                       WDATE              180 '  /  /    '
380700950123     O                       PAGE1         Z    198
380800940530     O          E            TESELE           03
380900950123     O                       KNSIF               10
381000950123     O                       KNMUS               22
381100940601     O                       ELE(4)             132
381200100222     o                       p0dpart             80
381300970207     O                       DS1GDA             104 '  /  /    '
381400950123     O                       UTIME              179 '  :  :  '
381500940530     O          E            TESELE           05
381600940601     O                       ELE(5)              66
381700940601     O                       ELE(8)             132
381800940601     O                       ELE(11)            198
381900940530     O          E            TESELE         1 06
382000940601     O                       ELE(6)              66
382100940601     O                       ELE(9)             132
382200940601     O                       ELE(12)            198
382300950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
382400940530     O          E            DETELE         1
382500950127     O                       P0CAAS
382600940530     O                       ARBLNP        Z   +  1
382700940530     O                       ARBNRS        Z   +  1
382800940530     O                       ARBNSP        Z   +  1
382900940530     O                       P0CMGS            +  1 '  /  '
383000940531     O                       ARBTSP            +  1
383100940601     O                       ARBLNA        Z   +  1
383200940601     O                       ARBZNC        Z   +  2
383300991130     O                       P0CRSD            +  1
383400950125     O                       P0CIND            +  1
383500940601     O                       ARBCAD            +  1
383600950127     O                       P0CLOD            +  0
383700940601     O                       ARBPRD            +  1
383800950127     O                       ARBNZD            +  1
383900940601     O                       ARBGC1            +  1
384000940601     O                       ARBGC2            +  1
384100950123     O                       ARBTC1            +  1
384200940601     O                                         +  0 '-'
384300950127     O                       P0DTC1            +  0
384400950123     O                       ARBNCL        Z   +  0
384500950127     O                       ARBPKF        4   +  0
384600991130     O                       ARBCBO            +  1
384700940530     O                                         +  0 '-'
384800950127     O                       P0DCBO            +  0
384900950125     O                       AR9CAS        4   +  0
385000991130     O                       AR9VCA            +  0
385100991130     O                       COMIFT            +  1
385200991130     O                       comdiv            +  0
385300991130     O                       ARBTMC            +  1
385400051128     O                       p0cTCR            +  1
385500940531     O                       P0CDCR        Z   +  1
385600940530     O                       ARBHCR            +  1 '  :  '
385700100222     O                       ARBgma            +  1
385800141119     O              n70      NUM3          M   +  1
385900141119     O               70                         198 'DIR'
386000950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
386100950127     O          E            DE2ELE         1
386200990329     O               07                             'C.A.'
386300990329     O               07      DCTFIL               8
386400990329     O               07      DCTNCA        Z     15
386500990329     O               07                          19 'del'
386600990329     O               07      STADCA              28 '  /  /  '
386700990329     O               07    06                    35 'chiusa'
386800990329     O               07    06STACHC              44 '  /  /  '
386900990329     O               07    06DCTCCH            +  1
387000990329     O               07    06                  +  0 '-'
387100990329     O               07    06STACCH            +  0
387200991130     O                       ARBTC2             114
387300991130     O                                          115 '-'
387400991130     O                       P0DTC2             125
387500991130     O                       P0CNO1             161
387600991130     O                       P0CNO2             197
387700990329     O          E            DETCA          1
387800990329     O                                              'C.A.'
387900990329     O                       DCTFIL               8
388000990329     O                       DCTNCA        Z     15
388100990329     O                                           19 'del'
388200990329     O                       STADCA              28 '  /  /  '
388300990329     O                     06                    35 'chiusa'
388400990329     O                     06STACHC              44 '  /  /  '
388500990329     O                     06DCTCCH            +  1
388600990329     O                     06                  +  0 '-'
388700990329     O                     06STACCH            +  0
388800060905     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
388900060905      * spedizione in distinta
389000060905     O          E    12      DETDIS         1
389100060905     O                       wstadis            197
389200000703     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
389300060905      * C/ASSEGNO NON ABILITATO
389400000703     O          E            DETCAA         1
389500110923     O              n18 28                      159 'Non Abilitato'
389600110923     O               18 28                       53 'Abilitazioni Bolla:'
389700110923     O               18 28   olrg6vssin       +   1
389800110923     O               19                       +   4 'Perfect Order:'
389900110927     O               19      stPorder         +   1
390000050805     O          E            FINES       2
390100050805     o                                          108 '*** FINE STAMPA ***'
390200950123     O**************************************************************************
390300950125**
390400090918XXXXXXXXXXXXXXXXXXXX                                      S T A T    1
390500950124I S T I C A    C O N S E G N E    D E L                              2
390600950124I S T I C A    C O N S E G N E    D A L                A L           3
390700950124                      FNLR78R                            PAG. XXXX   4
390800950123                                                      '  DISTR       5
390900930907------------------------------------------------------------------   6
391000090918                                                      '  yyyyy   y   7
391100950123L I N E A   D I   A R R I V O: XXX-XXXXXXXXXXXXXXXXXXX'    +         8
391200090917                                                      '1234567+123   9
391300090917SPR   TOTALE ' % SU' % SU' % SU'   CORR    ESPR    TOTALE ' % SU'   10
391400090917             'CONS.'TOTAL'CONSE'     +                    'TOTAL'C  11
391500090917             'PERIO'IN CO' GNAB'   DISTR                  'IN CO'   12
3916000909174567=12345678'XX,XX'XX,XX'XX,XX'XXXXXXXX+Xxxxxxxx=XXXXXXXX'XX,XX'X  13
391700090918% SU'   yyyyy    yyyyy  TOTALE ' % SU'   yyyyy    yyyyy  TOTALE     14
391800090917ONSE'     +                    'CONSE'     +                        15
391900090917GNAB'   DISTR                  ' GNAB'   DISTR                      16
392000090917X,XX'Xxxxxxxx+xXxxxxxx=xxxxxxxx'XX,XX'xXxxxxxx+xxxXxxxx=xxxxxxxx    17
392100090917SPEDIZIONI           C O N S E G N A T E:             '             18
392200940203                           In Anticipo oltre xxx Ore  '             19
392300940203                           In Anticipo di    xxx Ore  '             20
392400940323                           In CONFORMITA'             '              1
392500940203                           In Ritardo  di    xxx Ore  '              2
392600940203                           In Ritardo  oltre xxx Ore  '              3
392700970130SPEDIZIONI   N O N   C O N S E G N A T E:             '             24
392800940203              di cui  "GIACENZE APERTE"               '             25
392900940203              di cui  "LASCIATI AVVISI"               '             26
393000940203              di cui  "NON  FATTE"                    '             27
393100940203              di cui  "CHIUSE PER TURNO"              '             28
393200970130SPEDIZIONI   ANCORA  I N   D I S T I N T A:           '             29
393300970130TOTALE SPED.         I N   C O N S E G N A:           '             30
393400970130SPEDIZIONI   N O N   I N   C O N S E G N A:           '             31
393500970130TOTALE SPED. C O N S E G N A B I L I:                 '             32
393600940323                                       In Anticipo    '             33
393700940323                                       In CONFORMITA' '             34
393800940323                                       In Ritardo     '             35
393900940203             '     '     '     '                          '     '   36
394000940203    '                          '     '                              37
394100940208              di cui  "CONSEGNE PARZIALI"             '             38
394200940208              di cui  "RISERVE  CONSEGNE"             '             39
394300090917                                                      '1234567+123  40
394400100114yyyy   yyyyy  TOTALE ' % SU ' % SU '     yyyyy    yyyyy    yyyyy
394500100114                     ' CONS.' CONSE'       +
394600100114                     'PERIOD' GNAB.'     DISTR
3947001001144567+1234567=12345678' xx,xx' XX,XX'  XXXXXXXX+Xxxxxxxx+12345678=
394800100114 TOTALE ' % SU'     yyyyy    yyyyy    yyyyy  TOTALE ' % SU'         45
394900100114        'CONSE'       +                             'CONSE'         46
395000100114        ' GNAB'     DISTR                           ' GNAB'         47
39510010011412345678'XX,XX'  12345678+12345678+12345678=12345678'XX,XX'         48
395200100114                     '      '      '                                49
395300100114        '     '                                     '     '         50
395400940324** Comandi    567890123456789012345678901234567890123456789012345678901234567890
395500940324OVRPRTF FILE(PRTF198)  USRDTA('Stat.Cons.')
395600940530** Comandi    567890123456789012345678901234567890123456789012345678901234567890
395700940601OVRPRTF FILE(PRTF198X) USRDTA('Stat.Cons.')
395800940530**
395900090918XXXXXXXXXXXXXXXXXXXX                                                 1
396000940601         **  ELENCO  SPEDIZIONI  NON  IN  CONSEGNA  **               2
396100950125                       FNLR78R                           PAG. XXXX   3
396200950125                        AL  XX/XX/XXXX                               4
396300991130   S P E D I Z I O N E     ARRIVO      D    E    S    T    I   N     5
396400991130                          LIN ZON RAGIONE SOCIALE INDIRIZZO          6
396500950125                                                                     7
396600991130  A    T    A    R    I    O              GG   CONSEG COLLI    PES   8
396700991130         C.A.P    LOCALITA'      PRO NAZ CHIUS PARTIC                9
396800950125                                                                    10
396900100222O CD.BOLLA     C/ASSEGNO       TOTALE   MAN CONSEGNA   PARTIC ORE   11
397000100222                              FATTURA   CON RICHIESTA    CONS.RIT.  12
397100950125                                                                    13
397200040608      **  ELENCO SPEDIZIONI NON IN CONSEGNA: considera spunte part  14
397300040608enza  **                  FNLR78R                        PAG.       15

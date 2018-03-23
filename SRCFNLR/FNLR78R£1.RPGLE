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
022900110926     d
023000040812      * ds per il controllo della presenza di CA per la spedizione richiesta
023100040812     d FIDN12DS      e ds
023200040812     d  skKey                 26    305    dim(20)
023300040812     d  skAnn                306    325    dim(20)
023400040812     d  skDca                326    485  0 dim(20)
023500040812     d  skFca                486    545  0 dim(20)
023600040812     d  skDch                546    705  0 dim(20)
023700040812     d  skCch                706    745    dim(20)
023800040812
023900040812     d dsKey           ds
024000040812     d  dsaac                         4  0
024100040812     d  dsfil                         3  0
024200040812     d  dsnca                         7  0
024300040812      *
024400060905     D Cindistinta     C                   CONST('In Distinta n.xxxxx-
024500060905     D                                     xx  del xx/xx/xx')
024600100222     D CEscludiPar     C                   CONST('Escludi partConsegna"AT-
024700100222     D                                     TESA"per ConsRich.non raggiunta')
024800900523     C****************************************************************
024900900523     C*  RIEPILOGO INDICATORI
025000920306     C****************************************************************
025100940207     C* 01    - CONTROLLO LA STAMPA DELLE % NEL TOTALE CONSEGNABILE
025200940323     C* 02    - IMMESSA DATA AL
025300940329     C* 03    - STAMPO LA % SUL TOT.CONSEGNATO SOLO PER LE CONSEGNATE
025400040608     C* 04    -
025500040608     C* 05    - EVENTO RISERVA CONSEGNA
025600990329     C* 06-07 - PER STAMPARE LE C.A.
025700040608     C* 08    - lista non in consegna con spunte partenza
025800090916     C* 10    - SPEDIZIONE "E"- priority
025900090916     C* 11    - SPEDIZIONE "C" - corriere
026000090916     C* 12    -
026100090916     C* 13    -
026200970226     C* 15    - SI TRATTA DI UNA RISTAMPA
026300970226     C* 16    - LA SPEDIZIONE E' NON CONSEGNATA
026400940329     C* 17    - LA SPEDIZIONE NON E' COMPLETAMENTE ARRIVATA
026500070419     C* 18    - GEO attiva
026600110923     C* 19    - Perfect order
026700090916     C* 20    - SPEDIZIONE "D" distribuzione
026800090916     C* 21    - SPEDIZIONE "H" H1030
026900090917     C* 25    - servizio h1030 attivo
027000090918     C* 26    - non stampo la % finale
027100110923     C* 28    - Stampo le abilitazioni
027200920605     C* 30    - COMODO
027300950314RM*  C* 36    - FILIALE NON GESTITA DA SECONDO LIVELLO
027400950123     C* 34    - LETTURA FNARB54L
027500000531     C* 72    - Usato con CHAIN AZORG01L x controllo se bolla ha LNP Poste
027600000609     C* 73    - Usato con OG143 x controllo se bolla ha LNP Poste
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
077700040526     c
077800900523     C                   SETON                                        LR
077900991130      **********************************************************************
078000991130      * EDITO IMPORTI PER CAMPI in stampa
078100991130      **********************************************************************
078200991130     C     EDTIMPORTI    BEGSR
078300991130
078400991130      * -----------------------------------------------------------------------------------
078500991130      * W0122      contiene il numero da editare
078600991130      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il num
078700991130      *            ero ha decimali
078800991130      * IMPEDITATO ritorna il valore editato per la stampa lungo13 (troncate
078900991130      *            cifre oltre 10 mld)
079000991130      * -----------------------------------------------------------------------------------
079100991130
079200991130     c                   clear                   IMPeditato
079300991130     c                   clear                   W013A
079400991130     c                   if        Wtstdec = 'S'
079500991130     C                   eval      W013a = %EDITC(W0122:'4')
079600991130     c                   else
079700991130     c                   z-add     W0122         W0130
079800991130     C                   eval      W013a = %EDITC(W0130:'4')
079900991130     c                   endif
080000991130     c
080100991130     c                   move      W013a         IMPeditato
080200991130     c                   ENDSR
080300950124     C**************************************************************************
080400950124     C* OPERAZIONI INIZIALI
080500950124     C**************************************************************************
080600930910     C     INIZIO        BEGSR
080700950123     C*
080800950123     C                   TIME                    WTIME            14 0          ORA & DATA
080900950123     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
081000950124     C                   MOVEL     WTIME         UTIME             6 0          ORARIO
081100950127     C*
081200950127     C* INVERTO LA DATA DEL GIORNO
081300950127     C                   Z-ADD     WDATE         G02DAT
081400950127     C                   Z-ADD     *ZERO         G02INV
081500950127     C                   MOVEL     *BLANKS       G02ERR
081600950127     C                   CALL      'XSRDA8'
081700950127     C                   PARM                    WLBDAT
081800950127     C                   Z-ADD     G02INV        WDATE1            8 0          AAAA/MM/GG
081900930910     C*
082000940323     C* 02 ON - IMMESSA DATA AL
082100970207     C     DS1AAL        IFNE      0
082200970207     C     DS1ADA        IFNE      DS1AAL
082300940323     C                   SETON                                        02
082400940323     C                   END
082500940323     C                   ELSE
082600970207     C                   Z-ADD     DS1ADA        DS1AAL
082700940323     C                   END
082800930907     C*
082900930907     C* 09 ON - LINEA ARRIVO IMMESSA
083000060111     C***  DS1LNA        COMP      0                                  09
083100930907     C*
083200930910     C* REPERISCO VARIABILI ARRIVI
083300930907     C                   MOVEL     '7A'          COD
083400930907     C                   MOVEL     *BLANKS       KEY
083500930907     C                   MOVEL     '1'           KEY
083600970124     C     KTAB          CHAIN(N)  TABEL                              31
083700930907     C  N31              MOVEL     TBLUNI        DS7A
083800930907     C   31              CLEAR                   DS7A
083900930907     C* CARICO UNICA SCHIERA DEI RITARDI E CONSEGNE
084000930907     C                   Z-ADD     5             X                 3 0
084100930907     C                   Z-ADD     2             Y                 3 0
084200930907     C                   Z-ADD     -999          ORE(1)
084300930907     C*
084400930907     C                   DO        5
084500930907     C     ANT(X)        IFGT      0
084600930907     C     0             SUB       ANT(X)        ORE(Y)
084700930907     C                   ADD       1             Y
084800930907     C                   ENDIF
084900930907     C                   SUB       1             X
085000930907     C                   ENDDO
085100930907     C*
085200930907     C                   Z-ADD     1             X                 3 0
085300930907     C*
085400930907     C                   DO        5
085500930907     C     RIT(X)        IFGT      0
085600930907     C                   ADD       1             Y
085700930907     C                   Z-ADD     RIT(X)        ORE(Y)
085800930907     C                   ENDIF
085900930907     C                   ADD       1             X
086000930907     C                   ENDDO
086100930907     C*
086200930907     C                   ADD       1             Y
086300930907     C                   Z-ADD     999           ORE(Y)
086400940210     C* CARICO I TIPI BOLLA RECUPERO CHE DEVO SCARTARE
086500940210     C                   Z-ADD     1             X
086600940210     C                   MOVEL     '3A'          COD
086700970124     C     KTAB2         CHAIN(N)  TABEL                              30
086800940210     C*
086900940210     C     *IN30         DOWEQ     *OFF
087000940210     C     TBLFLG        IFEQ      ' '
087100940210     C                   MOVEL     TBLUNI        DS3A
087200940210     C     §3ARBL        IFEQ      'R'
087300940210     C                   MOVEL     TBLKEY        REC(X)
087400940210     C                   ADD       1             X
087500940210     C                   ENDIF
087600940210     C                   ENDIF
087700940210     C*
087800970124     C     KTAB2         READE(N)  TABEL                                  30
087900940210     C                   ENDDO
088000091112     c
088100091112     C* CARICO gli eventi particolari
088200091112     C                   Z-ADD     1             X
088300091112     C                   MOVEL     '2Z'          COD
088400091112     C     KTAB2         CHAIN(N)  TABEL                              30
088500091112     C*
088600091112     C     *IN30         DOWEQ     *OFF
088700091112     C     TBLFLG        IFEQ      ' '
088800091112     C                   MOVEL     TBLUNI        DS2Z
088900091112     C     §2zcep        IFne      ' '
089000091112     C                   MOVEL     TBLKEY        CEP(X)
089100091112     C                   ADD       1             X
089200091112     C                   ENDIF
089300091112     C                   ENDIF
089400091112     C*
089500091112     C     KTAB2         READE(N)  TABEL                                  30
089600091112     C                   ENDDO
089700930907     C*
089800091112     c*
089900940530     C* OVRPRTF SU PRTF198
090000091112     C                   Z-ADD     43            LUNG             15 5
090100970225     C     DS1FL1        IFEQ      'S'
090200970225     C     DS1FL1        OREQ      'E'
090300940324     C                   MOVEA     CMD           COMMAN           80
090400940324     C*
090500940329     C                   CALL      'QCMDEXC'                              98                A
090600940324     C                   PARM                    COMMAN
090700940324     C                   PARM                    LUNG
090800940329     C  N98              OPEN      PRTF198
090900950410     C                   END
091000940601     C* OVRPRTF SU PRTF198X
091100970225     C  N02
091200970225     CANN98DS1FL1        IFEQ      'L'
091300970225     C     DS1FL1        OREQ      'E'
091400970403     C     DS1FL3        OREQ      'L'
091500940530     C                   MOVEA     CM2           COMMAN           80
091600940530     C*
091700940530     C                   CALL      'QCMDEXC'                              98                A
091800940530     C                   PARM                    COMMAN
091900940530     C                   PARM                    LUNG
092000940601     C  N98              OPEN      PRTF198X
092100940530     C                   ENDIF
092200020429     C* SE RICHIESTA STATISTICA PER TUTTE LE FILIALI GESTITE
092300020429     C* E SE SONO UN SECONDO LIVELLO CARICO LA £6
092400060110     c***  simtpp        ifeq      '2'
092500060110     c***  simtpp        oreq      *blanks
092600060110     C***  DS1LNA        ifeq      *ZEROS
092700950314RM*  C                   CLEAR                   DSUL06
092800950314RM*  C                   MOVE      '£6'          D06COD
092900060111     C***                movel     simpou        D06KEY
093000060111     C                   movel     ds1lna        D06KEY
093100950314RM*  C                   MOVEL     'L'           D06TLA
093200060118 RM* C                   MOVEL     ds1aal        D06drf
093300060118 RM* C                   MOVE      'S'           D06key
093400950314RM*  C                   MOVEL     DSUL06        KPJBU
093500950314RM*  C                   CALL      'TRUL06R'
093600950314RM*  C                   PARM                    KPJBA
093700950314RM*  C                   MOVEL     KPJBU         DSUL06
093800950314RM*  C                   MOVEA     LIN           L6
093900060110RM*  C***                END
094000060110     C***                END
094100970128     C*
094200970130     C                   CLEAR                   WCONS
094300970130     C                   CLEAR                   WFEST
094400100222     c*
094500100222     C**                 CLEAR                   dsbs02
094600100222     c**                 clear                   dvpodeco
094700100222     C**                 MOVEL     'C'           T02MOD
094800100222     C**                 MOVEL     KNSIF         T02SIF
094900100222     C**                 MOVEL     'VPO'         T02COD
095000100222     c**                 movel(P)  'DECO'        t02ke1
095100100222     C**                 CALL      'TIBS02R'
095200100222     C**                 PARM                    KPJBA
095300100222     C**                 PARM                    dsbs02
095400100222    2C**                 IF        T02ERR = *BLANKS
095500100222     C**                 MOVEL     T02uni        dvpodeco
095600100222    2C**                 ENDIF
095700090917    c
095800090917     c* Accendo indicatore 25 de devo stampare anche tipo servizio H
095900100222     c**                 if        ds1aal>=§vpotsph
096000090917     c                   seton                                        25
096100100222     c**                 endif
096200070419     c**
096300070419     c* Verifico se ha GEO attiva
096400070419     C     DS1LNA        CHAIN     AZORG                              33
096500070419     c                   movel     orgde8        og148
096600070419     c                   if        §oggeot='S'
096700070419     c                   seton                                        18
096800070419     c                   endif
096900090604
097000090604      * carico le descrizioni fisse dei tipi servizio
097100090604      * chiodo la descrizione per ogni tipo servizio visto che la descrizione
097200090604      * è da stampare nella testata ed è fissa
097300090604      * le colonne dei tipi servizio da stampare sono
097400090917      * 'C + D' 'E' "H"
097500090604     c                   clear                   ds5e
097600090604     c                   clear                   wdtspc            5
097700090604     c                   clear                   wdtspd            5
097800090604     c                   clear                   wdtspe            5
097900090917     c                   clear                   wdtsph            5
098000090604     c                   eval      cod = '5E'
098100090604     c     ktab2         setll     tabel00f
098200090604     c                   do        *hival
098300090604     c     ktab2         reade(n)  tabel00f
098400090604     c                   if        %eof(tabel00f)
098500090604     c                   leave
098600090604     c                   endif
098700090604     c                   eval      ds5e = tbluni
098800090604     c                   if        tblkey = 'C'
098900090604     c                   eval      wdtspc = §5ed05
099000090604     c                   endif
099100090604     c                   if        tblkey = 'D'
099200090604     c                   eval      wdtspd = §5ed05
099300090604     c                   endif
099400090604     c                   if        tblkey = 'E'
099500090604     c                   eval      wdtspe = §5ed05
099600090604     c                   endif
099700090917     c                   if        tblkey = 'H'
099800090917     c                   eval      wdtsph = §5ed05
099900090917     c                   endif
100000090604     c                   enddo
100100971117     C***
100200971117     C                   SETON                                        OEOFOD
100300950124     C*
100400100222     c* Verifico se devo escludere le partic.consegna di attesa non raggiunte
100500100222     c                   if        ds1fl7='S'
100600100222     c                   eval      p0dpart=CEscludiPar
100700100222     c                   endif
100800930907     C                   ENDSR
100900970122     C**************************************************************************
101000970122     C* ELABORO LA STATISTICA PARTENDO DAI SALDI
101100970122     C**************************************************************************
101200970122     C     ELASAL        BEGSR
101300970122     C* S P E D I Z I O N I   C O N S E G N A T E
101400000703     C*********          MOVEL     DS1FEL        KFLE
101500970225     C                   MOVEL     DS1LNA        KLNA
101600970207     C                   MOVEL     DS1ADA        KDCM
101700970122     C*
101800970122     C                   MOVEL     'C'           KSCO
101900970122     C                   MOVEL     'A'           KOLT
102000970122     C                   MOVEL     ' '           KGBO
102100970122     C*
102200970122     C                   EXSR      RIEMPI
102300970122     C*
102400970122     C* C O N S E G N E   P A R Z I A L I
102500970122     C                   MOVEL     'C'           KSCO
102600970122     C                   MOVEL     ' '           KOLT
102700970122     C                   MOVEL     'P'           KGBO
102800970122     C                   EXSR      RIEMPI
102900970122     C* R I S E R V E   C O N S E G N E
103000970122     C                   MOVEL     'C'           KSCO
103100970122     C                   MOVEL     ' '           KOLT
103200970122     C                   MOVEL     'R'           KGBO
103300970122     C                   EXSR      RIEMPI
103400970122     C* SOLO SE RICHEISTA UNA SOLA DATA, CONTINUO A MEMORIZZATE LE SKI
103500970122     C     *IN02         IFEQ      *OFF
103600970122     C*
103700970122     C*  N O N   C O N S E G N A T E
103800970122     C*          DI CUI GIACENZE APERTE
103900970122     C                   MOVEL     'N'           KSCO
104000970122     C                   MOVEL     'A'           KOLT
104100970122     C                   MOVEL     'G'           KGBO
104200970122     C                   EXSR      RIEMPI
104300970122     C*          DI CUI LASCIATI AVVISI
104400970122     C                   MOVEL     'N'           KSCO
104500970122     C                   MOVEL     'A'           KOLT
104600970122     C                   MOVEL     'L'           KGBO
104700970122     C                   EXSR      RIEMPI
104800970122     C*          DI CUI CHIUSE PER TURNO
104900970122     C                   MOVEL     'N'           KSCO
105000970122     C                   MOVEL     'A'           KOLT
105100970122     C                   MOVEL     'C'           KGBO
105200970122     C                   EXSR      RIEMPI
105300970122     C*          DI CUI NON FATTE
105400970122     C                   MOVEL     'N'           KSCO
105500970122     C                   MOVEL     'A'           KOLT
105600970122     C                   MOVEL     'N'           KGBO
105700970122     C                   EXSR      RIEMPI
105800970122     C*  I N     D I S T I N T A
105900970122     C                   MOVEL     'D'           KSCO
106000970122     C                   MOVEL     'A'           KOLT
106100970122     C                   MOVEL     ' '           KGBO
106200970122     C                   EXSR      RIEMPI
106300970122     C* I N   C O N S E G N A
106400970122     C                   MOVEL     'I'           KSCO
106500970122     C                   MOVEL     'A'           KOLT
106600970122     C                   MOVEL     ' '           KGBO
106700970122     C                   EXSR      RIEMPI
106800970122     C* NON   I N   C O N S E G N A
106900970124     C                   MOVEL     'O'           KSCO
107000970122     C                   MOVEL     'A'           KOLT
107100970122     C                   MOVEL     ' '           KGBO
107200970122     C                   EXSR      RIEMPI
107300970122     C*  C O N S E G N A B I L I
107400970122     C                   MOVEL     'B'           KSCO
107500970122     C                   MOVEL     ' '           KOLT
107600970122     C                   MOVEL     ' '           KGBO
107700970122     C                   EXSR      RIEMPI
107800970122     C                   ENDIF
107900970122     C*
108000970122     C                   ENDSR
108100970122     C**************************************************************************
108200970122     C* RIEMPO LE SCHIERE DAL FILE SALDI FNSCO00F
108300970122     C**************************************************************************
108400970122     C     RIEMPI        BEGSR
108500000703     C     KSCO2         SETLL     FNSCO03L
108600000703     C  N02KSCO22        READE     FNSCO03L                               30
108700000703     C   02KSCO21        READE     FNSCO03L                               30
108800970122    1C     *IN30         DOWEQ     *OFF
108900970122     C*
109000970122     C* E' DENTRO LE DATE DI CONSEGNA IMMESSE
109100970122    2C     *IN02         IFEQ      *OFF
109200970207     C     SCODCM        ORLE      DS1AAL
109300970122     C* ESCUDO I RECORD DI TOTALE
109400970122    3C     SCOOLT        IFNE      'T'
109500970122     C     SCOOLT        ANDNE     'F'
109600970122     C* IMPOSTO PER CERCARE IL POSTO NELLA SKIERA DELLE ORE
109700970122    4C                   SELECT
109800970122     C     SCOOLT        WHENEQ    'A'
109900970122     C                   Z-SUB     999           NUM3
110000970122     C     SCOOLT        WHENEQ    'O'
110100970122     C                   Z-ADD     999           NUM3
110200970122     C     SCOOLT        WHENEQ    'D'
110300970122     C                   Z-ADD     SCOORE        NUM3
110400970122     C                   OTHER
110500970122     C* NON CI SONO ORE PER L'AFFIDABILITA'
110600970122     C                   Z-ADD     1             B
110700970122    4C                   ENDSL
110800970122     C*
110900970122     C     SCOOLT        IFNE      ' '
111000970122     C                   EXSR      CALRIT
111100970122     C                   ENDIF
111200970122     C*
111300970122     C                   SELECT
111400970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
111500970122     C     SCOGBO        ANDEQ     ' '
111600970310     C                   ADD       SCOSS1        CNE(B)
111700970310     C                   ADD       SCOSS2        CNC(B)
111800970310     C                   ADD       SCOSS3        CNN(B)
111900090916     C                   ADD       SCOSS4        CNh(B)
112000970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
112100970122     C     SCOGBO        ANDEQ     'P'                                          CONS PARZIAL
112200970310     C                   ADD       SCOSS1        PRE(B)
112300970310     C                   ADD       SCOSS2        PRC(B)
112400970310     C                   ADD       SCOSS3        PRN(B)
112500090916     C                   ADD       SCOSS4        PRh(B)
112600970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
112700970122     C     SCOGBO        ANDEQ     'R'                                          RISERVE CONS
112800970310     C                   ADD       SCOSS1        RSE(B)
112900970310     C                   ADD       SCOSS2        RSC(B)
113000970310     C                   ADD       SCOSS3        RSN(B)
113100090916     C                   ADD       SCOSS4        RSh(B)
113200970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
113300970122     C     SCOGBO        ANDEQ     'G'                                          GIACENZE AP
113400970310     C                   ADD       SCOSS1        GAE(B)
113500970310     C                   ADD       SCOSS2        GAC(B)
113600970310     C                   ADD       SCOSS3        GAN(B)
113700090916     C                   ADD       SCOSS4        GAh(B)
113800970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
113900970122     C     SCOGBO        ANDEQ     'L'                                          LASCIATI AVV
114000970310     C                   ADD       SCOSS1        LAE(B)
114100970310     C                   ADD       SCOSS2        LAC(B)
114200970310     C                   ADD       SCOSS3        LAN(B)
114300090916     C                   ADD       SCOSS4        LAh(B)
114400970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
114500970122     C     SCOGBO        ANDEQ     'C'                                          CHIUSE TURNO
114600970310     C                   ADD       SCOSS1        CTE(B)
114700970310     C                   ADD       SCOSS2        CTC(B)
114800970310     C                   ADD       SCOSS3        CTN(B)
114900090916     C                   ADD       SCOSS4        CTh(B)
115000970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
115100970122     C     SCOGBO        ANDEQ     'N'                                          NON FATTE
115200970310     C                   ADD       SCOSS1        NFE(B)
115300970310     C                   ADD       SCOSS2        NFC(B)
115400970310     C                   ADD       SCOSS3        NFN(B)
115500090916     C                   ADD       SCOSS4        NFh(B)
115600970122     C     SCOSCO        WHENEQ    'D'                                          IN DISTINTA
115700970122     C     SCOGBO        ANDEQ     ' '
115800970310     C                   ADD       SCOSS1        IDE(B)
115900970310     C                   ADD       SCOSS2        IDC(B)
116000970310     C                   ADD       SCOSS3        IDN(B)
116100090916     C                   ADD       SCOSS4        IDh(B)
116200970122     C     SCOSCO        WHENEQ    'I'                                          IN CONSEGNA
116300970122     C     SCOGBO        ANDEQ     ' '
116400970310     C                   ADD       SCOSS1        ICE(B)
116500970310     C                   ADD       SCOSS2        ICC(B)
116600970310     C                   ADD       SCOSS3        ICN(B)
116700090916     C                   ADD       SCOSS4        ICh(B)
116800970122     C     SCOSCO        WHENEQ    'O'                                          NOINCONSEGNA
116900970122     C     SCOGBO        ANDEQ     ' '
117000970310     C                   ADD       SCOSS1        NCE(B)
117100970310     C                   ADD       SCOSS2        NCC(B)
117200970310     C                   ADD       SCOSS3        NCN(B)
117300090916     C                   ADD       SCOSS4        NCh(B)
117400970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
117500970122     C     SCOORE        ANDEQ     -999                                         IN ANTICIPO
117600970310     C                   ADD       SCOSS1        COE(1)
117700970310     C                   ADD       SCOSS2        COC(1)
117800970310     C                   ADD       SCOSS3        CON(1)
117900090916     C                   ADD       SCOSS4        COh(1)
118000970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
118100970122     C     SCOORE        ANDEQ     0                                            IN CONFORMIT
118200970310     C                   ADD       SCOSS1        COE(2)
118300970310     C                   ADD       SCOSS2        COC(2)
118400970310     C                   ADD       SCOSS3        CON(2)
118500090916     C                   ADD       SCOSS4        COh(2)
118600970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
118700970122     C     SCOORE        ANDEQ     999                                          IN RITARDO
118800970310     C                   ADD       SCOSS1        COE(3)
118900970310     C                   ADD       SCOSS2        COC(3)
119000970310     C                   ADD       SCOSS3        CON(3)
119100090918     C                   ADD       SCOSS4        COh(3)
119200970122     C                   ENDSL
119300970122     C*
119400970122    3C                   ENDIF
119500970122     C*
119600000703     C  N02KSCO22        READE     FNSCO03L                               30
119700000703     C   02KSCO21        READE     FNSCO03L                               30
119800970122   X2C                   ELSE
119900970122     C                   SETON                                        30
120000970122    2C                   ENDIF
120100970122    1C                   ENDDO
120200970122     C                   ENDSR
120300950124     C**************************************************************************
120400950124     C*    CONTO LE SPEDIZIONI CONSEGNATE
120500950124     C**************************************************************************
120600920604     C     CONT1         BEGSR
120700950124     C*
120800090917     C* Tipo servizio --> imposta indicatore
120900090917     c                   exsr      IndtipoServ
121000930907     C*
121100950123     C* RICHIAMO PGM DI CALCOLO AFFIDABILITA'
121200950227     C* SE MANCA LA DATA TEORICA LA CALCOLO
121300950227    1C     ARBDCE        IFEQ      0
121400970925     C                   CLEAR                   DS99
121500930908     C                   EXSR      CALDAT
121600950227   X1C                   ELSE
121700950227     C                   Z-ADD     ARBNCI        NUM3                            NON CONF.CONSEGNA
121800950227    1C                   ENDIF
121900930907     C*
122000930908     C* CERCO LO SCAGLIONE DEL RITARDO
122100930907     C                   EXSR      CALRIT
122200930907     C*
122300930812     C* C O N S E G N A T E
122400090916     C   10              ADD       1             CNE(B)                          "E"
122500090916     C   11              ADD       1             CNC(B)                          "C"
122600090916     C
122700090916     C   20              ADD       1             CNN(B)                          "D"
122800090916     C   21              ADD       1             CNh(B)                          "H"
122900970130     C*
123000970130     C* TROVATA ALMENO UNA SPEDIZIONE CONSEGNATA
123100970130     C                   MOVEL     'S'           WCONS             1
123200940117     C*
123300940117     C   99              MOVEL     'C'           STAFLG
123400940203     C   99              ADD       1             STAC              7 0
123500940117     C   99              EXCEPT    SPED                                          S P E D I Z
123600930907     C*
123700930907     C* IN    C O N S E G N A
123800090916     C   10              ADD       1             ICE(B)                          "E"
123900090916     C   11              ADD       1             ICC(B)                          "C"
124000090916     C
124100090916     C   20              ADD       1             ICN(B)                          "D"
124200090916     C   21              ADD       1             ICh(B)                          "H"
124300920604     C*
124400930812     C* C O N S E G N A B I L I
124500930907     C                   EXSR      CALCON
124600940208     C*
124700940208     C*  VEDO SE C'E' ANCHE QUALCHE CONSEGNA NON FATTA LO STESSO GIORNO
124800970417     C***********          EXSR CONT3
124900950124     C*
125000930907     C                   ENDSR
125100090917     C**************************************************************************
125200090917     C* Accendi indicatore a seconda del tipo servizio della bolla
125300090917     C**************************************************************************
125400090917     c     IndTipoSERV   BEGSR
125500090917     C                   SETOFF                                       1011
125600090917     C                   SETOFF                                       2021
125700090917
125800090917     C***  ARBTSP        COMP      'E'                                    10
125900090917     C***  ARBTSP        COMP      'C'                                    11
126000090917
126100090917     c                   select
126200090917     c                   when      arbtsp='E'
126300090917     c                   seton                                        10
126400090917     c                   when      arbtsp='C'
126500090917     c                   seton                                        11
126600090917     c                   when      arbtsp='H'
126700090917     c                   seton                                        21
126800090917     c                   other
126900090917     c                   seton                                        20
127000090917     c                   endsl
127100090917     c                   ENDSR
127200950124     C**************************************************************************
127300950124     C* CALCOLO POSIZIONE SCHIERA DELL'ANTICIPO/RITARDO
127400950124     C**************************************************************************
127500930907     C     CALRIT        BEGSR
127600950124     C*
127700940117     C                   Z-ADD     1             B                 3 0
127800930907     C     NUM3          LOOKUP    ORE(B)                                 32
127900930907     C*
128000930907    1C     *IN32         IFEQ      *OFF
128100930907     C                   Z-ADD     1             B
128200930907     C*
128300930907    2C     NUM3          IFGT      0
128400930907     C     999           LOOKUP    ORE(B)                                 32
128500930907     C*
128600930907   X2C                   ELSE
128700930907     C*
128800930907     C     -999          LOOKUP    ORE(B)                                 32
128900930907    2C                   ENDIF
129000930907    1C                   ENDIF
129100950124     C*
129200930907     C                   ENDSR
129300950124     C**************************************************************************
129400950124     C* CALCOLO LE SPEDIZIONI CONSEGNABILI
129500950124     C**************************************************************************
129600930907     C     CALCON        BEGSR
129700950124     C*
129800930907     C                   SELECT
129900930907     C* NEL PERIODO
130000930907     C     NUM3          WHENEQ    0
130100090916     C   10              ADD       1             COE(2)                          "E"
130200090916     C   11              ADD       1             COC(2)                          "C"
130300090916     C   20              ADD       1             CON(2)                          "D"
130400090916     C   21              ADD       1             COh(2)                          "H"
130500930907     C     NUM3          WHENGT    0
130600090916     C   10              ADD       1             COE(3)                          "E"
130700090916     C   11              ADD       1             COC(3)                          "C"
130800090916     C   20              ADD       1             CON(3)                          "D"
130900090917     C   21              ADD       1             COh(3)                          "H"
131000930907     C                   OTHER
131100090916     C   10              ADD       1             COE(1)                          "E"
131200090916     C   11              ADD       1             COC(1)                          "C"
131300090916     C   20              ADD       1             CON(1)                          "D"
131400090916     C   21              ADD       1             COh(1)                          "H"
131500930907     C                   END
131600930907     C*
131700930907     C                   ENDSR
131800950124     C**************************************************************************
131900950124     C*    CONTO IN ARB00F QUELLE CONSEGNABILI O IN DISTINTA
132000950124     C**************************************************************************
132100930812     C     CONT4         BEGSR
132200930908     C*
132300930908     C* NORMALE/ESPRESSO
132400090917     C***                SETOFF                                       1011
132500090917     C***  ARBTSP        COMP      'E'                                    10
132600090917     C***  ARBTSP        COMP      'C'                                    11
132700090917     C*
132800090917     C* Tipo servizio --> imposta indicatore
132900090917     c                   exsr      IndtipoServ
133000930908     C**
133100970606     C* SE C'E' NUMERO DISTINTA E'    "I N    D I S T I N T A "
133200930908     C**
133300970606     C     ARBNDC        IFGT      0
133400970606     C     ARBDDC        ANDGT     0
133500970606     C     ARBDDC        ANDLE     DS1ADA
133600090917     c
133700930908     C* DATA E ORA CALCOLO
133800970606     C                   Z-ADD     DS1ADA        D98DFC
133900970606     C                   Z-ADD     2359          D98OFC
134000970606     C* SE IL GIORNO E' OGGI, METTO L'ORARIO ESATTO
134100970606     C     DS1ADA        IFEQ      WDATE1
134200970606     C                   MOVEL     UTIME         D98OFC                         HH:MM
134300970606     C                   ENDIF
134400930908     C*
134500930908     C                   EXSR      CALDAT
134600930908     C* CERCO SCAGLIONE DEL RITARDO
134700930908     C                   EXSR      CALRIT
134800930908     C*
134900090916     C   10              ADD       1             IDE(B)                          "E"
135000090916     C   11              ADD       1             IDC(B)                          "C"
135100090916     C   20              ADD       1             IDN(B)                          "D"
135200090916     C   21              ADD       1             IDh(B)                          "H"
135300940117     C*
135400940117     C   99              MOVEL     'I'           STAFLG
135500940203     C   99              ADD       1             STAI              7 0
135600940117     C   99              EXCEPT    SPED                                          S P E D I Z
135700930908     C*
135800930908     C* IN    C O N S E G N A
135900090916     C   10              ADD       1             ICE(B)                          "E"
136000090916     C   11              ADD       1             ICC(B)                          "C"
136100090916     C   20              ADD       1             ICN(B)                          "D"
136200090916     C   21              ADD       1             ICh(B)                          "H"
136300930908     C* C O N S E G N A B I L I
136400930908     C                   EXSR      CALCON
136500930908     C                   GOTO      ENDCT4
136600930908    1C                   ENDIF
136700930908     C**
136800930908     C* CONTROLLO SE E' "N O N   C O N S E G N A T A"
136900930908     C**
137000930908     C                   EXSR      CONT3
137100940117     C*
137200940117     C*
137300930910     C*  16 ON - E' UNA NON CONSEGNATA O CONSEGNA PARZIALE ESCO
137400930908     C   16              GOTO      ENDCT4
137500990211     C**
137600990211     C     ARBLNP        CHAIN     AZORG01L                           30
137700020207     C  N30              MOVEL     orgde3        og143
137800020207     C  N30              MOVEL     §ogntw        LNPntw            3
137900020211     c   30              clear                   lnpntw
138000930908     C**
138100930908     C* CONTROLLO LE    "N O N   I N   C O N S E G N A"
138200930908     C**
138300930812     C* ESCLUDO SE:
138400930812     C* 1) E' BLOCCATA
138500930908    1C     ARBFBC        IFEQ      'B'
138600970818     C     ARBFBC        OREQ      '*'
138700930812     C* 2) C'E' CONTRASSEGNO E NON E' ABILITATO
138800000703     C**** ARBACC        OREQ      ' '
138900930907     C* 3) E' IN DISTINTA E DATA DISTINTA MINORE DATA DAL
139000970606     C****       ARBDDC    ORLT DS1ADA
139100970606     C****       ARBNDC    ANDGT0
139200940324     C* 4) SE E' UN FERMO DEPOSITO
139300940324     C     ARBFFD        ORNE      *BLANKS
139400950616     C* 5) SE DATA ARRIVO ULTIMO COLLO > DATA CONSEGNABILITA'
139500970818     C*    (BOLLA NON IMPORT)
139600970207     C     ARBDTI        ORGT      DS1ADA
139700020207     C     LNPntw        ANDEQ     'COR'
139800020207     C     ARBDTI        ORGT      DS1ADA
139900020207     C     LNPntw        ANDEQ     'MES'
140000930812     C                   GOTO      ENDCT4
140100950616    1C                   ENDIF
140200990326     C**
140300990326     C** 6) ESCLUDO SE C'E' C.A. APERTA SULLA SPEDIZIONE
140400990326     C                   CLEAR                   WESCA
140500040812      *
140600040812     c                   clear                   fidn12ds
140700040812     c                   eval      i12aas = arbaas
140800040812     c                   eval      i12lnp = arblnp
140900040812     c                   eval      i12nrs = arbnrs
141000040812     c                   eval      i12nsp = arbnsp
141100040812     c                   eval      i12fel = 'E'
141200040812     c                   eval      i12fan = 'E'
141300040812      *
141400040812     c                   call      'FIDN12R'
141500040812     c                   parm                    fidn12ds
141600040812      *
141700040812      * se non ci sono errori
141800040812     c                   if        o12err = ' '
141900040812      * se numero di CA trovate maggiore di zero
142000040812     c                   if        o12nca > 0
142100040812     c
142200040812     c                   z-add     *zeros        ii                2 0
142300040812     c     1             do        o12nca        ii
142400040812     c                   movea     skkey(ii)     dskey
142500040812     C                   Z-ADD     dsfil         dctfil
142600040812     C                   Z-ADD     dsnca         dctnca
142700040812     C                   Z-ADD     skdca(ii)     dctdca
142800040812     C                   Z-ADD     skdch(ii)     dctdch
142900040812     C                   movea     skcch(ii)     dctcch
143000040812      * se CA aperta imposto il flag e vado a fine
143100040812     c                   If        dctdch = 0
143200040812     c                   eval      wesca = 'S'
143300040812     c                   leave
143400040812     c                   else
143500040812     c                   eval      wesca = 'C'
143600040812     c                   endif
143700040812     C                   ENDDO
143800040812      *
143900040812     c                   endif
144000040812     c                   endif
144100990326     C*
144200990326     C* -SE ESISTE C.A. APERTA  --> ESCO
144300990326    1C     WESCA         IFEQ      'S'
144400990326     C                   GOTO      ENDCT4
144500990326    1C                   ENDIF
144600970818     C*
144700970818     C* DECODIFICA CODICE BOLLA
144800970818     C                   MOVEL     '3A'          COD
144900970818     C                   MOVEL     *BLANKS       KEY
145000970818     C                   MOVEL     ARBCBO        KEY
145100970818     C     KTAB          CHAIN(N)  TABEL                              30
145200970818     C  N30              MOVEL     TBLUNI        DS3A
145300970818     C   30              CLEAR                   DS3A
145400970818     C                   MOVEL     §3ADES        P0DCBO            5
145500990211     C* 6) BOLLA IMPORT:
145600970818     C* ESCLUDO SE DATA ARRIVO ULTIMO COLLO > DATA CONSEGNABILITA
145700970818     C* SOLO SE SI TRATTA DI BOLLA ASSEGNATO CON FATTURA IMMEDIATA
145800020207    1C     LNPntw        IFEQ      'EEX'
145900020207    1C     LNPntw        oreq      'EUP'
146000020207    1C     LNPntw        oreq      'DPD'
146100020207    1C     LNPntw        oreq      'FED'
146200970818     C**
146300970818    2C     ARBDTI        IFGT      DS1ADA
146400970818     C     ARBDTI        OREQ      0
146500970818     C*
146600970818    3C     ARBDTI        IFGT      DS1ADA
146700970818     C     §3AFCA        ANDGT     0
146800970818     C                   GOTO      ENDCT4
146900970818    3C                   ENDIF
147000970818     C*
147100970818     C                   MOVE      ARBKSC        W0040             4 0
147200970818    3C     W0040         IFEQ      9999
147300991104     C                   MOVEL     '1'           KTRC
147400991104     C     KARB6         CHAIN     FIAR601L                           13
147500970818     C** PER ORA ESCO SOLO SE DTI E' CALCOLATO E > DELLA DATA ELABORAZ
147600970818     C*  PERCHE' DTI POTREBBE ESSERE A 0 MA TUTTE LE DATE DI ARRIVO
147700990326     C*  PRESENTI PERCHE' ANCORA NON E' STATO CALCOLATO
147800970818    4C     ARBDTI        IFGT      DS1ADA
147900970818     C     *IN13         ANDEQ     *OFF
148000970818     C     AR6DFT        ANDGT     0
148100970818     C                   GOTO      ENDCT4
148200970818    4C                   ENDIF
148300970818    3C                   ENDIF
148400970818    2C                   ENDIF
148500970818    1C                   END
148600970818     C**
148700990326     C* 7) SE NON E' COMPLETAMENTE ARRIVATA:
148800970818     C*    O E' ARRIVATA DOPO  --> LEGGO IL DETTAGLIO COLLI
148900990329     C* O SE HA IL C/A DEVO CONTROLLARE IL DETT.COLLI
149000970818     C                   CLEAR                   WCONS1
149100970818    1C     ARBDTI        IFEQ      0
149200970818     C     ARBDTI        ORGT      DS1ADA
149300990326     C     WESCA         OREQ      'C'
149400940329     C                   SETOFF                                       17
149500970818     C                   CLEAR                   KDAM1
149600950123     C     KARB3         SETLL     FNART01L
149700950123     C     KARB3         READE     FNART01L                               30
149800940329     C*
149900940329    2C     *IN30         DOWEQ     *OFF
150000040527     c                   clear                   Wartdam
150100040527     c
150200040527    3c                   if        artdam>0
150300040527     c                   eval      Wartdam=artdam
150400040527   x3c                   else
150500040527     c
150600040527     c* Memorizzo la data di arrivo o l'ipotetico arrivo se collo partito
150700040527     c* su FGV abbinato di ida non ancora chiuso: come data di arrivo
150800040527     c*  prendo la data foglio arrivi, solo se richiesto
150900040608    4c                   if        artdam=0  and artdfv>0  and ds1fl5='S'
151000040908     c* Se si tratta di bolla LOCALE, non esiste abbinamento quindi
151100040908     c* cerco di desumere la data del foglio arrivi per vedere se è
151200040908     c*  chiuso o perto
151300040908    5c                   if        arbtfp=arbtfa
151400050706     c                   clear                   wtipolancio
151500070420     c                   eval      kdataloc=artdfv
151600050706     c                   exsr      BOLLALOC
151700040908   X5c                   else
151800050706     C
151900040527     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
152000040527     C                   CLEAR                   fnlv53ds
152100040527     C                   MOVEL     'A'           D53TFO
152200040527     C                   MOVE      artNFV        D53NFV
152300040608     C                   MOVE      arbtfp        D53FGS
152400040527     C                   MOVEL     'B'           D53ABB
152500040527     C                   MOVE      ARBLNA        D53LNA
152600040527     C                   MOVE      ARBTFA        D53TFA
152700040527     C                   CALL      'FNLV53R'
152800040527     C                   PARM                    fnlv53ds
152900040527     c*
153000040908    6c                   if        d53err=*blanks and d53nfa>0
153100040527     c* verifico se fogli arrivi di abbinamento già chiuso
153200040527     C     KFVV          CHAIN     FNFVV000
153300040527     C                   IF        %FOUND(FNFVV01L)
153400040527     C                   IF        FVVFCF=' '
153500040527     C                   EVAL      Wartdam=D53DFA
153600040527     C                   ENDIF
153700040527     C                   ENDIF
153800040908    6c                   endif
153900040908    5c                   endif
154000040527     c
154100040527    4c                   endif
154200040527    3c                   endif
154300040527     c
154400040526     C* SE NON ARRIVATO O ARRIVATO CON DATA > (E'COME SE FOSSE A 0 QUEL
154500970818     C*  GIORNO)
154600040527    3C     Wartdam       IFEQ      0
154700040527     C     Wartdam       ORGT      DS1ADA
154800990329     C* CONTROLLO SE E' SU C.A. MANCANZA O AVARIA RESA O DISTRUTTA
154900990329     C*  NEL QUAL CASO IGNORO PERCHE' IL COLLI NON CI SARA' MAI
155000990329     C                   EXSR      CTRCA
155100990329     C*
155200990329     C     WMANCA        IFEQ      ' '
155300970818     C                   SETON                                        17
155400020207     C     LNPntw        IFEQ      'COR'
155500020207     C     LNPntw        oreq      'MES'
155600020207     C     LNPntw        oreq      'PPT'
155700970818     C                   SETON                                        30
155800970818     C                   END
155900990329     C                   END
156000990329     C*
156100940329   X3C                   ELSE
156200990329     C* MEMORIZZO CHE ALMENO UN COLLO E' ARRIVATO PRIMA O NEL GIORNO
156300970818     C*  DELLA DATA DI ELABORAZIONE
156400990326    4C     ARTDCM        IFEQ      0
156500970819     C     KDAM1         ANDEQ     *ZEROS
156600990326     C     ARTDCM        ORGT      DS1ADA
156700970818     C     KDAM1         ANDEQ     *ZEROS
156800990326     C*
156900990326     C** NON MEMORIZZO SE IL COLLO NON CONSEGNATO E' RELATIVO AD
157000990326     C**  AVARIE RESE O DISTRUTTE
157100990329     C                   EXSR      CTRCA
157200990329    5C     WMANCA        IFEQ      ' '
157300990326     C                   SETON                                        33
157400990326    5C                   ENDIF
157500990326     C**
157600040527     C   33              Z-ADD     Wartdam       KDAM1
157700970818     C     *LIKE         DEFINE    ARTDAM        KDAM1
157800990329    4C                   END
157900940329    3C                   ENDIF
158000970818     C**
158100970818     C  N30KARB3         READE     FNART01L                               30
158200940329    2C                   ENDDO
158300990326     C*
158400970818     C* SE BOLLA IMPORT NON LA STAMPO SOLO SE ENTRO LA DATA ELABORA-
158500970818     C* ZIONE NON E' ARRIVATO NEMMENO UN COLLO (KDAM1=0)
158600970818     C* STAMPO INVECE LE SPEDIZIONI ARRIVATE PARZIALMENTE
158700990329     C* SE PRESENTE C.A. CHIUSA: ESCO SE I SOLI COLLI NON CONSEGNATI
158800990329     C*  SONO QUELLI RELATIVI A MANCANZE O AVARIE RESE/DISTRUTTE
158900970818    3C     KDAM1         IFEQ      *ZEROS
159000970818     C                   GOTO      ENDCT4
159100040526    3C                   ENDIF
159200990326     C**
159300970818     C* E' UNA ARRIVO PARZIALE: STAMPO SOLO SE NON E' FATTURA IMMEDIATA
159400970818     C*  E SE NON HA IN CONTRASSEGNO
159500020207    2C     LNPntw        IFEQ      'EEX'
159600020207    2C     LNPntw        oreq      'EUP'
159700020207    2C     LNPntw        oreq      'DPD'
159800020207    2C     LNPntw        oreq      'FED'
159900970818    3C     *IN17         IFEQ      *ON
160000970818     C     ARBDTI        ANDEQ     0
160100970818     C*
160200970818    4C     §3AFCA        IFGT      0
160300970818     C     W0040         OREQ      9999
160400970818     C     *IN13         ANDEQ     *OFF
160500970818     C     AR6DFT        ANDGT     0
160600970818     C                   GOTO      ENDCT4
160700970818    4C                   ENDIF
160800970818    3C                   ENDIF
160900970818     C* MEMORIZZO CHE SI TRATTA DI SPEDIZIONE CONSEGNABILE ANCHE SE
161000970818     C* E' UN ARRIVO PARZIALE
161100970818     C   17              MOVE      '1'           WCONS1            1
161200970818     C**
161300970818   X2C                   ELSE
161400940329     C*
161500940329     C   17              GOTO      ENDCT4
161600970818    2C                   END
161700970818    1C                   ENDIF
161800930907     C*
161900961108     C* SE LA BOLLA E' GIACENTE ED HA UNA FIGLIA CON LNP = LNA IN
162000970925     C*   CONSEGNA NON RICHIAMO PGM TNSD99R PERCHE' NON E' CONSEGNABILE
162100961108    1C     ARBFBC        IFEQ      'G'
162200961108     C     ARBFBC        OREQ      '*'
162300050331     C     KARB3         SETLL     Tigcp21L                               33
162400961108    2C     *IN33         IFEQ      *ON
162500961112     C* SALVO I CAMPI DELLA BOLLA
162600961112     C                   MOVE      ARBDS         WSAVDS
162700961108     C                   CLEAR                   WTROV
162800961108     C*
162900961108     C     KARB3         SETLL     FNLBL02L
163000961108     C     KARB3         READE     FNLBL02L                               37
163100961108    3C     *IN37         DOWEQ     *OFF
163200961108    4C     LBLLPN        IFEQ      LBLLAN
163300961108     C     KARB1         CHAIN     FNARB01L                           38
163400961108    5C     *IN38         IFEQ      *OFF
163500961108     C     ARBNDC        ANDGT     0
163600961108     C                   SETON                                        37
163700961108     C                   MOVEL     'S'           WTROV             1
163800961108    5C                   ENDIF
163900961108    4C                   ENDIF
164000961108     C  N37KARB3         READE     FNLBL02L                               37
164100961108    3C                   ENDDO
164200961108     C*
164300961112     C* RIMETTO I CAMPI SALVATI IN PRECEDENZA
164400961112     C                   MOVE      WSAVDS        ARBDS
164500961112     C*
164600961108    3C     WTROV         IFEQ      'S'
164700961108     C                   GOTO      ENDCT4
164800961108    3C                   ENDIF
164900961108    2C                   ENDIF
165000961108    1C                   ENDIF
165100050706     c* Se richiesto, escludo supermercati e appuntamenti arrivati nella dat
165200050706     c*  a richiesta  se privi della cons. richiesta
165300050706     c                   if        ds1fl6='S' and arbdcr=0
165400050706     c                   if        arbtc1='S' or arbtc1='A' or arbtc2='S' or
165500050706     c                             arbtc2='A'
165600050706     c* Per bolla locale, desumo
165700050706     c                   if        arbtfp=arbtfa
165800050706     c                   eval      wtipolancio='SA'
165900070420     c                   eval      kdataloc=arbdbr
166000070420     c
166100050706     c                   exsr      BOLLALOC
166200050706     c                   clear                   d53err
166300050706     c                   eval      d53dfa=wartdam
166400050706     c                   else
166500050706     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
166600050706     C                   CLEAR                   fnlv53ds
166700050706     C                   MOVEL     'A'           D53TFO
166800050706     C                   MOVE      arbNFV        D53NFV
166900050706     C                   MOVE      arbtfp        D53FGS
167000050706     C                   MOVEL     'B'           D53ABB
167100050706     C                   MOVE      ARBLNA        D53LNA
167200050706     C                   MOVE      ARBTFA        D53TFA
167300050706     C                   CALL      'FNLV53R'
167400050706     C                   PARM                    fnlv53ds
167500050706     c                   endif
167600050706     c*
167700060314     c                   select
167800060314    6c                   when      d53err=*blanks and d53dfa=ds1ada
167900050706     C                   GOTO      ENDCT4
168000060314     c
168100060315    6c                   when      d53err=*blanks and d53dfa<ds1ada  and
168200060315     c                             d53dfa>0
168300060314     c* controllo se giorni festivi da d53dfa al giorno prima di ds1ada
168400060314     c     *iso          movel     d53dfa        dataiso
168500060314     c     *iso          movel     ds1ada        dataisofine
168600060314     c                   eval      wfest='S'
168700060314     c                   dow       dataiso<dataisofine and WFEST='S'
168800060314     c                   movel     dataiso       kdata             8 0
168900060314     c                   exsr      CTRFESTA
169000060314     c                   adddur    1:*d          dataiso
169100060314     c                   enddo
169200060314     c
169300060314     c                   if        wfest='S'
169400060314     c                   eval      wfest=' '
169500060314     C                   GOTO      ENDCT4
169600060314     c                   endif
169700060314     c
169800060314    3C                   ENDSL
169900961108     C*
170000050706    3C                   ENDIF
170100050706    3C                   ENDIF
170200050706     C*
170300970207     C                   Z-ADD     DS1ADA        D98DFC
170400950127     C                   Z-ADD     2359          D98OFC
170500950127     C* SE IL GIORNO E' OGGI, METTO L'ORARIO ESATTO
170600970207     C     DS1ADA        IFEQ      WDATE1
170700950127     C                   MOVEL     UTIME         D98OFC                         HH:MM
170800950127     C                   ENDIF
170900930908     C*
171000930908     C                   EXSR      CALDAT
171100940117     C*
171200940117     C                   EXSR      CALRIT
171300100218     c
171400100218     c* Se bolla con partic.consegna ATTESA verifico se da stampare a meno
171500100218     c*  in base all'anticipo che ha.
171600100218     c                   if        ds1fl7='S' and arbgma<>*blanks
171700100218     C                   MOVEL     '7R'          COD
171800100218     C                   MOVEL(P)  arbgma        KEY
171900100218     C     KTAB          CHAIN(N)  TABEL                              31
172000100218     C  N31              MOVEL     TBLUNI        DS7R
172100100218     C   31              CLEAR                   DS7R
172200100218     c                   if        §7RATT='S' and d98nci <=-24
172300100218     C                   GOTO      ENDCT4
172400100218     c                   endif
172500100218     c                   endif
172600110927     c*
172700110927     c* Se c'e' Perfect Order --> lo stampo e vedo se è abilitato
172800110927     c                   setoff                                       1928
172900110927     c                   clear                   StPorder         16
173000130221     c                   if        arbxco >*zeros
173100110927     c                   seton                                        19
173200110927     c                   exsr      POrder
173300110927     c                   if        olr78conso='S'
173400110927     c                   eval      stPorder='  Consegnabile  '
173500110927     c                   else
173600110927     c                   eval      stPorder='NON Consegnabile'
173700110927     c
173800110927     c* se non consegnabile tolgo 24 ore alla delivery per andare in anticipo
173900110927     c                   eval      num3=num3-24
174000110927     c                   exsr      CALRIT
174100110927     c                   endif
174200110927     c
174300110927     c                   endif
174400110927     c
174500930908     C*
174600940117    1C     D98OCI        IFGT      0
174700930908     C*
174800940117     C* N O N    IN    C O N S E G N A
174900090916     C   10              ADD       1             NCE(B)                          "E"
175000090916     C   11              ADD       1             NCC(B)                          "C"
175100090916     C   20              ADD       1             NCN(B)                          "D"
175200090916     C   21              ADD       1             NCh(B)                          "H"
175300940117     C*
175400940117     C   99              MOVEL     'D'           STAFLG            1
175500940203     C   99              ADD       1             STAD              7 0
175600940117     C   99              EXCEPT    SPED                                          S P E D I Z
175700940530     C*
175800940530     C* STAMPO LISTA SPEDIZIONI NON IN CONSEGNA
175900970225     C  N02DS1FL1        IFEQ      'L'
176000970225     C     DS1FL1        OREQ      'E'
176100970403     C     DS1FL3        OREQ      'L'
176200940530     C                   EXSR      STASNC
176300940530     C                   ENDIF
176400940530     C*
176500930908     C* C O N S E G N A B I L I
176600930908     C                   EXSR      CALCON
176700940117    1C                   ENDIF
176800930812     C*
176900990329     C     ENDCT4        ENDSR
177000990329     C**************************************************************************
177100990329     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
177200990329     C**************************************************************************
177300990329     C     CTRCA         BEGSR
177400990329     C                   CLEAR                   WMANCA
177500990329    1C     WESCA         IFEQ      'C'
177600990329     C**
177700990329     C     KART          CHAIN     FNDCD02L                           33
177800990329    2C     *IN33         IFEQ      *OFF
177900990330     C     DCTATB        ANDEQ     ' '
178000990329     C     KDCT          CHAIN     FNDCT01L                           33
178100050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
178200050221     c                   If        Not *In33 and dctdt2 > 0
178300050221     c                   Eval      *In33 = *On
178400050221     c                   EndIf
178500990329     C*
178600990329    3C     *IN33         IFEQ      *OFF
178700990329     C     DCTDCH        ANDEQ     DCDDCH
178800990329     C     DCDDCH        OREQ      0
178900990329     C* TESTO PRIMA LA CAUSALE DI CHIUSURA PER VEDERE SE MI INDICA LA
179000990329     C*  PRESENZA DELLA MERCE
179100990329     C                   CLEAR                   DSBS02
179200990329     C                   MOVEL     'C'           T02MOD
179300990329     C                   MOVEL     KNSIF         T02SIF
179400990329     C                   MOVEL     'CCH'         T02COD
179500990329     C                   MOVEL     DCTCCH        T02KE1
179600990329     C                   CALL      'TIBS02R'
179700990329     C                   PARM                    KPJBA
179800990329     C                   PARM                    DSBS02
179900990329     C**
180000990329    4C     T02ERR        IFEQ      ' '
180100990330     C                   MOVEL     T02UNI        DCCH
180200990329     C                   ELSE
180300990329     C                   MOVEL     'S'           §CHIMA
180400990329    4C                   ENDIF
180500990329     C*
180600990329    4C     §CHIMA        IFEQ      ' '
180700990329     C                   CLEAR                   DSBS02
180800990329     C                   MOVEL     'C'           T02MOD
180900990329     C                   MOVEL     KNSIF         T02SIF
181000990329     C                   MOVEL     'TAD'         T02COD
181100990329     C                   MOVEL     DCTTAD        T02KE1
181200990329     C                   CALL      'TIBS02R'
181300990329     C                   PARM                    KPJBA
181400990329     C                   PARM                    DSBS02
181500990329     C**
181600990329    5C     T02ERR        IFEQ      ' '
181700990329     C                   MOVEL     T02UNI        DTAD
181800990329     C                   ELSE
181900990329     C                   MOVEL     'M'           §TARGR
182000990329    5C                   ENDIF
182100990329     C*
182200990329     C* AVARIA RESA O DISTRUTTA
182300990329    5C     §TARGR        IFEQ      'V'
182400990329    6C     DCTDSD        IFEQ      'D'
182500990330     C     DCTDSD        OREQ      'R'
182600990329     C                   MOVEL     'V'           WMANCA            1
182700990329    6C                   ENDIF
182800990329    5C                   ENDIF
182900990329     C* MANCANZA
183000990329    5C     §TARGR        IFEQ      'M'
183100990329     C                   MOVEL     'M'           WMANCA            1
183200990329    5C                   ENDIF
183300990329     C*
183400990329    4C                   ENDIF
183500990329    3C                   ENDIF
183600990329    2C                   ENDIF
183700990329    1C                   ENDIF
183800990329     C*
183900990329     C                   ENDSR
184000950124     C**************************************************************************
184100950124     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
184200950124     C**************************************************************************
184300930908     C     CALDAT        BEGSR
184400950123     C*
184500930908     C                   MOVEL     ' '           D98TLA                         *TIPO LANCIO
184600930908     C                   MOVEL     'A'           D98TBO                         *TIPO BOLLA
184700950123     C                   Z-ADD     ARBAAS        D98AAS
184800950123     C                   Z-ADD     ARBLNP        D98LNP
184900950123     C                   Z-ADD     ARBNRS        D98NRS
185000950123     C                   Z-ADD     ARBNSP        D98NSP
185100970925     C                   CALL      'TNSD99R'
185200970925     C                   PARM                    DS99
185300930908     C*
185400950123     C                   Z-ADD     D98NCI        NUM3              3 0
185500950124     C*
185600930908     C                   ENDSR
185700950124     C**************************************************************************
185800950124     C* CONTROLLO LE NON CONSEGNATE DAGLI EVENTI
185900950124     C**************************************************************************
186000930908     C     CONT3         BEGSR
186100950124     C*
186200970417     C* PER OGNI BOLLA LEGGO SOLTANTO L'ULTIMO EVENTO
186300940208     C                   SETOFF                                       16
186400970207     C                   Z-ADD     DS1ADA        NUM6A
186500950123     C                   Z-ADD     ARBAAS        AR§AAS
186600970417     C                   MOVEL     *HIVAL        NUMORA
186700970417     C                   CLEAR                   WWRT
186800990805     C     KARB22        SETLL     FNEVB000                           32
186900990805     C     KARB2         READPE    FNEVB000                               32
187000091112     c                   seton                                        35
187100091112     c* Escludo gli eventi particolari
187200091112     c                   dow       not *in32 and *in35
187300091112     c     evbcev        lookup    cep                                    35
187400091112     c
187500091112     C   35KARB2         READPE    FNEVB000                               32
187600091112     c                   enddo
187700930908     C*
187800970417    1C     *IN32         IFEQ      *OFF
187900940208     C                   SETOFF                                       05
188000940208     C* DATA E ORA CALCOLO
188100950124     C                   Z-ADD     EVBDEV        D98DFC                          DATA EVENTO
188200950124     C                   Z-ADD     EVBHEV        D98OFC                          ORA  EVENTO
188300940208     C                   EXSR      CALDAT
188400940208     C*
188500940208     C* CERCO SCAGLIONE DEL RITARDO
188600940208     C                   EXSR      CALRIT
188700930908     C*
188800930908     C                   MOVEL     '2A'          COD
188900930908     C                   MOVEL     *BLANKS       KEY
189000930908     C                   MOVEL     EVBCEV        KEY
189100970124     C     KTAB          CHAIN(N)  TABEL                              30
189200930908     C   30              CLEAR                   DS2A
189300930908     C  N30              MOVEL     TBLUNI        DS2A
189400940208     C*
189500930908     C* TIPO CONSEGNA
189600970417    2C     §2AFTC        IFNE      *BLANKS
189700930908     C*
189800930908     C* SOLO SE RICHIESTA UNA DATA SCRIVO LE NON CONSEGNATE
189900930908     C                   SELECT
190000970417    3C     §2AFTC        WHENEQ    'G'
190100940208     C   99              MOVEL     'G'           STAFLG
190200940208     C   99              ADD       1             STAG              7 0
190300970121     C   10              ADD       1             GAE(B)                         GIAC APERTE
190400970121     C   11              ADD       1             GAC(B)
190500090916     C   20              ADD       1             GAN(B)
190600090916     C   21              ADD       1             GAh(B)
190700970417     C                   MOVEL     'S'           WWRT              1
190800970417    3C     §2AFTC        WHENEQ    'A'                                          LASC AVVISO
190900940208     C   99              MOVEL     'A'           STAFLG
191000940208     C   99              ADD       1             STAA              7 0
191100970121     C   10              ADD       1             LAE(B)
191200970121     C   11              ADD       1             LAC(B)
191300090916     C   20              ADD       1             LAN(B)
191400090916     C   21              ADD       1             LAh(B)
191500970417     C                   MOVEL     'S'           WWRT              1
191600930908     C*
191700970417   X3C                   OTHER
191800970417    4C     §2ACPT        IFEQ      'S'                                          CHIUSO TURNO
191900940208     C   99              MOVEL     'T'           STAFLG
192000940208     C   99              ADD       1             STAT              7 0
192100970121     C   10              ADD       1             CTE(B)
192200970121     C   11              ADD       1             CTC(B)
192300090916     C   20              ADD       1             CTN(B)
192400090916     C   21              ADD       1             CTh(B)
192500970417     C                   MOVEL     'S'           WWRT              1
192600970417   X4C                   ELSE
192700940208     C   99              MOVEL     'N'           STAFLG
192800940208     C   99              ADD       1             STAN              7 0
192900970121     C   10              ADD       1             NFE(B)                         NON FATTE
193000970121     C   11              ADD       1             NFC(B)
193100090916     C   20              ADD       1             NFN(B)
193200090916     C   21              ADD       1             NFh(B)
193300970417     C                   MOVEL     'S'           WWRT              1
193400970417    4C                   ENDIF
193500970417    3C                   ENDSL
193600940117     C*
193700970417   X2C                   ELSE
193800940208     C*
193900940208     C* SE CONSEGNA PARZIALE COME SE FOSSE CONSEGNATA
194000940208     C                   SELECT
194100970417    3C     §2ATIP        WHENEQ    'P'
194200930910     C*
194300930910     C* C O N S E G N A T E
194400090916     C   10              ADD       1             CNE(B)
194500090916     C   11              ADD       1             CNC(B)
194600090916     C   20              ADD       1             CNN(B)
194700090916     C   21              ADD       1             CNh(B)
194800940208     C* TOTALE A PARTE COME C.PARZIALE
194900970121     C   10              ADD       1             PRE(1)                          CONS PARZ
195000970121     C   11              ADD       1             PRC(1)
195100090916     C   20              ADD       1             PRN(1)
195200090916     C   21              ADD       1             PRh(1)
195300970417     C                   MOVEL     'S'           WWRT              1
195400940117     C*
195500940117     C   99              MOVEL     'P'           STAFLG
195600940203     C   99              ADD       1             STAP              7 0
195700940208     C*
195800940208     C* RISERVA CONSEGNA
195900970417    3C     §2ATIP        WHENEQ    'R'
196000940208     C   99              MOVEL     'R'           STAFLG
196100940208     C   99              ADD       1             STAR              7 0
196200970121     C   10              ADD       1             RSE(1)                         RISERVA CONS
196300970121     C   11              ADD       1             RSC(1)
196400090916     C   20              ADD       1             RSN(1)
196500090916     C   21              ADD       1             RSh(1)
196600970417     C                   MOVEL     'S'           WWRT              1
196700940208     C* 05 ON - NON DEVO CONTEGGIARLA COME IN CONSEGNA (GIA' FATTO)
196800940208     C                   SETON                                        05
196900091117     c
197000091117     C* Spedizioni Ripristinate --> tra  le "Non fatte"
197100091117    3C     §2ATIP        WHENEQ    'S'
197200091117     C   99              MOVEL     'N'           STAFLG
197300091117     C   99              ADD       1             STAN
197400091117     C   10              ADD       1             NFE(B)                         NON FATTE
197500091117     C   11              ADD       1             NFC(B)
197600091117     C   20              ADD       1             NFN(B)
197700091117     C   21              ADD       1             NFh(B)
197800091117     C                   MOVEL     'S'           WWRT
197900970417    3C                   ENDSL
198000970417    2C                   ENDIF
198100091117     c
198200970417     C* EVENTO DA CONTEGGIARE
198300970417    2C     WWRT          IFEQ      'S'
198400940208     C   99              EXCEPT    SPED
198500940208     C     *IN05         IFEQ      *OFF
198600940208     C* IN    C O N S E G N A
198700090916     C   10              ADD       1             ICE(B)
198800090916     C   11              ADD       1             ICC(B)
198900090916     C   20              ADD       1             ICN(B)
199000090916     C   21              ADD       1             ICh(B)
199100940208     C*
199200940208     C* C O N S E G N A B I L I
199300940208     C                   EXSR      CALCON
199400940208     C                   SETON                                          16
199500940208     C                   ENDIF
199600930910     C*
199700970417    2C                   ENDIF
199800970417    1C                   ENDIF
199900930908     C*
200000930908     C                   ENDSR
200100950124     C**************************************************************************
200200970417     C* CONTROLLO LE CONSEGNE PARZIALI E RISERVE SE DATA DAL AL
200300950124     C**************************************************************************
200400940324     C     CONT3P        BEGSR
200500090917     c
200600090917     C* Tipo servizio --> imposta indicatore
200700090917     c                   exsr      IndtipoServ
200800940324     C*
200900970207     C                   Z-ADD     DS1ADA        NUM6A
201000950123     C                   Z-ADD     ARBAAS        AR§AAS
201100990805     C     KARB2         SETLL     FNEVB000
201200990805     C     KARB4         READE     FNEVB000                               32
201300940324     C*
201400940324    1C     *IN32         DOWEQ     *OFF
201500950124    2C     EVBDEV        IFGT      CO1DAA
201600940324     C                   SETON                                        32
201700940324   X2C                   ELSE
201800940324     C*
201900940324     C                   MOVEL     '2A'          COD
202000940324     C                   MOVEL     *BLANKS       KEY
202100940324     C                   MOVEL     EVBCEV        KEY
202200970124     C     KTAB          CHAIN(N)  TABEL                              30
202300940324     C   30              CLEAR                   DS2A
202400940324     C  N30              MOVEL     TBLUNI        DS2A
202500940324     C*
202600940324     C* TIPO CONSEGNA
202700970417     C* CONSIDERO SOLO CONSEGNE PARZIALI E RISERVE
202800970417    3C     §2ATIP        IFEQ      'P'
202900970417     C     §2ATIP        OREQ      'R'
203000940324     C* DATA E ORA CALCOLO
203100950406     C                   Z-ADD     EVBDEV        D98DFC                          DATA EVENTO
203200950410     C                   MOVEL     EVBHEV        D98OFC                          ORA  EVENTO
203300940324     C                   EXSR      CALDAT
203400940324     C*
203500940324     C* CERCO SCAGLIONE DEL RITARDO
203600940324     C                   EXSR      CALRIT
203700940324     C*
203800940324     C* SE CONSEGNA PARZIALE COME SE FOSSE CONSEGNATA
203900940324    4C     §2ATIP        IFEQ      'P'
204000940324     C*
204100940324     C* C O N S E G N A T E
204200090916     C   10              ADD       1             CNE(B)                          "E"
204300090916     C   11              ADD       1             CNC(B)                          "C"
204400090916     C   20              ADD       1             CNN(B)                          "D"
204500090916     C   21              ADD       1             CNh(B)                          "H"
204600940324     C* TOTALE A PARTE COME C.PARZIALE
204700970121     C   10              ADD       1             PRE(1)                          CONS PARZ
204800970121     C   11              ADD       1             PRC(1)
204900090916     C   20              ADD       1             PRN(1)
205000090916     C   21              ADD       1             PRh(1)
205100940324     C*
205200940324     C   99              MOVEL     'P'           STAFLG
205300940324     C   99              ADD       1             STAP              7 0
205400940324     C*
205500940324   X4C                   ELSE
205600940324     C* RISERVA CONSEGNA
205700940324     C   99              MOVEL     'R'           STAFLG
205800940324     C   99              ADD       1             STAR              7 0
205900970121     C   10              ADD       1             RSE(1)                         RISERVA CONS
206000970121     C   11              ADD       1             RSC(1)
206100090916     C   20              ADD       1             RSN(1)
206200090916     C   21              ADD       1             RSh(1)
206300940324    4C                   ENDIF
206400940324    3C                   ENDIF
206500940324     C*
206600940324     C   99              EXCEPT    SPED
206700940324    2C                   ENDIF
206800940324     C*
206900990805     C  N32KARB4         READE     FNEVB000                               32
207000940324    1C                   ENDDO
207100940324     C*
207200940324     C                   ENDSR
207300950124     C**************************************************************************
207400950124     C*    STAMPA STATISTICA
207500950124     C**************************************************************************
207600920605     C     STAMPA        BEGSR
207700950124     C*
207800940117     C   OF              EXCEPT    TESTA
207900940117     C   OF              SETOFF                                       OF
208000940117     C*
208100930908     C                   CLEAR                   CONP1
208200930908     C                   CLEAR                   CONP2
208300930908     C*
208400930908     C* SPEDIZIONI IN CONSEGNA
208500090916     C                   XFOOT     ICE           GENICE            6 0           "E"
208600090917     C                   XFOOT     ICh           GENICH            6 0           "H"
208700090916     C                   XFOOT     ICN           GENICN            6 0           "D"
208800090916     C                   XFOOT     ICC           W0090                           "C"
208900970121     C                   ADD       W0090         GENICN
209000930908     C     GENICN        ADD       GENICE        GENIC             7 0           TOT CONSEG
209100090917     C                   ADD       GENICH        GENIC             7 0           TOT CONSEG
209200930908     C* SPEDIZIONI CONSEGNABILI
209300090916     C                   XFOOT     COE           GENCOE            6 0           "E"
209400090917     C                   XFOOT     COh           GENCOH            6 0           "H"
209500090916     C                   XFOOT     CON           GENCON            6 0           "D"
209600090916     C                   XFOOT     COC           W0090                           "C"
209700090916     C                   ADD       W0090         GENCON                          D+C
209800930908     C     GENCON        ADD       GENCOE        GENCO             7 0           TOT CONSEG
209900090917     C                   ADD       GENCOh        GENCO             7 0           TOT CONSEG
210000930908     C**
210100930908     C* SPEDIZIONI   C O N S E G N A T E
210200930908     C**
210300090916     C                   XFOOT     CNE           GENCNE            6 0           "E"
210400090917     C                   XFOOT     CNh           GENCNH            6 0           "H"
210500090916     C                   XFOOT     CNN           GENCNN            6 0           "D"
210600090916     C                   XFOOT     CNC           W0090                           "C"
210700090916     C                   ADD       W0090         GENCNN                          D+C
210800930812     C     GENCNN        ADD       GENCNE        GENCN             7 0           TOT CONSEG
210900090917     C                   ADD       GENCNh        GENCN             7 0           TOT CONSEG
211000930908     C*
211100930908     C* % CONSEGNATE        SU TOT IN CONSEGNA
211200930908    1C     GENCN         IFGT      0
211300930908     C*
211400930908     C     GENCN         MULT      100           NUM9              9 0
211500950123     C     GENIC         IFNE      *ZERO
211600940526     C     NUM9          DIV(H)    GENIC         W0052             5 2
211700950123     C                   ELSE
211800950123     C                   Z-ADD     *ZERO         W0052
211900950123     C                   ENDIF
212000940526     C                   EXSR      CTRP1
212100930908     C*
212200930908     C* % CONSEGNATE        SU TOT CONSEGNABILE
212300950123     C     GENCO         IFNE      *ZERO
212400940526     C     NUM9          DIV(H)    GENCO         W0052
212500950123     C                   ELSE
212600950123     C                   Z-ADD     *ZERO         W0052
212700950123     C                   ENDIF
212800940526     C                   EXSR      CTRP2
212900930908    1C                   ENDIF
213000930908     C*
213100940117     C                   MOVEL     TE(18)        RIGA             66
213200940202     C                   Z-ADD     GENCNN        GENNOR
213300940202     C                   Z-ADD     GENCNE        GENESP
213400090917     C                   Z-ADD     GENCNH        GENH10
213500940202     C                   Z-ADD     GENCN         GENTOT
213600930908     C                   EXCEPT    INTE1
213700940208     C*
213800940208     C* ESEGUO STAMPA DELLE CONSEGNE PARZIALI E RISERVE
213900940208     C                   EXSR      STAPR
214000930908     C*
214100930908     C* STAMPA DETTAGLIO
214200930908    1C     GENCN         IFGT      0
214300930908     C                   MOVEA     CNE           ESP
214400090917     C                   MOVEA     CNh           h10
214500970121     C     CNC           ADD       CNN           NOR
214600930908     C                   EXSR      STADCN
214700930908    1C                   ENDIF
214800940324     C**
214900940324     C     *IN02         IFEQ      *OFF
215000940329     C* LA 1 PERCENTUALE SUL TOTALE CONSEGNATO NON CI VUOLE PIU'
215100940329     C                   SETON                                        03
215200940324     C**
215300930908     C**
215400930908     C* SPEDIZIONI   N O N    C O N S E G N A T E
215500930908     C**
215600930908     C* GIACENZE APERTE
215700090916     C                   XFOOT     GAE           GENGAE            6 0           "E"
215800090918     C                   XFOOT     GAh           GENGAh            6 0           "H"
215900090916     C                   XFOOT     GAN           GENGAN            6 0           "D"
216000090916     C                   XFOOT     GAC           W0090                           "C"
216100090916     C                   ADD       W0090         GENGAN                          D+C
216200930908     C     GENGAN        ADD       GENGAE        GENGA             7 0           TOT CONSEG
216300090918     C                   ADD       GENGAH        GENGA             7 0           TOT CONSEG
216400930908     C* LASCIATI AVVISI
216500090916     C                   XFOOT     LAE           GENLAE            6 0           "E"
216600090918     C                   XFOOT     LAh           GENLAH            6 0           "H"
216700090916     C                   XFOOT     LAN           GENLAN            6 0           "D"
216800090916     C                   XFOOT     LAC           W0090                           "C"
216900090916     C                   ADD       W0090         GENLAN                          D+C
217000930908     C     GENLAN        ADD       GENLAE        GENLA             7 0           TOT CONSEG
217100090918     C                   ADD       GENLAh        GENLA             7 0           TOT CONSEG
217200930908     C* CHIUSE PER TURNO
217300090916     C                   XFOOT     CTE           GENCTE            6 0           "E"
217400090918     C                   XFOOT     CTh           GENCTH            6 0           "H"
217500090916     C                   XFOOT     CTN           GENCTN            6 0           "D"
217600090916     C                   XFOOT     CTC           W0090                           "C"
217700090916     C                   ADD       W0090         GENCTN                          D+C
217800930908     C     GENCTN        ADD       GENCTE        GENCT             7 0           TOT CONSEG
217900090918     C                   ADD       GENCTh        GENCT             7 0           TOT CONSEG
218000930908     C* NON FATTE
218100090916     C                   XFOOT     NFE           GENNFE            6 0           "E"
218200090918     C                   XFOOT     NFh           GENNFH            6 0           "H"
218300090916     C                   XFOOT     NFN           GENNFN            6 0           "D"
218400090916     C                   XFOOT     NFC           W0090                           "C"
218500090916     C                   ADD       W0090         GENNFN                          D+C
218600930908     C     GENNFN        ADD       GENNFE        GENNF             7 0           TOT CONSEG
218700090918     C                   ADD       GENNFH        GENNF             7 0           TOT CONSEG
218800930908     C*
218900930908    2C     GENGA         IFGT      0
219000930908     C     GENLA         ORGT      0
219100930908     C     GENCT         ORGT      0
219200930908     C     GENNF         ORGT      0
219300930908     C                   CLEAR                   CONP1
219400930908     C                   CLEAR                   CONP2
219500090918     C*
219600930908     C     GENGAN        ADD       GENLAN        GENNOR
219700930908     C                   ADD       GENCTN        GENNOR
219800930908     C                   ADD       GENNFN        GENNOR
219900090918     C*
220000930908     C     GENGAE        ADD       GENLAE        GENESP
220100930908     C                   ADD       GENCTE        GENESP
220200930908     C                   ADD       GENNFE        GENESP
220300090918     C*
220400090918     C     GENGAh        ADD       GENLAh        GENh10
220500090918     C                   ADD       GENCTh        GENh10
220600090918     C                   ADD       GENNFh        GENh10
220700090918     C* TOTALE
220800930908     C     GENNOR        ADD       GENESP        GENTOT
220900090918     C                   ADD       GENh10        GENTOT
221000930908     C*
221100930908     C* % NON CONSEGNATE    SU TOT IN CONSEGNA
221200930908     C*
221300930908     C     GENTOT        MULT      100           NUM9              9 0
221400950123     C     GENIC         IFNE      *ZERO
221500940526     C     NUM9          DIV(H)    GENIC         W0052
221600950123     C                   ELSE
221700950123     C                   Z-ADD     *ZERO         W0052
221800950123     C                   ENDIF
221900940526     C                   EXSR      CTRP1
222000930908     C*
222100930908     C* % NON CONSEGNATE    SU TOT CONSEGNABILE
222200950123     C     GENCO         IFNE      *ZERO
222300940526     C     NUM9          DIV(H)    GENCO         W0052
222400950123     C                   ELSE
222500950123     C                   Z-ADD     *ZERO         W0052
222600950123     C                   ENDIF
222700940526     C                   EXSR      CTRP2
222800930908     C*
222900940117     C                   MOVEL     TE(24)        RIGA
223000940117     C*
223100940117     C   OF              EXCEPT    TESTA
223200940117     C   OF              SETOFF                                       OF
223300930908     C                   EXCEPT    INTE1
223400930908     C**
223500930908     C* GIACENZE APERTE
223600930908     C**
223700930908     C                   Z-ADD     GENGA         GENTOT
223800930908     C                   Z-ADD     GENGAN        GENNOR
223900930908     C                   Z-ADD     GENGAE        GENESP
224000090918     C                   Z-ADD     GENGAh        GENh10
224100090918     C                   MOVEA     GAE           ESP
224200090918     C                   MOVEA     GAh           h10
224300090916     C     GAC           ADD       GAN           NOR
224400940202     C                   MOVEL     TE(25)        RIGA
224500930908     C*
224600930908     C                   EXSR      CTRNCO
224700930908     C**
224800930908     C* LASCIATI AVVISI
224900930908     C**
225000930908     C                   Z-ADD     GENLA         GENTOT
225100930908     C                   Z-ADD     GENLAN        GENNOR
225200930908     C                   Z-ADD     GENLAE        GENESP
225300090918     C                   Z-ADD     GENLAh        GENh10
225400090918     C                   MOVEA     LAE           ESP
225500090918     C                   MOVEA     LAh           h10
225600090916     C     LAC           ADD       LAN           NOR
225700940202     C                   MOVEL     TE(26)        RIGA
225800930908     C*
225900930908     C                   EXSR      CTRNCO
226000930908     C**
226100930908     C* CHIUSO PER TURNO
226200930908     C**
226300930908     C                   Z-ADD     GENCT         GENTOT
226400930908     C                   Z-ADD     GENCTN        GENNOR
226500930908     C                   Z-ADD     GENCTE        GENESP
226600090918     C                   Z-ADD     GENCTH        GENh10
226700090918     C                   MOVEA     CTE           ESP
226800090918     C                   MOVEA     CTh           h10
226900090916     C     CTC           ADD       CTN           NOR
227000940207     C                   MOVEL     TE(28)        RIGA
227100930908     C*
227200930908     C                   EXSR      CTRNCO
227300930908     C**
227400930908     C* NON FATTE
227500930908     C**
227600930908     C                   Z-ADD     GENNF         GENTOT
227700930908     C                   Z-ADD     GENNFN        GENNOR
227800930908     C                   Z-ADD     GENNFE        GENESP
227900090918     C                   Z-ADD     GENNFh        GENh10
228000090918     C                   MOVEA     NFE           ESP
228100090918     C                   MOVEA     NFh           h10
228200090916     C     NFC           ADD       NFN           NOR
228300940207     C                   MOVEL     TE(27)        RIGA
228400930908     C*
228500930908     C                   EXSR      CTRNCO
228600930908    2C                   ENDIF
228700930908     C**
228800930908     C* SPEDIZIONI   A N C O R A    I N    D I S T I N T A
228900930908     C**
229000090916     C                   XFOOT     IDE           GENIDE            6 0           "E"
229100090918     C                   XFOOT     IDh           GENIDH            6 0           "H"
229200090916     C                   XFOOT     IDN           GENIDN            6 0           "D"
229300090916     C                   XFOOT     IDC           W0090                           "C"
229400090916     C                   ADD       W0090         GENIDN                          D+c
229500930908     C     GENIDN        ADD       GENIDE        GENID             7 0           TOT CONSEG
229600090918     C                   ADD       GENIDh        GENID             7 0           TOT CONSEG
229700930908     C*
229800930908     C                   CLEAR                   CONP1
229900930908     C                   CLEAR                   CONP2
230000930908     C*
230100930908     C* % IN DISTINTA       SU TOT IN CONSEGNA
230200930908    1C     GENID         IFGT      0
230300930908     C*
230400930908     C     GENID         MULT      100           NUM9              9 0
230500950123     C     GENIC         IFNE      *ZERO
230600940526     C     NUM9          DIV(H)    GENIC         W0052
230700950123     C                   ELSE
230800950123     C                   Z-ADD     *ZERO         W0052
230900950123     C                   ENDIF
231000940526     C                   EXSR      CTRP1
231100930908     C*
231200930908     C* % IN DISTINTA       SU TOT CONSEGNABILE
231300950123     C     GENCO         IFNE      *ZERO
231400940526     C     NUM9          DIV(H)    GENCO         W0052
231500950123     C                   ELSE
231600950123     C                   Z-ADD     *ZERO         W0052
231700950123     C                   ENDIF
231800940526     C                   EXSR      CTRP2
231900930908     C*
232000940117     C                   MOVEL     TE(29)        RIGA
232100940202     C                   Z-ADD     GENIDN        GENNOR
232200940202     C                   Z-ADD     GENIDE        GENESP
232300090918     C                   Z-ADD     GENIDh        GENh10
232400940202     C                   Z-ADD     GENID         GENTOT
232500930908     C*
232600940117     C   OF              EXCEPT    TESTA
232700940117     C   OF              SETOFF                                       OF
232800930908     C                   EXCEPT    INTE1
232900930908     C*
233000930908     C* STAMPA DETTAGLIO
233100090918     C                   MOVEA     IDE           ESP
233200090918     C                   MOVEA     IDh           h10
233300090916     C     IDC           ADD       IDN           NOR
233400930908     C                   EXSR      STADCN
233500930908    1C                   ENDIF
233600930908     C**
233700930908     C* SPEDIZIONI   I N    C O N S E G N A
233800930908     C**
233900930908     C                   CLEAR                   CONP2
234000930908     C*
234100930908     C* % IN DISTINTA       SU TOT CONSEGNABILE
234200930908    2C     GENIC         IFGT      0
234300930908     C     GENIC         MULT      100           NUM9              9 0
234400950123     C     GENCO         IFNE      *ZERO
234500940526     C     NUM9          DIV(H)    GENCO         W0052
234600950123     C                   ELSE
234700950123     C                   Z-ADD     *ZERO         W0052
234800950123     C                   ENDIF
234900940526     C                   EXSR      CTRP2
235000930908    2C                   ENDIF
235100930908     C*
235200940117     C                   MOVEL     TE(30)        RIGA
235300940202     C                   Z-ADD     GENICN        GENNOR
235400940202     C                   Z-ADD     GENICE        GENESP
235500090918     C                   Z-ADD     GENICh        GENh10
235600940202     C                   Z-ADD     GENIC         GENTOT
235700930908     C*
235800940117     C   OF              EXCEPT    TESTA
235900940117     C   OF              SETOFF                                       OF
236000930908     C                   EXCEPT    INTE2
236100930908     C*
236200930908     C* STAMPA DETTAGLIO
236300930908    2C     GENIC         IFGT      0
236400090918     C                   MOVEA     ICE           ESP
236500090918     C                   MOVEA     ICh           h10
236600090916     C     ICC           ADD       ICN           NOR
236700930908     C                   EXSR      STADCN
236800930908    2C                   ENDIF
236900930908     C**
237000930908     C* SPEDIZIONI   N O N    I N    C O N S E G N A
237100930908     C**
237200090916     C                   XFOOT     NCE           GENNCE            6 0           "E"
237300090918     C                   XFOOT     NCh           GENNCH            6 0           "H"
237400090916     C                   XFOOT     NCN           GENNCN            6 0           "D"
237500090916     C                   XFOOT     NCC           W0090                           "C"
237600090916     C                   ADD       W0090         GENNCN                          D+C
237700940117     C     GENNCN        ADD       GENNCE        GENNC             7 0           TOT CONSEG
237800090918     C                   ADD       GENNCh        GENNC             7 0           TOT CONSEG
237900930908     C*
238000930908     C* % NON IN CONSEGNA   SU TOT CONSEGNABILE
238100930908    2C     GENNC         IFGT      0
238200940526     C                   SETON                                        04
238300940117     C                   CLEAR                   CONP2
238400940117     C*
238500930908     C     GENNC         MULT      100           NUM9              9 0
238600950123     C     GENCO         IFNE      *ZERO
238700940526     C     NUM9          DIV(H)    GENCO         W0052
238800950123     C                   ELSE
238900950123     C                   Z-ADD     *ZERO         W0052
239000950123     C                   ENDIF
239100940526     C                   EXSR      CTRP2
239200930908     C*
239300940117     C                   MOVEL     TE(31)        RIGA
239400940202     C                   Z-ADD     GENNCN        GENNOR
239500940202     C                   Z-ADD     GENNCE        GENESP
239600090918     C                   Z-ADD     GENNCh        GENh10
239700940202     C                   Z-ADD     GENNC         GENTOT
239800930908     C*
239900940117     C   OF              EXCEPT    TESTA
240000940117     C   OF              SETOFF                                       OF
240100930908     C                   EXCEPT    INTE2
240200940117     C*
240300940117     C* STAMPA DETTAGLIO
240400940526    3C     GENNC         IFGT      0
240500090918     C                   MOVEA     NCE           ESP
240600090918     C                   MOVEA     NCh           h10
240700090916     C     NCC           ADD       NCN           NOR
240800940117     C                   EXSR      STADCN
240900940526    3C                   ENDIF
241000940526     C                   SETOFF                                       04
241100940526    2C                   ENDIF
241200930908     C*
241300930908     C**
241400930908     C* SPEDIZIONI   C O N S E G N A B I L I
241500930908     C**
241600930908     C*
241700940117     C                   MOVEL     TE(32)        RIGA
241800940202     C                   Z-ADD     GENCON        GENNOR            8 0
241900940202     C                   Z-ADD     GENCOE        GENESP            8 0
242000090918     C                   Z-ADD     GENCOh        GENh10            8 0
242100940202     C                   Z-ADD     GENCO         GENTOT            8 0
242200930908     C*
242300940117     C   OF              EXCEPT    TESTA
242400940117     C   OF              SETOFF                                       OF
242500090918     c
242600090918     c                   if        not *in25
242700090918     C                   EXCEPT    INTE3
242800090918     c                   else
242900090918     c* con tipo servizio H
243000090918     c                   seton                                        26
243100090918     C                   EXCEPT    INTE2
243200090918     c                   endif
243300930909     C*
243400090918     c                   setoff                                       26
243500930909     C* STAMPO DETTAGLIO
243600930909     C                   Z-ADD     33            Y
243700930909     C*
243800940207     C                   SETON                                        01
243900930909     C                   DO        3             B
244000940207     C                   CLEAR                   CONP3
244100940117     C                   MOVEL     TE(Y)         RIGA
244200930909     C                   ADD       1             Y
244300090918     C                   Z-ADD     COE(B)        GENESP
244400090918     C                   Z-ADD     COh(B)        GENh10
244500090916     C     COC(B)        ADD       CON(B)        GENNOR
244600970121     C     GENESP        ADD       GENNOR        GENTOT
244700090918     C                   ADD       GENh10        GENTOT
244800090918     c
244900930909     C* % NON IN CONSEGNA   SU TOT CONSEGNABILE
245000930909    2C     GENTOT        IFGT      0
245100930909     C     GENTOT        MULT      100           NUM9              9 0
245200950123     C     GENCO         IFNE      *ZERO
245300940526     C     NUM9          DIV(H)    GENCO         W0052
245400950123     C                   ELSE
245500950123     C                   Z-ADD     *ZERO         W0052
245600950123     C                   ENDIF
245700940526     C                   EXSR      CTRP3
245800930909     C                   ENDIF
245900930909     C*
246000940117     C   OF              EXCEPT    TESTA
246100940117     C   OF              SETOFF                                       OF
246200930909     C                   EXCEPT    DET2
246300930909     C                   ENDDO
246400940324     C*
246500940324     C                   ENDIF
246600940117     C* RIGA FINALE
246700940117     C                   EXCEPT    DET3
246800940117     C                   SETON                                        OF
246900950124     C*
247000920617     C                   ENDSR
247100950124     C**************************************************************************
247200950124     C* ESEGUO STAMPA DELLE CONSEGNE PARZIALI E RISERVE
247300950124     C**************************************************************************
247400940208     C     STAPR         BEGSR
247500940208     C*
247600940329     C* S T A M P O   C  O N S E G N E   P A R Z I A L I
247700940208     C     PRE(1)        ADD       PRN(1)        GENTOT
247800970121     C                   ADD       PRC(1)        GENTOT
247900090916     C                   ADD       PRh(1)        GENTOT
248000940208     C     GENTOT        IFGT      0
248100970121     C     PRC(1)        ADD       PRN(1)        GENNOR
248200090918     C                   Z-ADD     PRE(1)        GENESP
248300090918     C                   Z-ADD     PRh(1)        GENh10
248400940208     C                   MOVEL     TE(38)        RIGA             66
248500940208     C* % CONSEGNATE        SU TOT CONSEGNATO
248600940208     C*
248700940208     C     GENTOT        MULT      100           NUM9              9 0
248800950123     C     GENCN         IFNE      *ZERO
248900940526     C     NUM9          DIV(H)    GENCN         W0052
249000950123     C                   ELSE
249100950123     C                   Z-ADD     *ZERO         W0052
249200950123     C                   ENDIF
249300940526     C                   EXSR      CTRP1
249400940208     C* % CONSEGNATE        SU TOT IN CONSEGNA
249500950123     C     GENIC         IFNE      *ZERO
249600940526     C     NUM9          DIV(H)    GENIC         W0052
249700950123     C                   ELSE
249800950123     C                   Z-ADD     *ZERO         W0052
249900950123     C                   ENDIF
250000940526     C                   EXSR      CTRP2
250100940208     C*
250200940208     C* % CONSEGNATE        SU TOT CONSEGNABILE
250300950123     C     GENCO         IFNE      *ZERO
250400940526     C     NUM9          DIV(H)    GENCO         W0052
250500950123     C                   ELSE
250600950123     C                   Z-ADD     *ZERO         W0052
250700950123     C                   ENDIF
250800940526     C                   EXSR      CTRP3
250900940526     C*
251000940208     C                   EXCEPT    DET2
251100940208     C                   ENDIF
251200940208     C*
251300940329     C* S T A M P O   L E   R I S E R V E
251400940208     C     RSE(1)        ADD       RSN(1)        GENTOT
251500970121     C                   ADD       RSC(1)        GENTOT
251600090916     C                   ADD       RSh(1)        GENTOT
251700940208     C     GENTOT        IFGT      0
251800970121     C     RSC(1)        ADD       RSN(1)        GENNOR
251900090918     C                   Z-ADD     RSE(1)        GENESP
252000090918     C                   Z-ADD     RSh(1)        GENh10
252100940208     C                   MOVEL     TE(39)        RIGA             66
252200940208     C* % CONSEGNATE        SU TOT CONSEGNATO
252300940208     C*
252400940208     C     GENTOT        MULT      100           NUM9              9 0
252500950123     C     GENCN         IFNE      *ZERO
252600940526     C     NUM9          DIV(H)    GENCN         W0052
252700950123     C                   ELSE
252800950123     C                   Z-ADD     *ZERO         W0052
252900950123     C                   ENDIF
253000940526     C                   EXSR      CTRP1
253100940208     C* % CONSEGNATE        SU TOT IN CONSEGNA
253200950123     C     GENIC         IFNE      *ZERO
253300940526     C     NUM9          DIV(H)    GENIC         W0052
253400950123     C                   ELSE
253500950123     C                   Z-ADD     *ZERO         W0052
253600950123     C                   ENDIF
253700940526     C                   EXSR      CTRP2
253800940208     C*
253900940208     C* % CONSEGNATE        SU TOT CONSEGNABILE
254000950123     C     GENCO         IFNE      *ZERO
254100940526     C     NUM9          DIV(H)    GENCO         W0052
254200950123     C                   ELSE
254300950123     C                   Z-ADD     *ZERO         W0052
254400950123     C                   ENDIF
254500940526     C                   EXSR      CTRP3
254600940526     C*
254700940208     C                   EXCEPT    DET2
254800940208     C                   ENDIF
254900940208     C*
255000940208     C                   ENDSR
255100950124     C**************************************************************************
255200950124     C* STAMPA DETTAGLIO DELLE CONSEGNATE
255300950124     C**************************************************************************
255400930908     C     STADCN        BEGSR
255500950124     C*
255600930908    1C                   DO        13            B
255700930908     C     ESP(B)        ADD       NOR(B)        TOT(B)
255800090918     C                   ADD       h10(B)        TOT(B)
255900930908     C* SCAGLIONE PIENO
256000930908    2C     TOT(B)        IFGT      0
256100930908     C*
256200930908     C                   SELECT
256300930908     C* RITARDO
256400930908    3C     ORE(B)        WHENGT    0
256500930908    4C     ORE(B)        IFEQ      999
256600940117     C                   MOVEL     TE(23)        RIGA
256700930908     C     B             SUB       1             Y
256800930908   X4C                   ELSE
256900940117     C                   MOVEL     TE(22)        RIGA
257000930908     C                   Z-ADD     B             Y
257100930908    4C                   ENDIF
257200930908     C* ANTICIPO
257300930908     C     ORE(B)        WHENLT    0
257400930908    4C     ORE(B)        IFEQ      -999
257500940117     C                   MOVEL     TE(19)        RIGA
257600930908     C     B             ADD       1             Y
257700930908   X4C                   ELSE
257800940117     C                   MOVEL     TE(20)        RIGA
257900930908     C                   Z-ADD     B             Y
258000930908    4C                   ENDIF
258100930908     C*
258200930908   X3C                   OTHER
258300930908     C* CONFORMITA'
258400940117     C                   MOVEL     TE(21)        RIGA
258500940202     C                   Z-ADD     B             Y
258600930908    3C                   ENDSL
258700930908     C*
258800930908     C                   CLEAR                   CONP1
258900930908     C                   CLEAR                   CONP2
259000940329     C                   CLEAR                   CONP3
259100930908     C*
259200940526     C     TOT(B)        MULT      100           NUM9              9 0
259300940526     C*
259400930908     C* % SUL TOTALE IN CONSEGNA
259500940526     C* (NON ELABORO PER LE NON IN CONSEGNA)
259600940526     C     *IN04         IFEQ      *OFF
259700950123     C     GENIC         IFNE      *ZERO
259800940526     C     NUM9          DIV(H)    GENIC         W0052
259900950123     C                   ELSE
260000950123     C                   Z-ADD     *ZERO         W0052
260100950123     C                   ENDIF
260200940526     C                   EXSR      CTRP2
260300940526     C                   ENDIF
260400930908     C*
260500930908     C* % SUL TOTALE CONSEGNABILE
260600950123     C     GENCO         IFNE      *ZERO
260700940526     C     NUM9          DIV(H)    GENCO         W0052
260800950123     C                   ELSE
260900950123     C                   Z-ADD     *ZERO         W0052
261000950123     C                   ENDIF
261100940526     C                   EXSR      CTRP3
261200940203     C* % SUL TOTALE CONSEGNATO
261300940329     C* (SOLO QUANDO ELABORO LE SPEDIZIONI CONSEGNATE)
261400940526     C     *IN03         IFEQ      *OFF
261500950123     C     GENCN         IFNE      *ZERO
261600940526     C     NUM9          DIV(H)    GENCN         W0052
261700950123     C                   ELSE
261800950123     C                   Z-ADD     *ZERO         W0052
261900950123     C                   ENDIF
262000940526     C                   EXSR      CTRP1
262100940526     C                   ENDIF
262200970121     C*
262300970121     C                   Z-ADD     NOR(B)        PNORM             7 0
262400970121     C                   Z-ADD     ESP(B)        PESPR             7 0
262500090918     C                   Z-ADD     h10(B)        Ph1030            7 0
262600970121     C                   Z-ADD     TOT(B)        PTOTAL            8 0
262700930908     C*
262800940117     C   OF              EXCEPT    TESTA
262900940117     C   OF              SETOFF                                       OF
263000940117     C                   EXCEPT    DET1
263100930908     C*
263200930908    2C                   ENDIF
263300930908    1C                   ENDDO
263400950124     C*
263500930908     C                   ENDSR
263600950124     C**************************************************************************
263700950124     C* CONTROLLO LE SPEDIZIONI NON CONSEGNATE DA STAMPARE
263800950124     C**************************************************************************
263900930908     C     CTRNCO        BEGSR
264000950124     C*
264100930908    1C     GENTOT        IFGT      0
264200930908     C* %    SU TOT IN CONSEGNA
264300930908     C     GENTOT        MULT      100           NUM9              9 0
264400950123     C     GENIC         IFNE      *ZERO
264500940526     C     NUM9          DIV(H)    GENIC         W0052
264600950123     C                   ELSE
264700950123     C                   Z-ADD     *ZERO         W0052
264800950123     C                   ENDIF
264900940526     C                   EXSR      CTRP2
265000930908     C*
265100930908     C* %    SU TOT CONSEGNABILE
265200950123     C     GENCO         IFNE      *ZERO
265300940526     C     NUM9          DIV(H)    GENCO         W0052
265400950123     C                   ELSE
265500950123     C                   Z-ADD     *ZERO         W0052
265600950123     C                   ENDIF
265700940526     C                   EXSR      CTRP3
265800940207     C*
265900940203     C                   EXCEPT    DET4
266000940203     C                   EXCEPT    DET2
266100930908     C*
266200930908     C                   EXSR      STADCN
266300930908    1C                   ENDIF
266400950124     C*
266500930908     C                   ENDSR
266600950124     C**************************************************************************
266700950124     C* CONTROLLO SE 1% E' > 99,99
266800950124     C**************************************************************************
266900940526     C     CTRP1         BEGSR
267000950124     C*
267100940526     C     W0052         IFGT      99,99
267200940526     C                   Z-ADD     99,99         CONP1             4 2
267300940526     C                   ELSE
267400940526     C                   Z-ADD     W0052         CONP1
267500940526     C                   ENDIF
267600950124     C*
267700940526     C                   ENDSR
267800950124     C**************************************************************************
267900950124     C* CONTROLLO SE 2% E' > 99,99
268000950124     C**************************************************************************
268100940526     C     CTRP2         BEGSR
268200950124     C*
268300940526     C     W0052         IFGT      99,99
268400940526     C                   Z-ADD     99,99         CONP2             4 2
268500940526     C                   ELSE
268600940526     C                   Z-ADD     W0052         CONP2
268700940526     C                   ENDIF
268800950124     C*
268900940526     C                   ENDSR
269000950124     C**************************************************************************
269100950124     C* CONTROLLO SE 3% E' > 99,99
269200950124     C**************************************************************************
269300940526     C     CTRP3         BEGSR
269400950124     C*
269500940526     C     W0052         IFGT      99,99
269600940526     C                   Z-ADD     99,99         CONP3             4 2
269700940526     C                   ELSE
269800940526     C                   Z-ADD     W0052         CONP3
269900940526     C                   ENDIF
270000950124     C*
270100940526     C                   ENDSR
270200950124     C**************************************************************************
270300950124     C* STAMPE SPEDIZIONI NON IN CONSEGNA
270400950124     C**************************************************************************
270500940530     C     STASNC        BEGSR
270600950124     C*
270700950127     C                   Z-ADD     ARBAAS        P0CAAS            2 0
270800940530     C* MESE GIORNO SPEDIZIONE
270900940530     C                   MOVEL     ARBMGS        MMGG
271000940530     C                   Z-ADD     MM            P0CMGS            4 0
271100940530     C                   MOVEL     GG            P0CMGS
271200940601     C* DESTINATARIO
271300940601     C                   MOVEL     ARBRSD        P0CRSD           15
271400950127     C                   MOVEL     ARBIND        P0CIND           24
271500940601     C                   MOVEL     ARBLOD        P0CLOD           15
271600940601     C* DECODIFICA CONSEGNA PARTICOLARE
271700950127     C                   MOVEL     *BLANKS       P0DTC1
271800950123     C     ARBTC1        IFNE      ' '
271900940601     C                   MOVEL     '1P'          COD
272000940601     C                   MOVEL     *BLANKS       KEY
272100950123     C                   MOVEL     ARBTC1        KEY
272200970124     C     KTAB          CHAIN(N)  TABEL                              30
272300950127     C  N30              MOVEL     TBLUNI        P0DTC1            5
272400940601     C                   ENDIF
272500950127     C*
272600950127     C                   MOVEL     *BLANKS       P0DTC2
272700950127     C     ARBTC2        IFNE      *BLANKS
272800950127     C                   MOVEL     '1P'          COD
272900950127     C                   MOVEL     *BLANKS       KEY
273000950127     C                   MOVEL     ARBTC2        KEY
273100970124     C     KTAB          CHAIN(N)  TABEL                              30
273200950127     C  N30              MOVEL     TBLUNI        P0DTC2           10
273300950127     C                   ENDIF
273400051128     C* DATA CONSEGNA RICHIESTA richiamo pgm per sapere se tassativa
273500051128     c                   clear                   p0cdcr            6 0
273600051128     c                   clear                   p0ctcr
273700051128     c                   if        arbdcr>0 or arbhcr>0
273800051128     c                   clear                   fnlv80ds
273900051128     c                   eval      ilv80aas=arbaas
274000051128     c                   eval      ilv80lnp=arblnp
274100051128     c                   eval      ilv80nrs=arbnrs
274200051128     c                   eval      ilv80nsp=arbnsp
274300051128     c                   eval      ilv80mgs=arbmgs
274400051128     c                   eval      ilv80tc1=arbtc1
274500051128     c                   eval      ilv80tc2=arbtc2
274600051128     c                   eval      ilv80dcr=arbdcr
274700051128     c                   eval      ilv80hcr=arbhcr
274800051128     c                   eval      ilv80tcr=arbtcr
274900051128     c                   call      'FNLV80R'
275000051128     c                   parm                    kpjba
275100051128     c                   parm                    fnlv80ds
275200051128     c
275300051128     c                   eval      p0cdcr=olv80dcr6
275400051128     c*
275500051128     c                   if        olv80tat='S'
275600051128     c                   eval      p0ctcr='TA'
275700051128     c                   else
275800051128     c                   clear                   p0ctcr            2
275900051128     c                   move      arbtcr        p0ctcr
276000051128     c                   endif
276100051128     c                   endif
276200940530     C* FERMO DEPOSITO
276300051107     C**   ARBFFD        IFEQ      'S'
276400051107     C**                 MOVEL     'SI'          P0CFFD            2
276500051107     C**                 ELSE
276600051107     C**                 MOVEL     '  '          P0CFFD
276700051107     C**                 ENDIF
276800940530     C*
276900940530     C     *INOD         IFEQ      *ON
277000940530     C                   EXCEPT    TESELE
277100940530     C                   SETOFF                                       OD
277200940530     C                   ENDIF
277300950123     C*
277400950123     C* TENTO DI REPERIRE IL CONTRASEGNO
277500950123     C                   CLEAR                   AR9CAS
277600950123     C                   CLEAR                   AR9VCA
277700051115     C     KARB3         SETLL     FiAR901L                               30     ON=TROVATO
277800051115     C   30KARB3         CHAIN     FiAR901L                           30
277900950127     C*
278000950127     C* TENTO DI REPERIRE L'IMPORTO FATTURA
278100950127     C                   CLEAR                   AR6IFT
278200991130     C                   CLEAR                   AR6DIV
278300991128     C                   CLEAR                   COMIFT
278400991130     C                   CLEAR                   COMdiv
278500991104     C                   MOVEL     '1'           KTRC
278600991104     C     KARB6         SETLL     FIAR601L                               30     ON=TROVATO
278700991128     C     *IN30         IFEQ      *ON
278800991130     C     KARB6         CHAIN     FIAR601L                           30
278900940530     C*
279000991130     C     *IN30         IFEQ      *OFF
279100991130     C* Vado su tabella divise per sapere se accetta i decimali
279200991130     C     AR6DIV        IFNE      SAVDIV
279300991130     C                   MOVEL     'CV'          COD
279400991130     C                   MOVEL(P)  AR6DIV        KEY
279500991130     C     KTAB          CHAIN(N)  TABEL                              37
279600991130     C  N37              MOVEL     TBLUNI        DSCV
279700991130     C   37              CLEAR                   DSCV
279800991130     C                   MOVEL     AR6DIV        SAVDIV
279900991130     C                   ENDIF
280000991130     C*
280100991130     C* Edito importo fattura per la stampa
280200991130     c     ar6ift        ifgt      *zeros
280300991130     C                   Z-ADD     AR6IFT        W0122
280400991130     C                   MOVEL     §CVFDC        WTSTDEC
280500991130     c                   exsr      edtimporti
280600991130     c                   move      impeditato    comift            9
280700991130     c                   movel     ar6div        comdiv
280800991130     c                   endif
280900991128     C                   ENDIF
281000991130     C                   ENDIF
281100940530     C                   EXCEPT    DETELE
281200110923     c*
281300070419     C* STAMPO C/ASSEGNO NON ABILITATO SU SECONDA RIGA se GEO non attiva
281400070419     c                   if        not *in18
281500070419     c
281600070419     C     ARBAcc        IFEQ      ' '
281700070419     C     AR9CAS        ANDNE     0
281800110923     c                   seton                                        28
281900110923     c                   endif
282000110923     c
282100110923     c                   if        *in28 or *in19
282200070419     C                   EXCEPT    DETCAA
282300070419     C                   ENDIF
282400070419     c
282500070419     c* Stampo tutte le abilitazioni bolle che ci sono
282600070419     c                   else
282700070419     c                   clear                   fnlrg6ds
282800070419     c                   eval      ilrg6aas=arbaas
282900070419     c                   eval      ilrg6lnp=arblnp
283000070419     c                   eval      ilrg6nrs=arbnrs
283100070419     c                   eval      ilrg6nsp=arbnsp
283200070419     c                   call      'FNLRG6R'
283300070419     c                   parm                    fnlrg6ds
283400070419     c
283500070419     c                   if        olrg6vssin<>*blanks
283600110923     c                   seton                                        28
283700110923     c                   endif
283800110923     c
283900110923     c                   if        *in28 or *in19
284000070419     C                   EXCEPT    DETCAA
284100070419     c                   endif
284200070419     c                   endif
284300070419     c
284400950127     C* VEDO SE SONO PRESENTI DELLE NOTE
284500950127     C     *LIKE         DEFINE    AR4NOT        P0CNO1
284600950127     C     *LIKE         DEFINE    AR4NOT        P0CNO2
284700060905     C     *LIKE         DEFINE    AR4NOT        wstadis
284800950127     C                   CLEAR                   P0CNO1
284900950127     C                   CLEAR                   P0CNO2
285000950127     C                   MOVEL     '8'           AR4TRC
285100060216     C     KAR41         SETLL     FiAR401L                               30     ON=TROVATO
285200950127     C   30              DO
285300060216     C     KAR41         CHAIN     FiAR401L                           30
285400950127     C  N30              MOVEL     AR4NOT        P0CNO1
285500950127     C                   MOVEL     '9'           AR4TRC
285600060216     C     KAR41         SETLL     FiAR401L                               30     ON=TROVATO
285700950127     C   30              DO
285800060216     C     KAR41         CHAIN     FiAR401L                           30
285900950127     C  N30              MOVEL     AR4NOT        P0CNO2
286000950127     C                   ENDDO
286100950127     C                   ENDDO
286200060905     c
286300060905     c* Vedo se spedizioni in distinta con data > di quella elaborata
286400060905     c                   setoff                                       12
286500060905     c                   clear                   wstadis
286600060905     c                   if        arbndc>0 and arbddc>0
286700060905     c                   eval      wstadis=Cindistinta
286800060905     c                   eval      %subst(wstadis:15:7)= %editc(arbndc: 'Z')
286900060905     c* giro la data
287000060905     C                   Z-ADD     arbddc        G02INV                         AAAA/MM/GG
287100060905     C                   Z-ADD     *ZERO         G02DAT
287200060905     C                   MOVEL     '3'           G02ERR
287300060905     C                   CALL      'XSRDA8'
287400060905     C                   PARM                    WLBDAT
287500060905     C                   MOVE      G02DAT        W0020             2 0
287600060905     C                   MOVEL     G02DAT        STAddc            6 0          GG/MM/AA
287700060905     C                   MOVE      W0020         STAddc            6 0          GG/MM/AA
287800060905     c                   eval      %subst(wstadis:28:8)= %editc(staddc: 'Y')
287900060905     c
288000060905     c* se non c'e' la seconda nota la riempo
288100060905     c* altrimenti scrivo 3°riga
288200060905     c                   if        p0cno2=*blanks
288300060905     c                   eval      p0cno2=wstadis
288400060905     c                   else
288500060905     c                   seton                                        12
288600060905     c                   endif
288700060905     c                   endif
288800990329     C*
288900990329     C* VEDO SE E' PRESENTE UNA C.A.
289000090922     C                   SETOFF                                       07
289100040812      * Sostituisco la lettura del file FNDTC02 con
289200040812      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
289300040812      * legate alla spedizione che passo
289400040812      *
289500040812     c                   clear                   fidn12ds
289600040812     c                   eval      i12aas = arbaas
289700040812     c                   eval      i12lnp = arblnp
289800040812     c                   eval      i12nrs = arbnrs
289900040812     c                   eval      i12nsp = arbnsp
290000040812     c                   eval      i12fel = 'E'
290100040812     c                   eval      i12fan = 'E'
290200040812      *
290300040812     c                   call      'FIDN12R'
290400040812     c                   parm                    fidn12ds
290500040812      *
290600040812      * se non ci sono errori
290700040812     c                   if        o12err = ' '
290800040812      * se numero di CA trovate maggiore di zero
290900040812     c                   if        o12nca > 0
291000040812     c* prendo la prima CA
291100040812     c                   z-add     1             ii                2 0
291200040812     c                   movea     skkey(ii)     dskey
291300040812     C                   Z-ADD     dsfil         dctfil
291400040812     C                   Z-ADD     dsnca         dctnca
291500040812     C                   Z-ADD     skdca(ii)     dctdca
291600040812     C                   Z-ADD     skdch(ii)     dctdch
291700040812     C                   movea     skcch(ii)     dctcch
291800040812     c                   seton                                        07
291900040812     c                   exsr      preca
292000040812      *
292100040812     c                   endif
292200040812     c                   endif
292300040812      * stampo le note
292400040812     C* NOTE
292500040812     C     P0CNO1        IFNE      *BLANKS
292600040812     C     P0CNO2        ORNE      *BLANKS
292700040812     C     ARBTC2        ORNE      *BLANKS
292800040812     C     *IN07         OREQ      *ON
292900040812     C*
293000040812     C                   EXCEPT    DE2ELE
293100040812     C*
293200040812     c* se ci sono altre CA stampo ancora
293300040812      * se non ci sono errori
293400040812     c                   if        o12err = ' '  and o12nca > 0
293500040812     c                   add       1             ii
293600040812     c                   dow       ii <= o12nca
293700040812     c                   movea     skkey(ii)     dskey
293800040812     C                   Z-ADD     dsfil         dctfil
293900040812     C                   Z-ADD     dsnca         dctnca
294000040812     C                   Z-ADD     skdca(ii)     dctdca
294100040812     C                   Z-ADD     skdch(ii)     dctdch
294200040812     C                   movea     skcch(ii)     dctcch
294300040812      *
294400040812     C                   EXSR      PRECA
294500040812     C** STAMPO TESTATA E DETT C.A.
294600040812     C   OD              EXCEPT    TESELE
294700040812     C                   EXCEPT    DETCA
294800040812      *
294900040812     c                   add       1             ii
295000040812     C                   ENDDO
295100040812      *
295200040812     c                   endif
295300040812      *
295400040812     c                   endif
295500060905     c
295600060905     C** STAMPO se in distinta
295700060905     c                   if        *in12
295800060905     C   OD              EXCEPT    TESELE
295900060905     C                   EXCEPT    DETdis
296000060905     c                   endif
296100950123     C*
296200940530     C                   ENDSR
296300110923     C**************************************************************************
296400110923     C* controllo Perfect Order
296500110923     C**************************************************************************
296600110923     C     POrder        BEGSR
296700110926
296800110926     c                   eval      ilr78dtela=ds1ada
296900111012     c                   eval      ilr78sppar=ds1fl5
297000110926     c                   eval      kpjbu=fnlr78ds1
297100110926     c
297200110923     c                   call      'FNLR78R1'
297300110926     c                   parm                    kpjba
297400110926     c                   parm                    arbds
297500110923     c
297600110926     c                   eval      fnlr78ds1=kpjbu
297700110926     c
297800110923     C                   ENDSR
297900990329     C**************************************************************************
298000990329     C* PREPARO I DATI DELLA C.A.
298100990329     C**************************************************************************
298200990329     C     PRECA         BEGSR
298300990329     C                   SETOFF                                       06
298400990329     C                   CLEAR                   STACCH
298500990329     C* DATA APERTURA C.A.
298600990329     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
298700990329     C                   Z-ADD     *ZERO         G02DAT
298800990329     C                   MOVEL     '3'           G02ERR
298900990329     C                   CALL      'XSRDA8'
299000990329     C                   PARM                    WLBDAT
299100990329     C                   MOVE      G02DAT        W0020             2 0
299200990329     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
299300990329     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
299400990329     C** DATA CNHIUSUA C.A.
299500990329     C                   CLEAR                   STACHC
299600990329     C     DCTDCH        IFGT      0
299700990329     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
299800990329     C                   Z-ADD     *ZERO         G02DAT
299900990329     C                   MOVEL     '3'           G02ERR
300000990329     C                   CALL      'XSRDA8'
300100990329     C                   PARM                    WLBDAT
300200990329     C                   MOVE      G02DAT        W0020             2 0
300300990329     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
300400990329     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
300500990329     C                   SETON                                        06
300600990329     C* CAUSALE CHIUSURA
300700990329    2C     DCTCCH        IFNE      *BLANKS
300800990329     C                   CLEAR                   DCCH
300900990329     C                   CLEAR                   DSBS02
301000990329     C                   MOVEL     'C'           T02MOD
301100990329     C                   MOVEL     KNSIF         T02SIF
301200990329     C                   MOVEL     'CCH'         T02COD
301300990329     C                   MOVEL     DCTCCH        T02KE1
301400990329     C                   CALL      'TIBS02R'
301500990329     C                   PARM                    KPJBA
301600990329     C                   PARM                    DSBS02
301700990329     C*
301800990329    3C     T02ERR        IFEQ      ' '
301900990329     C                   MOVEL     T02UNI        DCCH
302000990329    3C                   ENDIF
302100990329     C                   MOVEL     §CHDES        STACCH           25
302200990329    2C                   ENDIF
302300990329     C                   ENDIF
302400990329     C                   ENDSR
302500970121     C**************************************************************************
302600970121     C* SCRIVO I RECORD DI TOTALE IN FNSCO
302700970121     C**************************************************************************
302800970121     C     WRTSCO        BEGSR
302900970130     C                   CLEAR                   FNSCO000
303000970130     C*
303100970130      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
303200970130      * riempo il "flag di trasmissione" (SPAFTR)
303300040922     C***  §5CSCO        IFNE      'S'
303400040922     C***                MOVE      'N'           SCOFTR                         FLAG TRASMISS.
303500040922     C***                MOVEL     *HIVAL        SCODTR
303600040922     C***                ENDIF
303700040922     c                   z-add     wdate1        scodtr
303800040922     c
303900970207     C                   MOVEL     DS1FEL        SCOFLE
304000970225     C                   MOVEL     DS1LNA        SCOLNA
304100970207     C                   MOVEL     DS1ADA        SCODCM
304200970130     C**
304300970130     C* Se si tratta di un giorno festivo scrivo soltanto un record
304400970130     C*  indicando che e' festa con "F" in SCOSCO
304500970225    0C     WFEST         IFEQ      'S'
304600970130     C                   MOVEL     'F'           SCOSCO
304700970130     C                   WRITE     FNSCO000
304800970130     C                   ELSE
304900970130     C**
305000970130     C**
305100970121     C* LID: identifica in a che livello di deve stampare (livello
305200970121     c*      orrizontale, essendoci in stampa 4 scomparti-livelli)
305300970121     c* ORD: e' l'oridne di stampa posto in verticale
305400970122     c* OLT: =A se e' oltre un certo anticipo
305500970122     c*      =D se e' in ritardo/anticipi di un tot di ore
305600970122     c*      =O se e' oltre un certo ritardo
305700970122     c*      =T record di totale
305800970122     c*      =' '  se non c'e' affidabilita'
305900970128     C**
306000970121     C*  C O N S E G N A T E
306100970121     C                   MOVEL     1             SCOLIV
306200970121     C                   MOVEL     '2'           WTOTLI
306300970121     C                   Z-ADD     2             SCOORD
306400970121     C                   MOVEL     'C'           SCOSCO
306500970121     C                   MOVEL     'S'           WANRIT            1
306600970121     C                   MOVEL     ' '           WGBO              1
306700970121     C                   MOVEA     CNE           SKE
306800970121     C                   MOVEA     CNC           SKC
306900970121     C                   MOVEA     CNN           SKN
307000090918     C                   MOVEA     CNh           SKh
307100970121     C                   MOVEL     '2'           WTOTLI            1
307200970121     C                   EXSR      SCRIVI
307300970121     C*
307400970121     C*  C O N S E G N E   P A R Z I A L I
307500970121     C                   MOVEL     1             SCOLIV
307600970121     C                   Z-ADD     3             SCOORD
307700970121     C                   MOVEL     'N'           WANRIT            1
307800970121     C                   MOVEL     'P'           WGBO              1
307900970121     C                   CLEAR                   SKE
308000970121     C                   CLEAR                   SKC
308100970121     C                   CLEAR                   SKN
308200090918     C                   CLEAR                   SKh
308300970121     C                   Z-ADD     PRE(1)        SKE(1)
308400970121     C                   MOVEA     PRC(1)        SKC(1)
308500970121     C                   MOVEA     PRN(1)        SKN(1)
308600090918     C                   MOVEA     PRh(1)        SKh(1)
308700970121     C                   EXSR      SCRIVI
308800970121     C*  R I S E R V E     C O N S E G N E
308900970121     C                   MOVEL     1             SCOLIV
309000970121     C                   Z-ADD     4             SCOORD
309100970121     C                   MOVEL     'N'           WANRIT            1
309200970121     C                   MOVEL     'R'           WGBO              1
309300970121     C                   CLEAR                   SKE
309400970121     C                   CLEAR                   SKC
309500970121     C                   CLEAR                   SKN
309600090918     C                   CLEAR                   SKh
309700970121     C                   Z-ADD     RSE(1)        SKE(1)
309800970121     C                   MOVEA     RSC(1)        SKC(1)
309900970121     C                   MOVEA     RSN(1)        SKN(1)
310000090918     C                   MOVEA     RSh(1)        SKh(1)
310100970121     C                   EXSR      SCRIVI
310200970121     C*  N O N   C O N S E G N A T E
310300970121     C*          DI CUI GIACENZE APERTE
310400970121     C                   MOVEL     1             SCOLIV
310500970121     C                   MOVEL     '1'           WTOTLI            1
310600970121     C                   Z-ADD     7             SCOORD
310700970121     C                   MOVEL     'N'           SCOSCO
310800970121     C                   MOVEL     'S'           WANRIT            1
310900970121     C                   MOVEL     'G'           WGBO              1
311000970121     C                   MOVEA     GAE           SKE
311100970121     C                   MOVEA     GAC           SKC
311200970121     C                   MOVEA     GAN           SKN
311300090918     C                   MOVEA     GAh           SKh
311400970121     C                   EXSR      SCRIVI
311500970121     C*          DI CUI LASCIATI AVVISI
311600970121     C                   MOVEL     1             SCOLIV
311700970121     C                   Z-ADD     9             SCOORD
311800970121     C                   MOVEL     'L'           WGBO              1
311900970121     C                   MOVEA     LAE           SKE
312000970121     C                   MOVEA     LAC           SKC
312100970121     C                   MOVEA     LAN           SKN
312200090918     C                   MOVEA     LAh           SKh
312300970121     C                   EXSR      SCRIVI
312400970121     C*          DI CUI CHIUSE PER TURNO
312500970121     C                   MOVEL     1             SCOLIV
312600970121     C                   Z-ADD     11            SCOORD
312700970121     C                   MOVEL     'C'           WGBO              1
312800970121     C                   MOVEA     CTE           SKE
312900970121     C                   MOVEA     CTC           SKC
313000970121     C                   MOVEA     CTN           SKN
313100090918     C                   MOVEA     CTh           SKh
313200970121     C                   EXSR      SCRIVI
313300970121     C*          DI CUI NON FATTE
313400970121     C                   MOVEL     1             SCOLIV
313500970121     C                   Z-ADD     13            SCOORD
313600970121     C                   MOVEL     'N'           WGBO              1
313700970121     C                   MOVEA     NFE           SKE
313800970121     C                   MOVEA     NFC           SKC
313900970121     C                   MOVEA     NFN           SKN
314000090918     C                   MOVEA     NFh           SKh
314100970121     C                   EXSR      SCRIVI
314200090918
314300970121     C*          TOTALE DELLE NON CONSEGNATE
314400970121     C                   MOVEL     2             SCOLIV
314500970121     C                   Z-ADD     5             SCOORD
314600970121     C                   MOVEL     'N'           WANRIT            1
314700970121     C                   MOVEL     'T'           SCOOLT
314800970121     C                   CLEAR                   WGBO
314900970121     C                   CLEAR                   SKE
315000970121     C                   CLEAR                   SKC
315100970121     C                   CLEAR                   SKN
315200090918     C                   CLEAR                   SKh
315300970121     C                   XFOOT     GAE           W0090             9 0
315400970121     C                   ADD       W0090         SKE(1)
315500970121     C                   XFOOT     LAE           W0090             9 0
315600970121     C                   ADD       W0090         SKE(1)
315700970121     C                   XFOOT     NFE           W0090             9 0
315800970121     C                   ADD       W0090         SKE(1)
315900970121     C                   XFOOT     CTE           W0090             9 0
316000970121     C                   ADD       W0090         SKE(1)
316100970121     C*
316200970121     C                   XFOOT     GAC           W0090             9 0
316300970121     C                   ADD       W0090         SKC(1)
316400970121     C                   XFOOT     LAC           W0090             9 0
316500970121     C                   ADD       W0090         SKC(1)
316600970121     C                   XFOOT     NFC           W0090             9 0
316700970121     C                   ADD       W0090         SKC(1)
316800970121     C                   XFOOT     CTC           W0090             9 0
316900970121     C                   ADD       W0090         SKC(1)
317000970121     C*
317100970121     C                   XFOOT     GAN           W0090             9 0
317200970121     C                   ADD       W0090         SKN(1)
317300970121     C                   XFOOT     LAN           W0090             9 0
317400970121     C                   ADD       W0090         SKN(1)
317500970121     C                   XFOOT     NFN           W0090             9 0
317600970121     C                   ADD       W0090         SKN(1)
317700970121     C                   XFOOT     CTN           W0090             9 0
317800970121     C                   ADD       W0090         SKN(1)
317900090918     C*
318000090918     C                   XFOOT     GAh           W0090             9 0
318100090918     C                   ADD       W0090         SKh(1)
318200090918     C                   XFOOT     LAh           W0090             9 0
318300090918     C                   ADD       W0090         SKh(1)
318400090918     C                   XFOOT     NFh           W0090             9 0
318500090918     C                   ADD       W0090         SKh(1)
318600090918     C                   XFOOT     CTh           W0090             9 0
318700090918     C                   ADD       W0090         SKh(1)
318800090918
318900970121     C                   EXSR      SCRIVI
319000090918
319100970121     C*  I N     D I S T I N T A
319200970121     C                   MOVEL     1             SCOLIV
319300970121     C                   MOVEL     '2'           WTOTLI
319400970121     C                   Z-ADD     15            SCOORD
319500970121     C                   MOVEL     'D'           SCOSCO
319600970121     C                   MOVEL     'S'           WANRIT            1
319700970121     C                   MOVEL     ' '           WGBO              1
319800970121     C                   MOVEA     IDE           SKE
319900970121     C                   MOVEA     IDC           SKC
320000970121     C                   MOVEA     IDN           SKN
320100090918     C                   MOVEA     IDh           SKh
320200970121     C                   EXSR      SCRIVI
320300970121     C*  I N     C O N S E G N A
320400970121     C                   MOVEL     1             SCOLIV
320500970121     C                   MOVEL     '3'           WTOTLI
320600970121     C                   Z-ADD     17            SCOORD
320700970121     C                   MOVEL     'I'           SCOSCO
320800970121     C                   MOVEL     'S'           WANRIT            1
320900970121     C                   MOVEL     ' '           WGBO              1
321000970121     C                   MOVEA     ICE           SKE
321100970121     C                   MOVEA     ICC           SKC
321200970121     C                   MOVEA     ICN           SKN
321300090918     C                   MOVEA     ICh           SKh
321400970121     C                   EXSR      SCRIVI
321500970121     C*  NON    I N     C O N S E G N A
321600970121     C                   MOVEL     1             SCOLIV
321700970121     C                   MOVEL     '3'           WTOTLI
321800970121     C                   Z-ADD     19            SCOORD
321900970121     C                   MOVEL     'O'           SCOSCO
322000970121     C                   MOVEL     'S'           WANRIT            1
322100970121     C                   MOVEL     ' '           WGBO              1
322200970121     C                   MOVEA     NCE           SKE
322300970121     C                   MOVEA     NCC           SKC
322400970121     C                   MOVEA     NCN           SKN
322500090918     C                   MOVEA     NCh           SKh
322600970121     C                   EXSR      SCRIVI
322700970121     C*  C O N S E G N A B I L I
322800970121     C                   MOVEL     4             SCOLIV
322900970121     C                   Z-ADD     20            SCOORD
323000970121     C                   MOVEL     'T'           SCOOLT
323100970121     C                   MOVEL     'B'           SCOSCO
323200970121     C                   MOVEL     'N'           WANRIT            1
323300970121     C                   MOVEL     ' '           WGBO              1
323400970121     C                   CLEAR                   SKE
323500970121     C                   CLEAR                   SKC
323600970121     C                   CLEAR                   SKN
323700090918     C                   CLEAR                   SKh
323800970121     C                   XFOOT     COE           SKE(1)
323900970121     C                   XFOOT     COC           SKC(1)
324000970121     C                   XFOOT     CON           SKN(1)
324100090918     C                   XFOOT     COh           SKh(1)
324200970121     C                   EXSR      SCRIVI
324300970121     C*
324400970121     C     COE(1)        IFGT      0
324500970121     C     COC(1)        ORGT      0
324600970121     C     CON(1)        ORGT      0
324700090918     C     COh(1)        ORGT      0
324800970121     C                   MOVEL     1             SCOLIV
324900970121     C                   Z-ADD     21            SCOORD
325000970121     C                   MOVEL     'B'           SCOSCO
325100970121     C                   MOVEL     ' '           SCOOLT
325200970121     C                   Z-SUB     999           SCOORE
325300970121     C                   MOVEL     ' '           SCOGBO
325400970121     C                   Z-ADD     COE(1)        SCOSS1
325500970121     C                   Z-ADD     COC(1)        SCOSS2
325600970121     C                   Z-ADD     CON(1)        SCOSS3
325700090918     C                   Z-ADD     COh(1)        SCOSS4
325800970121     C                   WRITE     FNSCO000
325900970121     C                   ENDIF
326000970121     C     COE(2)        IFGT      0
326100970121     C     COC(2)        ORGT      0
326200970121     C     CON(2)        ORGT      0
326300090918     C     COh(2)        ORGT      0
326400970121     C                   MOVEL     1             SCOLIV
326500970121     C                   Z-ADD     22            SCOORD
326600970121     C                   MOVEL     'B'           SCOSCO
326700970121     C                   MOVEL     ' '           SCOOLT
326800970121     C                   Z-ADD     0             SCOORE
326900970121     C                   MOVEL     ' '           SCOGBO
327000970121     C                   Z-ADD     COE(2)        SCOSS1
327100970121     C                   Z-ADD     COC(2)        SCOSS2
327200970121     C                   Z-ADD     CON(2)        SCOSS3
327300090918     C                   Z-ADD     COh(2)        SCOSS4
327400970121     C                   WRITE     FNSCO000
327500970121     C                   ENDIF
327600970121     C     COE(3)        IFGT      0
327700970121     C     COC(3)        ORGT      0
327800970121     C     CON(3)        ORGT      0
327900090918     C     COh(3)        ORGT      0
328000970121     C                   MOVEL     1             SCOLIV
328100970121     C                   Z-ADD     23            SCOORD
328200970121     C                   MOVEL     'B'           SCOSCO
328300970121     C                   MOVEL     ' '           SCOOLT
328400970121     C                   Z-ADD     999           SCOORE
328500970121     C                   MOVEL     ' '           SCOGBO
328600970121     C                   Z-ADD     COE(3)        SCOSS1
328700970121     C                   Z-ADD     COC(3)        SCOSS2
328800970121     C                   Z-ADD     CON(3)        SCOSS3
328900090918     C                   Z-ADD     COh(3)        SCOSS4
329000970121     C                   WRITE     FNSCO000
329100970121     C                   ENDIF
329200970130     C*
329300970130     C                   ENDIF
329400970121     C*
329500970121     C                   ENDSR
329600970121     C**************************************************************************
329700970121     C* SCRIVO I RECORD DI TOTALE IN FNSCO
329800970121     C**************************************************************************
329900970121     C     SCRIVI        BEGSR
330000970121     C*
330100970121    1C     WANRIT        IFEQ      'S'
330200970121     C                   CLEAR                   WREC
330300970121     C*
330400970121    2C                   DO        13            X                 3 0
330500970121    3C     SKE(X)        IFGT      0
330600970121     C     SKC(X)        ORGT      0
330700970121     C     SKN(X)        ORGT      0
330800090918     C     SKh(X)        ORGT      0
330900970121     C*
331000970121     c* se il campo e' -999 o + 999 --> e' l'OLTRE
331100970121     c*  determino di quanto oltre e'
331200970121     C                   SELECT
331300970121    4C     ORE(X)        WHENEQ    -999                                         OLTRE
331400970121     C     X             ADD       1             Y                 3 0
331500970122     C                   MOVEL     'A'           SCOOLT
331600970121     C                   Z-ADD     ORE(Y)        SCOORE
331700970121    4C     ORE(X)        WHENEQ    999                                          OLTRE
331800970121     C     X             SUB       1             Y                 3 0
331900970121     C                   MOVEL     'O'           SCOOLT
332000970121     C                   Z-ADD     ORE(Y)        SCOORE
332100970121     C                   OTHER
332200970122     C                   MOVEL     'D'           SCOOLT
332300970122     C                   Z-ADD     ORE(X)        SCOORE
332400970121    4C                   ENDSL
332500970121     C*
332600970121     C                   MOVEL     WGBO          SCOGBO
332700970121     C                   Z-ADD     SKE(X)        SCOSS1
332800970121     C                   Z-ADD     SKC(X)        SCOSS2
332900970121     C                   Z-ADD     SKN(X)        SCOSS3
333000090918     C                   Z-ADD     SKh(X)        SCOSS4
333100970121     C                   WRITE     FNSCO000
333200970121     C                   CLEAR                   SCOOLT
333300970121     C                   MOVEL     'S'           WREC              1
333400970121    3C                   ENDIF
333500970121    2C                   ENDDO
333600970121     c**
333700970121     C* SCRIVO ANCHE IL RECORD DI TOTALE se richiesto
333800970121    2C     WTOTLI        IFNE      'N'
333900970121     c* solo se ho gia' scritto almeno un record di affidabilita'
334000970121     C     WREC          ANDEQ     'S'
334100970121     C                   CLEAR                   SCOORE
334200970121     C* l'ordine  di stampa del record di totale e' sempre 1 in meno
334300970121     C*  del dettaglio suddiviso per affidabilita'
334400970121     C                   SUB       1             SCOORD
334500970121     C* il livello della stampa del totale e' spesso diverso dal livell
334600970121     C*  del relativo dettaglio affidabilita', ecco perche' c'e' un
334700970121     C*  campo a parte che viene passato in questa rourine
334800970121     C                   MOVEL     WTOTLI        SCOLIV
334900970121     C* per identificare il record di totale metto un "T" in SCOOLT
335000970121     C                   MOVEL     'T'           SCOOLT
335100970121     C                   XFOOT     SKE           SCOSS1
335200970121     C                   XFOOT     SKC           SCOSS2
335300970121     C                   XFOOT     SKN           SCOSS3
335400090918     C                   XFOOT     SKh           SCOSS4
335500970121     C                   WRITE     FNSCO000
335600970121     C                   CLEAR                   SCOOLT
335700970121     C                   ENDIF
335800090918     C* SOLO 1 RECORD se non c'e' suddivisione di affidabilita'
335900970121   X1C                   ELSE
336000970121    2C     SKE(1)        IFGT      0
336100970121     C     SKC(1)        ORGT      0
336200970121     C     SKN(1)        ORGT      0
336300090918     C     SKh(1)        ORGT      0
336400970121     C*
336500970121     C                   CLEAR                   SCOORE
336600970121     C                   MOVEL     WGBO          SCOGBO
336700970121     C                   Z-ADD     SKE(1)        SCOSS1
336800970121     C                   Z-ADD     SKC(1)        SCOSS2
336900970121     C                   Z-ADD     SKN(1)        SCOSS3
337000090918     C                   Z-ADD     SKh(1)        SCOSS4
337100970121     C                   WRITE     FNSCO000
337200970121     C                   CLEAR                   SCOOLT
337300970121    2C                   ENDIF
337400970121    1C                   ENDIF
337500970121     C*
337600050706     C                   ENDSR
337700050706     C**************************************************************************
337800050706     C* Per bolla locale desumo la data dell'arrivo bolla
337900050706     C**************************************************************************
338000050706     c     BOLLALOC      begsr
338100050706     c     kfvv8         SETGT     FNFVV08L
338200050706     c     kfvv81        READe     FNFVV08L
338300070420    6C                   IF        not %eoF(FNFVV08L) AND FVVDFV>=kdataloc
338400050706    7C                   IF        FVVFCF=' '  or wtipolancio='SA'
338500050706     C                   EVAL      Wartdam=FVVDFV
338600050706    7C                   ENDIF
338700050706     C*
338800050706   X6C                   ELSE
338900070420     C* SE NON TROVATO LE STAMPO SEMPRE CON DATA  + 1
339000070420     C                   Z-ADD     kdataloc      G02INV
339100050706     C                   MOVEL     '3'           G02ERR
339200050706     C                   CALL      'XSRDA8'
339300050706     C                   PARM                    WLBDAT
339400050706     C*
339500050706     C     G02TGI        ADD       1             GIOTGI
339600050706     C                   CALL      'XSRGI8'
339700050706     C                   PARM                    WGIDAT
339800050706     C*
339900050706     C                   Z-ADD     GIOINV        WARTDAM
340000050706    6C                   ENDIF
340100050706     c                   ENDSR
340200060314     C**************************************************************************
340300060314     C* Verifico se giorno festivo
340400060314     C**************************************************************************
340500060314     c     CTRFESTA      begsr
340600060314     c                   clear                   ktfp
340700060314     c                   movel     ds1lna        ktfa
340800060314     C     KCLN          CHAIN     AZCLN01L                           34
340900060314     C     *IN34         IFEQ      *OFF
341000060314     c
341100060314     C     MAT(KGG)      IFEQ      'F'
341200060314     C     POM(KGG)      ANDEQ     'F'
341300060314     c                   eval      wfest='S'
341400060314     c                   else
341500060314     c                   eval      wfest=' '
341600060314     c                   endif
341700060314     c
341800060314     c                   endif
341900060314     c                   ENDSR
342000950123     C**************************************************************************
342100950123     O*  ?  S T A T I S T I C A    C O N S E G N E  ?
342200920605     OPRTF198   E            TESTA            02
342300920605     O                       TE(1)               66
342400940324     O                 N02   TE(2)              132
342500940324     O                  02   TE(3)              132
342600920605     O                       TE(4)              198
342700900523     O                       RSUT                20
342800970226     O               15                          47 '- RISTAMPA -'
342900970207     O                       DS1GDA             119 '  /  /    '
343000970207     O                  02   DS1GAL             136 '  /  /    '
343100950123     O                       WDATE              180 '  /  /    '
343200920605     O                       PAGE          Z    198
343300920605     O          E            TESTA       1
343400950123     O                       KNSIF               10
343500950123     O                       KNMUS               22
343600950123     O                       UTIME              179 '  :  :  '
343700940208     O          E            TESTA            04
343800920605     O                       TE(6)               66
343900920605     O                       TE(6)              132
344000920605     O                       TE(6)              198
344100090918     o* Senza tipo servizio H
344200090918     O          E   n25      TESTA            05
344300920605     O                       TE(7)               66
344400920605     O                       TE(10)             132
344500930908     O                       TE(14)             198
344600090604     o                       wdtspc              62
344700090604     o                       wdtspe              70
344800090604     o                       wdtspc             106
344900090604     o                       wdtspe             114
345000090604     o                       wdtspc             145
345100090604     o                       wdtspe             154
345200090604     o                       wdtspc             178
345300090604     o                       wdtspe             187
345400090918     O          E   n25      TESTA            06
345500920605     O                       TE(8)               66
345600920608     O                       TE(11)             132
345700930908     O                       TE(15)             198
345800970225     O                       DS1LNA              34
345900940323     O                       DESLNA              54
346000090918     O          E   n25      TESTA            07
346100940323     O                       TE(5)               66
346200930812     O                       TE(12)             132
346300930908     O                       TE(16)             198
346400090604     o                       wdtspd              62
346500090604     o                       wdtspd             106
346600090604     o                       wdtspd             145
346700090604     o                       wdtspd             178
346800090918     o* con tipo servizio H
346900090918     O          E    25      TESTA            05
347000090918     O                       TE(7)               66
347100090918     O                       TE(41)             132
347200090918     O                       TE(45)             198
347300090918     o                       wdtspc              62
347400090918     o                       wdtspe              70
347500090918     o                       wdtsph              78
347600100114     o                       wdtspc             112
347700100114     o                       wdtspe             121
347800100114     o                       wdtsph             130
347900100114     o                       wdtspc             157
348000100114     o                       wdtspe             166
348100100114     o                       wdtsph             175
348200090918     O          E    25      TESTA            06
348300090918     O                       TE(8)               66
348400090918     O                       TE(42)             132
348500090918     O                       TE(46)             198
348600090918     O                       DS1LNA              34
348700090918     O                       DESLNA              54
348800090918     O          E    25      TESTA            07
348900090918     O                       TE(5)               66
349000090918     O                       TE(43)             132
349100090918     O                       TE(47)             198
349200090918     o                       wdtspd              62
349300100114     o                       wdtspd             112
349400100114     o                       wdtspd             157
349500940208     O          E            TESTA            08
349600920608     O                       TE(6)               66
349700920608     O                       TE(6)              132
349800920608     O                       TE(6)              198
349900950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
350000930908     O* 1 INTESTAZIONE
350100090918     O          E   n25      INTE1       2
350200930908     O                       RIGA                66
350300930908     O                       TE(36)             132
350400930908     O                       TE(37)             198
350500090918     O                       GENNOR        Z    106
350600940203     O                                          107 '+'
350700940203     O                       GENESP        Z    115
350800940203     O                                          116 '='
350900940203     O                       GENTOT        Z    124
351000940324     O                 N02   CONP1              130 '0 ,  '
351100940324     O                 N02   CONP2              136 '0 ,  '
351200930909     O*
351300090918     O          E   n25      INTE1       0  0
351400930909     O                       RIGA                66
351500930909     O                       TE(36)             132
351600930909     O                       TE(37)             198
351700940203     O                       GENNOR        Z    106
351800940203     O                                          107 '+'
351900940203     O                       GENESP        Z    115
352000940203     O                                          116 '='
352100940203     O                       GENTOT        Z    124
352200940324     O                 N02   CONP1              130 '0 ,  '
352300940324     O                 N02   CONP2              136 '0 ,  '
352400090918     c* con tipo servizio H
352500090918     O          E    25      INTE1       2
352600090918     O                       RIGA                66
352700090918     O                       TE(49)             132
352800090918     O                       TE(50)             198
352900100114     O                       GENNOR        Z    112
353000100114     O                                         +  0 '+'
353100090918     O                       GENESP        Z   +  0
353200090918     O                                         +  0 '+'
353300090918     O                       GENh10        Z   +  0
353400090918     O                                         +  0 '='
353500100114     O                       GENTOT        Z   +  1
353600090918     O                 N02   CONP2             +  1 '0 ,  '
353700090918     O*
353800090918     O          E    25      INTE1       0  0
353900090918     O                       RIGA                66
354000090918     O                       TE(49)             132
354100090918     O                       TE(50)             198
354200100114     O                       GENNOR        Z    112
354300100114     O                                         +  0 '+'
354400090918     O                       GENESP        Z   +  0
354500090918     O                                         +  0 '+'
354600090918     O                       GENh10        Z   +  0
354700090918     O                                         +  0 '='
354800100114     O                       GENTOT        Z   +  1
354900090918     O                 N02   CONP2             +  1 '0 ,  '
355000950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
355100090918     O* 2 INTESTAZIONE
355200090918     O          E   n25      INTE2       2
355300930908     O                       RIGA                66
355400930908     O                       TE(36)             132
355500930908     O                       TE(37)             198
355600940203     O                       GENNOR        Z    145
355700940203     O                                          146 '+'
355800940203     O                       GENESP        Z    154
355900940203     O                                          155 '='
356000940203     O                       GENTOT        Z    163
356100940203     O                       CONP2              169 '0 ,  '
356200090918     O          E   n25      INTE2       0  0
356300930909     O                       RIGA                66
356400930909     O                       TE(36)             132
356500930909     O                       TE(37)             198
356600940203     O                       GENNOR        Z    145
356700090918     O                                          146 '+'
356800940203     O                       GENESP        Z    154
356900940203     O                                          155 '='
357000940203     O                       GENTOT        Z    163
357100940203     O                       CONP2              169 '0 ,  '
357200090918     o* con tipo servizio H
357300090918     O          E    25      INTE2       2
357400090918     O                       RIGA                66
357500090918     O                       TE(49)             132
357600090918     O                       TE(50)             198
357700100114     O                       GENNOR        Z    157
357800090918     O                                         +  0 '+'
357900090918     O                       GENESP        Z   +  0
358000090918     O                                         +  0 '+'
358100090918     O                       GENh10        Z   +  0
358200090918     O                                         +  0 '='
358300090918     O                       GENTOT        Z   +  0
358400100114     O              n26      CONP2              190 '0 ,  '
358500090918     O          E    25      INTE2       0  0
358600090918     O                       RIGA                66
358700090918     O                       TE(49)             132
358800090918     O                       TE(50)             198
358900100114     O                       GENNOR        Z    157
359000090918     O                                         +  0 '+'
359100090918     O                       GENESP        Z   +  0
359200090918     O                                         +  0 '+'
359300090918     O                       GENh10        Z   +  0
359400090918     O                                         +  0 '='
359500090918     O                       GENTOT        Z   +  0
359600100114     O              n26      CONP2              190 '0 ,  '
359700950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
359800090918     O* 3 INTESTAZIONE  solo senza tipo servizio H
359900940118     O          E            INTE3       2
360000930909     O                       RIGA                66
360100930909     O                       TE(36)             132
360200930909     O                       TE(37)             198
360300940203     O                       GENNOR        Z    178
360400940203     O                                          179 '+'
360500940203     O                       GENESP        Z    187
360600940203     O                                          188 '='
360700940203     O                       GENTOT        Z    196
360800930909     O          E            INTE3       0  0
360900930909     O                       RIGA                66
361000930909     O                       TE(36)             132
361100930909     O                       TE(37)             198
361200940203     O                       GENNOR        Z    178
361300940203     O                                          179 '+'
361400940203     O                       GENESP        Z    187
361500940203     O                                          188 '='
361600940203     O                       GENTOT        Z    196
361700950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
361800930908     O* 1 DETTAGLIO
361900090918     O          E   n25      DET1        1
362000930812     O                       RIGA                66
362100930908     O                       TE(36)             132
362200930908     O                       TE(37)             198
362300930908     O                       ORE(Y)        Z     48
362400970121     O                       PNORM         Z     62
362500930908     O                                           63 '+'
362600970121     O                       PESPR         Z     70
362700930908     O                                           71 '='
362800970121     O                       PTOTAL        Z     79
362900940526     O              N03      CONP1               85 '0 ,  '
363000940526     O              N04N02   CONP2               91 '0 ,  '
363100940324     O                 N02   CONP3               97 '0 ,  '
363200090918     o* con tipo servizio H
363300090918     O          E    25      DET1        1
363400090918     O                       RIGA                66
363500090918     O                       TE(49)             132
363600090918     O                       TE(50)             198
363700090918     O                       ORE(Y)        Z     48
363800090918     O                       PNORM         Z     62
363900090918     O                                         +  0 '+'
364000090918     O                       PESPR         Z   +  0
364100090918     O                                         +  0 '+'
364200090918     O                       ph1030        Z   +  0
364300090918     O                                         +  0 '='
364400100114     O                       PTOTAL        Z   +  0
364500100114     O              N03      CONP1               94 '0 ,  '
364600100114     O                 N02   CONP3              101 '0 ,  '
364700950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
364800930908     O* 2 DETTAGLIO
364900940203     O          E            DET4        1
365000940203     O                                              ' '
365100950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
365200090918     O          E      n25   DET2        1
365300930908     O                       RIGA                66
365400930908     O                       TE(36)             132
365500930908     O                       TE(37)             198
365600930908     O                       GENNOR        Z     62
365700930908     O                       GENESP        Z     70
365800940203     O                                           55 ''''
365900940203     O                                           63 '+'
366000930908     O                                           71 '='
366100930908     O                       GENTOT        Z     79
366200940329     O              N01   N03CONP1               85 '0 ,  '
366300940324     O              N01N02   CONP2               91 '0 ,  '
366400940324     O                 N02   CONP3               97 '0 ,  '
366500090918     O          E   N01n25   DET2        0  0
366600940207     O                       RIGA                66
366700940207     O                       GENNOR        Z     62
366800940207     O                       GENESP        Z     70
366900940207     O                                           63 '+'
367000940207     O                                           71 '='
367100940207     O                       GENTOT        Z     79
367200090918     O
367300940324     O                 N02   CONP2               91 '0 ,  '
367400940324     O                 N02   CONP3               97 '0 ,  '
367500090918     o* con tipo servizio H
367600090918     O          E       25   DET2        1
367700090918     O                       RIGA                66
367800090918     O                       TE(49)             132
367900090918     O                       TE(50)             198
368000090918     O                       GENNOR        Z     62
368100090918     O                                           55 ''''
368200090918     O                       GENESP        Z     70
368300090918     O                       GENh10        Z     78
368400090918     O                       GENTOT        Z     87
368500090918     O                                           63 '+'
368600090918     O                                           71 '+'
368700090918     O                                           79 '='
368800100114     O              N01   N03CONP1               94 '0 ,  '
368900100114     O                 N02   CONP3              101 '0 ,  '
369000090918     O          E   N01 25   DET2        0  0
369100090918     O                       RIGA                66
369200090918     O                       GENNOR        Z     62
369300090918     O                       GENESP        Z     70
369400090918     O                       GENh10        Z     78
369500090918     O                       GENTOT        Z     87
369600090918     O                                           63 '+'
369700090918     O                                           71 '+'
369800090918     O                                           79 '='
369900100114     O              N01   N03CONP1               94 '0 ,  '
370000100114     O                 N02   CONP3              101 '0 ,  '
370100950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
370200920609     O          E            DET3        2
370300920608     O                       TE(6)               66
370400920608     O                       TE(6)              132
370500920608     O                       TE(6)              198
370600950123     C**************************************************************************
370700950124     O*  ?  L I S T A  ?
370800920706     OQSYSPRT   E            SPED1          1 02
370900950124     O                                         +  3 'ANNO'
371000920706     O                                         +  2 'LNP'
371100920706     O                                         +  2 'SER'
371200920706     O                                         +  2 'SPEDIZ'
371300950124     O                                         +  1 'DATA SPED'
371400920706     O                                         +  2 'COLLI'
371500950124     O                                         +  3 'DATA ARR'
371600950124     O                                         +  2 'ABIL'
371700920706     O                                         +  1 'NUM DIST'
371800920706     O                                         +  2 'DAT DIST'
371900950124     O                                         +  2 'ULT CON'
372000950124     O***************                  +  1 'L.AVVISO'
372100920706     O                                         +  1 'CONSEGNA'
372200950124     O                                         +  3 'CON PARZ'
372300950124     O                                         +  2 'BLOCCO SP'
372400950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
372500920706     OQSYSPRT   E            SPED        1
372600940208     O                       STAFLG
372700940208     O                       ARBAAS            +  2
372800940208     O                       ARBLNP        Z   +  2
372900920706     O                       ARBNRS        Z   +  2
373000920706     O                       ARBNSP        Z   +  2
373100920706     O                       ARBMGS            +  2 '  /  '
373200920706     O                       ARBTSP            +  2
373300920706     O                       ARBNCL        Z   +  2
373400950124     O                       ARBDAM            +  2 '    /  /  '
373500950124     O                       ARBAMA            +  3
373600920706     O                       ARBNDC        Z   +  3
373700950124     O                       ARBDDC            +  2 '    /  /  '
373800950124     O                       ARBTMC            +  3
373900950124     O***************          ARBDLA  +  4 '  /  /  '
374000950124     O                       ARBDCM            +  3 '    /  /  '
374100950124     O                       ARBDCP            +  2 '    /  /  '
374200950123     O                       ARBFBC            +  3
374300950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
374400940203     O          E            SPEDT       1
374500940207     O                                              'CONSEGNATE'
374600940203     O                       STAC          Z   +  1
374700940208     O                                         +  3 'CONSEGNE PARZ'
374800940208     O                       STAP          Z   +  1
374900940208     O                                         +  3 'RISERVA CONS'
375000940208     O                       STAR          Z   +  1
375100940207     O                                         +  4 'IN DISTINTA'
375200940203     O                       STAI          Z   +  1
375300940207     O                                         +  4 'NON CONSEGNATE'
375400940203     O                       STAN          Z   +  1
375500940208     O          E            SPEDT       1
375600940208     O                                              'GIACENZE'
375700940208     O                       STAG          Z   +  1
375800950124     O                                         +  4 'L.AVVISI'
375900950124     O                       STAA          Z   +  1
376000940208     O                                         +  4 'CHIUSE X TURNO'
376100940208     O                       STAT          Z   +  1
376200940208     O                                         +  4 'DA CONSEGNARE'
376300940208     O                       STAD          Z   +  1
376400950123     O**************************************************************************
376500950123     O*  ?  ELENCO  SPEDIZIONI  NON  IN  CONSEGNA  ?
376600940601     OPRTF198X  E            TESELE           02
376700040608     O              n08      ELE(2)             132
376800040608     O              n08      ELE(3)             198
376900040608     O               08      ELE(14)            132
377000040608     O               08      ELE(15)            198
377100940530     O                       RSUT                20
377200950123     O                       WDATE              180 '  /  /    '
377300950123     O                       PAGE1         Z    198
377400940530     O          E            TESELE           03
377500950123     O                       KNSIF               10
377600950123     O                       KNMUS               22
377700940601     O                       ELE(4)             132
377800100222     o                       p0dpart             80
377900970207     O                       DS1GDA             104 '  /  /    '
378000950123     O                       UTIME              179 '  :  :  '
378100940530     O          E            TESELE           05
378200940601     O                       ELE(5)              66
378300940601     O                       ELE(8)             132
378400940601     O                       ELE(11)            198
378500940530     O          E            TESELE         1 06
378600940601     O                       ELE(6)              66
378700940601     O                       ELE(9)             132
378800940601     O                       ELE(12)            198
378900950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
379000940530     O          E            DETELE         1
379100950127     O                       P0CAAS
379200940530     O                       ARBLNP        Z   +  1
379300940530     O                       ARBNRS        Z   +  1
379400940530     O                       ARBNSP        Z   +  1
379500940530     O                       P0CMGS            +  1 '  /  '
379600940531     O                       ARBTSP            +  1
379700940601     O                       ARBLNA        Z   +  1
379800940601     O                       ARBZNC        Z   +  2
379900991130     O                       P0CRSD            +  1
380000950125     O                       P0CIND            +  1
380100940601     O                       ARBCAD            +  1
380200950127     O                       P0CLOD            +  0
380300940601     O                       ARBPRD            +  1
380400950127     O                       ARBNZD            +  1
380500940601     O                       ARBGC1            +  1
380600940601     O                       ARBGC2            +  1
380700950123     O                       ARBTC1            +  1
380800940601     O                                         +  0 '-'
380900950127     O                       P0DTC1            +  0
381000950123     O                       ARBNCL        Z   +  0
381100950127     O                       ARBPKF        4   +  0
381200991130     O                       ARBCBO            +  1
381300940530     O                                         +  0 '-'
381400950127     O                       P0DCBO            +  0
381500950125     O                       AR9CAS        4   +  0
381600991130     O                       AR9VCA            +  0
381700991130     O                       COMIFT            +  1
381800991130     O                       comdiv            +  0
381900991130     O                       ARBTMC            +  1
382000051128     O                       p0cTCR            +  1
382100940531     O                       P0CDCR        Z   +  1
382200940530     O                       ARBHCR            +  1 '  :  '
382300100222     O                       ARBgma            +  1
382400100222     O                       NUM3          M   +  1
382500950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
382600950127     O          E            DE2ELE         1
382700990329     O               07                             'C.A.'
382800990329     O               07      DCTFIL               8
382900990329     O               07      DCTNCA        Z     15
383000990329     O               07                          19 'del'
383100990329     O               07      STADCA              28 '  /  /  '
383200990329     O               07    06                    35 'chiusa'
383300990329     O               07    06STACHC              44 '  /  /  '
383400990329     O               07    06DCTCCH            +  1
383500990329     O               07    06                  +  0 '-'
383600990329     O               07    06STACCH            +  0
383700991130     O                       ARBTC2             114
383800991130     O                                          115 '-'
383900991130     O                       P0DTC2             125
384000991130     O                       P0CNO1             161
384100991130     O                       P0CNO2             197
384200990329     O          E            DETCA          1
384300990329     O                                              'C.A.'
384400990329     O                       DCTFIL               8
384500990329     O                       DCTNCA        Z     15
384600990329     O                                           19 'del'
384700990329     O                       STADCA              28 '  /  /  '
384800990329     O                     06                    35 'chiusa'
384900990329     O                     06STACHC              44 '  /  /  '
385000990329     O                     06DCTCCH            +  1
385100990329     O                     06                  +  0 '-'
385200990329     O                     06STACCH            +  0
385300060905     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
385400060905      * spedizione in distinta
385500060905     O          E    12      DETDIS         1
385600060905     O                       wstadis            197
385700000703     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
385800060905      * C/ASSEGNO NON ABILITATO
385900000703     O          E            DETCAA         1
386000110923     O              n18 28                      159 'Non Abilitato'
386100110923     O               18 28                       53 'Abilitazioni Bolla:'
386200110923     O               18 28   olrg6vssin       +   1
386300110923     O               19                       +   4 'Perfect Order:'
386400110927     O               19      stPorder         +   1
386500050805     O          E            FINES       2
386600050805     o                                          108 '*** FINE STAMPA ***'
386700950123     O**************************************************************************
386800950125**
386900090918XXXXXXXXXXXXXXXXXXXX                                      S T A T    1
387000950124I S T I C A    C O N S E G N E    D E L                              2
387100950124I S T I C A    C O N S E G N E    D A L                A L           3
387200950124                      FNLR78R                            PAG. XXXX   4
387300950123                                                      '  DISTR       5
387400930907------------------------------------------------------------------   6
387500090918                                                      '  yyyyy   y   7
387600950123L I N E A   D I   A R R I V O: XXX-XXXXXXXXXXXXXXXXXXX'    +         8
387700090917                                                      '1234567+123   9
387800090917SPR   TOTALE ' % SU' % SU' % SU'   CORR    ESPR    TOTALE ' % SU'   10
387900090917             'CONS.'TOTAL'CONSE'     +                    'TOTAL'C  11
388000090917             'PERIO'IN CO' GNAB'   DISTR                  'IN CO'   12
3881000909174567=12345678'XX,XX'XX,XX'XX,XX'XXXXXXXX+Xxxxxxxx=XXXXXXXX'XX,XX'X  13
388200090918% SU'   yyyyy    yyyyy  TOTALE ' % SU'   yyyyy    yyyyy  TOTALE     14
388300090917ONSE'     +                    'CONSE'     +                        15
388400090917GNAB'   DISTR                  ' GNAB'   DISTR                      16
388500090917X,XX'Xxxxxxxx+xXxxxxxx=xxxxxxxx'XX,XX'xXxxxxxx+xxxXxxxx=xxxxxxxx    17
388600090917SPEDIZIONI           C O N S E G N A T E:             '             18
388700940203                           In Anticipo oltre xxx Ore  '             19
388800940203                           In Anticipo di    xxx Ore  '             20
388900940323                           In CONFORMITA'             '              1
389000940203                           In Ritardo  di    xxx Ore  '              2
389100940203                           In Ritardo  oltre xxx Ore  '              3
389200970130SPEDIZIONI   N O N   C O N S E G N A T E:             '             24
389300940203              di cui  "GIACENZE APERTE"               '             25
389400940203              di cui  "LASCIATI AVVISI"               '             26
389500940203              di cui  "NON  FATTE"                    '             27
389600940203              di cui  "CHIUSE PER TURNO"              '             28
389700970130SPEDIZIONI   ANCORA  I N   D I S T I N T A:           '             29
389800970130TOTALE SPED.         I N   C O N S E G N A:           '             30
389900970130SPEDIZIONI   N O N   I N   C O N S E G N A:           '             31
390000970130TOTALE SPED. C O N S E G N A B I L I:                 '             32
390100940323                                       In Anticipo    '             33
390200940323                                       In CONFORMITA' '             34
390300940323                                       In Ritardo     '             35
390400940203             '     '     '     '                          '     '   36
390500940203    '                          '     '                              37
390600940208              di cui  "CONSEGNE PARZIALI"             '             38
390700940208              di cui  "RISERVE  CONSEGNE"             '             39
390800090917                                                      '1234567+123  40
390900100114yyyy   yyyyy  TOTALE ' % SU ' % SU '     yyyyy    yyyyy    yyyyy
391000100114                     ' CONS.' CONSE'       +
391100100114                     'PERIOD' GNAB.'     DISTR
3912001001144567+1234567=12345678' xx,xx' XX,XX'  XXXXXXXX+Xxxxxxxx+12345678=
391300100114 TOTALE ' % SU'     yyyyy    yyyyy    yyyyy  TOTALE ' % SU'         45
391400100114        'CONSE'       +                             'CONSE'         46
391500100114        ' GNAB'     DISTR                           ' GNAB'         47
39160010011412345678'XX,XX'  12345678+12345678+12345678=12345678'XX,XX'         48
391700100114                     '      '      '                                49
391800100114        '     '                                     '     '         50
391900940324** Comandi    567890123456789012345678901234567890123456789012345678901234567890
392000940324OVRPRTF FILE(PRTF198)  USRDTA('Stat.Cons.')
392100940530** Comandi    567890123456789012345678901234567890123456789012345678901234567890
392200940601OVRPRTF FILE(PRTF198X) USRDTA('Stat.Cons.')
392300940530**
392400090918XXXXXXXXXXXXXXXXXXXX                                                 1
392500940601         **  ELENCO  SPEDIZIONI  NON  IN  CONSEGNA  **               2
392600950125                       FNLR78R                           PAG. XXXX   3
392700950125                        AL  XX/XX/XXXX                               4
392800991130   S P E D I Z I O N E     ARRIVO      D    E    S    T    I   N     5
392900991130                          LIN ZON RAGIONE SOCIALE INDIRIZZO          6
393000950125                                                                     7
393100991130  A    T    A    R    I    O              GG   CONSEG COLLI    PES   8
393200991130         C.A.P    LOCALITA'      PRO NAZ CHIUS PARTIC                9
393300950125                                                                    10
393400100222O CD.BOLLA     C/ASSEGNO       TOTALE   MAN CONSEGNA   PARTIC ORE   11
393500100222                              FATTURA   CON RICHIESTA    CONS.RIT.  12
393600950125                                                                    13
393700040608      **  ELENCO SPEDIZIONI NON IN CONSEGNA: considera spunte part  14
393800040608enza  **                  FNLR78R                        PAG.       15

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
001100090309     FFiART01L  IF   E           K DISK
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
002900990329     FFNDCT01L  IF   E           K DISK
003000990329     F                                     RENAME(FNDCT000:FNDCT001)
003100990326     FFNDCD02L  IF   E           K DISK
003200940324     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
003300940324     F                                     USROPN
003400940210     F                                     FORMLEN(66)
003500940210     F                                     FORMOFL(63)
003600920706     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOE)
003700940210     F                                     FORMLEN(66)
003800940210     F                                     FORMOFL(65)
003900940601     FPRTF198X  O    F  198        PRINTER OFLIND(*INOD)
004000940601     F                                     USROPN
004100940601     F                                     FORMLEN(66)
004200940601     F                                     FORMOFL(64)
004300940530     D TE              S             66    DIM(39) CTDATA PERRCD(1)             RIGHE STATISTICA
004400940530     D CMD             S             43    DIM(1) CTDATA PERRCD(1)              OVR PRTF198
004500940601     D CM2             S             43    DIM(1) CTDATA PERRCD(1)              OVR PRTF198X
004600040608     D ELE             S             66    DIM(15) CTDATA PERRCD(1)             RIGHE ELENCO
004700940530     D*
004800950123     D REC             S              2    DIM(30)                              BOLLE RECUP
004900940210     D*
005000930907     D ORE             S              3  0 DIM(13)                              ORE ANT/RIT
005100930908     D*
005200970121     D ESP             S              9  0 DIM(13)                              SKI COM ESPR
005300970121     D NOR             S              9  0 DIM(13)                              SKI COM NORM
005400970121     D TOT             S              9  0 DIM(13)                              SKI COM TOT
005500970121     D*
005600970121     D SKE             S              9  0 DIM(13)                              SKI COM ESPR
005700970121     D SKC             S              9  0 DIM(13)                              SKI COM CORR
005800970121     D SKN             S              9  0 DIM(13)                              SKI COM DIST
005900930907     D*
006000970121     D CNE             S              9  0 DIM(13)                              CONS.TE ESPR
006100970121     D CNC             S              9  0 DIM(13)                              CONS.TE CORR
006200970121     D CNN             S              9  0 DIM(13)                              CONS.TE DIST
006300930907     D*
006400970121     D ICE             S              9  0 DIM(13)                              IN CONS ESPR
006500970121     D ICC             S              9  0 DIM(13)                              IN CONS CORR
006600970121     D ICN             S              9  0 DIM(13)                              IN CONS DIST
006700930908     D*
006800970121     D NCE             S              9  0 DIM(13)                              NO IN C ESPR
006900970121     D NCC             S              9  0 DIM(13)                              NO IN C DIST
007000970121     D NCN             S              9  0 DIM(13)                              NO IN C NORM
007100930907     D*
007200970121     D GAE             S              9  0 DIM(13)                              GIAC AP ESPR
007300970121     D GAC             S              9  0 DIM(13)                              GIAC AP CORR
007400970121     D GAN             S              9  0 DIM(13)                              GIAC AP NORM
007500930907     D*
007600970121     D LAE             S              9  0 DIM(13)                              LASC AV ESPR
007700970121     D LAC             S              9  0 DIM(13)                              LASC AV CORR
007800970121     D LAN             S              9  0 DIM(13)                              LASC AV NORM
007900930907     D*
008000970121     D NFE             S              9  0 DIM(13)                              NON FAT ESPR
008100970121     D NFC             S              9  0 DIM(13)                              NON FAT CORR
008200970121     D NFN             S              9  0 DIM(13)                              NON FAT NORM
008300930907     D*
008400970121     D CTE             S              9  0 DIM(13)                              CHIU TU ESPR
008500970121     D CTC             S              9  0 DIM(13)                              CHIU TU CORR
008600970121     D CTN             S              9  0 DIM(13)                              CHIU TU NORM
008700930907     D*
008800970121     D IDE             S              9  0 DIM(13)                              IN DIST ESPR
008900970121     D IDC             S              9  0 DIM(13)                              IN DIST CORR
009000970121     D IDN             S              9  0 DIM(13)                              IN DIST NORM
009100940208     D*
009200970121     D RSE             S              9  0 DIM(1)                               RIS CON ESPR
009300970121     D RSC             S              9  0 DIM(1)                               RIS CON CORR
009400970121     D RSN             S              9  0 DIM(1)                               RIS CON NORM
009500940208     D*
009600970121     D PRE             S              9  0 DIM(1)                               CON PAR ESPR
009700970121     D PRC             S              9  0 DIM(1)                               CON PAR CORR
009800970121     D PRN             S              9  0 DIM(1)                               CON PAR NORM
009900930907     D*
010000970121     D COE             S              9  0 DIM(3)                               CONS.LI ESPR
010100970121     D COC             S              9  0 DIM(3)                               CONS.LI CORR
010200970121     D CON             S              9  0 DIM(3)                               CONS.LI NORM
010300950314RM*  D*
010400950314RM*  D L6              S              3  0 DIM(30)                              FIL GEST.ARRIVI
010500000703     D L1              S              3  0 DIM(30)                              FIL GESTITE
010600930907     D*
010700040908     D knpg            S                   LIKE(fvvnpg) inz(2)
010800040908     D Wtstdec         S                   LIKE(§CVfdc)
010900040527     D Wartdam         S                   LIKE(artdam)
011000060314     d Dataiso         s               d   datfmt(*iso)
011100060314     d Dataisofine     s               d   datfmt(*iso)
011200050706     D wtipolancio     s              2
011300050706     D W013A           s             13
011400991130     D IMPeditato      s             13
011500991130     D w0130           s             13  0
011600991130     D w0122           s             12  2
011700021021     d Wfil            s                   Like(StaFil)
011800040812     d wesca           s              1
011900060314     d wfest           s              1
012000970130     D*
012100961112     D ARBDS         E DS                  EXTNAME(FNARB00F)
012200961112     D*
012300961112     D WSAVDS          DS           700
012400961112     D*
012500970207     D                 DS
012600970207     D  KDATA                  1      8  0
012700970207     D  KAAA                   1      4  0
012800970207     D  KMMM                   5      6  0
012900970207     D  KGG                    7      8  0
013000940530     D                 DS
013100950124     D  DATA                   1      8  0
013200950124     D  AA                     3      4  0
013300950124     D  MM                     5      6  0
013400950124     D  GG                     7      8  0
013500950124     D  MMGG                   5      8  0
013600970130     D CLNMAT          DS
013700970130     D  MAT                    1     31
013800970130     D                                     DIM(31)                              GG CALEND MATT
013900970130     D CLNPOM          DS
014000970130     D  POM                    1     31
014100970130     D                                     DIM(31)                              GG CALEND POMEI
014200930812     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
014300970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
014400920611     D DS2A          E DS
014500940210     D DS3A          E DS
014600040922     D*DS5C          E DS
014700930907     D* VARIABILI ARRIVI
014800930907     D DS7A          E DS
014900930907     D  ANT                   58     72  0
015000930907     D                                     DIM(5)                               ORE ANTICIPO
015100930907     D  RIT                   73     87  0
015200930907     D                                     DIM(5)                               ORE RITARDO
015300970124     D DS7AF           DS
015400970124     D  INUTIL                 1     67
015500970124     D  ALFDPE                68     75
015600970124     D  §7ACPE                68     75  0
015700970124     D  ALFDUE                76     83
015800970124     D  §7ACUE                76     83  0
015900950127     D WLBDAT          DS
016000950127     D  G02DAT                 1      8  0
016100950127     D  G02INV                 9     16  0
016200950127     D  G02ERR                17     17
016300950127     D  G02TGI                18     22  0
016400970124     D WGIDAT          DS
016500970124     D  GIODAT                 1      8  0
016600970124     D  GIOINV                 9     16  0
016700970124     D  GIOTGI                17     21  0
016800990329     D                 DS
016900990329     D  DCTDCA                 1      8  0
017000990329     D  DCTAAC                 1      4  0
017100990329     D  DCTMGC                 5      8  0
017200990326     D DTAD            DS
017300990326     D  §TARGR                26     26
017400990329     D DCCH            DS
017500990329     D  §CHDES                 1     50
017600990329     D  §CHIMA                53     53
017700970207     D* PASSAGGIO DATI DA FILTRO
017800970207     D DSLS01        E DS
017900970225     D* DS1RIS= S - ELABORA E STAMPA QUELLO INDICATO DA DS1FL1
018000970225     D*       = R -         RISTAMPA QUELLO INDICATO DA DS1FL1
018100970225     D*       =   - ELABORA E  IGNORA  IL  CAMPO        DS1FL1
018200970225     D* DS1FL1= S - STAMPA LA STATISTICA
018300970225     D*       = L - STAMPA LA LISTA SPEDIZIONI NON IN CONSEGNA
018400970225     D*       = E - STAMPA LA STATISTICA E LA LISTA
018500970225     D* DS1FL2= S - STAMPA LA LISTA DELLE SPEDIZIONI PREVISTE IN STATIS
018600970225     D*             PER UN CONTROLLO
018700040608     D* DS1FL3='L'- devo stampare la loista spedizioni non in consegna
018800040608     d*             anche se ds1ris=*blanks
018900040608     D* DS1FL4='E'- non si tratta di una vera ristampa ma di stampa
019000040608     D*             dopo aver elaborato i dati
019100040608     D* DS1FL5= S - considera spunta partenza
019200050706     D* DS1FL6= S - escludi supermercati e appuntamenti del giorno
019300970207     D*
019400970207     D CNCR80        E DS
019500900511     D KPJBA         E DS
019600950314RM*  D* DS PER TRUL06R - CARICAMENTO £X
019700950314RM*  D DSUL06        E DS                  EXTNAME(TRUL06DS)
019800950314RM*  D  LIN                    1     90  0
019900950314     D                                     DIM(30)                              FIL. COMODO
020000940118     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
020100990329     D DSBS02        E DS                  EXTNAME(TIBS02DS)
020200991130     D DSCV          E DS
020300000531     D OG143         E DS
020400070419     D OG148         E DS
020500040526     D FNLV53DS      E DS
020600051128     D fnlv80ds      E DS
020700070419     D FNLRG6DS      E DS
020900040812      * ds per il controllo della presenza di CA per la spedizione richiesta
021000040812     d FIDN12DS      e ds
021100040812     d  skKey                 26    305    dim(20)
021200040812     d  skAnn                306    325    dim(20)
021300040812     d  skDca                326    485  0 dim(20)
021400040812     d  skFca                486    545  0 dim(20)
021500040812     d  skDch                546    705  0 dim(20)
021600040812     d  skCch                706    745    dim(20)
021700040812
021800040812     d dsKey           ds
021900040812     d  dsaac                         4  0
022000040812     d  dsfil                         3  0
022100040812     d  dsnca                         7  0
022200040812      *
022300060905     D Cindistinta     C                   CONST('In Distinta n.xxxxx-
022400060905     D                                     xx  del xx/xx/xx')
022500040812
022600900523     C****************************************************************
022700900523     C*  RIEPILOGO INDICATORI
022800920306     C****************************************************************
022900940207     C* 01    - CONTROLLO LA STAMPA DELLE % NEL TOTALE CONSEGNABILE
023000940323     C* 02    - IMMESSA DATA AL
023100940329     C* 03    - STAMPO LA % SUL TOT.CONSEGNATO SOLO PER LE CONSEGNATE
023200040608     C* 04    -
023300040608     C* 05    - EVENTO RISERVA CONSEGNA
023400990329     C* 06-07 - PER STAMPARE LE C.A.
023500040608     C* 08    - lista non in consegna con spunte partenza
023600040608     C* 10    - SPEDIZIONE ESPRESSO
023700970226     C* 11    - SPEDIZIONE NORMALE
023800970226     C* 15    - SI TRATTA DI UNA RISTAMPA
023900970226     C* 16    - LA SPEDIZIONE E' NON CONSEGNATA
024000940329     C* 17    - LA SPEDIZIONE NON E' COMPLETAMENTE ARRIVATA
024100070419     C* 18    - GEO attiva
024200920605     C* 30    - COMODO
024300950314RM*  C* 36    - FILIALE NON GESTITA DA SECONDO LIVELLO
024400950123     C* 34    - LETTURA FNARB54L
024500000531     C* 72    - Usato con CHAIN AZORG01L x controllo se bolla ha LNP Poste
024700940117     C* 99    - STAMPA LA LISTA DI CONTROLLO
024800900523     C*****************************************************************
024900000000     C     *ENTRY        PLIST
025000000000     C                   PARM                    KPJBA
025100970207     C                   MOVEL     KPJBU         DSLS01
025200970225     C*
025300990805     C*DATA  PER CONFRONTARE CON GLI EVENTI
025400990805     C                   Z-ADD     DS1AAL        CO1DAA            8 0
025500970225     C*DATA DA SUDDIVIDERE IN AAAA MM GG
025600970207     C                   MOVEL     DS1ADA        KDATA
025700970225     C* SE DS1RIS = ' 'IGNORO IL CAMPO DS1FL1
025800970225     C     DS1RIS        IFEQ      ' '
025900970225     C                   CLEAR                   DS1FL1
026000970225     C                   ENDIF
026100040608     c*
026200040608     c* lista sped non in consegna con spunte partenza
026300040608     c                   if        ds1fl5='S'
026400040608     c                   seton                                        08
026500040608     c                   endif
026600970207     C*
026700000000     C                   Z-ADD     1             CODUT
026800900523     C                   CALL      'X§PARUT'
026900900523     C                   PARM                    UT§DSE
027000000000     C                   MOVEL     RAGUT         RSUT             20
027100920415     C                   MOVEL     REC80         CNCR80
027200000703      * CARICO L1
027300900511     C*---------------------------------------------------------------*
027400930812     C     KARB          KLIST
027500970225     C                   KFLD                    DS1LNA
027600950123     C                   KFLD                    NUM6              8 0
027700961108     C     KARB1         KLIST
027800961108     C                   KFLD                    LBLAAN
027900961108     C                   KFLD                    LBLLPN
028000961108     C                   KFLD                    LBLNRN
028100961108     C                   KFLD                    LBLNSN
028200930812     C     KARB2         KLIST
028300990805     C                   KFLD                    AR§AAS
028400930812     C                   KFLD                    ARBLNP
028500930812     C                   KFLD                    ARBNRS
028600930812     C                   KFLD                    ARBNSP
028700990805     C                   KFLD                    NUM6A
028800970417     C     KARB22        KLIST
028900990805     C                   KFLD                    AR§AAS            4 0
029000970417     C                   KFLD                    ARBLNP
029100970417     C                   KFLD                    ARBNRS
029200970417     C                   KFLD                    ARBNSP
029300990805     C                   KFLD                    NUM6A             8 0
029400970417     C                   KFLD                    NUMORA
029500940324     C     KARB3         KLIST
029600940324     C                   KFLD                    ARBAAS
029700940324     C                   KFLD                    ARBLNP
029800940324     C                   KFLD                    ARBNRS
029900940324     C                   KFLD                    ARBNSP
030000991104     C     KARB6         KLIST
030100991104     C                   KFLD                    ARBAAS
030200991104     C                   KFLD                    ARBLNP
030300991104     C                   KFLD                    ARBNRS
030400991104     C                   KFLD                    ARBNSP
030500991104     C                   KFLD                    KTRC
030600990326     C     KART          KLIST
030700990326     C                   KFLD                    ARTFLS
030800990326     C                   KFLD                    ARTLNA
030900990326     C                   KFLD                    ARTNRS
031000990326     C                   KFLD                    ARTNSC
031100950127     C     KAR41         KLIST
031200950127     C                   KFLD                    ARBAAS
031300950127     C                   KFLD                    ARBLNP
031400950127     C                   KFLD                    ARBNRS
031500950127     C                   KFLD                    ARBNSP
031600950127     C                   KFLD                    AR4TRC
031700950123     C     KARB4         KLIST
031800950123     C                   KFLD                    AR§AAS
031900950123     C                   KFLD                    ARBLNP
032000950123     C                   KFLD                    ARBNRS
032100950123     C                   KFLD                    ARBNSP
032200920611     C     KTAB          KLIST
032300920611     C                   KFLD                    CODUT
032400920611     C                   KFLD                    COD               2
032500920611     C                   KFLD                    KEY               8
032600940210     C     KTAB2         KLIST
032700940210     C                   KFLD                    CODUT
032800940210     C                   KFLD                    COD               2
032900970122     C     KSCO1         KLIST
033000000703     C************       KFLD                    KFLE
033100970121     C                   KFLD                    KLNA
033200970121     C                   KFLD                    KDCM
033300970122     C     KSCO2         KLIST
033400000703     C************       KFLD                    KFLE
033500970122     C                   KFLD                    KLNA
033600970122     C                   KFLD                    KSCO
033700970122     C                   KFLD                    KGBO
033800970122     C                   KFLD                    KDCM
033900970122     C                   KFLD                    KOLT
034000970122     C     KSCO21        KLIST
034100000703     C************       KFLD                    KFLE
034200970122     C                   KFLD                    KLNA
034300970122     C                   KFLD                    KSCO
034400970122     C                   KFLD                    KGBO
034500970122     C     KSCO22        KLIST
034600000703     C************       KFLD                    KFLE
034700970122     C                   KFLD                    KLNA
034800970122     C                   KFLD                    KSCO
034900970122     C                   KFLD                    KGBO
035000970122     C                   KFLD                    KDCM
035100970130     C     KCLN          KLIST
035200970130     C                   KFLD                    KTFP
035300970130     C                   KFLD                    KTFA
035400970130     C                   KFLD                    KAAA
035500970130     C                   KFLD                    KMMM              2 0
035600990329     C     KDCT          KLIST
035700990329     C                   KFLD                    DCTAAC
035800990329     C                   KFLD                    DCTFIL
035900990329     C                   KFLD                    DCTNCA
036000021017
036100021017     c     Kfista        Klist
036200021017     c                   Kfld                    KTRC
036300021017     c                   Kfld                    Simfel
036400021017     c                   Kfld                    Wfil
036500021017     c                   Kfld                    KDTS
036600040527     C     Kfvv          KLIST
036700040527     C                   KFLD                    D53NPA
036800040527     C                   KFLD                    D53NFA
036900040527     C                   KFLD                    D53LAI
037000040908     C     Kfvv8         KLIST
037100040908     C                   KFLD                    arbtfa
037200040908     C                   KFLD                    knpg
037300070420     C                   KFLD                    kdataloc
037400040908     C     Kfvv81        KLIST
037500040908     C                   KFLD                    arbtfa
037600040908     C                   KFLD                    knpg
037700971128     C*
037900020211     C     *LIKE         DEFINE    EVBHEV        NUMORA
038000970417     C     *LIKE         DEFINE    ARBLNA        KTFA
038100970130     C     *LIKE         DEFINE    ARBLNA        KTFP
038200970130     C     *LIKE         DEFINE    STATRC        KTRC
038300970124     C     *LIKE         DEFINE    STADTS        KDTS
038400970124     C     *LIKE         DEFINE    STANCL        KNCL
038500970124     C     *LIKE         DEFINE    SCOOLT        KOLT
038600970122     C     *LIKE         DEFINE    SCOSCO        KSCO
038700970122     C     *LIKE         DEFINE    SCOGBO        KGBO
038800970122     C     *LIKE         DEFINE    ARBDCM        KDCM
038900970121     C     *LIKE         DEFINE    ARBLNA        KLNA
039000070420     C     *LIKE         DEFINE    fvvdfv        Kdataloc
039200991130     C     *LIKE         DEFINE    AR6DIV        SAVDIV
039300991130     C     *LIKE         DEFINE    AR6DIV        comdiv
039400961112     C*
039500930812     C*---------------------------------------------------------------*
039600930812     C* 1°ASSIOMA: SE LA SPEDIZIONE E' CONSEGNATA
039700930812     C*            COME DATA DISTINTA PRENDO LA DATA CONSEGNA
039800930812     C* 2°ASSIOMA: C'E' SEMPRE UN EVENTO CREATO A FRONTE DI OGNI
039900930812     C*            MANCATA CONSEGNA
040000950123     C* 3°ASSIOMA: ESCLUDO LE BOLLE CON DATA CONSEGNA MA IN FNARB000
040100940208     C*            PERCHE' SONO CONSEGNE ANOMALE CHE ASPETTATO LA DEFI-
040200940208     C*            NIZIONE DELLA FIGLIA
040300930812     C*---------------------------------------------------------------*
040400940117     C* LISTA SPEDIZIONI
040500970207     C     DS1FL2        IFEQ      'S'
040600940117     C                   SETON                                        99
040700940117     C                   EXCEPT    SPED1
040800940117     C                   ENDIF
040900930907     C* OPERAZIONI INIZIALI
041000930910     C                   EXSR      INIZIO
041100060110     c
041200060111     C***  DS1LNA        IFLE      0
041300060111     C***                CLEAR                   DSUL06
041400060111     C**+                MOVE      '£1'          D06COD
041500060111     C***                MOVEL     SIMFEL        D06KEY
041600060111     C***                MOVEL     DSUL06        KPJBU
041700060111     C***                CALL      'TRUL06R'
041800060111     C***                PARM                    KPJBA
041900060111     C***                MOVEL     KPJBU         DSUL06
042000060111     C***                MOVEA     LIN           L1                             TUTTE
042100060110      *                  --------------------------
042200060111     C***                ELSE
042300060110     C***                Z-ADD     DS1LNA        L1(1)                          UNA A VIDEO
042400060110     C                   movea     l6            L1                             UNA A VIDEO
042500060110      *                  -----------------------------
042600060111     C***                ENDIF
042700970122     C*
042800940329     C     *IN98         IFEQ      *OFF
042900920428     C** ELABORAZIONE
043000920430     C*
043100000703    1C     1             DO        30            WX                3 0
043200000703    2C     L1(WX)        IFNE      0
043300970122RM*  C*
043400970122RM*  C                   SETOFF                                       36
043500970122RM*  C*
043600060111     c* Impossibile lanciare pgm senza la linea di arrivo impostata
043700060111     c*  aseterisco le specifiche con 09 = *off
043800060111RM*  C**   *IN09         IFEQ      *OFF
043900060111     c**   simtpp        andeq     '2'
044000060111     c**   *in09         oreq      *off
044100060111     c**   simtpp        andeq     *blanks
044200060111RM*  C**   L1(WX)        LOOKUP    L6                                     30
044300060111RM*  C**N30              SETON                                        36
044400060111RM*  C**                 END
044500970122RM*  C*
044600970122RM*2AC     *IN36         IFEQ      *OFF
044700920430     C*
044800920604     C* MEMORIZZO FILIALE DI ARRIVO
044900000703     C                   Z-ADD     L1(WX)        DS1LNA
045000971128     C     DS1LNA        CHAIN     AZORG                              33
045100940208     C                   MOVEL     ORGDES        DESLNA           19
045200970122     C*
045300970122   2AC                   SELECT
045400970207     C     DS1RIS        WHENEQ    'R'
045500980513     C* SE E' IMPOSTATO DS1FL4 VUOL DIRE CHE NON SI TRATTA DI
045600980513     C*  RISTAMPA VERA E PROPRIA MA DI UNA RISTAMPOA SUCCESSIVA
045700980513     C*  ALLA ELABORAZIONE DEI DATI PURA
045800980513     C     DS1FL4        IFEQ      ' '
045900970226     C                   SETON                                        15
046000980513     C                   ENDIF
046100980513     C**
046200970122     C                   EXSR      ELASAL
046300970122  X2AC                   OTHER
046400920604     C*
046500950124     C* ?  S P E D I Z I O N I    C O N S E G N A T E  ?
046600930908     C*
046700970207     C                   Z-ADD     DS1ADA        NUM6
046800950123     C     KARB          SETLL     FNARB54L
046900950123     C  N02KARB          READE     FNARB54L                               34
047000970225     C   02DS1LNA        READE     FNARB54L                               34
047100000602      *
047200940324    3C     *IN34         DOWEQ     *OFF
047300000602      *
048800000602     C*
048900940210     C* ESCLUDO I TIPI BOLLA RECUPERO
049000000609     C     ARBCBO        LOOKUP    REC                                    35
049100000609    4C     *IN35         IFEQ      *OFF
049200940324     C* SOLO SE DATA DAL AL CONTROLLO SE SUPERA LIMITE MAGGIORE
049300940324    5C     *IN02         IFEQ      *ON
049400970207     C     ARBDCM        ANDGT     DS1AAL
049500950124     C                   SETON                                        34
049600940323     C                   ELSE
049700090309     C                   EXSR      CONT1
049800940324    5C                   ENDIF
049900940324    4C                   ENDIF
050000920604     C*
050100000609     C  N02KARB          READE     FNARB54L                               34
050200970225     C  N34
050300970225     CAN 02DS1LNA        READE     FNARB54L                               34
050400940324    3C                   ENDDO
050500970130     C* SE NON HO TROVATO NESSUNA SPEDIZIONE CONSEGNATA
050600970130     C*  VERIFICO SE E' UN GIORNO FESTIVO: IN QUESTO CASO
050700970130     C*  NON ELABORO IL RESTO DELLA STATISTICA E SCRIVO UN RECORD
050800970130     C*  VUOTO CON F DI FESTIVO
050900970130     C     WCONS         IFEQ      ' '
051000060314     C                   MOVEL     DS1ADA        KDATA
051100060314     c                   exsr      CTRFesta
051200970130     C                   ENDIF
051300970130     C*
051400920605     C*
051500950124     C* ?  S P E D I Z I O N I   N O N   A N C O R A   C O N S E G N A T E  ?
051600930908     C*
051700970130     C* ELABORO SOLTANTO SE HO TROVATO ALMENO UNA SPEDIZIONE CONSEGNATA
051800970130     C*  E SE NON CE NE SONO MA SI TRATTA DI UN GIORNO LAVORATIVO
051900970130   2BC     WFEST         IFEQ      ' '
052000970130     C*
052100920605     C                   Z-ADD     0             NUM6
052200950123     C     KARB          SETLL     FNARB54L
052300950123     C     KARB          READE     FNARB54L                               34
052400920605     C*
052500940324    3C     *IN34         DOWEQ     *OFF
054100940210     C*
054200940210     C* ESCLUDO I TIPI BOLLA RECUPERO
054300950123     C     ARBCBO        LOOKUP    REC                                    35
054400940324    4C     *IN35         IFEQ      *OFF
054500940324     C* 02 OFF
054600940324     C* SE DATA ARRIVO > DATA DAL --> E' PER FORZA NON CONSEGNABILE
054700940324     C* CONTO LE NON CONSEGNATE E LE CONSEGNABILI
054800970207    5C  N02ARBDAM        IFLE      DS1ADA
054900090309     C                   EXSR      CONT4
055000940324    5C                   ENDIF
055100940324     C* 02 ON
055200940324     C* SE DATA ARRIVO > DATA AL  --> E' PER FORZA NON CONSEGNABILE
055300940324     C* CONTO SOLO LE RISERVE E LE CONSEGNE PARZIALI
055400970207    5C   02ARBDAM        IFLE      DS1AAL
055500090309     C                   EXSR      CONT3P
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
057100000609     C*
058700940210     C* ESCLUDO I TIPI BOLLA RECUPERO
058800950123     C     ARBCBO        LOOKUP    REC                                    35
058900940324    4C     *IN35         IFEQ      *OFF
059000940324     C* 02 OFF
059100970207    5C  N02ARBDAM        IFLE      DS1ADA
059200090309     C                   EXSR      CONT4
059300940324    5C                   ENDIF
059400940324     C* 02 ON
059500970207    5C   02ARBDAM        IFLE      DS1AAL
059600090309     C                   EXSR      CONT3P
059700940324    5C                   ENDIF
059800940324    4C                   ENDIF
059900920605     C*
060000970225     C     DS1LNA        READE     FNARB54L                               34
060100970130    3C                   ENDDO
060200970130   2BC                   ENDIF
060300940323     C*
060400920605     C*
060500970121     C* ?  S C R I V O    L A   S T A T I S T I C A  ?
060600930908     C*
060700970305     C* SOLO SE HO CHIESTO ANCHE LA STATISTICA
060800970305   2BC     DS1FL1        IFNE      'L'
060900970403     C     DS1RIS        ANDNE     'R'
061000970305     C**
061100970122     C* CANCELLO TUTTI I RECORD GIA' PRESENTI NEL FILE
061200000703     C*********          Z-ADD     DS1FEL        KFLE
061300970225     C                   MOVEL     DS1LNA        KLNA
061400970207     C                   Z-ADD     DS1ADA        KDCM
061500000703     C     KSCO1         CHAIN     FNSCO04L                           30
061600970122    3C     *IN30         DOWEQ     *OFF
061700970121     C                   DELETE    FNSCO000
061800000703     C     KSCO1         READE     FNSCO04L                               30
061900970122    3C                   ENDDO
062000970121     C**
062100970121     C                   EXSR      WRTSCO
062200970124     C**
062300970124     C** VEDO SE AGGIORNARE LA DATA PRIMA E ULTIMA ELABORAZIONE
062400970124     C                   MOVEL     '7A'          COD
062500970225     C                   MOVEL(P)  DS1LNA        KEY
062600970124     C     KTAB          CHAIN     TABEL                              30
062700970124     C* LA TABELLA CI DEVE ESSER EPER FORZA PERCHE' LA CREA SE MANCA
062800970124     C*  IL FILTRO
062900970124    3C     *IN30         IFEQ      *OFF
063000970124     C                   MOVEL     TBLUNI        DS7AF
063100970124    4C     ALFDUE        IFLT      *ZEROS
063200970124     C                   MOVEL     *ZEROS        ALFDUE
063300970124    4C                   ENDIF
063400970124    4C     ALFDPE        IFLT      *ZEROS
063500970124     C                   MOVEL     *ZEROS        ALFDPE
063600970124    4C                   ENDIF
063700970124     C*
063800021021     C* DATA RIELABORATA: DEVO AGGIORNARE FISTA
063900970207    5C     DS1ADA        IFGE      §7ACPE
064000970207    5C     DS1ADA        ANDLE     §7ACUE
064100970124     C                   MOVEL     'C'           KTRC
064200970207     C                   MOVEL     DS1ADA        KDTS
064300021017     c                   Movel     Ds1Lna        Wfil
064400021022     c     Kfista        Chain     Fista01l                           30
064500021022     c     *In30         DowEq     *Off
064600021022     c                   Delete    Fista01l
064700021022     c     Kfista        Reade     Fista01l                               30
064800021022     c                   EndDo
064900970124     C                   ENDIF
065000970124     C*
065100970124     C* SE MANCA LA PRIMA DATA ELABORAZIONE: AGGIORNO
065200970124    4C     §7ACPE        IFEQ      0
065300970207     C                   MOVEL     DS1ADA        ALFDPE
065400970124   X4C                   ELSE
065500970124     C* SE E' MINORE DI 1 GIORNO DELLA DATA PRIMA ELABORAZIONE
065600970124     C*  AGGIORNO ALTRIMENTI NO
065700970207    5C     DS1ADA        IFLT      §7ACPE
065800970207     C                   Z-ADD     DS1ADA        G02INV
065900970124     C                   MOVEL     '3'           G02ERR
066000970124     C                   CALL      'XSRDA8'
066100970124     C                   PARM                    WLBDAT
066200970124     C* TRASFORMO LA DATA IN GIORNI E ADDIZIONO 1
066300970124     C     G02TGI        ADD       1             GIOTGI
066400970124     C                   CALL      'XSRGI8'
066500970124     C                   PARM                    WGIDAT
066600970124    6C     GIOINV        IFEQ      §7ACPE
066700970207     C                   Z-ADD     DS1ADA        §7ACPE
066800970124    6C                   ENDIF
066900970124    5C                   ENDIF
067000970124    4C                   ENDIF
067100970124     C*
067200970124     C* SE MANCA L'ULTIMA DATA ELABORAZIONE: AGGIORNO
067300970124    4C     §7ACUE        IFEQ      0
067400970207     C                   MOVEL     DS1ADA        ALFDUE
067500970124   X4C                   ELSE
067600970124     C* SE E' MAGGIORE DI 1 GIORNO DELLA DATA ULTIMA ELABORAZIONE
067700970124     C*  AGGIORNO ALTRIMENTI NO
067800970207    5C     DS1ADA        IFGT      §7ACUE
067900970207     C                   Z-ADD     DS1ADA        G02INV
068000970124     C                   MOVEL     '3'           G02ERR
068100970124     C                   CALL      'XSRDA8'
068200970124     C                   PARM                    WLBDAT
068300970124     C* TRASFORMO LA DATA IN GIORNI E SOTTRAGGO 1
068400970124     C     G02TGI        SUB       1             GIOTGI
068500970124     C                   CALL      'XSRGI8'
068600970124     C                   PARM                    WGIDAT
068700970124    6C     GIOINV        IFEQ      §7ACUE
068800970207     C                   Z-ADD     DS1ADA        §7ACUE
068900970124    6C                   ENDIF
069000970124     C*
069100970124    5C                   ENDIF
069200970124    4C                   ENDIF
069300970124     C*
069400970124     C                   MOVEL     DS7AF         TBLUNI
069500970124     C                   UPDATE    TABEL
069600970124    3C                   ENDIF
069700970403   2BC                   ENDIF
069800970124     C*
069900970122   2AC                   ENDSL
070000970121     C*
070100970121     C* ?  S T A M P A    S T A T I S T I C A  ?
070200970225    3C     DS1FL1        IFEQ      'S'
070300970225    3C     DS1FL1        OREQ      'E'
070400920605     C                   EXSR      STAMPA
070500970122    3C                   ENDIF
070600060118     C*
070700060118     C* AZZERO CAMPI PER NUOVA LINEA DI ARRIVO
070800060118     C                   SETOFF                                       0103
070900060118     C                   CLEAR                   CNE
071000060118     C                   CLEAR                   CNN
071100060118     C                   CLEAR                   CNC
071200060118     C                   CLEAR                   ESP
071300060118     C                   CLEAR                   NOR
071400060118     C                   CLEAR                   TOT
071500060118     C                   CLEAR                   ICE
071600060118     C                   CLEAR                   ICN
071700060118     C                   CLEAR                   ICC
071800060118     C                   CLEAR                   GAE
071900060118     C                   CLEAR                   GAN
072000060118     C                   CLEAR                   GAC
072100060118     C                   CLEAR                   LAE
072200060118     C                   CLEAR                   LAN
072300060118     C                   CLEAR                   LAC
072400060118     C                   CLEAR                   NFE
072500060118     C                   CLEAR                   NFN
072600060118     C                   CLEAR                   NFC
072700060118     C                   CLEAR                   CTE
072800060118     C                   CLEAR                   CTN
072900060118     C                   CLEAR                   CTC
073000060118     C                   CLEAR                   IDE
073100060118     C                   CLEAR                   IDN
073200060118     C                   CLEAR                   IDC
073300060118     C                   CLEAR                   COE
073400060118     C                   CLEAR                   CON
073500060118     C                   CLEAR                   COC
073600060118     C                   CLEAR                   PRE
073700060118     C                   CLEAR                   PRN
073800060118     C                   CLEAR                   PRC
073900060118     C                   CLEAR                   RSE
074000060118     C                   CLEAR                   RSN
074100060118     C                   CLEAR                   RSC
074200060118     C                   CLEAR                   NCE
074300060118     C                   CLEAR                   NCN
074400060118     C                   CLEAR                   NCC
074500060118     C                   CLEAR                   GENNOR
074600060118     C                   CLEAR                   GENESP
074700060118     C                   CLEAR                   GENTOT
074800970122     C*
074900970122RM*2AC                   END
075000000703    2C                   ENDIF
075100980707    1C                   ENDDO
075200940203     C*
075300940324     C   99              EXCEPT    SPEDT
075400050805     C* STAMPO dicitura * fine stampa * in LISTA SPEDIZIONI NON IN CONSEGNA
075500050805     C  N02DS1FL1        IFEQ      'L'
075600050805     C     DS1FL1        OREQ      'E'
075700050805     C     DS1FL3        OREQ      'L'
075800050805     c                   if        *inod = *on
075900050805     c                   except    tesele
076000050805     c                   endif
076100050805     c                   except    fines
076200050805     C                   ENDIF
076300940324     C                   ENDIF
076400920504     C*
076500990329     C                   CLEAR                   DSBS02
076600990329     C                   MOVEL     'C'           T02TLA
076700990329     C                   CALL      'TIBS02R'
076800990329     C                   PARM                    KPJBA
076900990329     C                   PARM                    DSBS02
077000040526     C*
077100040526     C                   CLEAR                   FNLV53DS
077200040526     C                   MOVEL     'C'           D53TLA
077300040526     C                   CALL      'FNLV53R'
077400040526     C                   PARM                    FNLV53DS
077500040526     c
077600900523     C                   SETON                                        LR
077700991130      **********************************************************************
077800991130      * EDITO IMPORTI PER CAMPI in stampa
077900991130      **********************************************************************
078000991130     C     EDTIMPORTI    BEGSR
078100991130
078200991130      * -----------------------------------------------------------------------------------
078300991130      * W0122      contiene il numero da editare
078400991130      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il num
078500991130      *            ero ha decimali
078600991130      * IMPEDITATO ritorna il valore editato per la stampa lungo13 (troncate
078700991130      *            cifre oltre 10 mld)
078800991130      * -----------------------------------------------------------------------------------
078900991130
079000991130     c                   clear                   IMPeditato
079100991130     c                   clear                   W013A
079200991130     c                   if        Wtstdec = 'S'
079300991130     C                   eval      W013a = %EDITC(W0122:'4')
079400991130     c                   else
079500991130     c                   z-add     W0122         W0130
079600991130     C                   eval      W013a = %EDITC(W0130:'4')
079700991130     c                   endif
079800991130     c
079900991130     c                   move      W013a         IMPeditato
080000991130     c                   ENDSR
080100950124     C**************************************************************************
080200950124     C* OPERAZIONI INIZIALI
080300950124     C**************************************************************************
080400930910     C     INIZIO        BEGSR
080500950123     C*
080600950123     C                   TIME                    WTIME            14 0          ORA & DATA
080700950123     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
080800950124     C                   MOVEL     WTIME         UTIME             6 0          ORARIO
080900950127     C*
081000950127     C* INVERTO LA DATA DEL GIORNO
081100950127     C                   Z-ADD     WDATE         G02DAT
081200950127     C                   Z-ADD     *ZERO         G02INV
081300950127     C                   MOVEL     *BLANKS       G02ERR
081400950127     C                   CALL      'XSRDA8'
081500950127     C                   PARM                    WLBDAT
081600950127     C                   Z-ADD     G02INV        WDATE1            8 0          AAAA/MM/GG
081700930910     C*
081800940323     C* 02 ON - IMMESSA DATA AL
081900970207     C     DS1AAL        IFNE      0
082000970207     C     DS1ADA        IFNE      DS1AAL
082100940323     C                   SETON                                        02
082200940323     C                   END
082300940323     C                   ELSE
082400970207     C                   Z-ADD     DS1ADA        DS1AAL
082500940323     C                   END
082600930907     C*
082700930907     C* 09 ON - LINEA ARRIVO IMMESSA
082800060111     C***  DS1LNA        COMP      0                                  09
082900930907     C*
083000930910     C* REPERISCO VARIABILI ARRIVI
083100930907     C                   MOVEL     '7A'          COD
083200930907     C                   MOVEL     *BLANKS       KEY
083300930907     C                   MOVEL     '1'           KEY
083400970124     C     KTAB          CHAIN(N)  TABEL                              31
083500930907     C  N31              MOVEL     TBLUNI        DS7A
083600930907     C   31              CLEAR                   DS7A
083700930907     C* CARICO UNICA SCHIERA DEI RITARDI E CONSEGNE
083800930907     C                   Z-ADD     5             X                 3 0
083900930907     C                   Z-ADD     2             Y                 3 0
084000930907     C                   Z-ADD     -999          ORE(1)
084100930907     C*
084200930907     C                   DO        5
084300930907     C     ANT(X)        IFGT      0
084400930907     C     0             SUB       ANT(X)        ORE(Y)
084500930907     C                   ADD       1             Y
084600930907     C                   ENDIF
084700930907     C                   SUB       1             X
084800930907     C                   ENDDO
084900930907     C*
085000930907     C                   Z-ADD     1             X                 3 0
085100930907     C*
085200930907     C                   DO        5
085300930907     C     RIT(X)        IFGT      0
085400930907     C                   ADD       1             Y
085500930907     C                   Z-ADD     RIT(X)        ORE(Y)
085600930907     C                   ENDIF
085700930907     C                   ADD       1             X
085800930907     C                   ENDDO
085900930907     C*
086000930907     C                   ADD       1             Y
086100930907     C                   Z-ADD     999           ORE(Y)
086200940210     C* CARICO I TIPI BOLLA RECUPERO CHE DEVO SCARTARE
086300940210     C                   Z-ADD     1             X
086400940210     C                   MOVEL     '3A'          COD
086500970124     C     KTAB2         CHAIN(N)  TABEL                              30
086600940210     C*
086700940210     C     *IN30         DOWEQ     *OFF
086800940210     C     TBLFLG        IFEQ      ' '
086900940210     C                   MOVEL     TBLUNI        DS3A
087000940210     C     §3ARBL        IFEQ      'R'
087100940210     C                   MOVEL     TBLKEY        REC(X)
087200940210     C                   ADD       1             X
087300940210     C                   ENDIF
087400940210     C                   ENDIF
087500940210     C*
087600970124     C     KTAB2         READE(N)  TABEL                                  30
087700940210     C                   ENDDO
087800930907     C*
087900950410     C                   Z-ADD     43            LUNG             15 5
088000940530     C* OVRPRTF SU PRTF198
088100970225     C     DS1FL1        IFEQ      'S'
088200970225     C     DS1FL1        OREQ      'E'
088300940324     C                   MOVEA     CMD           COMMAN           80
088400940324     C*
088500940329     C                   CALL      'QCMDEXC'                              98                A
088600940324     C                   PARM                    COMMAN
088700940324     C                   PARM                    LUNG
088800940329     C  N98              OPEN      PRTF198
088900950410     C                   END
089000940601     C* OVRPRTF SU PRTF198X
089100970225     C  N02
089200970225     CANN98DS1FL1        IFEQ      'L'
089300970225     C     DS1FL1        OREQ      'E'
089400970403     C     DS1FL3        OREQ      'L'
089500940530     C                   MOVEA     CM2           COMMAN           80
089600940530     C*
089700940530     C                   CALL      'QCMDEXC'                              98                A
089800940530     C                   PARM                    COMMAN
089900940530     C                   PARM                    LUNG
090000940601     C  N98              OPEN      PRTF198X
090100940530     C                   ENDIF
090200020429     C* SE RICHIESTA STATISTICA PER TUTTE LE FILIALI GESTITE
090300020429     C* E SE SONO UN SECONDO LIVELLO CARICO LA £6
091000950314RM*  C                   CLEAR                   DSUL06
091100950314RM*  C                   MOVE      '£6'          D06COD
091400060111     C                   movel     ds1lna        D06KEY
091500950314RM*  C                   MOVEL     'L'           D06TLA
091600060118 RM* C                   MOVEL     ds1aal        D06drf
091700060118 RM* C                   MOVE      'S'           D06key
091800950314RM*  C                   MOVEL     DSUL06        KPJBU
091900950314RM*  C                   CALL      'TRUL06R'
092000950314RM*  C                   PARM                    KPJBA
092100950314RM*  C                   MOVEL     KPJBU         DSUL06
092200950314RM*  C                   MOVEA     LIN           L6
092500970128     C*
093100970130     C*
093200970130     C                   CLEAR                   WCONS
093300970130     C                   CLEAR                   WFEST
094800070419     c**
094900070419     c* Verifico se ha GEO attiva
095000070419     C     DS1LNA        CHAIN     AZORG                              33
095100070419     c                   movel     orgde8        og148
095200070419     c                   if        §oggeot='S'
095300070419     c                   seton                                        18
095400070419     c                   endif
095500971117     C***
095600971117     C                   SETON                                        OEOFOD
095700950124     C*
095800930907     C                   ENDSR
095900970122     C**************************************************************************
096000970122     C* ELABORO LA STATISTICA PARTENDO DAI SALDI
096100970122     C**************************************************************************
096200970122     C     ELASAL        BEGSR
096300970122     C* S P E D I Z I O N I   C O N S E G N A T E
096400000703     C*********          MOVEL     DS1FEL        KFLE
096500970225     C                   MOVEL     DS1LNA        KLNA
096600970207     C                   MOVEL     DS1ADA        KDCM
096700970122     C*
096800970122     C                   MOVEL     'C'           KSCO
096900970122     C                   MOVEL     'A'           KOLT
097000970122     C                   MOVEL     ' '           KGBO
097100970122     C*
097200970122     C                   EXSR      RIEMPI
097300970122     C*
097400970122     C* C O N S E G N E   P A R Z I A L I
097500970122     C                   MOVEL     'C'           KSCO
097600970122     C                   MOVEL     ' '           KOLT
097700970122     C                   MOVEL     'P'           KGBO
097800970122     C                   EXSR      RIEMPI
097900970122     C* R I S E R V E   C O N S E G N E
098000970122     C                   MOVEL     'C'           KSCO
098100970122     C                   MOVEL     ' '           KOLT
098200970122     C                   MOVEL     'R'           KGBO
098300970122     C                   EXSR      RIEMPI
098400970122     C* SOLO SE RICHEISTA UNA SOLA DATA, CONTINUO A MEMORIZZATE LE SKI
098500970122     C     *IN02         IFEQ      *OFF
098600970122     C*
098700970122     C*  N O N   C O N S E G N A T E
098800970122     C*          DI CUI GIACENZE APERTE
098900970122     C                   MOVEL     'N'           KSCO
099000970122     C                   MOVEL     'A'           KOLT
099100970122     C                   MOVEL     'G'           KGBO
099200970122     C                   EXSR      RIEMPI
099300970122     C*          DI CUI LASCIATI AVVISI
099400970122     C                   MOVEL     'N'           KSCO
099500970122     C                   MOVEL     'A'           KOLT
099600970122     C                   MOVEL     'L'           KGBO
099700970122     C                   EXSR      RIEMPI
099800970122     C*          DI CUI CHIUSE PER TURNO
099900970122     C                   MOVEL     'N'           KSCO
100000970122     C                   MOVEL     'A'           KOLT
100100970122     C                   MOVEL     'C'           KGBO
100200970122     C                   EXSR      RIEMPI
100300970122     C*          DI CUI NON FATTE
100400970122     C                   MOVEL     'N'           KSCO
100500970122     C                   MOVEL     'A'           KOLT
100600970122     C                   MOVEL     'N'           KGBO
100700970122     C                   EXSR      RIEMPI
100800970122     C*  I N     D I S T I N T A
100900970122     C                   MOVEL     'D'           KSCO
101000970122     C                   MOVEL     'A'           KOLT
101100970122     C                   MOVEL     ' '           KGBO
101200970122     C                   EXSR      RIEMPI
101300970122     C* I N   C O N S E G N A
101400970122     C                   MOVEL     'I'           KSCO
101500970122     C                   MOVEL     'A'           KOLT
101600970122     C                   MOVEL     ' '           KGBO
101700970122     C                   EXSR      RIEMPI
101800970122     C* NON   I N   C O N S E G N A
101900970124     C                   MOVEL     'O'           KSCO
102000970122     C                   MOVEL     'A'           KOLT
102100970122     C                   MOVEL     ' '           KGBO
102200970122     C                   EXSR      RIEMPI
102300970122     C*  C O N S E G N A B I L I
102400970122     C                   MOVEL     'B'           KSCO
102500970122     C                   MOVEL     ' '           KOLT
102600970122     C                   MOVEL     ' '           KGBO
102700970122     C                   EXSR      RIEMPI
102800970122     C                   ENDIF
102900970122     C*
103000970122     C                   ENDSR
103100970122     C**************************************************************************
103200970122     C* RIEMPO LE SCHIERE DAL FILE SALDI FNSCO00F
103300970122     C**************************************************************************
103400970122     C     RIEMPI        BEGSR
103500000703     C     KSCO2         SETLL     FNSCO03L
103600000703     C  N02KSCO22        READE     FNSCO03L                               30
103700000703     C   02KSCO21        READE     FNSCO03L                               30
103800970122    1C     *IN30         DOWEQ     *OFF
103900970122     C*
104000970122     C* E' DENTRO LE DATE DI CONSEGNA IMMESSE
104100970122    2C     *IN02         IFEQ      *OFF
104200970207     C     SCODCM        ORLE      DS1AAL
104300970122     C* ESCUDO I RECORD DI TOTALE
104400970122    3C     SCOOLT        IFNE      'T'
104500970122     C     SCOOLT        ANDNE     'F'
104600970122     C* IMPOSTO PER CERCARE IL POSTO NELLA SKIERA DELLE ORE
104700970122    4C                   SELECT
104800970122     C     SCOOLT        WHENEQ    'A'
104900970122     C                   Z-SUB     999           NUM3
105000970122     C     SCOOLT        WHENEQ    'O'
105100970122     C                   Z-ADD     999           NUM3
105200970122     C     SCOOLT        WHENEQ    'D'
105300970122     C                   Z-ADD     SCOORE        NUM3
105400970122     C                   OTHER
105500970122     C* NON CI SONO ORE PER L'AFFIDABILITA'
105600970122     C                   Z-ADD     1             B
105700970122    4C                   ENDSL
105800970122     C*
105900970122     C     SCOOLT        IFNE      ' '
106000970122     C                   EXSR      CALRIT
106100970122     C                   ENDIF
106200970122     C*
106300970122     C                   SELECT
106400970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
106500970122     C     SCOGBO        ANDEQ     ' '
106600970310     C                   ADD       SCOSS1        CNE(B)
106700970310     C                   ADD       SCOSS2        CNC(B)
106800970310     C                   ADD       SCOSS3        CNN(B)
106900970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
107000970122     C     SCOGBO        ANDEQ     'P'                                          CONS PARZIAL
107100970310     C                   ADD       SCOSS1        PRE(B)
107200970310     C                   ADD       SCOSS2        PRC(B)
107300970310     C                   ADD       SCOSS3        PRN(B)
107400970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
107500970122     C     SCOGBO        ANDEQ     'R'                                          RISERVE CONS
107600970310     C                   ADD       SCOSS1        RSE(B)
107700970310     C                   ADD       SCOSS2        RSC(B)
107800970310     C                   ADD       SCOSS3        RSN(B)
107900970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
108000970122     C     SCOGBO        ANDEQ     'G'                                          GIACENZE AP
108100970310     C                   ADD       SCOSS1        GAE(B)
108200970310     C                   ADD       SCOSS2        GAC(B)
108300970310     C                   ADD       SCOSS3        GAN(B)
108400970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
108500970122     C     SCOGBO        ANDEQ     'L'                                          LASCIATI AVV
108600970310     C                   ADD       SCOSS1        LAE(B)
108700970310     C                   ADD       SCOSS2        LAC(B)
108800970310     C                   ADD       SCOSS3        LAN(B)
108900970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
109000970122     C     SCOGBO        ANDEQ     'C'                                          CHIUSE TURNO
109100970310     C                   ADD       SCOSS1        CTE(B)
109200970310     C                   ADD       SCOSS2        CTC(B)
109300970310     C                   ADD       SCOSS3        CTN(B)
109400970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
109500970122     C     SCOGBO        ANDEQ     'N'                                          NON FATTE
109600970310     C                   ADD       SCOSS1        NFE(B)
109700970310     C                   ADD       SCOSS2        NFC(B)
109800970310     C                   ADD       SCOSS3        NFN(B)
109900970122     C     SCOSCO        WHENEQ    'D'                                          IN DISTINTA
110000970122     C     SCOGBO        ANDEQ     ' '
110100970310     C                   ADD       SCOSS1        IDE(B)
110200970310     C                   ADD       SCOSS2        IDC(B)
110300970310     C                   ADD       SCOSS3        IDN(B)
110400970122     C     SCOSCO        WHENEQ    'I'                                          IN CONSEGNA
110500970122     C     SCOGBO        ANDEQ     ' '
110600970310     C                   ADD       SCOSS1        ICE(B)
110700970310     C                   ADD       SCOSS2        ICC(B)
110800970310     C                   ADD       SCOSS3        ICN(B)
110900970122     C     SCOSCO        WHENEQ    'O'                                          NOINCONSEGNA
111000970122     C     SCOGBO        ANDEQ     ' '
111100970310     C                   ADD       SCOSS1        NCE(B)
111200970310     C                   ADD       SCOSS2        NCC(B)
111300970310     C                   ADD       SCOSS3        NCN(B)
111400970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
111500970122     C     SCOORE        ANDEQ     -999                                         IN ANTICIPO
111600970310     C                   ADD       SCOSS1        COE(1)
111700970310     C                   ADD       SCOSS2        COC(1)
111800970310     C                   ADD       SCOSS3        CON(1)
111900970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
112000970122     C     SCOORE        ANDEQ     0                                            IN CONFORMIT
112100970310     C                   ADD       SCOSS1        COE(2)
112200970310     C                   ADD       SCOSS2        COC(2)
112300970310     C                   ADD       SCOSS3        CON(2)
112400970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
112500970122     C     SCOORE        ANDEQ     999                                          IN RITARDO
112600970310     C                   ADD       SCOSS1        COE(3)
112700970310     C                   ADD       SCOSS2        COC(3)
112800970310     C                   ADD       SCOSS3        CON(3)
112900970122     C                   ENDSL
113000970122     C*
113100970122    3C                   ENDIF
113200970122     C*
113300000703     C  N02KSCO22        READE     FNSCO03L                               30
113400000703     C   02KSCO21        READE     FNSCO03L                               30
113500970122   X2C                   ELSE
113600970122     C                   SETON                                        30
113700970122    2C                   ENDIF
113800970122    1C                   ENDDO
113900970122     C                   ENDSR
114000950124     C**************************************************************************
114100950124     C*    CONTO LE SPEDIZIONI CONSEGNATE
114200950124     C**************************************************************************
114300920604     C     CONT1         BEGSR
114400950124     C*
114500920605     C* NORMALE/ESPRESSO
114600970121     C                   SETOFF                                       1011
114700970121     C     ARBTSP        COMP      'E'                                    10
114800970121     C     ARBTSP        COMP      'C'                                    11
114900930907     C*
115000950123     C* RICHIAMO PGM DI CALCOLO AFFIDABILITA'
115100950227     C* SE MANCA LA DATA TEORICA LA CALCOLO
115200950227    1C     ARBDCE        IFEQ      0
115300970925     C                   CLEAR                   DS99
115400930908     C                   EXSR      CALDAT
115500950227   X1C                   ELSE
115600950227     C                   Z-ADD     ARBNCI        NUM3                            NON CONF.CONSEGNA
115700950227    1C                   ENDIF
115800930907     C*
115900930908     C* CERCO LO SCAGLIONE DEL RITARDO
116000930907     C                   EXSR      CALRIT
116100930907     C*
116200930812     C* C O N S E G N A T E
116300970121     C   10              ADD       1             CNE(B)                          ESPRESSI
116400970121     C   11              ADD       1             CNC(B)                          CORRIERE
116500970121     C  N10
116600970121     CANN11              ADD       1             CNN(B)                          DISTRIB
116700970130     C*
116800970130     C* TROVATA ALMENO UNA SPEDIZIONE CONSEGNATA
116900970130     C                   MOVEL     'S'           WCONS             1
117000940117     C*
117100940117     C   99              MOVEL     'C'           STAFLG
117200940203     C   99              ADD       1             STAC              7 0
117300940117     C   99              EXCEPT    SPED                                          S P E D I Z
117400930907     C*
117500930907     C* IN    C O N S E G N A
117600970121     C   10              ADD       1             ICE(B)                          ESPRESSI
117700970121     C   11              ADD       1             ICC(B)                          CORRIERE
117800970121     C  N10
117900970121     CANN11              ADD       1             ICN(B)                          NORMALI
118000920604     C*
118100930812     C* C O N S E G N A B I L I
118200930907     C                   EXSR      CALCON
118300940208     C*
118400940208     C*  VEDO SE C'E' ANCHE QUALCHE CONSEGNA NON FATTA LO STESSO GIORNO
118500970417     C***********          EXSR CONT3
118600950124     C*
118700930907     C                   ENDSR
118800950124     C**************************************************************************
118900950124     C* CALCOLO POSIZIONE SCHIERA DELL'ANTICIPO/RITARDO
119000950124     C**************************************************************************
119100930907     C     CALRIT        BEGSR
119200950124     C*
119300940117     C                   Z-ADD     1             B                 3 0
119400930907     C     NUM3          LOOKUP    ORE(B)                                 32
119500930907     C*
119600930907    1C     *IN32         IFEQ      *OFF
119700930907     C                   Z-ADD     1             B
119800930907     C*
119900930907    2C     NUM3          IFGT      0
120000930907     C     999           LOOKUP    ORE(B)                                 32
120100930907     C*
120200930907   X2C                   ELSE
120300930907     C*
120400930907     C     -999          LOOKUP    ORE(B)                                 32
120500930907    2C                   ENDIF
120600930907    1C                   ENDIF
120700950124     C*
120800930907     C                   ENDSR
120900950124     C**************************************************************************
121000950124     C* CALCOLO LE SPEDIZIONI CONSEGNABILI
121100950124     C**************************************************************************
121200930907     C     CALCON        BEGSR
121300950124     C*
121400930907     C                   SELECT
121500930907     C* NEL PERIODO
121600930907     C     NUM3          WHENEQ    0
121700970121     C   10              ADD       1             COE(2)                          ESPRESSI
121800970121     C   11              ADD       1             COC(2)                          CORRIERE
121900970121     C  N10
122000970121     CANN11              ADD       1             CON(2)                          NORMALI
122100930907     C     NUM3          WHENGT    0
122200970121     C   10              ADD       1             COE(3)                          ESPRESSI
122300970121     C   11              ADD       1             COC(3)                          CORRIERE
122400970121     C  N10
122500970121     CANN11              ADD       1             CON(3)                          NORMALI
122600930907     C                   OTHER
122700970121     C   10              ADD       1             COE(1)                          ESPRESSI
122800970121     C   11              ADD       1             COC(1)                          CORRIERE
122900970121     C  N10
123000970121     CANN11              ADD       1             CON(1)                          NORMALI
123100930907     C                   END
123200930907     C*
123300930907     C                   ENDSR
123400950124     C**************************************************************************
123500950124     C*    CONTO IN ARB00F QUELLE CONSEGNABILI O IN DISTINTA
123600950124     C**************************************************************************
123700930812     C     CONT4         BEGSR
123800930908     C*
123900930908     C* NORMALE/ESPRESSO
124000970121     C                   SETOFF                                       1011
124100970121     C     ARBTSP        COMP      'E'                                    10
124200970121     C     ARBTSP        COMP      'C'                                    11
124300930908     C**
124400970606     C* SE C'E' NUMERO DISTINTA E'    "I N    D I S T I N T A "
124500930908     C**
124600970606    1C*****      ARBDDC    IFEQ DS1ADA
124700970606     C*****
124800970606     C     ARBNDC        IFGT      0
124900970606     C     ARBDDC        ANDGT     0
125000970606     C     ARBDDC        ANDLE     DS1ADA
125100930908     C* DATA E ORA CALCOLO
125200970606     C****                 Z-ADDARBDDC    D98DFC
125300970606     C****                 Z-ADD0         D98OFC
125400970606     C                   Z-ADD     DS1ADA        D98DFC
125500970606     C                   Z-ADD     2359          D98OFC
125600970606     C* SE IL GIORNO E' OGGI, METTO L'ORARIO ESATTO
125700970606     C     DS1ADA        IFEQ      WDATE1
125800970606     C                   MOVEL     UTIME         D98OFC                         HH:MM
125900970606     C                   ENDIF
126000930908     C*
126100930908     C                   EXSR      CALDAT
126200930908     C* CERCO SCAGLIONE DEL RITARDO
126300930908     C                   EXSR      CALRIT
126400930908     C*
126500970121     C   10              ADD       1             IDE(B)                          ESPRESSI
126600970121     C   11              ADD       1             IDC(B)                          CORRIERE
126700970121     C  N10
126800970121     CANN11              ADD       1             IDN(B)                          NORMALI
126900940117     C*
127000940117     C   99              MOVEL     'I'           STAFLG
127100940203     C   99              ADD       1             STAI              7 0
127200940117     C   99              EXCEPT    SPED                                          S P E D I Z
127300930908     C*
127400930908     C* IN    C O N S E G N A
127500970121     C   10              ADD       1             ICE(B)                          ESPRESSI
127600970121     C   11              ADD       1             ICC(B)                          CORRIERE
127700970121     C  N10
127800970121     CANN11              ADD       1             ICN(B)                          NORMALI
127900930908     C* C O N S E G N A B I L I
128000930908     C                   EXSR      CALCON
128100930908     C                   GOTO      ENDCT4
128200930908    1C                   ENDIF
128300930908     C**
128400930908     C* CONTROLLO SE E' "N O N   C O N S E G N A T A"
128500930908     C**
128600930908     C                   EXSR      CONT3
128700940117     C*
128800940117     C*
128900930910     C*  16 ON - E' UNA NON CONSEGNATA O CONSEGNA PARZIALE ESCO
129000930908     C   16              GOTO      ENDCT4
129100990211     C**
129200990211     C     ARBLNP        CHAIN     AZORG01L                           30
129300020207     C  N30              MOVEL     orgde3        og143
129400020207     C  N30              MOVEL     §ogntw        LNPntw            3
129500020211     c   30              clear                   lnpntw
129600930908     C**
129700930908     C* CONTROLLO LE    "N O N   I N   C O N S E G N A"
129800930908     C**
129900930812     C* ESCLUDO SE:
130000930812     C* 1) E' BLOCCATA
130100930908    1C     ARBFBC        IFEQ      'B'
130200970818     C     ARBFBC        OREQ      '*'
130300930812     C* 2) C'E' CONTRASSEGNO E NON E' ABILITATO
130400000703     C**** ARBACC        OREQ      ' '
130500930907     C* 3) E' IN DISTINTA E DATA DISTINTA MINORE DATA DAL
130600970606     C****       ARBDDC    ORLT DS1ADA
130700970606     C****       ARBNDC    ANDGT0
130800940324     C* 4) SE E' UN FERMO DEPOSITO
130900940324     C     ARBFFD        ORNE      *BLANKS
131000950616     C* 5) SE DATA ARRIVO ULTIMO COLLO > DATA CONSEGNABILITA'
131100970818     C*    (BOLLA NON IMPORT)
131200970207     C     ARBDTI        ORGT      DS1ADA
131300020207     C     LNPntw        ANDEQ     'COR'
131400020207     C     ARBDTI        ORGT      DS1ADA
131500020207     C     LNPntw        ANDEQ     'MES'
131800930812     C                   GOTO      ENDCT4
131900950616    1C                   ENDIF
132000990326     C**
132100990326     C** 6) ESCLUDO SE C'E' C.A. APERTA SULLA SPEDIZIONE
132200990326     C                   CLEAR                   WESCA
133900040812      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
134000040812      * legate alla spedizione che passo
134100040812      *
134200040812     c                   clear                   fidn12ds
134300040812     c                   eval      i12aas = arbaas
134400040812     c                   eval      i12lnp = arblnp
134500040812     c                   eval      i12nrs = arbnrs
134600040812     c                   eval      i12nsp = arbnsp
134700040812     c                   eval      i12fel = 'E'
134800040812     c                   eval      i12fan = 'E'
134900040812      *
135000040812     c                   call      'FIDN12R'
135100040812     c                   parm                    fidn12ds
135200040812      *
135300040812      * se non ci sono errori
135400040812     c                   if        o12err = ' '
135500040812      * se numero di CA trovate maggiore di zero
135600040812     c                   if        o12nca > 0
135700040812     c
135800040812     c                   z-add     *zeros        ii                2 0
135900040812     c     1             do        o12nca        ii
136000040812     c                   movea     skkey(ii)     dskey
136100040812     C                   Z-ADD     dsfil         dctfil
136200040812     C                   Z-ADD     dsnca         dctnca
136300040812     C                   Z-ADD     skdca(ii)     dctdca
136400040812     C                   Z-ADD     skdch(ii)     dctdch
136500040812     C                   movea     skcch(ii)     dctcch
136600040812      * se CA aperta imposto il flag e vado a fine
136700040812     c                   If        dctdch = 0
136800040812     c                   eval      wesca = 'S'
136900040812     c                   leave
137000040812     c                   else
137100040812     c                   eval      wesca = 'C'
137200040812     c                   endif
137300040812     C                   ENDDO
137400040812      *
137500040812     c                   endif
137600040812     c                   endif
137700990326     C*
137800990326     C* -SE ESISTE C.A. APERTA  --> ESCO
137900990326    1C     WESCA         IFEQ      'S'
138000990326     C                   GOTO      ENDCT4
138100990326    1C                   ENDIF
138200970818     C*
138300970818     C* DECODIFICA CODICE BOLLA
138400970818     C                   MOVEL     '3A'          COD
138500970818     C                   MOVEL     *BLANKS       KEY
138600970818     C                   MOVEL     ARBCBO        KEY
138700970818     C     KTAB          CHAIN(N)  TABEL                              30
138800970818     C  N30              MOVEL     TBLUNI        DS3A
138900970818     C   30              CLEAR                   DS3A
139000970818     C                   MOVEL     §3ADES        P0DCBO            5
139100990211     C* 6) BOLLA IMPORT:
139200970818     C* ESCLUDO SE DATA ARRIVO ULTIMO COLLO > DATA CONSEGNABILITA
139300970818     C* SOLO SE SI TRATTA DI BOLLA ASSEGNATO CON FATTURA IMMEDIATA
139400020207    1C     LNPntw        IFEQ      'EEX'
139500020207    1C     LNPntw        oreq      'EUP'
139600020207    1C     LNPntw        oreq      'DPD'
139700020207    1C     LNPntw        oreq      'FED'
139800970818     C**
139900970818    2C     ARBDTI        IFGT      DS1ADA
140000970818     C     ARBDTI        OREQ      0
140100970818     C*
140200970818    3C     ARBDTI        IFGT      DS1ADA
140300970818     C     §3AFCA        ANDGT     0
140400970818     C                   GOTO      ENDCT4
140500970818    3C                   ENDIF
140600970818     C*
140700970818     C                   MOVE      ARBKSC        W0040             4 0
140800970818    3C     W0040         IFEQ      9999
140900991104     C                   MOVEL     '1'           KTRC
141000991104     C     KARB6         CHAIN     FIAR601L                           13
141100970818     C** PER ORA ESCO SOLO SE DTI E' CALCOLATO E > DELLA DATA ELABORAZ
141200970818     C*  PERCHE' DTI POTREBBE ESSERE A 0 MA TUTTE LE DATE DI ARRIVO
141300990326     C*  PRESENTI PERCHE' ANCORA NON E' STATO CALCOLATO
141400970818    4C     ARBDTI        IFGT      DS1ADA
141500970818     C     *IN13         ANDEQ     *OFF
141600970818     C     AR6DFT        ANDGT     0
141700970818     C                   GOTO      ENDCT4
141800970818    4C                   ENDIF
141900970818    3C                   ENDIF
142000970818    2C                   ENDIF
142100970818    1C                   END
142200970818     C**
142300990326     C* 7) SE NON E' COMPLETAMENTE ARRIVATA:
142400970818     C*    O E' ARRIVATA DOPO  --> LEGGO IL DETTAGLIO COLLI
142500990329     C* O SE HA IL C/A DEVO CONTROLLARE IL DETT.COLLI
142600970818     C                   CLEAR                   WCONS1
142700970818    1C     ARBDTI        IFEQ      0
142800970818     C     ARBDTI        ORGT      DS1ADA
142900990326     C     WESCA         OREQ      'C'
143000940329     C                   SETOFF                                       17
143100970818     C                   CLEAR                   KDAM1
143200090309     C     KARB3         SETLL     FiART01L
143300090309     C     KARB3         READE     FiART01L                               30
143400940329     C*
143500940329    2C     *IN30         DOWEQ     *OFF
143600040527     c                   clear                   Wartdam
143700040527     c
143800040527    3c                   if        artdam>0
143900040527     c                   eval      Wartdam=artdam
144000040527   x3c                   else
144100040527     c
144200040527     c* Memorizzo la data di arrivo o l'ipotetico arrivo se collo partito
144300040527     c* su FGV abbinato di ida non ancora chiuso: come data di arrivo
144400040527     c*  prendo la data foglio arrivi, solo se richiesto
144500040608    4c                   if        artdam=0  and artdfv>0  and ds1fl5='S'
144600040908     c* Se si tratta di bolla LOCALE, non esiste abbinamento quindi
144700040908     c* cerco di desumere la data del foglio arrivi per vedere se è
144800040908     c*  chiuso o perto
144900040908    5c                   if        arbtfp=arbtfa
145000050706     c                   clear                   wtipolancio
145100070420     c                   eval      kdataloc=artdfv
145200050706     c                   exsr      BOLLALOC
145300040908   X5c                   else
145400050706     C
145500040527     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
145600040527     C                   CLEAR                   fnlv53ds
145700040527     C                   MOVEL     'A'           D53TFO
145800040527     C                   MOVE      artNFV        D53NFV
145900040608     C                   MOVE      arbtfp        D53FGS
146000040527     C                   MOVEL     'B'           D53ABB
146100040527     C                   MOVE      ARBLNA        D53LNA
146200040527     C                   MOVE      ARBTFA        D53TFA
146300040527     C                   CALL      'FNLV53R'
146400040527     C                   PARM                    fnlv53ds
146500040527     c*
146600040908    6c                   if        d53err=*blanks and d53nfa>0
146700040527     c* verifico se fogli arrivi di abbinamento già chiuso
146800040527     C     KFVV          CHAIN     FNFVV000
146900040527     C                   IF        %FOUND(FNFVV01L)
147000040527     C                   IF        FVVFCF=' '
147100040527     C                   EVAL      Wartdam=D53DFA
147200040527     C                   ENDIF
147300040527     C                   ENDIF
147400040908    6c                   endif
147500040908    5c                   endif
147600040527     c
147700040527    4c                   endif
147800040527    3c                   endif
147900040527     c
148000040526     C* SE NON ARRIVATO O ARRIVATO CON DATA > (E'COME SE FOSSE A 0 QUEL
148100970818     C*  GIORNO)
148200040527    3C     Wartdam       IFEQ      0
148300040527     C     Wartdam       ORGT      DS1ADA
148400990329     C* CONTROLLO SE E' SU C.A. MANCANZA O AVARIA RESA O DISTRUTTA
148500990329     C*  NEL QUAL CASO IGNORO PERCHE' IL COLLI NON CI SARA' MAI
148600990329     C                   EXSR      CTRCA
148700990329     C*
148800990329     C     WMANCA        IFEQ      ' '
148900970818     C                   SETON                                        17
149000020207     C     LNPntw        IFEQ      'COR'
149100020207     C     LNPntw        oreq      'MES'
149300970818     C                   SETON                                        30
149400970818     C                   END
149500990329     C                   END
149600990329     C*
149700940329   X3C                   ELSE
149800990329     C* MEMORIZZO CHE ALMENO UN COLLO E' ARRIVATO PRIMA O NEL GIORNO
149900970818     C*  DELLA DATA DI ELABORAZIONE
150000990326    4C     ARTDCM        IFEQ      0
150100970819     C     KDAM1         ANDEQ     *ZEROS
150200990326     C     ARTDCM        ORGT      DS1ADA
150300970818     C     KDAM1         ANDEQ     *ZEROS
150400990326     C*
150500990326     C** NON MEMORIZZO SE IL COLLO NON CONSEGNATO E' RELATIVO AD
150600990326     C**  AVARIE RESE O DISTRUTTE
150700990329     C                   EXSR      CTRCA
150800990329    5C     WMANCA        IFEQ      ' '
150900990326     C                   SETON                                        33
151000990326    5C                   ENDIF
151100990326     C**
151200040527     C   33              Z-ADD     Wartdam       KDAM1
151300970818     C     *LIKE         DEFINE    ARTDAM        KDAM1
151400990329    4C                   END
151500940329    3C                   ENDIF
151600970818     C**
151700090309     C  N30KARB3         READE     FiART01L                               30
151800940329    2C                   ENDDO
151900990326     C*
152000970818     C* SE BOLLA IMPORT NON LA STAMPO SOLO SE ENTRO LA DATA ELABORA-
152100970818     C* ZIONE NON E' ARRIVATO NEMMENO UN COLLO (KDAM1=0)
152200970818     C* STAMPO INVECE LE SPEDIZIONI ARRIVATE PARZIALMENTE
152300990329     C* SE PRESENTE C.A. CHIUSA: ESCO SE I SOLI COLLI NON CONSEGNATI
152400990329     C*  SONO QUELLI RELATIVI A MANCANZE O AVARIE RESE/DISTRUTTE
152500970818    3C     KDAM1         IFEQ      *ZEROS
152600970818     C                   GOTO      ENDCT4
152700040526    3C                   ENDIF
152800990326     C**
152900970818     C* E' UNA ARRIVO PARZIALE: STAMPO SOLO SE NON E' FATTURA IMMEDIATA
153000970818     C*  E SE NON HA IN CONTRASSEGNO
153100020207    2C     LNPntw        IFEQ      'EEX'
153200020207    2C     LNPntw        oreq      'EUP'
153300020207    2C     LNPntw        oreq      'DPD'
153400020207    2C     LNPntw        oreq      'FED'
153500970818    3C     *IN17         IFEQ      *ON
153600970818     C     ARBDTI        ANDEQ     0
153700970818     C*
153800970818    4C     §3AFCA        IFGT      0
153900970818     C     W0040         OREQ      9999
154000970818     C     *IN13         ANDEQ     *OFF
154100970818     C     AR6DFT        ANDGT     0
154200970818     C                   GOTO      ENDCT4
154300970818    4C                   ENDIF
154400970818    3C                   ENDIF
154500970818     C* MEMORIZZO CHE SI TRATTA DI SPEDIZIONE CONSEGNABILE ANCHE SE
154600970818     C* E' UN ARRIVO PARZIALE
154700970818     C   17              MOVE      '1'           WCONS1            1
154800970818     C**
154900970818   X2C                   ELSE
155000940329     C*
155100940329     C   17              GOTO      ENDCT4
155200970818    2C                   END
155300970818    1C                   ENDIF
155400930907     C*
155500961108     C* SE LA BOLLA E' GIACENTE ED HA UNA FIGLIA CON LNP = LNA IN
155600970925     C*   CONSEGNA NON RICHIAMO PGM TNSD99R PERCHE' NON E' CONSEGNABILE
155700961108    1C     ARBFBC        IFEQ      'G'
155800961108     C     ARBFBC        OREQ      '*'
155900050331     C     KARB3         SETLL     Tigcp21L                               33
156000961108    2C     *IN33         IFEQ      *ON
156100961112     C* SALVO I CAMPI DELLA BOLLA
156200961112     C                   MOVE      ARBDS         WSAVDS
156300961108     C                   CLEAR                   WTROV
156400961108     C*
156500961108     C     KARB3         SETLL     FNLBL02L
156600961108     C     KARB3         READE     FNLBL02L                               37
156700961108    3C     *IN37         DOWEQ     *OFF
156800961108    4C     LBLLPN        IFEQ      LBLLAN
156900961108     C     KARB1         CHAIN     FNARB01L                           38
157000961108    5C     *IN38         IFEQ      *OFF
157100961108     C     ARBNDC        ANDGT     0
157200961108     C                   SETON                                        37
157300961108     C                   MOVEL     'S'           WTROV             1
157400961108    5C                   ENDIF
157500961108    4C                   ENDIF
157600961108     C  N37KARB3         READE     FNLBL02L                               37
157700961108    3C                   ENDDO
157800961108     C*
157900961112     C* RIMETTO I CAMPI SALVATI IN PRECEDENZA
158000961112     C                   MOVE      WSAVDS        ARBDS
158100961112     C*
158200961108    3C     WTROV         IFEQ      'S'
158300961108     C                   GOTO      ENDCT4
158400961108    3C                   ENDIF
158500961108    2C                   ENDIF
158600961108    1C                   ENDIF
158700050706     c* Se richiesto, escludo supermercati e appuntamenti arrivati nella dat
158800050706     c*  a richiesta  se privi della cons. richiesta
158900050706     c                   if        ds1fl6='S' and arbdcr=0
159000050706     c                   if        arbtc1='S' or arbtc1='A' or arbtc2='S' or
159100050706     c                             arbtc2='A'
159200050706     c* Per bolla locale, desumo
159300050706     c                   if        arbtfp=arbtfa
159400050706     c                   eval      wtipolancio='SA'
159500070420     c                   eval      kdataloc=arbdbr
159600070420     c
159700050706     c                   exsr      BOLLALOC
159800050706     c                   clear                   d53err
159900050706     c                   eval      d53dfa=wartdam
160000050706     c                   else
160100050706     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
160200050706     C                   CLEAR                   fnlv53ds
160300050706     C                   MOVEL     'A'           D53TFO
160400050706     C                   MOVE      arbNFV        D53NFV
160500050706     C                   MOVE      arbtfp        D53FGS
160600050706     C                   MOVEL     'B'           D53ABB
160700050706     C                   MOVE      ARBLNA        D53LNA
160800050706     C                   MOVE      ARBTFA        D53TFA
160900050706     C                   CALL      'FNLV53R'
161000050706     C                   PARM                    fnlv53ds
161100050706     c                   endif
161200050706     c*
161300060314     c                   select
161400060314    6c                   when      d53err=*blanks and d53dfa=ds1ada
161500050706     C                   GOTO      ENDCT4
161600060314     c
161700060315    6c                   when      d53err=*blanks and d53dfa<ds1ada  and
161800060315     c                             d53dfa>0
161900060314     c* controllo se giorni festivi da d53dfa al giorno prima di ds1ada
162000060314     c     *iso          movel     d53dfa        dataiso
162100060314     c     *iso          movel     ds1ada        dataisofine
162200060314     c                   eval      wfest='S'
162300060314     c                   dow       dataiso<dataisofine and WFEST='S'
162400060314     c                   movel     dataiso       kdata             8 0
162500060314     c                   exsr      CTRFESTA
162600060314     c                   adddur    1:*d          dataiso
162700060314     c                   enddo
162800060314     c
162900060314     c                   if        wfest='S'
163000060314     c                   eval      wfest=' '
163100060314     C                   GOTO      ENDCT4
163200060314     c                   endif
163300060314     c
163400060314    3C                   ENDSL
163500961108     C*
163600050706    3C                   ENDIF
163700050706    3C                   ENDIF
163800050706     C*
163900970207     C                   Z-ADD     DS1ADA        D98DFC
164000950127     C                   Z-ADD     2359          D98OFC
164100950127     C* SE IL GIORNO E' OGGI, METTO L'ORARIO ESATTO
164200970207     C     DS1ADA        IFEQ      WDATE1
164300950127     C                   MOVEL     UTIME         D98OFC                         HH:MM
164400950127     C                   ENDIF
164500930908     C*
164600930908     C                   EXSR      CALDAT
164700940117     C*
164800940117     C                   EXSR      CALRIT
164900930908     C*
165000940117    1C     D98OCI        IFGT      0
165100970925     C*******    WCONS1    OREQ '1'
165200930908     C*
165300940117     C* N O N    IN    C O N S E G N A
165400970121     C   10              ADD       1             NCE(B)                          ESPRESSI
165500970121     C   11              ADD       1             NCC(B)                          CORRIERE
165600970121     C  N10
165700970121     CANN11              ADD       1             NCN(B)                          NORMALI
165800940117     C*
165900940117     C   99              MOVEL     'D'           STAFLG            1
166000940203     C   99              ADD       1             STAD              7 0
166100940117     C   99              EXCEPT    SPED                                          S P E D I Z
166200940530     C*
166300940530     C* STAMPO LISTA SPEDIZIONI NON IN CONSEGNA
166400970225     C  N02DS1FL1        IFEQ      'L'
166500970225     C     DS1FL1        OREQ      'E'
166600970403     C     DS1FL3        OREQ      'L'
166700940530     C                   EXSR      STASNC
166800940530     C                   ENDIF
166900940530     C*
167000930908     C* C O N S E G N A B I L I
167100930908     C                   EXSR      CALCON
167200940117    1C                   ENDIF
167300930812     C*
167400990329     C     ENDCT4        ENDSR
167500990329     C**************************************************************************
167600990329     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
167700990329     C**************************************************************************
167800990329     C     CTRCA         BEGSR
167900990329     C                   CLEAR                   WMANCA
168000990329    1C     WESCA         IFEQ      'C'
168100990329     C**
168200990329     C     KART          CHAIN     FNDCD02L                           33
168300990329    2C     *IN33         IFEQ      *OFF
168400990330     C     DCTATB        ANDEQ     ' '
168500990329     C     KDCT          CHAIN     FNDCT01L                           33
168600050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
168700050221     c                   If        Not *In33 and dctdt2 > 0
168800050221     c                   Eval      *In33 = *On
168900050221     c                   EndIf
169000990329     C*
169100990329    3C     *IN33         IFEQ      *OFF
169200990329     C     DCTDCH        ANDEQ     DCDDCH
169300990329     C     DCDDCH        OREQ      0
169400990329     C* TESTO PRIMA LA CAUSALE DI CHIUSURA PER VEDERE SE MI INDICA LA
169500990329     C*  PRESENZA DELLA MERCE
169600990329     C                   CLEAR                   DSBS02
169700990329     C                   MOVEL     'C'           T02MOD
169800990329     C                   MOVEL     KNSIF         T02SIF
169900990329     C                   MOVEL     'CCH'         T02COD
170000990329     C                   MOVEL     DCTCCH        T02KE1
170100990329     C                   CALL      'TIBS02R'
170200990329     C                   PARM                    KPJBA
170300990329     C                   PARM                    DSBS02
170400990329     C**
170500990329    4C     T02ERR        IFEQ      ' '
170600990330     C                   MOVEL     T02UNI        DCCH
170700990329     C                   ELSE
170800990329     C                   MOVEL     'S'           §CHIMA
170900990329    4C                   ENDIF
171000990329     C*
171100990329    4C     §CHIMA        IFEQ      ' '
171200990329     C                   CLEAR                   DSBS02
171300990329     C                   MOVEL     'C'           T02MOD
171400990329     C                   MOVEL     KNSIF         T02SIF
171500990329     C                   MOVEL     'TAD'         T02COD
171600990329     C                   MOVEL     DCTTAD        T02KE1
171700990329     C                   CALL      'TIBS02R'
171800990329     C                   PARM                    KPJBA
171900990329     C                   PARM                    DSBS02
172000990329     C**
172100990329    5C     T02ERR        IFEQ      ' '
172200990329     C                   MOVEL     T02UNI        DTAD
172300990329     C                   ELSE
172400990329     C                   MOVEL     'M'           §TARGR
172500990329    5C                   ENDIF
172600990329     C*
172700990329     C* AVARIA RESA O DISTRUTTA
172800990329    5C     §TARGR        IFEQ      'V'
172900990329    6C     DCTDSD        IFEQ      'D'
173000990330     C     DCTDSD        OREQ      'R'
173100990329     C                   MOVEL     'V'           WMANCA            1
173200990329    6C                   ENDIF
173300990329    5C                   ENDIF
173400990329     C* MANCANZA
173500990329    5C     §TARGR        IFEQ      'M'
173600990329     C                   MOVEL     'M'           WMANCA            1
173700990329    5C                   ENDIF
173800990329     C*
173900990329    4C                   ENDIF
174000990329    3C                   ENDIF
174100990329    2C                   ENDIF
174200990329    1C                   ENDIF
174300990329     C*
174400990329     C                   ENDSR
174500950124     C**************************************************************************
174600950124     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
174700950124     C**************************************************************************
174800930908     C     CALDAT        BEGSR
174900950123     C*
175000930908     C                   MOVEL     ' '           D98TLA                         *TIPO LANCIO
175100930908     C                   MOVEL     'A'           D98TBO                         *TIPO BOLLA
175200950123     C                   Z-ADD     ARBAAS        D98AAS
175300950123     C                   Z-ADD     ARBLNP        D98LNP
175400950123     C                   Z-ADD     ARBNRS        D98NRS
175500950123     C                   Z-ADD     ARBNSP        D98NSP
175600970925     C                   CALL      'TNSD99R'
175700970925     C                   PARM                    DS99
175800930908     C*
175900950123     C                   Z-ADD     D98NCI        NUM3              3 0
176000950124     C*
176100930908     C                   ENDSR
176200950124     C**************************************************************************
176300950124     C* CONTROLLO LE NON CONSEGNATE DAGLI EVENTI
176400950124     C**************************************************************************
176500930908     C     CONT3         BEGSR
176600950124     C*
176700970417     C* PER OGNI BOLLA LEGGO SOLTANTO L'ULTIMO EVENTO
176800940208     C                   SETOFF                                       16
176900970207     C                   Z-ADD     DS1ADA        NUM6A
177000950123     C                   Z-ADD     ARBAAS        AR§AAS
177100970417     C                   MOVEL     *HIVAL        NUMORA
177200970417     C                   CLEAR                   WWRT
177300990805     C     KARB22        SETLL     FNEVB000                           32
177400990805     C     KARB2         READPE    FNEVB000                               32
177500930908     C*
177600970417    1C     *IN32         IFEQ      *OFF
177700940208     C                   SETOFF                                       05
177800940208     C* DATA E ORA CALCOLO
177900950124     C                   Z-ADD     EVBDEV        D98DFC                          DATA EVENTO
178000990805     C****                 MOVELEVBDEV    WDUE    20       ANNO
178100990805    2C****       WDUE      IFGT 60
178200990805     C****                 MOVEL'19'      D98DFC           XX° SECOLO
178300990805     C****                 ELSE
178400990805     C****                 MOVEL'20'      D98DFC           XXI° SECOLO
178500990805    2C****                 ENDIF
178600950124     C                   Z-ADD     EVBHEV        D98OFC                          ORA  EVENTO
178700940208     C                   EXSR      CALDAT
178800940208     C*
178900940208     C* CERCO SCAGLIONE DEL RITARDO
179000940208     C                   EXSR      CALRIT
179100930908     C*
179200930908     C                   MOVEL     '2A'          COD
179300930908     C                   MOVEL     *BLANKS       KEY
179400930908     C                   MOVEL     EVBCEV        KEY
179500970124     C     KTAB          CHAIN(N)  TABEL                              30
179600930908     C   30              CLEAR                   DS2A
179700930908     C  N30              MOVEL     TBLUNI        DS2A
179800940208     C*
179900930908     C* TIPO CONSEGNA
180000970417    2C     §2AFTC        IFNE      *BLANKS
180100930908     C*
180200930908     C* SOLO SE RICHIESTA UNA DATA SCRIVO LE NON CONSEGNATE
180300930908     C                   SELECT
180400970417    3C     §2AFTC        WHENEQ    'G'
180500940208     C   99              MOVEL     'G'           STAFLG
180600940208     C   99              ADD       1             STAG              7 0
180700970121     C   10              ADD       1             GAE(B)                         GIAC APERTE
180800970121     C   11              ADD       1             GAC(B)
180900970121     C  N10
181000970121     CANN11              ADD       1             GAN(B)
181100970417     C                   MOVEL     'S'           WWRT              1
181200970417    3C     §2AFTC        WHENEQ    'A'                                          LASC AVVISO
181300940208     C   99              MOVEL     'A'           STAFLG
181400940208     C   99              ADD       1             STAA              7 0
181500970121     C   10              ADD       1             LAE(B)
181600970121     C   11              ADD       1             LAC(B)
181700970121     C  N10
181800970121     CANN11              ADD       1             LAN(B)
181900970417     C                   MOVEL     'S'           WWRT              1
182000930908     C*
182100970417   X3C                   OTHER
182200970417    4C     §2ACPT        IFEQ      'S'                                          CHIUSO TURNO
182300940208     C   99              MOVEL     'T'           STAFLG
182400940208     C   99              ADD       1             STAT              7 0
182500970121     C   10              ADD       1             CTE(B)
182600970121     C   11              ADD       1             CTC(B)
182700970121     C  N10
182800970121     CANN11              ADD       1             CTN(B)
182900970417     C                   MOVEL     'S'           WWRT              1
183000970417   X4C                   ELSE
183100940208     C   99              MOVEL     'N'           STAFLG
183200940208     C   99              ADD       1             STAN              7 0
183300970121     C   10              ADD       1             NFE(B)                         NON FATTE
183400970121     C   11              ADD       1             NFC(B)
183500970121     C  N10
183600970121     CANN11              ADD       1             NFN(B)
183700970417     C                   MOVEL     'S'           WWRT              1
183800970417    4C                   ENDIF
183900970417    3C                   ENDSL
184000940117     C*
184100970417   X2C                   ELSE
184200940208     C*
184300940208     C* SE CONSEGNA PARZIALE COME SE FOSSE CONSEGNATA
184400940208     C                   SELECT
184500970417    3C     §2ATIP        WHENEQ    'P'
184600930910     C*
184700930910     C* C O N S E G N A T E
184800970121     C   10              ADD       1             CNE(B)                          ESPRESSI
184900970121     C   11              ADD       1             CNC(B)                          CORRIERE
185000970121     C  N10
185100970121     CANN11              ADD       1             CNN(B)                          NORMALI
185200940208     C* TOTALE A PARTE COME C.PARZIALE
185300970121     C   10              ADD       1             PRE(1)                          CONS PARZ
185400970121     C   11              ADD       1             PRC(1)
185500970121     C  N10
185600970121     CANN11              ADD       1             PRN(1)
185700970417     C                   MOVEL     'S'           WWRT              1
185800940117     C*
185900940117     C   99              MOVEL     'P'           STAFLG
186000940203     C   99              ADD       1             STAP              7 0
186100940208     C*
186200940208     C* SE 'RIP' COME SE NON FATTA
186300970417     C***        §2ATIP    WHEQ 'S'
186400970417     C***99                MOVEL'N'       STAFLG
186500970417     C***99                ADD  1         STAN    70
186600970417     C***10                ADD  1         NFE,B            NON FATTE
186700970417     C***11                ADD  1         NFC,B
186800970417     C**N10N11             ADD  1         NFN,B
186900970417     C***                  OTHER
187000940208     C* RISERVA CONSEGNA
187100970417    3C     §2ATIP        WHENEQ    'R'
187200940208     C   99              MOVEL     'R'           STAFLG
187300940208     C   99              ADD       1             STAR              7 0
187400970121     C   10              ADD       1             RSE(1)                         RISERVA CONS
187500970121     C   11              ADD       1             RSC(1)
187600970121     C  N10
187700970121     CANN11              ADD       1             RSN(1)
187800970417     C                   MOVEL     'S'           WWRT              1
187900940208     C* 05 ON - NON DEVO CONTEGGIARLA COME IN CONSEGNA (GIA' FATTO)
188000940208     C                   SETON                                        05
188100970417    3C                   ENDSL
188200970417    2C                   ENDIF
188300970417     C* EVENTO DA CONTEGGIARE
188400970417    2C     WWRT          IFEQ      'S'
188500940208     C   99              EXCEPT    SPED
188600940208     C     *IN05         IFEQ      *OFF
188700940208     C* IN    C O N S E G N A
188800970121     C   10              ADD       1             ICE(B)                          ESPRESSI
188900970121     C   11              ADD       1             ICC(B)                          CORRIERE
189000970121     C  N10
189100970121     CANN11              ADD       1             ICN(B)                          NORMALI
189200940208     C*
189300940208     C* C O N S E G N A B I L I
189400940208     C                   EXSR      CALCON
189500940208     C                   SETON                                          16
189600940208     C                   ENDIF
189700930910     C*
189800970417    2C                   ENDIF
189900970417    1C                   ENDIF
190000930908     C*
190100930908     C                   ENDSR
190200950124     C**************************************************************************
190300970417     C* CONTROLLO LE CONSEGNE PARZIALI E RISERVE SE DATA DAL AL
190400950124     C**************************************************************************
190500940324     C     CONT3P        BEGSR
190600950124     C*
190700940324     C* NORMALE/ESPRESSO
190800970121     C                   SETOFF                                       1011
190900970121     C     ARBTSP        COMP      'E'                                    10
191000970121     C     ARBTSP        COMP      'C'                                    11
191100940324     C*
191200970207     C                   Z-ADD     DS1ADA        NUM6A
191300950123     C                   Z-ADD     ARBAAS        AR§AAS
191400990805     C     KARB2         SETLL     FNEVB000
191500990805     C     KARB4         READE     FNEVB000                               32
191600940324     C*
191700940324    1C     *IN32         DOWEQ     *OFF
191800950124    2C     EVBDEV        IFGT      CO1DAA
191900940324     C                   SETON                                        32
192000940324   X2C                   ELSE
192100940324     C*
192200940324     C                   MOVEL     '2A'          COD
192300940324     C                   MOVEL     *BLANKS       KEY
192400940324     C                   MOVEL     EVBCEV        KEY
192500970124     C     KTAB          CHAIN(N)  TABEL                              30
192600940324     C   30              CLEAR                   DS2A
192700940324     C  N30              MOVEL     TBLUNI        DS2A
192800940324     C*
192900940324     C* TIPO CONSEGNA
193000970417     C* CONSIDERO SOLO CONSEGNE PARZIALI E RISERVE
193100970417    3C     §2ATIP        IFEQ      'P'
193200970417     C     §2ATIP        OREQ      'R'
193300940324     C* DATA E ORA CALCOLO
193400950406     C                   Z-ADD     EVBDEV        D98DFC                          DATA EVENTO
194100950410     C                   MOVEL     EVBHEV        D98OFC                          ORA  EVENTO
194200940324     C                   EXSR      CALDAT
194300940324     C*
194400940324     C* CERCO SCAGLIONE DEL RITARDO
194500940324     C                   EXSR      CALRIT
194600940324     C*
194700940324     C* SE CONSEGNA PARZIALE COME SE FOSSE CONSEGNATA
194800940324    4C     §2ATIP        IFEQ      'P'
194900940324     C*
195000940324     C* C O N S E G N A T E
195100970121     C   10              ADD       1             CNE(B)                          ESPRESSI
195200970121     C   11              ADD       1             CNC(B)                          CORRIERE
195300970121     C  N10
195400970121     CANN11              ADD       1             CNN(B)                          NORMALI
195500940324     C* TOTALE A PARTE COME C.PARZIALE
195600970121     C   10              ADD       1             PRE(1)                          CONS PARZ
195700970121     C   11              ADD       1             PRC(1)
195800970121     C  N10
195900970121     CANN11              ADD       1             PRN(1)
196000940324     C*
196100940324     C   99              MOVEL     'P'           STAFLG
196200940324     C   99              ADD       1             STAP              7 0
196300940324     C*
196400940324   X4C                   ELSE
196500940324     C* RISERVA CONSEGNA
196600940324     C   99              MOVEL     'R'           STAFLG
196700940324     C   99              ADD       1             STAR              7 0
196800970121     C   10              ADD       1             RSE(1)                         RISERVA CONS
196900970121     C   11              ADD       1             RSC(1)
197000970121     C  N10
197100970121     CANN11              ADD       1             RSN(1)
197200940324    4C                   ENDIF
197300940324    3C                   ENDIF
197400940324     C*
197500940324     C   99              EXCEPT    SPED
197600940324    2C                   ENDIF
197700940324     C*
197800990805     C  N32KARB4         READE     FNEVB000                               32
197900940324    1C                   ENDDO
198000940324     C*
198100940324     C                   ENDSR
198200950124     C**************************************************************************
198300950124     C*    STAMPA STATISTICA
198400950124     C**************************************************************************
198500920605     C     STAMPA        BEGSR
198600950124     C*
198700940117     C   OF              EXCEPT    TESTA
198800940117     C   OF              SETOFF                                       OF
198900940117     C*
199000930908     C                   CLEAR                   CONP1
199100930908     C                   CLEAR                   CONP2
199200930908     C*
199300930908     C* SPEDIZIONI IN CONSEGNA
199400930908     C                   XFOOT     ICE           GENICE            6 0           ESPR
199500970121     C                   XFOOT     ICN           GENICN            6 0           DIST
199600970121     C                   XFOOT     ICC           W0090                           CORR
199700970121     C                   ADD       W0090         GENICN
199800930908     C     GENICN        ADD       GENICE        GENIC             7 0           TOT CONSEG
199900930908     C* SPEDIZIONI CONSEGNABILI
200000930908     C                   XFOOT     COE           GENCOE            6 0           ESPR
200100970121     C                   XFOOT     CON           GENCON            6 0           DIST
200200970121     C                   XFOOT     COC           W0090                           CORR
200300970121     C                   ADD       W0090         GENCON
200400930908     C     GENCON        ADD       GENCOE        GENCO             7 0           TOT CONSEG
200500930908     C**
200600930908     C* SPEDIZIONI   C O N S E G N A T E
200700930908     C**
200800930908     C                   XFOOT     CNE           GENCNE            6 0           ESPR
200900970121     C                   XFOOT     CNN           GENCNN            6 0           DIST
201000970121     C                   XFOOT     CNC           W0090                           CORR
201100970121     C                   ADD       W0090         GENCNN
201200930812     C     GENCNN        ADD       GENCNE        GENCN             7 0           TOT CONSEG
201300930908     C*
201400930908     C* % CONSEGNATE        SU TOT IN CONSEGNA
201500930908    1C     GENCN         IFGT      0
201600930908     C*
201700930908     C     GENCN         MULT      100           NUM9              9 0
201800950123     C     GENIC         IFNE      *ZERO
201900940526     C     NUM9          DIV(H)    GENIC         W0052             5 2
202000950123     C                   ELSE
202100950123     C                   Z-ADD     *ZERO         W0052
202200950123     C                   ENDIF
202300940526     C                   EXSR      CTRP1
202400930908     C*
202500930908     C* % CONSEGNATE        SU TOT CONSEGNABILE
202600950123     C     GENCO         IFNE      *ZERO
202700940526     C     NUM9          DIV(H)    GENCO         W0052
202800950123     C                   ELSE
202900950123     C                   Z-ADD     *ZERO         W0052
203000950123     C                   ENDIF
203100940526     C                   EXSR      CTRP2
203200930908    1C                   ENDIF
203300930908     C*
203400940117     C                   MOVEL     TE(18)        RIGA             66
203500940202     C                   Z-ADD     GENCNN        GENNOR
203600940202     C                   Z-ADD     GENCNE        GENESP
203700940202     C                   Z-ADD     GENCN         GENTOT
203800930908     C                   EXCEPT    INTE1
203900940208     C*
204000940208     C* ESEGUO STAMPA DELLE CONSEGNE PARZIALI E RISERVE
204100940208     C                   EXSR      STAPR
204200930908     C*
204300930908     C* STAMPA DETTAGLIO
204400930908    1C     GENCN         IFGT      0
204500930908     C                   MOVEA     CNE           ESP
204600970121     C     CNC           ADD       CNN           NOR
204700930908     C                   EXSR      STADCN
204800930908    1C                   ENDIF
204900940324     C**
205000940324     C     *IN02         IFEQ      *OFF
205100940329     C* LA 1 PERCENTUALE SUL TOTALE CONSEGNATO NON CI VUOLE PIU'
205200940329     C                   SETON                                        03
205300940324     C**
205400930908     C**
205500930908     C* SPEDIZIONI   N O N    C O N S E G N A T E
205600930908     C**
205700930908     C* GIACENZE APERTE
205800930908     C                   XFOOT     GAE           GENGAE            6 0           ESPR
205900930908     C                   XFOOT     GAN           GENGAN            6 0           NORM
206000970121     C                   XFOOT     GAC           W0090                           CORR
206100970121     C                   ADD       W0090         GENGAN
206200930908     C     GENGAN        ADD       GENGAE        GENGA             7 0           TOT CONSEG
206300930908     C* LASCIATI AVVISI
206400930908     C                   XFOOT     LAE           GENLAE            6 0           ESPR
206500930908     C                   XFOOT     LAN           GENLAN            6 0           NORM
206600970121     C                   XFOOT     LAC           W0090                           CORR
206700970121     C                   ADD       W0090         GENLAN
206800930908     C     GENLAN        ADD       GENLAE        GENLA             7 0           TOT CONSEG
206900930908     C* CHIUSE PER TURNO
207000930908     C                   XFOOT     CTE           GENCTE            6 0           ESPR
207100930908     C                   XFOOT     CTN           GENCTN            6 0           NORM
207200970121     C                   XFOOT     CTC           W0090                           CORR
207300970121     C                   ADD       W0090         GENCTN
207400930908     C     GENCTN        ADD       GENCTE        GENCT             7 0           TOT CONSEG
207500930908     C* NON FATTE
207600930908     C                   XFOOT     NFE           GENNFE            6 0           ESPR
207700930908     C                   XFOOT     NFN           GENNFN            6 0           NORM
207800970121     C                   XFOOT     NFC           W0090                           CORR
207900970121     C                   ADD       W0090         GENNFN
208000930908     C     GENNFN        ADD       GENNFE        GENNF             7 0           TOT CONSEG
208100930908     C*
208200930908    2C     GENGA         IFGT      0
208300930908     C     GENLA         ORGT      0
208400930908     C     GENCT         ORGT      0
208500930908     C     GENNF         ORGT      0
208600930908     C                   CLEAR                   CONP1
208700930908     C                   CLEAR                   CONP2
208800930908     C* NORMALI
208900930908     C     GENGAN        ADD       GENLAN        GENNOR
209000930908     C                   ADD       GENCTN        GENNOR
209100930908     C                   ADD       GENNFN        GENNOR
209200930908     C* ESPRESSO
209300930908     C     GENGAE        ADD       GENLAE        GENESP
209400930908     C                   ADD       GENCTE        GENESP
209500930908     C                   ADD       GENNFE        GENESP
209600930908     C* TOTALE
209700930908     C     GENNOR        ADD       GENESP        GENTOT
209800930908     C*
209900930908     C* % NON CONSEGNATE    SU TOT IN CONSEGNA
210000930908     C*
210100930908     C     GENTOT        MULT      100           NUM9              9 0
210200950123     C     GENIC         IFNE      *ZERO
210300940526     C     NUM9          DIV(H)    GENIC         W0052
210400950123     C                   ELSE
210500950123     C                   Z-ADD     *ZERO         W0052
210600950123     C                   ENDIF
210700940526     C                   EXSR      CTRP1
210800930908     C*
210900930908     C* % NON CONSEGNATE    SU TOT CONSEGNABILE
211000950123     C     GENCO         IFNE      *ZERO
211100940526     C     NUM9          DIV(H)    GENCO         W0052
211200950123     C                   ELSE
211300950123     C                   Z-ADD     *ZERO         W0052
211400950123     C                   ENDIF
211500940526     C                   EXSR      CTRP2
211600930908     C*
211700940117     C                   MOVEL     TE(24)        RIGA
211800940117     C*
211900940117     C   OF              EXCEPT    TESTA
212000940117     C   OF              SETOFF                                       OF
212100930908     C                   EXCEPT    INTE1
212200930908     C**
212300930908     C* GIACENZE APERTE
212400930908     C**
212500930908     C                   Z-ADD     GENGA         GENTOT
212600930908     C                   Z-ADD     GENGAN        GENNOR
212700930908     C                   Z-ADD     GENGAE        GENESP
212800930908     C                   MOVEA     GAE           ESP
212900970121     C     GAC           ADD       GAN           NOR
213000940202     C                   MOVEL     TE(25)        RIGA
213100930908     C*
213200930908     C                   EXSR      CTRNCO
213300930908     C**
213400930908     C* LASCIATI AVVISI
213500930908     C**
213600930908     C                   Z-ADD     GENLA         GENTOT
213700930908     C                   Z-ADD     GENLAN        GENNOR
213800930908     C                   Z-ADD     GENLAE        GENESP
213900930908     C                   MOVEA     LAE           ESP
214000970121     C     LAC           ADD       LAN           NOR
214100940202     C                   MOVEL     TE(26)        RIGA
214200930908     C*
214300930908     C                   EXSR      CTRNCO
214400930908     C**
214500930908     C* CHIUSO PER TURNO
214600930908     C**
214700930908     C                   Z-ADD     GENCT         GENTOT
214800930908     C                   Z-ADD     GENCTN        GENNOR
214900930908     C                   Z-ADD     GENCTE        GENESP
215000930908     C                   MOVEA     CTE           ESP
215100970121     C     CTC           ADD       CTN           NOR
215200940207     C                   MOVEL     TE(28)        RIGA
215300930908     C*
215400930908     C                   EXSR      CTRNCO
215500930908     C**
215600930908     C* NON FATTE
215700930908     C**
215800930908     C                   Z-ADD     GENNF         GENTOT
215900930908     C                   Z-ADD     GENNFN        GENNOR
216000930908     C                   Z-ADD     GENNFE        GENESP
216100930908     C                   MOVEA     NFE           ESP
216200970121     C     NFC           ADD       NFN           NOR
216300940207     C                   MOVEL     TE(27)        RIGA
216400930908     C*
216500930908     C                   EXSR      CTRNCO
216600930908    2C                   ENDIF
216700930908     C**
216800930908     C* SPEDIZIONI   A N C O R A    I N    D I S T I N T A
216900930908     C**
217000930908     C                   XFOOT     IDE           GENIDE            6 0           ESPR
217100930908     C                   XFOOT     IDN           GENIDN            6 0           NORM
217200970121     C                   XFOOT     IDC           W0090                           CORR
217300970121     C                   ADD       W0090         GENIDN
217400930908     C     GENIDN        ADD       GENIDE        GENID             7 0           TOT CONSEG
217500930908     C*
217600930908     C                   CLEAR                   CONP1
217700930908     C                   CLEAR                   CONP2
217800930908     C*
217900930908     C* % IN DISTINTA       SU TOT IN CONSEGNA
218000930908    1C     GENID         IFGT      0
218100930908     C*
218200930908     C     GENID         MULT      100           NUM9              9 0
218300950123     C     GENIC         IFNE      *ZERO
218400940526     C     NUM9          DIV(H)    GENIC         W0052
218500950123     C                   ELSE
218600950123     C                   Z-ADD     *ZERO         W0052
218700950123     C                   ENDIF
218800940526     C                   EXSR      CTRP1
218900930908     C*
219000930908     C* % IN DISTINTA       SU TOT CONSEGNABILE
219100950123     C     GENCO         IFNE      *ZERO
219200940526     C     NUM9          DIV(H)    GENCO         W0052
219300950123     C                   ELSE
219400950123     C                   Z-ADD     *ZERO         W0052
219500950123     C                   ENDIF
219600940526     C                   EXSR      CTRP2
219700930908     C*
219800940117     C                   MOVEL     TE(29)        RIGA
219900940202     C                   Z-ADD     GENIDN        GENNOR
220000940202     C                   Z-ADD     GENIDE        GENESP
220100940202     C                   Z-ADD     GENID         GENTOT
220200930908     C*
220300940117     C   OF              EXCEPT    TESTA
220400940117     C   OF              SETOFF                                       OF
220500930908     C                   EXCEPT    INTE1
220600930908     C*
220700930908     C* STAMPA DETTAGLIO
220800930908     C                   MOVEA     IDE           ESP
220900970121     C     IDC           ADD       IDN           NOR
221000930908     C                   EXSR      STADCN
221100930908    1C                   ENDIF
221200930908     C**
221300930908     C* SPEDIZIONI   I N    C O N S E G N A
221400930908     C**
221500930908     C*
221600930908     C                   CLEAR                   CONP2
221700930908     C*
221800930908     C* % IN DISTINTA       SU TOT CONSEGNABILE
221900930908    2C     GENIC         IFGT      0
222000930908     C     GENIC         MULT      100           NUM9              9 0
222100950123     C     GENCO         IFNE      *ZERO
222200940526     C     NUM9          DIV(H)    GENCO         W0052
222300950123     C                   ELSE
222400950123     C                   Z-ADD     *ZERO         W0052
222500950123     C                   ENDIF
222600940526     C                   EXSR      CTRP2
222700930908    2C                   ENDIF
222800930908     C*
222900940117     C                   MOVEL     TE(30)        RIGA
223000940202     C                   Z-ADD     GENICN        GENNOR
223100940202     C                   Z-ADD     GENICE        GENESP
223200940202     C                   Z-ADD     GENIC         GENTOT
223300930908     C*
223400940117     C   OF              EXCEPT    TESTA
223500940117     C   OF              SETOFF                                       OF
223600930908     C                   EXCEPT    INTE2
223700930908     C*
223800930908     C* STAMPA DETTAGLIO
223900930908    2C     GENIC         IFGT      0
224000930908     C                   MOVEA     ICE           ESP
224100970121     C     ICC           ADD       ICN           NOR
224200930908     C                   EXSR      STADCN
224300930908    2C                   ENDIF
224400930908     C**
224500930908     C* SPEDIZIONI   N O N    I N    C O N S E G N A
224600930908     C**
224700940117     C                   XFOOT     NCE           GENNCE            6 0           ESPR
224800940117     C                   XFOOT     NCN           GENNCN            6 0           NORM
224900970121     C                   XFOOT     NCC           W0090
225000970121     C                   ADD       W0090         GENNCN
225100940117     C     GENNCN        ADD       GENNCE        GENNC             7 0           TOT CONSEG
225200930908     C*
225300930908     C* % NON IN CONSEGNA   SU TOT CONSEGNABILE
225400930908    2C     GENNC         IFGT      0
225500940526     C                   SETON                                        04
225600940117     C                   CLEAR                   CONP2
225700940117     C*
225800930908     C     GENNC         MULT      100           NUM9              9 0
225900950123     C     GENCO         IFNE      *ZERO
226000940526     C     NUM9          DIV(H)    GENCO         W0052
226100950123     C                   ELSE
226200950123     C                   Z-ADD     *ZERO         W0052
226300950123     C                   ENDIF
226400940526     C                   EXSR      CTRP2
226500930908     C*
226600940117     C                   MOVEL     TE(31)        RIGA
226700940202     C                   Z-ADD     GENNCN        GENNOR
226800940202     C                   Z-ADD     GENNCE        GENESP
226900940202     C                   Z-ADD     GENNC         GENTOT
227000930908     C*
227100940117     C   OF              EXCEPT    TESTA
227200940117     C   OF              SETOFF                                       OF
227300930908     C                   EXCEPT    INTE2
227400940117     C*
227500940117     C* STAMPA DETTAGLIO
227600940526    3C     GENNC         IFGT      0
227700940117     C                   MOVEA     NCE           ESP
227800970121     C     NCC           ADD       NCN           NOR
227900940117     C                   EXSR      STADCN
228000940526    3C                   ENDIF
228100940526     C                   SETOFF                                       04
228200940526    2C                   ENDIF
228300930908     C*
228400930908     C**
228500930908     C* SPEDIZIONI   C O N S E G N A B I L I
228600930908     C**
228700930908     C*
228800940117     C                   MOVEL     TE(32)        RIGA
228900940202     C                   Z-ADD     GENCON        GENNOR            8 0
229000940202     C                   Z-ADD     GENCOE        GENESP            8 0
229100940202     C                   Z-ADD     GENCO         GENTOT            8 0
229200930908     C*
229300940117     C   OF              EXCEPT    TESTA
229400940117     C   OF              SETOFF                                       OF
229500930908     C                   EXCEPT    INTE3
229600930909     C*
229700930909     C* STAMPO DETTAGLIO
229800930909     C                   Z-ADD     33            Y
229900930909     C*
230000940207     C                   SETON                                        01
230100930909     C                   DO        3             B
230200940207     C                   CLEAR                   CONP3
230300940117     C                   MOVEL     TE(Y)         RIGA
230400930909     C                   ADD       1             Y
230500930909     C                   Z-ADD     COE(B)        GENESP
230600970121     C     COC(B)        ADD       CON(B)        GENNOR
230700970121     C     GENESP        ADD       GENNOR        GENTOT
230800930909     C* % NON IN CONSEGNA   SU TOT CONSEGNABILE
230900930909    2C     GENTOT        IFGT      0
231000930909     C     GENTOT        MULT      100           NUM9              9 0
231100950123     C     GENCO         IFNE      *ZERO
231200940526     C     NUM9          DIV(H)    GENCO         W0052
231300950123     C                   ELSE
231400950123     C                   Z-ADD     *ZERO         W0052
231500950123     C                   ENDIF
231600940526     C                   EXSR      CTRP3
231700930909     C                   ENDIF
231800930909     C*
231900940117     C   OF              EXCEPT    TESTA
232000940117     C   OF              SETOFF                                       OF
232100930909     C                   EXCEPT    DET2
232200930909     C                   ENDDO
232300940324     C*
232400940324     C                   ENDIF
232500940117     C* RIGA FINALE
232600940117     C                   EXCEPT    DET3
232700940117     C                   SETON                                        OF
232800950124     C*
232900920617     C                   ENDSR
233000950124     C**************************************************************************
233100950124     C* ESEGUO STAMPA DELLE CONSEGNE PARZIALI E RISERVE
233200950124     C**************************************************************************
233300940208     C     STAPR         BEGSR
233400940208     C*
233500940329     C* S T A M P O   C  O N S E G N E   P A R Z I A L I
233600940208     C     PRE(1)        ADD       PRN(1)        GENTOT
233700970121     C                   ADD       PRC(1)        GENTOT
233800940208     C     GENTOT        IFGT      0
233900970121     C     PRC(1)        ADD       PRN(1)        GENNOR
234000940208     C                   Z-ADD     PRE(1)        GENESP
234100940208     C                   MOVEL     TE(38)        RIGA             66
234200940208     C* % CONSEGNATE        SU TOT CONSEGNATO
234300940208     C*
234400940208     C     GENTOT        MULT      100           NUM9              9 0
234500950123     C     GENCN         IFNE      *ZERO
234600940526     C     NUM9          DIV(H)    GENCN         W0052
234700950123     C                   ELSE
234800950123     C                   Z-ADD     *ZERO         W0052
234900950123     C                   ENDIF
235000940526     C                   EXSR      CTRP1
235100940208     C* % CONSEGNATE        SU TOT IN CONSEGNA
235200950123     C     GENIC         IFNE      *ZERO
235300940526     C     NUM9          DIV(H)    GENIC         W0052
235400950123     C                   ELSE
235500950123     C                   Z-ADD     *ZERO         W0052
235600950123     C                   ENDIF
235700940526     C                   EXSR      CTRP2
235800940208     C*
235900940208     C* % CONSEGNATE        SU TOT CONSEGNABILE
236000950123     C     GENCO         IFNE      *ZERO
236100940526     C     NUM9          DIV(H)    GENCO         W0052
236200950123     C                   ELSE
236300950123     C                   Z-ADD     *ZERO         W0052
236400950123     C                   ENDIF
236500940526     C                   EXSR      CTRP3
236600940526     C*
236700940208     C                   EXCEPT    DET2
236800940208     C                   ENDIF
236900940208     C*
237000940329     C* S T A M P O   L E   R I S E R V E
237100940208     C     RSE(1)        ADD       RSN(1)        GENTOT
237200970121     C                   ADD       RSC(1)        GENTOT
237300940208     C     GENTOT        IFGT      0
237400970121     C     RSC(1)        ADD       RSN(1)        GENNOR
237500940208     C                   Z-ADD     RSE(1)        GENESP
237600940208     C                   MOVEL     TE(39)        RIGA             66
237700940208     C* % CONSEGNATE        SU TOT CONSEGNATO
237800940208     C*
237900940208     C     GENTOT        MULT      100           NUM9              9 0
238000950123     C     GENCN         IFNE      *ZERO
238100940526     C     NUM9          DIV(H)    GENCN         W0052
238200950123     C                   ELSE
238300950123     C                   Z-ADD     *ZERO         W0052
238400950123     C                   ENDIF
238500940526     C                   EXSR      CTRP1
238600940208     C* % CONSEGNATE        SU TOT IN CONSEGNA
238700950123     C     GENIC         IFNE      *ZERO
238800940526     C     NUM9          DIV(H)    GENIC         W0052
238900950123     C                   ELSE
239000950123     C                   Z-ADD     *ZERO         W0052
239100950123     C                   ENDIF
239200940526     C                   EXSR      CTRP2
239300940208     C*
239400940208     C* % CONSEGNATE        SU TOT CONSEGNABILE
239500950123     C     GENCO         IFNE      *ZERO
239600940526     C     NUM9          DIV(H)    GENCO         W0052
239700950123     C                   ELSE
239800950123     C                   Z-ADD     *ZERO         W0052
239900950123     C                   ENDIF
240000940526     C                   EXSR      CTRP3
240100940526     C*
240200940208     C                   EXCEPT    DET2
240300940208     C                   ENDIF
240400940208     C*
240500940208     C                   ENDSR
240600950124     C**************************************************************************
240700950124     C* STAMPA DETTAGLIO DELLE CONSEGNATE
240800950124     C**************************************************************************
240900930908     C     STADCN        BEGSR
241000950124     C*
241100930908    1C                   DO        13            B
241200930908     C     ESP(B)        ADD       NOR(B)        TOT(B)
241300930908     C* SCAGLIONE PIENO
241400930908    2C     TOT(B)        IFGT      0
241500930908     C*
241600930908     C                   SELECT
241700930908     C* RITARDO
241800930908    3C     ORE(B)        WHENGT    0
241900930908    4C     ORE(B)        IFEQ      999
242000940117     C                   MOVEL     TE(23)        RIGA
242100930908     C     B             SUB       1             Y
242200930908   X4C                   ELSE
242300940117     C                   MOVEL     TE(22)        RIGA
242400930908     C                   Z-ADD     B             Y
242500930908    4C                   ENDIF
242600930908     C* ANTICIPO
242700930908     C     ORE(B)        WHENLT    0
242800930908    4C     ORE(B)        IFEQ      -999
242900940117     C                   MOVEL     TE(19)        RIGA
243000930908     C     B             ADD       1             Y
243100930908   X4C                   ELSE
243200940117     C                   MOVEL     TE(20)        RIGA
243300930908     C                   Z-ADD     B             Y
243400930908    4C                   ENDIF
243500930908     C*
243600930908   X3C                   OTHER
243700930908     C* CONFORMITA'
243800940117     C                   MOVEL     TE(21)        RIGA
243900940202     C                   Z-ADD     B             Y
244000930908    3C                   ENDSL
244100930908     C*
244200930908     C                   CLEAR                   CONP1
244300930908     C                   CLEAR                   CONP2
244400940329     C                   CLEAR                   CONP3
244500930908     C*
244600940526     C     TOT(B)        MULT      100           NUM9              9 0
244700940526     C*
244800930908     C* % SUL TOTALE IN CONSEGNA
244900940526     C* (NON ELABORO PER LE NON IN CONSEGNA)
245000940526     C     *IN04         IFEQ      *OFF
245100950123     C     GENIC         IFNE      *ZERO
245200940526     C     NUM9          DIV(H)    GENIC         W0052
245300950123     C                   ELSE
245400950123     C                   Z-ADD     *ZERO         W0052
245500950123     C                   ENDIF
245600940526     C                   EXSR      CTRP2
245700940526     C                   ENDIF
245800930908     C*
245900930908     C* % SUL TOTALE CONSEGNABILE
246000950123     C     GENCO         IFNE      *ZERO
246100940526     C     NUM9          DIV(H)    GENCO         W0052
246200950123     C                   ELSE
246300950123     C                   Z-ADD     *ZERO         W0052
246400950123     C                   ENDIF
246500940526     C                   EXSR      CTRP3
246600940203     C* % SUL TOTALE CONSEGNATO
246700940329     C* (SOLO QUANDO ELABORO LE SPEDIZIONI CONSEGNATE)
246800940526     C     *IN03         IFEQ      *OFF
246900950123     C     GENCN         IFNE      *ZERO
247000940526     C     NUM9          DIV(H)    GENCN         W0052
247100950123     C                   ELSE
247200950123     C                   Z-ADD     *ZERO         W0052
247300950123     C                   ENDIF
247400940526     C                   EXSR      CTRP1
247500940526     C                   ENDIF
247600970121     C*
247700970121     C                   Z-ADD     NOR(B)        PNORM             7 0
247800970121     C                   Z-ADD     ESP(B)        PESPR             7 0
247900970121     C                   Z-ADD     TOT(B)        PTOTAL            8 0
248000930908     C*
248100940117     C   OF              EXCEPT    TESTA
248200940117     C   OF              SETOFF                                       OF
248300940117     C                   EXCEPT    DET1
248400930908     C*
248500930908    2C                   ENDIF
248600930908    1C                   ENDDO
248700950124     C*
248800930908     C                   ENDSR
248900950124     C**************************************************************************
249000950124     C* CONTROLLO LE SPEDIZIONI NON CONSEGNATE DA STAMPARE
249100950124     C**************************************************************************
249200930908     C     CTRNCO        BEGSR
249300950124     C*
249400930908    1C     GENTOT        IFGT      0
249500930908     C* %    SU TOT IN CONSEGNA
249600930908     C     GENTOT        MULT      100           NUM9              9 0
249700950123     C     GENIC         IFNE      *ZERO
249800940526     C     NUM9          DIV(H)    GENIC         W0052
249900950123     C                   ELSE
250000950123     C                   Z-ADD     *ZERO         W0052
250100950123     C                   ENDIF
250200940526     C                   EXSR      CTRP2
250300930908     C*
250400930908     C* %    SU TOT CONSEGNABILE
250500950123     C     GENCO         IFNE      *ZERO
250600940526     C     NUM9          DIV(H)    GENCO         W0052
250700950123     C                   ELSE
250800950123     C                   Z-ADD     *ZERO         W0052
250900950123     C                   ENDIF
251000940526     C                   EXSR      CTRP3
251100940207     C*
251200940203     C                   EXCEPT    DET4
251300940203     C                   EXCEPT    DET2
251400930908     C*
251500930908     C                   EXSR      STADCN
251600930908    1C                   ENDIF
251700950124     C*
251800930908     C                   ENDSR
251900950124     C**************************************************************************
252000950124     C* CONTROLLO SE 1% E' > 99,99
252100950124     C**************************************************************************
252200940526     C     CTRP1         BEGSR
252300950124     C*
252400940526     C     W0052         IFGT      99,99
252500940526     C                   Z-ADD     99,99         CONP1             4 2
252600940526     C                   ELSE
252700940526     C                   Z-ADD     W0052         CONP1
252800940526     C                   ENDIF
252900950124     C*
253000940526     C                   ENDSR
253100950124     C**************************************************************************
253200950124     C* CONTROLLO SE 2% E' > 99,99
253300950124     C**************************************************************************
253400940526     C     CTRP2         BEGSR
253500950124     C*
253600940526     C     W0052         IFGT      99,99
253700940526     C                   Z-ADD     99,99         CONP2             4 2
253800940526     C                   ELSE
253900940526     C                   Z-ADD     W0052         CONP2
254000940526     C                   ENDIF
254100950124     C*
254200940526     C                   ENDSR
254300950124     C**************************************************************************
254400950124     C* CONTROLLO SE 3% E' > 99,99
254500950124     C**************************************************************************
254600940526     C     CTRP3         BEGSR
254700950124     C*
254800940526     C     W0052         IFGT      99,99
254900940526     C                   Z-ADD     99,99         CONP3             4 2
255000940526     C                   ELSE
255100940526     C                   Z-ADD     W0052         CONP3
255200940526     C                   ENDIF
255300950124     C*
255400940526     C                   ENDSR
255500950124     C**************************************************************************
255600950124     C* STAMPE SPEDIZIONI NON IN CONSEGNA
255700950124     C**************************************************************************
255800940530     C     STASNC        BEGSR
255900950124     C*
256000950127     C                   Z-ADD     ARBAAS        P0CAAS            2 0
256100940530     C* MESE GIORNO SPEDIZIONE
256200940530     C                   MOVEL     ARBMGS        MMGG
256300940530     C                   Z-ADD     MM            P0CMGS            4 0
256400940530     C                   MOVEL     GG            P0CMGS
256500940601     C* DESTINATARIO
256600940601     C                   MOVEL     ARBRSD        P0CRSD           15
256700950127     C                   MOVEL     ARBIND        P0CIND           24
256800940601     C                   MOVEL     ARBLOD        P0CLOD           15
256900940601     C* DECODIFICA CONSEGNA PARTICOLARE
257000950127     C                   MOVEL     *BLANKS       P0DTC1
257100950123     C     ARBTC1        IFNE      ' '
257200940601     C                   MOVEL     '1P'          COD
257300940601     C                   MOVEL     *BLANKS       KEY
257400950123     C                   MOVEL     ARBTC1        KEY
257500970124     C     KTAB          CHAIN(N)  TABEL                              30
257600950127     C  N30              MOVEL     TBLUNI        P0DTC1            5
257700940601     C                   ENDIF
257800950127     C*
257900950127     C                   MOVEL     *BLANKS       P0DTC2
258000950127     C     ARBTC2        IFNE      *BLANKS
258100950127     C                   MOVEL     '1P'          COD
258200950127     C                   MOVEL     *BLANKS       KEY
258300950127     C                   MOVEL     ARBTC2        KEY
258400970124     C     KTAB          CHAIN(N)  TABEL                              30
258500950127     C  N30              MOVEL     TBLUNI        P0DTC2           10
258600950127     C                   ENDIF
258700051128     C* DATA CONSEGNA RICHIESTA richiamo pgm per sapere se tassativa
258800051128     c                   clear                   p0cdcr            6 0
258900051128     c                   clear                   p0ctcr
259000051128     c                   if        arbdcr>0 or arbhcr>0
259100051128     c                   clear                   fnlv80ds
259200051128     c                   eval      ilv80aas=arbaas
259300051128     c                   eval      ilv80lnp=arblnp
259400051128     c                   eval      ilv80nrs=arbnrs
259500051128     c                   eval      ilv80nsp=arbnsp
259600051128     c                   eval      ilv80mgs=arbmgs
259700051128     c                   eval      ilv80tc1=arbtc1
259800051128     c                   eval      ilv80tc2=arbtc2
259900051128     c                   eval      ilv80dcr=arbdcr
260000051128     c                   eval      ilv80hcr=arbhcr
260100051128     c                   eval      ilv80tcr=arbtcr
260200051128     c                   call      'FNLV80R'
260300051128     c                   parm                    kpjba
260400051128     c                   parm                    fnlv80ds
260500051128     c
260600051128     c                   eval      p0cdcr=olv80dcr6
260700051128     c*
260800051128     c                   if        olv80tat='S'
260900051128     c                   eval      p0ctcr='TA'
261000051128     c                   else
261100051128     c                   clear                   p0ctcr            2
261200051128     c                   move      arbtcr        p0ctcr
261300051128     c                   endif
261400051128     c                   endif
261500940530     C* FERMO DEPOSITO
261600051107     C**   ARBFFD        IFEQ      'S'
261700051107     C**                 MOVEL     'SI'          P0CFFD            2
261800051107     C**                 ELSE
261900051107     C**                 MOVEL     '  '          P0CFFD
262000051107     C**                 ENDIF
262100940530     C*
262200940530     C     *INOD         IFEQ      *ON
262300940530     C                   EXCEPT    TESELE
262400940530     C                   SETOFF                                       OD
262500940530     C                   ENDIF
262600950123     C*
262700950123     C* TENTO DI REPERIRE IL CONTRASEGNO
262800950123     C                   CLEAR                   AR9CAS
262900950123     C                   CLEAR                   AR9VCA
263000051115     C     KARB3         SETLL     FiAR901L                               30     ON=TROVATO
263100051115     C   30KARB3         CHAIN     FiAR901L                           30
263200950127     C*
263300950127     C* TENTO DI REPERIRE L'IMPORTO FATTURA
263400950127     C                   CLEAR                   AR6IFT
263500991130     C                   CLEAR                   AR6DIV
263600991128     C                   CLEAR                   COMIFT
263700991130     C                   CLEAR                   COMdiv
263800991104     C                   MOVEL     '1'           KTRC
263900991104     C     KARB6         SETLL     FIAR601L                               30     ON=TROVATO
264000991128     C     *IN30         IFEQ      *ON
264100991130     C     KARB6         CHAIN     FIAR601L                           30
264200940530     C*
264300991130     C     *IN30         IFEQ      *OFF
264400991130     C* Vado su tabella divise per sapere se accetta i decimali
264500991130     C     AR6DIV        IFNE      SAVDIV
264600991130     C                   MOVEL     'CV'          COD
264700991130     C                   MOVEL(P)  AR6DIV        KEY
264800991130     C     KTAB          CHAIN(N)  TABEL                              37
264900991130     C  N37              MOVEL     TBLUNI        DSCV
265000991130     C   37              CLEAR                   DSCV
265100991130     C                   MOVEL     AR6DIV        SAVDIV
265200991130     C                   ENDIF
265300991130     C*
265400991130     C* Edito importo fattura per la stampa
265500991130     c     ar6ift        ifgt      *zeros
265600991130     C                   Z-ADD     AR6IFT        W0122
265700991130     C                   MOVEL     §CVFDC        WTSTDEC
265800991130     c                   exsr      edtimporti
265900991130     c                   move      impeditato    comift            9
266000991130     c                   movel     ar6div        comdiv
266100991130     c                   endif
266200991128     C                   ENDIF
266300991130     C                   ENDIF
266400940530     C                   EXCEPT    DETELE
266500000703     C*
266600070419     C* STAMPO C/ASSEGNO NON ABILITATO SU SECONDA RIGA se GEO non attiva
266700070419     c                   if        not *in18
266800070419     c
266900070419     C     ARBAcc        IFEQ      ' '
267000070419     C     AR9CAS        ANDNE     0
267100070419     C                   EXCEPT    DETCAA
267200070419     C                   ENDIF
267300070419     c
267400070419     c* Stampo tutte le abilitazioni bolle che ci sono
267500070419     c                   else
267600070419     c                   clear                   fnlrg6ds
267700070419     c                   eval      ilrg6aas=arbaas
267800070419     c                   eval      ilrg6lnp=arblnp
267900070419     c                   eval      ilrg6nrs=arbnrs
268000070419     c                   eval      ilrg6nsp=arbnsp
268100070419     c                   call      'FNLRG6R'
268200070419     c                   parm                    fnlrg6ds
268300070419     c
268400070419     c                   if        olrg6vssin<>*blanks
268500070419     C                   EXCEPT    DETCAA
268600070419     c                   endif
268700070419     c                   endif
268800070419     c
268900950127     C* VEDO SE SONO PRESENTI DELLE NOTE
269000950127     C     *LIKE         DEFINE    AR4NOT        P0CNO1
269100950127     C     *LIKE         DEFINE    AR4NOT        P0CNO2
269200060905     C     *LIKE         DEFINE    AR4NOT        wstadis
269300950127     C                   CLEAR                   P0CNO1
269400950127     C                   CLEAR                   P0CNO2
269500950127     C                   MOVEL     '8'           AR4TRC
269600060216     C     KAR41         SETLL     FiAR401L                               30     ON=TROVATO
269700950127     C   30              DO
269800060216     C     KAR41         CHAIN     FiAR401L                           30
269900950127     C  N30              MOVEL     AR4NOT        P0CNO1
270000950127     C                   MOVEL     '9'           AR4TRC
270100060216     C     KAR41         SETLL     FiAR401L                               30     ON=TROVATO
270200950127     C   30              DO
270300060216     C     KAR41         CHAIN     FiAR401L                           30
270400950127     C  N30              MOVEL     AR4NOT        P0CNO2
270500950127     C                   ENDDO
270600950127     C                   ENDDO
270700060905     c
270800060905     c* Vedo se spedizioni in distinta con data > di quella elaborata
270900060905     c                   setoff                                       12
271000060905     c                   clear                   wstadis
271100060905     c                   if        arbndc>0 and arbddc>0
271200060905     c                   eval      wstadis=Cindistinta
271300060905     c                   eval      %subst(wstadis:15:7)= %editc(arbndc: 'Z')
271400060905     c* giro la data
271500060905     C                   Z-ADD     arbddc        G02INV                         AAAA/MM/GG
271600060905     C                   Z-ADD     *ZERO         G02DAT
271700060905     C                   MOVEL     '3'           G02ERR
271800060905     C                   CALL      'XSRDA8'
271900060905     C                   PARM                    WLBDAT
272000060905     C                   MOVE      G02DAT        W0020             2 0
272100060905     C                   MOVEL     G02DAT        STAddc            6 0          GG/MM/AA
272200060905     C                   MOVE      W0020         STAddc            6 0          GG/MM/AA
272300060905     c                   eval      %subst(wstadis:28:8)= %editc(staddc: 'Y')
272400060905     c
272500060905     c* se non c'e' la seconda nota la riempo
272600060905     c* altrimenti scrivo 3°riga
272700060905     c                   if        p0cno2=*blanks
272800060905     c                   eval      p0cno2=wstadis
272900060905     c                   else
273000060905     c                   seton                                        12
273100060905     c                   endif
273200060905     c                   endif
273300990329     C*
273400990329     C* VEDO SE E' PRESENTE UNA C.A.
273500990329     C                   SETOFF                                       07
273600040812     C**""**     KARB3         CHAIN     FNDCT02L                           30
273700990329     C**
273800040812    1C**""**     *IN30         DOWEQ     *OFF
273900040812     C**""**     DCTATB        ANDNE     ' '
274000040812     C**""**     KARB3         READE     FNDCT02L                               30
274100040812    1C**""**                   ENDDO
274200040812     C**""**  N30              SETON                                        07
274300040812     C**""**   07              EXSR      PRECA
274400950127     C*
274500940601     C* NOTE
274600040812     C**""**     P0CNO1        IFNE      *BLANKS
274700040812     C**""**     P0CNO2        ORNE      *BLANKS
274800040812     C**""**     ARBTC2        ORNE      *BLANKS
274900040812     C**""**     *IN07         OREQ      *ON
275000990329     C*
275100040812     C**""**                   EXCEPT    DE2ELE
275200990329     C*
275300990329     C* SE CI SONO ALTE C.A. STAMPO ANCORA
275400040812     C**""**     KARB3         READE     FNDCT02L                               30
275500040812    2C**""**     *IN30         DOWEQ     *OFF
275600040812    3C**""**     DCTATB        IFEQ      ' '
275700040812     C**""**                   EXSR      PRECA
275800990329     C** STAMPO TESTATA E DETT C.A.
275900040812     C**""**   OD              EXCEPT    TESELE
276000040812     C**""**                   EXCEPT    DETCA
276100040812    3C**""**                   ENDIF
276200040812     C**""**     KARB3         READE     FNDCT02L                               30
276300040812    2C**""**                   ENDDO
276400040812     C**""**                   ENDIF
276500040812      * Sostituisco la lettura del file FNDTC02 con
276600040812      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
276700040812      * legate alla spedizione che passo
276800040812      *
276900040812     c                   clear                   fidn12ds
277000040812     c                   eval      i12aas = arbaas
277100040812     c                   eval      i12lnp = arblnp
277200040812     c                   eval      i12nrs = arbnrs
277300040812     c                   eval      i12nsp = arbnsp
277400040812     c                   eval      i12fel = 'E'
277500040812     c                   eval      i12fan = 'E'
277600040812      *
277700040812     c                   call      'FIDN12R'
277800040812     c                   parm                    fidn12ds
277900040812      *
278000040812      * se non ci sono errori
278100040812     c                   if        o12err = ' '
278200040812      * se numero di CA trovate maggiore di zero
278300040812     c                   if        o12nca > 0
278400040812     c* prendo la prima CA
278500040812     c                   z-add     1             ii                2 0
278600040812     c                   movea     skkey(ii)     dskey
278700040812     C                   Z-ADD     dsfil         dctfil
278800040812     C                   Z-ADD     dsnca         dctnca
278900040812     C                   Z-ADD     skdca(ii)     dctdca
279000040812     C                   Z-ADD     skdch(ii)     dctdch
279100040812     C                   movea     skcch(ii)     dctcch
279200040812     c                   seton                                        07
279300040812     c                   exsr      preca
279400040812      *
279500040812     c                   endif
279600040812     c                   endif
279700040812      * stampo le note
279800040812     C* NOTE
279900040812     C     P0CNO1        IFNE      *BLANKS
280000040812     C     P0CNO2        ORNE      *BLANKS
280100040812     C     ARBTC2        ORNE      *BLANKS
280200040812     C     *IN07         OREQ      *ON
280300040812     C*
280400040812     C                   EXCEPT    DE2ELE
280500040812     C*
280600040812     c* se ci sono altre CA stampo ancora
280700040812      * se non ci sono errori
280800040812     c                   if        o12err = ' '  and o12nca > 0
280900040812     c                   add       1             ii
281000040812     c                   dow       ii <= o12nca
281100040812     c                   movea     skkey(ii)     dskey
281200040812     C                   Z-ADD     dsfil         dctfil
281300040812     C                   Z-ADD     dsnca         dctnca
281400040812     C                   Z-ADD     skdca(ii)     dctdca
281500040812     C                   Z-ADD     skdch(ii)     dctdch
281600040812     C                   movea     skcch(ii)     dctcch
281700040812      *
281800040812     C                   EXSR      PRECA
281900040812     C** STAMPO TESTATA E DETT C.A.
282000040812     C   OD              EXCEPT    TESELE
282100040812     C                   EXCEPT    DETCA
282200040812      *
282300040812     c                   add       1             ii
282400040812     C                   ENDDO
282500040812      *
282600040812     c                   endif
282700040812      *
282800040812     c                   endif
282900060905     c
283000060905     C** STAMPO se in distinta
283100060905     c                   if        *in12
283200060905     C   OD              EXCEPT    TESELE
283300060905     C                   EXCEPT    DETdis
283400060905     c                   endif
283500950123     C*
283600940530     C                   ENDSR
283700990329     C**************************************************************************
283800990329     C* PREPARO I DATI DELLA C.A.
283900990329     C**************************************************************************
284000990329     C     PRECA         BEGSR
284100990329     C                   SETOFF                                       06
284200990329     C                   CLEAR                   STACCH
284300990329     C* DATA APERTURA C.A.
284400990329     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
284500990329     C                   Z-ADD     *ZERO         G02DAT
284600990329     C                   MOVEL     '3'           G02ERR
284700990329     C                   CALL      'XSRDA8'
284800990329     C                   PARM                    WLBDAT
284900990329     C                   MOVE      G02DAT        W0020             2 0
285000990329     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
285100990329     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
285200990329     C** DATA CNHIUSUA C.A.
285300990329     C                   CLEAR                   STACHC
285400990329     C     DCTDCH        IFGT      0
285500990329     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
285600990329     C                   Z-ADD     *ZERO         G02DAT
285700990329     C                   MOVEL     '3'           G02ERR
285800990329     C                   CALL      'XSRDA8'
285900990329     C                   PARM                    WLBDAT
286000990329     C                   MOVE      G02DAT        W0020             2 0
286100990329     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
286200990329     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
286300990329     C                   SETON                                        06
286400990329     C* CAUSALE CHIUSURA
286500990329    2C     DCTCCH        IFNE      *BLANKS
286600990329     C                   CLEAR                   DCCH
286700990329     C                   CLEAR                   DSBS02
286800990329     C                   MOVEL     'C'           T02MOD
286900990329     C                   MOVEL     KNSIF         T02SIF
287000990329     C                   MOVEL     'CCH'         T02COD
287100990329     C                   MOVEL     DCTCCH        T02KE1
287200990329     C                   CALL      'TIBS02R'
287300990329     C                   PARM                    KPJBA
287400990329     C                   PARM                    DSBS02
287500990329     C*
287600990329    3C     T02ERR        IFEQ      ' '
287700990329     C                   MOVEL     T02UNI        DCCH
287800990329    3C                   ENDIF
287900990329     C                   MOVEL     §CHDES        STACCH           25
288000990329    2C                   ENDIF
288100990329     C                   ENDIF
288200990329     C                   ENDSR
288300970121     C**************************************************************************
288400970121     C* SCRIVO I RECORD DI TOTALE IN FNSCO
288500970121     C**************************************************************************
288600970121     C     WRTSCO        BEGSR
288700970130     C                   CLEAR                   FNSCO000
288800970130     C*
288900970130      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
289000970130      * riempo il "flag di trasmissione" (SPAFTR)
289100040922     C***  §5CSCO        IFNE      'S'
289200040922     C***                MOVE      'N'           SCOFTR                         FLAG TRASMISS.
289300040922     C***                MOVEL     *HIVAL        SCODTR
289400040922     C***                ENDIF
289500040922     c                   z-add     wdate1        scodtr
289600040922     c
289700970207     C                   MOVEL     DS1FEL        SCOFLE
289800970225     C                   MOVEL     DS1LNA        SCOLNA
289900970207     C                   MOVEL     DS1ADA        SCODCM
290000970130     C**
290100970130     C* Se si tratta di un giorno festivo scrivo soltanto un record
290200970130     C*  indicando che e' festa con "F" in SCOSCO
290300970225    0C     WFEST         IFEQ      'S'
290400970130     C                   MOVEL     'F'           SCOSCO
290500970130     C                   WRITE     FNSCO000
290600970130     C                   ELSE
290700970130     C**
290800970130     C**
290900970121     C* LID: identifica in a che livello di deve stampare (livello
291000970121     c*      orrizontale, essendoci in stampa 4 scomparti-livelli)
291100970121     c* ORD: e' l'oridne di stampa posto in verticale
291200970122     c* OLT: =A se e' oltre un certo anticipo
291300970122     c*      =D se e' in ritardo/anticipi di un tot di ore
291400970122     c*      =O se e' oltre un certo ritardo
291500970122     c*      =T record di totale
291600970122     c*      =' '  se non c'e' affidabilita'
291700970128     C**
291800970121     C*  C O N S E G N A T E
291900970121     C                   MOVEL     1             SCOLIV
292000970121     C                   MOVEL     '2'           WTOTLI
292100970121     C                   Z-ADD     2             SCOORD
292200970121     C                   MOVEL     'C'           SCOSCO
292300970121     C                   MOVEL     'S'           WANRIT            1
292400970121     C                   MOVEL     ' '           WGBO              1
292500970121     C                   MOVEA     CNE           SKE
292600970121     C                   MOVEA     CNC           SKC
292700970121     C                   MOVEA     CNN           SKN
292800970121     C                   MOVEL     '2'           WTOTLI            1
292900970121     C                   EXSR      SCRIVI
293000970121     C*
293100970121     C*  C O N S E G N E   P A R Z I A L I
293200970121     C                   MOVEL     1             SCOLIV
293300970121     C                   Z-ADD     3             SCOORD
293400970121     C                   MOVEL     'N'           WANRIT            1
293500970121     C                   MOVEL     'P'           WGBO              1
293600970121     C                   CLEAR                   SKE
293700970121     C                   CLEAR                   SKC
293800970121     C                   CLEAR                   SKN
293900970121     C                   Z-ADD     PRE(1)        SKE(1)
294000970121     C                   MOVEA     PRC(1)        SKC(1)
294100970121     C                   MOVEA     PRN(1)        SKN(1)
294200970121     C                   EXSR      SCRIVI
294300970121     C*  R I S E R V E     C O N S E G N E
294400970121     C                   MOVEL     1             SCOLIV
294500970121     C                   Z-ADD     4             SCOORD
294600970121     C                   MOVEL     'N'           WANRIT            1
294700970121     C                   MOVEL     'R'           WGBO              1
294800970121     C                   CLEAR                   SKE
294900970121     C                   CLEAR                   SKC
295000970121     C                   CLEAR                   SKN
295100970121     C                   Z-ADD     RSE(1)        SKE(1)
295200970121     C                   MOVEA     RSC(1)        SKC(1)
295300970121     C                   MOVEA     RSN(1)        SKN(1)
295400970121     C                   EXSR      SCRIVI
295500970121     C*  N O N   C O N S E G N A T E
295600970121     C*          DI CUI GIACENZE APERTE
295700970121     C                   MOVEL     1             SCOLIV
295800970121     C                   MOVEL     '1'           WTOTLI            1
295900970121     C                   Z-ADD     7             SCOORD
296000970121     C                   MOVEL     'N'           SCOSCO
296100970121     C                   MOVEL     'S'           WANRIT            1
296200970121     C                   MOVEL     'G'           WGBO              1
296300970121     C                   MOVEA     GAE           SKE
296400970121     C                   MOVEA     GAC           SKC
296500970121     C                   MOVEA     GAN           SKN
296600970121     C                   EXSR      SCRIVI
296700970121     C*          DI CUI LASCIATI AVVISI
296800970121     C                   MOVEL     1             SCOLIV
296900970121     C                   Z-ADD     9             SCOORD
297000970121     C                   MOVEL     'L'           WGBO              1
297100970121     C                   MOVEA     LAE           SKE
297200970121     C                   MOVEA     LAC           SKC
297300970121     C                   MOVEA     LAN           SKN
297400970121     C                   EXSR      SCRIVI
297500970121     C*          DI CUI CHIUSE PER TURNO
297600970121     C                   MOVEL     1             SCOLIV
297700970121     C                   Z-ADD     11            SCOORD
297800970121     C                   MOVEL     'C'           WGBO              1
297900970121     C                   MOVEA     CTE           SKE
298000970121     C                   MOVEA     CTC           SKC
298100970121     C                   MOVEA     CTN           SKN
298200970121     C                   EXSR      SCRIVI
298300970121     C*          DI CUI NON FATTE
298400970121     C                   MOVEL     1             SCOLIV
298500970121     C                   Z-ADD     13            SCOORD
298600970121     C                   MOVEL     'N'           WGBO              1
298700970121     C                   MOVEA     NFE           SKE
298800970121     C                   MOVEA     NFC           SKC
298900970121     C                   MOVEA     NFN           SKN
299000970121     C                   EXSR      SCRIVI
299100970121     C*          TOTALE DELLE NON CONSEGNATE
299200970121     C                   MOVEL     2             SCOLIV
299300970121     C                   Z-ADD     5             SCOORD
299400970121     C                   MOVEL     'N'           WANRIT            1
299500970121     C                   MOVEL     'T'           SCOOLT
299600970121     C                   CLEAR                   WGBO
299700970121     C                   CLEAR                   SKE
299800970121     C                   CLEAR                   SKC
299900970121     C                   CLEAR                   SKN
300000970121     C                   XFOOT     GAE           W0090             9 0
300100970121     C                   ADD       W0090         SKE(1)
300200970121     C                   XFOOT     LAE           W0090             9 0
300300970121     C                   ADD       W0090         SKE(1)
300400970121     C                   XFOOT     NFE           W0090             9 0
300500970121     C                   ADD       W0090         SKE(1)
300600970121     C                   XFOOT     CTE           W0090             9 0
300700970121     C                   ADD       W0090         SKE(1)
300800970121     C*
300900970121     C                   XFOOT     GAC           W0090             9 0
301000970121     C                   ADD       W0090         SKC(1)
301100970121     C                   XFOOT     LAC           W0090             9 0
301200970121     C                   ADD       W0090         SKC(1)
301300970121     C                   XFOOT     NFC           W0090             9 0
301400970121     C                   ADD       W0090         SKC(1)
301500970121     C                   XFOOT     CTC           W0090             9 0
301600970121     C                   ADD       W0090         SKC(1)
301700970121     C*
301800970121     C                   XFOOT     GAN           W0090             9 0
301900970121     C                   ADD       W0090         SKN(1)
302000970121     C                   XFOOT     LAN           W0090             9 0
302100970121     C                   ADD       W0090         SKN(1)
302200970121     C                   XFOOT     NFN           W0090             9 0
302300970121     C                   ADD       W0090         SKN(1)
302400970121     C                   XFOOT     CTN           W0090             9 0
302500970121     C                   ADD       W0090         SKN(1)
302600970121     C                   EXSR      SCRIVI
302700970121     C*  I N     D I S T I N T A
302800970121     C                   MOVEL     1             SCOLIV
302900970121     C                   MOVEL     '2'           WTOTLI
303000970121     C                   Z-ADD     15            SCOORD
303100970121     C                   MOVEL     'D'           SCOSCO
303200970121     C                   MOVEL     'S'           WANRIT            1
303300970121     C                   MOVEL     ' '           WGBO              1
303400970121     C                   MOVEA     IDE           SKE
303500970121     C                   MOVEA     IDC           SKC
303600970121     C                   MOVEA     IDN           SKN
303700970121     C                   EXSR      SCRIVI
303800970121     C*  I N     C O N S E G N A
303900970121     C                   MOVEL     1             SCOLIV
304000970121     C                   MOVEL     '3'           WTOTLI
304100970121     C                   Z-ADD     17            SCOORD
304200970121     C                   MOVEL     'I'           SCOSCO
304300970121     C                   MOVEL     'S'           WANRIT            1
304400970121     C                   MOVEL     ' '           WGBO              1
304500970121     C                   MOVEA     ICE           SKE
304600970121     C                   MOVEA     ICC           SKC
304700970121     C                   MOVEA     ICN           SKN
304800970121     C                   EXSR      SCRIVI
304900970121     C*  NON    I N     C O N S E G N A
305000970121     C                   MOVEL     1             SCOLIV
305100970121     C                   MOVEL     '3'           WTOTLI
305200970121     C                   Z-ADD     19            SCOORD
305300970121     C                   MOVEL     'O'           SCOSCO
305400970121     C                   MOVEL     'S'           WANRIT            1
305500970121     C                   MOVEL     ' '           WGBO              1
305600970121     C                   MOVEA     NCE           SKE
305700970121     C                   MOVEA     NCC           SKC
305800970121     C                   MOVEA     NCN           SKN
305900970121     C                   EXSR      SCRIVI
306000970121     C*  C O N S E G N A B I L I
306100970121     C                   MOVEL     4             SCOLIV
306200970121     C                   Z-ADD     20            SCOORD
306300970121     C                   MOVEL     'T'           SCOOLT
306400970121     C                   MOVEL     'B'           SCOSCO
306500970121     C                   MOVEL     'N'           WANRIT            1
306600970121     C                   MOVEL     ' '           WGBO              1
306700970121     C                   CLEAR                   SKE
306800970121     C                   CLEAR                   SKC
306900970121     C                   CLEAR                   SKN
307000970121     C                   XFOOT     COE           SKE(1)
307100970121     C                   XFOOT     COC           SKC(1)
307200970121     C                   XFOOT     CON           SKN(1)
307300970121     C                   EXSR      SCRIVI
307400970121     C*
307500970121     C     COE(1)        IFGT      0
307600970121     C     COC(1)        ORGT      0
307700970121     C     CON(1)        ORGT      0
307800970121     C                   MOVEL     1             SCOLIV
307900970121     C                   Z-ADD     21            SCOORD
308000970121     C                   MOVEL     'B'           SCOSCO
308100970121     C                   MOVEL     ' '           SCOOLT
308200970121     C                   Z-SUB     999           SCOORE
308300970121     C                   MOVEL     ' '           SCOGBO
308400970121     C                   Z-ADD     COE(1)        SCOSS1
308500970121     C                   Z-ADD     COC(1)        SCOSS2
308600970121     C                   Z-ADD     CON(1)        SCOSS3
308700970121     C                   WRITE     FNSCO000
308800970121     C                   ENDIF
308900970121     C     COE(2)        IFGT      0
309000970121     C     COC(2)        ORGT      0
309100970121     C     CON(2)        ORGT      0
309200970121     C                   MOVEL     1             SCOLIV
309300970121     C                   Z-ADD     22            SCOORD
309400970121     C                   MOVEL     'B'           SCOSCO
309500970121     C                   MOVEL     ' '           SCOOLT
309600970121     C                   Z-ADD     0             SCOORE
309700970121     C                   MOVEL     ' '           SCOGBO
309800970121     C                   Z-ADD     COE(2)        SCOSS1
309900970121     C                   Z-ADD     COC(2)        SCOSS2
310000970121     C                   Z-ADD     CON(2)        SCOSS3
310100970121     C                   WRITE     FNSCO000
310200970121     C                   ENDIF
310300970121     C     COE(3)        IFGT      0
310400970121     C     COC(3)        ORGT      0
310500970121     C     CON(3)        ORGT      0
310600970121     C                   MOVEL     1             SCOLIV
310700970121     C                   Z-ADD     23            SCOORD
310800970121     C                   MOVEL     'B'           SCOSCO
310900970121     C                   MOVEL     ' '           SCOOLT
311000970121     C                   Z-ADD     999           SCOORE
311100970121     C                   MOVEL     ' '           SCOGBO
311200970121     C                   Z-ADD     COE(3)        SCOSS1
311300970121     C                   Z-ADD     COC(3)        SCOSS2
311400970121     C                   Z-ADD     CON(3)        SCOSS3
311500970121     C                   WRITE     FNSCO000
311600970121     C                   ENDIF
311700970130     C*
311800970130     C                   ENDIF
311900970121     C*
312000970121     C                   ENDSR
312100970121     C**************************************************************************
312200970121     C* SCRIVO I RECORD DI TOTALE IN FNSCO
312300970121     C**************************************************************************
312400970121     C     SCRIVI        BEGSR
312500970121     C*
312600970121    1C     WANRIT        IFEQ      'S'
312700970121     C                   CLEAR                   WREC
312800970121     C*
312900970121    2C                   DO        13            X                 3 0
313000970121    3C     SKE(X)        IFGT      0
313100970121     C     SKC(X)        ORGT      0
313200970121     C     SKN(X)        ORGT      0
313300970121     C*
313400970121     c* se il campo e' -999 o + 999 --> e' l'OLTRE
313500970121     c*  determino di quanto oltre e'
313600970121     C                   SELECT
313700970121    4C     ORE(X)        WHENEQ    -999                                         OLTRE
313800970121     C     X             ADD       1             Y                 3 0
313900970122     C                   MOVEL     'A'           SCOOLT
314000970121     C                   Z-ADD     ORE(Y)        SCOORE
314100970121    4C     ORE(X)        WHENEQ    999                                          OLTRE
314200970121     C     X             SUB       1             Y                 3 0
314300970121     C                   MOVEL     'O'           SCOOLT
314400970121     C                   Z-ADD     ORE(Y)        SCOORE
314500970121     C                   OTHER
314600970122     C                   MOVEL     'D'           SCOOLT
314700970122     C                   Z-ADD     ORE(X)        SCOORE
314800970121    4C                   ENDSL
314900970121     C*
315000970121     C                   MOVEL     WGBO          SCOGBO
315100970121     C                   Z-ADD     SKE(X)        SCOSS1
315200970121     C                   Z-ADD     SKC(X)        SCOSS2
315300970121     C                   Z-ADD     SKN(X)        SCOSS3
315400970121     C                   WRITE     FNSCO000
315500970121     C                   CLEAR                   SCOOLT
315600970121     C                   MOVEL     'S'           WREC              1
315700970121    3C                   ENDIF
315800970121    2C                   ENDDO
315900970121     c**
316000970121     C* SCRIVO ANCHE IL RECORD DI TOTALE se richiesto
316100970121    2C     WTOTLI        IFNE      'N'
316200970121     c* solo se ho gia' scritto almeno un record di affidabilita'
316300970121     C     WREC          ANDEQ     'S'
316400970121     C                   CLEAR                   SCOORE
316500970121     C* l'ordine  di stampa del record di totale e' sempre 1 in meno
316600970121     C*  del dettaglio suddiviso per affidabilita'
316700970121     C                   SUB       1             SCOORD
316800970121     C* il livello della stampa del totale e' spesso diverso dal livell
316900970121     C*  del relativo dettaglio affidabilita', ecco perche' c'e' un
317000970121     C*  campo a parte che viene passato in questa rourine
317100970121     C                   MOVEL     WTOTLI        SCOLIV
317200970121     C* per identificare il record di totale metto un "T" in SCOOLT
317300970121     C                   MOVEL     'T'           SCOOLT
317400970121     C                   XFOOT     SKE           SCOSS1
317500970121     C                   XFOOT     SKC           SCOSS2
317600970121     C                   XFOOT     SKN           SCOSS3
317700970121     C                   WRITE     FNSCO000
317800970121     C                   CLEAR                   SCOOLT
317900970121     C                   ENDIF
318000970121     C* SOLO 1 RECORD se non c'e' suddivisione di addidabilita'
318100970121   X1C                   ELSE
318200970121    2C     SKE(1)        IFGT      0
318300970121     C     SKC(1)        ORGT      0
318400970121     C     SKN(1)        ORGT      0
318500970121     C*
318600970121     C                   CLEAR                   SCOORE
318700970121     C                   MOVEL     WGBO          SCOGBO
318800970121     C                   Z-ADD     SKE(1)        SCOSS1
318900970121     C                   Z-ADD     SKC(1)        SCOSS2
319000970121     C                   Z-ADD     SKN(1)        SCOSS3
319100970121     C                   WRITE     FNSCO000
319200970121     C                   CLEAR                   SCOOLT
319300970121    2C                   ENDIF
319400970121    1C                   ENDIF
319500970121     C*
319600050706     C                   ENDSR
319700050706     C**************************************************************************
319800050706     C* Per bolla locale desumo la data dell'arrivo bolla
319900050706     C**************************************************************************
320000050706     c     BOLLALOC      begsr
320100050706     c     kfvv8         SETGT     FNFVV08L
320200050706     c     kfvv81        READe     FNFVV08L
320300070420    6C                   IF        not %eoF(FNFVV08L) AND FVVDFV>=kdataloc
320400050706    7C                   IF        FVVFCF=' '  or wtipolancio='SA'
320500050706     C                   EVAL      Wartdam=FVVDFV
320600050706    7C                   ENDIF
320700050706     C*
320800050706   X6C                   ELSE
320900070420     C* SE NON TROVATO LE STAMPO SEMPRE CON DATA  + 1
321000070420     C                   Z-ADD     kdataloc      G02INV
321100050706     C                   MOVEL     '3'           G02ERR
321200050706     C                   CALL      'XSRDA8'
321300050706     C                   PARM                    WLBDAT
321400050706     C*
321500050706     C     G02TGI        ADD       1             GIOTGI
321600050706     C                   CALL      'XSRGI8'
321700050706     C                   PARM                    WGIDAT
321800050706     C*
321900050706     C                   Z-ADD     GIOINV        WARTDAM
322000050706    6C                   ENDIF
322100050706     c                   ENDSR
322200060314     C**************************************************************************
322300060314     C* Verifico se giorno festivo
322400060314     C**************************************************************************
322500060314     c     CTRFESTA      begsr
322600060314     c                   clear                   ktfp
322700060314     c                   movel     ds1lna        ktfa
322800060314     C     KCLN          CHAIN     AZCLN01L                           34
322900060314     C     *IN34         IFEQ      *OFF
323000060314     c
323100060314     C     MAT(KGG)      IFEQ      'F'
323200060314     C     POM(KGG)      ANDEQ     'F'
323300060314     c                   eval      wfest='S'
323400060314     c                   else
323500060314     c                   eval      wfest=' '
323600060314     c                   endif
323700060314     c
323800060314     c                   endif
323900060314     c                   ENDSR
324000950123     C**************************************************************************
324100950123     O*  ?  S T A T I S T I C A    C O N S E G N E  ?
324200920605     OPRTF198   E            TESTA            02
324300920605     O                       TE(1)               66
324400940324     O                 N02   TE(2)              132
324500940324     O                  02   TE(3)              132
324600920605     O                       TE(4)              198
324700900523     O                       RSUT                20
324800920415     O                                           21 '/'
324900970207     O                       DS1FEL              24
325000970226     O               15                          47 '- RISTAMPA -'
325100970207     O                       DS1GDA             119 '  /  /    '
325200970207     O                  02   DS1GAL             136 '  /  /    '
325300950123     O                       WDATE              180 '  /  /    '
325400920605     O                       PAGE          Z    198
325500920605     O          E            TESTA       1
325600950123     O                       KNSIF               10
325700950123     O                       KNMUS               22
325800950123     O                       UTIME              179 '  :  :  '
325900940208     O          E            TESTA            04
326000920605     O                       TE(6)               66
326100920605     O                       TE(6)              132
326200920605     O                       TE(6)              198
326300940208     O          E            TESTA            05
326400920605     O                       TE(7)               66
326500920605     O                       TE(10)             132
326600930908     O                       TE(14)             198
326700940208     O          E            TESTA            06
326800920605     O                       TE(8)               66
326900920608     O                       TE(11)             132
327000930908     O                       TE(15)             198
327100970225     O                       DS1LNA              34
327200940323     O                       DESLNA              54
327300940208     O          E            TESTA            07
327400940323     O                       TE(5)               66
327500930812     O                       TE(12)             132
327600930908     O                       TE(16)             198
327700940208     O          E            TESTA            08
327800920608     O                       TE(6)               66
327900920608     O                       TE(6)              132
328000920608     O                       TE(6)              198
328100950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
328200930908     O* 1 INTESTAZIONE
328300940118     O          E            INTE1       2
328400930908     O                       RIGA                66
328500930908     O                       TE(36)             132
328600930908     O                       TE(37)             198
328700940203     O                       GENNOR        Z    106
328800940203     O                                          107 '+'
328900940203     O                       GENESP        Z    115
329000940203     O                                          116 '='
329100940203     O                       GENTOT        Z    124
329200940324     O                 N02   CONP1              130 '0 ,  '
329300940324     O                 N02   CONP2              136 '0 ,  '
329400930909     O*
329500930909     O          E            INTE1       0  0
329600930909     O                       RIGA                66
329700930909     O                       TE(36)             132
329800930909     O                       TE(37)             198
329900940203     O                       GENNOR        Z    106
330000940203     O                                          107 '+'
330100940203     O                       GENESP        Z    115
330200940203     O                                          116 '='
330300940203     O                       GENTOT        Z    124
330400940324     O                 N02   CONP1              130 '0 ,  '
330500940324     O                 N02   CONP2              136 '0 ,  '
330600950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
330700930909     O* 2 INTESTAZIONE
330800940118     O          E            INTE2       2
330900930908     O                       RIGA                66
331000930908     O                       TE(36)             132
331100930908     O                       TE(37)             198
331200940203     O                       GENNOR        Z    145
331300940203     O                                          146 '+'
331400940203     O                       GENESP        Z    154
331500940203     O                                          155 '='
331600940203     O                       GENTOT        Z    163
331700940203     O                       CONP2              169 '0 ,  '
331800930909     O          E            INTE2       0  0
331900930909     O                       RIGA                66
332000930909     O                       TE(36)             132
332100930909     O                       TE(37)             198
332200940203     O                       GENNOR        Z    145
332300940203     O                                          146 '+'
332400940203     O                       GENESP        Z    154
332500940203     O                                          155 '='
332600940203     O                       GENTOT        Z    163
332700940203     O                       CONP2              169 '0 ,  '
332800950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
332900930909     O* 3 INTESTAZIONE
333000940118     O          E            INTE3       2
333100930909     O                       RIGA                66
333200930909     O                       TE(36)             132
333300930909     O                       TE(37)             198
333400940203     O                       GENNOR        Z    178
333500940203     O                                          179 '+'
333600940203     O                       GENESP        Z    187
333700940203     O                                          188 '='
333800940203     O                       GENTOT        Z    196
333900930909     O          E            INTE3       0  0
334000930909     O                       RIGA                66
334100930909     O                       TE(36)             132
334200930909     O                       TE(37)             198
334300940203     O                       GENNOR        Z    178
334400940203     O                                          179 '+'
334500940203     O                       GENESP        Z    187
334600940203     O                                          188 '='
334700940203     O                       GENTOT        Z    196
334800950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
334900930908     O* 1 DETTAGLIO
335000930908     O          E            DET1        1
335100930812     O                       RIGA                66
335200930908     O                       TE(36)             132
335300930908     O                       TE(37)             198
335400930908     O                       ORE(Y)        Z     48
335500970121     O                       PNORM         Z     62
335600930908     O                                           63 '+'
335700970121     O                       PESPR         Z     70
335800930908     O                                           71 '='
335900970121     O                       PTOTAL        Z     79
336000940526     O              N03      CONP1               85 '0 ,  '
336100940526     O              N04N02   CONP2               91 '0 ,  '
336200940324     O                 N02   CONP3               97 '0 ,  '
336300950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
336400930908     O* 2 DETTAGLIO
336500940203     O          E            DET4        1
336600940203     O                                              ' '
336700950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
336800940203     O          E            DET2        1
336900930908     O                       RIGA                66
337000930908     O                       TE(36)             132
337100930908     O                       TE(37)             198
337200930908     O                       GENNOR        Z     62
337300930908     O                       GENESP        Z     70
337400940203     O                                           55 ''''
337500940203     O                                           63 '+'
337600930908     O                                           71 '='
337700930908     O                       GENTOT        Z     79
337800940329     O              N01   N03CONP1               85 '0 ,  '
337900940324     O              N01N02   CONP2               91 '0 ,  '
338000940324     O                 N02   CONP3               97 '0 ,  '
338100940207     O          E   N01      DET2        0  0
338200940207     O                       RIGA                66
338300940207     O                       GENNOR        Z     62
338400940207     O                       GENESP        Z     70
338500940207     O                                           63 '+'
338600940207     O                                           71 '='
338700940207     O                       GENTOT        Z     79
338800940329     O                    N03CONP1               85 '0 ,  '
338900940324     O                 N02   CONP2               91 '0 ,  '
339000940324     O                 N02   CONP3               97 '0 ,  '
339100950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
339200920609     O          E            DET3        2
339300920608     O                       TE(6)               66
339400920608     O                       TE(6)              132
339500920608     O                       TE(6)              198
339600950123     C**************************************************************************
339700950124     O*  ?  L I S T A  ?
339800920706     OQSYSPRT   E            SPED1          1 02
339900950124     O                                         +  3 'ANNO'
340000920706     O                                         +  2 'LNP'
340100920706     O                                         +  2 'SER'
340200920706     O                                         +  2 'SPEDIZ'
340300950124     O                                         +  1 'DATA SPED'
340400920706     O                                         +  2 'COLLI'
340500950124     O                                         +  3 'DATA ARR'
340600950124     O                                         +  2 'ABIL'
340700920706     O                                         +  1 'NUM DIST'
340800920706     O                                         +  2 'DAT DIST'
340900950124     O                                         +  2 'ULT CON'
341000950124     O***************                  +  1 'L.AVVISO'
341100920706     O                                         +  1 'CONSEGNA'
341200950124     O                                         +  3 'CON PARZ'
341300950124     O                                         +  2 'BLOCCO SP'
341400950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
341500920706     OQSYSPRT   E            SPED        1
341600940208     O                       STAFLG
341700940208     O                       ARBAAS            +  2
341800940208     O                       ARBLNP        Z   +  2
341900920706     O                       ARBNRS        Z   +  2
342000920706     O                       ARBNSP        Z   +  2
342100920706     O                       ARBMGS            +  2 '  /  '
342200920706     O                       ARBTSP            +  2
342300920706     O                       ARBNCL        Z   +  2
342400950124     O                       ARBDAM            +  2 '    /  /  '
342500950124     O                       ARBAMA            +  3
342600920706     O                       ARBNDC        Z   +  3
342700950124     O                       ARBDDC            +  2 '    /  /  '
342800950124     O                       ARBTMC            +  3
342900950124     O***************          ARBDLA  +  4 '  /  /  '
343000950124     O                       ARBDCM            +  3 '    /  /  '
343100950124     O                       ARBDCP            +  2 '    /  /  '
343200950123     O                       ARBFBC            +  3
343300950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
343400940203     O          E            SPEDT       1
343500940207     O                                              'CONSEGNATE'
343600940203     O                       STAC          Z   +  1
343700940208     O                                         +  3 'CONSEGNE PARZ'
343800940208     O                       STAP          Z   +  1
343900940208     O                                         +  3 'RISERVA CONS'
344000940208     O                       STAR          Z   +  1
344100940207     O                                         +  4 'IN DISTINTA'
344200940203     O                       STAI          Z   +  1
344300940207     O                                         +  4 'NON CONSEGNATE'
344400940203     O                       STAN          Z   +  1
344500940208     O          E            SPEDT       1
344600940208     O                                              'GIACENZE'
344700940208     O                       STAG          Z   +  1
344800950124     O                                         +  4 'L.AVVISI'
344900950124     O                       STAA          Z   +  1
345000940208     O                                         +  4 'CHIUSE X TURNO'
345100940208     O                       STAT          Z   +  1
345200940208     O                                         +  4 'DA CONSEGNARE'
345300940208     O                       STAD          Z   +  1
345400950123     O**************************************************************************
345500950123     O*  ?  ELENCO  SPEDIZIONI  NON  IN  CONSEGNA  ?
345600940601     OPRTF198X  E            TESELE           02
345700040608     O              n08      ELE(2)             132
345800040608     O              n08      ELE(3)             198
345900040608     O               08      ELE(14)            132
346000040608     O               08      ELE(15)            198
346100940530     O                       RSUT                20
346200940530     O                                           21 '/'
346300970207     O                       DS1FEL              24
346400950123     O                       WDATE              180 '  /  /    '
346500950123     O                       PAGE1         Z    198
346600940530     O          E            TESELE           03
346700950123     O                       KNSIF               10
346800950123     O                       KNMUS               22
346900940601     O                       ELE(4)             132
347000970207     O                       DS1GDA             104 '  /  /    '
347100950123     O                       UTIME              179 '  :  :  '
347200940530     O          E            TESELE           05
347300940601     O                       ELE(5)              66
347400940601     O                       ELE(8)             132
347500940601     O                       ELE(11)            198
347600940530     O          E            TESELE         1 06
347700940601     O                       ELE(6)              66
347800940601     O                       ELE(9)             132
347900940601     O                       ELE(12)            198
348000950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
348100940530     O          E            DETELE         1
348200950127     O                       P0CAAS
348300940530     O                       ARBLNP        Z   +  1
348400940530     O                       ARBNRS        Z   +  1
348500940530     O                       ARBNSP        Z   +  1
348600940530     O                       P0CMGS            +  1 '  /  '
348700940531     O                       ARBTSP            +  1
348800940601     O                       ARBLNA        Z   +  1
348900940601     O                       ARBZNC        Z   +  2
349000991130     O                       P0CRSD            +  1
349100950125     O                       P0CIND            +  1
349200940601     O                       ARBCAD            +  1
349300950127     O                       P0CLOD            +  0
349400940601     O                       ARBPRD            +  1
349500950127     O                       ARBNZD            +  1
349600940601     O                       ARBGC1            +  1
349700940601     O                       ARBGC2            +  1
349800950123     O                       ARBTC1            +  1
349900940601     O                                         +  0 '-'
350000950127     O                       P0DTC1            +  0
350100950123     O                       ARBNCL        Z   +  0
350200950127     O                       ARBPKF        4   +  0
350300991130     O                       ARBCBO            +  1
350400940530     O                                         +  0 '-'
350500950127     O                       P0DCBO            +  0
350600950125     O                       AR9CAS        4   +  0
350700991130     O                       AR9VCA            +  0
350800991130     O                       COMIFT            +  1
350900991130     O                       comdiv            +  0
351000991130     O                       ARBTMC            +  1
351100950123     O***************          P0CDLAZ +  2
351200051128     O                       p0cTCR            +  1
351300940531     O                       P0CDCR        Z   +  1
351400940530     O                       ARBHCR            +  1 '  :  '
351500051107     O*****                  P0CFFD            +  1
351600950123     O                       NUM3          M   +  1
351700950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
351800950127     O          E            DE2ELE         1
351900990329     O               07                             'C.A.'
352000990329     O               07      DCTFIL               8
352100990329     O               07      DCTNCA        Z     15
352200990329     O               07                          19 'del'
352300990329     O               07      STADCA              28 '  /  /  '
352400990329     O               07    06                    35 'chiusa'
352500990329     O               07    06STACHC              44 '  /  /  '
352600990329     O               07    06DCTCCH            +  1
352700990329     O               07    06                  +  0 '-'
352800990329     O               07    06STACCH            +  0
352900991130     O                       ARBTC2             114
353000991130     O                                          115 '-'
353100991130     O                       P0DTC2             125
353200991130     O                       P0CNO1             161
353300991130     O                       P0CNO2             197
353400990329     O          E            DETCA          1
353500990329     O                                              'C.A.'
353600990329     O                       DCTFIL               8
353700990329     O                       DCTNCA        Z     15
353800990329     O                                           19 'del'
353900990329     O                       STADCA              28 '  /  /  '
354000990329     O                     06                    35 'chiusa'
354100990329     O                     06STACHC              44 '  /  /  '
354200990329     O                     06DCTCCH            +  1
354300990329     O                     06                  +  0 '-'
354400990329     O                     06STACCH            +  0
354500060905     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
354600060905      * spedizione in distinta
354700060905     O          E    12      DETDIS         1
354800060905     O                       wstadis            197
354900000703     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
355000060905      * C/ASSEGNO NON ABILITATO
355100000703     O          E            DETCAA         1
355200070419     O              n18                         159 'Non Abilitato'
355300070419     O               18                          53 'Abilitazioni Bolla:'
355400070419     O               18      olrg6vssin       +   1
355500050805     O          E            FINES       2
355600050805     o                                          108 '*** FINE STAMPA ***'
355700950123     O**************************************************************************
355800950125**
355900970226XXXXXXXXXXXXXXXXXXXX/XXX                                  S T A T    1
356000950124I S T I C A    C O N S E G N E    D E L                              2
356100950124I S T I C A    C O N S E G N E    D A L                A L           3
356200950124                      FNLR78R                            PAG. XXXX   4
356300950123                                                      '  DISTR       5
356400930907------------------------------------------------------------------   6
356500950123                                                      '  CORR    E   7
356600950123L I N E A   D I   A R R I V O: XXX-XXXXXXXXXXXXXXXXXXX'    +         8
356700930907                                                      'XXXXXXX+XXX   9
356800950123SPR   TOTALE ' % SU' % SU' % SU'   CORR    ESPR    TOTALE ' % SU'
356900950123             'CONS.'TOTAL'CONSE'     +                    'TOTAL'C
357000950123             'PERIO'IN CO' GNAB'   DISTR                  'IN CO'
357100940203XXXX=XXXXXXXX'XX,XX'XX,XX'XX,XX'XXXXXXXX+Xxxxxxxx=XXXXXXXX'XX,XX'X
357200950123% SU'   CORR     ESPR   TOTALE ' % SU'   CORR     ESPR   TOTALE
357300950123ONSE'     +                    'CONSE'     +
357400950123GNAB'   DISTR                  ' GNAB'   DISTR
357500940203X,XX'Xxxxxxxx+xXxxxxxx=xxxxxxxx'XX,XX'xXxxxxxx+xxxXxxxx=xxxxxxxx
357600970130SPEDIZIONI           C O N S E G N A T E:             '
357700940203                           In Anticipo oltre xxx Ore  '             19
357800940203                           In Anticipo di    xxx Ore  '             20
357900940323                           In CONFORMITA'             '              1
358000940203                           In Ritardo  di    xxx Ore  '              2
358100940203                           In Ritardo  oltre xxx Ore  '              3
358200970130SPEDIZIONI   N O N   C O N S E G N A T E:             '             24
358300940203              di cui  "GIACENZE APERTE"               '             25
358400940203              di cui  "LASCIATI AVVISI"               '             26
358500940203              di cui  "NON  FATTE"                    '             27
358600940203              di cui  "CHIUSE PER TURNO"              '             28
358700970130SPEDIZIONI   ANCORA  I N   D I S T I N T A:           '             29
358800970130TOTALE SPED.         I N   C O N S E G N A:           '             30
358900970130SPEDIZIONI   N O N   I N   C O N S E G N A:           '             31
359000970130TOTALE SPED. C O N S E G N A B I L I:                 '             32
359100940323                                       In Anticipo    '             33
359200940323                                       In CONFORMITA' '             34
359300940323                                       In Ritardo     '             35
359400940203             '     '     '     '                          '     '   36
359500940203    '                          '     '                              37
359600940208              di cui  "CONSEGNE PARZIALI"             '             38
359700940208              di cui  "RISERVE  CONSEGNE"             '             39
359800940324** Comandi    567890123456789012345678901234567890123456789012345678901234567890
359900940324OVRPRTF FILE(PRTF198)  USRDTA('Stat.Cons.')
360000940530** Comandi    567890123456789012345678901234567890123456789012345678901234567890
360100940601OVRPRTF FILE(PRTF198X) USRDTA('Stat.Cons.')
360200940530**
360300940601XXXXXXXXXXXXXXXXXXXX/XXX                                             1
360400940601         **  ELENCO  SPEDIZIONI  NON  IN  CONSEGNA  **               2
360500950125                       FNLR78R                           PAG. XXXX   3
360600950125                        AL  XX/XX/XXXX                               4
360700991130   S P E D I Z I O N E     ARRIVO      D    E    S    T    I   N     5
360800991130                          LIN ZON RAGIONE SOCIALE INDIRIZZO          6
360900950125                                                                     7
361000991130  A    T    A    R    I    O              GG   CONSEG COLLI    PES   8
361100991130         C.A.P    LOCALITA'      PRO NAZ CHIUS PARTIC                9
361200950125                                                                    10
361300051107O CD.BOLLA     C/ASSEGNO       TOTALE   MAN CONSEGNA           ORE  11
361400051107                              FATTURA   CON RICHIESTA         RIT.  12
361500950125                                                                    13
361600040608      **  ELENCO SPEDIZIONI NON IN CONSEGNA: considera spunte part  14
361700040608enza  **                  FNLR78R                        PAG.       15

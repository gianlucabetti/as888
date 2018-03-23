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
001300151008     FFiAR501L  IF   E           K DISK
001400991104     FFIAR601L  IF   E           K DISK
001500051115     FFiAR901L  IF   E           K DISK
001600961108     FFNARB01L  IF   E           K DISK
001700961108     F                                     RENAME(FNARB000:FNARB001)
001800050331     FTigcp21L  IF   E           K DISK
001900961108     FFNLBL02L  IF   E           K DISK
002000930812     F* EVENTI
002100990805     FFNEVB01L  IF   E           K DISK
002200930910     F*
002300900523     FAZORG01L  IF   E           K DISK
002400000703     FFNSCO04L  UF A E           K DISK
002500000703     FFNSCO03L  IF   E           K DISK
002600000703     F                                     RENAME(FNSCO000:FNSCO003)
002700970124     FTABEL00F  UF   E           K DISK
002800021017     fFista01l  uf   e           k Disk
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
004300090918     D TE              S             66    DIM(50) CTDATA PERRCD(1)             RIGHE STATISTICA
004400940530     D CMD             S             43    DIM(1) CTDATA PERRCD(1)              OVR PRTF198
004500940601     D CM2             S             43    DIM(1) CTDATA PERRCD(1)              OVR PRTF198X
004600040608     D ELE             S             66    DIM(15) CTDATA PERRCD(1)             RIGHE ELENCO
004700940530     D*
004800950123     D REC             S              2    DIM(30)                              BOLLE RECUP
004900091112     D CEP             S              3    DIM(150)
005000940210     D*
005100930907     D ORE             S              3  0 DIM(13)                              ORE ANT/RIT
005200930908     D*
005300090917     D ESP             S              9  0 DIM(13)                              SKI COM "E"
005400090917     D H10             S              9  0 DIM(13)                              SKI COM "H"
005500090917     D NOR             S              9  0 DIM(13)                              SKI COM "C+D"
005600970121     D TOT             S              9  0 DIM(13)                              SKI COM TOT
005700970121     D*
005800970121     D SKE             S              9  0 DIM(13)                              SKI COM ESPR
005900090918     D SKC             S              9  0 DIM(13)                              SKI COM "C"
006000970121     D SKN             S              9  0 DIM(13)                              SKI COM DIST
006100090918     D SKh             S              9  0 DIM(13)                              SKI COM "H"
006200930907     D*
006300090917     D CNE             S              9  0 DIM(13)                              CONS.TE "E"
006400090917     D CNC             S              9  0 DIM(13)                              CONS.TE "C"
006500090917     D CNN             S              9  0 DIM(13)                              CONS.TE "D"
006600090917     D CNh             S              9  0 DIM(13)                              CONS.TE "H"
006700930907     D*
006800090917     D ICE             S              9  0 DIM(13)                              IN CONS "E"
006900090917     D ICC             S              9  0 DIM(13)                              IN CONS "C"
007000090917     D ICN             S              9  0 DIM(13)                              IN CONS "D"
007100090917     D ICh             S              9  0 DIM(13)                              CONS.TE "H"
007200930908     D*
007300970121     D NCE             S              9  0 DIM(13)                              NO IN C ESPR
007400970121     D NCC             S              9  0 DIM(13)                              NO IN C DIST
007500970121     D NCN             S              9  0 DIM(13)                              NO IN C NORM
007600090916     D NCH             S              9  0 DIM(13)                              NO IN C NORM
007700930907     D*
007800970121     D GAE             S              9  0 DIM(13)                              GIAC AP ESPR
007900970121     D GAC             S              9  0 DIM(13)                              GIAC AP CORR
008000970121     D GAN             S              9  0 DIM(13)                              GIAC AP NORM
008100090916     D GAh             S              9  0 DIM(13)                              GIAC AP NORM
008200930907     D*
008300970121     D LAE             S              9  0 DIM(13)                              LASC AV ESPR
008400970121     D LAC             S              9  0 DIM(13)                              LASC AV CORR
008500970121     D LAN             S              9  0 DIM(13)                              LASC AV NORM
008600090916     D LAh             S              9  0 DIM(13)                              LASC AV NORM
008700930907     D*
008800970121     D NFE             S              9  0 DIM(13)                              NON FAT ESPR
008900970121     D NFC             S              9  0 DIM(13)                              NON FAT CORR
009000970121     D NFN             S              9  0 DIM(13)                              NON FAT NORM
009100090916     D NFh             S              9  0 DIM(13)                              NON FAT NORM
009200930907     D*
009300970121     D CTE             S              9  0 DIM(13)                              CHIU TU ESPR
009400970121     D CTC             S              9  0 DIM(13)                              CHIU TU CORR
009500970121     D CTN             S              9  0 DIM(13)                              CHIU TU NORM
009600090916     D CTh             S              9  0 DIM(13)                              CHIU TU NORM
009700930907     D*
009800970121     D IDE             S              9  0 DIM(13)                              IN DIST ESPR
009900970121     D IDC             S              9  0 DIM(13)                              IN DIST CORR
010000970121     D IDN             S              9  0 DIM(13)                              IN DIST NORM
010100090916     D IDh             S              9  0 DIM(13)                              IN DIST NORM
010200940208     D*
010300970121     D RSE             S              9  0 DIM(1)                               RIS CON ESPR
010400970121     D RSC             S              9  0 DIM(1)                               RIS CON CORR
010500970121     D RSN             S              9  0 DIM(1)                               RIS CON NORM
010600090916     D RSh             S              9  0 DIM(1)                               RIS CON NORM
010700940208     D*
010800970121     D PRE             S              9  0 DIM(1)                               CON PAR ESPR
010900970121     D PRC             S              9  0 DIM(1)                               CON PAR CORR
011000970121     D PRN             S              9  0 DIM(1)                               CON PAR NORM
011100090916     D PRh             S              9  0 DIM(1)                               CON PAR NORM
011200930907     D*
011300970121     D COE             S              9  0 DIM(3)                               CONS.LI ESPR
011400970121     D COC             S              9  0 DIM(3)                               CONS.LI CORR
011500970121     D CON             S              9  0 DIM(3)                               CONS.LI NORM
011600090916     D COh             S              9  0 DIM(3)                               CONS.LI NORM
011700950314RM*  D*
011800950314RM*  D L6              S              3  0 DIM(30)                              FIL GEST.ARRIVI
011900000703     D L1              S              3  0 DIM(30)                              FIL GESTITE
012000930907     D*
012100040908     D knpg            S                   LIKE(fvvnpg) inz(2)
012200151008     D ktrd            S                   LIKE(ar5trd) inz('EMD')
012300040908     D Wtstdec         S                   LIKE(§CVfdc)
012400040527     D Wartdam         S                   LIKE(artdam)
012500060314     d Dataiso         s               d   datfmt(*iso)
012600060314     d Dataisofine     s               d   datfmt(*iso)
012700050706     D wtipolancio     s              2
012800050706     D W013A           s             13
012900991130     D IMPeditato      s             13
013000991130     D w0130           s             13  0
013100991130     D w0122           s             12  2
013200021021     d Wfil            s                   Like(StaFil)
013300040812     d wesca           s              1
013400060314     d wfest           s              1
013500100222     d p0dpart         s             54
013600970130     D*
013700961112     D ARBDS         E DS                  EXTNAME(FNARB00F)
013800961112     D*
013900961112     D WSAVDS          DS           700
014000961112     D*
014100970207     D                 DS
014200970207     D  KDATA                  1      8  0
014300970207     D  KAAA                   1      4  0
014400970207     D  KMMM                   5      6  0
014500970207     D  KGG                    7      8  0
014600940530     D                 DS
014700950124     D  DATA                   1      8  0
014800950124     D  AA                     3      4  0
014900950124     D  MM                     5      6  0
015000950124     D  GG                     7      8  0
015100950124     D  MMGG                   5      8  0
015200970130     D CLNMAT          DS
015300970130     D  MAT                    1     31
015400970130     D                                     DIM(31)                              GG CALEND MATT
015500970130     D CLNPOM          DS
015600970130     D  POM                    1     31
015700970130     D                                     DIM(31)                              GG CALEND POMEI
015800930812     D* DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
015900970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
016000920611     D DS2A          E DS
016100091112     D DS2Z          E DS
016200940210     D DS3A          E DS
016300151008     D Dar5emd       E DS
016400930907     D* VARIABILI ARRIVI
016500930907     D DS7A          E DS
016600930907     D  ANT                   58     72  0
016700930907     D                                     DIM(5)                               ORE ANTICIPO
016800930907     D  RIT                   73     87  0
016900930907     D                                     DIM(5)                               ORE RITARDO
017000970124     D DS7AF           DS
017100970124     D  INUTIL                 1     67
017200970124     D  ALFDPE                68     75
017300970124     D  §7ACPE                68     75  0
017400970124     D  ALFDUE                76     83
017500970124     D  §7ACUE                76     83  0
017600950127     D WLBDAT          DS
017700950127     D  G02DAT                 1      8  0
017800950127     D  G02INV                 9     16  0
017900950127     D  G02ERR                17     17
018000950127     D  G02TGI                18     22  0
018100970124     D WGIDAT          DS
018200970124     D  GIODAT                 1      8  0
018300970124     D  GIOINV                 9     16  0
018400970124     D  GIOTGI                17     21  0
018500990329     D                 DS
018600990329     D  DCTDCA                 1      8  0
018700990329     D  DCTAAC                 1      4  0
018800990329     D  DCTMGC                 5      8  0
018900990326     D DTAD            DS
019000990326     D  §TARGR                26     26
019100990329     D DCCH            DS
019200990329     D  §CHDES                 1     50
019300990329     D  §CHIMA                53     53
019400970207     D* PASSAGGIO DATI DA FILTRO
019500970207     D DSLS01        E DS
019600151008     D* DS1RIS= S - ELABORA E 18AMPA QUELLO INDICATO DA DS1FL1
019700970225     D*       = R -         RISTAMPA QUELLO INDICATO DA DS1FL1
019800970225     D*       =   - ELABORA E  IGNORA  IL  CAMPO        DS1FL1
019900970225     D* DS1FL1= S - STAMPA LA STATISTICA
020000970225     D*       = L - STAMPA LA LISTA SPEDIZIONI NON IN CONSEGNA
020100970225     D*       = E - STAMPA LA STATISTICA E LA LISTA
020200970225     D* DS1FL2= S - STAMPA LA LISTA DELLE SPEDIZIONI PREVISTE IN STATIS
020300970225     D*             PER UN CONTROLLO
020400100311     D* DS1FL3='L'- devo stampare la lista spedizioni non in consegna
020500040608     d*             anche se ds1ris=*blanks
020600040608     D* DS1FL4='E'- non si tratta di una vera ristampa ma di stampa
020700040608     D*             dopo aver elaborato i dati
020800040608     D* DS1FL5= S - considera spunta partenza
020900050706     D* DS1FL6= S - escludi supermercati e appuntamenti del giorno
021000100218     D* DS1FL7= S - escludi partic.consegna ATTESA se non raggiunta data
021100100311     D* DS1FL8= N - Non aggiornare la tabella ultima elaborazione
021200970207     D*
021300970207     D CNCR80        E DS
021400900511     D KPJBA         E DS
021500950314RM*  D* DS PER TRUL06R - CARICAMENTO £X
021600950314RM*  D DSUL06        E DS                  EXTNAME(TRUL06DS)
021700950314RM*  D  LIN                    1     90  0
021800950314     D                                     DIM(30)                              FIL. COMODO
021900940118     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
022000990329     D DSBS02        E DS                  EXTNAME(TIBS02DS)
022100991130     D DSCV          E DS
022200000531     D OG143         E DS
022300070419     D OG148         E DS
022400040526     D FNLV53DS      E DS
022500051128     D fnlv80ds      E DS
022600151008     D FNLRG7DS      E DS
022700090917     d dvpoDECO      e ds
022800090604     d ds5e          e ds
022900100218     d ds7R          e ds
023000110926     d fnlr78ds1     e ds
023100141119     d fnlry09ds2    e ds
023200110926     d
023300040812      * ds per il controllo della presenza di CA per la spedizione richiesta
023400040812     d FIDN12DS      e ds
023500040812     d  skKey                 26    305    dim(20)
023600040812     d  skAnn                306    325    dim(20)
023700040812     d  skDca                326    485  0 dim(20)
023800040812     d  skFca                486    545  0 dim(20)
023900040812     d  skDch                546    705  0 dim(20)
024000040812     d  skCch                706    745    dim(20)
024100040812
024200040812     d dsKey           ds
024300040812     d  dsaac                         4  0
024400040812     d  dsfil                         3  0
024500040812     d  dsnca                         7  0
024600040812      *
024700060905     D Cindistinta     C                   CONST('In Distinta n.xxxxx-
024800060905     D                                     xx  del xx/xx/xx')
024900100222     D CEscludiPar     C                   CONST('Escludi partConsegna"AT-
025000100222     D                                     TESA"per ConsRich.non raggiunta')
025100900523     C****************************************************************
025200900523     C*  RIEPILOGO INDICATORI
025300920306     C****************************************************************
025400940207     C* 01    - CONTROLLO LA STAMPA DELLE % NEL TOTALE CONSEGNABILE
025500940323     C* 02    - IMMESSA DATA AL
025600940329     C* 03    - STAMPO LA % SUL TOT.CONSEGNATO SOLO PER LE CONSEGNATE
025700040608     C* 04    -
025800040608     C* 05    - EVENTO RISERVA CONSEGNA
025900990329     C* 06-07 - PER STAMPARE LE C.A.
026000040608     C* 08    - lista non in consegna con spunte partenza
026100090916     C* 10    - SPEDIZIONE "E"- priority
026200090916     C* 11    - SPEDIZIONE "C" - corriere
026300090916     C* 12    -
026400090916     C* 13    -
026500970226     C* 15    - SI TRATTA DI UNA RISTAMPA
026600970226     C* 16    - LA SPEDIZIONE E' NON CONSEGNATA
026700940329     C* 17    - LA SPEDIZIONE NON E' COMPLETAMENTE ARRIVATA
026800070419     C* 18    - GEO attiva
026900110923     C* 19    - Perfect order
027000090916     C* 20    - SPEDIZIONE "D" distribuzione
027100090916     C* 21    - SPEDIZIONE "H" H1030
027200151008     C* 22    - controalert inviato
027300090917     C* 25    - servizio h1030 attivo
027400090918     C* 26    - non stampo la % finale
027500110923     C* 28    - Stampo le abilitazioni
027600920605     C* 30    - COMODO
027700950314RM*  C* 36    - FILIALE NON GESTITA DA SECONDO LIVELLO
027800950123     C* 34    - LETTURA FNARB54L
027900141119     C* 70    - Presente dipo di dirottamento
028000940117     C* 99    - STAMPA LA LISTA DI CONTROLLO
028100900523     C*****************************************************************
028200000000     C     *ENTRY        PLIST
028300000000     C                   PARM                    KPJBA
028400970207     C                   MOVEL     KPJBU         DSLS01
028500970225     C*
028600990805     C*DATA  PER CONFRONTARE CON GLI EVENTI
028700990805     C                   Z-ADD     DS1AAL        CO1DAA            8 0
028800970225     C*DATA DA SUDDIVIDERE IN AAAA MM GG
028900970207     C                   MOVEL     DS1ADA        KDATA
029000970225     C* SE DS1RIS = ' 'IGNORO IL CAMPO DS1FL1
029100970225     C     DS1RIS        IFEQ      ' '
029200970225     C                   CLEAR                   DS1FL1
029300970225     C                   ENDIF
029400040608     c*
029500040608     c* lista sped non in consegna con spunte partenza
029600040608     c                   if        ds1fl5='S'
029700040608     c                   seton                                        08
029800040608     c                   endif
029900970207     C*
030000000000     C                   Z-ADD     1             CODUT
030100900523     C                   CALL      'X§PARUT'
030200900523     C                   PARM                    UT§DSE
030300000000     C                   MOVEL     RAGUT         RSUT             20
030400920415     C                   MOVEL     REC80         CNCR80
030500000703      * CARICO L1
030600900511     C*---------------------------------------------------------------*
030700930812     C     KARB          KLIST
030800970225     C                   KFLD                    DS1LNA
030900950123     C                   KFLD                    NUM6              8 0
031000961108     C     KARB1         KLIST
031100961108     C                   KFLD                    LBLAAN
031200961108     C                   KFLD                    LBLLPN
031300961108     C                   KFLD                    LBLNRN
031400961108     C                   KFLD                    LBLNSN
031500930812     C     KARB2         KLIST
031600990805     C                   KFLD                    AR§AAS
031700930812     C                   KFLD                    ARBLNP
031800930812     C                   KFLD                    ARBNRS
031900930812     C                   KFLD                    ARBNSP
032000990805     C                   KFLD                    NUM6A
032100970417     C     KARB22        KLIST
032200990805     C                   KFLD                    AR§AAS            4 0
032300970417     C                   KFLD                    ARBLNP
032400970417     C                   KFLD                    ARBNRS
032500970417     C                   KFLD                    ARBNSP
032600990805     C                   KFLD                    NUM6A             8 0
032700970417     C                   KFLD                    NUMORA
032800940324     C     KARB3         KLIST
032900940324     C                   KFLD                    ARBAAS
033000940324     C                   KFLD                    ARBLNP
033100940324     C                   KFLD                    ARBNRS
033200940324     C                   KFLD                    ARBNSP
033300991104     C     KARB6         KLIST
033400991104     C                   KFLD                    ARBAAS
033500991104     C                   KFLD                    ARBLNP
033600991104     C                   KFLD                    ARBNRS
033700991104     C                   KFLD                    ARBNSP
033800991104     C                   KFLD                    KTRC
033900151008     C     KAR5          KLIST
034000151008     C                   KFLD                    ARBAAS
034100151008     C                   KFLD                    ARBLNP
034200151008     C                   KFLD                    ARBNRS
034300151008     C                   KFLD                    ARBNSP
034400151008     C                   KFLD                    KTRd
034500990326     C     KART          KLIST
034600990326     C                   KFLD                    ARTFLS
034700990326     C                   KFLD                    ARTLNA
034800990326     C                   KFLD                    ARTNRS
034900990326     C                   KFLD                    ARTNSC
035000950127     C     KAR41         KLIST
035100950127     C                   KFLD                    ARBAAS
035200950127     C                   KFLD                    ARBLNP
035300950127     C                   KFLD                    ARBNRS
035400950127     C                   KFLD                    ARBNSP
035500950127     C                   KFLD                    AR4TRC
035600950123     C     KARB4         KLIST
035700950123     C                   KFLD                    AR§AAS
035800950123     C                   KFLD                    ARBLNP
035900950123     C                   KFLD                    ARBNRS
036000950123     C                   KFLD                    ARBNSP
036100920611     C     KTAB          KLIST
036200920611     C                   KFLD                    CODUT
036300920611     C                   KFLD                    COD               2
036400920611     C                   KFLD                    KEY               8
036500940210     C     KTAB2         KLIST
036600940210     C                   KFLD                    CODUT
036700940210     C                   KFLD                    COD               2
036800970122     C     KSCO1         KLIST
036900000703     C************       KFLD                    KFLE
037000970121     C                   KFLD                    KLNA
037100970121     C                   KFLD                    KDCM
037200970122     C     KSCO2         KLIST
037300000703     C************       KFLD                    KFLE
037400970122     C                   KFLD                    KLNA
037500970122     C                   KFLD                    KSCO
037600970122     C                   KFLD                    KGBO
037700970122     C                   KFLD                    KDCM
037800970122     C                   KFLD                    KOLT
037900970122     C     KSCO21        KLIST
038000000703     C************       KFLD                    KFLE
038100970122     C                   KFLD                    KLNA
038200970122     C                   KFLD                    KSCO
038300970122     C                   KFLD                    KGBO
038400970122     C     KSCO22        KLIST
038500000703     C************       KFLD                    KFLE
038600970122     C                   KFLD                    KLNA
038700970122     C                   KFLD                    KSCO
038800970122     C                   KFLD                    KGBO
038900970122     C                   KFLD                    KDCM
039000970130     C     KCLN          KLIST
039100970130     C                   KFLD                    KTFP
039200970130     C                   KFLD                    KTFA
039300970130     C                   KFLD                    KAAA
039400970130     C                   KFLD                    KMMM              2 0
039500990329     C     KDCT          KLIST
039600990329     C                   KFLD                    DCTAAC
039700990329     C                   KFLD                    DCTFIL
039800990329     C                   KFLD                    DCTNCA
039900021017
040000021017     c     Kfista        Klist
040100021017     c                   Kfld                    KTRC
040200021017     c                   Kfld                    Simfel
040300021017     c                   Kfld                    Wfil
040400021017     c                   Kfld                    KDTS
040500040527     C     Kfvv          KLIST
040600040527     C                   KFLD                    D53NPA
040700040527     C                   KFLD                    D53NFA
040800040527     C                   KFLD                    D53LAI
040900040908     C     Kfvv8         KLIST
041000040908     C                   KFLD                    arbtfa
041100040908     C                   KFLD                    knpg
041200070420     C                   KFLD                    kdataloc
041300040908     C     Kfvv81        KLIST
041400040908     C                   KFLD                    arbtfa
041500040908     C                   KFLD                    knpg
041600971128     C*
041700020211     C     *LIKE         DEFINE    arblnp        savlnp
041800020211     C     *LIKE         DEFINE    EVBHEV        NUMORA
041900970417     C     *LIKE         DEFINE    ARBLNA        KTFA
042000970130     C     *LIKE         DEFINE    ARBLNA        KTFP
042100970130     C     *LIKE         DEFINE    STATRC        KTRC
042200970124     C     *LIKE         DEFINE    STADTS        KDTS
042300970124     C     *LIKE         DEFINE    STANCL        KNCL
042400970124     C     *LIKE         DEFINE    SCOOLT        KOLT
042500970122     C     *LIKE         DEFINE    SCOSCO        KSCO
042600970122     C     *LIKE         DEFINE    SCOGBO        KGBO
042700970122     C     *LIKE         DEFINE    ARBDCM        KDCM
042800970121     C     *LIKE         DEFINE    ARBLNA        KLNA
042900070420     C     *LIKE         DEFINE    fvvdfv        Kdataloc
043000000703     C**** *LIKE         DEFINE    DS1FEL        KFLE
043100991130     C     *LIKE         DEFINE    AR6DIV        SAVDIV
043200991130     C     *LIKE         DEFINE    AR6DIV        comdiv
043300961112     C*
043400930812     C*---------------------------------------------------------------*
043500930812     C* 1°ASSIOMA: SE LA SPEDIZIONE E' CONSEGNATA
043600930812     C*            COME DATA DISTINTA PRENDO LA DATA CONSEGNA
043700930812     C* 2°ASSIOMA: C'E' SEMPRE UN EVENTO CREATO A FRONTE DI OGNI
043800930812     C*            MANCATA CONSEGNA
043900950123     C* 3°ASSIOMA: ESCLUDO LE BOLLE CON DATA CONSEGNA MA IN FNARB000
044000940208     C*            PERCHE' SONO CONSEGNE ANOMALE CHE ASPETTATO LA DEFI-
044100940208     C*            NIZIONE DELLA FIGLIA
044200930812     C*---------------------------------------------------------------*
044300940117     C* LISTA SPEDIZIONI
044400970207     C     DS1FL2        IFEQ      'S'
044500940117     C                   SETON                                        99
044600940117     C                   EXCEPT    SPED1
044700940117     C                   ENDIF
044800930907     C* OPERAZIONI INIZIALI
044900930910     C                   EXSR      INIZIO
045000060110     c
045100060111     C***  DS1LNA        IFLE      0
045200060111     C***                CLEAR                   DSUL06
045300060111     C**+                MOVE      '£1'          D06COD
045400060111     C***                MOVEL     SIMFEL        D06KEY
045500060111     C***                MOVEL     DSUL06        KPJBU
045600060111     C***                CALL      'TRUL06R'
045700060111     C***                PARM                    KPJBA
045800060111     C***                MOVEL     KPJBU         DSUL06
045900060111     C***                MOVEA     LIN           L1                             TUTTE
046000060110      *                  --------------------------
046100060111     C***                ELSE
046200060110     C***                Z-ADD     DS1LNA        L1(1)                          UNA A VIDEO
046300060110     C                   movea     l6            L1                             UNA A VIDEO
046400060110      *                  -----------------------------
046500060111     C***                ENDIF
046600970122     C*
046700940329     C     *IN98         IFEQ      *OFF
046800920428     C** ELABORAZIONE
046900920430     C*
047000000703    1C     1             DO        30            WX                3 0
047100000703    2C     L1(WX)        IFNE      0
047200970122RM*  C*
047300970122RM*  C                   SETOFF                                       36
047400970122RM*  C*
047500060111     c* Impossibile lanciare pgm senza la linea di arrivo impostata
047600060111     c*  aseterisco le specifiche con 09 = *off
047700060111RM*  C**   *IN09         IFEQ      *OFF
047800060111     c**   simtpp        andeq     '2'
047900060111     c**   *in09         oreq      *off
048000060111     c**   simtpp        andeq     *blanks
048100060111RM*  C**   L1(WX)        LOOKUP    L6                                     30
048200060111RM*  C**N30              SETON                                        36
048300060111RM*  C**                 END
048400970122RM*  C*
048500970122RM*2AC     *IN36         IFEQ      *OFF
048600920430     C*
048700920604     C* MEMORIZZO FILIALE DI ARRIVO
048800000703     C                   Z-ADD     L1(WX)        DS1LNA
048900971128     C     DS1LNA        CHAIN     AZORG                              33
049000940208     C                   MOVEL     ORGDES        DESLNA           19
049100970122     C*
049200970122   2AC                   SELECT
049300970207     C     DS1RIS        WHENEQ    'R'
049400980513     C* SE E' IMPOSTATO DS1FL4 VUOL DIRE CHE NON SI TRATTA DI
049500980513     C*  RISTAMPA VERA E PROPRIA MA DI UNA RISTAMPOA SUCCESSIVA
049600980513     C*  ALLA ELABORAZIONE DEI DATI PURA
049700980513     C     DS1FL4        IFEQ      ' '
049800970226     C                   SETON                                        15
049900980513     C                   ENDIF
050000980513     C**
050100970122     C                   EXSR      ELASAL
050200970122  X2AC                   OTHER
050300920604     C*
050400950124     C* ?  S P E D I Z I O N I    C O N S E G N A T E  ?
050500930908     C*
050600970207     C                   Z-ADD     DS1ADA        NUM6
050700950123     C     KARB          SETLL     FNARB54L
050800950123     C  N02KARB          READE     FNARB54L                               34
050900970225     C   02DS1LNA        READE     FNARB54L                               34
051000000602      *
051100940324    3C     *IN34         DOWEQ     *OFF
051200000602     C*
051300940210     C* ESCLUDO I TIPI BOLLA RECUPERO
051400000609     C     ARBCBO        LOOKUP    REC                                    35
051500000609    4C     *IN35         IFEQ      *OFF
051600940324     C* SOLO SE DATA DAL AL CONTROLLO SE SUPERA LIMITE MAGGIORE
051700940324    5C     *IN02         IFEQ      *ON
051800970207     C     ARBDCM        ANDGT     DS1AAL
051900950124     C                   SETON                                        34
052000940323     C                   ELSE
052100090917     C                   EXSR      CONT1
052200940324    5C                   ENDIF
052300940324    4C                   ENDIF
052400920604     C*
052500000609     C  N02KARB          READE     FNARB54L                               34
052600970225     C  N34
052700970225     CAN 02DS1LNA        READE     FNARB54L                               34
052800940324    3C                   ENDDO
052900970130     C* SE NON HO TROVATO NESSUNA SPEDIZIONE CONSEGNATA
053000970130     C*  VERIFICO SE E' UN GIORNO FESTIVO: IN QUESTO CASO
053100970130     C*  NON ELABORO IL RESTO DELLA STATISTICA E SCRIVO UN RECORD
053200970130     C*  VUOTO CON F DI FESTIVO
053300970130     C     WCONS         IFEQ      ' '
053400060314     C                   MOVEL     DS1ADA        KDATA
053500060314     c                   exsr      CTRFesta
053600970130     C                   ENDIF
053700970130     C*
053800920605     C*
053900950124     C* ?  S P E D I Z I O N I   N O N   A N C O R A   C O N S E G N A T E  ?
054000930908     C*
054100970130     C* ELABORO SOLTANTO SE HO TROVATO ALMENO UNA SPEDIZIONE CONSEGNATA
054200970130     C*  E SE NON CE NE SONO MA SI TRATTA DI UN GIORNO LAVORATIVO
054300970130   2BC     WFEST         IFEQ      ' '
054400970130     C*
054500920605     C                   Z-ADD     0             NUM6
054600950123     C     KARB          SETLL     FNARB54L
054700950123     C     KARB          READE     FNARB54L                               34
054800920605     C*
054900940324    3C     *IN34         DOWEQ     *OFF
055000000609     C*
055100940210     C* ESCLUDO I TIPI BOLLA RECUPERO
055200950123     C     ARBCBO        LOOKUP    REC                                    35
055300940324    4C     *IN35         IFEQ      *OFF
055400940324     C* 02 OFF
055500940324     C* SE DATA ARRIVO > DATA DAL --> E' PER FORZA NON CONSEGNABILE
055600940324     C* CONTO LE NON CONSEGNATE E LE CONSEGNABILI
055700970207    5C  N02ARBDAM        IFLE      DS1ADA
055800090917     C                   EXSR      CONT4
055900940324    5C                   ENDIF
056000940324     C* 02 ON
056100940324     C* SE DATA ARRIVO > DATA AL  --> E' PER FORZA NON CONSEGNABILE
056200940324     C* CONTO SOLO LE RISERVE E LE CONSEGNE PARZIALI
056300970207    5C   02ARBDAM        IFLE      DS1AAL
056400090917     C                   EXSR      CONT3P
056500940324    5C                   ENDIF
056600940324    4C                   ENDIF
056700920605     C*
056800950123     C  N34KARB          READE     FNARB54L                               34
056900940324    3C                   ENDDO
057000920605     C*
057100950124     C* ?  S P E D I Z I O N I    C O N S E G N A T E    D O P O  ?
057200930908     C*
057300970207     C  N02              Z-ADD     DS1ADA        NUM6
057400970207     C   02              Z-ADD     DS1AAL        NUM6
057500940324     C*
057600950124     C     KARB          SETGT     FNARB54L
057700970225     C     DS1LNA        READE     FNARB54L                               34
057800920605     C*
057900970130    3C     *IN34         DOWEQ     *OFF
058000940210     C*
058100940210     C* ESCLUDO I TIPI BOLLA RECUPERO
058200950123     C     ARBCBO        LOOKUP    REC                                    35
058300940324    4C     *IN35         IFEQ      *OFF
058400940324     C* 02 OFF
058500970207    5C  N02ARBDAM        IFLE      DS1ADA
058600090917     C                   EXSR      CONT4
058700940324    5C                   ENDIF
058800940324     C* 02 ON
058900970207    5C   02ARBDAM        IFLE      DS1AAL
059000090917     C                   EXSR      CONT3P
059100940324    5C                   ENDIF
059200940324    4C                   ENDIF
059300920605     C*
059400970225     C     DS1LNA        READE     FNARB54L                               34
059500970130    3C                   ENDDO
059600970130   2BC                   ENDIF
059700940323     C*
059800920605     C*
059900970121     C* ?  S C R I V O    L A   S T A T I S T I C A  ?
060000930908     C*
060100970305     C* SOLO SE HO CHIESTO ANCHE LA STATISTICA
060200970305   2BC     DS1FL1        IFNE      'L'
060300970403     C     DS1RIS        ANDNE     'R'
060400970305     C**
060500970122     C* CANCELLO TUTTI I RECORD GIA' PRESENTI NEL FILE
060600970225     C                   MOVEL     DS1LNA        KLNA
060700970207     C                   Z-ADD     DS1ADA        KDCM
060800000703     C     KSCO1         CHAIN     FNSCO04L                           30
060900970122    3C     *IN30         DOWEQ     *OFF
061000970121     C                   DELETE    FNSCO000
061100000703     C     KSCO1         READE     FNSCO04L                               30
061200970122    3C                   ENDDO
061300970121     C**
061400970121     C                   EXSR      WRTSCO
061500970124     C**
061600100311     c** Se richiesto non aggiorno la stampa
061700100311     c
061800100311   3ac                   if        ds1fl8<>'N'
061900100311     c
062000970124     C** VEDO SE AGGIORNARE LA DATA PRIMA E ULTIMA ELABORAZIONE
062100970124     C                   MOVEL     '7A'          COD
062200970225     C                   MOVEL(P)  DS1LNA        KEY
062300970124     C     KTAB          CHAIN     TABEL                              30
062400100311     C* LA TABELLA CI DEVE ESSERE PER FORZA PERCHE' LA CREA SE MANCA
062500970124     C*  IL FILTRO
062600970124    3C     *IN30         IFEQ      *OFF
062700970124     C                   MOVEL     TBLUNI        DS7AF
062800970124    4C     ALFDUE        IFLT      *ZEROS
062900970124     C                   MOVEL     *ZEROS        ALFDUE
063000970124    4C                   ENDIF
063100970124    4C     ALFDPE        IFLT      *ZEROS
063200970124     C                   MOVEL     *ZEROS        ALFDPE
063300970124    4C                   ENDIF
063400970124     C*
063500021021     C* DATA RIELABORATA: DEVO AGGIORNARE FISTA
063600970207    5C     DS1ADA        IFGE      §7ACPE
063700970207    5C     DS1ADA        ANDLE     §7ACUE
063800970124     C                   MOVEL     'C'           KTRC
063900970207     C                   MOVEL     DS1ADA        KDTS
064000021017     c                   Movel     Ds1Lna        Wfil
064100021022     c     Kfista        Chain     Fista01l                           30
064200021022     c     *In30         DowEq     *Off
064300021022     c                   Delete    Fista01l
064400021022     c     Kfista        Reade     Fista01l                               30
064500021022     c                   EndDo
064600970124     C                   ENDIF
064700970124     C*
064800970124     C* SE MANCA LA PRIMA DATA ELABORAZIONE: AGGIORNO
064900970124    4C     §7ACPE        IFEQ      0
065000970207     C                   MOVEL     DS1ADA        ALFDPE
065100970124   X4C                   ELSE
065200970124     C* SE E' MINORE DI 1 GIORNO DELLA DATA PRIMA ELABORAZIONE
065300970124     C*  AGGIORNO ALTRIMENTI NO
065400970207    5C     DS1ADA        IFLT      §7ACPE
065500970207     C                   Z-ADD     DS1ADA        G02INV
065600970124     C                   MOVEL     '3'           G02ERR
065700970124     C                   CALL      'XSRDA8'
065800970124     C                   PARM                    WLBDAT
065900970124     C* TRASFORMO LA DATA IN GIORNI E ADDIZIONO 1
066000970124     C     G02TGI        ADD       1             GIOTGI
066100970124     C                   CALL      'XSRGI8'
066200970124     C                   PARM                    WGIDAT
066300970124    6C     GIOINV        IFEQ      §7ACPE
066400970207     C                   Z-ADD     DS1ADA        §7ACPE
066500970124    6C                   ENDIF
066600970124    5C                   ENDIF
066700970124    4C                   ENDIF
066800970124     C*
066900970124     C* SE MANCA L'ULTIMA DATA ELABORAZIONE: AGGIORNO
067000970124    4C     §7ACUE        IFEQ      0
067100970207     C                   MOVEL     DS1ADA        ALFDUE
067200970124   X4C                   ELSE
067300970124     C* SE E' MAGGIORE DI 1 GIORNO DELLA DATA ULTIMA ELABORAZIONE
067400970124     C*  AGGIORNO ALTRIMENTI NO
067500970207    5C     DS1ADA        IFGT      §7ACUE
067600970207     C                   Z-ADD     DS1ADA        G02INV
067700970124     C                   MOVEL     '3'           G02ERR
067800970124     C                   CALL      'XSRDA8'
067900970124     C                   PARM                    WLBDAT
068000970124     C* TRASFORMO LA DATA IN GIORNI E SOTTRAGGO 1
068100970124     C     G02TGI        SUB       1             GIOTGI
068200970124     C                   CALL      'XSRGI8'
068300970124     C                   PARM                    WGIDAT
068400970124    6C     GIOINV        IFEQ      §7ACUE
068500970207     C                   Z-ADD     DS1ADA        §7ACUE
068600970124    6C                   ENDIF
068700970124     C*
068800970124    5C                   ENDIF
068900970124    4C                   ENDIF
069000970124     C*
069100970124     C                   MOVEL     DS7AF         TBLUNI
069200970124     C                   UPDATE    TABEL
069300970124    3C                   ENDIF
069400100311   3aC                   ENDIF
069500970403   2BC                   ENDIF
069600970124     C*
069700970122   2AC                   ENDSL
069800970121     C*
069900970121     C* ?  S T A M P A    S T A T I S T I C A  ?
070000970225    3C     DS1FL1        IFEQ      'S'
070100970225    3C     DS1FL1        OREQ      'E'
070200920605     C                   EXSR      STAMPA
070300970122    3C                   ENDIF
070400060118     C*
070500060118     C* AZZERO CAMPI PER NUOVA LINEA DI ARRIVO
070600060118     C                   SETOFF                                       0103
070700060118     C                   CLEAR                   CNE
070800060118     C                   CLEAR                   CNN
070900060118     C                   CLEAR                   CNC
071000090918     C                   CLEAR                   CNh
071100090918     C                   CLEAR                   H10
071200090918     C                   CLEAR                   ESP
071300060118     C                   CLEAR                   NOR
071400060118     C                   CLEAR                   TOT
071500060118     C                   CLEAR                   ICE
071600060118     C                   CLEAR                   ICN
071700060118     C                   CLEAR                   ICC
071800090918     C                   CLEAR                   ICh
071900060118     C                   CLEAR                   GAE
072000060118     C                   CLEAR                   GAN
072100060118     C                   CLEAR                   GAC
072200090918     C                   CLEAR                   GAh
072300060118     C                   CLEAR                   LAE
072400060118     C                   CLEAR                   LAN
072500060118     C                   CLEAR                   LAC
072600090918     C                   CLEAR                   LAh
072700060118     C                   CLEAR                   NFE
072800060118     C                   CLEAR                   NFN
072900060118     C                   CLEAR                   NFC
073000090918     C                   CLEAR                   NFh
073100060118     C                   CLEAR                   CTE
073200060118     C                   CLEAR                   CTN
073300060118     C                   CLEAR                   CTC
073400090918     C                   CLEAR                   CTh
073500060118     C                   CLEAR                   IDE
073600060118     C                   CLEAR                   IDN
073700060118     C                   CLEAR                   IDC
073800090918     C                   CLEAR                   IDh
073900060118     C                   CLEAR                   COE
074000060118     C                   CLEAR                   CON
074100060118     C                   CLEAR                   COC
074200090918     C                   CLEAR                   COh
074300060118     C                   CLEAR                   PRE
074400060118     C                   CLEAR                   PRN
074500060118     C                   CLEAR                   PRC
074600090918     C                   CLEAR                   PRh
074700060118     C                   CLEAR                   RSE
074800060118     C                   CLEAR                   RSN
074900060118     C                   CLEAR                   RSC
075000090918     C                   CLEAR                   RSh
075100060118     C                   CLEAR                   NCE
075200060118     C                   CLEAR                   NCN
075300060118     C                   CLEAR                   NCC
075400090918     C                   CLEAR                   NCh
075500060118     C                   CLEAR                   GENNOR
075600060118     C                   CLEAR                   GENESP
075700090918     C                   CLEAR                   GENh10
075800060118     C                   CLEAR                   GENTOT
075900970122     C*
076000970122RM*2AC                   END
076100000703    2C                   ENDIF
076200980707    1C                   ENDDO
076300940203     C*
076400940324     C   99              EXCEPT    SPEDT
076500050805     C* STAMPO dicitura * fine stampa * in LISTA SPEDIZIONI NON IN CONSEGNA
076600050805     C  N02DS1FL1        IFEQ      'L'
076700050805     C     DS1FL1        OREQ      'E'
076800050805     C     DS1FL3        OREQ      'L'
076900050805     c                   if        *inod = *on
077000050805     c                   except    tesele
077100050805     c                   endif
077200050805     c                   except    fines
077300050805     C                   ENDIF
077400940324     C                   ENDIF
077500920504     C*
077600990329     C                   CLEAR                   DSBS02
077700990329     C                   MOVEL     'C'           T02TLA
077800990329     C                   CALL      'TIBS02R'
077900990329     C                   PARM                    KPJBA
078000990329     C                   PARM                    DSBS02
078100040526     C*
078200040526     C                   CLEAR                   FNLV53DS
078300040526     C                   MOVEL     'C'           D53TLA
078400040526     C                   CALL      'FNLV53R'
078500040526     C                   PARM                    FNLV53DS
078600151008     c
078700151008     C                   CLEAR                   fnlrg7ds
078800151008     C                   MOVEL     'C'           ilrg7tla
078900151008     C                   CALL      'FNLRG7R'
079000151008     C                   PARM                    FNLrg7ds
079100141119     c
079200141119     c                   clear                   fnlry09ds2
079300141119     c                   eval      ilry09tla= 'C'
079400141119     c                   eval      kpjbu=fnlry09ds2
079500141119     c                   call      'FNLRY09C'
079600141119     c                   parm                    kpjba
079700141119
079800040526     c
079900900523     C                   SETON                                        LR
080000991130      **********************************************************************
080100991130      * EDITO IMPORTI PER CAMPI in stampa
080200991130      **********************************************************************
080300991130     C     EDTIMPORTI    BEGSR
080400991130
080500991130      * -----------------------------------------------------------------------------------
080600991130      * W0122      contiene il numero da editare
080700991130      * WTSTDEC    corrisponde a §CVfdc della tabella CV, vale "S" se il num
080800991130      *            ero ha decimali
080900991130      * IMPEDITATO ritorna il valore editato per la stampa lungo13 (troncate
081000991130      *            cifre oltre 10 mld)
081100991130      * -----------------------------------------------------------------------------------
081200991130
081300991130     c                   clear                   IMPeditato
081400991130     c                   clear                   W013A
081500991130     c                   if        Wtstdec = 'S'
081600991130     C                   eval      W013a = %EDITC(W0122:'4')
081700991130     c                   else
081800991130     c                   z-add     W0122         W0130
081900991130     C                   eval      W013a = %EDITC(W0130:'4')
082000991130     c                   endif
082100991130     c
082200991130     c                   move      W013a         IMPeditato
082300991130     c                   ENDSR
082400950124     C**************************************************************************
082500950124     C* OPERAZIONI INIZIALI
082600950124     C**************************************************************************
082700930910     C     INIZIO        BEGSR
082800950123     C*
082900950123     C                   TIME                    WTIME            14 0          ORA & DATA
083000950123     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
083100950124     C                   MOVEL     WTIME         UTIME             6 0          ORARIO
083200950127     C*
083300950127     C* INVERTO LA DATA DEL GIORNO
083400950127     C                   Z-ADD     WDATE         G02DAT
083500950127     C                   Z-ADD     *ZERO         G02INV
083600950127     C                   MOVEL     *BLANKS       G02ERR
083700950127     C                   CALL      'XSRDA8'
083800950127     C                   PARM                    WLBDAT
083900950127     C                   Z-ADD     G02INV        WDATE1            8 0          AAAA/MM/GG
084000930910     C*
084100940323     C* 02 ON - IMMESSA DATA AL
084200970207     C     DS1AAL        IFNE      0
084300970207     C     DS1ADA        IFNE      DS1AAL
084400940323     C                   SETON                                        02
084500940323     C                   END
084600940323     C                   ELSE
084700970207     C                   Z-ADD     DS1ADA        DS1AAL
084800940323     C                   END
084900930907     C*
085000930907     C* 09 ON - LINEA ARRIVO IMMESSA
085100060111     C***  DS1LNA        COMP      0                                  09
085200930907     C*
085300930910     C* REPERISCO VARIABILI ARRIVI
085400930907     C                   MOVEL     '7A'          COD
085500930907     C                   MOVEL     *BLANKS       KEY
085600930907     C                   MOVEL     '1'           KEY
085700970124     C     KTAB          CHAIN(N)  TABEL                              31
085800930907     C  N31              MOVEL     TBLUNI        DS7A
085900930907     C   31              CLEAR                   DS7A
086000930907     C* CARICO UNICA SCHIERA DEI RITARDI E CONSEGNE
086100930907     C                   Z-ADD     5             X                 3 0
086200930907     C                   Z-ADD     2             Y                 3 0
086300930907     C                   Z-ADD     -999          ORE(1)
086400930907     C*
086500930907     C                   DO        5
086600930907     C     ANT(X)        IFGT      0
086700930907     C     0             SUB       ANT(X)        ORE(Y)
086800930907     C                   ADD       1             Y
086900930907     C                   ENDIF
087000930907     C                   SUB       1             X
087100930907     C                   ENDDO
087200930907     C*
087300930907     C                   Z-ADD     1             X                 3 0
087400930907     C*
087500930907     C                   DO        5
087600930907     C     RIT(X)        IFGT      0
087700930907     C                   ADD       1             Y
087800930907     C                   Z-ADD     RIT(X)        ORE(Y)
087900930907     C                   ENDIF
088000930907     C                   ADD       1             X
088100930907     C                   ENDDO
088200930907     C*
088300930907     C                   ADD       1             Y
088400930907     C                   Z-ADD     999           ORE(Y)
088500940210     C* CARICO I TIPI BOLLA RECUPERO CHE DEVO SCARTARE
088600940210     C                   Z-ADD     1             X
088700940210     C                   MOVEL     '3A'          COD
088800970124     C     KTAB2         CHAIN(N)  TABEL                              30
088900940210     C*
089000940210     C     *IN30         DOWEQ     *OFF
089100940210     C     TBLFLG        IFEQ      ' '
089200940210     C                   MOVEL     TBLUNI        DS3A
089300940210     C     §3ARBL        IFEQ      'R'
089400940210     C                   MOVEL     TBLKEY        REC(X)
089500940210     C                   ADD       1             X
089600940210     C                   ENDIF
089700940210     C                   ENDIF
089800940210     C*
089900970124     C     KTAB2         READE(N)  TABEL                                  30
090000940210     C                   ENDDO
090100091112     c
090200091112     C* CARICO gli eventi particolari
090300091112     C                   Z-ADD     1             X
090400091112     C                   MOVEL     '2Z'          COD
090500091112     C     KTAB2         CHAIN(N)  TABEL                              30
090600091112     C*
090700091112     C     *IN30         DOWEQ     *OFF
090800091112     C     TBLFLG        IFEQ      ' '
090900091112     C                   MOVEL     TBLUNI        DS2Z
091000091112     C     §2zcep        IFne      ' '
091100091112     C                   MOVEL     TBLKEY        CEP(X)
091200091112     C                   ADD       1             X
091300091112     C                   ENDIF
091400091112     C                   ENDIF
091500091112     C*
091600091112     C     KTAB2         READE(N)  TABEL                                  30
091700091112     C                   ENDDO
091800930907     C*
091900091112     c*
092000940530     C* OVRPRTF SU PRTF198
092100091112     C                   Z-ADD     43            LUNG             15 5
092200970225     C     DS1FL1        IFEQ      'S'
092300970225     C     DS1FL1        OREQ      'E'
092400940324     C                   MOVEA     CMD           COMMAN           80
092500940324     C*
092600940329     C                   CALL      'QCMDEXC'                              98                A
092700940324     C                   PARM                    COMMAN
092800940324     C                   PARM                    LUNG
092900940329     C  N98              OPEN      PRTF198
093000950410     C                   END
093100940601     C* OVRPRTF SU PRTF198X
093200970225     C  N02
093300970225     CANN98DS1FL1        IFEQ      'L'
093400970225     C     DS1FL1        OREQ      'E'
093500970403     C     DS1FL3        OREQ      'L'
093600940530     C                   MOVEA     CM2           COMMAN           80
093700940530     C*
093800940530     C                   CALL      'QCMDEXC'                              98                A
093900940530     C                   PARM                    COMMAN
094000940530     C                   PARM                    LUNG
094100940601     C  N98              OPEN      PRTF198X
094200940530     C                   ENDIF
094300020429     C* SE RICHIESTA STATISTICA PER TUTTE LE FILIALI GESTITE
094400020429     C* E SE SONO UN SECONDO LIVELLO CARICO LA £6
094500060110     c***  simtpp        ifeq      '2'
094600060110     c***  simtpp        oreq      *blanks
094700060110     C***  DS1LNA        ifeq      *ZEROS
094800950314RM*  C                   CLEAR                   DSUL06
094900950314RM*  C                   MOVE      '£6'          D06COD
095000060111     C***                movel     simpou        D06KEY
095100060111     C                   movel     ds1lna        D06KEY
095200950314RM*  C                   MOVEL     'L'           D06TLA
095300060118 RM* C                   MOVEL     ds1aal        D06drf
095400060118 RM* C                   MOVE      'S'           D06key
095500950314RM*  C                   MOVEL     DSUL06        KPJBU
095600950314RM*  C                   CALL      'TRUL06R'
095700950314RM*  C                   PARM                    KPJBA
095800950314RM*  C                   MOVEL     KPJBU         DSUL06
095900950314RM*  C                   MOVEA     LIN           L6
096000060110RM*  C***                END
096100060110     C***                END
096200970128     C*
096300970130     C                   CLEAR                   WCONS
096400970130     C                   CLEAR                   WFEST
096500100222     c*
096600100222     C**                 CLEAR                   dsbs02
096700100222     c**                 clear                   dvpodeco
096800100222     C**                 MOVEL     'C'           T02MOD
096900100222     C**                 MOVEL     KNSIF         T02SIF
097000100222     C**                 MOVEL     'VPO'         T02COD
097100100222     c**                 movel(P)  'DECO'        t02ke1
097200100222     C**                 CALL      'TIBS02R'
097300100222     C**                 PARM                    KPJBA
097400100222     C**                 PARM                    dsbs02
097500100222    2C**                 IF        T02ERR = *BLANKS
097600100222     C**                 MOVEL     T02uni        dvpodeco
097700100222    2C**                 ENDIF
097800090917    c
097900090917     c* Accendo indicatore 25 de devo stampare anche tipo servizio H
098000100222     c**                 if        ds1aal>=§vpotsph
098100090917     c                   seton                                        25
098200100222     c**                 endif
098300070419     c**
098400070419     c* Verifico se ha GEO attiva
098500070419     C     DS1LNA        CHAIN     AZORG                              33
098600070419     c                   movel     orgde8        og148
098700070419     c                   if        §oggeot='S'
098800070419     c                   seton                                        18
098900070419     c                   endif
099000090604
099100090604      * carico le descrizioni fisse dei tipi servizio
099200090604      * chiodo la descrizione per ogni tipo servizio visto che la descrizione
099300090604      * è da stampare nella testata ed è fissa
099400090604      * le colonne dei tipi servizio da stampare sono
099500090917      * 'C + D' 'E' "H"
099600090604     c                   clear                   ds5e
099700090604     c                   clear                   wdtspc            5
099800090604     c                   clear                   wdtspd            5
099900090604     c                   clear                   wdtspe            5
100000090917     c                   clear                   wdtsph            5
100100090604     c                   eval      cod = '5E'
100200090604     c     ktab2         setll     tabel00f
100300090604     c                   do        *hival
100400090604     c     ktab2         reade(n)  tabel00f
100500090604     c                   if        %eof(tabel00f)
100600090604     c                   leave
100700090604     c                   endif
100800090604     c                   eval      ds5e = tbluni
100900090604     c                   if        tblkey = 'C'
101000090604     c                   eval      wdtspc = §5ed05
101100090604     c                   endif
101200090604     c                   if        tblkey = 'D'
101300090604     c                   eval      wdtspd = §5ed05
101400090604     c                   endif
101500090604     c                   if        tblkey = 'E'
101600090604     c                   eval      wdtspe = §5ed05
101700090604     c                   endif
101800090917     c                   if        tblkey = 'H'
101900090917     c                   eval      wdtsph = §5ed05
102000090917     c                   endif
102100090604     c                   enddo
102200971117     C***
102300971117     C                   SETON                                        OEOFOD
102400950124     C*
102500100222     c* Verifico se devo escludere le partic.consegna di attesa non raggiunte
102600100222     c                   if        ds1fl7='S'
102700100222     c                   eval      p0dpart=CEscludiPar
102800100222     c                   endif
102900930907     C                   ENDSR
103000970122     C**************************************************************************
103100970122     C* ELABORO LA STATISTICA PARTENDO DAI SALDI
103200970122     C**************************************************************************
103300970122     C     ELASAL        BEGSR
103400970122     C* S P E D I Z I O N I   C O N S E G N A T E
103500000703     C*********          MOVEL     DS1FEL        KFLE
103600970225     C                   MOVEL     DS1LNA        KLNA
103700970207     C                   MOVEL     DS1ADA        KDCM
103800970122     C*
103900970122     C                   MOVEL     'C'           KSCO
104000970122     C                   MOVEL     'A'           KOLT
104100970122     C                   MOVEL     ' '           KGBO
104200970122     C*
104300970122     C                   EXSR      RIEMPI
104400970122     C*
104500970122     C* C O N S E G N E   P A R Z I A L I
104600970122     C                   MOVEL     'C'           KSCO
104700970122     C                   MOVEL     ' '           KOLT
104800970122     C                   MOVEL     'P'           KGBO
104900970122     C                   EXSR      RIEMPI
105000970122     C* R I S E R V E   C O N S E G N E
105100970122     C                   MOVEL     'C'           KSCO
105200970122     C                   MOVEL     ' '           KOLT
105300970122     C                   MOVEL     'R'           KGBO
105400970122     C                   EXSR      RIEMPI
105500970122     C* SOLO SE RICHEISTA UNA SOLA DATA, CONTINUO A MEMORIZZATE LE SKI
105600970122     C     *IN02         IFEQ      *OFF
105700970122     C*
105800970122     C*  N O N   C O N S E G N A T E
105900970122     C*          DI CUI GIACENZE APERTE
106000970122     C                   MOVEL     'N'           KSCO
106100970122     C                   MOVEL     'A'           KOLT
106200970122     C                   MOVEL     'G'           KGBO
106300970122     C                   EXSR      RIEMPI
106400970122     C*          DI CUI LASCIATI AVVISI
106500970122     C                   MOVEL     'N'           KSCO
106600970122     C                   MOVEL     'A'           KOLT
106700970122     C                   MOVEL     'L'           KGBO
106800970122     C                   EXSR      RIEMPI
106900970122     C*          DI CUI CHIUSE PER TURNO
107000970122     C                   MOVEL     'N'           KSCO
107100970122     C                   MOVEL     'A'           KOLT
107200970122     C                   MOVEL     'C'           KGBO
107300970122     C                   EXSR      RIEMPI
107400970122     C*          DI CUI NON FATTE
107500970122     C                   MOVEL     'N'           KSCO
107600970122     C                   MOVEL     'A'           KOLT
107700970122     C                   MOVEL     'N'           KGBO
107800970122     C                   EXSR      RIEMPI
107900970122     C*  I N     D I S T I N T A
108000970122     C                   MOVEL     'D'           KSCO
108100970122     C                   MOVEL     'A'           KOLT
108200970122     C                   MOVEL     ' '           KGBO
108300970122     C                   EXSR      RIEMPI
108400970122     C* I N   C O N S E G N A
108500970122     C                   MOVEL     'I'           KSCO
108600970122     C                   MOVEL     'A'           KOLT
108700970122     C                   MOVEL     ' '           KGBO
108800970122     C                   EXSR      RIEMPI
108900970122     C* NON   I N   C O N S E G N A
109000970124     C                   MOVEL     'O'           KSCO
109100970122     C                   MOVEL     'A'           KOLT
109200970122     C                   MOVEL     ' '           KGBO
109300970122     C                   EXSR      RIEMPI
109400970122     C*  C O N S E G N A B I L I
109500970122     C                   MOVEL     'B'           KSCO
109600970122     C                   MOVEL     ' '           KOLT
109700970122     C                   MOVEL     ' '           KGBO
109800970122     C                   EXSR      RIEMPI
109900970122     C                   ENDIF
110000970122     C*
110100970122     C                   ENDSR
110200970122     C**************************************************************************
110300970122     C* RIEMPO LE SCHIERE DAL FILE SALDI FNSCO00F
110400970122     C**************************************************************************
110500970122     C     RIEMPI        BEGSR
110600000703     C     KSCO2         SETLL     FNSCO03L
110700000703     C  N02KSCO22        READE     FNSCO03L                               30
110800000703     C   02KSCO21        READE     FNSCO03L                               30
110900970122    1C     *IN30         DOWEQ     *OFF
111000970122     C*
111100970122     C* E' DENTRO LE DATE DI CONSEGNA IMMESSE
111200970122    2C     *IN02         IFEQ      *OFF
111300970207     C     SCODCM        ORLE      DS1AAL
111400970122     C* ESCUDO I RECORD DI TOTALE
111500970122    3C     SCOOLT        IFNE      'T'
111600970122     C     SCOOLT        ANDNE     'F'
111700970122     C* IMPOSTO PER CERCARE IL POSTO NELLA SKIERA DELLE ORE
111800970122    4C                   SELECT
111900970122     C     SCOOLT        WHENEQ    'A'
112000970122     C                   Z-SUB     999           NUM3
112100970122     C     SCOOLT        WHENEQ    'O'
112200970122     C                   Z-ADD     999           NUM3
112300970122     C     SCOOLT        WHENEQ    'D'
112400970122     C                   Z-ADD     SCOORE        NUM3
112500970122     C                   OTHER
112600970122     C* NON CI SONO ORE PER L'AFFIDABILITA'
112700970122     C                   Z-ADD     1             B
112800970122    4C                   ENDSL
112900970122     C*
113000970122     C     SCOOLT        IFNE      ' '
113100970122     C                   EXSR      CALRIT
113200970122     C                   ENDIF
113300970122     C*
113400970122     C                   SELECT
113500970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
113600970122     C     SCOGBO        ANDEQ     ' '
113700970310     C                   ADD       SCOSS1        CNE(B)
113800970310     C                   ADD       SCOSS2        CNC(B)
113900970310     C                   ADD       SCOSS3        CNN(B)
114000090916     C                   ADD       SCOSS4        CNh(B)
114100970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
114200970122     C     SCOGBO        ANDEQ     'P'                                          CONS PARZIAL
114300970310     C                   ADD       SCOSS1        PRE(B)
114400970310     C                   ADD       SCOSS2        PRC(B)
114500970310     C                   ADD       SCOSS3        PRN(B)
114600090916     C                   ADD       SCOSS4        PRh(B)
114700970122     C     SCOSCO        WHENEQ    'C'                                          CONSEGNATE
114800970122     C     SCOGBO        ANDEQ     'R'                                          RISERVE CONS
114900970310     C                   ADD       SCOSS1        RSE(B)
115000970310     C                   ADD       SCOSS2        RSC(B)
115100970310     C                   ADD       SCOSS3        RSN(B)
115200090916     C                   ADD       SCOSS4        RSh(B)
115300970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
115400970122     C     SCOGBO        ANDEQ     'G'                                          GIACENZE AP
115500970310     C                   ADD       SCOSS1        GAE(B)
115600970310     C                   ADD       SCOSS2        GAC(B)
115700970310     C                   ADD       SCOSS3        GAN(B)
115800090916     C                   ADD       SCOSS4        GAh(B)
115900970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
116000970122     C     SCOGBO        ANDEQ     'L'                                          LASCIATI AVV
116100970310     C                   ADD       SCOSS1        LAE(B)
116200970310     C                   ADD       SCOSS2        LAC(B)
116300970310     C                   ADD       SCOSS3        LAN(B)
116400090916     C                   ADD       SCOSS4        LAh(B)
116500970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
116600970122     C     SCOGBO        ANDEQ     'C'                                          CHIUSE TURNO
116700970310     C                   ADD       SCOSS1        CTE(B)
116800970310     C                   ADD       SCOSS2        CTC(B)
116900970310     C                   ADD       SCOSS3        CTN(B)
117000090916     C                   ADD       SCOSS4        CTh(B)
117100970122     C     SCOSCO        WHENEQ    'N'                                          NOCONSEGNATE
117200970122     C     SCOGBO        ANDEQ     'N'                                          NON FATTE
117300970310     C                   ADD       SCOSS1        NFE(B)
117400970310     C                   ADD       SCOSS2        NFC(B)
117500970310     C                   ADD       SCOSS3        NFN(B)
117600090916     C                   ADD       SCOSS4        NFh(B)
117700970122     C     SCOSCO        WHENEQ    'D'                                          IN DISTINTA
117800970122     C     SCOGBO        ANDEQ     ' '
117900970310     C                   ADD       SCOSS1        IDE(B)
118000970310     C                   ADD       SCOSS2        IDC(B)
118100970310     C                   ADD       SCOSS3        IDN(B)
118200090916     C                   ADD       SCOSS4        IDh(B)
118300970122     C     SCOSCO        WHENEQ    'I'                                          IN CONSEGNA
118400970122     C     SCOGBO        ANDEQ     ' '
118500970310     C                   ADD       SCOSS1        ICE(B)
118600970310     C                   ADD       SCOSS2        ICC(B)
118700970310     C                   ADD       SCOSS3        ICN(B)
118800090916     C                   ADD       SCOSS4        ICh(B)
118900970122     C     SCOSCO        WHENEQ    'O'                                          NOINCONSEGNA
119000970122     C     SCOGBO        ANDEQ     ' '
119100970310     C                   ADD       SCOSS1        NCE(B)
119200970310     C                   ADD       SCOSS2        NCC(B)
119300970310     C                   ADD       SCOSS3        NCN(B)
119400090916     C                   ADD       SCOSS4        NCh(B)
119500970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
119600970122     C     SCOORE        ANDEQ     -999                                         IN ANTICIPO
119700970310     C                   ADD       SCOSS1        COE(1)
119800970310     C                   ADD       SCOSS2        COC(1)
119900970310     C                   ADD       SCOSS3        CON(1)
120000090916     C                   ADD       SCOSS4        COh(1)
120100970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
120200970122     C     SCOORE        ANDEQ     0                                            IN CONFORMIT
120300970310     C                   ADD       SCOSS1        COE(2)
120400970310     C                   ADD       SCOSS2        COC(2)
120500970310     C                   ADD       SCOSS3        CON(2)
120600090916     C                   ADD       SCOSS4        COh(2)
120700970122     C     SCOSCO        WHENEQ    'B'                                          CONSEGNABILI
120800970122     C     SCOORE        ANDEQ     999                                          IN RITARDO
120900970310     C                   ADD       SCOSS1        COE(3)
121000970310     C                   ADD       SCOSS2        COC(3)
121100970310     C                   ADD       SCOSS3        CON(3)
121200090918     C                   ADD       SCOSS4        COh(3)
121300970122     C                   ENDSL
121400970122     C*
121500970122    3C                   ENDIF
121600970122     C*
121700000703     C  N02KSCO22        READE     FNSCO03L                               30
121800000703     C   02KSCO21        READE     FNSCO03L                               30
121900970122   X2C                   ELSE
122000970122     C                   SETON                                        30
122100970122    2C                   ENDIF
122200970122    1C                   ENDDO
122300970122     C                   ENDSR
122400950124     C**************************************************************************
122500950124     C*    CONTO LE SPEDIZIONI CONSEGNATE
122600950124     C**************************************************************************
122700920604     C     CONT1         BEGSR
122800950124     C*
122900090917     C* Tipo servizio --> imposta indicatore
123000090917     c                   exsr      IndtipoServ
123100930907     C*
123200950123     C* RICHIAMO PGM DI CALCOLO AFFIDABILITA'
123300950227     C* SE MANCA LA DATA TEORICA LA CALCOLO
123400950227    1C     ARBDCE        IFEQ      0
123500970925     C                   CLEAR                   DS99
123600930908     C                   EXSR      CALDAT
123700950227   X1C                   ELSE
123800950227     C                   Z-ADD     ARBNCI        NUM3                            NON CONF.CONSEGNA
123900950227    1C                   ENDIF
124000930907     C*
124100930908     C* CERCO LO SCAGLIONE DEL RITARDO
124200930907     C                   EXSR      CALRIT
124300930907     C*
124400930812     C* C O N S E G N A T E
124500090916     C   10              ADD       1             CNE(B)                          "E"
124600090916     C   11              ADD       1             CNC(B)                          "C"
124700090916     C
124800090916     C   20              ADD       1             CNN(B)                          "D"
124900090916     C   21              ADD       1             CNh(B)                          "H"
125000970130     C*
125100970130     C* TROVATA ALMENO UNA SPEDIZIONE CONSEGNATA
125200970130     C                   MOVEL     'S'           WCONS             1
125300940117     C*
125400940117     C   99              MOVEL     'C'           STAFLG
125500940203     C   99              ADD       1             STAC              7 0
125600940117     C   99              EXCEPT    SPED                                          S P E D I Z
125700930907     C*
125800930907     C* IN    C O N S E G N A
125900090916     C   10              ADD       1             ICE(B)                          "E"
126000090916     C   11              ADD       1             ICC(B)                          "C"
126100090916     C
126200090916     C   20              ADD       1             ICN(B)                          "D"
126300090916     C   21              ADD       1             ICh(B)                          "H"
126400920604     C*
126500930812     C* C O N S E G N A B I L I
126600930907     C                   EXSR      CALCON
126700940208     C*
126800940208     C*  VEDO SE C'E' ANCHE QUALCHE CONSEGNA NON FATTA LO STESSO GIORNO
126900970417     C***********          EXSR CONT3
127000950124     C*
127100930907     C                   ENDSR
127200090917     C**************************************************************************
127300090917     C* Accendi indicatore a seconda del tipo servizio della bolla
127400090917     C**************************************************************************
127500090917     c     IndTipoSERV   BEGSR
127600090917     C                   SETOFF                                       1011
127700090917     C                   SETOFF                                       2021
127800090917
127900090917     C***  ARBTSP        COMP      'E'                                    10
128000090917     C***  ARBTSP        COMP      'C'                                    11
128100090917
128200090917     c                   select
128300090917     c                   when      arbtsp='E'
128400090917     c                   seton                                        10
128500090917     c                   when      arbtsp='C'
128600090917     c                   seton                                        11
128700090917     c                   when      arbtsp='H'
128800090917     c                   seton                                        21
128900090917     c                   other
129000090917     c                   seton                                        20
129100090917     c                   endsl
129200090917     c                   ENDSR
129300950124     C**************************************************************************
129400950124     C* CALCOLO POSIZIONE SCHIERA DELL'ANTICIPO/RITARDO
129500950124     C**************************************************************************
129600930907     C     CALRIT        BEGSR
129700950124     C*
129800940117     C                   Z-ADD     1             B                 3 0
129900930907     C     NUM3          LOOKUP    ORE(B)                                 32
130000930907     C*
130100930907    1C     *IN32         IFEQ      *OFF
130200930907     C                   Z-ADD     1             B
130300930907     C*
130400930907    2C     NUM3          IFGT      0
130500930907     C     999           LOOKUP    ORE(B)                                 32
130600930907     C*
130700930907   X2C                   ELSE
130800930907     C*
130900930907     C     -999          LOOKUP    ORE(B)                                 32
131000930907    2C                   ENDIF
131100930907    1C                   ENDIF
131200950124     C*
131300930907     C                   ENDSR
131400950124     C**************************************************************************
131500950124     C* CALCOLO LE SPEDIZIONI CONSEGNABILI
131600950124     C**************************************************************************
131700930907     C     CALCON        BEGSR
131800950124     C*
131900930907     C                   SELECT
132000930907     C* NEL PERIODO
132100930907     C     NUM3          WHENEQ    0
132200090916     C   10              ADD       1             COE(2)                          "E"
132300090916     C   11              ADD       1             COC(2)                          "C"
132400090916     C   20              ADD       1             CON(2)                          "D"
132500090916     C   21              ADD       1             COh(2)                          "H"
132600930907     C     NUM3          WHENGT    0
132700090916     C   10              ADD       1             COE(3)                          "E"
132800090916     C   11              ADD       1             COC(3)                          "C"
132900090916     C   20              ADD       1             CON(3)                          "D"
133000090917     C   21              ADD       1             COh(3)                          "H"
133100930907     C                   OTHER
133200090916     C   10              ADD       1             COE(1)                          "E"
133300090916     C   11              ADD       1             COC(1)                          "C"
133400090916     C   20              ADD       1             CON(1)                          "D"
133500090916     C   21              ADD       1             COh(1)                          "H"
133600930907     C                   END
133700930907     C*
133800930907     C                   ENDSR
133900950124     C**************************************************************************
134000950124     C*    CONTO IN ARB00F QUELLE CONSEGNABILI O IN DISTINTA
134100950124     C**************************************************************************
134200930812     C     CONT4         BEGSR
134300930908     C*
134400930908     C* NORMALE/ESPRESSO
134500090917     C***                SETOFF                                       1011
134600090917     C***  ARBTSP        COMP      'E'                                    10
134700090917     C***  ARBTSP        COMP      'C'                                    11
134800090917     C*
134900090917     C* Tipo servizio --> imposta indicatore
135000090917     c                   exsr      IndtipoServ
135100930908     C**
135200970606     C* SE C'E' NUMERO DISTINTA E'    "I N    D I S T I N T A "
135300930908     C**
135400970606     C     ARBNDC        IFGT      0
135500970606     C     ARBDDC        ANDGT     0
135600970606     C     ARBDDC        ANDLE     DS1ADA
135700090917     c
135800930908     C* DATA E ORA CALCOLO
135900970606     C                   Z-ADD     DS1ADA        D98DFC
136000970606     C                   Z-ADD     2359          D98OFC
136100970606     C* SE IL GIORNO E' OGGI, METTO L'ORARIO ESATTO
136200970606     C     DS1ADA        IFEQ      WDATE1
136300970606     C                   MOVEL     UTIME         D98OFC                         HH:MM
136400970606     C                   ENDIF
136500930908     C*
136600930908     C                   EXSR      CALDAT
136700930908     C* CERCO SCAGLIONE DEL RITARDO
136800930908     C                   EXSR      CALRIT
136900930908     C*
137000090916     C   10              ADD       1             IDE(B)                          "E"
137100090916     C   11              ADD       1             IDC(B)                          "C"
137200090916     C   20              ADD       1             IDN(B)                          "D"
137300090916     C   21              ADD       1             IDh(B)                          "H"
137400940117     C*
137500940117     C   99              MOVEL     'I'           STAFLG
137600940203     C   99              ADD       1             STAI              7 0
137700940117     C   99              EXCEPT    SPED                                          S P E D I Z
137800930908     C*
137900930908     C* IN    C O N S E G N A
138000090916     C   10              ADD       1             ICE(B)                          "E"
138100090916     C   11              ADD       1             ICC(B)                          "C"
138200090916     C   20              ADD       1             ICN(B)                          "D"
138300090916     C   21              ADD       1             ICh(B)                          "H"
138400930908     C* C O N S E G N A B I L I
138500930908     C                   EXSR      CALCON
138600930908     C                   GOTO      ENDCT4
138700930908    1C                   ENDIF
138800930908     C**
138900930908     C* CONTROLLO SE E' "N O N   C O N S E G N A T A"
139000930908     C**
139100930908     C                   EXSR      CONT3
139200940117     C*
139300940117     C*
139400930910     C*  16 ON - E' UNA NON CONSEGNATA O CONSEGNA PARZIALE ESCO
139500930908     C   16              GOTO      ENDCT4
139600990211     C**
139700990211     C     ARBLNP        CHAIN     AZORG01L                           30
139800020207     C  N30              MOVEL     orgde3        og143
139900020207     C  N30              MOVEL     §ogntw        LNPntw            3
140000020211     c   30              clear                   lnpntw
140100930908     C**
140200930908     C* CONTROLLO LE    "N O N   I N   C O N S E G N A"
140300930908     C**
140400930812     C* ESCLUDO SE:
140500930812     C* 1) E' BLOCCATA
140600930908    1C     ARBFBC        IFEQ      'B'
140700970818     C     ARBFBC        OREQ      '*'
140800930812     C* 2) C'E' CONTRASSEGNO E NON E' ABILITATO
140900000703     C**** ARBACC        OREQ      ' '
141000930907     C* 3) E' IN DISTINTA E DATA DISTINTA MINORE DATA DAL
141100970606     C****       ARBDDC    ORLT DS1ADA
141200970606     C****       ARBNDC    ANDGT0
141300940324     C* 4) SE E' UN FERMO DEPOSITO
141400940324     C     ARBFFD        ORNE      *BLANKS
141500950616     C* 5) SE DATA ARRIVO ULTIMO COLLO > DATA CONSEGNABILITA'
141600970818     C*    (BOLLA NON IMPORT)
141700970207     C     ARBDTI        ORGT      DS1ADA
141800020207     C     LNPntw        ANDEQ     'COR'
141900020207     C     ARBDTI        ORGT      DS1ADA
142000020207     C     LNPntw        ANDEQ     'MES'
142100930812     C                   GOTO      ENDCT4
142200950616    1C                   ENDIF
142300990326     C**
142400990326     C** 6) ESCLUDO SE C'E' C.A. APERTA SULLA SPEDIZIONE
142500990326     C                   CLEAR                   WESCA
142600040812      *
142700040812     c                   clear                   fidn12ds
142800040812     c                   eval      i12aas = arbaas
142900040812     c                   eval      i12lnp = arblnp
143000040812     c                   eval      i12nrs = arbnrs
143100040812     c                   eval      i12nsp = arbnsp
143200040812     c                   eval      i12fel = 'E'
143300040812     c                   eval      i12fan = 'E'
143400040812      *
143500040812     c                   call      'FIDN12R'
143600040812     c                   parm                    fidn12ds
143700040812      *
143800040812      * se non ci sono errori
143900040812     c                   if        o12err = ' '
144000040812      * se numero di CA trovate maggiore di zero
144100040812     c                   if        o12nca > 0
144200040812     c
144300040812     c                   z-add     *zeros        ii                2 0
144400040812     c     1             do        o12nca        ii
144500040812     c                   movea     skkey(ii)     dskey
144600040812     C                   Z-ADD     dsfil         dctfil
144700040812     C                   Z-ADD     dsnca         dctnca
144800040812     C                   Z-ADD     skdca(ii)     dctdca
144900040812     C                   Z-ADD     skdch(ii)     dctdch
145000040812     C                   movea     skcch(ii)     dctcch
145100040812      * se CA aperta imposto il flag e vado a fine
145200040812     c                   If        dctdch = 0
145300040812     c                   eval      wesca = 'S'
145400040812     c                   leave
145500040812     c                   else
145600040812     c                   eval      wesca = 'C'
145700040812     c                   endif
145800040812     C                   ENDDO
145900040812      *
146000040812     c                   endif
146100040812     c                   endif
146200990326     C*
146300990326     C* -SE ESISTE C.A. APERTA  --> ESCO
146400990326    1C     WESCA         IFEQ      'S'
146500990326     C                   GOTO      ENDCT4
146600990326    1C                   ENDIF
146700970818     C*
146800970818     C* DECODIFICA CODICE BOLLA
146900970818     C                   MOVEL     '3A'          COD
147000970818     C                   MOVEL     *BLANKS       KEY
147100970818     C                   MOVEL     ARBCBO        KEY
147200970818     C     KTAB          CHAIN(N)  TABEL                              30
147300970818     C  N30              MOVEL     TBLUNI        DS3A
147400970818     C   30              CLEAR                   DS3A
147500970818     C                   MOVEL     §3ADES        P0DCBO            5
147600990211     C* 6) BOLLA IMPORT:
147700970818     C* ESCLUDO SE DATA ARRIVO ULTIMO COLLO > DATA CONSEGNABILITA
147800970818     C* SOLO SE SI TRATTA DI BOLLA ASSEGNATO CON FATTURA IMMEDIATA
147900020207    1C     LNPntw        IFEQ      'EEX'
148000020207    1C     LNPntw        oreq      'EUP'
148100020207    1C     LNPntw        oreq      'DPD'
148200020207    1C     LNPntw        oreq      'FED'
148300970818     C**
148400970818    2C     ARBDTI        IFGT      DS1ADA
148500970818     C     ARBDTI        OREQ      0
148600970818     C*
148700970818    3C     ARBDTI        IFGT      DS1ADA
148800970818     C     §3AFCA        ANDGT     0
148900970818     C                   GOTO      ENDCT4
149000970818    3C                   ENDIF
149100970818     C*
149200970818     C                   MOVE      ARBKSC        W0040             4 0
149300970818    3C     W0040         IFEQ      9999
149400991104     C                   MOVEL     '1'           KTRC
149500991104     C     KARB6         CHAIN     FIAR601L                           13
149600970818     C** PER ORA ESCO SOLO SE DTI E' CALCOLATO E > DELLA DATA ELABORAZ
149700970818     C*  PERCHE' DTI POTREBBE ESSERE A 0 MA TUTTE LE DATE DI ARRIVO
149800990326     C*  PRESENTI PERCHE' ANCORA NON E' STATO CALCOLATO
149900970818    4C     ARBDTI        IFGT      DS1ADA
150000970818     C     *IN13         ANDEQ     *OFF
150100970818     C     AR6DFT        ANDGT     0
150200970818     C                   GOTO      ENDCT4
150300970818    4C                   ENDIF
150400970818    3C                   ENDIF
150500970818    2C                   ENDIF
150600970818    1C                   END
150700970818     C**
150800990326     C* 7) SE NON E' COMPLETAMENTE ARRIVATA:
150900970818     C*    O E' ARRIVATA DOPO  --> LEGGO IL DETTAGLIO COLLI
151000990329     C* O SE HA IL C/A DEVO CONTROLLARE IL DETT.COLLI
151100970818     C                   CLEAR                   WCONS1
151200970818    1C     ARBDTI        IFEQ      0
151300970818     C     ARBDTI        ORGT      DS1ADA
151400990326     C     WESCA         OREQ      'C'
151500940329     C                   SETOFF                                       17
151600970818     C                   CLEAR                   KDAM1
151700950123     C     KARB3         SETLL     FNART01L
151800950123     C     KARB3         READE     FNART01L                               30
151900940329     C*
152000940329    2C     *IN30         DOWEQ     *OFF
152100040527     c                   clear                   Wartdam
152200040527     c
152300040527    3c                   if        artdam>0
152400040527     c                   eval      Wartdam=artdam
152500040527   x3c                   else
152600040527     c
152700040527     c* Memorizzo la data di arrivo o l'ipotetico arrivo se collo partito
152800040527     c* su FGV abbinato di ida non ancora chiuso: come data di arrivo
152900040527     c*  prendo la data foglio arrivi, solo se richiesto
153000040608    4c                   if        artdam=0  and artdfv>0  and ds1fl5='S'
153100040908     c* Se si tratta di bolla LOCALE, non esiste abbinamento quindi
153200040908     c* cerco di desumere la data del foglio arrivi per vedere se è
153300040908     c*  chiuso o perto
153400040908    5c                   if        arbtfp=arbtfa
153500050706     c                   clear                   wtipolancio
153600070420     c                   eval      kdataloc=artdfv
153700050706     c                   exsr      BOLLALOC
153800040908   X5c                   else
153900050706     C
154000040527     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
154100040527     C                   CLEAR                   fnlv53ds
154200040527     C                   MOVEL     'A'           D53TFO
154300040527     C                   MOVE      artNFV        D53NFV
154400040608     C                   MOVE      arbtfp        D53FGS
154500040527     C                   MOVEL     'B'           D53ABB
154600040527     C                   MOVE      ARBLNA        D53LNA
154700040527     C                   MOVE      ARBTFA        D53TFA
154800040527     C                   CALL      'FNLV53R'
154900040527     C                   PARM                    fnlv53ds
155000040527     c*
155100040908    6c                   if        d53err=*blanks and d53nfa>0
155200040527     c* verifico se fogli arrivi di abbinamento già chiuso
155300040527     C     KFVV          CHAIN     FNFVV000
155400040527     C                   IF        %FOUND(FNFVV01L)
155500040527     C                   IF        FVVFCF=' '
155600040527     C                   EVAL      Wartdam=D53DFA
155700040527     C                   ENDIF
155800040527     C                   ENDIF
155900040908    6c                   endif
156000040908    5c                   endif
156100040527     c
156200040527    4c                   endif
156300040527    3c                   endif
156400040527     c
156500040526     C* SE NON ARRIVATO O ARRIVATO CON DATA > (E'COME SE FOSSE A 0 QUEL
156600970818     C*  GIORNO)
156700040527    3C     Wartdam       IFEQ      0
156800040527     C     Wartdam       ORGT      DS1ADA
156900990329     C* CONTROLLO SE E' SU C.A. MANCANZA O AVARIA RESA O DISTRUTTA
157000990329     C*  NEL QUAL CASO IGNORO PERCHE' IL COLLI NON CI SARA' MAI
157100990329     C                   EXSR      CTRCA
157200990329     C*
157300990329     C     WMANCA        IFEQ      ' '
157400970818     C                   SETON                                        17
157500020207     C     LNPntw        IFEQ      'COR'
157600020207     C     LNPntw        oreq      'MES'
157700020207     C     LNPntw        oreq      'PPT'
157800970818     C                   SETON                                        30
157900970818     C                   END
158000990329     C                   END
158100990329     C*
158200940329   X3C                   ELSE
158300990329     C* MEMORIZZO CHE ALMENO UN COLLO E' ARRIVATO PRIMA O NEL GIORNO
158400970818     C*  DELLA DATA DI ELABORAZIONE
158500990326    4C     ARTDCM        IFEQ      0
158600970819     C     KDAM1         ANDEQ     *ZEROS
158700990326     C     ARTDCM        ORGT      DS1ADA
158800970818     C     KDAM1         ANDEQ     *ZEROS
158900990326     C*
159000990326     C** NON MEMORIZZO SE IL COLLO NON CONSEGNATO E' RELATIVO AD
159100990326     C**  AVARIE RESE O DISTRUTTE
159200990329     C                   EXSR      CTRCA
159300990329    5C     WMANCA        IFEQ      ' '
159400990326     C                   SETON                                        33
159500990326    5C                   ENDIF
159600990326     C**
159700040527     C   33              Z-ADD     Wartdam       KDAM1
159800970818     C     *LIKE         DEFINE    ARTDAM        KDAM1
159900990329    4C                   END
160000940329    3C                   ENDIF
160100970818     C**
160200970818     C  N30KARB3         READE     FNART01L                               30
160300940329    2C                   ENDDO
160400990326     C*
160500970818     C* SE BOLLA IMPORT NON LA STAMPO SOLO SE ENTRO LA DATA ELABORA-
160600970818     C* ZIONE NON E' ARRIVATO NEMMENO UN COLLO (KDAM1=0)
160700970818     C* STAMPO INVECE LE SPEDIZIONI ARRIVATE PARZIALMENTE
160800990329     C* SE PRESENTE C.A. CHIUSA: ESCO SE I SOLI COLLI NON CONSEGNATI
160900990329     C*  SONO QUELLI RELATIVI A MANCANZE O AVARIE RESE/DISTRUTTE
161000970818    3C     KDAM1         IFEQ      *ZEROS
161100970818     C                   GOTO      ENDCT4
161200040526    3C                   ENDIF
161300990326     C**
161400970818     C* E' UNA ARRIVO PARZIALE: STAMPO SOLO SE NON E' FATTURA IMMEDIATA
161500970818     C*  E SE NON HA IN CONTRASSEGNO
161600020207    2C     LNPntw        IFEQ      'EEX'
161700020207    2C     LNPntw        oreq      'EUP'
161800020207    2C     LNPntw        oreq      'DPD'
161900020207    2C     LNPntw        oreq      'FED'
162000970818    3C     *IN17         IFEQ      *ON
162100970818     C     ARBDTI        ANDEQ     0
162200970818     C*
162300970818    4C     §3AFCA        IFGT      0
162400970818     C     W0040         OREQ      9999
162500970818     C     *IN13         ANDEQ     *OFF
162600970818     C     AR6DFT        ANDGT     0
162700970818     C                   GOTO      ENDCT4
162800970818    4C                   ENDIF
162900970818    3C                   ENDIF
163000970818     C* MEMORIZZO CHE SI TRATTA DI SPEDIZIONE CONSEGNABILE ANCHE SE
163100970818     C* E' UN ARRIVO PARZIALE
163200970818     C   17              MOVE      '1'           WCONS1            1
163300970818     C**
163400970818   X2C                   ELSE
163500940329     C*
163600940329     C   17              GOTO      ENDCT4
163700970818    2C                   END
163800970818    1C                   ENDIF
163900930907     C*
164000961108     C* SE LA BOLLA E' GIACENTE ED HA UNA FIGLIA CON LNP = LNA IN
164100970925     C*   CONSEGNA NON RICHIAMO PGM TNSD99R PERCHE' NON E' CONSEGNABILE
164200961108    1C     ARBFBC        IFEQ      'G'
164300961108     C     ARBFBC        OREQ      '*'
164400050331     C     KARB3         SETLL     Tigcp21L                               33
164500961108    2C     *IN33         IFEQ      *ON
164600961112     C* SALVO I CAMPI DELLA BOLLA
164700961112     C                   MOVE      ARBDS         WSAVDS
164800961108     C                   CLEAR                   WTROV
164900961108     C*
165000961108     C     KARB3         SETLL     FNLBL02L
165100961108     C     KARB3         READE     FNLBL02L                               37
165200961108    3C     *IN37         DOWEQ     *OFF
165300961108    4C     LBLLPN        IFEQ      LBLLAN
165400961108     C     KARB1         CHAIN     FNARB01L                           38
165500961108    5C     *IN38         IFEQ      *OFF
165600961108     C     ARBNDC        ANDGT     0
165700961108     C                   SETON                                        37
165800961108     C                   MOVEL     'S'           WTROV             1
165900961108    5C                   ENDIF
166000961108    4C                   ENDIF
166100961108     C  N37KARB3         READE     FNLBL02L                               37
166200961108    3C                   ENDDO
166300961108     C*
166400961112     C* RIMETTO I CAMPI SALVATI IN PRECEDENZA
166500961112     C                   MOVE      WSAVDS        ARBDS
166600961112     C*
166700961108    3C     WTROV         IFEQ      'S'
166800961108     C                   GOTO      ENDCT4
166900961108    3C                   ENDIF
167000961108    2C                   ENDIF
167100961108    1C                   ENDIF
167200050706     c* Se richiesto, escludo supermercati e appuntamenti arrivati nella dat
167300050706     c*  a richiesta  se privi della cons. richiesta
167400141217    1c                   if        ds1fl6='S' and arbdcr=0
167500141217    2c                   if        arbtc1='S' or arbtc1='A' or arbtc2='S' or
167600050706     c                             arbtc2='A'
167700141217     c* Solo se non ha una dispo di dirottamento escludo
167800141217     c                   exsr      dispoDIR
167900141217    3c                   if        OLRY09ESIT<>'1'
168000050706     c* Per bolla locale, desumo
168100050706     c                   if        arbtfp=arbtfa
168200050706     c                   eval      wtipolancio='SA'
168300070420     c                   eval      kdataloc=arbdbr
168400070420     c
168500050706     c                   exsr      BOLLALOC
168600050706     c                   clear                   d53err
168700050706     c                   eval      d53dfa=wartdam
168800050706     c                   else
168900050706     C* CONTROLLO SE LA BOLLA ARRIVI E RELATIVA A FOGLIO VIAGGIO ABBINATO
169000050706     C                   CLEAR                   fnlv53ds
169100050706     C                   MOVEL     'A'           D53TFO
169200050706     C                   MOVE      arbNFV        D53NFV
169300050706     C                   MOVE      arbtfp        D53FGS
169400050706     C                   MOVEL     'B'           D53ABB
169500050706     C                   MOVE      ARBLNA        D53LNA
169600050706     C                   MOVE      ARBTFA        D53TFA
169700050706     C                   CALL      'FNLV53R'
169800050706     C                   PARM                    fnlv53ds
169900050706     c                   endif
170000050706     c*
170100141217    4c                   select
170200060314    6c                   when      d53err=*blanks and d53dfa=ds1ada
170300050706     C                   GOTO      ENDCT4
170400060314     c
170500060315    6c                   when      d53err=*blanks and d53dfa<ds1ada  and
170600060315     c                             d53dfa>0
170700060314     c* controllo se giorni festivi da d53dfa al giorno prima di ds1ada
170800060314     c     *iso          movel     d53dfa        dataiso
170900060314     c     *iso          movel     ds1ada        dataisofine
171000060314     c                   eval      wfest='S'
171100060314     c                   dow       dataiso<dataisofine and WFEST='S'
171200060314     c                   movel     dataiso       kdata             8 0
171300060314     c                   exsr      CTRFESTA
171400060314     c                   adddur    1:*d          dataiso
171500060314     c                   enddo
171600060314     c
171700060314     c                   if        wfest='S'
171800060314     c                   eval      wfest=' '
171900060314     C                   GOTO      ENDCT4
172000060314     c                   endif
172100060314     c
172200141217    4C                   ENDSL
172300961108     C*
172400050706    3C                   ENDIF
172500141217    2C                   ENDIF
172600141217    1C                   ENDIF
172700050706     C*
172800970207     C                   Z-ADD     DS1ADA        D98DFC
172900950127     C                   Z-ADD     2359          D98OFC
173000950127     C* SE IL GIORNO E' OGGI, METTO L'ORARIO ESATTO
173100970207     C     DS1ADA        IFEQ      WDATE1
173200950127     C                   MOVEL     UTIME         D98OFC                         HH:MM
173300950127     C                   ENDIF
173400930908     C*
173500930908     C                   EXSR      CALDAT
173600940117     C*
173700940117     C                   EXSR      CALRIT
173800100218     c
173900100218     c* Se bolla con partic.consegna ATTESA verifico se da stampare a meno
174000100218     c*  in base all'anticipo che ha.
174100100218     c                   if        ds1fl7='S' and arbgma<>*blanks
174200100218     C                   MOVEL     '7R'          COD
174300100218     C                   MOVEL(P)  arbgma        KEY
174400100218     C     KTAB          CHAIN(N)  TABEL                              31
174500100218     C  N31              MOVEL     TBLUNI        DS7R
174600100218     C   31              CLEAR                   DS7R
174700100218     c                   if        §7RATT='S' and d98nci <=-24
174800100218     C                   GOTO      ENDCT4
174900100218     c                   endif
175000100218     c                   endif
175100110927     c*
175200110927     c* Se c'e' Perfect Order --> lo stampo e vedo se è abilitato
175300110927     c                   setoff                                       1928
175400110927     c                   clear                   StPorder         16
175500130221     c                   if        arbxco >*zeros
175600110927     c                   seton                                        19
175700110927     c                   exsr      POrder
175800110927     c                   if        olr78conso='S'
175900110927     c                   eval      stPorder='  Consegnabile  '
176000110927     c                   else
176100110927     c                   eval      stPorder='NON Consegnabile'
176200110927     c
176300110927     c* se non consegnabile tolgo 24 ore alla delivery per andare in anticipo
176400110927     c                   eval      num3=num3-24
176500110927     c                   exsr      CALRIT
176600110927     c                   endif
176700110927     c
176800110927     c                   endif
176900110927     c
177000930908     C*
177100940117    1C     D98OCI        IFGT      0
177200930908     C*
177300940117     C* N O N    IN    C O N S E G N A
177400090916     C   10              ADD       1             NCE(B)                          "E"
177500090916     C   11              ADD       1             NCC(B)                          "C"
177600090916     C   20              ADD       1             NCN(B)                          "D"
177700090916     C   21              ADD       1             NCh(B)                          "H"
177800940117     C*
177900940117     C   99              MOVEL     'D'           STAFLG            1
178000940203     C   99              ADD       1             STAD              7 0
178100940117     C   99              EXCEPT    SPED                                          S P E D I Z
178200940530     C*
178300940530     C* STAMPO LISTA SPEDIZIONI NON IN CONSEGNA
178400970225     C  N02DS1FL1        IFEQ      'L'
178500970225     C     DS1FL1        OREQ      'E'
178600970403     C     DS1FL3        OREQ      'L'
178700940530     C                   EXSR      STASNC
178800940530     C                   ENDIF
178900940530     C*
179000930908     C* C O N S E G N A B I L I
179100930908     C                   EXSR      CALCON
179200940117    1C                   ENDIF
179300930812     C*
179400990329     C     ENDCT4        ENDSR
179500990329     C**************************************************************************
179600990329     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
179700990329     C**************************************************************************
179800990329     C     CTRCA         BEGSR
179900990329     C                   CLEAR                   WMANCA
180000990329    1C     WESCA         IFEQ      'C'
180100990329     C**
180200990329     C     KART          CHAIN     FNDCD02L                           33
180300990329    2C     *IN33         IFEQ      *OFF
180400990330     C     DCTATB        ANDEQ     ' '
180500990329     C     KDCT          CHAIN     FNDCT01L                           33
180600050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
180700050221     c                   If        Not *In33 and dctdt2 > 0
180800050221     c                   Eval      *In33 = *On
180900050221     c                   EndIf
181000990329     C*
181100990329    3C     *IN33         IFEQ      *OFF
181200990329     C     DCTDCH        ANDEQ     DCDDCH
181300990329     C     DCDDCH        OREQ      0
181400990329     C* TESTO PRIMA LA CAUSALE DI CHIUSURA PER VEDERE SE MI INDICA LA
181500990329     C*  PRESENZA DELLA MERCE
181600990329     C                   CLEAR                   DSBS02
181700990329     C                   MOVEL     'C'           T02MOD
181800990329     C                   MOVEL     KNSIF         T02SIF
181900990329     C                   MOVEL     'CCH'         T02COD
182000990329     C                   MOVEL     DCTCCH        T02KE1
182100990329     C                   CALL      'TIBS02R'
182200990329     C                   PARM                    KPJBA
182300990329     C                   PARM                    DSBS02
182400990329     C**
182500990329    4C     T02ERR        IFEQ      ' '
182600990330     C                   MOVEL     T02UNI        DCCH
182700990329     C                   ELSE
182800990329     C                   MOVEL     'S'           §CHIMA
182900990329    4C                   ENDIF
183000990329     C*
183100990329    4C     §CHIMA        IFEQ      ' '
183200990329     C                   CLEAR                   DSBS02
183300990329     C                   MOVEL     'C'           T02MOD
183400990329     C                   MOVEL     KNSIF         T02SIF
183500990329     C                   MOVEL     'TAD'         T02COD
183600990329     C                   MOVEL     DCTTAD        T02KE1
183700990329     C                   CALL      'TIBS02R'
183800990329     C                   PARM                    KPJBA
183900990329     C                   PARM                    DSBS02
184000990329     C**
184100990329    5C     T02ERR        IFEQ      ' '
184200990329     C                   MOVEL     T02UNI        DTAD
184300990329     C                   ELSE
184400990329     C                   MOVEL     'M'           §TARGR
184500990329    5C                   ENDIF
184600990329     C*
184700990329     C* AVARIA RESA O DISTRUTTA
184800990329    5C     §TARGR        IFEQ      'V'
184900990329    6C     DCTDSD        IFEQ      'D'
185000990330     C     DCTDSD        OREQ      'R'
185100990329     C                   MOVEL     'V'           WMANCA            1
185200990329    6C                   ENDIF
185300990329    5C                   ENDIF
185400990329     C* MANCANZA
185500990329    5C     §TARGR        IFEQ      'M'
185600990329     C                   MOVEL     'M'           WMANCA            1
185700990329    5C                   ENDIF
185800990329     C*
185900990329    4C                   ENDIF
186000990329    3C                   ENDIF
186100990329    2C                   ENDIF
186200990329    1C                   ENDIF
186300990329     C*
186400990329     C                   ENDSR
186500950124     C**************************************************************************
186600950124     C* CALCOLO DATA PREV.CONS/CONSEGNABILITA'/ORE RITARDO DI 1 BOLLA
186700950124     C**************************************************************************
186800930908     C     CALDAT        BEGSR
186900950123     C*
187000930908     C                   MOVEL     ' '           D98TLA                         *TIPO LANCIO
187100930908     C                   MOVEL     'A'           D98TBO                         *TIPO BOLLA
187200950123     C                   Z-ADD     ARBAAS        D98AAS
187300950123     C                   Z-ADD     ARBLNP        D98LNP
187400950123     C                   Z-ADD     ARBNRS        D98NRS
187500950123     C                   Z-ADD     ARBNSP        D98NSP
187600970925     C                   CALL      'TNSD99R'
187700970925     C                   PARM                    DS99
187800930908     C*
187900950123     C                   Z-ADD     D98NCI        NUM3              3 0
188000950124     C*
188100930908     C                   ENDSR
188200950124     C**************************************************************************
188300950124     C* CONTROLLO LE NON CONSEGNATE DAGLI EVENTI
188400950124     C**************************************************************************
188500930908     C     CONT3         BEGSR
188600950124     C*
188700970417     C* PER OGNI BOLLA LEGGO SOLTANTO L'ULTIMO EVENTO
188800940208     C                   SETOFF                                       16
188900970207     C                   Z-ADD     DS1ADA        NUM6A
189000950123     C                   Z-ADD     ARBAAS        AR§AAS
189100970417     C                   MOVEL     *HIVAL        NUMORA
189200970417     C                   CLEAR                   WWRT
189300990805     C     KARB22        SETLL     FNEVB000                           32
189400990805     C     KARB2         READPE    FNEVB000                               32
189500091112     c                   seton                                        35
189600091112     c* Escludo gli eventi particolari
189700091112     c                   dow       not *in32 and *in35
189800091112     c     evbcev        lookup    cep                                    35
189900091112     c
190000091112     C   35KARB2         READPE    FNEVB000                               32
190100091112     c                   enddo
190200930908     C*
190300970417    1C     *IN32         IFEQ      *OFF
190400940208     C                   SETOFF                                       05
190500940208     C* DATA E ORA CALCOLO
190600950124     C                   Z-ADD     EVBDEV        D98DFC                          DATA EVENTO
190700950124     C                   Z-ADD     EVBHEV        D98OFC                          ORA  EVENTO
190800940208     C                   EXSR      CALDAT
190900940208     C*
191000940208     C* CERCO SCAGLIONE DEL RITARDO
191100940208     C                   EXSR      CALRIT
191200930908     C*
191300930908     C                   MOVEL     '2A'          COD
191400930908     C                   MOVEL     *BLANKS       KEY
191500930908     C                   MOVEL     EVBCEV        KEY
191600970124     C     KTAB          CHAIN(N)  TABEL                              30
191700930908     C   30              CLEAR                   DS2A
191800930908     C  N30              MOVEL     TBLUNI        DS2A
191900940208     C*
192000930908     C* TIPO CONSEGNA
192100970417    2C     §2AFTC        IFNE      *BLANKS
192200930908     C*
192300930908     C* SOLO SE RICHIESTA UNA DATA SCRIVO LE NON CONSEGNATE
192400930908     C                   SELECT
192500970417    3C     §2AFTC        WHENEQ    'G'
192600940208     C   99              MOVEL     'G'           STAFLG
192700940208     C   99              ADD       1             STAG              7 0
192800970121     C   10              ADD       1             GAE(B)                         GIAC APERTE
192900970121     C   11              ADD       1             GAC(B)
193000090916     C   20              ADD       1             GAN(B)
193100090916     C   21              ADD       1             GAh(B)
193200970417     C                   MOVEL     'S'           WWRT              1
193300970417    3C     §2AFTC        WHENEQ    'A'                                          LASC AVVISO
193400940208     C   99              MOVEL     'A'           STAFLG
193500940208     C   99              ADD       1             STAA              7 0
193600970121     C   10              ADD       1             LAE(B)
193700970121     C   11              ADD       1             LAC(B)
193800090916     C   20              ADD       1             LAN(B)
193900090916     C   21              ADD       1             LAh(B)
194000970417     C                   MOVEL     'S'           WWRT              1
194100930908     C*
194200970417   X3C                   OTHER
194300970417    4C     §2ACPT        IFEQ      'S'                                          CHIUSO TURNO
194400940208     C   99              MOVEL     'T'           STAFLG
194500940208     C   99              ADD       1             STAT              7 0
194600970121     C   10              ADD       1             CTE(B)
194700970121     C   11              ADD       1             CTC(B)
194800090916     C   20              ADD       1             CTN(B)
194900090916     C   21              ADD       1             CTh(B)
195000970417     C                   MOVEL     'S'           WWRT              1
195100970417   X4C                   ELSE
195200940208     C   99              MOVEL     'N'           STAFLG
195300940208     C   99              ADD       1             STAN              7 0
195400970121     C   10              ADD       1             NFE(B)                         NON FATTE
195500970121     C   11              ADD       1             NFC(B)
195600090916     C   20              ADD       1             NFN(B)
195700090916     C   21              ADD       1             NFh(B)
195800970417     C                   MOVEL     'S'           WWRT              1
195900970417    4C                   ENDIF
196000970417    3C                   ENDSL
196100940117     C*
196200970417   X2C                   ELSE
196300940208     C*
196400940208     C* SE CONSEGNA PARZIALE COME SE FOSSE CONSEGNATA
196500940208     C                   SELECT
196600970417    3C     §2ATIP        WHENEQ    'P'
196700930910     C*
196800930910     C* C O N S E G N A T E
196900090916     C   10              ADD       1             CNE(B)
197000090916     C   11              ADD       1             CNC(B)
197100090916     C   20              ADD       1             CNN(B)
197200090916     C   21              ADD       1             CNh(B)
197300940208     C* TOTALE A PARTE COME C.PARZIALE
197400970121     C   10              ADD       1             PRE(1)                          CONS PARZ
197500970121     C   11              ADD       1             PRC(1)
197600090916     C   20              ADD       1             PRN(1)
197700090916     C   21              ADD       1             PRh(1)
197800970417     C                   MOVEL     'S'           WWRT              1
197900940117     C*
198000940117     C   99              MOVEL     'P'           STAFLG
198100940203     C   99              ADD       1             STAP              7 0
198200940208     C*
198300940208     C* RISERVA CONSEGNA
198400970417    3C     §2ATIP        WHENEQ    'R'
198500940208     C   99              MOVEL     'R'           STAFLG
198600940208     C   99              ADD       1             STAR              7 0
198700970121     C   10              ADD       1             RSE(1)                         RISERVA CONS
198800970121     C   11              ADD       1             RSC(1)
198900090916     C   20              ADD       1             RSN(1)
199000090916     C   21              ADD       1             RSh(1)
199100970417     C                   MOVEL     'S'           WWRT              1
199200940208     C* 05 ON - NON DEVO CONTEGGIARLA COME IN CONSEGNA (GIA' FATTO)
199300940208     C                   SETON                                        05
199400091117     c
199500091117     C* Spedizioni Ripristinate --> tra  le "Non fatte"
199600091117    3C     §2ATIP        WHENEQ    'S'
199700091117     C   99              MOVEL     'N'           STAFLG
199800091117     C   99              ADD       1             STAN
199900091117     C   10              ADD       1             NFE(B)                         NON FATTE
200000091117     C   11              ADD       1             NFC(B)
200100091117     C   20              ADD       1             NFN(B)
200200091117     C   21              ADD       1             NFh(B)
200300091117     C                   MOVEL     'S'           WWRT
200400970417    3C                   ENDSL
200500970417    2C                   ENDIF
200600091117     c
200700970417     C* EVENTO DA CONTEGGIARE
200800970417    2C     WWRT          IFEQ      'S'
200900940208     C   99              EXCEPT    SPED
201000940208     C     *IN05         IFEQ      *OFF
201100940208     C* IN    C O N S E G N A
201200090916     C   10              ADD       1             ICE(B)
201300090916     C   11              ADD       1             ICC(B)
201400090916     C   20              ADD       1             ICN(B)
201500090916     C   21              ADD       1             ICh(B)
201600940208     C*
201700940208     C* C O N S E G N A B I L I
201800940208     C                   EXSR      CALCON
201900940208     C                   SETON                                          16
202000940208     C                   ENDIF
202100930910     C*
202200970417    2C                   ENDIF
202300970417    1C                   ENDIF
202400930908     C*
202500930908     C                   ENDSR
202600950124     C**************************************************************************
202700970417     C* CONTROLLO LE CONSEGNE PARZIALI E RISERVE SE DATA DAL AL
202800950124     C**************************************************************************
202900940324     C     CONT3P        BEGSR
203000090917     c
203100090917     C* Tipo servizio --> imposta indicatore
203200090917     c                   exsr      IndtipoServ
203300940324     C*
203400970207     C                   Z-ADD     DS1ADA        NUM6A
203500950123     C                   Z-ADD     ARBAAS        AR§AAS
203600990805     C     KARB2         SETLL     FNEVB000
203700990805     C     KARB4         READE     FNEVB000                               32
203800940324     C*
203900940324    1C     *IN32         DOWEQ     *OFF
204000950124    2C     EVBDEV        IFGT      CO1DAA
204100940324     C                   SETON                                        32
204200940324   X2C                   ELSE
204300940324     C*
204400940324     C                   MOVEL     '2A'          COD
204500940324     C                   MOVEL     *BLANKS       KEY
204600940324     C                   MOVEL     EVBCEV        KEY
204700970124     C     KTAB          CHAIN(N)  TABEL                              30
204800940324     C   30              CLEAR                   DS2A
204900940324     C  N30              MOVEL     TBLUNI        DS2A
205000940324     C*
205100940324     C* TIPO CONSEGNA
205200970417     C* CONSIDERO SOLO CONSEGNE PARZIALI E RISERVE
205300970417    3C     §2ATIP        IFEQ      'P'
205400970417     C     §2ATIP        OREQ      'R'
205500940324     C* DATA E ORA CALCOLO
205600950406     C                   Z-ADD     EVBDEV        D98DFC                          DATA EVENTO
205700950410     C                   MOVEL     EVBHEV        D98OFC                          ORA  EVENTO
205800940324     C                   EXSR      CALDAT
205900940324     C*
206000940324     C* CERCO SCAGLIONE DEL RITARDO
206100940324     C                   EXSR      CALRIT
206200940324     C*
206300940324     C* SE CONSEGNA PARZIALE COME SE FOSSE CONSEGNATA
206400940324    4C     §2ATIP        IFEQ      'P'
206500940324     C*
206600940324     C* C O N S E G N A T E
206700090916     C   10              ADD       1             CNE(B)                          "E"
206800090916     C   11              ADD       1             CNC(B)                          "C"
206900090916     C   20              ADD       1             CNN(B)                          "D"
207000090916     C   21              ADD       1             CNh(B)                          "H"
207100940324     C* TOTALE A PARTE COME C.PARZIALE
207200970121     C   10              ADD       1             PRE(1)                          CONS PARZ
207300970121     C   11              ADD       1             PRC(1)
207400090916     C   20              ADD       1             PRN(1)
207500090916     C   21              ADD       1             PRh(1)
207600940324     C*
207700940324     C   99              MOVEL     'P'           STAFLG
207800940324     C   99              ADD       1             STAP              7 0
207900940324     C*
208000940324   X4C                   ELSE
208100940324     C* RISERVA CONSEGNA
208200940324     C   99              MOVEL     'R'           STAFLG
208300940324     C   99              ADD       1             STAR              7 0
208400970121     C   10              ADD       1             RSE(1)                         RISERVA CONS
208500970121     C   11              ADD       1             RSC(1)
208600090916     C   20              ADD       1             RSN(1)
208700090916     C   21              ADD       1             RSh(1)
208800940324    4C                   ENDIF
208900940324    3C                   ENDIF
209000940324     C*
209100940324     C   99              EXCEPT    SPED
209200940324    2C                   ENDIF
209300940324     C*
209400990805     C  N32KARB4         READE     FNEVB000                               32
209500940324    1C                   ENDDO
209600940324     C*
209700940324     C                   ENDSR
209800950124     C**************************************************************************
209900950124     C*    STAMPA STATISTICA
210000950124     C**************************************************************************
210100920605     C     STAMPA        BEGSR
210200950124     C*
210300940117     C   OF              EXCEPT    TESTA
210400940117     C   OF              SETOFF                                       OF
210500940117     C*
210600930908     C                   CLEAR                   CONP1
210700930908     C                   CLEAR                   CONP2
210800930908     C*
210900930908     C* SPEDIZIONI IN CONSEGNA
211000090916     C                   XFOOT     ICE           GENICE            6 0           "E"
211100090917     C                   XFOOT     ICh           GENICH            6 0           "H"
211200090916     C                   XFOOT     ICN           GENICN            6 0           "D"
211300090916     C                   XFOOT     ICC           W0090                           "C"
211400970121     C                   ADD       W0090         GENICN
211500930908     C     GENICN        ADD       GENICE        GENIC             7 0           TOT CONSEG
211600090917     C                   ADD       GENICH        GENIC             7 0           TOT CONSEG
211700930908     C* SPEDIZIONI CONSEGNABILI
211800090916     C                   XFOOT     COE           GENCOE            6 0           "E"
211900090917     C                   XFOOT     COh           GENCOH            6 0           "H"
212000090916     C                   XFOOT     CON           GENCON            6 0           "D"
212100090916     C                   XFOOT     COC           W0090                           "C"
212200090916     C                   ADD       W0090         GENCON                          D+C
212300930908     C     GENCON        ADD       GENCOE        GENCO             7 0           TOT CONSEG
212400090917     C                   ADD       GENCOh        GENCO             7 0           TOT CONSEG
212500930908     C**
212600930908     C* SPEDIZIONI   C O N S E G N A T E
212700930908     C**
212800090916     C                   XFOOT     CNE           GENCNE            6 0           "E"
212900090917     C                   XFOOT     CNh           GENCNH            6 0           "H"
213000090916     C                   XFOOT     CNN           GENCNN            6 0           "D"
213100090916     C                   XFOOT     CNC           W0090                           "C"
213200090916     C                   ADD       W0090         GENCNN                          D+C
213300930812     C     GENCNN        ADD       GENCNE        GENCN             7 0           TOT CONSEG
213400090917     C                   ADD       GENCNh        GENCN             7 0           TOT CONSEG
213500930908     C*
213600930908     C* % CONSEGNATE        SU TOT IN CONSEGNA
213700930908    1C     GENCN         IFGT      0
213800930908     C*
213900930908     C     GENCN         MULT      100           NUM9              9 0
214000950123     C     GENIC         IFNE      *ZERO
214100940526     C     NUM9          DIV(H)    GENIC         W0052             5 2
214200950123     C                   ELSE
214300950123     C                   Z-ADD     *ZERO         W0052
214400950123     C                   ENDIF
214500940526     C                   EXSR      CTRP1
214600930908     C*
214700930908     C* % CONSEGNATE        SU TOT CONSEGNABILE
214800950123     C     GENCO         IFNE      *ZERO
214900940526     C     NUM9          DIV(H)    GENCO         W0052
215000950123     C                   ELSE
215100950123     C                   Z-ADD     *ZERO         W0052
215200950123     C                   ENDIF
215300940526     C                   EXSR      CTRP2
215400930908    1C                   ENDIF
215500930908     C*
215600940117     C                   MOVEL     TE(18)        RIGA             66
215700940202     C                   Z-ADD     GENCNN        GENNOR
215800940202     C                   Z-ADD     GENCNE        GENESP
215900090917     C                   Z-ADD     GENCNH        GENH10
216000940202     C                   Z-ADD     GENCN         GENTOT
216100930908     C                   EXCEPT    INTE1
216200940208     C*
216300940208     C* ESEGUO STAMPA DELLE CONSEGNE PARZIALI E RISERVE
216400940208     C                   EXSR      STAPR
216500930908     C*
216600930908     C* STAMPA DETTAGLIO
216700930908    1C     GENCN         IFGT      0
216800930908     C                   MOVEA     CNE           ESP
216900090917     C                   MOVEA     CNh           h10
217000970121     C     CNC           ADD       CNN           NOR
217100930908     C                   EXSR      STADCN
217200930908    1C                   ENDIF
217300940324     C**
217400940324     C     *IN02         IFEQ      *OFF
217500940329     C* LA 1 PERCENTUALE SUL TOTALE CONSEGNATO NON CI VUOLE PIU'
217600940329     C                   SETON                                        03
217700940324     C**
217800930908     C**
217900930908     C* SPEDIZIONI   N O N    C O N S E G N A T E
218000930908     C**
218100930908     C* GIACENZE APERTE
218200090916     C                   XFOOT     GAE           GENGAE            6 0           "E"
218300090918     C                   XFOOT     GAh           GENGAh            6 0           "H"
218400090916     C                   XFOOT     GAN           GENGAN            6 0           "D"
218500090916     C                   XFOOT     GAC           W0090                           "C"
218600090916     C                   ADD       W0090         GENGAN                          D+C
218700930908     C     GENGAN        ADD       GENGAE        GENGA             7 0           TOT CONSEG
218800090918     C                   ADD       GENGAH        GENGA             7 0           TOT CONSEG
218900930908     C* LASCIATI AVVISI
219000090916     C                   XFOOT     LAE           GENLAE            6 0           "E"
219100090918     C                   XFOOT     LAh           GENLAH            6 0           "H"
219200090916     C                   XFOOT     LAN           GENLAN            6 0           "D"
219300090916     C                   XFOOT     LAC           W0090                           "C"
219400090916     C                   ADD       W0090         GENLAN                          D+C
219500930908     C     GENLAN        ADD       GENLAE        GENLA             7 0           TOT CONSEG
219600090918     C                   ADD       GENLAh        GENLA             7 0           TOT CONSEG
219700930908     C* CHIUSE PER TURNO
219800090916     C                   XFOOT     CTE           GENCTE            6 0           "E"
219900090918     C                   XFOOT     CTh           GENCTH            6 0           "H"
220000090916     C                   XFOOT     CTN           GENCTN            6 0           "D"
220100090916     C                   XFOOT     CTC           W0090                           "C"
220200090916     C                   ADD       W0090         GENCTN                          D+C
220300930908     C     GENCTN        ADD       GENCTE        GENCT             7 0           TOT CONSEG
220400090918     C                   ADD       GENCTh        GENCT             7 0           TOT CONSEG
220500930908     C* NON FATTE
220600090916     C                   XFOOT     NFE           GENNFE            6 0           "E"
220700090918     C                   XFOOT     NFh           GENNFH            6 0           "H"
220800090916     C                   XFOOT     NFN           GENNFN            6 0           "D"
220900090916     C                   XFOOT     NFC           W0090                           "C"
221000090916     C                   ADD       W0090         GENNFN                          D+C
221100930908     C     GENNFN        ADD       GENNFE        GENNF             7 0           TOT CONSEG
221200090918     C                   ADD       GENNFH        GENNF             7 0           TOT CONSEG
221300930908     C*
221400930908    2C     GENGA         IFGT      0
221500930908     C     GENLA         ORGT      0
221600930908     C     GENCT         ORGT      0
221700930908     C     GENNF         ORGT      0
221800930908     C                   CLEAR                   CONP1
221900930908     C                   CLEAR                   CONP2
222000090918     C*
222100930908     C     GENGAN        ADD       GENLAN        GENNOR
222200930908     C                   ADD       GENCTN        GENNOR
222300930908     C                   ADD       GENNFN        GENNOR
222400090918     C*
222500930908     C     GENGAE        ADD       GENLAE        GENESP
222600930908     C                   ADD       GENCTE        GENESP
222700930908     C                   ADD       GENNFE        GENESP
222800090918     C*
222900090918     C     GENGAh        ADD       GENLAh        GENh10
223000090918     C                   ADD       GENCTh        GENh10
223100090918     C                   ADD       GENNFh        GENh10
223200090918     C* TOTALE
223300930908     C     GENNOR        ADD       GENESP        GENTOT
223400090918     C                   ADD       GENh10        GENTOT
223500930908     C*
223600930908     C* % NON CONSEGNATE    SU TOT IN CONSEGNA
223700930908     C*
223800930908     C     GENTOT        MULT      100           NUM9              9 0
223900950123     C     GENIC         IFNE      *ZERO
224000940526     C     NUM9          DIV(H)    GENIC         W0052
224100950123     C                   ELSE
224200950123     C                   Z-ADD     *ZERO         W0052
224300950123     C                   ENDIF
224400940526     C                   EXSR      CTRP1
224500930908     C*
224600930908     C* % NON CONSEGNATE    SU TOT CONSEGNABILE
224700950123     C     GENCO         IFNE      *ZERO
224800940526     C     NUM9          DIV(H)    GENCO         W0052
224900950123     C                   ELSE
225000950123     C                   Z-ADD     *ZERO         W0052
225100950123     C                   ENDIF
225200940526     C                   EXSR      CTRP2
225300930908     C*
225400940117     C                   MOVEL     TE(24)        RIGA
225500940117     C*
225600940117     C   OF              EXCEPT    TESTA
225700940117     C   OF              SETOFF                                       OF
225800930908     C                   EXCEPT    INTE1
225900930908     C**
226000930908     C* GIACENZE APERTE
226100930908     C**
226200930908     C                   Z-ADD     GENGA         GENTOT
226300930908     C                   Z-ADD     GENGAN        GENNOR
226400930908     C                   Z-ADD     GENGAE        GENESP
226500090918     C                   Z-ADD     GENGAh        GENh10
226600090918     C                   MOVEA     GAE           ESP
226700090918     C                   MOVEA     GAh           h10
226800090916     C     GAC           ADD       GAN           NOR
226900940202     C                   MOVEL     TE(25)        RIGA
227000930908     C*
227100930908     C                   EXSR      CTRNCO
227200930908     C**
227300930908     C* LASCIATI AVVISI
227400930908     C**
227500930908     C                   Z-ADD     GENLA         GENTOT
227600930908     C                   Z-ADD     GENLAN        GENNOR
227700930908     C                   Z-ADD     GENLAE        GENESP
227800090918     C                   Z-ADD     GENLAh        GENh10
227900090918     C                   MOVEA     LAE           ESP
228000090918     C                   MOVEA     LAh           h10
228100090916     C     LAC           ADD       LAN           NOR
228200940202     C                   MOVEL     TE(26)        RIGA
228300930908     C*
228400930908     C                   EXSR      CTRNCO
228500930908     C**
228600930908     C* CHIUSO PER TURNO
228700930908     C**
228800930908     C                   Z-ADD     GENCT         GENTOT
228900930908     C                   Z-ADD     GENCTN        GENNOR
229000930908     C                   Z-ADD     GENCTE        GENESP
229100090918     C                   Z-ADD     GENCTH        GENh10
229200090918     C                   MOVEA     CTE           ESP
229300090918     C                   MOVEA     CTh           h10
229400090916     C     CTC           ADD       CTN           NOR
229500940207     C                   MOVEL     TE(28)        RIGA
229600930908     C*
229700930908     C                   EXSR      CTRNCO
229800930908     C**
229900930908     C* NON FATTE
230000930908     C**
230100930908     C                   Z-ADD     GENNF         GENTOT
230200930908     C                   Z-ADD     GENNFN        GENNOR
230300930908     C                   Z-ADD     GENNFE        GENESP
230400090918     C                   Z-ADD     GENNFh        GENh10
230500090918     C                   MOVEA     NFE           ESP
230600090918     C                   MOVEA     NFh           h10
230700090916     C     NFC           ADD       NFN           NOR
230800940207     C                   MOVEL     TE(27)        RIGA
230900930908     C*
231000930908     C                   EXSR      CTRNCO
231100930908    2C                   ENDIF
231200930908     C**
231300930908     C* SPEDIZIONI   A N C O R A    I N    D I S T I N T A
231400930908     C**
231500090916     C                   XFOOT     IDE           GENIDE            6 0           "E"
231600090918     C                   XFOOT     IDh           GENIDH            6 0           "H"
231700090916     C                   XFOOT     IDN           GENIDN            6 0           "D"
231800090916     C                   XFOOT     IDC           W0090                           "C"
231900090916     C                   ADD       W0090         GENIDN                          D+c
232000930908     C     GENIDN        ADD       GENIDE        GENID             7 0           TOT CONSEG
232100090918     C                   ADD       GENIDh        GENID             7 0           TOT CONSEG
232200930908     C*
232300930908     C                   CLEAR                   CONP1
232400930908     C                   CLEAR                   CONP2
232500930908     C*
232600930908     C* % IN DISTINTA       SU TOT IN CONSEGNA
232700930908    1C     GENID         IFGT      0
232800930908     C*
232900930908     C     GENID         MULT      100           NUM9              9 0
233000950123     C     GENIC         IFNE      *ZERO
233100940526     C     NUM9          DIV(H)    GENIC         W0052
233200950123     C                   ELSE
233300950123     C                   Z-ADD     *ZERO         W0052
233400950123     C                   ENDIF
233500940526     C                   EXSR      CTRP1
233600930908     C*
233700930908     C* % IN DISTINTA       SU TOT CONSEGNABILE
233800950123     C     GENCO         IFNE      *ZERO
233900940526     C     NUM9          DIV(H)    GENCO         W0052
234000950123     C                   ELSE
234100950123     C                   Z-ADD     *ZERO         W0052
234200950123     C                   ENDIF
234300940526     C                   EXSR      CTRP2
234400930908     C*
234500940117     C                   MOVEL     TE(29)        RIGA
234600940202     C                   Z-ADD     GENIDN        GENNOR
234700940202     C                   Z-ADD     GENIDE        GENESP
234800090918     C                   Z-ADD     GENIDh        GENh10
234900940202     C                   Z-ADD     GENID         GENTOT
235000930908     C*
235100940117     C   OF              EXCEPT    TESTA
235200940117     C   OF              SETOFF                                       OF
235300930908     C                   EXCEPT    INTE1
235400930908     C*
235500930908     C* STAMPA DETTAGLIO
235600090918     C                   MOVEA     IDE           ESP
235700090918     C                   MOVEA     IDh           h10
235800090916     C     IDC           ADD       IDN           NOR
235900930908     C                   EXSR      STADCN
236000930908    1C                   ENDIF
236100930908     C**
236200930908     C* SPEDIZIONI   I N    C O N S E G N A
236300930908     C**
236400930908     C                   CLEAR                   CONP2
236500930908     C*
236600930908     C* % IN DISTINTA       SU TOT CONSEGNABILE
236700930908    2C     GENIC         IFGT      0
236800930908     C     GENIC         MULT      100           NUM9              9 0
236900950123     C     GENCO         IFNE      *ZERO
237000940526     C     NUM9          DIV(H)    GENCO         W0052
237100950123     C                   ELSE
237200950123     C                   Z-ADD     *ZERO         W0052
237300950123     C                   ENDIF
237400940526     C                   EXSR      CTRP2
237500930908    2C                   ENDIF
237600930908     C*
237700940117     C                   MOVEL     TE(30)        RIGA
237800940202     C                   Z-ADD     GENICN        GENNOR
237900940202     C                   Z-ADD     GENICE        GENESP
238000090918     C                   Z-ADD     GENICh        GENh10
238100940202     C                   Z-ADD     GENIC         GENTOT
238200930908     C*
238300940117     C   OF              EXCEPT    TESTA
238400940117     C   OF              SETOFF                                       OF
238500930908     C                   EXCEPT    INTE2
238600930908     C*
238700930908     C* STAMPA DETTAGLIO
238800930908    2C     GENIC         IFGT      0
238900090918     C                   MOVEA     ICE           ESP
239000090918     C                   MOVEA     ICh           h10
239100090916     C     ICC           ADD       ICN           NOR
239200930908     C                   EXSR      STADCN
239300930908    2C                   ENDIF
239400930908     C**
239500930908     C* SPEDIZIONI   N O N    I N    C O N S E G N A
239600930908     C**
239700090916     C                   XFOOT     NCE           GENNCE            6 0           "E"
239800090918     C                   XFOOT     NCh           GENNCH            6 0           "H"
239900090916     C                   XFOOT     NCN           GENNCN            6 0           "D"
240000090916     C                   XFOOT     NCC           W0090                           "C"
240100090916     C                   ADD       W0090         GENNCN                          D+C
240200940117     C     GENNCN        ADD       GENNCE        GENNC             7 0           TOT CONSEG
240300090918     C                   ADD       GENNCh        GENNC             7 0           TOT CONSEG
240400930908     C*
240500930908     C* % NON IN CONSEGNA   SU TOT CONSEGNABILE
240600930908    2C     GENNC         IFGT      0
240700940526     C                   SETON                                        04
240800940117     C                   CLEAR                   CONP2
240900940117     C*
241000930908     C     GENNC         MULT      100           NUM9              9 0
241100950123     C     GENCO         IFNE      *ZERO
241200940526     C     NUM9          DIV(H)    GENCO         W0052
241300950123     C                   ELSE
241400950123     C                   Z-ADD     *ZERO         W0052
241500950123     C                   ENDIF
241600940526     C                   EXSR      CTRP2
241700930908     C*
241800940117     C                   MOVEL     TE(31)        RIGA
241900940202     C                   Z-ADD     GENNCN        GENNOR
242000940202     C                   Z-ADD     GENNCE        GENESP
242100090918     C                   Z-ADD     GENNCh        GENh10
242200940202     C                   Z-ADD     GENNC         GENTOT
242300930908     C*
242400940117     C   OF              EXCEPT    TESTA
242500940117     C   OF              SETOFF                                       OF
242600930908     C                   EXCEPT    INTE2
242700940117     C*
242800940117     C* STAMPA DETTAGLIO
242900940526    3C     GENNC         IFGT      0
243000090918     C                   MOVEA     NCE           ESP
243100090918     C                   MOVEA     NCh           h10
243200090916     C     NCC           ADD       NCN           NOR
243300940117     C                   EXSR      STADCN
243400940526    3C                   ENDIF
243500940526     C                   SETOFF                                       04
243600940526    2C                   ENDIF
243700930908     C*
243800930908     C**
243900930908     C* SPEDIZIONI   C O N S E G N A B I L I
244000930908     C**
244100930908     C*
244200940117     C                   MOVEL     TE(32)        RIGA
244300940202     C                   Z-ADD     GENCON        GENNOR            8 0
244400940202     C                   Z-ADD     GENCOE        GENESP            8 0
244500090918     C                   Z-ADD     GENCOh        GENh10            8 0
244600940202     C                   Z-ADD     GENCO         GENTOT            8 0
244700930908     C*
244800940117     C   OF              EXCEPT    TESTA
244900940117     C   OF              SETOFF                                       OF
245000090918     c
245100090918     c                   if        not *in25
245200090918     C                   EXCEPT    INTE3
245300090918     c                   else
245400090918     c* con tipo servizio H
245500090918     c                   seton                                        26
245600090918     C                   EXCEPT    INTE2
245700090918     c                   endif
245800930909     C*
245900090918     c                   setoff                                       26
246000930909     C* STAMPO DETTAGLIO
246100930909     C                   Z-ADD     33            Y
246200930909     C*
246300940207     C                   SETON                                        01
246400930909     C                   DO        3             B
246500940207     C                   CLEAR                   CONP3
246600940117     C                   MOVEL     TE(Y)         RIGA
246700930909     C                   ADD       1             Y
246800090918     C                   Z-ADD     COE(B)        GENESP
246900090918     C                   Z-ADD     COh(B)        GENh10
247000090916     C     COC(B)        ADD       CON(B)        GENNOR
247100970121     C     GENESP        ADD       GENNOR        GENTOT
247200090918     C                   ADD       GENh10        GENTOT
247300090918     c
247400930909     C* % NON IN CONSEGNA   SU TOT CONSEGNABILE
247500930909    2C     GENTOT        IFGT      0
247600930909     C     GENTOT        MULT      100           NUM9              9 0
247700950123     C     GENCO         IFNE      *ZERO
247800940526     C     NUM9          DIV(H)    GENCO         W0052
247900950123     C                   ELSE
248000950123     C                   Z-ADD     *ZERO         W0052
248100950123     C                   ENDIF
248200940526     C                   EXSR      CTRP3
248300930909     C                   ENDIF
248400930909     C*
248500940117     C   OF              EXCEPT    TESTA
248600940117     C   OF              SETOFF                                       OF
248700930909     C                   EXCEPT    DET2
248800930909     C                   ENDDO
248900940324     C*
249000940324     C                   ENDIF
249100940117     C* RIGA FINALE
249200940117     C                   EXCEPT    DET3
249300940117     C                   SETON                                        OF
249400950124     C*
249500920617     C                   ENDSR
249600950124     C**************************************************************************
249700950124     C* ESEGUO STAMPA DELLE CONSEGNE PARZIALI E RISERVE
249800950124     C**************************************************************************
249900940208     C     STAPR         BEGSR
250000940208     C*
250100940329     C* S T A M P O   C  O N S E G N E   P A R Z I A L I
250200940208     C     PRE(1)        ADD       PRN(1)        GENTOT
250300970121     C                   ADD       PRC(1)        GENTOT
250400090916     C                   ADD       PRh(1)        GENTOT
250500940208     C     GENTOT        IFGT      0
250600970121     C     PRC(1)        ADD       PRN(1)        GENNOR
250700090918     C                   Z-ADD     PRE(1)        GENESP
250800090918     C                   Z-ADD     PRh(1)        GENh10
250900940208     C                   MOVEL     TE(38)        RIGA             66
251000940208     C* % CONSEGNATE        SU TOT CONSEGNATO
251100940208     C*
251200940208     C     GENTOT        MULT      100           NUM9              9 0
251300950123     C     GENCN         IFNE      *ZERO
251400940526     C     NUM9          DIV(H)    GENCN         W0052
251500950123     C                   ELSE
251600950123     C                   Z-ADD     *ZERO         W0052
251700950123     C                   ENDIF
251800940526     C                   EXSR      CTRP1
251900940208     C* % CONSEGNATE        SU TOT IN CONSEGNA
252000950123     C     GENIC         IFNE      *ZERO
252100940526     C     NUM9          DIV(H)    GENIC         W0052
252200950123     C                   ELSE
252300950123     C                   Z-ADD     *ZERO         W0052
252400950123     C                   ENDIF
252500940526     C                   EXSR      CTRP2
252600940208     C*
252700940208     C* % CONSEGNATE        SU TOT CONSEGNABILE
252800950123     C     GENCO         IFNE      *ZERO
252900940526     C     NUM9          DIV(H)    GENCO         W0052
253000950123     C                   ELSE
253100950123     C                   Z-ADD     *ZERO         W0052
253200950123     C                   ENDIF
253300940526     C                   EXSR      CTRP3
253400940526     C*
253500940208     C                   EXCEPT    DET2
253600940208     C                   ENDIF
253700940208     C*
253800940329     C* S T A M P O   L E   R I S E R V E
253900940208     C     RSE(1)        ADD       RSN(1)        GENTOT
254000970121     C                   ADD       RSC(1)        GENTOT
254100090916     C                   ADD       RSh(1)        GENTOT
254200940208     C     GENTOT        IFGT      0
254300970121     C     RSC(1)        ADD       RSN(1)        GENNOR
254400090918     C                   Z-ADD     RSE(1)        GENESP
254500090918     C                   Z-ADD     RSh(1)        GENh10
254600940208     C                   MOVEL     TE(39)        RIGA             66
254700940208     C* % CONSEGNATE        SU TOT CONSEGNATO
254800940208     C*
254900940208     C     GENTOT        MULT      100           NUM9              9 0
255000950123     C     GENCN         IFNE      *ZERO
255100940526     C     NUM9          DIV(H)    GENCN         W0052
255200950123     C                   ELSE
255300950123     C                   Z-ADD     *ZERO         W0052
255400950123     C                   ENDIF
255500940526     C                   EXSR      CTRP1
255600940208     C* % CONSEGNATE        SU TOT IN CONSEGNA
255700950123     C     GENIC         IFNE      *ZERO
255800940526     C     NUM9          DIV(H)    GENIC         W0052
255900950123     C                   ELSE
256000950123     C                   Z-ADD     *ZERO         W0052
256100950123     C                   ENDIF
256200940526     C                   EXSR      CTRP2
256300940208     C*
256400940208     C* % CONSEGNATE        SU TOT CONSEGNABILE
256500950123     C     GENCO         IFNE      *ZERO
256600940526     C     NUM9          DIV(H)    GENCO         W0052
256700950123     C                   ELSE
256800950123     C                   Z-ADD     *ZERO         W0052
256900950123     C                   ENDIF
257000940526     C                   EXSR      CTRP3
257100940526     C*
257200940208     C                   EXCEPT    DET2
257300940208     C                   ENDIF
257400940208     C*
257500940208     C                   ENDSR
257600950124     C**************************************************************************
257700950124     C* STAMPA DETTAGLIO DELLE CONSEGNATE
257800950124     C**************************************************************************
257900930908     C     STADCN        BEGSR
258000950124     C*
258100930908    1C                   DO        13            B
258200930908     C     ESP(B)        ADD       NOR(B)        TOT(B)
258300090918     C                   ADD       h10(B)        TOT(B)
258400930908     C* SCAGLIONE PIENO
258500930908    2C     TOT(B)        IFGT      0
258600930908     C*
258700930908     C                   SELECT
258800930908     C* RITARDO
258900930908    3C     ORE(B)        WHENGT    0
259000930908    4C     ORE(B)        IFEQ      999
259100940117     C                   MOVEL     TE(23)        RIGA
259200930908     C     B             SUB       1             Y
259300930908   X4C                   ELSE
259400940117     C                   MOVEL     TE(22)        RIGA
259500930908     C                   Z-ADD     B             Y
259600930908    4C                   ENDIF
259700930908     C* ANTICIPO
259800930908     C     ORE(B)        WHENLT    0
259900930908    4C     ORE(B)        IFEQ      -999
260000940117     C                   MOVEL     TE(19)        RIGA
260100930908     C     B             ADD       1             Y
260200930908   X4C                   ELSE
260300940117     C                   MOVEL     TE(20)        RIGA
260400930908     C                   Z-ADD     B             Y
260500930908    4C                   ENDIF
260600930908     C*
260700930908   X3C                   OTHER
260800930908     C* CONFORMITA'
260900940117     C                   MOVEL     TE(21)        RIGA
261000940202     C                   Z-ADD     B             Y
261100930908    3C                   ENDSL
261200930908     C*
261300930908     C                   CLEAR                   CONP1
261400930908     C                   CLEAR                   CONP2
261500940329     C                   CLEAR                   CONP3
261600930908     C*
261700940526     C     TOT(B)        MULT      100           NUM9              9 0
261800940526     C*
261900930908     C* % SUL TOTALE IN CONSEGNA
262000940526     C* (NON ELABORO PER LE NON IN CONSEGNA)
262100940526     C     *IN04         IFEQ      *OFF
262200950123     C     GENIC         IFNE      *ZERO
262300940526     C     NUM9          DIV(H)    GENIC         W0052
262400950123     C                   ELSE
262500950123     C                   Z-ADD     *ZERO         W0052
262600950123     C                   ENDIF
262700940526     C                   EXSR      CTRP2
262800940526     C                   ENDIF
262900930908     C*
263000930908     C* % SUL TOTALE CONSEGNABILE
263100950123     C     GENCO         IFNE      *ZERO
263200940526     C     NUM9          DIV(H)    GENCO         W0052
263300950123     C                   ELSE
263400950123     C                   Z-ADD     *ZERO         W0052
263500950123     C                   ENDIF
263600940526     C                   EXSR      CTRP3
263700940203     C* % SUL TOTALE CONSEGNATO
263800940329     C* (SOLO QUANDO ELABORO LE SPEDIZIONI CONSEGNATE)
263900940526     C     *IN03         IFEQ      *OFF
264000950123     C     GENCN         IFNE      *ZERO
264100940526     C     NUM9          DIV(H)    GENCN         W0052
264200950123     C                   ELSE
264300950123     C                   Z-ADD     *ZERO         W0052
264400950123     C                   ENDIF
264500940526     C                   EXSR      CTRP1
264600940526     C                   ENDIF
264700970121     C*
264800970121     C                   Z-ADD     NOR(B)        PNORM             7 0
264900970121     C                   Z-ADD     ESP(B)        PESPR             7 0
265000090918     C                   Z-ADD     h10(B)        Ph1030            7 0
265100970121     C                   Z-ADD     TOT(B)        PTOTAL            8 0
265200930908     C*
265300940117     C   OF              EXCEPT    TESTA
265400940117     C   OF              SETOFF                                       OF
265500940117     C                   EXCEPT    DET1
265600930908     C*
265700930908    2C                   ENDIF
265800930908    1C                   ENDDO
265900950124     C*
266000930908     C                   ENDSR
266100950124     C**************************************************************************
266200950124     C* CONTROLLO LE SPEDIZIONI NON CONSEGNATE DA STAMPARE
266300950124     C**************************************************************************
266400930908     C     CTRNCO        BEGSR
266500950124     C*
266600930908    1C     GENTOT        IFGT      0
266700930908     C* %    SU TOT IN CONSEGNA
266800930908     C     GENTOT        MULT      100           NUM9              9 0
266900950123     C     GENIC         IFNE      *ZERO
267000940526     C     NUM9          DIV(H)    GENIC         W0052
267100950123     C                   ELSE
267200950123     C                   Z-ADD     *ZERO         W0052
267300950123     C                   ENDIF
267400940526     C                   EXSR      CTRP2
267500930908     C*
267600930908     C* %    SU TOT CONSEGNABILE
267700950123     C     GENCO         IFNE      *ZERO
267800940526     C     NUM9          DIV(H)    GENCO         W0052
267900950123     C                   ELSE
268000950123     C                   Z-ADD     *ZERO         W0052
268100950123     C                   ENDIF
268200940526     C                   EXSR      CTRP3
268300940207     C*
268400940203     C                   EXCEPT    DET4
268500940203     C                   EXCEPT    DET2
268600930908     C*
268700930908     C                   EXSR      STADCN
268800930908    1C                   ENDIF
268900950124     C*
269000930908     C                   ENDSR
269100950124     C**************************************************************************
269200950124     C* CONTROLLO SE 1% E' > 99,99
269300950124     C**************************************************************************
269400940526     C     CTRP1         BEGSR
269500950124     C*
269600940526     C     W0052         IFGT      99,99
269700940526     C                   Z-ADD     99,99         CONP1             4 2
269800940526     C                   ELSE
269900940526     C                   Z-ADD     W0052         CONP1
270000940526     C                   ENDIF
270100950124     C*
270200940526     C                   ENDSR
270300950124     C**************************************************************************
270400950124     C* CONTROLLO SE 2% E' > 99,99
270500950124     C**************************************************************************
270600940526     C     CTRP2         BEGSR
270700950124     C*
270800940526     C     W0052         IFGT      99,99
270900940526     C                   Z-ADD     99,99         CONP2             4 2
271000940526     C                   ELSE
271100940526     C                   Z-ADD     W0052         CONP2
271200940526     C                   ENDIF
271300950124     C*
271400940526     C                   ENDSR
271500950124     C**************************************************************************
271600950124     C* CONTROLLO SE 3% E' > 99,99
271700950124     C**************************************************************************
271800940526     C     CTRP3         BEGSR
271900950124     C*
272000940526     C     W0052         IFGT      99,99
272100940526     C                   Z-ADD     99,99         CONP3             4 2
272200940526     C                   ELSE
272300940526     C                   Z-ADD     W0052         CONP3
272400940526     C                   ENDIF
272500950124     C*
272600940526     C                   ENDSR
272700950124     C**************************************************************************
272800950124     C* STAMPE SPEDIZIONI NON IN CONSEGNA
272900950124     C**************************************************************************
273000940530     C     STASNC        BEGSR
273100151008     c                   setoff                                       7022
273200151008     c                   clear                   wabisin          50
273300151008     c                   clear                   wcalert          20
273400950124     C*
273500950127     C                   Z-ADD     ARBAAS        P0CAAS            2 0
273600940530     C* MESE GIORNO SPEDIZIONE
273700940530     C                   MOVEL     ARBMGS        MMGG
273800940530     C                   Z-ADD     MM            P0CMGS            4 0
273900940530     C                   MOVEL     GG            P0CMGS
274000940601     C* DESTINATARIO
274100940601     C                   MOVEL     ARBRSD        P0CRSD           15
274200950127     C                   MOVEL     ARBIND        P0CIND           24
274300940601     C                   MOVEL     ARBLOD        P0CLOD           15
274400940601     C* DECODIFICA CONSEGNA PARTICOLARE
274500950127     C                   MOVEL     *BLANKS       P0DTC1
274600950123     C     ARBTC1        IFNE      ' '
274700940601     C                   MOVEL     '1P'          COD
274800940601     C                   MOVEL     *BLANKS       KEY
274900950123     C                   MOVEL     ARBTC1        KEY
275000970124     C     KTAB          CHAIN(N)  TABEL                              30
275100950127     C  N30              MOVEL     TBLUNI        P0DTC1            5
275200940601     C                   ENDIF
275300950127     C*
275400950127     C                   MOVEL     *BLANKS       P0DTC2
275500950127     C     ARBTC2        IFNE      *BLANKS
275600950127     C                   MOVEL     '1P'          COD
275700950127     C                   MOVEL     *BLANKS       KEY
275800950127     C                   MOVEL     ARBTC2        KEY
275900970124     C     KTAB          CHAIN(N)  TABEL                              30
276000950127     C  N30              MOVEL     TBLUNI        P0DTC2           10
276100950127     C                   ENDIF
276200051128     C* DATA CONSEGNA RICHIESTA richiamo pgm per sapere se tassativa
276300051128     c                   clear                   p0cdcr            6 0
276400051128     c                   clear                   p0ctcr
276500051128     c                   if        arbdcr>0 or arbhcr>0
276600051128     c                   clear                   fnlv80ds
276700051128     c                   eval      ilv80aas=arbaas
276800051128     c                   eval      ilv80lnp=arblnp
276900051128     c                   eval      ilv80nrs=arbnrs
277000051128     c                   eval      ilv80nsp=arbnsp
277100051128     c                   eval      ilv80mgs=arbmgs
277200051128     c                   eval      ilv80tc1=arbtc1
277300051128     c                   eval      ilv80tc2=arbtc2
277400051128     c                   eval      ilv80dcr=arbdcr
277500051128     c                   eval      ilv80hcr=arbhcr
277600051128     c                   eval      ilv80tcr=arbtcr
277700051128     c                   call      'FNLV80R'
277800051128     c                   parm                    kpjba
277900051128     c                   parm                    fnlv80ds
278000051128     c
278100051128     c                   eval      p0cdcr=olv80dcr6
278200051128     c*
278300051128     c                   if        olv80tat='S'
278400051128     c                   eval      p0ctcr='TA'
278500051128     c                   else
278600051128     c                   clear                   p0ctcr            2
278700051128     c                   move      arbtcr        p0ctcr
278800051128     c                   endif
278900051128     c                   endif
279000141119     c
279100141119     c* Verifico se presente dispo di dirottamento
279200141217     c                   exsr      dispoDIR
279300141119     c*
279400141119     c                   if        OLRY09ESIT='1'
279500141119     c                   seton                                        70
279600141119     c                   endif
279700940530     C*
279800940530     C     *INOD         IFEQ      *ON
279900940530     C                   EXCEPT    TESELE
280000940530     C                   SETOFF                                       OD
280100940530     C                   ENDIF
280200950123     C*
280300950123     C* TENTO DI REPERIRE IL CONTRASEGNO
280400950123     C                   CLEAR                   AR9CAS
280500950123     C                   CLEAR                   AR9VCA
280600051115     C     KARB3         SETLL     FiAR901L                               30     ON=TROVATO
280700051115     C   30KARB3         CHAIN     FiAR901L                           30
280800950127     C*
280900950127     C* TENTO DI REPERIRE L'IMPORTO FATTURA
281000950127     C                   CLEAR                   AR6IFT
281100991130     C                   CLEAR                   AR6DIV
281200991128     C                   CLEAR                   COMIFT
281300991130     C                   CLEAR                   COMdiv
281400991104     C                   MOVEL     '1'           KTRC
281500991104     C     KARB6         SETLL     FIAR601L                               30     ON=TROVATO
281600991128     C     *IN30         IFEQ      *ON
281700991130     C     KARB6         CHAIN     FIAR601L                           30
281800940530     C*
281900991130     C     *IN30         IFEQ      *OFF
282000991130     C* Vado su tabella divise per sapere se accetta i decimali
282100991130     C     AR6DIV        IFNE      SAVDIV
282200991130     C                   MOVEL     'CV'          COD
282300991130     C                   MOVEL(P)  AR6DIV        KEY
282400991130     C     KTAB          CHAIN(N)  TABEL                              37
282500991130     C  N37              MOVEL     TBLUNI        DSCV
282600991130     C   37              CLEAR                   DSCV
282700991130     C                   MOVEL     AR6DIV        SAVDIV
282800991130     C                   ENDIF
282900991130     C*
283000991130     C* Edito importo fattura per la stampa
283100991130     c     ar6ift        ifgt      *zeros
283200991130     C                   Z-ADD     AR6IFT        W0122
283300991130     C                   MOVEL     §CVFDC        WTSTDEC
283400991130     c                   exsr      edtimporti
283500991130     c                   move      impeditato    comift            9
283600991130     c                   movel     ar6div        comdiv
283700991130     c                   endif
283800991128     C                   ENDIF
283900991130     C                   ENDIF
284000940530     C                   EXCEPT    DETELE
284100151008     c
284200151008     c* Verifico se inviato controalert
284300151008     c     kar5          chain     fiar501l
284400151008     c                   if        %found(fiar501l)  and ds1aal>20151011
284500151008     c                   eval      dar5emd=ar5uni
284600151008     c                   if        §AR5ICA='S'  and
284700151008     c                             §AR5DPC=%editc(ds1ada:'X')
284800151008     c                   eval      wcalert='Inviato CONTROALERT'
284900151008     c                   seton                                        22
285000151008     c                   endif
285100151008     c                   endif
285200110923     c*
285300070419     C* STAMPO C/ASSEGNO NON ABILITATO SU SECONDA RIGA se GEO non attiva
285400070419     c                   if        not *in18
285500070419     c
285600070419     C     ARBAcc        IFEQ      ' '
285700070419     C     AR9CAS        ANDNE     0
285800110923     c                   seton                                        28
285900110923     c                   endif
286000151008     c
286100151008     c
286200110923     c
286300151008     c                   if        *in28 or *in19 or *in22
286400070419     C                   EXCEPT    DETCAA
286500070419     C                   ENDIF
286600070419     c
286700070419     c* Stampo tutte le abilitazioni bolle che ci sono
286800070419     c                   else
286900151008     c                   clear                   fnlrg7ds
287000151008     c                   eval      ilrg7aas=arbaas
287100151008     c                   eval      ilrg7lnp=arblnp
287200151008     c                   eval      ilrg7nrs=arbnrs
287300151008     c                   eval      ilrg7nsp=arbnsp
287400151008     c                   eval      ILRG7LUNG=50
287500151008     c                   call      'FNLRG7R'
287600151008     c                   parm                    fnlrg7ds
287700070419     c
287800151008     c                   if        olrg7vssin<>*blanks
287900110923     c                   seton                                        28
288000151008     c                   movel     olrg7vssin    wabisin
288100110923     c                   endif
288200110923     c
288300151008     c                   if        *in28 or *in19 or *in22
288400070419     C                   EXCEPT    DETCAA
288500070419     c                   endif
288600070419     c                   endif
288700070419     c
288800950127     C* VEDO SE SONO PRESENTI DELLE NOTE
288900950127     C     *LIKE         DEFINE    AR4NOT        P0CNO1
289000950127     C     *LIKE         DEFINE    AR4NOT        P0CNO2
289100060905     C     *LIKE         DEFINE    AR4NOT        wstadis
289200950127     C                   CLEAR                   P0CNO1
289300950127     C                   CLEAR                   P0CNO2
289400950127     C                   MOVEL     '8'           AR4TRC
289500060216     C     KAR41         SETLL     FiAR401L                               30     ON=TROVATO
289600950127     C   30              DO
289700060216     C     KAR41         CHAIN     FiAR401L                           30
289800950127     C  N30              MOVEL     AR4NOT        P0CNO1
289900950127     C                   MOVEL     '9'           AR4TRC
290000060216     C     KAR41         SETLL     FiAR401L                               30     ON=TROVATO
290100950127     C   30              DO
290200060216     C     KAR41         CHAIN     FiAR401L                           30
290300950127     C  N30              MOVEL     AR4NOT        P0CNO2
290400950127     C                   ENDDO
290500950127     C                   ENDDO
290600060905     c
290700060905     c* Vedo se spedizioni in distinta con data > di quella elaborata
290800060905     c                   setoff                                       12
290900060905     c                   clear                   wstadis
291000060905     c                   if        arbndc>0 and arbddc>0
291100060905     c                   eval      wstadis=Cindistinta
291200060905     c                   eval      %subst(wstadis:15:7)= %editc(arbndc: 'Z')
291300060905     c* giro la data
291400060905     C                   Z-ADD     arbddc        G02INV                         AAAA/MM/GG
291500060905     C                   Z-ADD     *ZERO         G02DAT
291600060905     C                   MOVEL     '3'           G02ERR
291700060905     C                   CALL      'XSRDA8'
291800060905     C                   PARM                    WLBDAT
291900060905     C                   MOVE      G02DAT        W0020             2 0
292000060905     C                   MOVEL     G02DAT        STAddc            6 0          GG/MM/AA
292100060905     C                   MOVE      W0020         STAddc            6 0          GG/MM/AA
292200060905     c                   eval      %subst(wstadis:28:8)= %editc(staddc: 'Y')
292300060905     c
292400060905     c* se non c'e' la seconda nota la riempo
292500060905     c* altrimenti scrivo 3°riga
292600060905     c                   if        p0cno2=*blanks
292700060905     c                   eval      p0cno2=wstadis
292800060905     c                   else
292900060905     c                   seton                                        12
293000060905     c                   endif
293100060905     c                   endif
293200990329     C*
293300990329     C* VEDO SE E' PRESENTE UNA C.A.
293400090922     C                   SETOFF                                       07
293500040812      * Sostituisco la lettura del file FNDTC02 con
293600040812      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
293700040812      * legate alla spedizione che passo
293800040812      *
293900040812     c                   clear                   fidn12ds
294000040812     c                   eval      i12aas = arbaas
294100040812     c                   eval      i12lnp = arblnp
294200040812     c                   eval      i12nrs = arbnrs
294300040812     c                   eval      i12nsp = arbnsp
294400040812     c                   eval      i12fel = 'E'
294500040812     c                   eval      i12fan = 'E'
294600040812      *
294700040812     c                   call      'FIDN12R'
294800040812     c                   parm                    fidn12ds
294900040812      *
295000040812      * se non ci sono errori
295100040812     c                   if        o12err = ' '
295200040812      * se numero di CA trovate maggiore di zero
295300040812     c                   if        o12nca > 0
295400040812     c* prendo la prima CA
295500040812     c                   z-add     1             ii                2 0
295600040812     c                   movea     skkey(ii)     dskey
295700040812     C                   Z-ADD     dsfil         dctfil
295800040812     C                   Z-ADD     dsnca         dctnca
295900040812     C                   Z-ADD     skdca(ii)     dctdca
296000040812     C                   Z-ADD     skdch(ii)     dctdch
296100040812     C                   movea     skcch(ii)     dctcch
296200040812     c                   seton                                        07
296300040812     c                   exsr      preca
296400040812      *
296500040812     c                   endif
296600040812     c                   endif
296700040812      * stampo le note
296800040812     C* NOTE
296900040812     C     P0CNO1        IFNE      *BLANKS
297000040812     C     P0CNO2        ORNE      *BLANKS
297100040812     C     ARBTC2        ORNE      *BLANKS
297200040812     C     *IN07         OREQ      *ON
297300040812     C*
297400040812     C                   EXCEPT    DE2ELE
297500040812     C*
297600040812     c* se ci sono altre CA stampo ancora
297700040812      * se non ci sono errori
297800040812     c                   if        o12err = ' '  and o12nca > 0
297900040812     c                   add       1             ii
298000040812     c                   dow       ii <= o12nca
298100040812     c                   movea     skkey(ii)     dskey
298200040812     C                   Z-ADD     dsfil         dctfil
298300040812     C                   Z-ADD     dsnca         dctnca
298400040812     C                   Z-ADD     skdca(ii)     dctdca
298500040812     C                   Z-ADD     skdch(ii)     dctdch
298600040812     C                   movea     skcch(ii)     dctcch
298700040812      *
298800040812     C                   EXSR      PRECA
298900040812     C** STAMPO TESTATA E DETT C.A.
299000040812     C   OD              EXCEPT    TESELE
299100040812     C                   EXCEPT    DETCA
299200040812      *
299300040812     c                   add       1             ii
299400040812     C                   ENDDO
299500040812      *
299600040812     c                   endif
299700040812      *
299800040812     c                   endif
299900060905     c
300000060905     C** STAMPO se in distinta
300100060905     c                   if        *in12
300200060905     C   OD              EXCEPT    TESELE
300300060905     C                   EXCEPT    DETdis
300400060905     c                   endif
300500950123     C*
300600940530     C                   ENDSR
300700141217     C**************************************************************************
300800141217     c     DispoDIR      BEGSR
300900141217     C**************************************************************************
301000141217     c                   clear                   fnlry09ds2
301100141217     c                   eval      ILRY09TCH='T'
301200141217     c                   eval      ILRY09TDIS='D'
301300141217     c                   eval      ILRY09AAS=arbaas
301400141217     c                   eval      ILRY09LNP=arblnp
301500141217     c                   eval      ILRY09NRS=arbnrs
301600141217     c                   eval      ILRY09NSP=arbnsp
301700141217     c                   eval      kpjbu=fnlry09ds2
301800141217     c                   call      'FNLRY09C'
301900141217     c                   parm                    kpjba
302000141217     c                   eval      fnlry09ds2=kpjbu
302100141217     c                   ENDSR
302200110923     C**************************************************************************
302300110923     C* controllo Perfect Order
302400110923     C**************************************************************************
302500110923     C     POrder        BEGSR
302600110926
302700110926     c                   eval      ilr78dtela=ds1ada
302800111012     c                   eval      ilr78sppar=ds1fl5
302900110926     c                   eval      kpjbu=fnlr78ds1
303000110926     c
303100110923     c                   call      'FNLR78R1'
303200110926     c                   parm                    kpjba
303300110926     c                   parm                    arbds
303400110923     c
303500110926     c                   eval      fnlr78ds1=kpjbu
303600110926     c
303700110923     C                   ENDSR
303800990329     C**************************************************************************
303900990329     C* PREPARO I DATI DELLA C.A.
304000990329     C**************************************************************************
304100990329     C     PRECA         BEGSR
304200990329     C                   SETOFF                                       06
304300990329     C                   CLEAR                   STACCH
304400990329     C* DATA APERTURA C.A.
304500990329     C                   Z-ADD     DCTDCA        G02INV                         AAAA/MM/GG
304600990329     C                   Z-ADD     *ZERO         G02DAT
304700990329     C                   MOVEL     '3'           G02ERR
304800990329     C                   CALL      'XSRDA8'
304900990329     C                   PARM                    WLBDAT
305000990329     C                   MOVE      G02DAT        W0020             2 0
305100990329     C                   MOVEL     G02DAT        STADCA            6 0          GG/MM/AA
305200990329     C                   MOVE      W0020         STADCA            6 0          GG/MM/AA
305300990329     C** DATA CNHIUSUA C.A.
305400990329     C                   CLEAR                   STACHC
305500990329     C     DCTDCH        IFGT      0
305600990329     C                   Z-ADD     DCTDCH        G02INV                         AAAA/MM/GG
305700990329     C                   Z-ADD     *ZERO         G02DAT
305800990329     C                   MOVEL     '3'           G02ERR
305900990329     C                   CALL      'XSRDA8'
306000990329     C                   PARM                    WLBDAT
306100990329     C                   MOVE      G02DAT        W0020             2 0
306200990329     C                   MOVEL     G02DAT        STACHC            6 0          GG/MM/AA
306300990329     C                   MOVE      W0020         STACHC            6 0          GG/MM/AA
306400990329     C                   SETON                                        06
306500990329     C* CAUSALE CHIUSURA
306600990329    2C     DCTCCH        IFNE      *BLANKS
306700990329     C                   CLEAR                   DCCH
306800990329     C                   CLEAR                   DSBS02
306900990329     C                   MOVEL     'C'           T02MOD
307000990329     C                   MOVEL     KNSIF         T02SIF
307100990329     C                   MOVEL     'CCH'         T02COD
307200990329     C                   MOVEL     DCTCCH        T02KE1
307300990329     C                   CALL      'TIBS02R'
307400990329     C                   PARM                    KPJBA
307500990329     C                   PARM                    DSBS02
307600990329     C*
307700990329    3C     T02ERR        IFEQ      ' '
307800990329     C                   MOVEL     T02UNI        DCCH
307900990329    3C                   ENDIF
308000990329     C                   MOVEL     §CHDES        STACCH           25
308100990329    2C                   ENDIF
308200990329     C                   ENDIF
308300990329     C                   ENDSR
308400970121     C**************************************************************************
308500970121     C* SCRIVO I RECORD DI TOTALE IN FNSCO
308600970121     C**************************************************************************
308700970121     C     WRTSCO        BEGSR
308800970130     C                   CLEAR                   FNSCO000
308900970130     C*
309000970130      * Se nella tabella 5C c'e' scritto di non trasmettere le statistiche
309100970130      * riempo il "flag di trasmissione" (SPAFTR)
309200040922     C***  §5CSCO        IFNE      'S'
309300040922     C***                MOVE      'N'           SCOFTR                         FLAG TRASMISS.
309400040922     C***                MOVEL     *HIVAL        SCODTR
309500040922     C***                ENDIF
309600040922     c                   z-add     wdate1        scodtr
309700040922     c
309800970207     C                   MOVEL     DS1FEL        SCOFLE
309900970225     C                   MOVEL     DS1LNA        SCOLNA
310000970207     C                   MOVEL     DS1ADA        SCODCM
310100970130     C**
310200970130     C* Se si tratta di un giorno festivo scrivo soltanto un record
310300970130     C*  indicando che e' festa con "F" in SCOSCO
310400970225    0C     WFEST         IFEQ      'S'
310500970130     C                   MOVEL     'F'           SCOSCO
310600970130     C                   WRITE     FNSCO000
310700970130     C                   ELSE
310800970130     C**
310900970130     C**
311000970121     C* LID: identifica in a che livello di deve stampare (livello
311100970121     c*      orrizontale, essendoci in stampa 4 scomparti-livelli)
311200970121     c* ORD: e' l'oridne di stampa posto in verticale
311300970122     c* OLT: =A se e' oltre un certo anticipo
311400970122     c*      =D se e' in ritardo/anticipi di un tot di ore
311500970122     c*      =O se e' oltre un certo ritardo
311600970122     c*      =T record di totale
311700970122     c*      =' '  se non c'e' affidabilita'
311800970128     C**
311900970121     C*  C O N S E G N A T E
312000970121     C                   MOVEL     1             SCOLIV
312100970121     C                   MOVEL     '2'           WTOTLI
312200970121     C                   Z-ADD     2             SCOORD
312300970121     C                   MOVEL     'C'           SCOSCO
312400970121     C                   MOVEL     'S'           WANRIT            1
312500970121     C                   MOVEL     ' '           WGBO              1
312600970121     C                   MOVEA     CNE           SKE
312700970121     C                   MOVEA     CNC           SKC
312800970121     C                   MOVEA     CNN           SKN
312900090918     C                   MOVEA     CNh           SKh
313000970121     C                   MOVEL     '2'           WTOTLI            1
313100970121     C                   EXSR      SCRIVI
313200970121     C*
313300970121     C*  C O N S E G N E   P A R Z I A L I
313400970121     C                   MOVEL     1             SCOLIV
313500970121     C                   Z-ADD     3             SCOORD
313600970121     C                   MOVEL     'N'           WANRIT            1
313700970121     C                   MOVEL     'P'           WGBO              1
313800970121     C                   CLEAR                   SKE
313900970121     C                   CLEAR                   SKC
314000970121     C                   CLEAR                   SKN
314100090918     C                   CLEAR                   SKh
314200970121     C                   Z-ADD     PRE(1)        SKE(1)
314300970121     C                   MOVEA     PRC(1)        SKC(1)
314400970121     C                   MOVEA     PRN(1)        SKN(1)
314500090918     C                   MOVEA     PRh(1)        SKh(1)
314600970121     C                   EXSR      SCRIVI
314700970121     C*  R I S E R V E     C O N S E G N E
314800970121     C                   MOVEL     1             SCOLIV
314900970121     C                   Z-ADD     4             SCOORD
315000970121     C                   MOVEL     'N'           WANRIT            1
315100970121     C                   MOVEL     'R'           WGBO              1
315200970121     C                   CLEAR                   SKE
315300970121     C                   CLEAR                   SKC
315400970121     C                   CLEAR                   SKN
315500090918     C                   CLEAR                   SKh
315600970121     C                   Z-ADD     RSE(1)        SKE(1)
315700970121     C                   MOVEA     RSC(1)        SKC(1)
315800970121     C                   MOVEA     RSN(1)        SKN(1)
315900090918     C                   MOVEA     RSh(1)        SKh(1)
316000970121     C                   EXSR      SCRIVI
316100970121     C*  N O N   C O N S E G N A T E
316200970121     C*          DI CUI GIACENZE APERTE
316300970121     C                   MOVEL     1             SCOLIV
316400970121     C                   MOVEL     '1'           WTOTLI            1
316500970121     C                   Z-ADD     7             SCOORD
316600970121     C                   MOVEL     'N'           SCOSCO
316700970121     C                   MOVEL     'S'           WANRIT            1
316800970121     C                   MOVEL     'G'           WGBO              1
316900970121     C                   MOVEA     GAE           SKE
317000970121     C                   MOVEA     GAC           SKC
317100970121     C                   MOVEA     GAN           SKN
317200090918     C                   MOVEA     GAh           SKh
317300970121     C                   EXSR      SCRIVI
317400970121     C*          DI CUI LASCIATI AVVISI
317500970121     C                   MOVEL     1             SCOLIV
317600970121     C                   Z-ADD     9             SCOORD
317700970121     C                   MOVEL     'L'           WGBO              1
317800970121     C                   MOVEA     LAE           SKE
317900970121     C                   MOVEA     LAC           SKC
318000970121     C                   MOVEA     LAN           SKN
318100090918     C                   MOVEA     LAh           SKh
318200970121     C                   EXSR      SCRIVI
318300970121     C*          DI CUI CHIUSE PER TURNO
318400970121     C                   MOVEL     1             SCOLIV
318500970121     C                   Z-ADD     11            SCOORD
318600970121     C                   MOVEL     'C'           WGBO              1
318700970121     C                   MOVEA     CTE           SKE
318800970121     C                   MOVEA     CTC           SKC
318900970121     C                   MOVEA     CTN           SKN
319000090918     C                   MOVEA     CTh           SKh
319100970121     C                   EXSR      SCRIVI
319200970121     C*          DI CUI NON FATTE
319300970121     C                   MOVEL     1             SCOLIV
319400970121     C                   Z-ADD     13            SCOORD
319500970121     C                   MOVEL     'N'           WGBO              1
319600970121     C                   MOVEA     NFE           SKE
319700970121     C                   MOVEA     NFC           SKC
319800970121     C                   MOVEA     NFN           SKN
319900090918     C                   MOVEA     NFh           SKh
320000970121     C                   EXSR      SCRIVI
320100090918
320200970121     C*          TOTALE DELLE NON CONSEGNATE
320300970121     C                   MOVEL     2             SCOLIV
320400970121     C                   Z-ADD     5             SCOORD
320500970121     C                   MOVEL     'N'           WANRIT            1
320600970121     C                   MOVEL     'T'           SCOOLT
320700970121     C                   CLEAR                   WGBO
320800970121     C                   CLEAR                   SKE
320900970121     C                   CLEAR                   SKC
321000970121     C                   CLEAR                   SKN
321100090918     C                   CLEAR                   SKh
321200970121     C                   XFOOT     GAE           W0090             9 0
321300970121     C                   ADD       W0090         SKE(1)
321400970121     C                   XFOOT     LAE           W0090             9 0
321500970121     C                   ADD       W0090         SKE(1)
321600970121     C                   XFOOT     NFE           W0090             9 0
321700970121     C                   ADD       W0090         SKE(1)
321800970121     C                   XFOOT     CTE           W0090             9 0
321900970121     C                   ADD       W0090         SKE(1)
322000970121     C*
322100970121     C                   XFOOT     GAC           W0090             9 0
322200970121     C                   ADD       W0090         SKC(1)
322300970121     C                   XFOOT     LAC           W0090             9 0
322400970121     C                   ADD       W0090         SKC(1)
322500970121     C                   XFOOT     NFC           W0090             9 0
322600970121     C                   ADD       W0090         SKC(1)
322700970121     C                   XFOOT     CTC           W0090             9 0
322800970121     C                   ADD       W0090         SKC(1)
322900970121     C*
323000970121     C                   XFOOT     GAN           W0090             9 0
323100970121     C                   ADD       W0090         SKN(1)
323200970121     C                   XFOOT     LAN           W0090             9 0
323300970121     C                   ADD       W0090         SKN(1)
323400970121     C                   XFOOT     NFN           W0090             9 0
323500970121     C                   ADD       W0090         SKN(1)
323600970121     C                   XFOOT     CTN           W0090             9 0
323700970121     C                   ADD       W0090         SKN(1)
323800090918     C*
323900090918     C                   XFOOT     GAh           W0090             9 0
324000090918     C                   ADD       W0090         SKh(1)
324100090918     C                   XFOOT     LAh           W0090             9 0
324200090918     C                   ADD       W0090         SKh(1)
324300090918     C                   XFOOT     NFh           W0090             9 0
324400090918     C                   ADD       W0090         SKh(1)
324500090918     C                   XFOOT     CTh           W0090             9 0
324600090918     C                   ADD       W0090         SKh(1)
324700090918
324800970121     C                   EXSR      SCRIVI
324900090918
325000970121     C*  I N     D I S T I N T A
325100970121     C                   MOVEL     1             SCOLIV
325200970121     C                   MOVEL     '2'           WTOTLI
325300970121     C                   Z-ADD     15            SCOORD
325400970121     C                   MOVEL     'D'           SCOSCO
325500970121     C                   MOVEL     'S'           WANRIT            1
325600970121     C                   MOVEL     ' '           WGBO              1
325700970121     C                   MOVEA     IDE           SKE
325800970121     C                   MOVEA     IDC           SKC
325900970121     C                   MOVEA     IDN           SKN
326000090918     C                   MOVEA     IDh           SKh
326100970121     C                   EXSR      SCRIVI
326200970121     C*  I N     C O N S E G N A
326300970121     C                   MOVEL     1             SCOLIV
326400970121     C                   MOVEL     '3'           WTOTLI
326500970121     C                   Z-ADD     17            SCOORD
326600970121     C                   MOVEL     'I'           SCOSCO
326700970121     C                   MOVEL     'S'           WANRIT            1
326800970121     C                   MOVEL     ' '           WGBO              1
326900970121     C                   MOVEA     ICE           SKE
327000970121     C                   MOVEA     ICC           SKC
327100970121     C                   MOVEA     ICN           SKN
327200090918     C                   MOVEA     ICh           SKh
327300970121     C                   EXSR      SCRIVI
327400970121     C*  NON    I N     C O N S E G N A
327500970121     C                   MOVEL     1             SCOLIV
327600970121     C                   MOVEL     '3'           WTOTLI
327700970121     C                   Z-ADD     19            SCOORD
327800970121     C                   MOVEL     'O'           SCOSCO
327900970121     C                   MOVEL     'S'           WANRIT            1
328000970121     C                   MOVEL     ' '           WGBO              1
328100970121     C                   MOVEA     NCE           SKE
328200970121     C                   MOVEA     NCC           SKC
328300970121     C                   MOVEA     NCN           SKN
328400090918     C                   MOVEA     NCh           SKh
328500970121     C                   EXSR      SCRIVI
328600970121     C*  C O N S E G N A B I L I
328700970121     C                   MOVEL     4             SCOLIV
328800970121     C                   Z-ADD     20            SCOORD
328900970121     C                   MOVEL     'T'           SCOOLT
329000970121     C                   MOVEL     'B'           SCOSCO
329100970121     C                   MOVEL     'N'           WANRIT            1
329200970121     C                   MOVEL     ' '           WGBO              1
329300970121     C                   CLEAR                   SKE
329400970121     C                   CLEAR                   SKC
329500970121     C                   CLEAR                   SKN
329600090918     C                   CLEAR                   SKh
329700970121     C                   XFOOT     COE           SKE(1)
329800970121     C                   XFOOT     COC           SKC(1)
329900970121     C                   XFOOT     CON           SKN(1)
330000090918     C                   XFOOT     COh           SKh(1)
330100970121     C                   EXSR      SCRIVI
330200970121     C*
330300970121     C     COE(1)        IFGT      0
330400970121     C     COC(1)        ORGT      0
330500970121     C     CON(1)        ORGT      0
330600090918     C     COh(1)        ORGT      0
330700970121     C                   MOVEL     1             SCOLIV
330800970121     C                   Z-ADD     21            SCOORD
330900970121     C                   MOVEL     'B'           SCOSCO
331000970121     C                   MOVEL     ' '           SCOOLT
331100970121     C                   Z-SUB     999           SCOORE
331200970121     C                   MOVEL     ' '           SCOGBO
331300970121     C                   Z-ADD     COE(1)        SCOSS1
331400970121     C                   Z-ADD     COC(1)        SCOSS2
331500970121     C                   Z-ADD     CON(1)        SCOSS3
331600090918     C                   Z-ADD     COh(1)        SCOSS4
331700970121     C                   WRITE     FNSCO000
331800970121     C                   ENDIF
331900970121     C     COE(2)        IFGT      0
332000970121     C     COC(2)        ORGT      0
332100970121     C     CON(2)        ORGT      0
332200090918     C     COh(2)        ORGT      0
332300970121     C                   MOVEL     1             SCOLIV
332400970121     C                   Z-ADD     22            SCOORD
332500970121     C                   MOVEL     'B'           SCOSCO
332600970121     C                   MOVEL     ' '           SCOOLT
332700970121     C                   Z-ADD     0             SCOORE
332800970121     C                   MOVEL     ' '           SCOGBO
332900970121     C                   Z-ADD     COE(2)        SCOSS1
333000970121     C                   Z-ADD     COC(2)        SCOSS2
333100970121     C                   Z-ADD     CON(2)        SCOSS3
333200090918     C                   Z-ADD     COh(2)        SCOSS4
333300970121     C                   WRITE     FNSCO000
333400970121     C                   ENDIF
333500970121     C     COE(3)        IFGT      0
333600970121     C     COC(3)        ORGT      0
333700970121     C     CON(3)        ORGT      0
333800090918     C     COh(3)        ORGT      0
333900970121     C                   MOVEL     1             SCOLIV
334000970121     C                   Z-ADD     23            SCOORD
334100970121     C                   MOVEL     'B'           SCOSCO
334200970121     C                   MOVEL     ' '           SCOOLT
334300970121     C                   Z-ADD     999           SCOORE
334400970121     C                   MOVEL     ' '           SCOGBO
334500970121     C                   Z-ADD     COE(3)        SCOSS1
334600970121     C                   Z-ADD     COC(3)        SCOSS2
334700970121     C                   Z-ADD     CON(3)        SCOSS3
334800090918     C                   Z-ADD     COh(3)        SCOSS4
334900970121     C                   WRITE     FNSCO000
335000970121     C                   ENDIF
335100970130     C*
335200970130     C                   ENDIF
335300970121     C*
335400970121     C                   ENDSR
335500970121     C**************************************************************************
335600970121     C* SCRIVO I RECORD DI TOTALE IN FNSCO
335700970121     C**************************************************************************
335800970121     C     SCRIVI        BEGSR
335900970121     C*
336000970121    1C     WANRIT        IFEQ      'S'
336100970121     C                   CLEAR                   WREC
336200970121     C*
336300970121    2C                   DO        13            X                 3 0
336400970121    3C     SKE(X)        IFGT      0
336500970121     C     SKC(X)        ORGT      0
336600970121     C     SKN(X)        ORGT      0
336700090918     C     SKh(X)        ORGT      0
336800970121     C*
336900970121     c* se il campo e' -999 o + 999 --> e' l'OLTRE
337000970121     c*  determino di quanto oltre e'
337100970121     C                   SELECT
337200970121    4C     ORE(X)        WHENEQ    -999                                         OLTRE
337300970121     C     X             ADD       1             Y                 3 0
337400970122     C                   MOVEL     'A'           SCOOLT
337500970121     C                   Z-ADD     ORE(Y)        SCOORE
337600970121    4C     ORE(X)        WHENEQ    999                                          OLTRE
337700970121     C     X             SUB       1             Y                 3 0
337800970121     C                   MOVEL     'O'           SCOOLT
337900970121     C                   Z-ADD     ORE(Y)        SCOORE
338000970121     C                   OTHER
338100970122     C                   MOVEL     'D'           SCOOLT
338200970122     C                   Z-ADD     ORE(X)        SCOORE
338300970121    4C                   ENDSL
338400970121     C*
338500970121     C                   MOVEL     WGBO          SCOGBO
338600970121     C                   Z-ADD     SKE(X)        SCOSS1
338700970121     C                   Z-ADD     SKC(X)        SCOSS2
338800970121     C                   Z-ADD     SKN(X)        SCOSS3
338900090918     C                   Z-ADD     SKh(X)        SCOSS4
339000970121     C                   WRITE     FNSCO000
339100970121     C                   CLEAR                   SCOOLT
339200970121     C                   MOVEL     'S'           WREC              1
339300970121    3C                   ENDIF
339400970121    2C                   ENDDO
339500970121     c**
339600970121     C* SCRIVO ANCHE IL RECORD DI TOTALE se richiesto
339700970121    2C     WTOTLI        IFNE      'N'
339800970121     c* solo se ho gia' scritto almeno un record di affidabilita'
339900970121     C     WREC          ANDEQ     'S'
340000970121     C                   CLEAR                   SCOORE
340100970121     C* l'ordine  di stampa del record di totale e' sempre 1 in meno
340200970121     C*  del dettaglio suddiviso per affidabilita'
340300970121     C                   SUB       1             SCOORD
340400970121     C* il livello della stampa del totale e' spesso diverso dal livell
340500970121     C*  del relativo dettaglio affidabilita', ecco perche' c'e' un
340600970121     C*  campo a parte che viene passato in questa rourine
340700970121     C                   MOVEL     WTOTLI        SCOLIV
340800970121     C* per identificare il record di totale metto un "T" in SCOOLT
340900970121     C                   MOVEL     'T'           SCOOLT
341000970121     C                   XFOOT     SKE           SCOSS1
341100970121     C                   XFOOT     SKC           SCOSS2
341200970121     C                   XFOOT     SKN           SCOSS3
341300090918     C                   XFOOT     SKh           SCOSS4
341400970121     C                   WRITE     FNSCO000
341500970121     C                   CLEAR                   SCOOLT
341600970121     C                   ENDIF
341700090918     C* SOLO 1 RECORD se non c'e' suddivisione di affidabilita'
341800970121   X1C                   ELSE
341900970121    2C     SKE(1)        IFGT      0
342000970121     C     SKC(1)        ORGT      0
342100970121     C     SKN(1)        ORGT      0
342200090918     C     SKh(1)        ORGT      0
342300970121     C*
342400970121     C                   CLEAR                   SCOORE
342500970121     C                   MOVEL     WGBO          SCOGBO
342600970121     C                   Z-ADD     SKE(1)        SCOSS1
342700970121     C                   Z-ADD     SKC(1)        SCOSS2
342800970121     C                   Z-ADD     SKN(1)        SCOSS3
342900090918     C                   Z-ADD     SKh(1)        SCOSS4
343000970121     C                   WRITE     FNSCO000
343100970121     C                   CLEAR                   SCOOLT
343200970121    2C                   ENDIF
343300970121    1C                   ENDIF
343400970121     C*
343500050706     C                   ENDSR
343600050706     C**************************************************************************
343700050706     C* Per bolla locale desumo la data dell'arrivo bolla
343800050706     C**************************************************************************
343900050706     c     BOLLALOC      begsr
344000050706     c     kfvv8         SETGT     FNFVV08L
344100050706     c     kfvv81        READe     FNFVV08L
344200070420    6C                   IF        not %eoF(FNFVV08L) AND FVVDFV>=kdataloc
344300050706    7C                   IF        FVVFCF=' '  or wtipolancio='SA'
344400050706     C                   EVAL      Wartdam=FVVDFV
344500050706    7C                   ENDIF
344600050706     C*
344700050706   X6C                   ELSE
344800070420     C* SE NON TROVATO LE STAMPO SEMPRE CON DATA  + 1
344900070420     C                   Z-ADD     kdataloc      G02INV
345000050706     C                   MOVEL     '3'           G02ERR
345100050706     C                   CALL      'XSRDA8'
345200050706     C                   PARM                    WLBDAT
345300050706     C*
345400050706     C     G02TGI        ADD       1             GIOTGI
345500050706     C                   CALL      'XSRGI8'
345600050706     C                   PARM                    WGIDAT
345700050706     C*
345800050706     C                   Z-ADD     GIOINV        WARTDAM
345900050706    6C                   ENDIF
346000050706     c                   ENDSR
346100060314     C**************************************************************************
346200060314     C* Verifico se giorno festivo
346300060314     C**************************************************************************
346400060314     c     CTRFESTA      begsr
346500060314     c                   clear                   ktfp
346600060314     c                   movel     ds1lna        ktfa
346700060314     C     KCLN          CHAIN     AZCLN01L                           34
346800060314     C     *IN34         IFEQ      *OFF
346900060314     c
347000060314     C     MAT(KGG)      IFEQ      'F'
347100060314     C     POM(KGG)      ANDEQ     'F'
347200060314     c                   eval      wfest='S'
347300060314     c                   else
347400060314     c                   eval      wfest=' '
347500060314     c                   endif
347600060314     c
347700060314     c                   endif
347800060314     c                   ENDSR
347900950123     C**************************************************************************
348000950123     O*  ?  S T A T I S T I C A    C O N S E G N E  ?
348100920605     OPRTF198   E            TESTA            02
348200920605     O                       TE(1)               66
348300940324     O                 N02   TE(2)              132
348400940324     O                  02   TE(3)              132
348500920605     O                       TE(4)              198
348600900523     O                       RSUT                20
348700970226     O               15                          47 '- RISTAMPA -'
348800970207     O                       DS1GDA             119 '  /  /    '
348900970207     O                  02   DS1GAL             136 '  /  /    '
349000950123     O                       WDATE              180 '  /  /    '
349100920605     O                       PAGE          Z    198
349200920605     O          E            TESTA       1
349300950123     O                       KNSIF               10
349400950123     O                       KNMUS               22
349500950123     O                       UTIME              179 '  :  :  '
349600940208     O          E            TESTA            04
349700920605     O                       TE(6)               66
349800920605     O                       TE(6)              132
349900920605     O                       TE(6)              198
350000090918     o* Senza tipo servizio H
350100090918     O          E   n25      TESTA            05
350200920605     O                       TE(7)               66
350300920605     O                       TE(10)             132
350400930908     O                       TE(14)             198
350500090604     o                       wdtspc              62
350600090604     o                       wdtspe              70
350700090604     o                       wdtspc             106
350800090604     o                       wdtspe             114
350900090604     o                       wdtspc             145
351000090604     o                       wdtspe             154
351100090604     o                       wdtspc             178
351200090604     o                       wdtspe             187
351300090918     O          E   n25      TESTA            06
351400920605     O                       TE(8)               66
351500920608     O                       TE(11)             132
351600930908     O                       TE(15)             198
351700970225     O                       DS1LNA              34
351800940323     O                       DESLNA              54
351900090918     O          E   n25      TESTA            07
352000940323     O                       TE(5)               66
352100930812     O                       TE(12)             132
352200930908     O                       TE(16)             198
352300090604     o                       wdtspd              62
352400090604     o                       wdtspd             106
352500090604     o                       wdtspd             145
352600090604     o                       wdtspd             178
352700090918     o* con tipo servizio H
352800090918     O          E    25      TESTA            05
352900090918     O                       TE(7)               66
353000090918     O                       TE(41)             132
353100090918     O                       TE(45)             198
353200090918     o                       wdtspc              62
353300090918     o                       wdtspe              70
353400090918     o                       wdtsph              78
353500100114     o                       wdtspc             112
353600100114     o                       wdtspe             121
353700100114     o                       wdtsph             130
353800100114     o                       wdtspc             157
353900100114     o                       wdtspe             166
354000100114     o                       wdtsph             175
354100090918     O          E    25      TESTA            06
354200090918     O                       TE(8)               66
354300090918     O                       TE(42)             132
354400090918     O                       TE(46)             198
354500090918     O                       DS1LNA              34
354600090918     O                       DESLNA              54
354700090918     O          E    25      TESTA            07
354800090918     O                       TE(5)               66
354900090918     O                       TE(43)             132
355000090918     O                       TE(47)             198
355100090918     o                       wdtspd              62
355200100114     o                       wdtspd             112
355300100114     o                       wdtspd             157
355400940208     O          E            TESTA            08
355500920608     O                       TE(6)               66
355600920608     O                       TE(6)              132
355700920608     O                       TE(6)              198
355800950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
355900930908     O* 1 INTESTAZIONE
356000090918     O          E   n25      INTE1       2
356100930908     O                       RIGA                66
356200930908     O                       TE(36)             132
356300930908     O                       TE(37)             198
356400090918     O                       GENNOR        Z    106
356500940203     O                                          107 '+'
356600940203     O                       GENESP        Z    115
356700940203     O                                          116 '='
356800940203     O                       GENTOT        Z    124
356900940324     O                 N02   CONP1              130 '0 ,  '
357000940324     O                 N02   CONP2              136 '0 ,  '
357100930909     O*
357200090918     O          E   n25      INTE1       0  0
357300930909     O                       RIGA                66
357400930909     O                       TE(36)             132
357500930909     O                       TE(37)             198
357600940203     O                       GENNOR        Z    106
357700940203     O                                          107 '+'
357800940203     O                       GENESP        Z    115
357900940203     O                                          116 '='
358000940203     O                       GENTOT        Z    124
358100940324     O                 N02   CONP1              130 '0 ,  '
358200940324     O                 N02   CONP2              136 '0 ,  '
358300090918     c* con tipo servizio H
358400090918     O          E    25      INTE1       2
358500090918     O                       RIGA                66
358600090918     O                       TE(49)             132
358700090918     O                       TE(50)             198
358800100114     O                       GENNOR        Z    112
358900100114     O                                         +  0 '+'
359000090918     O                       GENESP        Z   +  0
359100090918     O                                         +  0 '+'
359200090918     O                       GENh10        Z   +  0
359300090918     O                                         +  0 '='
359400100114     O                       GENTOT        Z   +  1
359500090918     O                 N02   CONP2             +  1 '0 ,  '
359600090918     O*
359700090918     O          E    25      INTE1       0  0
359800090918     O                       RIGA                66
359900090918     O                       TE(49)             132
360000090918     O                       TE(50)             198
360100100114     O                       GENNOR        Z    112
360200100114     O                                         +  0 '+'
360300090918     O                       GENESP        Z   +  0
360400090918     O                                         +  0 '+'
360500090918     O                       GENh10        Z   +  0
360600090918     O                                         +  0 '='
360700100114     O                       GENTOT        Z   +  1
360800090918     O                 N02   CONP2             +  1 '0 ,  '
360900950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
361000090918     O* 2 INTESTAZIONE
361100090918     O          E   n25      INTE2       2
361200930908     O                       RIGA                66
361300930908     O                       TE(36)             132
361400930908     O                       TE(37)             198
361500940203     O                       GENNOR        Z    145
361600940203     O                                          146 '+'
361700940203     O                       GENESP        Z    154
361800940203     O                                          155 '='
361900940203     O                       GENTOT        Z    163
362000940203     O                       CONP2              169 '0 ,  '
362100090918     O          E   n25      INTE2       0  0
362200930909     O                       RIGA                66
362300930909     O                       TE(36)             132
362400930909     O                       TE(37)             198
362500940203     O                       GENNOR        Z    145
362600090918     O                                          146 '+'
362700940203     O                       GENESP        Z    154
362800940203     O                                          155 '='
362900940203     O                       GENTOT        Z    163
363000940203     O                       CONP2              169 '0 ,  '
363100090918     o* con tipo servizio H
363200090918     O          E    25      INTE2       2
363300090918     O                       RIGA                66
363400090918     O                       TE(49)             132
363500090918     O                       TE(50)             198
363600100114     O                       GENNOR        Z    157
363700090918     O                                         +  0 '+'
363800090918     O                       GENESP        Z   +  0
363900090918     O                                         +  0 '+'
364000090918     O                       GENh10        Z   +  0
364100090918     O                                         +  0 '='
364200090918     O                       GENTOT        Z   +  0
364300100114     O              n26      CONP2              190 '0 ,  '
364400090918     O          E    25      INTE2       0  0
364500090918     O                       RIGA                66
364600090918     O                       TE(49)             132
364700090918     O                       TE(50)             198
364800100114     O                       GENNOR        Z    157
364900090918     O                                         +  0 '+'
365000090918     O                       GENESP        Z   +  0
365100090918     O                                         +  0 '+'
365200090918     O                       GENh10        Z   +  0
365300090918     O                                         +  0 '='
365400090918     O                       GENTOT        Z   +  0
365500100114     O              n26      CONP2              190 '0 ,  '
365600950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
365700090918     O* 3 INTESTAZIONE  solo senza tipo servizio H
365800940118     O          E            INTE3       2
365900930909     O                       RIGA                66
366000930909     O                       TE(36)             132
366100930909     O                       TE(37)             198
366200940203     O                       GENNOR        Z    178
366300940203     O                                          179 '+'
366400940203     O                       GENESP        Z    187
366500940203     O                                          188 '='
366600940203     O                       GENTOT        Z    196
366700930909     O          E            INTE3       0  0
366800930909     O                       RIGA                66
366900930909     O                       TE(36)             132
367000930909     O                       TE(37)             198
367100940203     O                       GENNOR        Z    178
367200940203     O                                          179 '+'
367300940203     O                       GENESP        Z    187
367400940203     O                                          188 '='
367500940203     O                       GENTOT        Z    196
367600950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
367700930908     O* 1 DETTAGLIO
367800090918     O          E   n25      DET1        1
367900930812     O                       RIGA                66
368000930908     O                       TE(36)             132
368100930908     O                       TE(37)             198
368200930908     O                       ORE(Y)        Z     48
368300970121     O                       PNORM         Z     62
368400930908     O                                           63 '+'
368500970121     O                       PESPR         Z     70
368600930908     O                                           71 '='
368700970121     O                       PTOTAL        Z     79
368800940526     O              N03      CONP1               85 '0 ,  '
368900940526     O              N04N02   CONP2               91 '0 ,  '
369000940324     O                 N02   CONP3               97 '0 ,  '
369100090918     o* con tipo servizio H
369200090918     O          E    25      DET1        1
369300090918     O                       RIGA                66
369400090918     O                       TE(49)             132
369500090918     O                       TE(50)             198
369600090918     O                       ORE(Y)        Z     48
369700090918     O                       PNORM         Z     62
369800090918     O                                         +  0 '+'
369900090918     O                       PESPR         Z   +  0
370000090918     O                                         +  0 '+'
370100090918     O                       ph1030        Z   +  0
370200090918     O                                         +  0 '='
370300100114     O                       PTOTAL        Z   +  0
370400100114     O              N03      CONP1               94 '0 ,  '
370500100114     O                 N02   CONP3              101 '0 ,  '
370600950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
370700930908     O* 2 DETTAGLIO
370800940203     O          E            DET4        1
370900940203     O                                              ' '
371000950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
371100090918     O          E      n25   DET2        1
371200930908     O                       RIGA                66
371300930908     O                       TE(36)             132
371400930908     O                       TE(37)             198
371500930908     O                       GENNOR        Z     62
371600930908     O                       GENESP        Z     70
371700940203     O                                           55 ''''
371800940203     O                                           63 '+'
371900930908     O                                           71 '='
372000930908     O                       GENTOT        Z     79
372100940329     O              N01   N03CONP1               85 '0 ,  '
372200940324     O              N01N02   CONP2               91 '0 ,  '
372300940324     O                 N02   CONP3               97 '0 ,  '
372400090918     O          E   N01n25   DET2        0  0
372500940207     O                       RIGA                66
372600940207     O                       GENNOR        Z     62
372700940207     O                       GENESP        Z     70
372800940207     O                                           63 '+'
372900940207     O                                           71 '='
373000940207     O                       GENTOT        Z     79
373100090918     O
373200940324     O                 N02   CONP2               91 '0 ,  '
373300940324     O                 N02   CONP3               97 '0 ,  '
373400090918     o* con tipo servizio H
373500090918     O          E       25   DET2        1
373600090918     O                       RIGA                66
373700090918     O                       TE(49)             132
373800090918     O                       TE(50)             198
373900090918     O                       GENNOR        Z     62
374000090918     O                                           55 ''''
374100090918     O                       GENESP        Z     70
374200090918     O                       GENh10        Z     78
374300090918     O                       GENTOT        Z     87
374400090918     O                                           63 '+'
374500090918     O                                           71 '+'
374600090918     O                                           79 '='
374700100114     O              N01   N03CONP1               94 '0 ,  '
374800100114     O                 N02   CONP3              101 '0 ,  '
374900090918     O          E   N01 25   DET2        0  0
375000090918     O                       RIGA                66
375100090918     O                       GENNOR        Z     62
375200090918     O                       GENESP        Z     70
375300090918     O                       GENh10        Z     78
375400090918     O                       GENTOT        Z     87
375500090918     O                                           63 '+'
375600090918     O                                           71 '+'
375700090918     O                                           79 '='
375800100114     O              N01   N03CONP1               94 '0 ,  '
375900100114     O                 N02   CONP3              101 '0 ,  '
376000950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
376100920609     O          E            DET3        2
376200920608     O                       TE(6)               66
376300920608     O                       TE(6)              132
376400920608     O                       TE(6)              198
376500950123     C**************************************************************************
376600950124     O*  ?  L I S T A  ?
376700920706     OQSYSPRT   E            SPED1          1 02
376800950124     O                                         +  3 'ANNO'
376900920706     O                                         +  2 'LNP'
377000920706     O                                         +  2 'SER'
377100920706     O                                         +  2 'SPEDIZ'
377200950124     O                                         +  1 'DATA SPED'
377300920706     O                                         +  2 'COLLI'
377400950124     O                                         +  3 'DATA ARR'
377500950124     O                                         +  2 'ABIL'
377600920706     O                                         +  1 'NUM DIST'
377700920706     O                                         +  2 'DAT DIST'
377800950124     O                                         +  2 'ULT CON'
377900950124     O***************                  +  1 'L.AVVISO'
378000920706     O                                         +  1 'CONSEGNA'
378100950124     O                                         +  3 'CON PARZ'
378200950124     O                                         +  2 'BLOCCO SP'
378300950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
378400920706     OQSYSPRT   E            SPED        1
378500940208     O                       STAFLG
378600940208     O                       ARBAAS            +  2
378700940208     O                       ARBLNP        Z   +  2
378800920706     O                       ARBNRS        Z   +  2
378900920706     O                       ARBNSP        Z   +  2
379000920706     O                       ARBMGS            +  2 '  /  '
379100920706     O                       ARBTSP            +  2
379200920706     O                       ARBNCL        Z   +  2
379300950124     O                       ARBDAM            +  2 '    /  /  '
379400950124     O                       ARBAMA            +  3
379500920706     O                       ARBNDC        Z   +  3
379600950124     O                       ARBDDC            +  2 '    /  /  '
379700950124     O                       ARBTMC            +  3
379800950124     O***************          ARBDLA  +  4 '  /  /  '
379900950124     O                       ARBDCM            +  3 '    /  /  '
380000950124     O                       ARBDCP            +  2 '    /  /  '
380100950123     O                       ARBFBC            +  3
380200950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
380300940203     O          E            SPEDT       1
380400940207     O                                              'CONSEGNATE'
380500940203     O                       STAC          Z   +  1
380600940208     O                                         +  3 'CONSEGNE PARZ'
380700940208     O                       STAP          Z   +  1
380800940208     O                                         +  3 'RISERVA CONS'
380900940208     O                       STAR          Z   +  1
381000940207     O                                         +  4 'IN DISTINTA'
381100940203     O                       STAI          Z   +  1
381200940207     O                                         +  4 'NON CONSEGNATE'
381300940203     O                       STAN          Z   +  1
381400940208     O          E            SPEDT       1
381500940208     O                                              'GIACENZE'
381600940208     O                       STAG          Z   +  1
381700950124     O                                         +  4 'L.AVVISI'
381800950124     O                       STAA          Z   +  1
381900940208     O                                         +  4 'CHIUSE X TURNO'
382000940208     O                       STAT          Z   +  1
382100940208     O                                         +  4 'DA CONSEGNARE'
382200940208     O                       STAD          Z   +  1
382300950123     O**************************************************************************
382400950123     O*  ?  ELENCO  SPEDIZIONI  NON  IN  CONSEGNA  ?
382500940601     OPRTF198X  E            TESELE           02
382600040608     O              n08      ELE(2)             132
382700040608     O              n08      ELE(3)             198
382800040608     O               08      ELE(14)            132
382900040608     O               08      ELE(15)            198
383000940530     O                       RSUT                20
383100950123     O                       WDATE              180 '  /  /    '
383200950123     O                       PAGE1         Z    198
383300940530     O          E            TESELE           03
383400950123     O                       KNSIF               10
383500950123     O                       KNMUS               22
383600940601     O                       ELE(4)             132
383700100222     o                       p0dpart             80
383800970207     O                       DS1GDA             104 '  /  /    '
383900950123     O                       UTIME              179 '  :  :  '
384000940530     O          E            TESELE           05
384100940601     O                       ELE(5)              66
384200940601     O                       ELE(8)             132
384300940601     O                       ELE(11)            198
384400940530     O          E            TESELE         1 06
384500940601     O                       ELE(6)              66
384600940601     O                       ELE(9)             132
384700940601     O                       ELE(12)            198
384800950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
384900940530     O          E            DETELE         1
385000950127     O                       P0CAAS
385100940530     O                       ARBLNP        Z   +  1
385200940530     O                       ARBNRS        Z   +  1
385300940530     O                       ARBNSP        Z   +  1
385400940530     O                       P0CMGS            +  1 '  /  '
385500940531     O                       ARBTSP            +  1
385600940601     O                       ARBLNA        Z   +  1
385700940601     O                       ARBZNC        Z   +  2
385800991130     O                       P0CRSD            +  1
385900950125     O                       P0CIND            +  1
386000940601     O                       ARBCAD            +  1
386100950127     O                       P0CLOD            +  0
386200940601     O                       ARBPRD            +  1
386300950127     O                       ARBNZD            +  1
386400940601     O                       ARBGC1            +  1
386500940601     O                       ARBGC2            +  1
386600950123     O                       ARBTC1            +  1
386700940601     O                                         +  0 '-'
386800950127     O                       P0DTC1            +  0
386900950123     O                       ARBNCL        Z   +  0
387000950127     O                       ARBPKF        4   +  0
387100991130     O                       ARBCBO            +  1
387200940530     O                                         +  0 '-'
387300950127     O                       P0DCBO            +  0
387400950125     O                       AR9CAS        4   +  0
387500991130     O                       AR9VCA            +  0
387600991130     O                       COMIFT            +  1
387700991130     O                       comdiv            +  0
387800991130     O                       ARBTMC            +  1
387900051128     O                       p0cTCR            +  1
388000940531     O                       P0CDCR        Z   +  1
388100940530     O                       ARBHCR            +  1 '  :  '
388200100222     O                       ARBgma            +  1
388300141119     O              n70      NUM3          M   +  1
388400141119     O               70                         198 'DIR'
388500950123     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
388600950127     O          E            DE2ELE         1
388700990329     O               07                             'C.A.'
388800990329     O               07      DCTFIL               8
388900990329     O               07      DCTNCA        Z     15
389000990329     O               07                          19 'del'
389100990329     O               07      STADCA              28 '  /  /  '
389200990329     O               07    06                    35 'chiusa'
389300990329     O               07    06STACHC              44 '  /  /  '
389400990329     O               07    06DCTCCH            +  1
389500990329     O               07    06                  +  0 '-'
389600990329     O               07    06STACCH            +  0
389700991130     O                       ARBTC2             114
389800991130     O                                          115 '-'
389900991130     O                       P0DTC2             125
390000991130     O                       P0CNO1             161
390100991130     O                       P0CNO2             197
390200990329     O          E            DETCA          1
390300990329     O                                              'C.A.'
390400990329     O                       DCTFIL               8
390500990329     O                       DCTNCA        Z     15
390600990329     O                                           19 'del'
390700990329     O                       STADCA              28 '  /  /  '
390800990329     O                     06                    35 'chiusa'
390900990329     O                     06STACHC              44 '  /  /  '
391000990329     O                     06DCTCCH            +  1
391100990329     O                     06                  +  0 '-'
391200990329     O                     06STACCH            +  0
391300060905     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
391400060905      * spedizione in distinta
391500060905     O          E    12      DETDIS         1
391600060905     O                       wstadis            197
391700000703     O*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
391800060905      * C/ASSEGNO NON ABILITATO
391900000703     O          E            DETCAA         1
392000110923     O              n18 28                      159 'Non Abilitato'
392100110923     O               18 28                       53 'Abilitazioni Bolla:'
392200151008     O               18 28   Wabisin          +   1
392300110923     O               19                       +   4 'Perfect Order:'
392400110927     O               19      stPorder         +   1
392500151008     O               22      wcalert          +   2
392600050805     O          E            FINES       2
392700050805     o                                          108 '*** FINE STAMPA ***'
392800950123     O**************************************************************************
392900950125**
393000090918XXXXXXXXXXXXXXXXXXXX                                      S T A T    1
393100950124I S T I C A    C O N S E G N E    D E L                              2
393200950124I S T I C A    C O N S E G N E    D A L                A L           3
393300950124                      FNLR78R                            PAG. XXXX   4
393400950123                                                      '  DISTR       5
393500930907------------------------------------------------------------------   6
393600090918                                                      '  yyyyy   y   7
393700950123L I N E A   D I   A R R I V O: XXX-XXXXXXXXXXXXXXXXXXX'    +         8
393800090917                                                      '1234567+123   9
393900090917SPR   TOTALE ' % SU' % SU' % SU'   CORR    ESPR    TOTALE ' % SU'   10
394000090917             'CONS.'TOTAL'CONSE'     +                    'TOTAL'C  11
394100090917             'PERIO'IN CO' GNAB'   DISTR                  'IN CO'   12
3942000909174567=12345678'XX,XX'XX,XX'XX,XX'XXXXXXXX+Xxxxxxxx=XXXXXXXX'XX,XX'X  13
394300090918% SU'   yyyyy    yyyyy  TOTALE ' % SU'   yyyyy    yyyyy  TOTALE     14
394400090917ONSE'     +                    'CONSE'     +                        15
394500090917GNAB'   DISTR                  ' GNAB'   DISTR                      16
394600090917X,XX'Xxxxxxxx+xXxxxxxx=xxxxxxxx'XX,XX'xXxxxxxx+xxxXxxxx=xxxxxxxx    17
394700090917SPEDIZIONI           C O N S E G N A T E:             '             18
394800940203                           In Anticipo oltre xxx Ore  '             19
394900940203                           In Anticipo di    xxx Ore  '             20
395000940323                           In CONFORMITA'             '              1
395100940203                           In Ritardo  di    xxx Ore  '              2
395200940203                           In Ritardo  oltre xxx Ore  '              3
395300970130SPEDIZIONI   N O N   C O N S E G N A T E:             '             24
395400940203              di cui  "GIACENZE APERTE"               '             25
395500940203              di cui  "LASCIATI AVVISI"               '             26
395600940203              di cui  "NON  FATTE"                    '             27
395700940203              di cui  "CHIUSE PER TURNO"              '             28
395800970130SPEDIZIONI   ANCORA  I N   D I S T I N T A:           '             29
395900970130TOTALE SPED.         I N   C O N S E G N A:           '             30
396000970130SPEDIZIONI   N O N   I N   C O N S E G N A:           '             31
396100970130TOTALE SPED. C O N S E G N A B I L I:                 '             32
396200940323                                       In Anticipo    '             33
396300940323                                       In CONFORMITA' '             34
396400940323                                       In Ritardo     '             35
396500940203             '     '     '     '                          '     '   36
396600940203    '                          '     '                              37
396700940208              di cui  "CONSEGNE PARZIALI"             '             38
396800940208              di cui  "RISERVE  CONSEGNE"             '             39
396900090917                                                      '1234567+123  40
397000100114yyyy   yyyyy  TOTALE ' % SU ' % SU '     yyyyy    yyyyy    yyyyy
397100100114                     ' CONS.' CONSE'       +
397200100114                     'PERIOD' GNAB.'     DISTR
3973001001144567+1234567=12345678' xx,xx' XX,XX'  XXXXXXXX+Xxxxxxxx+12345678=
397400100114 TOTALE ' % SU'     yyyyy    yyyyy    yyyyy  TOTALE ' % SU'         45
397500100114        'CONSE'       +                             'CONSE'         46
397600100114        ' GNAB'     DISTR                           ' GNAB'         47
39770010011412345678'XX,XX'  12345678+12345678+12345678=12345678'XX,XX'         48
397800100114                     '      '      '                                49
397900100114        '     '                                     '     '         50
398000940324** Comandi    567890123456789012345678901234567890123456789012345678901234567890
398100940324OVRPRTF FILE(PRTF198)  USRDTA('Stat.Cons.')
398200940530** Comandi    567890123456789012345678901234567890123456789012345678901234567890
398300940601OVRPRTF FILE(PRTF198X) USRDTA('Stat.Cons.')
398400940530**
398500090918XXXXXXXXXXXXXXXXXXXX                                                 1
398600940601         **  ELENCO  SPEDIZIONI  NON  IN  CONSEGNA  **               2
398700950125                       FNLR78R                           PAG. XXXX   3
398800950125                        AL  XX/XX/XXXX                               4
398900991130   S P E D I Z I O N E     ARRIVO      D    E    S    T    I   N     5
399000991130                          LIN ZON RAGIONE SOCIALE INDIRIZZO          6
399100950125                                                                     7
399200991130  A    T    A    R    I    O              GG   CONSEG COLLI    PES   8
399300991130         C.A.P    LOCALITA'      PRO NAZ CHIUS PARTIC                9
399400950125                                                                    10
399500100222O CD.BOLLA     C/ASSEGNO       TOTALE   MAN CONSEGNA   PARTIC ORE   11
399600100222                              FATTURA   CON RICHIESTA    CONS.RIT.  12
399700950125                                                                    13
399800040608      **  ELENCO SPEDIZIONI NON IN CONSEGNA: considera spunte part  14
399900040608enza  **                  FNLR78R                        PAG.       15

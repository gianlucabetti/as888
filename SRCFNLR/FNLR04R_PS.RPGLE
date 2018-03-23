000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940727     H* FNLR04R *----------------------------------------------------*
000300941201     H*           GESTIONE ARCHIVIO SPUNTE
000400000000     H*--------------------------------------------------------------*
000500040122     FFNLR04D   CF   E             WORKSTN SFILE(LR04S02:NRR2)
000600090420     F                                     INFDS(slr04)
000700150824     FFNAGB01L  UF A E           K DISK
000800941201     FTABEL00F  IF   E           K DISK
000900020808     FAZORG01L  IF   E           K DISK
001000941130     FFNFVV01L  IF   E           K DISK
001100000503     FFNFGW01L  IF   E           K DISK
001200941130     FFNFGV01L  IF   E           K DISK
001300941205     FFNFVA01L  IF   E           K DISK
001400000522     FFNFWA01L  IF   E           K DISK
001500070116     FFnBRV00F  UF A E             DISK
001600070116     FFnBRV01L  UF   E           K DISK    RENAME(FnBRV000:FnBRV001)
001700941201     F                                     INFDS(TARD1)
001800070116     FFnBRV02L  IF   E           K DISK    RENAME(FnBRV000:FnBRV002)
001900941201     F                                     INFDS(TARD2)
002000140625     FFNBRV05L  uF   E           K DISK    RENAME(FnBRV000:FnBRV005)
002100941201     F                                     INFDS(TARD3)
002200070116     FFnBRV07L  IF   E           K DISK    RENAME(FnBRV000:FnBRV007)
002300101201     FFNARB01L  IF   E           K DISK    prefix(BLP:3)
002400120615     FFNART27L  iF   E           K DISK    prefix(BLT:3)
002500101201     FFNART01L  iF   E           K DISK    RENAME(Fnart000:Fnart001)
002600101202     f                                     prefix(BLT:3)
002700941130     FFNBLP01L  IF   E           K DISK
002800941130     FFNBLT27L  IF   E           K DISK
002900101201     FFNBLT01L  IF   E           K DISK    RENAME(Fnblt000:Fnblt001)
003000070116     FFnBRVE2L  UF A E           K DISK    RENAME(FnBRV000:FnBRVE)
003100070710     FFnBRVE1L  uF   E           K DISK    RENAME(FnBRV000:FnBRVE1)
003200090416     f                                     prefix(e_)
003300040122
003400101129     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
003500101129     D L6              S              3  0 DIM(30)                              FIL GEST ARRIV
003600930205     D L6S             S              3  0 DIM(30)                              FIL GEST DA AL
003700971003     D* L6 DELLA FILIALE GESTIONE INIZIALE
003800971003     D L6F             S              3  0 DIM(30)
003900920807     D*
004000101201     D CATor           S              3    DIM(20)                              CATEGORIE
004100101201     D CAT             S              1    DIM(20)                              CATEGORIE
004200920810     D DEC             S             10    DIM(20)                              DESCR CATEGOR
004300941209     D NST             S              1    DIM(20)                              NR.SPUNTE GG.
004400920810     D*
004500920810     D SPG             S              1    DIM(20)                              TIPO INVENTAR
004600941201     D DES             S              8    DIM(20)                              DESCR TIPO INV
004700960503     D FVA             S              9  0 DIM(70)                              FVA DA INTERR
004800960503     D*
004900960503     D FG              S              3  0 DIM(30)                              FIL GESTIBILI
005000920810     D KPJBA         E DS
005100140305     D fims40ds      E DS
005200031117     D L1              S              3  0 DIM(30)
005300090420     d
005400090420     D SLR04           DS
005500090420     D  pagnrr               378    379B 0
005600941201     D TARD1           DS
005700941201     D  BR1NRR               397    400B 0
005800900615     D TARD2           DS
005900900615     D  BR2NRR               397    400B 0
006000941201     D TARD3           DS
006100941201     D  BR3NRR               397    400B 0
006200910423     D WLBDAT          DS
006300941130     D  G02DAT                 1      8  0
006400941130     D  G02INV                 9     16  0
006500941130     D  G02ERR                17     17
006600941130     D  G02TGI                18     22  0
006700910423     D WGIDAT          DS
006800941130     D  GIODAT                 1      8  0
006900941130     D  GIOINV                 9     16  0
007000941130     D  GIOTGI                17     21  0
007100160729     D sflrcd          DS
007200941213     D  V2CNFV                 1      5  0
007300941213     D  V2CNPG                 6      6  0
007400941213     D  V2CLNP                 7      9  0
007500941213     D  V2CLNA                10     12  0
007600941213     D  V2CNRS                13     14  0
007700941213     D  V2CNSC                15     21  0
007800941213     D  V2CCAN                22     22
007900950704     D  V2CVUC                23     29  6
008000960522     D  V2CANN                30     30
008100990907     D  V2CPUC                31     37  3
008200930309     D  RECORD                 7     21  0
008300921020     D* CAMPI CONFRONTO
008400990907     D  VI2CON                 1     22
008500921230     D  VI2CO2                 6     21  0
008600990907     D  RCDSFL                 1     37
008601160728     d  v2cnrr                38     46  0
008602160728     d  v2ctps                47     47
008603160728     d  v2crps                48     48
008604160728     d  v2cse                 49     49
008605160728     d  v2cold                50     64  0
008606160728     d  v2hdfv                65     72  0
008607160728     d  v2hnpg                73     73  0
008608160728     d  V2HESI                74     74
008609160728     d  V2HI19                75     75
008610160728     d  V2HI21                76     76
008611160728     d  V2HI23                77     77
008612160728     d  V2HI24                78     78
008613160728     d  V2HI25                79     79
008614160728     d  V2HI63                80     80
008615160728     d  V2HI66                81     81
008616160728     d  V2HI70                82     82
008617160728     d  V2HI86                83     83
008618160728     d  V2HI94                84     84
008619160728     d  V2HI71                85     85
008620160728     d  V2HI72                86     86
008621160728     d  V2HI75                87     87
008622160728     d  V2HI76                88     88
008623160728     d  v2hrcd                89    125
008624160728     d  v2hvuc               126    132  6
008625160728     d  v2hpuc               133    139  3
008626160728     d  v2hdcs               140    147  0
008627160729     d  v2cfgs               148    150
008628160728     d  v2dspg               151    153
008629160728     d  v2cdfv               154    157  0
008630160728     d  v2dlnp               158    165
008631160729     d  v2dlna               156    163
008632160729     d  v2cznc               164    165
008633160729     d  v2dcan               166    170
008634160729     d  v2cnps               171    172
008635160729     d  v2canm               173    173
008636160729     d  v2cpes               174    176  0
008637160729     d  v2cdfs               177    184  0
008638160729     d  v2chms               185    190  0
008639160729     d  v2cmis               191    193  0
008640160729     d  v2cpru               194    203
008641160729     d  v2ctap               204    210
008642160729     d  v2datr               211    218
008643160729     d  v2ctsu               219    219
008644160729     d  v2cdcs               220    227  0
008645160729     d  v2chcs               228    233  0
008646160729     d  v2cftr               234    234
008647160729     d  v2cdtr               235    242  0
008648160729     d  v2chtr               243    246  0
008649160729     d  v2cabb               247    258
008650160729     d  v2hdfs               259    266  0
008651160729     d  Selected                      1A
008700970117     D                 DS
008800000118     D  WOKNFV                 1      6  0
008900000118     D  WOKNPG                 7      7  0
009000000118     D  WOKFGS                 8     10  0
009100000118     D  BRVKEY                 1     10  0
009200921230     D                 DS
009300941202     D  DATA                   1      8  0
009400941202     D  MM                     5      6  0
009500941202     D  GG                     7      8  0
009600941201     D PARAM1          DS
009700941201     D* PARAM1: PARAMETRI IN INPUT/OUTPUT AL PGM
009800941201     D  V1CNFV                 1      5
009900941201     D  V1CLNP                 6      8  0
010000941201     D  V1CLNA                 9     11  0
010100941201     D  V1CNRS                12     13  0
010200941201     D  V1CNCD                14     20  0
010300941201     D  V1CNCA                21     27  0
010400941201     D  V1CNPS                28     29  0
010500941201     D  V1CNPG                30     30
010600101129     D* PA1FLG: '1'=INTERROGAZIONE   con prima videata
010700101129     D*         '2'=manutenzione   senza prima videata
010800101129     D*         '3'=Interrogazione senza prima videata
010900941201     D  PA1FLG                31     31
011000941220     D* PA1PGM: PGM CHIAMANTE (SE RICHIAMATO)
011100941201     D  PA1PGM                32     38
011200941220     D  V1CFGS                39     41  0
011300101129     d* bolla
011400101129     d  v1cbolp               42     44  0
011500101129     d  v1cbors               45     46  0
011600101129     d  v1cbosp               47     53  0
011700101129     d  v1cboan               54     57  0
011800101129     D  PA1ERR                58     58
011900101129     d
012000921020     D PARAM2          DS
012100941201     D* PARAM2: PASSAGGIO AD ANNULLAMENTO ANOMALIE E DATE
012200000118     D  WRVNFV                 2      6  0
012300921230     D  BRVNPG                 7      7  0
012400921020     D  BRVLNP                 8     10  0
012500921020     D  BRVLNA                11     13  0
012600921020     D  BRVNRS                14     15  0
012700921020     D  BRVNSC                16     22  0
012800921020     D  BRVCAN                23     23
012900941201     D  BRVVUC                24     30  6
013000941201     D  PARFLG                31     31
013100941201     D  PARFEL                92     94  0
013200941201     D  BRVNPS                95     96  0
013300941212     D  BRVFGS                97     99  0
013400970530     D  BRVFLE               100    102  0
013500031117     D  BRVpes               103    105  0
013600921020     D* CAMPI CONFRONTO
013700990907     D  BRVCON                 2     23
013800921230     D  BRVCO2                 7     22  0
013900010628     D  BRVREC                 8     22  0
014000951108     D*
014100130520     D* CHIAMATA AL PGM DI INTERROGAZ.FV PARTENZA             -FNLST3R-
014200130520     D fnlst3ds        DS
014300951108     D  PA5FLG                 1      1    INZ('R')
014400951108     D  PA5FL2                 2      2    INZ
014500951108     D  PA5NFV                 3      5P 0 INZ
014600951108     D  PA5DIN                 6     10P 0
014700951108     D  PA5DFI                11     15P 0
014800951108     D  PA5TFV                16     16    INZ
014900000614     D  PA5FGS               242    244  0
015000951108     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
015100951108     D*   SELEZIONATI ---> SE NON UTILIZZATO NON LO DEFINISCO A PGM
015200951108     D***                                 P  17 2410FVA
015300951108     D*
015400950704     D* CHAIAMATA AL PGM DI RICERCA FOGLI PARTENZA ALL'ARRIVO -FNLR26R-
015500950704     D PARAM6          DS
015600950704     D  PA6PGM                 1      7
015700950704     D  PA6LAI                 8     10  0
015800950704     D  PA6FEL                11     13  0
015900950704     D  PA6ABB                14     14
016000950704     D  PA6DSD                15     22  0
016100950704     D  PA6DSA                23     30  0
016200950704     D  PA6FLG                31     31
016300950704     D  PA6FGS                32     34  0
016400971002     D  PA6LAN                35     35    INZ('4')
016500980204     D  PA6MSG                36    113    INZ
016600920807     D DS7N          E DS
016700921229     D DS7J          E DS
016800960829     D DSTV          E DS
016900040625     D DS3E          E DS
017000000522     D OG143         E DS
017100900615     D CNCR80        E DS
017200020808     D UT§DSE0F      E DS
017300960522     D DSLR44        E DS                  EXTNAME(FNLR44DS)
017400961121     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
017500961121     D DSLR45        E DS                  EXTNAME(FNLR45DS)
017600941201     D DSUL06        E DS                  EXTNAME(TRUL06DS)
017700941201     D  LIN                    1     90  0
017800941201     D                                     DIM(30)                              FIL. COMODO
017900971003     D* REPERISCE DATI FOGLI
018000971003     D DSLV53        E DS                  EXTNAME(FNLV53DS)
018100971003     D* CALCOLA TERMINAL DI ARRIVO
018200971003     D DSLV55        E DS                  EXTNAME(FNLV55DS)
018300070116     D*
018400070116     D tibs02ds      E DS
018500070220     d**
018600070220     D FNLS45DS      E DS
018700070221     D DSRECBRV      E DS                  extname(fnbrv00f)  prefix(D_)
018800971003     D**
018900941212     D DSLR01        E DS                  INZ
019000941213     D                 DS
019100941213     D*FILIALE DI SCARICO (QUANDO SI LEGGE FNFGV00F)
019200941213     D  FGVFFV                 1    240
019300941213     D  FGVFF2               241    447
019400000503     D  FGWFF3               448    687
019500000503     D  FGWFF4               688    897
019600000503     D  FFV1                   1    897
019700000503     D                                     DIM(299)                             FIL. FO. VIAGG
019800010628     D                 DS
019900010628     D  DS2NPG                 6      6  0
020000010628     D  DS2LNP                 7      9  0
020100010628     D  DS2LNA                10     12  0
020200010628     D  DS2NRS                13     14  0
020300010628     D  DS2NSC                15     21  0
020400010628     D  DS2RCD                 1     37
020500020808      *
020600020808      * Variabili
020700020808      *
020800020808     D COMDFS          S              8  0 INZ
020900020808     D COMHMS          S              4  0 INZ
021000090420     D savpagnrr       S                   LIKE(pagnrr)
021100090420     D sa2nrr          S                   LIKE(br1nrr)
021200020924     D WLANTW          S                   LIKE(§OGNTW)
021300020808     D SAVNTW          S                   LIKE(§OGNTW)
021400020808     D WSANPA          S                   LIKE(D53NPA)
021500020808     D WSANFA          S                   LIKE(D53NFA)
021600020808     D FGSTFP          S                   LIKE(D55TFP)
021700020808     D KFLE            S                   LIKE(BRVFLE)
021800020808     D KLN             S                   LIKE(BRVLNP)                          LINEA PART/ARR
021900020808     D KNPG            S                   LIKE(FVVNPG)                          CATEGORIA FOGLIO
022000020808     D KNFV            S                   LIKE(FVVNFV)                          NR.FOGLIO
022100020808     D KFGS            S                   LIKE(FVVFGS)                          FIL.GESTIONE
022200020808     D KCOD            S                   LIKE(TBLCOD)                          CODICE TABELLA
022300020808     D KKEY            S                   LIKE(TBLKEY)                          CODICE ELEMENTO
022400020808     D VI1NFV          S                   LIKE(BRVNFV)                          NR.FOGLIO VIAGGIO
022500020808     D COMVUC          S              7  6
022600020808     D COMPUC          S                   LIKE(BRVPUC)
022700020808     D COMZNC          S                   LIKE(BRVZNC)
022800020808     D WDFV            S                   LIKE(FVVDFV)                         DATA FOGLIO (A/M/G)
022900020808     D BRVDFV          S                   LIKE(WDFV)                           DATA FOGLIO (A/M/G)
023000080318     D TSU03           S                   LIKE(BRVTSU)
023100080318     D CAN03           S                   LIKE(BRVCAN)
023200070215     D NPS03           S                   LIKE(BRVNPS)
023300070215     D TAP03           S                   LIKE(BRVTAP)
023400070228     D DCS03           S                   LIKE(BRVdcs)
023500070228     D HCS03           S                   LIKE(BRVhcs)
023600020808     D WNPS            S                   LIKE(BRVNPS)
023700020808     D WPOSPU          S                   LIKE(BRVPES)
023800101202     D WFE0            S                   LIKE(blpTFA)
023900020808     D WFGS            S                   LIKE(V1CFGS)
024000101202     D WTFP            S                   LIKE(blpTFP)
024100101202     D WTFA            S                   LIKE(blpTFA)
024200020808     D NPS1            S                   LIKE(BRVNPS)
024300020808     D NPS2            S                   LIKE(BRVNPS)
024400020808     D NPSP            S                   LIKE(BRVNPS)
024500020808     D SAWNPS          S                   LIKE(BRVNPS)
024600070220     D SAWtap          S                   LIKE(BRVtap)
024700020808     D SAWCAN          S                   LIKE(BRVCAN)
024800020808     D SAWPUC          S                   LIKE(BRVPUC)
024900020808     D SAWVUC          S                   LIKE(BRVVUC)
025000020808     D SAWDCS          S                   LIKE(BRVDCS)
025100020808     D SAWHCS          S                   LIKE(BRVHCS)
025200020808     D DS2NPS          S                   LIKE(BRVNPS)
025300031120     D tfalna          S                   LIKE(d55tfa)
025400031120     D tfapes          S                   LIKE(d55tfa)
025500031120     D savpes          S                   LIKE(d55tfa)
025600070215     D TAPP            S                   LIKE(BRVTAP)
025700070215     D TAP1            S                   LIKE(BRVTAP)
025800070215     D TAP2            S                   LIKE(BRVTAP)
025900070228     D DCSP            S                   LIKE(BRVdcs)
026000070228     D DCS1            S                   LIKE(BRVdcs)
026100070228     D DCS2            S                   LIKE(BRVdcs)
026200070228     D HCSP            S                   LIKE(BRVhcs)
026300070228     D HCS1            S                   LIKE(BRVhcs)
026400070228     D HCS2            S                   LIKE(BRVhcs)
026500101201     D klnps           S                   LIKE(v1clnp)
026600101201     D klnas           S                   LIKE(v1clna)
026700101201     D knrss           S                   LIKE(v1cnrs)
026800101201     D knscs           S                   LIKE(v1cncd)
026900101201     D kfgss           S                   LIKE(v1cfgs)
027000101201     D SAVNCA          S                   LIKE(v1cnca)
027100101203     D SAVNCd          S                   LIKE(v1cnca)
027200101201     D boldsp          S              8  0 inz
027300070710     D WNST            S              1    inz
027400020808     D WRIEM           S              1    inz                                  RIEMETTO TESTATA
027500020808     D NUM3            S              3  0 inz
027600020808     D XI              S              2  0 inz
027700101126     D wlung           S              2  0 inz
027800070215     D wtimez          S               z
027900090420     D dataeur         S               d   datfmt(*eur)
028000090420     D dataiso         S               d   datfmt(*iso)
028100070215     D oraiso          S               t   timfmt(*iso)
028101160728     d ord_dta         c                   'F11=ORD x Data'
028102160728     d ord_nfv         c                   'F11=ORDxFoglio'
028103160728     d w_nfv           C                   1
028104160728     d w_dta           C                   2
028106160728     d W_ord           S              1  0
028107160729     D MaxKey          C                    3
028110160728     D Ascendente      C                   1
028111160728     D Discendente     C                   2
028112160728     D Carattere       C                   6
028113160728     D Put             C                   1
028114160728     D EndPut          C                   2
028115160728     D Get             C                   3
028116160728      **********************************************************************
028120160728      *     SizeList   - Size of list
028121160728      *     ReturnSize - Size of list returned by sort API
028122160728      **********************************************************************
028123160728     D NotUsed         S             16A
028124160728     D ReturnSize      S              9B 0
028126160728     D SizeList        S              9B 0
028127160728     D RrnLast         S              5I 0
028128160728      /COPY QSYSINC/QRPGLESRC,QLGSORT
028129160728     D QLGKL                         16    DIM(MaxKey)
028130160728     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
028131160728     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
028132160728     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
028133160728     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
028134160728
028135160728      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
028136160728      /COPY QSYSINC/QRPGLESRC,QUSEC
028200900521     C****************************************************************
028300900521     C*  RIEPILOGO INDICATORI
028400900521     C***************************************************************
028500941202     C* 01    - SELEZIONATO UN SOLO FOGLIO
028600941202     C* 02    - SELEZIONATA UNA SOLA LNP
028700941202     C* 03/06 - INDICANO LE SELEZIONI FATTE NELLA PRIMA VIDEATA
028800900619     C* 07    - PREMUTO AGGIUNTA RIGHE
028900901119     C* 08    - ROLLUP
029000031204     C* 09    - visualizzo campo I/E per int,anomalie italia o estere
029100910930     C* 10/16 - TASTI DI DUPLICA
029200910930     C* 17    - RIESEGUO LETTURA PER CAT FV=9 SE FINITO LEGGERE UNA CAT
029300101201     C* 18    - IMMESSA CATEGORIA specifica
029400080317     C* 19    - Visualizzazione solo per EDP
029500941212     C* 20 ON - INTERROGAZIONE SPUNTE
029600970131     C*    OFF- GESTIONE SPUNTE
029700950711     C* 21 OFF- PROTEGGO IN MANUTENZIONE SE SPUNTA CARICATA DA BRV
029800040514     C* 22
029900970131     C* 23    - SPROTEGGO LA CATEGORIA, MODIFICABILE SE FOGLIO CONSEGNA
030000970131     C*         O FOGLIO PARTENZA IN CATEGORIA 3 E VICEVERSA
030100950704     C* 24    - PROTEGGO IN MANUTENZIONE SE FV CHIUSO
030200960404     C* 25    - OFF--> CAMPO ANNULLAMENTO SPROTETTO
030300920803     C* 26    - INTERROGAZIONE SPUNTE RICHIAMATO
030400941212     C* 27    - PROTEGGO FILIALE IN GESTIONE
030500101126     C* 97    - PROTEGGO COD.ANOMALIA = "E"
030600911014     C* 31/34 - DI COMODO
030700900521     C* 35    - PULIZIA SUBFILE
030800921020     C* 36    - SFLNXTCHG
030900930707     C* 37/38 - LNP E LNA LOCALI
031000930707     C* 39    - CHAIN SU AZORG
031100910320     C* 40/49 - ERRORE
031200960503     C* 51/68 - ERRORE
031300970131     C* 62    - ERRORE SUL FLAG ANNULLAMENTO
031400970131     C* 63    - PROTEGGO CAMPI PER SPUNTA PARTENZA GIA' TRASMESSA
031500970131     C*         O PER SPUNTA DI DEFLUENZA ESTERO
031600960522     C* 65    - PROTEZ.CAMPO X INTERR.ANOMALIE SE RICHIAMATO DA FNLR44R
031700970131     C* 66    - PER FOGLIO DEFL ESTERO SPROTEGGO IL FLAG ANNULLAMENTO
031800970131     C*         CHE SARA' ABILITATO SOTANTO SE SPUNTA MANUALE
031900980213     C* 69    - ERRORE SPUNTA DI DEFLUENZA PER SEGNAC.LOCALE ESTERO SDI
032000090416     C* 70    - visulizza se presente spunta senza anomlia
032100090416     C* 71    - ND DATA ORA CARICAMENTO
032200000118     C* 72    - ND DATA ORA TRAMSISSIONE
032300150817      * 75/76 - Sproteggo volume e peso. Sempre protetti non si possono inserire
032400990428     C* 86    - METTO IN RI LE PISTOLE 88 SU DEFLUENZE EXPORT
032500990428     C* 87/89 - ERRORE IN SFL
032600900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
032700920810     C* 91    - PER RIEMETTERE ANCHE IL VIDEO DEI PIEDI
032800930115     C* 92    - SIMFEL HA CML: VISUALIZZO VOLUME DA CML
032900920811     C* 94    - SPROTEGGO SEGNACOLLI INSERITI MANUALMENTE POSSO VARIARL
033000920811     C*         IN TUTTO
033100921020     C* 95    - ERRORE IN REGISTRAZIONE DATI
033200950711     C* 96    - ERRORE:INSERIMENTO DI SPUNTA PARTENZA DI BOLLA CONSEGN.
033300101126     C* 98    -
033400900521     C*****************************************************************
033500000000     C     *ENTRY        PLIST
033600000000     C                   PARM                    KPJBA
033700941201     C                   MOVEL     KPJBU         PARAM1
033800000119     C*
033900900615     C                   CALL      'X§PARUT'
034000020808     C                   PARM                    UT§DSE0F
034100070116     C                   MOVEL     RAGUT         v1cdsa
034200900615     C                   MOVEL     REC80         CNCR80
034300021016     C                   Z-ADD     SIMFEL        COMTFP            3 0
034400020808     C                   z-add     SIMpou        WPOspu
034500101129     C                   MOVEL     SIMFEL        PARFEL
034600900511     C*---------------------------------------------------------------*
034700900615     C*
034800101202     c                   setoff                                       266520
034900920811     C* 20 ON - INTERROGAZIONE
035000101129     c                   if        pa1flg='1' or pa1flg='3'
035100910419     C                   SETON                                        20
035200910419     C                   END
035300910419     C*
035400101129     C* PGM NON RICHIAMAOT (noem programma vuoto)
035500941201     C     PA1PGM        IFEQ      *BLANKS
035600101129     c* Per sicurezza se pa1flg 3 --> imposto 1
035700101129     c                   if        pa1flg='3'
035800101129     c                   eval      pa1flg='1'
035900101129     c                   endif
036000101129     c
036100941220     C                   Z-ADD     *ZERO         V1CFGS
036200941201     C                   MOVEL     *BLANKS       V1CNFV
036300941201     C                   Z-ADD     0             V1CLNP
036400941201     C                   Z-ADD     0             V1CLNA
036500941201     C                   Z-ADD     99            V1CNRS
036600941201     C                   Z-ADD     0             V1CNCD
036700941201     C                   Z-ADD     0             V1CNCA
036800941201     C                   Z-ADD     99            V1CNPS
036900941201     C                   MOVEL     ' '           V1CNPG
037000101126     c                   clear                   v1cbolp
037100101126     c                   clear                   v1cbors
037200101129     c                   clear                   v1cbosp
037300101126     c                   movel     wdateu        v1cboan
037400101126     c
037500920810     C                   ELSE
037600941201     C* 26 ON = RICHIAMATO DA ALTRO PGM
037700920810     C                   SETON                                        26
037800960903     C* Se pgm richiamato accendo 65 per evitare richiami ricorsivi a
037900960903     C* fnlr44r
038000101129     c                   if        pa1pgm ='FNLR44R'
038100101129     C                   SETON                                        65
038200101129     c                   endif
038300101129     c
038400960522     C                   END
038500101129     C* CARICO TABELLE
038600101129     C                   EXSR      CARTAB
038700941201     C*
038800941201     C* EMETTO IL PRIMO FORMATO CON LE PARZIALIZZAZIONI
038900020808     C                   MOVEL     'S'           WRIEM                          RIEMETTO TESTATA
039000101129     C*
039100101129     C*  se non devo emettere la prima videata --> passo subito ai controlli
039200101129     C                   If        PA1flg = '2' or pa1flg = '3'
039300101129     c                   exsr      contr1
039400101129     c* se errore esco dal pgm
039500101129     c                   if        *in90
039600101129     c                   eval      pa1err='E'
039700101129     c                   goto      fine
039800101129     C                   ENDIF
039900101129     c
040000101129     c                   else
040100101129     c
040200930205     C     FOR01         TAG
040300941201     C     WRIEM         IFEQ      'S'
040400941201     C                   WRITE     LR04T01                                      INTESTAZIONE VIDEO
040500941201     C                   MOVEL     *BLANKS       WRIEM                          NO RIEMETTO TESTATA
040600941201     C                   ENDIF
040700101122     C*
040800941130     C                   EXFMT     LR04D01
040900101129     c                   setoff                                       2890
041000101129     c                   setoff                                       555960
041100941201     C*
041200101129     C** F3 - FINE LAVORO   o F12 se richiamato
041300920810     C   KC
041400920810     COR KL
041500920810     CAN 26              GOTO      FINE
041600101129     C** F18- CAMBIO FILIALE
041700941212     C   KS              SETON                                        27
041800941212     C   KS              GOTO      FOR01
041900941201     C*
042000941201     C                   EXSR      CONTR1                                       CONTROLLI D01
042100900528     C   90              GOTO      FOR01
042200101129     c                   endif
042300101129     c
042400900518     C*
042500900611     C* CARICO FILIALI DI ARRIVO IN SUBFILE
042600900611     C                   EXSR      CARPPG
042700920810     C*
042800910422     C* SE SONO IN INTERROGAZIONE E NON TROVATE SPUNTE ESCO
042900920810     C* SE TIPO NPG = 9 NON VISUALIZZO
043000940727     C     NRR2          IFEQ      0
043100920810     C     *IN20         IFEQ      '1'
043200941201     C     V1CNPG        OREQ      '9'
043300911018     C                   SETON                                        54
043400911018     C                   GOTO      FOR01
043500911018     C                   END
043600911018     C                   END
043601160728     c*   Imposto l'ordinamento sfl di default: per numero foglio
043602160728     c                   eval      w_ord=w_nfv
043603160728     c                   eval      v2zf11=ord_dta
043700900611     C*
043800910320     C     FORCT2        TAG
043900941201     C                   WRITE     LR04T01                                      INTESTAZIONE VIDEO
044000941201     C                   WRITE     LR04Z02                                      TASTI FUNZIONE
044100900518     C*
044200900518     C     FORCT         TAG
044300941201     C                   EXFMT     LR04C02                                      SFLCTL
044400101129     c                   setoff                                       2890
044500090416     C** F3 - FINE LAVORO
044600900518     C   KC              GOTO      FINE
044700090416     C** F12 - RITORNO
044800101201     c                   if        *inkl
044900101201     c                   if        pa1flg='2' or pa1flg='3'
045000101201     C                   GOTO      Fine
045100101201     c                   else
045200101201     C                   GOTO      FOR01
045300101201     c                   endif
045400101201     c                   endif
045401160728     c* F11=Ordinamento
045402160728     c                   if        *inkk=*on
045403160728     c                   select
045404160728     c                   when      w_ord=w_nfv
045405160728     c                   eval      w_ord=w_dta
045406160728     c                   eval      v2zf11=ord_nfv
045407160728     c                   exsr      sr_ordina
045408160728     c                   when      w_ord=w_dta
045409160728     c                   eval      w_ord=w_nfv
045410160728     c                   eval      v2zf11=ord_dta
045411160728     c                   exsr      carppg
045416160728     c                   endsl
045418160728     c                   goto      forct2
045419160728     c                   endif
045500090420     c
045600090420     c                   eval      rec2=pagnrr
045700090420     c                   eval      savpagnrr=pagnrr
045800900608     C*
045900090416     C* F13 - AGGIUNTA RIGHE
046000930707     C   KM              SETON                                        07
046100930707     C   KM
046200900618     C* SCORRIMENTO
046300901119     COR 08              DO
046400900618     C                   EXSR      CARLNA
046500900618     C                   GOTO      FORCT
046600900618     C                   END
046700090420     C
046800920323     C* ENTER SE IN INTERROGAZIONE RIEMETTO SUBFILE
046900960523     C* PRIMA PERO VERIFICO SE RICHIESTA INTERROGAZIONE ANOMALIE
047000031119     c     *in20         ifeq      *on
047100090420     c
047200090420     C                   EXSR      INTANM
047300031119     C   90              GOTO      FORCT
047400031119     c                   endif
047500090420     C
047600090420     c* F5 - interrogazione spunta senza anmomalia
047700090420     c                   if        *inke
047800090420     c                   seton                                        31
047900090420    4C     V6CPOS        IFGT      0
048000090420     C     V6CPOS        CHAIN     lr04s02                            31
048100090420     c                   if        not *in31 and v2ccan=' '
048200090420     c                   seton                                        31
048300090420     c                   endif
048400090420     c                   if        not *in31 and v2hi70='0'
048500090420     c                   seton                                        31
048600090420     c                   endif
048700090420     c
048800090420     c                   if        not *in31
048900090420     c                   EXSR      Wind06
049000090420     c                   eval      rec2=v6cpos
049100090420     C                   GOTO      FORCT
049200090420    4c                   endif
049300090420    4c                   endif
049400090420     c
049500090420    4c                   if        v6cpos=0 or *in31
049600090420     c
049700090420     c                   if        v6cpos=0
049800090420     c                   if        pagnrr>0
049900090420     c                   eval      rec2=pagnrr
050000090420     c                   endif
050100090420     C     rec2          CHAIN     lr04s02                            31
050200090420     c                   else
050300090420     c                   eval      rec2=v6cpos
050400090420     c                   endif
050500090420     c
050600101126     c                   seton                                        975731
050700090420     c                   seton                                        36
050800090420     C                   UPDATE    LR04S02
050900090420     c                   setoff                                       36
051000090420     C                   GOTO      FORCT
051100090420    4c                   endif
051200090420    3c                   endif
051300090420    c
051400090420     c* In interrogazione mi fermo qui   con la routine (il resto è per manutenzione)
051500090420     c                   if        *in20
051600090420     C                   GOTO      FORCT2
051700090420     c                   endif
051800920323     C*
051900920323     C* ENTER   CONTROLLO
052000920323     C                   EXSR      CONTR2
052100920323     C   91              GOTO      FORCT2
052200920323     C   90              GOTO      FORCT
052300960523     C* VERIFICO SE RICHIESTA INTERROGAZIONE ANOMALIE
052400960523     C                   EXSR      INTANM
052500031119     C   90              GOTO      FORCT
052600090420     c
052700920323     C*
052800920323     C* CMD15- AGGIUNTA RIGHE DAL AL
052900920323     C   KP              DO
053000920323     C*
053100920323     C                   EXSR      PULDAL
053200920323     C*
053300920323     C     FOR03         TAG
053400941201     C     WRIEM         IFEQ      'S'
053500941201     C                   WRITE     LR04T01                                      INTESTAZIONE VIDEO
053600941201     C                   MOVEL     *BLANKS       WRIEM                          NO RIEMETTO TESTATA
053700941201     C                   ENDIF
053800941130     C                   EXFMT     LR04D03
053900941201     C*
054000920323     C* CMD12 - RITORNO
054100920323     C   KL              GOTO      FORCT2
054200920323     C* CONTROLLO
054300920323     C                   EXSR      CTRDAL
054400920323     C  NKF
054500920323     COR 90              GOTO      FOR03
054600920323     C* CMD6 - CREO SPUNTE
054700920323     C                   EXSR      AGGDAL
054800920323     C                   GOTO      FORCT2
054900920323     C                   END
055000920323     C*
055100960523     C  NKF
055200090420     CANNKN              do
055300090420     c                   eval      rec2=savpagnrr
055400090420     C                   GOTO      FORCT2
055500090420     c                   enddo
055600910323     C*
055700910323     C* CMD6 - REGISTRA
055800920320     C   KN
055900920320     COR KF              DO
056000910323     C                   EXSR      REGIS
056100950711     C   96
056200960523     COR 95              GOTO      FORCT2
056300921020     C*
056400101201     c                   if        pa1flg='2' or pa1flg='3'
056500101201     C                   GOTO      Fine
056600101201     c                   else
056700930205     C                   GOTO      FOR01
056800101201     c                   endif
056900101201     c
057000910323     C                   END
057100900611     C*
057200000000     C     FINE          TAG
057300911014     C*
057400070220     C* CHIUSURA PGM
057500930521     C     CFS44         IFEQ      'C'
057600070220     c                   clear                   fnls45ds
057700070220     c                   movel     'C'           dls45FLG
057800070220     C                   CALL      'FNLS45R'
057900070220     c                   parm                    fnls45ds
058000930521     C                   END
058100950704     C*
058200950704     C                   CLEAR                   DSUL06
058300950704     C                   MOVEL     'C'           D06TLA
058400950704     C                   MOVEL     DSUL06        KPJBU
058500950704     C                   CALL      'TRUL06R'
058600950704     C                   PARM                    KPJBA
058700911017     C*
058800940727     C* CHIUSURA FILE FNLSA8R
058900930609     C     CFSA8         IFEQ      'C'
059000930609     C                   MOVEL     'C'           PARFLG
059100930609     C                   MOVEL     PARAM2        KPJBU
059200940727     C                   CALL      'FNLSA8R'
059300930609     C                   PARM                    KPJBA
059400930609     C                   END
059500971003     C* CHIUSURA PGM
059600971003     C                   CLEAR                   DSLV53
059700971003     C                   MOVEL     'C'           D53TLA
059800971003     C                   CALL      'FNLV53R'
059900971003     C                   PARM                    DSLV53
060000971003     C                   CLEAR                   DSLV55
060100971003     C                   MOVEL     'C'           D55TLA
060200971003     C                   CALL      'FNLV55R'
060300971003     C                   PARM                    DSLV55
060400930609     C*
060500950125     C* RISPEDISCO INDIETRO LA KPJBU COME ERA ALLA CHIAMATA
060600950125     C                   CLEAR                   KPJBU
060700950125     C                   MOVEL     PARAM1        KPJBU
060800000000     C                   SETON                                        LR
060900940727     C**************************************************************************
061000940727     C*    CARICO TABELLE
061100940727     C**************************************************************************
061200911017     C     CARTAB        BEGSR
061300930205     C***
061400911017     C* CARICO TABELLA FILIALI GESTITE £1
061500930205     C***
061600941201     C                   CLEAR                   DSUL06
061700941201     C                   MOVE      '£1'          D06COD
061800941201     C                   MOVEL     SIMFEL        D06KEY
061900941201     C                   MOVEL     DSUL06        KPJBU
062000941201     C                   CALL      'TRUL06R'
062100941201     C                   PARM                    KPJBA
062200941201     C                   MOVEL     KPJBU         DSUL06
062300941201     C                   MOVEA     LIN           L1
062400950704     C***
062500950704     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE (DA £6)
062600950704     C***
062700950704     C                   CLEAR                   DSUL06
062800950704     C                   MOVE      '£6'          D06COD
062900020509     C                   MOVEL     'G'           D06ESC
063000020509     C                   MOVEL     SIMFEL        D06KEY
063100950704     C                   MOVEL     DSUL06        KPJBU
063200950704     C                   CALL      'TRUL06R'
063300950704     C                   PARM                    KPJBA
063400950704     C                   MOVEL     KPJBU         DSUL06
063500950704     C                   MOVEA     LIN           L6S
063600950704     C***
063700920807     C* CARICO CATEGORIE
063800950704     C***
063900940727     C                   MOVEL     '7N'          KCOD
064000020808     C                   Z-ADD     0             XI
064100940727     C     KTAB1         SETLL     TABEL
064200940727     C     KTAB1         READE     TABEL                                  31
064300920807     C*
064400920807     C     *IN31         DOWEQ     '0'
064500920807     C     TBLFLG        IFEQ      ' '
064600920807     C                   MOVEL     TBLUNI        DS7N
064700920807     C*
064800031121     c     §7nuti        ifne      '9'
064900020808     C                   ADD       1             XI
065000020808     C                   MOVEL     TBLKEY        CAT(XI)
065100020808     C                   MOVEL     §7NDES        DEC(XI)
065200020808     C                   MOVEL     §7NNST        NST(XI)                        NR.SPUNTE GG.
065300101201     C                   MOVEL     §7Nord        cator(XI)
065400101201     C                   MOVE      cat(XI)       cator(XI)
065500031121     c                   endif
065600920807     C                   END
065700920807     C*
065800940727     C     KTAB1         READE     TABEL                                  31
065900920807     C                   END
066000920807     C*
066100950704     C     CAT(1)        IFNE      ' '
066200950704     C                   MOVEL     ' ='          V1CCA1
066300950704     C                   MOVEL     CAT(1)        V1CCA1
066400950704     C                   MOVE      DEC(1)        V1CCA1
066500920810     C                   END
066600920810     C*
066700950704     C     CAT(2)        IFNE      ' '
066800950704     C                   MOVEL     ' ='          V1CCA2
066900950704     C                   MOVEL     CAT(2)        V1CCA2
067000950704     C                   MOVE      DEC(2)        V1CCA2
067100920810     C                   END
067200920810     C*
067300950704     C     CAT(3)        IFNE      ' '
067400950704     C                   MOVEL     ' ='          V1CCA3
067500950704     C                   MOVEL     CAT(3)        V1CCA3
067600950704     C                   MOVE      DEC(3)        V1CCA3
067700920810     C                   END
067800920810     C*
067900950704     C     CAT(4)        IFNE      ' '
068000950704     C                   MOVEL     ' ='          V1CCA4
068100950704     C                   MOVEL     CAT(4)        V1CCA4
068200950704     C                   MOVE      DEC(4)        V1CCA4
068300950704     C                   ENDIF
068400920810     C*
068500950704     C     CAT(5)        IFNE      ' '
068600950704     C                   MOVEL     ' ='          V1CCA5
068700950704     C                   MOVEL     CAT(5)        V1CCA5
068800950704     C                   MOVE      DEC(5)        V1CCA5
068900920810     C                   END
069000920810     C*
069100950704     C     CAT(6)        IFNE      ' '
069200950704     C                   MOVEL     ' ='          V1CCA6
069300950704     C                   MOVEL     CAT(6)        V1CCA6
069400950704     C                   MOVE      DEC(6)        V1CCA6
069500920810     C                   END
069600941201     C*
069700950704     C     CAT(7)        IFNE      ' '
069800950704     C                   MOVEL     ' ='          V1CCA7
069900950704     C                   MOVEL     CAT(7)        V1CCA7
070000950704     C                   MOVE      DEC(7)        V1CCA7
070100941201     C                   END
070200941201     C*
070300950704     C     CAT(8)        IFNE      ' '
070400950704     C                   MOVEL     ' ='          V1CCA8
070500950704     C                   MOVEL     CAT(8)        V1CCA8
070600950704     C                   MOVE      DEC(8)        V1CCA8
070700941201     C                   END
070800101126     c* Preparo anche stringa unica : e piene <=7 decodifica di 8
070900101126     c*  altrimenti decodifica di 5
071000101201     c                   select
071100101201     c                   when      cat(6)=' '
071200101201     c                   eval      wlung=10
071300101201     c                   when      cat(7)=' '
071400101201     c                   eval      wlung=9
071500101201     c                   when      cat(8)=' '
071600101126     c                   eval      wlung=7
071700101201     c                   other
071800101126     c                   eval      wlung=5
071900101201     c                   endsl
072000101126     c
072100101126     c                   z-add     1             xi
072200101126     c                   dow       cat(xi)<>*blanks
072300101126     c                   if        xi = 1
072400101126     c                   eval      v1dcaf=cat(xi)+'='+%subst(dec(XI):1:wlung)
072500101126     c                   else
072600101126     c                   eval      v1dcaf=%trim(v1dcaf) + ' ' +
072700101126     c                                    cat(xi)+'='+%subst(dec(XI):1:wlung)
072800101126     c                   endif
072900101126     c                   add       1             xi
073000101126     c                   enddo
073100950704     C**
073200920810     C* CARICO TABELLA TIPI INVENTARI
073300950704     C**
073400940727     C                   MOVEL     '7B'          KCOD
073500020808     C                   Z-ADD     0             XI
073600940727     C     KTAB1         SETLL     TABEL
073700940727     C     KTAB1         READE     TABEL                                  31
073800920810     C*
073900920810     C     *IN31         DOWEQ     '0'
074000920810     C     TBLFLG        IFEQ      ' '
074100920810     C*
074200020808     C                   ADD       1             XI
074300020808     C                   MOVEL     TBLKEY        SPG(XI)
074400020808     C                   MOVEL     TBLUNI        DES(XI)
074500920810     C                   END
074600920810     C*
074700940727     C     KTAB1         READE     TABEL                                  31
074800920810     C                   END
074900950704     C**
075000920807     C* VEDO SE SONO SIMFEL O REMOTO
075100950704     C**
075200941230     C                   MOVEL     *BLANKS       V1DFGS
075300950705    1C  N20V1CFGS        IFEQ      *ZERO
075400941230     C     PA1PGM        ANDEQ     *BLANKS                                      SE NON RICHIAMATO
075500950124     C*
075600020808     c                   z-add     SIMpou        V1Cfgs
075700950704    1C                   ENDIF
075800960506     C*
075900960506     C* CARICO SCHIERA DELLE FILIALI GESTIBILI
076000960506     C*
076100960506     C                   CLEAR                   FG
076200020808     C                   IF           SIMtpp = '2'
076300020808     C                             or SIMtpp = *blanks
076400020808     C                   move      SIMpou        FG(1)
076500020808     C                   ELSE
076600960506     C                   MOVEA     L1            FG
076700960506     C                   END
076800941230     C*
076900960503     C                   CLEAR                   WFGS
077000950704    1C     V1CFGS        IFNE      *ZERO
077100941220     C                   MOVEL     V1CFGS        NUM3
077200930707     C                   EXSR      CAZORG
077300941230     C  N39              MOVEL     ORGDES        V1DFGS
077400960503     C  N39              MOVEL     V1CFGS        WFGS
077500930205     C*
077600930205     C                   EXSR      CARL6
077700971003     C                   MOVEA     L6            L6F
077800950704    1C                   ENDIF
077900101129     c                   ENDSR
078000940727     C**************************************************************************
078100940727     C*    CONTROLLI FORMATO1
078200940727     C**************************************************************************
078300900528     C     CONTR1        BEGSR
078400900528     C                   SETOFF                                       010203
078500900528     C                   SETOFF                                       900405
078600101129     C                   SETOFF                                       3018
078700921229     C                   SETOFF                                       5406
078800031204     c                   seton                                        09
078900971003     C                   CLEAR                   DSLV53
079000920810     C                   Z-ADD     0             VI1NFV
079100101202     c                   clear                   v2cbolp
079200101202     c                   clear                   v2cbors
079300101202     c                   clear                   v2cbosp
079400101202     c                   clear                   v2cmit
079500101201     c                   clear                   v2cdes
079600101201     c                   clear                   v2cbosp
079700101202     c                   clear                   v2cbods
079800101201     c                   clear                   boldsp
079900941202     C*----------------------------------------------------
080000101129     C*             FILIALE IN GESTIONE
080100941202     C*----------------------------------------------------
080200101129     C* In gestine NON PUO' ESSERE ZERO
080300101129     C* NON OBBLIGATORIA IN INTERROGAZIONE
080400101129     C     *IN20         IFEQ      *OFF                                         20OFF = GESTIONE
080500101129     C     V1CFGS        andeq     *ZERO
080600941212     C                   SETON                                        57  90
080700941212     C                   GOTO      ENDCT1
080800941212     C                   ENDIF
080900941202     C*
081000941202     C                   MOVEL     *BLANKS       V1DFGS
081100941212     C     V1CFGS        IFNE      *ZERO
081200941201     C                   MOVEL     V1CFGS        NUM3
081300941201     C                   EXSR      CAZORG
081400941205     C   39              SETON                                        57  90
081500941202     C   39              GOTO      ENDCT1
081600101129     c
081700941201     C                   MOVEL     ORGDES        V1DFGS
081800101129     c
081900971126     C                   CLEAR                   DSLV55
082000971126     C                   MOVEL     'P'           D55TPT
082100971126     C                   MOVE      V1CFGS        D55LIN
082200971126     C                   Z-ADD     WDATEU        D55DRF
082300971126     C                   CALL      'FNLV55R'
082400971126     C                   PARM                    DSLV55
082500971126     C                   MOVE      D55TFP        FGSTFP
082600941201     C*
082700941201     C                   EXSR      CARL6
082800941212     C                   ENDIF
082900941202     C*----------------------------------------------------
083000960503     C* CATEGORIA FOGLI VIAGGIO
083100941202     C*----------------------------------------------------
083200941201     C     V1CNPG        IFEQ      ' '
083300910320     C                   SETON                                        4990
083400910320     C                   GOTO      ENDCT1
083500910320     C                   END
083600920807     C* RICERCA IN TABELLA
083700941201     C     V1CNPG        IFEQ      '?'
083800920807     C                   MOVEL     CODUT         §KUT
083900920807     C                   MOVEL     '7N'          §COD
084000941201     C                   MOVE      ' '           V1CNPG
084100920807     C                   MOVEL     *BLANKS       §KEY
084200920807     C                   CALL      'X§TABER'
084300920807     C                   PARM                    §KUT              1 0
084400920807     C                   PARM                    §COD              2
084500920807     C                   PARM                    §KEY              8
084600920807     C                   PARM                    §DES             30
084700941201     C                   MOVEL     §KEY          V1CNPG
084800920807     C                   SETON                                        90
084900941205     C                   MOVEL     'S'           WRIEM             1            RIEMETTO TESTATA
085000920807     C                   GOTO      ENDCT1
085100920807     C                   END
085200920807     C*
085300941201     C     V1CNPG        IFNE      '9'
085400020808     C                   Z-ADD     1             XI
085500020808     C     V1CNPG        LOOKUP    CAT(XI)                                31
085600920807     C* ERRORE
085700920807     C  N31              SETON                                        4990
085800920807     C  N31              GOTO      ENDCT1
085900950704     C*
086000910930     C                   SETON                                        18
086100950704     C                   ENDIF
086200920810     C*
086300941201     C                   MOVEL     V1CNPG        SAVNPG
086400941202     C*----------------------------------------------------
086500950704     C* NUMERO FOGLIO
086600941202     C*----------------------------------------------------
086700101129     C*
086800101129     C     V1CNFV        IFNE      *BLANKS
086900101129     C     V1CNFV        ANDNE     *ZEROS
087000101129     c
087100101129     C* NON SI PUO' IMMETTERE SE CAT = 9
087200101129     C     SAVNPG        IFEQ      9
087300101129     C                   SETON                                        559028
087400101129     c                   eval      v1cmsg=msg(1)
087500910930     C                   GOTO      ENDCT1
087600910930     C                   END
087700101129     C* ERRORE SE NON IMMESSA FILIALE GESTIONE
087800101129     C     V1CFGS        IFEQ      0
087900101129     C                   SETON                                        599028
088000101129     c                   eval      v1cmsg=msg(2)
088100101129     C                   GOTO      ENDCT1
088200101129     C                   ENDIF
088300910423     C*
088400920810     C* CONTROLLO SE INTERROGAZIONE FOGLI
088500941201     C     '?'           SCAN      V1CNFV                                 90
088600920810     C*
088700930707     C     *IN90         IFEQ      *ON
088800950704     C*
088900910930     C*
089000941212     C     V1CNPG        IFEQ      '1'
089100941212     C*
089200941212     C* FOGLI PARTENZE
089300950704     C     V1CFGS        IFEQ      SIMFEL
089400130520     C                   RESET                   fnlst3ds
089500951108     C                   Z-ADD     WMENO1        PA5DIN                         MENO 5 GIORNI G/M/A
089600951108     C                   Z-ADD     WDATE         PA5DFI                         FINALE GG/MM/AAAA
089700000614     C                   Z-ADD     V1CFGS        PA5FGS                         P.O. GESTIONE
089800130520     C                   MOVEL     fnlst3ds      KPJBU
089900130520     C                   CALL      'FNLST3R'
090000941212     C                   PARM                    KPJBA
090100130520     C                   MOVEL     KPJBU         fnlst3ds
090200951108     C                   MOVEL     PA5NFV        V1CNFV
090300950704     C                   ELSE
090400950704     C                   MOVEL     'FNLR04R '    PA6PGM
090500950704     C                   Z-ADD     *ZERO         PA6LAI
090600950704     C                   MOVEL     SIMFEL        PA6FEL
090700950704     C                   MOVEL     *BLANKS       PA6ABB
090800950704     C                   Z-ADD     WMENO2        PA6DSD
090900950704     C                   Z-ADD     99991231      PA6DSA
091000950704     C                   MOVEL     'I'           PA6FLG                         'I'=INTERROGAZIONE
091100000609     C                   MOVEL     FGSTFP        PA6FGS
091200950704     C                   CLEAR                   KPJBU
091300950704     C                   MOVEL     PARAM6        KPJBU
091400950704     C                   MOVEA     *ZEROS        FVA
091500950704     C*
091600950704     C                   CALL      'FNLR26R'
091700950704     C                   PARM                    KPJBA
091800950704     C                   PARM                    FVA
091900950704     C                   MOVEL     KPJBU         PARAM6
092000950704     C                   MOVE      FVA(1)        V1CNFV
092100950704     C                   ENDIF
092200941212     C*
092300941212     C                   ELSE
092400941212     C*
092500941212     C* FOGLI ARRIVI
092600941212     C                   CLEAR                   DSLR01
092700941212     C                   MOVEL     'S'           DLRRIC                         PGM.RICHIAMATO
092800941212     C                   MOVEL     V1CNPG        DLRNPG                         CATEGORIA
092900941212     C                   Z-ADD     V1CFGS        DLRFGS                         FIL.GESTIONE
093000950616     C                   Z-ADD     WMENO1        DLRGDA                         MENO 5 GIORNI G/M/A
093100950616     C                   Z-ADD     WMENO2        DLRADA                         MENO 5 GIORNI A/M/G
093200941212     C                   MOVEL     'FNLR04R'     DLRPGM                         PROGRAMMA
093300941212     C                   MOVEL     DSLR01        KPJBU
093400941212     C                   CALL      'FNLR02R'
093500910423     C                   PARM                    KPJBA
093600941212     C                   MOVEL     KPJBU         DSLR01
093700941212     C                   MOVEL     DLRNFV        V1CNFV
093800941212     C*
093900941212     C                   ENDIF
094000941212     C*
094100101129     c                   seton                                        55
094200941205     C                   MOVEL     'S'           WRIEM             1            RIEMETTO TESTATA
094300910423     C                   GOTO      ENDCT1
094400910423     C                   END
094500941202     C*----------------------------------------------------
094600910423     C* CONTROLLO SE CAMPO NUMERICO
094700941201     C                   TESTN                   V1CNFV               30
094800101129     c                   if        not *in30 or %subst(v1cnfv:5:1)<'0'
094900101129     C                   SETON                                        559028
095000101129     c                   eval      v1cmsg=msg(3)
095100101129     C                   GOTO      ENDCT1
095200101129     C                   ENDIF
095300950124     C*
095400941201     C* SE RIEMPITO "NR.FOGLIO" VERIFICO L'ESISTENZA
095500101129    2C     V1CNFV        IFGT      *ZEROS
095600941202     C*
095700000120     C                   CLEAR                   VI1NFV
095800000120     C                   MOVE      V1CNFV        VI1NFV
095900971003     C**
096000971003     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
096100971003     C                   MOVEL     SIMFEL        D53FEL
096200031204     C                   MOVEL     V1CNFV        D53NFV
096300971003     C                   MOVEL     V1CNPG        D53NPG
096400031204     C                   MOVEL     V1CFGS        D53FGS
096500971003     C                   CALL      'FNLV53R'
096600971003     C                   PARM                    DSLV53
096700971003     C* ERRORE
096800971003    3C     D53ERR        IFNE      ' '
096900101129     C                   SETON                                        559028
097000101129     c                   eval      v1cmsg=msg(3)
097100101129     C                   GOTO      ENDCT1
097200971003    3C                   ENDIF
097300101129     c
097400981030     C* LA MANUTENZIONE DEI FV PARTENZA CONCESSA SOLO A SIMFEL
097500981030     C* SE E' DEFLUENZA POSSO
097600101129    3C  n20V1CNPG        IFEQ      '1'
097700000609     C     D53DEF        ANDNE     'S'
097800000609    4C     V1CFGS        IFNE      SIMFEL
097900000609     C*
098000101129     C                   SETON                                        559028
098100101129     c                   eval      v1cmsg=msg(4)
098200981030     C                   GOTO      ENDCT1
098300000609    4C                   ENDIF
098400000609    3C                   ENDIF
098500941202     C*
098600971003   X2C                   ELSE
098700971003     C* IL FOGLIO NON E' UN NUMERO
098800101129     C                   SETON                                        559028
098900101129     c                   eval      v1cmsg=msg(3)
099000101129     C                   GOTO      ENDCT1
099100971003    2C                   END
099200900626     C*
099300910320     C*
099400900626     C                   SETON                                        01
099500971003    1C                   END
099600980212     C* VERIFICO IN GESTIONE SE POSSO USALE TALE FOGLIO ANCHE SE
099700971003     C*  NON MIO
099800971003    1C     *IN20         IFEQ      *OFF
099900971003     C**
100000971003     C                   CLEAR                   WNOFG
100100971003     C     V1CFGS        LOOKUP    FG                                     32
100200971003    2C     *IN32         IFEQ      *OFF
100300971003     C                   MOVE      'N'           WNOFG             1
100400980216     C**NON PRESENTE IN FG: CONTROLLO CHE ABBIA IL MIO STESSO TERMINAL
100500980216     C**DI ARRIVO
100600010628     C**(N.B.:FG CONTIENE £1 SE I°LIVELLO, LA FILIALE SE II°LIVELLO)
100700971003     C* Lna obbligatoria
100800971003    4C     V1CLNA        IFEQ      *ZEROS
100900971003     C                   SETON                                        67  90
101000971003     C                   GOTO      ENDCT1
101100971003    4C                   END
101200971003     C* Deve essere la mia lna oppure lna in gestione
101300971003    4C     V1CLNA        IFNE      WFGS
101400971003     C     V1CLNA        LOOKUP    L6S                                    32
101500971003     C* Errore: non è mia lna e non è in gestione
101600971003     C  N32              SETON                                        68  90
101700971003     C  N32              GOTO      ENDCT1
101800971003     C     V1CLNA        LOOKUP    L6F                                    32
101900971003     C  N32              SETON                                        68  90
102000971003     C  N32              GOTO      ENDCT1
102100971003    4C                   END
102200971003    2C                   END
102300971003    1C                   END
102400941202     C*----------------------------------------------------
102500900528     C* LINEA PARTENZA
102600911014     C* OBBLIGATORIA SE CATEGORIA FV=9
102700941201     C     V1CLNP        IFEQ      0
102800921229     C     SAVNPG        ANDEQ     9
102900101203     c     v1cbosp       andeq     0
103000101203     c     v1cbolp       andeq     0
103100910930     C                   SETON                                        5690
103200910930     C                   GOTO      ENDCT1
103300910930     C                   END
103400910930     C*
103500941201     C     V1CLNP        IFGT      0
103600941201     C                   MOVEL     V1CLNP        NUM3
103700930707     C                   EXSR      CAZORG
103800910320     C* ERRORE
103900930707     C   39              SETON                                        4090
104000930707     C   39              GOTO      ENDCT1
104100910320     C*
104200900528     C                   SETON                                        02
104300960522     C     ORGDE5        IFNE      *BLANKS
104400960522     C                   MOVEL     ORGDE5        DESLNP
104500960522     C                   ELSE
104600020207     C                   MOVEL     ORGDES        DESLNP           20
104700960522     C                   END
104800900528     C                   END
104900900528     C* LINEA ARRIVO
105000941201     C     V1CLNA        IFGT      0
105100941201     C                   MOVEL     V1CLNA        NUM3
105200930707     C                   EXSR      CAZORG
105300910320     C*
105400930707     C   39              SETON                                        4190
105500930707     C   39              GOTO      ENDCT1
105600021126     C                   MOVE      §OGNTW        WLANTW
105700910320     C*
105800900619     C                   SETON                                        03
105900960522     C     ORGDE5        IFNE      *BLANKS
106000960522     C                   MOVEL     ORGDE5        DESLNA
106100960522     C                   ELSE
106200930707     C                   MOVEL     ORGDES        DESLNA           20
106300960522     C                   END
106400020207     C**
106500020207     C* MEMORIZZO LNA ESTERE CHE FANNO IL MANIFEST
106600020207     C                   CLEAR                   LNAMNF
106700020207     C     §OGNTW        IFEQ      'EEX'
106800020207     C     §OGNTW        OREQ      'FED'
106900060322     C     §OGNTW        OREQ      'DPD'
107000020207     C                   MOVEL     'S'           LNAMNF            1
107100900528     C                   END
107200031204     C     §OGNTW        IFne      'EEX'
107300031204     C     §OGNTW        andne     'FED'
107400031204     C     §OGNTW        andne     'DPD'
107500031204     c                   setoff                                       09
107600031204     C                   END
107700020207     C                   END
107800900608     C* SEGNACOLLO DAL
107900941201     C     V1CNCD        COMP      0                                  0404
108000900528     C* NUMERO COLLO AL > NUMERO COLLO DAL
108100941201     C     V1CNCA        IFGT      0
108200941201     C     V1CNCA        IFLT      V1CNCD
108300900528     C   30              SETON                                        4290
108400900528     C   30              GOTO      ENDCT1
108500900528     C                   END
108600900528     C                   SETON                                        05
108700900528     C                   END
108800921229     C* SERIE >< DA 99
108900941201     C     V1CNRS        IFNE      99
109000921229     C                   SETON                                        06
109100921229     C                   END
109200101129     c*
109300101129     C* Se immessa controllo numero spedizione
109400101129    1c                   if        v1cbosp>0 or  v1cbolp>0
109500101201     c
109600101129     c     kblp2         chain     fnblp01l
109700101129    2c                   if        not %found(fnblp01l)
109800101129     c     kblp2         chain     fnarb01l
109900101129    3c                   if        not %found(fnarb01l)
110000101129     C                   SETON                                        609028
110100101129     c                   eval      v1cmsg=msg(5)
110200101129     C                   GOTO      ENDCT1
110300101201    3C                   ENDIF
110400101201    2C                   ENDIF
110500101216     c
110600101216     c* Se la spedizione non ha segnacolli segnalo suvito errore
110700101216     c     kblp2         setll     fnblt01l
110800101216    3c                   if        not %equal(fnblt01l)
110900101216     c     kblp2         setll     fnart01l
111000101216    4c                   if        not %equal(fnart01l)
111100101216     C                   SETON                                        5490
111200101216     C                   GOTO      ENDCT1
111300101216    3C                   ENDIF
111400101216    2C                   ENDIF
111500101129     c*  Se immessa la serie, deve essere uguale a quella del segnacollo
111600101201    2c                   if        v1cnrs<>99
111700101202     c                             and  v1cnrs<>v1cbors
111800101129     C                   SETON                                        609028
111900101129     c                   eval      v1cmsg=msg(6)
112000101129     C                   GOTO      ENDCT1
112100101129    2C                   ENDIF
112200101129     c*
112300101202    2c                   if        v1clna>0 and v1clna<>blplna
112400101129     C                   SETON                                        609028
112500101129     c                   eval      v1cmsg=msg(7)
112600101129     C                   GOTO      ENDCT1
112700101202    2C                   ENDIF
112800101202    2c                   if        v1clnp>0 and v1clnp<>blpfls
112900101129     C                   SETON                                        609028
113000101129     c                   eval      v1cmsg=msg(8)
113100101129     C                   GOTO      ENDCT1
113200101202    2C                   ENDIF
113300101216     c* Se immesso un solo segnacollo deve far parte della spedizione
113400101216    2c                   if        v1cncd>0 and (v1cnca=0 or v1cnca=v1cncd)
113500101216     c                   eval      brvnsc=v1cncd
113600101216     c     kbodet        setll     fnblt01l
113700101216    3c                   if        not %equal(fnblt01l)
113800101216     c     kbodet        setll     fnart01l
113900101216    4c                   if        not %equal(fnart01l)
114000101216     C                   SETON                                        609028
114100101216     c                   eval      v1cmsg=msg(9)
114200101216     C                   GOTO      ENDCT1
114300101216    4C                   ENDIF
114400101216    3C                   ENDIF
114500101216    2C                   ENDIF
114600101201    c
114700101201     c* Memorizzo i dati da visualizzare nella seconda videata
114800101201     c                   eval      v2cmit=blprsm
114900101201     c                   eval      v2cdes=blprsd
115000101202     c                   eval      boldsp=(blpaas*10000)+blpmgs
115100101202     c                   eval      v2cbosp=v1cbosp
115200101202     c                   eval      v2cbolp=v1cbolp
115300101202     c                   eval      v2cbors=v1cbors
115400101129    1c                   endif
115500101129     c
115600900614     C     ENDCT1        ENDSR
115700940727     C**************************************************************************
115800940727     C*    CARICO PRIMA PAGINA IN SUBFILE
115900940727     C**************************************************************************
116000900608     C     CARPPG        BEGSR
116100940727     C*
116200900615     C                   SETOFF                                       0730
116300900518     C                   EXSR      PULSF
116400900709     C* AZZERO CAMPI DI DUP
116500900709     C                   Z-ADD     0             DUPNFV            5 0
116600900709     C                   Z-ADD     0             DUPLNP            3 0
116700900709     C                   Z-ADD     0             DUPLNA            3 0
116800900709     C                   Z-ADD     0             DUPNRS            2 0
116900910131     C                   Z-ADD     0             DUPNSC            7 0
117000910930     C                   Z-ADD     0             DUPNPG            1 0
117100910320     C                   MOVEL     *BLANKS       DUPCAN            1
117101160729     c*   Imposto l'ordinamento sfl di default: per numero foglio
117102160729     c                   eval      w_ord=w_nfv
117103160729     c                   eval      v2zf11=ord_dta
117200101201     c* Imposto i campi chiave
117300101201     c                   eval      klnps=v1clnp
117400101201     c                   eval      klnas=v1clna
117500101201     c                   eval      knrss=v1cnrs
117600101201     c                   eval      knscs=v1cncd
117700101201     c                   eval      kfgss=v1cfgs
117800101201     c                   eval      savnca=v1cnca
117900101203     c                   eval      savncd=v1cncd
118000900709     C*
118100940727     C                   Z-ADD     1             REC2
118200101201     C* 18 off - CATEGORIA FV E' 9
118300930707    1C     *IN18         IFEQ      *OFF
118400930707     C                   Z-ADD     0             SAVNPG
118500950704     C                   Z-ADD     1             C                 3 0
118600101201     C                   Z-ADD     1             CC                3 0
118700101201     c                   sorta     cator
118800101201     c                   dow       cator(CC)=*blanks
118900101201     c                   add       1             cc
119000101201     c                   enddo
119100101201     c
119200101201    2C     CATor(CC)     IFNE      ' '
119300101201     C                   MOVE      CATor(CC)     SAVNPG
119400930707     C*
119500101201   X2C                   ELSE
119600000522     C                   MOVEL     0             SAVNPG
119700101201    2C                   ENDIF
119800930707     C*
119900930707    1C                   ENDIF
120000930707     C*
120100101201    1c                   if        v1cbosp>0
120200101201     c                   exsr      DatiSped
120300101201    1c                   endif
120400101201     c* se presente solo un segnacollo cerco la bolla
120500101201     c                   if        v1clnp>0 and v1clna>0 and v1cnrs<>99
120600101202     c                             and v1cncd>0 and v1cbosp=0
120700101201     c                   exsr      cercaBol
120800101201     c                   endif
120900101202     c
121000101202     c* Se trovata spedizione la visualizzo
121100101202     c                   if        boldsp>0
121200101202     c     *iso          movel     boldsp        dataiso
121300101202     c                   movel     dataiso       dataeur
121400101202     c                   movel     dataeur       v2cbods
121500101202     c                   endif
121600101201     c
121700900618     C* NON EFFETTUATA NESSUNA SELEZIONE O SOLO LNA
121800930707    1C     *IN01         IFEQ      *OFF
121900930707    2C     *IN02         IFEQ      *OFF
122000930707     C                   Z-ADD     1             VI1NFV
122100070116     C     KBRV1         SETLL     FnBRV01L
122200930707   X2C                   ELSE
122300930707     C* LNP
122400140625     C  N03KBRV4         SETLL     FnBRV05L
122500930707     C*
122600930707     C* LNP LNA
122700930707    3C     *IN03         IFEQ      *ON
122800140625     C  N06KBRV5         SETLL     FnBRV05L                                      SENZA SERIE
122900000118     C   06
123000140625     CANN04KBRV51        SETLL     FnBRV05L                                      +SERIE
123100000118     C   06
123200140625     CAN 04KBRV52        SETLL     FnBRV05L                                      +SER+SEGNAC
123300930707    3C                   ENDIF
123400930707    2C                   ENDIF
123500930707     C*
123600000609   X1C                   ELSE
123700900615     C*
123800930707    2C     *IN02         IFEQ      *OFF
123900900528     C* SELEZIONI
124000900528     C* NFV
124100070116     C  N03KBRV1         SETLL     FnBRV01L
124200930707     C*
124300930707     C* NFV LNA
124400941201     C   03              Z-ADD     V1CLNA        KLN
124500070116     C   03KBRV2         SETLL     FnBRV02L
124600930707     C*
124700930707   X2C                   ELSE
124800900615     C*
124900900528     C* NFV LNP
125000941201     C  N03              Z-ADD     V1CLNP        KLN
125100070116     C  N03KBRV2         SETLL     FnBRV01L
125200900615     C*
125300900615     C*
125400900528     C* NFV LNP LNA
125500930707    3C     *IN03         IFEQ      '1'
125600070116     C  N06KBRV3         SETLL     FnBRV01L                                      SENZA SERIE
125700000118     C   06
125800070116     CANN04KBRV31        SETLL     FnBRV01L                                      +SERIE
125900000118     C   06
126000070116     CAN 04KBRV32        SETLL     FnBRV01L                                      +SER+SEGNAC
126100930707    3C                   ENDIF
126200930707    2C                   ENDIF
126300930707    1C                   ENDIF
126400921229     C**
126500900528     C* LETTURA
126600940727     C                   Z-ADD     0             NRR2              4 0
126700900608     C                   Z-ADD     0             SAVNRR            4 0
126800900608     C                   EXSR      CARLNA
126900900608     C                   ENDSR
127000101201     C**************************************************************************
127100101201     C*    Cerco la bolla dal segnacollo
127200101201     C**************************************************************************
127300101201     c     Cercabol      BEGSR
127400101201     c     ksegnac       chain     fnblt27l
127500101201    1c                   if        %found(fnblt27l)
127600101201     c     kblp          chain     fnblp01l
127700101201   x1c                   else
127800101202     c     ksegnac       chain     fnart27l
127900101202    2c                   if        %found(fnart27l)
128000150623     c     kblp          chain     fnarb01l
128100101201   x3c                   else
128200101201     c                   leavesr
128300101201    2c                   endif
128400101201    1c                   endif
128500101201     c* Se c'e' segnacollo AL verifico se fa parte della stessa sped
128600101202    1c                   if        *in05  and blpfns=' '
128700101201     c                   eval      knscs=v1cnca
128800101201     c     kblpdet       setll     fnblt01l
128900101202    2c                   if        not %equal(fnblt01l)
129000101201     c     kblpdet       setll     fnart01l
129100101202    3c                   if        not %equal(fnart01l)
129200101201     c                   eval      knscs=v1cncd
129300101201     c                   leavesr
129400101202    3c                   endif
129500101202    2c                   endif
129600101202    1c                   endif
129700101201     c
129800101202     c                   eval      knscs=v1cncd
129900101201     c                   eval      v2cmit=blprsm
130000101201     c                   eval      v2cdes=blprsd
130100101202     c                   eval      boldsp=(blpaas*10000)+blpmgs
130200101202     c                   eval      v2cbosp=blpnsp
130300101202     c                   eval      v2cbolp=blplnp
130400101202     c                   eval      v2cbors=blpnrs
130500101201     c
130600101201     c                   ENDSR
130700101201     C**************************************************************************
130800101201     C*    CARICO dati spedizione
130900101201     C**************************************************************************
131000101201     c     DatiSped      BEGSR
131100101201     c* Se immesso il numero di spedizione, cerco subito quello
131200101201     c*  le altre selezioni sono parzilizzazioni
131300101201     c                   seton                                        020304
131400101201     c                   seton                                        06
131500101201     c
131600101201     c     kblp2         setll     fnblt01l
131700101201     c     kblp2         reade     fnblt01l
131800101201    2c                   if        not %eof(fnblt01l)
131900101201     c* primo segnacollo
132000101216     c                   if        v1cncd=0 and v1cnca=0
132100101201     c                   eval      knscs=bltnsc
132200101203     c                   eval      savncd=bltnsc
132300101215     c                   endif
132400101215     c
132500101201     c     kblp2         setgt     fnblt01l
132600101201     c     kblp2         readpe    fnblt01l
132700101201     c* ultimo segnacollo
132800101201    3c                   if        not %eof(fnblt01l)
132900101216     c                   if        v1cncd=0 and v1cnca=0
133000101201     c                   eval      savnca=bltnsc
133100101201    3c                   endif
133200101215    3c                   endif
133300101201     c
133400101201   x2c                   else
133500101201     c     kblp2         setll     fnart01l
133600101201     c     kblp2         reade     fnart01l
133700101201    3c                   if        not %eof(fnart01l)
133800101201     c* primo segnacollo
133900101216     c                   if        v1cncd=0 and v1cnca=0
134000101201     c                   eval      knscs=bltnsc
134100101203     c                   eval      savncd=bltnsc
134200101215     c                   endif
134300101215     c
134400101201     c     kblp2         setgt     fnart01l
134500101201     c     kblp2         readpe    fnart01l
134600101201     c* ultimo segnacollo
134700101201    4c                   if        not %eof(fnart01l)
134800101216     c                   if        v1cncd=0 and v1cnca=0
134900101201     c                   eval      savnca=bltnsc
135000101201    4c                   endif
135100101215    4c                   endif
135200101201     c
135300101201   x3c                   else
135400101201     c
135500101201     c                   clear                   bltfls
135600101201     c                   clear                   bltlna
135700101201     c                   clear                   bltnrs
135800101201     c                   clear                   bltnsc
135900101201     c                   setoff                                       020304
136000101201     c                   setoff                                       06
136100101201    3c                   endif
136200101201    2c                   endif
136300101201     c
136400101201     c                   eval      klnps=bltfls
136500101201     c                   eval      klnas=bltlna
136600101201     c                   eval      knrss=bltnrs
136700101201     c* Segnacollo DAL
136800101201    2c                   if        savnca>0 and savnca<>knscs
136900101201     c                   seton                                        05
137000101201    2c                   endif
137100101202     c
137200101202     c                   ENDSR
137300940727     C**************************************************************************
137400940727     C*    CARICO IN SUBFILE LINEE DI ARRIVO
137500940727     C**************************************************************************
137600900608     C     CARLNA        BEGSR
137700940727     C*
137800921209     C                   Z-ADD     0             SAVLNA            3 0
137900921209     C                   Z-ADD     0             SAVLNP            3 0
138000900608     C                   Z-ADD     0             B                 3 0
138100920811     C                   Z-ADD     0             SAVKEY
138101160729     c                   clear                   sflrcd
138200940727     C                   Z-ADD     SAVNRR        NRR2
138300990920     C  N30
138400990920    1CANN07B             DOWLT     15
138500950711     C                   SETOFF                                       901721
138600021206     C                   SETOFF                                       7172
138700021206     c                   Setoff                                       7576
138800930707     C* 01 OFF
138900930707    2C     *IN01         IFEQ      *OFF
139000930707     C* 02 OFF
139100930707    3C     *IN02         IFEQ      *OFF
139200070116     C     SAVNPG        READE(N)  FnBRV01L                               30
139300930707     C*
139400930707   X3C                   ELSE
139500930707     C* 02 ON
139600930707     C* CATEGORIA FV NON E' 9
139700930707    4C     *IN18         IFEQ      *ON
139800140625     C  N03KBRV4         READE(N)  FnBRV05L                               30
139900000118     C   03
140000140625     CANN06KBRV5         READE(N)  FnBRV05L                               30     SENZA SERIE
140100000118     C   03
140200140625     CAN 06KBRV51        READE(N)  FnBRV05L                               30     +SERIE
140300930707    4C                   ENDIF
140400930707    3C                   ENDIF
140500930707     C*
140600930707   X2C                   ELSE
140700930707     C* 01 ON
140800930707     C* 02 OFF
140900930707    3C     *IN02         IFEQ      *OFF
141000070116     C  N03KBRV1         READE(N)  FnBRV01L                               30
141100070116     C   03KBRV2         READE     FnBRV02L                               30
141200930707     C*
141300930707   X3C                   ELSE
141400930707     C* 02 ON
141500070116     C  N03KBRV2         READE(N)  FnBRV01L                               30
141600921229     C*
141700930707    4C     *IN03         IFEQ      '1'
141800070116     C  N06KBRV3         READE(N)  FnBRV01L                               30     SENZA SERIE
141900070116     C   06KBRV31        READE(N)  FnBRV01L                               30     +SERIE
142000930707    4C                   ENDIF
142100930707    3C                   ENDIF
142200930707    2C                   ENDIF
142300921229     C*
142400910930     C* CATEGORIA FV     E' 9
142500920323     C  N01
142600920323     CAN 02
142700920323     CANN18*IN17         DOWEQ     '0'
142800910930     C*
142900140625     C  N03KBRV4         READE(N)  FnBRV05L                               30
143000000118     C   03
143100140625     CANN06KBRV5         READE(N)  FnBRV05L                               30     SENZA SERIE
143200000118     C   03
143300140625     CAN 06KBRV51        READE(N)  FnBRV05L                               30     +SERIE
143400910930     C                   SETON                                        17
143500921229     C   30              EXSR      RICCAT
143600910930     C                   END
143700900528     C*
143800911014     C  N30              EXSR      CONFR
143900971003     C**
144000900608     C* 90 ON - RECORD DA NON SELEZIONARE
144100971003     C  N30
144200971003    1CANN90              DO
144300020905     C*
144400920811     C* D E C O D I F I C H E
144500020905     C*
144600911017     C* LINEA PARTENZA
144700020905     C*
144800971003    3C     BRVLNP        IFNE      SAVLNP
144900930707     C*
145000971003    4C     *IN02         IFEQ      *ON
145100101203     c     deslnp        andne     *blanks
145200941213     C                   MOVEL     DESLNP        V2DLNP
145300930707     C                   ELSE
145400930707     C                   MOVEL     BRVLNP        NUM3
145500930707     C                   EXSR      CAZORG
145600971003    5C     *IN39         IFEQ      *OFF
145700971003    6C     ORGDE5        IFNE      *BLANKS
145800960522     C                   MOVEL     ORGDE5        V2DLNP
145900971003   X6C                   ELSE
146000960522     C                   MOVEL     ORGDES        V2DLNP
146100971003    6C                   END
146200971003   X5C                   ELSE
146300960522     C                   MOVEL     *BLANKS       V2DLNP
146400971003    5C                   END
146500971003    4C                   ENDIF
146600930707     C*
146700941213     C                   MOVEL     BRVLNP        V2CLNP
146800941213     C                   MOVEL     V2CLNP        SAVLNP
146900971003    3C                   END
147000020905     C*
147100911017     C* LINEA ARRIVO
147200020905     C*
147300971003    3C     BRVLNA        IFNE      SAVLNA
147400930707     C*
147500971003    4C     *IN03         IFEQ      *ON
147600101203     c     deslna        andne     *blanks
147700941213     C                   MOVEL     DESLNA        V2DLNA
147800930707     C                   ELSE
147900930714     C                   MOVEL     BRVLNA        NUM3
148000930707     C                   EXSR      CAZORG
148100020207     C                   CLEAR                   LNAMNF
148200971003    5C     *IN39         IFEQ      *OFF
148300021126     C                   MOVE      §OGNTW        WLANTW
148400020207     C**
148500020207     C* MEMORIZZO LNA ESTERE CHE FANNO IL MANIFEST
148600020207     C                   CLEAR                   LNAMNF
148700020207     C     §OGNTW        IFEQ      'EEX'
148800020207     C     §OGNTW        OREQ      'FED'
148900060322     C     §OGNTW        OREQ      'DPD'
149000020207     C                   MOVEL     'S'           LNAMNF            1
149100020207     C                   END
149200971003    6C     ORGDE5        IFNE      *BLANKS
149300960522     C                   MOVEL     ORGDE5        V2DLNA
149400960522     C                   ELSE
149500960522     C                   MOVEL     ORGDES        V2DLNA
149600971003    6C                   END
149700971003   X5C                   ELSE
149800960522     C                   MOVEL     *BLANKS       V2DLNA
149900971003    5C                   END
150000971003    4C                   ENDIF
150100930707     C*
150200941213     C                   MOVEL     BRVLNA        V2CLNA
150300941213     C                   MOVEL     V2CLNA        SAVLNA
150400971003    3C                   END
150500900521     C*
150600950704     C                   MOVEL     BRVNPG        V2HNPG
150700950704     C                   MOVEL     BRVNRS        V2CNRS
150800941213     C                   MOVEL     BRVNSC        V2CNSC
150900950207     C                   CLEAR                   V2CSE                          SEGNACOLLO ESISTENTE
151000941213     C                   MOVEL     BRVZNC        V2CZNC
151100941213     C                   MOVEL     BRVNPS        V2CNPS
151200000119     C                   Z-ADD     BRVPES        V2CPES                         P.O. SPUNTA
151300000119     C     BRVDFS        IFNE      *ZERO
151400160729     C                   Z-ADD     BRVDFS        v2hdfs                         DATA IMM/VARIAZ.
151401160729     C                   Z-ADD     BRVDFS        G02INV                         DATA IMM/VARIAZ.
151500941202     C                   Z-ADD     *ZERO         G02DAT
151600941202     C                   MOVEL     '3'           G02ERR
151700941202     C                   CALL      'XSRDA8'
151800941202     C                   PARM                    WLBDAT
151900000119     C                   Z-ADD     G02DAT        V2CDFS                         GG/MM/AAAA
152000941202     C                   ELSE
152100000119     C                   Z-ADD     *ZERO         V2CDFS
152101160729     C                   Z-ADD     *ZERO         V2hDFS
152200941202     C                   ENDIF
152300000119     C                   Z-ADD     BRVHMS        V2CHMS                         ORA IMM/VARIAZ.
152400070116     c* Millisecondi immissione spunta
152500070116     C                   Z-ADD     BRVmis        V2Cmis                         ORA IMM/VARIAZ.
152600070215     c* Profilo immissione spunte: se inizia per § non lo devo visualizzare
152700070215     c*  (solo per EDP)
152800070215     c                   if        %subst(brvpru:1:1)='§' and
152900070215     c                             %subst(Knmus:1:3)<>'EDP'
153000070215     c                   clear                   v2cpru
153100070215     c                   else
153200070221     c                   if        %subst(brvpru:1:1)='§'
153300070221     c                   eval      v2cpru=%subst(brvpru:2:9)
153400070221     c                   else
153500070221     c                   eval      v2cpru=brvpru
153600070215     c                   endif
153700070221     c                   endif
153800070221     c
153900070116     c* Tipo apparato spunta
154000070116     c                   clear                   v2ctap
154100070116     c                   if        brvtap<>' '
154200070131     C                   CLEAR                   tibs02ds
154300070116     C                   MOVEL     'C'           T02MOD
154400070116     C                   MOVEL     KNSIF         T02SIF
154500070116     C                   MOVEL     'APS'         T02COD
154600070116     C                   MOVEL     BRVTAP        T02KE1
154700070116     C                   CALL      'TIBS02R'
154800070116     C                   PARM                    KPJBA
154900070116     C                   PARM                    tibs02ds
155000070116     c
155100070116     c                   if        t02err<>' '
155200070116     c                   eval      t02uni=*all'?'
155300070116     c                   endif
155400070116     c
155500070116     c                   eval      v2ctap=brvtap+'-'+%subst(t02uni:1:5)
155600070116     c                   endif
155700070116     c
155800000119     C     BRVDCS        IFNE      *ZERO
155900070228     c                   z-add     brvdcs        v2hdcs
156000000119     C                   Z-ADD     BRVDCS        G02INV                         DATA CARICAM.  .
156100000119     C                   Z-ADD     *ZERO         G02DAT
156200000119     C                   MOVEL     '3'           G02ERR
156300000119     C                   CALL      'XSRDA8'
156400000119     C                   PARM                    WLBDAT
156500000119     C                   Z-ADD     G02DAT        V2CDCS                         GG/MM/AAAA
156600000119     C                   SETOFF                                       71
156700000119     C                   ELSE
156800070228     c                   clear                   v2hdcs
156900000119     C                   Z-ADD     *ZERO         V2CDCS
157000000119     C                   SETON                                        71
157100000119     C                   ENDIF
157200000119     C                   Z-ADD     BRVHCS        V2CHCS                         ORA CARICAM.
157300000119     C     BRVDTR        IFNE      *ZERO
157400000119     C                   Z-ADD     BRVDTR        G02INV                         DATA TRASMIS.  .
157500000119     C                   Z-ADD     *ZERO         G02DAT
157600000119     C                   MOVEL     '3'           G02ERR
157700000119     C                   CALL      'XSRDA8'
157800000119     C                   PARM                    WLBDAT
157900000119     C                   Z-ADD     G02DAT        V2CDTR                         GG/MM/AAAA
158000000119     C                   SETOFF                                       72
158100000119     C                   ELSE
158200000119     C                   Z-ADD     *ZERO         V2CDTR
158300000119     C                   SETON                                        72
158400000119     C                   ENDIF
158500000119     C                   Z-ADD     BRVHTR        V2CHTR                         ORA TRASMISS
158600000119     C                   CLEAR                   V2CFTR
158700070116     C**   BRVFTR        IFEQ      'R'
158800070116     C**   'Ricezion'    CAT       '.':0         V2CFTR
158900070116     C**                 ELSE
159000070116     C**   BRVFTR        IFEQ      'T'
159100070116     C**   'Trasmiss'    CAT       '.':0         V2CFTR
159200070116     C**                 ENDIF
159300070116     C**                 ENDIF
159400070131     c                   movel     brvftr        v2cftr
159500941213     C                   MOVE      BRVVUC        V2CVUC                         VOLUME CML
159600990907     C                   MOVE      BRVPUC        V2CPUC                         PESO CML
159700021127     c                   Z-Add     BrvVuc        V2hVuc
159800021127     c                   Z-Add     BrvPuc        V2hPuc
159900081028     c
160000081028     c                   clear                   v2cTSU
160100081028     c***                if        brvtsu<>' '  and
160200081028     c***                          (%subst(Knmus:1:3)= 'EDP'  or
160300081028     c***                          knmus='REA05902' or knmus='RED00603')
160400081028     c* Visualizzo a tutti se collo doppio
160500081028     c                   if        brvtsu>'0'
160600081028     c
160700081028     c                   if        brvtsu='9'
160800081028     c                   movel     BrvTSU        v2cTSU
160900081028     c                   else
161000081028     c                   movel     brvtsu        w0010             1 0
161100081028     c                   add       1             w0010
161200081028     c                   movel     w0010         v2ctsu
161300081028     c                   endif
161400081028     c
161500080317     c                   seton                                        19
161600080317     c                   else
161700080317     c                   setoff                                       19
161800080317     c                   endif
161900080317     c
162000941213     C                   MOVEL     *BLANKS       V2DATR                         TRANSITO
162100961203     C                   SELECT
162200961203     C     BRVATR        WHENEQ    'D'                                          'D'=DISGUIDO TRANSIT
162300941213     C                   MOVEL     'DISGUIDO'    V2DATR
162400961203     C     BRVATR        WHENEQ    'T'                                          'T'=ARRIVO DAL TRANS
162500070116     C                   movel     'ARR.TRAN'    V2DATR
162600070116     C**   BRVATR        WHENEQ    'A'                                          'A'=ATTESA TRANSITO
162700070116     C**   'ATT.TRAN'    CAT       '.':0         V2DATR
162800961203     C     BRVATR        WHENEQ    'F'                                          'F'=DEFLUENZRANSITO
162900070116     C                   movel     'DEFLUENZ'    V2DATR
163000961203     C                   ENDSL
163100950704     C*
163200960506     C***
163300941213     C     V2CNPS        IFNE      SAVNPS
163400980213     C                   MOVEL     '7J'          KCOD
163500950704     C                   MOVEL(P)  BRVNPS        KKEY
163600921229     C     KTAB2         CHAIN     TABEL                              31
163700921229     C  N31              MOVEL     TBLUNI        DS7J
163800921229     C   31              MOVEL     'M'           §7JRPS
163900921229     C   31              MOVEL     'V'           §7JTPS
164000921231     C                   MOVEL     BRVNPS        SAVNPS            2
164100950704     C                   ENDIF
164200990428     C* SE FOGLIO DEFLUENZA EXPORT, EVIDENZIO PISTOLA AUTOMATICA
164300020207     C**  E P.O. CHE FA IL MANIFEST
164400990428     C                   SETOFF                                       86
164500020207     C     LNAMNF        IFEQ      'S'
164600990428     C     §7JRPS        ANDEQ     'A'
164700990428     C     V2DSPG        ANDEQ     'D  '
164800990428     C                   SETON                                        86
164900990428     C                   ENDIF
165000960506     C* SE IN INTERROGAZIONE DETERMINO SE SPUNTA IN GESTIONE A ME
165100960506     C     *IN20         IFEQ      *ON
165200960506     C     BRVFGS        LOOKUP    FG                                     32
165300960506     C  N32              MOVE      'N'           WNOFG
165400960506     C   32              MOVE      ' '           WNOFG
165500960506     C                   END
165600960506     C* SE SPUNTA NON IN GESTIONE A ME E' AMMESSO SOLO L'ANNULLAMENTO
165700960503     C     WNOFG         IFEQ      'N'
165800960503     C                   SETOFF                                       94
165900960503     C                   SETON                                        24
166000960503     C                   END
166100980216     C* SE SPUNTA NON A ME IN GESTIONE CONFRONTO I TER-ARR PER
166200980216     C* DETERMINARE SE PROTEGGERE O NO IL CAMPO DI ANNULLAMENTO
166300980216     C                   MOVE      *BLANKS       WNOGE             1
166400980216     C     WNOFG         IFEQ      'N'
166500980216     C                   CLEAR                   DSLV55
166600980216     C                   MOVEL     'A'           D55TPT
166700980216     C                   MOVEL     WFGS          D55LIN
166800980216     C                   MOVEL     V2CLNP        D55LNP
166900980216     C                   Z-ADD     V2HDFV        D55DRF
167000980216     C                   CALL      'FNLV55R'
167100980216     C                   PARM                    DSLV55
167200980216     C     D55ERR        IFEQ      ' '
167300980216     C                   MOVEL     D55TFA        WFE0
167400980216     C                   ELSE
167500980216     C                   CLEAR                   WFE0
167600980216     C                   ENDIF
167700980216     C                   MOVEL     V2CFGS        D55LIN
167800980216     C                   CALL      'FNLV55R'
167900980216     C                   PARM                    DSLV55
168000980216     C* SE FILIALI GESTIONE HANNO DIFFERENTI TERMINAL DI ARRIVO DEVO
168100980216     C* PROTEGGERE ANCHE IL CAMPO DI ANNULLAMENTO
168200980216     C     D55ERR        IFNE      *BLANKS
168300980216     C     WFE0          ORNE      D55TFA
168400980216     C                   MOVEL     '1'           WNOGE                          SPUNTA NON GEST
168500980216     C                   END
168600980216     C                   ENDIF
168700980213     C*
168800960404     C* SE FOGLIO CHIUSO DI CATEGORIA <> 1 SPROTEGGO CAMPO ANNULLAMENTO
168900970131     C* SE SPUNTA MANUALE   (WCHIUS E' SOLO PER FNFVV CHIUSO)
169000981030     C                   SETOFF                                       66
169100980216     C     WCHIUS        IFEQ      '1'
169200980216     C     WNOGE         ANDEQ     *BLANKS
169300960917     C* SPROTEGGO CAMPO ANNULLAMENTO ANCHE SE SPUNTA NON A ME IN GESTIO
169400960917     C* NE E CATEGORIA <> 1
169500960503     C     WNOFG         OREQ      'N'
169600980216     C     WNOGE         ANDEQ     *BLANKS
169700960917     C     BRVNPG        ANDNE     1
169800960917     C* OPPURE SE SPUNTA NON A ME IN GESTIONE E SPUNTA DI DEFLUENZA
169900960917     C     WNOFG         OREQ      'N'
170000980216     C     WNOGE         ANDEQ     *BLANKS
170100960917     C     §TVDEF        ANDEQ     'S'
170200981030     C* SE FOGLIO DEFLUENZA ABILITO SOLTANTO L'ANNULLAMENTO
170300021205     C*  SE E' SPUNTA MANUALE non cml
170400970131     C     BRVNPG        OREQ      1
170500970131     C     §TVDEF        ANDEQ     'S'
170600981030     C     WNOFG         ANDEQ     *BLANKS
170700970131     C**
170800960503     C     §7JRPS        IFEQ      'M'
170900021205     C     §7JtPS        andne     'C'
171000960404     C                   SETOFF                                       25
171100981030     C   63              SETON                                        66
171200960404     C                   ELSE
171300960404     C                   SETON                                        25
171400960404     C                   END
171500970131     C*
171600960917     C                   ELSE
171700960917     C                   SETON                                        25
171800960503     C                   END
171900021205     C* SE PISTOLA MANUALE non cml POSSO VARIARE TUTTO
172000921229     C  N24§7JRPS        IFEQ      'M'
172100021205     c     §7jtps        andne     'C'
172200920928     C                   SETON                                        94
172300921229     C                   ELSE
172400921229     C                   SETOFF                                       94
172500920928     C                   END
172600930118     C*
172700941213     C                   MOVEL     §7JTPS        V2CTPS
172800941213     C                   MOVEL     §7JRPS        V2CRPS
172900960506     C***
173000911017     C* CODICE ANOMALIA
173100941213     C                   MOVEL     BRVCAN        V2CCAN
173200941213     C                   MOVEL     *BLANKS       V2DCAN
173300920810     C     BRVCAN        IFNE      ' '
173400940727     C                   MOVEL     '3E'          KCOD
173500950704     C                   MOVEL(P)  BRVCAN        KKEY
173600920807     C     KTAB2         CHAIN     TABEL                              31
173700941213     C  N31              MOVEL     TBLUNI        V2DCAN
173800920810     C                   END
173900980210     C* PROTEGGO COD.ANOMALIA SE ="E" (SPUNTA ERRATA)
174000101126     C     V2CCAN        COMP      'E'                                    97
174100090416     c* se anomalia, vedo se presente in BRVE spunte senza anomalia
174200090416     c* Non se spunta "unica"
174300090416     c                   setoff                                       70
174400090420     c
174500101126     c                   if        *in20 and not *in97 and brvcan<>' '
174600090416     C                   Z-ADD     1             XX                3 0
174700090416     C                   MOVEL     V2hNPG        ALF1
174800090416     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
174900090416     c                   if        *in99 and  NST(XX)<>'1'
175000090416     C     kbrve1        chain(n)  fnbrve1l
175100090416     c                   if        %found(fnbrve1l) and e_brvcan=' '
175200090416     c                   seton                                        70
175300090416     c                   endif
175400090416     c                   endif
175500090420     c                   endif
175600941205     C*
175700941205     C*----------------------------------------------------
175800950131     C*
175900941205     C* SE SPUNTA PARTENZA DI ALTRA FILIALE HO IL FOGLIO DI ABBINAMENTO
176000031121     C***                CLEAR                   V2CNPA                         CAT.F.V.ABBINATO
176100031121     C***                CLEAR                   V2CNFA                         F.V.ABBINATO
176200031121IF  3C     V2CNPG        IFEQ      1                                            SOLO PARTENZE
176300031121     C                   MOVEL     V2CFGS        NUM3
176400031121     C     NUM3          LOOKUP    L1                                     31     ON=TROVATA
176500031121DO 3AC  N31              DO
176600031121     C***  'Abbiname'    CAT       'n.':0        V2CABB
176700031121     C                   SETON                                        24        COME FOSSE CHIUSO
176800031121     C                   SETOFF                                       94
176900020905     c*
177000031121E  3AC                   ENDDO
177100941205E   3C                   ENDIF
177200941205     C*----------------------------------------------------
177300941213     C                   Z-ADD     BR1NRR        V2CNRR
177400941213     C  N01
177500941213     CAN 02              Z-ADD     BR3NRR        V2CNRR
177600941213     C   01
177700941213     CANN02
177800941213     CAN 03              Z-ADD     BR2NRR        V2CNRR
177900900608     C*
178000941213     C                   MOVEL     *BLANKS       V2CANN
178100040122      * IN MANUTENZIONE
178200150817     c***                If         *IN20 = *off   and
178300021206      * Se il foglio non è chiuso
178400150817     c***                           Wchius <> '1'  and
178500021206      * Posso inserire e modificare il volume e il peso
178600021206      * se è una spunta arrivi
178700150817     c***                           V2cnpg = 2     and
178800021206      * se la linea di arrivo è estera
178900150817     c***                          (WlaNtw = 'EEX' or WlaNtw = 'EUP' or
179000150817     c***                          WlaNtw = 'DPD' or WlaNtw = 'FED')
179100021206      * se non è una spunta da pistola CML
179200130506     c***                If        §7jTps <> 'C'
179300130506     c***                Eval      *In75 = *On
179400130506     c***                Eval      *In76 = *On
179500021206      * se è una spunta da pistola CML posso solo inserire il peso e il volume
179600130506     c***                Else
179700130506     c***                If        BrvVuc = *Zeros
179800130506     c***                Eval      *In75 = *On
179900130506     c***                EndIf
180000130506     c***                If        BrvPuc = *Zeros
180100130506     c***                Eval      *In76 = *On
180200130506     c***                EndIf
180300130506     c***                EndIf
180400150817     c***                EndIf
180500021126
180600960612     C                   CLEAR                   V2HESI
180700960405     C* Memorizzo indicatori in campi hidden
180800960405     C                   EXSR      SRIND
180900950207     C                   CLEAR                   V2COLD
181000960522     C                   CLEAR                   V2CANM
181100960522     C                   MOVE      RCDSFL        V2HRCD
181200940727     C                   ADD       1             NRR2
181300900608     C                   ADD       1             B
181400941201     C                   WRITE     LR04S02
181500900521     C*
181600900608     C                   END
181700900608     C  N30              END
181800911017     C*
181900911014     C* 30 ON - FINE LETTURA IL RESTO INIZIALIZZO CON ZERO
182000910320     C* SE CARICATO ALMENO UN RECORD VALIDO MEMORIZZO I TASTI DI DUP
182100910419     C  N20
182200910419     CAN 30B             IFGT      0
182300941213     C                   Z-ADD     V2CNFV        DUPNFV
182400941213     C                   Z-ADD     V2CLNP        DUPLNP
182500941213     C                   Z-ADD     V2CLNA        DUPLNA
182600941213     C                   Z-ADD     V2CNRS        DUPNRS
182700941213     C                   Z-ADD     V2CNSC        DUPNSC
182800980210     C     V2CCAN        IFEQ      'E'
182900980210     C                   CLEAR                   DUPCAN
183000980210     C                   ELSE
183100941213     C                   MOVEL     V2CCAN        DUPCAN
183200980210     C                   END
183300980210     C                   MOVEL     V2CNPG        DUPNPG
183400901221     C                   END
183500911017     C*
183600900608     C* SE FINE LETTURA INIZIALIZZO
183700910930     C   18
183800910930     CANN20
183900910930     CAN 30
184000990920     COR 18
184100990920     CANN20
184200990920     CAN 07B             IFLT      15
184300930118     C* PISTOLA --> 00
184400930118     C     SAVNPS        IFNE      '00'
184500940727     C                   MOVEL     '7J'          KCOD
184600940727     C                   MOVEL     *BLANKS       KKEY
184700940727     C                   MOVEL     '00'          KKEY
184800930118     C     KTAB2         CHAIN     TABEL                              31
184900930118     C  N31              MOVEL     TBLUNI        DS7J
185000930118     C   31              MOVEL     'M'           §7JRPS
185100930118     C   31              MOVEL     'V'           §7JTPS
185200930118     C                   MOVEL     '00'          SAVNPS            2
185300930118     C                   END
185400930118     C*
185500990428     C                   SETOFF                                       248663
185600080318     c                   Setoff                                       757619
185700021206     C                   SETON                                        2125
185800941213     C                   MOVEL     §7JRPS        V2CRPS
185900941213     C                   MOVEL     §7JTPS        V2CTPS
186000941213     C                   MOVEL     *BLANKS       V2CANN
186100941213     C                   MOVEL     V1CNPG        V2CNPG
186200950704     C                   CLEAR                   V2HNPG
186300080318     C                   CLEAR                   V2ctsu
186400941213     C                   MOVEL     V1CFGS        V2CFGS
186500941213     C                   Z-ADD     0             V2CNFV
186600941214     C                   Z-ADD     0             V2CDFV                         GG/MM
186700950706     C                   Z-ADD     0             V2HDFV                         AAAA/MM/GG
186800941213     C                   Z-ADD     0             V2CLNP
186900941213     C                   MOVEL     *BLANKS       V2DSPG
187000941213     C                   MOVEL     *BLANKS       V2DLNP
187100941213     C                   Z-ADD     0             V2CLNA
187200941213     C                   MOVEL     *BLANKS       V2DLNA
187300941213     C                   Z-ADD     0             V2CNRS
187400941213     C                   Z-ADD     0             V2CNSC
187500950207     C                   CLEAR                   V2CSE                          SEGNACOLLO ESISTENTE
187600941213     C                   MOVEL     '  '          V2CZNC
187700941213     C                   MOVEL     '  '          V2CNPS
187800941213     C                   MOVEL     *BLANKS       V2CCAN
187900941213     C                   MOVEL     *BLANKS       V2DCAN
188000941213     C                   Z-ADD     *ZERO         V2CDCS                         DATA CARICAMENTO
188100070228     C                   Z-ADD     *ZERO         V2hDCS                         DATA CARICAMENTO
188200941213     C                   Z-ADD     *ZERO         V2CHCS                         ORA CARICAMENTO
188300000119     C                   Z-ADD     *ZERO         V2CDFS                         DATA IMM/VARIAZ.
188301160729     C                   Z-ADD     0             V2HDFs                         AAAA/MM/GG
188400000119     C                   Z-ADD     *ZERO         V2CHMS                         ORA IMM/VARIAZ.
188500070130     C                   Z-ADD     *ZERO         V2Cmis                         ORA IMM/VARIAZ.
188600000119     C                   Z-ADD     *ZERO         V2CDTR                         DATA TRASMISIAZ.
188700000119     C                   Z-ADD     *ZERO         V2CHTR                         ORA TRASMISSAZ.
188800000119     C                   CLEAR                   V2CFTR
188900070130     C                   CLEAR                   V2Cpru
189000070130     C                   CLEAR                   V2Ctap
189100000119     C                   SETON                                        7172
189200021016     C                   movel     v2cfgs        V2CPES
189300941213     C                   MOVEL     *BLANKS       V2DATR                         TRANSITO
189400941213     C                   MOVEL     *BLANKS       V2CABB                         ABBINAMENTO
189500031121     C***                Z-ADD     *ZERO         V2CNPA                         CATEG.FOG.ABB.
189600031121     C***                Z-ADD     *ZERO         V2CNFA                         FOGLIO ABBINAATO
189700941213     C                   Z-ADD     *ZERO         V2CVUC                         VOLUME
189800990907     C                   Z-ADD     *ZERO         V2CPUC                         PESO
189900021127     c                   Clear                   V2hVuc
190000021127     c                   Clear                   V2hPuc
190100941213     C                   Z-ADD     0             V2CNRR
190200950207     C                   CLEAR                   V2COLD
190300960522     C                   CLEAR                   V2CANM
190400960612     C                   CLEAR                   V2HESI
190500960522     C                   MOVE      RCDSFL        V2HRCD
190600930118     C*
190700960612     C                   EXSR      SRIND
190800960612     C*
190900990920     C     B             DOWLT     15
191000940727     C                   ADD       1             NRR2
191100900608     C                   ADD       1             B
191200941201     C                   WRITE     LR04S02
191300960612     C                   ENDDO
191400930118     C                   END
191500930118     C*
191600940727     C                   Z-ADD     NRR2          SAVNRR            4 0
191700940727     C                   Z-ADD     NRR2          REC2
191800911014     C                   SETOFF                                       21
191801160728
191802160728     c                   clear                   rrnlast
191803160728     c                   if        nrr2>*zero
191804160728     c                   eval      rrnlast=nrr2-1
191805160728     c                   endif
191806160728     c* vedo se devo ordinare il sfl
191807160728     c                   if        w_ord<>w_nfv
191808160728     c                   exsr      sr_ordina
191809160728     c                   endif
191900900509     C*
192000900518     C                   ENDSR
192100940727     C**************************************************************************
192200940727     C* LEGGO NUOVA CATEGORIA
192300940727     C**************************************************************************
192400921229     C     RICCAT        BEGSR
192500940727     C*
192600921229     C                   Z-ADD     0             SAVNPG
192700101201     C                   ADD       1             CC
192800101201     c                   if        cc<=20 and cator(CC)<>*blanks
192900101201     C                   MOVE      CATor(CC)     SAVNPG
193000921229     C* TORNO A LEGGERE CON NUOVA CATEGORIA
193100140625     C  N03KBRV4         SETLL     FnBRV05L
193200000118     C   03
193300140625     CANN06KBRV5         SETLL     FnBRV05L
193400000118     C   03
193500000118     CAN 06
193600140625     CANN04KBRV51        SETLL     FnBRV05L
193700000118     C   03
193800000118     CAN 06
193900140625     CAN 04KBRV52        SETLL     FnBRV05L
194000921229     C                   SETOFF                                       17
194100921229     C*
194200921229     C                   ELSE
194300921229     C*
194400921229     C                   MOVEL     9             SAVNPG
194500921229     C                   END
194600921229     C                   ENDSR
194700940727     C**************************************************************************
194800940727     C* PULIZIA SUBFILE
194900940727     C**************************************************************************
195000900518     C     PULSF         BEGSR
195100940727     C*
195200900518     C                   SETON                                        35
195300941201     C                   WRITE     LR04C02                                      SFLCTL
195400900518     C                   SETOFF                                       35
195500940727     C*
195600900518     C                   ENDSR
195700940727     C**************************************************************************
195800940727     C* CONTROLLO DATI SUBFILE
195900940727     C**************************************************************************
196000900608     C     CONTR2        BEGSR
196100940727     C*
196200940727     C                   Z-ADD     1             NRR2
196300910320     C                   SETOFF                                       9091
196400911014     C*
196500910320     C                   SETOFF                                       32
196600941201     C                   READC     LR04S02                                31
196700930707     C*
196800930707    1C     *IN31         DOWEQ     *OFF
196900960405     C* Imposto indicatori memorizzati nei campi hidden
197000960405     C                   EXSR      SRIND1
197100960405     C*
197200950711     C   10              Z-ADD     DUPNFV        V2CNFV
197300950711     C   11              Z-ADD     DUPLNP        V2CLNP
197400950711     C   12              Z-ADD     DUPLNA        V2CLNA
197500950711     C   13              Z-ADD     DUPNRS        V2CNRS
197600950711     C   14              Z-ADD     DUPNSC        V2CNSC
197700950711     C   11
197800950711     COR 12
197900950711     COR 13
198000950711     COR 14              CLEAR                   V2CSE                          SEGNACOLLO ESISTENTE
198100980210     C   15DUPCAN        IFNE      'E'
198200980210     C                   MOVEL     DUPCAN        V2CCAN
198300980210     C                   ELSE
198400980210     C                   CLEAR                   V2CCAN
198500980210     C                   END
198600950711     C   16              MOVEL     DUPNPG        V2CNPG
198700911017     C*
198800930707    2C     RECORD        IFNE      *ZEROS
198900960522     C     RCDSFL        ANDNE     V2HRCD
199000960628     C     V2CNFV        ORNE      *ZEROS
199100960628     C     RCDSFL        ANDNE     V2HRCD
199200900709     C*
199300900709     C*
199400910320     C* CONTROLLO SE RICHIESTO RICERCA CODICE ANOMALIA
199500941213    3C     V2CCAN        IFEQ      '?'
199600940727     C                   MOVEL     CODUT         §KUT
199700940727     C                   MOVEL     '3E'          §COD
199800941213     C                   MOVE      *BLANKS       V2DCAN
199900941213     C                   MOVE      *BLANKS       V2CCAN
200000910320     C                   MOVE      *BLANKS       §KEY
200100910320     C                   CALL      'X§TABER'
200200031119     C                   PARM                    §KUT
200300910320     C                   PARM                    §COD
200400940727     C                   PARM                    §KEY
200500940727     C                   PARM                    §DES
200600941213     C                   MOVEL     §KEY          V2CCAN
200700941213     C                   MOVEL     §DES          V2DCAN
200800910320     C                   SETON                                        9131
200900910320     C                   GOTO      ENDCT2
201000930707    3C                   END
201100911017     C* C O N T R O L L I
201200920930     C**
201300920930     C* IMPOSSIBILE CANCELLARE SE RECORD DI NUOVA IMMISSIONE
201400941213    3C     V2CNRR        IFEQ      0
201500941213     C     V2CANN        ANDNE     *BLANKS
201600920930     C                   SETON                                        479031
201700920930     C                   GOTO      ENDCT2
201800930707    3C                   END
201900920807     C* CATEGORIA
202000941213    3C     V2CANN        IFEQ      ' '
202100020808     C                   Z-ADD     1             XI
202200941213     C                   MOVEL     V2CNPG        ALF1              1
202300020808     C     ALF1          LOOKUP    CAT(XI)                                32
202400920807     C* ERRORE
202500920807     C  N32              SETON                                        469031
202600920807     C  N32              GOTO      ENDCT2
202700021205     C* SE SPUNTA NON MANUALE o manuale non cml CONSEGNA, POSSO PASSARE SOLO ALLA
202800950704     C*  CATEGORIA 3
202900950705     C     V2CRPS        IFNE      'M'
203000021205     C     V2CRPS        oreq      'M'
203100021205     C     V2CtPS        andne     'C'
203200961202     C     V2HNPG        IFEQ      4
203300000726     C     V2HNPG        OREQ      8
203400000727     C     V2CNPG        IFNE      3
203500950705     C     V2CNPG        ANDNE     4
203600000726     C     V2CNPG        ANDNE     8
203700950704     C                   SETON                                        899031
203800950705     C                   GOTO      ENDCT2
203900950704     C                   ENDIF
204000000727     C                   ENDIF
204100961202     C     V2HNPG        IFEQ      1
204200961202     C     V2CNPG        ANDNE     3
204300961202     C     V2CNPG        ANDNE     1
204400961202     C                   SETON                                        879031
204500961202     C                   GOTO      ENDCT2
204600961202     C                   ENDIF
204700961202     C     V2HNPG        IFEQ      3
204800961202     C     V2CNPG        ANDNE     1
204900961202     C     V2CNPG        ANDNE     3
205000961202     C                   SETON                                        879031
205100961202     C                   GOTO      ENDCT2
205200961202     C                   ENDIF
205300961202     C                   ENDIF
205400941202     C*----------------------------------------------------
205500941202     C* LINEA PARTENZA
205600941213    4C     V2CLNP        IFEQ      0
205700941202     C                   SETON                                        429031
205800941202     C                   GOTO      ENDCT2
205900941202   X4C                   ELSE
206000941213     C                   MOVEL     V2CLNP        NUM3
206100941202     C                   EXSR      CAZORG
206200941202     C*
206300941202     C   39              SETON                                        429031
206400941202     C   39              GOTO      ENDCT2
206500941202     C*
206600001205     C                   MOVEL     ORGDE3        OG143
206700960522     C     ORGDE5        IFNE      *BLANKS
206800960522     C                   MOVEL     ORGDE5        V2DLNP
206900960522     C                   ELSE
207000941213     C                   MOVEL     ORGDES        V2DLNP
207100960522     C                   END
207200941202    4C                   END
207300941202     C*----------------------------------------------------
207400900608     C* FOGLIO VIAGGIO
207500941202     C* (FARE DOPO IL CONTROLLO SU LNP)
207600941202     C*----------------------------------------------------
207700941213    4C     V2CNFV        IFEQ      0
207800910323     C                   SETON                                        419031
207900900608     C                   GOTO      ENDCT2
208000941202    4C                   ENDIF
208100941202     C*
208200941202     C* PER CATEGORIA <> 1 CERCO FRA GLI ARRIVI
208300941213     C     V2CNPG        IFNE      1                                            1=PARTENZE
208400941202     C*
208500961202     C*?    F O G L I     V A R I      ?
208600941213     C                   MOVEL     V2CNPG        KNPG                           ..CATEGORIA
208700941213     C                   Z-ADD     V2CNFV        KNFV                           ..NR.FOGLIO
208800941213     C                   MOVEL     V2CFGS        KFGS                           ..FIL.GESTIONE
208900941201     C     KFVV1         CHAIN     FNFVV01L                           41
209000930707    5C  N41FVVATB        IFNE      *BLANKS
209100900626     C                   SETON                                        41        ANNULLATO
209200930707    5C                   END
209300900626     C*
209400910323     C   41              SETON                                        9031
209500900626     C   41              GOTO      ENDCT2
209600920807     C*
209700930423     C* NON E' FOGLIO GESTIBILE: QUESTO CONTROLLO NON LO ESEGUO SE
209800930423     C*   SI TRATTA DI FOGLIO I.M.P.
209900941201    5C     V1CNPG        IFNE      '3'
210000941201     C     V1CNPG        OREQ      '3'
210100930423     C     FVVSPG        ANDNE     'P'
210200930423     C*
210300941201     C     FVVFGS        LOOKUP    L6                                     32
210400920807     C  N32              SETON                                        499031
210500920807     C  N32              GOTO      ENDCT2
210600930707    5C                   END
210700920807     C*
210800911017     C* CHIUSO
210900001128     C* SE FOGLIO CAT 4 DEVO CONTROLLARE LA BOLLA
211000001128     C*  ALTRIMENTI SEMPRE ERRORE
211100001128     C     V2CNPG        IFNE      4
211200001128    5C     FVVFCF        ANDEQ     'S'
211300911014     C                   SETON                                        529031     CHIUSO
211400910320     C                   GOTO      ENDCT2
211500930707    5C                   END
211600911014     C*
211700920520     C* DATA FOGLIO VIAGGIO
211800950706     C                   Z-ADD     FVVDFV        V2HDFV                         AAAA/MM/GG
211900921229     C                   MOVE      FVVDFV        DATA
212000941213     C                   MOVE      MM            V2CDFV
212100941213     C                   MOVEL     GG            V2CDFV
212200950131     C*
212300950131     C* PER CATEGORIA 4=CONSEGNE METTO A VIDEO PARTE DELLA DESCRIZIONE
212400950131    5C     FVVNPG        IFEQ      4                                            4=CONSEGNE
212500000713    5C     FVVNPG        OREQ      8                                            4=CONSEGNE
212600950131     C                   MOVEL     FVVDSF        V2CABB
212700950704     C                   SETON                                        23
212800950704     C                   ELSE
212900961202     C                   CLEAR                   V2CABB
213000961202     C*
213100961202     C* LA CATEGORIA 3 PUO' ANDARE A 1 E VICEVERSA
213200961202     C     FVVNPG        IFEQ      3
213300961202     C                   SETON                                        23
213400961202     C                   ELSE
213500950704     C                   SETOFF                                       23
213600961202     C                   ENDIF
213700950131    5C                   ENDIF
213800920520     C*
213900911014     C* DECODIFICO TIPO INVENTARIO
214000941213     C                   MOVEL     *BLANKS       V2DSPG
214100930707    5C     FVVSPG        IFNE      ' '
214200020808     C                   Z-ADD     1             XI
214300020808     C     FVVSPG        LOOKUP    SPG(XI)                                32
214400070116     c   32              eval      v2dspg='IM'+ %subst(DES(XI):1:1)
214500070116     C***32              MOVEL     DES(XI)       V2DSPG
214600930707    5C                   END
214700941202     C*
214800941202     C                   ELSE
214900950705     C*
215000950705     C* LA MANUTENZIONE DEI FV PARTENZA CONCESSA SOLO A SIMFEL
215100950705     C     *IN20         IFEQ      *OFF
215200950705     C     V1CFGS        ANDNE     SIMFEL
215300950705     C                   SETON                                        499031     CHIUSO
215400950705     C                   GOTO      ENDCT2
215500950705     C                   ENDIF
215600941202     C*
215700941213     C                   MOVEL     *BLANKS       V2DSPG                         TIPO INVENTARIO
215800950704     C                   CLEAR                   V2CABB
215900941202     C*
216000941202     C*?    F O G L I O    P A R T E N Z E    ?
216100941202     C*
216200941213     C                   Z-ADD     V2CNFV        KNFV                           ..NR.FOGLIO
216300020920     C                   movel     v2cfgs        KFGS                           ..P.O.GESTIONE
216400020808     C     K02FGV01      chain     FNFGV01L                           41
216500941202     C   41              SETON                                        9031
216600941202     C   41              GOTO      ENDCT2
216700000503     C* ESTENSIONE
216800020808     C     K02FGV01      chain     FNFGW01L                           34
216900000503     C     *IN34         IFEQ      *ON
217000000503     C     FGWATB        ORNE      ' '
217100000503     C                   CLEAR                   FGWFF3
217200000503     C                   CLEAR                   FGWFF3
217300000503     C                   CLEAR                   FGWFL3
217400000503     C                   CLEAR                   FGWFL3
217500000503     C                   ENDIF
217600941202     C* DATA FOGLIO VIAGGIO
217700950706     C                   Z-ADD     FGVDFV        V2HDFV                         AAAA/MM/GG
217800941202     C                   MOVE      FGVDFV        DATA
217900941213     C                   MOVE      MM            V2CDFV
218000941213     C                   MOVEL     GG            V2CDFV
218100960829     C* VISUALIZZO SE FV DI DEFLUENZA O LOCALE
218200960829     C                   MOVE      FGVTTR        WTTR              1
218300101202     C***                MOVE      FGWFGS        WPT               3 0
218400960829     C                   EXSR      RICTTR
218500961202     C*
218600961202     C* LA CATEGORIA 1 LA POSSO PORTARE A 3 E VICEVERSA
218700961202     C                   SETON                                        23
218800040316     c* errore se foglio chiuso
218900040316     c     fgvfcf        ifeq      'S'
219000040316     C                   SETON                                        529031     CHIUSO
219100040316     C                   GOTO      ENDCT2
219200040316     c                   endif
219300941202     C*
219400941202     C                   ENDIF
219500941202     C*----------------------------------------------------
219600900608     C* LINEA ARRIVO
219700941213    4C     V2CLNA        IFEQ      0
219800910323     C                   SETON                                        439031
219900900608     C                   GOTO      ENDCT2
220000930707   X4C                   ELSE
220100941213     C                   MOVEL     V2CLNA        NUM3
220200930707     C                   EXSR      CAZORG
220300910320     C*
220400930707     C   39              SETON                                        439031
220500930707     C   39              GOTO      ENDCT2
220600960522     C     ORGDE5        IFNE      *BLANKS
220700960522     C                   MOVEL     ORGDE5        V2DLNA
220800960522     C                   ELSE
220900941213     C                   MOVEL     ORGDES        V2DLNA
221000960522     C                   END
221100020207     C                   MOVE      §OGNTW        WLANTW
221200941213     C*
221300941213     C* PER LE PARTENZE, LA LNA DEVE ESSERE FRA LE LINEE DI SCARICO
221400941213    5C     V2CNPG        IFEQ      1                                            1=PARTENZE
221500941213    6C     V2CLNA        IFNE      FGVLNA
221600971003     C                   MOVEL     V2CLNA        ALF3              3
221700941213     C     ALF3          LOOKUP    FFV1                                   39
221800941213     C  N39              SETON                                        439050
221900941213     C  N39              GOTO      ENDCT2
222000941213    6C                   ENDIF
222100941213    5C                   ENDIF
222200941213     C*
222300930707    4C                   END
222400941202     C*----------------------------------------------------
222500900608     C* SEGNACOLLO
222600941213    4C     V2CNSC        IFEQ      0
222700910323     C                   SETON                                        449031
222800900608     C                   GOTO      ENDCT2
222900930707    4C                   END
223000900608     C* CONTROLLO SE SEGNACOLLO DOPPIO
223100941227     C  NKN
223200941227     CANNKP              MOVEL     V2CFGS        KFGS                           FIL.GESTIONE
223300000119     C  NKN
223400140625     CANNKPKBRV6         CHAIN(N)  FnBRV05L                           32
223500941213     C  NKN
223600941213     CANNKP
223700941213    4CANN32BR3NRR        IFNE      V2CNRR
223800920320     C                   SETON                                        459031
223900900608     C                   GOTO      ENDCT2
224000930707    4C                   END
224100950207     C*
224200950207     C* VERIFICO L'ESISTENZA DEL SEGNACOLLO NELLE BOLLE;
224300950207     C* SE NON ESISTE, SEGNALO ERRORE MA POSSIBILE FORZATURA
224400950207     C     RECORD        IFNE      V2COLD
224500950207     C                   MOVEL     RECORD        V2COLD                         MEMORIZZO IL COLLO
224600950207     C                   CLEAR                   V2CSE
224700950207     C                   ENDIF
224800021028     c     v2clna        lookup    l1                                     34
224900021028     c* Solo se si tratta di mia bolla arrivi do eventuale errore su
225000021028     c*  foglio consegna chiuso senza rec bolla
225100021028    0c                   if        *in34
225200021028     C     KART          setll     FNART27L                               32     ON=TROVATO
225300001128    1C     V2CNPG        IFEQ      4
225400001128     C     FVVFCF        ANDEQ     'S'
225500001128    2C     *IN32         IFEQ      *OFF
225600001128     C                   SETON                                        529031     CHIUSO
225700001128     C                   GOTO      ENDCT2
225800001128     C*
225900001128   X2C                   ELSE
226000001128     C*
226100001128     C* BOLLA TROVATA: CERCO IN NUMERO DISTINTA SE = AL FOGLIO CHIUSO
226200021127     C     KART          CHAIN(n)  FNART27L                           34
226300101201     C  N34Kblp          CHAIN     FNARB01L                           35
226400001128    3C     *IN34         IFEQ      *ON
226500001128     C     *IN35         OREQ      *ON
226600101202     C     blpDCM        OREQ      0
226700101202     C     blpNDC        ORNE      FVVNFV
226800101202     C     blpifp        ORNE      FVVfgs
226900090416     C                   SETON                                        569031     CHIUSO
227000001128    3C                   ENDIF
227100001128    2C                   ENDIF
227200001128    1C                   ENDIF
227300021028   x0c                   else
227400021028     c* 32 off - do per non trovata la bolla arrivi
227500021028     c                   setoff                                       32
227600021028    0C                   ENDIF
227700021028     c* Controllo in assoluto l'esistenza della bolla o in blt o in btt
227800021028     c* tanto devo dare errore comunque se non lo trovo da nessuna
227900021028     c*  parte. non devo leggere solo le mie bolle (part o trans)
228000021028     C  N32KART          SETLL     FNBLT27L                               32     ON=TROVATO
228100150623     C  N32KART          SETLL     FNart27L                               32      ON=TROVATO
228200960612     C*
228300960612     C     *IN32         IFEQ      *OFF
228400960612     C                   MOVEL     'N'           V2HESI                          NON ESISTE
228500960612     C                   ELSE
228600960612     C                   MOVEL     'S'           V2HESI                          ESISTE
228700960612     C                   ENDIF
228800960612     C*
228900021028     c**  non trovata bolla:
229000001205     C     *IN32         IFEQ      *OFF
229100001205     C     *INKN         ANDEQ     *OFF
229200001205     C* NEGLI ALTRI CASI ERRORE FORZABILE
229300001205     C     V2CSE         IFEQ      *BLANKS
229400950207     C                   MOVEL     '1'           V2CSE
229500950207     C                   SETON                                        649031
229600950207     C                   GOTO      ENDCT2
229700950207     C                   ENDIF
229800001205     C                   ENDIF
229900021028     C***                ENDIF
230000911014     C*
230100910320     C* CONTROLLO CODICE ANOMALIA
230200941213    4C     V2CCAN        IFNE      *BLANKS
230300940727     C                   MOVEL     '3E'          KCOD
230400940727     C                   MOVEL     *BLANKS       KKEY
230500941213     C                   MOVEL     V2CCAN        KKEY
230600920807     C     KTAB2         CHAIN     TABEL                              32
230700040625     c  n32              movel     tbluni        ds3e
230800040625     c                   if        *in32 or tblflg<>' '  or §3euti='N'
230900910323     C   32              SETON                                        539031
231000910320     C   32              GOTO      ENDCT2
231100941213     C                   MOVEL     TBLUNI        V2DCAN
231200040625     c                   endif
231300980210     C* CODICE ANOMALIA 'E' NON UTILIZZABILE
231400980210     C     V2CCAN        IFEQ      'E'
231500980210     C                   SETON                                        519031
231600980210     C                   GOTO      ENDCT2
231700980210     C                   END
231800930707    4C                   END
231900920811     C*
232000900608     C* SE NON CI SONO ERRORI AGGIORNO E VADO AVANTI A LEGGERE
232100941213     C                   Z-ADD     V2CNPG        DUPNPG
232200941213     C                   Z-ADD     V2CNFV        DUPNFV
232300941213     C                   Z-ADD     V2CLNP        DUPLNP
232400941213     C                   Z-ADD     V2CLNA        DUPLNA
232500941213     C                   Z-ADD     V2CNRS        DUPNRS
232600941213     C                   Z-ADD     V2CNSC        DUPNSC
232700941213     C                   MOVEL     V2CCAN        DUPCAN
232800930707    3C                   END
232900910323     C     ENDCT2        TAG
233000921229     C* NUOVO INSERIMENTO
233100941213    3C     V2CNRR        IFEQ      0
233200941213     C                   MOVEL     '00'          V2CNPS
233300930707    3C                   END
233400021206
233500021206      * Posso inserire e modificare il volume e il peso
233600021206      * se è una spunta arrivi
233700150817     c***                If         V2cnpg = 2 and
233800021206      * se la linea di arrivo è estera
233900150817     c***                          (WlaNtw = 'EEX' or WlaNtw = 'EUP' or
234000150817     c***                          WlaNtw = 'DPD' or WlaNtw = 'FED')
234100021206      * se non è una spunta da pistola CML
234200150817     c***                If        V2cTps <> 'C'
234300150817     c***                Eval      *In75 = *On
234400150817     c***                Eval      *In76 = *On
234500021206      * se è una spunta da pistola CML posso solo inserire il peso e il volume
234600150817     c***                Else
234700150817     c***                If        V2hVuc = *Zeros
234800150817     c***                Eval      *In75 = *On
234900150817     c***                EndIf
235000150817     c***                If        V2hPuc = *Zeros
235100150817     c***                Eval      *In76 = *On
235200150817     c***                EndIf
235300150817     c***                EndIf
235400150817     c***                Endif
235500921228     C*
235600921229     C*
235700021205     C* SE PISTOLA MANUALE non cml POSSO VARIARE TUTTO
235800970131    3C  N24V2CRPS        IFNE      'M'
235900021205     c     v2crps        oreq      'M'
236000021212     c     v2ctps        andeq     'C'
236100931122     C* IMPOSSIBILE ANNULLARE SPUNTA SE INSERITA CON PISTOLA BUONA
236200941213     C     V2CANN        IFNE      ' '
236300941213     C     V2CRPS        ANDNE     'A'
236400931122     C                   SETON                                        629031
236500931122     C                   ENDIF
236600930707    3C                   END
236700960612     C*
236800970131   X2C                   ELSE
236900960612     C*
237000970131    3C     V2CNFV        IFEQ      *ZEROS
237100960612     C                   CLEAR                   V2DLNP
237200960612     C                   CLEAR                   V2DLNA
237300960612     C                   CLEAR                   V2DSPG
237400960612     C                   CLEAR                   V2CDFV
237500960612     C                   CLEAR                   V2HDFV
237600960612     C                   CLEAR                   V2DCAN
237700960612     C                   CLEAR                   V2HESI
237800960612     C                   MOVEL     RECORD        V2COLD
237900970131    3C                   END
238000950711    2C                   END
238100911017     C*
238200940727     C                   Z-ADD     NRR2          REC2
238300910323     C                   SETON                                        36
238400960612     C                   EXSR      SRIND
238500941201     C                   UPDATE    LR04S02
238600021206     C                   SETOFF                                       3621
238700930707     C*
238800930707     C                   SETOFF                                       32
238900941201     C  N31              READC     LR04S02                                31
239000930707    1C                   END
239100911017     C*
239200900614     C                   ENDSR
239300940727     C**************************************************************************
239400940727     C* CONTROLLO PER INSERIMENTO SPUNTE DAL AL
239500940727     C**************************************************************************
239600920323     C     CTRDAL        BEGSR
239700940727     C*
239800920323     C                   SETOFF                                       90
239900920323     C*
240000920807     C* CATEGORIA
240100941201     C     V3CNPG        IFEQ      ' '
240200920807     C                   SETON                                        4890
240300920807     C                   GOTO      ENDAGG
240400920807     C                   END
240500920807     C* RICERCA IN TABELLA
240600941201     C     V3CNPG        IFEQ      '?'
240700920807     C                   MOVEL     CODUT         §KUT
240800920807     C                   MOVEL     '7N'          §COD
240900941201     C                   MOVE      ' '           V3CNPG
241000920807     C                   MOVEL     *BLANKS       §KEY
241100920807     C                   CALL      'X§TABER'
241200940727     C                   PARM                    §KUT
241300940727     C                   PARM                    §COD
241400940727     C                   PARM                    §KEY
241500940727     C                   PARM                    §DES
241600941201     C                   MOVEL     §KEY          V3CNPG
241700920807     C                   SETON                                        90
241800941205     C                   MOVEL     'S'           WRIEM             1            RIEMETTO TESTATA
241900920807     C                   GOTO      ENDAGG
242000920807     C                   END
242100920807     C* CONTROLLO
242200020808     C                   Z-ADD     1             XI
242300020808     C     V3CNPG        LOOKUP    CAT(XI)                                32
242400920807     C*
242500920807     C  N32              SETON                                        9048
242600920807     C  N32              GOTO      ENDAGG
242700941202     C*----------------------------------------------------
242800941202     C* LINEA PARTENZA
242900941202     C     V3CLNP        IFEQ      0
243000941202     C                   SETON                                        4290
243100941202     C                   GOTO      ENDAGG
243200941202     C                   ELSE
243300941202     C                   MOVEL     V3CLNP        NUM3
243400941202     C                   EXSR      CAZORG
243500941202     C*
243600941202     C   39              SETON                                        4290
243700941202     C   39              GOTO      ENDAGG
243800941202     C*
243900960522     C     ORGDE5        IFNE      *BLANKS
244000960522     C                   MOVEL     ORGDE5        V3DLNP
244100960522     C                   ELSE
244200941202     C                   MOVEL     ORGDES        V3DLNP
244300960522     C                   END
244400941202     C                   END
244500941202     C*----------------------------------------------------
244600920323     C* FOGLIO VIAGGIO
244700941202     C* (FARE DOPO IL CONTROLLO SU LNP)
244800941202     C*----------------------------------------------------
244900941201     C     V3CNFV        IFEQ      0
245000920323     C                   SETON                                        4190
245100920323     C                   GOTO      ENDAGG
245200941202     C                   ENDIF
245300941202     C*
245400941202     C                   MOVEL     *BLANKS       V3DSPG
245500941202     C*
245600941202     C* PER CATEGORIA <> 1 CERCO FRA GLI ARRIVI
245700941202     C     V3CNPG        IFNE      '1'                                          1=PARTENZE
245800941202     C*
245900941202     C*?    F O G L I O    A R R I V I    ?
246000941201     C                   MOVEL     V3CNPG        KNPG                           ..CATEGORIA
246100941201     C                   Z-ADD     V3CNFV        KNFV                           ..NR.FOGLIO
246200941201     C                   Z-ADD     V1CFGS        KFGS                           ..FIL.GESTIONE
246300941201     C     KFVV1         CHAIN     FNFVV01L                           41
246400920323     C  N41FVVATB        IFNE      *BLANKS
246500920323     C                   SETON                                        41        ANNULLATO
246600920323     C                   END
246700920323     C*
246800920323     C   41              SETON                                        90
246900920323     C   41              GOTO      ENDAGG
247000920323     C* CHIUSO
247100941229     C     FVVFCF        IFEQ      'S'
247200950207     C                   SETON                                        5290       CHIUSO
247300920323     C                   GOTO      ENDAGG
247400920323     C                   END
247500930423     C*
247600930423     C* NON GESTIBILE: QUESTO CONTROLLO NON LO ESEGUO SE SI TRATTA
247700930423     C*   DI FOGLIO I.M.P.
247800941201     C     V1CNPG        IFNE      '3'
247900941201     C     V1CNPG        OREQ      '3'
248000930423     C     FVVSPG        ANDNE     'P'
248100930423     C*
248200941201     C     FVVFGS        LOOKUP    L6                                     32
248300920807     C  N32              SETON                                        5390
248400920807     C  N32              GOTO      ENDAGG
248500930423     C                   END
248600920323     C*
248700920323     C* DECODIFICO TIPO INVENTARIO
248800920810     C     FVVSPG        IFNE      ' '
248900020808     C                   Z-ADD     1             XI
249000020808     C     FVVSPG        LOOKUP    SPG(XI)                                32
249100020808     C   32              MOVEL     DES(XI)       V3DSPG
249200920323     C                   END
249300941202     C*
249400941202     C                   ELSE
249500941202     C*
249600941202     C*?    F O G L I O    P A R T E N Z E    ?
249700941202     C                   Z-ADD     V3CNFV        KNFV                           ..NR.FOGLIO
249800020920     C                   z-add     v1cfgs        KFGS                           ..P.O.GESTIONE
249900020808     C     K02FGV01      chain     FNFGV01L                           41
250000941202     C   41              SETON                                        90
250100941202     C   41              GOTO      ENDAGG
250200941202     C*
250300920810     C                   END
250400941202     C*----------------------------------------------------
250500920323     C* LINEA ARRIVO
250600941201     C     V3CLNA        IFEQ      0
250700920323     C                   SETON                                        4390
250800920323     C                   GOTO      ENDAGG
250900920323     C                   ELSE
251000941201     C                   MOVEL     V3CLNA        NUM3
251100930707     C                   EXSR      CAZORG
251200920323     C*
251300930707     C   39              SETON                                        4390
251400930707     C   39              GOTO      ENDAGG
251500960522     C     ORGDE5        IFNE      *BLANKS
251600960522     C                   MOVEL     ORGDE5        V3DLNA
251700960522     C                   ELSE
251800941201     C                   MOVEL     ORGDES        V3DLNA
251900960522     C                   END
252000920323     C                   END
252100920323     C* SEGNACOLLO  DAL
252200941201     C     V3CNSD        IFEQ      0
252300920323     C                   SETON                                        4490
252400920323     C                   GOTO      ENDAGG
252500920323     C                   END
252600920323     C* SEGNACOLLO  DAL
252700941201     C     V3CNSA        IFEQ      0
252800920323     C                   SETON                                        4590
252900920323     C                   GOTO      ENDAGG
253000920323     C                   END
253100920323     C* AL < DAL
253200941201     C     V3CNSA        IFLT      V3CNSD
253300920323     C                   SETON                                        4690
253400920323     C                   GOTO      ENDAGG
253500920323     C                   END
253600920323     C* CONTROLLO SE DIFFERENZA CORRISPONDE AL NUMERO IMMESSO
253700941201     C     V3CNSA        SUB       V3CNSD        DIFF              5 0
253800920323     C                   ADD       1             DIFF
253900941201     C     DIFF          IFNE      V3CNUM
254000920323     C                   SETON                                        4790
254100920323     C                   GOTO      ENDAGG
254200920323     C                   END
254300920323     C*
254400920323     C     ENDAGG        ENDSR
254500940727     C**************************************************************************
254600940727     C* CONFRONTO RECORD LETTI PER VEDERE SE SELEZIONARLI
254700940727     C**************************************************************************
254800900608     C     CONFR         BEGSR
254900900626     C*
255000941212     C* SOLO UNA FILIALE GESTIONE, SE IN GESTIONE
255100941212     C     BRVFGS        IFNE      V1CFGS
255200941202     C     V1CFGS        ANDNE     *ZERO
255300941202     C                   SETON                                        90
255400941202     C                   GOTO      ENDCON
255500941202     C                   ENDIF
255600941212     C*
255700101201     c                   if        v1clna>0   and brvlna<>v1clna
255800900608     C                   SETON                                        90
255900900608     C                   GOTO      ENDCON
256000900608     C                   END
256100900608     C* SERIE
256200101201     c                   if        v1cnrs<>99 and brvnrs<>v1cnrs
256300900608     C                   SETON                                        90
256400941201     C   02
256500941201     CAN 03BRVNRS        IFGT      V1CNRS
256600921229     C   18              SETON                                        30
256700921229     C  N18              EXSR      RICCAT
256800921229     C                   END
256900921229     C*
257000900608     C                   GOTO      ENDCON
257100900608     C                   END
257200921229     C*
257300921229     C* SEGNACOLLO DAL
257400101216     C   04BRVNSC        IFLT      savNCD
257500900608     C                   SETON                                        90
257600900608     C                   GOTO      ENDCON
257700900608     C                   END
257800101216     C***
257900921229     C* SEGNACOLLO DAL SENZA AL
258000101216     c***
258100101216     C   04
258200101216     CANN05BRVNSC        IFGT      savNCD
258300101216     c
258400921229     C                   SETON                                        90
258500921229     C     *IN02         IFEQ      '1'
258600921229     C     *IN03         ANDEQ     '1'
258700921229     C   06
258800921229     CAN 18              SETON                                        30
258900921229     C   06
259000921229     CANN18              EXSR      RICCAT
259100921229     C                   END
259200921229     C                   GOTO      ENDCON
259300921229     C                   END
259400101216     c***
259500921229     C* SEGNACOLLO DAL  E    AL
259600101216     c***
259700101216    1c                   if        *in05
259800101201     c
259900101216    2C     BRVNSC        IFGT      SAVNCA
260000900608     C                   SETON                                        90
260100101216    3C     *IN02         IFEQ      '1'
260200921229     C     *IN03         ANDEQ     '1'
260300921229     C   06
260400921229     CAN 18              SETON                                        30
260500921229     C   06
260600921229     CANN18              EXSR      RICCAT
260700101216    3C                   END
260800900608     C                   GOTO      ENDCON
260900101216    2c                   endif
261000101216    1c                   endif
261100101201     c
261200101201     c* Se entro i limiti impostati e c'e' num spedizione, controllo
261300101216     c*    se ne fa parte (serve per i non sequenziali)
261400101216    1c                   if        v1cbosp>0
261500101201     c     kbodet        setll     fnblt01l
261600101216    2c                   if        not %equal(fnblt01l)
261700101201     c     kbodet        setll     fnart01l
261800101216    3c                   if        not %equal(fnart01l)
261900101201     C                   SETON                                        90
262000101201     C                   GOTO      ENDCON
262100101216    3c                   endif
262200101216    2c                   endif
262300101216    1c                   endif
262400101201     c
262500101216     C**                 END
262600101216     C**                 END
262700101216     C* PISTOLA
262800941201     C     V1CNPS        IFNE      99
262900941201     C     BRVNPS        IFNE      V1CNPS
263000900608     C                   SETON                                        90
263100900608     C                   GOTO      ENDCON
263200900608     C                   END
263300900608     C                   END
263400920810     C*
263500941213     C                   MOVEL     BRVFGS        V2CFGS
263600941213     C                   MOVEL     BRVNPG        V2CNPG
263700000118     C                   Z-ADD     BRVNFV        V2CNFV
263800941213     C                   Z-ADD     BRVVUC        V2CVUC                         VOLUME
263900990907     C                   Z-ADD     BRVPUC        V2CPUC                         PESO
264000920810     C*
264100920911     C* CARICO SOLO SPUNTE DI FOGLI GESTITI SOLO SE NON E' RICHIAMATO
264200000118     C                   MOVEL     BRVNPG        WOKNPG                         CHIAVE FOGLIO
264300000118     C                   MOVE      BRVNFV        WOKNFV                           "      "
264400000118     C                   MOVE      BRVFGS        WOKFGS                           "      "
264500950208     C*
264600920911     C     BRVKEY        IFNE      SAVKEY
264700000118     C                   Z-ADD     BRVNPG        KNPG                           CATEGORIA
264800000118     C                   Z-ADD     BRVNFV        KNFV                           NR.FOGLIO
264900000118     C                   Z-ADD     BRVFGS        KFGS                           FIL.GESTIONE
265000000118     C                   EXSR      RILFOG
265100920810     C                   END
265200920810     C*
265300941201     C  N26
265400941201     CANN20V1CNFV        IFLE      *ZEROS
265500941202     C     V1CNPG        ANDNE     '1'                                          SOLO ARRIVI
265600930426     C*
265700941201     C     V1CNPG        IFNE      '3'
265800941201     C     V1CNPG        OREQ      '3'
265900930426     C     FVVSPG        ANDNE     'P'
266000941201     C     FVVFGS        LOOKUP    L6                                     33
266100920810     C  N33              SETON                                        90
266200920811     C  N33              GOTO      ENDCON
266300930426     C                   END
266400920911     C                   END
266500920811     C*
266600920811     C* D E C O D I F I C H E   F I S S E   D E L   F O G L I O
266700920811     C     BRVKEY        IFNE      SAVKEY
266800960411     C                   SETOFF                                       2423
266900960404     C                   MOVE      *BLANKS       WCHIUS            1
267000941213     C                   MOVEL     *BLANKS       V2DSPG
267100101202     C* DATA FOGLIO VIAGGIO
267200101202     C                   Z-ADD     wdfv          V2HDFV                         AAAA/MM/GG
267300101202     C                   MOVE      wdfv          DATA
267400101202     C                   MOVE      MM            V2CDFV
267500101202     C                   MOVEL     GG            V2CDFV
267600941202     C*
267700941202     C* PER CATEGORIA <> 1 CERCO FRA GLI ARRIVI
267800941213     C     V2CNPG        IFNE      1                                            1=PARTENZE
267900941202     C*
268000961202     C*?    F O G L I     V A R I     ?
268100920811     C*
268200920811     C* SE CHIUSO PROTEGGO TUTTI I CAMPI
268300941229     C     FVVFCF        IFEQ      'S'
268400920811     C                   SETOFF                                       94
268500920811     C                   SETON                                        24
268600960404     C                   MOVE      '1'           WCHIUS
268700920811     C                   END
268800920811     C*
268900950131     C*
269000950131     C* PER CATEGORIA 4=CONSEGNE METTO A VIDEO PARTE DELLA DESCRIZIONE
269100950131    5C     FVVNPG        IFEQ      4                                            4=CONSEGNE
269200000713    5C     FVVNPG        OREQ      8                                            4=CONSEGNE
269300950131     C                   MOVEL     FVVDSF        V2CABB
269400961202     C*
269500961202    6C     FVVFCF        IFNE      'S'
269600961202     C                   SETON                                        23
269700961202     C                   ELSE
269800961202     C                   SETOFF                                       23
269900961202    6C                   ENDIF
270000961202     C*
270100961202   X5C                   ELSE
270200961202    6C     FVVNPG        IFEQ      3                                            3=INV
270300961202    7C     FVVFCF        IFNE      'S'
270400070116     C                   SETON                                        23
270500961202     C                   ELSE
270600961202     C                   SETOFF                                       23
270700961202    7C                   ENDIF
270800961202     C**
270900961202   X6C                   ELSE
271000961202     C                   SETOFF                                       23
271100961202    6C                   ENDIF
271200961202     C**
271300950705     C                   CLEAR                   V2CABB
271400950131    5C                   ENDIF
271500920811     C*
271600920811     C* DECODIFICA TIPO INVENTARIO
271700920811     C     FVVSPG        IFNE      ' '
271800020808     C                   Z-ADD     1             XI
271900020808     C     FVVSPG        LOOKUP    SPG(XI)                                31
272000070116     c   31              eval      v2dspg='IM'+ %subst(DES(XI):1:1)
272100920811     C                   END
272200941202     C*
272300941202     C                   ELSE
272400941202     C*
272500941202     C*?    F O G L I O    P A R T E N Z E    ?
272600040709     C                   CLEAR                   V2CABB
272700970131     C*
272800941214     C     WFILE         IFEQ      'FGV'
272900960829     C* VISUALIZZO SE FV DI DEFLUENZA O LOCALE
273000960829     C                   MOVE      FGVTTR        WTTR              1
273100101202     C***                MOVE      FGWFGS        WPT               3 0
273200960829     C                   EXSR      RICTTR
273300980213     C     §TVDEF        IFEQ      'S'
273400020808     C                   MOVEL     FGVLNA        NUM3
273500980213     C                   EXSR      CAZORG
273600020207     C  N39              MOVEL     §OGNTW        SAVNTW
273700020207     C   39              MOVE      *BLANKS       SAVNTW
273800980213     C                   ENDIF
273900970131     C*
274000970131     C* 23 ON - SPROTEGGO LA CATEGORIA PER PERMETTERE DI VARIARLA
274100961202     C                   SETON                                        23
274200941214     C                   ENDIF
274300970131     C**
274400941214     C     WFILE         IFEQ      'FVA'
274500941214     C* IL FOGLIO E', PER FORZA, GIA' CHIUSO
274600941214     C                   SETOFF                                       94
274700941214     C                   SETON                                        24
274800960829     C* VISUALIZZO SE FV DI DEFLUENZA O LOCALE
274900960829     C                   MOVE      FVATTR        WTTR              1
275000101202     C****               MOVE      FWAFGS        WPT
275100960829     C                   EXSR      RICTTR
275200941202     C                   ENDIF
275300941202     C*
275400941202     C                   ENDIF
275500941202     C*
275600920811     C* SALVO NUOVA CHIAVE
275700000118     C                   MOVEL     BRVKEY        SAVKEY           10 0
275800920811     C                   END
275900000120     C*
276000000120     C* SOLO PARTENZE DELLA MIA AREA: SE LA SPUNTA E' GIA' TRASMESSA
276100000120     C* NON E' MODIFICABILE (QUINDI NIENTE ALTA INTENSITA')
276200000120     C                   SETOFF                                       63
276300000120     C     BRVNPG        IFEQ      1                                            1=PARTENZE
276400070220     C***  BRVFGS        LOOKUP    L1                                     99     ON=TROVATO
276500070220     C***99BRVFTR        IFNE      *BLANKS                                      GIA' TRASMESSO
276600070220     C     BRVFTR        IFNE      *BLANKS                                      GIA' TRASMESSO
276700000120     C                   SETON                                        63
276800000120     C                   ENDIF
276900000120     C                   ENDIF
277000000120     C* SE E' FOGLIO DI DEFLUENZA CHIUSO PER ESTERO COME SE LE SPUNTE
277100000120     C*  FOSSERO STATE TRASMESSE
277200000120     C     BRVNPG        IFEQ      1
277300000120     C     §TVDEF        ANDEQ     'S'
277400000120     C     FGVFCF        ANDEQ     'S'
277500020207     C     SAVNTW        IFEQ      'EEX'
277600020207     C     SAVNTW        OREQ      'DPD'
277700020207     C     SAVNTW        OREQ      'FED'
277800000120     C                   SETON                                        63
277900000120     C                   ENDIF
278000020207     C                   ENDIF
278100900608     C*
278200900608     C     ENDCON        ENDSR
278300940727     C**************************************************************************
278400940727     C* AGGIORNO RECORD
278500940727     C**************************************************************************
278600900608     C     REGIS         BEGSR
278700940727     C*
278800940727     C                   Z-ADD     1             NRR2
278900911014     C*
279000941201     C                   READC     LR04S02                                31     LETTURA
279100930707     C*
279200930707    1C     *IN31         DOWEQ     *OFF
279300960405     C* Imposto indicatori memorizzati nei campi hidden
279400960405     C                   EXSR      SRIND1
279500911014     C*
279600930707    2C     RECORD        IFNE      *ZEROS
279700960522     C     RCDSFL        ANDNE     V2HRCD
279800960612     C*
279900960612    2C     RECORD        OREQ      *ZEROS
280000960612     C     RCDSFL        ANDNE     V2HRCD
280100950706     C                   CLEAR                   WAGG
280200960502     C                   CLEAR                   WNPS
280300960612     C     RECORD        IFEQ      0
280400960612     C                   MOVEL     'A'           V2CANN
280500960612     C                   ENDIF
280600920811     C*
280700911014     C* A N N U L L O
280800941213    3C     V2CANN        IFNE      *BLANKS
280900920811     C*
281000941213     C                   Z-ADD     V2CNRR        BR1NRR
281100070116     C     BR1NRR        CHAIN     FnBRV00F                           33
281200920811     C*
281300920811     C* VEDO SE ELIMINARE LA DATA ARRIVO/ENTRATA: ESCLUDO SE SPUNTA IMP
281400921020     C  N33              MOVEL     '1'           PARFLG
281500921020     C  N33              EXSR      UPDBOL
281600911017     C*
281700000119     C  N33
281800070116     CANN95              DELETE    FnBRV000                                      DELETO
281900040303     C*
282000070215     C* SE SPUNTA CONSEGNA DELETO ANCHE RECORD SU FnBRVE SE PRESENTE
282100040303     C* VERIFICO IL FLAG NELLA DS7N
282200040303     c                   if        not *in33 and not *in95
282300040303     C                   Z-ADD     1             XX                3 0
282400040303     C                   MOVEL     V2hNPG        ALF1
282500040303     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
282600040303     c                   if        *in99 and  NST(XX)='1'
282700040303     C                   EXSR      DLBRVE
282800040303     C                   ENDIF
282900040303     C                   ENDIF
283000070710     c* Se no spunta unica, e con anomalia cancello eventuale
283100070710     c*  spunta senza anomalia in BRVE
283200070711     c***                if        not *in33 and not *in95  and
283300070711     c***                          *in99 and  NST(XX)<>'1' and brvcan<>' '
283400070711     C***  kbrve1        delete    fnbrve1l
283500070711     C***                ENDIF
283600040303     c
283700900611     C*
283800970116     C* PER LE ANOMALIE 145 E 145 VEDO SE RICREARE LEGGENDO TUTTE
283900970116     C*  LE ALTRE SPUNTE CON DATA >= A QUELLA CHE CANCELLO
284000001204     C  N95              EXSR      AN1456
284100970117     C*
284200970117   X3C                   ELSE
284300911017     C*
284400920811     C* 32 ON  - I M M I S S I O N E
284500920811     C* 32 OFF - V A R I A Z I O N E
284600911017     C                   SETOFF                                       32
284700070220     c                   clear                   fnls45ds
284800070220     c                   movel     'S'           dls45ric
284900070220     c                   movel     comtfp        dls45tfp
285000070220     c
285100070220     C                   MOVEL     ' '           PARFLG
285200070220     C***                MOVEL     ' '           FLG
285300070220     C***                MOVEL     *ZEROS        BAROFV
285400921020     C*
285500941213    4C     V2CNRR        IFNE      0
285600941213     C                   Z-ADD     V2CNRR        BR1NRR
285700070116     C     BR1NRR        CHAIN     FnBRV00F                           32
285800921020     C*
285900920811     C* SE MODIFICATO RECORD PER VARIAZIONE
286000000120     C  N32              Z-ADD     BRVNFV        WRVNFV
286100930707    5C  N32BRVCON        IFNE      VI2CON
286200921020     C*
286300070424     c* Se tutto uguale non faccio nulla
286400070424    6C     BRVCO2        IFEQ      VI2CO2
286500070710     C     BRVNFV        andeq     V2CNFV
286600930118     C*
286700930118     C* MODIFICATO SOLO FOGLIO VIAGGIO
286800070424    7C***  BRVNFV        IFNE      V2CNFV
286900060418     c
287000060418     c* Se categoria 3 devo richiamare fnlsa8r
287100070424     c***  brvnpg        ifeq      3
287200070424     C**                 MOVEL     '1'           PARFLG
287300070424     c**                 else
287400070220     C***                MOVEL     '2'           FLG
287500070424     C**                 MOVEL     '2'           dls45FLG
287600070220     C***                Z-ADD     BRVNFV        BAROFV            6 0
287700070424     C**                 Z-ADD     BRVNFV        dls45NFVO
287800070424    7C**                 END
287900070424    7C**                 END
288000960604     C*
288100070424     C* VARIATO TUTTO IL RECORD (compreso numero di foglio)
288200930707   X6C                   ELSE
288300921021     C                   MOVEL     '1'           PARFLG
288400930707    6C                   END
288500930707    5C                   END
288600921020     C*
288700921230     C* ELIMINAZIONE ANOMALIA
288800930707    5C     PARFLG        IFNE      '1'
288900930707    6C     BRVCAN        IFNE      ' '
289000941213     C     V2CCAN        ANDEQ     ' '
289100921020     C                   MOVEL     '3'           PARFLG
289200070710     c* Cancello eventuale BRVE scritto
289300090416     c* ES - meglio di no. potrebbero fare i furbi se se ne accorgono
289400090416     C**                 Z-ADD     1             XX                3 0
289500090416     C**                 MOVEL     V2hNPG        ALF1
289600090416     C**   ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
289700090416     c**                 if        *in99 and  NST(XX)<>'1'
289800090416     C**** kbrve1        delete    fnbrve1l
289900090416     c**                 endif
290000070710     c
290100930707    6C                   END
290200930707    5C                   END
290300921020     C*
290400950706     C* SE MODIFICATO RECORD LANCIO PGM CHE ELIMINA SUL DETTAGLIO COLLI
290500930707    5C     PARFLG        IFNE      ' '
290600921020     C                   EXSR      UPDBOL
290700930707    5C                   END
290800920811     C*
290900930707   X4C                   ELSE
291000950706     C* 32 ON - NUOVA SPUNTA INSERITA
291100900611     C                   SETON                                        32
291200970204     C                   CLEAR                   BR1NRR
291300950706    4C                   ENDIF
291400950706     C*
291500950706     C**
291600941207     C* SE NON VI SONO ERRORI (95OFF) REGISTRO LA SPUNTA
291700950706    4C     *IN95         IFEQ      *OFF
291800950706     C                   SETON                                        33
291900950706     C*
292000950706     C* SE DEL RECORD HO MODIFICATO SOLO CAN O VUC --> NON FACCIO NIENT
292100950706    5C     *IN32         IFEQ      *ON
292200950706     C     BRVCO2        ORNE      VI2CO2
292300950706     C     BRVNFV        ORNE      V2CNFV
292400040303     C** 99              MOVEL     V2CFGS        KFGS
292500040303     C**                 CLEAR                   DSLV55
292600040303     C**                 MOVEL     'P'           D55TPT
292700040303     C**                 MOVE      KFGS          D55LIN
292800040303     C**                 Z-ADD     V2HDFV        D55DRF
292900040303     C**                 CALL      'FNLV55R'
293000040303     C**                 PARM                    DSLV55
293100040303     C**   D55ERR        IFNE      *BLANKS
293200040303     C**                 MOVEL     V2CFGS        KFLE
293300040303     C**                 ELSE
293400040303     C**                 MOVEL     D55TFP        KFLE
293500040303     C**                 END
293600040303     c   99              movel     v2cfgs        kfgs
293700010628     C* CANCELLAZIONE SPUNTE DOPPIE RELATIVE ALLA SPUNTA ORIGINARIA
293800010628     C*
293900010628     C* DA FARE SOLO PER SPUNTE DI CUI E' VARIATO IL SEGNACOLLO
294000010628     C* (SE UN SEGNACOLLO DA CONSEGNA LO PASSO IN IMA LASCIO COMUNQUE
294100010628     C* LE EVENTUALI SPUNTE DOPPIE PRESENTI. SARA' LA STAMPA DELLA
294200010628     C* QUADRATURA CHE DOVRA' SEGNALARE CHE IL SEGNACOLLO SI TROVA IN
294300010628     C* IMA)
294400040303     c
294500040303     c                   if        not *in32 and
294600040303     c                             record <> brvrec
294700040303     C                   Z-ADD     1             XX                3 0
294800040303     C                   MOVEL     V2hNPG        ALF1
294900040303     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
295000040303     c                   if        *in99 and  NST(XX)='1'
295100040303     C                   EXSR      DLBRVE
295200040303     C                   ENDIF
295300040303     C                   ENDIF
295400070711     c* non deleto BRVE se cambiato segnacollo per l'anomalia
295500070711     c*  updato se cambia il numero del foglio
295600070711     c                   if        not *in32 and brvco2=vi2co2 and
295700070711     c                             brvnfv<>v2cnfv
295800070711     C                   Z-ADD     1             XX                3 0
295900070711     C                   MOVEL     V2CNPG        ALF1
296000070711     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
296100070711     c                   if        *in99 and  NST(XX)<>'1'
296200070711     C     kbrve1        chain     fnbrve1l
296300070711     c                   if        %found(fnbrve1l)
296400090416     c                   z-add     v2cnfv        e_brvnfv
296500070711     c                   update    fnbrve1
296600070711     C                   ENDIF
296700070711     c
296800070711     C                   ENDIF
296900070711     C                   ENDIF
297000950706     C*
297100950706     C* SE TROVO UNA SPUNTA ESATTAMENTE UGUALE, CANCELLO QUESTA E
297200950706     C*  L'ALTRA LA RIAGGIORNO
297300020924     c                   z-add     br1nrr        sa2nrr
297400000119     C                   Z-ADD     V2CNFV        W0060             6 0
297500070116     C     KBRV7         CHAIN     FnBRV001                           33
297600950706     C*
297700950706    6C     *IN33         IFEQ      *OFF
297800040303     c
297900040303     C                   Z-ADD     1             XX                3 0
298000040303     C                   MOVEL     V2CNPG        ALF1
298100040303     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
298200040303     c                   if        *in99 and  NST(XX)='1'
298300070710     c                   eval      wnst=nst(XX)
298400040303     C                   EXSR      SRBRVE
298500040303     C                   ENDIF
298600070710     c
298700070710     c* Se non spunta unica ma presente senza anomalia, memeorizzo
298800070710     c                   if        *in99 and  NST(XX)<>'1' and brvcan=' '
298900070710     c                             and v2ccan<>' '
299000070710     c                   eval      wnst=nst(XX)
299100070710     C                   EXSR      SRBRVE
299200070710     C                   ENDIF
299300070215     c
299400070116     C  N32              DELETE    FnBRV000
299500070223     c*
299600070223     c                   EXSR      riempi
299700950706     C*
299800950706    7C     V2CCAN        IFNE      ' '
299900950706     C                   MOVEL     V2CCAN        BRVCAN
300000950706    7C                   ENDIF
300100080318     c
300200080318     c* BRVTSU --> spunta multipla, tengo quella che c'e' delle 2
300300080318     c                   if        v2ctsu<>' '
300400080318     c                   eval      brvtsu=v2ctsu
300500080318     c                   endif
300600070223     c
300700070215     C* PER IMPOSTARE BRVNPS e BRVTAP DETERMINO LA PISTOLA PRIORITARIA
300800980216     C                   MOVEL     V2CNPS        NPS1
300900070228     C                   movel     v2hdcs        dcs1
301000071127     C                   movel     v2chcs        hcs1
301100070228     C                   movel     v2ctap        tap1
301200980213     C                   Z-ADD     BRVNPS        NPS2
301300070215     C                   movel     BRVtap        tap2
301400070228     C                   movel     BRVdcs        dcs2
301500070228     C                   movel     BRVhcs        hcs2
301600980213     C                   EXSR      SRNPS
301700980213     C                   Z-ADD     NPSP          BRVNPS
301800070215     c                   eval      brvtap=TAPP
301900070228     c                   eval      brvdcs=dcsP
302000070228     c                   eval      brvhcs=hcsP
302100070215     c
302200070116     C                   UPDATE    FnBRV001
302300070223     C                   MOVEL     'S'           WAGG              1
302400070223     c*
302500070223     c* Aggiornamenti finali sulle bolle
302600070223     c                   EXSR      AggioFINALE
302700150824     c*
302800150824     c* colli che partono da più filiali: se devo scrivere AGB record "CB"
302900150824     c                   if        %subst(dls45flo:1:1)='B'
303000150824     c                   exsr      DA_BSP
303100150824     c                   endif
303200950706     C*
303300950706   X6C                   ELSE
303400070223     c
303500070223     C* 33 ON - GIA' ESISTENTE: IN UPDATE
303600070116     C                   UNLOCK    fnbrv00F
303700020924     c* mi reimposto il num relativo di record
303800020924     c                   z-add     sa2nrr        br1nrr
303900911016     C*
304000040304     C* VERIFICO IL FLAG NELLA DS7N
304100040304     C                   Z-ADD     1             XI
304200040304     C                   MOVEL     V2CNPG        ALF1
304300040304     C     ALF1          LOOKUP    CAT(XI)                                99     ON=TROVATA
304400040304    7C   99NST(XI)       IFeq      '1'                                          NR.SPUNTE GG.
304500140625     C     KBRV3a        CHAIN     fnbrv05L                           33
304600040304     C*
304700040304    8c                   if        not *in33
304800040304     C* MI SALVO L'ANOMALIA PRESENTE SULLA SPUNTA
304900080318     C                   MOVEL     BRVtsu        tsu03
305000080318     C                   MOVEL     BRVCAN        CAN03
305100070215     C                   Z-ADD     BRVNPS        NPS03
305200070220     c                   movel     brvtap        TAP03
305300070228     c                   movel     brvdcs        DCS03
305400070228     c                   movel     brvhcs        HCS03
305500040304     C* SE  E' QUELLO CHE STO VARIANDO --> ACCENDO IL 33
305600040304    9C     BR1NRR        IFEQ      BR3NRR
305700040304     C                   SETON                                        33
305800040304    9C                   ENDIF
305900040304    8C                   ENDIF
306000040304   x7c                   else
306100040304     c* Se le devo tenere tutte , l'unico controllo è per num foglio
306200040304     c*  che il pgm ha già fatto, per cui vado per forza in write
306300040304     c                   seton                                        33
306400040304    7c                   endif
306500941207     C*
306600941212     C*----------------------------------------------------
306700040225     C* 1)IN UN FOGLIO (NPG+fgs+NFV)POSSO AVERE UNA SOLA SPUNTA PER COLLO
306800040225     C* 2) SE foglio diverso,  E' IL FLAG §7NNST CHE
306900040225     C*    DICE SE TENERE UNA SOLA SPUNTA (='1') O NO (anche + spunte
307000040225     c*    nella stessa data adesso)
307100070131     C*
307200140625     C   33              UNLOCK    fnbrv05L
307300040304     c
307400040304     c                   if        not *in33 and *in99 and  NST(XI)='1'
307500070710     c                   eval      wnst=nst(XI)
307600040304     C                   EXSR      SRBRVE
307700040304     C                   ENDIF
307800070710     c                   if        not *in33 and *in99 and  NST(XI)<>'1' and
307900070710     c                             brvcan=' ' and v2ccan<>' '
308000070710     c                   eval      wnst=nst(XI)
308100070710     C                   EXSR      SRBRVE
308200070710     C                   ENDIF
308300040304     c*
308400070116     C  N32BR1NRR        CHAIN     fnbrv00F                           32
308500950706    6C                   ENDIF
308600960613   X5C                   ELSE
308700960613     C*
308800960613     C* SE HO MODIFICATO SOLO L'ANOMALIA MESSA O TOLTA O VARIATA
308900070223     C*  LA RISCRIVO COSI' COM'E'( il 33 = *on)
309000070711     C                   Z-ADD     1             XX                3 0
309100070711     C                   MOVEL     V2cNPG        ALF1
309200070711     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
309300070711     c                   if        *in99 and  NST(XX)<>'1' and
309400070711     c                             brvcan=' ' and v2ccan<>' '
309500070711     c                   eval      wnst=nst(XX)
309600070711     C                   EXSR      SRBRVE
309700070711     c                   endif
309800070711     c*
309900960613     C                   MOVEL     V2CCAN        BRVCAN
310000070223     c* Di fatto sotto la riaggiorna, confrontando V2CCAN con BRVCAN
310100070223     c*  ma non servirebbe. Lo fa perchè entra nel giro degli
310200070711     c*  aggiornamenti normali
310300950706    5C                   ENDIF
310400941209     C*----------------------------------------------------
310500070221     c*
310600070223     c* non Faccio più nulla se già aggiornata la spunta e le bolle
310700070223     c*  (quando trovo spunta uguale si fnbrv01l)
310800950706     C*
310900070223   4ac                   if        wagg=' '
311000941213     C                   MOVEL     V2CFGS        BRVFGS                         FIL.GESTIONE
311100941229     C                   MOVEL     SIMFEL        BRVFLE                         FIL.ELABORATORE
311200941213     C                   MOVEL     V2CNPG        BRVNPG                         CATEGORIA FOGLIO
311300000120     C                   Z-ADD     V2CNFV        BRVNFV
311400941213     C                   MOVEL     V2CLNP        BRVLNP
311500941213     C                   MOVEL     V2CLNA        BRVLNA
311600941213     C                   MOVEL     V2CNRS        BRVNRS
311700941213     C                   MOVEL     V2CNSC        BRVNSC
311800941213     C                   MOVEL     V2CZNC        BRVZNC                         ZONA
311900980213     C* SE VADO IN UPDATE O IN WRITE DI UNA NUOVA SPUNTA TENGO LA PIS-
312000980213     C* TOLA DEL VIDEO (33 ON)
312100980213     C* SE VADO IN UPDATE DI UNA SPUNTA GIA' PRESENTE TENGO LA PISTOLA
312200980213     C* PRIORITARIA    (33 OFF)
312300980213    5C     *IN33         IFEQ      *ON
312400980213     C                   MOVEL     V2CNPS        BRVNPS                         PISTOLA
312500070223     c                   movel     v2ctap        brvtap
312600070223     c* Per nuova immissione, imposto A-As400
312700070228     c   32              if        brvtap=' '
312800070223     c                   movel     'A'           brvtap
312900070223     c                   endif
313000070223     c
313100980213   X5C                   ELSE
313200980216     C                   MOVEL     V2CNPS        NPS1
313300070223     C                   MOVEL     v2ctap        tap1
313400070228     C                   MOVEL     v2chcs        hcs1
313500070228     C                   MOVEL     v2hdcs        dcs1
313600070215     C                   Z-ADD     NPS03         NPS2
313700070215     C                   movel     TAP03         TAP2
313800070228     C                   movel     DCS03         dcs2
313900070228     C                   movel     HCS03         hcs2
314000980213     C                   EXSR      SRNPS
314100980213     C                   Z-ADD     NPSP          BRVNPS
314200070215     c                   movel     TAPP          BRVTAP
314300070228     c                   movel     DCSP          BRVDCS
314400070228     c                   movel     HCSP          BRVHCS
314500980213    5C                   END
314600070215     c
314700950906     C     *IN32         IFEQ      *OFF
314800950906     C     *IN33         OREQ      *OFF
314900950906     C*
315000950906     C     V2CCAN        IFNE      ' '                                          UPDAT
315100950906     C                   MOVEL     V2CCAN        BRVCAN
315200950906     C                   ELSE
315300950906     C* SOLO SE HO TROVATO UN'ALTRA SPUNTA SU CUI VADO IN UPDATE(33OFF)
315400950906     C*  MEMORIZZO LA SUA ANOMALIA SE C'E'
315500070215     C  N33              MOVEL     CAN03         BRVCAN
315600950906     C                   ENDIF
315700080318     c
315800080318     C     V2Ctsu        IFNE      ' '                                          UPDAT
315900080318     C                   MOVEL     V2Ctsu        BRVtsu
316000080318     C                   ELSE
316100080318     C* SOLO SE HO TROVATO UN'ALTRA SPUNTA SU CUI VADO IN UPDATE(33OFF)
316200080318     C*  MEMORIZZO LA SUA spunta multipla  se c'e'
316300080318     C  N33              MOVEL     tsu03         BRVtsu
316400080318     C                   ENDIF
316500950906     C*
316600950906     C                   ELSE
316700070223     c* Record completamente nuovo: 32 on (nuova immissione a video)
316800070223     c*                             33 on (non trovata altra spunta secondo
316900070223     c*                                    le regole sopra indicate)
317000950906     C                   MOVEL     V2CCAN        BRVCAN                         WRITE
317100070223     c* PESO e VOLUME VDL:  devo sempre tenere i dati del video,
317200070223     c*   per cui pulisco eventuali dati sporchi da precedente lettura
317300070223     c                   clear                   brvvuc
317400070223     c                   clear                   brvpuc
317500070223     c* DATA e ORA CARICAMENTO: =0
317600070223     C                   MOVE      *ZEROS        BRVDCS
317700070223     C                   MOVE      *ZEROS        BRVHCS
317800070223     C                   MOVEL     *BLANKS       BRVnvr
317900070223     C                   CLEAR                   BRVATR                         FLAG TRANSITO
318000070223     C                   CLEAR                   BRVtsu                         FLAG TRANSITO
318100950906     C                   ENDIF
318200070223     c
318300070223     c* Dati comuni a precedente aggiornamento
318400070223     c                   EXSR      RIEMPI
318500070131     c
318600000516     C* SE VARIATO TUTTO IL RECORD O PER IMMISSIONE: RICERCO ZONA
318700000516    6C     PARFLG        IFEQ      '1'
318800000516     C     PARFLG        OREQ      'A'
318900000516     C     *IN32         OREQ      '1'
319000000516     C                   EXSR      ZONA
319100000516     C                   MOVEL     BRVZNC        V2CZNC
319200000516    6C                   END
319300070221     c
319400070223     c                   EXSR      AggioFINALE
319500921021     C*
319600070223     C**  N95
319700070223    5C**ANN96WAGG          IFEQ      ' '
319800070223     c
319900070223     c                   if        not *in95 and not *in96
320000960612     C*
320100960612     C* SE E' SPUNTA ENTRATA O PARTENZA E NON HO TROVATO NE' LA BOLLA
320200960612     C*  PARTENZA NE' LA BOLLA TRANSITO, PER SERIE =0
320300960612     C*  IMPOSTO NELLA SPUNTA FLAG TRASMISSIONE "N" E 99999999
320400960612     C*  PER NON AUTOGENERARE LA SPUNTA PARTENZA O NON TRASMETTERLA
320500070221    6C     V2CNPG        IFEQ      5
320600070221     C     V2CNPG        OREQ      1
320700960612     C*
320800960612     C     V2CLNP        LOOKUP    L1                                     38
320900960612     C*
321000070221    7C   38V2CNRS        IFEQ      0
321100960612     C     V2HESI        ANDEQ     'N'
321200960612     C                   MOVEL     'N'           BRVFTR
321300960612     C                   MOVEL     *HIVAL        BRVDTR
321400000119     C                   MOVEL     *HIVAL        BRVHTR
321500070221    7C                   ENDIF
321600070221    6C                   ENDIF
321700910323     C*
321800070221    6C     *IN32         IFEQ      *OFF
321900140625     C  N33              DELETE    fnbrv005
322000070116     C                   UPDATE    fnbrv000
322100970117     C                   EXSR      AN1456
322200070221    6C                   ENDIF
322300950706     C*
322400000119     C   33
322500070116     CAN 32              WRITE     fnbrv000
322600000119     C  N33
322700140625     CAN 32              UPDATE    fnbrv005
322800950706    5C                   ENDIF
322900910325     C*
323000150824     c* colli che partono da più filiali: se devo scrivere AGB record "CB"
323100150824     c                   if        %subst(dls45flo:1:1)='B'
323200150824     c                   exsr      DA_BSP
323300150824     c                   endif
323400070223   4aC                   END
323500070223    4C                   END
323600930707    3C                   END
323700930707    2C                   END
323800930707     C*
323900950711     C  N31              READC     LR04S02                                31     LETTURA
324000950711    1C                   ENDDO
324100921020     C*
324200921020     C* AGGIORNO L'ULTIMO LETTO CHE NON HO AGGIORNATO PERCHE' ERRORE
324300950711     C     *IN95         IFEQ      *ON
324400950711     C     *IN96         OREQ      *ON
324500950711     C*
324600950711     C                   SETON                                        36
324700950711     C                   UPDATE    LR04S02
324800950711     C                   SETOFF                                       36
324900950711     C                   ENDIF
325000910323     C*
325100921020     C                   ENDSR
325200070223     C**************************************************************************
325300070223     C* RIempimento campi di FNBRV
325400070223     C**************************************************************************
325500070223     c     RIEMPI        BEGSR
325600070223     c
325700070223    7C     V2CVUC        IFGT      0
325800070223     C                   Z-ADD     V2CVUC        BRVVUC
325900070223    7C                   ENDIF
326000070223     C*
326100070223    7C     V2CPUC        IFGT      0
326200070223     C                   Z-ADD     V2CPUC        BRVPUC
326300070223    7C                   ENDIF
326400070223     C*
326500070223     C***                Z-ADD     WDATEU        BRVDFS                                     ENTO
326600070223     C***                TIME                    ORARIO            6 0                      NTO
326700070223     C***                MOVEL     ORARIO        BRVHMS
326800070223     C***                time                    BRVHMS
326900070223     c* millisecondi immissione
327000070223     c                   time                    wtimez
327100070223     c                   extrct    wtimez:*ms    millisec          6 0
327200070223     c                   movel     millisec      brvmis
327300070223     c* data immissione
327400070223     c                   movel     wtimez        dataiso
327500070223     c                   movel     dataiso       brvdfs
327600070223     c* ora immissione
327700070223     c                   move      wtimez        oraiso
327800070223     c                   movel     oraiso        brvhms
327900070223     c                   movel     knmus         brvpru
328000070223     C                   CLEAR                   BRVFTR
328100070223     C                   Z-ADD     *ZERO         BRVDTR                         DATA TRASMISSIONE
328200070223     C                   Z-ADD     *ZERO         BRVHTR                                     SIONE
328300070223     C**                 Z-ADD     WPOSPU        BRVPES
328400070223     C                   movel     v2cfgs        BRVPES
328500070223     c                   ENDSR
328600070223     C**************************************************************************
328700070223     C* RIempimento campi di FNBRV
328800070223     C**************************************************************************
328900070223     c     AggioFINALE   BEGSR
329000070223      * Se è stato modificato il volume o il peso della spunta devo anche azzerare
329100070223      * i campi di FNART in modo che fnls45r rielabori i dati per aggiornare la bolla
329200120615     c**                 If        V2hVuc <> V2cVuc or
329300120615     c**                           V2hPuc <> V2cPuc
329400120615     c**   Kart          Chain     Fnart27l
329500120615     c**                 If        %Found (Fnart27l)
329600120615     c**                 Clear                   bltVuc
329700120615     c**                 Clear                   bltFvc
329800120615     c**                 Clear                   bltPuc
329900120615     c**                 Clear                   bltFpc
330000120615     c**                 Update    Fnart000
330100120615     c**                 EndIf
330200120615     c**                 EndIf
330300070223     C* ABILITO BOLLE
330400070223    5C     PARFLG        IFNE      '2'
330500070223     C     PARFLG        ANDNE     '3'
330600070223     C     PARFLG        ANDNE     '4'
330700070223     C*
330800070223     C     PARFLG        OREQ      '2'
330900070223     C     dls45FLG      ANDNE     ' '
331000070223     C     PARFLG        OREQ      '3'
331100070223     C     dls45FLG      ANDNE     ' '
331200070223     C     PARFLG        OREQ      '4'
331300070223     C     dls45FLG      ANDNE     ' '
331400070223     C                   EXSR      ABIBOL
331500070223    5C                   END
331600070223     c                   ENDSR
331700941214     C**************************************************************************
331800941229     C* RILEVO IL FOGLIO VIAGGIO PARTENZE/ARRIVI DELLA MIA AREA
331900941214     C**************************************************************************
332000941214     C     RILFOG        BEGSR
332100941214     C*
332200941214     C                   Z-ADD     *ZERO         WDFV                           DATA FOGLIO
332300000118     C                   MOVE      *BLANKS       WFILE             3
332400941214     C*
332500941214     C* ARRIVI O PARTENZE ?
332600000118    1C     KNPG          IFEQ      1                                            1=PARTENZE
332700941214     C*
332800020808     C     K02FGV01      chain     FNFGV01L                           99
332900031121     C     *IN99         IFEQ      *Off
333000000118     C  N99              MOVEL     'FGV'         WFILE                          DI COMODO
333100941214     C  N99              Z-ADD     FGVDFV        WDFV                           DATA FOGLIO
333200000519     C*
333300000519     C* ESTENSIONE
333400020808     C     K02FGV01      chain     FNFGW01L                           34
333500070131    2C     *IN34         IFEQ      *ON
333600000519     C     FGWATB        ORNE      ' '
333700000519     C                   CLEAR                   FGWFF3
333800000519     C                   CLEAR                   FGWFF4
333900000519     C                   CLEAR                   FGWFL3
334000000519     C                   CLEAR                   FGWFL4
334100000519     C                   CLEAR                   FGWFGS
334200000519     C                   ENDIF
334300000519     C**
334400000118   X2C                   ELSE
334500000118     C     KFVA1         CHAIN     FNFVA01L                           99
334600000118     C  N99              MOVEL     'FVA'         WFILE                          DI COMODO
334700031121     C  N99              Z-ADD     FvaDFV        WDFV                           DATA FOGLIO
334800000519     C     KFVA1         CHAIN     FNFWA01L                           34
334900000519     C*
335000000519     C     *IN34         IFEQ      *ON
335100000519     C     FWAATB        ORNE      ' '
335200000519     C                   CLEAR                   FWAFF3
335300000519     C                   CLEAR                   FWAFF4
335400000519     C                   CLEAR                   FWAFL3
335500000519     C                   CLEAR                   FWAFL4
335600000519     C                   CLEAR                   FWAFGS
335700000519     C                   ENDIF
335800000118    2C                   ENDIF
335900941214     C*
336000000118   X1C                   ELSE                                                   ARRIVI
336100941214     C*
336200941229     C     KFVV1         CHAIN     FNFVV01L                           99
336300941229     C  N99              Z-ADD     FVVDFV        WDFV                           DATA FOGLIO
336400000118    1C                   ENDIF
336500941214     C*
336600941214     C                   ENDSR
336700940727     C**************************************************************************
336800940727     C* ELIMINO DATE E ANOMALIE DI SPUNTE CANCELLATE
336900940727     C**************************************************************************
337000921020     C     UPDBOL        BEGSR
337100940727     C*
337200000118     C                   Z-ADD     BRVNFV        WRVNFV
337300921020     C                   MOVEL     PARAM2        KPJBU
337400940727     C                   CALL      'FNLSA8R'
337500921020     C                   PARM                    KPJBA
337600930609     C                   MOVEL     'C'           CFSA8             1
337700921020     C*
337800921020     C                   MOVEL     KPJBU         PARAM2
337900921020     C* ERRORI DURANTE LA REGISTRAZIONE
338000921020     C     PARFLG        IFEQ      'E'
338100921020     C                   SETON                                        9531
338200921020     C                   END
338300921020     C*
338400921020     C                   ENDSR
338500970116     C**************************************************************************
338600070220     C* RICHIAMO FNLS45R PER VEDERE SE CREARE LA 145 - 146
338700970116     C**************************************************************************
338800970116     C     AN1456        BEGSR
338900031120     c                   clear                   savpes
339000070116     C     KART          SETLL     fnbrv07L
339100070116     C     KART          READE     fnbrv07L                               33
339200031120     c* determino ter-arr di lna (siccome il collo è sempre lo stesso
339300031120     c* do per scontato che il suo ter-arr sia sempre lo stesso e quindi
339400031120     c* lo determino solo la prima volta utilizzando la data foglio della
339500031120     c* spunta che annullo
339600031120     c     *in33         ifeq      *off
339700031120     c                   clear                   dslv55
339800031120     c                   movel     'A'           D55TPT
339900031120     c                   z-add     brvlna        D55LIN
340000031120     c                   z-add     brvlnp        D55LNP
340100031120     c                   z-add     v2hdfv        d55drf
340200031120     c                   call      'FNLV55R'
340300031120     c                   parm                    dslv55
340400031120     c                   if        D55err = *blanks
340500031120     c                   z-add     D55TFA        tfalna
340600031120     c                   endif
340700031121     c                   endif
340800970116     C     *IN33         DOWEQ     *OFF
340900970117     C*
341000031120     c* Leggo solo le spunte effettuate da p.o. appartenenti alla stessa are
341100031120     c* di arrivo di brvlna
341200031120     c     brvpes        ifne      tfalna
341300031120     c     brvpes        andne     savpes
341400031120     c                   clear                   dslv55
341500031120     c                   movel     'A'           d55tpt
341600031120     c                   z-add     brvpes        d55lin
341700031120     c                   z-add     brvlnp        d55lnp
341800031120     c                   z-add     v2hdfv        d55drf
341900031120     c                   call      'FNLV55R'
342000031120     c                   parm                    dslv55
342100031120     c                   z-add     d55tfa        tfapes
342200031120     c                   z-add     d55tfa        savpes
342300031120     c                   endif
342400031120     c     brvpes        ifeq      tfalna
342500031120     c     tfapes        oreq      tfalna
342600101201     c
342700070220     c*** richiamo nuovo pgm di caricamento spunte FNLs45R
342800070220     c                   movel     brvpes        dls45pes
342900070220     c                   movel     'A'           dls45flg2
343000070221     c*
343100070221     C                   eval      d_brvnvr= BrvNvr
343200070221     C                   eval      d_brvnpg= BrvNPG
343300070221     C                   eval      D_brvnfv= BrvNFV
343400070221     C                   eval      d_brvlnp= BrvLNP
343500070221     C                   eval      d_brvlna= BrvLNA
343600070221     C                   eval      d_brvnrs= BrvNRS
343700070221     C                   eval      d_brvnsc= BrvNSC
343800070221     C                   eval      d_brvcan= BrvCAN
343900070221     C                   eval      d_brvnps= BrvNPS
344000070221     C                   eval      d_brvvuc= BrvVUC
344100070221     C                   eval      d_brvpuc= BrvPUC
344200070221     C                   eval      d_brvdfs= BrvDFS
344300070221     C                   eval      d_brvhms= BrvHMS
344400070221     C                   eval      d_brvmis= Brvmis
344500070221     C                   eval      d_brvznc= BrvZNC
344600070221     c                   eval      d_brvpru= brvpru
344700070221     c                   eval      d_brvfgs= brvfgs
344800070221     c                   eval      d_brvtap= brvtap
344900070221     c                   eval      d_brvpes= brvpes
345000070221     c                   eval      d_brvatr= brvatr
345100070221     c                   eval      d_brvfle= brvfle
345200070221     c                   eval      d_brvftr= brvftr
345300070221     c                   eval      d_brvdtr= brvdtr
345400070221     c                   eval      d_brvhtr= brvhtr
345500070221     c                   eval      d_brvtsu= brvtsu
345600101202     c                   eval      d_brvdcs= brvdcs
345700101202     c                   eval      d_brvhcs= brvhcs
345800070221     c
345900070220     c                   call      'FNLS45R'
346000070220     c                   parm                    fnls45ds
346100070220     c                   parm                    DSrecbrv
346200070220     c
346300070220     C* FLAG PER  CHIUDERE POI I pgm
346400070220     C                   MOVEL     'C'           CFS44             1
346500970117     C                   ENDIF
346600970116     C*
346700070116     C     KART          READE     fnbrv07L                               33
346800970116     C                   ENDDO
346900970116     C*
347000070220     C                   CLEAR                   dls45FLG2
347100970116     C                   ENDSR
347200101201     C**************************************************************************
347300101201     C* AGGIORNO DATE E SE NECESSAIO ABILITO BOLLE
347400101201     C**************************************************************************
347500101201     C     ABIBOL        BEGSR
347600070220     c*** richiamo nuovo pgm di caricamento spunte FNLs45R
347700070220     c                   movel     brvpes        dls45pes
347800070221     c*
347900070221     C                   eval      d_brvnvr= BrvNvr
348000070221     C                   eval      d_brvnpg= BrvNPG
348100070221     C                   eval      D_brvnfv= BrvNFV
348200070221     C                   eval      d_brvlnp= BrvLNP
348300070221     C                   eval      d_brvlna= BrvLNA
348400070221     C                   eval      d_brvnrs= BrvNRS
348500070221     C                   eval      d_brvnsc= BrvNSC
348600070221     C                   eval      d_brvcan= BrvCAN
348700070221     C                   eval      d_brvnps= BrvNPS
348800070221     C                   eval      d_brvvuc= BrvVUC
348900070221     C                   eval      d_brvpuc= BrvPUC
349000070221     C                   eval      d_brvdfs= BrvDFS
349100070221     C                   eval      d_brvhms= BrvHMS
349200070221     C                   eval      d_brvmis= Brvmis
349300070221     C                   eval      d_brvznc= BrvZNC
349400070221     c                   eval      d_brvpru= brvpru
349500070221     c                   eval      d_brvfgs= brvfgs
349600070221     c                   eval      d_brvtap= brvtap
349700070221     c                   eval      d_brvpes= brvpes
349800070221     c                   eval      d_brvatr= brvatr
349900070221     c                   eval      d_brvfle= brvfle
350000070221     c                   eval      d_brvftr= brvftr
350100070221     c                   eval      d_brvdtr= brvdtr
350200070221     c                   eval      d_brvhtr= brvhtr
350300070221     c                   eval      d_brvtsu= brvtsu
350400101202     c                   eval      d_brvdcs= brvdcs
350500101202     c                   eval      d_brvhcs= brvhcs
350600070221     c
350700070220     c                   call      'FNLS45R'
350800070220     c                   parm                    fnls45ds
350900070220     c                   parm                    DSrecbrv
351000921020     C*
351100070220     C*  ERRORE: BOLLA  ALLOCATA
351200070220     C     dls45FLG      IFEQ      'E'
351300950711     C                   SETON                                        9531
351400921020     C                   END
351500950711     C*  ERRORE: BOLLA  GIA' CONSEGNATA
351600070220     C     dls45FLG      IFEQ      'N'
351700950711     C                   SETON                                        9631
351800950711     C                   END
351900950711     C* FLAG PER  CHIUDERE POI I FILE DEL PGM
352000930521     C                   MOVEL     'C'           CFS44             1
352100921020     C                   ENDSR
352200940727     C**************************************************************************
352300940727     C* PRENDO ZONA DA BOLLA
352400940727     C**************************************************************************
352500920812     C     ZONA          BEGSR
352600021028     c* Devo solo prendere la zona dalla bolla per cui posso leggere la boll
352700021028     c*  in assoluto tanto non mi interessa se è mio o no.
352800940727     C*
352900920812     C                   Z-ADD     0             BRVZNC
353000101201     C     KART          CHAIN     FNBLT27L                           34
353100101201     C  N34KBLP          CHAIN     FNBLP01L                           34
353200150623    1c                   if        not *in34
353300101201     C                   MOVEL     BLPZNC        BRVZNC
353400150623   x1C                   else
353500120615     C     KART          CHAIN     FNART27L                           34
353600101201     C  N34Kblp          CHAIN     FNARB01L                           34
353700150623    2c                   if        not *in34
353800101201     C                   MOVEL     blpZNC        BRVZNC
353900101201     C                   ENDif
354000101201     C                   ENDif
354100921020     C*
354200920812     C                   ENDSR
354300960523     C**************************************************************************
354400961121     C*    INTERROGAZIONE ANOMALIE / EVENTI SEGNACOLLI
354500960523     C**************************************************************************
354600960523     C     INTANM        BEGSR
354700031119     c                   setoff                                       90
354800960523     C                   READC     LR04S02                                31
354900960523     C*
355000960523    1C     *IN31         DOWEQ     *OFF
355100960523     C* Imposto indicatori memorizzati nei campi hidden
355200960523     C                   EXSR      SRIND1
355300960523    2C     RECORD        IFNE      *ZEROS
355400961121     C     V2CANM        ANDNE     ' '
355500960523     C     V2CNFV        ORNE      0
355600961121     C     V2CANM        ANDNE     ' '
355700960523    3C     V2CNSC        IFGT      *ZEROS
355800961122     C* INTERROGAZIONE ANOMALIE (SOLO SE PGM NON RICHIAMATO)
355900961122    4C  N65V2CANM        IFEQ      'I'
356000960523     C                   CLEAR                   DSLR44
356100960523     C                   MOVE      V2CLNP        D44FIL
356200960523     C                   MOVE      V2CLNA        D44LNA
356300960523     C                   MOVE      V2CNRS        D44NRS
356400960523     C                   MOVE      V2CNSC        D44SCN
356500960523     C                   MOVE      'I'           D44MOD
356600960523     C                   MOVEL     DSLR44        KPJBU
356700960523     C                   CALL      'FNLR44R'
356800960523     C                   PARM                    KPJBA
356900961121    4C                   ENDIF
357000070220     C* INTERROGAZIONE EVENTI SEGNACOLLI esteri
357100031119    4C     V2CANM        IFEQ      'E'
357200031119     c                   z-add     v2clna        num3
357300031119     c                   exsr      cazorg
357400031119     C     §OGNTW        IFEQ      'FED'
357500031119     C     §OGNTW        OREQ      'EEX'
357600060322     C     §OGNTW        OREQ      'DPD'
357700961121     C                   CLEAR                   DSLR45
357800961121     C                   MOVE      V2CLNP        D45FIL
357900961121     C                   MOVE      V2CLNA        D45LNA
358000961121     C                   MOVE      V2CNRS        D45NRS
358100961121     C                   MOVE      V2CNSC        D45SCN
358200961121     C                   MOVEL     DSLR45        KPJBU
358300961121     C                   CALL      'FNLR45R'
358400961121     C                   PARM                    KPJBA
358500031119     c                   else
358600031119     c* errore --> lna che non prevede eventi segnacollo
358700031119     c                   seton                                        789031
358800031119     c                   endif
358900961121    4C                   ENDIF
359000140305     C* INTERROGAZIONE spunte da pc vdl
359100140305    4C     V2CANM        IFEQ      'V'
359200140305     c                   clear                   fims40ds
359300140305     c                   eval      i40pes=v2cpes
359400140305     c                   eval      i40lnp=v2clnp
359500140305     c                   eval      i40lna=v2clna
359600140305     c                   eval      i40nrs=v2cnrs
359700140305     c                   eval      i40nsc=v2cnsc
359800140305     c                   eval      i40dfs=v2cdfs
359900140305     c                   call      'FIMS40R'
360000140305     C                   PARM                    kpjba
360100140305     C                   PARM                    fims40ds
360200140305     c                   endif
360300960523    3C                   END
360400961121     C*
360500960523     C                   CLEAR                   V2CANM
360600960523     C                   Z-ADD     NRR2          REC2
360700960523    2C                   END
360800960523     C*
360900960523     C                   SETON                                        36
361000960523     C                   UPDATE    LR04S02
361100960523     C                   SETOFF                                       36
361200960523     C*
361300031119     C  n31              READC     LR04S02                                31
361400960523    1C                   ENDDO
361500960523     C                   ENDSR
361600940727     C**************************************************************************
361700940727     C*    PULISCO CAMPI PER INSERIMENTO DAL AL
361800940727     C**************************************************************************
361900920323     C     PULDAL        BEGSR
362000940727     C*
362100941201     C                   MOVEL     V1CNPG        V3CNPG
362200941201     C                   Z-ADD     VI1NFV        V3CNFV
362300941201     C                   Z-ADD     V1CLNP        V3CLNP
362400941201     C                   MOVEL     *BLANKS       V3DLNP
362500941201     C                   Z-ADD     V1CLNA        V3CLNA
362600941201     C                   MOVEL     *BLANKS       V3DLNA
362700941201     C                   MOVEL     *BLANKS       V3DSPG
362800941201     C                   Z-ADD     0             V3CNRS
362900941201     C                   Z-ADD     0             V3CNSD
363000941201     C                   Z-ADD     0             V3CNSA
363100941201     C                   Z-ADD     0             V3CNUM
363200940727     C*
363300920323     C                   ENDSR
363400940727     C**************************************************************************
363500940727     C*    CREO SPUNTE DAL AL
363600940727     C**************************************************************************
363700920323     C     AGGDAL        BEGSR
363800940727     C*
363900920323     C* RICERCO PRIMO POSTO VUOTO PER INSERIMENTO IN SUBFILE
364000940727     C                   Z-ADD     SAVNRR        NRR2
364100920323     C                   Z-ADD     0             RECORD
364200920323     C                   SETON                                        36
364300950705     C                   SETOFF                                       24
364400941201     C     NRR2          CHAIN     LR04S02                            31
364500920323     C*
364600920323     C     RECORD        DOWEQ     *ZEROS
364700940727     C                   SUB       1             NRR2
364800940727     C     NRR2          IFEQ      0
364900920323     C                   SETON                                        31
365000920323     C                   END
365100941201     C  N31NRR2          CHAIN     LR04S02                            31
365200920323     C  N31              END
365300000121     C* DEVO AGGIORNARE L'ULTIMO RECORD LETTO SE NRR2 > 0 PERCHE'
365400000121     C* SE E' DI NUOVA IMMISSIONE O E' STATO MANUTENZIONATO
365500000121     C* DEVE POTER ESSERE RILETTO CON READE DALLA ROUTINE REGIS
365600000121     C     NRR2          IFGT      0
365700000121     C                   UPDATE    LR04S02
365800000121     C                   END
365900920323     C*
366000021206     C                   SETON                                        2125
366100000119     C                   SETON                                        947172
366200960612     C                   SETOFF                                       246323
366300990428     C                   SETOFF                                       86
366400950705     C                   CLEAR                   V2CSE                          SEGNACOLLO ESISTENTE
366500960612     C                   CLEAR                   V2HESI                         SEGNACOLLO ESISTENTE
366600950705     C                   MOVEL     ' '           V2CCAN
366700950705     C                   MOVEL     *BLANKS       V2DCAN
366800950705     C                   MOVEL     ' '           V2CANN
366900950705     C                   MOVEL     '  '          V2CNPS
367000950705     C                   MOVEL     '  '          V2CZNC
367100950705     C                   Z-ADD     0             V2CDFV                         GG/MM
367200950706     C                   Z-ADD     0             V2HDFV                         AAAA/MM/GG
367300950705     C                   Z-ADD     0             V2CNRR
367400070228     C                   Z-ADD     *ZERO         V2HDCS                         DATA CARICAMENTO
367500070228     C                   Z-ADD     *ZERO         V2CDCS                         DATA CARICAMENTO
367600950705     C                   Z-ADD     *ZERO         V2CHCS                         ORA CARICAMENTO
367700000119     C                   Z-ADD     *ZERO         V2CDFS                         DATA IMM/VARENTO
367701160729     C                   Z-ADD     *ZERO         V2hDFS                         DATA IMM/VARENTO
367800000119     C                   Z-ADD     *ZERO         V2CHMS                         ORA IMM/VARINTO
367900000119     C                   Z-ADD     *ZERO         V2CDTR                         DATA TRASMISNTO
368000000119     C                   Z-ADD     *ZERO         V2CHTR                         ORA TRASMISSENTO
368100000119     C                   MOVE      *BLANKS       V2CFTR                                     ENTO
368200021016     C***                Z-ADD     WPOSPU        V2CPES
368300021016     C                   movel     v1cfgs        V2CPES
368400950705     C                   MOVEL     *BLANKS       V2DATR                         TRANSITO
368500950705     C                   MOVEL     *BLANKS       V2CABB                         ABBINAMENTO
368600031121     C***                Z-ADD     *ZERO         V2CNPA                         CATEG.FOG.ABB.
368700031121     C***                Z-ADD     *ZERO         V2CNFA                         FOGLIO ABBINAATO
368800950705     C                   Z-ADD     *ZERO         V2CVUC                         VOLUME
368900990907     C                   Z-ADD     *ZERO         V2CPUC                         PESO
369000950705     C                   CLEAR                   V2COLD
369100960522     C                   CLEAR                   V2CANM
369200920323     C*
369300941201     C     V3CNSD        DOWLE     V3CNSA
369400940727     C                   ADD       1             NRR2
369500941201     C     NRR2          CHAIN     LR04S02                            31
369600941213     C                   MOVEL     V1CFGS        V2CFGS                         FIL.GESTIONE
369700941213     C                   MOVEL     V3CNPG        V2CNPG
369800941213     C                   Z-ADD     V3CNFV        V2CNFV
369900941213     C                   Z-ADD     V3CLNP        V2CLNP
370000941213     C                   MOVEL     V3DLNP        V2DLNP
370100941213     C                   Z-ADD     V3CLNA        V2CLNA
370200941213     C                   MOVEL     V3DLNA        V2DLNA
370300941213     C                   Z-ADD     V3CNRS        V2CNRS
370400941213     C                   Z-ADD     V3CNSD        V2CNSC
370500960531     C                   CLEAR                   V2HRCD
370600960612     C                   EXSR      SRIND
370700941201     C  N31              UPDATE    LR04S02
370800941201     C   31              WRITE     LR04S02
370900941201     C                   ADD       1             V3CNSD
371000950705     C                   ENDDO
371100920323     C* IMPOSTO CAMPI DI DUP
371200941213     C                   Z-ADD     V2CNPG        DUPNPG
371300941213     C                   Z-ADD     V2CNFV        DUPNFV
371400941213     C                   Z-ADD     V2CLNP        DUPLNP
371500941213     C                   Z-ADD     V2CLNA        DUPLNA
371600941213     C                   Z-ADD     V2CNRS        DUPNRS
371700941213     C                   Z-ADD     V2CNSC        DUPNSC
371800920323     C                   MOVEL     ' '           DUPCAN
371900920323     C*
372000920323     C* SE NON E' MULTIPLO DI 16 FINISCO DI INIZIALIZZARE
372100990920     C   31NRR2          DIV       15            DIFF
372200920323     C   31              MVR                     RESTO             5 0
372300920323     C*
372400920323     C   31RESTO         IFNE      0
372500990920     C     RESTO         DOWLT     15
372600941213     C                   MOVEL     *BLANKS       V2CANN
372700941213     C                   MOVEL     V1CNPG        V2CNPG
372800941213     C                   MOVEL     V1CFGS        V2CFGS
372900941213     C                   Z-ADD     0             V2CNFV
373000941214     C                   Z-ADD     0             V2CDFV                         GG/MM
373100950706     C                   Z-ADD     0             V2HDFV                         AAAA/MM/GG
373200941213     C                   Z-ADD     0             V2CLNP
373300941213     C                   MOVEL     *BLANKS       V2DSPG
373400941213     C                   MOVEL     *BLANKS       V2DLNP
373500941213     C                   Z-ADD     0             V2CLNA
373600941213     C                   MOVEL     *BLANKS       V2DLNA
373700941213     C                   Z-ADD     0             V2CNRS
373800941213     C                   Z-ADD     0             V2CNSC
373900950207     C                   CLEAR                   V2CSE                          SEGNACOLLO ESISTENTE
374000950705     C                   CLEAR                   V2HNPG
374100941213     C                   MOVEL     '  '          V2CZNC
374200941213     C                   MOVEL     '  '          V2CNPS
374300941213     C                   MOVEL     *BLANKS       V2CCAN
374400941213     C                   MOVEL     *BLANKS       V2DCAN
374500070228     C                   Z-ADD     *ZERO         V2HDCS                         DATA CARICAMENTO
374600070228     C                   Z-ADD     *ZERO         V2CDCS                         DATA CARICAMENTO
374700941213     C                   Z-ADD     *ZERO         V2CHCS                         ORA CARICAMENTO
374800000119     C                   Z-ADD     *ZERO         V2CDFS                                     ENTO
374801160729     C                   Z-ADD     *ZERO         V2hDFS                                     ENTO
374900000119     C                   Z-ADD     *ZERO         V2CHMS                                     NTO
375000070131     C                   Z-ADD     *ZERO         V2Cmis                         ORA IMM/VARIAZ.
375100000119     C                   Z-ADD     *ZERO         V2CDTR                                     ENTO
375200000119     C                   Z-ADD     *ZERO         V2CHTR                                     NTO
375300070131     C                   CLEAR                   V2CFTR
375400070131     C                   CLEAR                   V2Cpru
375500070131     C                   CLEAR                   V2Ctap
375600021016     C                   movel     v1cfgs        V2CPES
375700941213     C                   MOVEL     *BLANKS       V2DATR                         TRANSITO
375800941213     C                   MOVEL     *BLANKS       V2CABB                         ABBINAMENTO
375900941213     C                   Z-ADD     *ZERO         V2CVUC                         VOLUME
376000990907     C                   Z-ADD     *ZERO         V2CPUC                         PESO
376100021127     c                   Clear                   V2hVuc
376200021127     c                   Clear                   V2hPuc
376300941213     C                   Z-ADD     0             V2CNRR
376400950207     C                   CLEAR                   V2COLD
376500960522     C                   CLEAR                   V2CANM
376600960612     C                   CLEAR                   V2HESI
376700960522     C                   MOVE      RCDSFL        V2HRCD
376800960405     C                   EXSR      SRIND
376900940727     C                   ADD       1             NRR2
377000920323     C                   ADD       1             RESTO
377100941201     C                   WRITE     LR04S02
377200920323     C                   END
377300920323     C                   END
377400920323     C*
377500940727     C   31              Z-ADD     NRR2          SAVNRR
377600940727     C                   Z-ADD     NRR2          REC2
377700920323     C                   SETOFF                                       3621
377800940727     C*
377900920323     C                   ENDSR
378000940727     C**************************************************************************
378100940727     C* CARICO L6
378200940727     C**************************************************************************
378300920807     C     CARL6         BEGSR
378400940727     C*
378500950704     C                   CLEAR                   DSUL06
378600950704     C                   MOVE      '£6'          D06COD
378700950704     C                   MOVEL     V1CFGS        D06KEY
378800950704     C                   MOVEL     'L'           D06TLA
378900950704     C                   MOVEL     DSUL06        KPJBU
379000950704     C                   CALL      'TRUL06R'
379100950704     C                   PARM                    KPJBA
379200950704     C                   MOVEL     KPJBU         DSUL06
379300950704     C                   MOVEA     LIN           L6
379400950704     C                   ENDSR
379500940727     C**************************************************************************
379600940727     C* CHAINSU AZORG
379700940727     C**************************************************************************
379800930707     C     CAZORG        BEGSR
379900940727     C*
380000930707     C     NUM3          CHAIN     AZORG01L                           39
380100930707     C  N39ORGFAG        IFNE      'F'
380200930707     C     ORGFAG        ANDNE     'A'
380300930707     C                   SETON                                        39
380400930707     C                   END
380500020207     C                   MOVEL     ORGDE3        OG143
380600940727     C*
380700930707     C                   ENDSR
380800960405     C**************************************************************************
380900960405     C* MEMORIZZAZIONE INDICATORI
381000960405     C**************************************************************************
381100960405     C     SRIND         BEGSR
381200080318     C                   MOVE      *IN19         V2HI19
381300080318     C                   MOVE      *IN21         V2HI21
381400960405     C                   MOVE      *IN23         V2HI23
381500960405     C                   MOVE      *IN24         V2HI24
381600960405     C                   MOVE      *IN25         V2HI25
381700960405     C                   MOVE      *IN63         V2HI63
381800970131     C                   MOVE      *IN66         V2HI66
381900090416     C                   MOVE      *IN70         V2HI70
382000990428     C                   MOVE      *IN86         V2HI86
382100960405     C                   MOVE      *IN94         V2HI94
382200000119     C                   MOVE      *IN71         V2HI71
382300000119     C                   MOVE      *IN72         V2HI72
382400021206     C                   MOVE      *IN75         V2HI75
382500021206     C                   MOVE      *IN76         V2HI76
382600960405     C                   ENDSR
382700960405     C**************************************************************************
382800960405     C* IMPOSTA INDICATORI VIDEO CON CAMPI HIDDEN
382900960405     C**************************************************************************
383000960405     C     SRIND1        BEGSR
383100080318     C                   MOVE      V2HI19        *IN19
383200080318     C                   MOVE      V2HI21        *IN21
383300960405     C                   MOVE      V2HI23        *IN23
383400960405     C                   MOVE      V2HI24        *IN24
383500960405     C                   MOVE      V2HI25        *IN25
383600960405     C                   MOVE      V2HI63        *IN63
383700970131     C                   MOVE      V2HI66        *IN66
383800090416     C                   MOVE      V2HI70        *IN70
383900990428     C                   MOVE      V2HI86        *IN86
384000960405     C                   MOVE      V2HI94        *IN94
384100000119     C                   MOVE      V2HI71        *IN71
384200000119     C                   MOVE      V2HI72        *IN72
384300021206     C                   MOVE      V2HI75        *IN75
384400021206     C                   MOVE      V2HI76        *IN76
384500960405     C                   ENDSR
384600960829     C**************************************************************************
384700960829     C* RICERCA SE FV DI DEFLUENZA O LOCALE
384800960829     C**************************************************************************
384900960829     C     RICTTR        BEGSR
385000960917     C                   CLEAR                   DSTV
385100000522    1C     WTTR          IFNE      *BLANKS
385200960829     C* Se non è locale verifico se è una defluenza
385300960829    2C     WTTR          IFNE      'L'
385400960829     C                   MOVEL     'TV'          KCOD
385500960829     C                   MOVEL(P)  WTTR          KKEY
385600960829     C     KTAB2         CHAIN     TABEL                              34
385700960829    3C     *IN34         IFEQ      *OFF
385800960829     C                   MOVEL     TBLUNI        DSTV
385900960829    4C     §TVDEF        IFEQ      'S'
386000960829     C                   MOVEL     'D'           V2DSPG
386100960829    4C                   END
386200960829    3C                   END
386300960829   X2C                   ELSE
386400960829     C                   MOVEL     WTTR          V2DSPG
386500960829    2C                   END
386600000522    1C                   ENDIF
386700000519     C*
386800960829     C                   ENDSR
386900980213     C**************************************************************************
387000980213     C* DETERMINA PISTOLA PRIORITARIA
387100980213     C**************************************************************************
387200980213     C     SRNPS         BEGSR
387300980213     C* NPS1/NPS2 SONO I COD.PISTOLA DI CUI DETERMINARE IL PRIORITARIO
387400070228     C* NPSP      E' IL CODICE PISTOLA PRIORITARIO da tenere in spunta
387500980213     C                   CLEAR                   NPSP
387600070215     C                   CLEAR                   tapP
387700980213    1C     NPS1          IFEQ      NPS2
387800980213     C                   Z-ADD     NPS1          NPSP
387900070215     c                   eval      tapp=tap1
388000070228     c                   eval      DCSp=dcs1
388100070228     c                   eval      HCSp=hcs1
388200980213   X1C                   ELSE
388300980213     C* CERCO PRIORITA' NPS1
388400980213     C                   MOVEL     '7J'          KCOD
388500980213     C                   MOVEL(P)  NPS1          KKEY
388600980213     C     KTAB2         CHAIN     TABEL                              31
388700980213     C  N31              MOVEL     TBLUNI        DS7J
388800980213     C   31              MOVEL     999           §7JORD
388900980213     C                   MOVEL     §7JORD        W0030             3 0
389000980213     C* CERCO PRIORITA' NPS2
389100980213     C                   MOVEL(P)  NPS2          KKEY
389200980213     C     KTAB2         CHAIN     TABEL                              31
389300980213     C  N31              MOVEL     TBLUNI        DS7J
389400980213     C   31              MOVEL     999           §7JORD
389500980213    2C     W0030         IFLT      §7JORD
389600980213     C                   MOVEL     NPS1          NPSP
389700070215     C                   MOVEL     tap1          tapP
389800070228     c                   eval      DCSp=dcs1
389900070228     c                   eval      HCSp=hcs1
390000070215   X2C                   ELSE
390100980213     C                   MOVEL     NPS2          NPSP
390200070215     C                   MOVEL     tap2          tapP
390300070228     c                   eval      DCSp=dcs2
390400070228     c                   eval      HCSp=hcs2
390500980213    2C                   END
390600980213    1C                   END
390700980213     C                   ENDSR
390800940727     C**************************************************************************
390900940727     C*   R O U T I N E     I N I Z I A L E
391000940727     C**************************************************************************
391100940727     C     *INZSR        BEGSR
391200940727     C*
391300941130     C     KBRV1         KLIST
391400940727     C                   KFLD                    SAVNPG            1 0
391500940727     C                   KFLD                    VI1NFV
391600101201     C                   KFLD                    kFGSs
391700940727     C     KBRV2         KLIST
391800940727     C                   KFLD                    SAVNPG
391900940727     C                   KFLD                    VI1NFV
392000101201     C                   KFLD                    kFGSs
392100941201     C                   KFLD                    KLN
392200940727     C     KBRV3         KLIST
392300940727     C                   KFLD                    SAVNPG
392400940727     C                   KFLD                    VI1NFV
392500101201     C                   KFLD                    kFGSs
392600101201     C                   KFLD                    kLNPs
392700101201     C                   KFLD                    kLNAs
392800940727     C     KBRV31        KLIST
392900940727     C                   KFLD                    SAVNPG
393000940727     C                   KFLD                    VI1NFV
393100110202     C                   KFLD                    kFGSs
393200101201     C                   KFLD                    kLNPs
393300101201     C                   KFLD                    kLNAs
393400101201     C                   KFLD                    kNRSs
393500940727     C     KBRV32        KLIST
393600940727     C                   KFLD                    SAVNPG
393700940727     C                   KFLD                    VI1NFV
393800101201     C                   KFLD                    kFGSs
393900101201     C                   KFLD                    kLNPs
394000101201     C                   KFLD                    kLNAs
394100101201     C                   KFLD                    kNRSs
394200101201     C                   KFLD                    kNSCs
394300940727     C     KBRV4         KLIST
394400940727     C                   KFLD                    SAVNPG
394500101201     C                   KFLD                    kLNPs
394600940727     C     KBRV5         KLIST
394700940727     C                   KFLD                    SAVNPG
394800101201     C                   KFLD                    kLNPs
394900101201     C                   KFLD                    kLNAs
395000940727     C     KBRV51        KLIST
395100940727     C                   KFLD                    SAVNPG
395200101201     C                   KFLD                    kLNPs
395300101201     C                   KFLD                    kLNAs
395400101201     C                   KFLD                    kNRSs
395500940727     C     KBRV52        KLIST
395600940727     C                   KFLD                    SAVNPG
395700101201     C                   KFLD                    kLNPs
395800101201     C                   KFLD                    kLNAs
395900101201     C                   KFLD                    kNRSs
396000101201     C                   KFLD                    kNSCs
396100940727     C     KBRV6         KLIST
396200941213     C                   KFLD                    V2CNPG
396300941213     C                   KFLD                    V2CLNP
396400941213     C                   KFLD                    V2CLNA
396500941213     C                   KFLD                    V2CNRS
396600941213     C                   KFLD                    V2CNSC
396700941227     C                   KFLD                    KFGS
396800950706     C     KBRV7         KLIST
396900950706     C                   KFLD                    V2CNPG
397000000119     C                   KFLD                    W0060
397100950706     C                   KFLD                    KFGS
397200950706     C                   KFLD                    V2CLNP
397300950706     C                   KFLD                    V2CLNA
397400950706     C                   KFLD                    V2CNRS
397500950706     C                   KFLD                    V2CNSC
397600040303     C     KBRV3a        KLIST
397700040303     C                   KFLD                    V2CNPG
397800040303     C                   KFLD                    V2CLNP
397900040303     C                   KFLD                    V2CLNA
398000040303     C                   KFLD                    V2CNRS
398100040303     C                   KFLD                    V2CNSC
398200040303     C                   KFLD                    Kfgs
398300101201     C     KSEGNAC       KLIST
398400101201     C                   KFLD                    kLNPs
398500101201     C                   KFLD                    kLNAs
398600101201     C                   KFLD                    kNRSs
398700101201     C                   KFLD                    kNSCs
398800940727     C     KART          KLIST
398900941213     C                   KFLD                    V2CLNP
399000941213     C                   KFLD                    V2CLNA
399100941213     C                   KFLD                    V2CNRS
399200941213     C                   KFLD                    V2CNSC
399300971003     C     KBRV          KLIST
399400971003     C                   KFLD                    BRVLNP
399500971003     C                   KFLD                    BRVLNA
399600971003     C                   KFLD                    BRVNRS
399700971003     C                   KFLD                    BRVNSC
399800950118     C     KBLP          KLIST
399900940727     C                   KFLD                    BLTAAS
400000940727     C                   KFLD                    BLTLNP
400100940727     C                   KFLD                    BLTNRS
400200940727     C                   KFLD                    BLTNSP
400300101201     C     KBLPdet       KLIST
400400101201     C                   KFLD                    BLTAAS
400500101201     C                   KFLD                    BLTLNP
400600101201     C                   KFLD                    BLTNRS
400700101201     C                   KFLD                    BLTNSP
400800101201     C                   KFLD                    knscs
400900101129     C     KBLP2         KLIST
401000101129     C                   KFLD                    v1cboan
401100101129     C                   KFLD                    v1cbolp
401200101129     C                   KFLD                    v1cbors
401300101129     C                   KFLD                    v1cbosp
401400101201     C     Kbodet        KLIST
401500101201     C                   KFLD                    v1cboan
401600101201     C                   KFLD                    v1cbolp
401700101201     C                   KFLD                    v1cbors
401800101201     C                   KFLD                    v1cbosp
401900101201     C                   KFLD                    brvnsc
402000941229     C     KFVV1         KLIST                                                  FNFVV01L/05L
402100941201     C                   KFLD                    KNPG                           ..CATEGORIA
402200941201     C                   KFLD                    KNFV                           ..NR.FOGLIO
402300941229     C                   KFLD                    KFGS                           ..FIL.GESTIONE
402400941205     C     KFVA1         KLIST                                                  FNFVA01L
402500941205     C                   KFLD                    KFGS                           ..FIL.GESTIONE
402600941202     C                   KFLD                    KNFV                           ..NR.FOGLIO
402700020808     C*
402800020808     C     K02FGV01      klist                                                  FNFGV01L
402900020808     C                   kfld                    KNFV                           ..NR.FOGLIO
403000020808     C                   kfld                    KFGS                           ..P.O.GESTIONE
403100940727     C* ACCESSO   TABEL
403200940727     C     KTAB1         KLIST
403300940727     C                   KFLD                    CODUT
403400940727     C                   KFLD                    KCOD
403500940727     C     KTAB2         KLIST
403600940727     C                   KFLD                    CODUT
403700940727     C                   KFLD                    KCOD
403800940727     C                   KFLD                    KKEY
403900010622     C*
404000010622     C     KBRVE2        KLIST
404100010622     C                   KFLD                    BRVNPG
404200010622     C                   KFLD                    BRVLNP
404300010622     C                   KFLD                    BRVLNA
404400010622     C                   KFLD                    BRVNRS
404500010622     C                   KFLD                    BRVNSC
404600070710     C     KBRVE1        KLIST
404700070710     C                   KFLD                    BRVNPG
404800070710     C                   KFLD                    BRVNFV
404900070710     C                   KFLD                    BRVFGS
405000070710     C                   KFLD                    BRVLNP
405100070710     C                   KFLD                    BRVLNA
405200070710     C                   KFLD                    BRVNRS
405300070710     C                   KFLD                    BRVNSC
405400010628     C*
405500010628     C     KBRVE3        KLIST
405600010628     C                   KFLD                    DS2NPG
405700010628     C                   KFLD                    DS2LNP
405800010628     C                   KFLD                    DS2LNA
405900010628     C                   KFLD                    DS2NRS
406000010628     C                   KFLD                    DS2NSC
406100010628     C                   KFLD                    DS2NPS
406200101129     C***
406300101129     C* DATE PER INTERROGAZIONE
406400101129     C***
406500101129     C                   TIME                    WTIME            14 0          ORA E DATA
406600101129     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
406700101129     C                   Z-ADD     WDATE         G02DAT
406800101129     C                   Z-ADD     *ZERO         G02INV
406900101129     C                   MOVEL     *BLANK        G02ERR
407000101129     C                   CALL      'XSRDA8'
407100101129     C                   PARM                    WLBDAT
407200101129     C                   Z-ADD     G02INV        WDATEU            8 0
407300101129     C* DATA INIZIALE: UDATE -5
407400101129     C     G02TGI        SUB       5             GIOTGI
407500101129     C*
407600101129     C                   CALL      'XSRGI8'
407700101129     C                   PARM                    WGIDAT
407800101129     C                   Z-ADD     GIODAT        WMENO1            8 0          MENO 5 GIORNI G/M/A
407900101129     C                   Z-ADD     GIOINV        WMENO2            8 0          MENO 5 GIORNI A/M/G
408000101129     C*
408100101129     C*
408200940727     C*
408300940727     C                   ENDSR
408400010622     C**************************************************************************
408500070116     C* MEMORIZZAZIONE SPUNTE DI CONSEGNA DOPPIE SU fnbrvE0F
408600010622     C**************************************************************************
408700010622     C     SRBRVE        BEGSR
408800070710     c* Se ne devo tenere solo una controllo per categoria e segnacollo
408900070710     c                   if        wnst='1'
409000070710     c
409100070116     C     KBRVE2        SETLL     fnbrvE2L                               34
409200010622     C     *IN34         IFEQ      *OFF
409300010622     C* SPUNTA NON ESISTENTE: MEMORIZZO ANCHE LA VECCHIA
409400070116     C                   WRITE     fnbrvE
409500010622     C                   ENDIF
409600070116     C* Salvo campi di fnbrv che potrebbero rimanere invariati
409700070116     C* Li dovrò reimpostare dopo la scrittura di fnbrve per poi
409800010622     C* fare i giusti aggiornamenti
409900070131     c                   clear                   brvnvr
410000010622     C                   MOVE      BRVNPS        SAWNPS
410100070220     C                   MOVE      BRVtap        SAWtap
410200010622     C                   MOVE      BRVCAN        SAWCAN
410300010622     C                   MOVE      BRVPUC        SAWPUC
410400010622     C                   MOVE      BRVVUC        SAWVUC
410500010626     C                   MOVE      BRVDCS        SAWDCS
410600010626     C                   MOVE      BRVHCS        SAWHCS
410700010622     C* IN OGNI CASO MEMORIZZO LA SPUNTA NUOVA
410800010626     C                   MOVEL     V2CFGS        BRVFGS
410900010626     C                   Z-ADD     V2CNFV        BRVNFV
411000010622     C                   MOVEL     V2CCAN        BRVCAN
411100010622     C                   Z-ADD     V2CVUC        BRVVUC
411200010622     C                   Z-ADD     V2CPUC        BRVPUC
411300010629     C                   MOVE      V2CNPS        BRVNPS
411400021016     C                   movel     v2cfgs        BRVPES
411500070215     c* millisecondi  immissione
411600070131     c                   time                    wtimez
411700070131     c                   extrct    wtimez:*ms    millisec          6 0
411800070131     c                   movel     millisec      brvmis
411900070215     c* data immissione
412000070215     c                   movel     wtimez        dataiso
412100070215     c                   movel     dataiso       brvdfs
412200070215     c* ora  immissione
412300070215     c                   move      wtimez        oraiso
412400070215     c                   movel     oraiso        brvhms
412500070215     c
412600090420     c                   movel     knmus         brvpru
412700010626     C                   CLEAR                   BRVDTR
412800010626     C                   CLEAR                   BRVHTR
412900010626     C                   CLEAR                   BRVFTR
413000010626     C                   CLEAR                   BRVDCS
413100010626     C                   CLEAR                   BRVHCS
413200010626     C                   CLEAR                   BRVATR
413300070131     C                   CLEAR                   BRVtsu
413400070131     C                   CLEAR                   BRVtap
413500070116     C                   WRITE     fnbrvE
413600070116     C* Reimposto campi di fnbrv come da lettura record spunte
413700010622     C                   MOVE      SAWNPS        BRVNPS
413800070220     C                   MOVE      SAWtap        BRVtap
413900010622     C                   MOVE      SAWCAN        BRVCAN
414000010622     C                   MOVE      SAWPUC        BRVPUC
414100010622     C                   MOVE      SAWVUC        BRVVUC
414200010626     C                   MOVE      SAWDCS        BRVDCS
414300010626     C                   MOVE      SAWHCS        BRVHCS
414400070710     c
414500070710     c                   else
414600070710     c* Se è per l'anomalia controllo per foglio e segnacollo
414700070710     C     KBRVE1        SETLL     fnbrvE1L                               34
414800070710     C     *IN34         IFEQ      *OFF
414900070710     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
415000070710     C                   WRITE     fnbrvE
415100070710     C                   ENDIF
415200070710     C                   ENDIF
415300070710     c
415400010622     C                   ENDSR
415500010626     C**************************************************************************
415600010626     C* CANCELLAZIONE SPUNTE DOPPIE A FRONTE DI ANNULLAMENTO O MODIFICA
415700010626     C**************************************************************************
415800010626     C     DLBRVE        BEGSR
415900010628     C* DELETO SPUNTE A PARITA' DI PISTOLA E PER DATA FOGLIO >=
416000010628     C                   MOVEL     V2HRCD        DS2RCD
416100010628     C                   MOVE      V2CNPS        DS2NPS
416200070116     C     KBRVE3        SETLL     fnbrvE2L
416300070116     C     KBRVE3        READE     fnbrvE2L                               33
416400010626     C     *IN33         DOWEQ     *OFF
416500010628     C                   CLEAR                   DSLV53
416600010628     C                   MOVEL     'V'           D53TFO
416700010628     C                   Z-ADD     BRVNPG        D53NPG
416800010628     C                   Z-ADD     SIMFEL        D53FEL
416900010628     C                   Z-ADD     BRVNFV        D53NFV
417000010628     C                   Z-ADD     BRVFGS        D53FGS
417100010628     C                   CALL      'FNLV53R'
417200010628     C                   PARM                    DSLV53
417300010628     C     D53ERR        IFEQ      *BLANKS
417400010628     C     D53DFV        ANDGE     V2HDFV
417500070116     C                   DELETE    fnbrvE
417600010628     C                   ENDIF
417700070116     C     KBRVE3        READE     fnbrvE2L                               33
417800010626     C                   ENDDO
417900010626     C                   ENDSR
418000150824     C**************************************************************************
418100150824     C*    Aggiornamenti da tabella BSP
418200150824     C**************************************************************************
418300150824     c     DA_BSP        BEGsr
418400150824     c* Scrivo FNAGB per aggiornare la bolla
418500150824     c                   eval      klnps=brvlnp
418600150824     c                   eval      klnas=brvlna
418700150824     c                   eval      knrss=brvnrs
418800150824     c                   eval      knscs=brvnsc
418900150824     c     ksegnac       chain     fnblt27l
419000150824     c                   if        %found(fnblt27L)
419100150824     C                   CLEAR                   FNAGB000
419200150824     C                   MOVEL     'P'           AGBTBO
419300150824     C                   MOVEL     'CB'          AGBAGB
419400150824     C                   MOVEL     bltAAS        AGBAAS
419500150824     C                   MOVEL     bltLNP        AGBLNP
419600150824     C                   MOVEL     bltNRS        AGBNRS
419700150824     C                   MOVEL     bltNSP        AGBNSP
419800150824     C                   WRITE     FNAGB000                             99
419900150824     c                   endif
420000150824     c
420100150824     c                   ENDSR
420200090416      *****************************************************************
420300090416      *  windos per spunta senza anomalia
420400090416      *****************************************************************
420500090416     C     wind06        BEGSR
420600090416     c* Aggancio fndcd per avere segnacollo
420700090416     c                   clear                   w06nsc
420800090420     c                   movel     v2cfgs        kfgs
420900090420     c                   z-add     v2cnfv        w0060
421000090420     c     kbrv7         chain(n)  fnbrve1l
421100090420    1c                   if        %found(fnbrve1l) and e_brvcan=' '
421200090420     c
421300090416     c* riempo videata
421400090420     c                   eval      w06lnp=e_brvlnp
421500090420     c                   eval      w06lna=e_brvlna
421600090420     c                   eval      w06nrs=e_brvnrs
421700090420     c                   eval      w06nsc=e_brvnsc
421800090420     c                   eval      w06npg=e_brvnpg
421900090420     c                   eval      w06nfv=e_brvnfv
422000090420     c                   eval      w06fgs=e_brvfgs
422100090416     c                   clear                   w06dfv
422200090420     c                   clear                   dslv53
422300090416     c                   eval      d53nfv=w06nfv
422400090416     c                   eval      d53fgs=w06fgs
422500090416     c                   eval      d53npg=w06npg
422600090416     c                   call      'FNLV53R'
422700090420     c                   parm                    dslv53
422800090416     c
422900090420    2c                   if        d53dfv>0
423000090416     c                   eval      Dataiso=%date(d53dfv:*iso)
423100090416     c                   eval      Dataeur=dataiso
423200090416     c                   eval      w06dfv=%dec(Dataeur)
423300090420    2c                   endif
423400090416     c
423500090416     c* spunta senza anomalia
423600090416     c                   eval      Dataiso=%date(e_brvdfs:*iso)
423700090416     c                   eval      Dataeur=dataiso
423800090416     c                   eval      w06dfs=%dec(Dataeur)
423900090416     c
424000090416     c                   eval      Dataiso=%date(e_brvdcs:*iso)
424100090416     c                   eval      Dataeur=dataiso
424200090416     c                   eval      w06dcs=%dec(Dataeur)
424300090416     c
424400090416     c                   eval      w06hms=E_brvhms
424500090416     c                   eval      w06hcs=E_brvhcs
424600090416     c                   eval      w06nps=%editc(E_brvnps:'X')
424700090416     c                   clear                   w06tap
424800090420    2c                   if        e_brvtap<>' '
424900090416     C                   CLEAR                   tibs02ds
425000090416     C                   MOVEL     'C'           T02MOD
425100090416     C                   MOVEL     KNSIF         T02SIF
425200090416     C                   MOVEL     'APS'         T02COD
425300090416     C                   MOVEL     e_BRVTAP      T02KE1
425400090416     C                   CALL      'TIBS02R'
425500090416     C                   PARM                    KPJBA
425600090416     C                   PARM                    tibs02ds
425700090416     c
425800090420    3c                   if        t02err<>' '
425900090416     c                   eval      t02uni=*all'?'
426000090420    3c                   endif
426100090416     c
426200090416     c                   eval      w06tap=e_brvtap+'-'+%subst(t02uni:1:9)
426300090420    2c                   endif
426400090416     c
426500090416     c* spunta con   anomalia
426600090420     c                   eval      w06can='"'+v2ccan+'"'
426700090416     c
426800090420     c                   eval      w06cdfs=v2cdfs
426900090416     c
427000090420     c                   eval      w06cdcs=v2cdcs
427100090416     c
427200090420     c                   eval      w06chms=v2chms
427300090420     c                   eval      w06chcs=v2chcs
427400090420     c                   eval      w06cnps=v2cnps
427500090416     c                   clear                   w06ctap
427600090420    2c                   if        %subst(v2ctap:1:1)<>' '
427700090416     C                   CLEAR                   tibs02ds
427800090416     C                   MOVEL     'C'           T02MOD
427900090416     C                   MOVEL     KNSIF         T02SIF
428000090416     C                   MOVEL     'APS'         T02COD
428100090420     c                   eval      t02ke1=%subst(v2ctap:1:1)
428200090416     C                   CALL      'TIBS02R'
428300090416     C                   PARM                    KPJBA
428400090416     C                   PARM                    tibs02ds
428500090416     c
428600090416     c                   if        t02err<>' '
428700090416     c                   eval      t02uni=*all'?'
428800090416     c                   endif
428900090416     c
429000090420     c                   eval      w06ctap=%subst(v2ctap:1:1)+'-'+
429100090420     c                                     %subst(t02uni:1:9)
429200090420    2c                   endif
429300090416     c
429400090416     c
429500090416     c* se ho trovato visualizzo
429600090420    2c                   if        w06nsc>0
429700090420    3c                   do        *HIVAL
429800090420     c                   exfmt     lr04w06
429900090416     c   KL              leave
430000090420    3c                   enddo
430100090420    2c                   endif
430200090416     c
430300090420     c                   setoff                                       31
430400090420     c                   else
430500090420     c                   seton                                        31
430600090420    1c                   endif
430700090416      *
430800090416     C                   ENDSR
430801160728      *-------------------------------------------------------------------------
430802160728      * ordinamento sfl in base alle richieste
430803160728      *-------------------------------------------------------------------------
430804160728     C     sr_ordina     BEGSR
430805160728      * IMPOSTO ENTRATA AD 'A'
430806160728     C                   EVAL      nrr2   = 0
430807160728      *
430808160728     C                   CLEAR                   QLGSCB
430809160728     C                   CLEAR                   QLGSCB00
430810160728
430821160728     c* ordinamento per data foglio e data/ora immissione
430823160728     ** 3 campi chiave.
430824160728     c* data foglio
430825160728     C                   EVAL      QLGNBRK    = 3
430826160728     C                   EVAL      QLGSP      = 65
430827160728     C                   EVAL      QLGSS      = %SIZE(v2hdfv)
430828160728     C                   EVAL      QLGDT      = carattere
430829160728     C                   EVAL      QLGSO      = Ascendente
430830160728     C                   EVAL      QLGKL(1)   = QLGSKL
430831160728     ** data immissione
430832160728     C                   EVAL      QLGSP      = 269
430834160728     C                   EVAL      QLGSS      = %SIZE(v2hdfs)
430835160728     C                   EVAL      QLGDT      = carattere
430836160728     C                   EVAL      QLGSO      = Ascendente
430837160728     C                   EVAL      QLGKL(2)   = QLGSKL
430838160728     ** ora  immissione
430839160728     C                   EVAL      QLGSP      = 195
430840160728     C                   EVAL      QLGSS      = %SIZE(v2chms)
430841160728     C                   EVAL      QLGDT      = carattere
430842160728     C                   EVAL      QLGSO      = Ascendente
430843160728     C                   EVAL      QLGKL(3)   = QLGSKL
430848160728
430849160728      * Load other sort parameters.
430850160728     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
430851160728     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
430852160728     C                   EVAL      QLGRT     = 8
430853160728     C                   EVAL      QLGOKL    = 80
430854160728     C                   EVAL      QLGLKE    = 16
430855160728     C                   EVAL      QLGLSS    = 290
430856160728
430857160728      * Initialize Sort I/O API fields.
430858160728     C                   EVAL      QLGRL00  = QLGRL
430859160728     C                   EVAL      QLGRC00  = 1
430860160728     C                   CLEAR                   QUSEI
430861160728     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
430862160728
430863160728      * First step - Initialize the sort routine.
430864160728     C                   CALL      'QLGSORT'
430865160728     C                   PARM                    QLGSCB
430866160728     C                   PARM                    NotUsed
430867160728     C                   PARM                    NotUsed
430868160728     C                   PARM                    SizeList
430869160728     C                   PARM                    ReturnSize
430870160728     C                   PARM                    QUSEC
430871160728
430872160728      * Next step - Write records to I/O routine.
430873160728     C                   EVAL      QLGRT00 = Put
430874160728
430875160728     C                   DO        RrnLast       nrr2
430876160728
430877160728     C     nrr2          CHAIN     lr04s02
430878160728     c                   if        not %found
430879160728     c                   leave
430880160728     c                   endif
430881160728     ** Solo le righe con Selected = 'Y' sono riordinate,
430882160728     ** quindi per fare un ordinamento di tutte le righe
430883160728     ** metto 'Y' sempre.
430884160728     C                   EVAL      Selected = 'Y'
430885160728
430886160728     C                   CLEAR                   QUSEI
430887160728     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
430888160728     C                   CALL      'QLGSRTIO'
430889160728     C                   PARM                    QLGSCB00
430890160728     C                   PARM                    SflRcd
430891160728     C                   PARM                    NotUsed
430892160728     C                   PARM                    SizeList
430893160728     C                   PARM                    NotUsed
430894160728     C                   PARM                    QUSEC
430895160728
430896160728     C                   ENDDO
430897160728
430898160728      * Next step - Signal end of input, clear subfile for reload.
430899160728     C                   EVAL      QLGRT00 = EndPut
430900160728     C                   CLEAR                   QUSEI
430901160728     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
430902160728
430903160728     C                   CALL      'QLGSRTIO'
430904160728     C                   PARM                    QLGSCB00
430905160728     C                   PARM                    SflRcd
430906160728     C                   PARM                    NotUsed
430907160728     C                   PARM                    SizeList
430908160728     C                   PARM                    NotUsed
430909160728     C                   PARM                    QUSEC
430910160728
430911160728     c                   seton                                        35
430912160728     C                   WRITE     lr04c02
430913160728     c                   setoff                                       35
430914160728     c                   write     lr04z02
430915160728
430916160728      * Final step - Write the records back to the subfile.
430917160728     C                   EVAL      QLGRT00 = Get
430918160728     C                   DO        RrnLast       nrr2
430919160728     C                   CLEAR                   QUSEI
430920160728     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
430921160728     C                   CALL      'QLGSRTIO'
430922160728     C                   PARM                    QLGSCB00
430923160728     C                   PARM                    NotUsed
430924160728     C                   PARM                    SflRcd
430925160728     C                   PARM                    QLGRL00
430926160728     C                   PARM                    NotUsed
430927160728     C                   PARM                    QUSEC
430928160728      *
430943160728     C                   WRITE     lr04s02
430944160728     C                   ENDDO
430945160728
430946160728      *
430947160728     C                   ENDSR
430948101129**
431000101129Se ricerca o indicato numero foglio, immettere una categoria specifica          01
431100101129Se ricerca o indicato numero foglio, immettere la filiale gestione              02
431200101129Numero foglio errato o annullato                                                03
431300101129Manutenzione delle spunte dei fogli viaggio partenza permessa solo ai terminal  04
431400101129Numero spedizione inesistente                                                   05
431500101129Serie incongruente con quella della spedizione                                  06
431600101129Linea di arrivo incongruente con quella della spedizione                        07
431700101216Linea partenza  segnacollo incongruente con quella della spedizione             08
431800101216Il segnacollo immesso non è relativo alla spedizione indicata                   09

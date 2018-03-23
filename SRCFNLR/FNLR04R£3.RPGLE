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
004600161006     D DES             S              3    DIM(20)                              DESCR TIPO INV
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
008700160822     d  v2cnrr
008800160822     d  v2ctps
008900160822     d  v2crps
009000160822     d  v2cse
009100160822     d  v2cold
009200160822     d  v2hdfv
009300160822     d  v2hnpg
009400160822     d  V2HESI
009500160822     d  V2HI19
009600160822     d  V2HI21
009700160822     d  V2HI23
009800160822     d  V2HI24
009900160822     d  V2HI25
010000160822     d  V2HI63
010100160822     d  V2HI66
010200160822     d  V2HI70
010300160822     d  V2HI86
010400160822     d  V2HI94
010500160822     d  V2HI71
010600160822     d  V2HI72
010700160822     d  V2HI75
010800160822     d  V2HI76
010900160822     d  v2hrcd
011000160822     d  v2hvuc
011100160822     d  v2hpuc
011200160822     d  v2hdcs
011300160822     d  v2cfgs
011400160822     d  v2dspg
011500160822     d  v2cdfv
011600160822     d  v2dlnp
011700160822     d  v2dlna
011800160822     d  v2cznc
011900160822     d  v2dcan
012000160822     d  v2cnps
012100160822     d  v2canm
012200160822     d  v2cpes
012300160822     d  v2cdfs
012400160822     d  v2chms
012500160822     d  v2cmis
012600160822     d  v2cpru
012700160822     d  v2ctap
012800160822     d  v2datr
012900160822     d  v2ctsu
013000160822     d  v2cdcs
013100160822     d  v2chcs
013200160822     d  v2cftr
013300160822     d  v2cdtr
013400160822     d  v2chtr
013500160822     d  v2cabb
013600160822     d  v2hdfs
013700160729     d  Selected                      1A
013800970117     D                 DS
013900000118     D  WOKNFV                 1      6  0
014000000118     D  WOKNPG                 7      7  0
014100000118     D  WOKFGS                 8     10  0
014200000118     D  BRVKEY                 1     10  0
014300921230     D                 DS
014400941202     D  DATA                   1      8  0
014500941202     D  MM                     5      6  0
014600941202     D  GG                     7      8  0
014700941201     D PARAM1          DS
014800941201     D* PARAM1: PARAMETRI IN INPUT/OUTPUT AL PGM
014900941201     D  V1CNFV                 1      5
015000941201     D  V1CLNP                 6      8  0
015100941201     D  V1CLNA                 9     11  0
015200941201     D  V1CNRS                12     13  0
015300941201     D  V1CNCD                14     20  0
015400941201     D  V1CNCA                21     27  0
015500941201     D  V1CNPS                28     29  0
015600941201     D  V1CNPG                30     30
015700101129     D* PA1FLG: '1'=INTERROGAZIONE   con prima videata
015800101129     D*         '2'=manutenzione   senza prima videata
015900101129     D*         '3'=Interrogazione senza prima videata
016000941201     D  PA1FLG                31     31
016100941220     D* PA1PGM: PGM CHIAMANTE (SE RICHIAMATO)
016200941201     D  PA1PGM                32     38
016300941220     D  V1CFGS                39     41  0
016400101129     d* bolla
016500101129     d  v1cbolp               42     44  0
016600101129     d  v1cbors               45     46  0
016700101129     d  v1cbosp               47     53  0
016800101129     d  v1cboan               54     57  0
016900101129     D  PA1ERR                58     58
017000101129     d
017100921020     D PARAM2          DS
017200941201     D* PARAM2: PASSAGGIO AD ANNULLAMENTO ANOMALIE E DATE
017300000118     D  WRVNFV                 2      6  0
017400921230     D  BRVNPG                 7      7  0
017500921020     D  BRVLNP                 8     10  0
017600921020     D  BRVLNA                11     13  0
017700921020     D  BRVNRS                14     15  0
017800921020     D  BRVNSC                16     22  0
017900921020     D  BRVCAN                23     23
018000941201     D  BRVVUC                24     30  6
018100941201     D  PARFLG                31     31
018200941201     D  PARFEL                92     94  0
018300941201     D  BRVNPS                95     96  0
018400941212     D  BRVFGS                97     99  0
018500970530     D  BRVFLE               100    102  0
018600031117     D  BRVpes               103    105  0
018700921020     D* CAMPI CONFRONTO
018800990907     D  BRVCON                 2     23
018900921230     D  BRVCO2                 7     22  0
019000010628     D  BRVREC                 8     22  0
019100951108     D*
019200130520     D* CHIAMATA AL PGM DI INTERROGAZ.FV PARTENZA             -FNLST3R-
019300130520     D fnlst3ds        DS
019400951108     D  PA5FLG                 1      1    INZ('R')
019500951108     D  PA5FL2                 2      2    INZ
019600951108     D  PA5NFV                 3      5P 0 INZ
019700951108     D  PA5DIN                 6     10P 0
019800951108     D  PA5DFI                11     15P 0
019900951108     D  PA5TFV                16     16    INZ
020000000614     D  PA5FGS               242    244  0
020100951108     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
020200951108     D*   SELEZIONATI ---> SE NON UTILIZZATO NON LO DEFINISCO A PGM
020300951108     D***                                 P  17 2410FVA
020400951108     D*
020500950704     D* CHAIAMATA AL PGM DI RICERCA FOGLI PARTENZA ALL'ARRIVO -FNLR26R-
020600950704     D PARAM6          DS
020700950704     D  PA6PGM                 1      7
020800950704     D  PA6LAI                 8     10  0
020900950704     D  PA6FEL                11     13  0
021000950704     D  PA6ABB                14     14
021100950704     D  PA6DSD                15     22  0
021200950704     D  PA6DSA                23     30  0
021300950704     D  PA6FLG                31     31
021400950704     D  PA6FGS                32     34  0
021500971002     D  PA6LAN                35     35    INZ('4')
021600980204     D  PA6MSG                36    113    INZ
021700920807     D DS7N          E DS
021800161006     D DS7B          E DS
021900921229     D DS7J          E DS
022000960829     D DSTV          E DS
022100040625     D DS3E          E DS
022200000522     D OG143         E DS
022300900615     D CNCR80        E DS
022400020808     D UT§DSE0F      E DS
022500960522     D DSLR44        E DS                  EXTNAME(FNLR44DS)
022600961121     D* DS PER FNLR45R - INTERROGAZIONE EVENTI SEGNACOLLI
022700961121     D DSLR45        E DS                  EXTNAME(FNLR45DS)
022800941201     D DSUL06        E DS                  EXTNAME(TRUL06DS)
022900941201     D  LIN                    1     90  0
023000941201     D                                     DIM(30)                              FIL. COMODO
023100971003     D* REPERISCE DATI FOGLI
023200971003     D DSLV53        E DS                  EXTNAME(FNLV53DS)
023300971003     D* CALCOLA TERMINAL DI ARRIVO
023400971003     D DSLV55        E DS                  EXTNAME(FNLV55DS)
023500070116     D*
023600070116     D tibs02ds      E DS
023700070220     d**
023800070220     D FNLS45DS      E DS
023900070221     D DSRECBRV      E DS                  extname(fnbrv00f)  prefix(D_)
024000971003     D**
024100941212     D DSLR01        E DS                  INZ
024200941213     D                 DS
024300941213     D*FILIALE DI SCARICO (QUANDO SI LEGGE FNFGV00F)
024400941213     D  FGVFFV                 1    240
024500941213     D  FGVFF2               241    447
024600000503     D  FGWFF3               448    687
024700000503     D  FGWFF4               688    897
024800000503     D  FFV1                   1    897
024900000503     D                                     DIM(299)                             FIL. FO. VIAGG
025000010628     D                 DS
025100010628     D  DS2NPG                 6      6  0
025200010628     D  DS2LNP                 7      9  0
025300010628     D  DS2LNA                10     12  0
025400010628     D  DS2NRS                13     14  0
025500010628     D  DS2NSC                15     21  0
025600010628     D  DS2RCD                 1     37
025700020808      *
025800020808      * Variabili
025900020808      *
026000020808     D COMDFS          S              8  0 INZ
026100020808     D COMHMS          S              4  0 INZ
026200090420     D savpagnrr       S                   LIKE(pagnrr)
026300090420     D sa2nrr          S                   LIKE(br1nrr)
026400020924     D WLANTW          S                   LIKE(§OGNTW)
026500020808     D SAVNTW          S                   LIKE(§OGNTW)
026600020808     D WSANPA          S                   LIKE(D53NPA)
026700020808     D WSANFA          S                   LIKE(D53NFA)
026800020808     D FGSTFP          S                   LIKE(D55TFP)
026900020808     D KFLE            S                   LIKE(BRVFLE)
027000020808     D KLN             S                   LIKE(BRVLNP)                          LINEA PART/ARR
027100020808     D KNPG            S                   LIKE(FVVNPG)                          CATEGORIA FOGLIO
027200020808     D KNFV            S                   LIKE(FVVNFV)                          NR.FOGLIO
027300020808     D KFGS            S                   LIKE(FVVFGS)                          FIL.GESTIONE
027400020808     D KCOD            S                   LIKE(TBLCOD)                          CODICE TABELLA
027500020808     D KKEY            S                   LIKE(TBLKEY)                          CODICE ELEMENTO
027600020808     D VI1NFV          S                   LIKE(BRVNFV)                          NR.FOGLIO VIAGGIO
027700020808     D COMVUC          S              7  6
027800020808     D COMPUC          S                   LIKE(BRVPUC)
027900020808     D COMZNC          S                   LIKE(BRVZNC)
028000020808     D WDFV            S                   LIKE(FVVDFV)                         DATA FOGLIO (A/M/G)
028100020808     D BRVDFV          S                   LIKE(WDFV)                           DATA FOGLIO (A/M/G)
028200080318     D TSU03           S                   LIKE(BRVTSU)
028300080318     D CAN03           S                   LIKE(BRVCAN)
028400070215     D NPS03           S                   LIKE(BRVNPS)
028500070215     D TAP03           S                   LIKE(BRVTAP)
028600070228     D DCS03           S                   LIKE(BRVdcs)
028700070228     D HCS03           S                   LIKE(BRVhcs)
028800020808     D WNPS            S                   LIKE(BRVNPS)
028900020808     D WPOSPU          S                   LIKE(BRVPES)
029000101202     D WFE0            S                   LIKE(blpTFA)
029100020808     D WFGS            S                   LIKE(V1CFGS)
029200101202     D WTFP            S                   LIKE(blpTFP)
029300101202     D WTFA            S                   LIKE(blpTFA)
029400020808     D NPS1            S                   LIKE(BRVNPS)
029500020808     D NPS2            S                   LIKE(BRVNPS)
029600020808     D NPSP            S                   LIKE(BRVNPS)
029700020808     D SAWNPS          S                   LIKE(BRVNPS)
029800070220     D SAWtap          S                   LIKE(BRVtap)
029900020808     D SAWCAN          S                   LIKE(BRVCAN)
030000020808     D SAWPUC          S                   LIKE(BRVPUC)
030100020808     D SAWVUC          S                   LIKE(BRVVUC)
030200020808     D SAWDCS          S                   LIKE(BRVDCS)
030300020808     D SAWHCS          S                   LIKE(BRVHCS)
030400020808     D DS2NPS          S                   LIKE(BRVNPS)
030500031120     D tfalna          S                   LIKE(d55tfa)
030600031120     D tfapes          S                   LIKE(d55tfa)
030700031120     D savpes          S                   LIKE(d55tfa)
030800070215     D TAPP            S                   LIKE(BRVTAP)
030900070215     D TAP1            S                   LIKE(BRVTAP)
031000070215     D TAP2            S                   LIKE(BRVTAP)
031100070228     D DCSP            S                   LIKE(BRVdcs)
031200070228     D DCS1            S                   LIKE(BRVdcs)
031300070228     D DCS2            S                   LIKE(BRVdcs)
031400070228     D HCSP            S                   LIKE(BRVhcs)
031500070228     D HCS1            S                   LIKE(BRVhcs)
031600070228     D HCS2            S                   LIKE(BRVhcs)
031700101201     D klnps           S                   LIKE(v1clnp)
031800101201     D klnas           S                   LIKE(v1clna)
031900101201     D knrss           S                   LIKE(v1cnrs)
032000101201     D knscs           S                   LIKE(v1cncd)
032100101201     D kfgss           S                   LIKE(v1cfgs)
032200101201     D SAVNCA          S                   LIKE(v1cnca)
032300101203     D SAVNCd          S                   LIKE(v1cnca)
032400101201     D boldsp          S              8  0 inz
032500070710     D WNST            S              1    inz
032600020808     D WRIEM           S              1    inz                                  RIEMETTO TESTATA
032700020808     D NUM3            S              3  0 inz
032800020808     D XI              S              2  0 inz
032900101126     D wlung           S              2  0 inz
033000070215     D wtimez          S               z
033100090420     D dataeur         S               d   datfmt(*eur)
033200090420     D dataiso         S               d   datfmt(*iso)
033300070215     D oraiso          S               t   timfmt(*iso)
033400160823     d wEndRoll        S              1
033500160728     d ord_dta         c                   'F11=ORD x Data'
033600160728     d ord_nfv         c                   'F11=ORDxFoglio'
033700160728     d w_nfv           C                   1
033800160728     d w_dta           C                   2
033900160728     d W_ord           S              1  0
034000160729     D MaxKey          C                    3
034100160728     D Ascendente      C                   1
034200160728     D Discendente     C                   2
034300160728     D Carattere       C                   6
034400160728     D Put             C                   1
034500160728     D EndPut          C                   2
034600160728     D Get             C                   3
034700160728      **********************************************************************
034800160728      *     SizeList   - Size of list
034900160728      *     ReturnSize - Size of list returned by sort API
035000160728      **********************************************************************
035100160728     D NotUsed         S             16A
035200160728     D ReturnSize      S              9B 0
035300160728     D SizeList        S              9B 0
035400160728     D RrnLast         S              5I 0
035500160728      /COPY QSYSINC/QRPGLESRC,QLGSORT
035600160728     D QLGKL                         16    DIM(MaxKey)
035700160728     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
035800160728     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
035900160728     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
036000160728     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
036100160728
036200160728      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
036300160728      /COPY QSYSINC/QRPGLESRC,QUSEC
036400900521     C****************************************************************
036500900521     C*  RIEPILOGO INDICATORI
036600900521     C***************************************************************
036700941202     C* 01    - SELEZIONATO UN SOLO FOGLIO
036800941202     C* 02    - SELEZIONATA UNA SOLA LNP
036900941202     C* 03/06 - INDICANO LE SELEZIONI FATTE NELLA PRIMA VIDEATA
037000900619     C* 07    - PREMUTO AGGIUNTA RIGHE
037100901119     C* 08    - ROLLUP
037200031204     C* 09    - visualizzo campo I/E per int,anomalie italia o estere
037300910930     C* 10/16 - TASTI DI DUPLICA
037400910930     C* 17    - RIESEGUO LETTURA PER CAT FV=9 SE FINITO LEGGERE UNA CAT
037500101201     C* 18    - IMMESSA CATEGORIA specifica
037600080317     C* 19    - Visualizzazione solo per EDP
037700941212     C* 20 ON - INTERROGAZIONE SPUNTE
037800970131     C*    OFF- GESTIONE SPUNTE
037900950711     C* 21 OFF- PROTEGGO IN MANUTENZIONE SE SPUNTA CARICATA DA BRV
038000160823     C* 22    - On: interrogazione per singolo segnacollo oppure per numero spedizione
038100160823     c*         serve per l'attivazione del tasto funzionale f11-Ordinamendto sfl per data
038200970131     C* 23    - SPROTEGGO LA CATEGORIA, MODIFICABILE SE FOGLIO CONSEGNA
038300970131     C*         O FOGLIO PARTENZA IN CATEGORIA 3 E VICEVERSA
038400950704     C* 24    - PROTEGGO IN MANUTENZIONE SE FV CHIUSO
038500960404     C* 25    - OFF--> CAMPO ANNULLAMENTO SPROTETTO
038600920803     C* 26    - INTERROGAZIONE SPUNTE RICHIAMATO
038700941212     C* 27    - PROTEGGO FILIALE IN GESTIONE
038800101126     C* 97    - PROTEGGO COD.ANOMALIA = "E"
038900911014     C* 31/34 - DI COMODO
039000900521     C* 35    - PULIZIA SUBFILE
039100921020     C* 36    - SFLNXTCHG
039200930707     C* 37/38 - LNP E LNA LOCALI
039300930707     C* 39    - CHAIN SU AZORG
039400910320     C* 40/49 - ERRORE
039500960503     C* 51/68 - ERRORE
039600970131     C* 62    - ERRORE SUL FLAG ANNULLAMENTO
039700970131     C* 63    - PROTEGGO CAMPI PER SPUNTA PARTENZA GIA' TRASMESSA
039800970131     C*         O PER SPUNTA DI DEFLUENZA ESTERO
039900960522     C* 65    - PROTEZ.CAMPO X INTERR.ANOMALIE SE RICHIAMATO DA FNLR44R
040000970131     C* 66    - PER FOGLIO DEFL ESTERO SPROTEGGO IL FLAG ANNULLAMENTO
040100970131     C*         CHE SARA' ABILITATO SOTANTO SE SPUNTA MANUALE
040200980213     C* 69    - ERRORE SPUNTA DI DEFLUENZA PER SEGNAC.LOCALE ESTERO SDI
040300090416     C* 70    - visulizza se presente spunta senza anomlia
040400090416     C* 71    - ND DATA ORA CARICAMENTO
040500000118     C* 72    - ND DATA ORA TRAMSISSIONE
040600150817      * 75/76 - Sproteggo volume e peso. Sempre protetti non si possono inserire
040700990428     C* 86    - METTO IN RI LE PISTOLE 88 SU DEFLUENZE EXPORT
040800990428     C* 87/89 - ERRORE IN SFL
040900900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
041000920810     C* 91    - PER RIEMETTERE ANCHE IL VIDEO DEI PIEDI
041100930115     C* 92    - SIMFEL HA CML: VISUALIZZO VOLUME DA CML
041200920811     C* 94    - SPROTEGGO SEGNACOLLI INSERITI MANUALMENTE POSSO VARIARL
041300920811     C*         IN TUTTO
041400921020     C* 95    - ERRORE IN REGISTRAZIONE DATI
041500950711     C* 96    - ERRORE:INSERIMENTO DI SPUNTA PARTENZA DI BOLLA CONSEGN.
041600101126     C* 98    -
041700900521     C*****************************************************************
041800000000     C     *ENTRY        PLIST
041900000000     C                   PARM                    KPJBA
042000941201     C                   MOVEL     KPJBU         PARAM1
042100000119     C*
042200900615     C                   CALL      'X§PARUT'
042300020808     C                   PARM                    UT§DSE0F
042400070116     C                   MOVEL     RAGUT         v1cdsa
042500900615     C                   MOVEL     REC80         CNCR80
042600021016     C                   Z-ADD     SIMFEL        COMTFP            3 0
042700020808     C                   z-add     SIMpou        WPOspu
042800101129     C                   MOVEL     SIMFEL        PARFEL
042900900511     C*---------------------------------------------------------------*
043000900615     C*
043100101202     c                   setoff                                       266520
043200920811     C* 20 ON - INTERROGAZIONE
043300101129     c                   if        pa1flg='1' or pa1flg='3'
043400910419     C                   SETON                                        20
043500910419     C                   END
043600910419     C*
043700101129     C* PGM NON RICHIAMAOT (noem programma vuoto)
043800941201     C     PA1PGM        IFEQ      *BLANKS
043900101129     c* Per sicurezza se pa1flg 3 --> imposto 1
044000101129     c                   if        pa1flg='3'
044100101129     c                   eval      pa1flg='1'
044200101129     c                   endif
044300101129     c
044400941220     C                   Z-ADD     *ZERO         V1CFGS
044500941201     C                   MOVEL     *BLANKS       V1CNFV
044600941201     C                   Z-ADD     0             V1CLNP
044700941201     C                   Z-ADD     0             V1CLNA
044800941201     C                   Z-ADD     99            V1CNRS
044900941201     C                   Z-ADD     0             V1CNCD
045000941201     C                   Z-ADD     0             V1CNCA
045100941201     C                   Z-ADD     99            V1CNPS
045200941201     C                   MOVEL     ' '           V1CNPG
045300101126     c                   clear                   v1cbolp
045400101126     c                   clear                   v1cbors
045500101129     c                   clear                   v1cbosp
045600101126     c                   movel     wdateu        v1cboan
045700101126     c
045800920810     C                   ELSE
045900941201     C* 26 ON = RICHIAMATO DA ALTRO PGM
046000920810     C                   SETON                                        26
046100960903     C* Se pgm richiamato accendo 65 per evitare richiami ricorsivi a
046200960903     C* fnlr44r
046300101129     c                   if        pa1pgm ='FNLR44R'
046400101129     C                   SETON                                        65
046500101129     c                   endif
046600101129     c
046700960522     C                   END
046800101129     C* CARICO TABELLE
046900101129     C                   EXSR      CARTAB
047000941201     C*
047100941201     C* EMETTO IL PRIMO FORMATO CON LE PARZIALIZZAZIONI
047200020808     C                   MOVEL     'S'           WRIEM                          RIEMETTO TESTATA
047300101129     C*
047400101129     C*  se non devo emettere la prima videata --> passo subito ai controlli
047500101129     C                   If        PA1flg = '2' or pa1flg = '3'
047600101129     c                   exsr      contr1
047700101129     c* se errore esco dal pgm
047800101129     c                   if        *in90
047900101129     c                   eval      pa1err='E'
048000101129     c                   goto      fine
048100101129     C                   ENDIF
048200101129     c
048300101129     c                   else
048400101129     c
048500930205     C     FOR01         TAG
048600941201     C     WRIEM         IFEQ      'S'
048700941201     C                   WRITE     LR04T01                                      INTESTAZIONE VIDEO
048800941201     C                   MOVEL     *BLANKS       WRIEM                          NO RIEMETTO TESTATA
048900941201     C                   ENDIF
049000101122     C*
049100941130     C                   EXFMT     LR04D01
049200101129     c                   setoff                                       2890
049300101129     c                   setoff                                       555960
049400941201     C*
049500101129     C** F3 - FINE LAVORO   o F12 se richiamato
049600920810     C   KC
049700920810     COR KL
049800920810     CAN 26              GOTO      FINE
049900101129     C** F18- CAMBIO FILIALE
050000941212     C   KS              SETON                                        27
050100941212     C   KS              GOTO      FOR01
050200941201     C*
050300941201     C                   EXSR      CONTR1                                       CONTROLLI D01
050400900528     C   90              GOTO      FOR01
050500101129     c                   endif
050600101129     c
050700900518     C*
050800900611     C* CARICO FILIALI DI ARRIVO IN SUBFILE
050900900611     C                   EXSR      CARPPG
051000920810     C*
051100910422     C* SE SONO IN INTERROGAZIONE E NON TROVATE SPUNTE ESCO
051200920810     C* SE TIPO NPG = 9 NON VISUALIZZO
051300940727     C     NRR2          IFEQ      0
051400920810     C     *IN20         IFEQ      '1'
051500941201     C     V1CNPG        OREQ      '9'
051600911018     C                   SETON                                        54
051700911018     C                   GOTO      FOR01
051800911018     C                   END
051900911018     C                   END
052000160728     c*   Imposto l'ordinamento sfl di default: per numero foglio
052100160728     c                   eval      w_ord=w_nfv
052200160728     c                   eval      v2zf11=ord_dta
052300900611     C*
052400910320     C     FORCT2        TAG
052500941201     C                   WRITE     LR04T01                                      INTESTAZIONE VIDEO
052600941201     C                   WRITE     LR04Z02                                      TASTI FUNZIONE
052700900518     C*
052800900518     C     FORCT         TAG
052900941201     C                   EXFMT     LR04C02                                      SFLCTL
053000101129     c                   setoff                                       2890
053100090416     C** F3 - FINE LAVORO
053200900518     C   KC              GOTO      FINE
053300090416     C** F12 - RITORNO
053400101201     c                   if        *inkl
053500101201     c                   if        pa1flg='2' or pa1flg='3'
053600101201     C                   GOTO      Fine
053700101201     c                   else
053800101201     C                   GOTO      FOR01
053900101201     c                   endif
054000101201     c                   endif
054100160728     c* F11=Ordinamento
054200160728     c                   if        *inkk=*on
054300160728     c                   select
054400160728     c                   when      w_ord=w_nfv
054500160728     c                   eval      w_ord=w_dta
054600160728     c                   eval      v2zf11=ord_nfv
054700160823     c* Se richiesto l'ordinamento del sfl bisogna prima caricarlo tutto
054800160823     c* se non ancora fatto
054900160823     c* (il pgm carica il sfl una pagina alla volta)
055000160823     c                   if        wEndRoll='F'
055100160823     c                   exsr      sr_ordina
055200160823     c                   else
055300160823     c                   dow       *in20 and wEndRoll<>'F'
055400160823     c                   exsr      carlna
055500160823     c                   enddo
055600160823     c                   endif
055700160823     c****               exsr      sr_ordina
055800160728     c                   when      w_ord=w_dta
055900160728     c                   eval      w_ord=w_nfv
056000160728     c                   eval      v2zf11=ord_dta
056100160728     c                   exsr      carppg
056200160728     c                   endsl
056300160728     c                   goto      forct2
056400160728     c                   endif
056500090420     c
056600090420     c                   eval      rec2=pagnrr
056700090420     c                   eval      savpagnrr=pagnrr
056800900608     C*
056900090416     C* F13 - AGGIUNTA RIGHE
057000930707     C   KM              SETON                                        07
057100930707     C   KM
057200900618     C* SCORRIMENTO
057300901119     COR 08              DO
057400900618     C                   EXSR      CARLNA
057500900618     C                   GOTO      FORCT
057600900618     C                   END
057700090420     C
057800920323     C* ENTER SE IN INTERROGAZIONE RIEMETTO SUBFILE
057900960523     C* PRIMA PERO VERIFICO SE RICHIESTA INTERROGAZIONE ANOMALIE
058000031119     c     *in20         ifeq      *on
058100090420     c
058200090420     C                   EXSR      INTANM
058300031119     C   90              GOTO      FORCT
058400031119     c                   endif
058500090420     C
058600090420     c* F5 - interrogazione spunta senza anmomalia
058700090420     c                   if        *inke
058800090420     c                   seton                                        31
058900090420    4C     V6CPOS        IFGT      0
059000090420     C     V6CPOS        CHAIN     lr04s02                            31
059100090420     c                   if        not *in31 and v2ccan=' '
059200090420     c                   seton                                        31
059300090420     c                   endif
059400090420     c                   if        not *in31 and v2hi70='0'
059500090420     c                   seton                                        31
059600090420     c                   endif
059700090420     c
059800090420     c                   if        not *in31
059900090420     c                   EXSR      Wind06
060000090420     c                   eval      rec2=v6cpos
060100090420     C                   GOTO      FORCT
060200090420    4c                   endif
060300090420    4c                   endif
060400090420     c
060500090420    4c                   if        v6cpos=0 or *in31
060600090420     c
060700090420     c                   if        v6cpos=0
060800090420     c                   if        pagnrr>0
060900090420     c                   eval      rec2=pagnrr
061000090420     c                   endif
061100090420     C     rec2          CHAIN     lr04s02                            31
061200090420     c                   else
061300090420     c                   eval      rec2=v6cpos
061400090420     c                   endif
061500090420     c
061600101126     c                   seton                                        975731
061700090420     c                   seton                                        36
061800090420     C                   UPDATE    LR04S02
061900090420     c                   setoff                                       36
062000090420     C                   GOTO      FORCT
062100090420    4c                   endif
062200090420    3c                   endif
062300090420    c
062400090420     c* In interrogazione mi fermo qui   con la routine (il resto è per manutenzione)
062500090420     c                   if        *in20
062600090420     C                   GOTO      FORCT2
062700090420     c                   endif
062800920323     C*
062900920323     C* ENTER   CONTROLLO
063000920323     C                   EXSR      CONTR2
063100920323     C   91              GOTO      FORCT2
063200920323     C   90              GOTO      FORCT
063300960523     C* VERIFICO SE RICHIESTA INTERROGAZIONE ANOMALIE
063400960523     C                   EXSR      INTANM
063500031119     C   90              GOTO      FORCT
063600090420     c
063700920323     C*
063800920323     C* CMD15- AGGIUNTA RIGHE DAL AL
063900920323     C   KP              DO
064000920323     C*
064100920323     C                   EXSR      PULDAL
064200920323     C*
064300920323     C     FOR03         TAG
064400941201     C     WRIEM         IFEQ      'S'
064500941201     C                   WRITE     LR04T01                                      INTESTAZIONE VIDEO
064600941201     C                   MOVEL     *BLANKS       WRIEM                          NO RIEMETTO TESTATA
064700941201     C                   ENDIF
064800941130     C                   EXFMT     LR04D03
064900941201     C*
065000920323     C* CMD12 - RITORNO
065100920323     C   KL              GOTO      FORCT2
065200920323     C* CONTROLLO
065300920323     C                   EXSR      CTRDAL
065400920323     C  NKF
065500920323     COR 90              GOTO      FOR03
065600920323     C* CMD6 - CREO SPUNTE
065700920323     C                   EXSR      AGGDAL
065800920323     C                   GOTO      FORCT2
065900920323     C                   END
066000920323     C*
066100960523     C  NKF
066200090420     CANNKN              do
066300090420     c                   eval      rec2=savpagnrr
066400090420     C                   GOTO      FORCT2
066500090420     c                   enddo
066600910323     C*
066700910323     C* CMD6 - REGISTRA
066800920320     C   KN
066900920320     COR KF              DO
067000910323     C                   EXSR      REGIS
067100950711     C   96
067200960523     COR 95              GOTO      FORCT2
067300921020     C*
067400101201     c                   if        pa1flg='2' or pa1flg='3'
067500101201     C                   GOTO      Fine
067600101201     c                   else
067700930205     C                   GOTO      FOR01
067800101201     c                   endif
067900101201     c
068000910323     C                   END
068100900611     C*
068200000000     C     FINE          TAG
068300911014     C*
068400070220     C* CHIUSURA PGM
068500930521     C     CFS44         IFEQ      'C'
068600070220     c                   clear                   fnls45ds
068700070220     c                   movel     'C'           dls45FLG
068800070220     C                   CALL      'FNLS45R'
068900070220     c                   parm                    fnls45ds
069000930521     C                   END
069100950704     C*
069200950704     C                   CLEAR                   DSUL06
069300950704     C                   MOVEL     'C'           D06TLA
069400950704     C                   MOVEL     DSUL06        KPJBU
069500950704     C                   CALL      'TRUL06R'
069600950704     C                   PARM                    KPJBA
069700911017     C*
069800940727     C* CHIUSURA FILE FNLSA8R
069900930609     C     CFSA8         IFEQ      'C'
070000930609     C                   MOVEL     'C'           PARFLG
070100930609     C                   MOVEL     PARAM2        KPJBU
070200940727     C                   CALL      'FNLSA8R'
070300930609     C                   PARM                    KPJBA
070400930609     C                   END
070500971003     C* CHIUSURA PGM
070600971003     C                   CLEAR                   DSLV53
070700971003     C                   MOVEL     'C'           D53TLA
070800971003     C                   CALL      'FNLV53R'
070900971003     C                   PARM                    DSLV53
071000971003     C                   CLEAR                   DSLV55
071100971003     C                   MOVEL     'C'           D55TLA
071200971003     C                   CALL      'FNLV55R'
071300971003     C                   PARM                    DSLV55
071400930609     C*
071500950125     C* RISPEDISCO INDIETRO LA KPJBU COME ERA ALLA CHIAMATA
071600950125     C                   CLEAR                   KPJBU
071700950125     C                   MOVEL     PARAM1        KPJBU
071800000000     C                   SETON                                        LR
071900940727     C**************************************************************************
072000940727     C*    CARICO TABELLE
072100940727     C**************************************************************************
072200911017     C     CARTAB        BEGSR
072300930205     C***
072400911017     C* CARICO TABELLA FILIALI GESTITE £1
072500930205     C***
072600941201     C                   CLEAR                   DSUL06
072700941201     C                   MOVE      '£1'          D06COD
072800941201     C                   MOVEL     SIMFEL        D06KEY
072900941201     C                   MOVEL     DSUL06        KPJBU
073000941201     C                   CALL      'TRUL06R'
073100941201     C                   PARM                    KPJBA
073200941201     C                   MOVEL     KPJBU         DSUL06
073300941201     C                   MOVEA     LIN           L1
073400950704     C***
073500950704     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE (DA £6)
073600950704     C***
073700950704     C                   CLEAR                   DSUL06
073800950704     C                   MOVE      '£6'          D06COD
073900020509     C                   MOVEL     'G'           D06ESC
074000020509     C                   MOVEL     SIMFEL        D06KEY
074100950704     C                   MOVEL     DSUL06        KPJBU
074200950704     C                   CALL      'TRUL06R'
074300950704     C                   PARM                    KPJBA
074400950704     C                   MOVEL     KPJBU         DSUL06
074500950704     C                   MOVEA     LIN           L6S
074600950704     C***
074700920807     C* CARICO CATEGORIE
074800950704     C***
074900940727     C                   MOVEL     '7N'          KCOD
075000020808     C                   Z-ADD     0             XI
075100940727     C     KTAB1         SETLL     TABEL
075200940727     C     KTAB1         READE     TABEL                                  31
075300920807     C*
075400920807     C     *IN31         DOWEQ     '0'
075500920807     C     TBLFLG        IFEQ      ' '
075600920807     C                   MOVEL     TBLUNI        DS7N
075700920807     C*
075800031121     c     §7nuti        ifne      '9'
075900020808     C                   ADD       1             XI
076000020808     C                   MOVEL     TBLKEY        CAT(XI)
076100020808     C                   MOVEL     §7NDES        DEC(XI)
076200020808     C                   MOVEL     §7NNST        NST(XI)                        NR.SPUNTE GG.
076300101201     C                   MOVEL     §7Nord        cator(XI)
076400101201     C                   MOVE      cat(XI)       cator(XI)
076500031121     c                   endif
076600920807     C                   END
076700920807     C*
076800940727     C     KTAB1         READE     TABEL                                  31
076900920807     C                   END
077000920807     C*
077100950704     C     CAT(1)        IFNE      ' '
077200950704     C                   MOVEL     ' ='          V1CCA1
077300950704     C                   MOVEL     CAT(1)        V1CCA1
077400950704     C                   MOVE      DEC(1)        V1CCA1
077500920810     C                   END
077600920810     C*
077700950704     C     CAT(2)        IFNE      ' '
077800950704     C                   MOVEL     ' ='          V1CCA2
077900950704     C                   MOVEL     CAT(2)        V1CCA2
078000950704     C                   MOVE      DEC(2)        V1CCA2
078100920810     C                   END
078200920810     C*
078300950704     C     CAT(3)        IFNE      ' '
078400950704     C                   MOVEL     ' ='          V1CCA3
078500950704     C                   MOVEL     CAT(3)        V1CCA3
078600950704     C                   MOVE      DEC(3)        V1CCA3
078700920810     C                   END
078800920810     C*
078900950704     C     CAT(4)        IFNE      ' '
079000950704     C                   MOVEL     ' ='          V1CCA4
079100950704     C                   MOVEL     CAT(4)        V1CCA4
079200950704     C                   MOVE      DEC(4)        V1CCA4
079300950704     C                   ENDIF
079400920810     C*
079500950704     C     CAT(5)        IFNE      ' '
079600950704     C                   MOVEL     ' ='          V1CCA5
079700950704     C                   MOVEL     CAT(5)        V1CCA5
079800950704     C                   MOVE      DEC(5)        V1CCA5
079900920810     C                   END
080000920810     C*
080100950704     C     CAT(6)        IFNE      ' '
080200950704     C                   MOVEL     ' ='          V1CCA6
080300950704     C                   MOVEL     CAT(6)        V1CCA6
080400950704     C                   MOVE      DEC(6)        V1CCA6
080500920810     C                   END
080600941201     C*
080700950704     C     CAT(7)        IFNE      ' '
080800950704     C                   MOVEL     ' ='          V1CCA7
080900950704     C                   MOVEL     CAT(7)        V1CCA7
081000950704     C                   MOVE      DEC(7)        V1CCA7
081100941201     C                   END
081200941201     C*
081300950704     C     CAT(8)        IFNE      ' '
081400950704     C                   MOVEL     ' ='          V1CCA8
081500950704     C                   MOVEL     CAT(8)        V1CCA8
081600950704     C                   MOVE      DEC(8)        V1CCA8
081700941201     C                   END
081800101126     c* Preparo anche stringa unica : e piene <=7 decodifica di 8
081900101126     c*  altrimenti decodifica di 5
082000101201     c                   select
082100101201     c                   when      cat(6)=' '
082200101201     c                   eval      wlung=10
082300101201     c                   when      cat(7)=' '
082400101201     c                   eval      wlung=9
082500101201     c                   when      cat(8)=' '
082600101126     c                   eval      wlung=7
082700101201     c                   other
082800101126     c                   eval      wlung=5
082900101201     c                   endsl
083000101126     c
083100101126     c                   z-add     1             xi
083200101126     c                   dow       cat(xi)<>*blanks
083300101126     c                   if        xi = 1
083400101126     c                   eval      v1dcaf=cat(xi)+'='+%subst(dec(XI):1:wlung)
083500101126     c                   else
083600101126     c                   eval      v1dcaf=%trim(v1dcaf) + ' ' +
083700101126     c                                    cat(xi)+'='+%subst(dec(XI):1:wlung)
083800101126     c                   endif
083900101126     c                   add       1             xi
084000101126     c                   enddo
084100950704     C**
084200920810     C* CARICO TABELLA TIPI INVENTARI
084300950704     C**
084400940727     C                   MOVEL     '7B'          KCOD
084500020808     C                   Z-ADD     0             XI
084600940727     C     KTAB1         SETLL     TABEL
084700940727     C     KTAB1         READE     TABEL                                  31
084800920810     C*
084900920810     C     *IN31         DOWEQ     '0'
085000920810     C     TBLFLG        IFEQ      ' '
085100920810     C*
085200020808     C                   ADD       1             XI
085300020808     C                   MOVEL     TBLKEY        SPG(XI)
085400161006     c                   movel     tbluni        ds7b
085500161006     C                   MOVEL     §7bdesb       DES(XI)
085600920810     C                   END
085700920810     C*
085800940727     C     KTAB1         READE     TABEL                                  31
085900920810     C                   END
086000950704     C**
086100920807     C* VEDO SE SONO SIMFEL O REMOTO
086200950704     C**
086300941230     C                   MOVEL     *BLANKS       V1DFGS
086400950705    1C  N20V1CFGS        IFEQ      *ZERO
086500941230     C     PA1PGM        ANDEQ     *BLANKS                                      SE NON RICHIAMATO
086600950124     C*
086700020808     c                   z-add     SIMpou        V1Cfgs
086800950704    1C                   ENDIF
086900960506     C*
087000960506     C* CARICO SCHIERA DELLE FILIALI GESTIBILI
087100960506     C*
087200960506     C                   CLEAR                   FG
087300020808     C                   IF           SIMtpp = '2'
087400020808     C                             or SIMtpp = *blanks
087500020808     C                   move      SIMpou        FG(1)
087600020808     C                   ELSE
087700960506     C                   MOVEA     L1            FG
087800960506     C                   END
087900941230     C*
088000960503     C                   CLEAR                   WFGS
088100950704    1C     V1CFGS        IFNE      *ZERO
088200941220     C                   MOVEL     V1CFGS        NUM3
088300930707     C                   EXSR      CAZORG
088400941230     C  N39              MOVEL     ORGDES        V1DFGS
088500960503     C  N39              MOVEL     V1CFGS        WFGS
088600930205     C*
088700930205     C                   EXSR      CARL6
088800971003     C                   MOVEA     L6            L6F
088900950704    1C                   ENDIF
089000101129     c                   ENDSR
089100940727     C**************************************************************************
089200940727     C*    CONTROLLI FORMATO1
089300940727     C**************************************************************************
089400900528     C     CONTR1        BEGSR
089500900528     C                   SETOFF                                       010203
089600900528     C                   SETOFF                                       900405
089700101129     C                   SETOFF                                       3018
089800160823     C                   SETOFF                                       540622
089900031204     c                   seton                                        09
090000971003     C                   CLEAR                   DSLV53
090100920810     C                   Z-ADD     0             VI1NFV
090200101202     c                   clear                   v2cbolp
090300101202     c                   clear                   v2cbors
090400101202     c                   clear                   v2cbosp
090500101202     c                   clear                   v2cmit
090600101201     c                   clear                   v2cdes
090700101201     c                   clear                   v2cbosp
090800101202     c                   clear                   v2cbods
090900101201     c                   clear                   boldsp
091000941202     C*----------------------------------------------------
091100101129     C*             FILIALE IN GESTIONE
091200941202     C*----------------------------------------------------
091300101129     C* In gestine NON PUO' ESSERE ZERO
091400101129     C* NON OBBLIGATORIA IN INTERROGAZIONE
091500101129     C     *IN20         IFEQ      *OFF                                         20OFF = GESTIONE
091600101129     C     V1CFGS        andeq     *ZERO
091700941212     C                   SETON                                        57  90
091800941212     C                   GOTO      ENDCT1
091900941212     C                   ENDIF
092000941202     C*
092100941202     C                   MOVEL     *BLANKS       V1DFGS
092200941212     C     V1CFGS        IFNE      *ZERO
092300941201     C                   MOVEL     V1CFGS        NUM3
092400941201     C                   EXSR      CAZORG
092500941205     C   39              SETON                                        57  90
092600941202     C   39              GOTO      ENDCT1
092700101129     c
092800941201     C                   MOVEL     ORGDES        V1DFGS
092900101129     c
093000971126     C                   CLEAR                   DSLV55
093100971126     C                   MOVEL     'P'           D55TPT
093200971126     C                   MOVE      V1CFGS        D55LIN
093300971126     C                   Z-ADD     WDATEU        D55DRF
093400971126     C                   CALL      'FNLV55R'
093500971126     C                   PARM                    DSLV55
093600971126     C                   MOVE      D55TFP        FGSTFP
093700941201     C*
093800941201     C                   EXSR      CARL6
093900941212     C                   ENDIF
094000941202     C*----------------------------------------------------
094100960503     C* CATEGORIA FOGLI VIAGGIO
094200941202     C*----------------------------------------------------
094300941201     C     V1CNPG        IFEQ      ' '
094400910320     C                   SETON                                        4990
094500910320     C                   GOTO      ENDCT1
094600910320     C                   END
094700920807     C* RICERCA IN TABELLA
094800941201     C     V1CNPG        IFEQ      '?'
094900920807     C                   MOVEL     CODUT         §KUT
095000920807     C                   MOVEL     '7N'          §COD
095100941201     C                   MOVE      ' '           V1CNPG
095200920807     C                   MOVEL     *BLANKS       §KEY
095300920807     C                   CALL      'X§TABER'
095400920807     C                   PARM                    §KUT              1 0
095500920807     C                   PARM                    §COD              2
095600920807     C                   PARM                    §KEY              8
095700920807     C                   PARM                    §DES             30
095800941201     C                   MOVEL     §KEY          V1CNPG
095900920807     C                   SETON                                        90
096000941205     C                   MOVEL     'S'           WRIEM             1            RIEMETTO TESTATA
096100920807     C                   GOTO      ENDCT1
096200920807     C                   END
096300920807     C*
096400941201     C     V1CNPG        IFNE      '9'
096500020808     C                   Z-ADD     1             XI
096600020808     C     V1CNPG        LOOKUP    CAT(XI)                                31
096700920807     C* ERRORE
096800920807     C  N31              SETON                                        4990
096900920807     C  N31              GOTO      ENDCT1
097000950704     C*
097100910930     C                   SETON                                        18
097200950704     C                   ENDIF
097300920810     C*
097400941201     C                   MOVEL     V1CNPG        SAVNPG
097500941202     C*----------------------------------------------------
097600950704     C* NUMERO FOGLIO
097700941202     C*----------------------------------------------------
097800101129     C*
097900101129     C     V1CNFV        IFNE      *BLANKS
098000101129     C     V1CNFV        ANDNE     *ZEROS
098100101129     c
098200101129     C* NON SI PUO' IMMETTERE SE CAT = 9
098300101129     C     SAVNPG        IFEQ      9
098400101129     C                   SETON                                        559028
098500101129     c                   eval      v1cmsg=msg(1)
098600910930     C                   GOTO      ENDCT1
098700910930     C                   END
098800101129     C* ERRORE SE NON IMMESSA FILIALE GESTIONE
098900101129     C     V1CFGS        IFEQ      0
099000101129     C                   SETON                                        599028
099100101129     c                   eval      v1cmsg=msg(2)
099200101129     C                   GOTO      ENDCT1
099300101129     C                   ENDIF
099400910423     C*
099500920810     C* CONTROLLO SE INTERROGAZIONE FOGLI
099600941201     C     '?'           SCAN      V1CNFV                                 90
099700920810     C*
099800930707     C     *IN90         IFEQ      *ON
099900950704     C*
100000910930     C*
100100941212     C     V1CNPG        IFEQ      '1'
100200941212     C*
100300941212     C* FOGLI PARTENZE
100400950704     C     V1CFGS        IFEQ      SIMFEL
100500130520     C                   RESET                   fnlst3ds
100600951108     C                   Z-ADD     WMENO1        PA5DIN                         MENO 5 GIORNI G/M/A
100700951108     C                   Z-ADD     WDATE         PA5DFI                         FINALE GG/MM/AAAA
100800000614     C                   Z-ADD     V1CFGS        PA5FGS                         P.O. GESTIONE
100900130520     C                   MOVEL     fnlst3ds      KPJBU
101000130520     C                   CALL      'FNLST3R'
101100941212     C                   PARM                    KPJBA
101200130520     C                   MOVEL     KPJBU         fnlst3ds
101300951108     C                   MOVEL     PA5NFV        V1CNFV
101400950704     C                   ELSE
101500950704     C                   MOVEL     'FNLR04R '    PA6PGM
101600950704     C                   Z-ADD     *ZERO         PA6LAI
101700950704     C                   MOVEL     SIMFEL        PA6FEL
101800950704     C                   MOVEL     *BLANKS       PA6ABB
101900950704     C                   Z-ADD     WMENO2        PA6DSD
102000950704     C                   Z-ADD     99991231      PA6DSA
102100950704     C                   MOVEL     'I'           PA6FLG                         'I'=INTERROGAZIONE
102200000609     C                   MOVEL     FGSTFP        PA6FGS
102300950704     C                   CLEAR                   KPJBU
102400950704     C                   MOVEL     PARAM6        KPJBU
102500950704     C                   MOVEA     *ZEROS        FVA
102600950704     C*
102700950704     C                   CALL      'FNLR26R'
102800950704     C                   PARM                    KPJBA
102900950704     C                   PARM                    FVA
103000950704     C                   MOVEL     KPJBU         PARAM6
103100950704     C                   MOVE      FVA(1)        V1CNFV
103200950704     C                   ENDIF
103300941212     C*
103400941212     C                   ELSE
103500941212     C*
103600941212     C* FOGLI ARRIVI
103700941212     C                   CLEAR                   DSLR01
103800941212     C                   MOVEL     'S'           DLRRIC                         PGM.RICHIAMATO
103900941212     C                   MOVEL     V1CNPG        DLRNPG                         CATEGORIA
104000941212     C                   Z-ADD     V1CFGS        DLRFGS                         FIL.GESTIONE
104100950616     C                   Z-ADD     WMENO1        DLRGDA                         MENO 5 GIORNI G/M/A
104200950616     C                   Z-ADD     WMENO2        DLRADA                         MENO 5 GIORNI A/M/G
104300941212     C                   MOVEL     'FNLR04R'     DLRPGM                         PROGRAMMA
104400941212     C                   MOVEL     DSLR01        KPJBU
104500941212     C                   CALL      'FNLR02R'
104600910423     C                   PARM                    KPJBA
104700941212     C                   MOVEL     KPJBU         DSLR01
104800941212     C                   MOVEL     DLRNFV        V1CNFV
104900941212     C*
105000941212     C                   ENDIF
105100941212     C*
105200101129     c                   seton                                        55
105300941205     C                   MOVEL     'S'           WRIEM             1            RIEMETTO TESTATA
105400910423     C                   GOTO      ENDCT1
105500910423     C                   END
105600941202     C*----------------------------------------------------
105700910423     C* CONTROLLO SE CAMPO NUMERICO
105800941201     C                   TESTN                   V1CNFV               30
105900101129     c                   if        not *in30 or %subst(v1cnfv:5:1)<'0'
106000101129     C                   SETON                                        559028
106100101129     c                   eval      v1cmsg=msg(3)
106200101129     C                   GOTO      ENDCT1
106300101129     C                   ENDIF
106400950124     C*
106500941201     C* SE RIEMPITO "NR.FOGLIO" VERIFICO L'ESISTENZA
106600101129    2C     V1CNFV        IFGT      *ZEROS
106700941202     C*
106800000120     C                   CLEAR                   VI1NFV
106900000120     C                   MOVE      V1CNFV        VI1NFV
107000971003     C**
107100971003     C* CALL A PGM FNLV53R DI REPERIMENTO DATI FOGLI
107200971003     C                   MOVEL     SIMFEL        D53FEL
107300031204     C                   MOVEL     V1CNFV        D53NFV
107400971003     C                   MOVEL     V1CNPG        D53NPG
107500031204     C                   MOVEL     V1CFGS        D53FGS
107600971003     C                   CALL      'FNLV53R'
107700971003     C                   PARM                    DSLV53
107800971003     C* ERRORE
107900971003    3C     D53ERR        IFNE      ' '
108000101129     C                   SETON                                        559028
108100101129     c                   eval      v1cmsg=msg(3)
108200101129     C                   GOTO      ENDCT1
108300971003    3C                   ENDIF
108400101129     c
108500981030     C* LA MANUTENZIONE DEI FV PARTENZA CONCESSA SOLO A SIMFEL
108600981030     C* SE E' DEFLUENZA POSSO
108700101129    3C  n20V1CNPG        IFEQ      '1'
108800000609     C     D53DEF        ANDNE     'S'
108900000609    4C     V1CFGS        IFNE      SIMFEL
109000000609     C*
109100101129     C                   SETON                                        559028
109200101129     c                   eval      v1cmsg=msg(4)
109300981030     C                   GOTO      ENDCT1
109400000609    4C                   ENDIF
109500000609    3C                   ENDIF
109600941202     C*
109700971003   X2C                   ELSE
109800971003     C* IL FOGLIO NON E' UN NUMERO
109900101129     C                   SETON                                        559028
110000101129     c                   eval      v1cmsg=msg(3)
110100101129     C                   GOTO      ENDCT1
110200971003    2C                   END
110300900626     C*
110400910320     C*
110500900626     C                   SETON                                        01
110600971003    1C                   END
110700980212     C* VERIFICO IN GESTIONE SE POSSO USALE TALE FOGLIO ANCHE SE
110800971003     C*  NON MIO
110900971003    1C     *IN20         IFEQ      *OFF
111000971003     C**
111100971003     C                   CLEAR                   WNOFG
111200971003     C     V1CFGS        LOOKUP    FG                                     32
111300971003    2C     *IN32         IFEQ      *OFF
111400971003     C                   MOVE      'N'           WNOFG             1
111500980216     C**NON PRESENTE IN FG: CONTROLLO CHE ABBIA IL MIO STESSO TERMINAL
111600980216     C**DI ARRIVO
111700010628     C**(N.B.:FG CONTIENE £1 SE I°LIVELLO, LA FILIALE SE II°LIVELLO)
111800971003     C* Lna obbligatoria
111900971003    4C     V1CLNA        IFEQ      *ZEROS
112000971003     C                   SETON                                        67  90
112100971003     C                   GOTO      ENDCT1
112200971003    4C                   END
112300971003     C* Deve essere la mia lna oppure lna in gestione
112400971003    4C     V1CLNA        IFNE      WFGS
112500971003     C     V1CLNA        LOOKUP    L6S                                    32
112600971003     C* Errore: non è mia lna e non è in gestione
112700971003     C  N32              SETON                                        68  90
112800971003     C  N32              GOTO      ENDCT1
112900971003     C     V1CLNA        LOOKUP    L6F                                    32
113000971003     C  N32              SETON                                        68  90
113100971003     C  N32              GOTO      ENDCT1
113200971003    4C                   END
113300971003    2C                   END
113400971003    1C                   END
113500941202     C*----------------------------------------------------
113600900528     C* LINEA PARTENZA
113700911014     C* OBBLIGATORIA SE CATEGORIA FV=9
113800941201     C     V1CLNP        IFEQ      0
113900921229     C     SAVNPG        ANDEQ     9
114000101203     c     v1cbosp       andeq     0
114100101203     c     v1cbolp       andeq     0
114200910930     C                   SETON                                        5690
114300910930     C                   GOTO      ENDCT1
114400910930     C                   END
114500910930     C*
114600941201     C     V1CLNP        IFGT      0
114700941201     C                   MOVEL     V1CLNP        NUM3
114800930707     C                   EXSR      CAZORG
114900910320     C* ERRORE
115000930707     C   39              SETON                                        4090
115100930707     C   39              GOTO      ENDCT1
115200910320     C*
115300900528     C                   SETON                                        02
115400960522     C     ORGDE5        IFNE      *BLANKS
115500960522     C                   MOVEL     ORGDE5        DESLNP
115600960522     C                   ELSE
115700020207     C                   MOVEL     ORGDES        DESLNP           20
115800960522     C                   END
115900900528     C                   END
116000900528     C* LINEA ARRIVO
116100941201     C     V1CLNA        IFGT      0
116200941201     C                   MOVEL     V1CLNA        NUM3
116300930707     C                   EXSR      CAZORG
116400910320     C*
116500930707     C   39              SETON                                        4190
116600930707     C   39              GOTO      ENDCT1
116700021126     C                   MOVE      §OGNTW        WLANTW
116800910320     C*
116900900619     C                   SETON                                        03
117000960522     C     ORGDE5        IFNE      *BLANKS
117100960522     C                   MOVEL     ORGDE5        DESLNA
117200960522     C                   ELSE
117300930707     C                   MOVEL     ORGDES        DESLNA           20
117400960522     C                   END
117500020207     C**
117600020207     C* MEMORIZZO LNA ESTERE CHE FANNO IL MANIFEST
117700020207     C                   CLEAR                   LNAMNF
117800020207     C     §OGNTW        IFEQ      'EEX'
117900020207     C     §OGNTW        OREQ      'FED'
118000060322     C     §OGNTW        OREQ      'DPD'
118100020207     C                   MOVEL     'S'           LNAMNF            1
118200900528     C                   END
118300031204     C     §OGNTW        IFne      'EEX'
118400031204     C     §OGNTW        andne     'FED'
118500031204     C     §OGNTW        andne     'DPD'
118600031204     c                   setoff                                       09
118700031204     C                   END
118800020207     C                   END
118900900608     C* SEGNACOLLO DAL
119000941201     C     V1CNCD        COMP      0                                  0404
119100900528     C* NUMERO COLLO AL > NUMERO COLLO DAL
119200941201     C     V1CNCA        IFGT      0
119300941201     C     V1CNCA        IFLT      V1CNCD
119400900528     C   30              SETON                                        4290
119500900528     C   30              GOTO      ENDCT1
119600900528     C                   END
119700900528     C                   SETON                                        05
119800900528     C                   END
119900921229     C* SERIE >< DA 99
120000941201     C     V1CNRS        IFNE      99
120100921229     C                   SETON                                        06
120200921229     C                   END
120300101129     c*
120400101129     C* Se immessa controllo numero spedizione
120500101129    1c                   if        v1cbosp>0 or  v1cbolp>0
120600101201     c
120700101129     c     kblp2         chain     fnblp01l
120800101129    2c                   if        not %found(fnblp01l)
120900101129     c     kblp2         chain     fnarb01l
121000101129    3c                   if        not %found(fnarb01l)
121100101129     C                   SETON                                        609028
121200101129     c                   eval      v1cmsg=msg(5)
121300101129     C                   GOTO      ENDCT1
121400101201    3C                   ENDIF
121500101201    2C                   ENDIF
121600101216     c
121700101216     c* Se la spedizione non ha segnacolli segnalo suvito errore
121800101216     c     kblp2         setll     fnblt01l
121900101216    3c                   if        not %equal(fnblt01l)
122000101216     c     kblp2         setll     fnart01l
122100101216    4c                   if        not %equal(fnart01l)
122200101216     C                   SETON                                        5490
122300101216     C                   GOTO      ENDCT1
122400101216    3C                   ENDIF
122500101216    2C                   ENDIF
122600101129     c*  Se immessa la serie, deve essere uguale a quella del segnacollo
122700101201    2c                   if        v1cnrs<>99
122800101202     c                             and  v1cnrs<>v1cbors
122900101129     C                   SETON                                        609028
123000101129     c                   eval      v1cmsg=msg(6)
123100101129     C                   GOTO      ENDCT1
123200101129    2C                   ENDIF
123300101129     c*
123400101202    2c                   if        v1clna>0 and v1clna<>blplna
123500101129     C                   SETON                                        609028
123600101129     c                   eval      v1cmsg=msg(7)
123700101129     C                   GOTO      ENDCT1
123800101202    2C                   ENDIF
123900101202    2c                   if        v1clnp>0 and v1clnp<>blpfls
124000101129     C                   SETON                                        609028
124100101129     c                   eval      v1cmsg=msg(8)
124200101129     C                   GOTO      ENDCT1
124300101202    2C                   ENDIF
124400101216     c* Se immesso un solo segnacollo deve far parte della spedizione
124500101216    2c                   if        v1cncd>0 and (v1cnca=0 or v1cnca=v1cncd)
124600101216     c                   eval      brvnsc=v1cncd
124700101216     c     kbodet        setll     fnblt01l
124800101216    3c                   if        not %equal(fnblt01l)
124900101216     c     kbodet        setll     fnart01l
125000101216    4c                   if        not %equal(fnart01l)
125100101216     C                   SETON                                        609028
125200101216     c                   eval      v1cmsg=msg(9)
125300101216     C                   GOTO      ENDCT1
125400101216    4C                   ENDIF
125500101216    3C                   ENDIF
125600101216    2C                   ENDIF
125700160823
125800101201     c* Memorizzo i dati da visualizzare nella seconda videata
125900101201     c                   eval      v2cmit=blprsm
126000101201     c                   eval      v2cdes=blprsd
126100101202     c                   eval      boldsp=(blpaas*10000)+blpmgs
126200101202     c                   eval      v2cbosp=v1cbosp
126300101202     c                   eval      v2cbolp=v1cbolp
126400101202     c                   eval      v2cbors=v1cbors
126500101129    1c                   endif
126600160823     c*attivazione tasto funzionale per ordinamento sfl solo in interrogazione per
126700160823     c* spedizione o per singolo segnacollo
126800160823     c                   if        *in20 and
126900160823     c                             (v1cbosp>0 or (
127000160823     c                             v1cncd>0 and (v1cnca=0 or v1cnca=v1cncd)
127100160823     c                             ))
127200160823     c                   seton                                        22
127300160823     c                   endif
127400101129     c
127500900614     C     ENDCT1        ENDSR
127600940727     C**************************************************************************
127700940727     C*    CARICO PRIMA PAGINA IN SUBFILE
127800940727     C**************************************************************************
127900900608     C     CARPPG        BEGSR
128000940727     C*
128100900615     C                   SETOFF                                       0730
128200900518     C                   EXSR      PULSF
128300900709     C* AZZERO CAMPI DI DUP
128400900709     C                   Z-ADD     0             DUPNFV            5 0
128500900709     C                   Z-ADD     0             DUPLNP            3 0
128600900709     C                   Z-ADD     0             DUPLNA            3 0
128700900709     C                   Z-ADD     0             DUPNRS            2 0
128800910131     C                   Z-ADD     0             DUPNSC            7 0
128900910930     C                   Z-ADD     0             DUPNPG            1 0
129000910320     C                   MOVEL     *BLANKS       DUPCAN            1
129100160729     c*   Imposto l'ordinamento sfl di default: per numero foglio
129200160729     c                   eval      w_ord=w_nfv
129300160729     c                   eval      v2zf11=ord_dta
129400101201     c* Imposto i campi chiave
129500101201     c                   eval      klnps=v1clnp
129600101201     c                   eval      klnas=v1clna
129700101201     c                   eval      knrss=v1cnrs
129800101201     c                   eval      knscs=v1cncd
129900101201     c                   eval      kfgss=v1cfgs
130000101201     c                   eval      savnca=v1cnca
130100101203     c                   eval      savncd=v1cncd
130200900709     C*
130300940727     C                   Z-ADD     1             REC2
130400101201     C* 18 off - CATEGORIA FV E' 9
130500930707    1C     *IN18         IFEQ      *OFF
130600930707     C                   Z-ADD     0             SAVNPG
130700950704     C                   Z-ADD     1             C                 3 0
130800101201     C                   Z-ADD     1             CC                3 0
130900101201     c                   sorta     cator
131000101201     c                   dow       cator(CC)=*blanks
131100101201     c                   add       1             cc
131200101201     c                   enddo
131300101201     c
131400101201    2C     CATor(CC)     IFNE      ' '
131500101201     C                   MOVE      CATor(CC)     SAVNPG
131600930707     C*
131700101201   X2C                   ELSE
131800000522     C                   MOVEL     0             SAVNPG
131900101201    2C                   ENDIF
132000930707     C*
132100930707    1C                   ENDIF
132200930707     C*
132300101201    1c                   if        v1cbosp>0
132400101201     c                   exsr      DatiSped
132500101201    1c                   endif
132600101201     c* se presente solo un segnacollo cerco la bolla
132700101201     c                   if        v1clnp>0 and v1clna>0 and v1cnrs<>99
132800101202     c                             and v1cncd>0 and v1cbosp=0
132900101201     c                   exsr      cercaBol
133000101201     c                   endif
133100101202     c
133200101202     c* Se trovata spedizione la visualizzo
133300101202     c                   if        boldsp>0
133400101202     c     *iso          movel     boldsp        dataiso
133500101202     c                   movel     dataiso       dataeur
133600101202     c                   movel     dataeur       v2cbods
133700101202     c                   endif
133800101201     c
133900900618     C* NON EFFETTUATA NESSUNA SELEZIONE O SOLO LNA
134000930707    1C     *IN01         IFEQ      *OFF
134100930707    2C     *IN02         IFEQ      *OFF
134200930707     C                   Z-ADD     1             VI1NFV
134300070116     C     KBRV1         SETLL     FnBRV01L
134400930707   X2C                   ELSE
134500930707     C* LNP
134600140625     C  N03KBRV4         SETLL     FnBRV05L
134700930707     C*
134800930707     C* LNP LNA
134900930707    3C     *IN03         IFEQ      *ON
135000140625     C  N06KBRV5         SETLL     FnBRV05L                                      SENZA SERIE
135100000118     C   06
135200140625     CANN04KBRV51        SETLL     FnBRV05L                                      +SERIE
135300000118     C   06
135400140625     CAN 04KBRV52        SETLL     FnBRV05L                                      +SER+SEGNAC
135500930707    3C                   ENDIF
135600930707    2C                   ENDIF
135700930707     C*
135800000609   X1C                   ELSE
135900900615     C*
136000930707    2C     *IN02         IFEQ      *OFF
136100900528     C* SELEZIONI
136200900528     C* NFV
136300070116     C  N03KBRV1         SETLL     FnBRV01L
136400930707     C*
136500930707     C* NFV LNA
136600941201     C   03              Z-ADD     V1CLNA        KLN
136700070116     C   03KBRV2         SETLL     FnBRV02L
136800930707     C*
136900930707   X2C                   ELSE
137000900615     C*
137100900528     C* NFV LNP
137200941201     C  N03              Z-ADD     V1CLNP        KLN
137300070116     C  N03KBRV2         SETLL     FnBRV01L
137400900615     C*
137500900615     C*
137600900528     C* NFV LNP LNA
137700930707    3C     *IN03         IFEQ      '1'
137800070116     C  N06KBRV3         SETLL     FnBRV01L                                      SENZA SERIE
137900000118     C   06
138000070116     CANN04KBRV31        SETLL     FnBRV01L                                      +SERIE
138100000118     C   06
138200070116     CAN 04KBRV32        SETLL     FnBRV01L                                      +SER+SEGNAC
138300930707    3C                   ENDIF
138400930707    2C                   ENDIF
138500930707    1C                   ENDIF
138600921229     C**
138700900528     C* LETTURA
138800940727     C                   Z-ADD     0             NRR2              4 0
138900900608     C                   Z-ADD     0             SAVNRR            4 0
139000900608     C                   EXSR      CARLNA
139100900608     C                   ENDSR
139200101201     C**************************************************************************
139300101201     C*    Cerco la bolla dal segnacollo
139400101201     C**************************************************************************
139500101201     c     Cercabol      BEGSR
139600101201     c     ksegnac       chain     fnblt27l
139700101201    1c                   if        %found(fnblt27l)
139800101201     c     kblp          chain     fnblp01l
139900101201   x1c                   else
140000101202     c     ksegnac       chain     fnart27l
140100101202    2c                   if        %found(fnart27l)
140200150623     c     kblp          chain     fnarb01l
140300101201   x3c                   else
140400101201     c                   leavesr
140500101201    2c                   endif
140600101201    1c                   endif
140700101201     c* Se c'e' segnacollo AL verifico se fa parte della stessa sped
140800101202    1c                   if        *in05  and blpfns=' '
140900101201     c                   eval      knscs=v1cnca
141000101201     c     kblpdet       setll     fnblt01l
141100101202    2c                   if        not %equal(fnblt01l)
141200101201     c     kblpdet       setll     fnart01l
141300101202    3c                   if        not %equal(fnart01l)
141400101201     c                   eval      knscs=v1cncd
141500101201     c                   leavesr
141600101202    3c                   endif
141700101202    2c                   endif
141800101202    1c                   endif
141900101201     c
142000101202     c                   eval      knscs=v1cncd
142100101201     c                   eval      v2cmit=blprsm
142200101201     c                   eval      v2cdes=blprsd
142300101202     c                   eval      boldsp=(blpaas*10000)+blpmgs
142400101202     c                   eval      v2cbosp=blpnsp
142500101202     c                   eval      v2cbolp=blplnp
142600101202     c                   eval      v2cbors=blpnrs
142700101201     c
142800101201     c                   ENDSR
142900101201     C**************************************************************************
143000101201     C*    CARICO dati spedizione
143100101201     C**************************************************************************
143200101201     c     DatiSped      BEGSR
143300101201     c* Se immesso il numero di spedizione, cerco subito quello
143400101201     c*  le altre selezioni sono parzilizzazioni
143500101201     c                   seton                                        020304
143600101201     c                   seton                                        06
143700101201     c
143800101201     c     kblp2         setll     fnblt01l
143900101201     c     kblp2         reade     fnblt01l
144000101201    2c                   if        not %eof(fnblt01l)
144100101201     c* primo segnacollo
144200101216     c                   if        v1cncd=0 and v1cnca=0
144300101201     c                   eval      knscs=bltnsc
144400101203     c                   eval      savncd=bltnsc
144500101215     c                   endif
144600101215     c
144700101201     c     kblp2         setgt     fnblt01l
144800101201     c     kblp2         readpe    fnblt01l
144900101201     c* ultimo segnacollo
145000101201    3c                   if        not %eof(fnblt01l)
145100101216     c                   if        v1cncd=0 and v1cnca=0
145200101201     c                   eval      savnca=bltnsc
145300101201    3c                   endif
145400101215    3c                   endif
145500101201     c
145600101201   x2c                   else
145700101201     c     kblp2         setll     fnart01l
145800101201     c     kblp2         reade     fnart01l
145900101201    3c                   if        not %eof(fnart01l)
146000101201     c* primo segnacollo
146100101216     c                   if        v1cncd=0 and v1cnca=0
146200101201     c                   eval      knscs=bltnsc
146300101203     c                   eval      savncd=bltnsc
146400101215     c                   endif
146500101215     c
146600101201     c     kblp2         setgt     fnart01l
146700101201     c     kblp2         readpe    fnart01l
146800101201     c* ultimo segnacollo
146900101201    4c                   if        not %eof(fnart01l)
147000101216     c                   if        v1cncd=0 and v1cnca=0
147100101201     c                   eval      savnca=bltnsc
147200101201    4c                   endif
147300101215    4c                   endif
147400101201     c
147500101201   x3c                   else
147600101201     c
147700101201     c                   clear                   bltfls
147800101201     c                   clear                   bltlna
147900101201     c                   clear                   bltnrs
148000101201     c                   clear                   bltnsc
148100101201     c                   setoff                                       020304
148200101201     c                   setoff                                       06
148300101201    3c                   endif
148400101201    2c                   endif
148500101201     c
148600101201     c                   eval      klnps=bltfls
148700101201     c                   eval      klnas=bltlna
148800101201     c                   eval      knrss=bltnrs
148900101201     c* Segnacollo DAL
149000101201    2c                   if        savnca>0 and savnca<>knscs
149100101201     c                   seton                                        05
149200101201    2c                   endif
149300101202     c
149400101202     c                   ENDSR
149500940727     C**************************************************************************
149600940727     C*    CARICO IN SUBFILE LINEE DI ARRIVO
149700940727     C**************************************************************************
149800900608     C     CARLNA        BEGSR
149900940727     C*
150000921209     C                   Z-ADD     0             SAVLNA            3 0
150100921209     C                   Z-ADD     0             SAVLNP            3 0
150200900608     C                   Z-ADD     0             B                 3 0
150300920811     C                   Z-ADD     0             SAVKEY
150400160729     c                   clear                   sflrcd
150500160823     c                   clear                   wEndRoll
150600940727     C                   Z-ADD     SAVNRR        NRR2
150700990920     C  N30
150800990920    1CANN07B             DOWLT     15
150900950711     C                   SETOFF                                       901721
151000021206     C                   SETOFF                                       7172
151100021206     c                   Setoff                                       7576
151200930707     C* 01 OFF
151300930707    2C     *IN01         IFEQ      *OFF
151400930707     C* 02 OFF
151500930707    3C     *IN02         IFEQ      *OFF
151600070116     C     SAVNPG        READE(N)  FnBRV01L                               30
151700930707     C*
151800930707   X3C                   ELSE
151900930707     C* 02 ON
152000930707     C* CATEGORIA FV NON E' 9
152100930707    4C     *IN18         IFEQ      *ON
152200140625     C  N03KBRV4         READE(N)  FnBRV05L                               30
152300000118     C   03
152400140625     CANN06KBRV5         READE(N)  FnBRV05L                               30     SENZA SERIE
152500000118     C   03
152600140625     CAN 06KBRV51        READE(N)  FnBRV05L                               30     +SERIE
152700930707    4C                   ENDIF
152800930707    3C                   ENDIF
152900930707     C*
153000930707   X2C                   ELSE
153100930707     C* 01 ON
153200930707     C* 02 OFF
153300930707    3C     *IN02         IFEQ      *OFF
153400070116     C  N03KBRV1         READE(N)  FnBRV01L                               30
153500070116     C   03KBRV2         READE     FnBRV02L                               30
153600930707     C*
153700930707   X3C                   ELSE
153800930707     C* 02 ON
153900070116     C  N03KBRV2         READE(N)  FnBRV01L                               30
154000921229     C*
154100930707    4C     *IN03         IFEQ      '1'
154200070116     C  N06KBRV3         READE(N)  FnBRV01L                               30     SENZA SERIE
154300070116     C   06KBRV31        READE(N)  FnBRV01L                               30     +SERIE
154400930707    4C                   ENDIF
154500930707    3C                   ENDIF
154600930707    2C                   ENDIF
154700921229     C*
154800910930     C* CATEGORIA FV     E' 9
154900920323     C  N01
155000920323     CAN 02
155100920323     CANN18*IN17         DOWEQ     '0'
155200910930     C*
155300140625     C  N03KBRV4         READE(N)  FnBRV05L                               30
155400000118     C   03
155500140625     CANN06KBRV5         READE(N)  FnBRV05L                               30     SENZA SERIE
155600000118     C   03
155700140625     CAN 06KBRV51        READE(N)  FnBRV05L                               30     +SERIE
155800910930     C                   SETON                                        17
155900921229     C   30              EXSR      RICCAT
156000910930     C                   END
156100900528     C*
156200911014     C  N30              EXSR      CONFR
156300971003     C**
156400900608     C* 90 ON - RECORD DA NON SELEZIONARE
156500971003     C  N30
156600971003    1CANN90              DO
156700020905     C*
156800920811     C* D E C O D I F I C H E
156900020905     C*
157000911017     C* LINEA PARTENZA
157100020905     C*
157200971003    3C     BRVLNP        IFNE      SAVLNP
157300930707     C*
157400971003    4C     *IN02         IFEQ      *ON
157500101203     c     deslnp        andne     *blanks
157600941213     C                   MOVEL     DESLNP        V2DLNP
157700930707     C                   ELSE
157800930707     C                   MOVEL     BRVLNP        NUM3
157900930707     C                   EXSR      CAZORG
158000971003    5C     *IN39         IFEQ      *OFF
158100971003    6C     ORGDE5        IFNE      *BLANKS
158200960522     C                   MOVEL     ORGDE5        V2DLNP
158300971003   X6C                   ELSE
158400960522     C                   MOVEL     ORGDES        V2DLNP
158500971003    6C                   END
158600971003   X5C                   ELSE
158700960522     C                   MOVEL     *BLANKS       V2DLNP
158800971003    5C                   END
158900971003    4C                   ENDIF
159000930707     C*
159100941213     C                   MOVEL     BRVLNP        V2CLNP
159200941213     C                   MOVEL     V2CLNP        SAVLNP
159300971003    3C                   END
159400020905     C*
159500911017     C* LINEA ARRIVO
159600020905     C*
159700971003    3C     BRVLNA        IFNE      SAVLNA
159800930707     C*
159900971003    4C     *IN03         IFEQ      *ON
160000101203     c     deslna        andne     *blanks
160100941213     C                   MOVEL     DESLNA        V2DLNA
160200930707     C                   ELSE
160300930714     C                   MOVEL     BRVLNA        NUM3
160400930707     C                   EXSR      CAZORG
160500020207     C                   CLEAR                   LNAMNF
160600971003    5C     *IN39         IFEQ      *OFF
160700021126     C                   MOVE      §OGNTW        WLANTW
160800020207     C**
160900020207     C* MEMORIZZO LNA ESTERE CHE FANNO IL MANIFEST
161000020207     C                   CLEAR                   LNAMNF
161100020207     C     §OGNTW        IFEQ      'EEX'
161200020207     C     §OGNTW        OREQ      'FED'
161300060322     C     §OGNTW        OREQ      'DPD'
161400020207     C                   MOVEL     'S'           LNAMNF            1
161500020207     C                   END
161600971003    6C     ORGDE5        IFNE      *BLANKS
161700960522     C                   MOVEL     ORGDE5        V2DLNA
161800960522     C                   ELSE
161900960522     C                   MOVEL     ORGDES        V2DLNA
162000971003    6C                   END
162100971003   X5C                   ELSE
162200960522     C                   MOVEL     *BLANKS       V2DLNA
162300971003    5C                   END
162400971003    4C                   ENDIF
162500930707     C*
162600941213     C                   MOVEL     BRVLNA        V2CLNA
162700941213     C                   MOVEL     V2CLNA        SAVLNA
162800971003    3C                   END
162900900521     C*
163000950704     C                   MOVEL     BRVNPG        V2HNPG
163100950704     C                   MOVEL     BRVNRS        V2CNRS
163200941213     C                   MOVEL     BRVNSC        V2CNSC
163300950207     C                   CLEAR                   V2CSE                          SEGNACOLLO ESISTENTE
163400941213     C                   MOVEL     BRVZNC        V2CZNC
163500941213     C                   MOVEL     BRVNPS        V2CNPS
163600000119     C                   Z-ADD     BRVPES        V2CPES                         P.O. SPUNTA
163700000119     C     BRVDFS        IFNE      *ZERO
163800160729     C                   Z-ADD     BRVDFS        v2hdfs                         DATA IMM/VARIAZ.
163900160729     C                   Z-ADD     BRVDFS        G02INV                         DATA IMM/VARIAZ.
164000941202     C                   Z-ADD     *ZERO         G02DAT
164100941202     C                   MOVEL     '3'           G02ERR
164200941202     C                   CALL      'XSRDA8'
164300941202     C                   PARM                    WLBDAT
164400000119     C                   Z-ADD     G02DAT        V2CDFS                         GG/MM/AAAA
164500941202     C                   ELSE
164600000119     C                   Z-ADD     *ZERO         V2CDFS
164700160729     C                   Z-ADD     *ZERO         V2hDFS
164800941202     C                   ENDIF
164900000119     C                   Z-ADD     BRVHMS        V2CHMS                         ORA IMM/VARIAZ.
165000070116     c* Millisecondi immissione spunta
165100070116     C                   Z-ADD     BRVmis        V2Cmis                         ORA IMM/VARIAZ.
165200070215     c* Profilo immissione spunte: se inizia per § non lo devo visualizzare
165300070215     c*  (solo per EDP)
165400070215     c                   if        %subst(brvpru:1:1)='§' and
165500070215     c                             %subst(Knmus:1:3)<>'EDP'
165600070215     c                   clear                   v2cpru
165700070215     c                   else
165800070221     c                   if        %subst(brvpru:1:1)='§'
165900070221     c                   eval      v2cpru=%subst(brvpru:2:9)
166000070221     c                   else
166100070221     c                   eval      v2cpru=brvpru
166200070215     c                   endif
166300070221     c                   endif
166400070221     c
166500070116     c* Tipo apparato spunta
166600070116     c                   clear                   v2ctap
166700070116     c                   if        brvtap<>' '
166800070131     C                   CLEAR                   tibs02ds
166900070116     C                   MOVEL     'C'           T02MOD
167000070116     C                   MOVEL     KNSIF         T02SIF
167100070116     C                   MOVEL     'APS'         T02COD
167200070116     C                   MOVEL     BRVTAP        T02KE1
167300070116     C                   CALL      'TIBS02R'
167400070116     C                   PARM                    KPJBA
167500070116     C                   PARM                    tibs02ds
167600070116     c
167700070116     c                   if        t02err<>' '
167800070116     c                   eval      t02uni=*all'?'
167900070116     c                   endif
168000070116     c
168100070116     c                   eval      v2ctap=brvtap+'-'+%subst(t02uni:1:5)
168200070116     c                   endif
168300070116     c
168400000119     C     BRVDCS        IFNE      *ZERO
168500070228     c                   z-add     brvdcs        v2hdcs
168600000119     C                   Z-ADD     BRVDCS        G02INV                         DATA CARICAM.  .
168700000119     C                   Z-ADD     *ZERO         G02DAT
168800000119     C                   MOVEL     '3'           G02ERR
168900000119     C                   CALL      'XSRDA8'
169000000119     C                   PARM                    WLBDAT
169100000119     C                   Z-ADD     G02DAT        V2CDCS                         GG/MM/AAAA
169200000119     C                   SETOFF                                       71
169300000119     C                   ELSE
169400070228     c                   clear                   v2hdcs
169500000119     C                   Z-ADD     *ZERO         V2CDCS
169600000119     C                   SETON                                        71
169700000119     C                   ENDIF
169800000119     C                   Z-ADD     BRVHCS        V2CHCS                         ORA CARICAM.
169900000119     C     BRVDTR        IFNE      *ZERO
170000000119     C                   Z-ADD     BRVDTR        G02INV                         DATA TRASMIS.  .
170100000119     C                   Z-ADD     *ZERO         G02DAT
170200000119     C                   MOVEL     '3'           G02ERR
170300000119     C                   CALL      'XSRDA8'
170400000119     C                   PARM                    WLBDAT
170500000119     C                   Z-ADD     G02DAT        V2CDTR                         GG/MM/AAAA
170600000119     C                   SETOFF                                       72
170700000119     C                   ELSE
170800000119     C                   Z-ADD     *ZERO         V2CDTR
170900000119     C                   SETON                                        72
171000000119     C                   ENDIF
171100000119     C                   Z-ADD     BRVHTR        V2CHTR                         ORA TRASMISS
171200000119     C                   CLEAR                   V2CFTR
171300070116     C**   BRVFTR        IFEQ      'R'
171400070116     C**   'Ricezion'    CAT       '.':0         V2CFTR
171500070116     C**                 ELSE
171600070116     C**   BRVFTR        IFEQ      'T'
171700070116     C**   'Trasmiss'    CAT       '.':0         V2CFTR
171800070116     C**                 ENDIF
171900070116     C**                 ENDIF
172000070131     c                   movel     brvftr        v2cftr
172100941213     C                   MOVE      BRVVUC        V2CVUC                         VOLUME CML
172200990907     C                   MOVE      BRVPUC        V2CPUC                         PESO CML
172300021127     c                   Z-Add     BrvVuc        V2hVuc
172400021127     c                   Z-Add     BrvPuc        V2hPuc
172500081028     c
172600081028     c                   clear                   v2cTSU
172700081028     c***                if        brvtsu<>' '  and
172800081028     c***                          (%subst(Knmus:1:3)= 'EDP'  or
172900081028     c***                          knmus='REA05902' or knmus='RED00603')
173000081028     c* Visualizzo a tutti se collo doppio
173100081028     c                   if        brvtsu>'0'
173200081028     c
173300081028     c                   if        brvtsu='9'
173400081028     c                   movel     BrvTSU        v2cTSU
173500081028     c                   else
173600081028     c                   movel     brvtsu        w0010             1 0
173700081028     c                   add       1             w0010
173800081028     c                   movel     w0010         v2ctsu
173900081028     c                   endif
174000081028     c
174100080317     c                   seton                                        19
174200080317     c                   else
174300080317     c                   setoff                                       19
174400080317     c                   endif
174500080317     c
174600941213     C                   MOVEL     *BLANKS       V2DATR                         TRANSITO
174700961203     C                   SELECT
174800961203     C     BRVATR        WHENEQ    'D'                                          'D'=DISGUIDO TRANSIT
174900941213     C                   MOVEL     'DISGUIDO'    V2DATR
175000961203     C     BRVATR        WHENEQ    'T'                                          'T'=ARRIVO DAL TRANS
175100070116     C                   movel     'ARR.TRAN'    V2DATR
175200070116     C**   BRVATR        WHENEQ    'A'                                          'A'=ATTESA TRANSITO
175300070116     C**   'ATT.TRAN'    CAT       '.':0         V2DATR
175400961203     C     BRVATR        WHENEQ    'F'                                          'F'=DEFLUENZRANSITO
175500070116     C                   movel     'DEFLUENZ'    V2DATR
175600961203     C                   ENDSL
175700950704     C*
175800960506     C***
175900941213     C     V2CNPS        IFNE      SAVNPS
176000980213     C                   MOVEL     '7J'          KCOD
176100950704     C                   MOVEL(P)  BRVNPS        KKEY
176200921229     C     KTAB2         CHAIN     TABEL                              31
176300921229     C  N31              MOVEL     TBLUNI        DS7J
176400921229     C   31              MOVEL     'M'           §7JRPS
176500921229     C   31              MOVEL     'V'           §7JTPS
176600921231     C                   MOVEL     BRVNPS        SAVNPS            2
176700950704     C                   ENDIF
176800990428     C* SE FOGLIO DEFLUENZA EXPORT, EVIDENZIO PISTOLA AUTOMATICA
176900020207     C**  E P.O. CHE FA IL MANIFEST
177000990428     C                   SETOFF                                       86
177100020207     C     LNAMNF        IFEQ      'S'
177200990428     C     §7JRPS        ANDEQ     'A'
177300990428     C     V2DSPG        ANDEQ     'D  '
177400990428     C                   SETON                                        86
177500990428     C                   ENDIF
177600960506     C* SE IN INTERROGAZIONE DETERMINO SE SPUNTA IN GESTIONE A ME
177700960506     C     *IN20         IFEQ      *ON
177800960506     C     BRVFGS        LOOKUP    FG                                     32
177900960506     C  N32              MOVE      'N'           WNOFG
178000960506     C   32              MOVE      ' '           WNOFG
178100960506     C                   END
178200960506     C* SE SPUNTA NON IN GESTIONE A ME E' AMMESSO SOLO L'ANNULLAMENTO
178300960503     C     WNOFG         IFEQ      'N'
178400960503     C                   SETOFF                                       94
178500960503     C                   SETON                                        24
178600960503     C                   END
178700980216     C* SE SPUNTA NON A ME IN GESTIONE CONFRONTO I TER-ARR PER
178800980216     C* DETERMINARE SE PROTEGGERE O NO IL CAMPO DI ANNULLAMENTO
178900980216     C                   MOVE      *BLANKS       WNOGE             1
179000980216     C     WNOFG         IFEQ      'N'
179100980216     C                   CLEAR                   DSLV55
179200980216     C                   MOVEL     'A'           D55TPT
179300980216     C                   MOVEL     WFGS          D55LIN
179400980216     C                   MOVEL     V2CLNP        D55LNP
179500980216     C                   Z-ADD     V2HDFV        D55DRF
179600980216     C                   CALL      'FNLV55R'
179700980216     C                   PARM                    DSLV55
179800980216     C     D55ERR        IFEQ      ' '
179900980216     C                   MOVEL     D55TFA        WFE0
180000980216     C                   ELSE
180100980216     C                   CLEAR                   WFE0
180200980216     C                   ENDIF
180300980216     C                   MOVEL     V2CFGS        D55LIN
180400980216     C                   CALL      'FNLV55R'
180500980216     C                   PARM                    DSLV55
180600980216     C* SE FILIALI GESTIONE HANNO DIFFERENTI TERMINAL DI ARRIVO DEVO
180700980216     C* PROTEGGERE ANCHE IL CAMPO DI ANNULLAMENTO
180800980216     C     D55ERR        IFNE      *BLANKS
180900980216     C     WFE0          ORNE      D55TFA
181000980216     C                   MOVEL     '1'           WNOGE                          SPUNTA NON GEST
181100980216     C                   END
181200980216     C                   ENDIF
181300980213     C*
181400960404     C* SE FOGLIO CHIUSO DI CATEGORIA <> 1 SPROTEGGO CAMPO ANNULLAMENTO
181500970131     C* SE SPUNTA MANUALE   (WCHIUS E' SOLO PER FNFVV CHIUSO)
181600981030     C                   SETOFF                                       66
181700980216     C     WCHIUS        IFEQ      '1'
181800980216     C     WNOGE         ANDEQ     *BLANKS
181900960917     C* SPROTEGGO CAMPO ANNULLAMENTO ANCHE SE SPUNTA NON A ME IN GESTIO
182000960917     C* NE E CATEGORIA <> 1
182100960503     C     WNOFG         OREQ      'N'
182200980216     C     WNOGE         ANDEQ     *BLANKS
182300960917     C     BRVNPG        ANDNE     1
182400960917     C* OPPURE SE SPUNTA NON A ME IN GESTIONE E SPUNTA DI DEFLUENZA
182500960917     C     WNOFG         OREQ      'N'
182600980216     C     WNOGE         ANDEQ     *BLANKS
182700960917     C     §TVDEF        ANDEQ     'S'
182800981030     C* SE FOGLIO DEFLUENZA ABILITO SOLTANTO L'ANNULLAMENTO
182900021205     C*  SE E' SPUNTA MANUALE non cml
183000970131     C     BRVNPG        OREQ      1
183100970131     C     §TVDEF        ANDEQ     'S'
183200981030     C     WNOFG         ANDEQ     *BLANKS
183300970131     C**
183400960503     C     §7JRPS        IFEQ      'M'
183500021205     C     §7JtPS        andne     'C'
183600960404     C                   SETOFF                                       25
183700981030     C   63              SETON                                        66
183800960404     C                   ELSE
183900960404     C                   SETON                                        25
184000960404     C                   END
184100970131     C*
184200960917     C                   ELSE
184300960917     C                   SETON                                        25
184400960503     C                   END
184500021205     C* SE PISTOLA MANUALE non cml POSSO VARIARE TUTTO
184600921229     C  N24§7JRPS        IFEQ      'M'
184700021205     c     §7jtps        andne     'C'
184800920928     C                   SETON                                        94
184900921229     C                   ELSE
185000921229     C                   SETOFF                                       94
185100920928     C                   END
185200930118     C*
185300941213     C                   MOVEL     §7JTPS        V2CTPS
185400941213     C                   MOVEL     §7JRPS        V2CRPS
185500960506     C***
185600911017     C* CODICE ANOMALIA
185700941213     C                   MOVEL     BRVCAN        V2CCAN
185800941213     C                   MOVEL     *BLANKS       V2DCAN
185900920810     C     BRVCAN        IFNE      ' '
186000940727     C                   MOVEL     '3E'          KCOD
186100950704     C                   MOVEL(P)  BRVCAN        KKEY
186200920807     C     KTAB2         CHAIN     TABEL                              31
186300941213     C  N31              MOVEL     TBLUNI        V2DCAN
186400920810     C                   END
186500980210     C* PROTEGGO COD.ANOMALIA SE ="E" (SPUNTA ERRATA)
186600101126     C     V2CCAN        COMP      'E'                                    97
186700090416     c* se anomalia, vedo se presente in BRVE spunte senza anomalia
186800090416     c* Non se spunta "unica"
186900090416     c                   setoff                                       70
187000090420     c
187100101126     c                   if        *in20 and not *in97 and brvcan<>' '
187200090416     C                   Z-ADD     1             XX                3 0
187300090416     C                   MOVEL     V2hNPG        ALF1
187400090416     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
187500090416     c                   if        *in99 and  NST(XX)<>'1'
187600090416     C     kbrve1        chain(n)  fnbrve1l
187700090416     c                   if        %found(fnbrve1l) and e_brvcan=' '
187800090416     c                   seton                                        70
187900090416     c                   endif
188000090416     c                   endif
188100090420     c                   endif
188200941205     C*
188300941205     C*----------------------------------------------------
188400950131     C*
188500941205     C* SE SPUNTA PARTENZA DI ALTRA FILIALE HO IL FOGLIO DI ABBINAMENTO
188600031121     C***                CLEAR                   V2CNPA                         CAT.F.V.ABBINATO
188700031121     C***                CLEAR                   V2CNFA                         F.V.ABBINATO
188800031121IF  3C     V2CNPG        IFEQ      1                                            SOLO PARTENZE
188900031121     C                   MOVEL     V2CFGS        NUM3
189000031121     C     NUM3          LOOKUP    L1                                     31     ON=TROVATA
189100031121DO 3AC  N31              DO
189200031121     C***  'Abbiname'    CAT       'n.':0        V2CABB
189300031121     C                   SETON                                        24        COME FOSSE CHIUSO
189400031121     C                   SETOFF                                       94
189500020905     c*
189600031121E  3AC                   ENDDO
189700941205E   3C                   ENDIF
189800941205     C*----------------------------------------------------
189900941213     C                   Z-ADD     BR1NRR        V2CNRR
190000941213     C  N01
190100941213     CAN 02              Z-ADD     BR3NRR        V2CNRR
190200941213     C   01
190300941213     CANN02
190400941213     CAN 03              Z-ADD     BR2NRR        V2CNRR
190500900608     C*
190600941213     C                   MOVEL     *BLANKS       V2CANN
190700040122      * IN MANUTENZIONE
190800150817     c***                If         *IN20 = *off   and
190900021206      * Se il foglio non è chiuso
191000150817     c***                           Wchius <> '1'  and
191100021206      * Posso inserire e modificare il volume e il peso
191200021206      * se è una spunta arrivi
191300150817     c***                           V2cnpg = 2     and
191400021206      * se la linea di arrivo è estera
191500150817     c***                          (WlaNtw = 'EEX' or WlaNtw = 'EUP' or
191600150817     c***                          WlaNtw = 'DPD' or WlaNtw = 'FED')
191700021206      * se non è una spunta da pistola CML
191800130506     c***                If        §7jTps <> 'C'
191900130506     c***                Eval      *In75 = *On
192000130506     c***                Eval      *In76 = *On
192100021206      * se è una spunta da pistola CML posso solo inserire il peso e il volume
192200130506     c***                Else
192300130506     c***                If        BrvVuc = *Zeros
192400130506     c***                Eval      *In75 = *On
192500130506     c***                EndIf
192600130506     c***                If        BrvPuc = *Zeros
192700130506     c***                Eval      *In76 = *On
192800130506     c***                EndIf
192900130506     c***                EndIf
193000150817     c***                EndIf
193100021126
193200960612     C                   CLEAR                   V2HESI
193300960405     C* Memorizzo indicatori in campi hidden
193400960405     C                   EXSR      SRIND
193500950207     C                   CLEAR                   V2COLD
193600960522     C                   CLEAR                   V2CANM
193700960522     C                   MOVE      RCDSFL        V2HRCD
193800940727     C                   ADD       1             NRR2
193900900608     C                   ADD       1             B
194000941201     C                   WRITE     LR04S02
194100900521     C*
194200900608     C                   END
194300900608     C  N30              END
194400911017     C*
194500911014     C* 30 ON - FINE LETTURA IL RESTO INIZIALIZZO CON ZERO
194600910320     C* SE CARICATO ALMENO UN RECORD VALIDO MEMORIZZO I TASTI DI DUP
194700910419     C  N20
194800910419     CAN 30B             IFGT      0
194900941213     C                   Z-ADD     V2CNFV        DUPNFV
195000941213     C                   Z-ADD     V2CLNP        DUPLNP
195100941213     C                   Z-ADD     V2CLNA        DUPLNA
195200941213     C                   Z-ADD     V2CNRS        DUPNRS
195300941213     C                   Z-ADD     V2CNSC        DUPNSC
195400980210     C     V2CCAN        IFEQ      'E'
195500980210     C                   CLEAR                   DUPCAN
195600980210     C                   ELSE
195700941213     C                   MOVEL     V2CCAN        DUPCAN
195800980210     C                   END
195900980210     C                   MOVEL     V2CNPG        DUPNPG
196000901221     C                   END
196100911017     C*
196200900608     C* SE FINE LETTURA INIZIALIZZO
196300910930     C   18
196400910930     CANN20
196500910930     CAN 30
196600990920     COR 18
196700990920     CANN20
196800990920     CAN 07B             IFLT      15
196900930118     C* PISTOLA --> 00
197000930118     C     SAVNPS        IFNE      '00'
197100940727     C                   MOVEL     '7J'          KCOD
197200940727     C                   MOVEL     *BLANKS       KKEY
197300940727     C                   MOVEL     '00'          KKEY
197400930118     C     KTAB2         CHAIN     TABEL                              31
197500930118     C  N31              MOVEL     TBLUNI        DS7J
197600930118     C   31              MOVEL     'M'           §7JRPS
197700930118     C   31              MOVEL     'V'           §7JTPS
197800930118     C                   MOVEL     '00'          SAVNPS            2
197900930118     C                   END
198000930118     C*
198100990428     C                   SETOFF                                       248663
198200080318     c                   Setoff                                       757619
198300021206     C                   SETON                                        2125
198400941213     C                   MOVEL     §7JRPS        V2CRPS
198500941213     C                   MOVEL     §7JTPS        V2CTPS
198600941213     C                   MOVEL     *BLANKS       V2CANN
198700941213     C                   MOVEL     V1CNPG        V2CNPG
198800950704     C                   CLEAR                   V2HNPG
198900080318     C                   CLEAR                   V2ctsu
199000941213     C                   MOVEL     V1CFGS        V2CFGS
199100941213     C                   Z-ADD     0             V2CNFV
199200941214     C                   Z-ADD     0             V2CDFV                         GG/MM
199300950706     C                   Z-ADD     0             V2HDFV                         AAAA/MM/GG
199400941213     C                   Z-ADD     0             V2CLNP
199500941213     C                   MOVEL     *BLANKS       V2DSPG
199600941213     C                   MOVEL     *BLANKS       V2DLNP
199700941213     C                   Z-ADD     0             V2CLNA
199800941213     C                   MOVEL     *BLANKS       V2DLNA
199900941213     C                   Z-ADD     0             V2CNRS
200000941213     C                   Z-ADD     0             V2CNSC
200100950207     C                   CLEAR                   V2CSE                          SEGNACOLLO ESISTENTE
200200941213     C                   MOVEL     '  '          V2CZNC
200300941213     C                   MOVEL     '  '          V2CNPS
200400941213     C                   MOVEL     *BLANKS       V2CCAN
200500941213     C                   MOVEL     *BLANKS       V2DCAN
200600941213     C                   Z-ADD     *ZERO         V2CDCS                         DATA CARICAMENTO
200700070228     C                   Z-ADD     *ZERO         V2hDCS                         DATA CARICAMENTO
200800941213     C                   Z-ADD     *ZERO         V2CHCS                         ORA CARICAMENTO
200900000119     C                   Z-ADD     *ZERO         V2CDFS                         DATA IMM/VARIAZ.
201000160729     C                   Z-ADD     0             V2HDFs                         AAAA/MM/GG
201100000119     C                   Z-ADD     *ZERO         V2CHMS                         ORA IMM/VARIAZ.
201200070130     C                   Z-ADD     *ZERO         V2Cmis                         ORA IMM/VARIAZ.
201300000119     C                   Z-ADD     *ZERO         V2CDTR                         DATA TRASMISIAZ.
201400000119     C                   Z-ADD     *ZERO         V2CHTR                         ORA TRASMISSAZ.
201500000119     C                   CLEAR                   V2CFTR
201600070130     C                   CLEAR                   V2Cpru
201700070130     C                   CLEAR                   V2Ctap
201800000119     C                   SETON                                        7172
201900021016     C                   movel     v2cfgs        V2CPES
202000941213     C                   MOVEL     *BLANKS       V2DATR                         TRANSITO
202100941213     C                   MOVEL     *BLANKS       V2CABB                         ABBINAMENTO
202200031121     C***                Z-ADD     *ZERO         V2CNPA                         CATEG.FOG.ABB.
202300031121     C***                Z-ADD     *ZERO         V2CNFA                         FOGLIO ABBINAATO
202400941213     C                   Z-ADD     *ZERO         V2CVUC                         VOLUME
202500990907     C                   Z-ADD     *ZERO         V2CPUC                         PESO
202600021127     c                   Clear                   V2hVuc
202700021127     c                   Clear                   V2hPuc
202800941213     C                   Z-ADD     0             V2CNRR
202900950207     C                   CLEAR                   V2COLD
203000960522     C                   CLEAR                   V2CANM
203100960612     C                   CLEAR                   V2HESI
203200960522     C                   MOVE      RCDSFL        V2HRCD
203300930118     C*
203400960612     C                   EXSR      SRIND
203500960612     C*
203600990920     C     B             DOWLT     15
203700940727     C                   ADD       1             NRR2
203800900608     C                   ADD       1             B
203900941201     C                   WRITE     LR04S02
204000960612     C                   ENDDO
204100930118     C                   END
204200930118     C*
204300940727     C                   Z-ADD     NRR2          SAVNRR            4 0
204400940727     C                   Z-ADD     NRR2          REC2
204500911014     C                   SETOFF                                       21
204600160823     c* Se non in manutenzione ed è fine lettura imposto campo di fine rollup
204700160823     c* utilizzato in caso di richiesta di ordinamento sfl per data
204800160823     c                   if        *in20 and b<15
204900160823     c                   eval      wEndRoll='F'
205000160728
205100160728     c                   clear                   rrnlast
205200160728     c                   if        nrr2>*zero
205300160822     c                   eval      rrnlast=nrr2
205400160728     c                   endif
205500160728     c* vedo se devo ordinare il sfl
205600160728     c                   if        w_ord<>w_nfv
205700160728     c                   exsr      sr_ordina
205800160728     c                   endif
205900160823     c                   endif
206000900509     C*
206100900518     C                   ENDSR
206200940727     C**************************************************************************
206300940727     C* LEGGO NUOVA CATEGORIA
206400940727     C**************************************************************************
206500921229     C     RICCAT        BEGSR
206600940727     C*
206700921229     C                   Z-ADD     0             SAVNPG
206800101201     C                   ADD       1             CC
206900101201     c                   if        cc<=20 and cator(CC)<>*blanks
207000101201     C                   MOVE      CATor(CC)     SAVNPG
207100921229     C* TORNO A LEGGERE CON NUOVA CATEGORIA
207200140625     C  N03KBRV4         SETLL     FnBRV05L
207300000118     C   03
207400140625     CANN06KBRV5         SETLL     FnBRV05L
207500000118     C   03
207600000118     CAN 06
207700140625     CANN04KBRV51        SETLL     FnBRV05L
207800000118     C   03
207900000118     CAN 06
208000140625     CAN 04KBRV52        SETLL     FnBRV05L
208100921229     C                   SETOFF                                       17
208200921229     C*
208300921229     C                   ELSE
208400921229     C*
208500921229     C                   MOVEL     9             SAVNPG
208600921229     C                   END
208700921229     C                   ENDSR
208800940727     C**************************************************************************
208900940727     C* PULIZIA SUBFILE
209000940727     C**************************************************************************
209100900518     C     PULSF         BEGSR
209200940727     C*
209300900518     C                   SETON                                        35
209400941201     C                   WRITE     LR04C02                                      SFLCTL
209500900518     C                   SETOFF                                       35
209600940727     C*
209700900518     C                   ENDSR
209800940727     C**************************************************************************
209900940727     C* CONTROLLO DATI SUBFILE
210000940727     C**************************************************************************
210100900608     C     CONTR2        BEGSR
210200940727     C*
210300940727     C                   Z-ADD     1             NRR2
210400910320     C                   SETOFF                                       9091
210500911014     C*
210600910320     C                   SETOFF                                       32
210700941201     C                   READC     LR04S02                                31
210800930707     C*
210900930707    1C     *IN31         DOWEQ     *OFF
211000960405     C* Imposto indicatori memorizzati nei campi hidden
211100960405     C                   EXSR      SRIND1
211200960405     C*
211300950711     C   10              Z-ADD     DUPNFV        V2CNFV
211400950711     C   11              Z-ADD     DUPLNP        V2CLNP
211500950711     C   12              Z-ADD     DUPLNA        V2CLNA
211600950711     C   13              Z-ADD     DUPNRS        V2CNRS
211700950711     C   14              Z-ADD     DUPNSC        V2CNSC
211800950711     C   11
211900950711     COR 12
212000950711     COR 13
212100950711     COR 14              CLEAR                   V2CSE                          SEGNACOLLO ESISTENTE
212200980210     C   15DUPCAN        IFNE      'E'
212300980210     C                   MOVEL     DUPCAN        V2CCAN
212400980210     C                   ELSE
212500980210     C                   CLEAR                   V2CCAN
212600980210     C                   END
212700950711     C   16              MOVEL     DUPNPG        V2CNPG
212800911017     C*
212900930707    2C     RECORD        IFNE      *ZEROS
213000960522     C     RCDSFL        ANDNE     V2HRCD
213100960628     C     V2CNFV        ORNE      *ZEROS
213200960628     C     RCDSFL        ANDNE     V2HRCD
213300900709     C*
213400900709     C*
213500910320     C* CONTROLLO SE RICHIESTO RICERCA CODICE ANOMALIA
213600941213    3C     V2CCAN        IFEQ      '?'
213700940727     C                   MOVEL     CODUT         §KUT
213800940727     C                   MOVEL     '3E'          §COD
213900941213     C                   MOVE      *BLANKS       V2DCAN
214000941213     C                   MOVE      *BLANKS       V2CCAN
214100910320     C                   MOVE      *BLANKS       §KEY
214200910320     C                   CALL      'X§TABER'
214300031119     C                   PARM                    §KUT
214400910320     C                   PARM                    §COD
214500940727     C                   PARM                    §KEY
214600940727     C                   PARM                    §DES
214700941213     C                   MOVEL     §KEY          V2CCAN
214800941213     C                   MOVEL     §DES          V2DCAN
214900910320     C                   SETON                                        9131
215000910320     C                   GOTO      ENDCT2
215100930707    3C                   END
215200911017     C* C O N T R O L L I
215300920930     C**
215400920930     C* IMPOSSIBILE CANCELLARE SE RECORD DI NUOVA IMMISSIONE
215500941213    3C     V2CNRR        IFEQ      0
215600941213     C     V2CANN        ANDNE     *BLANKS
215700920930     C                   SETON                                        479031
215800920930     C                   GOTO      ENDCT2
215900930707    3C                   END
216000920807     C* CATEGORIA
216100941213    3C     V2CANN        IFEQ      ' '
216200020808     C                   Z-ADD     1             XI
216300941213     C                   MOVEL     V2CNPG        ALF1              1
216400020808     C     ALF1          LOOKUP    CAT(XI)                                32
216500920807     C* ERRORE
216600920807     C  N32              SETON                                        469031
216700920807     C  N32              GOTO      ENDCT2
216800021205     C* SE SPUNTA NON MANUALE o manuale non cml CONSEGNA, POSSO PASSARE SOLO ALLA
216900950704     C*  CATEGORIA 3
217000950705     C     V2CRPS        IFNE      'M'
217100021205     C     V2CRPS        oreq      'M'
217200021205     C     V2CtPS        andne     'C'
217300961202     C     V2HNPG        IFEQ      4
217400000726     C     V2HNPG        OREQ      8
217500000727     C     V2CNPG        IFNE      3
217600950705     C     V2CNPG        ANDNE     4
217700000726     C     V2CNPG        ANDNE     8
217800950704     C                   SETON                                        899031
217900950705     C                   GOTO      ENDCT2
218000950704     C                   ENDIF
218100000727     C                   ENDIF
218200961202     C     V2HNPG        IFEQ      1
218300961202     C     V2CNPG        ANDNE     3
218400961202     C     V2CNPG        ANDNE     1
218500961202     C                   SETON                                        879031
218600961202     C                   GOTO      ENDCT2
218700961202     C                   ENDIF
218800961202     C     V2HNPG        IFEQ      3
218900961202     C     V2CNPG        ANDNE     1
219000961202     C     V2CNPG        ANDNE     3
219100961202     C                   SETON                                        879031
219200961202     C                   GOTO      ENDCT2
219300961202     C                   ENDIF
219400961202     C                   ENDIF
219500941202     C*----------------------------------------------------
219600941202     C* LINEA PARTENZA
219700941213    4C     V2CLNP        IFEQ      0
219800941202     C                   SETON                                        429031
219900941202     C                   GOTO      ENDCT2
220000941202   X4C                   ELSE
220100941213     C                   MOVEL     V2CLNP        NUM3
220200941202     C                   EXSR      CAZORG
220300941202     C*
220400941202     C   39              SETON                                        429031
220500941202     C   39              GOTO      ENDCT2
220600941202     C*
220700001205     C                   MOVEL     ORGDE3        OG143
220800960522     C     ORGDE5        IFNE      *BLANKS
220900960522     C                   MOVEL     ORGDE5        V2DLNP
221000960522     C                   ELSE
221100941213     C                   MOVEL     ORGDES        V2DLNP
221200960522     C                   END
221300941202    4C                   END
221400941202     C*----------------------------------------------------
221500900608     C* FOGLIO VIAGGIO
221600941202     C* (FARE DOPO IL CONTROLLO SU LNP)
221700941202     C*----------------------------------------------------
221800941213    4C     V2CNFV        IFEQ      0
221900910323     C                   SETON                                        419031
222000900608     C                   GOTO      ENDCT2
222100941202    4C                   ENDIF
222200941202     C*
222300941202     C* PER CATEGORIA <> 1 CERCO FRA GLI ARRIVI
222400941213     C     V2CNPG        IFNE      1                                            1=PARTENZE
222500941202     C*
222600961202     C*?    F O G L I     V A R I      ?
222700941213     C                   MOVEL     V2CNPG        KNPG                           ..CATEGORIA
222800941213     C                   Z-ADD     V2CNFV        KNFV                           ..NR.FOGLIO
222900941213     C                   MOVEL     V2CFGS        KFGS                           ..FIL.GESTIONE
223000941201     C     KFVV1         CHAIN     FNFVV01L                           41
223100930707    5C  N41FVVATB        IFNE      *BLANKS
223200900626     C                   SETON                                        41        ANNULLATO
223300930707    5C                   END
223400900626     C*
223500910323     C   41              SETON                                        9031
223600900626     C   41              GOTO      ENDCT2
223700920807     C*
223800930423     C* NON E' FOGLIO GESTIBILE: QUESTO CONTROLLO NON LO ESEGUO SE
223900930423     C*   SI TRATTA DI FOGLIO I.M.P.
224000941201    5C     V1CNPG        IFNE      '3'
224100941201     C     V1CNPG        OREQ      '3'
224200930423     C     FVVSPG        ANDNE     'P'
224300930423     C*
224400941201     C     FVVFGS        LOOKUP    L6                                     32
224500920807     C  N32              SETON                                        499031
224600920807     C  N32              GOTO      ENDCT2
224700930707    5C                   END
224800920807     C*
224900911017     C* CHIUSO
225000001128     C* SE FOGLIO CAT 4 DEVO CONTROLLARE LA BOLLA
225100001128     C*  ALTRIMENTI SEMPRE ERRORE
225200001128     C     V2CNPG        IFNE      4
225300001128    5C     FVVFCF        ANDEQ     'S'
225400911014     C                   SETON                                        529031     CHIUSO
225500910320     C                   GOTO      ENDCT2
225600930707    5C                   END
225700911014     C*
225800920520     C* DATA FOGLIO VIAGGIO
225900950706     C                   Z-ADD     FVVDFV        V2HDFV                         AAAA/MM/GG
226000921229     C                   MOVE      FVVDFV        DATA
226100941213     C                   MOVE      MM            V2CDFV
226200941213     C                   MOVEL     GG            V2CDFV
226300950131     C*
226400950131     C* PER CATEGORIA 4=CONSEGNE METTO A VIDEO PARTE DELLA DESCRIZIONE
226500950131    5C     FVVNPG        IFEQ      4                                            4=CONSEGNE
226600000713    5C     FVVNPG        OREQ      8                                            4=CONSEGNE
226700950131     C                   MOVEL     FVVDSF        V2CABB
226800950704     C                   SETON                                        23
226900950704     C                   ELSE
227000961202     C                   CLEAR                   V2CABB
227100961202     C*
227200961202     C* LA CATEGORIA 3 PUO' ANDARE A 1 E VICEVERSA
227300961202     C     FVVNPG        IFEQ      3
227400961202     C                   SETON                                        23
227500961202     C                   ELSE
227600950704     C                   SETOFF                                       23
227700961202     C                   ENDIF
227800950131    5C                   ENDIF
227900920520     C*
228000911014     C* DECODIFICO TIPO INVENTARIO
228100941213     C                   MOVEL     *BLANKS       V2DSPG
228200930707    5C     FVVSPG        IFNE      ' '
228300020808     C                   Z-ADD     1             XI
228400020808     C     FVVSPG        LOOKUP    SPG(XI)                                32
228500161006     c***32              eval      v2dspg='IM'+ %subst(DES(XI):1:1)
228600161006     C   32              MOVEL     DES(XI)       V2DSPG
228700930707    5C                   END
228800941202     C*
228900941202     C                   ELSE
229000950705     C*
229100950705     C* LA MANUTENZIONE DEI FV PARTENZA CONCESSA SOLO A SIMFEL
229200950705     C     *IN20         IFEQ      *OFF
229300950705     C     V1CFGS        ANDNE     SIMFEL
229400950705     C                   SETON                                        499031     CHIUSO
229500950705     C                   GOTO      ENDCT2
229600950705     C                   ENDIF
229700941202     C*
229800941213     C                   MOVEL     *BLANKS       V2DSPG                         TIPO INVENTARIO
229900950704     C                   CLEAR                   V2CABB
230000941202     C*
230100941202     C*?    F O G L I O    P A R T E N Z E    ?
230200941202     C*
230300941213     C                   Z-ADD     V2CNFV        KNFV                           ..NR.FOGLIO
230400020920     C                   movel     v2cfgs        KFGS                           ..P.O.GESTIONE
230500020808     C     K02FGV01      chain     FNFGV01L                           41
230600941202     C   41              SETON                                        9031
230700941202     C   41              GOTO      ENDCT2
230800000503     C* ESTENSIONE
230900020808     C     K02FGV01      chain     FNFGW01L                           34
231000000503     C     *IN34         IFEQ      *ON
231100000503     C     FGWATB        ORNE      ' '
231200000503     C                   CLEAR                   FGWFF3
231300000503     C                   CLEAR                   FGWFF3
231400000503     C                   CLEAR                   FGWFL3
231500000503     C                   CLEAR                   FGWFL3
231600000503     C                   ENDIF
231700941202     C* DATA FOGLIO VIAGGIO
231800950706     C                   Z-ADD     FGVDFV        V2HDFV                         AAAA/MM/GG
231900941202     C                   MOVE      FGVDFV        DATA
232000941213     C                   MOVE      MM            V2CDFV
232100941213     C                   MOVEL     GG            V2CDFV
232200960829     C* VISUALIZZO SE FV DI DEFLUENZA O LOCALE
232300960829     C                   MOVE      FGVTTR        WTTR              1
232400101202     C***                MOVE      FGWFGS        WPT               3 0
232500960829     C                   EXSR      RICTTR
232600961202     C*
232700961202     C* LA CATEGORIA 1 LA POSSO PORTARE A 3 E VICEVERSA
232800961202     C                   SETON                                        23
232900040316     c* errore se foglio chiuso
233000040316     c     fgvfcf        ifeq      'S'
233100040316     C                   SETON                                        529031     CHIUSO
233200040316     C                   GOTO      ENDCT2
233300040316     c                   endif
233400941202     C*
233500941202     C                   ENDIF
233600941202     C*----------------------------------------------------
233700900608     C* LINEA ARRIVO
233800941213    4C     V2CLNA        IFEQ      0
233900910323     C                   SETON                                        439031
234000900608     C                   GOTO      ENDCT2
234100930707   X4C                   ELSE
234200941213     C                   MOVEL     V2CLNA        NUM3
234300930707     C                   EXSR      CAZORG
234400910320     C*
234500930707     C   39              SETON                                        439031
234600930707     C   39              GOTO      ENDCT2
234700960522     C     ORGDE5        IFNE      *BLANKS
234800960522     C                   MOVEL     ORGDE5        V2DLNA
234900960522     C                   ELSE
235000941213     C                   MOVEL     ORGDES        V2DLNA
235100960522     C                   END
235200020207     C                   MOVE      §OGNTW        WLANTW
235300941213     C*
235400941213     C* PER LE PARTENZE, LA LNA DEVE ESSERE FRA LE LINEE DI SCARICO
235500941213    5C     V2CNPG        IFEQ      1                                            1=PARTENZE
235600941213    6C     V2CLNA        IFNE      FGVLNA
235700971003     C                   MOVEL     V2CLNA        ALF3              3
235800941213     C     ALF3          LOOKUP    FFV1                                   39
235900941213     C  N39              SETON                                        439050
236000941213     C  N39              GOTO      ENDCT2
236100941213    6C                   ENDIF
236200941213    5C                   ENDIF
236300941213     C*
236400930707    4C                   END
236500941202     C*----------------------------------------------------
236600900608     C* SEGNACOLLO
236700941213    4C     V2CNSC        IFEQ      0
236800910323     C                   SETON                                        449031
236900900608     C                   GOTO      ENDCT2
237000930707    4C                   END
237100900608     C* CONTROLLO SE SEGNACOLLO DOPPIO
237200941227     C  NKN
237300941227     CANNKP              MOVEL     V2CFGS        KFGS                           FIL.GESTIONE
237400000119     C  NKN
237500140625     CANNKPKBRV6         CHAIN(N)  FnBRV05L                           32
237600941213     C  NKN
237700941213     CANNKP
237800941213    4CANN32BR3NRR        IFNE      V2CNRR
237900920320     C                   SETON                                        459031
238000900608     C                   GOTO      ENDCT2
238100930707    4C                   END
238200950207     C*
238300950207     C* VERIFICO L'ESISTENZA DEL SEGNACOLLO NELLE BOLLE;
238400950207     C* SE NON ESISTE, SEGNALO ERRORE MA POSSIBILE FORZATURA
238500950207     C     RECORD        IFNE      V2COLD
238600950207     C                   MOVEL     RECORD        V2COLD                         MEMORIZZO IL COLLO
238700950207     C                   CLEAR                   V2CSE
238800950207     C                   ENDIF
238900021028     c     v2clna        lookup    l1                                     34
239000021028     c* Solo se si tratta di mia bolla arrivi do eventuale errore su
239100021028     c*  foglio consegna chiuso senza rec bolla
239200021028    0c                   if        *in34
239300021028     C     KART          setll     FNART27L                               32     ON=TROVATO
239400001128    1C     V2CNPG        IFEQ      4
239500001128     C     FVVFCF        ANDEQ     'S'
239600001128    2C     *IN32         IFEQ      *OFF
239700001128     C                   SETON                                        529031     CHIUSO
239800001128     C                   GOTO      ENDCT2
239900001128     C*
240000001128   X2C                   ELSE
240100001128     C*
240200001128     C* BOLLA TROVATA: CERCO IN NUMERO DISTINTA SE = AL FOGLIO CHIUSO
240300021127     C     KART          CHAIN(n)  FNART27L                           34
240400101201     C  N34Kblp          CHAIN     FNARB01L                           35
240500001128    3C     *IN34         IFEQ      *ON
240600001128     C     *IN35         OREQ      *ON
240700101202     C     blpDCM        OREQ      0
240800101202     C     blpNDC        ORNE      FVVNFV
240900101202     C     blpifp        ORNE      FVVfgs
241000090416     C                   SETON                                        569031     CHIUSO
241100001128    3C                   ENDIF
241200001128    2C                   ENDIF
241300001128    1C                   ENDIF
241400021028   x0c                   else
241500021028     c* 32 off - do per non trovata la bolla arrivi
241600021028     c                   setoff                                       32
241700021028    0C                   ENDIF
241800021028     c* Controllo in assoluto l'esistenza della bolla o in blt o in btt
241900021028     c* tanto devo dare errore comunque se non lo trovo da nessuna
242000021028     c*  parte. non devo leggere solo le mie bolle (part o trans)
242100021028     C  N32KART          SETLL     FNBLT27L                               32     ON=TROVATO
242200150623     C  N32KART          SETLL     FNart27L                               32      ON=TROVATO
242300960612     C*
242400960612     C     *IN32         IFEQ      *OFF
242500960612     C                   MOVEL     'N'           V2HESI                          NON ESISTE
242600960612     C                   ELSE
242700960612     C                   MOVEL     'S'           V2HESI                          ESISTE
242800960612     C                   ENDIF
242900960612     C*
243000021028     c**  non trovata bolla:
243100001205     C     *IN32         IFEQ      *OFF
243200001205     C     *INKN         ANDEQ     *OFF
243300001205     C* NEGLI ALTRI CASI ERRORE FORZABILE
243400001205     C     V2CSE         IFEQ      *BLANKS
243500950207     C                   MOVEL     '1'           V2CSE
243600950207     C                   SETON                                        649031
243700950207     C                   GOTO      ENDCT2
243800950207     C                   ENDIF
243900001205     C                   ENDIF
244000021028     C***                ENDIF
244100911014     C*
244200910320     C* CONTROLLO CODICE ANOMALIA
244300941213    4C     V2CCAN        IFNE      *BLANKS
244400940727     C                   MOVEL     '3E'          KCOD
244500940727     C                   MOVEL     *BLANKS       KKEY
244600941213     C                   MOVEL     V2CCAN        KKEY
244700920807     C     KTAB2         CHAIN     TABEL                              32
244800040625     c  n32              movel     tbluni        ds3e
244900040625     c                   if        *in32 or tblflg<>' '  or §3euti='N'
245000910323     C   32              SETON                                        539031
245100910320     C   32              GOTO      ENDCT2
245200941213     C                   MOVEL     TBLUNI        V2DCAN
245300040625     c                   endif
245400980210     C* CODICE ANOMALIA 'E' NON UTILIZZABILE
245500980210     C     V2CCAN        IFEQ      'E'
245600980210     C                   SETON                                        519031
245700980210     C                   GOTO      ENDCT2
245800980210     C                   END
245900930707    4C                   END
246000920811     C*
246100900608     C* SE NON CI SONO ERRORI AGGIORNO E VADO AVANTI A LEGGERE
246200941213     C                   Z-ADD     V2CNPG        DUPNPG
246300941213     C                   Z-ADD     V2CNFV        DUPNFV
246400941213     C                   Z-ADD     V2CLNP        DUPLNP
246500941213     C                   Z-ADD     V2CLNA        DUPLNA
246600941213     C                   Z-ADD     V2CNRS        DUPNRS
246700941213     C                   Z-ADD     V2CNSC        DUPNSC
246800941213     C                   MOVEL     V2CCAN        DUPCAN
246900930707    3C                   END
247000910323     C     ENDCT2        TAG
247100921229     C* NUOVO INSERIMENTO
247200941213    3C     V2CNRR        IFEQ      0
247300941213     C                   MOVEL     '00'          V2CNPS
247400930707    3C                   END
247500021206
247600021206      * Posso inserire e modificare il volume e il peso
247700021206      * se è una spunta arrivi
247800150817     c***                If         V2cnpg = 2 and
247900021206      * se la linea di arrivo è estera
248000150817     c***                          (WlaNtw = 'EEX' or WlaNtw = 'EUP' or
248100150817     c***                          WlaNtw = 'DPD' or WlaNtw = 'FED')
248200021206      * se non è una spunta da pistola CML
248300150817     c***                If        V2cTps <> 'C'
248400150817     c***                Eval      *In75 = *On
248500150817     c***                Eval      *In76 = *On
248600021206      * se è una spunta da pistola CML posso solo inserire il peso e il volume
248700150817     c***                Else
248800150817     c***                If        V2hVuc = *Zeros
248900150817     c***                Eval      *In75 = *On
249000150817     c***                EndIf
249100150817     c***                If        V2hPuc = *Zeros
249200150817     c***                Eval      *In76 = *On
249300150817     c***                EndIf
249400150817     c***                EndIf
249500150817     c***                Endif
249600921228     C*
249700921229     C*
249800021205     C* SE PISTOLA MANUALE non cml POSSO VARIARE TUTTO
249900970131    3C  N24V2CRPS        IFNE      'M'
250000021205     c     v2crps        oreq      'M'
250100021212     c     v2ctps        andeq     'C'
250200931122     C* IMPOSSIBILE ANNULLARE SPUNTA SE INSERITA CON PISTOLA BUONA
250300941213     C     V2CANN        IFNE      ' '
250400941213     C     V2CRPS        ANDNE     'A'
250500931122     C                   SETON                                        629031
250600931122     C                   ENDIF
250700930707    3C                   END
250800960612     C*
250900970131   X2C                   ELSE
251000960612     C*
251100970131    3C     V2CNFV        IFEQ      *ZEROS
251200960612     C                   CLEAR                   V2DLNP
251300960612     C                   CLEAR                   V2DLNA
251400960612     C                   CLEAR                   V2DSPG
251500960612     C                   CLEAR                   V2CDFV
251600960612     C                   CLEAR                   V2HDFV
251700960612     C                   CLEAR                   V2DCAN
251800960612     C                   CLEAR                   V2HESI
251900960612     C                   MOVEL     RECORD        V2COLD
252000970131    3C                   END
252100950711    2C                   END
252200911017     C*
252300940727     C                   Z-ADD     NRR2          REC2
252400910323     C                   SETON                                        36
252500960612     C                   EXSR      SRIND
252600941201     C                   UPDATE    LR04S02
252700021206     C                   SETOFF                                       3621
252800930707     C*
252900930707     C                   SETOFF                                       32
253000941201     C  N31              READC     LR04S02                                31
253100930707    1C                   END
253200911017     C*
253300900614     C                   ENDSR
253400940727     C**************************************************************************
253500940727     C* CONTROLLO PER INSERIMENTO SPUNTE DAL AL
253600940727     C**************************************************************************
253700920323     C     CTRDAL        BEGSR
253800940727     C*
253900920323     C                   SETOFF                                       90
254000920323     C*
254100920807     C* CATEGORIA
254200941201     C     V3CNPG        IFEQ      ' '
254300920807     C                   SETON                                        4890
254400920807     C                   GOTO      ENDAGG
254500920807     C                   END
254600920807     C* RICERCA IN TABELLA
254700941201     C     V3CNPG        IFEQ      '?'
254800920807     C                   MOVEL     CODUT         §KUT
254900920807     C                   MOVEL     '7N'          §COD
255000941201     C                   MOVE      ' '           V3CNPG
255100920807     C                   MOVEL     *BLANKS       §KEY
255200920807     C                   CALL      'X§TABER'
255300940727     C                   PARM                    §KUT
255400940727     C                   PARM                    §COD
255500940727     C                   PARM                    §KEY
255600940727     C                   PARM                    §DES
255700941201     C                   MOVEL     §KEY          V3CNPG
255800920807     C                   SETON                                        90
255900941205     C                   MOVEL     'S'           WRIEM             1            RIEMETTO TESTATA
256000920807     C                   GOTO      ENDAGG
256100920807     C                   END
256200920807     C* CONTROLLO
256300020808     C                   Z-ADD     1             XI
256400020808     C     V3CNPG        LOOKUP    CAT(XI)                                32
256500920807     C*
256600920807     C  N32              SETON                                        9048
256700920807     C  N32              GOTO      ENDAGG
256800941202     C*----------------------------------------------------
256900941202     C* LINEA PARTENZA
257000941202     C     V3CLNP        IFEQ      0
257100941202     C                   SETON                                        4290
257200941202     C                   GOTO      ENDAGG
257300941202     C                   ELSE
257400941202     C                   MOVEL     V3CLNP        NUM3
257500941202     C                   EXSR      CAZORG
257600941202     C*
257700941202     C   39              SETON                                        4290
257800941202     C   39              GOTO      ENDAGG
257900941202     C*
258000960522     C     ORGDE5        IFNE      *BLANKS
258100960522     C                   MOVEL     ORGDE5        V3DLNP
258200960522     C                   ELSE
258300941202     C                   MOVEL     ORGDES        V3DLNP
258400960522     C                   END
258500941202     C                   END
258600941202     C*----------------------------------------------------
258700920323     C* FOGLIO VIAGGIO
258800941202     C* (FARE DOPO IL CONTROLLO SU LNP)
258900941202     C*----------------------------------------------------
259000941201     C     V3CNFV        IFEQ      0
259100920323     C                   SETON                                        4190
259200920323     C                   GOTO      ENDAGG
259300941202     C                   ENDIF
259400941202     C*
259500941202     C                   MOVEL     *BLANKS       V3DSPG
259600941202     C*
259700941202     C* PER CATEGORIA <> 1 CERCO FRA GLI ARRIVI
259800941202     C     V3CNPG        IFNE      '1'                                          1=PARTENZE
259900941202     C*
260000941202     C*?    F O G L I O    A R R I V I    ?
260100941201     C                   MOVEL     V3CNPG        KNPG                           ..CATEGORIA
260200941201     C                   Z-ADD     V3CNFV        KNFV                           ..NR.FOGLIO
260300941201     C                   Z-ADD     V1CFGS        KFGS                           ..FIL.GESTIONE
260400941201     C     KFVV1         CHAIN     FNFVV01L                           41
260500920323     C  N41FVVATB        IFNE      *BLANKS
260600920323     C                   SETON                                        41        ANNULLATO
260700920323     C                   END
260800920323     C*
260900920323     C   41              SETON                                        90
261000920323     C   41              GOTO      ENDAGG
261100920323     C* CHIUSO
261200941229     C     FVVFCF        IFEQ      'S'
261300950207     C                   SETON                                        5290       CHIUSO
261400920323     C                   GOTO      ENDAGG
261500920323     C                   END
261600930423     C*
261700930423     C* NON GESTIBILE: QUESTO CONTROLLO NON LO ESEGUO SE SI TRATTA
261800930423     C*   DI FOGLIO I.M.P.
261900941201     C     V1CNPG        IFNE      '3'
262000941201     C     V1CNPG        OREQ      '3'
262100930423     C     FVVSPG        ANDNE     'P'
262200930423     C*
262300941201     C     FVVFGS        LOOKUP    L6                                     32
262400920807     C  N32              SETON                                        5390
262500920807     C  N32              GOTO      ENDAGG
262600930423     C                   END
262700920323     C*
262800920323     C* DECODIFICO TIPO INVENTARIO
262900920810     C     FVVSPG        IFNE      ' '
263000020808     C                   Z-ADD     1             XI
263100020808     C     FVVSPG        LOOKUP    SPG(XI)                                32
263200020808     C   32              MOVEL     DES(XI)       V3DSPG
263300920323     C                   END
263400941202     C*
263500941202     C                   ELSE
263600941202     C*
263700941202     C*?    F O G L I O    P A R T E N Z E    ?
263800941202     C                   Z-ADD     V3CNFV        KNFV                           ..NR.FOGLIO
263900020920     C                   z-add     v1cfgs        KFGS                           ..P.O.GESTIONE
264000020808     C     K02FGV01      chain     FNFGV01L                           41
264100941202     C   41              SETON                                        90
264200941202     C   41              GOTO      ENDAGG
264300941202     C*
264400920810     C                   END
264500941202     C*----------------------------------------------------
264600920323     C* LINEA ARRIVO
264700941201     C     V3CLNA        IFEQ      0
264800920323     C                   SETON                                        4390
264900920323     C                   GOTO      ENDAGG
265000920323     C                   ELSE
265100941201     C                   MOVEL     V3CLNA        NUM3
265200930707     C                   EXSR      CAZORG
265300920323     C*
265400930707     C   39              SETON                                        4390
265500930707     C   39              GOTO      ENDAGG
265600960522     C     ORGDE5        IFNE      *BLANKS
265700960522     C                   MOVEL     ORGDE5        V3DLNA
265800960522     C                   ELSE
265900941201     C                   MOVEL     ORGDES        V3DLNA
266000960522     C                   END
266100920323     C                   END
266200920323     C* SEGNACOLLO  DAL
266300941201     C     V3CNSD        IFEQ      0
266400920323     C                   SETON                                        4490
266500920323     C                   GOTO      ENDAGG
266600920323     C                   END
266700920323     C* SEGNACOLLO  DAL
266800941201     C     V3CNSA        IFEQ      0
266900920323     C                   SETON                                        4590
267000920323     C                   GOTO      ENDAGG
267100920323     C                   END
267200920323     C* AL < DAL
267300941201     C     V3CNSA        IFLT      V3CNSD
267400920323     C                   SETON                                        4690
267500920323     C                   GOTO      ENDAGG
267600920323     C                   END
267700920323     C* CONTROLLO SE DIFFERENZA CORRISPONDE AL NUMERO IMMESSO
267800941201     C     V3CNSA        SUB       V3CNSD        DIFF              5 0
267900920323     C                   ADD       1             DIFF
268000941201     C     DIFF          IFNE      V3CNUM
268100920323     C                   SETON                                        4790
268200920323     C                   GOTO      ENDAGG
268300920323     C                   END
268400920323     C*
268500920323     C     ENDAGG        ENDSR
268600940727     C**************************************************************************
268700940727     C* CONFRONTO RECORD LETTI PER VEDERE SE SELEZIONARLI
268800940727     C**************************************************************************
268900900608     C     CONFR         BEGSR
269000900626     C*
269100941212     C* SOLO UNA FILIALE GESTIONE, SE IN GESTIONE
269200941212     C     BRVFGS        IFNE      V1CFGS
269300941202     C     V1CFGS        ANDNE     *ZERO
269400941202     C                   SETON                                        90
269500941202     C                   GOTO      ENDCON
269600941202     C                   ENDIF
269700941212     C*
269800101201     c                   if        v1clna>0   and brvlna<>v1clna
269900900608     C                   SETON                                        90
270000900608     C                   GOTO      ENDCON
270100900608     C                   END
270200900608     C* SERIE
270300101201     c                   if        v1cnrs<>99 and brvnrs<>v1cnrs
270400900608     C                   SETON                                        90
270500941201     C   02
270600941201     CAN 03BRVNRS        IFGT      V1CNRS
270700921229     C   18              SETON                                        30
270800921229     C  N18              EXSR      RICCAT
270900921229     C                   END
271000921229     C*
271100900608     C                   GOTO      ENDCON
271200900608     C                   END
271300921229     C*
271400921229     C* SEGNACOLLO DAL
271500101216     C   04BRVNSC        IFLT      savNCD
271600900608     C                   SETON                                        90
271700900608     C                   GOTO      ENDCON
271800900608     C                   END
271900101216     C***
272000921229     C* SEGNACOLLO DAL SENZA AL
272100101216     c***
272200101216     C   04
272300101216     CANN05BRVNSC        IFGT      savNCD
272400101216     c
272500921229     C                   SETON                                        90
272600921229     C     *IN02         IFEQ      '1'
272700921229     C     *IN03         ANDEQ     '1'
272800921229     C   06
272900921229     CAN 18              SETON                                        30
273000921229     C   06
273100921229     CANN18              EXSR      RICCAT
273200921229     C                   END
273300921229     C                   GOTO      ENDCON
273400921229     C                   END
273500101216     c***
273600921229     C* SEGNACOLLO DAL  E    AL
273700101216     c***
273800101216    1c                   if        *in05
273900101201     c
274000101216    2C     BRVNSC        IFGT      SAVNCA
274100900608     C                   SETON                                        90
274200101216    3C     *IN02         IFEQ      '1'
274300921229     C     *IN03         ANDEQ     '1'
274400921229     C   06
274500921229     CAN 18              SETON                                        30
274600921229     C   06
274700921229     CANN18              EXSR      RICCAT
274800101216    3C                   END
274900900608     C                   GOTO      ENDCON
275000101216    2c                   endif
275100101216    1c                   endif
275200101201     c
275300101201     c* Se entro i limiti impostati e c'e' num spedizione, controllo
275400101216     c*    se ne fa parte (serve per i non sequenziali)
275500101216    1c                   if        v1cbosp>0
275600101201     c     kbodet        setll     fnblt01l
275700101216    2c                   if        not %equal(fnblt01l)
275800101201     c     kbodet        setll     fnart01l
275900101216    3c                   if        not %equal(fnart01l)
276000101201     C                   SETON                                        90
276100101201     C                   GOTO      ENDCON
276200101216    3c                   endif
276300101216    2c                   endif
276400101216    1c                   endif
276500101201     c
276600101216     C**                 END
276700101216     C**                 END
276800101216     C* PISTOLA
276900941201     C     V1CNPS        IFNE      99
277000941201     C     BRVNPS        IFNE      V1CNPS
277100900608     C                   SETON                                        90
277200900608     C                   GOTO      ENDCON
277300900608     C                   END
277400900608     C                   END
277500920810     C*
277600941213     C                   MOVEL     BRVFGS        V2CFGS
277700941213     C                   MOVEL     BRVNPG        V2CNPG
277800000118     C                   Z-ADD     BRVNFV        V2CNFV
277900941213     C                   Z-ADD     BRVVUC        V2CVUC                         VOLUME
278000990907     C                   Z-ADD     BRVPUC        V2CPUC                         PESO
278100920810     C*
278200920911     C* CARICO SOLO SPUNTE DI FOGLI GESTITI SOLO SE NON E' RICHIAMATO
278300000118     C                   MOVEL     BRVNPG        WOKNPG                         CHIAVE FOGLIO
278400000118     C                   MOVE      BRVNFV        WOKNFV                           "      "
278500000118     C                   MOVE      BRVFGS        WOKFGS                           "      "
278600950208     C*
278700920911     C     BRVKEY        IFNE      SAVKEY
278800000118     C                   Z-ADD     BRVNPG        KNPG                           CATEGORIA
278900000118     C                   Z-ADD     BRVNFV        KNFV                           NR.FOGLIO
279000000118     C                   Z-ADD     BRVFGS        KFGS                           FIL.GESTIONE
279100000118     C                   EXSR      RILFOG
279200920810     C                   END
279300920810     C*
279400941201     C  N26
279500941201     CANN20V1CNFV        IFLE      *ZEROS
279600941202     C     V1CNPG        ANDNE     '1'                                          SOLO ARRIVI
279700930426     C*
279800941201     C     V1CNPG        IFNE      '3'
279900941201     C     V1CNPG        OREQ      '3'
280000930426     C     FVVSPG        ANDNE     'P'
280100941201     C     FVVFGS        LOOKUP    L6                                     33
280200920810     C  N33              SETON                                        90
280300920811     C  N33              GOTO      ENDCON
280400930426     C                   END
280500920911     C                   END
280600920811     C*
280700920811     C* D E C O D I F I C H E   F I S S E   D E L   F O G L I O
280800920811     C     BRVKEY        IFNE      SAVKEY
280900960411     C                   SETOFF                                       2423
281000960404     C                   MOVE      *BLANKS       WCHIUS            1
281100941213     C                   MOVEL     *BLANKS       V2DSPG
281200101202     C* DATA FOGLIO VIAGGIO
281300101202     C                   Z-ADD     wdfv          V2HDFV                         AAAA/MM/GG
281400101202     C                   MOVE      wdfv          DATA
281500101202     C                   MOVE      MM            V2CDFV
281600101202     C                   MOVEL     GG            V2CDFV
281700941202     C*
281800941202     C* PER CATEGORIA <> 1 CERCO FRA GLI ARRIVI
281900941213     C     V2CNPG        IFNE      1                                            1=PARTENZE
282000941202     C*
282100961202     C*?    F O G L I     V A R I     ?
282200920811     C*
282300920811     C* SE CHIUSO PROTEGGO TUTTI I CAMPI
282400941229     C     FVVFCF        IFEQ      'S'
282500920811     C                   SETOFF                                       94
282600920811     C                   SETON                                        24
282700960404     C                   MOVE      '1'           WCHIUS
282800920811     C                   END
282900920811     C*
283000950131     C*
283100950131     C* PER CATEGORIA 4=CONSEGNE METTO A VIDEO PARTE DELLA DESCRIZIONE
283200950131    5C     FVVNPG        IFEQ      4                                            4=CONSEGNE
283300000713    5C     FVVNPG        OREQ      8                                            4=CONSEGNE
283400950131     C                   MOVEL     FVVDSF        V2CABB
283500961202     C*
283600961202    6C     FVVFCF        IFNE      'S'
283700961202     C                   SETON                                        23
283800961202     C                   ELSE
283900961202     C                   SETOFF                                       23
284000961202    6C                   ENDIF
284100961202     C*
284200961202   X5C                   ELSE
284300961202    6C     FVVNPG        IFEQ      3                                            3=INV
284400961202    7C     FVVFCF        IFNE      'S'
284500070116     C                   SETON                                        23
284600961202     C                   ELSE
284700961202     C                   SETOFF                                       23
284800961202    7C                   ENDIF
284900961202     C**
285000961202   X6C                   ELSE
285100961202     C                   SETOFF                                       23
285200961202    6C                   ENDIF
285300961202     C**
285400950705     C                   CLEAR                   V2CABB
285500950131    5C                   ENDIF
285600920811     C*
285700920811     C* DECODIFICA TIPO INVENTARIO
285800920811     C     FVVSPG        IFNE      ' '
285900020808     C                   Z-ADD     1             XI
286000020808     C     FVVSPG        LOOKUP    SPG(XI)                                31
286100161006     c***31              eval      v2dspg='IM'+ %subst(DES(XI):1:1)
286200161006     c                   eval      v2dspg=des(XI)
286300920811     C                   END
286400941202     C*
286500941202     C                   ELSE
286600941202     C*
286700941202     C*?    F O G L I O    P A R T E N Z E    ?
286800040709     C                   CLEAR                   V2CABB
286900970131     C*
287000941214     C     WFILE         IFEQ      'FGV'
287100960829     C* VISUALIZZO SE FV DI DEFLUENZA O LOCALE
287200960829     C                   MOVE      FGVTTR        WTTR              1
287300101202     C***                MOVE      FGWFGS        WPT               3 0
287400960829     C                   EXSR      RICTTR
287500980213     C     §TVDEF        IFEQ      'S'
287600020808     C                   MOVEL     FGVLNA        NUM3
287700980213     C                   EXSR      CAZORG
287800020207     C  N39              MOVEL     §OGNTW        SAVNTW
287900020207     C   39              MOVE      *BLANKS       SAVNTW
288000980213     C                   ENDIF
288100970131     C*
288200970131     C* 23 ON - SPROTEGGO LA CATEGORIA PER PERMETTERE DI VARIARLA
288300961202     C                   SETON                                        23
288400941214     C                   ENDIF
288500970131     C**
288600941214     C     WFILE         IFEQ      'FVA'
288700941214     C* IL FOGLIO E', PER FORZA, GIA' CHIUSO
288800941214     C                   SETOFF                                       94
288900941214     C                   SETON                                        24
289000960829     C* VISUALIZZO SE FV DI DEFLUENZA O LOCALE
289100960829     C                   MOVE      FVATTR        WTTR              1
289200101202     C****               MOVE      FWAFGS        WPT
289300960829     C                   EXSR      RICTTR
289400941202     C                   ENDIF
289500941202     C*
289600941202     C                   ENDIF
289700941202     C*
289800920811     C* SALVO NUOVA CHIAVE
289900000118     C                   MOVEL     BRVKEY        SAVKEY           10 0
290000920811     C                   END
290100000120     C*
290200000120     C* SOLO PARTENZE DELLA MIA AREA: SE LA SPUNTA E' GIA' TRASMESSA
290300000120     C* NON E' MODIFICABILE (QUINDI NIENTE ALTA INTENSITA')
290400000120     C                   SETOFF                                       63
290500000120     C     BRVNPG        IFEQ      1                                            1=PARTENZE
290600070220     C***  BRVFGS        LOOKUP    L1                                     99     ON=TROVATO
290700070220     C***99BRVFTR        IFNE      *BLANKS                                      GIA' TRASMESSO
290800070220     C     BRVFTR        IFNE      *BLANKS                                      GIA' TRASMESSO
290900000120     C                   SETON                                        63
291000000120     C                   ENDIF
291100000120     C                   ENDIF
291200000120     C* SE E' FOGLIO DI DEFLUENZA CHIUSO PER ESTERO COME SE LE SPUNTE
291300000120     C*  FOSSERO STATE TRASMESSE
291400000120     C     BRVNPG        IFEQ      1
291500000120     C     §TVDEF        ANDEQ     'S'
291600000120     C     FGVFCF        ANDEQ     'S'
291700020207     C     SAVNTW        IFEQ      'EEX'
291800020207     C     SAVNTW        OREQ      'DPD'
291900020207     C     SAVNTW        OREQ      'FED'
292000000120     C                   SETON                                        63
292100000120     C                   ENDIF
292200020207     C                   ENDIF
292300900608     C*
292400900608     C     ENDCON        ENDSR
292500940727     C**************************************************************************
292600940727     C* AGGIORNO RECORD
292700940727     C**************************************************************************
292800900608     C     REGIS         BEGSR
292900940727     C*
293000940727     C                   Z-ADD     1             NRR2
293100911014     C*
293200941201     C                   READC     LR04S02                                31     LETTURA
293300930707     C*
293400930707    1C     *IN31         DOWEQ     *OFF
293500960405     C* Imposto indicatori memorizzati nei campi hidden
293600960405     C                   EXSR      SRIND1
293700911014     C*
293800930707    2C     RECORD        IFNE      *ZEROS
293900960522     C     RCDSFL        ANDNE     V2HRCD
294000960612     C*
294100960612    2C     RECORD        OREQ      *ZEROS
294200960612     C     RCDSFL        ANDNE     V2HRCD
294300950706     C                   CLEAR                   WAGG
294400960502     C                   CLEAR                   WNPS
294500960612     C     RECORD        IFEQ      0
294600960612     C                   MOVEL     'A'           V2CANN
294700960612     C                   ENDIF
294800920811     C*
294900911014     C* A N N U L L O
295000941213    3C     V2CANN        IFNE      *BLANKS
295100920811     C*
295200941213     C                   Z-ADD     V2CNRR        BR1NRR
295300070116     C     BR1NRR        CHAIN     FnBRV00F                           33
295400920811     C*
295500920811     C* VEDO SE ELIMINARE LA DATA ARRIVO/ENTRATA: ESCLUDO SE SPUNTA IMP
295600921020     C  N33              MOVEL     '1'           PARFLG
295700921020     C  N33              EXSR      UPDBOL
295800911017     C*
295900000119     C  N33
296000070116     CANN95              DELETE    FnBRV000                                      DELETO
296100040303     C*
296200070215     C* SE SPUNTA CONSEGNA DELETO ANCHE RECORD SU FnBRVE SE PRESENTE
296300040303     C* VERIFICO IL FLAG NELLA DS7N
296400040303     c                   if        not *in33 and not *in95
296500040303     C                   Z-ADD     1             XX                3 0
296600040303     C                   MOVEL     V2hNPG        ALF1
296700040303     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
296800040303     c                   if        *in99 and  NST(XX)='1'
296900040303     C                   EXSR      DLBRVE
297000040303     C                   ENDIF
297100040303     C                   ENDIF
297200070710     c* Se no spunta unica, e con anomalia cancello eventuale
297300070710     c*  spunta senza anomalia in BRVE
297400070711     c***                if        not *in33 and not *in95  and
297500070711     c***                          *in99 and  NST(XX)<>'1' and brvcan<>' '
297600070711     C***  kbrve1        delete    fnbrve1l
297700070711     C***                ENDIF
297800040303     c
297900900611     C*
298000970116     C* PER LE ANOMALIE 145 E 145 VEDO SE RICREARE LEGGENDO TUTTE
298100970116     C*  LE ALTRE SPUNTE CON DATA >= A QUELLA CHE CANCELLO
298200001204     C  N95              EXSR      AN1456
298300970117     C*
298400970117   X3C                   ELSE
298500911017     C*
298600920811     C* 32 ON  - I M M I S S I O N E
298700920811     C* 32 OFF - V A R I A Z I O N E
298800911017     C                   SETOFF                                       32
298900070220     c                   clear                   fnls45ds
299000070220     c                   movel     'S'           dls45ric
299100070220     c                   movel     comtfp        dls45tfp
299200070220     c
299300070220     C                   MOVEL     ' '           PARFLG
299400070220     C***                MOVEL     ' '           FLG
299500070220     C***                MOVEL     *ZEROS        BAROFV
299600921020     C*
299700941213    4C     V2CNRR        IFNE      0
299800941213     C                   Z-ADD     V2CNRR        BR1NRR
299900070116     C     BR1NRR        CHAIN     FnBRV00F                           32
300000921020     C*
300100920811     C* SE MODIFICATO RECORD PER VARIAZIONE
300200000120     C  N32              Z-ADD     BRVNFV        WRVNFV
300300930707    5C  N32BRVCON        IFNE      VI2CON
300400921020     C*
300500070424     c* Se tutto uguale non faccio nulla
300600070424    6C     BRVCO2        IFEQ      VI2CO2
300700070710     C     BRVNFV        andeq     V2CNFV
300800930118     C*
300900930118     C* MODIFICATO SOLO FOGLIO VIAGGIO
301000070424    7C***  BRVNFV        IFNE      V2CNFV
301100060418     c
301200060418     c* Se categoria 3 devo richiamare fnlsa8r
301300070424     c***  brvnpg        ifeq      3
301400070424     C**                 MOVEL     '1'           PARFLG
301500070424     c**                 else
301600070220     C***                MOVEL     '2'           FLG
301700070424     C**                 MOVEL     '2'           dls45FLG
301800070220     C***                Z-ADD     BRVNFV        BAROFV            6 0
301900070424     C**                 Z-ADD     BRVNFV        dls45NFVO
302000070424    7C**                 END
302100070424    7C**                 END
302200960604     C*
302300070424     C* VARIATO TUTTO IL RECORD (compreso numero di foglio)
302400930707   X6C                   ELSE
302500921021     C                   MOVEL     '1'           PARFLG
302600930707    6C                   END
302700930707    5C                   END
302800921020     C*
302900921230     C* ELIMINAZIONE ANOMALIA
303000930707    5C     PARFLG        IFNE      '1'
303100930707    6C     BRVCAN        IFNE      ' '
303200941213     C     V2CCAN        ANDEQ     ' '
303300921020     C                   MOVEL     '3'           PARFLG
303400070710     c* Cancello eventuale BRVE scritto
303500090416     c* ES - meglio di no. potrebbero fare i furbi se se ne accorgono
303600090416     C**                 Z-ADD     1             XX                3 0
303700090416     C**                 MOVEL     V2hNPG        ALF1
303800090416     C**   ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
303900090416     c**                 if        *in99 and  NST(XX)<>'1'
304000090416     C**** kbrve1        delete    fnbrve1l
304100090416     c**                 endif
304200070710     c
304300930707    6C                   END
304400930707    5C                   END
304500921020     C*
304600950706     C* SE MODIFICATO RECORD LANCIO PGM CHE ELIMINA SUL DETTAGLIO COLLI
304700930707    5C     PARFLG        IFNE      ' '
304800921020     C                   EXSR      UPDBOL
304900930707    5C                   END
305000920811     C*
305100930707   X4C                   ELSE
305200950706     C* 32 ON - NUOVA SPUNTA INSERITA
305300900611     C                   SETON                                        32
305400970204     C                   CLEAR                   BR1NRR
305500950706    4C                   ENDIF
305600950706     C*
305700950706     C**
305800941207     C* SE NON VI SONO ERRORI (95OFF) REGISTRO LA SPUNTA
305900950706    4C     *IN95         IFEQ      *OFF
306000950706     C                   SETON                                        33
306100950706     C*
306200950706     C* SE DEL RECORD HO MODIFICATO SOLO CAN O VUC --> NON FACCIO NIENT
306300950706    5C     *IN32         IFEQ      *ON
306400950706     C     BRVCO2        ORNE      VI2CO2
306500950706     C     BRVNFV        ORNE      V2CNFV
306600040303     C** 99              MOVEL     V2CFGS        KFGS
306700040303     C**                 CLEAR                   DSLV55
306800040303     C**                 MOVEL     'P'           D55TPT
306900040303     C**                 MOVE      KFGS          D55LIN
307000040303     C**                 Z-ADD     V2HDFV        D55DRF
307100040303     C**                 CALL      'FNLV55R'
307200040303     C**                 PARM                    DSLV55
307300040303     C**   D55ERR        IFNE      *BLANKS
307400040303     C**                 MOVEL     V2CFGS        KFLE
307500040303     C**                 ELSE
307600040303     C**                 MOVEL     D55TFP        KFLE
307700040303     C**                 END
307800040303     c   99              movel     v2cfgs        kfgs
307900010628     C* CANCELLAZIONE SPUNTE DOPPIE RELATIVE ALLA SPUNTA ORIGINARIA
308000010628     C*
308100010628     C* DA FARE SOLO PER SPUNTE DI CUI E' VARIATO IL SEGNACOLLO
308200010628     C* (SE UN SEGNACOLLO DA CONSEGNA LO PASSO IN IMA LASCIO COMUNQUE
308300010628     C* LE EVENTUALI SPUNTE DOPPIE PRESENTI. SARA' LA STAMPA DELLA
308400010628     C* QUADRATURA CHE DOVRA' SEGNALARE CHE IL SEGNACOLLO SI TROVA IN
308500010628     C* IMA)
308600040303     c
308700040303     c                   if        not *in32 and
308800040303     c                             record <> brvrec
308900040303     C                   Z-ADD     1             XX                3 0
309000040303     C                   MOVEL     V2hNPG        ALF1
309100040303     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
309200040303     c                   if        *in99 and  NST(XX)='1'
309300040303     C                   EXSR      DLBRVE
309400040303     C                   ENDIF
309500040303     C                   ENDIF
309600070711     c* non deleto BRVE se cambiato segnacollo per l'anomalia
309700070711     c*  updato se cambia il numero del foglio
309800070711     c                   if        not *in32 and brvco2=vi2co2 and
309900070711     c                             brvnfv<>v2cnfv
310000070711     C                   Z-ADD     1             XX                3 0
310100070711     C                   MOVEL     V2CNPG        ALF1
310200070711     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
310300070711     c                   if        *in99 and  NST(XX)<>'1'
310400070711     C     kbrve1        chain     fnbrve1l
310500070711     c                   if        %found(fnbrve1l)
310600090416     c                   z-add     v2cnfv        e_brvnfv
310700070711     c                   update    fnbrve1
310800070711     C                   ENDIF
310900070711     c
311000070711     C                   ENDIF
311100070711     C                   ENDIF
311200950706     C*
311300950706     C* SE TROVO UNA SPUNTA ESATTAMENTE UGUALE, CANCELLO QUESTA E
311400950706     C*  L'ALTRA LA RIAGGIORNO
311500020924     c                   z-add     br1nrr        sa2nrr
311600000119     C                   Z-ADD     V2CNFV        W0060             6 0
311700070116     C     KBRV7         CHAIN     FnBRV001                           33
311800950706     C*
311900950706    6C     *IN33         IFEQ      *OFF
312000040303     c
312100040303     C                   Z-ADD     1             XX                3 0
312200040303     C                   MOVEL     V2CNPG        ALF1
312300040303     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
312400040303     c                   if        *in99 and  NST(XX)='1'
312500070710     c                   eval      wnst=nst(XX)
312600040303     C                   EXSR      SRBRVE
312700040303     C                   ENDIF
312800070710     c
312900070710     c* Se non spunta unica ma presente senza anomalia, memeorizzo
313000070710     c                   if        *in99 and  NST(XX)<>'1' and brvcan=' '
313100070710     c                             and v2ccan<>' '
313200070710     c                   eval      wnst=nst(XX)
313300070710     C                   EXSR      SRBRVE
313400070710     C                   ENDIF
313500070215     c
313600070116     C  N32              DELETE    FnBRV000
313700070223     c*
313800070223     c                   EXSR      riempi
313900950706     C*
314000950706    7C     V2CCAN        IFNE      ' '
314100950706     C                   MOVEL     V2CCAN        BRVCAN
314200950706    7C                   ENDIF
314300080318     c
314400080318     c* BRVTSU --> spunta multipla, tengo quella che c'e' delle 2
314500080318     c                   if        v2ctsu<>' '
314600080318     c                   eval      brvtsu=v2ctsu
314700080318     c                   endif
314800070223     c
314900070215     C* PER IMPOSTARE BRVNPS e BRVTAP DETERMINO LA PISTOLA PRIORITARIA
315000980216     C                   MOVEL     V2CNPS        NPS1
315100070228     C                   movel     v2hdcs        dcs1
315200071127     C                   movel     v2chcs        hcs1
315300070228     C                   movel     v2ctap        tap1
315400980213     C                   Z-ADD     BRVNPS        NPS2
315500070215     C                   movel     BRVtap        tap2
315600070228     C                   movel     BRVdcs        dcs2
315700070228     C                   movel     BRVhcs        hcs2
315800980213     C                   EXSR      SRNPS
315900980213     C                   Z-ADD     NPSP          BRVNPS
316000070215     c                   eval      brvtap=TAPP
316100070228     c                   eval      brvdcs=dcsP
316200070228     c                   eval      brvhcs=hcsP
316300070215     c
316400070116     C                   UPDATE    FnBRV001
316500070223     C                   MOVEL     'S'           WAGG              1
316600070223     c*
316700070223     c* Aggiornamenti finali sulle bolle
316800070223     c                   EXSR      AggioFINALE
316900150824     c*
317000150824     c* colli che partono da più filiali: se devo scrivere AGB record "CB"
317100150824     c                   if        %subst(dls45flo:1:1)='B'
317200150824     c                   exsr      DA_BSP
317300150824     c                   endif
317400950706     C*
317500950706   X6C                   ELSE
317600070223     c
317700070223     C* 33 ON - GIA' ESISTENTE: IN UPDATE
317800070116     C                   UNLOCK    fnbrv00F
317900020924     c* mi reimposto il num relativo di record
318000020924     c                   z-add     sa2nrr        br1nrr
318100911016     C*
318200040304     C* VERIFICO IL FLAG NELLA DS7N
318300040304     C                   Z-ADD     1             XI
318400040304     C                   MOVEL     V2CNPG        ALF1
318500040304     C     ALF1          LOOKUP    CAT(XI)                                99     ON=TROVATA
318600040304    7C   99NST(XI)       IFeq      '1'                                          NR.SPUNTE GG.
318700140625     C     KBRV3a        CHAIN     fnbrv05L                           33
318800040304     C*
318900040304    8c                   if        not *in33
319000040304     C* MI SALVO L'ANOMALIA PRESENTE SULLA SPUNTA
319100080318     C                   MOVEL     BRVtsu        tsu03
319200080318     C                   MOVEL     BRVCAN        CAN03
319300070215     C                   Z-ADD     BRVNPS        NPS03
319400070220     c                   movel     brvtap        TAP03
319500070228     c                   movel     brvdcs        DCS03
319600070228     c                   movel     brvhcs        HCS03
319700040304     C* SE  E' QUELLO CHE STO VARIANDO --> ACCENDO IL 33
319800040304    9C     BR1NRR        IFEQ      BR3NRR
319900040304     C                   SETON                                        33
320000040304    9C                   ENDIF
320100040304    8C                   ENDIF
320200040304   x7c                   else
320300040304     c* Se le devo tenere tutte , l'unico controllo è per num foglio
320400040304     c*  che il pgm ha già fatto, per cui vado per forza in write
320500040304     c                   seton                                        33
320600040304    7c                   endif
320700941207     C*
320800941212     C*----------------------------------------------------
320900040225     C* 1)IN UN FOGLIO (NPG+fgs+NFV)POSSO AVERE UNA SOLA SPUNTA PER COLLO
321000040225     C* 2) SE foglio diverso,  E' IL FLAG §7NNST CHE
321100040225     C*    DICE SE TENERE UNA SOLA SPUNTA (='1') O NO (anche + spunte
321200040225     c*    nella stessa data adesso)
321300070131     C*
321400140625     C   33              UNLOCK    fnbrv05L
321500040304     c
321600040304     c                   if        not *in33 and *in99 and  NST(XI)='1'
321700070710     c                   eval      wnst=nst(XI)
321800040304     C                   EXSR      SRBRVE
321900040304     C                   ENDIF
322000070710     c                   if        not *in33 and *in99 and  NST(XI)<>'1' and
322100070710     c                             brvcan=' ' and v2ccan<>' '
322200070710     c                   eval      wnst=nst(XI)
322300070710     C                   EXSR      SRBRVE
322400070710     C                   ENDIF
322500040304     c*
322600070116     C  N32BR1NRR        CHAIN     fnbrv00F                           32
322700950706    6C                   ENDIF
322800960613   X5C                   ELSE
322900960613     C*
323000960613     C* SE HO MODIFICATO SOLO L'ANOMALIA MESSA O TOLTA O VARIATA
323100070223     C*  LA RISCRIVO COSI' COM'E'( il 33 = *on)
323200070711     C                   Z-ADD     1             XX                3 0
323300070711     C                   MOVEL     V2cNPG        ALF1
323400070711     C     ALF1          LOOKUP    CAT(XX)                                99     ON=TROVATA
323500070711     c                   if        *in99 and  NST(XX)<>'1' and
323600070711     c                             brvcan=' ' and v2ccan<>' '
323700070711     c                   eval      wnst=nst(XX)
323800070711     C                   EXSR      SRBRVE
323900070711     c                   endif
324000070711     c*
324100960613     C                   MOVEL     V2CCAN        BRVCAN
324200070223     c* Di fatto sotto la riaggiorna, confrontando V2CCAN con BRVCAN
324300070223     c*  ma non servirebbe. Lo fa perchè entra nel giro degli
324400070711     c*  aggiornamenti normali
324500950706    5C                   ENDIF
324600941209     C*----------------------------------------------------
324700070221     c*
324800070223     c* non Faccio più nulla se già aggiornata la spunta e le bolle
324900070223     c*  (quando trovo spunta uguale si fnbrv01l)
325000950706     C*
325100070223   4ac                   if        wagg=' '
325200941213     C                   MOVEL     V2CFGS        BRVFGS                         FIL.GESTIONE
325300941229     C                   MOVEL     SIMFEL        BRVFLE                         FIL.ELABORATORE
325400941213     C                   MOVEL     V2CNPG        BRVNPG                         CATEGORIA FOGLIO
325500000120     C                   Z-ADD     V2CNFV        BRVNFV
325600941213     C                   MOVEL     V2CLNP        BRVLNP
325700941213     C                   MOVEL     V2CLNA        BRVLNA
325800941213     C                   MOVEL     V2CNRS        BRVNRS
325900941213     C                   MOVEL     V2CNSC        BRVNSC
326000941213     C                   MOVEL     V2CZNC        BRVZNC                         ZONA
326100980213     C* SE VADO IN UPDATE O IN WRITE DI UNA NUOVA SPUNTA TENGO LA PIS-
326200980213     C* TOLA DEL VIDEO (33 ON)
326300980213     C* SE VADO IN UPDATE DI UNA SPUNTA GIA' PRESENTE TENGO LA PISTOLA
326400980213     C* PRIORITARIA    (33 OFF)
326500980213    5C     *IN33         IFEQ      *ON
326600980213     C                   MOVEL     V2CNPS        BRVNPS                         PISTOLA
326700070223     c                   movel     v2ctap        brvtap
326800070223     c* Per nuova immissione, imposto A-As400
326900070228     c   32              if        brvtap=' '
327000070223     c                   movel     'A'           brvtap
327100070223     c                   endif
327200070223     c
327300980213   X5C                   ELSE
327400980216     C                   MOVEL     V2CNPS        NPS1
327500070223     C                   MOVEL     v2ctap        tap1
327600070228     C                   MOVEL     v2chcs        hcs1
327700070228     C                   MOVEL     v2hdcs        dcs1
327800070215     C                   Z-ADD     NPS03         NPS2
327900070215     C                   movel     TAP03         TAP2
328000070228     C                   movel     DCS03         dcs2
328100070228     C                   movel     HCS03         hcs2
328200980213     C                   EXSR      SRNPS
328300980213     C                   Z-ADD     NPSP          BRVNPS
328400070215     c                   movel     TAPP          BRVTAP
328500070228     c                   movel     DCSP          BRVDCS
328600070228     c                   movel     HCSP          BRVHCS
328700980213    5C                   END
328800070215     c
328900950906     C     *IN32         IFEQ      *OFF
329000950906     C     *IN33         OREQ      *OFF
329100950906     C*
329200950906     C     V2CCAN        IFNE      ' '                                          UPDAT
329300950906     C                   MOVEL     V2CCAN        BRVCAN
329400950906     C                   ELSE
329500950906     C* SOLO SE HO TROVATO UN'ALTRA SPUNTA SU CUI VADO IN UPDATE(33OFF)
329600950906     C*  MEMORIZZO LA SUA ANOMALIA SE C'E'
329700070215     C  N33              MOVEL     CAN03         BRVCAN
329800950906     C                   ENDIF
329900080318     c
330000080318     C     V2Ctsu        IFNE      ' '                                          UPDAT
330100080318     C                   MOVEL     V2Ctsu        BRVtsu
330200080318     C                   ELSE
330300080318     C* SOLO SE HO TROVATO UN'ALTRA SPUNTA SU CUI VADO IN UPDATE(33OFF)
330400080318     C*  MEMORIZZO LA SUA spunta multipla  se c'e'
330500080318     C  N33              MOVEL     tsu03         BRVtsu
330600080318     C                   ENDIF
330700950906     C*
330800950906     C                   ELSE
330900070223     c* Record completamente nuovo: 32 on (nuova immissione a video)
331000070223     c*                             33 on (non trovata altra spunta secondo
331100070223     c*                                    le regole sopra indicate)
331200950906     C                   MOVEL     V2CCAN        BRVCAN                         WRITE
331300070223     c* PESO e VOLUME VDL:  devo sempre tenere i dati del video,
331400070223     c*   per cui pulisco eventuali dati sporchi da precedente lettura
331500070223     c                   clear                   brvvuc
331600070223     c                   clear                   brvpuc
331700070223     c* DATA e ORA CARICAMENTO: =0
331800070223     C                   MOVE      *ZEROS        BRVDCS
331900070223     C                   MOVE      *ZEROS        BRVHCS
332000070223     C                   MOVEL     *BLANKS       BRVnvr
332100070223     C                   CLEAR                   BRVATR                         FLAG TRANSITO
332200070223     C                   CLEAR                   BRVtsu                         FLAG TRANSITO
332300950906     C                   ENDIF
332400070223     c
332500070223     c* Dati comuni a precedente aggiornamento
332600070223     c                   EXSR      RIEMPI
332700070131     c
332800000516     C* SE VARIATO TUTTO IL RECORD O PER IMMISSIONE: RICERCO ZONA
332900000516    6C     PARFLG        IFEQ      '1'
333000000516     C     PARFLG        OREQ      'A'
333100000516     C     *IN32         OREQ      '1'
333200000516     C                   EXSR      ZONA
333300000516     C                   MOVEL     BRVZNC        V2CZNC
333400000516    6C                   END
333500070221     c
333600070223     c                   EXSR      AggioFINALE
333700921021     C*
333800070223     C**  N95
333900070223    5C**ANN96WAGG          IFEQ      ' '
334000070223     c
334100070223     c                   if        not *in95 and not *in96
334200960612     C*
334300960612     C* SE E' SPUNTA ENTRATA O PARTENZA E NON HO TROVATO NE' LA BOLLA
334400960612     C*  PARTENZA NE' LA BOLLA TRANSITO, PER SERIE =0
334500960612     C*  IMPOSTO NELLA SPUNTA FLAG TRASMISSIONE "N" E 99999999
334600960612     C*  PER NON AUTOGENERARE LA SPUNTA PARTENZA O NON TRASMETTERLA
334700070221    6C     V2CNPG        IFEQ      5
334800070221     C     V2CNPG        OREQ      1
334900960612     C*
335000960612     C     V2CLNP        LOOKUP    L1                                     38
335100960612     C*
335200070221    7C   38V2CNRS        IFEQ      0
335300960612     C     V2HESI        ANDEQ     'N'
335400960612     C                   MOVEL     'N'           BRVFTR
335500960612     C                   MOVEL     *HIVAL        BRVDTR
335600000119     C                   MOVEL     *HIVAL        BRVHTR
335700070221    7C                   ENDIF
335800070221    6C                   ENDIF
335900910323     C*
336000070221    6C     *IN32         IFEQ      *OFF
336100140625     C  N33              DELETE    fnbrv005
336200070116     C                   UPDATE    fnbrv000
336300970117     C                   EXSR      AN1456
336400070221    6C                   ENDIF
336500950706     C*
336600000119     C   33
336700070116     CAN 32              WRITE     fnbrv000
336800000119     C  N33
336900140625     CAN 32              UPDATE    fnbrv005
337000950706    5C                   ENDIF
337100910325     C*
337200150824     c* colli che partono da più filiali: se devo scrivere AGB record "CB"
337300150824     c                   if        %subst(dls45flo:1:1)='B'
337400150824     c                   exsr      DA_BSP
337500150824     c                   endif
337600070223   4aC                   END
337700070223    4C                   END
337800930707    3C                   END
337900930707    2C                   END
338000930707     C*
338100950711     C  N31              READC     LR04S02                                31     LETTURA
338200950711    1C                   ENDDO
338300921020     C*
338400921020     C* AGGIORNO L'ULTIMO LETTO CHE NON HO AGGIORNATO PERCHE' ERRORE
338500950711     C     *IN95         IFEQ      *ON
338600950711     C     *IN96         OREQ      *ON
338700950711     C*
338800950711     C                   SETON                                        36
338900950711     C                   UPDATE    LR04S02
339000950711     C                   SETOFF                                       36
339100950711     C                   ENDIF
339200910323     C*
339300921020     C                   ENDSR
339400070223     C**************************************************************************
339500070223     C* RIempimento campi di FNBRV
339600070223     C**************************************************************************
339700070223     c     RIEMPI        BEGSR
339800070223     c
339900070223    7C     V2CVUC        IFGT      0
340000070223     C                   Z-ADD     V2CVUC        BRVVUC
340100070223    7C                   ENDIF
340200070223     C*
340300070223    7C     V2CPUC        IFGT      0
340400070223     C                   Z-ADD     V2CPUC        BRVPUC
340500070223    7C                   ENDIF
340600070223     C*
340700070223     C***                Z-ADD     WDATEU        BRVDFS                                     ENTO
340800070223     C***                TIME                    ORARIO            6 0                      NTO
340900070223     C***                MOVEL     ORARIO        BRVHMS
341000070223     C***                time                    BRVHMS
341100070223     c* millisecondi immissione
341200070223     c                   time                    wtimez
341300070223     c                   extrct    wtimez:*ms    millisec          6 0
341400070223     c                   movel     millisec      brvmis
341500070223     c* data immissione
341600070223     c                   movel     wtimez        dataiso
341700070223     c                   movel     dataiso       brvdfs
341800070223     c* ora immissione
341900070223     c                   move      wtimez        oraiso
342000070223     c                   movel     oraiso        brvhms
342100070223     c                   movel     knmus         brvpru
342200070223     C                   CLEAR                   BRVFTR
342300070223     C                   Z-ADD     *ZERO         BRVDTR                         DATA TRASMISSIONE
342400070223     C                   Z-ADD     *ZERO         BRVHTR                                     SIONE
342500070223     C**                 Z-ADD     WPOSPU        BRVPES
342600070223     C                   movel     v2cfgs        BRVPES
342700070223     c                   ENDSR
342800070223     C**************************************************************************
342900070223     C* RIempimento campi di FNBRV
343000070223     C**************************************************************************
343100070223     c     AggioFINALE   BEGSR
343200070223      * Se è stato modificato il volume o il peso della spunta devo anche azzerare
343300070223      * i campi di FNART in modo che fnls45r rielabori i dati per aggiornare la bolla
343400120615     c**                 If        V2hVuc <> V2cVuc or
343500120615     c**                           V2hPuc <> V2cPuc
343600120615     c**   Kart          Chain     Fnart27l
343700120615     c**                 If        %Found (Fnart27l)
343800120615     c**                 Clear                   bltVuc
343900120615     c**                 Clear                   bltFvc
344000120615     c**                 Clear                   bltPuc
344100120615     c**                 Clear                   bltFpc
344200120615     c**                 Update    Fnart000
344300120615     c**                 EndIf
344400120615     c**                 EndIf
344500070223     C* ABILITO BOLLE
344600070223    5C     PARFLG        IFNE      '2'
344700070223     C     PARFLG        ANDNE     '3'
344800070223     C     PARFLG        ANDNE     '4'
344900070223     C*
345000070223     C     PARFLG        OREQ      '2'
345100070223     C     dls45FLG      ANDNE     ' '
345200070223     C     PARFLG        OREQ      '3'
345300070223     C     dls45FLG      ANDNE     ' '
345400070223     C     PARFLG        OREQ      '4'
345500070223     C     dls45FLG      ANDNE     ' '
345600070223     C                   EXSR      ABIBOL
345700070223    5C                   END
345800070223     c                   ENDSR
345900941214     C**************************************************************************
346000941229     C* RILEVO IL FOGLIO VIAGGIO PARTENZE/ARRIVI DELLA MIA AREA
346100941214     C**************************************************************************
346200941214     C     RILFOG        BEGSR
346300941214     C*
346400941214     C                   Z-ADD     *ZERO         WDFV                           DATA FOGLIO
346500000118     C                   MOVE      *BLANKS       WFILE             3
346600941214     C*
346700941214     C* ARRIVI O PARTENZE ?
346800000118    1C     KNPG          IFEQ      1                                            1=PARTENZE
346900941214     C*
347000020808     C     K02FGV01      chain     FNFGV01L                           99
347100031121     C     *IN99         IFEQ      *Off
347200000118     C  N99              MOVEL     'FGV'         WFILE                          DI COMODO
347300941214     C  N99              Z-ADD     FGVDFV        WDFV                           DATA FOGLIO
347400000519     C*
347500000519     C* ESTENSIONE
347600020808     C     K02FGV01      chain     FNFGW01L                           34
347700070131    2C     *IN34         IFEQ      *ON
347800000519     C     FGWATB        ORNE      ' '
347900000519     C                   CLEAR                   FGWFF3
348000000519     C                   CLEAR                   FGWFF4
348100000519     C                   CLEAR                   FGWFL3
348200000519     C                   CLEAR                   FGWFL4
348300000519     C                   CLEAR                   FGWFGS
348400000519     C                   ENDIF
348500000519     C**
348600000118   X2C                   ELSE
348700000118     C     KFVA1         CHAIN     FNFVA01L                           99
348800000118     C  N99              MOVEL     'FVA'         WFILE                          DI COMODO
348900031121     C  N99              Z-ADD     FvaDFV        WDFV                           DATA FOGLIO
349000000519     C     KFVA1         CHAIN     FNFWA01L                           34
349100000519     C*
349200000519     C     *IN34         IFEQ      *ON
349300000519     C     FWAATB        ORNE      ' '
349400000519     C                   CLEAR                   FWAFF3
349500000519     C                   CLEAR                   FWAFF4
349600000519     C                   CLEAR                   FWAFL3
349700000519     C                   CLEAR                   FWAFL4
349800000519     C                   CLEAR                   FWAFGS
349900000519     C                   ENDIF
350000000118    2C                   ENDIF
350100941214     C*
350200000118   X1C                   ELSE                                                   ARRIVI
350300941214     C*
350400941229     C     KFVV1         CHAIN     FNFVV01L                           99
350500941229     C  N99              Z-ADD     FVVDFV        WDFV                           DATA FOGLIO
350600000118    1C                   ENDIF
350700941214     C*
350800941214     C                   ENDSR
350900940727     C**************************************************************************
351000940727     C* ELIMINO DATE E ANOMALIE DI SPUNTE CANCELLATE
351100940727     C**************************************************************************
351200921020     C     UPDBOL        BEGSR
351300940727     C*
351400000118     C                   Z-ADD     BRVNFV        WRVNFV
351500921020     C                   MOVEL     PARAM2        KPJBU
351600940727     C                   CALL      'FNLSA8R'
351700921020     C                   PARM                    KPJBA
351800930609     C                   MOVEL     'C'           CFSA8             1
351900921020     C*
352000921020     C                   MOVEL     KPJBU         PARAM2
352100921020     C* ERRORI DURANTE LA REGISTRAZIONE
352200921020     C     PARFLG        IFEQ      'E'
352300921020     C                   SETON                                        9531
352400921020     C                   END
352500921020     C*
352600921020     C                   ENDSR
352700970116     C**************************************************************************
352800070220     C* RICHIAMO FNLS45R PER VEDERE SE CREARE LA 145 - 146
352900970116     C**************************************************************************
353000970116     C     AN1456        BEGSR
353100031120     c                   clear                   savpes
353200070116     C     KART          SETLL     fnbrv07L
353300070116     C     KART          READE     fnbrv07L                               33
353400031120     c* determino ter-arr di lna (siccome il collo è sempre lo stesso
353500031120     c* do per scontato che il suo ter-arr sia sempre lo stesso e quindi
353600031120     c* lo determino solo la prima volta utilizzando la data foglio della
353700031120     c* spunta che annullo
353800031120     c     *in33         ifeq      *off
353900031120     c                   clear                   dslv55
354000031120     c                   movel     'A'           D55TPT
354100031120     c                   z-add     brvlna        D55LIN
354200031120     c                   z-add     brvlnp        D55LNP
354300031120     c                   z-add     v2hdfv        d55drf
354400031120     c                   call      'FNLV55R'
354500031120     c                   parm                    dslv55
354600031120     c                   if        D55err = *blanks
354700031120     c                   z-add     D55TFA        tfalna
354800031120     c                   endif
354900031121     c                   endif
355000970116     C     *IN33         DOWEQ     *OFF
355100970117     C*
355200031120     c* Leggo solo le spunte effettuate da p.o. appartenenti alla stessa are
355300031120     c* di arrivo di brvlna
355400031120     c     brvpes        ifne      tfalna
355500031120     c     brvpes        andne     savpes
355600031120     c                   clear                   dslv55
355700031120     c                   movel     'A'           d55tpt
355800031120     c                   z-add     brvpes        d55lin
355900031120     c                   z-add     brvlnp        d55lnp
356000031120     c                   z-add     v2hdfv        d55drf
356100031120     c                   call      'FNLV55R'
356200031120     c                   parm                    dslv55
356300031120     c                   z-add     d55tfa        tfapes
356400031120     c                   z-add     d55tfa        savpes
356500031120     c                   endif
356600031120     c     brvpes        ifeq      tfalna
356700031120     c     tfapes        oreq      tfalna
356800101201     c
356900070220     c*** richiamo nuovo pgm di caricamento spunte FNLs45R
357000070220     c                   movel     brvpes        dls45pes
357100070220     c                   movel     'A'           dls45flg2
357200070221     c*
357300070221     C                   eval      d_brvnvr= BrvNvr
357400070221     C                   eval      d_brvnpg= BrvNPG
357500070221     C                   eval      D_brvnfv= BrvNFV
357600070221     C                   eval      d_brvlnp= BrvLNP
357700070221     C                   eval      d_brvlna= BrvLNA
357800070221     C                   eval      d_brvnrs= BrvNRS
357900070221     C                   eval      d_brvnsc= BrvNSC
358000070221     C                   eval      d_brvcan= BrvCAN
358100070221     C                   eval      d_brvnps= BrvNPS
358200070221     C                   eval      d_brvvuc= BrvVUC
358300070221     C                   eval      d_brvpuc= BrvPUC
358400070221     C                   eval      d_brvdfs= BrvDFS
358500070221     C                   eval      d_brvhms= BrvHMS
358600070221     C                   eval      d_brvmis= Brvmis
358700070221     C                   eval      d_brvznc= BrvZNC
358800070221     c                   eval      d_brvpru= brvpru
358900070221     c                   eval      d_brvfgs= brvfgs
359000070221     c                   eval      d_brvtap= brvtap
359100070221     c                   eval      d_brvpes= brvpes
359200070221     c                   eval      d_brvatr= brvatr
359300070221     c                   eval      d_brvfle= brvfle
359400070221     c                   eval      d_brvftr= brvftr
359500070221     c                   eval      d_brvdtr= brvdtr
359600070221     c                   eval      d_brvhtr= brvhtr
359700070221     c                   eval      d_brvtsu= brvtsu
359800101202     c                   eval      d_brvdcs= brvdcs
359900101202     c                   eval      d_brvhcs= brvhcs
360000070221     c
360100070220     c                   call      'FNLS45R'
360200070220     c                   parm                    fnls45ds
360300070220     c                   parm                    DSrecbrv
360400070220     c
360500070220     C* FLAG PER  CHIUDERE POI I pgm
360600070220     C                   MOVEL     'C'           CFS44             1
360700970117     C                   ENDIF
360800970116     C*
360900070116     C     KART          READE     fnbrv07L                               33
361000970116     C                   ENDDO
361100970116     C*
361200070220     C                   CLEAR                   dls45FLG2
361300970116     C                   ENDSR
361400101201     C**************************************************************************
361500101201     C* AGGIORNO DATE E SE NECESSAIO ABILITO BOLLE
361600101201     C**************************************************************************
361700101201     C     ABIBOL        BEGSR
361800070220     c*** richiamo nuovo pgm di caricamento spunte FNLs45R
361900070220     c                   movel     brvpes        dls45pes
362000070221     c*
362100070221     C                   eval      d_brvnvr= BrvNvr
362200070221     C                   eval      d_brvnpg= BrvNPG
362300070221     C                   eval      D_brvnfv= BrvNFV
362400070221     C                   eval      d_brvlnp= BrvLNP
362500070221     C                   eval      d_brvlna= BrvLNA
362600070221     C                   eval      d_brvnrs= BrvNRS
362700070221     C                   eval      d_brvnsc= BrvNSC
362800070221     C                   eval      d_brvcan= BrvCAN
362900070221     C                   eval      d_brvnps= BrvNPS
363000070221     C                   eval      d_brvvuc= BrvVUC
363100070221     C                   eval      d_brvpuc= BrvPUC
363200070221     C                   eval      d_brvdfs= BrvDFS
363300070221     C                   eval      d_brvhms= BrvHMS
363400070221     C                   eval      d_brvmis= Brvmis
363500070221     C                   eval      d_brvznc= BrvZNC
363600070221     c                   eval      d_brvpru= brvpru
363700070221     c                   eval      d_brvfgs= brvfgs
363800070221     c                   eval      d_brvtap= brvtap
363900070221     c                   eval      d_brvpes= brvpes
364000070221     c                   eval      d_brvatr= brvatr
364100070221     c                   eval      d_brvfle= brvfle
364200070221     c                   eval      d_brvftr= brvftr
364300070221     c                   eval      d_brvdtr= brvdtr
364400070221     c                   eval      d_brvhtr= brvhtr
364500070221     c                   eval      d_brvtsu= brvtsu
364600101202     c                   eval      d_brvdcs= brvdcs
364700101202     c                   eval      d_brvhcs= brvhcs
364800070221     c
364900070220     c                   call      'FNLS45R'
365000070220     c                   parm                    fnls45ds
365100070220     c                   parm                    DSrecbrv
365200921020     C*
365300070220     C*  ERRORE: BOLLA  ALLOCATA
365400070220     C     dls45FLG      IFEQ      'E'
365500950711     C                   SETON                                        9531
365600921020     C                   END
365700950711     C*  ERRORE: BOLLA  GIA' CONSEGNATA
365800070220     C     dls45FLG      IFEQ      'N'
365900950711     C                   SETON                                        9631
366000950711     C                   END
366100950711     C* FLAG PER  CHIUDERE POI I FILE DEL PGM
366200930521     C                   MOVEL     'C'           CFS44             1
366300921020     C                   ENDSR
366400940727     C**************************************************************************
366500940727     C* PRENDO ZONA DA BOLLA
366600940727     C**************************************************************************
366700920812     C     ZONA          BEGSR
366800021028     c* Devo solo prendere la zona dalla bolla per cui posso leggere la boll
366900021028     c*  in assoluto tanto non mi interessa se è mio o no.
367000940727     C*
367100920812     C                   Z-ADD     0             BRVZNC
367200101201     C     KART          CHAIN     FNBLT27L                           34
367300101201     C  N34KBLP          CHAIN     FNBLP01L                           34
367400150623    1c                   if        not *in34
367500101201     C                   MOVEL     BLPZNC        BRVZNC
367600150623   x1C                   else
367700120615     C     KART          CHAIN     FNART27L                           34
367800101201     C  N34Kblp          CHAIN     FNARB01L                           34
367900150623    2c                   if        not *in34
368000101201     C                   MOVEL     blpZNC        BRVZNC
368100101201     C                   ENDif
368200101201     C                   ENDif
368300921020     C*
368400920812     C                   ENDSR
368500960523     C**************************************************************************
368600961121     C*    INTERROGAZIONE ANOMALIE / EVENTI SEGNACOLLI
368700960523     C**************************************************************************
368800960523     C     INTANM        BEGSR
368900031119     c                   setoff                                       90
369000960523     C                   READC     LR04S02                                31
369100960523     C*
369200960523    1C     *IN31         DOWEQ     *OFF
369300960523     C* Imposto indicatori memorizzati nei campi hidden
369400960523     C                   EXSR      SRIND1
369500960523    2C     RECORD        IFNE      *ZEROS
369600961121     C     V2CANM        ANDNE     ' '
369700960523     C     V2CNFV        ORNE      0
369800961121     C     V2CANM        ANDNE     ' '
369900960523    3C     V2CNSC        IFGT      *ZEROS
370000961122     C* INTERROGAZIONE ANOMALIE (SOLO SE PGM NON RICHIAMATO)
370100961122    4C  N65V2CANM        IFEQ      'I'
370200960523     C                   CLEAR                   DSLR44
370300960523     C                   MOVE      V2CLNP        D44FIL
370400960523     C                   MOVE      V2CLNA        D44LNA
370500960523     C                   MOVE      V2CNRS        D44NRS
370600960523     C                   MOVE      V2CNSC        D44SCN
370700960523     C                   MOVE      'I'           D44MOD
370800960523     C                   MOVEL     DSLR44        KPJBU
370900960523     C                   CALL      'FNLR44R'
371000960523     C                   PARM                    KPJBA
371100961121    4C                   ENDIF
371200070220     C* INTERROGAZIONE EVENTI SEGNACOLLI esteri
371300031119    4C     V2CANM        IFEQ      'E'
371400031119     c                   z-add     v2clna        num3
371500031119     c                   exsr      cazorg
371600031119     C     §OGNTW        IFEQ      'FED'
371700031119     C     §OGNTW        OREQ      'EEX'
371800060322     C     §OGNTW        OREQ      'DPD'
371900961121     C                   CLEAR                   DSLR45
372000961121     C                   MOVE      V2CLNP        D45FIL
372100961121     C                   MOVE      V2CLNA        D45LNA
372200961121     C                   MOVE      V2CNRS        D45NRS
372300961121     C                   MOVE      V2CNSC        D45SCN
372400961121     C                   MOVEL     DSLR45        KPJBU
372500961121     C                   CALL      'FNLR45R'
372600961121     C                   PARM                    KPJBA
372700031119     c                   else
372800031119     c* errore --> lna che non prevede eventi segnacollo
372900031119     c                   seton                                        789031
373000031119     c                   endif
373100961121    4C                   ENDIF
373200140305     C* INTERROGAZIONE spunte da pc vdl
373300140305    4C     V2CANM        IFEQ      'V'
373400140305     c                   clear                   fims40ds
373500140305     c                   eval      i40pes=v2cpes
373600140305     c                   eval      i40lnp=v2clnp
373700140305     c                   eval      i40lna=v2clna
373800140305     c                   eval      i40nrs=v2cnrs
373900140305     c                   eval      i40nsc=v2cnsc
374000140305     c                   eval      i40dfs=v2cdfs
374100140305     c                   call      'FIMS40R'
374200140305     C                   PARM                    kpjba
374300140305     C                   PARM                    fims40ds
374400140305     c                   endif
374500960523    3C                   END
374600961121     C*
374700960523     C                   CLEAR                   V2CANM
374800960523     C                   Z-ADD     NRR2          REC2
374900960523    2C                   END
375000960523     C*
375100960523     C                   SETON                                        36
375200960523     C                   UPDATE    LR04S02
375300960523     C                   SETOFF                                       36
375400960523     C*
375500031119     C  n31              READC     LR04S02                                31
375600960523    1C                   ENDDO
375700960523     C                   ENDSR
375800940727     C**************************************************************************
375900940727     C*    PULISCO CAMPI PER INSERIMENTO DAL AL
376000940727     C**************************************************************************
376100920323     C     PULDAL        BEGSR
376200940727     C*
376300941201     C                   MOVEL     V1CNPG        V3CNPG
376400941201     C                   Z-ADD     VI1NFV        V3CNFV
376500941201     C                   Z-ADD     V1CLNP        V3CLNP
376600941201     C                   MOVEL     *BLANKS       V3DLNP
376700941201     C                   Z-ADD     V1CLNA        V3CLNA
376800941201     C                   MOVEL     *BLANKS       V3DLNA
376900941201     C                   MOVEL     *BLANKS       V3DSPG
377000941201     C                   Z-ADD     0             V3CNRS
377100941201     C                   Z-ADD     0             V3CNSD
377200941201     C                   Z-ADD     0             V3CNSA
377300941201     C                   Z-ADD     0             V3CNUM
377400940727     C*
377500920323     C                   ENDSR
377600940727     C**************************************************************************
377700940727     C*    CREO SPUNTE DAL AL
377800940727     C**************************************************************************
377900920323     C     AGGDAL        BEGSR
378000940727     C*
378100920323     C* RICERCO PRIMO POSTO VUOTO PER INSERIMENTO IN SUBFILE
378200940727     C                   Z-ADD     SAVNRR        NRR2
378300920323     C                   Z-ADD     0             RECORD
378400920323     C                   SETON                                        36
378500950705     C                   SETOFF                                       24
378600941201     C     NRR2          CHAIN     LR04S02                            31
378700920323     C*
378800920323     C     RECORD        DOWEQ     *ZEROS
378900940727     C                   SUB       1             NRR2
379000940727     C     NRR2          IFEQ      0
379100920323     C                   SETON                                        31
379200920323     C                   END
379300941201     C  N31NRR2          CHAIN     LR04S02                            31
379400920323     C  N31              END
379500000121     C* DEVO AGGIORNARE L'ULTIMO RECORD LETTO SE NRR2 > 0 PERCHE'
379600000121     C* SE E' DI NUOVA IMMISSIONE O E' STATO MANUTENZIONATO
379700000121     C* DEVE POTER ESSERE RILETTO CON READE DALLA ROUTINE REGIS
379800000121     C     NRR2          IFGT      0
379900000121     C                   UPDATE    LR04S02
380000000121     C                   END
380100920323     C*
380200021206     C                   SETON                                        2125
380300000119     C                   SETON                                        947172
380400960612     C                   SETOFF                                       246323
380500990428     C                   SETOFF                                       86
380600950705     C                   CLEAR                   V2CSE                          SEGNACOLLO ESISTENTE
380700960612     C                   CLEAR                   V2HESI                         SEGNACOLLO ESISTENTE
380800950705     C                   MOVEL     ' '           V2CCAN
380900950705     C                   MOVEL     *BLANKS       V2DCAN
381000950705     C                   MOVEL     ' '           V2CANN
381100950705     C                   MOVEL     '  '          V2CNPS
381200950705     C                   MOVEL     '  '          V2CZNC
381300950705     C                   Z-ADD     0             V2CDFV                         GG/MM
381400950706     C                   Z-ADD     0             V2HDFV                         AAAA/MM/GG
381500950705     C                   Z-ADD     0             V2CNRR
381600070228     C                   Z-ADD     *ZERO         V2HDCS                         DATA CARICAMENTO
381700070228     C                   Z-ADD     *ZERO         V2CDCS                         DATA CARICAMENTO
381800950705     C                   Z-ADD     *ZERO         V2CHCS                         ORA CARICAMENTO
381900000119     C                   Z-ADD     *ZERO         V2CDFS                         DATA IMM/VARENTO
382000160729     C                   Z-ADD     *ZERO         V2hDFS                         DATA IMM/VARENTO
382100000119     C                   Z-ADD     *ZERO         V2CHMS                         ORA IMM/VARINTO
382200000119     C                   Z-ADD     *ZERO         V2CDTR                         DATA TRASMISNTO
382300000119     C                   Z-ADD     *ZERO         V2CHTR                         ORA TRASMISSENTO
382400000119     C                   MOVE      *BLANKS       V2CFTR                                     ENTO
382500021016     C***                Z-ADD     WPOSPU        V2CPES
382600021016     C                   movel     v1cfgs        V2CPES
382700950705     C                   MOVEL     *BLANKS       V2DATR                         TRANSITO
382800950705     C                   MOVEL     *BLANKS       V2CABB                         ABBINAMENTO
382900031121     C***                Z-ADD     *ZERO         V2CNPA                         CATEG.FOG.ABB.
383000031121     C***                Z-ADD     *ZERO         V2CNFA                         FOGLIO ABBINAATO
383100950705     C                   Z-ADD     *ZERO         V2CVUC                         VOLUME
383200990907     C                   Z-ADD     *ZERO         V2CPUC                         PESO
383300950705     C                   CLEAR                   V2COLD
383400960522     C                   CLEAR                   V2CANM
383500920323     C*
383600941201     C     V3CNSD        DOWLE     V3CNSA
383700940727     C                   ADD       1             NRR2
383800941201     C     NRR2          CHAIN     LR04S02                            31
383900941213     C                   MOVEL     V1CFGS        V2CFGS                         FIL.GESTIONE
384000941213     C                   MOVEL     V3CNPG        V2CNPG
384100941213     C                   Z-ADD     V3CNFV        V2CNFV
384200941213     C                   Z-ADD     V3CLNP        V2CLNP
384300941213     C                   MOVEL     V3DLNP        V2DLNP
384400941213     C                   Z-ADD     V3CLNA        V2CLNA
384500941213     C                   MOVEL     V3DLNA        V2DLNA
384600941213     C                   Z-ADD     V3CNRS        V2CNRS
384700941213     C                   Z-ADD     V3CNSD        V2CNSC
384800960531     C                   CLEAR                   V2HRCD
384900960612     C                   EXSR      SRIND
385000941201     C  N31              UPDATE    LR04S02
385100941201     C   31              WRITE     LR04S02
385200941201     C                   ADD       1             V3CNSD
385300950705     C                   ENDDO
385400920323     C* IMPOSTO CAMPI DI DUP
385500941213     C                   Z-ADD     V2CNPG        DUPNPG
385600941213     C                   Z-ADD     V2CNFV        DUPNFV
385700941213     C                   Z-ADD     V2CLNP        DUPLNP
385800941213     C                   Z-ADD     V2CLNA        DUPLNA
385900941213     C                   Z-ADD     V2CNRS        DUPNRS
386000941213     C                   Z-ADD     V2CNSC        DUPNSC
386100920323     C                   MOVEL     ' '           DUPCAN
386200920323     C*
386300920323     C* SE NON E' MULTIPLO DI 16 FINISCO DI INIZIALIZZARE
386400990920     C   31NRR2          DIV       15            DIFF
386500920323     C   31              MVR                     RESTO             5 0
386600920323     C*
386700920323     C   31RESTO         IFNE      0
386800990920     C     RESTO         DOWLT     15
386900941213     C                   MOVEL     *BLANKS       V2CANN
387000941213     C                   MOVEL     V1CNPG        V2CNPG
387100941213     C                   MOVEL     V1CFGS        V2CFGS
387200941213     C                   Z-ADD     0             V2CNFV
387300941214     C                   Z-ADD     0             V2CDFV                         GG/MM
387400950706     C                   Z-ADD     0             V2HDFV                         AAAA/MM/GG
387500941213     C                   Z-ADD     0             V2CLNP
387600941213     C                   MOVEL     *BLANKS       V2DSPG
387700941213     C                   MOVEL     *BLANKS       V2DLNP
387800941213     C                   Z-ADD     0             V2CLNA
387900941213     C                   MOVEL     *BLANKS       V2DLNA
388000941213     C                   Z-ADD     0             V2CNRS
388100941213     C                   Z-ADD     0             V2CNSC
388200950207     C                   CLEAR                   V2CSE                          SEGNACOLLO ESISTENTE
388300950705     C                   CLEAR                   V2HNPG
388400941213     C                   MOVEL     '  '          V2CZNC
388500941213     C                   MOVEL     '  '          V2CNPS
388600941213     C                   MOVEL     *BLANKS       V2CCAN
388700941213     C                   MOVEL     *BLANKS       V2DCAN
388800070228     C                   Z-ADD     *ZERO         V2HDCS                         DATA CARICAMENTO
388900070228     C                   Z-ADD     *ZERO         V2CDCS                         DATA CARICAMENTO
389000941213     C                   Z-ADD     *ZERO         V2CHCS                         ORA CARICAMENTO
389100000119     C                   Z-ADD     *ZERO         V2CDFS                                     ENTO
389200160729     C                   Z-ADD     *ZERO         V2hDFS                                     ENTO
389300000119     C                   Z-ADD     *ZERO         V2CHMS                                     NTO
389400070131     C                   Z-ADD     *ZERO         V2Cmis                         ORA IMM/VARIAZ.
389500000119     C                   Z-ADD     *ZERO         V2CDTR                                     ENTO
389600000119     C                   Z-ADD     *ZERO         V2CHTR                                     NTO
389700070131     C                   CLEAR                   V2CFTR
389800070131     C                   CLEAR                   V2Cpru
389900070131     C                   CLEAR                   V2Ctap
390000021016     C                   movel     v1cfgs        V2CPES
390100941213     C                   MOVEL     *BLANKS       V2DATR                         TRANSITO
390200941213     C                   MOVEL     *BLANKS       V2CABB                         ABBINAMENTO
390300941213     C                   Z-ADD     *ZERO         V2CVUC                         VOLUME
390400990907     C                   Z-ADD     *ZERO         V2CPUC                         PESO
390500021127     c                   Clear                   V2hVuc
390600021127     c                   Clear                   V2hPuc
390700941213     C                   Z-ADD     0             V2CNRR
390800950207     C                   CLEAR                   V2COLD
390900960522     C                   CLEAR                   V2CANM
391000960612     C                   CLEAR                   V2HESI
391100960522     C                   MOVE      RCDSFL        V2HRCD
391200960405     C                   EXSR      SRIND
391300940727     C                   ADD       1             NRR2
391400920323     C                   ADD       1             RESTO
391500941201     C                   WRITE     LR04S02
391600920323     C                   END
391700920323     C                   END
391800920323     C*
391900940727     C   31              Z-ADD     NRR2          SAVNRR
392000940727     C                   Z-ADD     NRR2          REC2
392100920323     C                   SETOFF                                       3621
392200940727     C*
392300920323     C                   ENDSR
392400940727     C**************************************************************************
392500940727     C* CARICO L6
392600940727     C**************************************************************************
392700920807     C     CARL6         BEGSR
392800940727     C*
392900950704     C                   CLEAR                   DSUL06
393000950704     C                   MOVE      '£6'          D06COD
393100950704     C                   MOVEL     V1CFGS        D06KEY
393200950704     C                   MOVEL     'L'           D06TLA
393300950704     C                   MOVEL     DSUL06        KPJBU
393400950704     C                   CALL      'TRUL06R'
393500950704     C                   PARM                    KPJBA
393600950704     C                   MOVEL     KPJBU         DSUL06
393700950704     C                   MOVEA     LIN           L6
393800950704     C                   ENDSR
393900940727     C**************************************************************************
394000940727     C* CHAINSU AZORG
394100940727     C**************************************************************************
394200930707     C     CAZORG        BEGSR
394300940727     C*
394400930707     C     NUM3          CHAIN     AZORG01L                           39
394500930707     C  N39ORGFAG        IFNE      'F'
394600930707     C     ORGFAG        ANDNE     'A'
394700930707     C                   SETON                                        39
394800930707     C                   END
394900020207     C                   MOVEL     ORGDE3        OG143
395000940727     C*
395100930707     C                   ENDSR
395200960405     C**************************************************************************
395300960405     C* MEMORIZZAZIONE INDICATORI
395400960405     C**************************************************************************
395500960405     C     SRIND         BEGSR
395600080318     C                   MOVE      *IN19         V2HI19
395700080318     C                   MOVE      *IN21         V2HI21
395800960405     C                   MOVE      *IN23         V2HI23
395900960405     C                   MOVE      *IN24         V2HI24
396000960405     C                   MOVE      *IN25         V2HI25
396100960405     C                   MOVE      *IN63         V2HI63
396200970131     C                   MOVE      *IN66         V2HI66
396300090416     C                   MOVE      *IN70         V2HI70
396400990428     C                   MOVE      *IN86         V2HI86
396500960405     C                   MOVE      *IN94         V2HI94
396600000119     C                   MOVE      *IN71         V2HI71
396700000119     C                   MOVE      *IN72         V2HI72
396800021206     C                   MOVE      *IN75         V2HI75
396900021206     C                   MOVE      *IN76         V2HI76
397000960405     C                   ENDSR
397100960405     C**************************************************************************
397200960405     C* IMPOSTA INDICATORI VIDEO CON CAMPI HIDDEN
397300960405     C**************************************************************************
397400960405     C     SRIND1        BEGSR
397500080318     C                   MOVE      V2HI19        *IN19
397600080318     C                   MOVE      V2HI21        *IN21
397700960405     C                   MOVE      V2HI23        *IN23
397800960405     C                   MOVE      V2HI24        *IN24
397900960405     C                   MOVE      V2HI25        *IN25
398000960405     C                   MOVE      V2HI63        *IN63
398100970131     C                   MOVE      V2HI66        *IN66
398200090416     C                   MOVE      V2HI70        *IN70
398300990428     C                   MOVE      V2HI86        *IN86
398400960405     C                   MOVE      V2HI94        *IN94
398500000119     C                   MOVE      V2HI71        *IN71
398600000119     C                   MOVE      V2HI72        *IN72
398700021206     C                   MOVE      V2HI75        *IN75
398800021206     C                   MOVE      V2HI76        *IN76
398900960405     C                   ENDSR
399000960829     C**************************************************************************
399100960829     C* RICERCA SE FV DI DEFLUENZA O LOCALE
399200960829     C**************************************************************************
399300960829     C     RICTTR        BEGSR
399400960917     C                   CLEAR                   DSTV
399500000522    1C     WTTR          IFNE      *BLANKS
399600960829     C* Se non è locale verifico se è una defluenza
399700960829    2C     WTTR          IFNE      'L'
399800960829     C                   MOVEL     'TV'          KCOD
399900960829     C                   MOVEL(P)  WTTR          KKEY
400000960829     C     KTAB2         CHAIN     TABEL                              34
400100960829    3C     *IN34         IFEQ      *OFF
400200960829     C                   MOVEL     TBLUNI        DSTV
400300960829    4C     §TVDEF        IFEQ      'S'
400400960829     C                   MOVEL     'D'           V2DSPG
400500960829    4C                   END
400600960829    3C                   END
400700960829   X2C                   ELSE
400800960829     C                   MOVEL     WTTR          V2DSPG
400900960829    2C                   END
401000000522    1C                   ENDIF
401100000519     C*
401200960829     C                   ENDSR
401300980213     C**************************************************************************
401400980213     C* DETERMINA PISTOLA PRIORITARIA
401500980213     C**************************************************************************
401600980213     C     SRNPS         BEGSR
401700980213     C* NPS1/NPS2 SONO I COD.PISTOLA DI CUI DETERMINARE IL PRIORITARIO
401800070228     C* NPSP      E' IL CODICE PISTOLA PRIORITARIO da tenere in spunta
401900980213     C                   CLEAR                   NPSP
402000070215     C                   CLEAR                   tapP
402100980213    1C     NPS1          IFEQ      NPS2
402200980213     C                   Z-ADD     NPS1          NPSP
402300070215     c                   eval      tapp=tap1
402400070228     c                   eval      DCSp=dcs1
402500070228     c                   eval      HCSp=hcs1
402600980213   X1C                   ELSE
402700980213     C* CERCO PRIORITA' NPS1
402800980213     C                   MOVEL     '7J'          KCOD
402900980213     C                   MOVEL(P)  NPS1          KKEY
403000980213     C     KTAB2         CHAIN     TABEL                              31
403100980213     C  N31              MOVEL     TBLUNI        DS7J
403200980213     C   31              MOVEL     999           §7JORD
403300980213     C                   MOVEL     §7JORD        W0030             3 0
403400980213     C* CERCO PRIORITA' NPS2
403500980213     C                   MOVEL(P)  NPS2          KKEY
403600980213     C     KTAB2         CHAIN     TABEL                              31
403700980213     C  N31              MOVEL     TBLUNI        DS7J
403800980213     C   31              MOVEL     999           §7JORD
403900980213    2C     W0030         IFLT      §7JORD
404000980213     C                   MOVEL     NPS1          NPSP
404100070215     C                   MOVEL     tap1          tapP
404200070228     c                   eval      DCSp=dcs1
404300070228     c                   eval      HCSp=hcs1
404400070215   X2C                   ELSE
404500980213     C                   MOVEL     NPS2          NPSP
404600070215     C                   MOVEL     tap2          tapP
404700070228     c                   eval      DCSp=dcs2
404800070228     c                   eval      HCSp=hcs2
404900980213    2C                   END
405000980213    1C                   END
405100980213     C                   ENDSR
405200940727     C**************************************************************************
405300940727     C*   R O U T I N E     I N I Z I A L E
405400940727     C**************************************************************************
405500940727     C     *INZSR        BEGSR
405600940727     C*
405700941130     C     KBRV1         KLIST
405800940727     C                   KFLD                    SAVNPG            1 0
405900940727     C                   KFLD                    VI1NFV
406000101201     C                   KFLD                    kFGSs
406100940727     C     KBRV2         KLIST
406200940727     C                   KFLD                    SAVNPG
406300940727     C                   KFLD                    VI1NFV
406400101201     C                   KFLD                    kFGSs
406500941201     C                   KFLD                    KLN
406600940727     C     KBRV3         KLIST
406700940727     C                   KFLD                    SAVNPG
406800940727     C                   KFLD                    VI1NFV
406900101201     C                   KFLD                    kFGSs
407000101201     C                   KFLD                    kLNPs
407100101201     C                   KFLD                    kLNAs
407200940727     C     KBRV31        KLIST
407300940727     C                   KFLD                    SAVNPG
407400940727     C                   KFLD                    VI1NFV
407500110202     C                   KFLD                    kFGSs
407600101201     C                   KFLD                    kLNPs
407700101201     C                   KFLD                    kLNAs
407800101201     C                   KFLD                    kNRSs
407900940727     C     KBRV32        KLIST
408000940727     C                   KFLD                    SAVNPG
408100940727     C                   KFLD                    VI1NFV
408200101201     C                   KFLD                    kFGSs
408300101201     C                   KFLD                    kLNPs
408400101201     C                   KFLD                    kLNAs
408500101201     C                   KFLD                    kNRSs
408600101201     C                   KFLD                    kNSCs
408700940727     C     KBRV4         KLIST
408800940727     C                   KFLD                    SAVNPG
408900101201     C                   KFLD                    kLNPs
409000940727     C     KBRV5         KLIST
409100940727     C                   KFLD                    SAVNPG
409200101201     C                   KFLD                    kLNPs
409300101201     C                   KFLD                    kLNAs
409400940727     C     KBRV51        KLIST
409500940727     C                   KFLD                    SAVNPG
409600101201     C                   KFLD                    kLNPs
409700101201     C                   KFLD                    kLNAs
409800101201     C                   KFLD                    kNRSs
409900940727     C     KBRV52        KLIST
410000940727     C                   KFLD                    SAVNPG
410100101201     C                   KFLD                    kLNPs
410200101201     C                   KFLD                    kLNAs
410300101201     C                   KFLD                    kNRSs
410400101201     C                   KFLD                    kNSCs
410500940727     C     KBRV6         KLIST
410600941213     C                   KFLD                    V2CNPG
410700941213     C                   KFLD                    V2CLNP
410800941213     C                   KFLD                    V2CLNA
410900941213     C                   KFLD                    V2CNRS
411000941213     C                   KFLD                    V2CNSC
411100941227     C                   KFLD                    KFGS
411200950706     C     KBRV7         KLIST
411300950706     C                   KFLD                    V2CNPG
411400000119     C                   KFLD                    W0060
411500950706     C                   KFLD                    KFGS
411600950706     C                   KFLD                    V2CLNP
411700950706     C                   KFLD                    V2CLNA
411800950706     C                   KFLD                    V2CNRS
411900950706     C                   KFLD                    V2CNSC
412000040303     C     KBRV3a        KLIST
412100040303     C                   KFLD                    V2CNPG
412200040303     C                   KFLD                    V2CLNP
412300040303     C                   KFLD                    V2CLNA
412400040303     C                   KFLD                    V2CNRS
412500040303     C                   KFLD                    V2CNSC
412600040303     C                   KFLD                    Kfgs
412700101201     C     KSEGNAC       KLIST
412800101201     C                   KFLD                    kLNPs
412900101201     C                   KFLD                    kLNAs
413000101201     C                   KFLD                    kNRSs
413100101201     C                   KFLD                    kNSCs
413200940727     C     KART          KLIST
413300941213     C                   KFLD                    V2CLNP
413400941213     C                   KFLD                    V2CLNA
413500941213     C                   KFLD                    V2CNRS
413600941213     C                   KFLD                    V2CNSC
413700971003     C     KBRV          KLIST
413800971003     C                   KFLD                    BRVLNP
413900971003     C                   KFLD                    BRVLNA
414000971003     C                   KFLD                    BRVNRS
414100971003     C                   KFLD                    BRVNSC
414200950118     C     KBLP          KLIST
414300940727     C                   KFLD                    BLTAAS
414400940727     C                   KFLD                    BLTLNP
414500940727     C                   KFLD                    BLTNRS
414600940727     C                   KFLD                    BLTNSP
414700101201     C     KBLPdet       KLIST
414800101201     C                   KFLD                    BLTAAS
414900101201     C                   KFLD                    BLTLNP
415000101201     C                   KFLD                    BLTNRS
415100101201     C                   KFLD                    BLTNSP
415200101201     C                   KFLD                    knscs
415300101129     C     KBLP2         KLIST
415400101129     C                   KFLD                    v1cboan
415500101129     C                   KFLD                    v1cbolp
415600101129     C                   KFLD                    v1cbors
415700101129     C                   KFLD                    v1cbosp
415800101201     C     Kbodet        KLIST
415900101201     C                   KFLD                    v1cboan
416000101201     C                   KFLD                    v1cbolp
416100101201     C                   KFLD                    v1cbors
416200101201     C                   KFLD                    v1cbosp
416300101201     C                   KFLD                    brvnsc
416400941229     C     KFVV1         KLIST                                                  FNFVV01L/05L
416500941201     C                   KFLD                    KNPG                           ..CATEGORIA
416600941201     C                   KFLD                    KNFV                           ..NR.FOGLIO
416700941229     C                   KFLD                    KFGS                           ..FIL.GESTIONE
416800941205     C     KFVA1         KLIST                                                  FNFVA01L
416900941205     C                   KFLD                    KFGS                           ..FIL.GESTIONE
417000941202     C                   KFLD                    KNFV                           ..NR.FOGLIO
417100020808     C*
417200020808     C     K02FGV01      klist                                                  FNFGV01L
417300020808     C                   kfld                    KNFV                           ..NR.FOGLIO
417400020808     C                   kfld                    KFGS                           ..P.O.GESTIONE
417500940727     C* ACCESSO   TABEL
417600940727     C     KTAB1         KLIST
417700940727     C                   KFLD                    CODUT
417800940727     C                   KFLD                    KCOD
417900940727     C     KTAB2         KLIST
418000940727     C                   KFLD                    CODUT
418100940727     C                   KFLD                    KCOD
418200940727     C                   KFLD                    KKEY
418300010622     C*
418400010622     C     KBRVE2        KLIST
418500010622     C                   KFLD                    BRVNPG
418600010622     C                   KFLD                    BRVLNP
418700010622     C                   KFLD                    BRVLNA
418800010622     C                   KFLD                    BRVNRS
418900010622     C                   KFLD                    BRVNSC
419000070710     C     KBRVE1        KLIST
419100070710     C                   KFLD                    BRVNPG
419200070710     C                   KFLD                    BRVNFV
419300070710     C                   KFLD                    BRVFGS
419400070710     C                   KFLD                    BRVLNP
419500070710     C                   KFLD                    BRVLNA
419600070710     C                   KFLD                    BRVNRS
419700070710     C                   KFLD                    BRVNSC
419800010628     C*
419900010628     C     KBRVE3        KLIST
420000010628     C                   KFLD                    DS2NPG
420100010628     C                   KFLD                    DS2LNP
420200010628     C                   KFLD                    DS2LNA
420300010628     C                   KFLD                    DS2NRS
420400010628     C                   KFLD                    DS2NSC
420500010628     C                   KFLD                    DS2NPS
420600101129     C***
420700101129     C* DATE PER INTERROGAZIONE
420800101129     C***
420900101129     C                   TIME                    WTIME            14 0          ORA E DATA
421000101129     C                   MOVE      WTIME         WDATE             8 0          GG/MM/AAAA
421100101129     C                   Z-ADD     WDATE         G02DAT
421200101129     C                   Z-ADD     *ZERO         G02INV
421300101129     C                   MOVEL     *BLANK        G02ERR
421400101129     C                   CALL      'XSRDA8'
421500101129     C                   PARM                    WLBDAT
421600101129     C                   Z-ADD     G02INV        WDATEU            8 0
421700101129     C* DATA INIZIALE: UDATE -5
421800101129     C     G02TGI        SUB       5             GIOTGI
421900101129     C*
422000101129     C                   CALL      'XSRGI8'
422100101129     C                   PARM                    WGIDAT
422200101129     C                   Z-ADD     GIODAT        WMENO1            8 0          MENO 5 GIORNI G/M/A
422300101129     C                   Z-ADD     GIOINV        WMENO2            8 0          MENO 5 GIORNI A/M/G
422400101129     C*
422500101129     C*
422600940727     C*
422700940727     C                   ENDSR
422800010622     C**************************************************************************
422900070116     C* MEMORIZZAZIONE SPUNTE DI CONSEGNA DOPPIE SU fnbrvE0F
423000010622     C**************************************************************************
423100010622     C     SRBRVE        BEGSR
423200070710     c* Se ne devo tenere solo una controllo per categoria e segnacollo
423300070710     c                   if        wnst='1'
423400070710     c
423500070116     C     KBRVE2        SETLL     fnbrvE2L                               34
423600010622     C     *IN34         IFEQ      *OFF
423700010622     C* SPUNTA NON ESISTENTE: MEMORIZZO ANCHE LA VECCHIA
423800070116     C                   WRITE     fnbrvE
423900010622     C                   ENDIF
424000070116     C* Salvo campi di fnbrv che potrebbero rimanere invariati
424100070116     C* Li dovrò reimpostare dopo la scrittura di fnbrve per poi
424200010622     C* fare i giusti aggiornamenti
424300070131     c                   clear                   brvnvr
424400010622     C                   MOVE      BRVNPS        SAWNPS
424500070220     C                   MOVE      BRVtap        SAWtap
424600010622     C                   MOVE      BRVCAN        SAWCAN
424700010622     C                   MOVE      BRVPUC        SAWPUC
424800010622     C                   MOVE      BRVVUC        SAWVUC
424900010626     C                   MOVE      BRVDCS        SAWDCS
425000010626     C                   MOVE      BRVHCS        SAWHCS
425100010622     C* IN OGNI CASO MEMORIZZO LA SPUNTA NUOVA
425200010626     C                   MOVEL     V2CFGS        BRVFGS
425300010626     C                   Z-ADD     V2CNFV        BRVNFV
425400010622     C                   MOVEL     V2CCAN        BRVCAN
425500010622     C                   Z-ADD     V2CVUC        BRVVUC
425600010622     C                   Z-ADD     V2CPUC        BRVPUC
425700010629     C                   MOVE      V2CNPS        BRVNPS
425800021016     C                   movel     v2cfgs        BRVPES
425900070215     c* millisecondi  immissione
426000070131     c                   time                    wtimez
426100070131     c                   extrct    wtimez:*ms    millisec          6 0
426200070131     c                   movel     millisec      brvmis
426300070215     c* data immissione
426400070215     c                   movel     wtimez        dataiso
426500070215     c                   movel     dataiso       brvdfs
426600070215     c* ora  immissione
426700070215     c                   move      wtimez        oraiso
426800070215     c                   movel     oraiso        brvhms
426900070215     c
427000090420     c                   movel     knmus         brvpru
427100010626     C                   CLEAR                   BRVDTR
427200010626     C                   CLEAR                   BRVHTR
427300010626     C                   CLEAR                   BRVFTR
427400010626     C                   CLEAR                   BRVDCS
427500010626     C                   CLEAR                   BRVHCS
427600010626     C                   CLEAR                   BRVATR
427700070131     C                   CLEAR                   BRVtsu
427800070131     C                   CLEAR                   BRVtap
427900070116     C                   WRITE     fnbrvE
428000070116     C* Reimposto campi di fnbrv come da lettura record spunte
428100010622     C                   MOVE      SAWNPS        BRVNPS
428200070220     C                   MOVE      SAWtap        BRVtap
428300010622     C                   MOVE      SAWCAN        BRVCAN
428400010622     C                   MOVE      SAWPUC        BRVPUC
428500010622     C                   MOVE      SAWVUC        BRVVUC
428600010626     C                   MOVE      SAWDCS        BRVDCS
428700010626     C                   MOVE      SAWHCS        BRVHCS
428800070710     c
428900070710     c                   else
429000070710     c* Se è per l'anomalia controllo per foglio e segnacollo
429100070710     C     KBRVE1        SETLL     fnbrvE1L                               34
429200070710     C     *IN34         IFEQ      *OFF
429300070710     C* SPUNTA NON ESISTENTE: MEMORIZZO solo  LA VECCHIA
429400070710     C                   WRITE     fnbrvE
429500070710     C                   ENDIF
429600070710     C                   ENDIF
429700070710     c
429800010622     C                   ENDSR
429900010626     C**************************************************************************
430000010626     C* CANCELLAZIONE SPUNTE DOPPIE A FRONTE DI ANNULLAMENTO O MODIFICA
430100010626     C**************************************************************************
430200010626     C     DLBRVE        BEGSR
430300010628     C* DELETO SPUNTE A PARITA' DI PISTOLA E PER DATA FOGLIO >=
430400010628     C                   MOVEL     V2HRCD        DS2RCD
430500010628     C                   MOVE      V2CNPS        DS2NPS
430600070116     C     KBRVE3        SETLL     fnbrvE2L
430700070116     C     KBRVE3        READE     fnbrvE2L                               33
430800010626     C     *IN33         DOWEQ     *OFF
430900010628     C                   CLEAR                   DSLV53
431000010628     C                   MOVEL     'V'           D53TFO
431100010628     C                   Z-ADD     BRVNPG        D53NPG
431200010628     C                   Z-ADD     SIMFEL        D53FEL
431300010628     C                   Z-ADD     BRVNFV        D53NFV
431400010628     C                   Z-ADD     BRVFGS        D53FGS
431500010628     C                   CALL      'FNLV53R'
431600010628     C                   PARM                    DSLV53
431700010628     C     D53ERR        IFEQ      *BLANKS
431800010628     C     D53DFV        ANDGE     V2HDFV
431900070116     C                   DELETE    fnbrvE
432000010628     C                   ENDIF
432100070116     C     KBRVE3        READE     fnbrvE2L                               33
432200010626     C                   ENDDO
432300010626     C                   ENDSR
432400150824     C**************************************************************************
432500150824     C*    Aggiornamenti da tabella BSP
432600150824     C**************************************************************************
432700150824     c     DA_BSP        BEGsr
432800150824     c* Scrivo FNAGB per aggiornare la bolla
432900150824     c                   eval      klnps=brvlnp
433000150824     c                   eval      klnas=brvlna
433100150824     c                   eval      knrss=brvnrs
433200150824     c                   eval      knscs=brvnsc
433300150824     c     ksegnac       chain     fnblt27l
433400150824     c                   if        %found(fnblt27L)
433500150824     C                   CLEAR                   FNAGB000
433600150824     C                   MOVEL     'P'           AGBTBO
433700150824     C                   MOVEL     'CB'          AGBAGB
433800150824     C                   MOVEL     bltAAS        AGBAAS
433900150824     C                   MOVEL     bltLNP        AGBLNP
434000150824     C                   MOVEL     bltNRS        AGBNRS
434100150824     C                   MOVEL     bltNSP        AGBNSP
434200150824     C                   WRITE     FNAGB000                             99
434300150824     c                   endif
434400150824     c
434500150824     c                   ENDSR
434600090416      *****************************************************************
434700090416      *  windos per spunta senza anomalia
434800090416      *****************************************************************
434900090416     C     wind06        BEGSR
435000090416     c* Aggancio fndcd per avere segnacollo
435100090416     c                   clear                   w06nsc
435200090420     c                   movel     v2cfgs        kfgs
435300090420     c                   z-add     v2cnfv        w0060
435400090420     c     kbrv7         chain(n)  fnbrve1l
435500090420    1c                   if        %found(fnbrve1l) and e_brvcan=' '
435600090420     c
435700090416     c* riempo videata
435800090420     c                   eval      w06lnp=e_brvlnp
435900090420     c                   eval      w06lna=e_brvlna
436000090420     c                   eval      w06nrs=e_brvnrs
436100090420     c                   eval      w06nsc=e_brvnsc
436200090420     c                   eval      w06npg=e_brvnpg
436300090420     c                   eval      w06nfv=e_brvnfv
436400090420     c                   eval      w06fgs=e_brvfgs
436500090416     c                   clear                   w06dfv
436600090420     c                   clear                   dslv53
436700090416     c                   eval      d53nfv=w06nfv
436800090416     c                   eval      d53fgs=w06fgs
436900090416     c                   eval      d53npg=w06npg
437000090416     c                   call      'FNLV53R'
437100090420     c                   parm                    dslv53
437200090416     c
437300090420    2c                   if        d53dfv>0
437400090416     c                   eval      Dataiso=%date(d53dfv:*iso)
437500090416     c                   eval      Dataeur=dataiso
437600090416     c                   eval      w06dfv=%dec(Dataeur)
437700090420    2c                   endif
437800090416     c
437900090416     c* spunta senza anomalia
438000090416     c                   eval      Dataiso=%date(e_brvdfs:*iso)
438100090416     c                   eval      Dataeur=dataiso
438200090416     c                   eval      w06dfs=%dec(Dataeur)
438300090416     c
438400090416     c                   eval      Dataiso=%date(e_brvdcs:*iso)
438500090416     c                   eval      Dataeur=dataiso
438600090416     c                   eval      w06dcs=%dec(Dataeur)
438700090416     c
438800090416     c                   eval      w06hms=E_brvhms
438900090416     c                   eval      w06hcs=E_brvhcs
439000090416     c                   eval      w06nps=%editc(E_brvnps:'X')
439100090416     c                   clear                   w06tap
439200090420    2c                   if        e_brvtap<>' '
439300090416     C                   CLEAR                   tibs02ds
439400090416     C                   MOVEL     'C'           T02MOD
439500090416     C                   MOVEL     KNSIF         T02SIF
439600090416     C                   MOVEL     'APS'         T02COD
439700090416     C                   MOVEL     e_BRVTAP      T02KE1
439800090416     C                   CALL      'TIBS02R'
439900090416     C                   PARM                    KPJBA
440000090416     C                   PARM                    tibs02ds
440100090416     c
440200090420    3c                   if        t02err<>' '
440300090416     c                   eval      t02uni=*all'?'
440400090420    3c                   endif
440500090416     c
440600090416     c                   eval      w06tap=e_brvtap+'-'+%subst(t02uni:1:9)
440700090420    2c                   endif
440800090416     c
440900090416     c* spunta con   anomalia
441000090420     c                   eval      w06can='"'+v2ccan+'"'
441100090416     c
441200090420     c                   eval      w06cdfs=v2cdfs
441300090416     c
441400090420     c                   eval      w06cdcs=v2cdcs
441500090416     c
441600090420     c                   eval      w06chms=v2chms
441700090420     c                   eval      w06chcs=v2chcs
441800090420     c                   eval      w06cnps=v2cnps
441900090416     c                   clear                   w06ctap
442000090420    2c                   if        %subst(v2ctap:1:1)<>' '
442100090416     C                   CLEAR                   tibs02ds
442200090416     C                   MOVEL     'C'           T02MOD
442300090416     C                   MOVEL     KNSIF         T02SIF
442400090416     C                   MOVEL     'APS'         T02COD
442500090420     c                   eval      t02ke1=%subst(v2ctap:1:1)
442600090416     C                   CALL      'TIBS02R'
442700090416     C                   PARM                    KPJBA
442800090416     C                   PARM                    tibs02ds
442900090416     c
443000090416     c                   if        t02err<>' '
443100090416     c                   eval      t02uni=*all'?'
443200090416     c                   endif
443300090416     c
443400090420     c                   eval      w06ctap=%subst(v2ctap:1:1)+'-'+
443500090420     c                                     %subst(t02uni:1:9)
443600090420    2c                   endif
443700090416     c
443800090416     c
443900090416     c* se ho trovato visualizzo
444000090420    2c                   if        w06nsc>0
444100090420    3c                   do        *HIVAL
444200090420     c                   exfmt     lr04w06
444300090416     c   KL              leave
444400090420    3c                   enddo
444500090420    2c                   endif
444600090416     c
444700090420     c                   setoff                                       31
444800090420     c                   else
444900090420     c                   seton                                        31
445000090420    1c                   endif
445100090416      *
445200090416     C                   ENDSR
445300160728      *-------------------------------------------------------------------------
445400160728      * ordinamento sfl in base alle richieste
445500160728      *-------------------------------------------------------------------------
445600160728     C     sr_ordina     BEGSR
445700160728      * IMPOSTO ENTRATA AD 'A'
445800160728     C                   EVAL      nrr2   = 0
445900160728      *
446000160728     C                   CLEAR                   QLGSCB
446100160728     C                   CLEAR                   QLGSCB00
446200160728
446300160728     c* ordinamento per data foglio e data/ora immissione
446400160728     ** 3 campi chiave.
446500160728     c* data foglio
446600160728     C                   EVAL      QLGNBRK    = 3
446700160823     C****               EVAL      QLGSP      = 65
446800160822     C                   EVAL      QLGSP      = 1 + %size(rcdsfl) +
446900160822     C                                            + %size(v2cnrr) +
447000160822     C                                            + %size(v2ctps) +
447100160822     C                                            + %size(v2crps) +
447200160822     C                                            + %size(v2cse) +
447300160822     C                                            + %size(v2cold)
447400160728     C                   EVAL      QLGSS      = %SIZE(v2hdfv)
447500160728     C                   EVAL      QLGDT      = carattere
447600161019     C                   EVAL      QLGSO      = Discendente
447700160728     C                   EVAL      QLGKL(1)   = QLGSKL
447800160728     ** data immissione
447900160823     C***                EVAL      QLGSP      = 269
448000160822     C                   EVAL      QLGSP      = 1 + %size(rcdsfl) +
448100160822     C                                            + %size(v2cnrr) +
448200160822     C                                            + %size(v2ctps) +
448300160822     C                                            + %size(v2crps) +
448400160822     C                                            + %size(v2cse) +
448500160822     C                                            + %size(v2cold)+
448600160822     C                                            + %size(v2hdfv) +
448700160822     C                                            + %size(v2hnpg) +
448800160822     C                                            + %size(V2HESI) +
448900160822     C                                            + %size(V2HI19)
449000160822     C                                            + %size(V2HI21)
449100160822     C                                            + %size(V2HI23)
449200160822     C                                            + %size(V2HI24)
449300160822     C                                            + %size(V2HI25)
449400160822     C                                            + %size(V2HI63)
449500160822     C                                            + %size(V2HI66)
449600160822     C                                            + %size(V2HI70)
449700160822     C                                            + %size(V2HI86)
449800160822     C                                            + %size(V2HI94)
449900160822     C                                            + %size(V2HI71)
450000160822     C                                            + %size(V2HI72)
450100160822     C                                            + %size(V2HI75)
450200160822     C                                            + %size(V2HI76)
450300160822     C                                            + %size(v2hrcd)
450400160822     C                                            + %size(v2hvuc)
450500160822     C                                            + %size(v2hpuc)
450600160822     C                                            + %size(v2hdcs)
450700160822     C                                            + %size(v2cfgs)
450800160822     C                                            + %size(v2dspg)
450900160822     C                                            + %size(v2cdfv)
451000160822     C                                            + %size(v2dlnp)
451100160822     C                                            + %size(v2dlna)
451200160822     C                                            + %size(v2cznc)
451300160822     C                                            + %size(v2dcan)
451400160822     C                                            + %size(v2cnps)
451500160822     C                                            + %size(v2canm)
451600160822     C                                            + %size(v2cpes)
451700160822     C                                            + %size(v2cdfs)
451800160822     C                                            + %size(v2chms)
451900160822     C                                            + %size(v2cmis)
452000160822     C                                            + %size(v2cpru)
452100160822     C                                            + %size(v2ctap)
452200160822     C                                            + %size(v2datr)
452300160822     C                                            + %size(v2ctsu)
452400160822     C                                            + %size(v2cdcs)
452500160822     C                                            + %size(v2chcs)
452600160822     c                                            + %size(v2cftr)
452700160822     c                                            + %size(v2cdtr)
452800160822     c                                            + %size(v2chtr)
452900160822     c                                            + %size(v2cabb)
453000160728     C                   EVAL      QLGSS      = %SIZE(v2hdfs)
453100160728     C                   EVAL      QLGDT      = carattere
453200161019     C                   EVAL      QLGSO      = Discendente
453300160728     C                   EVAL      QLGKL(2)   = QLGSKL
453400160728     ** ora  immissione
453500160823     C****               EVAL      QLGSP      = 195
453600160822     C                   EVAL      QLGSP      = 1 + %size(rcdsfl) +
453700160822     C                                            + %size(v2cnrr) +
453800160822     C                                            + %size(v2ctps) +
453900160822     C                                            + %size(v2crps) +
454000160822     C                                            + %size(v2cse) +
454100160822     C                                            + %size(v2cold)+
454200160822     C                                            + %size(v2hdfv) +
454300160822     C                                            + %size(v2hnpg) +
454400160822     C                                            + %size(V2HESI) +
454500160822     C                                            + %size(V2HI19)
454600160822     C                                            + %size(V2HI21)
454700160822     C                                            + %size(V2HI23)
454800160822     C                                            + %size(V2HI24)
454900160822     C                                            + %size(V2HI25)
455000160822     C                                            + %size(V2HI63)
455100160822     C                                            + %size(V2HI66)
455200160822     C                                            + %size(V2HI70)
455300160822     C                                            + %size(V2HI86)
455400160822     C                                            + %size(V2HI94)
455500160822     C                                            + %size(V2HI71)
455600160822     C                                            + %size(V2HI72)
455700160822     C                                            + %size(V2HI75)
455800160822     C                                            + %size(V2HI76)
455900160822     C                                            + %size(v2hrcd)
456000160822     C                                            + %size(v2hvuc)
456100160822     C                                            + %size(v2hpuc)
456200160822     C                                            + %size(v2hdcs)
456300160822     C                                            + %size(v2cfgs)
456400160822     C                                            + %size(v2dspg)
456500160822     C                                            + %size(v2cdfv)
456600160822     C                                            + %size(v2dlnp)
456700160822     C                                            + %size(v2dlna)
456800160822     C                                            + %size(v2cznc)
456900160822     C                                            + %size(v2dcan)
457000160822     C                                            + %size(v2cnps)
457100160822     C                                            + %size(v2canm)
457200160822     C                                            + %size(v2cpes)
457300160822     C                                            + %size(v2cdfs)
457400160728     C                   EVAL      QLGSS      = %SIZE(v2chms)
457500160728     C                   EVAL      QLGDT      = carattere
457600161019     C                   EVAL      QLGSO      = Discendente
457700160728     C                   EVAL      QLGKL(3)   = QLGSKL
457800160728
457900160728      * Load other sort parameters.
458000160728     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
458100160728     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
458200160728     C                   EVAL      QLGRT     = 8
458300160728     C                   EVAL      QLGOKL    = 80
458400160728     C                   EVAL      QLGLKE    = 16
458500160728     C                   EVAL      QLGLSS    = 290
458600160728
458700160728      * Initialize Sort I/O API fields.
458800160728     C                   EVAL      QLGRL00  = QLGRL
458900160728     C                   EVAL      QLGRC00  = 1
459000160728     C                   CLEAR                   QUSEI
459100160728     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
459200160728
459300160728      * First step - Initialize the sort routine.
459400160728     C                   CALL      'QLGSORT'
459500160728     C                   PARM                    QLGSCB
459600160728     C                   PARM                    NotUsed
459700160728     C                   PARM                    NotUsed
459800160728     C                   PARM                    SizeList
459900160728     C                   PARM                    ReturnSize
460000160728     C                   PARM                    QUSEC
460100160728
460200160728      * Next step - Write records to I/O routine.
460300160728     C                   EVAL      QLGRT00 = Put
460400160728
460500160728     C                   DO        RrnLast       nrr2
460600160728
460700160728     C     nrr2          CHAIN     lr04s02
460800160728     c                   if        not %found
460900160728     c                   leave
461000160728     c                   endif
461100160728     ** Solo le righe con Selected = 'Y' sono riordinate,
461200160728     ** quindi per fare un ordinamento di tutte le righe
461300160728     ** metto 'Y' sempre.
461400160728     C                   EVAL      Selected = 'Y'
461500160728
461600160728     C                   CLEAR                   QUSEI
461700160728     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
461800160728     C                   CALL      'QLGSRTIO'
461900160728     C                   PARM                    QLGSCB00
462000160728     C                   PARM                    SflRcd
462100160728     C                   PARM                    NotUsed
462200160728     C                   PARM                    SizeList
462300160728     C                   PARM                    NotUsed
462400160728     C                   PARM                    QUSEC
462500160728
462600160728     C                   ENDDO
462700160728
462800160728      * Next step - Signal end of input, clear subfile for reload.
462900160728     C                   EVAL      QLGRT00 = EndPut
463000160728     C                   CLEAR                   QUSEI
463100160728     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
463200160728
463300160728     C                   CALL      'QLGSRTIO'
463400160728     C                   PARM                    QLGSCB00
463500160728     C                   PARM                    SflRcd
463600160728     C                   PARM                    NotUsed
463700160728     C                   PARM                    SizeList
463800160728     C                   PARM                    NotUsed
463900160728     C                   PARM                    QUSEC
464000160728
464100160728     c                   seton                                        35
464200160728     C                   WRITE     lr04c02
464300160728     c                   setoff                                       35
464400160728     c                   write     lr04z02
464500160728
464600160728      * Final step - Write the records back to the subfile.
464700160728     C                   EVAL      QLGRT00 = Get
464800160728     C                   DO        RrnLast       nrr2
464900160728     C                   CLEAR                   QUSEI
465000160728     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
465100160728     C                   CALL      'QLGSRTIO'
465200160728     C                   PARM                    QLGSCB00
465300160728     C                   PARM                    NotUsed
465400160728     C                   PARM                    SflRcd
465500160728     C                   PARM                    QLGRL00
465600160728     C                   PARM                    NotUsed
465700160728     C                   PARM                    QUSEC
465800160728      *
465900160822     c                   exsr      srind1
466000160728     C                   WRITE     lr04s02
466100160728     C                   ENDDO
466200160823     c                   z-add     1             rec2
466300160728
466400160728      *
466500160728     C                   ENDSR
466600101129**
466700101129Se ricerca o indicato numero foglio, immettere una categoria specifica          01
466800101129Se ricerca o indicato numero foglio, immettere la filiale gestione              02
466900101129Numero foglio errato o annullato                                                03
467000101129Manutenzione delle spunte dei fogli viaggio partenza permessa solo ai terminal  04
467100101129Numero spedizione inesistente                                                   05
467200101129Serie incongruente con quella della spedizione                                  06
467300101129Linea di arrivo incongruente con quella della spedizione                        07
467400101216Linea partenza  segnacollo incongruente con quella della spedizione             08
467500101216Il segnacollo immesso non è relativo alla spedizione indicata                   09

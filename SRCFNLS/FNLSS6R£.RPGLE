000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940913     H* FNLS36R *----------------------------------------------------*
000300940921     H*         - BORDERIZZAZIONE BOLLE - PROCEDURA BARCODE          *
000400940921     H*           PROGRAMMA FILTRO                                   *
000500000000     H*--------------------------------------------------------------*
000600940913     FFNLS36D   CF   E             WORKSTN
000700940913     F                                     SFILE(LS36S03:NR3)
000800940913     FFNFGV01L  IF   E           K DISK
000900000427     FFNFGW01L  IF   E           K DISK
001000960628     FTABEL00F  IF   E           K DISK
001100900509     FAZORG01L  IF   E           K DISK
001200130612     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001300931130     D****
001400940913     D* PASSAGGIO DATI A STAMPA BORDERO'         - FNLS37R -
001500940914     D PARAM1          DS
001600900518     D  VIDFFV                 1      3  0
001700900522     D  VIDFLP                 4      6  0
001800941007     D* STAMPANTE E MODULO ETICHETTE
001900941007     D  PA1PRE                 7     16
002000941007     D  PA1ETI                17     26
002100941007     D* STAMPANTE E MODULO BORDERO'
002200941007     D  PA1PRT                27     36
002300941007     D  PA1MDT                37     46
002400941007     D* PGM DI STAMPA ETICHETTA BORDERO'
002500941007     D  PA1PSE               228    237
002600020813     D* P.O. di Gestione
002700020813     D  PA1pog               238    240  0 inz
002800940919     D  NFV                  241    245  0
002900940914     D  DBR                  247    254  0
003000931130     D* FLG = ' ' PER STAMPA ><' ' PER RISTAMPA BORDERO'
003100931130     D  FLG                  255    255    INZ
003200120202     D  vidssp               256    256
003300941007     D***
003400951031     D* PASSAGGIO DATI A MANUTENZIONE FV            - FNLS52R -
003500940913     D* PASSAGGIO DATI A STAMPA FV                  - FNLS94C -
003600900521     D PARAM2          DS
003700951031     D  PA2NFV                11     15  0
003800951031     D* FNLS52R -->  'B'BORDERO'   'M'MANUTENZIONE
003900951031     D* FNLS94C -->  ' 'STAMPA     'R'RSTAMPA
004000951031     D  PA2FLG                16     16
004100951031     D* SE PA2MSG PIENO --> C'E' STATO ERRORE
004200960201     D  PA2MSG                17     94
004300951031     D* PARF12= 'S'     --> PREMUTO F12
004400951031     D  PA2F12               102    102
004500000612     D  PA2FGS               103    105
004600951031     D*
004700941007     D  PA2PRT               164    173
004800941007     D  PA2MDT               174    183
004900130502
005000130502     D fnlst4ds        DS
005100130502     D* ilst4gES = M - MANUTENZIONE FV RICHIAMATA
005200130502     D*          B - DA BORDERO'
005300130502     D*          A - apertura foglio
005400130502     D*          C - DA CHIUSURA FV SINGOLA
005500130502     D*          I - DA CHIUSURA DA IMP
005600130502     D*          N - DA INTERROGAZIONE  FV
005700130502     D*          M - Manutenzione FGV
005800130502     D*          P - Manutenzione FGV con videata piombi
005900130502
006000130502     D* CAMPI INPUT OBBLIGATORI:
006100130502     D*  GES / FGS
006200130502     d
006300130502     D* CAMPI INPUT PER MANUTENZIONE :
006400130502     D*  NFV
006500130502     D*
006600130502     D* CAMPI INPUT SOLO PER APERTURA :
006700130502     d* DFV /OLD / TRN / NRRP / PDR
006800130502     D
006900130502     D  ilst4DFV               3     10
007000130502     D
007100130502     D  ilst4nfv              11     15
007200130502     D  IlsT4GES              16     16
007300130502     D* Se MSG pieno --> c'è stato ERRORE
007400130502     D  olst4msg              17     94
007500130502     d* se iolst4FOR è pieno --> premuto F6 per forzare foglio di traini doppio
007600130502     D  iolst4FOR             95     97
007700130502     D
007800130502     D  IlsT4OLD              98     98
007900130502     D* OLST4F12 = 'S'    --> premuto F12
008000130502     D  olst4F12             102    102
008100130502     d
008200130502     D  ilst4fgs             103    105
008300130502     D  ilst4TRN             106    112  0
008400130502     D  ilst4NRRP            113    125
008500130502     D* se non immessa il numero relativo record della proposta
008600130502     d*  si può immettere a modo vecchio il trazionista
008700130502     D  ilst4PDR             126    132
008800941007     D***
008900130520     D* CHIAMATA AL PGM DI INTERROGAZ.FV PARTENZA             -FNLST3R-
009000130520     D fnlst3ds        DS
009100951108     D  PA3FLG                 1      1    INZ('R')
009200951108     D  PA3FL2                 2      2    INZ
009300951108     D  PA3NFV                 3      5P 0 INZ
009400951108     D  PA3DIN                 6     10P 0 INZ
009500000612     D  PA3DFI                11     15P 0
009600951108     D  PA3TFV                16     16    INZ('2')
009700951108     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
009800951108     D*   SELEZIONATI ---> SE NON UTILIZZATO NON LO DEFINISCO A PGM
009900951108     D***                                 P  17 2410FVA
010000000612     D  PA3FGS               242    244  0
010100900530     D WLBDAT          DS
010200940913     D  G02DAT                 1      8  0
010300940913     D  G02INV                 9     16  0
010400940913     D  G02ERR                17     17
010500940913     D  G02TGI                18     22  0
010600900530     D WGIDAT          DS
010700940914     D  GIODAT                 1      8  0
010800940914     D  GIOINV                 9     16  0
010900940914     D  GIOTGI                17     21  0
011000910130     D                 DS
011100910405     D  FGVFFV                 1    240
011200910130     D  FGVFF2               241    447
011300000427     D  FGWFF3               448    687
011400000427     D  FGWFF4               688    897
011500000427     D  VUOTO                898    900    INZ
011600000427     D  FFV                    1    900
011700000427     D                                     DIM(300)
011800910130     D                 DS
011900910405     D  FGVFLP                 1    240
012000910130     D  FGVFL2               241    447
012100000427     D  FGWFL3               448    687
012200000427     D  FGWFL4               688    897
012300000427     D  VUOTO2               898    900    INZ
012400000427     D  FLP                    1    900
012500000427     D                                     DIM(300)
012600900511     D KPJBA         E DS
012700941007     D  MODETI               453    462
012800941007     D  VIDPRE               463    472
012900941007     D  MODTAB               473    482
013000941007     D  WPGMSE               483    492
013100941007     D  VIDPRT               493    502
013200960628     D DSTV          E DS
013300020813     D UT§DSE0F      E DS                  inz
013400020813     D CNCR80        E DS                  inz
013500020813     D OG143         e ds                  inz
013600130612     D tibs02ds      E DS
013700130612     D dvpodecofi    E DS
013800020813      *
013900020813      * Variabili
014000020813      *
014100020813     d $Poste          s              1    inz(*off)
014200130502     d parmsg          s                   like(olst4msg)
014300130612      //---------------------------------------------------------------
014400130612      /copy gaitrasrc/srcprotopr,tibs02r
014500130612
014600900521     C****************************************************************
014700900521     C*  RIEPILOGO INDICATORI
014800900521     C***************************************************************
014900931130     C* 04    - SE DOPO UN ERRORE PREMO CMD3 O CMD12 SOTTOMETTO
015000931130     C*       - COMUNQUE LA STAMPA DEL FOGLIO VIAGGIO
015100940315     C* 18    - TESTN SUL NUMERO FOGLIO VIAGGIO
015200900521     C* 40/45 - ERRORE
015300900521     C* 31/32 - DI COMODO
015400900521     C* 35    - PULIZIA SUBFILE
015500931130     C* 36    - SFLNXTCHG
015600900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
015700931130     C* 92    - OFF BOERDERO' RPOCEDURA BARCOD
015800900521     C*
015900900521     C*****************************************************************
016000000000     C     *ENTRY        PLIST
016100000000     C                   PARM                    KPJBA
016200020813     ***C                   Z-ADD     1             CODUT
016300000612     C                   CALL      'X§PARUT'
016400020813     C                   PARM                    UT§DSE0F
016500940914     C                   MOVEL     RAGUT         VIDRSU
016600000612     C                   MOVEL     REC80         CNCR80
016700941007     C* IMPOSTO STAMPANTE E MODULO FV
016800941007     C                   MOVEL     MODTAB        PA1MDT
016900941007     C                   MOVEL     VIDPRT        PA1PRT
017000941007     C* PGM STAMPA ETICHETTTE
017100941007     C                   MOVEL     WPGMSE        PA1PSE
017200941007     C* STAMPANTE E MODULO ETICHETTE
017300941007     C                   MOVEL     MODETI        PA1ETI
017400941007     C                   MOVEL     VIDPRE        PA1PRE
017500900511     C*---------------------------------------------------------------*
017600900521     C* ACCESSO   FILE NUMERATORE
017700960628     C     KTAB          KLIST
017800960628     C                   KFLD                    CODUT
017900960628     C                   KFLD                    COD               2
018000960628     C                   KFLD                    KEY               8
018100020807     C     K02FGV01      klist
018200020807     C                   kfld                    COMNFV
018300020807     C                   kfld                    SIMfel
018400020807     C     K02FGW01      klist
018500020807     C                   kfld                    FGVnfv
018600020807     C                   kfld                    FGVlnp
018700931130     C*---------------------------------------------------------------*
018800931130     C                   EXSR      CARTAB
018900020813     C*
019000020813     C* Imposto il P.O. di gestione
019100020813     C                   z-add     SIMpou        VIDpog
019200020813     C     VIDpog        chain     AZORG
019300020813if  1C                   if        %found(AZORG01L)
019400020813     C                   movel     ORGdes        DESpog
019500020813     C                   movel     ORGde3        OG143
019600020813x   1C                   else
019700020813     C                   clear                   DESpog
019800020813     C                   clear                   OG143
019900020813e   1C                   endif
020000020813if  1C                   if        §OGntw = 'PPT'
020100020813     C                   eval      $Poste = *on
020200020813e   1C                   else
020300020813     C                   z-add     SIMfel        VIDpog
020400020813     C     VIDpog        chain     AZORG
020500020813if  2C                   if        %found(AZORG01L)
020600020813     C                   movel     ORGdes        DESpog
020700020813     C                   movel     ORGde3        OG143
020800020813x   2C                   else
020900020813     C                   clear                   DESpog
021000020813     C                   clear                   OG143
021100020813e   2C                   endif
021200020813e   1C                   endif
021300931130     C*
021400900509     C     INIZIO        TAG
021500940315     C                   MOVEL     '?    '       VIDNFV
021600120206     C                   MOVEL     'S'           VIDssp
021700931130     C                   SETOFF                                       04
021800931130     C*
021900900509     C     FOR01         TAG
022000940914     C                   WRITE     LS36T01
022100940913     C                   EXFMT     LS36D01
022200940914     C                   SETOFF                                       28
022300940914     C*
022400130612     C** F3 - FINE LAVORO
022500130612     c                   if        *inkc or *in99
022600130612     C                   GOTO      FINE
022700130612     c                   endif
022800130612     c
022900931130     C* CONTROLLI FOGLIO VIAGGIO
023000931130     C                   EXSR      CONTR1
023100931130     C   90              GOTO      FOR01
023200900706     C*
023300951031     C* VADO IN VIDEATA FOGLI VIAGGIO PER METTERE I DATI
023400130502     c                   clear                   fnlst4ds
023500130502     c                   eval      ilst4ges='B'
023600130502     c                   eval      ilst4fgs=%editc(vidpog:'X')
023700130502     c                   eval      ilst4nfv=vidnfv
023800130502     c                   movel     fnlst4ds      kpjbu
023900130502     C                   CALL      'FNLST4R'
024000130502     C                   PARM                    KPJBA
024100130502     C                   MOVEL     KPJBU         fnlst4ds
024200130502     c                   eval      pa2f12=olst4f12
024300130502     c                   eval      pa2msg=olst4msg
024400130612     c**                 else
024500130612     C**                 MOVEL     VIDNFV        PA2NFV
024600130612     C**                 MOVEL     'B'           PA2FLG
024700130612     C**                 CLEAR                   PA2MSG
024800130612     C**                 CLEAR                   PA2F12
024900020813     ***C                   MOVE      SIMFEL        PA2FGS
025000130612     C**                 move      VIDpog        PA2fgs
025100130612     C**                 MOVEL     PARAM2        KPJBU
025200130612     C**                 CALL      'FNLS52R'
025300130612     C**                 PARM                    KPJBA
025400130612     C**                 MOVEL     KPJBU         PARAM2
025500130612     c**                 endif
025600951031     C* SE PA2F12 = 'S'  VADO A VIDEO 1
025700951031     C     PA2F12        IFEQ      'S'
025800900706     C                   GOTO      FOR01
025900951031     C                   ENDIF
026000951031     C* SE ERRORE LO VISUALIZZO
026100951031     C     PA2MSG        IFNE      *BLANKS
026200951031     C                   CLEAR                   VIDMSG
026300951031     C                   MOVEL     PA2MSG        VIDMSG
026400951031     C                   SETON                                        409028
026500951031     C                   GOTO      FOR01
026600951031     C                   ENDIF
026700900706     C*
026800900518     C* CARICO FILIALI DI ARRIVO IN SUBFILE
026900900518     C                   EXSR      CARLNA
027000900518     C*
027100900521     C     FORCT2        TAG
027200940914     C                   WRITE     LS36T01
027300940913     C                   WRITE     LS36D02
027400940913     C                   WRITE     LS36Z03
027500900518     C*
027600900518     C     FORCT         TAG
027700940913     C                   EXFMT     LS36C03
027800940914     C                   SETOFF                                       2845
027900940914     C*
028000911104     C** CMD3  - FINE LAVORO
028100911104     C   KC
028200911104     CAN 04              EXSR      STAFV
028300900518     C   KC              GOTO      FINE
028400900518     C** CMD12 - RITORNO
028500911104     C   KL
028600911104     CAN 04              EXSR      STAFV
028700900518     C   KL              GOTO      FOR01
028800900521     C** CMD7  - VADO IN MANUTENZIONE FOGLI VIAGGIO
028900931130    1C   KG              DO
029000130502     c                   clear                   fnlst4ds
029100130502     c                   eval      ilst4ges='M'
029200130502     c                   eval      ilst4fgs=%editc(vidpog:'X')
029300130502     c                   eval      ilst4nfv=vidnfv
029400130502     c                   movel     fnlst4ds      kpjbu
029500130502     C                   CALL      'FNLST4R'
029600130502     C                   PARM                    KPJBA
029700130502     C                   MOVEL     KPJBU         fnlst4ds
029800130502     c                   movel     olst4msg      parmsg
029900951031     C* SE ERRORE LO VISUALIZZO
030000130502     C     PArMSG        IFNE      *BLANKS
030100951031     C                   CLEAR                   VIDMSG
030200130502     C                   MOVEL     PArMSG        VIDMSG
030300951031     C                   SETON                                        409028
030400951031     C                   GOTO      FOR01
030500951031     C                   ENDIF
030600931130     C* RICONTROLLO FOGLIO VIAGGIO
030700931130     C                   EXSR      CONTR1
030800931130     C   90              GOTO      FOR01
030900931130     C*
031000931130     C* RICARICO LE LINEE DI ARRIVO
031100900524     C                   EXSR      CARLNA
031200900521     C                   GOTO      FORCT2
031300931130    1C                   END
031400931130     C*
031500900615     C** CONTROLLO SELEZIONE SCELTE FATTA
031600931130     C                   EXSR      CONTRC
031700931130     C  NKF
031800931130     COR 90              GOTO      FORCT
031900931130     C**
032000931130     C* CMD6 - TUTTO OK LANCIO SINGOLI BORDERO'
032100931130    1C     *INKF         IFEQ      *ON
032200940913     C                   Z-ADD     1             NR3
032300931130     C*
032400940913    2C     NR3           DOWLE     B
032500940913     C     NR3           CHAIN     LS36S03                            30
032600900615     C*
032700940913     C   30              Z-ADD     150           NR3
032800900615     C   30              GOTO      SOTTO
032900931130     C*
033000940315     C* SE SCELTE PARZIALI E NON C'E '1' NON LANCIO
033100940315    3C   10VI5SCE        IFNE      '1'
033200900615     C                   GOTO      SOTTO
033300931130    3C                   END
033400900629     C* STAMPA BORDERO'
033500910918     C* VEDO SE LANCIARE 1 O 2 COPIE
033600931130    3C     VI5FAG        IFEQ      'A'
033700910918     C                   MOVEL     ' 2'          KNMCP
033800910918     C                   ELSE
033900910918     C                   MOVEL     ' 1'          KNMCP
034000931130    3C                   END
034100020813     C                   z-add     VIDpog        PA1pog
034200940315     C                   MOVEL     VIDNFV        NFV
034300900615     C                   Z-ADD     VI5LNA        VIDFFV
034400900615     C                   Z-ADD     VI5FLP        VIDFLP
034500940914     C                   MOVEL     PARAM1        KPJBU
034600940920     C                   MOVEL     'LS37'        KCOAZ
034700120203     C                   CALL      'BCH10'
034800900521     C                   PARM                    KPJBA
034900911104     C                   SETON                                        04
035000900828     C*
035100900615     C     SOTTO         TAG
035200940913     C                   ADD       1             NR3
035300931130    2C                   ENDDO
035400900521     C*
035500940914     C   45              GOTO      FORCT
035600911104     C* STAMPO FOGLIO VIAGGIO
035700911104     C                   EXSR      STAFV
035800940921     C*
035900940921     C* DOPO LA CONFERMA ESCO DAL PROGRAMMA
036000931130     C                   ENDIF
036100931130     C*
036200000000     C     FINE          TAG
036300000000     C                   SETON                                        LR
036400940913     C**************************************************************************
036500940913     C* CARICAMENTO INIZIALI
036600940913     C**************************************************************************
036700931130     C     CARTAB        BEGSR
036800931130     C*
036900940914     C* GIRO LA DATA DEL JOB
037000940914     C                   TIME                    WTIME            14 0
037100940914     C                   MOVE      WTIME         WDATE             8 0
037200940914     C                   MOVE      WDATE         G02DAT
037300940914     C                   MOVEL     *ZERO         G02INV
037400931130     C                   MOVEL     *BLANK        G02ERR
037500940913     C                   CALL      'XSRDA8'
037600931130     C                   PARM                    WLBDAT
037700940914     C                   Z-ADD     G02INV        DATEU             8 0
037800931130     C*
037900931130     C                   CLEAR                   VI2LNP
038000130612     c                   movel     simfel        alfafgs           3
038100130612      /free
038200130612
038300130612        // Carico filiali di test abilitate
038400130612         clear TIBS02ds;
038500130612         T02Mod = 'C';
038600130612         T02sif = knsif;
038700130612         T02cod = 'VPO';
038800130613         T02ke1 ='DECOFI1'   ;
038900130612         TNTBE_RicercaControllo  (kpjba : tibs02ds);
039000130612         if  T02err  = *blank;
039100130612           dvpodecofi = T02uni;
039200130612         endif;
039300130612       if  §vpofgvf1= '999'  or
039400130612           alfafgs= §vpofgvf1 or  alfafgs= §vpofgvf2 or
039500130612           alfafgs= §vpofgvf3 or  alfafgs= §vpofgvf4 or
039600130612           alfafgs= §vpofgvf5 ;
039700130612          *in90=*on  ;
039800130612          *in28=*on  ;
039900130612          *in99=*on  ;
040000130612          vidmsg=msg(9)  ;
040100130612           leavesr;
040200130612         endif         ;
040300130612      /end-free
040400940913     C*
040500931130     C                   ENDSR
040600940913     C**************************************************************************
040700940913     C* CONTROLLI SUL FOGLIO VIAGGIO
040800940913     C**************************************************************************
040900931130     C     CONTR1        BEGSR
041000940913     C*
041100940914     C                   SETOFF                                       90  40
041200940315     C*
041300940315     C     '?'           SCAN      VIDNFV                                 90
041400940315     C     *IN90         IFEQ      *ON
041500130520     C                   RESET                   fnlst3ds
041600940914     C* DATA FINALE: DATA DEL GIORNO
041700951108     C                   Z-ADD     WDATE         PA3DFI
041800020813     C                   move      VIDpog        PA3fgs
041900940315     C*
042000130520     C                   MOVEL     fnlst3ds      KPJBU
042100130520     C                   CALL      'FNLST3R'
042200940315     C                   PARM                    KPJBA
042300130520     C                   MOVEL     KPJBU         fnlst3ds
042400940315     C                   MOVEL     PA3NFV        VIDNFV
042500940315     C                   GOTO      ENDCT1
042600940315     C                   ENDIF
042700940315     C*
042800940315     C                   TESTN                   VIDNFV               18
042900940914     C  N18              MOVEL     MSG(1)        VIDMSG
043000940914     C  N18              SETON                                        409028
043100940315     C  N18              GOTO      ENDCT1
043200940315     C*
043300931130     C* CONTROLLI SE ESISTE NUMERO DI FOGLIO VIAGGIO
043400940315     C                   MOVEL     VIDNFV        COMNFV            5 0
043500020807     ***C     COMNFV        CHAIN     FNFGV01L                           40
043600020807     C     K02FGV01      chain     FNFGV01L                           40
043700931130     C  N40FGVATB        IFNE      *BLANKS
043800931130     C                   SETON                                        40
043900931130     C                   END
044000940914     C   40              MOVEL     MSG(1)        VIDMSG
044100940914     C   40              SETON                                        2890
044200931130     C   90              GOTO      ENDCT1
044300931130     C*
044400931130     C* FOGLIO VIAGGIO GIA' CHIUSO
044500931130     C     FGVFCF        IFNE      *BLANKS
044600940914     C                   MOVEL     MSG(2)        VIDMSG
044700940914     C                   SETON                                        409028
044800931130     C                   GOTO      ENDCT1
044900931130     C                   END
045000960201     C* NON SI POSSONO UTILIZZARE TIPI TRAINO INOLTRO PER BORDERIZZARE
045100960628     C*  NEMMENO IL TIPO TRAINO DI DEFLUENZA
045200960628     C     FGVTTR        IFNE      'I'
045300960628     C                   MOVEL     'TV'          COD
045400960628     C                   MOVEL(P)  FGVTTR        KEY
045500960628     C     KTAB          CHAIN     TABEL                              31
045600960628     C     *IN31         IFEQ      *OFF
045700960628     C                   MOVEL     TBLUNI        DSTV
045800960628     C                   ELSE
045900960628     C                   CLEAR                   DSTV
046000960628     C                   ENDIF
046100960628     C*
046200960628     C                   ENDIF
046300960628     C*
046400960201     C     FGVTTR        IFEQ      'I'
046500960628     C     §TVDEF        OREQ      'S'
046600960201     C                   MOVEL     MSG(6)        VIDMSG
046700960201     C                   SETON                                        409028
046800960201     C                   GOTO      ENDCT1
046900960201     C                   END
047000000612     C* NON SI POSSONO BORDERIZZARE FOGLI VIAGGIO POSTE
047100000612     C* AGGANCIO ESTENSIONE FOGLI
047200000612     C* ESTENSIONE FOGLI
047300020807     ***C     FGVNFV        CHAIN     FNFGW01L                           31
047400020807     C     K02FGW01      chain     FNFGW01L                           31
047500000612     C     *IN31         IFEQ      *ON
047600000612     C     FGWATB        ORNE      ' '
047700000612     C                   CLEAR                   FGWFF3
047800000612     C                   CLEAR                   FGWFF4
047900000612     C                   CLEAR                   FGWFL3
048000000612     C                   CLEAR                   FGWFL4
048100000612     C                   CLEAR                   FGWFGS
048200000612     C                   ENDIF
048300000612     C     FGWFGS        IFGT      *ZEROS
048400000612     C                   MOVEL     MSG(7)        VIDMSG
048500000612     C                   SETON                                        409028
048600000612     C                   GOTO      ENDCT1
048700000612     C                   ENDIF
048800931130     C*
048900931130     C     ENDCT1        ENDSR
049000940913     C**************************************************************************
049100940913     C*    CARICO IN SUBFILE LINEE DI ARRIVO
049200940913     C**************************************************************************
049300900518     C     CARLNA        BEGSR
049400940913     C*
049500900518     C                   EXSR      PULSF
049600940913     C                   Z-ADD     1             REC3
049700900530     C* DATA BORDERO'
049800940916     C* SE SONO PRIMA DELLE SEI DEL MATTINO IMPOSTO LA DATA DI IERI
049900051011     C**                 TIME                    WTIME
050000051011     C**                 MOVE      WTIME         WDATE
050100051011     C**                 MOVEL     WTIME         UHH               2 0
050200051011     C**   UHH           IFLE      5
050300051011     C**                 MOVE      WDATE         G02DAT
050400051011     C**                 MOVEL     *ZERO         G02INV
050500051011     C**                 MOVEL     *BLANK        G02ERR
050600051011     C**                 CALL      'XSRDA8'
050700051011     C**                 PARM                    WLBDAT
050800900530     C* SOTRAGGO 1 AI GIORNI
050900051011     C**   G02TGI        SUB       1             GIOTGI
051000051011     C**                 CALL      'XSRGI8'
051100051011     C**                 PARM                    WGIDAT
051200900530     C* DATA BORDERO' = DATA DEL GIORNO - 1
051300051011     C**                 Z-ADD     GIODAT        VIDDBR
051400051011     C**                 ELSE
051500900530     C* DATA BORDERO' = DATA DEL GIORNO
051600051011     C**                 MOVE      WDATE         VIDDBR
051700051011     C**                 END
051800940916     C*
051900051011     c* 11/10/05 --> come data borderò imposto data foglio non modificabile
052000051011     c*              per avere corrispondenza tra data borderò e data FGV
052100051011     C                   MOVE      fgvdfv        G02inv
052200051011     C                   MOVEL     '3'           G02ERR
052300051011     C                   CALL      'XSRDA8'
052400051011     C                   PARM                    WLBDAT
052500051011     c                   movel     g02dat        viddbr
052600051011     c
052700900521     C* LINEA PARTENZA
052800900518     C                   Z-ADD     FGVLNP        VIDLNP
052900940914     C     FGVLNP        CHAIN     AZORG01L                           31
053000900521     C   31              MOVEL     *BLANKS       DESLNP
053100900521     C  N31              MOVEL     ORGDES        DESLNP
053200900521     C* LA LINEA ARRIVO FINALE E' = ALLA PRIMA LINEA DEL SUBFILE
053300900521     C*    COME PRIMA LINEA PER IL BORDERO' ED E' = ALLA FIL.TRANSITO
053400900518     C                   Z-ADD     FGVLNA        VIDLNA
053500900521     C                   MOVEL     FGVLNA        VI5LNA
053600900521     C                   MOVEL     FGVLNA        VI5FLP
053700940914     C     FGVLNA        CHAIN     AZORG01L                           31
053800931130     C     *IN31         IFEQ      *ON
053900931130     C                   MOVEL     *BLANKS       DE2LNA
054000931130     C                   MOVEL     *BLANKS       DESLNA
054100931130     C                   MOVEL     *BLANKS       DESFLP
054200931130     C                   MOVEL     'F'           VI5FAG
054300931130     C                   ELSE
054400931130     C                   MOVEL     ORGDES        DE2LNA
054500931130     C                   MOVEL     ORGDES        DESLNA
054600931130     C                   MOVEL     ORGDES        DESFLP
054700931130     C                   MOVEL     ORGFAG        VI5FAG
054800931130     C                   ENDIF
054900900521     C*
055000900518     C                   MOVEL     *BLANKS       VI5SCE
055100940913     C                   Z-ADD     1             NR3               4 0
055200940913     C                   WRITE     LS36S03
055300900521     C*
055400900518     C* FIN CHE CI SONO FILIALI EMETTO NEL SUBFILE
055500940913     C     FFV(NR3)      DOWNE     *BLANKS
055600900626     C*
055700940913     C                   MOVEL     FFV(NR3)      VI5LNA
055800940914     C     VI5LNA        CHAIN     AZORG01L                           31
055900931130     C     *IN31         IFEQ      *ON
056000931130     C                   MOVEL     *BLANKS       DESLNA
056100931130     C                   MOVEL     'F'           VI5FAG
056200931130     C                   ELSE
056300931130     C                   MOVEL     ORGDES        DESLNA
056400931130     C                   MOVEL     ORGFAG        VI5FAG
056500931130     C                   ENDIF
056600900615     C* SE NON C'E' FIL TRANSITO METTO SE STESSA
056700940913     C     FLP(NR3)      IFEQ      *BLANKS
056800900615     C                   MOVEL     VI5LNA        VI5FLP
056900900615     C                   ELSE
057000940913     C                   MOVEL     FLP(NR3)      VI5FLP
057100900615     C                   END
057200940914     C     VI5FLP        CHAIN     AZORG01L                           31
057300900521     C   31              MOVEL     *BLANKS       DESFLP
057400900521     C  N31              MOVEL     ORGDES        DESFLP
057500900521     C                   MOVEL     *BLANKS       VI5SCE
057600940913     C                   ADD       1             NR3
057700940913     C                   WRITE     LS36S03
057800900518     C                   END
057900940913     C                   Z-ADD     NR3           B                 4 0
058000900509     C*
058100900706     C     ENDCAR        ENDSR
058200940913     C**************************************************************************
058300940913     C* PULIZIA SUBFILE
058400940913     C**************************************************************************
058500900518     C     PULSF         BEGSR
058600940913     C*
058700900518     C                   SETON                                        35
058800940913     C                   WRITE     LS36C03
058900900518     C                   SETOFF                                       35
059000940913     C*
059100900518     C                   ENDSR
059200940913     C**************************************************************************
059300940913     C* CONTROLLO SE EFFETTUATE O NO SCELTE
059400940913     C**************************************************************************
059500931130     C     CONTRC        BEGSR
059600940913     C*
059700940914     C                   SETOFF                                       43
059800900530     C                   SETOFF                                       90
059900940914     C*
060000931130     C* CONTROLLO DATA BORDERO'
060100940914     C                   MOVE      VIDDBR        G02DAT
060200940914     C                   MOVEL     *ZERO         G02INV
060300931130     C                   MOVEL     *BLANK        G02ERR
060400940913     C                   CALL      'XSRDA8'
060500931130     C                   PARM                    WLBDAT
060600931130     C     G02ERR        IFEQ      '1'
060700940914     C                   MOVEL     MSG(3)        VIDMSG
060800940914     C                   SETON                                        432890
060900931130     C                   GOTO      ENDCTR
061000931130     C                   END
061100940914     C                   Z-ADD     G02DAT        VIDDBR
061200940914     C                   Z-ADD     G02INV        DBR
061300900530     C* DATA BORDERO' NO MAGGIORE DATA DEL GIORNO
061400900530     C     DBR           IFGT      DATEU
061500940914     C                   MOVEL     MSG(5)        VIDMSG
061600940914     C                   SETON                                        432890
061700900530     C                   GOTO      ENDCTR
061800900530     C                   END
061900931130     C*
062000940914     C* CONTROLLO SE IMMESSE SCLETE CON "1"
062100931130     C                   SETOFF                                       10
062200931130     C                   SETON                                        36
062300940913     C                   READC     LS36S03                                31
062400931130     C*
062500931130     C     *IN31         DOWEQ     *OFF
062600940315     C     VI5SCE        IFEQ      '1'
062700931130     C                   SETON                                        1031
062800931130     C                   ENDIF
062900940913     C                   UPDATE    LS36S03
063000931130     C*
063100940913     C  N31              READC     LS36S03                                31
063200931130     C                   ENDDO
063300931130     C                   SETOFF                                       36
063400900521     C*
063500900521     C     ENDCTR        ENDSR
063600940913     C**************************************************************************
063700940913     C* STAMPO FOGLIO VIAGGIO
063800940913     C**************************************************************************
063900911104     C     STAFV         BEGSR
064000951031     C* IN QUESTO CASO IL FLAG SIGNIFICA STAMPA( ) / RISTAMPA(R)
064100951031     C                   MOVEL     ' '           PA2FLG
064200951031     C                   CLEAR                   PA2MSG
064300951031     C                   CLEAR                   PA2F12
064400130516     C                   MOVEL     VIDNFV        PA2NFV
064500020813     C                   move      VIDpog        PA2FGS
064600951031     C                   MOVEL     PA1PRT        PA2PRT
064700951031     C                   MOVEL     PA1MDT        PA2MDT
064800951031     C                   MOVEL     PARAM2        KPJBU
064900940921     C                   CALL      'FNLS94C'
065000911104     C                   PARM                    KPJBA
065100940913     C*
065200911104     C                   ENDSR
065300940913     C**************************************************************************
065400000612** MESSAGGI DI ERRORE
065500940914Numero di foglio viaggio inesistente o annullato                               1
065600940914Foglio di viaggio gia' chiuso                                                  2
065700940914Immettere data formalmente valida                                              3
065800940914Manca numero bordero':  TELEFONARE IN SEDE !                                   4
065900940914La data bordero' non puo' essere superiore alla data del giorno                5
066000130612Non si possono utilizzare FV INOLTRO o DEFLUENZA per borderizzare bolle        6
066100130612Impossibile borderizzare foglio viaggio in partenza da p.o. poste              7
066200130612Impossibile borderizzare linea di partenza poste                               8
066300130612Utente di filiale non + abilitata all'uso. Utilizzare nuova chiusura f.viaggio 9

000100040601     H DECEDIT('0,') DATEDIT(*yMd.)
000200960327     H* FNLR35R *----------------------------------------------------*
000300921027     H*        - LISTA ANOMALIE / I.D.D.
000400000000     H*--------------------------------------------------------------*
000500960409     FTABEL00F  UF   E           K DISK
000600031007     FTNTBE01L  IF   E           K DISK
000700031031     FWFLAN01L  UF A E           K DISK    USROPN
000800911021     FAZORG01L  IF   E           K DISK
000900960510     FAZCLN01L  IF   E           K DISK
001000040615     FAZCAE03l  IF   E           K DISK
001100040616     FAZCAE01l  IF   E           K DISK
001200040616     F                                     RENAME(azcae000:azcae001)
001300990121     FFNDCD02L  IF   E           K DISK
001400990122     FFNDCT01L  IF   E           K DISK
001500960508     FFNFVV01L  IF   E           K DISK
001600960930     FFNFGV01L  IF   E           K DISK
001700000503     FFNFGW01L  IF   E           K DISK
001800960930     FFNFVA01L  IF   E           K DISK
001900070111     FFNBRV07L  IF   E           K DISK
002000140618     FFNBRV05L  IF   E           K DISK
002100140618     F                                     RENAME(FNBRV000:BRV005)
002200970305     FFNANT02L  UF A E           K DISK
002300990511     FFNANM00F  UF A E             DISK
002400160907     F*** FNANMSED  o  A E             DISK    usropn
002500160907     F***                                      RENAME(FNANM000:FNANM0SD)
002600990511     FFNANM01L  IF   E           K DISK
002700990511     F                                     RENAME(FNANM000:FNANM001)
002800990511     F                                     INFDS(FNANM1)
002900990511     FFNANM02L  IF   E           K DISK
003000990511     F                                     RENAME(FNANM000:FNANM002)
003100020905     F                                     prefix(AN2:3)
003200990511     F                                     INFDS(FNANM2)
003300990511     FFNANM04L  IF   E           K DISK
003400990511     F                                     RENAME(FNANM000:FNANM004)
003500990511     FFNANM10L  IF   E           K DISK
003600990511     F                                     RENAME(FNANM000:FNANM010)
003700990511     F                                     INFDS(FNAN10)
003800021021     FFNANM20L  IF   E           K DISK
003900021021     F                                     RENAME(FNANM000:FNANM020)
004000990511     FFNANM07L  IF   E           K DISK
004100990511     F                                     RENAME(FNANM000:FNANM007)
004200990511     F                                     INFDS(FNANM7)
004300990511     FFNANM08L  IF   E           K DISK
004400990511     F                                     RENAME(FNANM000:FNANM008)
004500990511     F                                     INFDS(FNANM8)
004600990511     FFNANM09L  IF   E           K DISK
004700990511     F                                     RENAME(FNANM000:FNANM009)
004800990511     F                                     INFDS(FNANM9)
004900990511     FFNANM03L  IF   E           K DISK
005000990511     F                                     RENAME(FNANM000:FNANM003)
005100990511     F                                     INFDS(FNANM3)
005200001030     FFNANM11L  UF   E           K DISK
005300001030     F                                     RENAME(FNANM000:FNANM011)
005400160907     F*** FNANM00T  O  A E           K DISK    USROPN
005500160907     F***                                      RENAME(FNANM000:FNANM0TT)
005600950321     FFNART27L  IF   E           K DISK
005700950320     FFNARB01L  IF   E           K DISK
005800960403     FFNBLT27L  IF   E           K DISK
005900960412     FFNBLP01L  IF   E           K DISK
006000021203     FFIAPD01L  IF   E           K DISK
006100960403     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
006200960403     F                                     USROPN
006300950321     F                                     FORMLEN(66)
006400950321     F                                     FORMOFL(63)
006500960403     FPRTF198X  O    F  198        PRINTER OFLIND(*INOE)
006600960403     F                                     USROPN
006700950321     F                                     FORMLEN(66)
006800950321     F                                     FORMOFL(63)
006900020905     D*
007000020905     D* DEFINIZIONE SCHIERE A TEMPO DI COMPILAZIONE
007100020905     D*
007200040615     D STA             S             66    DIM(35) CTDATA PERRCD(1)             SKIERE STAMPA
007300960506     D DAN             S             17    DIM(8) CTDATA PERRCD(1)              DESCR ANMOMAL
007400160907     D** CMD1            S             49    DIM(1) CTDATA PERRCD(1)
007500160907     D** OVRSED          S             49    DIM(1) CTDATA PERRCD(1)
007600921016     D EL              S              1    DIM(4)                               ANOM DA ELAB
007700020905     D*
007800020905     D* DEFINIZIONE SCHIERE A TEMPO DI ESECUZIONE
007900960326     D*
008000960927     D L6              S              3  0 DIM(30)                              P.O.GEST
008100170519     D L6_a            S              3  0 DIM(30)                              P.O.GEST
008200040614     D L1              S              3  0 DIM(30)                              P.O.AREA
008300170519     D*** L1RIF           S              3  0 DIM(30)                              P.O.AREA
008400960326     D*
008500961017     D FLE             S              3  0 DIM(50)                              ANOM.USO FLE
008600970829     D FLO             S              3  0 DIM(50)                              ANOM.ARR FLE SOLO
008700970829     D FLT             S              3  0 DIM(50)                              ANOM.ARR FLE E LNA
008800970829     D FLM             S              3  0 DIM(50)                              ANOM.PAR FLE SOLO
008900970829     D FLR             S              3  0 DIM(50)                              ANOM.PAR FLE E LNP
009000960422     D*
009100960927     D FID             S              3  0 DIM(7)                               P.O. NO IDD
009200050831     D DFI             S             22    DIM(7)                               DECODIFC FIL
009300960927     D FM1             S              3  0 DIM(100)                             P.O. IDD-1
009400960510     D*
009500960506     D K51             S             17    DIM(3)                               COMODO
009600040615     D STpo            S              3    DIM(30)
009700160623     d
009800160623     d sps             s              2  0 dim(101)
009900020905     D*
010000020905     D* DEFINIZIONE STRUTTURE DATI (INTERNE)
010100960327     D*
010200990511     D FNANM1          DS
010300960405     D  ANMNR1               397    400B 0
010400990511     D FNANM2          DS
010500960705     D  ANMNR2               397    400B 0
010600990511     D FNANM3          DS
010700970529     D  ANMNR3               397    400B 0
010800990511     D FNAN10          DS
010900960417     D  ANMN10               397    400B 0
011000990511     D FNANM7          DS
011100960327     D  ANMNR7               397    400B 0
011200990511     D FNANM8          DS
011300960327     D  ANMNR8               397    400B 0
011400990511     D FNANM9          DS
011500960327     D  ANMNR9               397    400B 0
011600960327     D**
011700911015     D WLBDAT          DS
011800971126     D  G02DAT                 1      8  0
011900971126     D  G02INV                 9     16  0
012000971126     D  G02ERR                17     17
012100971126     D  G02TGI                18     22  0
012200960510     D WGIDAT          DS
012300990512     D  GI8DAT                 1      8  0
012400990512     D  GI8INV                 9     16  0
012500990512     D  WAA                    9     12  0
012600990512     D  WMM                   13     14  0
012700990512     D  WGG                   15     16  0
012800990512     D  GI8TGI                17     21  0
012900960927     D                 DS
013000000503     D  FFV                    1    897
013100000503     D                                     DIM(299)                             LINEE FOGLIO
013200960927    +D  FGVFFV                 1    240
013300960927    +D  FGVFF2               241    447
013400000503    +D  FGWFF3               448    687
013500000503    +D  FGWFF4               688    897
013600960927     D*
013700960927     D                 DS
013800000503    +D  FLP                    1    897
013900000503     D                                     DIM(299)                             LINEE TRANSITO
014000960927    +D  FGVFLP                 1    240
014100960927    +D  FGVFL2               241    447
014200000503    +D  FGWFL3               448    687
014300000503    +D  FGWFL4               688    897
014400960510     D                 DS
014500960510     D  CLNPOM                 1     31
014600960510     D  POM                    1     31
014700960510     D                                     DIM(31)                              FESTIV.MATTINO
014800990121     D                 DS
014900031007     D  DCTAAC                 1      4  0
015000031007     D  DCTMGC                 5      8  0
015100031007     D  DCTDCA                 1      8  0 INZ
015200021008     D*
015300160907     D** DSmembro        ds             7    inz
015400160907     D**  MbrM                   1      1    inz('M')
015500160907     D**  MbrPOP                 2      4  0 inz
015600160907     D**  MbrPOA                 5      7  0 inz
015700020905     D*
015800020905     D* DEFINIZIONE STRUTTURE DATI (ESTERNE)
015900020905     D*
016000900622     D KPJBA         E DS
016100001102     D  PARSCH               502    502
016200020905     D UT§DSE0F      E DS
016300020905     D CNCR80        E DS
016400960422     D OG148         E DS
016500991217     D OG143         E DS
016600960510     D DS1Y          E DS
016700990902     D DS7A          E DS
016800990902     D DS7AF         E DS
016900960403     D DS7A2         E DS
017000161003     D DS7B          E DS
017100161003     D DS7C          E DS
017200160623     D DS7J          E DS
017300020905     D DCCH          E DS
017400911021     D DSCMZ         E DS
017500990511     D DSANM         E DS                  EXTNAME(FNANM00F)
017600031007     D DLAN          E DS
017700040616     D DANMFLO       E DS
017800960326     D*
017900960326     D* PASSAGGIO PARAMETRI - DAL PGM FNLR34R FNLR49R
018000960326     D DSLR34        E DS                  EXTNAME(FNLR34DS)
018100990513     D  ANM                   89    118
018200921015     D                                     DIM(10)                              ANOMALIE VIDEO
018300040617     D  spo                  140    229
018400040614     D                                     DIM(30)                              ANOMALIE VIDEO
018500960326     D*
018600960326     D* ds per pgm trul06r - caricamento £x
018700960326     D DSUL06        E DS                  EXTNAME(TRUL06DS)
018800960326     D  LIN                    1     90  0
018900960927     D                                     DIM(30)                              COMODO
019000040614      * Parametri per richiamo a TRUL09R - carico p.o. 2 livello in arr
019100040614     d Trul09ds      e ds
019200040614     d  po2liv                19    108  0 dim(30)
019300020905     D* PASSAGGIO PARAMETRI AL PGM DI GESTIONE ANOMALIE
019400020905     D DSLR44        E DS                  EXTNAME(FNLR44DS)
019500020905     D* PASSAGGIO PARAMETRI AL PGM DI ALLOCAZIONE FILE
019600021009     D FNLV60DS      E DS
019700020905     D* CALL A PGM CHE CARICA I DATI FOGLI  - FNLV53R
019800020905     D DSLV53        E DS                  EXTNAME(FNLV53DS)
019900020905     D DSLV55        E DS                  EXTNAME(FNLV55DS)
020000020905     D DSLR33        E DS                  EXTNAME(FNLR33DS)
020100160907     D** TIBS55DS      E DS
020200020905     D DSBS02        E DS                  EXTNAME(TIBS02DS)
020300040811      * ds per il controllo della presenza di CA per la spedizione richiesta
020400040811     d FIDN12DS      e ds
020500040811     d  skKey                 26    305    dim(20)
020600040811     d  skAnn                306    325    dim(20)
020700040811     d  skDca                326    485  0 dim(20)
020800040811     d  skFca                486    545  0 dim(20)
020900040811     d  skDch                546    705  0 dim(20)
021000040811     d  skCch                706    745    dim(20)
021100040811
021200040811     d dsKey           ds
021300040811     d  dsaac                         4  0
021400040811     d  dsfil                         3  0
021500040811     d  dsnca                         7  0
021600040811      *
021700031007     D****
021800031007     D* DS RIDEFINIZIONE CHIAVE X TABELLA "LAN"
021900031007     D****
022000031007     D                 DS
022100040615     D  WD34POS                            lIKE(D34POS)
022200031007     D  WD34NRS                            LIKE(D34NRS)
022300031007     D  DSKEYLAN               1      5
022400020905     D*
022500020905     D* DEFINIZIONE CAMPI
022600020905     D*
022700040505     D WLNP            s                   LIKE(anmlnp) inz
022800170519     D FGSTFP          S                   LIKE(FVVFGS) inz
022900170519     D FGSTFA          S                   LIKE(FVVFGS) inz
023000020905     D WLNA            S                   LIKE(FVVFGS) inz
023100160907     D*** WERR            S                   LIKE(D60ERR) inz
023200160907     D** WMB1            S                   LIKE(D60MB1) inz
023300160907     D** WMB2            S                   LIKE(D60MB2) inz
023400160907     D** SAVFL1          S                   LIKE(ANMFL1) inz
023500160907     D** SAVFL2          S                   LIKE(ANMFL2) inz
023600020905     D KTFP            S                   LIKE(CLNTFP) inz
023700020905     D KANN            S                   LIKE(CLNANN) inz
023800020905     D KMES            S                   LIKE(CLNMES) inz
023900020905     D FELDID          S                   LIKE(§7ADID) inz
024000020905     D KDCH            S                   LIKE(ANMDCH) inz
024100020905     D KFIL            S                   LIKE(ANMLNP) inz
024200020905     D KLNP            S                   LIKE(ANMLNP) inz
024300020905     D KLNP9           S                   LIKE(ANMLNP) inz
024400020905     D KLNP92          S                   LIKE(ANMLNP) inz
024500020905     D KFLS            S                   LIKE(ANMLNP) inz
024600020905     D KLNA            S                   LIKE(ANMLNA) inz
024700020905     D SAVLNA          S                   LIKE(ANMLNA) inz
024800020905     D KLNA7           S                   LIKE(ANMLNA) inz
024900020905     D KLNA8           S                   LIKE(ANMLNA) inz
025000020905     D KAIE            S                   LIKE(ANMAIE) inz
025100020905     D KCAA            S                   LIKE(ANMCAA) inz
025200020905     D SAVCAA          S                   LIKE(ANMCAA) inz
025300020905     D WCAA            S                   LIKE(ANMCAA) inz
025400020905     D KAAS2           S                   LIKE(ANMAAS) inz
025500020905     D KLNP2           S                   LIKE(ANMLNP) inz
025600020905     D KNRS2           S                   LIKE(ANMNRS) inz
025700020905     D KNSP2           S                   LIKE(ANMNSP) inz
025800020905     D KDAO            S                   LIKE(ANMDAO) inz
025900020905     D W53DFV          S                   LIKE(D53DFV) inz
026000020905     D SAVNFV          S                   LIKE(ANMNFV) inz
026100020905     D CCHPRT          S                   LIKE(ANMCCH) inz
026200020905     D KCDU            S                   LIKE(ANMCDU) inz
026300020905     D WDDET           S                   LIKE(ARTDET) inz
026400020905     D WDFLP           S                   LIKE(ARTFLP) inz
026500020905     D KNPG            S                   LIKE(BRVNPG) inz
026600020905     D WNFV            S                   LIKE(FVVNFV) inz
026700020905     D WNPG            S                   LIKE(FVVNPG) inz
026800020905     D WSPG            S                   LIKE(FVVSPG) inz
026900020905     D WDFV            S                   LIKE(FVVDFV) inz
027000020905     D WFGS            S                   LIKE(FVVFGS) inz
027100020905     D WMAX            S                   LIKE(TBLKEY) inz
027200021203     D KPDPDR          S                   LIKE(APDpdr)
027300021203     D KPDTIP          S                   LIKE(APDtip)
027400040401     D SAV_dslr34      S                   LIKE(dslr34)
027500040614     D savl1dao        S                   LIKE(anmdao) inz
027600040614     D sav2ldao        S                   LIKE(anmdao) inz
027700040615     D kcaeepa         S                   LIKE(caeepa) inz
027800040615     D kcaetfp         S                   LIKE(caetfp) inz
027900040616     D kcaetfe         S                   LIKE(caetfe) inz
028000040615     D kcaedde         S                   LIKE(caedde) inz
028100040616     D kcaetfa         S                   LIKE(caetfa) inz
028200040401     d wgir200         s              1
028300040616     d wsetgt          s              1
028400160623     d ix              s              3  0
028500170519     d la              s              4  0
028600040401     d wdta            s               d   datfmt(*Iso)
028700040405     d wdta200         s                   LIKE(D34ALA)
028800041020     D KFOA            S                   LIKE(ANTFOA)
028900041020     D KANLNP          S                   LIKE(ANTLNP) inz(000)
029000960508     D*
029100960508     D* DEFINIZIONE COSTANTI
029200960508     D*
029300960508     D CDISG           C                   CONST(' DISGUIDO A XXX IL')
029400040614     D VVALORE         C                   CONST('V-Valore')
029500960807     D CTRANS          C                   CONST(' TRANSITO A XXX IL')
029600960704     D CDOVE           C                   CONST(' IN XXXXX A XXX IL')
029700040616     D CCOAN           C                   CONST('C.A.XXX/XXXXXXX D-
029800040616     D                                     t')
029900040616     D CCOAN1          C                   CONST('IN C.A.XXX/XXXXXXX D-
030000040616     D                                     EL')
030100040615     D CPO             C                   CONST('PUNTO  OPERATIVO :')
030200040615     D CTERpar         C                   CONST('TERMINAL PARTENZA:')
030300040615     D CTERarr         C                   CONST('TERMINAL  ARRIVO :')
030400050503     D CCPcort         C                   CONST('CP-NO C.Parziale')
030500050503     D CCPlung         C                   CONST('CP-NO Consegna Parzi-
030600050503     D                                     ale')
030700050831     D CGestito        C                   CONST(' Gestito da ')
030800911018     C*--------------------------------------------------------------- *
030900911018     C*                  INDICATORI                                    *
031000911018     C*--------------------------------------------------------------- *
031100911021     C* 01    - ELABORAZIONE DEFINITIVA IDD
031200911021     C* 02    - ANOMALIA SEGNACOLLO
031300911021     C* 03    - ANOMALIA BOLLA
031400040616     C* 04
031500960927     C* 05    - RICHIESTE IN PARTENZA LE ANOMALIE IDD3 CHIUSE
031600000317     C* 06    - CONDIZIONA STAMPA DELLA LINEA DI ARRIVO
031700170519     C*
031800960510     C* 08    - ELENCO MANCATA TRASMISSIONE ANOMALIE IDD
031900990121     C* 09    - BOLLA IN CONSEGNA
032000960422     C* 10    - ON - NON STAMPA L'ANOMALIA
032100960419     C* 11    - ON - NON STAMPA MA AUMENTA LID E TRASMETTE
032200960419     C* 12    - ON - NON STAMPA NON AUMENTA LID MA TRASMETTE
032300000317     C* 13    - RICHIESTA IDD A STAMPA
032400960403     C* 14    - RICHIESTO INCLUSIONE/ESCLUSIONE DI UNA ANOMALIA
032500960403     C* 15    - RICHIESTO NUMERO FOGLIO
032600960403     C* 16    - RICHIESTA FASE APERTURA
032700921016     C* 17    - PASSATO IL TIPO ANOMALIA
032800960426     C* 18    - STAMPA DEL LIVELLO IDD
032900960426     C* 19    - RICHIESTA CAUSALE CHIUSURA
033000960508     C* 20    - INDICA SE COLLO PARTITO NON ARRIVATO E' IN DISGUIDO
033100960704     C* 21    - COLLO IN INVENT. O IN CONSEGNA E MANCA RECORD BOLLA ARR
033200040614     C* 23    - selezione p.o. segnacollo
033300990114     C* 24/29 - INDICANO QUALE FILE STO LEGGENDO
033400960327     C* 30/31 - LETTURA
033500960327     C* 32    - COMODO
033600990122     C* 40/42 - STAMPO NUMERO C.A. E DATE C.A. SE C'E' SU COLLO
033700050503     C* 43    - stampa particolarità consegne: CP se presente
033800051221     C* 44    - stampa dati mittente e destinatario se richiesto
033900151125     C* 45    - stampa se bolla chiusa con consegna anomala 7
034000040615     C* 70    - COLLI DI VALORE MANCANTI
034100040615     C* 80    - campo d34spo pieno, passati p.o. da stampare
034200040615     C* 97    - ANOMALIA IDD
034300960404     C* 98/99 - PER I FILE DI TRASMISSIONE
034400900828     C*-----------------------------------------------------
034500911015     C                   EXSR      IMPO
034600960416     C*
034700001030     C                   SELECT
034800001030     C* SE SOLO INVIO RICHIAMO ROUTINE DI LETTURA
034900001030     C* DELLE ANOMALIE DA TRASMETTERE CON AZIONE SCHEDULATA
035000960416     C* ELABORO SOLO SE LA DATA ULTIMO IDD E' MINORE DELLA DATA ELABORA
035100001030     C     D34GES        WHENEQ    'V'
035200001030     C                   EXSR      SCHANM
035300001030     C*
035400001030    1C     *IN01         WHENEQ    *OFF
035500990512     C     §7ADID        ORLT      D34ALA
035600000317     C*
035700960416     C*
035800960405     C                   MOVEL     'S'           D34RIL
035900960404     C     RILEG         TAG
036000960404     C                   CLEAR                   D34END
036100960327     C**
036200960327     C* RILEGGO SE MI E' STATO CHIESTO DI RICARICARE
036300960327     C**
036400000317    3C     D34RIL        DOWEQ     'S'
036500960327     C                   CLEAR                   D34RIL
036600960402     C                   CLEAR                   WUSCIT
036700960327     C**
036800960326     C* LEGGO PRIMA LE ANOMALIE PARTENZA
036900960326     C**
037000000317    4C     D34PAR        IFEQ      'P'
037100960412     C     D34PAR        OREQ      ' '
037200980327     C                   MOVEL     'P'           WANOM             1
037300970829     C* SONO LE ANOMALIE CHE USANO SOLO FLE PER ESCUDERLE DAL GIRO
037400970829     C*  DELLA LETTURA PER LNP
037500970829     C                   MOVE      FLM           FLE
037600960412     C                   EXSR      PARTEN
037700970529     C*
037800970529     C* POI ELABORO LE ANOMALIE RICEVUTE PER LE QUALI SONO FILIALE DI
037900970529     C* TRANSITO O DISGUIDO (SOLO SE SONO SIMFEL)
038000970529     C* Potenzialmente questa elabor. sarebbe da fare anche per le anom
038100970529     C* arrivo ma per ora non serve perchè in arrivo riparte sempre
038200970529     C* tutto in automatico
038300980401     C* Solo se le anomalie richieste sono anche idd
038400000317    5C     D34IE         IFEQ      ' '
038500980401     C     D34IE         OREQ      'D'
038600000317     C     D34FGS        IFEQ      SIMFEL
038700970529     C                   EXSR      USACDU
038800000317     C                   END
038900000317    5C                   END
039000970829     C**
039100970829     C* POI  LEGGO  LE ANOMALIE UTILIZZANDO ANMFLE
039200970829     C*  CONTENUTE NELLA SKIERA FLR
039300970829     C**
039400970829     C                   MOVE      FLR           FLE
039500990114     C                   SETON                                        24
039600990114     C                   SETOFF                                       25
039700170519     c                   eval      wfle='P'
039800970829     C                   EXSR      USAFLE
039900170519     c                   eval      wfle='N'
040000000317    4C                   ENDIF
040100960327     C**
040200960327     C* POI  LEGGO  LE ANOMALIE ARRIVO
040300960327     C**
040400000317    4C     D34PAR        IFEQ      'A'
040500960412     C     D34PAR        OREQ      ' '
040600980327     C                   MOVEL     'A'           WANOM             1
040700970829     C* SONO LE ANOMALIE CHE USANO SOLO FLE PER ESCUDERLE DAL GIRO
040800970829     C*  DELLA LETTURA PER LNA
040900970829     C                   MOVE      FLO           FLE
041000960412     C                   EXSR      ARRIVO
041100960412     C**
041200960412     C* POI  LEGGO  LE ANOMALIE UTILIZZANDO ANMFLE
041300970829     C*  CONTENUTE NELLA SKIERA FLT
041400960412     C**
041500961017     C                   MOVE      FLT           FLE
041600990114     C                   SETOFF                                       24
041700990114     C                   SETON                                        25
041800170519     c                   eval      wfle='A'
041900960412     C                   EXSR      USAFLE
042000170519     c                   eval      wfle='N'
042100000317    4C                   ENDIF
042200000317    3C                   ENDDO
042300960327     C*
042400960327     C* FINE LETTURA : IN GESTIONE RICHIAMO LO STESSO IL PGM
042500000317    3C     D34F03        IFEQ      ' '
042600960327     C     D34F12        ANDEQ     ' '
042700000317    4C     D34GES        IFEQ      'I'
042800000317     C     D34GES        OREQ      'G'
042900960327     C                   CLEAR                   DSLR44
043000960327     C                   CLEAR                   D34F03
043100960327     C                   CLEAR                   D34F12
043200960327     C                   CLEAR                   D34RIL
043300960327     C                   MOVEL     'S'           D34END
043400960327     C                   MOVEL     DSLR34        D44LR
043500960521     C                   MOVEL     'G'           D44MOD
043600960327     C*
043700960327     C                   MOVEL     DSLR44        KPJBU
043800960327     C                   CALL      'FNLR44R'
043900960327     C                   PARM                    KPJBA
044000111116     C                   PARM                    dslr34
044100960402     C*
044200960402     C                   MOVEL     KPJBU         DSLR44
044300111116     C***                MOVEL     D44LR         DSLR34
044400960404     C     D34RIL        IFEQ      'S'
044500960412     C                   GOTO      RILEG                                        RILETTURA
044600960404     C                   ENDIF
044700960404     C*
044800000317    4C                   ENDIF
044900000317    3C                   ENDIF
045000040401     C**
045100040401     C* Stampa elenco anomalie 200 aperte fino a data apertura = data idd-1
045200040401     C**
045300040401     c     d34ela        ifne      *blanks
045400040401     c                   clear                   fle
045500040401     C                   MOVE      200           FLE(1)
045600040401     C                   SETOFF                                       24
045700040401     C                   SETON                                        25
045800040401     c* mi salvo la dslr34 prima di sporcarla
045900040401     c                   eval      SAV_dslr34 = dslr34
046000040401     c                   eval      wgir200='S'
046100040401     c                   eval      d34ges='S'
046200040401     c                   eval      d34ie ='I'
046300040401     c                   eval      d34anm = '200'
046400040401     c                   eval      d34inc = 'I'
046500040401     c                   eval      d34ela = *blanks
046600040405     c                   eval      wdta200=d34ala
046700070208     c* Cerco il primo giorno lavorativo
046800070208     C                   Z-ADD     D34ala        WDATA8
046900070208     C                   EXSR      RICDAT
047000070208     C                   Z-ADD     WDATAM        d34ala
047100070208     c
047200070208     c**** *iso          Movel     d34ala        wdta
047300070208     c****               subdur    1:*D          wdta
047400070208     c****               Movel     wdta          D34ala
047500040401     C                   exsr      usafle
047600040401     c                   eval      dslr34 = sav_dslr34
047700040401     c                   eval      wgir200=' '
047800040401     c                   endif
047900960422     C**
048000960422     C* STAMPA SE NESSUNA ANOMALIA RISCONTRATA
048100960422     C**
048200000317    3C     D34GES        IFEQ      'S'
048300960422     C*
048400000317    4C     WSTAMP        IFEQ      ' '
048500040615     c                   if        *inOF
048600040615     C                   EXCEPT    TES
048700040615     C  n70              EXCEPT    TES2
048800040615     C                   EXCEPT    TES3
048900040615     C                   SETOFF                                       OF
049000040615     c                   endif
049100960422     C*
049200960422     C                   EXCEPT    NOANOM
049300000317    4C                   ENDIF
049400960422     C**
049500960422     C* STAMPA ELENCO DI CHI NON HA FATTO L'IDD
049600040615     c*  se non richiamato per altre liste
049700960422     C**
049800040615     c                   if        not *in70
049900040615    4C     D34PAR        IFEQ      'P'
050000960506     C     D34IE         ANDEQ     'D'
050100960423     C     D34ELA        ORNE      ' '
050200960424     C     SIMFEL        ANDEQ     D34FGS
050300960423     C                   EXSR      NOIDD
050400000317    4C                   ENDIF
050500000317    3C                   ENDIF
050600040615    3C                   ENDIF
050700960327     C**
050800960327     C* I N V I O   I D D
050900960327     C**
051000000317    3C     *IN01         IFEQ      *ON
051100000317    4C     W7AIDD        IFEQ      'S'
051200960404     C* CONTROLLO PER TRASMETTERE LE CHIUSURE DELLE ANOMALIE  70 E 71
051300960404     C                   EXSR      CHIUAN
051400160907     C* Creo L'ANOMALIA OK IDD
051500970306     C                   EXSR      CAEX
051600160907     c
051700000317    4C                   ENDIF
051800921027     C*
051900960409     C* AGGIORNO DATA ULTIMA ESECUZIONE IDD  CON DATA DEL GIORNO
052000960409     C                   MOVEL     '7A'          COD
052100960409     C                   MOVEL(P)  D34FGS        KEY
052200960409     C     KTAB          CHAIN     TABEL                              3130
052300000317    4C     *IN31         IFEQ      *OFF
052400960409     C     *IN30         ANDEQ     *OFF
052500960409     C                   MOVEL     TBLUNI        DS7AF
052600960409     C                   Z-ADD     D34ALA        §7ADID
052700960409     C                   MOVEL     DS7AF         TBLUNI
052800960409     C                   UPDATE    TABEL
052900000317    4C                   END
053000960409     C*
053100000317    3C                   ENDIF
053200001030     C                   ENDSL
053300000317     C**
053400000320     C**
053500960327     C                   CLEAR                   KPJBU
053600111116     C                   MOVEL     DSLR34        KPJBU
053700000320     C* ULTIMA DISALLOCAZIONE
053800160907     c**
053900160907     c**                 eval      D60po0 = SIMfel
054000160907     C**                 MOVEL     SAVFL1        D60FL1
054100160907     C**                 MOVEL     SAVFL2        D60FL2
054200160907     C**                 MOVEL     AM8GIO        D60DRF
054300160907     C**                 MOVEL     'D'           D60AZN
054400160907     C**                 MOVEL     'FNANM00T'    D60FIL
054500160907     C**                 MOVEL     FNLV60DS      KPJBU
054600160907     C**                 CALL      'FNLV60R'
054700160907     C**                 PARM                    KPJBA
054800160907     C**                 CLEAR                   SAVFL1
054900160907     C**                 CLEAR                   SAVFL2
055000970603     C*
055100970603     C* RICHIAMO PGM PER CHIUDERE IN LR
055200970603     C     WCL53         IFEQ      '1'
055300970603     C                   MOVEL     'C'           D53TLA
055400970603     C                   CALL      'FNLV53R'
055500970603     C                   PARM                    DSLV53
055600970603     C                   END
055700971118     C*
055800971118     C                   MOVEL     'C'           D55TLA
055900971118     C                   CALL      'FNLV55R'
056000971118     C                   PARM                    DSLV55
056100160907     C**                 CLEAR                   TIBS55DS
056200160907     C**                 MOVEL     'C'           I50TLA
056300160907     C**                 CALL      'TIBS55R'
056400160907     C**                 PARM                    TIBS55DS
056500990122     C                   CLEAR                   DSBS02
056600990122     C                   MOVEL     'C'           T02TLA
056700990122     C                   CALL      'TIBS02R'
056800990122     C                   PARM                    KPJBA
056900990122     C                   PARM                    DSBS02
057000040614     C*
057100040614     c                   clear                   trul09ds
057200040614     C                   MOVEL     'C'           i09TLA
057300040614     C                   CALL      'TRUL09R'
057400040614     C                   PARM                    trul09ds
057500031031     C*
057600031031     C* Se aperto wrk-file WFLAN lo chiudo
057700031031     C                   IF        %open(WFLAN01L)
057800031031     C                   CLOSE     WFLAN01L
057900031031     C                   ENDIF
058000960416     C*
058100000000     C                   SETON                                        LR
058200911018     C*
058300911018     C* IMPOSTO CAMPI FISSI -----------------------------------------*
058400911018     C     IMPO          BEGSR
058500921015     C                   Z-ADD     0             PAGE2             4 0
058600921029     C                   Z-ADD     0             PAGE1             4 0
058700921029     C                   TIME                    UTIME             6 0
058800160907     C***                Z-ADD     0             SAVFL1
058900960403     C*
059000960403     C* SE HO RICHIESTO LA STAMPA APRO I FILE DI STAMPA
059100960403     C     D34GES        IFEQ      'S'
059200000317     C**
059300960403     C                   OPEN      PRTF198
059400960403     C     D34ELA        IFNE      ' '
059500960403     C                   OPEN      PRTF198X
059600960403     C                   ENDIF
059700971120     C**
059800971120     C                   SETON                                        OFOE
059900960604     C**
060000960927     C* DECODIFICO IL P.O. IN GESTIONE
060100040614     c                   if        d34fgs>0
060200960604     C     D34FGS        CHAIN     AZORG01L                           30
060300960604     C  N30              MOVEL     ORGDES        PRTFGS           20
060400040615     C* IMPOSTO LA SCRITTA IN TESTATA
060500040615     C                   MOVEL     CPO           PRTPO            18
060600040614     c                   endif
060700040615     c
060800040615     C* SE VENGO DALLA STAMPA DEI COLLI DI VALORE IMPOSTO
060900040615     C*  INDICATORE 70 E LA SCRITTA IN TESTATA
061000040615    1C                   IF        D34STV='S'
061100040615     C                   SETON                                        70
061200040615     C* SE STAMPA PER TERMINAL, IMPOSTO TERMINAL IN TESTATA
061300040615    2C                   IF        D34SPO<>*BLANKS  and d34spo<>*zeros
061400040615    3C                   IF        D34PAR='A'
061500040615     C                   MOVEL     CTERARR       PRTPO
061600040615     C                   ELSE
061700040615     C                   MOVEL     CTERPAR       PRTPO
061800040615    3C                   ENDIF
061900040615    2C                   ENDIF
062000040615    1C                   ENDIF
062100980402     C**
062200980402     C* DECODIFICO LA LINEA DI ARRIVO SE RICHIESTA
062300980402     C     D34LNA        IFNE      *ZEROS
062400980403     C     D34TRA        ANDNE     'S'
062500980402     C                   SETON                                        06
062600980402     C     D34LNA        CHAIN     AZORG01L                           30
062700040614     C  N30              MOVEL     ORGDES        PRTLNA           16
062800980402     C                   END
062900000317     C                   ENDIF
063000040614     c*
063100040614     c* Selezione p.o. segnacollo
063200040614     c                   if        d34pos>0
063300040614     C                   SETON                                        23
063400040614     C                   ENDIF
063500960327     C*
063600960412     C* CARICO IN UNA SCHIERA TUTTE LE ANOMALIE IDD E LE ANOMALIE
063700960412     C*  DA LEGGERE CON ANMFLE
063800960326     C                   MOVEL     '7C'          COD
063900960412     C                   Z-ADD     0             Y                 3 0
064000961017     C                   Z-ADD     0             T                 3 0
064100970829     C                   Z-ADD     0             U                 3 0
064200970829     C                   Z-ADD     0             V                 3 0
064300960326     C     KTAB2         SETLL     TABEL
064400960409     C     KTAB2         READE(N)  TABEL                                  30
064500960416    1C     *IN30         DOWEQ     *OFF
064600960326     C*
064700960416    2C     TBLFLG        IFEQ      ' '
064800960326     C                   MOVEL     TBLUNI        DS7C
064900960506     C*
065000960412     C* ANOMALIE CHE CONSIDERANO ANMFLE
065100961017     C* §7CUEA='S' --> CONSIDERANO SOLO ANMFLE
065200961017     C* §7CUEA='E' --> CONSIDERANO SIA ANMFLE CHE ANMLNA
065300960416    3C     §7CUEA        IFEQ      'S'
065400960412     C                   ADD       1             Y
065500961017     C                   ADD       1             T
065600961017     C                   MOVEL     TBLKEY        FLO(Y)
065700961023     C                   MOVEL     TBLKEY        FLT(T)
065800961017   X3C                   ELSE
065900961017    4C     §7CUEA        IFEQ      'E'
066000961017     C                   ADD       1             T
066100961023     C                   MOVEL     TBLKEY        FLT(T)
066200961017    4C                   ENDIF
066300960416    3C                   ENDIF
066400970829     C** PARTENZA
066500970829    3C     §7CUEP        IFEQ      'S'
066600970829     C                   ADD       1             U
066700970829     C                   ADD       1             V
066800970829     C                   MOVEL     TBLKEY        FLM(U)
066900970829     C                   MOVEL     TBLKEY        FLR(V)
067000970829   X3C                   ELSE
067100970829    4C     §7CUEP        IFEQ      'E'
067200970829     C                   ADD       1             V
067300970829     C                   MOVEL     TBLKEY        FLR(V)
067400970829    4C                   ENDIF
067500970829    3C                   ENDIF
067600960416    2C                   ENDIF
067700960326     C*
067800960409     C     KTAB2         READE(N)  TABEL                                  30
067900000317    1C                   ENDDO
068000160623     c* Carico le pistola reali L o V
068100160623      /free
068200160623       ix=0;
068300160623       clear sps;
068400160623       setll (1:'7J') tabel00f;
068500160623       reade (1:'7J') tabel00f;
068600160623
068700160623  1    dow    not %eof(tabel00f)  ;
068800160623          ds7j=tbluni             ;
068900160623          if §7jtps = 'L' or §7jtps='C'  ;
069000160623           ix=ix+1;
069100160623           sps(ix)=%dec(%subst(tblkey:1:2):2:0);
069200160623          endif  ;
069300160623          reade (1:'7J') tabel00f  ;
069400160623  1    enddo     ;
069500160623      /end-free
069600160623     c
069700921015     C*
069800960327     C* GIRO DATA FOGLIO SE C'E'
069900960327     C                   MOVE      D34DFV        G02INV
070000960327     C                   MOVEL     '3'           G02ERR
070100971126     C                   CALL      'XSRDA8'
070200960327     C                   PARM                    WLBDAT
070300990513     C                   MOVEL     G02DAT        STADFV            8 0
070400960327     C*
070500960326     C* SE NON IMMESSA DAL AL  = ALLA DATA DAL
070600960326     C     D34ALA        IFEQ      0
070700960403     C                   MOVEL     D34DLA        D34ALA                         DT APERTURA
070800960326     C                   ENDIF
070900960403     C     D34ACA        IFEQ      0
071000960403     C                   MOVEL     D34DCA        D34ACA                         DT CHIUSURA
071100960403     C                   ENDIF
071200960403     C*
071300960403     C* 14 ON - RICHIESTA ALMENO UNA ANOMALIA
071400960403     C     D34ANM        IFNE      *BLANKS
071500960403     C                   SETON                                        14
071600960403     C     D34INC        IFEQ      'I'
071700960403     C                   MOVEL     STA(22)       STARIG           66
071800960403     C                   ELSE
071900960403     C                   MOVEL     STA(23)       STARIG           66
072000960403     C                   ENDIF
072100960403     C                   ENDIF
072200921015     C*
072300921029     C* 15 ON  - IMMESSO NUMERO FOGLIO
072400960326     C     D34NFV        IFGT      0
072500921015     C                   SETON                                        15
072600960403     C                   ENDIF
072700921029     C*
072800921029     C* 16 ON  - IMMESSA FASE APERTURA
072900960326     C     D34FAS        IFNE      ' '
073000921015     C                   SETON                                        16
073100960403     C                   MOVEL     '7D'          COD
073200960403     C                   MOVEL(P)  D34FAS        KEY
073300960409     C     KTAB          CHAIN(N)  TABEL                              32
073400980403     C  N32              MOVEL     TBLUNI        TESFSA           12
073500960403     C  N32              MOVEL     TBLUNI        DESFV             8
073600960403     C                   ENDIF
073700040611     C**                           ì
073800960426     C* 19 ON  - IMMESSA CAUSALE CHIUSURA
073900960426     C     D34CCH        IFNE      ' '
074000960426     C                   SETON                                        19
074100960426     C                   MOVEL     '7E'          COD
074200960426     C                   MOVEL(P)  D34CCH        KEY
074300960426     C     KTAB          CHAIN(N)  TABEL                              32
074400960426     C  N32              MOVEL     TBLUNI        TESCCH           25
074500960426     C                   ENDIF
074600960403     C*
074700960403     C* RICHIESTA DELLE ANOMALIE SOLO APERTE O SOLO CHIUSE
074800960403     C     D34APR        IFEQ      'S'
074900960506     C                   MOVEL     DAN(7)        K51(3)
075000960426     C                   ENDIF
075100960426     C     D34APR        IFEQ      'C'
075200960506     C                   MOVEL     DAN(8)        K51(3)
075300960403     C                   ENDIF
075400040611     c                   if        d34bva='V'
075500040611     C                   MOVEL     vvalore       PRTVAL            8
075600040611     C                   ENDIF
075700921029     C**
075800921029     C* 13 ON  - RICHIESTA I.D.D.
075900921029     C**
076000000317    1C     D34ELA        IFNE      ' '
076100921029     C                   SETON                                        13
076200960403     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
076300960403     C* L'IDD
076400960403     C                   MOVEL     '7A'          COD
076500960403     C                   MOVEL     '2       '    KEY
076600960409     C     KTAB          CHAIN(N)  TABEL                              30
076700960403     C  N30              MOVEL     TBLUNI        DS7A2
076800921029     C*
076900921029     C* 01 ON  - ELABORAZIONE DEFINITIVA I.D.D.
077000000317    2C     D34ELA        IFEQ      'S'
077100921029     C                   SETON                                        01
077200960419     C*
077300960419     C* SE NON SONO SIMFEL VEDO A CHE DATA HA LANCIATO IL 1 LIVELLO
077400000317    3C     D34FGS        IFNE      SIMFEL
077500960419     C                   MOVEL     '7A'          COD
077600960419     C                   MOVEL(P)  SIMFEL        KEY
077700960419     C     KTAB          CHAIN(N)  TABEL                              31
077800960419     C   31              CLEAR                   DS7AF
077900960419     C  N31              MOVEL     TBLUNI        DS7AF
078000960419     C                   MOVE      §7ADID        FELDID
078100000317    3C                   ENDIF
078200921029     C* CHAIN A VARIABILI ARRIVI PER VEDERE SE DEVO INVIARE IDD O NO
078300921029     C                   MOVEL     '7A'          COD
078400921029     C                   MOVEL     '1       '    KEY
078500960409     C     KTAB          CHAIN(N)  TABEL                              30
078600990902     C  N30              MOVEL     TBLUNI        DS7A
078700990902     C   30              CLEAR                   DS7A
078800960417     C*
078900960417     C                   MOVEL     §7AIDD        W7AIDD            1
079000000317     C**
079100000317     C* SE DATA DI ELABORAZIONE <= ALLA DATA IN TABELLA, ESCO SENZA
079200000317     C*  ESEGUIRE IL PGM
079300000317     C                   MOVEL     '7A'          COD
079400000317     C                   MOVEL(P)  D34FGS        KEY
079500000317     C     KTAB          CHAIN(N)  TABEL                              31
079600000317     C   31              CLEAR                   DS7AF
079700000317     C  N31              MOVEL     TBLUNI        DS7AF
079800000317    2C                   ENDIF
079900000317    1C                   ENDIF
080000000317     C**
080100000317     C* D34GES='V' SOLO INVIO, IMPOSTO "S" IN W7AIDD
080200000317     C     D34GES        IFEQ      'V'
080300000317     C                   MOVEL     'S'           W7AIDD
080400000317     C                   ENDIF
080500921029     C*
080600160907    1C**   D34GES        IFEQ      'V'
080700160907     C**   D34ELA        OREQ      'S'
080800921029     C* SE DEVO INVIARE: CHKOBJ E ADDPF PER TUTTI I MEMBRI
080900160907    2C**   W7AIDD        IFEQ      'S'
081000921029     C*
081100160907     C**                 MOVE      ')'           VAR               8
081200160907     C**                 Z-ADD     49            LUNG             15 5
081300921029     C* MEMBRO RICEZIONE
081400160907     c**                 reset                   DSmembro
081500160907     c**                 eval      MbrPOP = SIMfel
081600160907     c**                 eval      MbrPOA = D34fgs
081700160907     c**                 movel(p)  DSmembro      wMBR             10
081800160907    2C**                 END
081900160907    1C**                 END
082000960506     C**
082100960506     C*  ANOMALIE PARTENZA ARRIVO
082200960506     C**
082300960506     C     D34PAR        IFEQ      'P'
082400960506     C                   MOVEL     DAN(2)        K51(1)
082500960506     C                   ENDIF
082600960506     C     D34PAR        IFEQ      'A'
082700960506     C                   MOVEL     DAN(1)        K51(1)
082800960506     C                   ENDIF
082900921029     C**
083000960506     C* 17 OFF - RICHIESTO SPECIFICO TIPO DI ANOMALIE  INT/IDD/ NON IDD
083100921029     C**
083200960326    1C     D34IE         IFNE      ' '
083300921016     C                   SETON                                        17
083400921016     C* ANOMALIE INTERNE
083500960506     C     D34IE         IFEQ      'I'
083600960506     C                   MOVEL     DAN(5)        K51(2)
083700960327     C                   MOVEL     'I'           EL(1)
083800960403     C                   SETON                                        18
083900960506     C                   ELSE
084000960506     C* ANOMALIE IDD
084100960506    3C     D34IE         IFEQ      'D'
084200960506     C                   MOVEL     DAN(4)        K51(2)
084300960506   X3C                   ELSE
084400960506     C                   MOVEL     DAN(3)        K51(2)
084500960506    3C                   ENDIF
084600960506     C**
084700960506    3C     D34PAR        IFEQ      'P'
084800040116     C****               MOVEL     'R'           EL(1)
084900040116     C                   MOVEL     'E'           EL(1)
085000960506     C                   ELSE
085100960506     C                   MOVEL     'E'           EL(1)
085200960506    3C                   ENDIF
085300960506    2C                   ENDIF
085400960506     C*
085500960506   X1C                   ELSE
085600960506     C                   MOVEL     DAN(6)        K51(2)
085700960326    1C                   ENDIF
085800960506     C                   MOVEA     K51           DESIE            50
085900961018     C***
086000961018     C* CARICO £1
086100961018     C***
086200961104     C                   CLEAR                   DSUL06
086300961018     C                   MOVE      '£1'          D06COD
086400961018     C                   MOVEL     *BLANKS       D06TLA
086500961104     C                   MOVEL     SIMFEL        D06KEY
086600961018     C                   MOVEL     DSUL06        KPJBU
086700961018     C                   CALL      'TRUL06R'
086800961018     C                   PARM                    KPJBA
086900961018     C                   MOVEL     KPJBU         DSUL06
087000961018     C                   MOVEA     LIN           L1
087100960326     C*
087200960927     C* CALCOLO DATA -1 SE RICHIESTE IN PARTENZA LE ANOMALIE IDD
087300960927    1C     D34IE         IFEQ      'D'
087400990512     C     D34DCA        ANDGT     0
087500960927     C*
087600960927    2C     D34PAR        IFEQ      'P'
087700960927     C     D34PAR        OREQ      ' '
087800960927     C* SE IDD CHIUSE O CON CAUSALE PR
087900960927    3C     D34APR        IFEQ      'C'
088000960927     C     D34CCH        OREQ      'PR'
088100960927     C                   SETON                                        05
088200960927     C*
088300971126     C                   Z-ADD     D34DCA        WDATA8
088400960927     C                   EXSR      RICDAT
088500990512     C                   Z-ADD     WDATAM        W08DM1            8 0
088600960927     C*
088700971126    4C     D34ACA        IFGT      0
088800971126     C                   Z-ADD     D34ACA        WDATA8
088900960927     C                   EXSR      RICDAT
089000990512     C                   Z-ADD     WDATAM        W08AM1            8 0
089100960927    4C                   ENDIF
089200960927    3C                   ENDIF
089300960927    2C                   ENDIF
089400960927    1C                   ENDIF
089500960927     C*
089600960927     C* DETERMINO IN UNA SCHIERA I P.O. CHE FANNO L'IDD IL GIORNO DOPO
089700960927    1C     D34PAR        IFEQ      'P'
089800960927     C     D34PAR        OREQ      ' '
089900960927    2C     D34IE         IFNE      'I'
090000960927     C                   Z-ADD     1             L
090100960927     C     *LOVAL        SETLL     AZORG01L
090200960927     C                   READ      AZORG01L                               30
090300960927     C*
090400960927    3C     *IN30         DOWEQ     *OFF
090500960927     C* ESCLUDO LE ANNULLATE
090600960927    4C     ORGFVA        IFEQ      ' '
090700960927     C* SOLO FILIALE E AGENZIA
090800960927    5C     ORGFAG        IFEQ      'F'
090900960927     C     ORGFAG        OREQ      'A'
091000960927     C                   MOVEL     ORGDE8        OG148
091100960927     C**
091200960927    6C     §OGEID        IFNE      ' '
091300960927     C                   Z-ADD     ORGFIL        FM1(L)                         GIORNO PREC
091400960927     C                   ADD       1             L
091500960927    6C                   ENDIF
091600960927    5C                   ENDIF
091700960927    4C                   ENDIF
091800960927     C*
091900960927     C                   READ      AZORG01L                               30
092000960927    3C                   ENDDO
092100960927     C*
092200960927    2C                   ENDIF
092300960927    1C                   ENDIF
092400971126     C***
092500040614     C* CARICO LA L6 DEI P.O. IN GESTIONE  --> p.o. da elaborare solo se
092600040614     c*        schiera p.o. passata vuota
092700971126     C***
092800040614    0c                   if        d34spo=*blanks or d34spo=*zeros
092900971126     C                   CLEAR                   DSUL06
093000971126     C                   MOVE      '£6'          D06COD
093100971126     C                   MOVEL     'L'           D06TLA
093200971126     C                   MOVEL     D34FGS        D06KEY
093300971126     C                   MOVEL     DSUL06        KPJBU
093400971126     C                   CALL      'TRUL06R'
093500971126     C                   PARM                    KPJBA
093600971126     C                   MOVEL     KPJBU         DSUL06
093700971126     C                   MOVEA     LIN           L6
093800170519     C                   MOVEA     LIN           L6_a
093900971126     C*
094000170519     C* VEDO SE IL P.O. E' UN TERMINAL  E SE HA DEI P.O.
094100050906     C* ESTERI O D.P.D. o la 101 resi --> LI CARICO
094200170519     c* considero terminal parteza o arrivo a seconda delle anomalie
094300170519     c
094400971126     C                   CLEAR                   DSLV55
094500170519     C****                   MOVEL     'P'           D55TPT
094600971126     C                   MOVEL     D34FGS        D55LIN
094700980403     C                   MOVEL     AM8GIO        D55DRF
094800971126     C                   CALL      'FNLV55R'
094900971126     C                   PARM                    DSLV55
095000170519     c
095100971126     C     D55ERR        IFNE      ' '
095200971126     C                   MOVEL     D55LIN        D55TFP
095300170519     C                   MOVEL     D55LIN        D55TFa
095400971126     C                   ENDIF
095500170519
095600170519     C                   MOVEL     D55TFP        FGSTFP
095700170519     C                   MOVEL     D55TFa        FGStfa
095800971126     C*§
095900170519    1C***  D34FGS        IFEQ      D55TFP
096000170519    c
096100170519     c* Se la filiale gestione è term partenza o arrivo
096200170519     c                   if        d34fgs=fgstfp
096300170519     c                             or d34fgs=fgstfa
096400971126     C                   Z-ADD     1             L
096500971126     C                   CLEAR                   W0030
096600971126     C     W0030         LOOKUP    L6(L)                                  30
096700170519     c                   eval      la=l
096800971126     C*
096900971126     C     *LOVAL        SETLL     AZORG01L
097000971126     C                   READ      AZORG01L                               30
097100971126     C*
097200971126    2C     *IN30         DOWEQ     *OFF
097300991217    3C     ORGFAG        IFEQ      'F'
097400991217     C     ORGFAG        OREQ      'A'
097500971126     C* P.O. NON ANNULLATO
097600991217    4C     ORGFVA        IFEQ      ' '
097700991217     C                   MOVEL     ORGDE3        OG143
097800991217     C*
097900020221    5C     §OGNTW        IFEQ      'EEX'                                        ESTERA
098000020221     C     §OGNTW        OREQ      'EUP'
098100020221     C     §OGNTW        OREQ      'FED'
098200020221     C     §OGNTW        OREQ      'DPD'                                        DPD
098300050906     c* anche la 101 resi la tratto come estero: caricata col terminal
098400050906     C     orgfil        OREQ      101                                          DPD
098500971126     C*
098600170519     C* VEDO SE HA COME TERMINAL  LA FIL.GESTIONE
098700971126     C                   CLEAR                   DSLV55
098800170519     C****                   MOVEL     'P'           D55TPT
098900971126     C                   MOVEL     ORGFIL        D55LIN
099000980403     C                   MOVEL     AM8GIO        D55DRF
099100971126     C                   CALL      'FNLV55R'
099200971126     C                   PARM                    DSLV55
099300991217    6C     D55ERR        IFNE      ' '
099400971126     C                   MOVEL     D55LIN        D55TFP
099500170519     C                   MOVEL     D55LIN        D55TFa
099600991217    6C                   ENDIF
099700170519
099800170519     c                   if        d55tfp=d34fgs
099900971126     C                   Z-ADD     ORGFIL        L6(L)
100000971126     C                   ADD       1             L
100100991217    6C                   ENDIF
100200170519
100300170519     c                   if        d55tfa=d34fgs
100400170519     C                   Z-ADD     ORGFIL        L6_a(La)
100500170519     C                   ADD       1             La
100600170519    6C                   ENDIF
100700170519
100800991217    5C                   ENDIF
100900971126    4C                   ENDIF
101000971126    3C                   ENDIF
101100971126     C*
101200971126     C                   READ      AZORG01L                               30
101300971126    2C                   ENDDO
101400971126     C*
101500971126    1C                   ENDIF
101600040614   x0c                   else
101700040617     c                   z-add     1             xx
101800040617     c                   z-add     1             yy                3 0
101900040617     c                   do        30            xx
102000040617     c                   if        spo(xx)>*zeros
102100040617     c* imposto schiera di elaborazione
102200040617     c                   movel     spo(xx)       l6(yy)
102300040617     c* imposto schiera di stampa alfanumerica
102400040617     c                   movel     spo(xx)       stpo(yy)
102500040617     c                   add       1             yy
102600040615     c                   seton                                        80
102700040617     c                   endif
102800040615     c                   enddo
102900040617     c
103000040614     c                   endif
103100960927     C*
103200911018     C                   ENDSR
103300960412     C*
103400960412     C* LEGGO LE ANOMALIE IN PARTENZA --------------------------------*
103500960412     C     PARTEN        BEGSR
103600960412     C                   MOVEL     'N'           WFLE              1
103700960412     C*
103800960412     C* IMPOSTO LE ANOMALIE CHE DEVO LEGGERE
103900960412     C*
104000960412    2C     D34IE         IFEQ      ' '
104100960412     C                   CLEAR                   EL
104200040116     C**                 MOVEL     'R'           EL(1)
104300040116     C                   MOVEL     'E'           EL(1)
104400960412     C                   MOVEL     'I'           EL(2)
104500960412    2C                   ENDIF
104600960412     C**
104700960412     C                   Z-ADD     1             L                 3 0
104800960412    2C     L6(L)         DOWGT     0
104900960412     C     WUSCIT        ANDEQ     ' '
105000960412     C**
105100960412     C                   MOVEL     L6(L)         KFIL
105200960412     C**
105300960412     C                   Z-ADD     1             E                 3 0
105400960412    3C     EL(E)         DOWNE     ' '
105500960412     C     WUSCIT        ANDEQ     ' '
105600960412     C                   MOVEL     EL(E)         KAIE
105700970217     C                   Z-SUB     1             KLNA
105800960412     C                   SETOFF                                       30
105900960412     C**
106000960412    4C     *IN30         DOWEQ     *OFF
106100960412     C     WUSCIT        ANDEQ     ' '
106200990512     C     KANM7         SETGT     FNANM07L
106300990512     C     KANM71        READE     FNANM07L                               30
106400960412     C   30              MOVEL     999           KLNA7
106500960412     C  N30              MOVEL     ANMLNA        KLNA7
106600960412     C*
106700960412     C                   CLEAR                   KLNP
106800990512     C     KANM8         SETGT     FNANM08L
106900990512     C     KANM9         READE     FNANM08L                               30
107000960412     C   30              MOVEL     999           KLNA8
107100960412     C  N30              MOVEL     ANMLNA        KLNA8
107200960412     C*
107300960412    5C     KLNA7         IFNE      999
107400960412     C     KLNA8         ORNE      999
107500960412     C                   SETOFF                                       30
107600960412     C** TROVATA LA LNA
107700960412    6C     KLNA7         IFLT      KLNA8
107800960412     C                   MOVEL     KLNA7         KLNA
107900960412     C                   ELSE
108000960412     C                   MOVEL     KLNA8         KLNA
108100960412    6C                   ENDIF
108200960927     C*
108300960412     C**
108400040116     C**** WUSCIT        IFEQ      ' '
108500960927     C                   EXSR      LEGPAR
108600040116     C****               ENDIF
108700960412     C**
108800960927   X5C                   ELSE
108900960412     C                   SETON                                        30
109000990518     C                   Z-ADD     999           KLNA
109100990518     C*
109200960412    5C                   ENDIF
109300960412    4C                   ENDDO
109400960412     C*
109500960412     C                   ADD       1             E
109600960412    3C                   ENDDO
109700960412     C*
109800960412     C                   ADD       1             L
109900960412    2C                   ENDDO
110000960412     C                   ENDSR
110100960927     C*
110200960927     C* LETTURA ANOMALIE PER LA PARTENZA------------------------------*
110300960927     C     LEGPAR        BEGSR
110400960927    6C                   SELECT
110500960927     C* SOLO ANOMALIE CHIUSE
110600960927     C     D34APR        WHENEQ    'C'
110700960927     C                   Z-ADD     1             KDCH
110800990512     C     KANM72        SETLL     FNANM07L
110900990512     C     KANM7         READE     FNANM07L                               31
111000960927   X6C                   OTHER
111100990512     C     KANM7         SETLL     FNANM07L
111200990512     C     KANM7         READE     FNANM07L                               31
111300960927    6C                   ENDSL
111400960927     C**
111500960927     C                   SETON                                        27
111600960927     C                   SETOFF                                       252829
111700990114     C                   SETOFF                                       2624
111800960927    6C     *IN31         DOWEQ     *OFF
111900960927     C**
112000960927     C                   EXSR      ELASTA                                        STAMPA
112100960927     C** RILEGGO DA CAPO
112200960927    7C     WUSCIT        IFEQ      ' '
112300960927     C*
112400990512     C     KANM7         READE     FNANM07L                               31     TUTTE O CHI
112500960927    7C                   ENDIF
112600960927    6C                   ENDDO
112700960927     C*
112800960927     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
112900960927   6AC     WUSCIT        IFEQ      ' '
113000960927     C                   CLEAR                   KLNP
113100960927    6C                   SELECT
113200960927     C* SOLO ANOMALIE CHIUSE
113300960927     C     D34APR        WHENEQ    'C'
113400960927     C                   Z-ADD     1             KDCH
113500990512     C     KANM82        SETLL     FNANM08L
113600990512     C     KANM8         READE     FNANM08L                               31
113700960927   X6C                   OTHER
113800990512     C     KANM8         SETLL     FNANM08L
113900990512     C     KANM8         READE     FNANM08L                               31
114000960927    6C                   ENDSL
114100960927     C**
114200960927     C                   SETON                                        28
114300960927     C                   SETOFF                                       252729
114400990114     C                   SETOFF                                       2624
114500960927    6C     *IN31         DOWEQ     *OFF
114600960927     C**
114700960927     C                   EXSR      ELASTA                                        STAMPA
114800960927     C**
114900960927    7C     WUSCIT        IFEQ      ' '
115000960927     C*
115100990512     C     KANM8         READE     FNANM08L                               31     TUTTE O CHI
115200960927    7C                   ENDIF
115300960927     C*
115400960927    6C                   ENDDO
115500960927   6AC                   ENDIF
115600960927     C                   ENDSR
115700960412     C*
115800960412     C* LEGGO LE ANOMALIE IN ARRIVO ----------------------------------*
115900960412     C     ARRIVO        BEGSR
116000960412     C                   MOVEL     'N'           WFLE              1
116100960412     C*
116200960412     C* IMPOSTO LE ANOMALIE CHE DEVO LEGGERE
116300960412    2C     D34IE         IFEQ      ' '
116400960412     C                   CLEAR                   EL
116500960412     C                   MOVEL     'E'           EL(1)
116600960412     C                   MOVEL     'I'           EL(2)
116700960412    2C                   ENDIF
116800960412     C**
116900960412     C                   Z-ADD     1             L                 3 0
117000170519    2C     L6_a(L)       DOWGT     0
117100960412     C     WUSCIT        ANDEQ     ' '
117200960412     C**
117300170519     C                   MOVEL     L6_a(L)       KFIL
117400960412     C**
117500960412     C                   Z-ADD     1             E                 3 0
117600960412    3C     EL(E)         DOWNE     ' '
117700960412     C     WUSCIT        ANDEQ     ' '
117800960412     C                   MOVEL     EL(E)         KAIE
117900960412     C                   CLEAR                   KLNP
118000960412     C                   SETOFF                                       30
118100960412     C**
118200960412    4C     *IN30         DOWEQ     *OFF
118300960412     C     WUSCIT        ANDEQ     ' '
118400960412     C                   CLEAR                   KFLS
118500990512     C     KANM9         SETGT     FNANM09L
118600990512     C     KANM71        READE     FNANM09L                               30
118700960412     C   30              MOVEL     999           KLNP9
118800960412     C  N30              MOVEL     ANMLNP        KLNP9
118900960412     C*
119000960412     C                   MOVEL     KLNP          KFLS
119100960412     C                   CLEAR                   KLNP
119200990512     C     KANM94        SETGT     FNANM09L
119300990512     C     KANM9         READE     FNANM09L                               30
119400960412     C   30              MOVEL     999           KLNP92
119500960412     C  N30              MOVEL     ANMFLS        KLNP92
119600960412     C                   CLEAR                   KFLS
119700960412     C*
119800960412    5C     KLNP9         IFNE      999
119900960412     C     KLNP92        ORNE      999
120000960412     C                   SETOFF                                       30
120100960412     C** TROVATA LA LNP
120200960412     C     KLNP9         IFLT      KLNP92
120300960412     C                   MOVEL     KLNP9         KLNP
120400960412     C                   ELSE
120500960412     C                   MOVEL     KLNP92        KLNP
120600960412     C                   ENDIF
120700960412     C**
120800990512     C     KANM9         SETLL     FNANM09L
120900990512     C     KANM9         READE     FNANM09L                               31
121000960412     C**
121100960412     C                   SETON                                        29
121200960412     C                   SETOFF                                       252728
121300990114     C                   SETOFF                                       2624
121400960412    6C     *IN31         DOWEQ     *OFF
121500960412     C**
121600960412     C                   EXSR      ELASTA                                        STAMPA
121700960412     C**
121800960412     C     WUSCIT        IFEQ      ' '
121900990512     C     KANM9         READE     FNANM09L                               31     TUTTE O CHI
122000960412     C                   ENDIF
122100960412    6C                   ENDDO
122200960412     C*
122300960412     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
122400960412   6AC     WUSCIT        IFEQ      ' '
122500960412     C                   MOVEL     KLNP          KFLS
122600960412     C                   CLEAR                   KLNP
122700960412    6C                   SELECT
122800960419     C* SOLO ANOMALIE APERTE (SE NO CONFERMA IDD)
122900960412     C     D34APR        WHENEQ    'S'
123000960419     C     D34ELA        ANDNE     'S'
123100960412     C                   CLEAR                   KDCH
123200990512     C     KANM93        SETLL     FNANM09L
123300990512     C     KANM93        READE     FNANM09L                               31
123400960419     C* SOLO ANOMALIE CHIUSE  (SE NO CONFERMA IDD)
123500960412     C     D34APR        WHENEQ    'C'
123600960419     C     D34ELA        ANDNE     'S'
123700960412     C                   Z-ADD     1             KDCH
123800990512     C     KANM93        SETLL     FNANM09L
123900990512     C     KANM94        READE     FNANM09L                               31
124000960412   X6C                   OTHER
124100990512     C     KANM94        SETLL     FNANM09L
124200990512     C     KANM94        READE     FNANM09L                               31
124300960412    6C                   ENDSL
124400960412     C**
124500960412    6C     *IN31         DOWEQ     *OFF
124600960412     C**
124700960412     C                   EXSR      ELASTA                                        STAMPA
124800960412     C**
124900960412    7C     WUSCIT        IFEQ      ' '
125000960412     C*
125100960412    8C     D34APR        IFEQ      'S'
125200960423     C     D34ELA        ANDNE     'S'
125300990512     C     KANM93        READE     FNANM09L                               31     ANOM APERTE
125400960412   X8C                   ELSE
125500990512     C     KANM94        READE     FNANM09L                               31     TUTTE O CHI
125600960412    8C                   ENDIF
125700960412    7C                   ENDIF
125800960412     C*
125900960412    6C                   ENDDO
126000960412   6AC                   ENDIF
126100960412     C**
126200960412     C* REIMPOSTO IL CAMPO DELLA LINEA DI PARTENZA
126300960412     C                   MOVEL     KFLS          KLNP
126400960412     C                   CLEAR                   KFLS
126500960412     C                   ELSE
126600960412     C                   SETON                                        30
126700960412    5C                   ENDIF
126800960412    4C                   ENDDO
126900960412     C*
127000960412     C                   ADD       1             E
127100960412    3C                   ENDDO
127200960412     C*
127300960412     C                   ADD       1             L
127400960412    2C                   ENDDO
127500960412     C                   ENDSR
127600960412     C*
127700960412     C* LEGGO LE ANOMALIE CON ANMFLE ---------------------------------*
127800960412     C     USAFLE        BEGSR
127900170519     C****               MOVEL     'S'           WFLE              1
128000960412     C                   Z-ADD     1             Y
128100960412     C**
128200960412    1C     FLE(Y)        DOWGT     0
128300960412     C     WUSCIT        ANDEQ     ' '
128400960412     C                   MOVEL     FLE(Y)        KCAA
128500960412     C                   CLEAR                   KFIL
128600960412     C                   SETOFF                                       30
128700960523     C*
128800960508     C* VERIFICO SE L'ANOMALIA E' SECONDO I PARAMETRI RICHIESTI
128900960508     C                   Z-ADD     KCAA          ANMCAA
129000960508     C                   EXSR      LEGCAA
129100960523     C*
129200960508     C* RICHIESTE LE ANOMALIE INTERNE
129300960508     C     D34IE         IFEQ      'I'
129400960508     C     §7CAIE        ANDNE     'I'
129500960508     C                   SETON                                        30
129600960508     C                   ENDIF
129700960523     C**
129800960523     C* RICHIESTE LE ANOMALIE NON IDD
129900960523     C     D34IE         IFEQ      'N'
130000960523     C     §7CIDD        ANDEQ     'S'
130100060428     C     d34ie         oreq      'N'
130200060428     C     §7CAIE        ANDNE     'E'
130300960523     C                   SETON                                        30
130400960523     C                   ENDIF
130500960508     C**
130600960508     C* RICHIESTE LE ANOMALIE IDD
130700960523    1C     D34IE         IFEQ      'D'
130800960508     C     §7CIDD        ANDNE     'S'
130900960523     C*  SE E' LA CONFERMA DELL'IDD LE DEVO ELABORARE COMUNQUE
131000960523    2C     D34ELA        IFNE      'S'
131100960508     C                   SETON                                        30
131200960523   X2C                   ELSE
131300960523    3C     §7CAIE        IFNE      'E'                                          ESTERNA NO I
131400960523     C                   SETON                                        30
131500960523    3C                   ENDIF
131600960523    2C                   ENDIF
131700960523    1C                   ENDIF
131800960412     C**
131900960417    2C     *IN30         DOWEQ     *OFF
132000990512     C     KANM5         SETGT     FNANM10L
132100990512     C     KCAA          READE     FNANM10L                               30
132200960412    3C     *IN30         IFEQ      *OFF
132300960412     C                   MOVEL     ANMLNA        KFIL
132400960412     C                   CLEAR                   KLNP
132500960412     C                   CLEAR                   KFLS
132600960412     C                   SETOFF                                       31
132700960412     C*
132800960412    4C     *IN31         DOWEQ     *OFF
132900960412     C     WUSCIT        ANDEQ     ' '
133000990512     C     KANM51        SETGT     FNANM10L
133100990512     C     KANM5         READE     FNANM10L                               31
133200960412     C  N31              MOVEL     ANMLNP        KLNP9
133300960412     C   31              MOVEL     999           KLNP9
133400960412     C*
133500960412     C                   MOVEL     KLNP          KFLS
133600960412     C                   CLEAR                   KLNP
133700990512     C     KANM52        SETGT     FNANM10L
133800990512     C     KANM51        READE     FNANM10L                               31
133900960412     C  N31              MOVEL     ANMFLS        KLNP92
134000960412     C   31              MOVEL     999           KLNP92
134100960412     C*
134200960412    5C     KLNP9         IFNE      999
134300960416     C     KLNP92        ORNE      999
134400960412     C** TROVATA LA LNP
134500960412    6C     KLNP9         IFLT      KLNP92
134600960412     C                   MOVEL     KLNP9         KLNP
134700960412     C                   ELSE
134800960412     C                   MOVEL     KLNP92        KLNP
134900960412    6C                   ENDIF
135000960412     C**
135100990512     C     KANM51        SETLL     FNANM10L
135200990512     C     KANM51        READE     FNANM10L                               31
135300960412     C**
135400960412     C                   SETOFF                                       272829
135500970529     C                   SETOFF                                       26
135600960412    6C     *IN31         DOWEQ     *OFF
135700960412     C**
135800960412     C                   EXSR      ELASTA                                        STAMPA
135900960412     C**
136000960412    7C     WUSCIT        IFEQ      ' '
136100990512     C     KANM51        READE     FNANM10L                               31     TUTTE O CHI
136200960412    7C                   ENDIF
136300960412    6C                   ENDDO
136400960412     C*
136500960412     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
136600960412    6C     WUSCIT        IFEQ      ' '
136700960412     C                   MOVEL     KLNP          KFLS
136800960412     C                   CLEAR                   KLNP
136900990512     C     KANM52        SETLL     FNANM10L
137000990512     C     KANM52        READE     FNANM10L                               31
137100960412     C**
137200960412    7C     *IN31         DOWEQ     *OFF
137300960412     C**
137400960412     C                   EXSR      ELASTA                                        STAMPA
137500960412     C**
137600960412    8C     WUSCIT        IFEQ      ' '
137700960412     C*
137800990512     C     KANM52        READE     FNANM10L                               31     TUTTE O CHI
137900960412    8C                   ENDIF
138000960412     C*
138100960412    7C                   ENDDO
138200960412    6C                   ENDIF
138300960412     C**
138400960412     C* REIMPOSTO IL CAMPO DELLA LINEA DI PARTENZA
138500960412     C                   MOVEL     KFLS          KLNP
138600960412     C                   CLEAR                   KFLS
138700960412     C                   SETOFF                                       31
138800960412   X5C                   ELSE
138900960412     C                   SETON                                        31
139000960412    5C                   ENDIF
139100960412    4C                   ENDDO
139200960412    3C                   ENDIF
139300960412    2C                   ENDDO
139400960412     C*
139500960412     C                   ADD       1             Y
139600960412    1C                   ENDDO
139700960412     C*
139800960412     C                   ENDSR
139900970529     C*
140000040614     C* LEGGO LE ANOMALIE CON ANMCDU solo in partenza ----------------*
140100970529     C     USACDU        BEGSR
140200970529     C                   MOVEL     'N'           WFLE              1
140300040615     C                   MOVEL     'S'           Wcdu              1
140400970529     C                   SETOFF                                       252728
140500990114     C                   SETOFF                                       2924
140600970529     C                   SETON                                        26
140700970529     C* LEGGO TUTTE LE ANOMALIE RICEVUTE CON CDU=SIMFEL SCARTANDO
140800040616     C* QUELLE CON LNP O FLS PRESENTI IN £1(lo scato avviene nella
140900040616     c*        exsr CONFR)
141000970529     C****
141100970529     C* 1°CICLO: LEGGO CON LNP > 0 E <> £1
141200970529     C****
141300040116     C                   MOVEL     'E'           KAIE
141400970529     C                   Z-ADD     SIMFEL        KCDU
141500970529     C                   Z-ADD     1             ANMLNP
141600040616     c
141700970529    0C     WUSCIT        IFEQ      *BLANKS
141800990512     C     KANM3         SETLL     FNANM03L
141900990512     C     KANM3A        READE     FNANM03L                               30
142000970529    1C     *IN30         DOWEQ     *OFF
142100040615     c                   eval      wsetgt='N'
142200040615     c
142300040429     C                   EXSR      ELASTA
142400040615     c*
142500040615     c                   if        wsetgt='S'
142600040615     C     KANM3         SETGT     FNANM03L
142700040429     c                   endif
142800040615     c
142900970529    2C     WUSCIT        IFEQ      *BLANKS
143000990512     C     KANM3A        READE     FNANM03L                               30
143100970529    2C                   END
143200970529    1C                   ENDDO
143300970529     C****
143400970529     C* 2°CICLO: LEGGO CON LNP = 0
143500970529     C*                 ED FLS > 0 E <> £1
143600970529     C****
143700970529   1AC     WUSCIT        IFEQ      *BLANKS
143800970529     C                   MOVE      *ZEROS        KLNP
143900970529     C                   Z-ADD     1             ANMFLS
144000990512     C     KANM3B        SETLL     FNANM03L
144100990512     C     KANM3D        READE     FNANM03L                               30
144200970529    1C     *IN30         DOWEQ     *OFF
144300040616     c                   eval      wsetgt='N'
144400040616     c
144500040429     C                   EXSR      ELASTA
144600040616     c
144700040616     C* Fls in £1 --> Scarto e mi posiziono su nuova Fls
144800040616    2c                   if        wsetgt='S'
144900990512     C     KANM3B        SETGT     FNANM03L
145000970529    2C                   END
145100040616    c
145200970529    2C     WUSCIT        IFEQ      *BLANKS
145300990512     C     KANM3D        READE     FNANM03L                               30
145400040616    2C                   END
145500970529    1C                   ENDDO
145600970529   1AC                   END
145700970529    0C                   END
145800040615     C                   MOVEL     'N'           Wcdu              1
145900970529     C                   ENDSR
146000960327     C*
146100960523     C* ELABORO I DATI DELLE ANOMALIE PER LA STAMPA -----------------*
146200960327     C     ELASTA        BEGSR
146300960327     C*
146400960327     C* CONFRONTO SELEZIONI FATTE
146500960327     C                   EXSR      CONFR
146600960327     C**
146700960327    1C     *IN10         IFEQ      *ON
146800960327     C**
146900960327     C* SE ELABORAZIONE DEFINITIVA IDD, INVIO ANCHE LE ANOMALIE
147000960705     C*  CHE CI SONO DA TRASMETTERE
147100960327    2C     *IN01         IFEQ      *ON
147200960419     C     *IN12         ANDEQ     *ON
147300960327    3C     ANMFT1        IFEQ      ' '
147400960412     C     ANMFL1        ANDGT     0
147500960327     C     ANMFT2        OREQ      ' '
147600960412     C     ANMFL2        ANDGT     0
147700960705     C                   CLEAR                   WTRM
147800960827     C* SERIE > 0
147900960827    4C     ANMNRS        IFGT      *ZEROS
148000960827     C**SE ANOMALIA 55 ED ESISTE BOLLA PARTENZA NON LA TRASMETTO E
148100960827     C* LA CHIUDO. CHIUDO ANCHE LA 200
148200960827     C* (SE BOLLA DA ME CONFERMABILE E CONFERMABILE ANCHE DA ALTRI AS)
148300960827     C**STESSA COSA SE ANOMALIA 50 (SENZA CHIUDERE LA 200)
148400960827     C     ANMCAA        IFEQ      50
148500960827     C     ANMCAA        OREQ      55
148600990409     C     ANMCAA        OREQ      56
148700960827     C                   EXSR      A50A55
148800960827     C                   ENDIF
148900960827    4C                   ENDIF
149000960827     C*
149100960705    4C     WTRM          IFEQ      ' '
149200990114     C   24
149300990512     COR 25ANMN10        CHAIN     FNANM000                           33
149400000306     C   26ANMNR3        CHAIN     FNANM000                           33
149500000306     C   27ANMNR7        CHAIN     FNANM000                           33
149600990512     C   28ANMNR8        CHAIN     FNANM000                           33
149700990512     C   29ANMNR9        CHAIN     FNANM000                           33
149800960404     C*
149900960327     C                   EXSR      ELAB
150000960705    4C                   ENDIF
150100960327    3C                   ENDIF
150200960327    2C                   ENDIF
150300960327     C**
150400960327   X1C                   ELSE
150500960327     C* 10 OFF - RECORD SELEZIONABILE
150600960327     C                   CLEAR                   WUSCIT
150700960327     C**
150800960327     C** G E S T I O N E
150900000317    2C     D34GES        IFEQ      'I'
151000000317     C     D34GES        OREQ      'G'
151100960327     C* RICHIAMO IL PGM FNLR44R PASSANDOGLI IL RECORD DELL'ANOMALIA
151200960327     C*  IL NUMERO RELATIVO DI RECORD
151300960327     C*  LA DS DEI PARAMETRI RICHIESTI
151400960515     C* LO RICHIAMO SOLO SE IL SOGGETTO ANOMALIA E' <> DA "E"
151500040116    3C**   ANMSAN        IFNE      'E'
151600040116     C**   ANMAIE        ANDEQ     'E'
151700040116     C**   ANMAIE        ORNE      'E'
151800040116    c
151900040116    3C     ANMSAN        IFNE      'E'
152000040116     C     wanom         ANDEQ     'A'
152100040116     C     ANMAIE        ORNE      'E'
152200040116     C     wanom         orne      'A'
152300960327     C                   CLEAR                   DSLR44
152400960515    4C     *IN27         IFEQ      *ON
152500960401     C     *IN28         OREQ      *ON
152600970529     C     *IN26         OREQ      *ON
152700990114     C     *IN24         OREQ      *ON
152800990114     C   24              Z-ADD     ANMN10        D44NRR
152900970529     C   26              Z-ADD     ANMNR3        D44NRR
153000960327     C   27              Z-ADD     ANMNR7        D44NRR
153100960327     C   28              Z-ADD     ANMNR8        D44NRR
153200960401     C                   MOVEL     'P'           D34APA
153300960515   X4C                   ELSE
153400960327     C   29              Z-ADD     ANMNR9        D44NRR
153500960417     C   25              Z-ADD     ANMN10        D44NRR
153600960401     C                   MOVEL     'A'           D34APA
153700960515    4C                   ENDIF
153800960327     C                   CLEAR                   D34F03
153900960327     C                   CLEAR                   D34F12
154000960327     C                   CLEAR                   D34RIL
154100960327     C                   MOVEL     DSLR34        D44LR
154200960327     C                   MOVEL     DSANM         D44ANM
154300960521     C                   MOVEL     'G'           D44MOD
154400960327     C*
154500960327     C                   MOVEL     DSLR44        KPJBU
154600960327     C                   CALL      'FNLR44R'
154700960327     C                   PARM                    KPJBA
154800111116     C                   PARM                    dslr34
154900960327     C                   MOVEL     KPJBU         DSLR44
155000111116     C***                MOVEL     D44LR         DSLR34
155100960327     C* SE PREMUTO F3 F12 O RILETTURA
155200960515    4C     D34F03        IFEQ      'S'
155300960327     C     D34F12        OREQ      'S'
155400960327     C     D34RIL        OREQ      'S'
155500960327     C                   MOVEL     'S'           WUSCIT            1
155600960327     C                   SETON                                        3031
155700960515    4C                   ENDIF
155800960515    3C                   ENDIF
155900960327     C**
156000960327   X2C                   ELSE
156100960327     C** S T A M P A
156200960327     C*
156300000317    3C                   SELECT
156400000317     C* ELABORAZIONE IDD
156500000317     C     D34ELA        WHENNE    ' '
156600960405     C  N11              EXSR      STAMPA
156700960327     C*
156800960327     C* ELABORAZIONE DEFINITIVA AGGIORNO E SPEDISCO
156900960405     C   01              EXSR      ELAB
157000960327     C**
157100000317     C     D34GES        WHENEQ    'S'
157200960327     C* NON E' STAMPA IDD --> STAMPO TUTTO
157300960327     C                   EXSR      STAMPA
157400000317     C*
157500000317    3C                   ENDSL
157600960327    2C                   ENDIF
157700960327    1C                   ENDIF
157800960327     C*
157900960327     C                   ENDSR
158000921015     C*
158100921015     C* CONFRONTO RICHIESTA FATTA A VIDEO ---------------------------*
158200921015     C     CONFR         BEGSR
158300160908     C                   SETOFF                                         2097
158400960419     C                   SETOFF                                       101112
158500960509     C                   CLEAR                   PRTZNC
158600960927     C                   CLEAR                   PRTDET
158700960927     C                   CLEAR                   WDDET
158800960927     C                   CLEAR                   WDFLP
158900960927     C                   CLEAR                   PSTAM
159000030717     C**
159100030717     C* SE RICHIESTO ESEGUO LA SELEZIONE X P.O. SEGNACOLLO E NUMERO SERIE
159200030717     C                   IF        D34POS  > *ZEROS
159300030717     C                   IF        D34POS <> ANMFLS
159400030717     C                   SETON                                        10
159500030717     C                   GOTO      ENDCON
159600030717     C                   ENDIF
159700040614     c
159800030717     C                   IF        D34NRS <> 99
159900030717     C                   IF        D34NRS <> ANMNRS
160000030717     C                   SETON                                        10
160100030717     C                   GOTO      ENDCON
160200030717     C                   ENDIF
160300030717     C                   ENDIF
160400040614     C                   ENDIF
160500040609     c
160600040609     c* se richiesto, faccio vedere solo anomalie di colli di valore
160700040609     c                   if        D34bva='V'  and
160800040609     c                             anmft4<>'V'
160900040609     C                   SETON                                        10
161000040609     C                   GOTO      ENDCON
161100040609     C                   ENDIF
161200000317     C**
161300980402     C* CONTROLLO CHE LA LINEA DI ARRIVO SIA UGUALE A QUELLA RICHIESTA A VIDEO
161400980403     C* SE NON SONO IN UNA FASE DI TRASFERIMENTO
161500980403     C     D34TRA        IFNE      'S'
161600980402     C     D34LNA        IFNE      *ZEROS
161700980402     C     D34LNA        ANDNE     ANMLNA
161800980402     C                   SETON                                        10
161900980402     C                   GOTO      ENDCON
162000980402     C                   ENDIF
162100980403     C                   ENDIF
162200960424     C*
162300960424     C* SE STO LEGGENDO UNA ANOMALIA RICEVUTA, NON LA STAMPO
162400960424     C*  SE IL SEGNACOLLO NEL FRATTEMPO E' STATO CONSEGNATO
162500960524     C*  (NON COME IDD3 DANNI CODICE 5)
162600970127     C*  (NON SE DATA APERTURA ANOMALIA > DATA CONSEGNA)
162700040116     C     ANMSCN        ifgt      0
162800040223     C     wanom         andeq     'P'
162900111115     c* In gestione/int non lo faccio --> devo vedere le anomalie come sono
163000111115     c*  per poterle annullare
163100111115     c     d34ges        andeq     'S'
163200960424     C*
163300960424     C     KART          CHAIN     FNBLT27L                           32
163400960424    2C     *IN32         IFEQ      *OFF
163500960424     C*
163600960424    3C     BLTDCM        IFGT      0
163700960927     C     BLTDET        ORGT      0
163800960524     C**
163900960524     C     KBLP          CHAIN     FNBLP01L                           32
164000960524     C**
164100970127    4C     BLTDCM        IFGT      0
164200970127     C     BLPCCA        ANDNE     '5'
164300151125     C     BLPCCA        ANDNE     '7'
164400151125     C     ANMDAO        ANDLE     BLTDCM
164500960424     C* SE HO CHIESTO SOLO LE APERTE ESCO
164600960524    5C     D34APR        IFEQ      'S'
164700960424     C                   SETON                                        10
164800960424     C                   GOTO      ENDCON
164900960524   X5C                   ELSE
165000960424     C* ALTRIMENTI DICO CHE E' CHIUSA COME CONSEGNATA
165100960524    6C     ANMDCH        IFEQ      0
165200960424     C                   MOVE      BLTDCM        ANMDCH
165300960524    6C                   ENDIF
165400960524     C                   MOVEL     'CO'          ANMCCH
165500960524    5C                   ENDIF
165600960424    4C                   ENDIF
165700960424    3C                   ENDIF
165800960424     C*
165900960424     C* SE ANOMALIA 10 NON STAMPO SE LA SPEDIZIONE C'E' ED E' TRASMESSA
166000960424    3C     ANMCAA        IFEQ      10
166100960424     C     KBLP          CHAIN     FNBLP01L                           32
166200960509     C  N32              MOVEL     BLPZNC        PRTZNC            2
166300160908
166400151125    4C  N32BLPFT1        IFeq      'T'
166500960424     C* SE HO CHIESTO SOLO LE APERTE ESCO
166600960424    5C     D34APR        IFEQ      'S'
166700960424     C                   SETON                                        10
166800960424     C                   GOTO      ENDCON
166900960424   X5C                   ELSE
167000960424    6C     ANMDCH        IFEQ      0
167100960424     C                   Z-ADD     BLPMGS        ANMDCH
167200990512     C                   MOVEL     BLPAAS        ANMDCH
167300960424    6C                   ENDIF
167400960424    5C                   ENDIF
167500160908     c
167600960424    4C                   ENDIF
167700960424    3C                   ENDIF
167800960508     C**
167900960508     C** SE ANOMALIA 5 STAMPO SE E' ANDATA IN DISGUIDO
168000960508    3C     ANMCAA        IFEQ      005
168100040617     c
168200970604     C* PRIMA VERIFICO SE PER IL SEGNACOLLO ESISTE UN'ALTRA SPUNTA
168300970604     C* PARTENZA CON DATA > PERCHE' IN QUESTO CASO L'ANOMALIA
168400970604     C* NON E' DA STAMPARE
168500970604   3AC     ANMCDU        IFEQ      SIMFEL
168600970604     C* DETERMINO DATA DEL FOGLIO DELL'ANOMALIA
168700970604    4C     ANMNFV        IFNE      SAVNFV
168800970604     C                   MOVE      ANMNFV        SAVNFV
168900970604     C                   CLEAR                   DSLV53
169000970604     C                   MOVEL     'G'           D53TFO
169100970604     C                   Z-ADD     SIMFEL        D53FEL
169200970604     C                   Z-ADD     ANMNFV        D53NFV
169300020926     C                   Z-ADD     ANMcdu        D53FGS
169400970604     C                   CALL      'FNLV53R'
169500970604     C                   PARM                    DSLV53
169600970604     C                   MOVEL     '1'           WCL53             1
169700970604     C                   Z-ADD     D53DFV        W53DFV
169800970604    4C                   END
169900970603     C*
170000970603     C                   Z-ADD     1             KNPG
170100140618     C     KBRV3A        SETLL     FNBRV05L
170200140618     C     KBRV3A        READE     FNBRV05L                               32
170300970603    4C     *IN32         DOWEQ     *OFF
170400160623
170500970604   4AC     BRVNFV        IFNE      ANMNFV
170600160623     c* Solo se spunta reale lo eseguo
170700160623   4Bc                   if        %lookup(brvnps:sps)>0
170800970603     C                   CLEAR                   DSLV53
170900970604     C                   MOVEL     'G'           D53TFO
171000970603     C                   Z-ADD     SIMFEL        D53FEL
171100970603     C                   Z-ADD     BRVNFV        D53NFV
171200020926     C                   Z-ADD     BRVfgs        D53FGS
171300970603     C                   CALL      'FNLV53R'
171400970603     C                   PARM                    DSLV53
171500970603     C                   MOVEL     '1'           WCL53             1
171600980424     C* QUESTO CONTROLLO LO IGNORO SE SONO IN TRASFERIMENTO
171700980424    5C     D34TRA        IFNE      'S'
171800980424     C     D53ERR        ANDEQ     *BLANKS
171900970604     C     D53DFV        ANDGT     W53DFV
172000970603     C                   SETON                                        10
172100970603     C                   GOTO      ENDCON
172200970603    5C                   END
172300160623   4BC                   END
172400970604   4AC                   END
172500140618     C     KBRV3A        READE     FNBRV05L                               32
172600970603    4C                   ENDDO
172700970604   3AC                   ENDIF
172800970603     C*
172900960927     C                   EXSR      DISPAR
173000960927    3C                   ENDIF
173100960927     C**
173200960424     C* MEMORIZZO IL NUMERO DI SPEDIZIONE SE C'E'
173300960424     C                   MOVE      BLTAAS        ANMAAS
173400960424     C                   MOVE      BLTLNP        ANMLNP
173500960424     C                   MOVE      BLTNSP        ANMNSP
173600960424    2C                   ENDIF
173700960424    1C                   ENDIF
173800040617     C
173900040617     c* Per stampa colli di valore in arrivo, escludo anomalie se non
174000040617     c*  crerate dal p.o. gestione  SE STAMPA DEL TERMINAL
174100040617     c                   if        *in70 and wanom='A' AND ANMCAA=005 AND
174200040617     C                             *IN80
174300040617     c                   if        anmfle<>d34fgs
174400040617     C                   SETON                                        10
174500040617     C                   GOTO      ENDCON
174600040617     c                   endif
174700040617     c                   endif
174800960506     C* CODICE ANOMALIA
174900960508     C                   EXSR      LEGCAA
175000040615     c
175100040615     c* In USACDU escludo sempre le anomalie non IDD (in partenza sempre
175200040615     c*           lette per lnp)
175300040615     c                   if        wcdu='S' and §7caie='E' and §7cidd=' '
175400040615     C                   SETON                                        10
175500040615     C                   GOTO      ENDCON
175600040615    4C                   ENDIF
175700960419     C**
175800960506     C**
175900960419     C* 1) SE CONTROLLO NON PASSATO NON FACCIO NULLA
176000960419     C*
176100170519     C* SE DEVO USARE FLE, VERIFICO SE E' IN L6  partenza o arrivo a secoda
176200170519    1C***  WFLE          IFEQ      'S'
176300170519    1C                   IF        wfle<>'N'
176400170519     c
176500170519    2C                   IF        wfle='P'
176600960416     C     ANMFLE        LOOKUP    L6                                     32
176700960416     C  N32              SETON                                            10
176800960417     C  N32              GOTO      ENDCON
176900170519    2c                   endif
177000170519     c
177100170519    2C                   IF        wfle='A'
177200170519     C     ANMFLE        LOOKUP    L6_a                                   32
177300170519     C  N32              SETON                                            10
177400170519     C  N32              GOTO      ENDCON
177500170519    2c                   endif
177600961230     C*
177700970829    2C     WANOM         IFEQ      'A'
177800961017     C* VERIFICO ANCHE CHE NON SIA ANOMALIA CHE UTILIZZA
177900961017     C* SIA LNA CHE FLE CON LNA=FLE
178000970829    3C     §7CUEA        IFEQ      'E'
178100970829    4C     ANMLNA        IFEQ      ANMFLE
178200961017     C                   SETON                                        10
178300961017     C                   GOTO      ENDCON
178400970829    4C                   ENDIF
178500040413     C* SE E' IN L6  MA LO E' ANCHE LA LNA
178600961230     C*  ED E' UN TIPO ANOMALIA CHE VIENE STAMPATO  PER ENTRANBI
178700961230     C*  ESCLUDO PERCHE' L'HO GIA' STAMPATA
178800170519     C     ANMLNA        LOOKUP    L6_a                                   32
178900961230     C   32              SETON                                            10
179000961230     C   32              GOTO      ENDCON
179100970829    3C                   ENDIF
179200970829     C*
179300970829   X2C                   ELSE
179400970829     C* FLE UTILIZZATO IN PARTENZA
179500970829    3C     §7CUEP        IFEQ      'E'
179600970829     C* PRENDO O LNP O FLS
179700970829    4C     ANMLNP        IFGT      0
179800971125     C                   Z-ADD     ANMLNP        W0030             3 0
179900970829     C                   ELSE
180000970829     C                   Z-ADD     ANMFLS        W0030
180100970829    4C                   ENDIF
180200970829     C     W0030         LOOKUP    L1                                     32
180300970829     C   32              SETON                                            10
180400970829     C   32              GOTO      ENDCON
180500970829    3C                   ENDIF
180600970829     C**
180700970829    2C                   ENDIF
180800961230    1C                   ENDIF
180900960419     C**
181000960419     C* 2) SE CONTROLLO NON PASSATO NON FACCIO NULLA
181100960419     C**
181200960419     C* DATA APERTURA ANOMALIA DAL/AL
181300960419    1C     D34DLA        IFGT      0
181400960419    1C     D34ALA        ORGT      0
181500990512    2C     ANMDAO        IFLT      D34DLA
181600990512     C     ANMDAO        ORGT      D34ALA
181700960419     C                   SETON                                        10
181800960419     C                   GOTO      ENDCON
181900960419    2C                   ENDIF
182000960419    1C                   ENDIF
182100040615     c**
182200040615     c* 2a) Il controllo che faceva in USACDU lo faccio qui dopo aver
182300040615     c*     controllato la data
182400040615    1c                   if        wcdu='S'
182500040615     c
182600040615     c* cerco alla data anmdao se d34fgs è terminal di anmlnp
182700040615     c                   eval      kcaeepa='P'
182800040615    2c                   if        anmlnp>0
182900040615     c                   eval      kcaetfp=anmlnp
183000040615     c                   else
183100040615     c                   eval      kcaetfp=anmfls
183200040615    2c                   endif
183300040615     c                   eval      kcaetfe=d34fgs
183400040615     c                   eval      kcaedde=anmdao
183500040615     c     kcae          setll     azcae03l
183600040615     c     kcae2         readpe    azcae03l
183700040616     c
183800040616    2c                   dow       not %eof(azcae03l) and caeatb<>*blanks
183900040616     c     kcae2         readpe    azcae03l
184000040616    2c                   enddo
184100040616     c
184200040616    2c                   if        not %eof(azcae03l)
184300040615     c* se decorrente alla data anomalia --> p.o. della £1 di d34fgs escludo
184400040615    3c                   if        caedsc>=anmdao
184500040615     c                   seton                                        10
184600040615     c* Se Compreso tutto il periodo richiesto a video, escludo tutta la
184700040615     c*  lnp e quindi faccio SETGT in usacdu
184800040615    4c                   if        caedde<=d34dla  and
184900040615     c                             caedsc>=d34ala
185000040615     c                   eval      wsetgt='S'
185100040615    4c                   endif
185200040615     c
185300040615    3c                   endif
185400040615    2c                   endif
185500040615     c   10              goto      endcon
185600040615     c*
185700040615     c* Escludo anche dalla stampa anomalie con anmlna 2 livello di
185800040616     c*  d34fgs per non stampare anomalie di arrivo e  non di transito
185900040615    2c                   if        anmlna=d34fgs
186000040615     C                   SETON                                        10
186100040615     C                   GOTO      ENDCON
186200040616   x2c                   else
186300040616     c
186400040616     c                   clear                   wanomok           1
186500040616     c* Controllo il terminal di arrivo per sta anomalia
186600040616     c                   eval      kcaeepa='A'
186700040616    3c                   if        anmlnp>0
186800040616     c                   eval      kcaetfp=anmlnp
186900040616     c                   else
187000040616     c                   eval      kcaetfp=anmfls
187100040616    3c                   endif
187200040616     c                   eval      kcaetfa=anmlna
187300040616     c                   eval      kcaedde=anmdao
187400040616     c
187500040616     c                   EXSR      CercaCAE
187600040616     c
187700040616     c* Se non ho trovato eccezione terminal, cerco terminal normale
187800040616    3c                   if        wanomok=' '
187900040616     c                   clear                   kcaetfp
188000040616     c                   EXSR      CercaCAE
188100040616    3c                   endif
188200040616    c
188300040616     c* anomalia da non stampare
188400040616    3c                   if        wanomok='N'
188500040616     C                   SETON                                        10
188600040616     C                   GOTO      ENDCON
188700040616    3c                   endif
188800040616     c
188900040616    2c                   endif
189000040615    1c                   endif
189100960419     C**
189200960419     C* 3) SE CONTROLLO NON PASSATO NON FACCIO NULLA
189300960419     C**
189400960419     C* SE NON STO ELABORANDO PER FLE ESCLUDO LE ANOMALIE FLE
189500970829     C     WFLE          IFEQ      'N'
189600960419     C     ANMCAA        LOOKUP    FLE                                    10
189700960419     C   10              GOTO      ENDCON
189800960419     C                   ENDIF
189900960419     C**
190000960419     C* 4) SE E COMUNQUE ANOMALIE ESTERNA ANCHE SE NO IDD LA TRASMETTO
190100960419     C**
190200960327     C* SE HO RICHIESTO LE ESTERNE O LE RICEVUTE, VEDO SE
190300960327     C*  SONO QUELLE IDD O NO
190400960327     C     D34IE         IFNE      'I'
190500960327     C                   MOVEL     ANMCAA        W003A             3
190600960506     C     §7CIDD        IFEQ      'S'
190700960506     C                   SETON                                        97
190800960506     C                   ENDIF
190900960404     C* 97 ON - E' ANOMALIA IDD
191000960404    2C     *IN97         IFEQ      *ON
191100960506    3C     D34IE         IFEQ      'N'
191200960327     C                   SETON                                        10
191300960327     C                   GOTO      ENDCON
191400960327    3C                   ENDIF
191500960327     C*
191600960327   X2C                   ELSE
191700960404     C* 97 OFF  - NO IDD
191800960506    3C     D34IE         IFEQ      'D'
191900960327     C                   SETON                                        10
192000960419     C* 12 ON SE COMUNQUE ANOMALIA ESTERNA
192100040116     C   01aNMAIE        IFEQ      'E'
192200960419     C                   SETON                                        12
192300960419    3C                   ENDIF
192400960419     C*
192500960327     C                   GOTO      ENDCON
192600960327    3C                   ENDIF
192700960327    2C                   ENDIF
192800960327    1C                   ENDIF
192900960419     C**
193000960419     C* 5) SE ANOMALIA IDD CHIUSA TRASMETTO SENZA STAMPARE
193100960419     C**
193200960419     C* STAMPA SOLO ANOMALIE APERTE
193300961217     C* PER LE ANOMALIE LOCALI 70 E 71 FACCIO UN GIRO A PARTE
193400040116    0C**   *IN07         IFEQ      *OFF
193500040116     C**   *IN07         OREQ      *ON
193600040116     C**   ANMCAA        ANDNE     70
193700040116     C**   ANMCAA        ANDNE     71
193800040116     c
193900040116     c                   if        wanom='A'  or
194000040116     c                             wanom='P' and anmcaa<>70 and anmcaa<>71
194100961217     C*
194200960419    1C     D34APR        IFEQ      'S'
194300960419     C     ANMDCH        ANDNE     0
194400960419     C                   SETON                                        10
194500960419     C   01ANMAIE        IFEQ      'E'
194600960419     C                   SETON                                        12
194700960419     C                   ENDIF
194800960419     C                   GOTO      ENDCON
194900960419    1C                   ENDIF
195000960419     C* SOLO ANOMALIE CHIUSE
195100960419    1C     D34APR        IFEQ      'C'
195200960419     C     ANMDCH        ANDEQ     0
195300960419     C                   SETON                                        10
195400960419     C                   GOTO      ENDCON
195500960419    1C                   ENDIF
195600961217     C*
195700961217   X0C                   ELSE
195800961217     C* NON TESTO LA DATA DI CHIUSURA MA LA CASUALE
195900961217    1C     D34APR        IFEQ      'S'
196000961217    2C     ANMCCH        IFNE      '  '
196100961217     C                   SETON                                        10
196200961217   X2C                   ELSE
196300961217     C*
196400961217     C* PRIMA DI STAMPARLA VERIFICO SE CE N'E' UN'ALTRA COM DATA >
196500961217     C*                    MI SALVO LA DATA
196600990512     C                   MOVEL     ANMDAO        W0080             8 0
196700990512     C                   MOVEL     ANMAAS        KAAS2
196800961217     C                   MOVEL     ANMLNP        KLNP2
196900961217     C                   MOVEL     ANMNRS        KNRS2
197000961217     C                   MOVEL     ANMNSP        KNSP2
197100961217     C                   Z-ADD     ANMCAA        KCAA
197200961217     C*
197300961217     C* LEGGO IL COLLO FINTANTO CHE TROVO DELLE ANOMALIE INSERITE
197400961217     C*  NEL CASO STAMPERO' QUELLA
197500990512     C     KANM1         SETLL     FNANM01L
197600990512     C     KANM1         READE     FNANM01L                               31
197700961217    3C     *IN31         DOWEQ     *OFF
197800990512     C     ANMDAO        ANDLE     W0080
197900990512     C     KANM1         READE     FNANM01L                               31
198000961217    3C                   ENDDO
198100961217     C*
198200961217    3C     *IN31         IFEQ      *ON
198300990512     C   27ANMNR7        CHAIN(N)  FNANM000                           33
198400990512     C   28ANMNR8        CHAIN(N)  FNANM000                           33
198500961217     C*
198600961217     C                   CLEAR                   ANMDCH
198700961217     C                   CLEAR                   ANMFSC
198800961217   X3C                   ELSE
198900961217     C                   SETON                                        10
199000961217     C                   GOTO      ENDCON
199100961217    3C                   ENDIF
199200961217    2C                   ENDIF
199300961217    1C                   ENDIF
199400961217     C*
199500961217    1C     D34APR        IFEQ      'C'
199600961217     C     ANMCCH        ANDEQ     '  '
199700961217     C                   SETON                                        10
199800961217     C                   GOTO      ENDCON
199900961217    1C                   ENDIF
200000961217     C*
200100961217    0C                   ENDIF
200200960419     C**
200300960426     C* 7) TUTTI GLI ALTRI CONFRONTI, TRANNE IL LIVELLO IDD NON FANNO
200400960419     C*    NE STAMPARE NE TRASMETTERE
200500960419     C**
200600960327     C* DATA CHIUSURA ANOMALIA DAL/AL
200700960927     C* PER LE ANOMALIE NON IDD3
200800960927     C     *IN05         IFEQ      *ON
200900960927     C     ANMCCH        ANDEQ     'PR'
201000960927     C     ANMLNA        LOOKUP    FM1                                    32
201100960927     C                   ENDIF
201200960927     C*
201300960927    0C     *IN05         IFEQ      *OFF
201400960927     C     ANMCCH        ORNE      'PR'
201500960927     C     *IN32         OREQ      *OFF
201600960927     C*
201700960327    1C     D34DCA        IFGT      0
201800960419     C     D34ACA        ORGT      0
201900990512    2C     ANMDCH        IFLT      D34DCA
202000990512     C     ANMDCH        ORGT      D34ACA
202100960327     C                   SETON                                        10
202200960327     C                   GOTO      ENDCON
202300960327    2C                   ENDIF
202400960327    1C                   ENDIF
202500960927     C*
202600960927   X0C                   ELSE
202700990512    1C     ANMDCH        IFLT      W08DM1
202800990512     C     ANMDCH        ORGT      W08AM1
202900960927     C                   SETON                                        10
203000960927     C                   GOTO      ENDCON
203100960927    1C                   ENDIF
203200960927    0C                   ENDIF
203300921015     C*
203400921015     C* STAMPA FASE SPECIFICA
203500960327    1C     D34FAS        IFNE      ' '
203600960326     C     ANMFSA        ANDNE     D34FAS
203700921015     C                   SETON                                        10
203800921015     C                   GOTO      ENDCON
203900960327    1C                   ENDIF
204000960426     C*
204100960506     C* STAMPA CAUSALE CHIUSURA SPECIFICA
204200960426    1C     D34CCH        IFNE      ' '
204300960426     C     ANMCCH        ANDNE     D34CCH
204400960426     C                   SETON                                        10
204500960426     C                   GOTO      ENDCON
204600960426    1C                   ENDIF
204700961108     C*
204800040116     C* in partenza escludo le anomalie appena create (livello 0 o 2
204900040616     c*  per anom 15) solo per le anomalie IDD e se non prevista la
205000040616     c*  stampa dalla DS D34SL0
205100040116     C** 07ANMLID        IFEQ      0
205200040616     c                   if        d34sl0=' '
205300040119     c                   if        anmlid=§7clid  and §7cidd='S' and
205400040119     c                             wanom='P' and §7cinv=' '
205500961108     C                   SETON                                        10
205600961108     C                   GOTO      ENDCON
205700961108    1C                   ENDIF
205800040616    1C                   ENDIF
205900960419     C*
206000960506     C* STAMPA CODICI ANOMALIE RICHIESTE
206100960327    1C     D34ANM        IFNE      *BLANKS
206200921015     C                   MOVEL     ANMCAA        ALF3              3
206300921015     C     ALF3          LOOKUP    ANM                                    31
206400960327    2C     D34INC        IFEQ      'I'
206500921015     C  N31              SETON                                        10
206600921015     C  N31              GOTO      ENDCON
206700921015     C*
206800960327   X2C                   ELSE
206900921015     C*
207000921015     C   31              SETON                                        10
207100921015     C   31              GOTO      ENDCON
207200960327    2C                   END
207300960402     C*
207400960402     C                   ELSE
207500960402     C*
207600960402     C* SE NON HO RICHIESTO UNO SPECIFICO CODICE ANOMALIA
207700960402     C*  GESTISCO SOLO QUELLI STAMPABILI
207800960402     C     §7CSTA        IFNE      'S'
207900960402     C                   SETON                                        10
208000960402     C                   GOTO      ENDCON
208100960506    2C                   ENDIF
208200960506    1C                   ENDIF
208300921015     C*
208400921015     C* NUMERO FOGLIO VIAGGIO
208500960327    1C     D34NFV        IFNE      0
208600960326     C     ANMNFV        ANDNE     D34NFV
208700921015     C                   SETON                                        10
208800921015     C                   GOTO      ENDCON
208900960327    1C                   ENDIF
209000960506     C*
209100960506     C* SE ANOMALIA INTERNA VEDO SE RICHIESTO PART O ARR
209200960506    1C     ANMAIE        IFEQ      'I'
209300960506     C     D34PAR        ANDNE     ' '
209400960506     C*
209500960506     C* RICHIESTE LE ANOMALIE PARTENZA MA QUESTA E' ANOMALIA ARRIVO
209600960506    2C     D34PAR        IFEQ      'P'
209700960506     C     §7CPAR        ANDEQ     'A'
209800960506     C                   SETON                                        10
209900960506    2C                   ENDIF
210000960506     C* VICEVERSA
210100960506    2C     D34PAR        IFEQ      'A'
210200960506     C     §7CPAR        ANDEQ     'P'
210300960506     C                   SETON                                        10
210400960506    2C                   ENDIF
210500960506     C*
210600960506     C   10              GOTO      ENDCON
210700960506    1C                   ENDIF
210800040615     c
210900960403     C* DAL LIVELLO IDD
211000980327     C     *IN97         IFEQ      *ON
211100980327     C     *IN01         ANDEQ     *ON
211200980327     C     D34TRA        OREQ      'S'
211300000317     C     D34GES        OREQ      'V'
211400990114     C   24
211500990512     COR 25ANMN10        CHAIN     FNANM000                           33
211600000306     C   26ANMNR3        CHAIN     FNANM000                           33
211700000306     C   27ANMNR7        CHAIN     FNANM000                           33
211800990512     C   28ANMNR8        CHAIN     FNANM000                           33
211900990512     C   29ANMNR9        CHAIN     FNANM000                           33
212000960403     C                   ENDIF
212100960403     C*
212200960404    1C   97D34ELA        IFNE      ' '
212300960403     C                   ADD       1             ANMLID
212400960403     C                   ENDIF
212500960403     C**
212600960405     C   97ANMLID        IFLT      D34LID
212700960405     C* SE NON E' ELABORAZIONE DEFINITIVA IDD 10 ON
212800960405     C     D34ELA        IFNE      'S'
212900960403     C                   SETON                                        10
213000960417     C                   GOTO      ENDCON
213100960405     C                   ELSE
213200960419     C* ALTRIMENTI ACCENDO L' 11  PERCHE' DEVO AGGIORNARE SENZA STAMPAR
213300960405     C                   SETON                                        11
213400960405     C                   ENDIF
213500960405     C*
213600960403    2C                   ENDIF
213700960412     C*
213800960327     C     ENDCON        ENDSR
213900040616     c     CercaCAE      begsr
214000040616     c     kcae1         setll     azcae01l
214100040616     c     kcae3         readpe    azcae01l
214200040616    3c                   dow       not %eof(azcae01l) and caeatb<>*blanks
214300040616     c     kcae3         readpe    azcae01l
214400040616    3c                   enddo
214500040616     c
214600040616    3c                   if        not %eof(azcae01l)
214700040616    4c                   if        caedde<=anmdao   and caedsc>=anmdao
214800040616     c* Trovata eccezione terminal --> se il terminal  d34fgs esco
214900040616    5c                   if        caetfe=d34fgs
215000040616     c* Anomalia da non stampare
215100040616     c                   eval      wanomok='N'
215200040616     c                   else
215300040616     c* Anomalia da     stampare
215400040616     c                   eval      wanomok='S'
215500040616    5c                   endif
215600040616    4c                   endif
215700040616    3c                   endif
215800040616     c                   ENDSR
215900960927     C*
216000960927     C* CONTROLLO SE C'E' DISGUIDO IN LETTURA ANOMALIE PARTENZA ------*
216100960930     C     DISPAR        BEGSR
216200971124     C* VEDO SE E' PRESENTE UNA ANOMALIA DI DISGUIDO
216300990409     C                   SETOFF                                       20
216400990409     C                   DO        2
216500990409     C                   Z-ADD     56            KCAA
216600990409     C     *IN20         IFEQ      *OFF
216700990512     C     KANM2         CHAIN     FNANM02L                           32
216800960927    1C     *IN32         DOWEQ     *OFF
216900971126    2C     AN2DAO        IFGT      WDDET
217000960927     C                   SETON                                        20
217100960927     C                   MOVEL     CDISG         PSTAM
217200960927     C                   MOVEL     AN2FLE        WDFLP
217300971126     C                   Z-ADD     AN2DAO        WDDET
217400960927    2C                   ENDIF
217500990512     C     KANM2         READE     FNANM02L                               32
217600960927    1C                   ENDDO
217700990409     C*
217800990409     C                   Z-ADD     55            KCAA
217900990409    2C                   ENDIF
218000990409    1C                   ENDDO
218100960927     C*
218200960927     C* SE NON TROVATA CONTROLLO BLTFLP
218300960927    1C     *IN20         IFEQ      *OFF
218400960927     C     BLTDET        ANDGT     0
218500960927     C**
218600960927     C* CHAIN IL FV
218700020807     C     K02FGV01      CHAIN     FNFGV01L                           32
218800960927    4C     *IN32         IFEQ      *OFF
218900020807     C     K02FGV01      CHAIN     FNFGW01L                           32
219000000503    5C     *IN32         IFEQ      *ON
219100000503     C     FGWATB        ORNE      ' '
219200000503     C                   CLEAR                   FGWFF3
219300000503     C                   CLEAR                   FGWFF4
219400000503     C                   CLEAR                   FGWFL3
219500000503     C                   CLEAR                   FGWFL4
219600000503    5C                   ENDIF
219700960927     C                   MOVEL     BLTFLP        W003A             3
219800960930     C                   MOVEL     ANMLNA        W003AA            3
219900961001     C                   Z-ADD     0             XX                3 0
220000961001     C                   SETON                                        32
220100961001     C*
220200961001    5C     *IN32         DOWEQ     *ON
220300961001     C     PSTAM         ANDEQ     *BLANKS
220400961001     C                   ADD       1             XX
220500960927     C     W003A         LOOKUP    FLP(XX)                                32
220600960927     C**
220700961001    6C     *IN32         IFEQ      *ON
220800960930     C     FFV(XX)       ANDEQ     W003AA
220900960930     C                   MOVEL     CTRANS        PSTAM            18
221000961001    6C                   ENDIF
221100961001    5C                   ENDDO
221200961001     C**
221300961001    5C     PSTAM         IFEQ      *BLANKS
221400960927     C                   MOVEL     CDISG         PSTAM
221500960927    5C                   ENDIF
221600960927   X4C                   ELSE
221700960927     C                   MOVEL     CDISG         PSTAM
221800960927    4C                   ENDIF
221900960927     C                   SETON                                        20
222000960927     C                   MOVEL     BLTFLP        WDFLP
222100960927     C                   MOVEL     BLTDET        WDDET
222200960927    1C                   ENDIF
222300960927     C                   ENDSR
222400960508     C*
222500960508     C* CHAIN SULLE TABELLE DELL'ANOMALIA ---------------------------*
222600960508     C     LEGCAA        BEGSR
222700960508     C     ANMCAA        IFNE      SAVCAA
222800960508     C                   MOVEL     '7C'          COD
222900960508     C                   MOVEL(P)  ANMCAA        KEY
223000960508     C     KTAB          CHAIN(N)  TABEL                              31
223100960508     C  N31              MOVEL     TBLUNI        DS7C
223200960508     C   31              CLEAR                   DS7C
223300960508     C                   MOVEL     ANMCAA        SAVCAA
223400960508     C                   ENDIF
223500960508     C                   ENDSR
223600911015     C*
223700960827     C* CHIUDO ANOMALIE DI DISGUIDO O ANOMALIE 50 SE ESISTE BLP ------*
223800960827     C     A50A55        BEGSR
223900021025     C                   SETOFF                                       32
224000021025     C     KART          CHAIN     FNBLT27L                           39
224100021025     C  N39BLTLNP        LOOKUP    L1                                     32
224200021025    2C     *IN32         IFEQ      *ON
224300960705     C* TROVATA BOLLA PARTENZA:
224400990409     C* CHIUDO ANOMALIA 55 O 56
224500960705     C                   MOVE      'N'           WTRM              1
224600960705     C                   CLEAR                   DSLR33
224700960705     C                   MOVE      ANMDAO        D33DCH
224800960705     C                   MOVEL     'M'           D33FSC
224900960705     C                   MOVEL     'AN'          D33CCH
225000960705     C                   MOVEL     'S'           D33MAC
225100990114     C   24
225200990114     COR 25              MOVE      ANMN10        D33NRR
225300000306     C   26              MOVE      ANMNR3        D33NRR
225400000306     C   27              MOVE      ANMNR7        D33NRR
225500960709     C   28              MOVE      ANMNR8        D33NRR
225600960709     C   29              MOVE      ANMNR9        D33NRR
225700960705     C                   CALL      'FNLR33R'
225800960705     C                   PARM                    DSLR33
225900960827     C* SE ANOMALIA 55:
226000960827    3C     ANMCAA        IFEQ      55
226100990409    3C     ANMCAA        OREQ      56
226200960705     C* CHIUDO ANOMALIA 200
226300960709     C                   MOVE      KCAA          WCAA
226400960705     C                   Z-ADD     200           KCAA
226500990512     C     KANM2         CHAIN     FNANM02L                           32
226600960705     C     *IN32         IFEQ      *OFF
226700960705     C                   CLEAR                   DSLR33
226800960705     C                   MOVE      AN2DAO        D33DCH
226900960705     C                   MOVEL     'AN'          D33CCH
227000960705     C                   MOVE      ANMNR2        D33NRR
227100960705     C                   CALL      'FNLR33R'
227200960705     C                   PARM                    DSLR33
227300960705     C                   END
227400960709     C                   MOVE      WCAA          KCAA
227500960827    3C                   END
227600960705    2C                   END
227700960705     C                   ENDSR
227800960705     C*
227900911021     C* STAMPO ANOMALIE ---------------------------------------------*
228000160908     C     STAMPA        BEGSR
228100151125     c                   setoff                                       434445
228200160908     c                   setoff                                       81
228300151125     c                   clear                   wdcm
228400960405     C* PER IDD
228500911025     C* SE DIVENTA 3 LA CHIUDO CON CAUSALE PR = PRATICA
228600960405    1C   97D34ELA        IFNE      ' '
228700960403    2C     ANMLID        IFEQ      §7ALCI
228800990512     C                   MOVEL     D34ALA        ANMDCH
228900911025     C                   MOVEL     'PR'          ANMCCH
229000921027     C                   MOVEL     'D'           ANMFSC
229100960327    2C                   ENDIF
229200960327    1C                   ENDIF
229300160908     c
229400160908     c* se si tratta di un manca record, verifico se bolla in partenza da partire
229500170119    1c****               if        anmcaa=010
229600170119     c* Per tutte le anomalie su cui è presente la key segnacollo,
229700170119     c* cerco la bolla per memorizzarla sull'anomalia
229800170119    1c                   if        anmscn>0
229900160908    2c                   if        anmnsp=0
230000160908     C     KART          CHAIN     FNBLT27L
230100160908    3c                   if        %found(fnblt27l)
230200160908     c                   z-add     bltnsp        anmnsp
230300160908     c                   z-add     bltlnp        anmlnp
230400160908     c                   z-add     bltaas        anmaas
230500160908    3c                   endif
230600160908    2c                   endif
230700160908     c
230800160908     c     kanm          chain     fnblp01l
230900160908    2c                   if        %found(fnblp01l) and blpft1=' '
231000160908     c                   seton                                        81
231100160908    2c                   endif
231200160908    2c                   if        not %found(fnblp01l)
231300160908     c                   clear                   anmnsp
231400160908     c                   clear                   anmlnp
231500160908     c                   clear                   anmaas
231600160908    2c                   endif
231700160908    1c                   endif
231800960514     C*
231900960514     C* SE IL SOGGETTO ANOMALIA E' "E" --> NON STAMPO L'ANOMALIA
232000960515     C*                                    SE IDD IN ARRIVO
232100040116    c
232200040116    3C     ANMSAN        IFNE      'E'
232300040116     C     wanom         ANDEQ     'A'
232400040116     C     ANMAIE        ORNE      'E'
232500040116     C     wanom         orne      'A'
232600911025     C*
232700911021     C* NUMERO SPEDIZIONE
232800911021     C     ANMNSP        COMP      0                                  0202
232900911021     C* NUMERO SEGNACOLLO
233000970314     C     ANMFLS        COMP      0                                  0303
233100960417     C**
233200960417     C* SE C'E' IL NUMERO DI SPEDIZIONE, VADO A PRENDERE LA DATA
233300960514    2C     ANMNSP        IFGT      0
233400960417     C                   SETOFF                                       33
233500960417     C                   CLEAR                   G02INV
233600960417     C*
233700960417     C     KANM          CHAIN     FNBLP01L                           33
233800960514    4C     *IN33         IFEQ      *OFF
233900960509     C                   MOVEL     BLPZNC        PRTZNC
234000960417     C                   Z-ADD     BLPMGS        G02INV
234100971126     C                   MOVEL     BLPAAS        G02INV
234200050503     c* Se c'e' particolarità consegna CP, la stampo
234300050503     c                   if        blpgma='CP'
234400050503     c                   seton                                        43
234500050503     c                   endif
234600151125     c* Se consegna anomala 7 stampo
234700151125     c                   if        blpcca='7' and blpdcm>0
234800151125     c                   seton                                        45
234900151125     c                   movel     blpdcm        wdcm              8 0
235000151125     c                   endif
235100151125     c
235200051221     c* Se richiesto stampo dati mittente e destinatario
235300051221     c                   if        d34med = 'S' and d34ela = ' '
235400051221     c                   seton                                        44
235500051221     c                   movel     blprsm        prtrsm           32
235600051222     c* rmn : lo stampo eliminando gli spazi a sinistra non significativi
235700051222     c                   move      blprmn        prtrmn           15
235800051222     c     ' 0'          check     prtrmn        N                 2 0
235900051222     c                   eval      prtrmn = %subst(prtrmn:N)
236000051221     c                   endif
236100050503     c
236200960514    4C                   ENDIF
236300960417     C*
236400961018     C                   CLEAR                   PRTDSP
236500960514    3C     G02INV        IFGT      0
236600960417     C                   MOVEL     3             G02ERR
236700971126     C                   CALL      'XSRDA8'
236800960417     C                   PARM                    WLBDAT
236900960417     C                   MOVEL     G02DAT        PRTDSP            4 0
237000960514    3C                   ENDIF
237100151125     c*
237200151125    3c                   if        wdcm>0  and *in45
237300151125     c                   eval      g02inv=wdcm
237400151125     C                   MOVEL     3             G02ERR
237500151125     C                   CALL      'XSRDA8'
237600151125     C                   PARM                    WLBDAT
237700151125     C                   MOVEL     G02DAT        PRTDcm            4 0
237800151125    3C                   ENDIF
237900151125    2C                   ENDIF
238000960508     C*
238100960508     C* PER L'ANOMALIA 005 VERIFICO SE IL COLLO E' ANDATO IN DISGUIDO
238200040116     C***  ANMAIE        IFNE      'R'
238300040116     C     ANMCAA        ifeq      005
238400960508     C                   EXSR      ANM005
238500960514    2C                   ENDIF
238600960927     C*
238700960508     C* SE HO TROVATO IL DISGUIDO LO STAMPO
238800960514    2C     *IN20         IFEQ      *ON
238900960508     C                   MOVE      WDDET         G02INV
239000960508     C                   MOVEL     3             G02ERR
239100971126     C                   CALL      'XSRDA8'
239200960508     C                   PARM                    WLBDAT
239300960508     C                   MOVEL     G02DAT        PRTDET            4 0
239400960514    2C                   ENDIF
239500911021     C* TIPO ANOMALIA
239600960514    2C     ANMTAN        IFNE      SAVTAN
239700911021     C                   MOVEL     '7F'          COD
239800960327     C                   MOVEL(P)  ANMTAN        KEY
239900960409     C     KTAB          CHAIN(N)  TABEL                              31
240000040616     C   31              MOVEL     *BLANKS       tbluni
240100040616     c* Vedo semettere o meno il tipo di anomalia (Idd/int/no idd)
240200040616     c                   if        *in17
240300040616     C                   MOVEL     TBLUNI        DESTAN           12
240400040616     c                   else
240500040616     c                   movel     tbluni        w008a             8
240600040616     c                   endif
240700921015     C*
240800921015     C                   MOVEL     ANMTAN        SAVTAN            1
240900960514    2C                   END
241000040616     c*
241100040616     c                   if        not *in17
241200040616     c                   clear                   destan
241300040616     C* ANOMALIA
241400040616    2C     ANMAIE        IFEQ      'I'
241500040616     C                   MOVEL     'INT'         DESAIE            3
241600040616   X2C                   ELSE
241700040616     C**
241800040616    3C     §7CIDD        IFEQ      'S'
241900040616     C                   MOVEL     'IDD'         DESAIE                         IDD
242000040616     C                   ELSE
242100040616     C                   MOVEL     'NON'         DESAIE                         NON IDD
242200040616    3C                   ENDIF
242300040616    2C                   ENDIF
242400040616     c                   movel     desaie        destan
242500040616     c                   move      w008a         destan
242600040616     c                   endif
242700960506     C*
242800960506     C* DESCRIZIONE CODICE ANOMALIA
242900960506     C                   MOVEL     §7CDES        DESCAA           25
243000960704     C*
243100960705     C* SE ANOMALIA 10-MANCA RECORD BOLLA ARRIVO O 138-COLLO GIA' CONS.
243200960705     C* CONTROLLO SE COLLO SPARATO IN INVENTARIO O IN CONSEGNA
243300960704     C                   SETOFF                                       21
243400960704     C     ANMCAA        IFEQ      10
243500960705     C     ANMCAA        OREQ      138
243600170519     C     ANMLNA        LOOKUP    L6_a                                   32
243700960704     C     *IN32         IFEQ      *ON
243800960704     C                   EXSR      RCRSPU
243900960704     C                   END
244000960704     C                   END
244100960402     C*
244200950320     C* SE BOLLA IN CONSEGNA REPERISCO NR/DATA DISTINTA E PADRONCINO
244300950320     C                   SETOFF                                       09
244400960514    2C     ANMCAA        IFEQ      135
244500950321     C     KART          CHAIN     FNART27L                           31
244600960514    3C     *IN31         IFEQ      *OFF
244700950320     C     KARB          CHAIN     FNARB01L                           31
244800960514    4C     *IN31         IFEQ      *OFF
244900960509     C                   MOVEL     ARBZNC        PRTZNC
245000960509     C*
245100960514    5C     ARBNDC        IFNE      0
245200950320     C                   SETON                                        09
245300950320     C*
245400950320     C                   MOVEL     DESCAA        DESCA1           18
245500950320     C                   MOVE      ':'           DESCA1
245600950320     C* Data distinta
245700950320     C                   MOVE      ARBDDC        G02INV
245800950320     C                   MOVEL     3             G02ERR
245900971126     C                   CALL      'XSRDA8'
246000950320     C                   PARM                    WLBDAT
246100950320     C                   MOVEL     G02DAT        STADDC            4 0
246200950320     C* Padroncino
246300950320     C                   MOVE      *BLANKS       STAPDC           12
246400021203     C*    ARBPDC        CHAIN     FNAPD01L                           32
246500021205     C                   move      arbpdc        kpdpdr
246600021203     C                   move      'A'           kpdtip
246700021203     C     kfapd         chain     fiapd01L                           32
246800960327     C  N32              MOVEL     APDRSC        STAPDC
246900960514    5C                   END
247000960514    4C                   END
247100960514    3C                   END
247200960514    2C                   END
247300990121     C**
247400990121     C** NUMERO CA SE C'E'
247500990122     C                   SETOFF                                       404142
247600990121     C     KART          CHAIN     FNDCD02L                           32
247700990121    2C     *IN32         IFEQ      *OFF
247800990121     C     DCDATB        ANDEQ     ' '
247900990121     C     KDCT          CHAIN     FNDCT01L                           32
248000050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
248100050221     c                   If        Not *In32 and dctdt2 > 0
248200050221     c                   Eval      *In32 = *On
248300050221     c                   EndIf
248400990121     C* CONTROLLO LA CAUSALE DI CHIUSURA SE C'E'
248500990121    3C  N32DCTDCH        IFGT      0
248600990121     C     DCTCCH        ANDNE     ' '
248700990121     C                   CLEAR                   DSBS02
248800990121     C                   MOVEL     'C'           T02MOD
248900990121     C                   MOVEL     KNSIF         T02SIF
249000990121     C                   MOVEL     'CCH'         T02COD
249100990121     C                   MOVEL     DCTCCH        T02KE1
249200990121     C                   CALL      'TIBS02R'
249300990121     C                   PARM                    KPJBA
249400990121     C                   PARM                    DSBS02
249500990121    4C     T02ERR        IFEQ      ' '
249600990121     C                   MOVEL     T02UNI        DCCH
249700990121     C                   ELSE
249800020905     ***C                   MOVEL     'S'           §CHCRA
249900020905     C                   MOVEL     'S'           §CCHCRAN
250000990121    4C                   ENDIF
250100990121    3C                   ENDIF
250200990121     C**
250300990121     C** STAMPO SE CHIUSA C.A. SENZA CAUSALE
250400990122    3C  N32DCTCCH        IFEQ      *BLANKS
250500990121     C     DCTDCH        ANDGT     0
250600990121     C** STAMPO SE CHIUSA C.A. CON CAUSALE PER CUI CREARE ANOMALIA
250700990121     C*  (SIGNIFICA CHE E' CA REALE)
250800020905     ***C     §CHCRA        OREQ      'S'
250900020905     C     §CCHCRAN      OREQ      'S'
251000990121     C     DCTDCH        ANDGT     0
251100990122     C* STAMPO SE C.A. APERTA E DETT C.A. APERTO
251200990122     C     DCTDCH        OREQ      0
251300990122     C     DCDDCH        ANDEQ     0
251400990122     C* DECODIFICA DANNI
251500990122     C                   EXSR      DECDAN
251600990122    3C                   ENDIF
251700990121    2C                   ENDIF
251800050503     c
251900050503     C* VERIFICO PER CP SE STAMPARE decodifica corta o lunga
252000050503     c                   clear                   decodicp         23
252100050503     c                   if        *in43
252200050503     c                   if        *in41
252300050503     c                   eval      decodicp=CCPcort
252400050503     c                   else
252500050503     c                   eval      decodicp=CCPlung
252600050503     c                   endif
252700050503     c                   endif
252800990122     C**
252900990122     C* SE NON E' SU C.A. PER LA 151 STAMPO LA 1 C.A. CHIUSA CHE TROVO
253000990122    2C     *IN32         IFEQ      *ON
253100990122     C     ANMNSP        ANDGT     0
253200990122     C     ANMCAA        ANDEQ     151
253300990122     C                   CLEAR                   WTRDAN
253400990122     C**
253500040811      * Sostituisco la lettura del file FNDTC02 con
253600040811      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul e chiuse
253700040811      * legate alla spedizione che passo
253800040811      *
253900040811     c                   clear                   fidn12ds
254000040811     c                   eval      i12aas = anmaas
254100040811     c                   eval      i12lnp = anmlnp
254200040811     c                   eval      i12nrs = anmnrs
254300040811     c                   eval      i12nsp = anmnsp
254400040811     c                   eval      i12fel = 'E'
254500040811     c                   eval      i12fan = 'E'
254600040811      *
254700040811     c                   call      'FIDN12R'
254800040811     c                   parm                    fidn12ds
254900040811      *
255000040811      * se non ci sono errori
255100040811     c                   if        o12err = ' '
255200040811      * se numero di CA trovate maggiore di zero
255300040811     c                   if        o12nca > 0
255400040811      * prendo solo la prima CA chiusa
255500040811     c                   z-add     *zeros        jj                2 0
255600040811     c     1             do        o12nca        jj
255700040811     c                   if        skdch(jj) > 0
255800040811     c                   movel     'S'           wtrdan            1
255900040811     c                   movea     skkey(jj)     dskey
256000040811     C                   Z-ADD     dsfil         dctfil
256100040811     C                   Z-ADD     dsnca         dctnca
256200040811     C                   Z-ADD     skdca(jj)     dctdca
256300040811     C                   Z-ADD     skdch(jj)     dctdch
256400040811     c                   z-add     o12nca        jj
256500040811     c                   endif
256600040811     C                   ENDDO
256700040811      *
256800040811     c                   endif
256900040811     c                   endif
257000040811     C*
257100990122     C** TROVATA LA C.A. CHIUSA: STAMPO I DATI
257200990122     C     WTRDAN        IFEQ      'S'
257300990122     C                   EXSR      DECDAN
257400990122    3C                   ENDIF
257500990122    2C                   ENDIF
257600911021     C* FASE APERTURA
257700960514    2C     ANMFSA        IFNE      SAVFSA
257800911021     C                   MOVEL     '7D'          COD
257900960327     C                   MOVEL(P)  ANMFSA        KEY
258000960409     C     KTAB          CHAIN(N)  TABEL                              32
258100040616     C  N32              MOVEL     TBLUNI        DESFSA            8
258200960327     C   32              MOVEL     *BLANKS       DESFSA
258300921015     C*
258400921015     C                   MOVEL     ANMFSA        SAVFSA            1
258500960514    2C                   END
258600911021     C* DATA APERTURA
258700971126     C                   CLEAR                   G02INV
258800911021     C                   MOVE      ANMDAO        G02INV
258900911018     C                   MOVEL     3             G02ERR
259000971126     C                   CALL      'XSRDA8'
259100911018     C                   PARM                    WLBDAT
259200950320     C                   MOVEL     G02DAT        STADAO            4 0
259300911021     C* FASE CHIUSURA
259400960514    2C     ANMFSC        IFNE      SAVFSC
259500911021     C                   MOVEL     '7D'          COD
259600960327     C                   MOVEL(P)  ANMFSC        KEY
259700960409     C     KTAB          CHAIN(N)  TABEL                              32
259800040616     C  N32              MOVEL     TBLUNI        DESFSC            8
259900960327     C   32              MOVEL     *BLANKS       DESFSC
260000921015     C*
260100921015     C                   MOVEL     ANMFSC        SAVFSC            1
260200960514    2C                   END
260300960506     C*
260400911021     C* DATA CHIUSURA
260500960514    2C     ANMDCH        IFGT      0
260600971126     C                   CLEAR                   G02INV
260700911021     C                   MOVE      ANMDCH        G02INV
260800911021     C                   MOVEL     3             G02ERR
260900971126     C                   CALL      'XSRDA8'
261000911021     C                   PARM                    WLBDAT
261100950320     C                   MOVEL     G02DAT        STADCH            4 0
261200960417     C                   ELSE
261300960417     C                   CLEAR                   STADCH
261400960514    2C                   ENDIF
261500911021     C* CAUSALE CHIUSURA
261600960514    2C     ANMCCH        IFNE      SAVCCH
261700911021     C                   MOVEL     '7E'          COD
261800960417     C                   MOVEL(P)  ANMCCH        KEY
261900960409     C     KTAB          CHAIN(N)  TABEL                              31
262000960417     C  N31              MOVEL     TBLUNI        DESCCH           12
262100911021     C   31              MOVEL     *BLANKS       DESCCH
262200921015     C*
262300921015     C                   MOVEL     ANMCCH        SAVCCH            2
262400960514    2C                   END
262500970523     C*
262600970523     C                   MOVE      ANMCCH        CCHPRT
262700040616     C* DATA/ora creazione anomalia
262800040616     c                   clear                   stadac
262900040616     c                   clear                   staorc
263000040616     c                   if        anmflo<>*blanks
263100040616     c                   movel     anmflo        danmflo
263200040616     C                   CLEAR                   G02INV
263300040616     C                   MOVE      §anmdac       G02INV
263400040616     C                   MOVEL     3             G02ERR
263500040616     C                   CALL      'XSRDA8'
263600040616     C                   PARM                    WLBDAT
263700040616     C                   MOVEL     G02DAT        STADAc            4 0
263800040616     c                   move      §anmorc       staorc            4 0
263900040616     c                   endif
264000970523     C*
264100960327     C**
264200960403     C* SOLO SE ANOMALIA ESTERNE IDD STAMPO IL LIVELLO IDD
264300960404     C   97              MOVEL     ANMLID        STALID            1
264400960404     C  N97              CLEAR                   STALID
264500960509     C**
264600960509     C* SE NON L'HO ANCORA TROVATA PRENDO LA ZONA DALLE SPUNTE
264700960514    2C     PRTZNC        IFEQ      '  '
264800070111     C     KART          SETLL     FNBRV07L
264900070111     C     KART          READE     FNBRV07L                               32
265000960514    3C     *IN32         DOWEQ     *OFF
265100960509     C     PRTZNC        ANDEQ     '  '
265200960514    4C     BRVZNC        IFGT      0
265300960509     C                   MOVEL     BRVZNC        PRTZNC
265400960509     C                   ELSE
265500070111     C     KART          READE     FNBRV07L                               32
265600960514    4C                   ENDIF
265700960514    3C                   ENDDO
265800960514    2C                   ENDIF
265900990512     C* ANNO A 2
266000990512     C   02              MOVE      ANMAAS        PRTAAS            2 0
266100911025     C*
266200900622     C* STAMPO DETTAGLIO
266300921015     C*
266400921015     C* STAMPA TESTATA
266500960514    2C     D34ELA        IFNE      ' '
266600960403     C     ANMLID        ANDLT     §7ALCI
266700960327     C     D34ELA        OREQ      ' '
266800960327     C*
266900040615     c                   if        *inOF
267000040615     C                   EXCEPT    TES
267100040615     C  n70              EXCEPT    TES2
267200040615     C                   EXCEPT    TES3
267300040615     C                   SETOFF                                       OF
267400040615     c                   endif
267500921015     C*
267600921015     C                   EXCEPT    DET
267700050503     c   41
267800050503     cor 43              except    det1
267900051221     c   44              except    det2
268000151125     c   45              except    det3
268100160908     c   81              except    det4
268200050503     c
268300031007     C   55              EXSR      WRIDET
268400960422     C                   MOVEL     '1'           WSTAMP            1
268500921015     C*
268600960514   X2C                   ELSE
268700921015     C*
268800921015     C   OE              EXCEPT    TESX
268900921015     C   OE              SETOFF                                       OE
269000921015     C*
269100911025     C                   EXCEPT    DETX
269200050503     c   41
269300050503     Cor 43              EXCEPT    DETX1
269400031007     C   55              EXSR      WRIDET
269500960514    2C                   ENDIF
269600900622     C*
269700040616     C**N17              SETOFF                                       04
269800040401     c* Scrivo file per report di sede
269900040401     c     wgir200       ifeq      'S'
270000040407     c     *in01         andeq     *on
270100040401     c                   exsr      srant
270200040401     c                   endif
270300960514    1C                   ENDIF
270400900622     C                   ENDSR
270500990122     C*
270600990122     C* DECODIFICA DATI RECORD C.A. ----------------------------------*
270700990122     C     DECDAN        BEGSR
270800990122     C* SE 09 20 O 21 STAMPO SU 2 RIGA ALTRIMENTI SULLA PRIMA
270900990122    1C     *IN09         IFEQ      *OFF
271000990122     C     *IN20         ANDEQ     *OFF
271100990122     C     *IN21         ANDEQ     *OFF
271200990122     C                   SETON                                        40
271300990122     C                   MOVEL     DESCAA        DESCA1           18
271400990122     C                   MOVE      ':'           DESCA1
271500990122     C                   ELSE
271600990122     C                   SETON                                        41
271700990122    1C                   ENDIF
271800990122     C**
271900990122     C* Data C.A.
272000990122     C                   MOVE      DCTDCA        G02INV
272100990122     C                   MOVEL     3             G02ERR
272200990122     C                   CALL      'XSRDA8'
272300990122     C                   PARM                    WLBDAT
272400990122     C                   MOVEL     G02DAT        STADCA            6 0
272500990122     C                   Z-ADD     G02DAT        W0020             2 0
272600990122     C                   MOVE      W0020         STADCA
272700990122     C* Data CHIUSURA C.A.
272800990122    1C     DCTDCH        IFGT      0
272900990122     C                   MOVE      DCTDCH        G02INV
273000990122     C                   MOVEL     3             G02ERR
273100990122     C                   CALL      'XSRDA8'
273200990122     C                   PARM                    WLBDAT
273300990122     C                   MOVEL     G02DAT        STACHA            4 0
273400990122     C                   SETON                                        42
273500990122     C**
273600990122    1C                   ENDIF
273700990122     C                   ENDSR
273800960508     C*
273900960508     C* CONTROLLO PER ANOMALIA 005 SE E' COLLO IN DISGUIDO ----------*
274000960508     C     ANM005        BEGSR
274100960508     C* PROVO A CHAINARE FNART
274200960508     C     KART          CHAIN     FNART27L                           32
274300960927    1C     *IN32         IFEQ      *OFF
274400960927     C     ARTDET        ANDGT     0
274500960508     C                   SETON                                        20
274600960508     C                   MOVEL     ARTFLP        WDFLP
274700960508     C                   MOVEL     ARTDET        WDDET
274800960807     C*
274900960927     C                   Z-ADD     1             KNPG
275000960927     C* CERCO LA SPUNTA CON FGS=ARTFLP E DATA= ARTDET
275100140618     C     KBRV3         SETLL     FNBRV05L
275200140618     C     KBRV3         READE     FNBRV05L                               32
275300960927    2C     *IN32         DOWEQ     *OFF
275400960509     C* ZONA
275500960927    3C     BRVZNC        IFGT      0
275600960509     C                   MOVEL     BRVZNC        PRTZNC
275700960927    3C                   ENDIF
275800960927     C*
275900000201     C                   Z-ADD     BRVNFV        WNFV
276000960927     C     KFVA          CHAIN     FNFVA01L                           32
276100960927    3C     *IN32         IFEQ      *OFF
276200040617     C     FVADFV        ANDEQ     ARTDUT
276300960927     C                   SETON                                        32
276400960927    4C     BRVATR        IFEQ      'T'
276500960927     C                   MOVEL     CTRANS        PSTAM
276600960927     C                   ELSE
276700960927     C                   MOVEL     CDISG         PSTAM
276800960927    4C                   ENDIF
276900960927   X3C                   ELSE
277000960927     C                   SETOFF                                       32
277100960927    3C                   ENDIF
277200960927     C**
277300140618     C  N32KBRV3         READE     FNBRV05L                               32
277400960927    2C                   ENDDO
277500960927     C**
277600960927     C* SE NON TROVATO METTO DISGUIDO
277700960927     C     PSTAM         IFEQ      *BLANKS
277800960927     C                   MOVEL     CDISG         PSTAM
277900960927     C                   ENDIF
278000960927    1C                   ENDIF
278100960508     C                   ENDSR
278200911021     C*
278300960704     C* RICERCA SPUNTA INVENTARIO O CONSEGNA PER ANOMALIA 10 --------*
278400960704     C     RCRSPU        BEGSR
278500960704     C                   CLEAR                   WNPG
278600960704     C                   CLEAR                   WSPG
278700960704     C                   CLEAR                   WDFV
278800960704     C                   CLEAR                   WFGS
278900960704     C* CERCO SPUNTA INVENTARIO E CONSEGNA E PRENDO QUELLA PIU' RECENTE
279000960704    1C                   DO        2             II                1 0
279100960704    2C     II            IFEQ      1
279200960704     C                   Z-ADD     3             KNPG
279300960704     C                   ELSE
279400960704     C                   Z-ADD     4             KNPG
279500960704    2C                   END
279600140618     C     KBRV          SETLL     FNBRV05L
279700140618     C     KBRV          READE     FNBRV05L                               32
279800960704    2C     *IN32         DOWEQ     *OFF
279900000201     C                   Z-ADD     BRVNFV        WNFV
280000960704     C     KFVV          CHAIN     FNFVV01L                           32
280100960704    3C     *IN32         IFEQ      *OFF
280200960704     C     FVVDFV        ANDGT     WDFV
280300960704     C                   MOVE      FVVNPG        WNPG
280400960704     C                   MOVE      FVVSPG        WSPG
280500960704     C                   MOVE      FVVDFV        WDFV
280600960704     C                   MOVE      FVVFGS        WFGS
280700960704    3C                   END
280800140618     C     KBRV          READE     FNBRV05L                               32
280900960704    2C                   ENDDO
281000960704    1C                   ENDDO
281100960704    1C     WDFV          IFGT      0
281200960704     C                   SETON                                        21
281300960704     C* ROVESCIO DATA SPUNTA
281400960704     C                   CLEAR                   WLBDAT
281500960704     C                   MOVE      WDFV          G02INV
281600960704     C                   MOVEL     3             G02ERR
281700971126     C                   CALL      'XSRDA8'
281800960704     C                   PARM                    WLBDAT
281900960704     C                   MOVEL     G02DAT        PRTDFV            4 0
282000960704     C* IMPOSTO CAMPO PER STAMPA DECODIFICA CATEGORIA/SOTTOCATEGORIA
282100960704    2C     WNPG          IFEQ      3
282200161003     C                   MOVEL     '7B'          COD
282300161003     C                   MOVEL(P)  wspg          KEY
282400161003     C     KTAB          CHAIN(N)  TABEL                              32
282500161003     c  n32              movel     tbluni        ds7b
282600161003     c   32              clear                   ds7b
282700161003     C                   MOVEL(P)  §7bdesb       PRTDOV            5
282800161003     C***                MOVEL     'IM'          W003A             3
282900161003     C***                MOVE      WSPG          W003A
283000960704     C                   ELSE
283100960704     C                   MOVEL(P)  'CONS.'       PRTDOV
283200960704    2C                   END
283300960704    1C                   END
283400960704     C                   ENDSR
283500911021     C* INVIO ANOMALIE ----------------------------------------------*
283600911021     C     ELAB          BEGSR
283700160907     C**FLAG TRASMISSIONE
283800160907    1C     ANMFL1        IFNE      0
283900160907     C                   MOVEL     'T'           ANMFT1
284000160907     C                   MOVE      AM8GIO        ANMDTR
284100160907    1C                   END
284200160907    1C     ANMFL2        IFNE      0
284300160907     C                   MOVEL     'T'           ANMFT2
284400160907     C                   MOVE      AM8GIO        ANMDTR
284500160907    1C                   END
284600161003     C* SI INVIO IDD
284700161003    1C     W7AIDD        IFEQ      'S'
284800970306     C** PER REPORT DI SEDE
284900970306    3C     ANMCAA        IFEQ      55
285000990409    3C     ANMCAA        OREQ      56
285100040504     c* la lnp disguido è quella della bolla, se c'e'
285200040504     c                   if        anmlnp>0
285300040504     c                   movel     anmlnp        wlnp
285400040504     c                   else
285500040504     C                   MOVEL     ANMFLS        wLNP
285600040504     c                   endif
285700040504     c
285800970306     C     KA55          CHAIN     FNANT02L                           39
285900970306     C*
286000970306    4C     *IN39         IFEQ      *ON
286100970306     C                   CLEAR                   FNANT000
286200970306     C                   MOVEL     ANMFLE        ANTFOA
286300040504     C****               MOVEL     ANMFLS        ANTLNP
286400040504     C                   MOVEL     wlnp          ANTLNP
286500990512     C                   MOVEL     ANMDAO        ANTDAO
286600970306     C                   MOVEL     ANMCAA        ANTCAA
286700970306     C                   Z-ADD     1             ANTTOT
286800990504     C*
286900990504     C                   CLEAR                   DSLV55
287000990504     C                   MOVEL     'P'           D55TPT
287100040504     C**                 MOVEL     anmfls        D55LIN
287200040504     C                   MOVEL     wlnp          D55LIN
287300990512     C                   MOVEL     ANMDAO        D55DRF
287400990504     C                   CALL      'FNLV55R'
287500990504     C                   PARM                    DSLV55
287600000317     C* PER LE ANOMALIE 55 E 56 ANTLNA CONTIENE IL TERMINAL DI PARTENZA
287700990504     C*  DELLE BOLLE
287800990504     C                   MOVEL     D55TFP        ANTLNA
287900040927     C                   Z-ADD     AM8GIO        antdt1
288000970306     C                   WRITE     FNANT000
288100970306   X4C                   ELSE
288200970306     C                   ADD       1             ANTTOT
288300040927     C                   Z-ADD     AM8GIO        antdt1
288400970306     C                   UPDATE    FNANT000
288500970306    4C                   ENDIF
288600970306    3C                   ENDIF
288700960523    1C                   ENDIF
288800911021     C*
288900960404     C* LE ANOMALIE 70 E 71 LE CHIUDO
289000960404     C* CON CAUSALE
289100000317    1C   01ANMCAA        IFEQ      70
289200960404     C     ANMCAA        OREQ      71
289300960523    2C     ANMDCH        IFEQ      0
289400990512     C                   MOVEL     D34ALA        ANMDCH
289500960404     C                   MOVEL     'D'           ANMFSC
289600970305     C*
289700970305    3C     ANMCAA        IFEQ      71
289800970306     C     KA71          CHAIN     FNANT02L                           39
289900970305     C*
290000970305    4C     *IN39         IFEQ      *ON
290100970305     C                   CLEAR                   FNANT000
290200970306     C                   MOVEL     ANMFLE        ANTFOA
290300970306     C                   MOVEL     ANMLNP        ANTLNP
290400990512     C                   MOVEL     ANMDAO        ANTDAO
290500970305     C                   MOVEL     ANMCAA        ANTCAA
290600970305     C                   Z-ADD     1             ANTTOT
290700040927     c                   z-add     am8gio        antdt1
290800970305     C                   WRITE     FNANT000
290900970305   X4C                   ELSE
291000970305     C                   ADD       1             ANTTOT
291100040927     c                   z-add     am8gio        antdt1
291200970305     C                   UPDATE    FNANT000
291300970305    4C                   ENDIF
291400970305    3C                   ENDIF
291500970305     C**
291600960523    2C                   ENDIF
291700960523    1C                   ENDIF
291800960404     C*
291900000317     C* SE NON CI SONO STATI ERRORI DI TRASMISSIONE UPDATO
292000160907     C***  WERR          IFNE      'S'
292100001102     C     D34GES        IFEQ      'V'
292200001102     C                   UPDATE    FNANM011
292300001102     C                   ELSE
292400990512     C                   UPDATE    FNANM000                                      AGGIORNO
292500001102     C                   ENDIF
292600160907     C***                ELSE
292700160907     C***  D34GES        IFEQ      'V'
292800160907     C***                UNLOCK    FNANM11L
292900160907     C***                ELSE
293000160907     C***                UNLOCK    FNANM00F
293100160907     C***                ENDIF
293200160907    1C***                ENDIF
293300911021     C                   ENDSR
293400040401     C* SCRITTURA FILE PER REPORT DI SEDE ---------------------------*
293500040401     C     SRANT         BEGSR
293600040401     C** PER REPORT DI SEDE
293700040504     c                   if        anmlnp>0
293800040504     c                   movel     anmlnp        wlnp
293900040504     c                   else
294000040504     C                   MOVEL     ANMFLS        wLNP
294100040504     c                   endif
294200040405     C     KA200         CHAIN     FNANT02L                           39
294300040401     C*
294400040401    4C     *IN39         IFEQ      *ON
294500040401     C                   CLEAR                   FNANT000
294600040401     C                   MOVEL     ANMFLE        ANTFOA
294700040504     C                   MOVEL     wlnp          ANTLNP
294800040405     C* Come data anomalia metto data idd per poter conteggiare le anomalie
294900040405     c* lette tutte nello stesso report controllo qualità
295000040405     C                   MOVEL     WDTA200       ANTDAO
295100040401     C                   MOVEL     ANMCAA        ANTCAA
295200040401     C                   Z-ADD     1             ANTTOT
295300040401     C*
295400040401     C                   CLEAR                   DSLV55
295500040401     C                   MOVEL     'P'           D55TPT
295600040401     C                   MOVEL     ANMFLe        D55LIN
295700040401     C                   MOVEL     ANMDAO        D55DRF
295800040401     C                   CALL      'FNLV55R'
295900040401     C                   PARM                    DSLV55
296000040401     C* PER l'anomalia 200 ANTLNA CONTIENE IL TERMINAL DI PARTENZA
296100040401     C*  del p.o. che ha generato il disguido
296200040401     C                   MOVEL     D55TFP        ANTLNA
296300040927     c                   z-add     am8gio        antdt1
296400040401     C                   WRITE     FNANT000
296500040401     C                   ELSE
296600040401     C                   ADD       1             ANTTOT
296700040927     c                   z-add     am8gio        antdt1
296800040401     C                   UPDATE    FNANT000
296900040401     C                   ENDIF
297000040401     c                   endsr
297100911021     C*
297200960404     C* CONTROLLO PER TRASMETTERE LA CHIUSURA ANOMALIE 70 E 71 ------*
297300960404     C     CHIUAN        BEGSR
297400960417     C** MANCA BAM
297500960417     C                   Z-ADD     71            KCAA
297600960404     C                   Z-ADD     1             L
297700960404     C*
297800170519    1C     L6_a(L)       DOWGT     0
297900170519     C                   Z-ADD     L6_a(L)       KFIL
298000990512     C     KANM5         SETLL     FNANM10L
298100990512     C     KANM5         READE     FNANM10L                               30
298200960417    2C     *IN30         DOWEQ     *OFF
298300960417     C                   EXSR      BAM
298400990512     C     KANM5         READE     FNANM10L                               30
298500960417    2C                   ENDDO
298600960404     C**
298700960404     C                   ADD       1             L
298800960417    1C                   ENDDO
298900960417     C** ECCEDE BAM
299000960417     C                   Z-ADD     70            KCAA
299100021021     C                   Z-ADD     1             L
299200170519    1C     L6_a(L)       DOWGT     0
299300170519     C                   Z-ADD     L6_a(L)       KFIL
299400021021     C     Kanm20        SETLL     FNANM20L
299500021021     C     Kanm20        READE     FNANM20L                               30
299600960417    2C     *IN30         DOWEQ     *OFF
299700960417     C                   EXSR      BAM
299800960417     C*
299900021021     C     Kanm20        READE     FNANM20L                               30
300000960417    2C                   ENDDO
300100021021     C**
300200021021     C                   ADD       1             L
300300021021    2C                   ENDDO
300400960404     C*
300500960404     C                   ENDSR
300600960417     C*
300700960417     C* CONTROLLO LE ANOMALIE 70 E 71 PER CHIUDERLE -----------------*
300800960417     C     BAM           BEGSR
300900960417     C                   MOVEL     ANMAAS        KAAS2
301000960417     C                   MOVEL     ANMLNP        KLNP2
301100960417     C                   MOVEL     ANMNRS        KNRS2
301200960417     C                   MOVEL     ANMNSP        KNSP2
301300960417     C*
301400960417     C* LEGGO IL COLLO FINTANTO CHE TROVO DELLE ANOMALIE INSERITE
301500990512     C     KANM1         SETLL     FNANM01L
301600990512     C     KANM1         READE     FNANM01L                               31
301700960417    4C     *IN31         DOWEQ     *OFF
301800990512     C     KANM1         READE     FNANM01L                               31
301900960417    4C                   ENDDO
302000960417     C*
302100960417     C* LETTA L'ULTIMA, VEDO CHE DATA CHIUSURA HA
302200960417    4C     ANMCCH        IFEQ      '  '
302300970424     C     ANMDCH        ANDGT     0
302400990512     C     ANMDCH        ANDLT     D34ALA
302500960417     C* RICHAINO CON NUMERO RELATIVO DI RECORD
302600990512     C     ANMNR1        CHAIN     FNANM00F                           31
302700960417     C* TRASMETTO IN PARTENZA LA CHIUSURA DELLA ANOMALIA
302800960417     C                   MOVEL     'OB'          ANMCCH
302900990512     C                   MOVEL     D34ALA        ANMDCH
303000960417     C                   EXSR      ELAB
303100960417    4C                   ENDIF
303200960417     C                   ENDSR
303300960422     C*
303400960423     C* STAMPA DI CHI NON HA FATTO L'IDD ----------------------------*
303500960423     C     NOIDD         BEGSR
303600960422     C                   SETON                                        08
303700040615     C                   SETOff                                       09
303800960423     C                   CLEAR                   WMANID
303900960423     C                   Z-ADD     95            KCAA
304000960422     C* SE IMMESSA LA DATA AL CERCO NELLA DATA AL
304100960422     C*  ALTRIMENTI CERCO LA DATA IDD PIU' ALTA CHE HO
304200990512    1C     D34ALA        IFEQ      0
304300990512     C     D34ACA        ANDEQ     0
304400990512     C     KCAA          SETGT     FNANM04L
304500990512     C     KCAA          READPE    FNANM04L                               30
304600960422    2C     *IN30         DOWEQ     *OFF
304700050504     C**   ANMAIE        ANDEQ     'E'
304800050504    2C**   *IN30         OREQ      *OFF
304900990512     C     ANMDAO        ANDGE     AM8GIO
305000990512     C     KCAA          READPE    FNANM04L                               30
305100960422    2C                   ENDDO
305200960422     C*
305300971126     C     *IN30         IFEQ      *OFF
305400971126     C                   Z-ADD     ANMDAO        WDATA8            8 0
305500971126     C                   ENDIF
305600971126     C**
305700960422   X1C                   ELSE
305800971126     C     D34ALA        IFGT      0
305900971126     C                   MOVEL     D34ALA        WDATA8
306000961016     C                   ELSE
306100971126     C                   MOVEL     D34ACA        WDATA8
306200961016     C                   ENDIF
306300961016     C*
306400960422     C                   SETOFF                                       30
306500960422    1C                   ENDIF
306600960510     C*
306700960926     C* PER I P.O. CHE FANNO L'IDD IL GIORNO DOPO
306800960510     C*  CERCO LA DATA PRECEDENTE
306900960510     C  N30              EXSR      RICDAT
307000990512     C  N30              MOVEL     GI8INV        WDATAM            8 0
307100960510     C*
307200960422     C* 30 OFF - TROVATA
307300960422    1C     *IN30         IFEQ      *OFF
307400960422     C                   Z-ADD     0             L
307500990518     C* GIRO LA DATA ELABORAZIONE DEL GIORNO
307600971126     C                   MOVE      WDATA8        G02INV
307700960422     C                   MOVEL     '3'           G02ERR
307800971126     C                   CALL      'XSRDA8'
307900960422     C                   PARM                    WLBDAT
308000990513     C                   MOVEL     G02DAT        STADCI            8 0
308100960510     C*
308200971126     C                   CLEAR                   G02INV
308300960510     C                   MOVE      WDATAM        G02INV
308400960510     C                   MOVEL     '3'           G02ERR
308500971126     C                   CALL      'XSRDA8'
308600960510     C                   PARM                    WLBDAT
308700990513     C                   MOVEL     G02DAT        STADCM            8 0
308800960422     C*
308900960422     C     *LOVAL        SETLL     AZORG01L
309000960423     C                   READ      AZORG01L                               30
309100960422     C*
309200960422    2C     *IN30         DOWEQ     *OFF
309300960423    C**
309400960510     C* ESCLUDO LE ANNULLATE
309500960422    3C     ORGFVA        IFEQ      ' '
309600960423     C**
309700960422     C* SOLO FILIALE E AGENZIA
309800960422    4C     ORGFAG        IFEQ      'F'
309900960422     C     ORGFAG        OREQ      'A'
310000960423     C**
310100960926     C* ESCLUDO I P.O. CHE NON DEVONO FARE IDD
310200960422     C                   MOVEL     ORGDE8        OG148
310300050831     C                   MOVEL     ORGDE3        OG143
310400960422    5C     §OGNID        IFEQ      ' '
310500971124     C* CERCO IL TERMINAL DI PARTENZA
310600971124     C* SE STO ELABORANDO L'IDD E NON SONO IL TERMINAL DI PARTENZA
310700971124   5AC     D34ELA        IFNE      ' '
310800971124     C     ORGFIL        ANDNE     D34FGS
310900971124     C**
311000971124     C                   CLEAR                   DSLV55
311100971124     C                   MOVEL     'P'           D55TPT
311200971124     C                   MOVEL     ORGFIL        D55LIN
311300980403     C                   Z-ADD     AM8GIO        D55DRF
311400971124     C                   CALL      'FNLV55R'
311500971124     C                   PARM                    DSLV55
311600971124   5BC     D55ERR        IFNE      ' '
311700971124     C                   Z-ADD     SIMFEL        D55TFP
311800971124   5BC                   ENDIF
311900971124   5AC                   ENDIF
312000960510     C**
312100960509     C* SE ANOMALIE IDD --> ESCLUDO SE NON SONO DELL'AREA
312200960423   5AC     D34ELA        IFEQ      ' '
312300971124     C     ORGFIL        ORNE      D34FGS
312400971124     C     D55TFP        ANDEQ     D34FGS
312500960422     C*
312600960422     C                   MOVEL     ORGFIL        KLNA
312700960510     C* IMPOSTO LA DATA
312800960510     C     §OGEID        IFEQ      ' '
312900971126     C                   Z-ADD     WDATA8        KDAO                           DEL GIORNO
313000960510     C                   ELSE
313100960510     C                   Z-ADD     WDATAM        KDAO                           GIORNO PREC
313200960510     C                   ENDIF
313300960510     C*
313400990512     C     KANM4         SETLL     FNANM04L                               31
313500960422    6C     *IN31         IFEQ      *OFF
313600960423     C                   MOVEL     '1'           WMANID            1
313700960422     C*
313800960422     C* NON TROVATA STAMPO ERRORE
313900960422    7C     L             IFEQ      7
314000960422     C                   EXSR      ERRIDD
314100960422    7C                   ENDIF
314200050830     c
314300050830     C                   ADD       1             L
314400050830     C                   Z-ADD     ORGFIL        FID(L)
314500050831     C                   MOVEL     ORGDES        DFI(L)
314600050831     c                   clear                   dslv55
314700050830     c* Verifico se il £6
314800050830     c                   if        §ogntw<>'EEX' and §ogntw<>'DPD' and
314900050830     c                             §ogntw<>'FED' and §ogntw<>'EUP'
315000050830     C                   MOVEL     '6'           D55TPT
315100050831     c                   else
315200050831     c                   movel     'P'           D55tpt
315300050831     c                   endif
315400050830     C                   MOVEL     ORGFIL        D55LIN
315500050830     C                   Z-ADD     kdao          D55DRF
315600050830     C                   CALL      'FNLV55R'
315700050830     C                   PARM                    DSLV55
315800960422     C**
315900050831     c                   if        §ogntw<>'EEX' and §ogntw<>'DPD' and
316000050906     c                             §ogntw<>'FED' and §ogntw<>'EUP' and
316100050906     c                             orgfil<>101
316200050906     c
316300050830     c                   if        d55tfa=orgfil
316400050831     c* Imposto la decodifica del p.o. se non è in gestione ad altri
316500050831     C***                MOVEL     ORGDES        DFI(L)
316600050830     c                   else
316700050831     c* Imposto il p.o. gestione
316800050831     c****               eval      DFI(L)=CGestito
316900050831     c                   movel     d55tfa        w003a             3
317000050831     c                   eval      %subst(DFI(L):8:12)=CGESTITO
317100050831     c                   eval      %subst(DFI(L):20:3)=w003a
317200050830     c                   endif
317300050830     c
317400050830     c                   else
317500050906     c* Imposto il terminal di partenza del P.o estero o della 101
317600050906     c*  come gestione
317700050831     c***                eval      DFI(L)=CGestito
317800050831     c                   movel     d55tfp        w003a             3
317900050831     c                   eval      %subst(DFI(L):8:12)=CGESTITO
318000050831     c                   eval      %subst(DFI(L):20:3)=w003a
318100050830     c                   endif
318200960422     C*
318300960422    6C                   ENDIF
318400960422     C*
318500960423   5AC                   ENDIF
318600960423    5C                   ENDIF
318700960422    4C                   ENDIF
318800960422    3C                   ENDIF
318900960422     C*
319000960423     C                   READ      AZORG01L                               30
319100960422    2C                   ENDDO
319200960422     C*
319300960422     C* SE NE RIMANGONO FINISCO DI STAMPARE
319400960423    2C     L             IFGT      0
319500960422     C                   EXSR      ERRIDD
319600960423   X2C                   ELSE
319700960423     C* SE CI SONO TUTTI LO DICO
319800960423    3C     WMANID        IFEQ      ' '
319900960423     C*
320000960423    4C     *INOF         IFEQ      *ON
320100960423     C     *IN09         OREQ      *OFF
320200040615     c                   if        *inof
320300040615     C                   EXCEPT    TES
320400040615     C  n70              EXCEPT    TES2
320500040615     c                   endif
320600960423     C*
320700960423    5C     D34ELA        IFEQ      ' '
320800960423     C                   EXCEPT    TESIDR
320900960423     C                   ELSE
321000960423     C                   EXCEPT    TESIDE
321100960423    5C                   ENDIF
321200960423     C                   SETON                                        09
321300960423     C                   SETOFF                                       OF
321400960423    4C                   ENDIF
321500960423     C**
321600960423     C* TUTTI HANNO ESEGUITO L'IDD
321700960423     C                   EXCEPT    SIIDD
321800960423    3C                   ENDIF
321900960423     C*
322000960423    2C                   ENDIF
322100960422     C*
322200960422    1C                   ENDIF
322300960422     C*
322400960422     C                   SETOFF                                       08
322500960422     C                   ENDSR
322600960510     C*
322700960926     C* RICERCA DATA IDD-1 X I P.O. CHE LO FANNO IL GG DOPO----------*
322800990512     C     RICDAT        BEGSR
322900960510     C* UTILIZZO IL CALENDARIO
323000971126     C                   MOVE      WDATA8        G02INV
323100960510     C                   MOVEL     '3'           G02ERR
323200971126     C                   CALL      'XSRDA8'
323300960510     C                   PARM                    WLBDAT
323400960510     C*
323500960510     C                   MOVEL     'S'           WFEST             1
323600960510     C                   CLEAR                   KTFP
323700960510     C* CERCO IL PRIMO GIORNO LAVORATIVO PRECEDENTE
323800960510     C     WFEST         DOWEQ     'S'
323900960510     C                   SUB       1             G02TGI
324000990512     C                   Z-ADD     G02TGI        GI8TGI
324100990518     C                   CALL      'XSRGI8'
324200960510     C                   PARM                    WGIDAT
324300990512     C                   Z-ADD     GI8INV        WDATAM
324400960510     C*
324500960510     C                   CLEAR                   WFEST
324600960510     C     KCLN          CHAIN     AZCLN01L                           31
324700960510    1C     *IN31         IFEQ      *OFF
324800960510     C     POM(WGG)      ANDNE     *BLANKS
324900960510     C                   MOVEL     '1Y'          COD
325000960510     C                   MOVEL(P)  POM(WGG)      KEY
325100960510     C     KTAB          CHAIN     TABEL                              31
325200960510    2C     *IN31         IFEQ      *OFF
325300960510     C                   MOVEL     TBLUNI        DS1Y
325400960510    3C     §1YFEI        IFEQ      'S'
325500960510     C                   MOVEL     'S'           WFEST
325600960510    3C                   ENDIF
325700960510    2C                   ENDIF
325800960510    1C                   ENDIF
325900960510     C                   ENDDO
326000960510     C                   ENDSR
326100960422     C*
326200960422     C* STAMPA ERRORE NO IDD ----------------------------------------*
326300960422     C     ERRIDD        BEGSR
326400960422     C* SE OF ACCESO LA PRIMA VOLTA STAMPO LA TESTATA COMPLETA
326500960422    1C     *INOF         IFEQ      *ON
326600960423     C     *IN09         OREQ      *OFF
326700040615     c                   if        *inOF
326800040615     C                   EXCEPT    TES
326900040615     C  n70              EXCEPT    TES2
327000040615     c                   endif
327100960423     C*
327200960423     C     D34ELA        IFEQ      ' '
327300960423     C                   EXCEPT    TESIDR
327400960423     C                   ELSE
327500960423     C                   EXCEPT    TESIDE
327600960423     C                   ENDIF
327700960423     C                   SETON                                        09
327800960423     C                   SETOFF                                       OF
327900960422    1C                   ENDIF
328000960422     C*
328100960422     C                   EXCEPT    DETIDD
328200960422     C*
328300960422     C                   CLEAR                   FID
328400960422     C                   CLEAR                   DFI
328500960422     C                   CLEAR                   L
328600960422     C*
328700960422     C                   ENDSR
328800960404     C*
328900911021     C* INVIO MEMBRI TRASMISSIONE -----------------------------------*
329000911021     C     CAEX          BEGSR
329100050804     C* CREO ANOMALIA OK IDD SUL SISTEMA per tutti i p.o. della £6
329200000317     C     D34ELA        IFEQ      'S'
329300060619     c* apro il file anomalie di sede per scriverci l'anomalia 95
329400160907     c***                EXSR      opensed
329500060619     c
329600050804     C                   Z-ADD     1             LL                3 0
329700170519    2C     L6_a(LL)      DOWGT     0
329800050804     c
329900170519     C                   Z-ADD     L6_a(LL)      anmlna
330000170519     C***                Z-ADD     FGSTFP        D55TFP
330100170519     C                   Z-ADD     FGStfa        D55TFP
330200911021     C                   Z-ADD     95            ANMCAA
330300960404     C                   EXSR      CRTANM
330400990512     C                   WRITE     FNANM000
330500050804     c
330600050804     c                   add       1             LL
330700050804     c                   enddo
330800050804     c
330900060711    1C                   ENDIF
331000060711     c
331100060711     C* Trasmissione membri  solo se chi esegue è il terminal o se
331200060711     c*  un 2 livello che lo fa dopo il terminal
331300160907     c**
331400160907    0C**   D34FGS        IFEQ      SIMFEL
331500160907     C**   D34ALA        ORLE      FELDID
331600160907     C**
331700160907     C**   *LOVAL        SETLL     AZORG
331800160907     C**                 READ      AZORG                                  30
331900160907     C**
332000160907    1C**   *IN30         DOWEQ     *OFF
332100160907    2C**   ORGFAG        IFEQ      'F'
332200160907     C**   ORGFAG        OREQ      'A'
332300160907    3C**   ORGFVA        IFEQ      ' '
332400160907     C**PRENDO IL TERMINAL DI PARTENZA
332500160907     C**                 CLEAR                   DSLV55
332600160907     C**                 MOVEL     'P'           D55TPT
332700160907     C**                 MOVEL     ORGFIL        D55LIN
332800160907     C**                 MOVEL     AM8GIO        D55DRF
332900160907     C**                 CALL      'FNLV55R'
333000160907     C**                 PARM                    DSLV55
333100160907     C**SE NON TROVATO TERMINAL DI PARTENZA NON INVIO OK IDD
333200160907     C**(P.O. NON ANCORA OPERATIVO MA GIA' PRESENTE IN ORGANIGRAMMA)
333300160907    4C**   D55ERR        IFEQ      *BLANKS
333400160907    4C**   ORGFIL        ANDEQ     D55TFP
333500160907     C**   ORGFIL        ANDNE     SIMFEL
333600160907     c**
333700160907    4C**   D55ERR        orEQ      *BLANKS
333800160907     C**   ORGFIL        ANDEQ     D55TFP
333900160907     C**   ORGFIL        ANDeq     SIMFEL
334000160907     c**   d34ges        andeq     'V'
334100160907     c**
334200160907     C**LA SEDE NON HA TERMINAL MA BISOGNA INVIARLA LO STESSO
334300160907     c**La sede non la invio più scrivo subito fnanm00f di sede
334400160907     C**SOLO PER FILIALI O AGENZIE
334500160907     C** 99              CLOSE     FNANM00T
334600160907     C** 99              SETOFF                                       99
334700160907     C**DISALLOCAZIONE
334800160907     c**                 eval      D60po0 = SIMfel
334900160907     C**                 MOVEL     SAVFL1        D60FL1
335000160907     C**                 MOVEL     SAVFL2        D60FL2
335100160907     C**                 MOVEL     AM8GIO        D60DRF
335200160907     C**                 MOVEL     'D'           D60AZN
335300160907     C**                 MOVEL     'FNANM00T'    D60FIL
335400160907     C**                 MOVEL     FNLV60DS      KPJBU
335500160907     C**                 CALL      'FNLV60R'
335600160907     C**                 PARM                    KPJBA
335700160907     C**                 CLEAR                   SAVFL1
335800160907     C**                 CLEAR                   SAVFL2
335900160907     C** ALLOCAZIONE
336000160907     c**                 eval      D60po0 = SIMfel
336100160907     c**                 reset                   DSmembro
336200160907     c**                 eval      MbrPOP = SIMfel
336300160907     c**                 eval      MbrPOA = D55tfp
336400160907     c**                 movel     DSmembro      VAR
336500160907     C**                 CLEAR                   D60FL2
336600160907     C**                 MOVEL     D55TFP        D60FL1
336700160907     C**                 MOVEL     'X'           D60AZN
336800160907     C**                 MOVEL     AM8GIO        D60DRF
336900160907     C**                 MOVEL(P)  '*EXCL  '     D60VNC
337000160907     C**                 MOVEL     FNLV60DS      KPJBU
337100160907     C**                 CALL      'FNLV60R'
337200160907     C**                 PARM                    KPJBA
337300160907     C**                 MOVEL     KPJBU         FNLV60DS
337400160907     C**
337500160907     C**NON TROVATI ERRORI DI ALLOCAZIONE
337600160907    5C**   D60ERR        IFEQ      'N'
337700160907     C**                 MOVEL     D55TFP        SAVFL1
337800160907     c**                 movel     D60mb1        DSmembro
337900160907     c**                 movel     DSmembro      VAR
338000160907     c**                 movel     D60mb1        wMBR
338100160907     C**
338200160907     C**PRENDO NUMERO RECORD
338300160907     C**                 MOVEL     'FNANM00T'    PGM              10
338400160907     C**                 EXSR      NUMREC
338500160907     C**
338600060711     C*
338700160907    6C**   NRR           IFGT      0
338800060711     c* controllo spostato sopra
338900060711    7C***  D34FGS        IFEQ      SIMFEL
339000060711     C***  D34ALA        ORLE      FELDID
339100160907     C**                 MOVEL     ORGFEL        §DST              3
339200160907     C**
339300160907     C**                 CLEAR                   TIBS55DS
339400160907     C**                 MOVEL     SIMFEL        I50PPO
339500160907     C**                 MOVEL     ORGFIL        I50APO
339600160907     C**                 CALL      'TIBS55R'
339700160907     C**                 PARM                    TIBS55DS
339800160907     C**
339900160907     C**                 MOVEL     O50PLA        CMZLBD
340000160907     C**                 MOVEL     O50ALA        CMZLBA
340100160907     C**                 MOVEL     O50ASI        CMZSYF
340200160907     C**
340300160907     C**                 EXSR      X25ANM
340400160907    7C****               ENDIF
340500160907    6C**                 ENDIF
340600160907    5C**                 ENDIF
340700160907    4C**                 ENDIF
340800160907    3C**                 END
340900160907    2C**                 END
341000911021     C*
341100160907     C**                 READ      AZORG                                  30
341200160907    1C**                 ENDDO
341300160907    0C**                 ENDif
341400911021     C                   ENDSR
341500060619     C*
341600060619     C* open file anomalie di sede ----------------------------------*
341700160907     C**   OPENSED       BEGSR
341800160907     c**
341900160907     C**                 MOVEL     *BLANKS       COMMAN           80
342000160907     C**                 MOVEL     OVRSED(1)     COMMAN
342100160907     C**                 CALL      'QCMDEXC'                            98
342200160907     C**                 PARM                    COMMAN
342300160907     C**                 PARM                    LUNG
342400160907     C**                 OPEN(E)   FNANMSED
342500160907     c**provo in gaitragrps
342600160907     C**                 if        %error
342700160907     c**                 eval      %subst(comman:38:11)='PS/FNANM00F'
342800160907     C**                 CALL      'QCMDEXC'                            98
342900160907     C**                 PARM                    COMMAN
343000160907     C**                 PARM                    LUNG
343100160907     C**                 OPEN(E)   FNANMSED
343200160907     c**                 endif
343300160907     c**
343400160907     C**                 ENDSR
343500911021     C*
343600911021     C* CREO ANOMALIE ESEGUITO IDD ----------------------------------*
343700911021     C     CRTANM        BEGSR
343800911021     C                   MOVEL     *BLANKS       ANMATB
343900950922     C                   CLEAR                   ANMCDU
344000911021     C                   Z-ADD     0             ANMFLS
344100911021     C                   Z-ADD     0             ANMNRS
344200911021     C                   Z-ADD     0             ANMSCN
344300911021     C                   Z-ADD     0             ANMAAS
344400960404     C                   Z-ADD     0             ANMLNP
344500911021     C                   Z-ADD     0             ANMNSP
344600911021     C                   MOVEL     '7C'          COD
344700960404     C                   MOVEL(P)  ANMCAA        KEY
344800960409     C     KTAB          CHAIN(N)  TABEL                              31
344900911021     C  N31              MOVEL     TBLUNI        DS7C
345000911021     C   31              MOVEL     *BLANKS       DS7C
345100911021     C                   MOVEL     §7CTAN        ANMTAN
345200911021     C                   MOVEL     §7CSAN        ANMSAN
345300921027     C                   MOVEL     'D'           ANMFSA
345400990512     C                   MOVEL     D34ALA        ANMDAO
345500911021     C                   MOVEL     §7CAIE        ANMAIE
345600911025     C                   MOVEL     ' '           ANMFT1
345700911025     C                   MOVEL     *ZEROS        ANMFL1
345800000824     C                   MOVEL     *ZEROS        ANMDTR
345900911021     C* CHIUSURA AUTOMATICA
346000911021     C     §7CCHA        IFEQ      'S'
346100921027     C                   MOVEL     'D'           ANMFSC
346200990512     C                   MOVEL     D34ALA        ANMDCH
346300911021     C                   END
346400960509     C* DA INVIARE
346500911021     C     §7CAIE        IFEQ      'E'
346600971124     C                   MOVEL     D55TFP        ANMFL1
346700911025     C                   MOVEL     'T'           ANMFT1
346800000824     C                   MOVE      AM8GIO        ANMDTR
346900911021     C                   END
347000911021     C                   Z-ADD     0             ANMFL2
347100911021     C                   MOVEL     *BLANKS       ANMFT2
347200911021     C                   MOVEL     *BLANKS       ANMCCH
347300911021     C                   Z-ADD     0             ANMNPS
347400911021     C                   Z-ADD     0             ANMNFV
347500911021     C                   MOVEL     SIMFEL        ANMFLE
347600911021     C                   MOVEL     §7CLID        ANMLID
347700990512     C                   CLEAR                   ANMDUE
347800990512     C                   CLEAR                   ANMNPG
347900990512     C                   CLEAR                   ANMFGS
348000990512     C                   CLEAR                   ANMFLO
348100040601     c* data ora di creazione
348200040601     c                   z-add     *date         wdata             6 0
348300040601     c                   time                    wora              6 0
348400040607     c                   movel     wora          wora4             4 0
348500040601     c                   movel     wdata         anmflo
348600040607     c                   move      wora4         anmflo
348700040601     c
348800990512     C                   CLEAR                   ANMFT3
348900990512     C                   CLEAR                   ANMFL3
349000990512     C                   CLEAR                   ANMFT4
349100040601     C                   z-add     999           ANMFL4
349200911021     C*
349300911021     C                   ENDSR
349400911021     C*
349500990512     C** SOTTOMETTE TRASMISSIONE FNANM00T ---------------------------*
349600160907     C**   X25ANM        BEGSR
349700160907     c**
349800160907     C**   X25ANM        MOVER     *BLANK        CMZPRR                          PRE-REQ.
349900160907     C**   X25ANM        MOVER     'S'           §CLR              1             CLEAR
350000160907     C**   X25ANM        MOVEL     §DST          CMZDST                          DESTINAT.
350100160907     C**   X25ANM        MOVEL     'FNANM00T'    CMZFLD                          DA: >>>
350200160907     C**   X25ANM        MOVEL     *BLANK        CMZMBD
350300160907     C**   X25ANM        MOVEL     wMBR          CMZMBD
350400160907     C**   X25ANM        MOVEL     'FNANM00R'    CMZFLA                          A: >>>
350500160907     C**   X25ANM        MOVEL     *BLANK        CMZMBA
350600160907     C**   X25ANM        MOVEL     wMBR          CMZMBA
350700160907     C**   X25ANM        MOVER     '0000'        CMZORI                          ORA
350800160907     C**   X25ANM        MOVER     '0000000'     CMZDTI                          DATA
350900040615     C**   D34TRA        IFEQ      'S'
351000040615     C**                 MOVE      'T'           CMZURG
351100040615     C**                 ELSE
351200160907     C**   X25ANM        MOVER     'N'           CMZURG                          ATTESA
351300040615     C**                 END
351400160907     C**   X25ANM        MOVER     'A'           CMZADD                          IN ADD
351500160907     C**   X25ANM        MOVER     §CLR          CMZCLR                          CLEAR
351600160907     C**   X25ANM        MOVER     'S'           CMZFTR                          DA TRANS.
351700160907     C**   X25ANM        MOVER     *BLANK        CMZERR                          1= ERR.
351800160907     C**   D34TRA        IFEQR     'S'
351900160907     C**   X25ANM        MOVER     'FF20'        CMZAZN                          AZIONE
352000040615     C**                 ELSE
352100160907     C**   X25ANM        MOVER     'LR16'        CMZAZN                          AZIONE
352200040615     C**                 END
352300160907     C**   X25ANM        MOVER     NRR           CMZNRR                          N.REC.
352400160907     C**   X25ANM        CALLR     'TIBS53C'
352500160907     C**   X25ANM        PARMR                   O50LCL
352600160907     C**   X25ANM        PARMR                   CMZDST
352700160907     C**   X25ANM        PARMR                   CMZFLD
352800160907     C**   X25ANM        PARMR                   CMZMBD
352900160907     C**   X25ANM        PARMR                   CMZLBD
353000160907     C**   X25ANM        PARMR                   CMZFLA
353100160907     C**   X25ANM        PARMR                   CMZMBA
353200160907     C**   X25ANM        PARMR                   CMZLBA
353300160907     C**   X25ANM        PARMR                   CMZORI
353400160907     C**   X25ANM        PARMR                   CMZDTI
353500160907     C**                 PARM                    CMZURG
353600160907     C**                 PARM                    CMZADD
353700160907     C**                 PARM                    CMZCLR
353800160907     C**                 PARM                    CMZFTR
353900160907     C**                 PARM                    CMZERR
354000160907     C**                 PARM                    CMZAZN
354100160907     C**                 PARM                    CMZSYF
354200160907     C**                 PARM                    CMZNRR
354300160907     C**                 PARM                    CMZPRR
354400160907     C**                 PARM                    W236            236
354500160907     C**                 ENDSR
354600911021     C*
354700911021     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE --------------------*
354800160907     C**   NUMREC        BEGSR
354900160907     c**                 reset                   DSmembro
355000160907     c**                 eval      MbrPOP = SIMfel
355100160907     c**                 eval      MbrPOA = D55tfp
355200160907     c**                 movel     DSmembro      ALF10
355300160907     C**                 Z-ADD     0             NRR              10 0
355400160907     C**                 CALL      'TRUL51C'
355500160907     C**                 PARM                    KNSIF
355600160907     C**                 PARM                    PGM
355700160907     C**                 PARM                    ALF10            10
355800160907     C**                 PARM                    NRR
355900160907     C**                 ENDSR
356000001030     C*-------------------------------------------------------------------
356100001030     C* TRASMISSIONE SCHEDULATA ANOMALIE (ESTERNE NON TRASMESSE)
356200001030     C     SCHANM        BEGSR
356300001102     C* N.B. LEGGO SOLO LE ANOMALIE ESTERNE VISTO CHE PER IL MOMENTO
356400001102     C*      LE AZIONI CHE PREVEDONO L'INVIO SCHEDULATO SONO TUTTE
356500001102     C*      ANOMALIE ESTERNE
356600001102     C                   MOVEL     'E'           KAIE
356700001030     C                   MOVEL     '7C'          COD
356800001030     C     KTAB2         SETLL     TABEL
356900001030     C     KTAB2         READE     TABEL                                  30
357000001030     C     *IN30         DOWEQ     *OFF
357100001030     C                   MOVEL     TBLUNI        DS7C
357200001030     C     §7CINV        IFEQ      'S'
357300001102     C                   MOVEL     TBLKEY        KCAA
357400001030     C     KAN11         SETLL     FNANM11L
357500001030     C     KAN11         READE     FNANM11L                               30
357600001030     C     *IN30         DOWEQ     *OFF
357700001030     C                   EXSR      ELAB
357800001030     C     KAN11         READE     FNANM11L                               30
357900001030     C                   ENDDO
358000001030     C                   ENDIF
358100001030     C     KTAB2         READE     TABEL                                  30
358200001030     C                   ENDDO
358300001030     C                   EXSR      CAEX
358400041020     c
358500041020     c* Leggo le anomalie 145 146 per le quali devo scrivere fnant
358600041020     c* sbr   spostata dal pgm fnlr76r che viene eliminato
358700041020     C                   MOVEL     'I'           KAIE
358800041020     c                   z-add     145           kcaa
358900041020     c                   do        2
359000041020     c     kan11         setll     fnanm11l
359100041020     C     KAN11         READE     FNANM11L
359200041020     c                   dow       not %eof(fnanm11l)
359300041020     c                   if        anmcch<>'AN'
359400041020     c                   EXSR      SCRANT
359500041020     c                   update    fnanm011
359600041020     C                   ENDIF
359700041020     c
359800041020     C     KAN11         READE     FNANM11L
359900041020     c                   enddo
360000041020     c                   z-add     146           kcaa
360100041020     c                   enddo
360200001030     C                   ENDSR
360300031007     C*-------------------------------------------------------------------
360400031007     C* WRIDET - ROUTINE SCRITTURA WRK-FILE WFLAN00F SU STAMPA FORMATO "DET"
360500031007     C*-------------------------------------------------------------------
360600031007     C     WRIDET        BEGSR
360700031007     C*
360800031007     C* Inizializzo il buffer del formato record
360900031007     C                   CLEAR                   WFLAN000
361000031007     C*
361100031031     C                   EVAL      WLANFGS = D34FGS
361200031007     C                   EVAL      WLANPOA = D34PAR
361300031007     C                   EVAL      WLANAAS = ANMAAS
361400031007     C                   EVAL      WLANLNP = ANMLNP
361500031007     C                   EVAL      WLANNRS = ANMNRS
361600031007     C                   EVAL      WLANNSP = ANMNSP
361700031007     C                   EVAL      WLANLNA = ANMLNA
361800031007     C                   MOVE      PRTDSP        WLANDSP
361900031007     C                   EVAL      WLANFLS = ANMFLS
362000031007     C                   EVAL      WLANSCN = ANMSCN
362100031007     C                   MOVE      PRTZNC        WLANZNC
362200031007     C                   EVAL      WLANNPS = ANMNPS
362300031007     C                   EVAL      WLANAIE = DESAIE
362400031007     C                   EVAL      WLANDEA = DESTAN
362500031007     C                   EVAL      WLANCAA = ANMCAA
362600031007     C                   EVAL      WLANDEC = DESCAA
362700031007     C                   EVAL      WLANNDC = ARBNDC
362800031007     C                   MOVE      STADDC        WLANDDC
362900031007     C                   EVAL      WLANPDC = STAPDC
363000031007     C* Se già valorizzato il campo PSTAM compondo la descrizione
363100031007     C                   IF        PSTAM <> *blanks
363200031007     C                   EVAL      WLANCO1 = PSTAM
363300031007     C                   MOVE      WDFLP         WDFLPALFA         3
363400031007     C                   EVAL      %subst(WLANCO1:13:3) = WDFLPALFA
363500031007     C                   MOVE      PRTDET        WPRTDET           4
363600031007     C                   EVAL      WLANCO1 = %trim(WLANCO1) + ' ' +
363700031007     C                                       %subst(WPRTDET:1:2)  + '/' +
363800031007     C                                       %subst(WPRTDET:3:2)
363900031007     C***                EVAL      WLANFLP = WDFLP
364000031007     C***                MOVE      PRTDET        WLANDET
364100031007     C                   ENDIF
364200031007     C*
364300031007     C* Se valorizzato il campo WFGS compondo la descrizione
364400031007     C                   IF        WFGS <> *zeros
364500031007     C                   EVAL      WLANCO2 = CDOVE
364600031007     C                   MOVE      WFGS          WDFLPALFA
364700031007     C                   EVAL      %subst(WLANCO2:13:3) = WDFLPALFA
364800031007     C                   EVAL      %subst(WLANCO2:5:5) = PRTDOV
364900031007     C                   MOVE      PRTDFV        WPRTDET
365000031007     C                   EVAL      WLANCO2 = %trim(WLANCO2) + ' ' +
365100031007     C                                       %subst(WPRTDET:1:2)  + '/' +
365200031007     C                                       %subst(WPRTDET:3:2)
365300031007     C***                EVAL      WLANDOV = PRTDOV
365400031007     C***                EVAL      WLANFGS = WFGS
365500031007     C***                MOVE      PRTDFV        WLANDFV
365600031007     C                   ENDIF
365700031007     C*
365800031007     C* Se valorizzato il numero C.A. compongo la descrizione
365900031007     C                   IF        DCTNCA > *zeros
366000040616     C                   EVAL      WLANCO3 = CCOAN1
366100031007     C                   MOVE      DCTFIL        WDFLPALFA
366200031007     C                   EVAL      %subst(WLANCO3:8:3) = WDFLPALFA
366300031007     C                   MOVE      DCTNCA        WNCAALFA          7
366400031007     C                   EVAL      %subst(WLANCO3:12:7) = WNCAALFA
366500031007     C                   MOVE      STADCA        WDCAALFA          6
366600031007     C                   EVAL      WLANCO3 = %trim(WLANCO3) + ' ' +
366700031007     C                                       %subst(WDCAALFA:1:2)  + '/' +
366800031007     C                                       %subst(WDCAALFA:3:2)  + '/' +
366900031007     C                                       %subst(WDCAALFA:5:2)
367000031007     C***                EVAL      WLANFIL = DCTFIL
367100031007     C***                EVAL      WLANNCA = DCTNCA
367200031007     C***                MOVE      DCTDCA        WLANDCA
367300031007     C                   ENDIF
367400031007     C*
367500031007     C                   EVAL      WLANLID = ANMLID
367600031007     C                   EVAL      WLANDFA = DESFSA
367700031007     C                   MOVE      STADAO        WLANDAA
367800031007     C                   EVAL      WLANCDU = ANMCDU
367900031007     C                   EVAL      WLANNFV = ANMNFV
368000031007     C                   EVAL      WLANDFC = DESFSC
368100031007     C                   MOVE      STADCH        WLANDAC
368200031007     C                   MOVE      CCHPRT        WLANCCH
368300031007     C                   EVAL      WLANDCC = DESCCH
368400031007     C                   EVAL      WLANFLE = ANMFLE
368500031007     C                   EVAL      WLANCHA = DCTDCH
368600031007     C*
368700031007     C* SCarico il buffer sul file
368800031007     C                   WRITE     WFLAN000
368900031007     C*
369000031007     C                   ENDSR
369100031007     C*-------------------------------------------------------------------
369200031007     C* DELWF  - ROUTINE DI CANCELLAZIONE WRK-FILE WFLAN00F X LANCIO CORRENTE
369300031007     C*-------------------------------------------------------------------
369400031007     C     DELWF         BEGSR
369500031007     C*
369600031007     C     KLAN01        SETLL     WFLAN01L
369700031007     C                   IF        %equal(WFLAN01L)
369800031007     C     KLAN01        READE     WFLAN01L
369900031007     C                   DOW       not %eof(WFLAN01L)
370000031007     C                   DELETE    WFLAN01L
370100031007     C     KLAN01        READE     WFLAN01L
370200031007     C                   ENDDO
370300031007     C                   ENDIF
370400031007     C*
370500031007     C                   ENDSR
370600960327     C****************************************************************
370700960327     C     *INZSR        BEGSR
370800960327     C*
370900960327     C     *ENTRY        PLIST
371000960327     C                   PARM                    KPJBA
371100960327     C                   MOVEL     KPJBU         DSLR34
371200960327     C                   Z-ADD     1             CODUT
371300960327     C                   CALL      'X§PARUT'
371400020905     C                   PARM                    UT§DSE0F
371500960327     C                   MOVEL     RAGUT         RSUT             20
371600960327     C                   MOVEL     REC80         CNCR80
371700031007     C*
371800031007     C* Verifico subito se per il lancio richiesto è prevista anche la creazione del wrk-file
371900031009     C* Lista Anomalie (WFLAN00F)
372000111201     C                   IF        DSLR34 <> *blanks  and                       * NO x trasmisisone
372100111201     c                             parsch<>'V'
372200031009     C***                IF        D34IE = 'D'                                  * solo anomalie IDD
372300031007     C* Imposto a NO l'indicatore d condizionamento creazoine wrk-file
372400031007     C                   SETOFF                                       55
372500031007     C* Verifico se x il cliente richiesto nel lancio è prevista la creazine del wrk-file
372600031007     C                   EVAL      WD34POS = D34POS
372700031007     C                   EVAL      WD34NRS = D34NRS
372800031007     C                   EVAL      TBECOD = 'LAN'
372900031007     C                   EVAL      TBEKE1 = DSKEYLAN
373000031007     C     KTBE01P       CHAIN     TNTBE01L
373100031007     C                   IF        %found(TNTBE01L)
373200031007     C* Se trovo il cliente in tabella, verifico i flag x le abilitazioni elaborazione
373300031007     C* anomalie in partenza e/o in arrivo
373400031007     C                   EVAL      DLAN = TBEUNI
373500031007     C                   IF        (§LANPAR = 'S' AND D34PAR = 'P') OR
373600031007     C                             (§LANARR = 'S' AND D34PAR = 'A')
373700031007     C                   SETON                                        55
373800031031     C                   OPEN      WFLAN01L
373900031007     C                   EXSR      DELWF
374000031007     C                   ENDIF
374100031007     C                   ENDIF
374200031007     C***                ENDIF
374300031009     C                   ENDIF
374400960327     C*-----------------------------------------------------
374500960327     C     KTAB          KLIST
374600960327     C                   KFLD                    CODUT
374700960327     C                   KFLD                    COD               2
374800960327     C                   KFLD                    KEY               8
374900031007     C     KTBE01P       KLIST                                                  * Parziale
375000031007     C                   KFLD                    TBECOD
375100031007     C                   KFLD                    TBEKE1
375200031007     C     KLAN01        KLIST                                                  * Completa
375300031031     C                   KFLD                    D34FGS
375400031007     C                   KFLD                    D34POS
375500031007     C                   KFLD                    D34NRS
375600960327     C     KTAB2         KLIST
375700960327     C                   KFLD                    CODUT
375800960327     C                   KFLD                    COD               2
375900960927     C     KFVA          KLIST
376000960927     C                   KFLD                    BRVFGS
376100000201     C                   KFLD                    WNFV
376200960510     C     KCLN          KLIST
376300960510     C                   KFLD                    KTFP
376400960510     C                   KFLD                    KLNA
376500990512     C                   KFLD                    WAA
376600990512     C                   KFLD                    WMM
376700960417     C     KANM          KLIST
376800990512     C                   KFLD                    ANMAAS
376900960417     C                   KFLD                    ANMLNP
377000960417     C                   KFLD                    ANMNRS
377100960417     C                   KFLD                    ANMNSP
377200960405     C     KANM1         KLIST
377300960405     C                   KFLD                    KAAS2
377400960405     C                   KFLD                    KLNP2
377500960405     C                   KFLD                    KNRS2
377600960405     C                   KFLD                    KNSP2
377700960404     C                   KFLD                    KCAA
377800960509     C     KANM2         KLIST
377900960509     C                   KFLD                    ANMFLS
378000960509     C                   KFLD                    ANMLNA
378100960509     C                   KFLD                    ANMNRS
378200960509     C                   KFLD                    ANMSCN
378300960509     C                   KFLD                    KCAA
378400021021     C     KANM20        KLIST
378500021021     C                   KFLD                    KFIL
378600021021     C                   KFLD                    KCAA
378700970529     C     KANM3         KLIST
378800970529     C                   KFLD                    KAIE
378900970529     C                   KFLD                    KCDU
379000970529     C                   KFLD                    ANMLNP
379100970529     C     KANM3A        KLIST
379200970529     C                   KFLD                    KAIE
379300970529     C                   KFLD                    KCDU
379400970529     C     KANM3B        KLIST
379500970529     C                   KFLD                    KAIE
379600970529     C                   KFLD                    KCDU
379700970529     C                   KFLD                    KLNP
379800970529     C                   KFLD                    ANMFLS
379900970529     C     KANM3D        KLIST
380000970529     C                   KFLD                    KAIE
380100970529     C                   KFLD                    KCDU
380200970529     C                   KFLD                    KLNP
380300960422     C     KANM4         KLIST
380400960422     C                   KFLD                    KCAA
380500960422     C                   KFLD                    KDAO
380600960423     C                   KFLD                    KLNA
380700960404     C     KANM5         KLIST
380800960404     C                   KFLD                    KCAA
380900960404     C                   KFLD                    KFIL
381000960412     C     KANM51        KLIST
381100960412     C                   KFLD                    KCAA
381200960412     C                   KFLD                    KFIL
381300960412     C                   KFLD                    KLNP
381400960412     C     KANM52        KLIST
381500960412     C                   KFLD                    KCAA
381600960412     C                   KFLD                    KFIL
381700960412     C                   KFLD                    KLNP
381800960412     C                   KFLD                    KFLS
381900960327     C     KANM7         KLIST
382000960327     C                   KFLD                    KAIE
382100960327     C                   KFLD                    KFIL
382200960327     C                   KFLD                    KLNA
382300960327     C     KANM71        KLIST
382400960327     C                   KFLD                    KAIE
382500960327     C                   KFLD                    KFIL
382600960327     C     KANM72        KLIST
382700960327     C                   KFLD                    KAIE
382800960327     C                   KFLD                    KFIL
382900960327     C                   KFLD                    KLNA
383000960327     C                   KFLD                    KDCH
383100960327     C     KANM8         KLIST
383200960327     C                   KFLD                    KAIE
383300960327     C                   KFLD                    KFIL
383400960327     C                   KFLD                    KLNP
383500960327     C                   KFLD                    KLNA
383600960327     C     KANM82        KLIST
383700960327     C                   KFLD                    KAIE
383800960327     C                   KFLD                    KFIL
383900960327     C                   KFLD                    KLNP
384000960327     C                   KFLD                    KLNA
384100960327     C                   KFLD                    KDCH
384200960327     C     KANM9         KLIST
384300960327     C                   KFLD                    KAIE
384400960327     C                   KFLD                    KFIL
384500960327     C                   KFLD                    KLNP
384600960327     C     KANM93        KLIST
384700960327     C                   KFLD                    KAIE
384800960327     C                   KFLD                    KFIL
384900960327     C                   KFLD                    KLNP
385000960327     C                   KFLD                    KFLS
385100960327     C                   KFLD                    KDCH
385200960327     C     KANM94        KLIST
385300960327     C                   KFLD                    KAIE
385400960327     C                   KFLD                    KFIL
385500960327     C                   KFLD                    KLNP
385600960327     C                   KFLD                    KFLS
385700001102     C     KAN11         KLIST
385800001102     C                   KFLD                    KAIE
385900001102     C                   KFLD                    KCAA
386000960327     C     KART          KLIST
386100960327     C                   KFLD                    ANMFLS
386200960327     C                   KFLD                    ANMLNA
386300960327     C                   KFLD                    ANMNRS
386400960327     C                   KFLD                    ANMSCN
386500960327     C     KARB          KLIST
386600960327     C                   KFLD                    ARTAAS
386700960327     C                   KFLD                    ARTLNP
386800960327     C                   KFLD                    ARTNRS
386900960327     C                   KFLD                    ARTNSP
387000960412     C     KBLP          KLIST
387100960412     C                   KFLD                    BLTAAS
387200960412     C                   KFLD                    BLTLNP
387300960412     C                   KFLD                    BLTNRS
387400960412     C                   KFLD                    BLTNSP
387500960508     C     KFVV          KLIST
387600960508     C                   KFLD                    BRVNPG
387700000201     C                   KFLD                    WNFV
387800960508     C                   KFLD                    BRVFGS
387900960704     C     KBRV          KLIST
388000960704     C                   KFLD                    KNPG
388100960704     C                   KFLD                    ANMFLS
388200960704     C                   KFLD                    ANMLNA
388300960704     C                   KFLD                    ANMNRS
388400960704     C                   KFLD                    ANMSCN
388500960927     C     KBRV3         KLIST
388600960927     C                   KFLD                    KNPG
388700960927     C                   KFLD                    ANMFLS
388800960927     C                   KFLD                    ANMLNA
388900960927     C                   KFLD                    ANMNRS
389000960927     C                   KFLD                    ANMSCN
389100960927     C                   KFLD                    ARTFLP
389200970604     C     KBRV3A        KLIST
389300970604     C                   KFLD                    KNPG
389400970604     C                   KFLD                    ANMFLS
389500970604     C                   KFLD                    ANMLNA
389600970604     C                   KFLD                    ANMNRS
389700970604     C                   KFLD                    ANMSCN
389800970604     C                   KFLD                    SIMFEL
389900970306     C     KA71          KLIST
390000970306     C                   KFLD                    ANMFLE
390100990512     C                   KFLD                    ANMDAO
390200970305     C                   KFLD                    ANMCAA
390300970306     C                   KFLD                    ANMLNP
390400970306     C     KA55          KLIST
390500970306     C                   KFLD                    ANMFLE
390600990512     C                   KFLD                    ANMDAO
390700970306     C                   KFLD                    ANMCAA
390800040504     C                   KFLD                    wlnp
390900040405     C     KA200         KLIST
391000040405     C                   KFLD                    ANMFLE
391100040405     C                   KFLD                    wdta200
391200040405     C                   KFLD                    ANMCAA
391300040504     C                   KFLD                    wlnp
391400041020     C     KANT          KLIST
391500041020     C                   KFLD                    KFOA
391600041020     C                   KFLD                    ANMDAO
391700041020     C                   KFLD                    ANMCAA                                     NE
391800041020     C                   KFLD                    KANLNP                                     NE
391900041020     C                   KFLD                    ANMLNA                                     NE
392000990122     C     KDCT          KLIST
392100990122     C                   KFLD                    DCDAAC
392200990122     C                   KFLD                    DCDFIL
392300990122     C                   KFLD                    DCDNCA
392400020807     C     K02FGV01      klist
392500020807     C                   kfld                    BLTnfv
392600020926     C                   kfld                    SIMfel
392700021203
392800021203     C     Kfapd         klist
392900021203     C                   kfld                    kpdtip
393000021203     C                   kfld                    kpdpdr
393100040615
393200040615     C     Kcae          klist
393300040615     C                   kfld                    kcaeepa
393400040615     C                   kfld                    kcaetfp
393500040615     C                   kfld                    kcaetfe
393600040615     C                   kfld                    kcaedde
393700040615     C     Kcae2         klist
393800040615     C                   kfld                    kcaeepa
393900040615     C                   kfld                    kcaetfp
394000040615     C                   kfld                    kcaetfe
394100040616     C     Kcae1         klist
394200040616     C                   kfld                    kcaeepa
394300040616     C                   kfld                    kcaetfp
394400040616     C                   kfld                    kcaetfa
394500040616     C                   kfld                    kcaedde
394600040616     C     Kcae3         klist
394700040616     C                   kfld                    kcaeepa
394800040616     C                   kfld                    kcaetfp
394900040616     C                   kfld                    kcaetfa
395000960417     C*
395100960402     C* UDATE IN GGMMAAAA
395200960417     C                   TIME                    W0140            14 0
395300960402     C                   MOVE      W0140         WDTGIO            8 0
395400960422     C*
395500960422     C* PRENDO DATA IN AAMMGG
395600960422     C                   MOVEL     WDTGIO        G02DAT
395700960422     C                   MOVEL     0             G02ERR
395800971126     C                   CALL      'XSRDA8'
395900960422     C                   PARM                    WLBDAT
396000971126     C                   MOVE      G02INV        AM8GIO            8 0
396100001102     C* SE AZIONE SCHEDULATA IMPOSTO D34GES E D34FGS
396200001102     C     PARSCH        IFEQ      'V'
396300001102     C                   CLEAR                   DSLR34
396400001102     C                   MOVEL     'V'           D34GES
396500001102     C                   Z-ADD     SIMFEL        D34FGS
396600001102     C                   ENDIF
396700960327     C                   ENDSR
396800041020     C**************************************************************************
396900041020     C* SCRITTURA DEL FILE FNANT00F
397000041020     C**************************************************************************
397100041020     C     SCRANT        BEGSR
397200041020     C**
397300041020     C* PER L'ANOMALIA 145  FOA = ANMFLE
397400041020     C* PER L'ANOMALIA 146  FOA  = TERMINAL DI ARRIVO
397500041020    2C     ANMCAA        IFEQ      145
397600041020     C                   Z-ADD     ANMFLE        KFOA
397700041020   X2C                   ELSE
397800041020     C*
397900041020     C                   CLEAR                   KFOA
398000041020     C* PRENDO FLS SE LNP =0
398100041020     C*
398200041020     C                   CLEAR                   DSLV55
398300041020     C                   MOVEL     'A'           D55TPT
398400041020     C                   Z-ADD     ANMLNA        D55LIN
398500041020     C                   Z-ADD     ANMDAO        D55DRF
398600041020    3C     ANMLNP        IFGT      0
398700041020     C                   Z-ADD     ANMLNP        d55lnp
398800041020     C                   ELSE
398900041020     C                   Z-ADD     ANMFLS        d55lnp
399000041020    3C                   ENDIF
399100041020     C                   CALL      'FNLV55R'
399200041020     C                   PARM                    DSLV55
399300041020     C                   Z-ADD     D55TFA        KFOA
399400041020    2C                   ENDIF
399500041020     C*
399600041020     C     KANT          CHAIN     FNANT02L                           39
399700041020     C*
399800041020    2C     *IN39         IFEQ      *ON
399900041020     C                   CLEAR                   FNANT000
400000041020     C                   MOVEL     KFOA          ANTFOA
400100041020     C                   MOVEL     ANMLNA        ANTLNA
400200041020     C                   MOVEL     ANMDAO        ANTDAO
400300041020     C                   MOVEL     ANMCAA        ANTCAA
400400041020     C                   Z-ADD     1             ANTTOT
400500041020     c                   z-add     *date         antdt1
400600041020     C                   WRITE     FNANT000
400700041020   X2C                   ELSE
400800041020     C                   ADD       1             ANTTOT
400900041020     C                   CLEAR                   ANTFT1
401000041020     C                   CLEAR                   ANTFT2
401100041020     c                   z-add     *date         antdt1
401200041020     C                   UPDATE    FNANT000
401300041020    2C                   ENDIF
401400041020     C*
401500041020     C                   MOVEL     'T'           ANMFT1
401600041020     C                   ENDSR
401700911021     C*
401800960422     O* TESTATA IDD
401900911021     OPRTF198   E            TES              02
402000960604     O                       STA(1)              66
402100960604     O              N13      STA(2)             132
402200040614     o               13      STA(19)            132
402300921015     O                       STA(3)             198
402400900626     O                       RSUT                20
402500040615     O                       prtpo               46
402600040615     O                       D34FGS              50
402700960926     O                       PRTFGS              71
402800960402     O                       WDTGIO             168 '  .  .    '
402900921029     O                       PAGE1         Z    198
403000921015     O          E            TES         1
403100040615     O               70      STA(34)            132
403200960402     O                       UTIME              167 '  :  :  '
403300960402     O                  01                      190 '-C O N F E R M A-'
403400040615     O          E            TES         1
403500960402     O                       STA(18)             66
403600960402     O                       STA(20)            132
403700960402     O                       STA(21)            198
403800960506     O                       DESIE               61
403900960402     O                       D34DLG             106 '  /  /    '
404000960402     O                       D34ALG             123 '  /  /    '
404100960403     O                       D34DCG             172 '  /  /    '
404200960402     O                       D34ACG             189 '  /  /    '
404300030717     C*
404400040615     O          E            TES2        1
404500960403     O                  15   STA(16)             66
404600960403     O                 N15   STA(17)            132
404700960403     O                 N18   STA(15)            198
404800960403     O                  15   DESFV               29
404900990513     O                  15   D34NFV        Z     44
405000990513     O                  15   STADFV              61 '  /  /    '
405100980403     O                 N15   TESFSA             103
405200040611     O                 N18   D34LID             198
405300040614     O                                          120 'LINEA ARRIVO :'
405400980403     O               06      D34LNA             124
405500040614     O               06      PRTLNA             141
405600040614     O                                          154 'TIPO MERCE :'
405700040614     O                       PRTVAL             163
405800030717     O*
405900040615     O          E    23      TES2        1
406000030717     O                       STA(33)            132
406100030717     O                       D34POS              87
406200030717     O                       D34NRS             110
406300960403     O*
406400040615     O          E   N14 19   TES2        1
406500960426     O                       STA(32)             66
406600960426     O                       D34CCH              29
406700960426     O                                         +  0 '-'
406800960426     O                       TESCCH            +  0
406900040615     O          E    14      TES2        1
407000960426     O                  19   STA(32)             66
407100960426     O                       STARIG             132
407200960426     O                  19   D34CCH              29
407300960426     O                                         +  0 '-'
407400960426     O                  19   TESCCH            +  0
407500960418     O                       ANM(1)              94
407600960403     O                       ANM(2)            +  3
407700960403     O                       ANM(3)            +  3
407800960403     O                       ANM(4)            +  3
407900960403     O                       ANM(5)            +  3
408000960403     O                       ANM(6)            +  3
408100960403     O                       ANM(7)            +  3
408200960403     O                       ANM(8)            +  3
408300960403     O                       ANM(9)            +  3
408400960403     O                       ANM(10)           +  3
408500040615     o*
408600040615     O          E    80      TES3        1
408700040615     O                       STA(35)             66
408800040615     o                       stpo(1)             20
408900040615     o                       stpo(2)           +  1
409000040615     o                       stpo(3)           +  1
409100040615     o                       stpo(4)           +  1
409200040615     o                       stpo(5)           +  1
409300040615     o                       stpo(6)           +  1
409400040615     o                       stpo(7)           +  1
409500040615     o                       stpo(8)           +  1
409600040615     o                       stpo(9)           +  1
409700040615     o                       stpo(10)          +  1
409800040615     o                       stpo(11)          +  1
409900040615     o                       stpo(12)          +  1
410000040615     o                       stpo(13)          +  1
410100040615     o                       stpo(14)          +  1
410200040615     o                       stpo(15)          +  1
410300040615     o                       stpo(16)          +  1
410400040615     o                       stpo(17)          +  1
410500040615     o                       stpo(18)          +  1
410600040615     o                       stpo(19)          +  1
410700040615     o                       stpo(20)          +  1
410800040615     o                       stpo(21)          +  1
410900040615     o                       stpo(22)          +  1
411000040615     o                       stpo(23)          +  1
411100040615     o                       stpo(24)          +  1
411200040615     o                       stpo(25)          +  1
411300040615     o                       stpo(26)          +  1
411400040615     o                       stpo(27)          +  1
411500040615     o                       stpo(28)          +  1
411600040615     o                       stpo(29)          +  1
411700040615     o                       stpo(30)          +  1
411800040615     O          E            TES3        2
411900911021     O                       STA(4)              66
412000911021     O                       STA(7)             132
412100911021     O                       STA(10)            198
412200040616     O**               N04 17                   198 '     '
412300040615     O          E            TES3        1
412400911018     O                       STA(5)              66
412500911018     O                       STA(8)             132
412600911021     O                       STA(11)            198
412700040616     O**               N04 17                   198 '     '
412800960422     O*
412900960423     O          E            TESIDR      3  1
413000960422     O                       STA(25)             66
413100960422     O                       STA(26)            132
413200960513     O                       STA(13)            198
413300990513     O                       STADCI              82 '  /  /    '
413400990513     O                       STADCM             166 '  /  /    '
413500960423     O*
413600960423     O          E            TESIDE      3  1
413700960423     O                       STA(30)             66
413800960423     O                       STA(31)            132
413900960513     O                       STA(13)            198
414000990513     O                       STADCI              84 '  /  /    '
414100990513     O                       STADCM             166 '  /  /    '
414200911015     O*
414300911015     O          E            DET         1
414400990512     O                  02   PRTAAS        Z
414500921015     O                  02   ANMLNP        Z   +  1
414600921015     O                  02   ANMNRS        Z   +  1
414700921015     O                  02   ANMNSP        Z   +  1
414800921015     O                  02   ANMLNA            +  1
414900960417     O                  02   PRTDSP            +  1 '  /  '
415000960417     O*
415100990121     O                  03   ANMFLS            +  3
415200921015     O                  03   ANMLNA            +  1
415300921015     O                  03   ANMNRS        Z   +  1
415400921015     O                  03   ANMSCN        Z   +  1
415500040609     O                  03   PRTZNC            +  1
415600040609     O                  03   anmft4            +  0
415700990121     O                  03   ANMNPS        Z   +  1
415800040616     O                       DESTAN            +  1
415900990121     O                       ANMCAA            +  1
416000921015     O                                         +  0 '-'
416100990122     O              N40   N09DESCAA            +  0
416200990122     O                     09DESCA1              95
416300950320     O                     09ARBNDC        Z   +  0
416400950320     O                     09STADDC            +  1 '  /  '
416500950320     O                     09STAPDC            +  1
416600040616     O                     20PSTAM              110
416700040616     O                     20WDFLP              107
416800040616     O                     20PRTDET             118 '  /  '
416900040616     O                     21CDOVE              110
417000040616     O                     21PRTDOV             101
417100040616     O                     21WFGS               107
417200040616     O                     21PRTDFV             118 '  /  '
417300990121     O* NUMERO C.A. SU CUI E' IL COLLO
417400040616     O                     40CCOAN              109
417500040616     O                     40DESCA1              91
417600040616     O                     40DCTFIL        Z     98
417700040616     O                     40DCTNCA        Z    106
417800040616     O                     40STADCA             118 '  /  /  '
417900040616     O                  42 40                   124 'CHIUSA'
418000040616     O                       STALID             125
418100040616     O                       STADAc            +  1 '  /  '
418200040616     O                       STAorc            +  1 '  :  '
418300040616     O                       DESFSA            +  1
418400950320     O                       STADAO            +  1 '  /  '
418500040616     O                       ANMCDU        Z   +  1
418600040616     O                       ANMNFV        Z   +  1
418700990121     O                       DESFSC            +  1
418800950320     O                       STADCH            +  1 '  /  '
418900970522     O                       CCHPRT            +  1
419000960423     O                                         +  0 '-'
419100960423     O                       DESCCH            +  0
419200040616     O                       ANMFLE            +  1
419300050503     O*  STAMPA NUMERO C.A. e/o particolarità consegna CP
419400050503     O          E            DET1        1
419500050503     O               43      decodiCP            96
419600050503     O               41      CCOAN              109
419700050503     O               41      DCTFIL        Z     98
419800050503     O               41      DCTNCA        Z    106
419900050503     O               41      STADCA             118 '  /  /  '
420000050503     O               41                         128 'CHIUSA IL'
420100050503     O               41      STACHA             134 '  /  '
420200050504     O*  STAMPA NUMERO C.A. e/o particolarità consegna CP
420300050504     O          E            DET1        0  0
420400050504     O               43      decodiCP            96
420500051221     O*  STAMPA dati mittente e destinatario
420600051221     O          E            DET2        1
420700051222     o*                                        + 30 'Mittente:'
420800051222     o                                         + 69 'Mittente:'
420900051221     o                       blprsm            +  0
421000051222     o                                         +  2 'Rif.Mitt.Num:'
421100051222     o                       prtrmn            +  0
421200051222     o                                         +  2 'Destinatario:'
421300051221     o                       blprsd            +  0
421400151125     O*  STAMPA consegna anomala 7
421500151125     O          E            DET3        1
421600151125     o                                         + 30 'Bolla chiusa per '
421700151125     o                                         +  0 'merce mai affidata il'
421800151125     o                       prtdcm            +  2 '  /  '
421900160908     O
422000160908     O*  STAMPA spedizione in partenza per anomalia 010
422100160908     O          E            DET4        1
422200160908     o                                         + 30 'Bolla confermata DA '
422300160908     o                                         +  0 'PARTIRE '
422400960422     O*
422500960422     O*  STAMPA RIGA: DETTAGLIO FILIALE CHE NON HANNO FATTO L'IDD
422600960422     O          E            DETIDD      1
422700960423     O                       FID(1)        Z
422800960422     O                                         +  0 '-'
422900960422     O                       DFI(1)            +  0
423000050831     O                       FID(2)        Z   +  2
423100960422     O                                         +  0 '-'
423200960422     O                       DFI(2)            +  0
423300050831     O                       FID(3)        Z   +  2
423400960422     O                                         +  0 '-'
423500960422     O                       DFI(3)            +  0
423600050831     O                       FID(4)        Z   +  2
423700960422     O                                         +  0 '-'
423800960422     O                       DFI(4)            +  0
423900050831     O                       FID(5)        Z   +  2
424000960422     O                                         +  0 '-'
424100960422     O                       DFI(5)            +  0
424200050831     O                       FID(6)        Z   +  2
424300960422     O                                         +  0 '-'
424400960422     O                       DFI(6)            +  0
424500050831     O                       FID(7)        Z   +  2
424600960422     O                                         +  0 '-'
424700960422     O                       DFI(7)            +  0
424800960422     O*
424900960422     O*  STAMPA RIGA: NESSUNA ANOMALIA RISCONTRATA
425000960422     O          E            NOANOM      3  3
425100960422     O                       STA(24)            132
425200960423     O*  STAMPA RIGA: TUTTE LE FILIALI HANNO FATTO L'IDD
425300960423     O          E            SIIDD       2
425400960423     O                       STA(27)             66
425500960423     O                       STA(28)            132
425600960423     O                       STA(29)            198
425700960422     O*  STAMPA IDD3
425800911025     OPRTF198X  E            TESX             02
425900960604     O                       STA(1)              66
426000040618     O                       STA(14)            132
426100040618     O                       STA(3)             198
426200040618     O                       prtpo               46
426300960926     O                       D34FGS              50
426400960926     O                       PRTFGS              71
426500960604     O                       §7ALCI             121
426600911025     O                       RSUT                20
426700960402     O                       WDTGIO             168 '  .  .    '
426800921015     O                       PAGE2         Z    198
426900960403     O          E            TESX        1
427000960402     O                       UTIME              167 '  :  :  '
427100960402     O                  01                      190 '-C O N F E R M A-'
427200921015     O          E            TESX        2
427300911025     O                       STA(4)              66
427400911025     O                       STA(7)             132
427500911025     O                       STA(10)            198
427600921016     O                                          198 '     '
427700911025     O          E            TESX        1
427800911025     O                       STA(5)              66
427900911025     O                       STA(8)             132
428000911025     O                       STA(11)            198
428100921016     O                                          198 '     '
428200911025     O*
428300911025     O          E            DETX        1
428400990512     O                  02   PRTAAS        Z
428500921015     O                  02   ANMLNP        Z   +  1
428600921015     O                  02   ANMNRS        Z   +  1
428700921015     O                  02   ANMNSP        Z   +  1
428800921015     O                  02   ANMLNA            +  1
428900960417     O                  02   PRTDSP            +  1 '  /  '
429000960417     O*
429100990121     O                  03   ANMFLS            +  3
429200921015     O                  03   ANMLNA            +  1
429300921015     O                  03   ANMNRS        Z   +  1
429400921015     O                  03   ANMSCN        Z   +  1
429500130423     O                  03   PRTZNC            +  1
429600130423     O                  03   anmft4            +  0
429700990121     O                  03   ANMNPS        Z   +  1
429800990121     O                       DESTAN            +  1
429900990121     O                       ANMCAA            +  1
430000921015     O                                         +  0 '-'
430100990122     O                    N40DESCAA            +  0
430200040616     O                     20PSTAM              110
430300040616     O                     20WDFLP              107
430400040616     O                     20PRTDET             118 '  /  '
430500040616     O                     21CDOVE              110
430600040616     O                     21PRTDOV             101
430700040616     O                     21WFGS               107
430800040616     O                     21PRTDFV             118 '  /  '
430900990122     O* NUMERO C.A. SU CUI E' IL COLLO
431000040616     O                     40CCOAN              109
431100040616     O                     40DESCA1              91
431200040616     O                     40DCTFIL        Z     98
431300040616     O                     40DCTNCA        Z    106
431400040616     O                     40STADCA             118 '  /  /  '
431500040616     O                  42 40                   124 'CHIUSA'
431600040616     O                       ANMLID             125
431700040616     O                       STADAc            +  1 '  /  '
431800040616     O                       STAorc            +  1 '  :  '
431900040616     O                       DESFSA            +  1
432000040616     O                       STADAO            +  1 '  /  '
432100040616     O                       ANMCDU        Z   +  1
432200040616     O                       ANMNFV        Z   +  1
432300990121     O                       DESFSC            +  1
432400040616     O                       STADCH            +  1 '  /  '
432500960423     O                       ANMCCH            +  1
432600960423     O                                         +  0 '-'
432700960423     O                       DESCCH            +  0
432800040616     O                       ANMFLE            +  1
432900990122     O*  STAMPA NUMERO C.A.
433000050503     O          E            DETX1       1
433100050503     O               43      decodiCP            96
433200050503     O               41      CCOAN              109
433300050503     O               41      DCTFIL        Z     98
433400050503     O               41      DCTNCA        Z    106
433500050503     O               41      STADCA             118 '  /  /  '
433600050503     O               41                         128 'CHIUSA IL'
433700050503     O               41      STACHA             134 '  /  '
433800050504     O          E            DETX1       0  0
433900050504     O               43      decodiCP            96
434000990122     O*
434100990512**
434200040615XXXXXXXXXXXXXXXXXXXX        xxxxxxxxxxxxxxxxxx XXX-XXXXXXXXXXXXXXX
434300960926XXXXX           **   L I S T A     A N O M A L I E   **             2
434400960417    FNLR35R               XX/XX/XXXX                     PAG. XXXX  3
434500040616    S P E D I Z I O N E          S E G N A C O L L O      A      N
434600040616AN LNP SER NUMERO LNA  DATA   LNP LNA SER NUMERO ZON PIS T I P O
434700040616XX XXX XX XXXXXXX XXX XX/XX   XXX XXX XX XXXXXXX xxV XX xxxxxxxxxx
434800040616      O      M      A      L      I      A               LIV CREAZ
434900040616    C  O  D  I  C  E                                     IDD DATA
435000040616xx XXX-bolla in consegna:XXXXXXX XX/XX XXXXXXXXXXXX  X    X XX/xx
435100040616IONE  A  P  E  R  T  U  R  A    C  H  I  U  S  U  R  A         P.O  10
435200040616 ORA  FASE     DATA   FOGLIO    FASE      DATA  CAUSALE        EMI  11
435300040616xx:xx xxxxxxxx XX/XX XXX XXXXXX xxxxxxxx XX/XX XX-XXXXXXXXXXXX XXX 12
435400990513RNO PRECEDENTE, IN DATA XX/XX/XXXX) :
435500960604        **   E L A B O R A Z I O N E      " I. D. D.  X "   **     14
435600040614                                  ANOMALIE IDD DAL LIVELLO IDD : X 15
435700990513ANOMALIE DEL FOGLIO  XXXXXXXX   N.RO  XXXXXX   DEL XX/XX/XXXX      16
435800040614FASE APERTURA ANOMALIE : XXXXXXXXXXXX                              17
435900960403ANOMALIE :  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX         18
436000960604        **   E L A B O R A Z I O N E      "I. D. D."   **          19
436100960402DATA APERTURA ANOMALIA : DAL  XX/XX/XXXX   AL  XX/XX/XXXX          20
436200960402DATA CHIUSURA ANOMALIA : DAL  XX/XX/XXXX   AL  XX/XX/XXXX          21
436300960506CODICI       RICHIESTI : XXX                                       22
436400960506CODICI  NON  RICHIESTI : XXX                                       23
436500960422 *** NESSUNA  ANOMALIA  RISCONTRATA  NEI  PARAMETRI  RICHIESTI *** 24
436600960926ELENCO PUNTI OPER.DI CUI MANCA LA TRASMISSIONE ANOMALIE I.D.D. IN  25
436700990513DATA  XX/XX/XXXX     (PER I PUNTI OPER. CHE ESEGUONO L'IDD DEL GIO 26
436800960926                                                    T U T T I    I 27
436900030717    P U N T I    O P E R A T I V I    H A N N O    C O N F E R M A 28
437000030717 T O    L'  I . D . D .                                            29
437100030717ELENCO PUNTI OPER.DELL'AREA DI CUI MANCA LA CONFERMA DELL'I.D.D. I 30
437200030717N DATA  XX/XX/XXXX   (PER I PUNTI OPER. CHE ESEGUONO L'IDD DEL GIO 31
437300960426CAUSALE CHIUSURA ANOMALIA: XX-XXXXXXXXXXXXXXXXXXXXXXXXX            32
437400030717P.O. SEGNACOLLO : XXX      NUMERO SERIE : XX                       33
437500040615                **  COLLI   DI   VALORE   MANCANTI   **            34
437600040615Punti Operativi: xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx   35
437700921016**
437800960506A R R I V O
437900960506P A R T E N Z A
438000960506  N O N    I.D.D.
438100960506  I . D . D.
438200960506  I N T E R N E
438300960506  T U T T E
438400960506     SOLO APERTE
438500960506     SOLO CHIUSE

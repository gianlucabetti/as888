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
008100040614     D L1              S              3  0 DIM(30)                              P.O.AREA
008200040614     D L1RIF           S              3  0 DIM(30)                              P.O.AREA
008300960326     D*
008400961017     D FLE             S              3  0 DIM(50)                              ANOM.USO FLE
008500970829     D FLO             S              3  0 DIM(50)                              ANOM.ARR FLE SOLO
008600970829     D FLT             S              3  0 DIM(50)                              ANOM.ARR FLE E LNA
008700970829     D FLM             S              3  0 DIM(50)                              ANOM.PAR FLE SOLO
008800970829     D FLR             S              3  0 DIM(50)                              ANOM.PAR FLE E LNP
008900960422     D*
009000960927     D FID             S              3  0 DIM(7)                               P.O. NO IDD
009100050831     D DFI             S             22    DIM(7)                               DECODIFC FIL
009200960927     D FM1             S              3  0 DIM(100)                             P.O. IDD-1
009300960510     D*
009400960506     D K51             S             17    DIM(3)                               COMODO
009500040615     D STpo            S              3    DIM(30)
009600160623     d
009700160623     d sps             s              2  0 dim(101)
009800020905     D*
009900020905     D* DEFINIZIONE STRUTTURE DATI (INTERNE)
010000960327     D*
010100990511     D FNANM1          DS
010200960405     D  ANMNR1               397    400B 0
010300990511     D FNANM2          DS
010400960705     D  ANMNR2               397    400B 0
010500990511     D FNANM3          DS
010600970529     D  ANMNR3               397    400B 0
010700990511     D FNAN10          DS
010800960417     D  ANMN10               397    400B 0
010900990511     D FNANM7          DS
011000960327     D  ANMNR7               397    400B 0
011100990511     D FNANM8          DS
011200960327     D  ANMNR8               397    400B 0
011300990511     D FNANM9          DS
011400960327     D  ANMNR9               397    400B 0
011500960327     D**
011600911015     D WLBDAT          DS
011700971126     D  G02DAT                 1      8  0
011800971126     D  G02INV                 9     16  0
011900971126     D  G02ERR                17     17
012000971126     D  G02TGI                18     22  0
012100960510     D WGIDAT          DS
012200990512     D  GI8DAT                 1      8  0
012300990512     D  GI8INV                 9     16  0
012400990512     D  WAA                    9     12  0
012500990512     D  WMM                   13     14  0
012600990512     D  WGG                   15     16  0
012700990512     D  GI8TGI                17     21  0
012800960927     D                 DS
012900000503     D  FFV                    1    897
013000000503     D                                     DIM(299)                             LINEE FOGLIO
013100960927    +D  FGVFFV                 1    240
013200960927    +D  FGVFF2               241    447
013300000503    +D  FGWFF3               448    687
013400000503    +D  FGWFF4               688    897
013500960927     D*
013600960927     D                 DS
013700000503    +D  FLP                    1    897
013800000503     D                                     DIM(299)                             LINEE TRANSITO
013900960927    +D  FGVFLP                 1    240
014000960927    +D  FGVFL2               241    447
014100000503    +D  FGWFL3               448    687
014200000503    +D  FGWFL4               688    897
014300960510     D                 DS
014400960510     D  CLNPOM                 1     31
014500960510     D  POM                    1     31
014600960510     D                                     DIM(31)                              FESTIV.MATTINO
014700990121     D                 DS
014800031007     D  DCTAAC                 1      4  0
014900031007     D  DCTMGC                 5      8  0
015000031007     D  DCTDCA                 1      8  0 INZ
015100021008     D*
015200160907     D** DSmembro        ds             7    inz
015300160907     D**  MbrM                   1      1    inz('M')
015400160907     D**  MbrPOP                 2      4  0 inz
015500160907     D**  MbrPOA                 5      7  0 inz
015600020905     D*
015700020905     D* DEFINIZIONE STRUTTURE DATI (ESTERNE)
015800020905     D*
015900900622     D KPJBA         E DS
016000001102     D  PARSCH               502    502
016100020905     D UT§DSE0F      E DS
016200020905     D CNCR80        E DS
016300960422     D OG148         E DS
016400991217     D OG143         E DS
016500960510     D DS1Y          E DS
016600990902     D DS7A          E DS
016700990902     D DS7AF         E DS
016800960403     D DS7A2         E DS
016900161003     D DS7B          E DS
017000161003     D DS7C          E DS
017100160623     D DS7J          E DS
017200020905     D DCCH          E DS
017300911021     D DSCMZ         E DS
017400990511     D DSANM         E DS                  EXTNAME(FNANM00F)
017500031007     D DLAN          E DS
017600040616     D DANMFLO       E DS
017700960326     D*
017800960326     D* PASSAGGIO PARAMETRI - DAL PGM FNLR34R FNLR49R
017900960326     D DSLR34        E DS                  EXTNAME(FNLR34DS)
018000990513     D  ANM                   89    118
018100921015     D                                     DIM(10)                              ANOMALIE VIDEO
018200040617     D  spo                  140    229
018300040614     D                                     DIM(30)                              ANOMALIE VIDEO
018400960326     D*
018500960326     D* ds per pgm trul06r - caricamento £x
018600960326     D DSUL06        E DS                  EXTNAME(TRUL06DS)
018700960326     D  LIN                    1     90  0
018800960927     D                                     DIM(30)                              COMODO
018900040614      * Parametri per richiamo a TRUL09R - carico p.o. 2 livello in arr
019000040614     d Trul09ds      e ds
019100040614     d  po2liv                19    108  0 dim(30)
019200020905     D* PASSAGGIO PARAMETRI AL PGM DI GESTIONE ANOMALIE
019300020905     D DSLR44        E DS                  EXTNAME(FNLR44DS)
019400020905     D* PASSAGGIO PARAMETRI AL PGM DI ALLOCAZIONE FILE
019500021009     D FNLV60DS      E DS
019600020905     D* CALL A PGM CHE CARICA I DATI FOGLI  - FNLV53R
019700020905     D DSLV53        E DS                  EXTNAME(FNLV53DS)
019800020905     D DSLV55        E DS                  EXTNAME(FNLV55DS)
019900020905     D DSLR33        E DS                  EXTNAME(FNLR33DS)
020000160907     D** TIBS55DS      E DS
020100020905     D DSBS02        E DS                  EXTNAME(TIBS02DS)
020200040811      * ds per il controllo della presenza di CA per la spedizione richiesta
020300040811     d FIDN12DS      e ds
020400040811     d  skKey                 26    305    dim(20)
020500040811     d  skAnn                306    325    dim(20)
020600040811     d  skDca                326    485  0 dim(20)
020700040811     d  skFca                486    545  0 dim(20)
020800040811     d  skDch                546    705  0 dim(20)
020900040811     d  skCch                706    745    dim(20)
021000040811
021100040811     d dsKey           ds
021200040811     d  dsaac                         4  0
021300040811     d  dsfil                         3  0
021400040811     d  dsnca                         7  0
021500040811      *
021600031007     D****
021700031007     D* DS RIDEFINIZIONE CHIAVE X TABELLA "LAN"
021800031007     D****
021900031007     D                 DS
022000040615     D  WD34POS                            lIKE(D34POS)
022100031007     D  WD34NRS                            LIKE(D34NRS)
022200031007     D  DSKEYLAN               1      5
022300020905     D*
022400020905     D* DEFINIZIONE CAMPI
022500020905     D*
022600040505     D WLNP            s                   LIKE(anmlnp) inz
022700020905     D FGSTFP          S                   LIKE(FVVFGS) inz
022800020905     D WLNA            S                   LIKE(FVVFGS) inz
022900160907     D*** WERR            S                   LIKE(D60ERR) inz
023000160907     D** WMB1            S                   LIKE(D60MB1) inz
023100160907     D** WMB2            S                   LIKE(D60MB2) inz
023200160907     D** SAVFL1          S                   LIKE(ANMFL1) inz
023300160907     D** SAVFL2          S                   LIKE(ANMFL2) inz
023400020905     D KTFP            S                   LIKE(CLNTFP) inz
023500020905     D KANN            S                   LIKE(CLNANN) inz
023600020905     D KMES            S                   LIKE(CLNMES) inz
023700020905     D FELDID          S                   LIKE(§7ADID) inz
023800020905     D KDCH            S                   LIKE(ANMDCH) inz
023900020905     D KFIL            S                   LIKE(ANMLNP) inz
024000020905     D KLNP            S                   LIKE(ANMLNP) inz
024100020905     D KLNP9           S                   LIKE(ANMLNP) inz
024200020905     D KLNP92          S                   LIKE(ANMLNP) inz
024300020905     D KFLS            S                   LIKE(ANMLNP) inz
024400020905     D KLNA            S                   LIKE(ANMLNA) inz
024500020905     D SAVLNA          S                   LIKE(ANMLNA) inz
024600020905     D KLNA7           S                   LIKE(ANMLNA) inz
024700020905     D KLNA8           S                   LIKE(ANMLNA) inz
024800020905     D KAIE            S                   LIKE(ANMAIE) inz
024900020905     D KCAA            S                   LIKE(ANMCAA) inz
025000020905     D SAVCAA          S                   LIKE(ANMCAA) inz
025100020905     D WCAA            S                   LIKE(ANMCAA) inz
025200020905     D KAAS2           S                   LIKE(ANMAAS) inz
025300020905     D KLNP2           S                   LIKE(ANMLNP) inz
025400020905     D KNRS2           S                   LIKE(ANMNRS) inz
025500020905     D KNSP2           S                   LIKE(ANMNSP) inz
025600020905     D KDAO            S                   LIKE(ANMDAO) inz
025700020905     D W53DFV          S                   LIKE(D53DFV) inz
025800020905     D SAVNFV          S                   LIKE(ANMNFV) inz
025900020905     D CCHPRT          S                   LIKE(ANMCCH) inz
026000020905     D KCDU            S                   LIKE(ANMCDU) inz
026100020905     D WDDET           S                   LIKE(ARTDET) inz
026200020905     D WDFLP           S                   LIKE(ARTFLP) inz
026300020905     D KNPG            S                   LIKE(BRVNPG) inz
026400020905     D WNFV            S                   LIKE(FVVNFV) inz
026500020905     D WNPG            S                   LIKE(FVVNPG) inz
026600020905     D WSPG            S                   LIKE(FVVSPG) inz
026700020905     D WDFV            S                   LIKE(FVVDFV) inz
026800020905     D WFGS            S                   LIKE(FVVFGS) inz
026900020905     D WMAX            S                   LIKE(TBLKEY) inz
027000021203     D KPDPDR          S                   LIKE(APDpdr)
027100021203     D KPDTIP          S                   LIKE(APDtip)
027200040401     D SAV_dslr34      S                   LIKE(dslr34)
027300040614     D savl1dao        S                   LIKE(anmdao) inz
027400040614     D sav2ldao        S                   LIKE(anmdao) inz
027500040615     D kcaeepa         S                   LIKE(caeepa) inz
027600040615     D kcaetfp         S                   LIKE(caetfp) inz
027700040616     D kcaetfe         S                   LIKE(caetfe) inz
027800040615     D kcaedde         S                   LIKE(caedde) inz
027900040616     D kcaetfa         S                   LIKE(caetfa) inz
028000040401     d wgir200         s              1
028100040616     d wsetgt          s              1
028200160623     d ix              s              3  0
028300040401     d wdta            s               d   datfmt(*Iso)
028400040405     d wdta200         s                   LIKE(D34ALA)
028500041020     D KFOA            S                   LIKE(ANTFOA)
028600041020     D KANLNP          S                   LIKE(ANTLNP) inz(000)
028700960508     D*
028800960508     D* DEFINIZIONE COSTANTI
028900960508     D*
029000960508     D CDISG           C                   CONST(' DISGUIDO A XXX IL')
029100040614     D VVALORE         C                   CONST('V-Valore')
029200960807     D CTRANS          C                   CONST(' TRANSITO A XXX IL')
029300960704     D CDOVE           C                   CONST(' IN XXXXX A XXX IL')
029400040616     D CCOAN           C                   CONST('C.A.XXX/XXXXXXX D-
029500040616     D                                     t')
029600040616     D CCOAN1          C                   CONST('IN C.A.XXX/XXXXXXX D-
029700040616     D                                     EL')
029800040615     D CPO             C                   CONST('PUNTO  OPERATIVO :')
029900040615     D CTERpar         C                   CONST('TERMINAL PARTENZA:')
030000040615     D CTERarr         C                   CONST('TERMINAL  ARRIVO :')
030100050503     D CCPcort         C                   CONST('CP-NO C.Parziale')
030200050503     D CCPlung         C                   CONST('CP-NO Consegna Parzi-
030300050503     D                                     ale')
030400050831     D CGestito        C                   CONST(' Gestito da ')
030500911018     C*--------------------------------------------------------------- *
030600911018     C*                  INDICATORI                                    *
030700911018     C*--------------------------------------------------------------- *
030800911021     C* 01    - ELABORAZIONE DEFINITIVA IDD
030900911021     C* 02    - ANOMALIA SEGNACOLLO
031000911021     C* 03    - ANOMALIA BOLLA
031100040616     C* 04
031200960927     C* 05    - RICHIESTE IN PARTENZA LE ANOMALIE IDD3 CHIUSE
031300000317     C* 06    - CONDIZIONA STAMPA DELLA LINEA DI ARRIVO
031400960927     C* 07    - STAMPA LE ANOMALIE LOCALI
031500960510     C* 08    - ELENCO MANCATA TRASMISSIONE ANOMALIE IDD
031600990121     C* 09    - BOLLA IN CONSEGNA
031700960422     C* 10    - ON - NON STAMPA L'ANOMALIA
031800960419     C* 11    - ON - NON STAMPA MA AUMENTA LID E TRASMETTE
031900960419     C* 12    - ON - NON STAMPA NON AUMENTA LID MA TRASMETTE
032000000317     C* 13    - RICHIESTA IDD A STAMPA
032100960403     C* 14    - RICHIESTO INCLUSIONE/ESCLUSIONE DI UNA ANOMALIA
032200960403     C* 15    - RICHIESTO NUMERO FOGLIO
032300960403     C* 16    - RICHIESTA FASE APERTURA
032400921016     C* 17    - PASSATO IL TIPO ANOMALIA
032500960426     C* 18    - STAMPA DEL LIVELLO IDD
032600960426     C* 19    - RICHIESTA CAUSALE CHIUSURA
032700960508     C* 20    - INDICA SE COLLO PARTITO NON ARRIVATO E' IN DISGUIDO
032800960704     C* 21    - COLLO IN INVENT. O IN CONSEGNA E MANCA RECORD BOLLA ARR
032900040614     C* 23    - selezione p.o. segnacollo
033000990114     C* 24/29 - INDICANO QUALE FILE STO LEGGENDO
033100960327     C* 30/31 - LETTURA
033200960327     C* 32    - COMODO
033300990122     C* 40/42 - STAMPO NUMERO C.A. E DATE C.A. SE C'E' SU COLLO
033400050503     C* 43    - stampa particolarità consegne: CP se presente
033500051221     C* 44    - stampa dati mittente e destinatario se richiesto
033600151125     C* 45    - stampa se bolla chiusa con consegna anomala 7
033700040615     C* 70    - COLLI DI VALORE MANCANTI
033800040615     C* 80    - campo d34spo pieno, passati p.o. da stampare
033900040615     C* 97    - ANOMALIA IDD
034000960404     C* 98/99 - PER I FILE DI TRASMISSIONE
034100900828     C*-----------------------------------------------------
034200911015     C                   EXSR      IMPO
034300960416     C*
034400001030     C                   SELECT
034500001030     C* SE SOLO INVIO RICHIAMO ROUTINE DI LETTURA
034600001030     C* DELLE ANOMALIE DA TRASMETTERE CON AZIONE SCHEDULATA
034700960416     C* ELABORO SOLO SE LA DATA ULTIMO IDD E' MINORE DELLA DATA ELABORA
034800001030     C     D34GES        WHENEQ    'V'
034900001030     C                   EXSR      SCHANM
035000001030     C*
035100001030    1C     *IN01         WHENEQ    *OFF
035200990512     C     §7ADID        ORLT      D34ALA
035300000317     C*
035400960416     C*
035500960405     C                   MOVEL     'S'           D34RIL
035600960404     C     RILEG         TAG
035700960404     C                   CLEAR                   D34END
035800960327     C**
035900960327     C* RILEGGO SE MI E' STATO CHIESTO DI RICARICARE
036000960327     C**
036100000317    3C     D34RIL        DOWEQ     'S'
036200960327     C                   CLEAR                   D34RIL
036300960402     C                   CLEAR                   WUSCIT
036400960327     C**
036500960326     C* LEGGO PRIMA LE ANOMALIE PARTENZA
036600960326     C**
036700000317    4C     D34PAR        IFEQ      'P'
036800960412     C     D34PAR        OREQ      ' '
036900980327     C                   MOVEL     'P'           WANOM             1
037000970829     C* SONO LE ANOMALIE CHE USANO SOLO FLE PER ESCUDERLE DAL GIRO
037100970829     C*  DELLA LETTURA PER LNP
037200970829     C                   MOVE      FLM           FLE
037300960412     C                   EXSR      PARTEN
037400970529     C*
037500970529     C* POI ELABORO LE ANOMALIE RICEVUTE PER LE QUALI SONO FILIALE DI
037600970529     C* TRANSITO O DISGUIDO (SOLO SE SONO SIMFEL)
037700970529     C* Potenzialmente questa elabor. sarebbe da fare anche per le anom
037800970529     C* arrivo ma per ora non serve perchè in arrivo riparte sempre
037900970529     C* tutto in automatico
038000980401     C* Solo se le anomalie richieste sono anche idd
038100000317    5C     D34IE         IFEQ      ' '
038200980401     C     D34IE         OREQ      'D'
038300000317     C     D34FGS        IFEQ      SIMFEL
038400970529     C                   EXSR      USACDU
038500000317     C                   END
038600000317    5C                   END
038700970829     C**
038800970829     C* POI  LEGGO  LE ANOMALIE UTILIZZANDO ANMFLE
038900970829     C*  CONTENUTE NELLA SKIERA FLR
039000970829     C**
039100970829     C                   MOVE      FLR           FLE
039200990114     C                   SETON                                        24
039300990114     C                   SETOFF                                       25
039400970829     C                   EXSR      USAFLE
039500000317    4C                   ENDIF
039600960327     C**
039700960327     C* POI  LEGGO  LE ANOMALIE ARRIVO
039800960327     C**
039900000317    4C     D34PAR        IFEQ      'A'
040000960412     C     D34PAR        OREQ      ' '
040100980327     C                   MOVEL     'A'           WANOM             1
040200970829     C* SONO LE ANOMALIE CHE USANO SOLO FLE PER ESCUDERLE DAL GIRO
040300970829     C*  DELLA LETTURA PER LNA
040400970829     C                   MOVE      FLO           FLE
040500960412     C                   EXSR      ARRIVO
040600960412     C**
040700960412     C* POI  LEGGO  LE ANOMALIE UTILIZZANDO ANMFLE
040800970829     C*  CONTENUTE NELLA SKIERA FLT
040900960412     C**
041000961017     C                   MOVE      FLT           FLE
041100990114     C                   SETOFF                                       24
041200990114     C                   SETON                                        25
041300960412     C                   EXSR      USAFLE
041400000317    4C                   ENDIF
041500000317    3C                   ENDDO
041600960327     C*
041700960327     C* FINE LETTURA : IN GESTIONE RICHIAMO LO STESSO IL PGM
041800000317    3C     D34F03        IFEQ      ' '
041900960327     C     D34F12        ANDEQ     ' '
042000000317    4C     D34GES        IFEQ      'I'
042100000317     C     D34GES        OREQ      'G'
042200960327     C                   CLEAR                   DSLR44
042300960327     C                   CLEAR                   D34F03
042400960327     C                   CLEAR                   D34F12
042500960327     C                   CLEAR                   D34RIL
042600960327     C                   MOVEL     'S'           D34END
042700960327     C                   MOVEL     DSLR34        D44LR
042800960521     C                   MOVEL     'G'           D44MOD
042900960327     C*
043000960327     C                   MOVEL     DSLR44        KPJBU
043100960327     C                   CALL      'FNLR44R'
043200960327     C                   PARM                    KPJBA
043300111116     C                   PARM                    dslr34
043400960402     C*
043500960402     C                   MOVEL     KPJBU         DSLR44
043600111116     C***                MOVEL     D44LR         DSLR34
043700960404     C     D34RIL        IFEQ      'S'
043800960412     C                   GOTO      RILEG                                        RILETTURA
043900960404     C                   ENDIF
044000960404     C*
044100000317    4C                   ENDIF
044200000317    3C                   ENDIF
044300040401     C**
044400040401     C* Stampa elenco anomalie 200 aperte fino a data apertura = data idd-1
044500040401     C**
044600040401     c     d34ela        ifne      *blanks
044700040401     c                   clear                   fle
044800040401     C                   MOVE      200           FLE(1)
044900040401     C                   SETOFF                                       24
045000040401     C                   SETON                                        25
045100040401     c* mi salvo la dslr34 prima di sporcarla
045200040401     c                   eval      SAV_dslr34 = dslr34
045300040401     c                   eval      wgir200='S'
045400040401     c                   eval      d34ges='S'
045500040401     c                   eval      d34ie ='I'
045600040401     c                   eval      d34anm = '200'
045700040401     c                   eval      d34inc = 'I'
045800040401     c                   eval      d34ela = *blanks
045900040405     c                   eval      wdta200=d34ala
046000070208     c* Cerco il primo giorno lavorativo
046100070208     C                   Z-ADD     D34ala        WDATA8
046200070208     C                   EXSR      RICDAT
046300070208     C                   Z-ADD     WDATAM        d34ala
046400070208     c
046500070208     c**** *iso          Movel     d34ala        wdta
046600070208     c****               subdur    1:*D          wdta
046700070208     c****               Movel     wdta          D34ala
046800040401     C                   exsr      usafle
046900040401     c                   eval      dslr34 = sav_dslr34
047000040401     c                   eval      wgir200=' '
047100040401     c                   endif
047200960422     C**
047300960422     C* STAMPA SE NESSUNA ANOMALIA RISCONTRATA
047400960422     C**
047500000317    3C     D34GES        IFEQ      'S'
047600960422     C*
047700000317    4C     WSTAMP        IFEQ      ' '
047800040615     c                   if        *inOF
047900040615     C                   EXCEPT    TES
048000040615     C  n70              EXCEPT    TES2
048100040615     C                   EXCEPT    TES3
048200040615     C                   SETOFF                                       OF
048300040615     c                   endif
048400960422     C*
048500960422     C                   EXCEPT    NOANOM
048600000317    4C                   ENDIF
048700960422     C**
048800960422     C* STAMPA ELENCO DI CHI NON HA FATTO L'IDD
048900040615     c*  se non richiamato per altre liste
049000960422     C**
049100040615     c                   if        not *in70
049200040615    4C     D34PAR        IFEQ      'P'
049300960506     C     D34IE         ANDEQ     'D'
049400960423     C     D34ELA        ORNE      ' '
049500960424     C     SIMFEL        ANDEQ     D34FGS
049600960423     C                   EXSR      NOIDD
049700000317    4C                   ENDIF
049800000317    3C                   ENDIF
049900040615    3C                   ENDIF
050000960327     C**
050100960327     C* I N V I O   I D D
050200960327     C**
050300000317    3C     *IN01         IFEQ      *ON
050400000317    4C     W7AIDD        IFEQ      'S'
050500960404     C* CONTROLLO PER TRASMETTERE LE CHIUSURE DELLE ANOMALIE  70 E 71
050600960404     C                   EXSR      CHIUAN
050700160907     C* Creo L'ANOMALIA OK IDD
050800970306     C                   EXSR      CAEX
050900160907     c
051000000317    4C                   ENDIF
051100921027     C*
051200960409     C* AGGIORNO DATA ULTIMA ESECUZIONE IDD  CON DATA DEL GIORNO
051300960409     C                   MOVEL     '7A'          COD
051400960409     C                   MOVEL(P)  D34FGS        KEY
051500960409     C     KTAB          CHAIN     TABEL                              3130
051600000317    4C     *IN31         IFEQ      *OFF
051700960409     C     *IN30         ANDEQ     *OFF
051800960409     C                   MOVEL     TBLUNI        DS7AF
051900960409     C                   Z-ADD     D34ALA        §7ADID
052000960409     C                   MOVEL     DS7AF         TBLUNI
052100960409     C                   UPDATE    TABEL
052200000317    4C                   END
052300960409     C*
052400000317    3C                   ENDIF
052500001030     C                   ENDSL
052600000317     C**
052700000320     C**
052800960327     C                   CLEAR                   KPJBU
052900111116     C                   MOVEL     DSLR34        KPJBU
053000000320     C* ULTIMA DISALLOCAZIONE
053100160907     c**
053200160907     c**                 eval      D60po0 = SIMfel
053300160907     C**                 MOVEL     SAVFL1        D60FL1
053400160907     C**                 MOVEL     SAVFL2        D60FL2
053500160907     C**                 MOVEL     AM8GIO        D60DRF
053600160907     C**                 MOVEL     'D'           D60AZN
053700160907     C**                 MOVEL     'FNANM00T'    D60FIL
053800160907     C**                 MOVEL     FNLV60DS      KPJBU
053900160907     C**                 CALL      'FNLV60R'
054000160907     C**                 PARM                    KPJBA
054100160907     C**                 CLEAR                   SAVFL1
054200160907     C**                 CLEAR                   SAVFL2
054300970603     C*
054400970603     C* RICHIAMO PGM PER CHIUDERE IN LR
054500970603     C     WCL53         IFEQ      '1'
054600970603     C                   MOVEL     'C'           D53TLA
054700970603     C                   CALL      'FNLV53R'
054800970603     C                   PARM                    DSLV53
054900970603     C                   END
055000971118     C*
055100971118     C                   MOVEL     'C'           D55TLA
055200971118     C                   CALL      'FNLV55R'
055300971118     C                   PARM                    DSLV55
055400160907     C**                 CLEAR                   TIBS55DS
055500160907     C**                 MOVEL     'C'           I50TLA
055600160907     C**                 CALL      'TIBS55R'
055700160907     C**                 PARM                    TIBS55DS
055800990122     C                   CLEAR                   DSBS02
055900990122     C                   MOVEL     'C'           T02TLA
056000990122     C                   CALL      'TIBS02R'
056100990122     C                   PARM                    KPJBA
056200990122     C                   PARM                    DSBS02
056300040614     C*
056400040614     c                   clear                   trul09ds
056500040614     C                   MOVEL     'C'           i09TLA
056600040614     C                   CALL      'TRUL09R'
056700040614     C                   PARM                    trul09ds
056800031031     C*
056900031031     C* Se aperto wrk-file WFLAN lo chiudo
057000031031     C                   IF        %open(WFLAN01L)
057100031031     C                   CLOSE     WFLAN01L
057200031031     C                   ENDIF
057300960416     C*
057400000000     C                   SETON                                        LR
057500911018     C*
057600911018     C* IMPOSTO CAMPI FISSI -----------------------------------------*
057700911018     C     IMPO          BEGSR
057800921015     C                   Z-ADD     0             PAGE2             4 0
057900921029     C                   Z-ADD     0             PAGE1             4 0
058000921029     C                   TIME                    UTIME             6 0
058100160907     C***                Z-ADD     0             SAVFL1
058200960403     C*
058300960403     C* SE HO RICHIESTO LA STAMPA APRO I FILE DI STAMPA
058400960403     C     D34GES        IFEQ      'S'
058500000317     C**
058600960403     C                   OPEN      PRTF198
058700960403     C     D34ELA        IFNE      ' '
058800960403     C                   OPEN      PRTF198X
058900960403     C                   ENDIF
059000971120     C**
059100971120     C                   SETON                                        OFOE
059200960604     C**
059300960927     C* DECODIFICO IL P.O. IN GESTIONE
059400040614     c                   if        d34fgs>0
059500960604     C     D34FGS        CHAIN     AZORG01L                           30
059600960604     C  N30              MOVEL     ORGDES        PRTFGS           20
059700040615     C* IMPOSTO LA SCRITTA IN TESTATA
059800040615     C                   MOVEL     CPO           PRTPO            18
059900040614     c                   endif
060000040615     c
060100040615     C* SE VENGO DALLA STAMPA DEI COLLI DI VALORE IMPOSTO
060200040615     C*  INDICATORE 70 E LA SCRITTA IN TESTATA
060300040615    1C                   IF        D34STV='S'
060400040615     C                   SETON                                        70
060500040615     C* SE STAMPA PER TERMINAL, IMPOSTO TERMINAL IN TESTATA
060600040615    2C                   IF        D34SPO<>*BLANKS  and d34spo<>*zeros
060700040615    3C                   IF        D34PAR='A'
060800040615     C                   MOVEL     CTERARR       PRTPO
060900040615     C                   ELSE
061000040615     C                   MOVEL     CTERPAR       PRTPO
061100040615    3C                   ENDIF
061200040615    2C                   ENDIF
061300040615    1C                   ENDIF
061400980402     C**
061500980402     C* DECODIFICO LA LINEA DI ARRIVO SE RICHIESTA
061600980402     C     D34LNA        IFNE      *ZEROS
061700980403     C     D34TRA        ANDNE     'S'
061800980402     C                   SETON                                        06
061900980402     C     D34LNA        CHAIN     AZORG01L                           30
062000040614     C  N30              MOVEL     ORGDES        PRTLNA           16
062100980402     C                   END
062200000317     C                   ENDIF
062300040614     c*
062400040614     c* Selezione p.o. segnacollo
062500040614     c                   if        d34pos>0
062600040614     C                   SETON                                        23
062700040614     C                   ENDIF
062800960327     C*
062900960412     C* CARICO IN UNA SCHIERA TUTTE LE ANOMALIE IDD E LE ANOMALIE
063000960412     C*  DA LEGGERE CON ANMFLE
063100960326     C                   MOVEL     '7C'          COD
063200960412     C                   Z-ADD     0             Y                 3 0
063300961017     C                   Z-ADD     0             T                 3 0
063400970829     C                   Z-ADD     0             U                 3 0
063500970829     C                   Z-ADD     0             V                 3 0
063600960326     C     KTAB2         SETLL     TABEL
063700960409     C     KTAB2         READE(N)  TABEL                                  30
063800960416    1C     *IN30         DOWEQ     *OFF
063900960326     C*
064000960416    2C     TBLFLG        IFEQ      ' '
064100960326     C                   MOVEL     TBLUNI        DS7C
064200960506     C*
064300960412     C* ANOMALIE CHE CONSIDERANO ANMFLE
064400961017     C* §7CUEA='S' --> CONSIDERANO SOLO ANMFLE
064500961017     C* §7CUEA='E' --> CONSIDERANO SIA ANMFLE CHE ANMLNA
064600960416    3C     §7CUEA        IFEQ      'S'
064700960412     C                   ADD       1             Y
064800961017     C                   ADD       1             T
064900961017     C                   MOVEL     TBLKEY        FLO(Y)
065000961023     C                   MOVEL     TBLKEY        FLT(T)
065100961017   X3C                   ELSE
065200961017    4C     §7CUEA        IFEQ      'E'
065300961017     C                   ADD       1             T
065400961023     C                   MOVEL     TBLKEY        FLT(T)
065500961017    4C                   ENDIF
065600960416    3C                   ENDIF
065700970829     C** PARTENZA
065800970829    3C     §7CUEP        IFEQ      'S'
065900970829     C                   ADD       1             U
066000970829     C                   ADD       1             V
066100970829     C                   MOVEL     TBLKEY        FLM(U)
066200970829     C                   MOVEL     TBLKEY        FLR(V)
066300970829   X3C                   ELSE
066400970829    4C     §7CUEP        IFEQ      'E'
066500970829     C                   ADD       1             V
066600970829     C                   MOVEL     TBLKEY        FLR(V)
066700970829    4C                   ENDIF
066800970829    3C                   ENDIF
066900960416    2C                   ENDIF
067000960326     C*
067100960409     C     KTAB2         READE(N)  TABEL                                  30
067200000317    1C                   ENDDO
067300160623     c* Carico le pistola reali L o V
067400160623      /free
067500160623       ix=0;
067600160623       clear sps;
067700160623       setll (1:'7J') tabel00f;
067800160623       reade (1:'7J') tabel00f;
067900160623
068000160623  1    dow    not %eof(tabel00f)  ;
068100160623          ds7j=tbluni             ;
068200160623          if §7jtps = 'L' or §7jtps='C'  ;
068300160623           ix=ix+1;
068400160623           sps(ix)=%dec(%subst(tblkey:1:2):2:0);
068500160623          endif  ;
068600160623          reade (1:'7J') tabel00f  ;
068700160623  1    enddo     ;
068800160623      /end-free
068900160623     c
069000921015     C*
069100960327     C* GIRO DATA FOGLIO SE C'E'
069200960327     C                   MOVE      D34DFV        G02INV
069300960327     C                   MOVEL     '3'           G02ERR
069400971126     C                   CALL      'XSRDA8'
069500960327     C                   PARM                    WLBDAT
069600990513     C                   MOVEL     G02DAT        STADFV            8 0
069700960327     C*
069800960326     C* SE NON IMMESSA DAL AL  = ALLA DATA DAL
069900960326     C     D34ALA        IFEQ      0
070000960403     C                   MOVEL     D34DLA        D34ALA                         DT APERTURA
070100960326     C                   ENDIF
070200960403     C     D34ACA        IFEQ      0
070300960403     C                   MOVEL     D34DCA        D34ACA                         DT CHIUSURA
070400960403     C                   ENDIF
070500960403     C*
070600960403     C* 14 ON - RICHIESTA ALMENO UNA ANOMALIA
070700960403     C     D34ANM        IFNE      *BLANKS
070800960403     C                   SETON                                        14
070900960403     C     D34INC        IFEQ      'I'
071000960403     C                   MOVEL     STA(22)       STARIG           66
071100960403     C                   ELSE
071200960403     C                   MOVEL     STA(23)       STARIG           66
071300960403     C                   ENDIF
071400960403     C                   ENDIF
071500921015     C*
071600921029     C* 15 ON  - IMMESSO NUMERO FOGLIO
071700960326     C     D34NFV        IFGT      0
071800921015     C                   SETON                                        15
071900960403     C                   ENDIF
072000921029     C*
072100921029     C* 16 ON  - IMMESSA FASE APERTURA
072200960326     C     D34FAS        IFNE      ' '
072300921015     C                   SETON                                        16
072400960403     C                   MOVEL     '7D'          COD
072500960403     C                   MOVEL(P)  D34FAS        KEY
072600960409     C     KTAB          CHAIN(N)  TABEL                              32
072700980403     C  N32              MOVEL     TBLUNI        TESFSA           12
072800960403     C  N32              MOVEL     TBLUNI        DESFV             8
072900960403     C                   ENDIF
073000040611     C**                           ì
073100960426     C* 19 ON  - IMMESSA CAUSALE CHIUSURA
073200960426     C     D34CCH        IFNE      ' '
073300960426     C                   SETON                                        19
073400960426     C                   MOVEL     '7E'          COD
073500960426     C                   MOVEL(P)  D34CCH        KEY
073600960426     C     KTAB          CHAIN(N)  TABEL                              32
073700960426     C  N32              MOVEL     TBLUNI        TESCCH           25
073800960426     C                   ENDIF
073900960403     C*
074000960403     C* RICHIESTA DELLE ANOMALIE SOLO APERTE O SOLO CHIUSE
074100960403     C     D34APR        IFEQ      'S'
074200960506     C                   MOVEL     DAN(7)        K51(3)
074300960426     C                   ENDIF
074400960426     C     D34APR        IFEQ      'C'
074500960506     C                   MOVEL     DAN(8)        K51(3)
074600960403     C                   ENDIF
074700040611     c                   if        d34bva='V'
074800040611     C                   MOVEL     vvalore       PRTVAL            8
074900040611     C                   ENDIF
075000921029     C**
075100921029     C* 13 ON  - RICHIESTA I.D.D.
075200921029     C**
075300000317    1C     D34ELA        IFNE      ' '
075400921029     C                   SETON                                        13
075500960403     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
075600960403     C* L'IDD
075700960403     C                   MOVEL     '7A'          COD
075800960403     C                   MOVEL     '2       '    KEY
075900960409     C     KTAB          CHAIN(N)  TABEL                              30
076000960403     C  N30              MOVEL     TBLUNI        DS7A2
076100921029     C*
076200921029     C* 01 ON  - ELABORAZIONE DEFINITIVA I.D.D.
076300000317    2C     D34ELA        IFEQ      'S'
076400921029     C                   SETON                                        01
076500960419     C*
076600960419     C* SE NON SONO SIMFEL VEDO A CHE DATA HA LANCIATO IL 1 LIVELLO
076700000317    3C     D34FGS        IFNE      SIMFEL
076800960419     C                   MOVEL     '7A'          COD
076900960419     C                   MOVEL(P)  SIMFEL        KEY
077000960419     C     KTAB          CHAIN(N)  TABEL                              31
077100960419     C   31              CLEAR                   DS7AF
077200960419     C  N31              MOVEL     TBLUNI        DS7AF
077300960419     C                   MOVE      §7ADID        FELDID
077400000317    3C                   ENDIF
077500921029     C* CHAIN A VARIABILI ARRIVI PER VEDERE SE DEVO INVIARE IDD O NO
077600921029     C                   MOVEL     '7A'          COD
077700921029     C                   MOVEL     '1       '    KEY
077800960409     C     KTAB          CHAIN(N)  TABEL                              30
077900990902     C  N30              MOVEL     TBLUNI        DS7A
078000990902     C   30              CLEAR                   DS7A
078100960417     C*
078200960417     C                   MOVEL     §7AIDD        W7AIDD            1
078300000317     C**
078400000317     C* SE DATA DI ELABORAZIONE <= ALLA DATA IN TABELLA, ESCO SENZA
078500000317     C*  ESEGUIRE IL PGM
078600000317     C                   MOVEL     '7A'          COD
078700000317     C                   MOVEL(P)  D34FGS        KEY
078800000317     C     KTAB          CHAIN(N)  TABEL                              31
078900000317     C   31              CLEAR                   DS7AF
079000000317     C  N31              MOVEL     TBLUNI        DS7AF
079100000317    2C                   ENDIF
079200000317    1C                   ENDIF
079300000317     C**
079400000317     C* D34GES='V' SOLO INVIO, IMPOSTO "S" IN W7AIDD
079500000317     C     D34GES        IFEQ      'V'
079600000317     C                   MOVEL     'S'           W7AIDD
079700000317     C                   ENDIF
079800921029     C*
079900160907    1C**   D34GES        IFEQ      'V'
080000160907     C**   D34ELA        OREQ      'S'
080100921029     C* SE DEVO INVIARE: CHKOBJ E ADDPF PER TUTTI I MEMBRI
080200160907    2C**   W7AIDD        IFEQ      'S'
080300921029     C*
080400160907     C**                 MOVE      ')'           VAR               8
080500160907     C**                 Z-ADD     49            LUNG             15 5
080600921029     C* MEMBRO RICEZIONE
080700160907     c**                 reset                   DSmembro
080800160907     c**                 eval      MbrPOP = SIMfel
080900160907     c**                 eval      MbrPOA = D34fgs
081000160907     c**                 movel(p)  DSmembro      wMBR             10
081100160907    2C**                 END
081200160907    1C**                 END
081300960506     C**
081400960506     C*  ANOMALIE PARTENZA ARRIVO
081500960506     C**
081600960506     C     D34PAR        IFEQ      'P'
081700960506     C                   MOVEL     DAN(2)        K51(1)
081800960506     C                   ENDIF
081900960506     C     D34PAR        IFEQ      'A'
082000960506     C                   MOVEL     DAN(1)        K51(1)
082100960506     C                   ENDIF
082200921029     C**
082300960506     C* 17 OFF - RICHIESTO SPECIFICO TIPO DI ANOMALIE  INT/IDD/ NON IDD
082400921029     C**
082500960326    1C     D34IE         IFNE      ' '
082600921016     C                   SETON                                        17
082700921016     C* ANOMALIE INTERNE
082800960506     C     D34IE         IFEQ      'I'
082900960506     C                   MOVEL     DAN(5)        K51(2)
083000960327     C                   MOVEL     'I'           EL(1)
083100960403     C                   SETON                                        18
083200960506     C                   ELSE
083300960506     C* ANOMALIE IDD
083400960506    3C     D34IE         IFEQ      'D'
083500960506     C                   MOVEL     DAN(4)        K51(2)
083600960506   X3C                   ELSE
083700960506     C                   MOVEL     DAN(3)        K51(2)
083800960506    3C                   ENDIF
083900960506     C**
084000960506    3C     D34PAR        IFEQ      'P'
084100040116     C****               MOVEL     'R'           EL(1)
084200040116     C                   MOVEL     'E'           EL(1)
084300960506     C                   ELSE
084400960506     C                   MOVEL     'E'           EL(1)
084500960506    3C                   ENDIF
084600960506    2C                   ENDIF
084700960506     C*
084800960506   X1C                   ELSE
084900960506     C                   MOVEL     DAN(6)        K51(2)
085000960326    1C                   ENDIF
085100960506     C                   MOVEA     K51           DESIE            50
085200961018     C***
085300961018     C* CARICO £1
085400961018     C***
085500961104     C                   CLEAR                   DSUL06
085600961018     C                   MOVE      '£1'          D06COD
085700961018     C                   MOVEL     *BLANKS       D06TLA
085800961104     C                   MOVEL     SIMFEL        D06KEY
085900961018     C                   MOVEL     DSUL06        KPJBU
086000961018     C                   CALL      'TRUL06R'
086100961018     C                   PARM                    KPJBA
086200961018     C                   MOVEL     KPJBU         DSUL06
086300961018     C                   MOVEA     LIN           L1
086400960326     C*
086500960927     C* CALCOLO DATA -1 SE RICHIESTE IN PARTENZA LE ANOMALIE IDD
086600960927    1C     D34IE         IFEQ      'D'
086700990512     C     D34DCA        ANDGT     0
086800960927     C*
086900960927    2C     D34PAR        IFEQ      'P'
087000960927     C     D34PAR        OREQ      ' '
087100960927     C* SE IDD CHIUSE O CON CAUSALE PR
087200960927    3C     D34APR        IFEQ      'C'
087300960927     C     D34CCH        OREQ      'PR'
087400960927     C                   SETON                                        05
087500960927     C*
087600971126     C                   Z-ADD     D34DCA        WDATA8
087700960927     C                   EXSR      RICDAT
087800990512     C                   Z-ADD     WDATAM        W08DM1            8 0
087900960927     C*
088000971126    4C     D34ACA        IFGT      0
088100971126     C                   Z-ADD     D34ACA        WDATA8
088200960927     C                   EXSR      RICDAT
088300990512     C                   Z-ADD     WDATAM        W08AM1            8 0
088400960927    4C                   ENDIF
088500960927    3C                   ENDIF
088600960927    2C                   ENDIF
088700960927    1C                   ENDIF
088800960927     C*
088900960927     C* DETERMINO IN UNA SCHIERA I P.O. CHE FANNO L'IDD IL GIORNO DOPO
089000960927    1C     D34PAR        IFEQ      'P'
089100960927     C     D34PAR        OREQ      ' '
089200960927    2C     D34IE         IFNE      'I'
089300960927     C                   Z-ADD     1             L
089400960927     C     *LOVAL        SETLL     AZORG01L
089500960927     C                   READ      AZORG01L                               30
089600960927     C*
089700960927    3C     *IN30         DOWEQ     *OFF
089800960927     C* ESCLUDO LE ANNULLATE
089900960927    4C     ORGFVA        IFEQ      ' '
090000960927     C* SOLO FILIALE E AGENZIA
090100960927    5C     ORGFAG        IFEQ      'F'
090200960927     C     ORGFAG        OREQ      'A'
090300960927     C                   MOVEL     ORGDE8        OG148
090400960927     C**
090500960927    6C     §OGEID        IFNE      ' '
090600960927     C                   Z-ADD     ORGFIL        FM1(L)                         GIORNO PREC
090700960927     C                   ADD       1             L
090800960927    6C                   ENDIF
090900960927    5C                   ENDIF
091000960927    4C                   ENDIF
091100960927     C*
091200960927     C                   READ      AZORG01L                               30
091300960927    3C                   ENDDO
091400960927     C*
091500960927    2C                   ENDIF
091600960927    1C                   ENDIF
091700971126     C***
091800040614     C* CARICO LA L6 DEI P.O. IN GESTIONE  --> p.o. da elaborare solo se
091900040614     c*        schiera p.o. passata vuota
092000971126     C***
092100040614    0c                   if        d34spo=*blanks or d34spo=*zeros
092200971126     C                   CLEAR                   DSUL06
092300971126     C                   MOVE      '£6'          D06COD
092400971126     C                   MOVEL     'L'           D06TLA
092500971126     C                   MOVEL     D34FGS        D06KEY
092600971126     C                   MOVEL     DSUL06        KPJBU
092700971126     C                   CALL      'TRUL06R'
092800971126     C                   PARM                    KPJBA
092900971126     C                   MOVEL     KPJBU         DSUL06
093000971126     C                   MOVEA     LIN           L6
093100971126     C*
093200971126     C* VEDO SE IL P.O. E' UN TERMINAL DI PARTENZA E SE HA DEI P.O.
093300050906     C* ESTERI O D.P.D. o la 101 resi --> LI CARICO
093400971126     C                   CLEAR                   DSLV55
093500971126     C                   MOVEL     'P'           D55TPT
093600971126     C                   MOVEL     D34FGS        D55LIN
093700980403     C                   MOVEL     AM8GIO        D55DRF
093800971126     C                   CALL      'FNLV55R'
093900971126     C                   PARM                    DSLV55
094000971126     C     D55ERR        IFNE      ' '
094100971126     C                   MOVEL     D55LIN        D55TFP
094200971126     C                   ENDIF
094300971126     C                   MOVEL     D55TFP        FGSTFP
094400971126     C*§
094500971126    1C     D34FGS        IFEQ      D55TFP
094600971126     C                   Z-ADD     1             L
094700971126     C                   CLEAR                   W0030
094800971126     C     W0030         LOOKUP    L6(L)                                  30
094900971126     C*
095000971126     C     *LOVAL        SETLL     AZORG01L
095100971126     C                   READ      AZORG01L                               30
095200971126     C*
095300971126    2C     *IN30         DOWEQ     *OFF
095400991217    3C     ORGFAG        IFEQ      'F'
095500991217     C     ORGFAG        OREQ      'A'
095600971126     C* P.O. NON ANNULLATO
095700991217    4C     ORGFVA        IFEQ      ' '
095800991217     C                   MOVEL     ORGDE3        OG143
095900991217     C*
096000020221    5C     §OGNTW        IFEQ      'EEX'                                        ESTERA
096100020221     C     §OGNTW        OREQ      'EUP'
096200020221     C     §OGNTW        OREQ      'FED'
096300020221     C     §OGNTW        OREQ      'DPD'                                        DPD
096400050906     c* anche la 101 resi la tratto come estero: caricata col terminal
096500050906     C     orgfil        OREQ      101                                          DPD
096600971126     C*
096700971126     C* VEDO SE HA COME TERMINAL DI PARTENZA LA FIL.GESTIONE
096800971126     C                   CLEAR                   DSLV55
096900971126     C                   MOVEL     'P'           D55TPT
097000971126     C                   MOVEL     ORGFIL        D55LIN
097100980403     C                   MOVEL     AM8GIO        D55DRF
097200971126     C                   CALL      'FNLV55R'
097300971126     C                   PARM                    DSLV55
097400991217    6C     D55ERR        IFNE      ' '
097500971126     C                   MOVEL     D55LIN        D55TFP
097600991217    6C                   ENDIF
097700991217    6C     D55TFP        IFEQ      D34FGS
097800971126     C                   Z-ADD     ORGFIL        L6(L)
097900971126     C                   ADD       1             L
098000991217    6C                   ENDIF
098100991217    5C                   ENDIF
098200971126    4C                   ENDIF
098300971126    3C                   ENDIF
098400971126     C*
098500971126     C                   READ      AZORG01L                               30
098600971126    2C                   ENDDO
098700971126     C*
098800971126    1C                   ENDIF
098900040614   x0c                   else
099000040617     c                   z-add     1             xx
099100040617     c                   z-add     1             yy                3 0
099200040617     c                   do        30            xx
099300040617     c                   if        spo(xx)>*zeros
099400040617     c* imposto schiera di elaborazione
099500040617     c                   movel     spo(xx)       l6(yy)
099600040617     c* imposto schiera di stampa alfanumerica
099700040617     c                   movel     spo(xx)       stpo(yy)
099800040617     c                   add       1             yy
099900040615     c                   seton                                        80
100000040617     c                   endif
100100040615     c                   enddo
100200040617     c
100300040614     c                   endif
100400960927     C*
100500911018     C                   ENDSR
100600960412     C*
100700960412     C* LEGGO LE ANOMALIE IN PARTENZA --------------------------------*
100800960412     C     PARTEN        BEGSR
100900960412     C                   MOVEL     'N'           WFLE              1
101000960412     C*
101100960412     C* IMPOSTO LE ANOMALIE CHE DEVO LEGGERE
101200960412     C*
101300960412    2C     D34IE         IFEQ      ' '
101400960412     C                   CLEAR                   EL
101500040116     C**                 MOVEL     'R'           EL(1)
101600040116     C                   MOVEL     'E'           EL(1)
101700960412     C                   MOVEL     'I'           EL(2)
101800960412    2C                   ENDIF
101900960412     C**
102000960412     C                   Z-ADD     1             L                 3 0
102100960412    2C     L6(L)         DOWGT     0
102200960412     C     WUSCIT        ANDEQ     ' '
102300960412     C**
102400960412     C                   MOVEL     L6(L)         KFIL
102500960412     C**
102600960412     C                   Z-ADD     1             E                 3 0
102700960412    3C     EL(E)         DOWNE     ' '
102800960412     C     WUSCIT        ANDEQ     ' '
102900960412     C                   MOVEL     EL(E)         KAIE
103000970217     C                   Z-SUB     1             KLNA
103100960412     C                   SETOFF                                       30
103200960412     C**
103300960412    4C     *IN30         DOWEQ     *OFF
103400960412     C     WUSCIT        ANDEQ     ' '
103500990512     C     KANM7         SETGT     FNANM07L
103600990512     C     KANM71        READE     FNANM07L                               30
103700960412     C   30              MOVEL     999           KLNA7
103800960412     C  N30              MOVEL     ANMLNA        KLNA7
103900960412     C*
104000960412     C                   CLEAR                   KLNP
104100990512     C     KANM8         SETGT     FNANM08L
104200990512     C     KANM9         READE     FNANM08L                               30
104300960412     C   30              MOVEL     999           KLNA8
104400960412     C  N30              MOVEL     ANMLNA        KLNA8
104500960412     C*
104600960412    5C     KLNA7         IFNE      999
104700960412     C     KLNA8         ORNE      999
104800960412     C                   SETOFF                                       30
104900960412     C** TROVATA LA LNA
105000960412    6C     KLNA7         IFLT      KLNA8
105100960412     C                   MOVEL     KLNA7         KLNA
105200960412     C                   ELSE
105300960412     C                   MOVEL     KLNA8         KLNA
105400960412    6C                   ENDIF
105500960927     C*
105600960927     C* SE STO STAMPANDO LE RICEVUTE, VEDO SE STAMPARE LE LOCALI
105700040116     C********           EXSR      LOCALI
105800960412     C**
105900040116     C**** WUSCIT        IFEQ      ' '
106000960927     C                   EXSR      LEGPAR
106100040116     C****               ENDIF
106200960412     C**
106300960927   X5C                   ELSE
106400960412     C                   SETON                                        30
106500990518     C                   Z-ADD     999           KLNA
106600990518     C*
106700990518     C* SE STO STAMPANDO LE RICEVUTE, VEDO SE STAMPARE LE LOCALI
106800040116     C*****              EXSR      LOCALI
106900960412    5C                   ENDIF
107000960412    4C                   ENDDO
107100960412     C*
107200960412     C                   ADD       1             E
107300960412    3C                   ENDDO
107400960412     C*
107500960412     C                   ADD       1             L
107600960412    2C                   ENDDO
107700960412     C                   ENDSR
107800960927     C*
107900960927     C* LETTURA ANOMALIE PER LA PARTENZA------------------------------*
108000960927     C     LEGPAR        BEGSR
108100960927    6C                   SELECT
108200960927     C* SOLO ANOMALIE APERTE
108300961218     C*  SOLTANTO SE NON STO LEGGENDO LE LOCALI PERCHE' ALTRIMENTI
108400961218     C*  NON RIUSCIREI A LEGGERE LE ANOMALIE 70 E 71 CHE SONO
108500961218     C*  CHIUSE VIRUTALMENTE MA NON DI FATTO (MANCA SOLO AL CASUALE CH)
108600040116     C**   D34APR        WHENEQ    'S'
108700040116     C**   *IN07         ANDEQ     *OFF
108800040116     C**                 CLEAR                   KDCH
108900040116     C**   KANM72        SETLL     FNANM07L
109000040116     C**   KANM72        READE     FNANM07L                               31
109100960927     C* SOLO ANOMALIE CHIUSE
109200960927     C     D34APR        WHENEQ    'C'
109300960927     C                   Z-ADD     1             KDCH
109400990512     C     KANM72        SETLL     FNANM07L
109500990512     C     KANM7         READE     FNANM07L                               31
109600960927   X6C                   OTHER
109700990512     C     KANM7         SETLL     FNANM07L
109800990512     C     KANM7         READE     FNANM07L                               31
109900960927    6C                   ENDSL
110000960927     C**
110100960927     C                   SETON                                        27
110200960927     C                   SETOFF                                       252829
110300990114     C                   SETOFF                                       2624
110400960927    6C     *IN31         DOWEQ     *OFF
110500960927     C**
110600960927     C                   EXSR      ELASTA                                        STAMPA
110700960927     C** RILEGGO DA CAPO
110800960927    7C     WUSCIT        IFEQ      ' '
110900960927     C*
111000040116    8C**   D34APR        IFEQ      'S'
111100040116     C**   *IN07         ANDEQ     *OFF
111200040116     C**   KANM72        READE     FNANM07L                               31     ANOM APERTE
111300040116   X8C**                 ELSE
111400990512     C     KANM7         READE     FNANM07L                               31     TUTTE O CHI
111500040116    8C**                 ENDIF
111600960927    7C                   ENDIF
111700960927    6C                   ENDDO
111800960927     C*
111900960927     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
112000960927   6AC     WUSCIT        IFEQ      ' '
112100960927     C                   CLEAR                   KLNP
112200960927    6C                   SELECT
112300960927     C* SOLO ANOMALIE APERTE
112400040116     C**   D34APR        WHENEQ    'S'
112500040116     C**   *IN07         ANDEQ     *OFF
112600040116     C**                 CLEAR                   KDCH
112700040116     C**   KANM82        SETLL     FNANM08L
112800040116     C**   KANM82        READE     FNANM08L                               31
112900960927     C* SOLO ANOMALIE CHIUSE
113000960927     C     D34APR        WHENEQ    'C'
113100960927     C                   Z-ADD     1             KDCH
113200990512     C     KANM82        SETLL     FNANM08L
113300990512     C     KANM8         READE     FNANM08L                               31
113400960927   X6C                   OTHER
113500990512     C     KANM8         SETLL     FNANM08L
113600990512     C     KANM8         READE     FNANM08L                               31
113700960927    6C                   ENDSL
113800960927     C**
113900960927     C                   SETON                                        28
114000960927     C                   SETOFF                                       252729
114100990114     C                   SETOFF                                       2624
114200960927    6C     *IN31         DOWEQ     *OFF
114300960927     C**
114400960927     C                   EXSR      ELASTA                                        STAMPA
114500960927     C**
114600960927    7C     WUSCIT        IFEQ      ' '
114700960927     C*
114800040116    8C**   D34APR        IFEQ      'S'
114900040116     C**   *IN07         ANDEQ     *OFF
115000040116     C**   KANM82        READE     FNANM08L                               31     ANOM APERTE
115100040116   X8C**                 ELSE
115200990512     C     KANM8         READE     FNANM08L                               31     TUTTE O CHI
115300040116    8C**                 ENDIF
115400960927    7C                   ENDIF
115500960927     C*
115600960927    6C                   ENDDO
115700960927   6AC                   ENDIF
115800960927     C                   ENDSR
115900960412     C*
116000960412     C* LEGGO LE ANOMALIE IN ARRIVO ----------------------------------*
116100960412     C     ARRIVO        BEGSR
116200960412     C                   MOVEL     'N'           WFLE              1
116300960412     C*
116400960412     C* IMPOSTO LE ANOMALIE CHE DEVO LEGGERE
116500960412    2C     D34IE         IFEQ      ' '
116600960412     C                   CLEAR                   EL
116700960412     C                   MOVEL     'E'           EL(1)
116800960412     C                   MOVEL     'I'           EL(2)
116900960412    2C                   ENDIF
117000960412     C**
117100960412     C                   Z-ADD     1             L                 3 0
117200960412    2C     L6(L)         DOWGT     0
117300960412     C     WUSCIT        ANDEQ     ' '
117400960412     C**
117500960412     C                   MOVEL     L6(L)         KFIL
117600960412     C**
117700960412     C                   Z-ADD     1             E                 3 0
117800960412    3C     EL(E)         DOWNE     ' '
117900960412     C     WUSCIT        ANDEQ     ' '
118000960412     C                   MOVEL     EL(E)         KAIE
118100960412     C                   CLEAR                   KLNP
118200960412     C                   SETOFF                                       30
118300960412     C**
118400960412    4C     *IN30         DOWEQ     *OFF
118500960412     C     WUSCIT        ANDEQ     ' '
118600960412     C                   CLEAR                   KFLS
118700990512     C     KANM9         SETGT     FNANM09L
118800990512     C     KANM71        READE     FNANM09L                               30
118900960412     C   30              MOVEL     999           KLNP9
119000960412     C  N30              MOVEL     ANMLNP        KLNP9
119100960412     C*
119200960412     C                   MOVEL     KLNP          KFLS
119300960412     C                   CLEAR                   KLNP
119400990512     C     KANM94        SETGT     FNANM09L
119500990512     C     KANM9         READE     FNANM09L                               30
119600960412     C   30              MOVEL     999           KLNP92
119700960412     C  N30              MOVEL     ANMFLS        KLNP92
119800960412     C                   CLEAR                   KFLS
119900960412     C*
120000960412    5C     KLNP9         IFNE      999
120100960412     C     KLNP92        ORNE      999
120200960412     C                   SETOFF                                       30
120300960412     C** TROVATA LA LNP
120400960412     C     KLNP9         IFLT      KLNP92
120500960412     C                   MOVEL     KLNP9         KLNP
120600960412     C                   ELSE
120700960412     C                   MOVEL     KLNP92        KLNP
120800960412     C                   ENDIF
120900960412     C**
121000990512     C     KANM9         SETLL     FNANM09L
121100990512     C     KANM9         READE     FNANM09L                               31
121200960412     C**
121300960412     C                   SETON                                        29
121400960412     C                   SETOFF                                       252728
121500990114     C                   SETOFF                                       2624
121600960412    6C     *IN31         DOWEQ     *OFF
121700960412     C**
121800960412     C                   EXSR      ELASTA                                        STAMPA
121900960412     C**
122000960412     C     WUSCIT        IFEQ      ' '
122100990512     C     KANM9         READE     FNANM09L                               31     TUTTE O CHI
122200960412     C                   ENDIF
122300960412    6C                   ENDDO
122400960412     C*
122500960412     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
122600960412   6AC     WUSCIT        IFEQ      ' '
122700960412     C                   MOVEL     KLNP          KFLS
122800960412     C                   CLEAR                   KLNP
122900960412    6C                   SELECT
123000960419     C* SOLO ANOMALIE APERTE (SE NO CONFERMA IDD)
123100960412     C     D34APR        WHENEQ    'S'
123200960419     C     D34ELA        ANDNE     'S'
123300960412     C                   CLEAR                   KDCH
123400990512     C     KANM93        SETLL     FNANM09L
123500990512     C     KANM93        READE     FNANM09L                               31
123600960419     C* SOLO ANOMALIE CHIUSE  (SE NO CONFERMA IDD)
123700960412     C     D34APR        WHENEQ    'C'
123800960419     C     D34ELA        ANDNE     'S'
123900960412     C                   Z-ADD     1             KDCH
124000990512     C     KANM93        SETLL     FNANM09L
124100990512     C     KANM94        READE     FNANM09L                               31
124200960412   X6C                   OTHER
124300990512     C     KANM94        SETLL     FNANM09L
124400990512     C     KANM94        READE     FNANM09L                               31
124500960412    6C                   ENDSL
124600960412     C**
124700960412    6C     *IN31         DOWEQ     *OFF
124800960412     C**
124900960412     C                   EXSR      ELASTA                                        STAMPA
125000960412     C**
125100960412    7C     WUSCIT        IFEQ      ' '
125200960412     C*
125300960412    8C     D34APR        IFEQ      'S'
125400960423     C     D34ELA        ANDNE     'S'
125500990512     C     KANM93        READE     FNANM09L                               31     ANOM APERTE
125600960412   X8C                   ELSE
125700990512     C     KANM94        READE     FNANM09L                               31     TUTTE O CHI
125800960412    8C                   ENDIF
125900960412    7C                   ENDIF
126000960412     C*
126100960412    6C                   ENDDO
126200960412   6AC                   ENDIF
126300960412     C**
126400960412     C* REIMPOSTO IL CAMPO DELLA LINEA DI PARTENZA
126500960412     C                   MOVEL     KFLS          KLNP
126600960412     C                   CLEAR                   KFLS
126700960412     C                   ELSE
126800960412     C                   SETON                                        30
126900960412    5C                   ENDIF
127000960412    4C                   ENDDO
127100960412     C*
127200960412     C                   ADD       1             E
127300960412    3C                   ENDDO
127400960412     C*
127500960412     C                   ADD       1             L
127600960412    2C                   ENDDO
127700960412     C                   ENDSR
127800960412     C*
127900960412     C* LEGGO LE ANOMALIE CON ANMFLE ---------------------------------*
128000960412     C     USAFLE        BEGSR
128100960412     C                   MOVEL     'S'           WFLE              1
128200960412     C                   Z-ADD     1             Y
128300960412     C**
128400960412    1C     FLE(Y)        DOWGT     0
128500960412     C     WUSCIT        ANDEQ     ' '
128600960412     C                   MOVEL     FLE(Y)        KCAA
128700960412     C                   CLEAR                   KFIL
128800960412     C                   SETOFF                                       30
128900960523     C*
129000960508     C* VERIFICO SE L'ANOMALIA E' SECONDO I PARAMETRI RICHIESTI
129100960508     C                   Z-ADD     KCAA          ANMCAA
129200960508     C                   EXSR      LEGCAA
129300960523     C*
129400960508     C* RICHIESTE LE ANOMALIE INTERNE
129500960508     C     D34IE         IFEQ      'I'
129600960508     C     §7CAIE        ANDNE     'I'
129700960508     C                   SETON                                        30
129800960508     C                   ENDIF
129900960523     C**
130000960523     C* RICHIESTE LE ANOMALIE NON IDD
130100960523     C     D34IE         IFEQ      'N'
130200960523     C     §7CIDD        ANDEQ     'S'
130300060428     C     d34ie         oreq      'N'
130400060428     C     §7CAIE        ANDNE     'E'
130500960523     C                   SETON                                        30
130600960523     C                   ENDIF
130700960508     C**
130800960508     C* RICHIESTE LE ANOMALIE IDD
130900960523    1C     D34IE         IFEQ      'D'
131000960508     C     §7CIDD        ANDNE     'S'
131100960523     C*  SE E' LA CONFERMA DELL'IDD LE DEVO ELABORARE COMUNQUE
131200960523    2C     D34ELA        IFNE      'S'
131300960508     C                   SETON                                        30
131400960523   X2C                   ELSE
131500960523    3C     §7CAIE        IFNE      'E'                                          ESTERNA NO I
131600960523     C                   SETON                                        30
131700960523    3C                   ENDIF
131800960523    2C                   ENDIF
131900960523    1C                   ENDIF
132000960412     C**
132100960417    2C     *IN30         DOWEQ     *OFF
132200990512     C     KANM5         SETGT     FNANM10L
132300990512     C     KCAA          READE     FNANM10L                               30
132400960412    3C     *IN30         IFEQ      *OFF
132500960412     C                   MOVEL     ANMLNA        KFIL
132600960412     C                   CLEAR                   KLNP
132700960412     C                   CLEAR                   KFLS
132800960412     C                   SETOFF                                       31
132900960412     C*
133000960412    4C     *IN31         DOWEQ     *OFF
133100960412     C     WUSCIT        ANDEQ     ' '
133200990512     C     KANM51        SETGT     FNANM10L
133300990512     C     KANM5         READE     FNANM10L                               31
133400960412     C  N31              MOVEL     ANMLNP        KLNP9
133500960412     C   31              MOVEL     999           KLNP9
133600960412     C*
133700960412     C                   MOVEL     KLNP          KFLS
133800960412     C                   CLEAR                   KLNP
133900990512     C     KANM52        SETGT     FNANM10L
134000990512     C     KANM51        READE     FNANM10L                               31
134100960412     C  N31              MOVEL     ANMFLS        KLNP92
134200960412     C   31              MOVEL     999           KLNP92
134300960412     C*
134400960412    5C     KLNP9         IFNE      999
134500960416     C     KLNP92        ORNE      999
134600960412     C** TROVATA LA LNP
134700960412    6C     KLNP9         IFLT      KLNP92
134800960412     C                   MOVEL     KLNP9         KLNP
134900960412     C                   ELSE
135000960412     C                   MOVEL     KLNP92        KLNP
135100960412    6C                   ENDIF
135200960412     C**
135300990512     C     KANM51        SETLL     FNANM10L
135400990512     C     KANM51        READE     FNANM10L                               31
135500960412     C**
135600960412     C                   SETOFF                                       272829
135700970529     C                   SETOFF                                       26
135800960412    6C     *IN31         DOWEQ     *OFF
135900960412     C**
136000960412     C                   EXSR      ELASTA                                        STAMPA
136100960412     C**
136200960412    7C     WUSCIT        IFEQ      ' '
136300990512     C     KANM51        READE     FNANM10L                               31     TUTTE O CHI
136400960412    7C                   ENDIF
136500960412    6C                   ENDDO
136600960412     C*
136700960412     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
136800960412    6C     WUSCIT        IFEQ      ' '
136900960412     C                   MOVEL     KLNP          KFLS
137000960412     C                   CLEAR                   KLNP
137100990512     C     KANM52        SETLL     FNANM10L
137200990512     C     KANM52        READE     FNANM10L                               31
137300960412     C**
137400960412    7C     *IN31         DOWEQ     *OFF
137500960412     C**
137600960412     C                   EXSR      ELASTA                                        STAMPA
137700960412     C**
137800960412    8C     WUSCIT        IFEQ      ' '
137900960412     C*
138000990512     C     KANM52        READE     FNANM10L                               31     TUTTE O CHI
138100960412    8C                   ENDIF
138200960412     C*
138300960412    7C                   ENDDO
138400960412    6C                   ENDIF
138500960412     C**
138600960412     C* REIMPOSTO IL CAMPO DELLA LINEA DI PARTENZA
138700960412     C                   MOVEL     KFLS          KLNP
138800960412     C                   CLEAR                   KFLS
138900960412     C                   SETOFF                                       31
139000960412   X5C                   ELSE
139100960412     C                   SETON                                        31
139200960412    5C                   ENDIF
139300960412    4C                   ENDDO
139400960412    3C                   ENDIF
139500960412    2C                   ENDDO
139600960412     C*
139700960412     C                   ADD       1             Y
139800960412    1C                   ENDDO
139900960412     C*
140000960412     C                   ENDSR
140100970529     C*
140200040614     C* LEGGO LE ANOMALIE CON ANMCDU solo in partenza ----------------*
140300970529     C     USACDU        BEGSR
140400970529     C                   MOVEL     'N'           WFLE              1
140500040615     C                   MOVEL     'S'           Wcdu              1
140600970529     C                   SETOFF                                       252728
140700990114     C                   SETOFF                                       2924
140800970529     C                   SETON                                        26
140900970529     C* LEGGO TUTTE LE ANOMALIE RICEVUTE CON CDU=SIMFEL SCARTANDO
141000040616     C* QUELLE CON LNP O FLS PRESENTI IN £1(lo scato avviene nella
141100040616     c*        exsr CONFR)
141200970529     C****
141300970529     C* 1°CICLO: LEGGO CON LNP > 0 E <> £1
141400970529     C****
141500040116     C                   MOVEL     'E'           KAIE
141600970529     C                   Z-ADD     SIMFEL        KCDU
141700970529     C                   Z-ADD     1             ANMLNP
141800040616     c
141900970529    0C     WUSCIT        IFEQ      *BLANKS
142000990512     C     KANM3         SETLL     FNANM03L
142100990512     C     KANM3A        READE     FNANM03L                               30
142200970529    1C     *IN30         DOWEQ     *OFF
142300040615     c                   eval      wsetgt='N'
142400040615     c
142500040429     C                   EXSR      ELASTA
142600040615     c*
142700040615     c                   if        wsetgt='S'
142800040615     C     KANM3         SETGT     FNANM03L
142900040429     c                   endif
143000040615     c
143100970529    2C     WUSCIT        IFEQ      *BLANKS
143200990512     C     KANM3A        READE     FNANM03L                               30
143300970529    2C                   END
143400970529    1C                   ENDDO
143500970529     C****
143600970529     C* 2°CICLO: LEGGO CON LNP = 0
143700970529     C*                 ED FLS > 0 E <> £1
143800970529     C****
143900970529   1AC     WUSCIT        IFEQ      *BLANKS
144000970529     C                   MOVE      *ZEROS        KLNP
144100970529     C                   Z-ADD     1             ANMFLS
144200990512     C     KANM3B        SETLL     FNANM03L
144300990512     C     KANM3D        READE     FNANM03L                               30
144400970529    1C     *IN30         DOWEQ     *OFF
144500040616     c                   eval      wsetgt='N'
144600040616     c
144700040429     C                   EXSR      ELASTA
144800040616     c
144900040616     C* Fls in £1 --> Scarto e mi posiziono su nuova Fls
145000040616    2c                   if        wsetgt='S'
145100990512     C     KANM3B        SETGT     FNANM03L
145200970529    2C                   END
145300040616    c
145400970529    2C     WUSCIT        IFEQ      *BLANKS
145500990512     C     KANM3D        READE     FNANM03L                               30
145600040616    2C                   END
145700970529    1C                   ENDDO
145800970529   1AC                   END
145900970529    0C                   END
146000040615     C                   MOVEL     'N'           Wcdu              1
146100970529     C                   ENDSR
146200960327     C*
146300960523     C* ELABORO I DATI DELLE ANOMALIE PER LA STAMPA -----------------*
146400960327     C     ELASTA        BEGSR
146500960327     C*
146600960327     C* CONFRONTO SELEZIONI FATTE
146700960327     C                   EXSR      CONFR
146800960327     C**
146900960327    1C     *IN10         IFEQ      *ON
147000960327     C**
147100960327     C* SE ELABORAZIONE DEFINITIVA IDD, INVIO ANCHE LE ANOMALIE
147200960705     C*  CHE CI SONO DA TRASMETTERE
147300960327    2C     *IN01         IFEQ      *ON
147400960419     C     *IN12         ANDEQ     *ON
147500960327    3C     ANMFT1        IFEQ      ' '
147600960412     C     ANMFL1        ANDGT     0
147700960327     C     ANMFT2        OREQ      ' '
147800960412     C     ANMFL2        ANDGT     0
147900960705     C                   CLEAR                   WTRM
148000960827     C* SERIE > 0
148100960827    4C     ANMNRS        IFGT      *ZEROS
148200960827     C**SE ANOMALIA 55 ED ESISTE BOLLA PARTENZA NON LA TRASMETTO E
148300960827     C* LA CHIUDO. CHIUDO ANCHE LA 200
148400960827     C* (SE BOLLA DA ME CONFERMABILE E CONFERMABILE ANCHE DA ALTRI AS)
148500960827     C**STESSA COSA SE ANOMALIA 50 (SENZA CHIUDERE LA 200)
148600960827     C     ANMCAA        IFEQ      50
148700960827     C     ANMCAA        OREQ      55
148800990409     C     ANMCAA        OREQ      56
148900960827     C                   EXSR      A50A55
149000960827     C                   ENDIF
149100960827    4C                   ENDIF
149200960827     C*
149300960705    4C     WTRM          IFEQ      ' '
149400990114     C   24
149500990512     COR 25ANMN10        CHAIN     FNANM000                           33
149600000306     C   26ANMNR3        CHAIN     FNANM000                           33
149700000306     C   27ANMNR7        CHAIN     FNANM000                           33
149800990512     C   28ANMNR8        CHAIN     FNANM000                           33
149900990512     C   29ANMNR9        CHAIN     FNANM000                           33
150000960404     C*
150100960327     C                   EXSR      ELAB
150200960705    4C                   ENDIF
150300960327    3C                   ENDIF
150400960327    2C                   ENDIF
150500960327     C**
150600960327   X1C                   ELSE
150700960327     C* 10 OFF - RECORD SELEZIONABILE
150800960327     C                   CLEAR                   WUSCIT
150900960327     C**
151000960327     C** G E S T I O N E
151100000317    2C     D34GES        IFEQ      'I'
151200000317     C     D34GES        OREQ      'G'
151300960327     C* RICHIAMO IL PGM FNLR44R PASSANDOGLI IL RECORD DELL'ANOMALIA
151400960327     C*  IL NUMERO RELATIVO DI RECORD
151500960327     C*  LA DS DEI PARAMETRI RICHIESTI
151600960515     C* LO RICHIAMO SOLO SE IL SOGGETTO ANOMALIA E' <> DA "E"
151700040116    3C**   ANMSAN        IFNE      'E'
151800040116     C**   ANMAIE        ANDEQ     'E'
151900040116     C**   ANMAIE        ORNE      'E'
152000040116    c
152100040116    3C     ANMSAN        IFNE      'E'
152200040116     C     wanom         ANDEQ     'A'
152300040116     C     ANMAIE        ORNE      'E'
152400040116     C     wanom         orne      'A'
152500960327     C                   CLEAR                   DSLR44
152600960515    4C     *IN27         IFEQ      *ON
152700960401     C     *IN28         OREQ      *ON
152800970529     C     *IN26         OREQ      *ON
152900990114     C     *IN24         OREQ      *ON
153000990114     C   24              Z-ADD     ANMN10        D44NRR
153100970529     C   26              Z-ADD     ANMNR3        D44NRR
153200960327     C   27              Z-ADD     ANMNR7        D44NRR
153300960327     C   28              Z-ADD     ANMNR8        D44NRR
153400960401     C                   MOVEL     'P'           D34APA
153500960515   X4C                   ELSE
153600960327     C   29              Z-ADD     ANMNR9        D44NRR
153700960417     C   25              Z-ADD     ANMN10        D44NRR
153800960401     C                   MOVEL     'A'           D34APA
153900960515    4C                   ENDIF
154000960327     C                   CLEAR                   D34F03
154100960327     C                   CLEAR                   D34F12
154200960327     C                   CLEAR                   D34RIL
154300960327     C                   MOVEL     DSLR34        D44LR
154400960327     C                   MOVEL     DSANM         D44ANM
154500960521     C                   MOVEL     'G'           D44MOD
154600960327     C*
154700960327     C                   MOVEL     DSLR44        KPJBU
154800960327     C                   CALL      'FNLR44R'
154900960327     C                   PARM                    KPJBA
155000111116     C                   PARM                    dslr34
155100960327     C                   MOVEL     KPJBU         DSLR44
155200111116     C***                MOVEL     D44LR         DSLR34
155300960327     C* SE PREMUTO F3 F12 O RILETTURA
155400960515    4C     D34F03        IFEQ      'S'
155500960327     C     D34F12        OREQ      'S'
155600960327     C     D34RIL        OREQ      'S'
155700960327     C                   MOVEL     'S'           WUSCIT            1
155800960327     C                   SETON                                        3031
155900960515    4C                   ENDIF
156000960515    3C                   ENDIF
156100960327     C**
156200960327   X2C                   ELSE
156300960327     C** S T A M P A
156400960327     C*
156500000317    3C                   SELECT
156600000317     C* ELABORAZIONE IDD
156700000317     C     D34ELA        WHENNE    ' '
156800960405     C  N11              EXSR      STAMPA
156900960327     C*
157000960327     C* ELABORAZIONE DEFINITIVA AGGIORNO E SPEDISCO
157100960405     C   01              EXSR      ELAB
157200960327     C**
157300000317     C     D34GES        WHENEQ    'S'
157400960327     C* NON E' STAMPA IDD --> STAMPO TUTTO
157500960327     C                   EXSR      STAMPA
157600000317     C*
157700000317    3C                   ENDSL
157800960327    2C                   ENDIF
157900960327    1C                   ENDIF
158000960327     C*
158100960327     C                   ENDSR
158200921015     C*
158300921015     C* CONFRONTO RICHIESTA FATTA A VIDEO ---------------------------*
158400921015     C     CONFR         BEGSR
158500160908     C                   SETOFF                                         2097
158600960419     C                   SETOFF                                       101112
158700960509     C                   CLEAR                   PRTZNC
158800960927     C                   CLEAR                   PRTDET
158900960927     C                   CLEAR                   WDDET
159000960927     C                   CLEAR                   WDFLP
159100960927     C                   CLEAR                   PSTAM
159200030717     C**
159300030717     C* SE RICHIESTO ESEGUO LA SELEZIONE X P.O. SEGNACOLLO E NUMERO SERIE
159400030717     C                   IF        D34POS  > *ZEROS
159500030717     C                   IF        D34POS <> ANMFLS
159600030717     C                   SETON                                        10
159700030717     C                   GOTO      ENDCON
159800030717     C                   ENDIF
159900040614     c
160000030717     C                   IF        D34NRS <> 99
160100030717     C                   IF        D34NRS <> ANMNRS
160200030717     C                   SETON                                        10
160300030717     C                   GOTO      ENDCON
160400030717     C                   ENDIF
160500030717     C                   ENDIF
160600040614     C                   ENDIF
160700040609     c
160800040609     c* se richiesto, faccio vedere solo anomalie di colli di valore
160900040609     c                   if        D34bva='V'  and
161000040609     c                             anmft4<>'V'
161100040609     C                   SETON                                        10
161200040609     C                   GOTO      ENDCON
161300040609     C                   ENDIF
161400000317     C**
161500980402     C* CONTROLLO CHE LA LINEA DI ARRIVO SIA UGUALE A QUELLA RICHIESTA A VIDEO
161600980403     C* SE NON SONO IN UNA FASE DI TRASFERIMENTO
161700980403     C     D34TRA        IFNE      'S'
161800980402     C     D34LNA        IFNE      *ZEROS
161900980402     C     D34LNA        ANDNE     ANMLNA
162000980402     C                   SETON                                        10
162100980402     C                   GOTO      ENDCON
162200980402     C                   ENDIF
162300980403     C                   ENDIF
162400960424     C*
162500960424     C* SE STO LEGGENDO UNA ANOMALIA RICEVUTA, NON LA STAMPO
162600960424     C*  SE IL SEGNACOLLO NEL FRATTEMPO E' STATO CONSEGNATO
162700960524     C*  (NON COME IDD3 DANNI CODICE 5)
162800970127     C*  (NON SE DATA APERTURA ANOMALIA > DATA CONSEGNA)
162900040116     C     ANMSCN        ifgt      0
163000040223     C     wanom         andeq     'P'
163100111115     c* In gestione/int non lo faccio --> devo vedere le anomalie come sono
163200111115     c*  per poterle annullare
163300111115     c     d34ges        andeq     'S'
163400960424     C*
163500960424     C     KART          CHAIN     FNBLT27L                           32
163600960424    2C     *IN32         IFEQ      *OFF
163700960424     C*
163800960424    3C     BLTDCM        IFGT      0
163900960927     C     BLTDET        ORGT      0
164000960524     C**
164100960524     C     KBLP          CHAIN     FNBLP01L                           32
164200960524     C**
164300970127    4C     BLTDCM        IFGT      0
164400970127     C     BLPCCA        ANDNE     '5'
164500151125     C     BLPCCA        ANDNE     '7'
164600151125     C     ANMDAO        ANDLE     BLTDCM
164700960424     C* SE HO CHIESTO SOLO LE APERTE ESCO
164800960524    5C     D34APR        IFEQ      'S'
164900960424     C                   SETON                                        10
165000960424     C                   GOTO      ENDCON
165100960524   X5C                   ELSE
165200960424     C* ALTRIMENTI DICO CHE E' CHIUSA COME CONSEGNATA
165300960524    6C     ANMDCH        IFEQ      0
165400960424     C                   MOVE      BLTDCM        ANMDCH
165500960524    6C                   ENDIF
165600960524     C                   MOVEL     'CO'          ANMCCH
165700960524    5C                   ENDIF
165800960424    4C                   ENDIF
165900960424    3C                   ENDIF
166000960424     C*
166100960424     C* SE ANOMALIA 10 NON STAMPO SE LA SPEDIZIONE C'E' ED E' TRASMESSA
166200960424    3C     ANMCAA        IFEQ      10
166300960424     C     KBLP          CHAIN     FNBLP01L                           32
166400960509     C  N32              MOVEL     BLPZNC        PRTZNC            2
166500160908
166600151125    4C  N32BLPFT1        IFeq      'T'
166700960424     C* SE HO CHIESTO SOLO LE APERTE ESCO
166800960424    5C     D34APR        IFEQ      'S'
166900960424     C                   SETON                                        10
167000960424     C                   GOTO      ENDCON
167100960424   X5C                   ELSE
167200960424    6C     ANMDCH        IFEQ      0
167300960424     C                   Z-ADD     BLPMGS        ANMDCH
167400990512     C                   MOVEL     BLPAAS        ANMDCH
167500960424    6C                   ENDIF
167600960424    5C                   ENDIF
167700160908     c
167800960424    4C                   ENDIF
167900960424    3C                   ENDIF
168000960508     C**
168100960508     C** SE ANOMALIA 5 STAMPO SE E' ANDATA IN DISGUIDO
168200960508    3C     ANMCAA        IFEQ      005
168300040617     c
168400970604     C* PRIMA VERIFICO SE PER IL SEGNACOLLO ESISTE UN'ALTRA SPUNTA
168500970604     C* PARTENZA CON DATA > PERCHE' IN QUESTO CASO L'ANOMALIA
168600970604     C* NON E' DA STAMPARE
168700970604   3AC     ANMCDU        IFEQ      SIMFEL
168800970604     C* DETERMINO DATA DEL FOGLIO DELL'ANOMALIA
168900970604    4C     ANMNFV        IFNE      SAVNFV
169000970604     C                   MOVE      ANMNFV        SAVNFV
169100970604     C                   CLEAR                   DSLV53
169200970604     C                   MOVEL     'G'           D53TFO
169300970604     C                   Z-ADD     SIMFEL        D53FEL
169400970604     C                   Z-ADD     ANMNFV        D53NFV
169500020926     C                   Z-ADD     ANMcdu        D53FGS
169600970604     C                   CALL      'FNLV53R'
169700970604     C                   PARM                    DSLV53
169800970604     C                   MOVEL     '1'           WCL53             1
169900970604     C                   Z-ADD     D53DFV        W53DFV
170000970604    4C                   END
170100970603     C*
170200970603     C                   Z-ADD     1             KNPG
170300140618     C     KBRV3A        SETLL     FNBRV05L
170400140618     C     KBRV3A        READE     FNBRV05L                               32
170500970603    4C     *IN32         DOWEQ     *OFF
170600160623
170700970604   4AC     BRVNFV        IFNE      ANMNFV
170800160623     c* Solo se spunta reale lo eseguo
170900160623   4Bc                   if        %lookup(brvnps:sps)>0
171000970603     C                   CLEAR                   DSLV53
171100970604     C                   MOVEL     'G'           D53TFO
171200970603     C                   Z-ADD     SIMFEL        D53FEL
171300970603     C                   Z-ADD     BRVNFV        D53NFV
171400020926     C                   Z-ADD     BRVfgs        D53FGS
171500970603     C                   CALL      'FNLV53R'
171600970603     C                   PARM                    DSLV53
171700970603     C                   MOVEL     '1'           WCL53             1
171800980424     C* QUESTO CONTROLLO LO IGNORO SE SONO IN TRASFERIMENTO
171900980424    5C     D34TRA        IFNE      'S'
172000980424     C     D53ERR        ANDEQ     *BLANKS
172100970604     C     D53DFV        ANDGT     W53DFV
172200970603     C                   SETON                                        10
172300970603     C                   GOTO      ENDCON
172400970603    5C                   END
172500160623   4BC                   END
172600970604   4AC                   END
172700140618     C     KBRV3A        READE     FNBRV05L                               32
172800970603    4C                   ENDDO
172900970604   3AC                   ENDIF
173000970603     C*
173100960927     C                   EXSR      DISPAR
173200960927    3C                   ENDIF
173300960927     C**
173400960424     C* MEMORIZZO IL NUMERO DI SPEDIZIONE SE C'E'
173500960424     C                   MOVE      BLTAAS        ANMAAS
173600960424     C                   MOVE      BLTLNP        ANMLNP
173700960424     C                   MOVE      BLTNSP        ANMNSP
173800960424    2C                   ENDIF
173900960424    1C                   ENDIF
174000040617     C
174100040617     c* Per stampa colli di valore in arrivo, escludo anomalie se non
174200040617     c*  crerate dal p.o. gestione  SE STAMPA DEL TERMINAL
174300040617     c                   if        *in70 and wanom='A' AND ANMCAA=005 AND
174400040617     C                             *IN80
174500040617     c                   if        anmfle<>d34fgs
174600040617     C                   SETON                                        10
174700040617     C                   GOTO      ENDCON
174800040617     c                   endif
174900040617     c                   endif
175000960506     C* CODICE ANOMALIA
175100960508     C                   EXSR      LEGCAA
175200040615     c
175300040615     c* In USACDU escludo sempre le anomalie non IDD (in partenza sempre
175400040615     c*           lette per lnp)
175500040615     c                   if        wcdu='S' and §7caie='E' and §7cidd=' '
175600040615     C                   SETON                                        10
175700040615     C                   GOTO      ENDCON
175800040615    4C                   ENDIF
175900960419     C**
176000960506     C**
176100960419     C* 1) SE CONTROLLO NON PASSATO NON FACCIO NULLA
176200960419     C*
176300960416     C* SE DEVO USARE FLE, VERIFICO SE E' IN L6
176400961230    1C     WFLE          IFEQ      'S'
176500960416     C     ANMFLE        LOOKUP    L6                                     32
176600960416     C  N32              SETON                                            10
176700960417     C  N32              GOTO      ENDCON
176800961230     C*
176900970829    2C     WANOM         IFEQ      'A'
177000961017     C* VERIFICO ANCHE CHE NON SIA ANOMALIA CHE UTILIZZA
177100961017     C* SIA LNA CHE FLE CON LNA=FLE
177200970829    3C     §7CUEA        IFEQ      'E'
177300970829    4C     ANMLNA        IFEQ      ANMFLE
177400961017     C                   SETON                                        10
177500961017     C                   GOTO      ENDCON
177600970829    4C                   ENDIF
177700040413     C* SE E' IN L6  MA LO E' ANCHE LA LNA
177800961230     C*  ED E' UN TIPO ANOMALIA CHE VIENE STAMPATO  PER ENTRANBI
177900961230     C*  ESCLUDO PERCHE' L'HO GIA' STAMPATA
178000961230     C     ANMLNA        LOOKUP    L6                                     32
178100961230     C   32              SETON                                            10
178200961230     C   32              GOTO      ENDCON
178300970829    3C                   ENDIF
178400970829     C*
178500970829   X2C                   ELSE
178600970829     C* FLE UTILIZZATO IN PARTENZA
178700970829    3C     §7CUEP        IFEQ      'E'
178800970829     C* PRENDO O LNP O FLS
178900970829    4C     ANMLNP        IFGT      0
179000971125     C                   Z-ADD     ANMLNP        W0030             3 0
179100970829     C                   ELSE
179200970829     C                   Z-ADD     ANMFLS        W0030
179300970829    4C                   ENDIF
179400970829     C     W0030         LOOKUP    L1                                     32
179500970829     C   32              SETON                                            10
179600970829     C   32              GOTO      ENDCON
179700970829    3C                   ENDIF
179800970829     C**
179900970829    2C                   ENDIF
180000961230    1C                   ENDIF
180100960419     C**
180200960419     C* 2) SE CONTROLLO NON PASSATO NON FACCIO NULLA
180300960419     C**
180400960419     C* DATA APERTURA ANOMALIA DAL/AL
180500960419    1C     D34DLA        IFGT      0
180600960419    1C     D34ALA        ORGT      0
180700990512    2C     ANMDAO        IFLT      D34DLA
180800990512     C     ANMDAO        ORGT      D34ALA
180900960419     C                   SETON                                        10
181000960419     C                   GOTO      ENDCON
181100960419    2C                   ENDIF
181200960419    1C                   ENDIF
181300040615     c**
181400040615     c* 2a) Il controllo che faceva in USACDU lo faccio qui dopo aver
181500040615     c*     controllato la data
181600040615    1c                   if        wcdu='S'
181700040615     c
181800040615     c* cerco alla data anmdao se d34fgs è terminal di anmlnp
181900040615     c                   eval      kcaeepa='P'
182000040615    2c                   if        anmlnp>0
182100040615     c                   eval      kcaetfp=anmlnp
182200040615     c                   else
182300040615     c                   eval      kcaetfp=anmfls
182400040615    2c                   endif
182500040615     c                   eval      kcaetfe=d34fgs
182600040615     c                   eval      kcaedde=anmdao
182700040615     c     kcae          setll     azcae03l
182800040615     c     kcae2         readpe    azcae03l
182900040616     c
183000040616    2c                   dow       not %eof(azcae03l) and caeatb<>*blanks
183100040616     c     kcae2         readpe    azcae03l
183200040616    2c                   enddo
183300040616     c
183400040616    2c                   if        not %eof(azcae03l)
183500040615     c* se decorrente alla data anomalia --> p.o. della £1 di d34fgs escludo
183600040615    3c                   if        caedsc>=anmdao
183700040615     c                   seton                                        10
183800040615     c* Se Compreso tutto il periodo richiesto a video, escludo tutta la
183900040615     c*  lnp e quindi faccio SETGT in usacdu
184000040615    4c                   if        caedde<=d34dla  and
184100040615     c                             caedsc>=d34ala
184200040615     c                   eval      wsetgt='S'
184300040615    4c                   endif
184400040615     c
184500040615    3c                   endif
184600040615    2c                   endif
184700040615     c   10              goto      endcon
184800040615     c*
184900040615     c* Escludo anche dalla stampa anomalie con anmlna 2 livello di
185000040616     c*  d34fgs per non stampare anomalie di arrivo e  non di transito
185100040615    2c                   if        anmlna=d34fgs
185200040615     C                   SETON                                        10
185300040615     C                   GOTO      ENDCON
185400040616   x2c                   else
185500040616     c
185600040616     c                   clear                   wanomok           1
185700040616     c* Controllo il terminal di arrivo per sta anomalia
185800040616     c                   eval      kcaeepa='A'
185900040616    3c                   if        anmlnp>0
186000040616     c                   eval      kcaetfp=anmlnp
186100040616     c                   else
186200040616     c                   eval      kcaetfp=anmfls
186300040616    3c                   endif
186400040616     c                   eval      kcaetfa=anmlna
186500040616     c                   eval      kcaedde=anmdao
186600040616     c
186700040616     c                   EXSR      CercaCAE
186800040616     c
186900040616     c* Se non ho trovato eccezione terminal, cerco terminal normale
187000040616    3c                   if        wanomok=' '
187100040616     c                   clear                   kcaetfp
187200040616     c                   EXSR      CercaCAE
187300040616    3c                   endif
187400040616    c
187500040616     c* anomalia da non stampare
187600040616    3c                   if        wanomok='N'
187700040616     C                   SETON                                        10
187800040616     C                   GOTO      ENDCON
187900040616    3c                   endif
188000040616     c
188100040616    2c                   endif
188200040615    1c                   endif
188300960419     C**
188400960419     C* 3) SE CONTROLLO NON PASSATO NON FACCIO NULLA
188500960419     C**
188600960419     C* SE NON STO ELABORANDO PER FLE ESCLUDO LE ANOMALIE FLE
188700970829     C     WFLE          IFEQ      'N'
188800960419     C     ANMCAA        LOOKUP    FLE                                    10
188900960419     C   10              GOTO      ENDCON
189000960419     C                   ENDIF
189100960419     C**
189200960419     C* 4) SE E COMUNQUE ANOMALIE ESTERNA ANCHE SE NO IDD LA TRASMETTO
189300960419     C**
189400960327     C* SE HO RICHIESTO LE ESTERNE O LE RICEVUTE, VEDO SE
189500960327     C*  SONO QUELLE IDD O NO
189600960327     C     D34IE         IFNE      'I'
189700960327     C                   MOVEL     ANMCAA        W003A             3
189800960506     C     §7CIDD        IFEQ      'S'
189900960506     C                   SETON                                        97
190000960506     C                   ENDIF
190100960404     C* 97 ON - E' ANOMALIA IDD
190200960404    2C     *IN97         IFEQ      *ON
190300960506    3C     D34IE         IFEQ      'N'
190400960327     C                   SETON                                        10
190500960327     C                   GOTO      ENDCON
190600960327    3C                   ENDIF
190700960327     C*
190800960327   X2C                   ELSE
190900960404     C* 97 OFF  - NO IDD
191000960506    3C     D34IE         IFEQ      'D'
191100960327     C                   SETON                                        10
191200960419     C* 12 ON SE COMUNQUE ANOMALIA ESTERNA
191300040116     C   01aNMAIE        IFEQ      'E'
191400960419     C                   SETON                                        12
191500960419    3C                   ENDIF
191600960419     C*
191700960327     C                   GOTO      ENDCON
191800960327    3C                   ENDIF
191900960327    2C                   ENDIF
192000960327    1C                   ENDIF
192100960419     C**
192200960419     C* 5) SE ANOMALIA IDD CHIUSA TRASMETTO SENZA STAMPARE
192300960419     C**
192400960419     C* STAMPA SOLO ANOMALIE APERTE
192500961217     C* PER LE ANOMALIE LOCALI 70 E 71 FACCIO UN GIRO A PARTE
192600040116    0C**   *IN07         IFEQ      *OFF
192700040116     C**   *IN07         OREQ      *ON
192800040116     C**   ANMCAA        ANDNE     70
192900040116     C**   ANMCAA        ANDNE     71
193000040116     c
193100040116     c                   if        wanom='A'  or
193200040116     c                             wanom='P' and anmcaa<>70 and anmcaa<>71
193300961217     C*
193400960419    1C     D34APR        IFEQ      'S'
193500960419     C     ANMDCH        ANDNE     0
193600960419     C                   SETON                                        10
193700960419     C   01ANMAIE        IFEQ      'E'
193800960419     C                   SETON                                        12
193900960419     C                   ENDIF
194000960419     C                   GOTO      ENDCON
194100960419    1C                   ENDIF
194200960419     C* SOLO ANOMALIE CHIUSE
194300960419    1C     D34APR        IFEQ      'C'
194400960419     C     ANMDCH        ANDEQ     0
194500960419     C                   SETON                                        10
194600960419     C                   GOTO      ENDCON
194700960419    1C                   ENDIF
194800961217     C*
194900961217   X0C                   ELSE
195000961217     C* NON TESTO LA DATA DI CHIUSURA MA LA CASUALE
195100961217    1C     D34APR        IFEQ      'S'
195200961217    2C     ANMCCH        IFNE      '  '
195300961217     C                   SETON                                        10
195400961217   X2C                   ELSE
195500961217     C*
195600961217     C* PRIMA DI STAMPARLA VERIFICO SE CE N'E' UN'ALTRA COM DATA >
195700961217     C*                    MI SALVO LA DATA
195800990512     C                   MOVEL     ANMDAO        W0080             8 0
195900990512     C                   MOVEL     ANMAAS        KAAS2
196000961217     C                   MOVEL     ANMLNP        KLNP2
196100961217     C                   MOVEL     ANMNRS        KNRS2
196200961217     C                   MOVEL     ANMNSP        KNSP2
196300961217     C                   Z-ADD     ANMCAA        KCAA
196400961217     C*
196500961217     C* LEGGO IL COLLO FINTANTO CHE TROVO DELLE ANOMALIE INSERITE
196600961217     C*  NEL CASO STAMPERO' QUELLA
196700990512     C     KANM1         SETLL     FNANM01L
196800990512     C     KANM1         READE     FNANM01L                               31
196900961217    3C     *IN31         DOWEQ     *OFF
197000990512     C     ANMDAO        ANDLE     W0080
197100990512     C     KANM1         READE     FNANM01L                               31
197200961217    3C                   ENDDO
197300961217     C*
197400961217    3C     *IN31         IFEQ      *ON
197500990512     C   27ANMNR7        CHAIN(N)  FNANM000                           33
197600990512     C   28ANMNR8        CHAIN(N)  FNANM000                           33
197700961217     C*
197800961217     C                   CLEAR                   ANMDCH
197900961217     C                   CLEAR                   ANMFSC
198000961217   X3C                   ELSE
198100961217     C                   SETON                                        10
198200961217     C                   GOTO      ENDCON
198300961217    3C                   ENDIF
198400961217    2C                   ENDIF
198500961217    1C                   ENDIF
198600961217     C*
198700961217    1C     D34APR        IFEQ      'C'
198800961217     C     ANMCCH        ANDEQ     '  '
198900961217     C                   SETON                                        10
199000961217     C                   GOTO      ENDCON
199100961217    1C                   ENDIF
199200961217     C*
199300961217    0C                   ENDIF
199400960419     C**
199500960426     C* 7) TUTTI GLI ALTRI CONFRONTI, TRANNE IL LIVELLO IDD NON FANNO
199600960419     C*    NE STAMPARE NE TRASMETTERE
199700960419     C**
199800960327     C* DATA CHIUSURA ANOMALIA DAL/AL
199900960927     C* PER LE ANOMALIE NON IDD3
200000960927     C     *IN05         IFEQ      *ON
200100960927     C     ANMCCH        ANDEQ     'PR'
200200960927     C     ANMLNA        LOOKUP    FM1                                    32
200300960927     C                   ENDIF
200400960927     C*
200500960927    0C     *IN05         IFEQ      *OFF
200600960927     C     ANMCCH        ORNE      'PR'
200700960927     C     *IN32         OREQ      *OFF
200800960927     C*
200900960327    1C     D34DCA        IFGT      0
201000960419     C     D34ACA        ORGT      0
201100990512    2C     ANMDCH        IFLT      D34DCA
201200990512     C     ANMDCH        ORGT      D34ACA
201300960327     C                   SETON                                        10
201400960327     C                   GOTO      ENDCON
201500960327    2C                   ENDIF
201600960327    1C                   ENDIF
201700960927     C*
201800960927   X0C                   ELSE
201900990512    1C     ANMDCH        IFLT      W08DM1
202000990512     C     ANMDCH        ORGT      W08AM1
202100960927     C                   SETON                                        10
202200960927     C                   GOTO      ENDCON
202300960927    1C                   ENDIF
202400960927    0C                   ENDIF
202500921015     C*
202600921015     C* STAMPA FASE SPECIFICA
202700960327    1C     D34FAS        IFNE      ' '
202800960326     C     ANMFSA        ANDNE     D34FAS
202900921015     C                   SETON                                        10
203000921015     C                   GOTO      ENDCON
203100960327    1C                   ENDIF
203200960426     C*
203300960506     C* STAMPA CAUSALE CHIUSURA SPECIFICA
203400960426    1C     D34CCH        IFNE      ' '
203500960426     C     ANMCCH        ANDNE     D34CCH
203600960426     C                   SETON                                        10
203700960426     C                   GOTO      ENDCON
203800960426    1C                   ENDIF
203900961108     C*
204000040116     C* in partenza escludo le anomalie appena create (livello 0 o 2
204100040616     c*  per anom 15) solo per le anomalie IDD e se non prevista la
204200040616     c*  stampa dalla DS D34SL0
204300040116     C** 07ANMLID        IFEQ      0
204400040616     c                   if        d34sl0=' '
204500040119     c                   if        anmlid=§7clid  and §7cidd='S' and
204600040119     c                             wanom='P' and §7cinv=' '
204700961108     C                   SETON                                        10
204800961108     C                   GOTO      ENDCON
204900961108    1C                   ENDIF
205000040616    1C                   ENDIF
205100960419     C*
205200960506     C* STAMPA CODICI ANOMALIE RICHIESTE
205300960327    1C     D34ANM        IFNE      *BLANKS
205400921015     C                   MOVEL     ANMCAA        ALF3              3
205500921015     C     ALF3          LOOKUP    ANM                                    31
205600960327    2C     D34INC        IFEQ      'I'
205700921015     C  N31              SETON                                        10
205800921015     C  N31              GOTO      ENDCON
205900921015     C*
206000960327   X2C                   ELSE
206100921015     C*
206200921015     C   31              SETON                                        10
206300921015     C   31              GOTO      ENDCON
206400960327    2C                   END
206500960402     C*
206600960402     C                   ELSE
206700960402     C*
206800960402     C* SE NON HO RICHIESTO UNO SPECIFICO CODICE ANOMALIA
206900960402     C*  GESTISCO SOLO QUELLI STAMPABILI
207000960402     C     §7CSTA        IFNE      'S'
207100960402     C                   SETON                                        10
207200960402     C                   GOTO      ENDCON
207300960506    2C                   ENDIF
207400960506    1C                   ENDIF
207500921015     C*
207600921015     C* NUMERO FOGLIO VIAGGIO
207700960327    1C     D34NFV        IFNE      0
207800960326     C     ANMNFV        ANDNE     D34NFV
207900921015     C                   SETON                                        10
208000921015     C                   GOTO      ENDCON
208100960327    1C                   ENDIF
208200960506     C*
208300960506     C* SE ANOMALIA INTERNA VEDO SE RICHIESTO PART O ARR
208400960506    1C     ANMAIE        IFEQ      'I'
208500960506     C     D34PAR        ANDNE     ' '
208600960506     C*
208700960506     C* RICHIESTE LE ANOMALIE PARTENZA MA QUESTA E' ANOMALIA ARRIVO
208800960506    2C     D34PAR        IFEQ      'P'
208900960506     C     §7CPAR        ANDEQ     'A'
209000960506     C                   SETON                                        10
209100960506    2C                   ENDIF
209200960506     C* VICEVERSA
209300960506    2C     D34PAR        IFEQ      'A'
209400960506     C     §7CPAR        ANDEQ     'P'
209500960506     C                   SETON                                        10
209600960506    2C                   ENDIF
209700960506     C*
209800960506     C   10              GOTO      ENDCON
209900960506    1C                   ENDIF
210000040615     c
210100960403     C* DAL LIVELLO IDD
210200980327     C     *IN97         IFEQ      *ON
210300980327     C     *IN01         ANDEQ     *ON
210400980327     C     D34TRA        OREQ      'S'
210500000317     C     D34GES        OREQ      'V'
210600990114     C   24
210700990512     COR 25ANMN10        CHAIN     FNANM000                           33
210800000306     C   26ANMNR3        CHAIN     FNANM000                           33
210900000306     C   27ANMNR7        CHAIN     FNANM000                           33
211000990512     C   28ANMNR8        CHAIN     FNANM000                           33
211100990512     C   29ANMNR9        CHAIN     FNANM000                           33
211200960403     C                   ENDIF
211300960403     C*
211400960404    1C   97D34ELA        IFNE      ' '
211500960403     C                   ADD       1             ANMLID
211600960403     C                   ENDIF
211700960403     C**
211800960405     C   97ANMLID        IFLT      D34LID
211900960405     C* SE NON E' ELABORAZIONE DEFINITIVA IDD 10 ON
212000960405     C     D34ELA        IFNE      'S'
212100960403     C                   SETON                                        10
212200960417     C                   GOTO      ENDCON
212300960405     C                   ELSE
212400960419     C* ALTRIMENTI ACCENDO L' 11  PERCHE' DEVO AGGIORNARE SENZA STAMPAR
212500960405     C                   SETON                                        11
212600960405     C                   ENDIF
212700960405     C*
212800960403    2C                   ENDIF
212900960412     C*
213000960327     C     ENDCON        ENDSR
213100040616     c     CercaCAE      begsr
213200040616     c     kcae1         setll     azcae01l
213300040616     c     kcae3         readpe    azcae01l
213400040616    3c                   dow       not %eof(azcae01l) and caeatb<>*blanks
213500040616     c     kcae3         readpe    azcae01l
213600040616    3c                   enddo
213700040616     c
213800040616    3c                   if        not %eof(azcae01l)
213900040616    4c                   if        caedde<=anmdao   and caedsc>=anmdao
214000040616     c* Trovata eccezione terminal --> se il terminal  d34fgs esco
214100040616    5c                   if        caetfe=d34fgs
214200040616     c* Anomalia da non stampare
214300040616     c                   eval      wanomok='N'
214400040616     c                   else
214500040616     c* Anomalia da     stampare
214600040616     c                   eval      wanomok='S'
214700040616    5c                   endif
214800040616    4c                   endif
214900040616    3c                   endif
215000040616     c                   ENDSR
215100960927     C*
215200960927     C* CONTROLLO SE C'E' DISGUIDO IN LETTURA ANOMALIE PARTENZA ------*
215300960930     C     DISPAR        BEGSR
215400971124     C* VEDO SE E' PRESENTE UNA ANOMALIA DI DISGUIDO
215500990409     C                   SETOFF                                       20
215600990409     C                   DO        2
215700990409     C                   Z-ADD     56            KCAA
215800990409     C     *IN20         IFEQ      *OFF
215900990512     C     KANM2         CHAIN     FNANM02L                           32
216000960927    1C     *IN32         DOWEQ     *OFF
216100971126    2C     AN2DAO        IFGT      WDDET
216200960927     C                   SETON                                        20
216300960927     C                   MOVEL     CDISG         PSTAM
216400960927     C                   MOVEL     AN2FLE        WDFLP
216500971126     C                   Z-ADD     AN2DAO        WDDET
216600960927    2C                   ENDIF
216700990512     C     KANM2         READE     FNANM02L                               32
216800960927    1C                   ENDDO
216900990409     C*
217000990409     C                   Z-ADD     55            KCAA
217100990409    2C                   ENDIF
217200990409    1C                   ENDDO
217300960927     C*
217400960927     C* SE NON TROVATA CONTROLLO BLTFLP
217500960927    1C     *IN20         IFEQ      *OFF
217600960927     C     BLTDET        ANDGT     0
217700960927     C**
217800960927     C* CHAIN IL FV
217900020807     C     K02FGV01      CHAIN     FNFGV01L                           32
218000960927    4C     *IN32         IFEQ      *OFF
218100020807     C     K02FGV01      CHAIN     FNFGW01L                           32
218200000503    5C     *IN32         IFEQ      *ON
218300000503     C     FGWATB        ORNE      ' '
218400000503     C                   CLEAR                   FGWFF3
218500000503     C                   CLEAR                   FGWFF4
218600000503     C                   CLEAR                   FGWFL3
218700000503     C                   CLEAR                   FGWFL4
218800000503    5C                   ENDIF
218900960927     C                   MOVEL     BLTFLP        W003A             3
219000960930     C                   MOVEL     ANMLNA        W003AA            3
219100961001     C                   Z-ADD     0             XX                3 0
219200961001     C                   SETON                                        32
219300961001     C*
219400961001    5C     *IN32         DOWEQ     *ON
219500961001     C     PSTAM         ANDEQ     *BLANKS
219600961001     C                   ADD       1             XX
219700960927     C     W003A         LOOKUP    FLP(XX)                                32
219800960927     C**
219900961001    6C     *IN32         IFEQ      *ON
220000960930     C     FFV(XX)       ANDEQ     W003AA
220100960930     C                   MOVEL     CTRANS        PSTAM            18
220200961001    6C                   ENDIF
220300961001    5C                   ENDDO
220400961001     C**
220500961001    5C     PSTAM         IFEQ      *BLANKS
220600960927     C                   MOVEL     CDISG         PSTAM
220700960927    5C                   ENDIF
220800960927   X4C                   ELSE
220900960927     C                   MOVEL     CDISG         PSTAM
221000960927    4C                   ENDIF
221100960927     C                   SETON                                        20
221200960927     C                   MOVEL     BLTFLP        WDFLP
221300960927     C                   MOVEL     BLTDET        WDDET
221400960927    1C                   ENDIF
221500960927     C                   ENDSR
221600960508     C*
221700960508     C* CHAIN SULLE TABELLE DELL'ANOMALIA ---------------------------*
221800960508     C     LEGCAA        BEGSR
221900960508     C     ANMCAA        IFNE      SAVCAA
222000960508     C                   MOVEL     '7C'          COD
222100960508     C                   MOVEL(P)  ANMCAA        KEY
222200960508     C     KTAB          CHAIN(N)  TABEL                              31
222300960508     C  N31              MOVEL     TBLUNI        DS7C
222400960508     C   31              CLEAR                   DS7C
222500960508     C                   MOVEL     ANMCAA        SAVCAA
222600960508     C                   ENDIF
222700960508     C                   ENDSR
222800911015     C*
222900960827     C* CHIUDO ANOMALIE DI DISGUIDO O ANOMALIE 50 SE ESISTE BLP ------*
223000960827     C     A50A55        BEGSR
223100021025     C                   SETOFF                                       32
223200021025     C     KART          CHAIN     FNBLT27L                           39
223300021025     C  N39BLTLNP        LOOKUP    L1                                     32
223400021025    2C     *IN32         IFEQ      *ON
223500960705     C* TROVATA BOLLA PARTENZA:
223600990409     C* CHIUDO ANOMALIA 55 O 56
223700960705     C                   MOVE      'N'           WTRM              1
223800960705     C                   CLEAR                   DSLR33
223900960705     C                   MOVE      ANMDAO        D33DCH
224000960705     C                   MOVEL     'M'           D33FSC
224100960705     C                   MOVEL     'AN'          D33CCH
224200960705     C                   MOVEL     'S'           D33MAC
224300990114     C   24
224400990114     COR 25              MOVE      ANMN10        D33NRR
224500000306     C   26              MOVE      ANMNR3        D33NRR
224600000306     C   27              MOVE      ANMNR7        D33NRR
224700960709     C   28              MOVE      ANMNR8        D33NRR
224800960709     C   29              MOVE      ANMNR9        D33NRR
224900960705     C                   CALL      'FNLR33R'
225000960705     C                   PARM                    DSLR33
225100960827     C* SE ANOMALIA 55:
225200960827    3C     ANMCAA        IFEQ      55
225300990409    3C     ANMCAA        OREQ      56
225400960705     C* CHIUDO ANOMALIA 200
225500960709     C                   MOVE      KCAA          WCAA
225600960705     C                   Z-ADD     200           KCAA
225700990512     C     KANM2         CHAIN     FNANM02L                           32
225800960705     C     *IN32         IFEQ      *OFF
225900960705     C                   CLEAR                   DSLR33
226000960705     C                   MOVE      AN2DAO        D33DCH
226100960705     C                   MOVEL     'AN'          D33CCH
226200960705     C                   MOVE      ANMNR2        D33NRR
226300960705     C                   CALL      'FNLR33R'
226400960705     C                   PARM                    DSLR33
226500960705     C                   END
226600960709     C                   MOVE      WCAA          KCAA
226700960827    3C                   END
226800960705    2C                   END
226900960705     C                   ENDSR
227000960705     C*
227100911021     C* STAMPO ANOMALIE ---------------------------------------------*
227200160908     C     STAMPA        BEGSR
227300151125     c                   setoff                                       434445
227400160908     c                   setoff                                       81
227500151125     c                   clear                   wdcm
227600960405     C* PER IDD
227700911025     C* SE DIVENTA 3 LA CHIUDO CON CAUSALE PR = PRATICA
227800960405    1C   97D34ELA        IFNE      ' '
227900960403    2C     ANMLID        IFEQ      §7ALCI
228000990512     C                   MOVEL     D34ALA        ANMDCH
228100911025     C                   MOVEL     'PR'          ANMCCH
228200921027     C                   MOVEL     'D'           ANMFSC
228300960327    2C                   ENDIF
228400960327    1C                   ENDIF
228500160908     c
228600160908     c* se si tratta di un manca record, verifico se bolla in partenza da partire
228700170119    1c****               if        anmcaa=010
228701170119     c* Per tutte le anomalie su cui è presente la key segnacollo,
228702170119     c* cerco la bolla per memorizzarla sull'anomalia
228703170119    1c                   if        anmscn>0
228800160908    2c                   if        anmnsp=0
228900160908     C     KART          CHAIN     FNBLT27L
229000160908    3c                   if        %found(fnblt27l)
229100160908     c                   z-add     bltnsp        anmnsp
229200160908     c                   z-add     bltlnp        anmlnp
229300160908     c                   z-add     bltaas        anmaas
229400160908    3c                   endif
229500160908    2c                   endif
229600160908     c
229700160908     c     kanm          chain     fnblp01l
229800160908    2c                   if        %found(fnblp01l) and blpft1=' '
229900160908     c                   seton                                        81
230000160908    2c                   endif
230100160908    2c                   if        not %found(fnblp01l)
230200160908     c                   clear                   anmnsp
230300160908     c                   clear                   anmlnp
230400160908     c                   clear                   anmaas
230500160908    2c                   endif
230600160908    1c                   endif
230700960514     C*
230800960514     C* SE IL SOGGETTO ANOMALIA E' "E" --> NON STAMPO L'ANOMALIA
230900960515     C*                                    SE IDD IN ARRIVO
231000040116    c
231100040116    3C     ANMSAN        IFNE      'E'
231200040116     C     wanom         ANDEQ     'A'
231300040116     C     ANMAIE        ORNE      'E'
231400040116     C     wanom         orne      'A'
231500911025     C*
231600911021     C* NUMERO SPEDIZIONE
231700911021     C     ANMNSP        COMP      0                                  0202
231800911021     C* NUMERO SEGNACOLLO
231900970314     C     ANMFLS        COMP      0                                  0303
232000960417     C**
232100960417     C* SE C'E' IL NUMERO DI SPEDIZIONE, VADO A PRENDERE LA DATA
232200960514    2C     ANMNSP        IFGT      0
232300960417     C                   SETOFF                                       33
232400960417     C                   CLEAR                   G02INV
232500960417     C*
232600960417     C     KANM          CHAIN     FNBLP01L                           33
232700960514    4C     *IN33         IFEQ      *OFF
232800960509     C                   MOVEL     BLPZNC        PRTZNC
232900960417     C                   Z-ADD     BLPMGS        G02INV
233000971126     C                   MOVEL     BLPAAS        G02INV
233100050503     c* Se c'e' particolarità consegna CP, la stampo
233200050503     c                   if        blpgma='CP'
233300050503     c                   seton                                        43
233400050503     c                   endif
233500151125     c* Se consegna anomala 7 stampo
233600151125     c                   if        blpcca='7' and blpdcm>0
233700151125     c                   seton                                        45
233800151125     c                   movel     blpdcm        wdcm              8 0
233900151125     c                   endif
234000151125     c
234100051221     c* Se richiesto stampo dati mittente e destinatario
234200051221     c                   if        d34med = 'S' and d34ela = ' '
234300051221     c                   seton                                        44
234400051221     c                   movel     blprsm        prtrsm           32
234500051222     c* rmn : lo stampo eliminando gli spazi a sinistra non significativi
234600051222     c                   move      blprmn        prtrmn           15
234700051222     c     ' 0'          check     prtrmn        N                 2 0
234800051222     c                   eval      prtrmn = %subst(prtrmn:N)
234900051221     c                   endif
235000050503     c
235100960514    4C                   ENDIF
235200960417     C*
235300961018     C                   CLEAR                   PRTDSP
235400960514    3C     G02INV        IFGT      0
235500960417     C                   MOVEL     3             G02ERR
235600971126     C                   CALL      'XSRDA8'
235700960417     C                   PARM                    WLBDAT
235800960417     C                   MOVEL     G02DAT        PRTDSP            4 0
235900960514    3C                   ENDIF
236000151125     c*
236100151125    3c                   if        wdcm>0  and *in45
236200151125     c                   eval      g02inv=wdcm
236300151125     C                   MOVEL     3             G02ERR
236400151125     C                   CALL      'XSRDA8'
236500151125     C                   PARM                    WLBDAT
236600151125     C                   MOVEL     G02DAT        PRTDcm            4 0
236700151125    3C                   ENDIF
236800151125    2C                   ENDIF
236900960508     C*
237000960508     C* PER L'ANOMALIA 005 VERIFICO SE IL COLLO E' ANDATO IN DISGUIDO
237100040116     C***  ANMAIE        IFNE      'R'
237200040116     C     ANMCAA        ifeq      005
237300960508     C                   EXSR      ANM005
237400960514    2C                   ENDIF
237500960927     C*
237600960508     C* SE HO TROVATO IL DISGUIDO LO STAMPO
237700960514    2C     *IN20         IFEQ      *ON
237800960508     C                   MOVE      WDDET         G02INV
237900960508     C                   MOVEL     3             G02ERR
238000971126     C                   CALL      'XSRDA8'
238100960508     C                   PARM                    WLBDAT
238200960508     C                   MOVEL     G02DAT        PRTDET            4 0
238300960514    2C                   ENDIF
238400911021     C* TIPO ANOMALIA
238500960514    2C     ANMTAN        IFNE      SAVTAN
238600911021     C                   MOVEL     '7F'          COD
238700960327     C                   MOVEL(P)  ANMTAN        KEY
238800960409     C     KTAB          CHAIN(N)  TABEL                              31
238900040616     C   31              MOVEL     *BLANKS       tbluni
239000040616     c* Vedo semettere o meno il tipo di anomalia (Idd/int/no idd)
239100040616     c                   if        *in17
239200040616     C                   MOVEL     TBLUNI        DESTAN           12
239300040616     c                   else
239400040616     c                   movel     tbluni        w008a             8
239500040616     c                   endif
239600921015     C*
239700921015     C                   MOVEL     ANMTAN        SAVTAN            1
239800960514    2C                   END
239900040616     c*
240000040616     c                   if        not *in17
240100040616     c                   clear                   destan
240200040616     C* ANOMALIA
240300040616    2C     ANMAIE        IFEQ      'I'
240400040616     C                   MOVEL     'INT'         DESAIE            3
240500040616   X2C                   ELSE
240600040616     C**
240700040616    3C     §7CIDD        IFEQ      'S'
240800040616     C                   MOVEL     'IDD'         DESAIE                         IDD
240900040616     C                   ELSE
241000040616     C                   MOVEL     'NON'         DESAIE                         NON IDD
241100040616    3C                   ENDIF
241200040616    2C                   ENDIF
241300040616     c                   movel     desaie        destan
241400040616     c                   move      w008a         destan
241500040616     c                   endif
241600960506     C*
241700960506     C* DESCRIZIONE CODICE ANOMALIA
241800960506     C                   MOVEL     §7CDES        DESCAA           25
241900960704     C*
242000960705     C* SE ANOMALIA 10-MANCA RECORD BOLLA ARRIVO O 138-COLLO GIA' CONS.
242100960705     C* CONTROLLO SE COLLO SPARATO IN INVENTARIO O IN CONSEGNA
242200960704     C                   SETOFF                                       21
242300960704     C     ANMCAA        IFEQ      10
242400960705     C     ANMCAA        OREQ      138
242500960704     C     ANMLNA        LOOKUP    L6                                     32
242600960704     C     *IN32         IFEQ      *ON
242700960704     C                   EXSR      RCRSPU
242800960704     C                   END
242900960704     C                   END
243000960402     C*
243100950320     C* SE BOLLA IN CONSEGNA REPERISCO NR/DATA DISTINTA E PADRONCINO
243200950320     C                   SETOFF                                       09
243300960514    2C     ANMCAA        IFEQ      135
243400950321     C     KART          CHAIN     FNART27L                           31
243500960514    3C     *IN31         IFEQ      *OFF
243600950320     C     KARB          CHAIN     FNARB01L                           31
243700960514    4C     *IN31         IFEQ      *OFF
243800960509     C                   MOVEL     ARBZNC        PRTZNC
243900960509     C*
244000960514    5C     ARBNDC        IFNE      0
244100950320     C                   SETON                                        09
244200950320     C*
244300950320     C                   MOVEL     DESCAA        DESCA1           18
244400950320     C                   MOVE      ':'           DESCA1
244500950320     C* Data distinta
244600950320     C                   MOVE      ARBDDC        G02INV
244700950320     C                   MOVEL     3             G02ERR
244800971126     C                   CALL      'XSRDA8'
244900950320     C                   PARM                    WLBDAT
245000950320     C                   MOVEL     G02DAT        STADDC            4 0
245100950320     C* Padroncino
245200950320     C                   MOVE      *BLANKS       STAPDC           12
245300021203     C*    ARBPDC        CHAIN     FNAPD01L                           32
245400021205     C                   move      arbpdc        kpdpdr
245500021203     C                   move      'A'           kpdtip
245600021203     C     kfapd         chain     fiapd01L                           32
245700960327     C  N32              MOVEL     APDRSC        STAPDC
245800960514    5C                   END
245900960514    4C                   END
246000960514    3C                   END
246100960514    2C                   END
246200990121     C**
246300990121     C** NUMERO CA SE C'E'
246400990122     C                   SETOFF                                       404142
246500990121     C     KART          CHAIN     FNDCD02L                           32
246600990121    2C     *IN32         IFEQ      *OFF
246700990121     C     DCDATB        ANDEQ     ' '
246800990121     C     KDCT          CHAIN     FNDCT01L                           32
246900050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
247000050221     c                   If        Not *In32 and dctdt2 > 0
247100050221     c                   Eval      *In32 = *On
247200050221     c                   EndIf
247300990121     C* CONTROLLO LA CAUSALE DI CHIUSURA SE C'E'
247400990121    3C  N32DCTDCH        IFGT      0
247500990121     C     DCTCCH        ANDNE     ' '
247600990121     C                   CLEAR                   DSBS02
247700990121     C                   MOVEL     'C'           T02MOD
247800990121     C                   MOVEL     KNSIF         T02SIF
247900990121     C                   MOVEL     'CCH'         T02COD
248000990121     C                   MOVEL     DCTCCH        T02KE1
248100990121     C                   CALL      'TIBS02R'
248200990121     C                   PARM                    KPJBA
248300990121     C                   PARM                    DSBS02
248400990121    4C     T02ERR        IFEQ      ' '
248500990121     C                   MOVEL     T02UNI        DCCH
248600990121     C                   ELSE
248700020905     ***C                   MOVEL     'S'           §CHCRA
248800020905     C                   MOVEL     'S'           §CCHCRAN
248900990121    4C                   ENDIF
249000990121    3C                   ENDIF
249100990121     C**
249200990121     C** STAMPO SE CHIUSA C.A. SENZA CAUSALE
249300990122    3C  N32DCTCCH        IFEQ      *BLANKS
249400990121     C     DCTDCH        ANDGT     0
249500990121     C** STAMPO SE CHIUSA C.A. CON CAUSALE PER CUI CREARE ANOMALIA
249600990121     C*  (SIGNIFICA CHE E' CA REALE)
249700020905     ***C     §CHCRA        OREQ      'S'
249800020905     C     §CCHCRAN      OREQ      'S'
249900990121     C     DCTDCH        ANDGT     0
250000990122     C* STAMPO SE C.A. APERTA E DETT C.A. APERTO
250100990122     C     DCTDCH        OREQ      0
250200990122     C     DCDDCH        ANDEQ     0
250300990122     C* DECODIFICA DANNI
250400990122     C                   EXSR      DECDAN
250500990122    3C                   ENDIF
250600990121    2C                   ENDIF
250700050503     c
250800050503     C* VERIFICO PER CP SE STAMPARE decodifica corta o lunga
250900050503     c                   clear                   decodicp         23
251000050503     c                   if        *in43
251100050503     c                   if        *in41
251200050503     c                   eval      decodicp=CCPcort
251300050503     c                   else
251400050503     c                   eval      decodicp=CCPlung
251500050503     c                   endif
251600050503     c                   endif
251700990122     C**
251800990122     C* SE NON E' SU C.A. PER LA 151 STAMPO LA 1 C.A. CHIUSA CHE TROVO
251900990122    2C     *IN32         IFEQ      *ON
252000990122     C     ANMNSP        ANDGT     0
252100990122     C     ANMCAA        ANDEQ     151
252200990122     C                   CLEAR                   WTRDAN
252300990122     C**
252400040811      * Sostituisco la lettura del file FNDTC02 con
252500040811      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul e chiuse
252600040811      * legate alla spedizione che passo
252700040811      *
252800040811     c                   clear                   fidn12ds
252900040811     c                   eval      i12aas = anmaas
253000040811     c                   eval      i12lnp = anmlnp
253100040811     c                   eval      i12nrs = anmnrs
253200040811     c                   eval      i12nsp = anmnsp
253300040811     c                   eval      i12fel = 'E'
253400040811     c                   eval      i12fan = 'E'
253500040811      *
253600040811     c                   call      'FIDN12R'
253700040811     c                   parm                    fidn12ds
253800040811      *
253900040811      * se non ci sono errori
254000040811     c                   if        o12err = ' '
254100040811      * se numero di CA trovate maggiore di zero
254200040811     c                   if        o12nca > 0
254300040811      * prendo solo la prima CA chiusa
254400040811     c                   z-add     *zeros        jj                2 0
254500040811     c     1             do        o12nca        jj
254600040811     c                   if        skdch(jj) > 0
254700040811     c                   movel     'S'           wtrdan            1
254800040811     c                   movea     skkey(jj)     dskey
254900040811     C                   Z-ADD     dsfil         dctfil
255000040811     C                   Z-ADD     dsnca         dctnca
255100040811     C                   Z-ADD     skdca(jj)     dctdca
255200040811     C                   Z-ADD     skdch(jj)     dctdch
255300040811     c                   z-add     o12nca        jj
255400040811     c                   endif
255500040811     C                   ENDDO
255600040811      *
255700040811     c                   endif
255800040811     c                   endif
255900040811     C*
256000990122     C** TROVATA LA C.A. CHIUSA: STAMPO I DATI
256100990122     C     WTRDAN        IFEQ      'S'
256200990122     C                   EXSR      DECDAN
256300990122    3C                   ENDIF
256400990122    2C                   ENDIF
256500911021     C* FASE APERTURA
256600960514    2C     ANMFSA        IFNE      SAVFSA
256700911021     C                   MOVEL     '7D'          COD
256800960327     C                   MOVEL(P)  ANMFSA        KEY
256900960409     C     KTAB          CHAIN(N)  TABEL                              32
257000040616     C  N32              MOVEL     TBLUNI        DESFSA            8
257100960327     C   32              MOVEL     *BLANKS       DESFSA
257200921015     C*
257300921015     C                   MOVEL     ANMFSA        SAVFSA            1
257400960514    2C                   END
257500911021     C* DATA APERTURA
257600971126     C                   CLEAR                   G02INV
257700911021     C                   MOVE      ANMDAO        G02INV
257800911018     C                   MOVEL     3             G02ERR
257900971126     C                   CALL      'XSRDA8'
258000911018     C                   PARM                    WLBDAT
258100950320     C                   MOVEL     G02DAT        STADAO            4 0
258200911021     C* FASE CHIUSURA
258300960514    2C     ANMFSC        IFNE      SAVFSC
258400911021     C                   MOVEL     '7D'          COD
258500960327     C                   MOVEL(P)  ANMFSC        KEY
258600960409     C     KTAB          CHAIN(N)  TABEL                              32
258700040616     C  N32              MOVEL     TBLUNI        DESFSC            8
258800960327     C   32              MOVEL     *BLANKS       DESFSC
258900921015     C*
259000921015     C                   MOVEL     ANMFSC        SAVFSC            1
259100960514    2C                   END
259200960506     C*
259300911021     C* DATA CHIUSURA
259400960514    2C     ANMDCH        IFGT      0
259500971126     C                   CLEAR                   G02INV
259600911021     C                   MOVE      ANMDCH        G02INV
259700911021     C                   MOVEL     3             G02ERR
259800971126     C                   CALL      'XSRDA8'
259900911021     C                   PARM                    WLBDAT
260000950320     C                   MOVEL     G02DAT        STADCH            4 0
260100960417     C                   ELSE
260200960417     C                   CLEAR                   STADCH
260300960514    2C                   ENDIF
260400911021     C* CAUSALE CHIUSURA
260500960514    2C     ANMCCH        IFNE      SAVCCH
260600911021     C                   MOVEL     '7E'          COD
260700960417     C                   MOVEL(P)  ANMCCH        KEY
260800960409     C     KTAB          CHAIN(N)  TABEL                              31
260900960417     C  N31              MOVEL     TBLUNI        DESCCH           12
261000911021     C   31              MOVEL     *BLANKS       DESCCH
261100921015     C*
261200921015     C                   MOVEL     ANMCCH        SAVCCH            2
261300960514    2C                   END
261400970523     C*
261500970523     C                   MOVE      ANMCCH        CCHPRT
261600040616     C* DATA/ora creazione anomalia
261700040616     c                   clear                   stadac
261800040616     c                   clear                   staorc
261900040616     c                   if        anmflo<>*blanks
262000040616     c                   movel     anmflo        danmflo
262100040616     C                   CLEAR                   G02INV
262200040616     C                   MOVE      §anmdac       G02INV
262300040616     C                   MOVEL     3             G02ERR
262400040616     C                   CALL      'XSRDA8'
262500040616     C                   PARM                    WLBDAT
262600040616     C                   MOVEL     G02DAT        STADAc            4 0
262700040616     c                   move      §anmorc       staorc            4 0
262800040616     c                   endif
262900970523     C*
263000960327     C**
263100960403     C* SOLO SE ANOMALIA ESTERNE IDD STAMPO IL LIVELLO IDD
263200960404     C   97              MOVEL     ANMLID        STALID            1
263300960404     C  N97              CLEAR                   STALID
263400960509     C**
263500960509     C* SE NON L'HO ANCORA TROVATA PRENDO LA ZONA DALLE SPUNTE
263600960514    2C     PRTZNC        IFEQ      '  '
263700070111     C     KART          SETLL     FNBRV07L
263800070111     C     KART          READE     FNBRV07L                               32
263900960514    3C     *IN32         DOWEQ     *OFF
264000960509     C     PRTZNC        ANDEQ     '  '
264100960514    4C     BRVZNC        IFGT      0
264200960509     C                   MOVEL     BRVZNC        PRTZNC
264300960509     C                   ELSE
264400070111     C     KART          READE     FNBRV07L                               32
264500960514    4C                   ENDIF
264600960514    3C                   ENDDO
264700960514    2C                   ENDIF
264800990512     C* ANNO A 2
264900990512     C   02              MOVE      ANMAAS        PRTAAS            2 0
265000911025     C*
265100900622     C* STAMPO DETTAGLIO
265200921015     C*
265300921015     C* STAMPA TESTATA
265400960514    2C     D34ELA        IFNE      ' '
265500960403     C     ANMLID        ANDLT     §7ALCI
265600960327     C     D34ELA        OREQ      ' '
265700960327     C*
265800040615     c                   if        *inOF
265900040615     C                   EXCEPT    TES
266000040615     C  n70              EXCEPT    TES2
266100040615     C                   EXCEPT    TES3
266200040615     C                   SETOFF                                       OF
266300040615     c                   endif
266400921015     C*
266500921015     C                   EXCEPT    DET
266600050503     c   41
266700050503     cor 43              except    det1
266800051221     c   44              except    det2
266900151125     c   45              except    det3
267000160908     c   81              except    det4
267100050503     c
267200031007     C   55              EXSR      WRIDET
267300960422     C                   MOVEL     '1'           WSTAMP            1
267400921015     C*
267500960514   X2C                   ELSE
267600921015     C*
267700921015     C   OE              EXCEPT    TESX
267800921015     C   OE              SETOFF                                       OE
267900921015     C*
268000911025     C                   EXCEPT    DETX
268100050503     c   41
268200050503     Cor 43              EXCEPT    DETX1
268300031007     C   55              EXSR      WRIDET
268400960514    2C                   ENDIF
268500900622     C*
268600040616     C**N17              SETOFF                                       04
268700040401     c* Scrivo file per report di sede
268800040401     c     wgir200       ifeq      'S'
268900040407     c     *in01         andeq     *on
269000040401     c                   exsr      srant
269100040401     c                   endif
269200960514    1C                   ENDIF
269300900622     C                   ENDSR
269400990122     C*
269500990122     C* DECODIFICA DATI RECORD C.A. ----------------------------------*
269600990122     C     DECDAN        BEGSR
269700990122     C* SE 09 20 O 21 STAMPO SU 2 RIGA ALTRIMENTI SULLA PRIMA
269800990122    1C     *IN09         IFEQ      *OFF
269900990122     C     *IN20         ANDEQ     *OFF
270000990122     C     *IN21         ANDEQ     *OFF
270100990122     C                   SETON                                        40
270200990122     C                   MOVEL     DESCAA        DESCA1           18
270300990122     C                   MOVE      ':'           DESCA1
270400990122     C                   ELSE
270500990122     C                   SETON                                        41
270600990122    1C                   ENDIF
270700990122     C**
270800990122     C* Data C.A.
270900990122     C                   MOVE      DCTDCA        G02INV
271000990122     C                   MOVEL     3             G02ERR
271100990122     C                   CALL      'XSRDA8'
271200990122     C                   PARM                    WLBDAT
271300990122     C                   MOVEL     G02DAT        STADCA            6 0
271400990122     C                   Z-ADD     G02DAT        W0020             2 0
271500990122     C                   MOVE      W0020         STADCA
271600990122     C* Data CHIUSURA C.A.
271700990122    1C     DCTDCH        IFGT      0
271800990122     C                   MOVE      DCTDCH        G02INV
271900990122     C                   MOVEL     3             G02ERR
272000990122     C                   CALL      'XSRDA8'
272100990122     C                   PARM                    WLBDAT
272200990122     C                   MOVEL     G02DAT        STACHA            4 0
272300990122     C                   SETON                                        42
272400990122     C**
272500990122    1C                   ENDIF
272600990122     C                   ENDSR
272700960508     C*
272800960508     C* CONTROLLO PER ANOMALIA 005 SE E' COLLO IN DISGUIDO ----------*
272900960508     C     ANM005        BEGSR
273000960508     C* PROVO A CHAINARE FNART
273100960508     C     KART          CHAIN     FNART27L                           32
273200960927    1C     *IN32         IFEQ      *OFF
273300960927     C     ARTDET        ANDGT     0
273400960508     C                   SETON                                        20
273500960508     C                   MOVEL     ARTFLP        WDFLP
273600960508     C                   MOVEL     ARTDET        WDDET
273700960807     C*
273800960927     C                   Z-ADD     1             KNPG
273900960927     C* CERCO LA SPUNTA CON FGS=ARTFLP E DATA= ARTDET
274000140618     C     KBRV3         SETLL     FNBRV05L
274100140618     C     KBRV3         READE     FNBRV05L                               32
274200960927    2C     *IN32         DOWEQ     *OFF
274300960509     C* ZONA
274400960927    3C     BRVZNC        IFGT      0
274500960509     C                   MOVEL     BRVZNC        PRTZNC
274600960927    3C                   ENDIF
274700960927     C*
274800000201     C                   Z-ADD     BRVNFV        WNFV
274900960927     C     KFVA          CHAIN     FNFVA01L                           32
275000960927    3C     *IN32         IFEQ      *OFF
275100040617     C     FVADFV        ANDEQ     ARTDUT
275200960927     C                   SETON                                        32
275300960927    4C     BRVATR        IFEQ      'T'
275400960927     C                   MOVEL     CTRANS        PSTAM
275500960927     C                   ELSE
275600960927     C                   MOVEL     CDISG         PSTAM
275700960927    4C                   ENDIF
275800960927   X3C                   ELSE
275900960927     C                   SETOFF                                       32
276000960927    3C                   ENDIF
276100960927     C**
276200140618     C  N32KBRV3         READE     FNBRV05L                               32
276300960927    2C                   ENDDO
276400960927     C**
276500960927     C* SE NON TROVATO METTO DISGUIDO
276600960927     C     PSTAM         IFEQ      *BLANKS
276700960927     C                   MOVEL     CDISG         PSTAM
276800960927     C                   ENDIF
276900960927    1C                   ENDIF
277000960508     C                   ENDSR
277100911021     C*
277200960704     C* RICERCA SPUNTA INVENTARIO O CONSEGNA PER ANOMALIA 10 --------*
277300960704     C     RCRSPU        BEGSR
277400960704     C                   CLEAR                   WNPG
277500960704     C                   CLEAR                   WSPG
277600960704     C                   CLEAR                   WDFV
277700960704     C                   CLEAR                   WFGS
277800960704     C* CERCO SPUNTA INVENTARIO E CONSEGNA E PRENDO QUELLA PIU' RECENTE
277900960704    1C                   DO        2             II                1 0
278000960704    2C     II            IFEQ      1
278100960704     C                   Z-ADD     3             KNPG
278200960704     C                   ELSE
278300960704     C                   Z-ADD     4             KNPG
278400960704    2C                   END
278500140618     C     KBRV          SETLL     FNBRV05L
278600140618     C     KBRV          READE     FNBRV05L                               32
278700960704    2C     *IN32         DOWEQ     *OFF
278800000201     C                   Z-ADD     BRVNFV        WNFV
278900960704     C     KFVV          CHAIN     FNFVV01L                           32
279000960704    3C     *IN32         IFEQ      *OFF
279100960704     C     FVVDFV        ANDGT     WDFV
279200960704     C                   MOVE      FVVNPG        WNPG
279300960704     C                   MOVE      FVVSPG        WSPG
279400960704     C                   MOVE      FVVDFV        WDFV
279500960704     C                   MOVE      FVVFGS        WFGS
279600960704    3C                   END
279700140618     C     KBRV          READE     FNBRV05L                               32
279800960704    2C                   ENDDO
279900960704    1C                   ENDDO
280000960704    1C     WDFV          IFGT      0
280100960704     C                   SETON                                        21
280200960704     C* ROVESCIO DATA SPUNTA
280300960704     C                   CLEAR                   WLBDAT
280400960704     C                   MOVE      WDFV          G02INV
280500960704     C                   MOVEL     3             G02ERR
280600971126     C                   CALL      'XSRDA8'
280700960704     C                   PARM                    WLBDAT
280800960704     C                   MOVEL     G02DAT        PRTDFV            4 0
280900960704     C* IMPOSTO CAMPO PER STAMPA DECODIFICA CATEGORIA/SOTTOCATEGORIA
281000960704    2C     WNPG          IFEQ      3
281100161003     C                   MOVEL     '7B'          COD
281200161003     C                   MOVEL(P)  wspg          KEY
281300161003     C     KTAB          CHAIN(N)  TABEL                              32
281400161003     c  n32              movel     tbluni        ds7b
281500161003     c   32              clear                   ds7b
281600161003     C                   MOVEL(P)  §7bdesb       PRTDOV            5
281700161003     C***                MOVEL     'IM'          W003A             3
281800161003     C***                MOVE      WSPG          W003A
281900960704     C                   ELSE
282000960704     C                   MOVEL(P)  'CONS.'       PRTDOV
282100960704    2C                   END
282200960704    1C                   END
282300960704     C                   ENDSR
282400911021     C* INVIO ANOMALIE ----------------------------------------------*
282500911021     C     ELAB          BEGSR
282600160907     C**FLAG TRASMISSIONE
282700160907    1C     ANMFL1        IFNE      0
282800160907     C                   MOVEL     'T'           ANMFT1
282900160907     C                   MOVE      AM8GIO        ANMDTR
283000160907    1C                   END
283100160907    1C     ANMFL2        IFNE      0
283200160907     C                   MOVEL     'T'           ANMFT2
283300160907     C                   MOVE      AM8GIO        ANMDTR
283400160907    1C                   END
283500161003     C* SI INVIO IDD
283600161003    1C     W7AIDD        IFEQ      'S'
283700970306     C** PER REPORT DI SEDE
283800970306    3C     ANMCAA        IFEQ      55
283900990409    3C     ANMCAA        OREQ      56
284000040504     c* la lnp disguido è quella della bolla, se c'e'
284100040504     c                   if        anmlnp>0
284200040504     c                   movel     anmlnp        wlnp
284300040504     c                   else
284400040504     C                   MOVEL     ANMFLS        wLNP
284500040504     c                   endif
284600040504     c
284700970306     C     KA55          CHAIN     FNANT02L                           39
284800970306     C*
284900970306    4C     *IN39         IFEQ      *ON
285000970306     C                   CLEAR                   FNANT000
285100970306     C                   MOVEL     ANMFLE        ANTFOA
285200040504     C****               MOVEL     ANMFLS        ANTLNP
285300040504     C                   MOVEL     wlnp          ANTLNP
285400990512     C                   MOVEL     ANMDAO        ANTDAO
285500970306     C                   MOVEL     ANMCAA        ANTCAA
285600970306     C                   Z-ADD     1             ANTTOT
285700990504     C*
285800990504     C                   CLEAR                   DSLV55
285900990504     C                   MOVEL     'P'           D55TPT
286000040504     C**                 MOVEL     anmfls        D55LIN
286100040504     C                   MOVEL     wlnp          D55LIN
286200990512     C                   MOVEL     ANMDAO        D55DRF
286300990504     C                   CALL      'FNLV55R'
286400990504     C                   PARM                    DSLV55
286500000317     C* PER LE ANOMALIE 55 E 56 ANTLNA CONTIENE IL TERMINAL DI PARTENZA
286600990504     C*  DELLE BOLLE
286700990504     C                   MOVEL     D55TFP        ANTLNA
286800040927     C                   Z-ADD     AM8GIO        antdt1
286900970306     C                   WRITE     FNANT000
287000970306   X4C                   ELSE
287100970306     C                   ADD       1             ANTTOT
287200040927     C                   Z-ADD     AM8GIO        antdt1
287300970306     C                   UPDATE    FNANT000
287400970306    4C                   ENDIF
287500970306    3C                   ENDIF
287600960523    1C                   ENDIF
287700911021     C*
287800960404     C* LE ANOMALIE 70 E 71 LE CHIUDO
287900960404     C* CON CAUSALE
288000000317    1C   01ANMCAA        IFEQ      70
288100960404     C     ANMCAA        OREQ      71
288200960523    2C     ANMDCH        IFEQ      0
288300990512     C                   MOVEL     D34ALA        ANMDCH
288400960404     C                   MOVEL     'D'           ANMFSC
288500970305     C*
288600970305    3C     ANMCAA        IFEQ      71
288700970306     C     KA71          CHAIN     FNANT02L                           39
288800970305     C*
288900970305    4C     *IN39         IFEQ      *ON
289000970305     C                   CLEAR                   FNANT000
289100970306     C                   MOVEL     ANMFLE        ANTFOA
289200970306     C                   MOVEL     ANMLNP        ANTLNP
289300990512     C                   MOVEL     ANMDAO        ANTDAO
289400970305     C                   MOVEL     ANMCAA        ANTCAA
289500970305     C                   Z-ADD     1             ANTTOT
289600040927     c                   z-add     am8gio        antdt1
289700970305     C                   WRITE     FNANT000
289800970305   X4C                   ELSE
289900970305     C                   ADD       1             ANTTOT
290000040927     c                   z-add     am8gio        antdt1
290100970305     C                   UPDATE    FNANT000
290200970305    4C                   ENDIF
290300970305    3C                   ENDIF
290400970305     C**
290500960523    2C                   ENDIF
290600960523    1C                   ENDIF
290700960404     C*
290800000317     C* SE NON CI SONO STATI ERRORI DI TRASMISSIONE UPDATO
290900160907     C***  WERR          IFNE      'S'
291000001102     C     D34GES        IFEQ      'V'
291100001102     C                   UPDATE    FNANM011
291200001102     C                   ELSE
291300990512     C                   UPDATE    FNANM000                                      AGGIORNO
291400001102     C                   ENDIF
291500160907     C***                ELSE
291600160907     C***  D34GES        IFEQ      'V'
291700160907     C***                UNLOCK    FNANM11L
291800160907     C***                ELSE
291900160907     C***                UNLOCK    FNANM00F
292000160907     C***                ENDIF
292100160907    1C***                ENDIF
292200911021     C                   ENDSR
292300040401     C* SCRITTURA FILE PER REPORT DI SEDE ---------------------------*
292400040401     C     SRANT         BEGSR
292500040401     C** PER REPORT DI SEDE
292600040504     c                   if        anmlnp>0
292700040504     c                   movel     anmlnp        wlnp
292800040504     c                   else
292900040504     C                   MOVEL     ANMFLS        wLNP
293000040504     c                   endif
293100040405     C     KA200         CHAIN     FNANT02L                           39
293200040401     C*
293300040401    4C     *IN39         IFEQ      *ON
293400040401     C                   CLEAR                   FNANT000
293500040401     C                   MOVEL     ANMFLE        ANTFOA
293600040504     C                   MOVEL     wlnp          ANTLNP
293700040405     C* Come data anomalia metto data idd per poter conteggiare le anomalie
293800040405     c* lette tutte nello stesso report controllo qualità
293900040405     C                   MOVEL     WDTA200       ANTDAO
294000040401     C                   MOVEL     ANMCAA        ANTCAA
294100040401     C                   Z-ADD     1             ANTTOT
294200040401     C*
294300040401     C                   CLEAR                   DSLV55
294400040401     C                   MOVEL     'P'           D55TPT
294500040401     C                   MOVEL     ANMFLe        D55LIN
294600040401     C                   MOVEL     ANMDAO        D55DRF
294700040401     C                   CALL      'FNLV55R'
294800040401     C                   PARM                    DSLV55
294900040401     C* PER l'anomalia 200 ANTLNA CONTIENE IL TERMINAL DI PARTENZA
295000040401     C*  del p.o. che ha generato il disguido
295100040401     C                   MOVEL     D55TFP        ANTLNA
295200040927     c                   z-add     am8gio        antdt1
295300040401     C                   WRITE     FNANT000
295400040401     C                   ELSE
295500040401     C                   ADD       1             ANTTOT
295600040927     c                   z-add     am8gio        antdt1
295700040401     C                   UPDATE    FNANT000
295800040401     C                   ENDIF
295900040401     c                   endsr
296000911021     C*
296100960404     C* CONTROLLO PER TRASMETTERE LA CHIUSURA ANOMALIE 70 E 71 ------*
296200960404     C     CHIUAN        BEGSR
296300960417     C** MANCA BAM
296400960417     C                   Z-ADD     71            KCAA
296500960404     C                   Z-ADD     1             L
296600960404     C*
296700960417    1C     L6(L)         DOWGT     0
296800960404     C                   Z-ADD     L6(L)         KFIL
296900990512     C     KANM5         SETLL     FNANM10L
297000990512     C     KANM5         READE     FNANM10L                               30
297100960417    2C     *IN30         DOWEQ     *OFF
297200960417     C                   EXSR      BAM
297300990512     C     KANM5         READE     FNANM10L                               30
297400960417    2C                   ENDDO
297500960404     C**
297600960404     C                   ADD       1             L
297700960417    1C                   ENDDO
297800960417     C** ECCEDE BAM
297900960417     C                   Z-ADD     70            KCAA
298000021021     C                   Z-ADD     1             L
298100021021    1C     L6(L)         DOWGT     0
298200021021     C                   Z-ADD     L6(L)         KFIL
298300021021     C     Kanm20        SETLL     FNANM20L
298400021021     C     Kanm20        READE     FNANM20L                               30
298500960417    2C     *IN30         DOWEQ     *OFF
298600960417     C* SE ANMFLE E' PRESENTE IL L6 --> ELABORO
298700021021     c*  tolto il controllo perchè anmfle già in key
298800021021     C**   ANMFLE        LOOKUP    L6                                     31
298900021021     C**   *IN31         IFEQ      *ON
299000960417     C                   EXSR      BAM
299100021021     C**                 ENDIF
299200960417     C*
299300021021     C     Kanm20        READE     FNANM20L                               30
299400960417    2C                   ENDDO
299500021021     C**
299600021021     C                   ADD       1             L
299700021021    2C                   ENDDO
299800960404     C*
299900960404     C                   ENDSR
300000960417     C*
300100960417     C* CONTROLLO LE ANOMALIE 70 E 71 PER CHIUDERLE -----------------*
300200960417     C     BAM           BEGSR
300300960417     C                   MOVEL     ANMAAS        KAAS2
300400960417     C                   MOVEL     ANMLNP        KLNP2
300500960417     C                   MOVEL     ANMNRS        KNRS2
300600960417     C                   MOVEL     ANMNSP        KNSP2
300700960417     C*
300800960417     C* LEGGO IL COLLO FINTANTO CHE TROVO DELLE ANOMALIE INSERITE
300900990512     C     KANM1         SETLL     FNANM01L
301000990512     C     KANM1         READE     FNANM01L                               31
301100960417    4C     *IN31         DOWEQ     *OFF
301200990512     C     KANM1         READE     FNANM01L                               31
301300960417    4C                   ENDDO
301400960417     C*
301500960417     C* LETTA L'ULTIMA, VEDO CHE DATA CHIUSURA HA
301600960417    4C     ANMCCH        IFEQ      '  '
301700970424     C     ANMDCH        ANDGT     0
301800990512     C     ANMDCH        ANDLT     D34ALA
301900960417     C* RICHAINO CON NUMERO RELATIVO DI RECORD
302000990512     C     ANMNR1        CHAIN     FNANM00F                           31
302100960417     C* TRASMETTO IN PARTENZA LA CHIUSURA DELLA ANOMALIA
302200960417     C                   MOVEL     'OB'          ANMCCH
302300990512     C                   MOVEL     D34ALA        ANMDCH
302400960417     C                   EXSR      ELAB
302500960417    4C                   ENDIF
302600960417     C                   ENDSR
302700960422     C*
302800960423     C* STAMPA DI CHI NON HA FATTO L'IDD ----------------------------*
302900960423     C     NOIDD         BEGSR
303000960422     C                   SETON                                        08
303100040615     C                   SETOff                                       09
303200960423     C                   CLEAR                   WMANID
303300960423     C                   Z-ADD     95            KCAA
303400960422     C* SE IMMESSA LA DATA AL CERCO NELLA DATA AL
303500960422     C*  ALTRIMENTI CERCO LA DATA IDD PIU' ALTA CHE HO
303600990512    1C     D34ALA        IFEQ      0
303700990512     C     D34ACA        ANDEQ     0
303800990512     C     KCAA          SETGT     FNANM04L
303900990512     C     KCAA          READPE    FNANM04L                               30
304000960422    2C     *IN30         DOWEQ     *OFF
304100050504     C**   ANMAIE        ANDEQ     'E'
304200050504    2C**   *IN30         OREQ      *OFF
304300990512     C     ANMDAO        ANDGE     AM8GIO
304400990512     C     KCAA          READPE    FNANM04L                               30
304500960422    2C                   ENDDO
304600960422     C*
304700971126     C     *IN30         IFEQ      *OFF
304800971126     C                   Z-ADD     ANMDAO        WDATA8            8 0
304900971126     C                   ENDIF
305000971126     C**
305100960422   X1C                   ELSE
305200971126     C     D34ALA        IFGT      0
305300971126     C                   MOVEL     D34ALA        WDATA8
305400961016     C                   ELSE
305500971126     C                   MOVEL     D34ACA        WDATA8
305600961016     C                   ENDIF
305700961016     C*
305800960422     C                   SETOFF                                       30
305900960422    1C                   ENDIF
306000960510     C*
306100960926     C* PER I P.O. CHE FANNO L'IDD IL GIORNO DOPO
306200960510     C*  CERCO LA DATA PRECEDENTE
306300960510     C  N30              EXSR      RICDAT
306400990512     C  N30              MOVEL     GI8INV        WDATAM            8 0
306500960510     C*
306600960422     C* 30 OFF - TROVATA
306700960422    1C     *IN30         IFEQ      *OFF
306800960422     C                   Z-ADD     0             L
306900990518     C* GIRO LA DATA ELABORAZIONE DEL GIORNO
307000971126     C                   MOVE      WDATA8        G02INV
307100960422     C                   MOVEL     '3'           G02ERR
307200971126     C                   CALL      'XSRDA8'
307300960422     C                   PARM                    WLBDAT
307400990513     C                   MOVEL     G02DAT        STADCI            8 0
307500960510     C*
307600971126     C                   CLEAR                   G02INV
307700960510     C                   MOVE      WDATAM        G02INV
307800960510     C                   MOVEL     '3'           G02ERR
307900971126     C                   CALL      'XSRDA8'
308000960510     C                   PARM                    WLBDAT
308100990513     C                   MOVEL     G02DAT        STADCM            8 0
308200960422     C*
308300960422     C     *LOVAL        SETLL     AZORG01L
308400960423     C                   READ      AZORG01L                               30
308500960422     C*
308600960422    2C     *IN30         DOWEQ     *OFF
308700960423    C**
308800960510     C* ESCLUDO LE ANNULLATE
308900960422    3C     ORGFVA        IFEQ      ' '
309000960423     C**
309100960422     C* SOLO FILIALE E AGENZIA
309200960422    4C     ORGFAG        IFEQ      'F'
309300960422     C     ORGFAG        OREQ      'A'
309400960423     C**
309500960926     C* ESCLUDO I P.O. CHE NON DEVONO FARE IDD
309600960422     C                   MOVEL     ORGDE8        OG148
309700050831     C                   MOVEL     ORGDE3        OG143
309800960422    5C     §OGNID        IFEQ      ' '
309900971124     C* CERCO IL TERMINAL DI PARTENZA
310000971124     C* SE STO ELABORANDO L'IDD E NON SONO IL TERMINAL DI PARTENZA
310100971124   5AC     D34ELA        IFNE      ' '
310200971124     C     ORGFIL        ANDNE     D34FGS
310300971124     C**
310400971124     C                   CLEAR                   DSLV55
310500971124     C                   MOVEL     'P'           D55TPT
310600971124     C                   MOVEL     ORGFIL        D55LIN
310700980403     C                   Z-ADD     AM8GIO        D55DRF
310800971124     C                   CALL      'FNLV55R'
310900971124     C                   PARM                    DSLV55
311000971124   5BC     D55ERR        IFNE      ' '
311100971124     C                   Z-ADD     SIMFEL        D55TFP
311200971124   5BC                   ENDIF
311300971124   5AC                   ENDIF
311400960510     C**
311500960509     C* SE ANOMALIE IDD --> ESCLUDO SE NON SONO DELL'AREA
311600960423   5AC     D34ELA        IFEQ      ' '
311700971124     C     ORGFIL        ORNE      D34FGS
311800971124     C     D55TFP        ANDEQ     D34FGS
311900960422     C*
312000960422     C                   MOVEL     ORGFIL        KLNA
312100960510     C* IMPOSTO LA DATA
312200960510     C     §OGEID        IFEQ      ' '
312300971126     C                   Z-ADD     WDATA8        KDAO                           DEL GIORNO
312400960510     C                   ELSE
312500960510     C                   Z-ADD     WDATAM        KDAO                           GIORNO PREC
312600960510     C                   ENDIF
312700960510     C*
312800990512     C     KANM4         SETLL     FNANM04L                               31
312900960422    6C     *IN31         IFEQ      *OFF
313000960423     C                   MOVEL     '1'           WMANID            1
313100960422     C*
313200960422     C* NON TROVATA STAMPO ERRORE
313300960422    7C     L             IFEQ      7
313400960422     C                   EXSR      ERRIDD
313500960422    7C                   ENDIF
313600050830     c
313700050830     C                   ADD       1             L
313800050830     C                   Z-ADD     ORGFIL        FID(L)
313900050831     C                   MOVEL     ORGDES        DFI(L)
314000050831     c                   clear                   dslv55
314100050830     c* Verifico se il £6
314200050830     c                   if        §ogntw<>'EEX' and §ogntw<>'DPD' and
314300050830     c                             §ogntw<>'FED' and §ogntw<>'EUP'
314400050830     C                   MOVEL     '6'           D55TPT
314500050831     c                   else
314600050831     c                   movel     'P'           D55tpt
314700050831     c                   endif
314800050830     C                   MOVEL     ORGFIL        D55LIN
314900050830     C                   Z-ADD     kdao          D55DRF
315000050830     C                   CALL      'FNLV55R'
315100050830     C                   PARM                    DSLV55
315200960422     C**
315300050831     c                   if        §ogntw<>'EEX' and §ogntw<>'DPD' and
315400050906     c                             §ogntw<>'FED' and §ogntw<>'EUP' and
315500050906     c                             orgfil<>101
315600050906     c
315700050830     c                   if        d55tfa=orgfil
315800050831     c* Imposto la decodifica del p.o. se non è in gestione ad altri
315900050831     C***                MOVEL     ORGDES        DFI(L)
316000050830     c                   else
316100050831     c* Imposto il p.o. gestione
316200050831     c****               eval      DFI(L)=CGestito
316300050831     c                   movel     d55tfa        w003a             3
316400050831     c                   eval      %subst(DFI(L):8:12)=CGESTITO
316500050831     c                   eval      %subst(DFI(L):20:3)=w003a
316600050830     c                   endif
316700050830     c
316800050830     c                   else
316900050906     c* Imposto il terminal di partenza del P.o estero o della 101
317000050906     c*  come gestione
317100050831     c***                eval      DFI(L)=CGestito
317200050831     c                   movel     d55tfp        w003a             3
317300050831     c                   eval      %subst(DFI(L):8:12)=CGESTITO
317400050831     c                   eval      %subst(DFI(L):20:3)=w003a
317500050830     c                   endif
317600960422     C*
317700960422    6C                   ENDIF
317800960422     C*
317900960423   5AC                   ENDIF
318000960423    5C                   ENDIF
318100960422    4C                   ENDIF
318200960422    3C                   ENDIF
318300960422     C*
318400960423     C                   READ      AZORG01L                               30
318500960422    2C                   ENDDO
318600960422     C*
318700960422     C* SE NE RIMANGONO FINISCO DI STAMPARE
318800960423    2C     L             IFGT      0
318900960422     C                   EXSR      ERRIDD
319000960423   X2C                   ELSE
319100960423     C* SE CI SONO TUTTI LO DICO
319200960423    3C     WMANID        IFEQ      ' '
319300960423     C*
319400960423    4C     *INOF         IFEQ      *ON
319500960423     C     *IN09         OREQ      *OFF
319600040615     c                   if        *inof
319700040615     C                   EXCEPT    TES
319800040615     C  n70              EXCEPT    TES2
319900040615     c                   endif
320000960423     C*
320100960423    5C     D34ELA        IFEQ      ' '
320200960423     C                   EXCEPT    TESIDR
320300960423     C                   ELSE
320400960423     C                   EXCEPT    TESIDE
320500960423    5C                   ENDIF
320600960423     C                   SETON                                        09
320700960423     C                   SETOFF                                       OF
320800960423    4C                   ENDIF
320900960423     C**
321000960423     C* TUTTI HANNO ESEGUITO L'IDD
321100960423     C                   EXCEPT    SIIDD
321200960423    3C                   ENDIF
321300960423     C*
321400960423    2C                   ENDIF
321500960422     C*
321600960422    1C                   ENDIF
321700960422     C*
321800960422     C                   SETOFF                                       08
321900960422     C                   ENDSR
322000960510     C*
322100960926     C* RICERCA DATA IDD-1 X I P.O. CHE LO FANNO IL GG DOPO----------*
322200990512     C     RICDAT        BEGSR
322300960510     C* UTILIZZO IL CALENDARIO
322400971126     C                   MOVE      WDATA8        G02INV
322500960510     C                   MOVEL     '3'           G02ERR
322600971126     C                   CALL      'XSRDA8'
322700960510     C                   PARM                    WLBDAT
322800960510     C*
322900960510     C                   MOVEL     'S'           WFEST             1
323000960510     C                   CLEAR                   KTFP
323100960510     C* CERCO IL PRIMO GIORNO LAVORATIVO PRECEDENTE
323200960510     C     WFEST         DOWEQ     'S'
323300960510     C                   SUB       1             G02TGI
323400990512     C                   Z-ADD     G02TGI        GI8TGI
323500990518     C                   CALL      'XSRGI8'
323600960510     C                   PARM                    WGIDAT
323700990512     C                   Z-ADD     GI8INV        WDATAM
323800960510     C*
323900960510     C                   CLEAR                   WFEST
324000960510     C     KCLN          CHAIN     AZCLN01L                           31
324100960510    1C     *IN31         IFEQ      *OFF
324200960510     C     POM(WGG)      ANDNE     *BLANKS
324300960510     C                   MOVEL     '1Y'          COD
324400960510     C                   MOVEL(P)  POM(WGG)      KEY
324500960510     C     KTAB          CHAIN     TABEL                              31
324600960510    2C     *IN31         IFEQ      *OFF
324700960510     C                   MOVEL     TBLUNI        DS1Y
324800960510    3C     §1YFEI        IFEQ      'S'
324900960510     C                   MOVEL     'S'           WFEST
325000960510    3C                   ENDIF
325100960510    2C                   ENDIF
325200960510    1C                   ENDIF
325300960510     C                   ENDDO
325400960510     C                   ENDSR
325500960422     C*
325600960422     C* STAMPA ERRORE NO IDD ----------------------------------------*
325700960422     C     ERRIDD        BEGSR
325800960422     C* SE OF ACCESO LA PRIMA VOLTA STAMPO LA TESTATA COMPLETA
325900960422    1C     *INOF         IFEQ      *ON
326000960423     C     *IN09         OREQ      *OFF
326100040615     c                   if        *inOF
326200040615     C                   EXCEPT    TES
326300040615     C  n70              EXCEPT    TES2
326400040615     c                   endif
326500960423     C*
326600960423     C     D34ELA        IFEQ      ' '
326700960423     C                   EXCEPT    TESIDR
326800960423     C                   ELSE
326900960423     C                   EXCEPT    TESIDE
327000960423     C                   ENDIF
327100960423     C                   SETON                                        09
327200960423     C                   SETOFF                                       OF
327300960422    1C                   ENDIF
327400960422     C*
327500960422     C                   EXCEPT    DETIDD
327600960422     C*
327700960422     C                   CLEAR                   FID
327800960422     C                   CLEAR                   DFI
327900960422     C                   CLEAR                   L
328000960422     C*
328100960422     C                   ENDSR
328200960404     C*
328300911021     C* INVIO MEMBRI TRASMISSIONE -----------------------------------*
328400911021     C     CAEX          BEGSR
328500050804     C* CREO ANOMALIA OK IDD SUL SISTEMA per tutti i p.o. della £6
328600000317     C     D34ELA        IFEQ      'S'
328700060619     c* apro il file anomalie di sede per scriverci l'anomalia 95
328800160907     c***                EXSR      opensed
328900060619     c
329000050804     C                   Z-ADD     1             LL                3 0
329100050804    2C     L6(LL)        DOWGT     0
329200050804     c
329300050804     C                   Z-ADD     L6(LL)        anmlna
329400050804     C                   Z-ADD     FGSTFP        D55TFP
329500911021     C                   Z-ADD     95            ANMCAA
329600960404     C                   EXSR      CRTANM
329700990512     C                   WRITE     FNANM000
329800160907     c***                if        %open(fnanmsed)
329900160907     C***                WRITE     FNANM0sd
330000160907     c***                endif
330100050804     c
330200050804     c                   add       1             LL
330300050804     c                   enddo
330400050804     c
330500060711    1C                   ENDIF
330600060711     c
330700060711     C* Trasmissione membri  solo se chi esegue è il terminal o se
330800060711     c*  un 2 livello che lo fa dopo il terminal
330900160907     c**
331000160907    0C**   D34FGS        IFEQ      SIMFEL
331100160907     C**   D34ALA        ORLE      FELDID
331200160907     C**
331300160907     C**   *LOVAL        SETLL     AZORG
331400160907     C**                 READ      AZORG                                  30
331500160907     C**
331600160907    1C**   *IN30         DOWEQ     *OFF
331700160907    2C**   ORGFAG        IFEQ      'F'
331800160907     C**   ORGFAG        OREQ      'A'
331900160907    3C**   ORGFVA        IFEQ      ' '
332000160907     C**PRENDO IL TERMINAL DI PARTENZA
332100160907     C**                 CLEAR                   DSLV55
332200160907     C**                 MOVEL     'P'           D55TPT
332300160907     C**                 MOVEL     ORGFIL        D55LIN
332400160907     C**                 MOVEL     AM8GIO        D55DRF
332500160907     C**                 CALL      'FNLV55R'
332600160907     C**                 PARM                    DSLV55
332700160907     C**SE NON TROVATO TERMINAL DI PARTENZA NON INVIO OK IDD
332800160907     C**(P.O. NON ANCORA OPERATIVO MA GIA' PRESENTE IN ORGANIGRAMMA)
332900160907    4C**   D55ERR        IFEQ      *BLANKS
333000160907    4C**   ORGFIL        ANDEQ     D55TFP
333100160907     C**   ORGFIL        ANDNE     SIMFEL
333200160907     c**
333300160907    4C**   D55ERR        orEQ      *BLANKS
333400160907     C**   ORGFIL        ANDEQ     D55TFP
333500160907     C**   ORGFIL        ANDeq     SIMFEL
333600160907     c**   d34ges        andeq     'V'
333700160907     c**
333800160907     C**LA SEDE NON HA TERMINAL MA BISOGNA INVIARLA LO STESSO
333900160907     c**La sede non la invio più scrivo subito fnanm00f di sede
334000160907     C**SOLO PER FILIALI O AGENZIE
334100160907     C** 99              CLOSE     FNANM00T
334200160907     C** 99              SETOFF                                       99
334300160907     C**DISALLOCAZIONE
334400160907     c**                 eval      D60po0 = SIMfel
334500160907     C**                 MOVEL     SAVFL1        D60FL1
334600160907     C**                 MOVEL     SAVFL2        D60FL2
334700160907     C**                 MOVEL     AM8GIO        D60DRF
334800160907     C**                 MOVEL     'D'           D60AZN
334900160907     C**                 MOVEL     'FNANM00T'    D60FIL
335000160907     C**                 MOVEL     FNLV60DS      KPJBU
335100160907     C**                 CALL      'FNLV60R'
335200160907     C**                 PARM                    KPJBA
335300160907     C**                 CLEAR                   SAVFL1
335400160907     C**                 CLEAR                   SAVFL2
335500160907     C** ALLOCAZIONE
335600160907     c**                 eval      D60po0 = SIMfel
335700160907     c**                 reset                   DSmembro
335800160907     c**                 eval      MbrPOP = SIMfel
335900160907     c**                 eval      MbrPOA = D55tfp
336000160907     c**                 movel     DSmembro      VAR
336100160907     C**                 CLEAR                   D60FL2
336200160907     C**                 MOVEL     D55TFP        D60FL1
336300160907     C**                 MOVEL     'X'           D60AZN
336400160907     C**                 MOVEL     AM8GIO        D60DRF
336500160907     C**                 MOVEL(P)  '*EXCL  '     D60VNC
336600160907     C**                 MOVEL     FNLV60DS      KPJBU
336700160907     C**                 CALL      'FNLV60R'
336800160907     C**                 PARM                    KPJBA
336900160907     C**                 MOVEL     KPJBU         FNLV60DS
337000160907     C**
337100160907     C**NON TROVATI ERRORI DI ALLOCAZIONE
337200160907    5C**   D60ERR        IFEQ      'N'
337300160907     C**                 MOVEL     D55TFP        SAVFL1
337400160907     c**                 movel     D60mb1        DSmembro
337500160907     c**                 movel     DSmembro      VAR
337600160907     c**                 movel     D60mb1        wMBR
337700160907     C**
337800160907     C**PRENDO NUMERO RECORD
337900160907     C**                 MOVEL     'FNANM00T'    PGM              10
338000160907     C**                 EXSR      NUMREC
338100160907     C**
338200060711     C*
338300160907    6C**   NRR           IFGT      0
338400060711     c* controllo spostato sopra
338500060711    7C***  D34FGS        IFEQ      SIMFEL
338600060711     C***  D34ALA        ORLE      FELDID
338700160907     C**                 MOVEL     ORGFEL        §DST              3
338800160907     C**
338900160907     C**                 CLEAR                   TIBS55DS
339000160907     C**                 MOVEL     SIMFEL        I50PPO
339100160907     C**                 MOVEL     ORGFIL        I50APO
339200160907     C**                 CALL      'TIBS55R'
339300160907     C**                 PARM                    TIBS55DS
339400160907     C**
339500160907     C**                 MOVEL     O50PLA        CMZLBD
339600160907     C**                 MOVEL     O50ALA        CMZLBA
339700160907     C**                 MOVEL     O50ASI        CMZSYF
339800160907     C**
339900160907     C**                 EXSR      X25ANM
340000160907    7C****               ENDIF
340100160907    6C**                 ENDIF
340200160907    5C**                 ENDIF
340300160907    4C**                 ENDIF
340400160907    3C**                 END
340500160907    2C**                 END
340600911021     C*
340700160907     C**                 READ      AZORG                                  30
340800160907    1C**                 ENDDO
340900160907    0C**                 ENDif
341000911021     C                   ENDSR
341100060619     C*
341200060619     C* open file anomalie di sede ----------------------------------*
341300160907     C**   OPENSED       BEGSR
341400160907     c**
341500160907     C**                 MOVEL     *BLANKS       COMMAN           80
341600160907     C**                 MOVEL     OVRSED(1)     COMMAN
341700160907     C**                 CALL      'QCMDEXC'                            98
341800160907     C**                 PARM                    COMMAN
341900160907     C**                 PARM                    LUNG
342000160907     C**                 OPEN(E)   FNANMSED
342100160907     c**provo in gaitragrps
342200160907     C**                 if        %error
342300160907     c**                 eval      %subst(comman:38:11)='PS/FNANM00F'
342400160907     C**                 CALL      'QCMDEXC'                            98
342500160907     C**                 PARM                    COMMAN
342600160907     C**                 PARM                    LUNG
342700160907     C**                 OPEN(E)   FNANMSED
342800160907     c**                 endif
342900160907     c**
343000160907     C**                 ENDSR
343100911021     C*
343200911021     C* CREO ANOMALIE ESEGUITO IDD ----------------------------------*
343300911021     C     CRTANM        BEGSR
343400911021     C                   MOVEL     *BLANKS       ANMATB
343500950922     C                   CLEAR                   ANMCDU
343600911021     C                   Z-ADD     0             ANMFLS
343700911021     C                   Z-ADD     0             ANMNRS
343800911021     C                   Z-ADD     0             ANMSCN
343900911021     C                   Z-ADD     0             ANMAAS
344000960404     C                   Z-ADD     0             ANMLNP
344100911021     C                   Z-ADD     0             ANMNSP
344200911021     C                   MOVEL     '7C'          COD
344300960404     C                   MOVEL(P)  ANMCAA        KEY
344400960409     C     KTAB          CHAIN(N)  TABEL                              31
344500911021     C  N31              MOVEL     TBLUNI        DS7C
344600911021     C   31              MOVEL     *BLANKS       DS7C
344700911021     C                   MOVEL     §7CTAN        ANMTAN
344800911021     C                   MOVEL     §7CSAN        ANMSAN
344900921027     C                   MOVEL     'D'           ANMFSA
345000990512     C                   MOVEL     D34ALA        ANMDAO
345100911021     C                   MOVEL     §7CAIE        ANMAIE
345200911025     C                   MOVEL     ' '           ANMFT1
345300911025     C                   MOVEL     *ZEROS        ANMFL1
345400000824     C                   MOVEL     *ZEROS        ANMDTR
345500911021     C* CHIUSURA AUTOMATICA
345600911021     C     §7CCHA        IFEQ      'S'
345700921027     C                   MOVEL     'D'           ANMFSC
345800990512     C                   MOVEL     D34ALA        ANMDCH
345900911021     C                   END
346000960509     C* DA INVIARE
346100911021     C     §7CAIE        IFEQ      'E'
346200971124     C                   MOVEL     D55TFP        ANMFL1
346300911025     C                   MOVEL     'T'           ANMFT1
346400000824     C                   MOVE      AM8GIO        ANMDTR
346500911021     C                   END
346600911021     C                   Z-ADD     0             ANMFL2
346700911021     C                   MOVEL     *BLANKS       ANMFT2
346800911021     C                   MOVEL     *BLANKS       ANMCCH
346900911021     C                   Z-ADD     0             ANMNPS
347000911021     C                   Z-ADD     0             ANMNFV
347100911021     C                   MOVEL     SIMFEL        ANMFLE
347200911021     C                   MOVEL     §7CLID        ANMLID
347300990512     C                   CLEAR                   ANMDUE
347400990512     C                   CLEAR                   ANMNPG
347500990512     C                   CLEAR                   ANMFGS
347600990512     C                   CLEAR                   ANMFLO
347700040601     c* data ora di creazione
347800040601     c                   z-add     *date         wdata             6 0
347900040601     c                   time                    wora              6 0
348000040607     c                   movel     wora          wora4             4 0
348100040601     c                   movel     wdata         anmflo
348200040607     c                   move      wora4         anmflo
348300040601     c
348400990512     C                   CLEAR                   ANMFT3
348500990512     C                   CLEAR                   ANMFL3
348600990512     C                   CLEAR                   ANMFT4
348700040601     C                   z-add     999           ANMFL4
348800911021     C*
348900911021     C                   ENDSR
349000911021     C*
349100990512     C** SOTTOMETTE TRASMISSIONE FNANM00T ---------------------------*
349200160907     C**   X25ANM        BEGSR
349300160907     c**
349400160907     C**   X25ANM        MOVER     *BLANK        CMZPRR                          PRE-REQ.
349500160907     C**   X25ANM        MOVER     'S'           §CLR              1             CLEAR
349600160907     C**   X25ANM        MOVEL     §DST          CMZDST                          DESTINAT.
349700160907     C**   X25ANM        MOVEL     'FNANM00T'    CMZFLD                          DA: >>>
349800160907     C**   X25ANM        MOVEL     *BLANK        CMZMBD
349900160907     C**   X25ANM        MOVEL     wMBR          CMZMBD
350000160907     C**   X25ANM        MOVEL     'FNANM00R'    CMZFLA                          A: >>>
350100160907     C**   X25ANM        MOVEL     *BLANK        CMZMBA
350200160907     C**   X25ANM        MOVEL     wMBR          CMZMBA
350300160907     C**   X25ANM        MOVER     '0000'        CMZORI                          ORA
350400160907     C**   X25ANM        MOVER     '0000000'     CMZDTI                          DATA
350500040615     C**   D34TRA        IFEQ      'S'
350600040615     C**                 MOVE      'T'           CMZURG
350700040615     C**                 ELSE
350800160907     C**   X25ANM        MOVER     'N'           CMZURG                          ATTESA
350900040615     C**                 END
351000160907     C**   X25ANM        MOVER     'A'           CMZADD                          IN ADD
351100160907     C**   X25ANM        MOVER     §CLR          CMZCLR                          CLEAR
351200160907     C**   X25ANM        MOVER     'S'           CMZFTR                          DA TRANS.
351300160907     C**   X25ANM        MOVER     *BLANK        CMZERR                          1= ERR.
351400160907     C**   D34TRA        IFEQR     'S'
351500160907     C**   X25ANM        MOVER     'FF20'        CMZAZN                          AZIONE
351600040615     C**                 ELSE
351700160907     C**   X25ANM        MOVER     'LR16'        CMZAZN                          AZIONE
351800040615     C**                 END
351900160907     C**   X25ANM        MOVER     NRR           CMZNRR                          N.REC.
352000160907     C**   X25ANM        CALLR     'TIBS53C'
352100160907     C**   X25ANM        PARMR                   O50LCL
352200160907     C**   X25ANM        PARMR                   CMZDST
352300160907     C**   X25ANM        PARMR                   CMZFLD
352400160907     C**   X25ANM        PARMR                   CMZMBD
352500160907     C**   X25ANM        PARMR                   CMZLBD
352600160907     C**   X25ANM        PARMR                   CMZFLA
352700160907     C**   X25ANM        PARMR                   CMZMBA
352800160907     C**   X25ANM        PARMR                   CMZLBA
352900160907     C**   X25ANM        PARMR                   CMZORI
353000160907     C**   X25ANM        PARMR                   CMZDTI
353100160907     C**                 PARM                    CMZURG
353200160907     C**                 PARM                    CMZADD
353300160907     C**                 PARM                    CMZCLR
353400160907     C**                 PARM                    CMZFTR
353500160907     C**                 PARM                    CMZERR
353600160907     C**                 PARM                    CMZAZN
353700160907     C**                 PARM                    CMZSYF
353800160907     C**                 PARM                    CMZNRR
353900160907     C**                 PARM                    CMZPRR
354000160907     C**                 PARM                    W236            236
354100160907     C**                 ENDSR
354200911021     C*
354300911021     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE --------------------*
354400160907     C**   NUMREC        BEGSR
354500160907     c**                 reset                   DSmembro
354600160907     c**                 eval      MbrPOP = SIMfel
354700160907     c**                 eval      MbrPOA = D55tfp
354800160907     c**                 movel     DSmembro      ALF10
354900160907     C**                 Z-ADD     0             NRR              10 0
355000160907     C**                 CALL      'TRUL51C'
355100160907     C**                 PARM                    KNSIF
355200160907     C**                 PARM                    PGM
355300160907     C**                 PARM                    ALF10            10
355400160907     C**                 PARM                    NRR
355500160907     C**                 ENDSR
355600001030     C*-------------------------------------------------------------------
355700001030     C* TRASMISSIONE SCHEDULATA ANOMALIE (ESTERNE NON TRASMESSE)
355800001030     C     SCHANM        BEGSR
355900001102     C* N.B. LEGGO SOLO LE ANOMALIE ESTERNE VISTO CHE PER IL MOMENTO
356000001102     C*      LE AZIONI CHE PREVEDONO L'INVIO SCHEDULATO SONO TUTTE
356100001102     C*      ANOMALIE ESTERNE
356200001102     C                   MOVEL     'E'           KAIE
356300001030     C                   MOVEL     '7C'          COD
356400001030     C     KTAB2         SETLL     TABEL
356500001030     C     KTAB2         READE     TABEL                                  30
356600001030     C     *IN30         DOWEQ     *OFF
356700001030     C                   MOVEL     TBLUNI        DS7C
356800001030     C     §7CINV        IFEQ      'S'
356900001102     C                   MOVEL     TBLKEY        KCAA
357000001030     C     KAN11         SETLL     FNANM11L
357100001030     C     KAN11         READE     FNANM11L                               30
357200001030     C     *IN30         DOWEQ     *OFF
357300001030     C                   EXSR      ELAB
357400001030     C     KAN11         READE     FNANM11L                               30
357500001030     C                   ENDDO
357600001030     C                   ENDIF
357700001030     C     KTAB2         READE     TABEL                                  30
357800001030     C                   ENDDO
357900001030     C                   EXSR      CAEX
358000041020     c
358100041020     c* Leggo le anomalie 145 146 per le quali devo scrivere fnant
358200041020     c* sbr   spostata dal pgm fnlr76r che viene eliminato
358300041020     C                   MOVEL     'I'           KAIE
358400041020     c                   z-add     145           kcaa
358500041020     c                   do        2
358600041020     c     kan11         setll     fnanm11l
358700041020     C     KAN11         READE     FNANM11L
358800041020     c                   dow       not %eof(fnanm11l)
358900041020     c                   if        anmcch<>'AN'
359000041020     c                   EXSR      SCRANT
359100041020     c                   update    fnanm011
359200041020     C                   ENDIF
359300041020     c
359400041020     C     KAN11         READE     FNANM11L
359500041020     c                   enddo
359600041020     c                   z-add     146           kcaa
359700041020     c                   enddo
359800001030     C                   ENDSR
359900031007     C*-------------------------------------------------------------------
360000031007     C* WRIDET - ROUTINE SCRITTURA WRK-FILE WFLAN00F SU STAMPA FORMATO "DET"
360100031007     C*-------------------------------------------------------------------
360200031007     C     WRIDET        BEGSR
360300031007     C*
360400031007     C* Inizializzo il buffer del formato record
360500031007     C                   CLEAR                   WFLAN000
360600031007     C*
360700031031     C                   EVAL      WLANFGS = D34FGS
360800031007     C                   EVAL      WLANPOA = D34PAR
360900031007     C                   EVAL      WLANAAS = ANMAAS
361000031007     C                   EVAL      WLANLNP = ANMLNP
361100031007     C                   EVAL      WLANNRS = ANMNRS
361200031007     C                   EVAL      WLANNSP = ANMNSP
361300031007     C                   EVAL      WLANLNA = ANMLNA
361400031007     C                   MOVE      PRTDSP        WLANDSP
361500031007     C                   EVAL      WLANFLS = ANMFLS
361600031007     C                   EVAL      WLANSCN = ANMSCN
361700031007     C                   MOVE      PRTZNC        WLANZNC
361800031007     C                   EVAL      WLANNPS = ANMNPS
361900031007     C                   EVAL      WLANAIE = DESAIE
362000031007     C                   EVAL      WLANDEA = DESTAN
362100031007     C                   EVAL      WLANCAA = ANMCAA
362200031007     C                   EVAL      WLANDEC = DESCAA
362300031007     C                   EVAL      WLANNDC = ARBNDC
362400031007     C                   MOVE      STADDC        WLANDDC
362500031007     C                   EVAL      WLANPDC = STAPDC
362600031007     C* Se già valorizzato il campo PSTAM compondo la descrizione
362700031007     C                   IF        PSTAM <> *blanks
362800031007     C                   EVAL      WLANCO1 = PSTAM
362900031007     C                   MOVE      WDFLP         WDFLPALFA         3
363000031007     C                   EVAL      %subst(WLANCO1:13:3) = WDFLPALFA
363100031007     C                   MOVE      PRTDET        WPRTDET           4
363200031007     C                   EVAL      WLANCO1 = %trim(WLANCO1) + ' ' +
363300031007     C                                       %subst(WPRTDET:1:2)  + '/' +
363400031007     C                                       %subst(WPRTDET:3:2)
363500031007     C***                EVAL      WLANFLP = WDFLP
363600031007     C***                MOVE      PRTDET        WLANDET
363700031007     C                   ENDIF
363800031007     C*
363900031007     C* Se valorizzato il campo WFGS compondo la descrizione
364000031007     C                   IF        WFGS <> *zeros
364100031007     C                   EVAL      WLANCO2 = CDOVE
364200031007     C                   MOVE      WFGS          WDFLPALFA
364300031007     C                   EVAL      %subst(WLANCO2:13:3) = WDFLPALFA
364400031007     C                   EVAL      %subst(WLANCO2:5:5) = PRTDOV
364500031007     C                   MOVE      PRTDFV        WPRTDET
364600031007     C                   EVAL      WLANCO2 = %trim(WLANCO2) + ' ' +
364700031007     C                                       %subst(WPRTDET:1:2)  + '/' +
364800031007     C                                       %subst(WPRTDET:3:2)
364900031007     C***                EVAL      WLANDOV = PRTDOV
365000031007     C***                EVAL      WLANFGS = WFGS
365100031007     C***                MOVE      PRTDFV        WLANDFV
365200031007     C                   ENDIF
365300031007     C*
365400031007     C* Se valorizzato il numero C.A. compongo la descrizione
365500031007     C                   IF        DCTNCA > *zeros
365600040616     C                   EVAL      WLANCO3 = CCOAN1
365700031007     C                   MOVE      DCTFIL        WDFLPALFA
365800031007     C                   EVAL      %subst(WLANCO3:8:3) = WDFLPALFA
365900031007     C                   MOVE      DCTNCA        WNCAALFA          7
366000031007     C                   EVAL      %subst(WLANCO3:12:7) = WNCAALFA
366100031007     C                   MOVE      STADCA        WDCAALFA          6
366200031007     C                   EVAL      WLANCO3 = %trim(WLANCO3) + ' ' +
366300031007     C                                       %subst(WDCAALFA:1:2)  + '/' +
366400031007     C                                       %subst(WDCAALFA:3:2)  + '/' +
366500031007     C                                       %subst(WDCAALFA:5:2)
366600031007     C***                EVAL      WLANFIL = DCTFIL
366700031007     C***                EVAL      WLANNCA = DCTNCA
366800031007     C***                MOVE      DCTDCA        WLANDCA
366900031007     C                   ENDIF
367000031007     C*
367100031007     C                   EVAL      WLANLID = ANMLID
367200031007     C                   EVAL      WLANDFA = DESFSA
367300031007     C                   MOVE      STADAO        WLANDAA
367400031007     C                   EVAL      WLANCDU = ANMCDU
367500031007     C                   EVAL      WLANNFV = ANMNFV
367600031007     C                   EVAL      WLANDFC = DESFSC
367700031007     C                   MOVE      STADCH        WLANDAC
367800031007     C                   MOVE      CCHPRT        WLANCCH
367900031007     C                   EVAL      WLANDCC = DESCCH
368000031007     C                   EVAL      WLANFLE = ANMFLE
368100031007     C                   EVAL      WLANCHA = DCTDCH
368200031007     C*
368300031007     C* SCarico il buffer sul file
368400031007     C                   WRITE     WFLAN000
368500031007     C*
368600031007     C                   ENDSR
368700031007     C*-------------------------------------------------------------------
368800031007     C* DELWF  - ROUTINE DI CANCELLAZIONE WRK-FILE WFLAN00F X LANCIO CORRENTE
368900031007     C*-------------------------------------------------------------------
369000031007     C     DELWF         BEGSR
369100031007     C*
369200031007     C     KLAN01        SETLL     WFLAN01L
369300031007     C                   IF        %equal(WFLAN01L)
369400031007     C     KLAN01        READE     WFLAN01L
369500031007     C                   DOW       not %eof(WFLAN01L)
369600031007     C                   DELETE    WFLAN01L
369700031007     C     KLAN01        READE     WFLAN01L
369800031007     C                   ENDDO
369900031007     C                   ENDIF
370000031007     C*
370100031007     C                   ENDSR
370200960327     C****************************************************************
370300960327     C     *INZSR        BEGSR
370400960327     C*
370500960327     C     *ENTRY        PLIST
370600960327     C                   PARM                    KPJBA
370700960327     C                   MOVEL     KPJBU         DSLR34
370800960327     C                   Z-ADD     1             CODUT
370900960327     C                   CALL      'X§PARUT'
371000020905     C                   PARM                    UT§DSE0F
371100960327     C                   MOVEL     RAGUT         RSUT             20
371200960327     C                   MOVEL     REC80         CNCR80
371300031007     C*
371400031007     C* Verifico subito se per il lancio richiesto è prevista anche la creazione del wrk-file
371500031009     C* Lista Anomalie (WFLAN00F)
371600111201     C                   IF        DSLR34 <> *blanks  and                       * NO x trasmisisone
371700111201     c                             parsch<>'V'
371800031009     C***                IF        D34IE = 'D'                                  * solo anomalie IDD
371900031007     C* Imposto a NO l'indicatore d condizionamento creazoine wrk-file
372000031007     C                   SETOFF                                       55
372100031007     C* Verifico se x il cliente richiesto nel lancio è prevista la creazine del wrk-file
372200031007     C                   EVAL      WD34POS = D34POS
372300031007     C                   EVAL      WD34NRS = D34NRS
372400031007     C                   EVAL      TBECOD = 'LAN'
372500031007     C                   EVAL      TBEKE1 = DSKEYLAN
372600031007     C     KTBE01P       CHAIN     TNTBE01L
372700031007     C                   IF        %found(TNTBE01L)
372800031007     C* Se trovo il cliente in tabella, verifico i flag x le abilitazioni elaborazione
372900031007     C* anomalie in partenza e/o in arrivo
373000031007     C                   EVAL      DLAN = TBEUNI
373100031007     C                   IF        (§LANPAR = 'S' AND D34PAR = 'P') OR
373200031007     C                             (§LANARR = 'S' AND D34PAR = 'A')
373300031007     C                   SETON                                        55
373400031031     C                   OPEN      WFLAN01L
373500031007     C                   EXSR      DELWF
373600031007     C                   ENDIF
373700031007     C                   ENDIF
373800031007     C***                ENDIF
373900031009     C                   ENDIF
374000960327     C*-----------------------------------------------------
374100960327     C     KTAB          KLIST
374200960327     C                   KFLD                    CODUT
374300960327     C                   KFLD                    COD               2
374400960327     C                   KFLD                    KEY               8
374500031007     C     KTBE01P       KLIST                                                  * Parziale
374600031007     C                   KFLD                    TBECOD
374700031007     C                   KFLD                    TBEKE1
374800031007     C     KLAN01        KLIST                                                  * Completa
374900031031     C                   KFLD                    D34FGS
375000031007     C                   KFLD                    D34POS
375100031007     C                   KFLD                    D34NRS
375200960327     C     KTAB2         KLIST
375300960327     C                   KFLD                    CODUT
375400960327     C                   KFLD                    COD               2
375500960927     C     KFVA          KLIST
375600960927     C                   KFLD                    BRVFGS
375700000201     C                   KFLD                    WNFV
375800960510     C     KCLN          KLIST
375900960510     C                   KFLD                    KTFP
376000960510     C                   KFLD                    KLNA
376100990512     C                   KFLD                    WAA
376200990512     C                   KFLD                    WMM
376300960417     C     KANM          KLIST
376400990512     C                   KFLD                    ANMAAS
376500960417     C                   KFLD                    ANMLNP
376600960417     C                   KFLD                    ANMNRS
376700960417     C                   KFLD                    ANMNSP
376800960405     C     KANM1         KLIST
376900960405     C                   KFLD                    KAAS2
377000960405     C                   KFLD                    KLNP2
377100960405     C                   KFLD                    KNRS2
377200960405     C                   KFLD                    KNSP2
377300960404     C                   KFLD                    KCAA
377400960509     C     KANM2         KLIST
377500960509     C                   KFLD                    ANMFLS
377600960509     C                   KFLD                    ANMLNA
377700960509     C                   KFLD                    ANMNRS
377800960509     C                   KFLD                    ANMSCN
377900960509     C                   KFLD                    KCAA
378000021021     C     KANM20        KLIST
378100021021     C                   KFLD                    KFIL
378200021021     C                   KFLD                    KCAA
378300970529     C     KANM3         KLIST
378400970529     C                   KFLD                    KAIE
378500970529     C                   KFLD                    KCDU
378600970529     C                   KFLD                    ANMLNP
378700970529     C     KANM3A        KLIST
378800970529     C                   KFLD                    KAIE
378900970529     C                   KFLD                    KCDU
379000970529     C     KANM3B        KLIST
379100970529     C                   KFLD                    KAIE
379200970529     C                   KFLD                    KCDU
379300970529     C                   KFLD                    KLNP
379400970529     C                   KFLD                    ANMFLS
379500970529     C     KANM3D        KLIST
379600970529     C                   KFLD                    KAIE
379700970529     C                   KFLD                    KCDU
379800970529     C                   KFLD                    KLNP
379900960422     C     KANM4         KLIST
380000960422     C                   KFLD                    KCAA
380100960422     C                   KFLD                    KDAO
380200960423     C                   KFLD                    KLNA
380300960404     C     KANM5         KLIST
380400960404     C                   KFLD                    KCAA
380500960404     C                   KFLD                    KFIL
380600960412     C     KANM51        KLIST
380700960412     C                   KFLD                    KCAA
380800960412     C                   KFLD                    KFIL
380900960412     C                   KFLD                    KLNP
381000960412     C     KANM52        KLIST
381100960412     C                   KFLD                    KCAA
381200960412     C                   KFLD                    KFIL
381300960412     C                   KFLD                    KLNP
381400960412     C                   KFLD                    KFLS
381500960327     C     KANM7         KLIST
381600960327     C                   KFLD                    KAIE
381700960327     C                   KFLD                    KFIL
381800960327     C                   KFLD                    KLNA
381900960327     C     KANM71        KLIST
382000960327     C                   KFLD                    KAIE
382100960327     C                   KFLD                    KFIL
382200960327     C     KANM72        KLIST
382300960327     C                   KFLD                    KAIE
382400960327     C                   KFLD                    KFIL
382500960327     C                   KFLD                    KLNA
382600960327     C                   KFLD                    KDCH
382700960327     C     KANM8         KLIST
382800960327     C                   KFLD                    KAIE
382900960327     C                   KFLD                    KFIL
383000960327     C                   KFLD                    KLNP
383100960327     C                   KFLD                    KLNA
383200960327     C     KANM82        KLIST
383300960327     C                   KFLD                    KAIE
383400960327     C                   KFLD                    KFIL
383500960327     C                   KFLD                    KLNP
383600960327     C                   KFLD                    KLNA
383700960327     C                   KFLD                    KDCH
383800960327     C     KANM9         KLIST
383900960327     C                   KFLD                    KAIE
384000960327     C                   KFLD                    KFIL
384100960327     C                   KFLD                    KLNP
384200960327     C     KANM93        KLIST
384300960327     C                   KFLD                    KAIE
384400960327     C                   KFLD                    KFIL
384500960327     C                   KFLD                    KLNP
384600960327     C                   KFLD                    KFLS
384700960327     C                   KFLD                    KDCH
384800960327     C     KANM94        KLIST
384900960327     C                   KFLD                    KAIE
385000960327     C                   KFLD                    KFIL
385100960327     C                   KFLD                    KLNP
385200960327     C                   KFLD                    KFLS
385300001102     C     KAN11         KLIST
385400001102     C                   KFLD                    KAIE
385500001102     C                   KFLD                    KCAA
385600960327     C     KART          KLIST
385700960327     C                   KFLD                    ANMFLS
385800960327     C                   KFLD                    ANMLNA
385900960327     C                   KFLD                    ANMNRS
386000960327     C                   KFLD                    ANMSCN
386100960327     C     KARB          KLIST
386200960327     C                   KFLD                    ARTAAS
386300960327     C                   KFLD                    ARTLNP
386400960327     C                   KFLD                    ARTNRS
386500960327     C                   KFLD                    ARTNSP
386600960412     C     KBLP          KLIST
386700960412     C                   KFLD                    BLTAAS
386800960412     C                   KFLD                    BLTLNP
386900960412     C                   KFLD                    BLTNRS
387000960412     C                   KFLD                    BLTNSP
387100960508     C     KFVV          KLIST
387200960508     C                   KFLD                    BRVNPG
387300000201     C                   KFLD                    WNFV
387400960508     C                   KFLD                    BRVFGS
387500960704     C     KBRV          KLIST
387600960704     C                   KFLD                    KNPG
387700960704     C                   KFLD                    ANMFLS
387800960704     C                   KFLD                    ANMLNA
387900960704     C                   KFLD                    ANMNRS
388000960704     C                   KFLD                    ANMSCN
388100960927     C     KBRV3         KLIST
388200960927     C                   KFLD                    KNPG
388300960927     C                   KFLD                    ANMFLS
388400960927     C                   KFLD                    ANMLNA
388500960927     C                   KFLD                    ANMNRS
388600960927     C                   KFLD                    ANMSCN
388700960927     C                   KFLD                    ARTFLP
388800970604     C     KBRV3A        KLIST
388900970604     C                   KFLD                    KNPG
389000970604     C                   KFLD                    ANMFLS
389100970604     C                   KFLD                    ANMLNA
389200970604     C                   KFLD                    ANMNRS
389300970604     C                   KFLD                    ANMSCN
389400970604     C                   KFLD                    SIMFEL
389500970306     C     KA71          KLIST
389600970306     C                   KFLD                    ANMFLE
389700990512     C                   KFLD                    ANMDAO
389800970305     C                   KFLD                    ANMCAA
389900970306     C                   KFLD                    ANMLNP
390000970306     C     KA55          KLIST
390100970306     C                   KFLD                    ANMFLE
390200990512     C                   KFLD                    ANMDAO
390300970306     C                   KFLD                    ANMCAA
390400040504     C                   KFLD                    wlnp
390500040405     C     KA200         KLIST
390600040405     C                   KFLD                    ANMFLE
390700040405     C                   KFLD                    wdta200
390800040405     C                   KFLD                    ANMCAA
390900040504     C                   KFLD                    wlnp
391000041020     C     KANT          KLIST
391100041020     C                   KFLD                    KFOA
391200041020     C                   KFLD                    ANMDAO
391300041020     C                   KFLD                    ANMCAA                                     NE
391400041020     C                   KFLD                    KANLNP                                     NE
391500041020     C                   KFLD                    ANMLNA                                     NE
391600990122     C     KDCT          KLIST
391700990122     C                   KFLD                    DCDAAC
391800990122     C                   KFLD                    DCDFIL
391900990122     C                   KFLD                    DCDNCA
392000020807     C     K02FGV01      klist
392100020807     C                   kfld                    BLTnfv
392200020926     C                   kfld                    SIMfel
392300021203
392400021203     C     Kfapd         klist
392500021203     C                   kfld                    kpdtip
392600021203     C                   kfld                    kpdpdr
392700040615
392800040615     C     Kcae          klist
392900040615     C                   kfld                    kcaeepa
393000040615     C                   kfld                    kcaetfp
393100040615     C                   kfld                    kcaetfe
393200040615     C                   kfld                    kcaedde
393300040615     C     Kcae2         klist
393400040615     C                   kfld                    kcaeepa
393500040615     C                   kfld                    kcaetfp
393600040615     C                   kfld                    kcaetfe
393700040616     C     Kcae1         klist
393800040616     C                   kfld                    kcaeepa
393900040616     C                   kfld                    kcaetfp
394000040616     C                   kfld                    kcaetfa
394100040616     C                   kfld                    kcaedde
394200040616     C     Kcae3         klist
394300040616     C                   kfld                    kcaeepa
394400040616     C                   kfld                    kcaetfp
394500040616     C                   kfld                    kcaetfa
394600960417     C*
394700960402     C* UDATE IN GGMMAAAA
394800960417     C                   TIME                    W0140            14 0
394900960402     C                   MOVE      W0140         WDTGIO            8 0
395000960422     C*
395100960422     C* PRENDO DATA IN AAMMGG
395200960422     C                   MOVEL     WDTGIO        G02DAT
395300960422     C                   MOVEL     0             G02ERR
395400971126     C                   CALL      'XSRDA8'
395500960422     C                   PARM                    WLBDAT
395600971126     C                   MOVE      G02INV        AM8GIO            8 0
395700001102     C* SE AZIONE SCHEDULATA IMPOSTO D34GES E D34FGS
395800001102     C     PARSCH        IFEQ      'V'
395900001102     C                   CLEAR                   DSLR34
396000001102     C                   MOVEL     'V'           D34GES
396100001102     C                   Z-ADD     SIMFEL        D34FGS
396200001102     C                   ENDIF
396300960327     C                   ENDSR
396400041020     C**************************************************************************
396500041020     C* SCRITTURA DEL FILE FNANT00F
396600041020     C**************************************************************************
396700041020     C     SCRANT        BEGSR
396800041020     C**
396900041020     C* PER L'ANOMALIA 145  FOA = ANMFLE
397000041020     C* PER L'ANOMALIA 146  FOA  = TERMINAL DI ARRIVO
397100041020    2C     ANMCAA        IFEQ      145
397200041020     C                   Z-ADD     ANMFLE        KFOA
397300041020   X2C                   ELSE
397400041020     C*
397500041020     C                   CLEAR                   KFOA
397600041020     C* PRENDO FLS SE LNP =0
397700041020     C*
397800041020     C                   CLEAR                   DSLV55
397900041020     C                   MOVEL     'A'           D55TPT
398000041020     C                   Z-ADD     ANMLNA        D55LIN
398100041020     C                   Z-ADD     ANMDAO        D55DRF
398200041020    3C     ANMLNP        IFGT      0
398300041020     C                   Z-ADD     ANMLNP        d55lnp
398400041020     C                   ELSE
398500041020     C                   Z-ADD     ANMFLS        d55lnp
398600041020    3C                   ENDIF
398700041020     C                   CALL      'FNLV55R'
398800041020     C                   PARM                    DSLV55
398900041020     C                   Z-ADD     D55TFA        KFOA
399000041020    2C                   ENDIF
399100041020     C*
399200041020     C     KANT          CHAIN     FNANT02L                           39
399300041020     C*
399400041020    2C     *IN39         IFEQ      *ON
399500041020     C                   CLEAR                   FNANT000
399600041020     C                   MOVEL     KFOA          ANTFOA
399700041020     C                   MOVEL     ANMLNA        ANTLNA
399800041020     C                   MOVEL     ANMDAO        ANTDAO
399900041020     C                   MOVEL     ANMCAA        ANTCAA
400000041020     C                   Z-ADD     1             ANTTOT
400100041020     c                   z-add     *date         antdt1
400200041020     C                   WRITE     FNANT000
400300041020   X2C                   ELSE
400400041020     C                   ADD       1             ANTTOT
400500041020     C                   CLEAR                   ANTFT1
400600041020     C                   CLEAR                   ANTFT2
400700041020     c                   z-add     *date         antdt1
400800041020     C                   UPDATE    FNANT000
400900041020    2C                   ENDIF
401000041020     C*
401100041020     C                   MOVEL     'T'           ANMFT1
401200041020     C                   ENDSR
401300911021     C*
401400960422     O* TESTATA IDD
401500911021     OPRTF198   E            TES              02
401600960604     O                       STA(1)              66
401700960604     O              N13      STA(2)             132
401800040614     o               13      STA(19)            132
401900921015     O                       STA(3)             198
402000900626     O                       RSUT                20
402100040615     O                       prtpo               46
402200040615     O                       D34FGS              50
402300960926     O                       PRTFGS              71
402400960402     O                       WDTGIO             168 '  .  .    '
402500921029     O                       PAGE1         Z    198
402600921015     O          E            TES         1
402700040615     O               70      STA(34)            132
402800960402     O                       UTIME              167 '  :  :  '
402900960402     O                  01                      190 '-C O N F E R M A-'
403000040615     O          E            TES         1
403100960402     O                       STA(18)             66
403200960402     O                       STA(20)            132
403300960402     O                       STA(21)            198
403400960506     O                       DESIE               61
403500960402     O                       D34DLG             106 '  /  /    '
403600960402     O                       D34ALG             123 '  /  /    '
403700960403     O                       D34DCG             172 '  /  /    '
403800960402     O                       D34ACG             189 '  /  /    '
403900030717     C*
404000040615     O          E            TES2        1
404100960403     O                  15   STA(16)             66
404200960403     O                 N15   STA(17)            132
404300960403     O                 N18   STA(15)            198
404400960403     O                  15   DESFV               29
404500990513     O                  15   D34NFV        Z     44
404600990513     O                  15   STADFV              61 '  /  /    '
404700980403     O                 N15   TESFSA             103
404800040611     O                 N18   D34LID             198
404900040614     O                                          120 'LINEA ARRIVO :'
405000980403     O               06      D34LNA             124
405100040614     O               06      PRTLNA             141
405200040614     O                                          154 'TIPO MERCE :'
405300040614     O                       PRTVAL             163
405400030717     O*
405500040615     O          E    23      TES2        1
405600030717     O                       STA(33)            132
405700030717     O                       D34POS              87
405800030717     O                       D34NRS             110
405900960403     O*
406000040615     O          E   N14 19   TES2        1
406100960426     O                       STA(32)             66
406200960426     O                       D34CCH              29
406300960426     O                                         +  0 '-'
406400960426     O                       TESCCH            +  0
406500040615     O          E    14      TES2        1
406600960426     O                  19   STA(32)             66
406700960426     O                       STARIG             132
406800960426     O                  19   D34CCH              29
406900960426     O                                         +  0 '-'
407000960426     O                  19   TESCCH            +  0
407100960418     O                       ANM(1)              94
407200960403     O                       ANM(2)            +  3
407300960403     O                       ANM(3)            +  3
407400960403     O                       ANM(4)            +  3
407500960403     O                       ANM(5)            +  3
407600960403     O                       ANM(6)            +  3
407700960403     O                       ANM(7)            +  3
407800960403     O                       ANM(8)            +  3
407900960403     O                       ANM(9)            +  3
408000960403     O                       ANM(10)           +  3
408100040615     o*
408200040615     O          E    80      TES3        1
408300040615     O                       STA(35)             66
408400040615     o                       stpo(1)             20
408500040615     o                       stpo(2)           +  1
408600040615     o                       stpo(3)           +  1
408700040615     o                       stpo(4)           +  1
408800040615     o                       stpo(5)           +  1
408900040615     o                       stpo(6)           +  1
409000040615     o                       stpo(7)           +  1
409100040615     o                       stpo(8)           +  1
409200040615     o                       stpo(9)           +  1
409300040615     o                       stpo(10)          +  1
409400040615     o                       stpo(11)          +  1
409500040615     o                       stpo(12)          +  1
409600040615     o                       stpo(13)          +  1
409700040615     o                       stpo(14)          +  1
409800040615     o                       stpo(15)          +  1
409900040615     o                       stpo(16)          +  1
410000040615     o                       stpo(17)          +  1
410100040615     o                       stpo(18)          +  1
410200040615     o                       stpo(19)          +  1
410300040615     o                       stpo(20)          +  1
410400040615     o                       stpo(21)          +  1
410500040615     o                       stpo(22)          +  1
410600040615     o                       stpo(23)          +  1
410700040615     o                       stpo(24)          +  1
410800040615     o                       stpo(25)          +  1
410900040615     o                       stpo(26)          +  1
411000040615     o                       stpo(27)          +  1
411100040615     o                       stpo(28)          +  1
411200040615     o                       stpo(29)          +  1
411300040615     o                       stpo(30)          +  1
411400040615     O          E            TES3        2
411500911021     O                       STA(4)              66
411600911021     O                       STA(7)             132
411700911021     O                       STA(10)            198
411800040616     O**               N04 17                   198 '     '
411900040615     O          E            TES3        1
412000911018     O                       STA(5)              66
412100911018     O                       STA(8)             132
412200911021     O                       STA(11)            198
412300040616     O**               N04 17                   198 '     '
412400960422     O*
412500960423     O          E            TESIDR      3  1
412600960422     O                       STA(25)             66
412700960422     O                       STA(26)            132
412800960513     O                       STA(13)            198
412900990513     O                       STADCI              82 '  /  /    '
413000990513     O                       STADCM             166 '  /  /    '
413100960423     O*
413200960423     O          E            TESIDE      3  1
413300960423     O                       STA(30)             66
413400960423     O                       STA(31)            132
413500960513     O                       STA(13)            198
413600990513     O                       STADCI              84 '  /  /    '
413700990513     O                       STADCM             166 '  /  /    '
413800911015     O*
413900911015     O          E            DET         1
414000990512     O                  02   PRTAAS        Z
414100921015     O                  02   ANMLNP        Z   +  1
414200921015     O                  02   ANMNRS        Z   +  1
414300921015     O                  02   ANMNSP        Z   +  1
414400921015     O                  02   ANMLNA            +  1
414500960417     O                  02   PRTDSP            +  1 '  /  '
414600960417     O*
414700990121     O                  03   ANMFLS            +  3
414800921015     O                  03   ANMLNA            +  1
414900921015     O                  03   ANMNRS        Z   +  1
415000921015     O                  03   ANMSCN        Z   +  1
415100040609     O                  03   PRTZNC            +  1
415200040609     O                  03   anmft4            +  0
415300990121     O                  03   ANMNPS        Z   +  1
415400040616     O                       DESTAN            +  1
415500990121     O                       ANMCAA            +  1
415600921015     O                                         +  0 '-'
415700990122     O              N40   N09DESCAA            +  0
415800990122     O                     09DESCA1              95
415900950320     O                     09ARBNDC        Z   +  0
416000950320     O                     09STADDC            +  1 '  /  '
416100950320     O                     09STAPDC            +  1
416200040616     O                     20PSTAM              110
416300040616     O                     20WDFLP              107
416400040616     O                     20PRTDET             118 '  /  '
416500040616     O                     21CDOVE              110
416600040616     O                     21PRTDOV             101
416700040616     O                     21WFGS               107
416800040616     O                     21PRTDFV             118 '  /  '
416900990121     O* NUMERO C.A. SU CUI E' IL COLLO
417000040616     O                     40CCOAN              109
417100040616     O                     40DESCA1              91
417200040616     O                     40DCTFIL        Z     98
417300040616     O                     40DCTNCA        Z    106
417400040616     O                     40STADCA             118 '  /  /  '
417500040616     O                  42 40                   124 'CHIUSA'
417600040616     O                       STALID             125
417700040616     O                       STADAc            +  1 '  /  '
417800040616     O                       STAorc            +  1 '  :  '
417900040616     O                       DESFSA            +  1
418000950320     O                       STADAO            +  1 '  /  '
418100040616     O                       ANMCDU        Z   +  1
418200040616     O                       ANMNFV        Z   +  1
418300990121     O                       DESFSC            +  1
418400950320     O                       STADCH            +  1 '  /  '
418500970522     O                       CCHPRT            +  1
418600960423     O                                         +  0 '-'
418700960423     O                       DESCCH            +  0
418800040616     O                       ANMFLE            +  1
418900050503     O*  STAMPA NUMERO C.A. e/o particolarità consegna CP
419000050503     O          E            DET1        1
419100050503     O               43      decodiCP            96
419200050503     O               41      CCOAN              109
419300050503     O               41      DCTFIL        Z     98
419400050503     O               41      DCTNCA        Z    106
419500050503     O               41      STADCA             118 '  /  /  '
419600050503     O               41                         128 'CHIUSA IL'
419700050503     O               41      STACHA             134 '  /  '
419800050504     O*  STAMPA NUMERO C.A. e/o particolarità consegna CP
419900050504     O          E            DET1        0  0
420000050504     O               43      decodiCP            96
420100051221     O*  STAMPA dati mittente e destinatario
420200051221     O          E            DET2        1
420300051222     o*                                        + 30 'Mittente:'
420400051222     o                                         + 69 'Mittente:'
420500051221     o                       blprsm            +  0
420600051222     o                                         +  2 'Rif.Mitt.Num:'
420700051222     o                       prtrmn            +  0
420800051222     o                                         +  2 'Destinatario:'
420900051221     o                       blprsd            +  0
421000151125     O*  STAMPA consegna anomala 7
421100151125     O          E            DET3        1
421200151125     o                                         + 30 'Bolla chiusa per '
421300151125     o                                         +  0 'merce mai affidata il'
421400151125     o                       prtdcm            +  2 '  /  '
421500160908     O
421600160908     O*  STAMPA spedizione in partenza per anomalia 010
421700160908     O          E            DET4        1
421800160908     o                                         + 30 'Bolla confermata DA '
421900160908     o                                         +  0 'PARTIRE '
422000960422     O*
422100960422     O*  STAMPA RIGA: DETTAGLIO FILIALE CHE NON HANNO FATTO L'IDD
422200960422     O          E            DETIDD      1
422300960423     O                       FID(1)        Z
422400960422     O                                         +  0 '-'
422500960422     O                       DFI(1)            +  0
422600050831     O                       FID(2)        Z   +  2
422700960422     O                                         +  0 '-'
422800960422     O                       DFI(2)            +  0
422900050831     O                       FID(3)        Z   +  2
423000960422     O                                         +  0 '-'
423100960422     O                       DFI(3)            +  0
423200050831     O                       FID(4)        Z   +  2
423300960422     O                                         +  0 '-'
423400960422     O                       DFI(4)            +  0
423500050831     O                       FID(5)        Z   +  2
423600960422     O                                         +  0 '-'
423700960422     O                       DFI(5)            +  0
423800050831     O                       FID(6)        Z   +  2
423900960422     O                                         +  0 '-'
424000960422     O                       DFI(6)            +  0
424100050831     O                       FID(7)        Z   +  2
424200960422     O                                         +  0 '-'
424300960422     O                       DFI(7)            +  0
424400960422     O*
424500960422     O*  STAMPA RIGA: NESSUNA ANOMALIA RISCONTRATA
424600960422     O          E            NOANOM      3  3
424700960422     O                       STA(24)            132
424800960423     O*  STAMPA RIGA: TUTTE LE FILIALI HANNO FATTO L'IDD
424900960423     O          E            SIIDD       2
425000960423     O                       STA(27)             66
425100960423     O                       STA(28)            132
425200960423     O                       STA(29)            198
425300960422     O*  STAMPA IDD3
425400911025     OPRTF198X  E            TESX             02
425500960604     O                       STA(1)              66
425600040618     O                       STA(14)            132
425700040618     O                       STA(3)             198
425800040618     O                       prtpo               46
425900960926     O                       D34FGS              50
426000960926     O                       PRTFGS              71
426100960604     O                       §7ALCI             121
426200911025     O                       RSUT                20
426300960402     O                       WDTGIO             168 '  .  .    '
426400921015     O                       PAGE2         Z    198
426500960403     O          E            TESX        1
426600960402     O                       UTIME              167 '  :  :  '
426700960402     O                  01                      190 '-C O N F E R M A-'
426800921015     O          E            TESX        2
426900911025     O                       STA(4)              66
427000911025     O                       STA(7)             132
427100911025     O                       STA(10)            198
427200921016     O                                          198 '     '
427300911025     O          E            TESX        1
427400911025     O                       STA(5)              66
427500911025     O                       STA(8)             132
427600911025     O                       STA(11)            198
427700921016     O                                          198 '     '
427800911025     O*
427900911025     O          E            DETX        1
428000990512     O                  02   PRTAAS        Z
428100921015     O                  02   ANMLNP        Z   +  1
428200921015     O                  02   ANMNRS        Z   +  1
428300921015     O                  02   ANMNSP        Z   +  1
428400921015     O                  02   ANMLNA            +  1
428500960417     O                  02   PRTDSP            +  1 '  /  '
428600960417     O*
428700990121     O                  03   ANMFLS            +  3
428800921015     O                  03   ANMLNA            +  1
428900921015     O                  03   ANMNRS        Z   +  1
429000921015     O                  03   ANMSCN        Z   +  1
429100130423     O                  03   PRTZNC            +  1
429200130423     O                  03   anmft4            +  0
429300990121     O                  03   ANMNPS        Z   +  1
429400990121     O                       DESTAN            +  1
429500990121     O                       ANMCAA            +  1
429600921015     O                                         +  0 '-'
429700990122     O                    N40DESCAA            +  0
429800040616     O                     20PSTAM              110
429900040616     O                     20WDFLP              107
430000040616     O                     20PRTDET             118 '  /  '
430100040616     O                     21CDOVE              110
430200040616     O                     21PRTDOV             101
430300040616     O                     21WFGS               107
430400040616     O                     21PRTDFV             118 '  /  '
430500990122     O* NUMERO C.A. SU CUI E' IL COLLO
430600040616     O                     40CCOAN              109
430700040616     O                     40DESCA1              91
430800040616     O                     40DCTFIL        Z     98
430900040616     O                     40DCTNCA        Z    106
431000040616     O                     40STADCA             118 '  /  /  '
431100040616     O                  42 40                   124 'CHIUSA'
431200040616     O                       ANMLID             125
431300040616     O                       STADAc            +  1 '  /  '
431400040616     O                       STAorc            +  1 '  :  '
431500040616     O                       DESFSA            +  1
431600040616     O                       STADAO            +  1 '  /  '
431700040616     O                       ANMCDU        Z   +  1
431800040616     O                       ANMNFV        Z   +  1
431900990121     O                       DESFSC            +  1
432000040616     O                       STADCH            +  1 '  /  '
432100960423     O                       ANMCCH            +  1
432200960423     O                                         +  0 '-'
432300960423     O                       DESCCH            +  0
432400040616     O                       ANMFLE            +  1
432500990122     O*  STAMPA NUMERO C.A.
432600050503     O          E            DETX1       1
432700050503     O               43      decodiCP            96
432800050503     O               41      CCOAN              109
432900050503     O               41      DCTFIL        Z     98
433000050503     O               41      DCTNCA        Z    106
433100050503     O               41      STADCA             118 '  /  /  '
433200050503     O               41                         128 'CHIUSA IL'
433300050503     O               41      STACHA             134 '  /  '
433400050504     O          E            DETX1       0  0
433500050504     O               43      decodiCP            96
433600990122     O*
433700990512**
433800040615XXXXXXXXXXXXXXXXXXXX        xxxxxxxxxxxxxxxxxx XXX-XXXXXXXXXXXXXXX
433900960926XXXXX           **   L I S T A     A N O M A L I E   **             2
434000960417    FNLR35R               XX/XX/XXXX                     PAG. XXXX  3
434100040616    S P E D I Z I O N E          S E G N A C O L L O      A      N
434200040616AN LNP SER NUMERO LNA  DATA   LNP LNA SER NUMERO ZON PIS T I P O
434300040616XX XXX XX XXXXXXX XXX XX/XX   XXX XXX XX XXXXXXX xxV XX xxxxxxxxxx
434400040616      O      M      A      L      I      A               LIV CREAZ
434500040616    C  O  D  I  C  E                                     IDD DATA
434600040616xx XXX-bolla in consegna:XXXXXXX XX/XX XXXXXXXXXXXX  X    X XX/xx
434700040616IONE  A  P  E  R  T  U  R  A    C  H  I  U  S  U  R  A         P.O  10
434800040616 ORA  FASE     DATA   FOGLIO    FASE      DATA  CAUSALE        EMI  11
434900040616xx:xx xxxxxxxx XX/XX XXX XXXXXX xxxxxxxx XX/XX XX-XXXXXXXXXXXX XXX 12
435000990513RNO PRECEDENTE, IN DATA XX/XX/XXXX) :
435100960604        **   E L A B O R A Z I O N E      " I. D. D.  X "   **     14
435200040614                                  ANOMALIE IDD DAL LIVELLO IDD : X 15
435300990513ANOMALIE DEL FOGLIO  XXXXXXXX   N.RO  XXXXXX   DEL XX/XX/XXXX      16
435400040614FASE APERTURA ANOMALIE : XXXXXXXXXXXX                              17
435500960403ANOMALIE :  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX         18
435600960604        **   E L A B O R A Z I O N E      "I. D. D."   **          19
435700960402DATA APERTURA ANOMALIA : DAL  XX/XX/XXXX   AL  XX/XX/XXXX          20
435800960402DATA CHIUSURA ANOMALIA : DAL  XX/XX/XXXX   AL  XX/XX/XXXX          21
435900960506CODICI       RICHIESTI : XXX                                       22
436000960506CODICI  NON  RICHIESTI : XXX                                       23
436100960422 *** NESSUNA  ANOMALIA  RISCONTRATA  NEI  PARAMETRI  RICHIESTI *** 24
436200960926ELENCO PUNTI OPER.DI CUI MANCA LA TRASMISSIONE ANOMALIE I.D.D. IN  25
436300990513DATA  XX/XX/XXXX     (PER I PUNTI OPER. CHE ESEGUONO L'IDD DEL GIO 26
436400960926                                                    T U T T I    I 27
436500030717    P U N T I    O P E R A T I V I    H A N N O    C O N F E R M A 28
436600030717 T O    L'  I . D . D .                                            29
436700030717ELENCO PUNTI OPER.DELL'AREA DI CUI MANCA LA CONFERMA DELL'I.D.D. I 30
436800030717N DATA  XX/XX/XXXX   (PER I PUNTI OPER. CHE ESEGUONO L'IDD DEL GIO 31
436900960426CAUSALE CHIUSURA ANOMALIA: XX-XXXXXXXXXXXXXXXXXXXXXXXXX            32
437000030717P.O. SEGNACOLLO : XXX      NUMERO SERIE : XX                       33
437100040615                **  COLLI   DI   VALORE   MANCANTI   **            34
437200040615Punti Operativi: xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx   35
437300921016**
437400960506A R R I V O
437500960506P A R T E N Z A
437600960506  N O N    I.D.D.
437700960506  I . D . D.
437800960506  I N T E R N E
437900960506  T U T T E
438000960506     SOLO APERTE
438100960506     SOLO CHIUSE

000100040601     H DECEDIT('0,') DATEDIT(*yMd.)
000200960327     H* FNLR35R *----------------------------------------------------*
000300921027     H*        - LISTA ANOMALIE / I.D.D.
000400060619     h* Necessita di ovrdbf fnanmsed fnanm00f
000500000000     H*--------------------------------------------------------------*
000600960409     FTABEL00F  UF   E           K DISK
000700031007     FTNTBE01L  IF   E           K DISK
000800031031     FWFLAN01L  UF A E           K DISK    USROPN
000900911021     FAZORG01L  IF   E           K DISK
001000960510     FAZCLN01L  IF   E           K DISK
001100040615     FAZCAE03l  IF   E           K DISK
001200040616     FAZCAE01l  IF   E           K DISK
001300040616     F                                     RENAME(azcae000:azcae001)
001400990121     FFNDCD02L  IF   E           K DISK
001500990122     FFNDCT01L  IF   E           K DISK
001800960508     FFNFVV01L  IF   E           K DISK
001900960930     FFNFGV01L  IF   E           K DISK
002000000503     FFNFGW01L  IF   E           K DISK
002100960930     FFNFVA01L  IF   E           K DISK
002200070111     FFNBRV07L  IF   E           K DISK
002300070111     FFNBRV03L  IF   E           K DISK
002400070111     F                                     RENAME(FNBRV000:BRV003)
002500970305     FFNANT02L  UF A E           K DISK
002600990511     FFNANM00F  UF A E             DISK
002700060619     FFNANMSED  o  A E             DISK    usropn
002800060619     F                                     RENAME(FNANM000:FNANM0SD)
002900990511     FFNANM01L  IF   E           K DISK
003000990511     F                                     RENAME(FNANM000:FNANM001)
003100990511     F                                     INFDS(FNANM1)
003200990511     FFNANM02L  IF   E           K DISK
003300990511     F                                     RENAME(FNANM000:FNANM002)
003400020905     F                                     prefix(AN2:3)
003500990511     F                                     INFDS(FNANM2)
003600990511     FFNANM04L  IF   E           K DISK
003700990511     F                                     RENAME(FNANM000:FNANM004)
003800990511     FFNANM10L  IF   E           K DISK
003900990511     F                                     RENAME(FNANM000:FNANM010)
004000990511     F                                     INFDS(FNAN10)
004100021021     FFNANM20L  IF   E           K DISK
004200021021     F                                     RENAME(FNANM000:FNANM020)
004300990511     FFNANM07L  IF   E           K DISK
004400990511     F                                     RENAME(FNANM000:FNANM007)
004500990511     F                                     INFDS(FNANM7)
004600990511     FFNANM08L  IF   E           K DISK
004700990511     F                                     RENAME(FNANM000:FNANM008)
004800990511     F                                     INFDS(FNANM8)
004900990511     FFNANM09L  IF   E           K DISK
005000990511     F                                     RENAME(FNANM000:FNANM009)
005100990511     F                                     INFDS(FNANM9)
005200990511     FFNANM03L  IF   E           K DISK
005300990511     F                                     RENAME(FNANM000:FNANM003)
005400990511     F                                     INFDS(FNANM3)
005500001030     FFNANM11L  UF   E           K DISK
005600001030     F                                     RENAME(FNANM000:FNANM011)
005700990511     FFNANM00T  O  A E           K DISK    USROPN
005800990511     F                                     RENAME(FNANM000:FNANM0TT)
005900090312     FFiART27L  IF   E           K DISK
006000950320     FFNARB01L  IF   E           K DISK
006200960412     FFNBLP01L  IF   E           K DISK
006500021203     FFIAPD01L  IF   E           K DISK
006600960403     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
006700960403     F                                     USROPN
006800950321     F                                     FORMLEN(66)
006900950321     F                                     FORMOFL(63)
007000960403     FPRTF198X  O    F  198        PRINTER OFLIND(*INOE)
007100960403     F                                     USROPN
007200950321     F                                     FORMLEN(66)
007300950321     F                                     FORMOFL(63)
007400020905     D*
007500020905     D* DEFINIZIONE SCHIERE A TEMPO DI COMPILAZIONE
007600020905     D*
007700040615     D STA             S             66    DIM(35) CTDATA PERRCD(1)             SKIERE STAMPA
007800960506     D DAN             S             17    DIM(8) CTDATA PERRCD(1)              DESCR ANMOMAL
007900060619     D CMD1            S             49    DIM(1) CTDATA PERRCD(1)
008000060619     D CMD3            S             49    DIM(1) CTDATA PERRCD(1)
008100060619     D CMD5            S             49    DIM(1) CTDATA PERRCD(1)
008200060619     D OVRSED          S             49    DIM(1) CTDATA PERRCD(1)
008300921016     D EL              S              1    DIM(4)                               ANOM DA ELAB
008400020905     D*
008500020905     D* DEFINIZIONE SCHIERE A TEMPO DI ESECUZIONE
008600960326     D*
008700960927     D L6              S              3  0 DIM(30)                              P.O.GEST
008800040614     D L1              S              3  0 DIM(30)                              P.O.AREA
008900040614     D L1RIF           S              3  0 DIM(30)                              P.O.AREA
009000960326     D*
009100961017     D FLE             S              3  0 DIM(50)                              ANOM.USO FLE
009200970829     D FLO             S              3  0 DIM(50)                              ANOM.ARR FLE SOLO
009300970829     D FLT             S              3  0 DIM(50)                              ANOM.ARR FLE E LNA
009400970829     D FLM             S              3  0 DIM(50)                              ANOM.PAR FLE SOLO
009500970829     D FLR             S              3  0 DIM(50)                              ANOM.PAR FLE E LNP
009600960422     D*
009700960927     D FID             S              3  0 DIM(7)                               P.O. NO IDD
009800050831     D DFI             S             22    DIM(7)                               DECODIFC FIL
009900960927     D FM1             S              3  0 DIM(100)                             P.O. IDD-1
010000960510     D*
010100960506     D K51             S             17    DIM(3)                               COMODO
010200040615     D STpo            S              3    DIM(30)
010300020905     D*
010400020905     D* DEFINIZIONE STRUTTURE DATI (INTERNE)
010500960327     D*
010600990511     D FNANM1          DS
010700960405     D  ANMNR1               397    400B 0
010800990511     D FNANM2          DS
010900960705     D  ANMNR2               397    400B 0
011000990511     D FNANM3          DS
011100970529     D  ANMNR3               397    400B 0
011200990511     D FNAN10          DS
011300960417     D  ANMN10               397    400B 0
011400990511     D FNANM7          DS
011500960327     D  ANMNR7               397    400B 0
011600990511     D FNANM8          DS
011700960327     D  ANMNR8               397    400B 0
011800990511     D FNANM9          DS
011900960327     D  ANMNR9               397    400B 0
012000960327     D**
012100911015     D WLBDAT          DS
012200971126     D  G02DAT                 1      8  0
012300971126     D  G02INV                 9     16  0
012400971126     D  G02ERR                17     17
012500971126     D  G02TGI                18     22  0
012600960510     D WGIDAT          DS
012700990512     D  GI8DAT                 1      8  0
012800990512     D  GI8INV                 9     16  0
012900990512     D  WAA                    9     12  0
013000990512     D  WMM                   13     14  0
013100990512     D  WGG                   15     16  0
013200990512     D  GI8TGI                17     21  0
013300960927     D                 DS
013400000503     D  FFV                    1    897
013500000503     D                                     DIM(299)                             LINEE FOGLIO
013600960927    +D  FGVFFV                 1    240
013700960927    +D  FGVFF2               241    447
013800000503    +D  FGWFF3               448    687
013900000503    +D  FGWFF4               688    897
014000960927     D*
014100960927     D                 DS
014200000503    +D  FLP                    1    897
014300000503     D                                     DIM(299)                             LINEE TRANSITO
014400960927    +D  FGVFLP                 1    240
014500960927    +D  FGVFL2               241    447
014600000503    +D  FGWFL3               448    687
014700000503    +D  FGWFL4               688    897
014800960510     D                 DS
014900960510     D  CLNPOM                 1     31
015000960510     D  POM                    1     31
015100960510     D                                     DIM(31)                              FESTIV.MATTINO
015200990121     D                 DS
015300031007     D  DCTAAC                 1      4  0
015400031007     D  DCTMGC                 5      8  0
015500031007     D  DCTDCA                 1      8  0 INZ
015600021008     D*
015700021008     D DSmembro        ds             7    inz
015800021008     D  MbrM                   1      1    inz('M')
015900021008     D  MbrPOP                 2      4  0 inz
016000021008     D  MbrPOA                 5      7  0 inz
016100020905     D*
016200020905     D* DEFINIZIONE STRUTTURE DATI (ESTERNE)
016300020905     D*
016400900622     D KPJBA         E DS
016500021008     ***D  LIBSYS                92    100
016600001102     D  PARSCH               502    502
016700020905     D UT§DSE0F      E DS
016800020905     D CNCR80        E DS
016900960422     D OG148         E DS
017000991217     D OG143         E DS
017100960510     D DS1Y          E DS
017200990902     D DS7A          E DS
017300990902     D DS7AF         E DS
017400960403     D DS7A2         E DS
017500921027     D DS7C          E DS
017600020905     D DCCH          E DS
017700911021     D DSCMZ         E DS
017800990511     D DSANM         E DS                  EXTNAME(FNANM00F)
017900031007     D DLAN          E DS
018000040616     D DANMFLO       E DS
018100960326     D*
018200960326     D* PASSAGGIO PARAMETRI - DAL PGM FNLR34R FNLR49R
018300960326     D DSLR34        E DS                  EXTNAME(FNLR34DS)
018400990513     D  ANM                   89    118
018500921015     D                                     DIM(10)                              ANOMALIE VIDEO
018600040617     D  spo                  140    229
018700040614     D                                     DIM(30)                              ANOMALIE VIDEO
018800960326     D*
018900960326     D* ds per pgm trul06r - caricamento £x
019000960326     D DSUL06        E DS                  EXTNAME(TRUL06DS)
019100960326     D  LIN                    1     90  0
019200960927     D                                     DIM(30)                              COMODO
019300040614      * Parametri per richiamo a TRUL09R - carico p.o. 2 livello in arr
019400040614     d Trul09ds      e ds
019500040614     d  po2liv                19    108  0 dim(30)
019600020905     D* PASSAGGIO PARAMETRI AL PGM DI GESTIONE ANOMALIE
019700020905     D DSLR44        E DS                  EXTNAME(FNLR44DS)
019800020905     D* PASSAGGIO PARAMETRI AL PGM DI ALLOCAZIONE FILE
019900021009     D FNLV60DS      E DS
020000020905     D* CALL A PGM CHE CARICA I DATI FOGLI  - FNLV53R
020100020905     D DSLV53        E DS                  EXTNAME(FNLV53DS)
020200020905     D DSLV55        E DS                  EXTNAME(FNLV55DS)
020300020905     D DSLR33        E DS                  EXTNAME(FNLR33DS)
020400020930     D TIBS55DS      E DS
020500020905     D DSBS02        E DS                  EXTNAME(TIBS02DS)
020600040811      * ds per il controllo della presenza di CA per la spedizione richiesta
020700040811     d FIDN12DS      e ds
020800040811     d  skKey                 26    305    dim(20)
020900040811     d  skAnn                306    325    dim(20)
021000040811     d  skDca                326    485  0 dim(20)
021100040811     d  skFca                486    545  0 dim(20)
021200040811     d  skDch                546    705  0 dim(20)
021300040811     d  skCch                706    745    dim(20)
021400040811
021500040811     d dsKey           ds
021600040811     d  dsaac                         4  0
021700040811     d  dsfil                         3  0
021800040811     d  dsnca                         7  0
021900040811      *
022000031007     D****
022100031007     D* DS RIDEFINIZIONE CHIAVE X TABELLA "LAN"
022200031007     D****
022300031007     D                 DS
022400040615     D  WD34POS                            lIKE(D34POS)
022500031007     D  WD34NRS                            LIKE(D34NRS)
022600031007     D  DSKEYLAN               1      5
022700020905     D*
022800020905     D* DEFINIZIONE CAMPI
022900020905     D*
023000040505     D WLNP            s                   LIKE(anmlnp) inz
023100020905     D FGSTFP          S                   LIKE(FVVFGS) inz
023200020905     D WLNA            S                   LIKE(FVVFGS) inz
023300020905     D WERR            S                   LIKE(D60ERR) inz
023400020905     D WMB1            S                   LIKE(D60MB1) inz
023500020905     D WMB2            S                   LIKE(D60MB2) inz
023600020905     D SAVFL1          S                   LIKE(ANMFL1) inz
023700020905     D SAVFL2          S                   LIKE(ANMFL2) inz
023800020905     D KTFP            S                   LIKE(CLNTFP) inz
023900020905     D KANN            S                   LIKE(CLNANN) inz
024000020905     D KMES            S                   LIKE(CLNMES) inz
024100020905     D FELDID          S                   LIKE(§7ADID) inz
024200020905     D KDCH            S                   LIKE(ANMDCH) inz
024300020905     D KFIL            S                   LIKE(ANMLNP) inz
024400020905     D KLNP            S                   LIKE(ANMLNP) inz
024500020905     D KLNP9           S                   LIKE(ANMLNP) inz
024600020905     D KLNP92          S                   LIKE(ANMLNP) inz
024700020905     D KFLS            S                   LIKE(ANMLNP) inz
024800020905     D KLNA            S                   LIKE(ANMLNA) inz
024900020905     D SAVLNA          S                   LIKE(ANMLNA) inz
025000020905     D KLNA7           S                   LIKE(ANMLNA) inz
025100020905     D KLNA8           S                   LIKE(ANMLNA) inz
025200020905     D KAIE            S                   LIKE(ANMAIE) inz
025300020905     D KCAA            S                   LIKE(ANMCAA) inz
025400020905     D SAVCAA          S                   LIKE(ANMCAA) inz
025500020905     D WCAA            S                   LIKE(ANMCAA) inz
025600020905     D KAAS2           S                   LIKE(ANMAAS) inz
025700020905     D KLNP2           S                   LIKE(ANMLNP) inz
025800020905     D KNRS2           S                   LIKE(ANMNRS) inz
025900020905     D KNSP2           S                   LIKE(ANMNSP) inz
026000020905     D KDAO            S                   LIKE(ANMDAO) inz
026100020905     D W53DFV          S                   LIKE(D53DFV) inz
026200020905     D SAVNFV          S                   LIKE(ANMNFV) inz
026300020905     D CCHPRT          S                   LIKE(ANMCCH) inz
026400020905     D KCDU            S                   LIKE(ANMCDU) inz
026500020905     D WDDET           S                   LIKE(ARTDET) inz
026600020905     D WDFLP           S                   LIKE(ARTFLP) inz
026700020905     D KNPG            S                   LIKE(BRVNPG) inz
026800020905     D WNFV            S                   LIKE(FVVNFV) inz
026900020905     D WNPG            S                   LIKE(FVVNPG) inz
027000020905     D WSPG            S                   LIKE(FVVSPG) inz
027100020905     D WDFV            S                   LIKE(FVVDFV) inz
027200020905     D WFGS            S                   LIKE(FVVFGS) inz
027300020905     D WMAX            S                   LIKE(TBLKEY) inz
027400021203     D KPDPDR          S                   LIKE(APDpdr)
027500021203     D KPDTIP          S                   LIKE(APDtip)
027600040401     D SAV_dslr34      S                   LIKE(dslr34)
027700040614     D savl1dao        S                   LIKE(anmdao) inz
027800040614     D sav2ldao        S                   LIKE(anmdao) inz
027900040615     D kcaeepa         S                   LIKE(caeepa) inz
028000040615     D kcaetfp         S                   LIKE(caetfp) inz
028100040616     D kcaetfe         S                   LIKE(caetfe) inz
028200040615     D kcaedde         S                   LIKE(caedde) inz
028300040616     D kcaetfa         S                   LIKE(caetfa) inz
028400040401     d wgir200         s              1
028500040616     d wsetgt          s              1
028600040401     d wdta            s               d   datfmt(*Iso)
028700040405     d wdta200         s                   LIKE(D34ALA)
028800041020     D KFOA            S                   LIKE(ANTFOA)
028900041020     D KANLNP          S                   LIKE(ANTLNP) inz(000)
028901090312     D Knfv            S                   LIKE(fgvnfv)
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
031700960927     C* 07    - STAMPA LE ANOMALIE LOCALI
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
033200970314     C* 22    - CONDIZIONAMENTO IN STAMPA DEL N.SEGNACOLLO PER ANOM.300
033300040614     C* 23    - selezione p.o. segnacollo
033400990114     C* 24/29 - INDICANO QUALE FILE STO LEGGENDO
033500960327     C* 30/31 - LETTURA
033600960327     C* 32    - COMODO
033700990122     C* 40/42 - STAMPO NUMERO C.A. E DATE C.A. SE C'E' SU COLLO
033800050503     C* 43    - stampa particolarità consegne: CP se presente
033900051221     C* 44    - stampa dati mittente e destinatario se richiesto
034000040615     C* 70    - COLLI DI VALORE MANCANTI
034100040615     C* 80    - campo d34spo pieno, passati p.o. da stampare
034200040615     C* 95/95 - per allocazione membri
034300040615     C* 97    - ANOMALIA IDD
034400960404     C* 98/99 - PER I FILE DI TRASMISSIONE
034500900828     C*-----------------------------------------------------
034600911015     C                   EXSR      IMPO
034700960416     C*
034800001030     C                   SELECT
034900001030     C* SE SOLO INVIO RICHIAMO ROUTINE DI LETTURA
035000001030     C* DELLE ANOMALIE DA TRASMETTERE CON AZIONE SCHEDULATA
035100960416     C* ELABORO SOLO SE LA DATA ULTIMO IDD E' MINORE DELLA DATA ELABORA
035200001030     C     D34GES        WHENEQ    'V'
035300001030     C                   EXSR      SCHANM
035400001030     C*
035500001030    1C     *IN01         WHENEQ    *OFF
035600990512     C     §7ADID        ORLT      D34ALA
035700000317     C*
035800960416     C*
035900960405     C                   MOVEL     'S'           D34RIL
036000960404     C     RILEG         TAG
036100960404     C                   CLEAR                   D34END
036200960327     C**
036300960327     C* RILEGGO SE MI E' STATO CHIESTO DI RICARICARE
036400960327     C**
036500000317    3C     D34RIL        DOWEQ     'S'
036600960327     C                   CLEAR                   D34RIL
036700960402     C                   CLEAR                   WUSCIT
036800960327     C**
036900960326     C* LEGGO PRIMA LE ANOMALIE PARTENZA
037000960326     C**
037100000317    4C     D34PAR        IFEQ      'P'
037200960412     C     D34PAR        OREQ      ' '
037300980327     C                   MOVEL     'P'           WANOM             1
037400970829     C* SONO LE ANOMALIE CHE USANO SOLO FLE PER ESCUDERLE DAL GIRO
037500970829     C*  DELLA LETTURA PER LNP
037600970829     C                   MOVE      FLM           FLE
037700960412     C                   EXSR      PARTEN
037800970529     C*
037900970529     C* POI ELABORO LE ANOMALIE RICEVUTE PER LE QUALI SONO FILIALE DI
038000970529     C* TRANSITO O DISGUIDO (SOLO SE SONO SIMFEL)
038100970529     C* Potenzialmente questa elabor. sarebbe da fare anche per le anom
038200970529     C* arrivo ma per ora non serve perchè in arrivo riparte sempre
038300970529     C* tutto in automatico
038400980401     C* Solo se le anomalie richieste sono anche idd
038500000317    5C     D34IE         IFEQ      ' '
038600980401     C     D34IE         OREQ      'D'
038700000317     C     D34FGS        IFEQ      SIMFEL
038800970529     C                   EXSR      USACDU
038900000317     C                   END
039000000317    5C                   END
039100970829     C**
039200970829     C* POI  LEGGO  LE ANOMALIE UTILIZZANDO ANMFLE
039300970829     C*  CONTENUTE NELLA SKIERA FLR
039400970829     C**
039500970829     C                   MOVE      FLR           FLE
039600990114     C                   SETON                                        24
039700990114     C                   SETOFF                                       25
039800970829     C                   EXSR      USAFLE
039900000317    4C                   ENDIF
040000960327     C**
040100960327     C* POI  LEGGO  LE ANOMALIE ARRIVO
040200960327     C**
040300000317    4C     D34PAR        IFEQ      'A'
040400960412     C     D34PAR        OREQ      ' '
040500980327     C                   MOVEL     'A'           WANOM             1
040600970829     C* SONO LE ANOMALIE CHE USANO SOLO FLE PER ESCUDERLE DAL GIRO
040700970829     C*  DELLA LETTURA PER LNA
040800970829     C                   MOVE      FLO           FLE
040900960412     C                   EXSR      ARRIVO
041000960412     C**
041100960412     C* POI  LEGGO  LE ANOMALIE UTILIZZANDO ANMFLE
041200970829     C*  CONTENUTE NELLA SKIERA FLT
041300960412     C**
041400961017     C                   MOVE      FLT           FLE
041500990114     C                   SETOFF                                       24
041600990114     C                   SETON                                        25
041700960412     C                   EXSR      USAFLE
041800000317    4C                   ENDIF
041900000317    3C                   ENDDO
042000960327     C*
042100960327     C* FINE LETTURA : IN GESTIONE RICHIAMO LO STESSO IL PGM
042200000317    3C     D34F03        IFEQ      ' '
042300960327     C     D34F12        ANDEQ     ' '
042400000317    4C     D34GES        IFEQ      'I'
042500000317     C     D34GES        OREQ      'G'
042600960327     C                   CLEAR                   DSLR44
042700960327     C                   CLEAR                   D34F03
042800960327     C                   CLEAR                   D34F12
042900960327     C                   CLEAR                   D34RIL
043000960327     C                   MOVEL     'S'           D34END
043100960327     C                   MOVEL     DSLR34        D44LR
043200960521     C                   MOVEL     'G'           D44MOD
043300960327     C*
043400960327     C                   MOVEL     DSLR44        KPJBU
043500960327     C                   CALL      'FNLR44R'
043600960327     C                   PARM                    KPJBA
043700960402     C*
043800960402     C                   MOVEL     KPJBU         DSLR44
043900960402     C                   MOVEL     D44LR         DSLR34
044000960404     C     D34RIL        IFEQ      'S'
044100960412     C                   GOTO      RILEG                                        RILETTURA
044200960404     C                   ENDIF
044300960404     C*
044400000317    4C                   ENDIF
044500000317    3C                   ENDIF
044600040401     C**
044700040401     C* Stampa elenco anomalie 200 aperte fino a data apertura = data idd-1
044800040401     C**
044900040401     c     d34ela        ifne      *blanks
045000040401     c                   clear                   fle
045100040401     C                   MOVE      200           FLE(1)
045200040401     C                   SETOFF                                       24
045300040401     C                   SETON                                        25
045400040401     c* mi salvo la dslr34 prima di sporcarla
045500040401     c                   eval      SAV_dslr34 = dslr34
045600040401     c                   eval      wgir200='S'
045700040401     c                   eval      d34ges='S'
045800040401     c                   eval      d34ie ='I'
045900040401     c                   eval      d34anm = '200'
046000040401     c                   eval      d34inc = 'I'
046100040401     c                   eval      d34ela = *blanks
046200040405     c                   eval      wdta200=d34ala
046300070208     c* Cerco il primo giorno lavorativo
046400070208     C                   Z-ADD     D34ala        WDATA8
046500070208     C                   EXSR      RICDAT
046600070208     C                   Z-ADD     WDATAM        d34ala
046700070208     c
046800070208     c**** *iso          Movel     d34ala        wdta
046900070208     c****               subdur    1:*D          wdta
047000070208     c****               Movel     wdta          D34ala
047100040401     C                   exsr      usafle
047200040401     c                   eval      dslr34 = sav_dslr34
047300040401     c                   eval      wgir200=' '
047400040401     c                   endif
047500960422     C**
047600960422     C* STAMPA SE NESSUNA ANOMALIA RISCONTRATA
047700960422     C**
047800000317    3C     D34GES        IFEQ      'S'
047900960422     C*
048000000317    4C     WSTAMP        IFEQ      ' '
048100040615     c                   if        *inOF
048200040615     C                   EXCEPT    TES
048300040615     C  n70              EXCEPT    TES2
048400040615     C                   EXCEPT    TES3
048500040615     C                   SETOFF                                       OF
048600040615     c                   endif
048700960422     C*
048800960422     C                   EXCEPT    NOANOM
048900000317    4C                   ENDIF
049000960422     C**
049100960422     C* STAMPA ELENCO DI CHI NON HA FATTO L'IDD
049200040615     c*  se non richiamato per altre liste
049300960422     C**
049400040615     c                   if        not *in70
049500040615    4C     D34PAR        IFEQ      'P'
049600960506     C     D34IE         ANDEQ     'D'
049700960423     C     D34ELA        ORNE      ' '
049800960424     C     SIMFEL        ANDEQ     D34FGS
049900960423     C                   EXSR      NOIDD
050000000317    4C                   ENDIF
050100000317    3C                   ENDIF
050200040615    3C                   ENDIF
050300960327     C**
050400960327     C* I N V I O   I D D
050500960327     C**
050600000317    3C     *IN01         IFEQ      *ON
050700000317    4C     W7AIDD        IFEQ      'S'
050800960404     C* CONTROLLO PER TRASMETTERE LE CHIUSURE DELLE ANOMALIE  70 E 71
050900960404     C                   EXSR      CHIUAN
051000960405     C* INVIO FILE CON L'ANOMALIA OK IDD
051100970306     C                   EXSR      CAEX
051200000317     C* PER ELABORAZIONE IDD DEFINITIVA TRASMETTO I SALDI ANOMALIE
051300040927    5C***  D34FGS        IFEQ      SIMFEL
051400040927     C***  D34ALA        ORLE      FELDID
051500040927     C***                CALL      'FNLR38R'
051600040927     C***                PARM                    KPJBA
051700040927    5C***                ENDIF
051800000317    4C                   ENDIF
051900921027     C*
052000960409     C* AGGIORNO DATA ULTIMA ESECUZIONE IDD  CON DATA DEL GIORNO
052100960409     C                   MOVEL     '7A'          COD
052200960409     C                   MOVEL(P)  D34FGS        KEY
052300960409     C     KTAB          CHAIN     TABEL                              3130
052400000317    4C     *IN31         IFEQ      *OFF
052500960409     C     *IN30         ANDEQ     *OFF
052600960409     C                   MOVEL     TBLUNI        DS7AF
052700960409     C                   Z-ADD     D34ALA        §7ADID
052800960409     C                   MOVEL     DS7AF         TBLUNI
052900960409     C                   UPDATE    TABEL
053000000317    4C                   END
053100960409     C*
053200000317    3C                   ENDIF
053300001030     C                   ENDSL
053400000317     C**
053500000320     C**
053600960327     C                   CLEAR                   KPJBU
053700000317     C                   MOVEL     DSLR34        KPJBU
053800000317     C**
053900980327     C* TRASFERIMENTO DA FILIALE A FILIALE
054000040615    1C**   D34TRA        IFEQ      'S'
054100040615     C**   RECAN         ANDNE     *ZEROS
054200980327     C* MEMBRO
054300040615     c*                  reset                   DSmembro
054400040615     c*                  eval      MbrPOP = SIMfel
054500040615     c*                  eval      MbrPOA = D34fgs
054600040615     c*                  movel(p)  DSmembro      wMBR             10
054700980327     C* §CLR
054800040615     C*                  MOVE      'S'           §CLR
054900980327     C* §DST
055000040615     C*                  MOVE      D34LNA        §DST
055100980327     C*
055200040615     C*                  Z-ADD     RECAN         NRR
055300980327     C*
055400040615     C*                  CLEAR                   TIBS55DS
055500040615     C*                  movel     SIMfel        I50ppo
055600040615     C*                  movel     §Dst          I50apo
055700040615     C*                  CALL      'TIBS55R'
055800040615     C*                  PARM                    TIBS55DS
055900040615     C*                  MOVEL     O50PLA        CMZLBD
056000040615     C*                  MOVEL     O50ala        CMZLBA
056100040615     C*                  MOVEL     O50asi        CMZSYF
056200980327     C*
056300040615     C*                  CLOSE     FNANM00T
056400040615     C*                  EXSR      X25ANM
056500040615    1C*                  END
056600000320     C*
056700000320     C* ULTIMA DISALLOCAZIONE
056800021009     c                   eval      D60po0 = SIMfel
056900000320     C                   MOVEL     SAVFL1        D60FL1
057000000320     C                   MOVEL     SAVFL2        D60FL2
057100000320     C                   MOVEL     AM8GIO        D60DRF
057200000320     C                   MOVEL     'D'           D60AZN
057300000320     C                   MOVEL     'FNANM00T'    D60FIL
057400021009     C                   MOVEL     FNLV60DS      KPJBU
057500000320     C                   CALL      'FNLV60R'
057600000320     C                   PARM                    KPJBA
057700000320     C                   CLEAR                   SAVFL1
057800000320     C                   CLEAR                   SAVFL2
057900970603     C*
058000970603     C* RICHIAMO PGM PER CHIUDERE IN LR
058100970603     C     WCL53         IFEQ      '1'
058200970603     C                   MOVEL     'C'           D53TLA
058300970603     C                   CALL      'FNLV53R'
058400970603     C                   PARM                    DSLV53
058500970603     C                   END
058600971118     C*
058700971118     C                   MOVEL     'C'           D55TLA
058800971118     C                   CALL      'FNLV55R'
058900971118     C                   PARM                    DSLV55
059000020930     C                   CLEAR                   TIBS55DS
059100971125     C                   MOVEL     'C'           I50TLA
059200020930     C                   CALL      'TIBS55R'
059300020930     C                   PARM                    TIBS55DS
059400990122     C                   CLEAR                   DSBS02
059500990122     C                   MOVEL     'C'           T02TLA
059600990122     C                   CALL      'TIBS02R'
059700990122     C                   PARM                    KPJBA
059800990122     C                   PARM                    DSBS02
059900040614     C*
060000040614     c                   clear                   trul09ds
060100040614     C                   MOVEL     'C'           i09TLA
060200040614     C                   CALL      'TRUL09R'
060300040614     C                   PARM                    trul09ds
060400031031     C*
060500031031     C* Se aperto wrk-file WFLAN lo chiudo
060600031031     C                   IF        %open(WFLAN01L)
060700031031     C                   CLOSE     WFLAN01L
060800031031     C                   ENDIF
060900960416     C*
061000000000     C                   SETON                                        LR
061100911018     C*
061200911018     C* IMPOSTO CAMPI FISSI -----------------------------------------*
061300911018     C     IMPO          BEGSR
061400921015     C                   Z-ADD     0             PAGE2             4 0
061500921029     C                   Z-ADD     0             PAGE1             4 0
061600921029     C                   TIME                    UTIME             6 0
061700960604     C                   Z-ADD     0             SAVFL1
061800960403     C*
061900960403     C* SE HO RICHIESTO LA STAMPA APRO I FILE DI STAMPA
062000960403     C     D34GES        IFEQ      'S'
062100000317     C**
062200960403     C                   OPEN      PRTF198
062300960403     C     D34ELA        IFNE      ' '
062400960403     C                   OPEN      PRTF198X
062500960403     C                   ENDIF
062600971120     C**
062700971120     C                   SETON                                        OFOE
062800960604     C**
062900960927     C* DECODIFICO IL P.O. IN GESTIONE
063000040614     c                   if        d34fgs>0
063100960604     C     D34FGS        CHAIN     AZORG01L                           30
063200960604     C  N30              MOVEL     ORGDES        PRTFGS           20
063300040615     C* IMPOSTO LA SCRITTA IN TESTATA
063400040615     C                   MOVEL     CPO           PRTPO            18
063500040614     c                   endif
063600040615     c
063700040615     C* SE VENGO DALLA STAMPA DEI COLLI DI VALORE IMPOSTO
063800040615     C*  INDICATORE 70 E LA SCRITTA IN TESTATA
063900040615    1C                   IF        D34STV='S'
064000040615     C                   SETON                                        70
064100040615     C* SE STAMPA PER TERMINAL, IMPOSTO TERMINAL IN TESTATA
064200040615    2C                   IF        D34SPO<>*BLANKS  and d34spo<>*zeros
064300040615    3C                   IF        D34PAR='A'
064400040615     C                   MOVEL     CTERARR       PRTPO
064500040615     C                   ELSE
064600040615     C                   MOVEL     CTERPAR       PRTPO
064700040615    3C                   ENDIF
064800040615    2C                   ENDIF
064900040615    1C                   ENDIF
065000980402     C**
065100980402     C* DECODIFICO LA LINEA DI ARRIVO SE RICHIESTA
065200980402     C     D34LNA        IFNE      *ZEROS
065300980403     C     D34TRA        ANDNE     'S'
065400980402     C                   SETON                                        06
065500980402     C     D34LNA        CHAIN     AZORG01L                           30
065600040614     C  N30              MOVEL     ORGDES        PRTLNA           16
065700980402     C                   END
065800000317     C                   ENDIF
065900040614     c*
066000040614     c* Selezione p.o. segnacollo
066100040614     c                   if        d34pos>0
066200040614     C                   SETON                                        23
066300040614     C                   ENDIF
066400960327     C*
066500960412     C* CARICO IN UNA SCHIERA TUTTE LE ANOMALIE IDD E LE ANOMALIE
066600960412     C*  DA LEGGERE CON ANMFLE
066700960326     C                   MOVEL     '7C'          COD
066800960412     C                   Z-ADD     0             Y                 3 0
066900961017     C                   Z-ADD     0             T                 3 0
067000970829     C                   Z-ADD     0             U                 3 0
067100970829     C                   Z-ADD     0             V                 3 0
067200960326     C     KTAB2         SETLL     TABEL
067300960409     C     KTAB2         READE(N)  TABEL                                  30
067400960416    1C     *IN30         DOWEQ     *OFF
067500960326     C*
067600960416    2C     TBLFLG        IFEQ      ' '
067700960326     C                   MOVEL     TBLUNI        DS7C
067800960506     C*
067900960412     C* ANOMALIE CHE CONSIDERANO ANMFLE
068000961017     C* §7CUEA='S' --> CONSIDERANO SOLO ANMFLE
068100961017     C* §7CUEA='E' --> CONSIDERANO SIA ANMFLE CHE ANMLNA
068200960416    3C     §7CUEA        IFEQ      'S'
068300960412     C                   ADD       1             Y
068400961017     C                   ADD       1             T
068500961017     C                   MOVEL     TBLKEY        FLO(Y)
068600961023     C                   MOVEL     TBLKEY        FLT(T)
068700961017   X3C                   ELSE
068800961017    4C     §7CUEA        IFEQ      'E'
068900961017     C                   ADD       1             T
069000961023     C                   MOVEL     TBLKEY        FLT(T)
069100961017    4C                   ENDIF
069200960416    3C                   ENDIF
069300970829     C** PARTENZA
069400970829    3C     §7CUEP        IFEQ      'S'
069500970829     C                   ADD       1             U
069600970829     C                   ADD       1             V
069700970829     C                   MOVEL     TBLKEY        FLM(U)
069800970829     C                   MOVEL     TBLKEY        FLR(V)
069900970829   X3C                   ELSE
070000970829    4C     §7CUEP        IFEQ      'E'
070100970829     C                   ADD       1             V
070200970829     C                   MOVEL     TBLKEY        FLR(V)
070300970829    4C                   ENDIF
070400970829    3C                   ENDIF
070500960416    2C                   ENDIF
070600960326     C*
070700960409     C     KTAB2         READE(N)  TABEL                                  30
070800000317    1C                   ENDDO
070900921015     C*
071000960327     C* GIRO DATA FOGLIO SE C'E'
071100960327     C                   MOVE      D34DFV        G02INV
071200960327     C                   MOVEL     '3'           G02ERR
071300971126     C                   CALL      'XSRDA8'
071400960327     C                   PARM                    WLBDAT
071500990513     C                   MOVEL     G02DAT        STADFV            8 0
071600960327     C*
071700960326     C* SE NON IMMESSA DAL AL  = ALLA DATA DAL
071800960326     C     D34ALA        IFEQ      0
071900960403     C                   MOVEL     D34DLA        D34ALA                         DT APERTURA
072000960326     C                   ENDIF
072100960403     C     D34ACA        IFEQ      0
072200960403     C                   MOVEL     D34DCA        D34ACA                         DT CHIUSURA
072300960403     C                   ENDIF
072400960403     C*
072500960403     C* 14 ON - RICHIESTA ALMENO UNA ANOMALIA
072600960403     C     D34ANM        IFNE      *BLANKS
072700960403     C                   SETON                                        14
072800960403     C     D34INC        IFEQ      'I'
072900960403     C                   MOVEL     STA(22)       STARIG           66
073000960403     C                   ELSE
073100960403     C                   MOVEL     STA(23)       STARIG           66
073200960403     C                   ENDIF
073300960403     C                   ENDIF
073400921015     C*
073500921029     C* 15 ON  - IMMESSO NUMERO FOGLIO
073600960326     C     D34NFV        IFGT      0
073700921015     C                   SETON                                        15
073800960403     C                   ENDIF
073900921029     C*
074000921029     C* 16 ON  - IMMESSA FASE APERTURA
074100960326     C     D34FAS        IFNE      ' '
074200921015     C                   SETON                                        16
074300960403     C                   MOVEL     '7D'          COD
074400960403     C                   MOVEL(P)  D34FAS        KEY
074500960409     C     KTAB          CHAIN(N)  TABEL                              32
074600980403     C  N32              MOVEL     TBLUNI        TESFSA           12
074700960403     C  N32              MOVEL     TBLUNI        DESFV             8
074800960403     C                   ENDIF
074900040611     C**                           ì
075000960426     C* 19 ON  - IMMESSA CAUSALE CHIUSURA
075100960426     C     D34CCH        IFNE      ' '
075200960426     C                   SETON                                        19
075300960426     C                   MOVEL     '7E'          COD
075400960426     C                   MOVEL(P)  D34CCH        KEY
075500960426     C     KTAB          CHAIN(N)  TABEL                              32
075600960426     C  N32              MOVEL     TBLUNI        TESCCH           25
075700960426     C                   ENDIF
075800960403     C*
075900960403     C* RICHIESTA DELLE ANOMALIE SOLO APERTE O SOLO CHIUSE
076000960403     C     D34APR        IFEQ      'S'
076100960506     C                   MOVEL     DAN(7)        K51(3)
076200960426     C                   ENDIF
076300960426     C     D34APR        IFEQ      'C'
076400960506     C                   MOVEL     DAN(8)        K51(3)
076500960403     C                   ENDIF
076600040611     c                   if        d34bva='V'
076700040611     C                   MOVEL     vvalore       PRTVAL            8
076800040611     C                   ENDIF
076900921029     C**
077000921029     C* 13 ON  - RICHIESTA I.D.D.
077100921029     C**
077200000317    1C     D34ELA        IFNE      ' '
077300921029     C                   SETON                                        13
077400960403     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
077500960403     C* L'IDD
077600960403     C                   MOVEL     '7A'          COD
077700960403     C                   MOVEL     '2       '    KEY
077800960409     C     KTAB          CHAIN(N)  TABEL                              30
077900960403     C  N30              MOVEL     TBLUNI        DS7A2
078000921029     C*
078100921029     C* 01 ON  - ELABORAZIONE DEFINITIVA I.D.D.
078200000317    2C     D34ELA        IFEQ      'S'
078300921029     C                   SETON                                        01
078400960419     C*
078500960419     C* SE NON SONO SIMFEL VEDO A CHE DATA HA LANCIATO IL 1 LIVELLO
078600000317    3C     D34FGS        IFNE      SIMFEL
078700960419     C                   MOVEL     '7A'          COD
078800960419     C                   MOVEL(P)  SIMFEL        KEY
078900960419     C     KTAB          CHAIN(N)  TABEL                              31
079000960419     C   31              CLEAR                   DS7AF
079100960419     C  N31              MOVEL     TBLUNI        DS7AF
079200960419     C                   MOVE      §7ADID        FELDID
079300000317    3C                   ENDIF
079400921029     C* CHAIN A VARIABILI ARRIVI PER VEDERE SE DEVO INVIARE IDD O NO
079500921029     C                   MOVEL     '7A'          COD
079600921029     C                   MOVEL     '1       '    KEY
079700960409     C     KTAB          CHAIN(N)  TABEL                              30
079800990902     C  N30              MOVEL     TBLUNI        DS7A
079900990902     C   30              CLEAR                   DS7A
080000960417     C*
080100960417     C                   MOVEL     §7AIDD        W7AIDD            1
080200000317     C**
080300000317     C* SE DATA DI ELABORAZIONE <= ALLA DATA IN TABELLA, ESCO SENZA
080400000317     C*  ESEGUIRE IL PGM
080500000317     C                   MOVEL     '7A'          COD
080600000317     C                   MOVEL(P)  D34FGS        KEY
080700000317     C     KTAB          CHAIN(N)  TABEL                              31
080800000317     C   31              CLEAR                   DS7AF
080900000317     C  N31              MOVEL     TBLUNI        DS7AF
081000000317    2C                   ENDIF
081100000317    1C                   ENDIF
081200000317     C**
081300000317     C* D34GES='V' SOLO INVIO, IMPOSTO "S" IN W7AIDD
081400000317     C     D34GES        IFEQ      'V'
081500000317     C                   MOVEL     'S'           W7AIDD
081600000317     C                   ENDIF
081700921029     C*
081800000317    1C     D34GES        IFEQ      'V'
081900000317     C     D34ELA        OREQ      'S'
082000921029     C* SE DEVO INVIARE: CHKOBJ E ADDPF PER TUTTI I MEMBRI
082100000317    2C     W7AIDD        IFEQ      'S'
082200921029     C*
082300021008     C                   MOVE      ')'           VAR               8
082400060619     C                   Z-ADD     49            LUNG             15 5
082500921029     C* MEMBRO RICEZIONE
082600021008     c                   reset                   DSmembro
082700021008     c                   eval      MbrPOP = SIMfel
082800021008     c                   eval      MbrPOA = D34fgs
082900040615     c                   movel(p)  DSmembro      wMBR             10
083000000317    2C                   END
083100000317    1C                   END
083200960506     C**
083300960506     C*  ANOMALIE PARTENZA ARRIVO
083400960506     C**
083500960506     C     D34PAR        IFEQ      'P'
083600960506     C                   MOVEL     DAN(2)        K51(1)
083700960506     C                   ENDIF
083800960506     C     D34PAR        IFEQ      'A'
083900960506     C                   MOVEL     DAN(1)        K51(1)
084000960506     C                   ENDIF
084100921029     C**
084200960506     C* 17 OFF - RICHIESTO SPECIFICO TIPO DI ANOMALIE  INT/IDD/ NON IDD
084300921029     C**
084400960326    1C     D34IE         IFNE      ' '
084500921016     C                   SETON                                        17
084600921016     C* ANOMALIE INTERNE
084700960506     C     D34IE         IFEQ      'I'
084800960506     C                   MOVEL     DAN(5)        K51(2)
084900960327     C                   MOVEL     'I'           EL(1)
085000960403     C                   SETON                                        18
085100960506     C                   ELSE
085200960506     C* ANOMALIE IDD
085300960506    3C     D34IE         IFEQ      'D'
085400960506     C                   MOVEL     DAN(4)        K51(2)
085500960506   X3C                   ELSE
085600960506     C                   MOVEL     DAN(3)        K51(2)
085700960506    3C                   ENDIF
085800960506     C**
085900960506    3C     D34PAR        IFEQ      'P'
086000040116     C****               MOVEL     'R'           EL(1)
086100040116     C                   MOVEL     'E'           EL(1)
086200960506     C                   ELSE
086300960506     C                   MOVEL     'E'           EL(1)
086400960506    3C                   ENDIF
086500960506    2C                   ENDIF
086600960506     C*
086700960506   X1C                   ELSE
086800960506     C                   MOVEL     DAN(6)        K51(2)
086900960326    1C                   ENDIF
087000960506     C                   MOVEA     K51           DESIE            50
087100961018     C***
087200961018     C* CARICO £1
087300961018     C***
087400961104     C                   CLEAR                   DSUL06
087500961018     C                   MOVE      '£1'          D06COD
087600961018     C                   MOVEL     *BLANKS       D06TLA
087700961104     C                   MOVEL     SIMFEL        D06KEY
087800961018     C                   MOVEL     DSUL06        KPJBU
087900961018     C                   CALL      'TRUL06R'
088000961018     C                   PARM                    KPJBA
088100961018     C                   MOVEL     KPJBU         DSUL06
088200961018     C                   MOVEA     LIN           L1
088300960326     C*
088400960927     C* CALCOLO DATA -1 SE RICHIESTE IN PARTENZA LE ANOMALIE IDD
088500960927    1C     D34IE         IFEQ      'D'
088600990512     C     D34DCA        ANDGT     0
088700960927     C*
088800960927    2C     D34PAR        IFEQ      'P'
088900960927     C     D34PAR        OREQ      ' '
089000960927     C* SE IDD CHIUSE O CON CAUSALE PR
089100960927    3C     D34APR        IFEQ      'C'
089200960927     C     D34CCH        OREQ      'PR'
089300960927     C                   SETON                                        05
089400960927     C*
089500971126     C                   Z-ADD     D34DCA        WDATA8
089600960927     C                   EXSR      RICDAT
089700990512     C                   Z-ADD     WDATAM        W08DM1            8 0
089800960927     C*
089900971126    4C     D34ACA        IFGT      0
090000971126     C                   Z-ADD     D34ACA        WDATA8
090100960927     C                   EXSR      RICDAT
090200990512     C                   Z-ADD     WDATAM        W08AM1            8 0
090300960927    4C                   ENDIF
090400960927    3C                   ENDIF
090500960927    2C                   ENDIF
090600960927    1C                   ENDIF
090700960927     C*
090800960927     C* DETERMINO IN UNA SCHIERA I P.O. CHE FANNO L'IDD IL GIORNO DOPO
090900960927    1C     D34PAR        IFEQ      'P'
091000960927     C     D34PAR        OREQ      ' '
091100960927    2C     D34IE         IFNE      'I'
091200960927     C                   Z-ADD     1             L
091300960927     C     *LOVAL        SETLL     AZORG01L
091400960927     C                   READ      AZORG01L                               30
091500960927     C*
091600960927    3C     *IN30         DOWEQ     *OFF
091700960927     C* ESCLUDO LE ANNULLATE
091800960927    4C     ORGFVA        IFEQ      ' '
091900960927     C* SOLO FILIALE E AGENZIA
092000960927    5C     ORGFAG        IFEQ      'F'
092100960927     C     ORGFAG        OREQ      'A'
092200960927     C                   MOVEL     ORGDE8        OG148
092300960927     C**
092400960927    6C     §OGEID        IFNE      ' '
092500960927     C                   Z-ADD     ORGFIL        FM1(L)                         GIORNO PREC
092600960927     C                   ADD       1             L
092700960927    6C                   ENDIF
092800960927    5C                   ENDIF
092900960927    4C                   ENDIF
093000960927     C*
093100960927     C                   READ      AZORG01L                               30
093200960927    3C                   ENDDO
093300960927     C*
093400960927    2C                   ENDIF
093500960927    1C                   ENDIF
093600971126     C***
093700040614     C* CARICO LA L6 DEI P.O. IN GESTIONE  --> p.o. da elaborare solo se
093800040614     c*        schiera p.o. passata vuota
093900971126     C***
094000040614    0c                   if        d34spo=*blanks or d34spo=*zeros
094100971126     C                   CLEAR                   DSUL06
094200971126     C                   MOVE      '£6'          D06COD
094300971126     C                   MOVEL     'L'           D06TLA
094400971126     C                   MOVEL     D34FGS        D06KEY
094500971126     C                   MOVEL     DSUL06        KPJBU
094600971126     C                   CALL      'TRUL06R'
094700971126     C                   PARM                    KPJBA
094800971126     C                   MOVEL     KPJBU         DSUL06
094900971126     C                   MOVEA     LIN           L6
095000971126     C*
095100971126     C* VEDO SE IL P.O. E' UN TERMINAL DI PARTENZA E SE HA DEI P.O.
095200050906     C* ESTERI O D.P.D. o la 101 resi --> LI CARICO
095300971126     C                   CLEAR                   DSLV55
095400971126     C                   MOVEL     'P'           D55TPT
095500971126     C                   MOVEL     D34FGS        D55LIN
095600980403     C                   MOVEL     AM8GIO        D55DRF
095700971126     C                   CALL      'FNLV55R'
095800971126     C                   PARM                    DSLV55
095900971126     C     D55ERR        IFNE      ' '
096000971126     C                   MOVEL     D55LIN        D55TFP
096100971126     C                   ENDIF
096200971126     C                   MOVEL     D55TFP        FGSTFP
096300971126     C*§
096400971126    1C     D34FGS        IFEQ      D55TFP
096500971126     C                   Z-ADD     1             L
096600971126     C                   CLEAR                   W0030
096700971126     C     W0030         LOOKUP    L6(L)                                  30
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
098600971126     C* VEDO SE HA COME TERMINAL DI PARTENZA LA FIL.GESTIONE
098700971126     C                   CLEAR                   DSLV55
098800971126     C                   MOVEL     'P'           D55TPT
098900971126     C                   MOVEL     ORGFIL        D55LIN
099000980403     C                   MOVEL     AM8GIO        D55DRF
099100971126     C                   CALL      'FNLV55R'
099200971126     C                   PARM                    DSLV55
099300991217    6C     D55ERR        IFNE      ' '
099400971126     C                   MOVEL     D55LIN        D55TFP
099500991217    6C                   ENDIF
099600991217    6C     D55TFP        IFEQ      D34FGS
099700971126     C                   Z-ADD     ORGFIL        L6(L)
099800971126     C                   ADD       1             L
099900991217    6C                   ENDIF
100000991217    5C                   ENDIF
100100971126    4C                   ENDIF
100200971126    3C                   ENDIF
100300971126     C*
100400971126     C                   READ      AZORG01L                               30
100500971126    2C                   ENDDO
100600971126     C*
100700971126    1C                   ENDIF
100800040614   x0c                   else
100900040617     c                   z-add     1             xx
101000040617     c                   z-add     1             yy                3 0
101100040617     c                   do        30            xx
101200040617     c                   if        spo(xx)>*zeros
101300040617     c* imposto schiera di elaborazione
101400040617     c                   movel     spo(xx)       l6(yy)
101500040617     c* imposto schiera di stampa alfanumerica
101600040617     c                   movel     spo(xx)       stpo(yy)
101700040617     c                   add       1             yy
101800040615     c                   seton                                        80
101900040617     c                   endif
102000040615     c                   enddo
102100040617     c
102200040614     c                   endif
102300960927     C*
102400911018     C                   ENDSR
102500960412     C*
102600960412     C* LEGGO LE ANOMALIE IN PARTENZA --------------------------------*
102700960412     C     PARTEN        BEGSR
102800960412     C                   MOVEL     'N'           WFLE              1
102900960412     C*
103000960412     C* IMPOSTO LE ANOMALIE CHE DEVO LEGGERE
103100960412     C*
103200960412    2C     D34IE         IFEQ      ' '
103300960412     C                   CLEAR                   EL
103400040116     C**                 MOVEL     'R'           EL(1)
103500040116     C                   MOVEL     'E'           EL(1)
103600960412     C                   MOVEL     'I'           EL(2)
103700960412    2C                   ENDIF
103800960412     C**
103900960412     C                   Z-ADD     1             L                 3 0
104000960412    2C     L6(L)         DOWGT     0
104100960412     C     WUSCIT        ANDEQ     ' '
104200960412     C**
104300960412     C                   MOVEL     L6(L)         KFIL
104400960412     C**
104500960412     C                   Z-ADD     1             E                 3 0
104600960412    3C     EL(E)         DOWNE     ' '
104700960412     C     WUSCIT        ANDEQ     ' '
104800960412     C                   MOVEL     EL(E)         KAIE
104900970217     C                   Z-SUB     1             KLNA
105000960412     C                   SETOFF                                       30
105100960412     C**
105200960412    4C     *IN30         DOWEQ     *OFF
105300960412     C     WUSCIT        ANDEQ     ' '
105400990512     C     KANM7         SETGT     FNANM07L
105500990512     C     KANM71        READE     FNANM07L                               30
105600960412     C   30              MOVEL     999           KLNA7
105700960412     C  N30              MOVEL     ANMLNA        KLNA7
105800960412     C*
105900960412     C                   CLEAR                   KLNP
106000990512     C     KANM8         SETGT     FNANM08L
106100990512     C     KANM9         READE     FNANM08L                               30
106200960412     C   30              MOVEL     999           KLNA8
106300960412     C  N30              MOVEL     ANMLNA        KLNA8
106400960412     C*
106500960412    5C     KLNA7         IFNE      999
106600960412     C     KLNA8         ORNE      999
106700960412     C                   SETOFF                                       30
106800960412     C** TROVATA LA LNA
106900960412    6C     KLNA7         IFLT      KLNA8
107000960412     C                   MOVEL     KLNA7         KLNA
107100960412     C                   ELSE
107200960412     C                   MOVEL     KLNA8         KLNA
107300960412    6C                   ENDIF
107400960927     C*
107500960927     C* SE STO STAMPANDO LE RICEVUTE, VEDO SE STAMPARE LE LOCALI
107600040116     C********           EXSR      LOCALI
107700960412     C**
107800040116     C**** WUSCIT        IFEQ      ' '
107900960927     C                   EXSR      LEGPAR
108000040116     C****               ENDIF
108100960412     C**
108200960927   X5C                   ELSE
108300960412     C                   SETON                                        30
108400990518     C                   Z-ADD     999           KLNA
108500990518     C*
108600990518     C* SE STO STAMPANDO LE RICEVUTE, VEDO SE STAMPARE LE LOCALI
108700040116     C*****              EXSR      LOCALI
108800960412    5C                   ENDIF
108900960412    4C                   ENDDO
109000960412     C*
109100960412     C                   ADD       1             E
109200960412    3C                   ENDDO
109300960412     C*
109400960412     C                   ADD       1             L
109500960412    2C                   ENDDO
109600960412     C                   ENDSR
109700960927     C*
109800960927     C* LETTURA ANOMALIE PER LA PARTENZA------------------------------*
109900960927     C     LEGPAR        BEGSR
110000960927    6C                   SELECT
110100960927     C* SOLO ANOMALIE APERTE
110200961218     C*  SOLTANTO SE NON STO LEGGENDO LE LOCALI PERCHE' ALTRIMENTI
110300961218     C*  NON RIUSCIREI A LEGGERE LE ANOMALIE 70 E 71 CHE SONO
110400961218     C*  CHIUSE VIRUTALMENTE MA NON DI FATTO (MANCA SOLO AL CASUALE CH)
110500040116     C**   D34APR        WHENEQ    'S'
110600040116     C**   *IN07         ANDEQ     *OFF
110700040116     C**                 CLEAR                   KDCH
110800040116     C**   KANM72        SETLL     FNANM07L
110900040116     C**   KANM72        READE     FNANM07L                               31
111000960927     C* SOLO ANOMALIE CHIUSE
111100960927     C     D34APR        WHENEQ    'C'
111200960927     C                   Z-ADD     1             KDCH
111300990512     C     KANM72        SETLL     FNANM07L
111400990512     C     KANM7         READE     FNANM07L                               31
111500960927   X6C                   OTHER
111600990512     C     KANM7         SETLL     FNANM07L
111700990512     C     KANM7         READE     FNANM07L                               31
111800960927    6C                   ENDSL
111900960927     C**
112000960927     C                   SETON                                        27
112100960927     C                   SETOFF                                       252829
112200990114     C                   SETOFF                                       2624
112300960927    6C     *IN31         DOWEQ     *OFF
112400960927     C**
112500960927     C                   EXSR      ELASTA                                        STAMPA
112600960927     C** RILEGGO DA CAPO
112700960927    7C     WUSCIT        IFEQ      ' '
112800960927     C*
112900040116    8C**   D34APR        IFEQ      'S'
113000040116     C**   *IN07         ANDEQ     *OFF
113100040116     C**   KANM72        READE     FNANM07L                               31     ANOM APERTE
113200040116   X8C**                 ELSE
113300990512     C     KANM7         READE     FNANM07L                               31     TUTTE O CHI
113400040116    8C**                 ENDIF
113500960927    7C                   ENDIF
113600960927    6C                   ENDDO
113700960927     C*
113800960927     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
113900960927   6AC     WUSCIT        IFEQ      ' '
114000960927     C                   CLEAR                   KLNP
114100960927    6C                   SELECT
114200960927     C* SOLO ANOMALIE APERTE
114300040116     C**   D34APR        WHENEQ    'S'
114400040116     C**   *IN07         ANDEQ     *OFF
114500040116     C**                 CLEAR                   KDCH
114600040116     C**   KANM82        SETLL     FNANM08L
114700040116     C**   KANM82        READE     FNANM08L                               31
114800960927     C* SOLO ANOMALIE CHIUSE
114900960927     C     D34APR        WHENEQ    'C'
115000960927     C                   Z-ADD     1             KDCH
115100990512     C     KANM82        SETLL     FNANM08L
115200990512     C     KANM8         READE     FNANM08L                               31
115300960927   X6C                   OTHER
115400990512     C     KANM8         SETLL     FNANM08L
115500990512     C     KANM8         READE     FNANM08L                               31
115600960927    6C                   ENDSL
115700960927     C**
115800960927     C                   SETON                                        28
115900960927     C                   SETOFF                                       252729
116000990114     C                   SETOFF                                       2624
116100960927    6C     *IN31         DOWEQ     *OFF
116200960927     C**
116300960927     C                   EXSR      ELASTA                                        STAMPA
116400960927     C**
116500960927    7C     WUSCIT        IFEQ      ' '
116600960927     C*
116700040116    8C**   D34APR        IFEQ      'S'
116800040116     C**   *IN07         ANDEQ     *OFF
116900040116     C**   KANM82        READE     FNANM08L                               31     ANOM APERTE
117000040116   X8C**                 ELSE
117100990512     C     KANM8         READE     FNANM08L                               31     TUTTE O CHI
117200040116    8C**                 ENDIF
117300960927    7C                   ENDIF
117400960927     C*
117500960927    6C                   ENDDO
117600960927   6AC                   ENDIF
117700960927     C                   ENDSR
117800960412     C*
117900960412     C* LEGGO LE ANOMALIE IN ARRIVO ----------------------------------*
118000960412     C     ARRIVO        BEGSR
118100960412     C                   MOVEL     'N'           WFLE              1
118200960412     C*
118300960412     C* IMPOSTO LE ANOMALIE CHE DEVO LEGGERE
118400960412    2C     D34IE         IFEQ      ' '
118500960412     C                   CLEAR                   EL
118600960412     C                   MOVEL     'E'           EL(1)
118700960412     C                   MOVEL     'I'           EL(2)
118800960412    2C                   ENDIF
118900960412     C**
119000960412     C                   Z-ADD     1             L                 3 0
119100960412    2C     L6(L)         DOWGT     0
119200960412     C     WUSCIT        ANDEQ     ' '
119300960412     C**
119400960412     C                   MOVEL     L6(L)         KFIL
119500960412     C**
119600960412     C                   Z-ADD     1             E                 3 0
119700960412    3C     EL(E)         DOWNE     ' '
119800960412     C     WUSCIT        ANDEQ     ' '
119900960412     C                   MOVEL     EL(E)         KAIE
120000960412     C                   CLEAR                   KLNP
120100960412     C                   SETOFF                                       30
120200960412     C**
120300960412    4C     *IN30         DOWEQ     *OFF
120400960412     C     WUSCIT        ANDEQ     ' '
120500960412     C                   CLEAR                   KFLS
120600990512     C     KANM9         SETGT     FNANM09L
120700990512     C     KANM71        READE     FNANM09L                               30
120800960412     C   30              MOVEL     999           KLNP9
120900960412     C  N30              MOVEL     ANMLNP        KLNP9
121000960412     C*
121100960412     C                   MOVEL     KLNP          KFLS
121200960412     C                   CLEAR                   KLNP
121300990512     C     KANM94        SETGT     FNANM09L
121400990512     C     KANM9         READE     FNANM09L                               30
121500960412     C   30              MOVEL     999           KLNP92
121600960412     C  N30              MOVEL     ANMFLS        KLNP92
121700960412     C                   CLEAR                   KFLS
121800960412     C*
121900960412    5C     KLNP9         IFNE      999
122000960412     C     KLNP92        ORNE      999
122100960412     C                   SETOFF                                       30
122200960412     C** TROVATA LA LNP
122300960412     C     KLNP9         IFLT      KLNP92
122400960412     C                   MOVEL     KLNP9         KLNP
122500960412     C                   ELSE
122600960412     C                   MOVEL     KLNP92        KLNP
122700960412     C                   ENDIF
122800960412     C**
122900990512     C     KANM9         SETLL     FNANM09L
123000990512     C     KANM9         READE     FNANM09L                               31
123100960412     C**
123200960412     C                   SETON                                        29
123300960412     C                   SETOFF                                       252728
123400990114     C                   SETOFF                                       2624
123500960412    6C     *IN31         DOWEQ     *OFF
123600960412     C**
123700960412     C                   EXSR      ELASTA                                        STAMPA
123800960412     C**
123900960412     C     WUSCIT        IFEQ      ' '
124000990512     C     KANM9         READE     FNANM09L                               31     TUTTE O CHI
124100960412     C                   ENDIF
124200960412    6C                   ENDDO
124300960412     C*
124400960412     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
124500960412   6AC     WUSCIT        IFEQ      ' '
124600960412     C                   MOVEL     KLNP          KFLS
124700960412     C                   CLEAR                   KLNP
124800960412    6C                   SELECT
124900960419     C* SOLO ANOMALIE APERTE (SE NO CONFERMA IDD)
125000960412     C     D34APR        WHENEQ    'S'
125100960419     C     D34ELA        ANDNE     'S'
125200960412     C                   CLEAR                   KDCH
125300990512     C     KANM93        SETLL     FNANM09L
125400990512     C     KANM93        READE     FNANM09L                               31
125500960419     C* SOLO ANOMALIE CHIUSE  (SE NO CONFERMA IDD)
125600960412     C     D34APR        WHENEQ    'C'
125700960419     C     D34ELA        ANDNE     'S'
125800960412     C                   Z-ADD     1             KDCH
125900990512     C     KANM93        SETLL     FNANM09L
126000990512     C     KANM94        READE     FNANM09L                               31
126100960412   X6C                   OTHER
126200990512     C     KANM94        SETLL     FNANM09L
126300990512     C     KANM94        READE     FNANM09L                               31
126400960412    6C                   ENDSL
126500960412     C**
126600960412    6C     *IN31         DOWEQ     *OFF
126700960412     C**
126800960412     C                   EXSR      ELASTA                                        STAMPA
126900960412     C**
127000960412    7C     WUSCIT        IFEQ      ' '
127100960412     C*
127200960412    8C     D34APR        IFEQ      'S'
127300960423     C     D34ELA        ANDNE     'S'
127400990512     C     KANM93        READE     FNANM09L                               31     ANOM APERTE
127500960412   X8C                   ELSE
127600990512     C     KANM94        READE     FNANM09L                               31     TUTTE O CHI
127700960412    8C                   ENDIF
127800960412    7C                   ENDIF
127900960412     C*
128000960412    6C                   ENDDO
128100960412   6AC                   ENDIF
128200960412     C**
128300960412     C* REIMPOSTO IL CAMPO DELLA LINEA DI PARTENZA
128400960412     C                   MOVEL     KFLS          KLNP
128500960412     C                   CLEAR                   KFLS
128600960412     C                   ELSE
128700960412     C                   SETON                                        30
128800960412    5C                   ENDIF
128900960412    4C                   ENDDO
129000960412     C*
129100960412     C                   ADD       1             E
129200960412    3C                   ENDDO
129300960412     C*
129400960412     C                   ADD       1             L
129500960412    2C                   ENDDO
129600960412     C                   ENDSR
129700960412     C*
129800960412     C* LEGGO LE ANOMALIE CON ANMFLE ---------------------------------*
129900960412     C     USAFLE        BEGSR
130000960412     C                   MOVEL     'S'           WFLE              1
130100960412     C                   Z-ADD     1             Y
130200960412     C**
130300960412    1C     FLE(Y)        DOWGT     0
130400960412     C     WUSCIT        ANDEQ     ' '
130500960412     C                   MOVEL     FLE(Y)        KCAA
130600960412     C                   CLEAR                   KFIL
130700960412     C                   SETOFF                                       30
130800960523     C*
130900960508     C* VERIFICO SE L'ANOMALIA E' SECONDO I PARAMETRI RICHIESTI
131000960508     C                   Z-ADD     KCAA          ANMCAA
131100960508     C                   EXSR      LEGCAA
131200960523     C*
131300960508     C* RICHIESTE LE ANOMALIE INTERNE
131400960508     C     D34IE         IFEQ      'I'
131500960508     C     §7CAIE        ANDNE     'I'
131600960508     C                   SETON                                        30
131700960508     C                   ENDIF
131800960523     C**
131900960523     C* RICHIESTE LE ANOMALIE NON IDD
132000960523     C     D34IE         IFEQ      'N'
132100960523     C     §7CIDD        ANDEQ     'S'
132200060428     C     d34ie         oreq      'N'
132300060428     C     §7CAIE        ANDNE     'E'
132400960523     C                   SETON                                        30
132500960523     C                   ENDIF
132600960508     C**
132700960508     C* RICHIESTE LE ANOMALIE IDD
132800960523    1C     D34IE         IFEQ      'D'
132900960508     C     §7CIDD        ANDNE     'S'
133000960523     C*  SE E' LA CONFERMA DELL'IDD LE DEVO ELABORARE COMUNQUE
133100960523    2C     D34ELA        IFNE      'S'
133200960508     C                   SETON                                        30
133300960523   X2C                   ELSE
133400960523    3C     §7CAIE        IFNE      'E'                                          ESTERNA NO I
133500960523     C                   SETON                                        30
133600960523    3C                   ENDIF
133700960523    2C                   ENDIF
133800960523    1C                   ENDIF
133900960412     C**
134000960417    2C     *IN30         DOWEQ     *OFF
134100990512     C     KANM5         SETGT     FNANM10L
134200990512     C     KCAA          READE     FNANM10L                               30
134300960412    3C     *IN30         IFEQ      *OFF
134400960412     C                   MOVEL     ANMLNA        KFIL
134500960412     C                   CLEAR                   KLNP
134600960412     C                   CLEAR                   KFLS
134700960412     C                   SETOFF                                       31
134800960412     C*
134900960412    4C     *IN31         DOWEQ     *OFF
135000960412     C     WUSCIT        ANDEQ     ' '
135100990512     C     KANM51        SETGT     FNANM10L
135200990512     C     KANM5         READE     FNANM10L                               31
135300960412     C  N31              MOVEL     ANMLNP        KLNP9
135400960412     C   31              MOVEL     999           KLNP9
135500960412     C*
135600960412     C                   MOVEL     KLNP          KFLS
135700960412     C                   CLEAR                   KLNP
135800990512     C     KANM52        SETGT     FNANM10L
135900990512     C     KANM51        READE     FNANM10L                               31
136000960412     C  N31              MOVEL     ANMFLS        KLNP92
136100960412     C   31              MOVEL     999           KLNP92
136200960412     C*
136300960412    5C     KLNP9         IFNE      999
136400960416     C     KLNP92        ORNE      999
136500960412     C** TROVATA LA LNP
136600960412    6C     KLNP9         IFLT      KLNP92
136700960412     C                   MOVEL     KLNP9         KLNP
136800960412     C                   ELSE
136900960412     C                   MOVEL     KLNP92        KLNP
137000960412    6C                   ENDIF
137100960412     C**
137200990512     C     KANM51        SETLL     FNANM10L
137300990512     C     KANM51        READE     FNANM10L                               31
137400960412     C**
137500960412     C                   SETOFF                                       272829
137600970529     C                   SETOFF                                       26
137700960412    6C     *IN31         DOWEQ     *OFF
137800960412     C**
137900960412     C                   EXSR      ELASTA                                        STAMPA
138000960412     C**
138100960412    7C     WUSCIT        IFEQ      ' '
138200990512     C     KANM51        READE     FNANM10L                               31     TUTTE O CHI
138300960412    7C                   ENDIF
138400960412    6C                   ENDDO
138500960412     C*
138600960412     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
138700960412    6C     WUSCIT        IFEQ      ' '
138800960412     C                   MOVEL     KLNP          KFLS
138900960412     C                   CLEAR                   KLNP
139000990512     C     KANM52        SETLL     FNANM10L
139100990512     C     KANM52        READE     FNANM10L                               31
139200960412     C**
139300960412    7C     *IN31         DOWEQ     *OFF
139400960412     C**
139500960412     C                   EXSR      ELASTA                                        STAMPA
139600960412     C**
139700960412    8C     WUSCIT        IFEQ      ' '
139800960412     C*
139900990512     C     KANM52        READE     FNANM10L                               31     TUTTE O CHI
140000960412    8C                   ENDIF
140100960412     C*
140200960412    7C                   ENDDO
140300960412    6C                   ENDIF
140400960412     C**
140500960412     C* REIMPOSTO IL CAMPO DELLA LINEA DI PARTENZA
140600960412     C                   MOVEL     KFLS          KLNP
140700960412     C                   CLEAR                   KFLS
140800960412     C                   SETOFF                                       31
140900960412   X5C                   ELSE
141000960412     C                   SETON                                        31
141100960412    5C                   ENDIF
141200960412    4C                   ENDDO
141300960412    3C                   ENDIF
141400960412    2C                   ENDDO
141500960412     C*
141600960412     C                   ADD       1             Y
141700960412    1C                   ENDDO
141800960412     C*
141900960412     C                   ENDSR
142000970529     C*
142100040614     C* LEGGO LE ANOMALIE CON ANMCDU solo in partenza ----------------*
142200970529     C     USACDU        BEGSR
142300970529     C                   MOVEL     'N'           WFLE              1
142400040615     C                   MOVEL     'S'           Wcdu              1
142500970529     C                   SETOFF                                       252728
142600990114     C                   SETOFF                                       2924
142700970529     C                   SETON                                        26
142800970529     C* LEGGO TUTTE LE ANOMALIE RICEVUTE CON CDU=SIMFEL SCARTANDO
142900040616     C* QUELLE CON LNP O FLS PRESENTI IN £1(lo scato avviene nella
143000040616     c*        exsr CONFR)
143100970529     C****
143200970529     C* 1°CICLO: LEGGO CON LNP > 0 E <> £1
143300970529     C****
143400040116     C                   MOVEL     'E'           KAIE
143500970529     C                   Z-ADD     SIMFEL        KCDU
143600970529     C                   Z-ADD     1             ANMLNP
143700040616     c
143800970529    0C     WUSCIT        IFEQ      *BLANKS
143900990512     C     KANM3         SETLL     FNANM03L
144000990512     C     KANM3A        READE     FNANM03L                               30
144100970529    1C     *IN30         DOWEQ     *OFF
144200040615     c                   eval      wsetgt='N'
144300040615     c
144400040429     C                   EXSR      ELASTA
144500040615     c*
144600040615     c                   if        wsetgt='S'
144700040615     C     KANM3         SETGT     FNANM03L
144800040429     c                   endif
144900040615     c
145000970529    2C     WUSCIT        IFEQ      *BLANKS
145100990512     C     KANM3A        READE     FNANM03L                               30
145200970529    2C                   END
145300970529    1C                   ENDDO
145400970529     C****
145500970529     C* 2°CICLO: LEGGO CON LNP = 0
145600970529     C*                 ED FLS > 0 E <> £1
145700970529     C****
145800970529   1AC     WUSCIT        IFEQ      *BLANKS
145900970529     C                   MOVE      *ZEROS        KLNP
146000970529     C                   Z-ADD     1             ANMFLS
146100990512     C     KANM3B        SETLL     FNANM03L
146200990512     C     KANM3D        READE     FNANM03L                               30
146300970529    1C     *IN30         DOWEQ     *OFF
146400040616     c                   eval      wsetgt='N'
146500040616     c
146600040429     C                   EXSR      ELASTA
146700040616     c
146800040616     C* Fls in £1 --> Scarto e mi posiziono su nuova Fls
146900040616    2c                   if        wsetgt='S'
147000990512     C     KANM3B        SETGT     FNANM03L
147100970529    2C                   END
147200040616    c
147300970529    2C     WUSCIT        IFEQ      *BLANKS
147400990512     C     KANM3D        READE     FNANM03L                               30
147500040616    2C                   END
147600970529    1C                   ENDDO
147700970529   1AC                   END
147800970529    0C                   END
147900040615     C                   MOVEL     'N'           Wcdu              1
148000970529     C                   ENDSR
148100960327     C*
148200960523     C* ELABORO I DATI DELLE ANOMALIE PER LA STAMPA -----------------*
148300960327     C     ELASTA        BEGSR
148400960327     C*
148500960327     C* CONFRONTO SELEZIONI FATTE
148600960327     C                   EXSR      CONFR
148700960327     C**
148800960327    1C     *IN10         IFEQ      *ON
148900960327     C**
149000960327     C* SE ELABORAZIONE DEFINITIVA IDD, INVIO ANCHE LE ANOMALIE
149100960705     C*  CHE CI SONO DA TRASMETTERE
149200960327    2C     *IN01         IFEQ      *ON
149300960419     C     *IN12         ANDEQ     *ON
149400960327    3C     ANMFT1        IFEQ      ' '
149500960412     C     ANMFL1        ANDGT     0
149600960327     C     ANMFT2        OREQ      ' '
149700960412     C     ANMFL2        ANDGT     0
149800960705     C                   CLEAR                   WTRM
149900960827     C* SERIE > 0
150000960827    4C     ANMNRS        IFGT      *ZEROS
150100960827     C**SE ANOMALIA 55 ED ESISTE BOLLA PARTENZA NON LA TRASMETTO E
150200960827     C* LA CHIUDO. CHIUDO ANCHE LA 200
150300960827     C* (SE BOLLA DA ME CONFERMABILE E CONFERMABILE ANCHE DA ALTRI AS)
150400960827     C**STESSA COSA SE ANOMALIA 50 (SENZA CHIUDERE LA 200)
150500960827     C     ANMCAA        IFEQ      50
150600960827     C     ANMCAA        OREQ      55
150700990409     C     ANMCAA        OREQ      56
150800960827     C                   EXSR      A50A55
150900960827     C                   ENDIF
151000960827    4C                   ENDIF
151100960827     C*
151200960705    4C     WTRM          IFEQ      ' '
151300990114     C   24
151400990512     COR 25ANMN10        CHAIN     FNANM000                           33
151500000306     C   26ANMNR3        CHAIN     FNANM000                           33
151600000306     C   27ANMNR7        CHAIN     FNANM000                           33
151700990512     C   28ANMNR8        CHAIN     FNANM000                           33
151800990512     C   29ANMNR9        CHAIN     FNANM000                           33
151900960404     C*
152000960327     C                   EXSR      ELAB
152100960705    4C                   ENDIF
152200960327    3C                   ENDIF
152300960327    2C                   ENDIF
152400960327     C**
152500960327   X1C                   ELSE
152600960327     C* 10 OFF - RECORD SELEZIONABILE
152700960327     C                   CLEAR                   WUSCIT
152800980327     C*** TRASFERIMENTO DA FILIALE A FILIALE
152900980327     C**
153000040615     C**   D34TRA        IFEQ      'S'
153100040615     C**                 EXSR      TRASFE
153200040615     C**                 END
153300960327     C**
153400960327     C** G E S T I O N E
153500000317    2C     D34GES        IFEQ      'I'
153600000317     C     D34GES        OREQ      'G'
153700960327     C* RICHIAMO IL PGM FNLR44R PASSANDOGLI IL RECORD DELL'ANOMALIA
153800960327     C*  IL NUMERO RELATIVO DI RECORD
153900960327     C*  LA DS DEI PARAMETRI RICHIESTI
154000960515     C* LO RICHIAMO SOLO SE IL SOGGETTO ANOMALIA E' <> DA "E"
154100040116    3C**   ANMSAN        IFNE      'E'
154200040116     C**   ANMAIE        ANDEQ     'E'
154300040116     C**   ANMAIE        ORNE      'E'
154400040116    c
154500040116    3C     ANMSAN        IFNE      'E'
154600040116     C     wanom         ANDEQ     'A'
154700040116     C     ANMAIE        ORNE      'E'
154800040116     C     wanom         orne      'A'
154900960327     C                   CLEAR                   DSLR44
155000960515    4C     *IN27         IFEQ      *ON
155100960401     C     *IN28         OREQ      *ON
155200970529     C     *IN26         OREQ      *ON
155300990114     C     *IN24         OREQ      *ON
155400990114     C   24              Z-ADD     ANMN10        D44NRR
155500970529     C   26              Z-ADD     ANMNR3        D44NRR
155600960327     C   27              Z-ADD     ANMNR7        D44NRR
155700960327     C   28              Z-ADD     ANMNR8        D44NRR
155800960401     C                   MOVEL     'P'           D34APA
155900960515   X4C                   ELSE
156000960327     C   29              Z-ADD     ANMNR9        D44NRR
156100960417     C   25              Z-ADD     ANMN10        D44NRR
156200960401     C                   MOVEL     'A'           D34APA
156300960515    4C                   ENDIF
156400960327     C                   CLEAR                   D34F03
156500960327     C                   CLEAR                   D34F12
156600960327     C                   CLEAR                   D34RIL
156700960327     C                   MOVEL     DSLR34        D44LR
156800960327     C                   MOVEL     DSANM         D44ANM
156900960521     C                   MOVEL     'G'           D44MOD
157000960327     C*
157100960327     C                   MOVEL     DSLR44        KPJBU
157200960327     C                   CALL      'FNLR44R'
157300960327     C                   PARM                    KPJBA
157400960327     C                   MOVEL     KPJBU         DSLR44
157500960327     C                   MOVEL     D44LR         DSLR34
157600960327     C* SE PREMUTO F3 F12 O RILETTURA
157700960515    4C     D34F03        IFEQ      'S'
157800960327     C     D34F12        OREQ      'S'
157900960327     C     D34RIL        OREQ      'S'
158000960327     C                   MOVEL     'S'           WUSCIT            1
158100960327     C                   SETON                                        3031
158200960515    4C                   ENDIF
158300960515    3C                   ENDIF
158400960327     C**
158500960327   X2C                   ELSE
158600960327     C** S T A M P A
158700960327     C*
158800000317    3C                   SELECT
158900000317     C* ELABORAZIONE IDD
159000000317     C     D34ELA        WHENNE    ' '
159100960405     C  N11              EXSR      STAMPA
159200960327     C*
159300960327     C* ELABORAZIONE DEFINITIVA AGGIORNO E SPEDISCO
159400960405     C   01              EXSR      ELAB
159500960327     C**
159600000317     C     D34GES        WHENEQ    'S'
159700960327     C* NON E' STAMPA IDD --> STAMPO TUTTO
159800960327     C                   EXSR      STAMPA
159900000317     C*
160000000317    3C                   ENDSL
160100960327    2C                   ENDIF
160200960327    1C                   ENDIF
160300960327     C*
160400960327     C                   ENDSR
160500921015     C*
160600921015     C* CONFRONTO RICHIESTA FATTA A VIDEO ---------------------------*
160700921015     C     CONFR         BEGSR
160800960508     C                   SETOFF                                         2097
160900960419     C                   SETOFF                                       101112
161000960509     C                   CLEAR                   PRTZNC
161100960927     C                   CLEAR                   PRTDET
161200960927     C                   CLEAR                   WDDET
161300960927     C                   CLEAR                   WDFLP
161400960927     C                   CLEAR                   PSTAM
161500980327     C* PULISCO IL CAMPO DI CANCELLAZIONE RECORD IN CASO DI TRASFERIMENTO
161600980327     C* A FILIALE
161700040615     C**                 CLEAR                   NODEL
161800040615     C**                 CLEAR                   WRTF
161900030717     C**
162000030717     C* SE RICHIESTO ESEGUO LA SELEZIONE X P.O. SEGNACOLLO E NUMERO SERIE
162100030717     C                   IF        D34POS  > *ZEROS
162200030717     C                   IF        D34POS <> ANMFLS
162300030717     C                   SETON                                        10
162400030717     C                   GOTO      ENDCON
162500030717     C                   ENDIF
162600040614     c
162700030717     C                   IF        D34NRS <> 99
162800030717     C                   IF        D34NRS <> ANMNRS
162900030717     C                   SETON                                        10
163000030717     C                   GOTO      ENDCON
163100030717     C                   ENDIF
163200030717     C                   ENDIF
163300040614     C                   ENDIF
163400040609     c
163500040609     c* se richiesto, faccio vedere solo anomalie di colli di valore
163600040609     c                   if        D34bva='V'  and
163700040609     c                             anmft4<>'V'
163800040609     C                   SETON                                        10
163900040609     C                   GOTO      ENDCON
164000040609     C                   ENDIF
164100000317     C**
164200980402     C* CONTROLLO CHE LA LINEA DI ARRIVO SIA UGUALE A QUELLA RICHIESTA A VIDEO
164300980403     C* SE NON SONO IN UNA FASE DI TRASFERIMENTO
164400980403     C     D34TRA        IFNE      'S'
164500980402     C     D34LNA        IFNE      *ZEROS
164600980402     C     D34LNA        ANDNE     ANMLNA
164700980402     C                   SETON                                        10
164800980402     C                   GOTO      ENDCON
164900980402     C                   ENDIF
165000980403     C                   ENDIF
165100960424     C*
165200090312     C* SE STO LEGGENDO UNA ANOMALIA in partenza, NON LA STAMPO
165300960424     C*  SE IL SEGNACOLLO NEL FRATTEMPO E' STATO CONSEGNATO
165400960524     C*  (NON COME IDD3 DANNI CODICE 5)
165500970127     C*  (NON SE DATA APERTURA ANOMALIA > DATA CONSEGNA)
165700040116     C     ANMSCN        ifgt      0
165800040223     C     wanom         andeq     'P'
165900960424     C*
166000090312     C     KART          CHAIN     Fiart27L                           32
166100960424    2C     *IN32         IFEQ      *OFF
166200960424     C*
166300090312    3C     artDCM        IFGT      0
166400090312     C     artDET        ORGT      0
166500960524     C**
166600960524     C     KBLP          CHAIN     FNBLP01L                           32
166601090312     c                   if        *in32
166602090312     c                   eval      blpcca='5'
166603090312     c                   endif
166700960524     C**
166800090312    4C     artDCM        IFGT      0
166900090312     C     artCCA        ANDNE     '5'
166901090312     C     BLPCCA        ANDNE     '5'
167000090312     C     ANMDAO        ANDLE     artDCM
167100960424     C* SE HO CHIESTO SOLO LE APERTE ESCO
167200960524    5C     D34APR        IFEQ      'S'
167300960424     C                   SETON                                        10
167400960424     C                   GOTO      ENDCON
167500960524   X5C                   ELSE
167600960424     C* ALTRIMENTI DICO CHE E' CHIUSA COME CONSEGNATA
167700960524    6C     ANMDCH        IFEQ      0
167800090312     C                   MOVE      arTDCM        ANMDCH
167900960524    6C                   ENDIF
168000960524     C                   MOVEL     'CO'          ANMCCH
168100960524    5C                   ENDIF
168200960424    4C                   ENDIF
168300960424    3C                   ENDIF
168400960424     C*
168500960424     C* SE ANOMALIA 10 NON STAMPO SE LA SPEDIZIONE C'E' ED E' TRASMESSA
168600960424    3C     ANMCAA        IFEQ      10
168700960424     C     KBLP          CHAIN     FNBLP01L                           32
168800960509     C  N32              MOVEL     BLPZNC        PRTZNC            2
168900960424    4C  N32BLPFT1        IFNE      ' '
169000960424     C* SE HO CHIESTO SOLO LE APERTE ESCO
169100960424    5C     D34APR        IFEQ      'S'
169200960424     C                   SETON                                        10
169300960424     C                   GOTO      ENDCON
169400960424   X5C                   ELSE
169500960424    6C     ANMDCH        IFEQ      0
169600960424     C                   Z-ADD     BLPMGS        ANMDCH
169700990512     C                   MOVEL     BLPAAS        ANMDCH
169800960424    6C                   ENDIF
169900960424    5C                   ENDIF
170000960424    4C                   ENDIF
170100960424    3C                   ENDIF
170200960508     C**
170300960508     C** SE ANOMALIA 5 STAMPO SE E' ANDATA IN DISGUIDO
170400960508    3C     ANMCAA        IFEQ      005
170500040617     c
170600970604     C* PRIMA VERIFICO SE PER IL SEGNACOLLO ESISTE UN'ALTRA SPUNTA
170700970604     C* PARTENZA CON DATA > PERCHE' IN QUESTO CASO L'ANOMALIA
170800970604     C* NON E' DA STAMPARE
170900970604   3AC     ANMCDU        IFEQ      SIMFEL
171000970604     C* DETERMINO DATA DEL FOGLIO DELL'ANOMALIA
171100970604    4C     ANMNFV        IFNE      SAVNFV
171200970604     C                   MOVE      ANMNFV        SAVNFV
171300970604     C                   CLEAR                   DSLV53
171400970604     C                   MOVEL     'G'           D53TFO
171500970604     C                   Z-ADD     SIMFEL        D53FEL
171600970604     C                   Z-ADD     ANMNFV        D53NFV
171700020926     C                   Z-ADD     ANMcdu        D53FGS
171800970604     C                   CALL      'FNLV53R'
171900970604     C                   PARM                    DSLV53
172000970604     C                   MOVEL     '1'           WCL53             1
172100970604     C                   Z-ADD     D53DFV        W53DFV
172200970604    4C                   END
172300970603     C*
172400970603     C                   Z-ADD     1             KNPG
172500070111     C     KBRV3A        SETLL     FNBRV03L
172600070111     C     KBRV3A        READE     FNBRV03L                               32
172700970603    4C     *IN32         DOWEQ     *OFF
172800970604   4AC     BRVNFV        IFNE      ANMNFV
172900970603     C                   CLEAR                   DSLV53
173000970604     C                   MOVEL     'G'           D53TFO
173100970603     C                   Z-ADD     SIMFEL        D53FEL
173200970603     C                   Z-ADD     BRVNFV        D53NFV
173300020926     C                   Z-ADD     BRVfgs        D53FGS
173400970603     C                   CALL      'FNLV53R'
173500970603     C                   PARM                    DSLV53
173600970603     C                   MOVEL     '1'           WCL53             1
173700980424     C* QUESTO CONTROLLO LO IGNORO SE SONO IN TRASFERIMENTO
173800980424    5C     D34TRA        IFNE      'S'
173900980424     C     D53ERR        ANDEQ     *BLANKS
174000970604     C     D53DFV        ANDGT     W53DFV
174100970603     C                   SETON                                        10
174200970603     C                   GOTO      ENDCON
174300970603    5C                   END
174400970604   4AC                   END
174500070111     C     KBRV3A        READE     FNBRV03L                               32
174600970603    4C                   ENDDO
174700970604   3AC                   ENDIF
174800970603     C*
174900960927     C                   EXSR      DISPAR
175000960927    3C                   ENDIF
175100960927     C**
175200960424     C* MEMORIZZO IL NUMERO DI SPEDIZIONE SE C'E'
175300090312     C                   MOVE      arTAAS        ANMAAS
175400090312     C                   MOVE      arTLNP        ANMLNP
175500090312     C                   MOVE      arTNSP        ANMNSP
175600960424    2C                   ENDIF
175700960424    1C                   ENDIF
175800040617     C
175900040617     c* Per stampa colli di valore in arrivo, escludo anomalie se non
176000040617     c*  crerate dal p.o. gestione  SE STAMPA DEL TERMINAL
176100040617     c                   if        *in70 and wanom='A' AND ANMCAA=005 AND
176200040617     C                             *IN80
176300040617     c                   if        anmfle<>d34fgs
176400040617     C                   SETON                                        10
176500040617     C                   GOTO      ENDCON
176600040617     c                   endif
176700040617     c                   endif
176800960506     C* CODICE ANOMALIA
176900960508     C                   EXSR      LEGCAA
177000040615     c
177100040615     c* In USACDU escludo sempre le anomalie non IDD (in partenza sempre
177200040615     c*           lette per lnp)
177300040615     c                   if        wcdu='S' and §7caie='E' and §7cidd=' '
177400040615     C                   SETON                                        10
177500040615     C                   GOTO      ENDCON
177600040615    4C                   ENDIF
177700960419     C**
177800960506     C**
177900960419     C* 1) SE CONTROLLO NON PASSATO NON FACCIO NULLA
178000960419     C*
178100960416     C* SE DEVO USARE FLE, VERIFICO SE E' IN L6
178200961230    1C     WFLE          IFEQ      'S'
178300960416     C     ANMFLE        LOOKUP    L6                                     32
178400960416     C  N32              SETON                                            10
178500960417     C  N32              GOTO      ENDCON
178600961230     C*
178700970829    2C     WANOM         IFEQ      'A'
178800961017     C* VERIFICO ANCHE CHE NON SIA ANOMALIA CHE UTILIZZA
178900961017     C* SIA LNA CHE FLE CON LNA=FLE
179000970829    3C     §7CUEA        IFEQ      'E'
179100970829    4C     ANMLNA        IFEQ      ANMFLE
179200961017     C                   SETON                                        10
179300961017     C                   GOTO      ENDCON
179400970829    4C                   ENDIF
179500040413     C* SE E' IN L6  MA LO E' ANCHE LA LNA
179600961230     C*  ED E' UN TIPO ANOMALIA CHE VIENE STAMPATO  PER ENTRANBI
179700961230     C*  ESCLUDO PERCHE' L'HO GIA' STAMPATA
179800961230     C     ANMLNA        LOOKUP    L6                                     32
179900961230     C   32              SETON                                            10
180000961230     C   32              GOTO      ENDCON
180100970829    3C                   ENDIF
180200970829     C*
180300970829   X2C                   ELSE
180400970829     C* FLE UTILIZZATO IN PARTENZA
180500970829    3C     §7CUEP        IFEQ      'E'
180600970829     C* PRENDO O LNP O FLS
180700970829    4C     ANMLNP        IFGT      0
180800971125     C                   Z-ADD     ANMLNP        W0030             3 0
180900970829     C                   ELSE
181000970829     C                   Z-ADD     ANMFLS        W0030
181100970829    4C                   ENDIF
181200970829     C     W0030         LOOKUP    L1                                     32
181300970829     C   32              SETON                                            10
181400970829     C   32              GOTO      ENDCON
181500970829    3C                   ENDIF
181600970829     C**
181700970829    2C                   ENDIF
181800961230    1C                   ENDIF
181900980327     C* IN CASO DI TRASFERIMENTO DI FILIALE
182000040615    1C**   WFLE          IFNE      'S'
182100040615     C**   D34TRA        ANDEQ     'S'
182200040615    2C**   §7CUEA        IFEQ      'E'
182300980327     C* ARRIVO
182400040615     C**   WANOM         ANDEQ     'A'
182500040615     C**   ANMFLE        LOOKUP    L6                                     32
182600040615     C**N32ANMFLE        LOOKUP    L1                                     34
182700040615     C**N32
182800040615     C*N 34              MOVEL     'S'           NODEL             1
182900040615    2C**                 END
183000980327     C* PARTENZA
183100040615    2C**   §7CUEP        IFEQ      'E'
183200040615     C**   WANOM         ANDEQ     'P'
183300040615     C**   ANMFLE        LOOKUP    L6                                     32
183400040615     C**N32ANMFLE        LOOKUP    L1                                     34
183500040615     C**N32
183600040615     C*N 34              MOVEL     'S'           NODEL
183700040615    2C**                 END
183800980327     C*
183900040615    1C**                 END
184000960419     C**
184100960419     C* 2) SE CONTROLLO NON PASSATO NON FACCIO NULLA
184200960419     C**
184300960419     C* DATA APERTURA ANOMALIA DAL/AL
184400960419    1C     D34DLA        IFGT      0
184500960419    1C     D34ALA        ORGT      0
184600990512    2C     ANMDAO        IFLT      D34DLA
184700990512     C     ANMDAO        ORGT      D34ALA
184800960419     C                   SETON                                        10
184900960419     C                   GOTO      ENDCON
185000960419    2C                   ENDIF
185100960419    1C                   ENDIF
185200040615     c**
185300040615     c* 2a) Il controllo che faceva in USACDU lo faccio qui dopo aver
185400040615     c*     controllato la data
185500040615    1c                   if        wcdu='S'
185600040615     c
185700040615     c* cerco alla data anmdao se d34fgs è terminal di anmlnp
185800040615     c                   eval      kcaeepa='P'
185900040615    2c                   if        anmlnp>0
186000040615     c                   eval      kcaetfp=anmlnp
186100040615     c                   else
186200040615     c                   eval      kcaetfp=anmfls
186300040615    2c                   endif
186400040615     c                   eval      kcaetfe=d34fgs
186500040615     c                   eval      kcaedde=anmdao
186600040615     c     kcae          setll     azcae03l
186700040615     c     kcae2         readpe    azcae03l
186800040616     c
186900040616    2c                   dow       not %eof(azcae03l) and caeatb<>*blanks
187000040616     c     kcae2         readpe    azcae03l
187100040616    2c                   enddo
187200040616     c
187300040616    2c                   if        not %eof(azcae03l)
187400040615     c* se decorrente alla data anomalia --> p.o. della £1 di d34fgs escludo
187500040615    3c                   if        caedsc>=anmdao
187600040615     c                   seton                                        10
187700040615     c* Se Compreso tutto il periodo richiesto a video, escludo tutta la
187800040615     c*  lnp e quindi faccio SETGT in usacdu
187900040615    4c                   if        caedde<=d34dla  and
188000040615     c                             caedsc>=d34ala
188100040615     c                   eval      wsetgt='S'
188200040615    4c                   endif
188300040615     c
188400040615    3c                   endif
188500040615    2c                   endif
188600040615     c   10              goto      endcon
188700040615     c*
188800040615     c* Escludo anche dalla stampa anomalie con anmlna 2 livello di
188900040616     c*  d34fgs per non stampare anomalie di arrivo e  non di transito
189000040615    2c                   if        anmlna=d34fgs
189100040615     C                   SETON                                        10
189200040615     C                   GOTO      ENDCON
189300040616   x2c                   else
189400040616     c
189500040616     c                   clear                   wanomok           1
189600040616     c* Controllo il terminal di arrivo per sta anomalia
189700040616     c                   eval      kcaeepa='A'
189800040616    3c                   if        anmlnp>0
189900040616     c                   eval      kcaetfp=anmlnp
190000040616     c                   else
190100040616     c                   eval      kcaetfp=anmfls
190200040616    3c                   endif
190300040616     c                   eval      kcaetfa=anmlna
190400040616     c                   eval      kcaedde=anmdao
190500040616     c
190600040616     c                   EXSR      CercaCAE
190700040616     c
190800040616     c* Se non ho trovato eccezione terminal, cerco terminal normale
190900040616    3c                   if        wanomok=' '
191000040616     c                   clear                   kcaetfp
191100040616     c                   EXSR      CercaCAE
191200040616    3c                   endif
191300040616    c
191400040616     c* anomalia da non stampare
191500040616    3c                   if        wanomok='N'
191600040616     C                   SETON                                        10
191700040616     C                   GOTO      ENDCON
191800040616    3c                   endif
191900040616     c
192000040616    2c                   endif
192100040615    1c                   endif
192200960419     C**
192300960419     C* 3) SE CONTROLLO NON PASSATO NON FACCIO NULLA
192400960419     C**
192500960419     C* SE NON STO ELABORANDO PER FLE ESCLUDO LE ANOMALIE FLE
192600970829     C     WFLE          IFEQ      'N'
192700960419     C     ANMCAA        LOOKUP    FLE                                    10
192800960419     C   10              GOTO      ENDCON
192900960419     C                   ENDIF
193000960419     C**
193100960419     C* 4) SE E COMUNQUE ANOMALIE ESTERNA ANCHE SE NO IDD LA TRASMETTO
193200960419     C**
193300960327     C* SE HO RICHIESTO LE ESTERNE O LE RICEVUTE, VEDO SE
193400960327     C*  SONO QUELLE IDD O NO
193500960327     C     D34IE         IFNE      'I'
193600960327     C                   MOVEL     ANMCAA        W003A             3
193700960506     C     §7CIDD        IFEQ      'S'
193800960506     C                   SETON                                        97
193900960506     C                   ENDIF
194000960404     C* 97 ON - E' ANOMALIA IDD
194100960404    2C     *IN97         IFEQ      *ON
194200960506    3C     D34IE         IFEQ      'N'
194300960327     C                   SETON                                        10
194400960327     C                   GOTO      ENDCON
194500960327    3C                   ENDIF
194600960327     C*
194700960327   X2C                   ELSE
194800960404     C* 97 OFF  - NO IDD
194900960506    3C     D34IE         IFEQ      'D'
195000960327     C                   SETON                                        10
195100960419     C* 12 ON SE COMUNQUE ANOMALIA ESTERNA
195200040116     C   01aNMAIE        IFEQ      'E'
195300960419     C                   SETON                                        12
195400960419    3C                   ENDIF
195500960419     C*
195600960327     C                   GOTO      ENDCON
195700960327    3C                   ENDIF
195800960327    2C                   ENDIF
195900960327    1C                   ENDIF
196000960419     C**
196100960419     C* 5) SE ANOMALIA IDD CHIUSA TRASMETTO SENZA STAMPARE
196200960419     C**
196300960419     C* STAMPA SOLO ANOMALIE APERTE
196400961217     C* PER LE ANOMALIE LOCALI 70 E 71 FACCIO UN GIRO A PARTE
196500040116    0C**   *IN07         IFEQ      *OFF
196600040116     C**   *IN07         OREQ      *ON
196700040116     C**   ANMCAA        ANDNE     70
196800040116     C**   ANMCAA        ANDNE     71
196900040116     c
197000040116     c                   if        wanom='A'  or
197100040116     c                             wanom='P' and anmcaa<>70 and anmcaa<>71
197200961217     C*
197300960419    1C     D34APR        IFEQ      'S'
197400960419     C     ANMDCH        ANDNE     0
197500960419     C                   SETON                                        10
197600960419     C   01ANMAIE        IFEQ      'E'
197700960419     C                   SETON                                        12
197800960419     C                   ENDIF
197900960419     C                   GOTO      ENDCON
198000960419    1C                   ENDIF
198100960419     C* SOLO ANOMALIE CHIUSE
198200960419    1C     D34APR        IFEQ      'C'
198300960419     C     ANMDCH        ANDEQ     0
198400960419     C                   SETON                                        10
198500960419     C                   GOTO      ENDCON
198600960419    1C                   ENDIF
198700961217     C*
198800961217   X0C                   ELSE
198900961217     C* NON TESTO LA DATA DI CHIUSURA MA LA CASUALE
199000961217    1C     D34APR        IFEQ      'S'
199100961217    2C     ANMCCH        IFNE      '  '
199200961217     C                   SETON                                        10
199300961217   X2C                   ELSE
199400961217     C*
199500961217     C* PRIMA DI STAMPARLA VERIFICO SE CE N'E' UN'ALTRA COM DATA >
199600961217     C*                    MI SALVO LA DATA
199700990512     C                   MOVEL     ANMDAO        W0080             8 0
199800990512     C                   MOVEL     ANMAAS        KAAS2
199900961217     C                   MOVEL     ANMLNP        KLNP2
200000961217     C                   MOVEL     ANMNRS        KNRS2
200100961217     C                   MOVEL     ANMNSP        KNSP2
200200961217     C                   Z-ADD     ANMCAA        KCAA
200300961217     C*
200400961217     C* LEGGO IL COLLO FINTANTO CHE TROVO DELLE ANOMALIE INSERITE
200500961217     C*  NEL CASO STAMPERO' QUELLA
200600990512     C     KANM1         SETLL     FNANM01L
200700990512     C     KANM1         READE     FNANM01L                               31
200800961217    3C     *IN31         DOWEQ     *OFF
200900990512     C     ANMDAO        ANDLE     W0080
201000990512     C     KANM1         READE     FNANM01L                               31
201100961217    3C                   ENDDO
201200961217     C*
201300961217    3C     *IN31         IFEQ      *ON
201400990512     C   27ANMNR7        CHAIN(N)  FNANM000                           33
201500990512     C   28ANMNR8        CHAIN(N)  FNANM000                           33
201600961217     C*
201700961217     C                   CLEAR                   ANMDCH
201800961217     C                   CLEAR                   ANMFSC
201900961217   X3C                   ELSE
202000961217     C                   SETON                                        10
202100961217     C                   GOTO      ENDCON
202200961217    3C                   ENDIF
202300961217    2C                   ENDIF
202400961217    1C                   ENDIF
202500961217     C*
202600961217    1C     D34APR        IFEQ      'C'
202700961217     C     ANMCCH        ANDEQ     '  '
202800961217     C                   SETON                                        10
202900961217     C                   GOTO      ENDCON
203000961217    1C                   ENDIF
203100961217     C*
203200961217    0C                   ENDIF
203300960419     C**
203400960426     C* 7) TUTTI GLI ALTRI CONFRONTI, TRANNE IL LIVELLO IDD NON FANNO
203500960419     C*    NE STAMPARE NE TRASMETTERE
203600960419     C**
203700960327     C* DATA CHIUSURA ANOMALIA DAL/AL
203800960927     C* PER LE ANOMALIE NON IDD3
203900960927     C     *IN05         IFEQ      *ON
204000960927     C     ANMCCH        ANDEQ     'PR'
204100960927     C     ANMLNA        LOOKUP    FM1                                    32
204200960927     C                   ENDIF
204300960927     C*
204400960927    0C     *IN05         IFEQ      *OFF
204500960927     C     ANMCCH        ORNE      'PR'
204600960927     C     *IN32         OREQ      *OFF
204700960927     C*
204800960327    1C     D34DCA        IFGT      0
204900960419     C     D34ACA        ORGT      0
205000990512    2C     ANMDCH        IFLT      D34DCA
205100990512     C     ANMDCH        ORGT      D34ACA
205200960327     C                   SETON                                        10
205300960327     C                   GOTO      ENDCON
205400960327    2C                   ENDIF
205500960327    1C                   ENDIF
205600960927     C*
205700960927   X0C                   ELSE
205800990512    1C     ANMDCH        IFLT      W08DM1
205900990512     C     ANMDCH        ORGT      W08AM1
206000960927     C                   SETON                                        10
206100960927     C                   GOTO      ENDCON
206200960927    1C                   ENDIF
206300960927    0C                   ENDIF
206400921015     C*
206500921015     C* STAMPA FASE SPECIFICA
206600960327    1C     D34FAS        IFNE      ' '
206700960326     C     ANMFSA        ANDNE     D34FAS
206800921015     C                   SETON                                        10
206900921015     C                   GOTO      ENDCON
207000960327    1C                   ENDIF
207100960426     C*
207200960506     C* STAMPA CAUSALE CHIUSURA SPECIFICA
207300960426    1C     D34CCH        IFNE      ' '
207400960426     C     ANMCCH        ANDNE     D34CCH
207500960426     C                   SETON                                        10
207600960426     C                   GOTO      ENDCON
207700960426    1C                   ENDIF
207800961108     C*
207900040116     C* in partenza escludo le anomalie appena create (livello 0 o 2
208000040616     c*  per anom 15) solo per le anomalie IDD e se non prevista la
208100040616     c*  stampa dalla DS D34SL0
208200040116     C** 07ANMLID        IFEQ      0
208300040616     c                   if        d34sl0=' '
208400040119     c                   if        anmlid=§7clid  and §7cidd='S' and
208500040119     c                             wanom='P' and §7cinv=' '
208600961108     C                   SETON                                        10
208700961108     C                   GOTO      ENDCON
208800961108    1C                   ENDIF
208900040616    1C                   ENDIF
209000960419     C*
209100960506     C* STAMPA CODICI ANOMALIE RICHIESTE
209200960327    1C     D34ANM        IFNE      *BLANKS
209300921015     C                   MOVEL     ANMCAA        ALF3              3
209400921015     C     ALF3          LOOKUP    ANM                                    31
209500960327    2C     D34INC        IFEQ      'I'
209600921015     C  N31              SETON                                        10
209700921015     C  N31              GOTO      ENDCON
209800921015     C*
209900960327   X2C                   ELSE
210000921015     C*
210100921015     C   31              SETON                                        10
210200921015     C   31              GOTO      ENDCON
210300960327    2C                   END
210400960402     C*
210500960402     C                   ELSE
210600960402     C*
210700960402     C* SE NON HO RICHIESTO UNO SPECIFICO CODICE ANOMALIA
210800960402     C*  GESTISCO SOLO QUELLI STAMPABILI
210900960402     C     §7CSTA        IFNE      'S'
211000960402     C                   SETON                                        10
211100960402     C                   GOTO      ENDCON
211200960506    2C                   ENDIF
211300960506    1C                   ENDIF
211400921015     C*
211500921015     C* NUMERO FOGLIO VIAGGIO
211600960327    1C     D34NFV        IFNE      0
211700960326     C     ANMNFV        ANDNE     D34NFV
211800921015     C                   SETON                                        10
211900921015     C                   GOTO      ENDCON
212000960327    1C                   ENDIF
212100960506     C*
212200960506     C* SE ANOMALIA INTERNA VEDO SE RICHIESTO PART O ARR
212300960506    1C     ANMAIE        IFEQ      'I'
212400960506     C     D34PAR        ANDNE     ' '
212500960506     C*
212600960506     C* RICHIESTE LE ANOMALIE PARTENZA MA QUESTA E' ANOMALIA ARRIVO
212700960506    2C     D34PAR        IFEQ      'P'
212800960506     C     §7CPAR        ANDEQ     'A'
212900960506     C                   SETON                                        10
213000960506    2C                   ENDIF
213100960506     C* VICEVERSA
213200960506    2C     D34PAR        IFEQ      'A'
213300960506     C     §7CPAR        ANDEQ     'P'
213400960506     C                   SETON                                        10
213500960506    2C                   ENDIF
213600960506     C*
213700960506     C   10              GOTO      ENDCON
213800960506    1C                   ENDIF
213900040615     c
214000980327     C* IN CASO DI TRASFERIMENTO DI FILIALE
214100040615    1C**   D34TRA        IFEQ      'S'
214200980327     C* ANOMALIA ESTERNA
214300040615     C**   ANMAIE        ANDEQ     'E'
214400980327     C* ARRIVO
214500040615    2C**   WANOM         IFEQ      'A'
214600980327     C* PRENDO O LNP O FLS
214700040615    3C**   ANMLNP        IFGT      0
214800040615     C**                 Z-ADD     ANMLNP        W0030             3 0
214900040615     C**                 ELSE
215000040615     C**                 Z-ADD     ANMFLS        W0030
215100040615    3C**                 ENDIF
215200980327     C*
215300040615     C**   W0030         LOOKUP    L6                                     32
215400040615     C**N32W0030         LOOKUP    L1                                     34
215500040116     C**N32
215600040116     C** 34              MOVEL     'R'           WRTF
215700040116     C* N32
215800040116     C*N 34              MOVEL     'E'           WRTT
215900040615     C* N32
216000040615     C*N 34              MOVEL     'U'           NODEL
216100040615    2C**                 ENDIF
216200980327     C* PARTENZE
216300040615    2C**   WANOM         IFEQ      'P'
216400040615     C**   ANMLNA        LOOKUP    L6                                     32
216500040615     C**N32ANMLNA        LOOKUP    L1                                     34
216600040116     C**N32
216700040116     C*N 34              MOVEL     'R'           WRTT              1
216800040615     C**N32
216900040615     C*N 34              MOVEL     'E'           WRTF              1
217000040615     C**N32
217100040615     C*N 34              MOVEL     'U'           NODEL
217200040615    2C**                 ENDIF
217300980327     C*
217400040615    1C**                 ENDIF
217500960403     C* DAL LIVELLO IDD
217600980327     C     *IN97         IFEQ      *ON
217700980327     C     *IN01         ANDEQ     *ON
217800980327     C     D34TRA        OREQ      'S'
217900000317     C     D34GES        OREQ      'V'
218000990114     C   24
218100990512     COR 25ANMN10        CHAIN     FNANM000                           33
218200000306     C   26ANMNR3        CHAIN     FNANM000                           33
218300000306     C   27ANMNR7        CHAIN     FNANM000                           33
218400990512     C   28ANMNR8        CHAIN     FNANM000                           33
218500990512     C   29ANMNR9        CHAIN     FNANM000                           33
218600960403     C                   ENDIF
218700960403     C*
218800960404    1C   97D34ELA        IFNE      ' '
218900960403     C                   ADD       1             ANMLID
219000960403     C                   ENDIF
219100960403     C**
219200960405     C   97ANMLID        IFLT      D34LID
219300960405     C* SE NON E' ELABORAZIONE DEFINITIVA IDD 10 ON
219400960405     C     D34ELA        IFNE      'S'
219500960403     C                   SETON                                        10
219600960417     C                   GOTO      ENDCON
219700960405     C                   ELSE
219800960419     C* ALTRIMENTI ACCENDO L' 11  PERCHE' DEVO AGGIORNARE SENZA STAMPAR
219900960405     C                   SETON                                        11
220000960405     C                   ENDIF
220100960405     C*
220200960403    2C                   ENDIF
220300960412     C*
220400960327     C     ENDCON        ENDSR
220500040616     c     CercaCAE      begsr
220600040616     c     kcae1         setll     azcae01l
220700040616     c     kcae3         readpe    azcae01l
220800040616    3c                   dow       not %eof(azcae01l) and caeatb<>*blanks
220900040616     c     kcae3         readpe    azcae01l
221000040616    3c                   enddo
221100040616     c
221200040616    3c                   if        not %eof(azcae01l)
221300040616    4c                   if        caedde<=anmdao   and caedsc>=anmdao
221400040616     c* Trovata eccezione terminal --> se il terminal  d34fgs esco
221500040616    5c                   if        caetfe=d34fgs
221600040616     c* Anomalia da non stampare
221700040616     c                   eval      wanomok='N'
221800040616     c                   else
221900040616     c* Anomalia da     stampare
222000040616     c                   eval      wanomok='S'
222100040616    5c                   endif
222200040616    4c                   endif
222300040616    3c                   endif
222400040616     c                   ENDSR
222500960927     C*
222600960927     C* CONTROLLO SE C'E' DISGUIDO IN LETTURA ANOMALIE PARTENZA ------*
222700960930     C     DISPAR        BEGSR
222800971124     C* VEDO SE E' PRESENTE UNA ANOMALIA DI DISGUIDO
222900990409     C                   SETOFF                                       20
223000990409     C                   DO        2
223100990409     C                   Z-ADD     56            KCAA
223200990409     C     *IN20         IFEQ      *OFF
223300990512     C     KANM2         CHAIN     FNANM02L                           32
223400960927    1C     *IN32         DOWEQ     *OFF
223500971126    2C     AN2DAO        IFGT      WDDET
223600960927     C                   SETON                                        20
223700960927     C                   MOVEL     CDISG         PSTAM
223800960927     C                   MOVEL     AN2FLE        WDFLP
223900971126     C                   Z-ADD     AN2DAO        WDDET
224000960927    2C                   ENDIF
224100990512     C     KANM2         READE     FNANM02L                               32
224200960927    1C                   ENDDO
224300990409     C*
224400990409     C                   Z-ADD     55            KCAA
224500990409    2C                   ENDIF
224600990409    1C                   ENDDO
224700960927     C*
224800090312     C* SE NON TROVATA CONTROLLO artFLP
224900960927    1C     *IN20         IFEQ      *OFF
225000090312     C     artDET        ANDGT     0
225100960927     C**
225200960927     C* CHAIN IL FV
225201090312     c                   z-add     artnfv        knfv
225300020807     C     K02FGV01      CHAIN     FNFGV01L                           32
225400960927    4C     *IN32         IFEQ      *OFF
225500020807     C     K02FGV01      CHAIN     FNFGW01L                           32
225600000503    5C     *IN32         IFEQ      *ON
225700000503     C     FGWATB        ORNE      ' '
225800000503     C                   CLEAR                   FGWFF3
225900000503     C                   CLEAR                   FGWFF4
226000000503     C                   CLEAR                   FGWFL3
226100000503     C                   CLEAR                   FGWFL4
226200000503    5C                   ENDIF
226300090312     C                   MOVEL     artFLP        W003A             3
226400960930     C                   MOVEL     ANMLNA        W003AA            3
226500961001     C                   Z-ADD     0             XX                3 0
226600961001     C                   SETON                                        32
226700961001     C*
226800961001    5C     *IN32         DOWEQ     *ON
226900961001     C     PSTAM         ANDEQ     *BLANKS
227000961001     C                   ADD       1             XX
227100960927     C     W003A         LOOKUP    FLP(XX)                                32
227200960927     C**
227300961001    6C     *IN32         IFEQ      *ON
227400960930     C     FFV(XX)       ANDEQ     W003AA
227500960930     C                   MOVEL     CTRANS        PSTAM            18
227600961001    6C                   ENDIF
227700961001    5C                   ENDDO
227800961001     C**
227900961001    5C     PSTAM         IFEQ      *BLANKS
228000960927     C                   MOVEL     CDISG         PSTAM
228100960927    5C                   ENDIF
228200960927   X4C                   ELSE
228300960927     C                   MOVEL     CDISG         PSTAM
228400960927    4C                   ENDIF
228500960927     C                   SETON                                        20
228600090312     C                   MOVEL     artFLP        WDFLP
228700090312     C                   MOVEL     artDET        WDDET
228800960927    1C                   ENDIF
228900960927     C                   ENDSR
229000960508     C*
229100960508     C* CHAIN SULLE TABELLE DELL'ANOMALIA ---------------------------*
229200960508     C     LEGCAA        BEGSR
229300960508     C     ANMCAA        IFNE      SAVCAA
229400960508     C                   MOVEL     '7C'          COD
229500960508     C                   MOVEL(P)  ANMCAA        KEY
229600960508     C     KTAB          CHAIN(N)  TABEL                              31
229700960508     C  N31              MOVEL     TBLUNI        DS7C
229800960508     C   31              CLEAR                   DS7C
229900960508     C                   MOVEL     ANMCAA        SAVCAA
230000960508     C                   ENDIF
230100960508     C                   ENDSR
230200911015     C*
230300960827     C* CHIUDO ANOMALIE DI DISGUIDO O ANOMALIE 50 SE ESISTE BLP ------*
230400960827     C     A50A55        BEGSR
230500021025     C                   SETOFF                                       32
230600090312     C     KART          CHAIN     FiarT27L                           39
230700090312     C  N39arTLNP        LOOKUP    L1                                     32
230800021025    2C     *IN32         IFEQ      *ON
230900960705     C* TROVATA BOLLA PARTENZA:
231000990409     C* CHIUDO ANOMALIA 55 O 56
231100960705     C                   MOVE      'N'           WTRM              1
231200960705     C                   CLEAR                   DSLR33
231300960705     C                   MOVE      ANMDAO        D33DCH
231400960705     C                   MOVEL     'M'           D33FSC
231500960705     C                   MOVEL     'AN'          D33CCH
231600960705     C                   MOVEL     'S'           D33MAC
231700990114     C   24
231800990114     COR 25              MOVE      ANMN10        D33NRR
231900000306     C   26              MOVE      ANMNR3        D33NRR
232000000306     C   27              MOVE      ANMNR7        D33NRR
232100960709     C   28              MOVE      ANMNR8        D33NRR
232200960709     C   29              MOVE      ANMNR9        D33NRR
232300960705     C                   CALL      'FNLR33R'
232400960705     C                   PARM                    DSLR33
232500960827     C* SE ANOMALIA 55:
232600960827    3C     ANMCAA        IFEQ      55
232700990409    3C     ANMCAA        OREQ      56
232800960705     C* CHIUDO ANOMALIA 200
232900960709     C                   MOVE      KCAA          WCAA
233000960705     C                   Z-ADD     200           KCAA
233100990512     C     KANM2         CHAIN     FNANM02L                           32
233200960705     C     *IN32         IFEQ      *OFF
233300960705     C                   CLEAR                   DSLR33
233400960705     C                   MOVE      AN2DAO        D33DCH
233500960705     C                   MOVEL     'AN'          D33CCH
233600960705     C                   MOVE      ANMNR2        D33NRR
233700960705     C                   CALL      'FNLR33R'
233800960705     C                   PARM                    DSLR33
233900960705     C                   END
234000960709     C                   MOVE      WCAA          KCAA
234100960827    3C                   END
234200960705    2C                   END
234300960705     C                   ENDSR
234400960705     C*
234500911021     C* STAMPO ANOMALIE ---------------------------------------------*
234600900621     C     STAMPA        BEGSR
234700051221     c                   setoff                                       4344
234800960405     C* PER IDD
234900911025     C* SE DIVENTA 3 LA CHIUDO CON CAUSALE PR = PRATICA
235000960405    1C   97D34ELA        IFNE      ' '
235100960403    2C     ANMLID        IFEQ      §7ALCI
235200990512     C                   MOVEL     D34ALA        ANMDCH
235300911025     C                   MOVEL     'PR'          ANMCCH
235400921027     C                   MOVEL     'D'           ANMFSC
235500960327    2C                   ENDIF
235600960327    1C                   ENDIF
235700960514     C*
235800960514     C* SE IL SOGGETTO ANOMALIA E' "E" --> NON STAMPO L'ANOMALIA
235900960515     C*                                    SE IDD IN ARRIVO
236000040116    1C**   ANMSAN        IFNE      'E'
236100040116    1C**   ANMAIE        ANDEQ     'E'
236200040116    1C**   ANMAIE        ORNE      'E'
236300040116    c
236400040116    3C     ANMSAN        IFNE      'E'
236500040116     C     wanom         ANDEQ     'A'
236600040116     C     ANMAIE        ORNE      'E'
236700040116     C     wanom         orne      'A'
236800911025     C*
236900911021     C* NUMERO SPEDIZIONE
237000911021     C     ANMNSP        COMP      0                                  0202
237100911021     C* NUMERO SEGNACOLLO
237200970314     C     ANMFLS        COMP      0                                  0303
237300970314     C     ANMCAA        COMP      300                                    22
237400960417     C**
237500960417     C* SE C'E' IL NUMERO DI SPEDIZIONE, VADO A PRENDERE LA DATA
237600960514    2C     ANMNSP        IFGT      0
237700960417     C                   SETOFF                                       33
237800960417     C                   CLEAR                   G02INV
237900960417     C*
238000960417     C**
238100040609     C**   ANMLNP        LOOKUP    L1                                     32
238200960620     C* C'E' COME LNP
238300040609    3C**   *IN32         IFEQ      *ON
238400960620     C*
238500960417     C     KANM          CHAIN     FNBLP01L                           33
238600960514    4C     *IN33         IFEQ      *OFF
238700960509     C                   MOVEL     BLPZNC        PRTZNC
238800960417     C                   Z-ADD     BLPMGS        G02INV
238900971126     C                   MOVEL     BLPAAS        G02INV
239000050503     c* Se c'e' particolarità consegna CP, la stampo
239100050503     c                   if        blpgma='CP'
239200050503     c                   seton                                        43
239300050503     c                   endif
239400051221     c* Se richiesto stampo dati mittente e destinatario
239500051221     c                   if        d34med = 'S' and d34ela = ' '
239600051221     c                   seton                                        44
239700051221     c                   movel     blprsm        prtrsm           32
239800051222     c* rmn : lo stampo eliminando gli spazi a sinistra non significativi
239900051222     c                   move      blprmn        prtrmn           15
240000051222     c     ' 0'          check     prtrmn        N                 2 0
240100051222     c                   eval      prtrmn = %subst(prtrmn:N)
240200051221     c                   endif
240300050503     c
240400960514    4C                   ENDIF
240500040609    3C**                 ENDIF
240600960417     C*
240700040609    3C**   *IN32         IFEQ      *OFF
240800040609     C**   *IN33         OREQ      *ON
240900040609     C**   ANMLNA        LOOKUP    L1                                     32
241000040609     C**                 SETOFF                                       33
241100040609    4C**   *IN32         IFEQ      *ON
241200960620     C*
241300040609     C**   KANM          CHAIN     FNARB01L                           33
241400040609    5C**   *IN33         IFEQ      *OFF
241500040609     C**                 MOVEL     ARBZNC        PRTZNC
241600040609     C**                 Z-ADD     ARBMGS        G02INV
241700040609     C**                 MOVEL     ARBAAS        G02INV
241800040609    5C**                 ENDIF
241900040609    4C**                 ENDIF
242000960417     C*
242100960417     C* SE NON TROVATA CERCO NELLE BOLLE TRANSITO
242200040609    4C**   *IN32         IFEQ      *OFF
242300040609     C**   *IN33         OREQ      *ON
242400040609     C**   KBTP          CHAIN     FNBTP11L                           33
242500040609    5C**   *IN33         IFEQ      *OFF
242600040609     C**                 MOVEL     BTPZNC        PRTZNC
242700040609     C**                 Z-ADD     BTPMGS        G02INV
242800040609     C**                 MOVEL     BTPAAS        G02INV
242900040609    5C**                 ENDIF
243000040609    4C**                 ENDIF
243100040609    3C**                 ENDIF
243200960417     C*
243300961018     C                   CLEAR                   PRTDSP
243400960514    3C     G02INV        IFGT      0
243500960417     C                   MOVEL     3             G02ERR
243600971126     C                   CALL      'XSRDA8'
243700960417     C                   PARM                    WLBDAT
243800960417     C                   MOVEL     G02DAT        PRTDSP            4 0
243900960514    3C                   ENDIF
244000960514    2C                   ENDIF
244100960508     C*
244200960508     C* PER L'ANOMALIA 005 VERIFICO SE IL COLLO E' ANDATO IN DISGUIDO
244300040116     C***  ANMAIE        IFNE      'R'
244400040116     C     ANMCAA        ifeq      005
244500960508     C                   EXSR      ANM005
244600960514    2C                   ENDIF
244700960927     C*
244800960508     C* SE HO TROVATO IL DISGUIDO LO STAMPO
244900960514    2C     *IN20         IFEQ      *ON
245000960508     C                   MOVE      WDDET         G02INV
245100960508     C                   MOVEL     3             G02ERR
245200971126     C                   CALL      'XSRDA8'
245300960508     C                   PARM                    WLBDAT
245400960508     C                   MOVEL     G02DAT        PRTDET            4 0
245500960514    2C                   ENDIF
245600911021     C* TIPO ANOMALIA
245700960514    2C     ANMTAN        IFNE      SAVTAN
245800911021     C                   MOVEL     '7F'          COD
245900960327     C                   MOVEL(P)  ANMTAN        KEY
246000960409     C     KTAB          CHAIN(N)  TABEL                              31
246100040616     C   31              MOVEL     *BLANKS       tbluni
246200040616     c* Vedo semettere o meno il tipo di anomalia (Idd/int/no idd)
246300040616     c                   if        *in17
246400040616     C                   MOVEL     TBLUNI        DESTAN           12
246500040616     c                   else
246600040616     c                   movel     tbluni        w008a             8
246700040616     c                   endif
246800921015     C*
246900921015     C                   MOVEL     ANMTAN        SAVTAN            1
247000960514    2C                   END
247100040616     c*
247200040616     c                   if        not *in17
247300040616     c                   clear                   destan
247400040616     C* ANOMALIA
247500040616    2C     ANMAIE        IFEQ      'I'
247600040616     C                   MOVEL     'INT'         DESAIE            3
247700040616   X2C                   ELSE
247800040616     C**
247900040616    3C     §7CIDD        IFEQ      'S'
248000040616     C                   MOVEL     'IDD'         DESAIE                         IDD
248100040616     C                   ELSE
248200040616     C                   MOVEL     'NON'         DESAIE                         NON IDD
248300040616    3C                   ENDIF
248400040616    2C                   ENDIF
248500040616     c                   movel     desaie        destan
248600040616     c                   move      w008a         destan
248700040616     c                   endif
248800960506     C*
248900960506     C* DESCRIZIONE CODICE ANOMALIA
249000960506     C                   MOVEL     §7CDES        DESCAA           25
249100960704     C*
249200960705     C* SE ANOMALIA 10-MANCA RECORD BOLLA ARRIVO O 138-COLLO GIA' CONS.
249300960705     C* CONTROLLO SE COLLO SPARATO IN INVENTARIO O IN CONSEGNA
249400960704     C                   SETOFF                                       21
249500960704     C     ANMCAA        IFEQ      10
249600960705     C     ANMCAA        OREQ      138
249700960704     C     ANMLNA        LOOKUP    L6                                     32
249800960704     C     *IN32         IFEQ      *ON
249900960704     C                   EXSR      RCRSPU
250000960704     C                   END
250100960704     C                   END
250200960402     C*
250300950320     C* SE BOLLA IN CONSEGNA REPERISCO NR/DATA DISTINTA E PADRONCINO
250400950320     C                   SETOFF                                       09
250500960514    2C     ANMCAA        IFEQ      135
250600090312     C     KART          CHAIN     FiART27L                           31
250800090312     C  n31KARB          CHAIN     FNARB01L                           31
250900960514    4C     *IN31         IFEQ      *OFF
251000960509     C                   MOVEL     ARBZNC        PRTZNC
251100960509     C*
251200960514    5C     ARBNDC        IFNE      0
251300950320     C                   SETON                                        09
251400950320     C*
251500950320     C                   MOVEL     DESCAA        DESCA1           18
251600950320     C                   MOVE      ':'           DESCA1
251700950320     C* Data distinta
251800950320     C                   MOVE      ARBDDC        G02INV
251900950320     C                   MOVEL     3             G02ERR
252000971126     C                   CALL      'XSRDA8'
252100950320     C                   PARM                    WLBDAT
252200950320     C                   MOVEL     G02DAT        STADDC            4 0
252300950320     C* Padroncino
252400950320     C                   MOVE      *BLANKS       STAPDC           12
252500021203     C*    ARBPDC        CHAIN     FNAPD01L                           32
252600021205     C                   move      arbpdc        kpdpdr
252700021203     C                   move      'A'           kpdtip
252800021203     C     kfapd         chain     fiapd01L                           32
252900960327     C  N32              MOVEL     APDRSC        STAPDC
253000960514    5C                   END
253100960514    4C                   END
253300960514    2C                   END
253400990121     C**
253500990121     C** NUMERO CA SE C'E'
253600990122     C                   SETOFF                                       404142
253700990121     C     KART          CHAIN     FNDCD02L                           32
253800990121    2C     *IN32         IFEQ      *OFF
253900990121     C     DCDATB        ANDEQ     ' '
254000990121     C     KDCT          CHAIN     FNDCT01L                           32
254100050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
254200050221     c                   If        Not *In32 and dctdt2 > 0
254300050221     c                   Eval      *In32 = *On
254400050221     c                   EndIf
254500990121     C* CONTROLLO LA CAUSALE DI CHIUSURA SE C'E'
254600990121    3C  N32DCTDCH        IFGT      0
254700990121     C     DCTCCH        ANDNE     ' '
254800990121     C                   CLEAR                   DSBS02
254900990121     C                   MOVEL     'C'           T02MOD
255000990121     C                   MOVEL     KNSIF         T02SIF
255100990121     C                   MOVEL     'CCH'         T02COD
255200990121     C                   MOVEL     DCTCCH        T02KE1
255300990121     C                   CALL      'TIBS02R'
255400990121     C                   PARM                    KPJBA
255500990121     C                   PARM                    DSBS02
255600990121    4C     T02ERR        IFEQ      ' '
255700990121     C                   MOVEL     T02UNI        DCCH
255800990121     C                   ELSE
255900020905     ***C                   MOVEL     'S'           §CHCRA
256000020905     C                   MOVEL     'S'           §CCHCRAN
256100990121    4C                   ENDIF
256200990121    3C                   ENDIF
256300990121     C**
256400990121     C** STAMPO SE CHIUSA C.A. SENZA CAUSALE
256500990122    3C  N32DCTCCH        IFEQ      *BLANKS
256600990121     C     DCTDCH        ANDGT     0
256700990121     C** STAMPO SE CHIUSA C.A. CON CAUSALE PER CUI CREARE ANOMALIA
256800990121     C*  (SIGNIFICA CHE E' CA REALE)
256900020905     ***C     §CHCRA        OREQ      'S'
257000020905     C     §CCHCRAN      OREQ      'S'
257100990121     C     DCTDCH        ANDGT     0
257200990122     C* STAMPO SE C.A. APERTA E DETT C.A. APERTO
257300990122     C     DCTDCH        OREQ      0
257400990122     C     DCDDCH        ANDEQ     0
257500990122     C* DECODIFICA DANNI
257600990122     C                   EXSR      DECDAN
257700990122    3C                   ENDIF
257800990121    2C                   ENDIF
257900050503     c
258000050503     C* VERIFICO PER CP SE STAMPARE decodifica corta o lunga
258100050503     c                   clear                   decodicp         23
258200050503     c                   if        *in43
258300050503     c                   if        *in41
258400050503     c                   eval      decodicp=CCPcort
258500050503     c                   else
258600050503     c                   eval      decodicp=CCPlung
258700050503     c                   endif
258800050503     c                   endif
258900990122     C**
259000990122     C* SE NON E' SU C.A. PER LA 151 STAMPO LA 1 C.A. CHIUSA CHE TROVO
259100990122    2C     *IN32         IFEQ      *ON
259200990122     C     ANMNSP        ANDGT     0
259300990122     C     ANMCAA        ANDEQ     151
259400990122     C                   CLEAR                   WTRDAN
259500990122     C**
259600040811     C**""**     KANM          CHAIN     FNDCT02L                           32
259700040811    3C**""**     *IN32         DOWEQ     *OFF
259800040811    4C**""**     DCTATB        IFNE      ' '
259900040811     C**""**     DCTDCH        OREQ      0
260000040811     C**""**     KANM          READE     FNDCT02L                               32
260100040811     C**""**                   ELSE
260200040811     C**""**                   SETON                                        32
260300040811     C**""**                   MOVEL     'S'           WTRDAN            1
260400040811    4C**""**                   ENDIF
260500040811    3C**""**                   ENDDO
260600990122     C**
260700040811      * Sostituisco la lettura del file FNDTC02 con
260800040811      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul e chiuse
260900040811      * legate alla spedizione che passo
261000040811      *
261100040811     c                   clear                   fidn12ds
261200040811     c                   eval      i12aas = anmaas
261300040811     c                   eval      i12lnp = anmlnp
261400040811     c                   eval      i12nrs = anmnrs
261500040811     c                   eval      i12nsp = anmnsp
261600040811     c                   eval      i12fel = 'E'
261700040811     c                   eval      i12fan = 'E'
261800040811      *
261900040811     c                   call      'FIDN12R'
262000040811     c                   parm                    fidn12ds
262100040811      *
262200040811      * se non ci sono errori
262300040811     c                   if        o12err = ' '
262400040811      * se numero di CA trovate maggiore di zero
262500040811     c                   if        o12nca > 0
262600040811      * prendo solo la prima CA chiusa
262700040811     c                   z-add     *zeros        jj                2 0
262800040811     c     1             do        o12nca        jj
262900040811     c                   if        skdch(jj) > 0
263000040811     c                   movel     'S'           wtrdan            1
263100040811     c                   movea     skkey(jj)     dskey
263200040811     C                   Z-ADD     dsfil         dctfil
263300040811     C                   Z-ADD     dsnca         dctnca
263400040811     C                   Z-ADD     skdca(jj)     dctdca
263500040811     C                   Z-ADD     skdch(jj)     dctdch
263600040811     c                   z-add     o12nca        jj
263700040811     c                   endif
263800040811     C                   ENDDO
263900040811      *
264000040811     c                   endif
264100040811     c                   endif
264200040811     C*
264300990122     C** TROVATA LA C.A. CHIUSA: STAMPO I DATI
264400990122     C     WTRDAN        IFEQ      'S'
264500990122     C                   EXSR      DECDAN
264600990122    3C                   ENDIF
264700990122    2C                   ENDIF
264800911021     C* FASE APERTURA
264900960514    2C     ANMFSA        IFNE      SAVFSA
265000911021     C                   MOVEL     '7D'          COD
265100960327     C                   MOVEL(P)  ANMFSA        KEY
265200960409     C     KTAB          CHAIN(N)  TABEL                              32
265300040616     C  N32              MOVEL     TBLUNI        DESFSA            8
265400960327     C   32              MOVEL     *BLANKS       DESFSA
265500921015     C*
265600921015     C                   MOVEL     ANMFSA        SAVFSA            1
265700960514    2C                   END
265800911021     C* DATA APERTURA
265900971126     C                   CLEAR                   G02INV
266000911021     C                   MOVE      ANMDAO        G02INV
266100911018     C                   MOVEL     3             G02ERR
266200971126     C                   CALL      'XSRDA8'
266300911018     C                   PARM                    WLBDAT
266400950320     C                   MOVEL     G02DAT        STADAO            4 0
266500911021     C* FASE CHIUSURA
266600960514    2C     ANMFSC        IFNE      SAVFSC
266700911021     C                   MOVEL     '7D'          COD
266800960327     C                   MOVEL(P)  ANMFSC        KEY
266900960409     C     KTAB          CHAIN(N)  TABEL                              32
267000040616     C  N32              MOVEL     TBLUNI        DESFSC            8
267100960327     C   32              MOVEL     *BLANKS       DESFSC
267200921015     C*
267300921015     C                   MOVEL     ANMFSC        SAVFSC            1
267400960514    2C                   END
267500960506     C*
267600960508     C** SE RICEVUTA STAMPO ANMFLE
267700040116    2C**   ANMAIE        IFEQ      'R'
267800040116     C**                 SETON                                        04
267900040116     C**                 ELSE
268000040116     C**                 SETOFF                                       04
268100040116    2C**                 ENDIF
268200040116     c* lo faccio se sto leggendo le anomalie partenza
268300040616    2C**   wanom         IFEQ      'P'
268400040616     C**                 SETON                                        04
268500040616     C**                 ELSE
268600040616     C**                 SETOFF                                       04
268700040616    2C**                 ENDIF
268800911021     C* DATA CHIUSURA
268900960514    2C     ANMDCH        IFGT      0
269000971126     C                   CLEAR                   G02INV
269100911021     C                   MOVE      ANMDCH        G02INV
269200911021     C                   MOVEL     3             G02ERR
269300971126     C                   CALL      'XSRDA8'
269400911021     C                   PARM                    WLBDAT
269500950320     C                   MOVEL     G02DAT        STADCH            4 0
269600960417     C                   ELSE
269700960417     C                   CLEAR                   STADCH
269800960514    2C                   ENDIF
269900911021     C* CAUSALE CHIUSURA
270000960514    2C     ANMCCH        IFNE      SAVCCH
270100970523     C     ANMCCH        IFNE      'RA'
270200911021     C                   MOVEL     '7E'          COD
270300960417     C                   MOVEL(P)  ANMCCH        KEY
270400960409     C     KTAB          CHAIN(N)  TABEL                              31
270500960417     C  N31              MOVEL     TBLUNI        DESCCH           12
270600911021     C   31              MOVEL     *BLANKS       DESCCH
270700970523     C                   ELSE
270800970523     C                   CLEAR                   DESCCH
270900970523     C                   END
271000921015     C*
271100921015     C                   MOVEL     ANMCCH        SAVCCH            2
271200960514    2C                   END
271300970523     C*
271400970523     C     ANMCCH        IFEQ      'RA'
271500970523     C                   CLEAR                   CCHPRT
271600970523     C                   ELSE
271700970523     C                   MOVE      ANMCCH        CCHPRT
271800970523     C                   END
271900040616     C* DATA/ora creazione anomalia
272000040616     c                   clear                   stadac
272100040616     c                   clear                   staorc
272200040616     c                   if        anmflo<>*blanks
272300040616     c                   movel     anmflo        danmflo
272400040616     C                   CLEAR                   G02INV
272500040616     C                   MOVE      §anmdac       G02INV
272600040616     C                   MOVEL     3             G02ERR
272700040616     C                   CALL      'XSRDA8'
272800040616     C                   PARM                    WLBDAT
272900040616     C                   MOVEL     G02DAT        STADAc            4 0
273000040616     c                   move      §anmorc       staorc            4 0
273100040616     c                   endif
273200970523     C*
273300960327     C**
273400960403     C* SOLO SE ANOMALIA ESTERNE IDD STAMPO IL LIVELLO IDD
273500960404     C   97              MOVEL     ANMLID        STALID            1
273600960404     C  N97              CLEAR                   STALID
273700960509     C**
273800960509     C* SE NON L'HO ANCORA TROVATA PRENDO LA ZONA DALLE SPUNTE
273900960514    2C     PRTZNC        IFEQ      '  '
274000070111     C     KART          SETLL     FNBRV07L
274100070111     C     KART          READE     FNBRV07L                               32
274200960514    3C     *IN32         DOWEQ     *OFF
274300960509     C     PRTZNC        ANDEQ     '  '
274400960514    4C     BRVZNC        IFGT      0
274500960509     C                   MOVEL     BRVZNC        PRTZNC
274600960509     C                   ELSE
274700070111     C     KART          READE     FNBRV07L                               32
274800960514    4C                   ENDIF
274900960514    3C                   ENDDO
275000960514    2C                   ENDIF
275100990512     C* ANNO A 2
275200990512     C   02              MOVE      ANMAAS        PRTAAS            2 0
275300911025     C*
275400900622     C* STAMPO DETTAGLIO
275500921015     C*
275600921015     C* STAMPA TESTATA
275700960514    2C     D34ELA        IFNE      ' '
275800960403     C     ANMLID        ANDLT     §7ALCI
275900960327     C     D34ELA        OREQ      ' '
276000960327     C*
276100040615     c                   if        *inOF
276200040615     C                   EXCEPT    TES
276300040615     C  n70              EXCEPT    TES2
276400040615     C                   EXCEPT    TES3
276500040615     C                   SETOFF                                       OF
276600040615     c                   endif
276700921015     C*
276800921015     C                   EXCEPT    DET
276900050503     c   41
277000050503     cor 43              except    det1
277100051221     c   44              except    det2
277200050503     c
277300031007     C   55              EXSR      WRIDET
277400960422     C                   MOVEL     '1'           WSTAMP            1
277500921015     C*
277600960514   X2C                   ELSE
277700921015     C*
277800921015     C   OE              EXCEPT    TESX
277900921015     C   OE              SETOFF                                       OE
278000921015     C*
278100911025     C                   EXCEPT    DETX
278200050503     c   41
278300050503     Cor 43              EXCEPT    DETX1
278400031007     C   55              EXSR      WRIDET
278500960514    2C                   ENDIF
278600900622     C*
278700040616     C**N17              SETOFF                                       04
278800040401     c* Scrivo file per report di sede
278900040401     c     wgir200       ifeq      'S'
279000040407     c     *in01         andeq     *on
279100040401     c                   exsr      srant
279200040401     c                   endif
279300960514    1C                   ENDIF
279400900622     C                   ENDSR
279500990122     C*
279600990122     C* DECODIFICA DATI RECORD C.A. ----------------------------------*
279700990122     C     DECDAN        BEGSR
279800990122     C* SE 09 20 O 21 STAMPO SU 2 RIGA ALTRIMENTI SULLA PRIMA
279900990122    1C     *IN09         IFEQ      *OFF
280000990122     C     *IN20         ANDEQ     *OFF
280100990122     C     *IN21         ANDEQ     *OFF
280200990122     C                   SETON                                        40
280300990122     C                   MOVEL     DESCAA        DESCA1           18
280400990122     C                   MOVE      ':'           DESCA1
280500990122     C                   ELSE
280600990122     C                   SETON                                        41
280700990122    1C                   ENDIF
280800990122     C**
280900990122     C* Data C.A.
281000990122     C                   MOVE      DCTDCA        G02INV
281100990122     C                   MOVEL     3             G02ERR
281200990122     C                   CALL      'XSRDA8'
281300990122     C                   PARM                    WLBDAT
281400990122     C                   MOVEL     G02DAT        STADCA            6 0
281500990122     C                   Z-ADD     G02DAT        W0020             2 0
281600990122     C                   MOVE      W0020         STADCA
281700990122     C* Data CHIUSURA C.A.
281800990122    1C     DCTDCH        IFGT      0
281900990122     C                   MOVE      DCTDCH        G02INV
282000990122     C                   MOVEL     3             G02ERR
282100990122     C                   CALL      'XSRDA8'
282200990122     C                   PARM                    WLBDAT
282300990122     C                   MOVEL     G02DAT        STACHA            4 0
282400990122     C                   SETON                                        42
282500990122     C**
282600990122    1C                   ENDIF
282700990122     C                   ENDSR
282800960508     C*
282900960508     C* CONTROLLO PER ANOMALIA 005 SE E' COLLO IN DISGUIDO ----------*
283000960508     C     ANM005        BEGSR
283100960508     C* PROVO A CHAINARE FNART
283200090312     C     KART          CHAIN     FIART27L                           32
283300960927    1C     *IN32         IFEQ      *OFF
283400960927     C     ARTDET        ANDGT     0
283500960508     C                   SETON                                        20
283600960508     C                   MOVEL     ARTFLP        WDFLP
283700960508     C                   MOVEL     ARTDET        WDDET
283800960807     C*
283900960927     C                   Z-ADD     1             KNPG
284000960927     C* CERCO LA SPUNTA CON FGS=ARTFLP E DATA= ARTDET
284100070111     C     KBRV3         SETLL     FNBRV03L
284200070111     C     KBRV3         READE     FNBRV03L                               32
284300960927    2C     *IN32         DOWEQ     *OFF
284400960509     C* ZONA
284500960927    3C     BRVZNC        IFGT      0
284600960509     C                   MOVEL     BRVZNC        PRTZNC
284700960927    3C                   ENDIF
284800960927     C*
284900000201     C                   Z-ADD     BRVNFV        WNFV
285000960927     C     KFVA          CHAIN     FNFVA01L                           32
285100960927    3C     *IN32         IFEQ      *OFF
285200040617     C     FVADFV        ANDEQ     ARTDUT
285300960927     C                   SETON                                        32
285400960927    4C     BRVATR        IFEQ      'T'
285500960927     C                   MOVEL     CTRANS        PSTAM
285600960927     C                   ELSE
285700960927     C                   MOVEL     CDISG         PSTAM
285800960927    4C                   ENDIF
285900960927   X3C                   ELSE
286000960927     C                   SETOFF                                       32
286100960927    3C                   ENDIF
286200960927     C**
286300070111     C  N32KBRV3         READE     FNBRV03L                               32
286400960927    2C                   ENDDO
286500960927     C**
286600960927     C* SE NON TROVATO METTO DISGUIDO
286700960927     C     PSTAM         IFEQ      *BLANKS
286800960927     C                   MOVEL     CDISG         PSTAM
286900960927     C                   ENDIF
287000960927    1C                   ENDIF
287100960508     C                   ENDSR
287200911021     C*
287300960704     C* RICERCA SPUNTA INVENTARIO O CONSEGNA PER ANOMALIA 10 --------*
287400960704     C     RCRSPU        BEGSR
287500960704     C                   CLEAR                   WNPG
287600960704     C                   CLEAR                   WSPG
287700960704     C                   CLEAR                   WDFV
287800960704     C                   CLEAR                   WFGS
287900960704     C* CERCO SPUNTA INVENTARIO E CONSEGNA E PRENDO QUELLA PIU' RECENTE
288000960704    1C                   DO        2             II                1 0
288100960704    2C     II            IFEQ      1
288200960704     C                   Z-ADD     3             KNPG
288300960704     C                   ELSE
288400960704     C                   Z-ADD     4             KNPG
288500960704    2C                   END
288600070111     C     KBRV          SETLL     FNBRV03L
288700070111     C     KBRV          READE     FNBRV03L                               32
288800960704    2C     *IN32         DOWEQ     *OFF
288900000201     C                   Z-ADD     BRVNFV        WNFV
289000960704     C     KFVV          CHAIN     FNFVV01L                           32
289100960704    3C     *IN32         IFEQ      *OFF
289200960704     C     FVVDFV        ANDGT     WDFV
289300960704     C                   MOVE      FVVNPG        WNPG
289400960704     C                   MOVE      FVVSPG        WSPG
289500960704     C                   MOVE      FVVDFV        WDFV
289600960704     C                   MOVE      FVVFGS        WFGS
289700960704    3C                   END
289800070111     C     KBRV          READE     FNBRV03L                               32
289900960704    2C                   ENDDO
290000960704    1C                   ENDDO
290100960704    1C     WDFV          IFGT      0
290200960704     C                   SETON                                        21
290300960704     C* ROVESCIO DATA SPUNTA
290400960704     C                   CLEAR                   WLBDAT
290500960704     C                   MOVE      WDFV          G02INV
290600960704     C                   MOVEL     3             G02ERR
290700971126     C                   CALL      'XSRDA8'
290800960704     C                   PARM                    WLBDAT
290900960704     C                   MOVEL     G02DAT        PRTDFV            4 0
291000960704     C* IMPOSTO CAMPO PER STAMPA DECODIFICA CATEGORIA/SOTTOCATEGORIA
291100960704    2C     WNPG          IFEQ      3
291200960704     C                   MOVEL     'IM'          W003A             3
291300960704     C                   MOVE      WSPG          W003A
291400960704     C                   MOVEL(P)  W003A         PRTDOV            5
291500960704     C                   ELSE
291600960704     C                   MOVEL(P)  'CONS.'       PRTDOV
291700960704    2C                   END
291800960704    1C                   END
291900960704     C                   ENDSR
292000911021     C* INVIO ANOMALIE ----------------------------------------------*
292100911021     C     ELAB          BEGSR
292200990512     C                   MOVEL     'FNANM00T'    D60FIL
292300971126     C                   Z-ADD     AM8GIO        D60DRF
292400971128     C                   MOVEL(P)  '*SHRUPD'     D60VNC
292500921027     C* FLAG TRASMISSIONE
292600960604    1C     ANMFL1        IFNE      0
292700921027     C                   MOVEL     'T'           ANMFT1
292800000824     C                   MOVE      AM8GIO        ANMDTR
292900960604    1C                   END
293000960604    1C     ANMFL2        IFNE      0
293100921027     C                   MOVEL     'T'           ANMFT2
293200000824     C                   MOVE      AM8GIO        ANMDTR
293300960604    1C                   END
293400921027     C* SI INVIO IDD
293500960523    1C     W7AIDD        IFEQ      'S'
293600960604     C* DISALLOCAZIONE OGGETTI
293700021009     c                   eval      D60po0 = SIMfel
293800961218     C                   CLEAR                   D60FL1
293900961218     C                   CLEAR                   D60FL2
294000960604     C*
294100961218    2C  N95SAVFL1        IFNE      ANMFL1
294200961218     C                   MOVEL     SAVFL1        D60FL1
294300960604    2C                   ENDIF
294400961218    2C  N96SAVFL2        IFNE      ANMFL2
294500961218     C                   MOVEL     SAVFL2        D60FL2
294600960604    2C                   ENDIF
294700960604     C*
294800960604    2C     D60FL1        IFGT      0
294900960604     C     D60FL2        ORGT      0
295000990512     C   99              CLOSE     FNANM00T
295100960604     C   99              SETOFF                                       99
295200960604     C                   MOVEL     'D'           D60AZN
295300021009     C                   MOVEL     FNLV60DS      KPJBU
295400960604     C                   CALL      'FNLV60R'
295500960604     C                   PARM                    KPJBA
295600960604    2C                   ENDIF
295700960604     C* ALLOCAZIONE OGGETTI
295800021009     c                   eval      D60po0 = SIMfel
295900961218     C                   CLEAR                   D60FL1
296000961218     C                   CLEAR                   D60FL2
296100961218     C                   SETOFF                                       9596
296200961218     C* ANMFL1 >0 E IN L1 --> 95 ON
296300961218     C     ANMFL1        IFGT      0
296400961218     C     ANMFL1        LOOKUP    L1                                     95
296500961218     C                   ENDIF
296600961218     C* ANMFL2 >0 E IN L1 --> 96 ON
296700961218     C     ANMFL2        IFGT      0
296800961218     C     ANMFL2        LOOKUP    L1                                     96
296900961218     C                   ENDIF
297000960604     C*
297100961218    2C  N95ANMFL1        IFNE      SAVFL1
297200961218     C                   MOVEL     ANMFL1        D60FL1
297300960604    2C                   ENDIF
297400961218    2C  N96ANMFL2        IFNE      SAVFL2
297500961218     C                   MOVEL     ANMFL2        D60FL2
297600960604    2C                   ENDIF
297700960604     C*
297800960604    2C     D60FL1        IFGT      0
297900960604     C     D60FL2        ORGT      0
298000960604     C                   MOVEL     'A'           D60AZN
298100021009     C                   MOVEL     FNLV60DS      KPJBU
298200960604     C                   CALL      'FNLV60R'
298300960604     C                   PARM                    KPJBA
298400021009     C                   MOVEL     KPJBU         FNLV60DS
298500960604     C                   MOVEL     D60ERR        WERR
298600961023     C     D60FL1        IFGT      0
298700960604     C                   MOVEL     D60MB1        WMB1
298800961023     C                   ENDIF
298900961023     C     D60FL2        IFGT      0
299000960604     C                   MOVEL     D60MB2        WMB2
299100961023     C                   ENDIF
299200021009     C     WMB2          IFEQ      *BLANKS
299300040223     C**** D60FL2        OREQ      *zeros
299400961023     C                   CLEAR                   ANMFL2
299500961023     C                   ENDIF
299600960604    2C                   ENDIF
299700960604     C**
299800960604     C* NON CI SONO ERRORI --> SCRIVO E AGGIORNO
299900961219    2C     WERR          IFNE      'S'
300000960604     C                   MOVEL     ANMFL1        SAVFL1
300100960604     C                   MOVEL     ANMFL2        SAVFL2
300200961218    3C  N95SAVFL1        IFGT      0
300300021008     ***C                   MOVEL     WMB1          VAR
300400021009     c                   movel     wMB1          DSmembro
300500021008     c                   movel     DSmembro      VAR
300600950115     C*
300700960604     C     *IN99         IFEQ      *OFF
300800990512     C                   MOVE      VAR           CMD1                           FNANM00T
300900911021     C                   MOVEL     *BLANKS       COMMAN           80
301000911021     C                   MOVEA     CMD1(1)       COMMAN
301100960604     C                   CALL      'QCMDEXC'                              98
301200911021     C                   PARM                    COMMAN
301300911021     C                   PARM                    LUNG
301400960604     C*
301500960604     C   98              MOVEL     'S'           WERR
301600911021     C**
301700911021     C** SCRIVO FILE INVIO
301800911021     C**
301900990512     C  N98              OPEN      FNANM00T
302000960604     C                   ENDIF
302100960604     C*
302200990512     C  N98              WRITE     FNANM0TT                                      INVIO
302300960604     C  N98              SETON                                        99
302400960604    3C                   ENDIF
302500960604    2C                   ENDIF
302600960604     C**
302700960604     C* NON CI SONO ERRORI --> SCRIVO E AGGIORNO
302800961219    2C  N96WERR          IFNE      'S'
302900961023     C     SAVFL2        ANDGT     0
303000961023     C     WMB2          ANDNE     WMB1
303100990512     C                   CLOSE     FNANM00T
303200960604     C                   SETOFF                                       99
303300960604     C**
303400021009     c                   movel     wMB2          DSmembro
303500021008     c                   movel     DSmembro      VAR
303600960604     C*
303700990512     C                   MOVE      VAR           CMD1                           FNANM00T
303800960604     C                   MOVEL     *BLANKS       COMMAN           80
303900960604     C                   MOVEA     CMD1(1)       COMMAN
304000960604     C                   CALL      'QCMDEXC'                              98
304100960604     C                   PARM                    COMMAN
304200960604     C                   PARM                    LUNG
304300960604     C*
304400960604     C   98              MOVEL     'S'           WERR
304500960604     C**
304600960604     C** SCRIVO FILE INVIO
304700960604     C**
304800990512     C  N98              OPEN      FNANM00T
304900990512     C  N98              WRITE     FNANM0TT                                      INVIO
305000990512     C  N98              CLOSE     FNANM00T
305100970306    2C                   ENDIF
305200970306     C** PER REPORT DI SEDE
305300970306    3C     ANMCAA        IFEQ      55
305400990409    3C     ANMCAA        OREQ      56
305500040504     c* la lnp disguido è quella della bolla, se c'e'
305600040504     c                   if        anmlnp>0
305700040504     c                   movel     anmlnp        wlnp
305800040504     c                   else
305900040504     C                   MOVEL     ANMFLS        wLNP
306000040504     c                   endif
306100040504     c
306200970306     C     KA55          CHAIN     FNANT02L                           39
306300970306     C*
306400970306    4C     *IN39         IFEQ      *ON
306500970306     C                   CLEAR                   FNANT000
306600970306     C                   MOVEL     ANMFLE        ANTFOA
306700040504     C****               MOVEL     ANMFLS        ANTLNP
306800040504     C                   MOVEL     wlnp          ANTLNP
306900990512     C                   MOVEL     ANMDAO        ANTDAO
307000970306     C                   MOVEL     ANMCAA        ANTCAA
307100970306     C                   Z-ADD     1             ANTTOT
307200990504     C*
307300990504     C                   CLEAR                   DSLV55
307400990504     C                   MOVEL     'P'           D55TPT
307500040504     C**                 MOVEL     anmfls        D55LIN
307600040504     C                   MOVEL     wlnp          D55LIN
307700990512     C                   MOVEL     ANMDAO        D55DRF
307800990504     C                   CALL      'FNLV55R'
307900990504     C                   PARM                    DSLV55
308000000317     C* PER LE ANOMALIE 55 E 56 ANTLNA CONTIENE IL TERMINAL DI PARTENZA
308100990504     C*  DELLE BOLLE
308200990504     C                   MOVEL     D55TFP        ANTLNA
308300040927     C                   Z-ADD     AM8GIO        antdt1
308400970306     C                   WRITE     FNANT000
308500970306   X4C                   ELSE
308600970306     C                   ADD       1             ANTTOT
308700040927     C****               CLEAR                   ANTFT1
308800040927     C****               CLEAR                   ANTFT2
308900040927     C                   Z-ADD     AM8GIO        antdt1
309000970306     C                   UPDATE    FNANT000
309100970306    4C                   ENDIF
309200970306    3C                   ENDIF
309300960523    1C                   ENDIF
309400911021     C*
309500960404     C* LE ANOMALIE 70 E 71 LE CHIUDO
309600960404     C* CON CAUSALE
309700000317    1C   01ANMCAA        IFEQ      70
309800960404     C     ANMCAA        OREQ      71
309900960523    2C     ANMDCH        IFEQ      0
310000990512     C                   MOVEL     D34ALA        ANMDCH
310100960404     C                   MOVEL     'D'           ANMFSC
310200970305     C*
310300970305    3C     ANMCAA        IFEQ      71
310400970306     C     KA71          CHAIN     FNANT02L                           39
310500970305     C*
310600970305    4C     *IN39         IFEQ      *ON
310700970305     C                   CLEAR                   FNANT000
310800970306     C                   MOVEL     ANMFLE        ANTFOA
310900970306     C                   MOVEL     ANMLNP        ANTLNP
311000990512     C                   MOVEL     ANMDAO        ANTDAO
311100970305     C                   MOVEL     ANMCAA        ANTCAA
311200970305     C                   Z-ADD     1             ANTTOT
311300040927     c                   z-add     am8gio        antdt1
311400970305     C                   WRITE     FNANT000
311500970305   X4C                   ELSE
311600970305     C                   ADD       1             ANTTOT
311700040927     c                   z-add     am8gio        antdt1
311800970305     C                   UPDATE    FNANT000
311900970305    4C                   ENDIF
312000970305    3C                   ENDIF
312100970305     C**
312200960523    2C                   ENDIF
312300960523    1C                   ENDIF
312400970523     C*
312500970523     C* SE CAUSALE CHIUSURA = 'RA' LA ABBLENCO
312600970523     C* ('RA' --> RIPRISTINO DI ANOMALIA 10 CHIUSA)
312700970523     C     ANMCCH        IFEQ      'RA'
312800970523     C                   CLEAR                   ANMCCH
312900970523     C                   END
313000960404     C*
313100000317     C* SE NON CI SONO STATI ERRORI DI TRASMISSIONE UPDATO
313200961219     C     WERR          IFNE      'S'
313300001102     C     D34GES        IFEQ      'V'
313400001102     C                   UPDATE    FNANM011
313500001102     C                   ELSE
313600990512     C                   UPDATE    FNANM000                                      AGGIORNO
313700001102     C                   ENDIF
313800961023     C                   ELSE
313900001102     C     D34GES        IFEQ      'V'
314000001102     C                   UNLOCK    FNANM11L
314100001102     C                   ELSE
314200990512     C                   UNLOCK    FNANM00F
314300001102     C                   ENDIF
314400960604    1C                   ENDIF
314500911021     C                   ENDSR
314600040401     C* SCRITTURA FILE PER REPORT DI SEDE ---------------------------*
314700040401     C     SRANT         BEGSR
314800040401     C** PER REPORT DI SEDE
314900040504     c                   if        anmlnp>0
315000040504     c                   movel     anmlnp        wlnp
315100040504     c                   else
315200040504     C                   MOVEL     ANMFLS        wLNP
315300040504     c                   endif
315400040405     C     KA200         CHAIN     FNANT02L                           39
315500040401     C*
315600040401    4C     *IN39         IFEQ      *ON
315700040401     C                   CLEAR                   FNANT000
315800040401     C                   MOVEL     ANMFLE        ANTFOA
315900040504     C                   MOVEL     wlnp          ANTLNP
316000040405     C* Come data anomalia metto data idd per poter conteggiare le anomalie
316100040405     c* lette tutte nello stesso report controllo qualità
316200040405     C                   MOVEL     WDTA200       ANTDAO
316300040401     C                   MOVEL     ANMCAA        ANTCAA
316400040401     C                   Z-ADD     1             ANTTOT
316500040401     C*
316600040401     C                   CLEAR                   DSLV55
316700040401     C                   MOVEL     'P'           D55TPT
316800040401     C                   MOVEL     ANMFLe        D55LIN
316900040401     C                   MOVEL     ANMDAO        D55DRF
317000040401     C                   CALL      'FNLV55R'
317100040401     C                   PARM                    DSLV55
317200040401     C* PER l'anomalia 200 ANTLNA CONTIENE IL TERMINAL DI PARTENZA
317300040401     C*  del p.o. che ha generato il disguido
317400040401     C                   MOVEL     D55TFP        ANTLNA
317500040927     c                   z-add     am8gio        antdt1
317600040401     C                   WRITE     FNANT000
317700040401     C                   ELSE
317800040401     C                   ADD       1             ANTTOT
317900040927     c                   z-add     am8gio        antdt1
318000040401     C                   UPDATE    FNANT000
318100040401     C                   ENDIF
318200040401     c                   endsr
318300911021     C*
318400960404     C* CONTROLLO PER TRASMETTERE LA CHIUSURA ANOMALIE 70 E 71 ------*
318500960404     C     CHIUAN        BEGSR
318600960417     C** MANCA BAM
318700960417     C                   Z-ADD     71            KCAA
318800960404     C                   Z-ADD     1             L
318900960404     C*
319000960417    1C     L6(L)         DOWGT     0
319100960404     C                   Z-ADD     L6(L)         KFIL
319200990512     C     KANM5         SETLL     FNANM10L
319300990512     C     KANM5         READE     FNANM10L                               30
319400960417    2C     *IN30         DOWEQ     *OFF
319500960417     C                   EXSR      BAM
319600990512     C     KANM5         READE     FNANM10L                               30
319700960417    2C                   ENDDO
319800960404     C**
319900960404     C                   ADD       1             L
320000960417    1C                   ENDDO
320100960417     C** ECCEDE BAM
320200960417     C                   Z-ADD     70            KCAA
320300021021     C                   Z-ADD     1             L
320400021021    1C     L6(L)         DOWGT     0
320500021021     C                   Z-ADD     L6(L)         KFIL
320600021021     C     Kanm20        SETLL     FNANM20L
320700021021     C     Kanm20        READE     FNANM20L                               30
320800960417    2C     *IN30         DOWEQ     *OFF
320900960417     C* SE ANMFLE E' PRESENTE IL L6 --> ELABORO
321000021021     c*  tolto il controllo perchè anmfle già in key
321100021021     C**   ANMFLE        LOOKUP    L6                                     31
321200021021     C**   *IN31         IFEQ      *ON
321300960417     C                   EXSR      BAM
321400021021     C**                 ENDIF
321500960417     C*
321600021021     C     Kanm20        READE     FNANM20L                               30
321700960417    2C                   ENDDO
321800021021     C**
321900021021     C                   ADD       1             L
322000021021    2C                   ENDDO
322100960404     C*
322200960404     C                   ENDSR
322300960417     C*
322400960417     C* CONTROLLO LE ANOMALIE 70 E 71 PER CHIUDERLE -----------------*
322500960417     C     BAM           BEGSR
322600960417     C                   MOVEL     ANMAAS        KAAS2
322700960417     C                   MOVEL     ANMLNP        KLNP2
322800960417     C                   MOVEL     ANMNRS        KNRS2
322900960417     C                   MOVEL     ANMNSP        KNSP2
323000960417     C*
323100960417     C* LEGGO IL COLLO FINTANTO CHE TROVO DELLE ANOMALIE INSERITE
323200990512     C     KANM1         SETLL     FNANM01L
323300990512     C     KANM1         READE     FNANM01L                               31
323400960417    4C     *IN31         DOWEQ     *OFF
323500990512     C     KANM1         READE     FNANM01L                               31
323600960417    4C                   ENDDO
323700960417     C*
323800960417     C* LETTA L'ULTIMA, VEDO CHE DATA CHIUSURA HA
323900960417    4C     ANMCCH        IFEQ      '  '
324000970424     C     ANMDCH        ANDGT     0
324100990512     C     ANMDCH        ANDLT     D34ALA
324200960417     C* RICHAINO CON NUMERO RELATIVO DI RECORD
324300990512     C     ANMNR1        CHAIN     FNANM00F                           31
324400960417     C* TRASMETTO IN PARTENZA LA CHIUSURA DELLA ANOMALIA
324500960417     C                   MOVEL     'OB'          ANMCCH
324600990512     C                   MOVEL     D34ALA        ANMDCH
324700960417     C                   EXSR      ELAB
324800960417    4C                   ENDIF
324900960417     C                   ENDSR
325000960422     C*
325100960423     C* STAMPA DI CHI NON HA FATTO L'IDD ----------------------------*
325200960423     C     NOIDD         BEGSR
325300960422     C                   SETON                                        08
325400040615     C                   SETOff                                       09
325500960423     C                   CLEAR                   WMANID
325600960423     C                   Z-ADD     95            KCAA
325700960422     C* SE IMMESSA LA DATA AL CERCO NELLA DATA AL
325800960422     C*  ALTRIMENTI CERCO LA DATA IDD PIU' ALTA CHE HO
325900990512    1C     D34ALA        IFEQ      0
326000990512     C     D34ACA        ANDEQ     0
326100990512     C     KCAA          SETGT     FNANM04L
326200990512     C     KCAA          READPE    FNANM04L                               30
326300960422    2C     *IN30         DOWEQ     *OFF
326400050504     C**   ANMAIE        ANDEQ     'E'
326500050504    2C**   *IN30         OREQ      *OFF
326600990512     C     ANMDAO        ANDGE     AM8GIO
326700990512     C     KCAA          READPE    FNANM04L                               30
326800960422    2C                   ENDDO
326900960422     C*
327000971126     C     *IN30         IFEQ      *OFF
327100971126     C                   Z-ADD     ANMDAO        WDATA8            8 0
327200971126     C                   ENDIF
327300971126     C**
327400960422   X1C                   ELSE
327500971126     C     D34ALA        IFGT      0
327600971126     C                   MOVEL     D34ALA        WDATA8
327700961016     C                   ELSE
327800971126     C                   MOVEL     D34ACA        WDATA8
327900961016     C                   ENDIF
328000961016     C*
328100960422     C                   SETOFF                                       30
328200960422    1C                   ENDIF
328300960510     C*
328400960926     C* PER I P.O. CHE FANNO L'IDD IL GIORNO DOPO
328500960510     C*  CERCO LA DATA PRECEDENTE
328600960510     C  N30              EXSR      RICDAT
328700990512     C  N30              MOVEL     GI8INV        WDATAM            8 0
328800960510     C*
328900960422     C* 30 OFF - TROVATA
329000960422    1C     *IN30         IFEQ      *OFF
329100960422     C                   Z-ADD     0             L
329200990518     C* GIRO LA DATA ELABORAZIONE DEL GIORNO
329300971126     C                   MOVE      WDATA8        G02INV
329400960422     C                   MOVEL     '3'           G02ERR
329500971126     C                   CALL      'XSRDA8'
329600960422     C                   PARM                    WLBDAT
329700990513     C                   MOVEL     G02DAT        STADCI            8 0
329800960510     C*
329900971126     C                   CLEAR                   G02INV
330000960510     C                   MOVE      WDATAM        G02INV
330100960510     C                   MOVEL     '3'           G02ERR
330200971126     C                   CALL      'XSRDA8'
330300960510     C                   PARM                    WLBDAT
330400990513     C                   MOVEL     G02DAT        STADCM            8 0
330500960422     C*
330600960422     C     *LOVAL        SETLL     AZORG01L
330700960423     C                   READ      AZORG01L                               30
330800960422     C*
330900960422    2C     *IN30         DOWEQ     *OFF
331000960423    C**
331100960510     C* ESCLUDO LE ANNULLATE
331200960422    3C     ORGFVA        IFEQ      ' '
331300960423     C**
331400960422     C* SOLO FILIALE E AGENZIA
331500960422    4C     ORGFAG        IFEQ      'F'
331600960422     C     ORGFAG        OREQ      'A'
331700960423     C**
331800960926     C* ESCLUDO I P.O. CHE NON DEVONO FARE IDD
331900960422     C                   MOVEL     ORGDE8        OG148
332000050831     C                   MOVEL     ORGDE3        OG143
332100960422    5C     §OGNID        IFEQ      ' '
332200971124     C* CERCO IL TERMINAL DI PARTENZA
332300971124     C* SE STO ELABORANDO L'IDD E NON SONO IL TERMINAL DI PARTENZA
332400971124   5AC     D34ELA        IFNE      ' '
332500971124     C     ORGFIL        ANDNE     D34FGS
332600971124     C**
332700971124     C                   CLEAR                   DSLV55
332800971124     C                   MOVEL     'P'           D55TPT
332900971124     C                   MOVEL     ORGFIL        D55LIN
333000980403     C                   Z-ADD     AM8GIO        D55DRF
333100971124     C                   CALL      'FNLV55R'
333200971124     C                   PARM                    DSLV55
333300971124   5BC     D55ERR        IFNE      ' '
333400971124     C                   Z-ADD     SIMFEL        D55TFP
333500971124   5BC                   ENDIF
333600971124   5AC                   ENDIF
333700960510     C**
333800960509     C* SE ANOMALIE IDD --> ESCLUDO SE NON SONO DELL'AREA
333900960423   5AC     D34ELA        IFEQ      ' '
334000971124     C     ORGFIL        ORNE      D34FGS
334100971124     C     D55TFP        ANDEQ     D34FGS
334200960422     C*
334300960422     C                   MOVEL     ORGFIL        KLNA
334400960510     C* IMPOSTO LA DATA
334500960510     C     §OGEID        IFEQ      ' '
334600971126     C                   Z-ADD     WDATA8        KDAO                           DEL GIORNO
334700960510     C                   ELSE
334800960510     C                   Z-ADD     WDATAM        KDAO                           GIORNO PREC
334900960510     C                   ENDIF
335000960510     C*
335100990512     C     KANM4         SETLL     FNANM04L                               31
335200960422    6C     *IN31         IFEQ      *OFF
335300960423     C                   MOVEL     '1'           WMANID            1
335400960422     C*
335500960422     C* NON TROVATA STAMPO ERRORE
335600960422    7C     L             IFEQ      7
335700960422     C                   EXSR      ERRIDD
335800960422    7C                   ENDIF
335900050830     c
336000050830     C                   ADD       1             L
336100050830     C                   Z-ADD     ORGFIL        FID(L)
336200050831     C                   MOVEL     ORGDES        DFI(L)
336300050831     c                   clear                   dslv55
336400050830     c* Verifico se il £6
336500050830     c                   if        §ogntw<>'EEX' and §ogntw<>'DPD' and
336600050830     c                             §ogntw<>'FED' and §ogntw<>'EUP'
336700050830     C                   MOVEL     '6'           D55TPT
336800050831     c                   else
336900050831     c                   movel     'P'           D55tpt
337000050831     c                   endif
337100050830     C                   MOVEL     ORGFIL        D55LIN
337200050830     C                   Z-ADD     kdao          D55DRF
337300050830     C                   CALL      'FNLV55R'
337400050830     C                   PARM                    DSLV55
337500960422     C**
337600050831     c                   if        §ogntw<>'EEX' and §ogntw<>'DPD' and
337700050906     c                             §ogntw<>'FED' and §ogntw<>'EUP' and
337800050906     c                             orgfil<>101
337900050906     c
338000050830     c                   if        d55tfa=orgfil
338100050831     c* Imposto la decodifica del p.o. se non è in gestione ad altri
338200050831     C***                MOVEL     ORGDES        DFI(L)
338300050830     c                   else
338400050831     c* Imposto il p.o. gestione
338500050831     c****               eval      DFI(L)=CGestito
338600050831     c                   movel     d55tfa        w003a             3
338700050831     c                   eval      %subst(DFI(L):8:12)=CGESTITO
338800050831     c                   eval      %subst(DFI(L):20:3)=w003a
338900050830     c                   endif
339000050830     c
339100050830     c                   else
339200050906     c* Imposto il terminal di partenza del P.o estero o della 101
339300050906     c*  come gestione
339400050831     c***                eval      DFI(L)=CGestito
339500050831     c                   movel     d55tfp        w003a             3
339600050831     c                   eval      %subst(DFI(L):8:12)=CGESTITO
339700050831     c                   eval      %subst(DFI(L):20:3)=w003a
339800050830     c                   endif
339900960422     C*
340000960422    6C                   ENDIF
340100960422     C*
340200960423   5AC                   ENDIF
340300960423    5C                   ENDIF
340400960422    4C                   ENDIF
340500960422    3C                   ENDIF
340600960422     C*
340700960423     C                   READ      AZORG01L                               30
340800960422    2C                   ENDDO
340900960422     C*
341000960422     C* SE NE RIMANGONO FINISCO DI STAMPARE
341100960423    2C     L             IFGT      0
341200960422     C                   EXSR      ERRIDD
341300960423   X2C                   ELSE
341400960423     C* SE CI SONO TUTTI LO DICO
341500960423    3C     WMANID        IFEQ      ' '
341600960423     C*
341700960423    4C     *INOF         IFEQ      *ON
341800960423     C     *IN09         OREQ      *OFF
341900040615     c                   if        *inof
342000040615     C                   EXCEPT    TES
342100040615     C  n70              EXCEPT    TES2
342200040615     c                   endif
342300960423     C*
342400960423    5C     D34ELA        IFEQ      ' '
342500960423     C                   EXCEPT    TESIDR
342600960423     C                   ELSE
342700960423     C                   EXCEPT    TESIDE
342800960423    5C                   ENDIF
342900960423     C                   SETON                                        09
343000960423     C                   SETOFF                                       OF
343100960423    4C                   ENDIF
343200960423     C**
343300960423     C* TUTTI HANNO ESEGUITO L'IDD
343400960423     C                   EXCEPT    SIIDD
343500960423    3C                   ENDIF
343600960423     C*
343700960423    2C                   ENDIF
343800960422     C*
343900960422    1C                   ENDIF
344000960422     C*
344100960422     C                   SETOFF                                       08
344200960422     C                   ENDSR
344300960510     C*
344400960926     C* RICERCA DATA IDD-1 X I P.O. CHE LO FANNO IL GG DOPO----------*
344500990512     C     RICDAT        BEGSR
344600960510     C* UTILIZZO IL CALENDARIO
344700971126     C                   MOVE      WDATA8        G02INV
344800960510     C                   MOVEL     '3'           G02ERR
344900971126     C                   CALL      'XSRDA8'
345000960510     C                   PARM                    WLBDAT
345100960510     C*
345200960510     C                   MOVEL     'S'           WFEST             1
345300960510     C                   CLEAR                   KTFP
345400960510     C* CERCO IL PRIMO GIORNO LAVORATIVO PRECEDENTE
345500960510     C     WFEST         DOWEQ     'S'
345600960510     C                   SUB       1             G02TGI
345700990512     C                   Z-ADD     G02TGI        GI8TGI
345800990518     C                   CALL      'XSRGI8'
345900960510     C                   PARM                    WGIDAT
346000990512     C                   Z-ADD     GI8INV        WDATAM
346100960510     C*
346200960510     C                   CLEAR                   WFEST
346300960510     C     KCLN          CHAIN     AZCLN01L                           31
346400960510    1C     *IN31         IFEQ      *OFF
346500960510     C     POM(WGG)      ANDNE     *BLANKS
346600960510     C                   MOVEL     '1Y'          COD
346700960510     C                   MOVEL(P)  POM(WGG)      KEY
346800960510     C     KTAB          CHAIN     TABEL                              31
346900960510    2C     *IN31         IFEQ      *OFF
347000960510     C                   MOVEL     TBLUNI        DS1Y
347100960510    3C     §1YFEI        IFEQ      'S'
347200960510     C                   MOVEL     'S'           WFEST
347300960510    3C                   ENDIF
347400960510    2C                   ENDIF
347500960510    1C                   ENDIF
347600960510     C                   ENDDO
347700960510     C                   ENDSR
347800960422     C*
347900960422     C* STAMPA ERRORE NO IDD ----------------------------------------*
348000960422     C     ERRIDD        BEGSR
348100960422     C* SE OF ACCESO LA PRIMA VOLTA STAMPO LA TESTATA COMPLETA
348200960422    1C     *INOF         IFEQ      *ON
348300960423     C     *IN09         OREQ      *OFF
348400040615     c                   if        *inOF
348500040615     C                   EXCEPT    TES
348600040615     C  n70              EXCEPT    TES2
348700040615     c                   endif
348800960423     C*
348900960423     C     D34ELA        IFEQ      ' '
349000960423     C                   EXCEPT    TESIDR
349100960423     C                   ELSE
349200960423     C                   EXCEPT    TESIDE
349300960423     C                   ENDIF
349400960423     C                   SETON                                        09
349500960423     C                   SETOFF                                       OF
349600960422    1C                   ENDIF
349700960422     C*
349800960422     C                   EXCEPT    DETIDD
349900960422     C*
350000960422     C                   CLEAR                   FID
350100960422     C                   CLEAR                   DFI
350200960422     C                   CLEAR                   L
350300960422     C*
350400960422     C                   ENDSR
350500960404     C*
350600911021     C* INVIO MEMBRI TRASMISSIONE -----------------------------------*
350700911021     C     CAEX          BEGSR
350800050804     C* CREO ANOMALIA OK IDD SUL SISTEMA per tutti i p.o. della £6
350900000317     C     D34ELA        IFEQ      'S'
351000060619     c* apro il file anomalie di sede per scriverci l'anomalia 95
351100060619     c                   EXSR      opensed
351200060619     c
351300050804     C                   Z-ADD     1             LL                3 0
351400050804    2C     L6(LL)        DOWGT     0
351500050804     c
351600050804     C                   Z-ADD     L6(LL)        anmlna
351700050804     C                   Z-ADD     FGSTFP        D55TFP
351800911021     C                   Z-ADD     95            ANMCAA
351900960404     C                   EXSR      CRTANM
352000990512     C                   WRITE     FNANM000
352100060619     c                   if        %open(fnanmsed)
352200060619     C                   WRITE     FNANM0sd
352300060619     c                   endif
352400050804     c
352500050804     c                   add       1             LL
352600050804     c                   enddo
352700050804     c
352800060711    1C                   ENDIF
352900060711     c
353000060711     C* Trasmissione membri  solo se chi esegue è il terminal o se
353100060711     c*  un 2 livello che lo fa dopo il terminal
353200060711    0C     D34FGS        IFEQ      SIMFEL
353300060711     C     D34ALA        ORLE      FELDID
353400911021     C*
353500921029     C     *LOVAL        SETLL     AZORG
353600911021     C                   READ      AZORG                                  30
353700911021     C*
353800960604    1C     *IN30         DOWEQ     *OFF
353900971124    2C     ORGFAG        IFEQ      'F'
354000971124     C     ORGFAG        OREQ      'A'
354100971124    3C     ORGFVA        IFEQ      ' '
354200971124     C* PRENDO IL TERMINAL DI PARTENZA
354300971124     C                   CLEAR                   DSLV55
354400971124     C                   MOVEL     'P'           D55TPT
354500971124     C                   MOVEL     ORGFIL        D55LIN
354600980403     C                   MOVEL     AM8GIO        D55DRF
354700971124     C                   CALL      'FNLV55R'
354800971124     C                   PARM                    DSLV55
354900990517     C* SE NON TROVATO TERMINAL DI PARTENZA NON INVIO OK IDD
355000990517     C* (P.O. NON ANCORA OPERATIVO MA GIA' PRESENTE IN ORGANIGRAMMA)
355100990517    4C     D55ERR        IFEQ      *BLANKS
355200990517    4C     ORGFIL        ANDEQ     D55TFP
355300990331     C     ORGFIL        ANDNE     SIMFEL
355400060711     c
355500990610     C* LA SEDE NON HA TERMINAL MA BISOGNA INVIARLA LO STESSO
355600060711     c* La sede non la invio più scrivo subito fnanm00f di sede
355700960409     C* SOLO PER FILIALI O AGENZIE
355800990512     C   99              CLOSE     FNANM00T
355900960604     C   99              SETOFF                                       99
356000960604     C* DISALLOCAZIONE
356100021009     c                   eval      D60po0 = SIMfel
356200960604     C                   MOVEL     SAVFL1        D60FL1
356300960604     C                   MOVEL     SAVFL2        D60FL2
356400971126     C                   MOVEL     AM8GIO        D60DRF
356500960604     C                   MOVEL     'D'           D60AZN
356600990512     C                   MOVEL     'FNANM00T'    D60FIL
356700021009     C                   MOVEL     FNLV60DS      KPJBU
356800960604     C                   CALL      'FNLV60R'
356900960604     C                   PARM                    KPJBA
357000960604     C                   CLEAR                   SAVFL1
357100960604     C                   CLEAR                   SAVFL2
357200960604     C** ALLOCAZIONE
357300021009     c                   eval      D60po0 = SIMfel
357400021008     c                   reset                   DSmembro
357500021008     c                   eval      MbrPOP = SIMfel
357600021008     c                   eval      MbrPOA = D55tfp
357700021008     c                   movel     DSmembro      VAR
357800960604     C                   CLEAR                   D60FL2
357900971124     C                   MOVEL     D55TFP        D60FL1
358000060711     C                   MOVEL     'X'           D60AZN
358100971126     C                   MOVEL     AM8GIO        D60DRF
358200971128     C                   MOVEL(P)  '*EXCL  '     D60VNC
358300021009     C                   MOVEL     FNLV60DS      KPJBU
358400960604     C                   CALL      'FNLV60R'
358500960604     C                   PARM                    KPJBA
358600021009     C                   MOVEL     KPJBU         FNLV60DS
358700960604     C**
358800960604     C* NON TROVATI ERRORI DI ALLOCAZIONE
358900971124    5C     D60ERR        IFEQ      'N'
359000971124     C                   MOVEL     D55TFP        SAVFL1
359100021009     c                   movel     D60mb1        DSmembro
359200021008     c                   movel     DSmembro      VAR
359300021009     c                   movel     D60mb1        wMBR
359400000317     C**
359500911025     C* PRENDO NUMERO RECORD
359600990512     C                   MOVEL     'FNANM00T'    PGM              10
359700911021     C                   EXSR      NUMREC
359800960405     C*
359900060711     C*
360000971124    6C     NRR           IFGT      0
360100060711     c* controllo spostato sopra
360200060711    7C***  D34FGS        IFEQ      SIMFEL
360300060711     C***  D34ALA        ORLE      FELDID
360400911021     C                   MOVEL     ORGFEL        §DST              3
360500971125     C*
360600020930     C                   CLEAR                   TIBS55DS
360700971125     C                   MOVEL     SIMFEL        I50PPO
360800971125     C                   MOVEL     ORGFIL        I50APO
360900020930     C                   CALL      'TIBS55R'
361000020930     C                   PARM                    TIBS55DS
361100971125     C*
361200971125     C                   MOVEL     O50PLA        CMZLBD
361300971125     C                   MOVEL     O50ALA        CMZLBA
361400971125     C                   MOVEL     O50ASI        CMZSYF
361500911021     C*
361600960405     C                   EXSR      X25ANM
361700060711    7C****               ENDIF
361800971124    6C                   ENDIF
361900960604    5C                   ENDIF
362000960604    4C                   ENDIF
362100960604    3C                   END
362200960604    2C                   END
362300911021     C*
362400911021     C                   READ      AZORG                                  30
362500960604    1C                   ENDDO
362600060711    0C                   ENDif
362700911021     C                   ENDSR
362800060619     C*
362900060619     C* open file anomalie di sede ----------------------------------*
363000060619     C     OPENSED       BEGSR
363100060619     C                   MOVEL     *BLANKS       COMMAN           80
363200060619     C                   MOVEL     OVRSED(1)     COMMAN
363300060706     C                   CALL      'QCMDEXC'                            98
363400060619     C                   PARM                    COMMAN
363500060619     C                   PARM                    LUNG
363600060619     C                   OPEN(E)   FNANMSED
363700060619     c* provo in gaitragrps
363800060619     C                   if        %error
363900060710     c                   eval      %subst(comman:38:11)='PS/FNANM00F'
364000060706     C                   CALL      'QCMDEXC'                            98
364100060619     C                   PARM                    COMMAN
364200060619     C                   PARM                    LUNG
364300060619     C                   OPEN(E)   FNANMSED
364400060619     c                   endif
364500060619     c
364600060619     C                   ENDSR
364700911021     C*
364800911021     C* CREO ANOMALIE ESEGUITO IDD ----------------------------------*
364900911021     C     CRTANM        BEGSR
365000911021     C                   MOVEL     *BLANKS       ANMATB
365100950922     C                   CLEAR                   ANMCDU
365200911021     C                   Z-ADD     0             ANMFLS
365300911021     C                   Z-ADD     0             ANMNRS
365400911021     C                   Z-ADD     0             ANMSCN
365500911021     C                   Z-ADD     0             ANMAAS
365600960404     C                   Z-ADD     0             ANMLNP
365700911021     C                   Z-ADD     0             ANMNSP
365800911021     C                   MOVEL     '7C'          COD
365900960404     C                   MOVEL(P)  ANMCAA        KEY
366000960409     C     KTAB          CHAIN(N)  TABEL                              31
366100911021     C  N31              MOVEL     TBLUNI        DS7C
366200911021     C   31              MOVEL     *BLANKS       DS7C
366300911021     C                   MOVEL     §7CTAN        ANMTAN
366400911021     C                   MOVEL     §7CSAN        ANMSAN
366500921027     C                   MOVEL     'D'           ANMFSA
366600990512     C                   MOVEL     D34ALA        ANMDAO
366700911021     C                   MOVEL     §7CAIE        ANMAIE
366800911025     C                   MOVEL     ' '           ANMFT1
366900911025     C                   MOVEL     *ZEROS        ANMFL1
367000000824     C                   MOVEL     *ZEROS        ANMDTR
367100911021     C* CHIUSURA AUTOMATICA
367200911021     C     §7CCHA        IFEQ      'S'
367300921027     C                   MOVEL     'D'           ANMFSC
367400990512     C                   MOVEL     D34ALA        ANMDCH
367500911021     C                   END
367600960509     C* DA INVIARE
367700911021     C     §7CAIE        IFEQ      'E'
367800971124     C                   MOVEL     D55TFP        ANMFL1
367900911025     C                   MOVEL     'T'           ANMFT1
368000000824     C                   MOVE      AM8GIO        ANMDTR
368100911021     C                   END
368200911021     C                   Z-ADD     0             ANMFL2
368300911021     C                   MOVEL     *BLANKS       ANMFT2
368400911021     C                   MOVEL     *BLANKS       ANMCCH
368500911021     C                   Z-ADD     0             ANMNPS
368600911021     C                   Z-ADD     0             ANMNFV
368700911021     C                   MOVEL     SIMFEL        ANMFLE
368800911021     C                   MOVEL     §7CLID        ANMLID
368900990512     C                   CLEAR                   ANMDUE
369000990512     C                   CLEAR                   ANMNPG
369100990512     C                   CLEAR                   ANMFGS
369200990512     C                   CLEAR                   ANMFLO
369300040601     c* data ora di creazione
369400040601     c                   z-add     *date         wdata             6 0
369500040601     c                   time                    wora              6 0
369600040607     c                   movel     wora          wora4             4 0
369700040601     c                   movel     wdata         anmflo
369800040607     c                   move      wora4         anmflo
369900040601     c
370000990512     C                   CLEAR                   ANMFT3
370100990512     C                   CLEAR                   ANMFL3
370200990512     C                   CLEAR                   ANMFT4
370300040601     C                   z-add     999           ANMFL4
370400911021     C*
370500911021     C                   ENDSR
370600911021     C*
370700990512     C** SOTTOMETTE TRASMISSIONE FNANM00T ---------------------------*
370800911021     C     X25ANM        BEGSR
370900911021     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
371000911021     C                   MOVE      'S'           §CLR              1             CLEAR
371100911021     C                   MOVEL     §DST          CMZDST                          DESTINAT.
371200990512     C                   MOVEL     'FNANM00T'    CMZFLD                          DA: >>>
371300911021     C                   MOVEL     *BLANK        CMZMBD
371400021008     C                   MOVEL     wMBR          CMZMBD
371500990512     C                   MOVEL     'FNANM00R'    CMZFLA                          A: >>>
371600911021     C                   MOVEL     *BLANK        CMZMBA
371700021008     C                   MOVEL     wMBR          CMZMBA
371800911021     C                   MOVE      '0000'        CMZORI                          ORA
371900911021     C                   MOVE      '0000000'     CMZDTI                          DATA
372000040615     C**   D34TRA        IFEQ      'S'
372100040615     C**                 MOVE      'T'           CMZURG
372200040615     C**                 ELSE
372300911115     C                   MOVE      'N'           CMZURG                          ATTESA
372400040615     C**                 END
372500911021     C                   MOVE      'A'           CMZADD                          IN ADD
372600911021     C                   MOVE      §CLR          CMZCLR                          CLEAR
372700911021     C                   MOVE      'S'           CMZFTR                          DA TRANS.
372800911021     C                   MOVE      *BLANK        CMZERR                          1= ERR.
372900040615     C**   D34TRA        IFEQ      'S'
373000040615     C**                 MOVE      'FF20'        CMZAZN                          AZIONE
373100040615     C**                 ELSE
373200990518     C                   MOVE      'LR16'        CMZAZN                          AZIONE
373300040615     C**                 END
373400911021     C                   MOVE      NRR           CMZNRR                          N.REC.
373500020930     C                   CALL      'TIBS53C'
373600971125     C                   PARM                    O50LCL
373700971125     C                   PARM                    CMZDST
373800911021     C                   PARM                    CMZFLD
373900911021     C                   PARM                    CMZMBD
374000911021     C                   PARM                    CMZLBD
374100911021     C                   PARM                    CMZFLA
374200911021     C                   PARM                    CMZMBA
374300911021     C                   PARM                    CMZLBA
374400911021     C                   PARM                    CMZORI
374500911021     C                   PARM                    CMZDTI
374600911021     C                   PARM                    CMZURG
374700911021     C                   PARM                    CMZADD
374800911021     C                   PARM                    CMZCLR
374900911021     C                   PARM                    CMZFTR
375000911021     C                   PARM                    CMZERR
375100911021     C                   PARM                    CMZAZN
375200911021     C                   PARM                    CMZSYF
375300911021     C                   PARM                    CMZNRR
375400911021     C                   PARM                    CMZPRR
375500971125     C                   PARM                    W236            236
375600911021     C                   ENDSR
375700911021     C*
375800911021     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE --------------------*
375900911021     C     NUMREC        BEGSR
376000021008     ***C                   MOVEL     'M   '        VAR2              4
376100021008     ***C                   MOVE      D55TFP        VAR2
376200021008     ***C                   MOVEL     VAR2          ALF10
376300021008     c                   reset                   DSmembro
376400021008     c                   eval      MbrPOP = SIMfel
376500021008     c                   eval      MbrPOA = D55tfp
376600021008     c                   movel     DSmembro      ALF10
376700911021     C                   Z-ADD     0             NRR              10 0
376800921027     C                   CALL      'TRUL51C'
376900921027     C                   PARM                    KNSIF
377000921027     C                   PARM                    PGM
377100921027     C                   PARM                    ALF10            10
377200911021     C                   PARM                    NRR
377300911021     C                   ENDSR
377400980327     C*-------------------------------------------------------------------
377500980327     C* TRASFERIMENTO DA FILIALE A FILIALE
377600040615     C**   TRASFE        BEGSR
377700980327     C* SCRIVO IL RECORD NEL T
377800040615     C**   NODEL         IFEQ      'U'
377900040116     C******             MOVEL     WRTT          ANMAIE
378000980327     C*
378100040615     C**                 MOVEL     'T'           ANMFT1
378200040615     C**                 MOVE      AM8GIO        ANMDTR
378300040615     C**   ANMFL2        IFNE      *ZEROS
378400040615     C**                 MOVEL     'T'           ANMFT2
378500040615     C**                 END
378600040615     C**                 END
378700980327     C*
378800040615     C**   ANMCAA        IFEQ      70
378900040615     C*+   ANMCAA        OREQ      71
379000040116     C**   ANMAIE        IFEQ      'R'
379100040615     c**   wanom         ifeq      'P'
379200040615     C**   ANMCCH        ANDEQ     '  '
379300040615     C**                 CLEAR                   ANMDCH
379400040615     C**                 ENDIF
379500040615     C**                 ENDIF
379600980327     C*
379700040615     C**                 WRITE     FNANM0TT
379800040615     C**                 ADD       1             RECAN             5 0
379900980327     C*
380000980327     C* SE NON SONO IN FASE DECISIVA
380100040615     C**   D34AGG        IFEQ      'AGG'
380200040615     C**   NODEL         IFEQ      'U'
380300040615     C**                 MOVEL     WRTF          ANMAIE
380400040615     C**                 MOVEL     'T'           ANMFT1
380500040615     C**                 MOVE      AM8GIO        ANMDTR
380600040615     C**   ANMFL2        IFNE      *ZEROS
380700040615     C**                 MOVEL     'T'           ANMFT2
380800040615     C**                 END
380900040615     C**   ANMCAA        IFEQ      70
381000040615     C**   ANMCAA        OREQ      71
381100040116     C**   ANMAIE        IFEQ      'R'
381200040615     c**   wanom         ifeq      'P'
381300040615     C**   ANMCCH        ANDEQ     '  '
381400040615     C**                 CLEAR                   ANMDCH
381500040615     C**                 ENDIF
381600040615     C**                 ENDIF
381700040615     C**                 UPDATE    FNANM000
381800040615     C**                 END
381900980327     C*
382000040615     C**   NODEL         IFEQ      ' '
382100040615     C**                 DELETE    FNANM000
382200040615     C**                 END
382300980327     C*
382400040615     C**   NODEL         IFEQ      'S'
382500040615     C**                 UNLOCK    FNANM00F
382600040615     C**                 END
382700980327     C*
382800040615     C**                 END
382900980327     C*
383000040615     C**                 ENDSR
383100980327     C**-------------------------------------------------------------**
383200980327     C** ESEGUE CALL QCAEXEC SUL MEMBRO                              **
383300980327     C**-------------------------------------------------------------**
383400040615     C**   CAEX2         BEGSR
383500040615     C**                 MOVE      ')'           VAR
383600040615     C**                 Z-ADD     48            LUNG             15 5
383700040615     c**                 clear                   DSmembro
383800040615     c**                 eval      MbrM   = 'M'
383900040615     c**                 eval      MbrPOP = SIMfel
384000040615     c**                 eval      MbrPOA = D34fgs
384100040615     c**                 movel     DSmembro      VAR
384200980327     C** FILIALE DI PARTENZA PER APERTURA MEMBRO
384300980327     C** CONTROLLO SE ESISTE IL MEMBRO
384400040615     C**                 SETOFF                                       20        FNANM00T
384500040615     C**                 MOVE      VAR           CMD5
384600040615     C**                 MOVEL     *BLANKS       COMMAN           80
384700040615     C**                 MOVEA     CMD5(1)       COMMAN
384800040615     C**                 CALL      'QCMDEXC'                            20
384900040615     C**                 PARM                    COMMAN
385000040615     C**                 PARM                    LUNG
385100980327     C** CONTROLLO SE ESISTE IL MEMBRO
385200040615     C** 20              DO
385300040615     C**                 MOVE      VAR           CMD3
385400040615     C**                 MOVEL     *BLANKS       COMMAN
385500040615     C**                 MOVEA     CMD3(1)       COMMAN
385600040615     C**                 CALL      'QCMDEXC'
385700040615     C**                 PARM                    COMMAN
385800040615     C**                 PARM                    LUNG
385900040615     C**                 END
386000980327     C** CONTROLLO SE ESISTE IL MEMBRO
386100980327     C** ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
386200040615     C**                 MOVE      VAR           CMD1
386300040615     C**                 MOVEL     *BLANKS       COMMAN
386400040615     C**                 MOVEA     CMD1(1)       COMMAN
386500040615     C**                 CALL      'QCMDEXC'
386600040615     C**                 PARM                    COMMAN
386700040615     C**                 PARM                    LUNG
386800980327     C*
386900040615     C**                 OPEN      FNANM00T
387000040615     C**                 ENDSR
387100001030     C*-------------------------------------------------------------------
387200001030     C* TRASMISSIONE SCHEDULATA ANOMALIE (ESTERNE NON TRASMESSE)
387300001030     C     SCHANM        BEGSR
387400001102     C* N.B. LEGGO SOLO LE ANOMALIE ESTERNE VISTO CHE PER IL MOMENTO
387500001102     C*      LE AZIONI CHE PREVEDONO L'INVIO SCHEDULATO SONO TUTTE
387600001102     C*      ANOMALIE ESTERNE
387700001102     C                   MOVEL     'E'           KAIE
387800001030     C                   MOVEL     '7C'          COD
387900001030     C     KTAB2         SETLL     TABEL
388000001030     C     KTAB2         READE     TABEL                                  30
388100001030     C     *IN30         DOWEQ     *OFF
388200001030     C                   MOVEL     TBLUNI        DS7C
388300001030     C     §7CINV        IFEQ      'S'
388400001102     C                   MOVEL     TBLKEY        KCAA
388500001030     C     KAN11         SETLL     FNANM11L
388600001030     C     KAN11         READE     FNANM11L                               30
388700001030     C     *IN30         DOWEQ     *OFF
388800001030     C                   EXSR      ELAB
388900001030     C     KAN11         READE     FNANM11L                               30
389000001030     C                   ENDDO
389100001030     C                   ENDIF
389200001030     C     KTAB2         READE     TABEL                                  30
389300001030     C                   ENDDO
389400001030     C                   EXSR      CAEX
389500041020     c
389600041020     c* Leggo le anomalie 145 146 per le quali devo scrivere fnant
389700041020     c* sbr   spostata dal pgm fnlr76r che viene eliminato
389800041020     C                   MOVEL     'I'           KAIE
389900041020     c                   z-add     145           kcaa
390000041020     c                   do        2
390100041020     c     kan11         setll     fnanm11l
390200041020     C     KAN11         READE     FNANM11L
390300041020     c                   dow       not %eof(fnanm11l)
390400041020     c                   if        anmcch<>'AN'
390500041020     c                   EXSR      SCRANT
390600041020     c                   update    fnanm011
390700041020     C                   ENDIF
390800041020     c
390900041020     C     KAN11         READE     FNANM11L
391000041020     c                   enddo
391100041020     c                   z-add     146           kcaa
391200041020     c                   enddo
391300001030     C                   ENDSR
391400031007     C*-------------------------------------------------------------------
391500031007     C* WRIDET - ROUTINE SCRITTURA WRK-FILE WFLAN00F SU STAMPA FORMATO "DET"
391600031007     C*-------------------------------------------------------------------
391700031007     C     WRIDET        BEGSR
391800031007     C*
391900031007     C* Inizializzo il buffer del formato record
392000031007     C                   CLEAR                   WFLAN000
392100031007     C*
392200031031     C                   EVAL      WLANFGS = D34FGS
392300031007     C                   EVAL      WLANPOA = D34PAR
392400031007     C                   EVAL      WLANAAS = ANMAAS
392500031007     C                   EVAL      WLANLNP = ANMLNP
392600031007     C                   EVAL      WLANNRS = ANMNRS
392700031007     C                   EVAL      WLANNSP = ANMNSP
392800031007     C                   EVAL      WLANLNA = ANMLNA
392900031007     C                   MOVE      PRTDSP        WLANDSP
393000031007     C                   EVAL      WLANFLS = ANMFLS
393100031007     C                   EVAL      WLANSCN = ANMSCN
393200031007     C                   MOVE      PRTZNC        WLANZNC
393300031007     C                   EVAL      WLANNPS = ANMNPS
393400031007     C                   EVAL      WLANAIE = DESAIE
393500031007     C                   EVAL      WLANDEA = DESTAN
393600031007     C                   EVAL      WLANCAA = ANMCAA
393700031007     C                   EVAL      WLANDEC = DESCAA
393800031007     C                   EVAL      WLANNDC = ARBNDC
393900031007     C                   MOVE      STADDC        WLANDDC
394000031007     C                   EVAL      WLANPDC = STAPDC
394100031007     C* Se già valorizzato il campo PSTAM compondo la descrizione
394200031007     C                   IF        PSTAM <> *blanks
394300031007     C                   EVAL      WLANCO1 = PSTAM
394400031007     C                   MOVE      WDFLP         WDFLPALFA         3
394500031007     C                   EVAL      %subst(WLANCO1:13:3) = WDFLPALFA
394600031007     C                   MOVE      PRTDET        WPRTDET           4
394700031007     C                   EVAL      WLANCO1 = %trim(WLANCO1) + ' ' +
394800031007     C                                       %subst(WPRTDET:1:2)  + '/' +
394900031007     C                                       %subst(WPRTDET:3:2)
395000031007     C***                EVAL      WLANFLP = WDFLP
395100031007     C***                MOVE      PRTDET        WLANDET
395200031007     C                   ENDIF
395300031007     C*
395400031007     C* Se valorizzato il campo WFGS compondo la descrizione
395500031007     C                   IF        WFGS <> *zeros
395600031007     C                   EVAL      WLANCO2 = CDOVE
395700031007     C                   MOVE      WFGS          WDFLPALFA
395800031007     C                   EVAL      %subst(WLANCO2:13:3) = WDFLPALFA
395900031007     C                   EVAL      %subst(WLANCO2:5:5) = PRTDOV
396000031007     C                   MOVE      PRTDFV        WPRTDET
396100031007     C                   EVAL      WLANCO2 = %trim(WLANCO2) + ' ' +
396200031007     C                                       %subst(WPRTDET:1:2)  + '/' +
396300031007     C                                       %subst(WPRTDET:3:2)
396400031007     C***                EVAL      WLANDOV = PRTDOV
396500031007     C***                EVAL      WLANFGS = WFGS
396600031007     C***                MOVE      PRTDFV        WLANDFV
396700031007     C                   ENDIF
396800031007     C*
396900031007     C* Se valorizzato il numero C.A. compongo la descrizione
397000031007     C                   IF        DCTNCA > *zeros
397100040616     C                   EVAL      WLANCO3 = CCOAN1
397200031007     C                   MOVE      DCTFIL        WDFLPALFA
397300031007     C                   EVAL      %subst(WLANCO3:8:3) = WDFLPALFA
397400031007     C                   MOVE      DCTNCA        WNCAALFA          7
397500031007     C                   EVAL      %subst(WLANCO3:12:7) = WNCAALFA
397600031007     C                   MOVE      STADCA        WDCAALFA          6
397700031007     C                   EVAL      WLANCO3 = %trim(WLANCO3) + ' ' +
397800031007     C                                       %subst(WDCAALFA:1:2)  + '/' +
397900031007     C                                       %subst(WDCAALFA:3:2)  + '/' +
398000031007     C                                       %subst(WDCAALFA:5:2)
398100031007     C***                EVAL      WLANFIL = DCTFIL
398200031007     C***                EVAL      WLANNCA = DCTNCA
398300031007     C***                MOVE      DCTDCA        WLANDCA
398400031007     C                   ENDIF
398500031007     C*
398600031007     C                   EVAL      WLANLID = ANMLID
398700031007     C                   EVAL      WLANDFA = DESFSA
398800031007     C                   MOVE      STADAO        WLANDAA
398900031007     C                   EVAL      WLANCDU = ANMCDU
399000031007     C                   EVAL      WLANNFV = ANMNFV
399100031007     C                   EVAL      WLANDFC = DESFSC
399200031007     C                   MOVE      STADCH        WLANDAC
399300031007     C                   MOVE      CCHPRT        WLANCCH
399400031007     C                   EVAL      WLANDCC = DESCCH
399500031007     C                   EVAL      WLANFLE = ANMFLE
399600031007     C                   EVAL      WLANCHA = DCTDCH
399700031007     C*
399800031007     C* SCarico il buffer sul file
399900031007     C                   WRITE     WFLAN000
400000031007     C*
400100031007     C                   ENDSR
400200031007     C*-------------------------------------------------------------------
400300031007     C* DELWF  - ROUTINE DI CANCELLAZIONE WRK-FILE WFLAN00F X LANCIO CORRENTE
400400031007     C*-------------------------------------------------------------------
400500031007     C     DELWF         BEGSR
400600031007     C*
400700031007     C     KLAN01        SETLL     WFLAN01L
400800031007     C                   IF        %equal(WFLAN01L)
400900031007     C     KLAN01        READE     WFLAN01L
401000031007     C                   DOW       not %eof(WFLAN01L)
401100031007     C                   DELETE    WFLAN01L
401200031007     C     KLAN01        READE     WFLAN01L
401300031007     C                   ENDDO
401400031007     C                   ENDIF
401500031007     C*
401600031007     C                   ENDSR
401700960327     C****************************************************************
401800960327     C     *INZSR        BEGSR
401900960327     C*
402000960327     C     *ENTRY        PLIST
402100960327     C                   PARM                    KPJBA
402200960327     C                   MOVEL     KPJBU         DSLR34
402300960327     C                   Z-ADD     1             CODUT
402400960327     C                   CALL      'X§PARUT'
402500020905     C                   PARM                    UT§DSE0F
402600960327     C                   MOVEL     RAGUT         RSUT             20
402700960327     C                   MOVEL     REC80         CNCR80
402800031007     C*
402900031007     C* Verifico subito se per il lancio richiesto è prevista anche la creazione del wrk-file
403000031009     C* Lista Anomalie (WFLAN00F)
403100031009     C                   IF        DSLR34 <> *blanks                            * NO x trasmisisone
403200031009     C***                IF        D34IE = 'D'                                  * solo anomalie IDD
403300031007     C* Imposto a NO l'indicatore d condizionamento creazoine wrk-file
403400031007     C                   SETOFF                                       55
403500031007     C* Verifico se x il cliente richiesto nel lancio è prevista la creazine del wrk-file
403600031007     C                   EVAL      WD34POS = D34POS
403700031007     C                   EVAL      WD34NRS = D34NRS
403800031007     C                   EVAL      TBECOD = 'LAN'
403900031007     C                   EVAL      TBEKE1 = DSKEYLAN
404000031007     C     KTBE01P       CHAIN     TNTBE01L
404100031007     C                   IF        %found(TNTBE01L)
404200031007     C* Se trovo il cliente in tabella, verifico i flag x le abilitazioni elaborazione
404300031007     C* anomalie in partenza e/o in arrivo
404400031007     C                   EVAL      DLAN = TBEUNI
404500031007     C                   IF        (§LANPAR = 'S' AND D34PAR = 'P') OR
404600031007     C                             (§LANARR = 'S' AND D34PAR = 'A')
404700031007     C                   SETON                                        55
404800031031     C                   OPEN      WFLAN01L
404900031007     C                   EXSR      DELWF
405000031007     C                   ENDIF
405100031007     C                   ENDIF
405200031007     C***                ENDIF
405300031009     C                   ENDIF
405400960327     C*-----------------------------------------------------
405500960327     C     KTAB          KLIST
405600960327     C                   KFLD                    CODUT
405700960327     C                   KFLD                    COD               2
405800960327     C                   KFLD                    KEY               8
405900031007     C     KTBE01P       KLIST                                                  * Parziale
406000031007     C                   KFLD                    TBECOD
406100031007     C                   KFLD                    TBEKE1
406200031007     C     KLAN01        KLIST                                                  * Completa
406300031031     C                   KFLD                    D34FGS
406400031007     C                   KFLD                    D34POS
406500031007     C                   KFLD                    D34NRS
406600960327     C     KTAB2         KLIST
406700960327     C                   KFLD                    CODUT
406800960327     C                   KFLD                    COD               2
406900960927     C     KFVA          KLIST
407000960927     C                   KFLD                    BRVFGS
407100000201     C                   KFLD                    WNFV
407200960510     C     KCLN          KLIST
407300960510     C                   KFLD                    KTFP
407400960510     C                   KFLD                    KLNA
407500990512     C                   KFLD                    WAA
407600990512     C                   KFLD                    WMM
407700960417     C     KANM          KLIST
407800990512     C                   KFLD                    ANMAAS
407900960417     C                   KFLD                    ANMLNP
408000960417     C                   KFLD                    ANMNRS
408100960417     C                   KFLD                    ANMNSP
408200020614     C     KBTP          KLIST
408300020614     C                   KFLD                    SIMFEL
408400020614     C                   KFLD                    ANMAAS
408500020614     C                   KFLD                    ANMLNP
408600020614     C                   KFLD                    ANMNRS
408700020614     C                   KFLD                    ANMNSP
408800960405     C     KANM1         KLIST
408900960405     C                   KFLD                    KAAS2
409000960405     C                   KFLD                    KLNP2
409100960405     C                   KFLD                    KNRS2
409200960405     C                   KFLD                    KNSP2
409300960404     C                   KFLD                    KCAA
409400960509     C     KANM2         KLIST
409500960509     C                   KFLD                    ANMFLS
409600960509     C                   KFLD                    ANMLNA
409700960509     C                   KFLD                    ANMNRS
409800960509     C                   KFLD                    ANMSCN
409900960509     C                   KFLD                    KCAA
410000021021     C     KANM20        KLIST
410100021021     C                   KFLD                    KFIL
410200021021     C                   KFLD                    KCAA
410300970529     C     KANM3         KLIST
410400970529     C                   KFLD                    KAIE
410500970529     C                   KFLD                    KCDU
410600970529     C                   KFLD                    ANMLNP
410700970529     C     KANM3A        KLIST
410800970529     C                   KFLD                    KAIE
410900970529     C                   KFLD                    KCDU
411000970529     C     KANM3B        KLIST
411100970529     C                   KFLD                    KAIE
411200970529     C                   KFLD                    KCDU
411300970529     C                   KFLD                    KLNP
411400970529     C                   KFLD                    ANMFLS
411500970529     C     KANM3D        KLIST
411600970529     C                   KFLD                    KAIE
411700970529     C                   KFLD                    KCDU
411800970529     C                   KFLD                    KLNP
411900960422     C     KANM4         KLIST
412000960422     C                   KFLD                    KCAA
412100960422     C                   KFLD                    KDAO
412200960423     C                   KFLD                    KLNA
412300960404     C     KANM5         KLIST
412400960404     C                   KFLD                    KCAA
412500960404     C                   KFLD                    KFIL
412600960412     C     KANM51        KLIST
412700960412     C                   KFLD                    KCAA
412800960412     C                   KFLD                    KFIL
412900960412     C                   KFLD                    KLNP
413000960412     C     KANM52        KLIST
413100960412     C                   KFLD                    KCAA
413200960412     C                   KFLD                    KFIL
413300960412     C                   KFLD                    KLNP
413400960412     C                   KFLD                    KFLS
413500960327     C     KANM7         KLIST
413600960327     C                   KFLD                    KAIE
413700960327     C                   KFLD                    KFIL
413800960327     C                   KFLD                    KLNA
413900960327     C     KANM71        KLIST
414000960327     C                   KFLD                    KAIE
414100960327     C                   KFLD                    KFIL
414200960327     C     KANM72        KLIST
414300960327     C                   KFLD                    KAIE
414400960327     C                   KFLD                    KFIL
414500960327     C                   KFLD                    KLNA
414600960327     C                   KFLD                    KDCH
414700960327     C     KANM8         KLIST
414800960327     C                   KFLD                    KAIE
414900960327     C                   KFLD                    KFIL
415000960327     C                   KFLD                    KLNP
415100960327     C                   KFLD                    KLNA
415200960327     C     KANM82        KLIST
415300960327     C                   KFLD                    KAIE
415400960327     C                   KFLD                    KFIL
415500960327     C                   KFLD                    KLNP
415600960327     C                   KFLD                    KLNA
415700960327     C                   KFLD                    KDCH
415800960327     C     KANM9         KLIST
415900960327     C                   KFLD                    KAIE
416000960327     C                   KFLD                    KFIL
416100960327     C                   KFLD                    KLNP
416200960327     C     KANM93        KLIST
416300960327     C                   KFLD                    KAIE
416400960327     C                   KFLD                    KFIL
416500960327     C                   KFLD                    KLNP
416600960327     C                   KFLD                    KFLS
416700960327     C                   KFLD                    KDCH
416800960327     C     KANM94        KLIST
416900960327     C                   KFLD                    KAIE
417000960327     C                   KFLD                    KFIL
417100960327     C                   KFLD                    KLNP
417200960327     C                   KFLD                    KFLS
417300001102     C     KAN11         KLIST
417400001102     C                   KFLD                    KAIE
417500001102     C                   KFLD                    KCAA
417600960327     C     KART          KLIST
417700960327     C                   KFLD                    ANMFLS
417800960327     C                   KFLD                    ANMLNA
417900960327     C                   KFLD                    ANMNRS
418000960327     C                   KFLD                    ANMSCN
418100960327     C     KARB          KLIST
418200960327     C                   KFLD                    ARTAAS
418300960327     C                   KFLD                    ARTLNP
418400960327     C                   KFLD                    ARTNRS
418500960327     C                   KFLD                    ARTNSP
418600960412     C     KBLP          KLIST
418700090312     C                   KFLD                    arTAAS
418800090312     C                   KFLD                    arTLNP
418900090312     C                   KFLD                    arTNRS
419000090312     C                   KFLD                    arTNSP
419100960508     C     KFVV          KLIST
419200960508     C                   KFLD                    BRVNPG
419300000201     C                   KFLD                    WNFV
419400960508     C                   KFLD                    BRVFGS
419500960704     C     KBRV          KLIST
419600960704     C                   KFLD                    KNPG
419700960704     C                   KFLD                    ANMFLS
419800960704     C                   KFLD                    ANMLNA
419900960704     C                   KFLD                    ANMNRS
420000960704     C                   KFLD                    ANMSCN
420100960927     C     KBRV3         KLIST
420200960927     C                   KFLD                    KNPG
420300960927     C                   KFLD                    ANMFLS
420400960927     C                   KFLD                    ANMLNA
420500960927     C                   KFLD                    ANMNRS
420600960927     C                   KFLD                    ANMSCN
420700960927     C                   KFLD                    ARTFLP
420800970604     C     KBRV3A        KLIST
420900970604     C                   KFLD                    KNPG
421000970604     C                   KFLD                    ANMFLS
421100970604     C                   KFLD                    ANMLNA
421200970604     C                   KFLD                    ANMNRS
421300970604     C                   KFLD                    ANMSCN
421400970604     C                   KFLD                    SIMFEL
421500970306     C     KA71          KLIST
421600970306     C                   KFLD                    ANMFLE
421700990512     C                   KFLD                    ANMDAO
421800970305     C                   KFLD                    ANMCAA
421900970306     C                   KFLD                    ANMLNP
422000970306     C     KA55          KLIST
422100970306     C                   KFLD                    ANMFLE
422200990512     C                   KFLD                    ANMDAO
422300970306     C                   KFLD                    ANMCAA
422400040504     C                   KFLD                    wlnp
422500040405     C     KA200         KLIST
422600040405     C                   KFLD                    ANMFLE
422700040405     C                   KFLD                    wdta200
422800040405     C                   KFLD                    ANMCAA
422900040504     C                   KFLD                    wlnp
423000041020     C     KANT          KLIST
423100041020     C                   KFLD                    KFOA
423200041020     C                   KFLD                    ANMDAO
423300041020     C                   KFLD                    ANMCAA                                     NE
423400041020     C                   KFLD                    KANLNP                                     NE
423500041020     C                   KFLD                    ANMLNA                                     NE
423600990122     C     KDCT          KLIST
423700990122     C                   KFLD                    DCDAAC
423800990122     C                   KFLD                    DCDFIL
423900990122     C                   KFLD                    DCDNCA
424000020807     C     K02FGV01      klist
424100090312     C                   kfld                    knfv
424200020926     C                   kfld                    SIMfel
424300021203
424400021203     C     Kfapd         klist
424500021203     C                   kfld                    kpdtip
424600021203     C                   kfld                    kpdpdr
424700040615
424800040615     C     Kcae          klist
424900040615     C                   kfld                    kcaeepa
425000040615     C                   kfld                    kcaetfp
425100040615     C                   kfld                    kcaetfe
425200040615     C                   kfld                    kcaedde
425300040615     C     Kcae2         klist
425400040615     C                   kfld                    kcaeepa
425500040615     C                   kfld                    kcaetfp
425600040615     C                   kfld                    kcaetfe
425700040616     C     Kcae1         klist
425800040616     C                   kfld                    kcaeepa
425900040616     C                   kfld                    kcaetfp
426000040616     C                   kfld                    kcaetfa
426100040616     C                   kfld                    kcaedde
426200040616     C     Kcae3         klist
426300040616     C                   kfld                    kcaeepa
426400040616     C                   kfld                    kcaetfp
426500040616     C                   kfld                    kcaetfa
431000960417     C*
431100960402     C* UDATE IN GGMMAAAA
431200960417     C                   TIME                    W0140            14 0
431300960402     C                   MOVE      W0140         WDTGIO            8 0
431400960422     C*
431500960422     C* PRENDO DATA IN AAMMGG
431600960422     C                   MOVEL     WDTGIO        G02DAT
431700960422     C                   MOVEL     0             G02ERR
431800971126     C                   CALL      'XSRDA8'
431900960422     C                   PARM                    WLBDAT
432000971126     C                   MOVE      G02INV        AM8GIO            8 0
432100980327     C* SE RICHIESTO TRASFERIMENTO APRO I MEMBRI
432200040615     C**   D34TRA        IFEQ      'S'
432300040615     C**                 EXSR      CAEX2
432400040615     C**                 END
432500001102     C* SE AZIONE SCHEDULATA IMPOSTO D34GES E D34FGS
432600001102     C     PARSCH        IFEQ      'V'
432700001102     C                   CLEAR                   DSLR34
432800001102     C                   MOVEL     'V'           D34GES
432900001102     C                   Z-ADD     SIMFEL        D34FGS
433000001102     C                   ENDIF
433100960327     C                   ENDSR
433200041020     C**************************************************************************
433300041020     C* SCRITTURA DEL FILE FNANT00F
433400041020     C**************************************************************************
433500041020     C     SCRANT        BEGSR
433600041020     C**
433700041020     C* PER L'ANOMALIA 145  FOA = ANMFLE
433800041020     C* PER L'ANOMALIA 146  FOA  = TERMINAL DI ARRIVO
433900041020    2C     ANMCAA        IFEQ      145
434000041020     C                   Z-ADD     ANMFLE        KFOA
434100041020   X2C                   ELSE
434200041020     C*
434300041020     C                   CLEAR                   KFOA
434400041020     C* PRENDO FLS SE LNP =0
434500041020     C*
434600041020     C                   CLEAR                   DSLV55
434700041020     C                   MOVEL     'A'           D55TPT
434800041020     C                   Z-ADD     ANMLNA        D55LIN
434900041020     C                   Z-ADD     ANMDAO        D55DRF
435000041020    3C     ANMLNP        IFGT      0
435100041020     C                   Z-ADD     ANMLNP        d55lnp
435200041020     C                   ELSE
435300041020     C                   Z-ADD     ANMFLS        d55lnp
435400041020    3C                   ENDIF
435500041020     C                   CALL      'FNLV55R'
435600041020     C                   PARM                    DSLV55
435700041020     C                   Z-ADD     D55TFA        KFOA
435800041020    2C                   ENDIF
435900041020     C*
436000041020     C     KANT          CHAIN     FNANT02L                           39
436100041020     C*
436200041020    2C     *IN39         IFEQ      *ON
436300041020     C                   CLEAR                   FNANT000
436400041020     C                   MOVEL     KFOA          ANTFOA
436500041020     C                   MOVEL     ANMLNA        ANTLNA
436600041020     C                   MOVEL     ANMDAO        ANTDAO
436700041020     C                   MOVEL     ANMCAA        ANTCAA
436800041020     C                   Z-ADD     1             ANTTOT
436900041020     c                   z-add     *date         antdt1
437000041020     C                   WRITE     FNANT000
437100041020   X2C                   ELSE
437200041020     C                   ADD       1             ANTTOT
437300041020     C                   CLEAR                   ANTFT1
437400041020     C                   CLEAR                   ANTFT2
437500041020     c                   z-add     *date         antdt1
437600041020     C                   UPDATE    FNANT000
437700041020    2C                   ENDIF
437800041020     C*
437900041020     C                   MOVEL     'T'           ANMFT1
438000041020     C                   ENDSR
438100911021     C*
438200960422     O* TESTATA IDD
438300911021     OPRTF198   E            TES              02
438400960604     O                       STA(1)              66
438500960604     O              N13      STA(2)             132
438600040614     o               13      STA(19)            132
438700921015     O                       STA(3)             198
438800900626     O                       RSUT                20
438900040615     O                       prtpo               46
439000040615     O                       D34FGS              50
439100960926     O                       PRTFGS              71
439200960402     O                       WDTGIO             168 '  .  .    '
439300921029     O                       PAGE1         Z    198
439400921015     O          E            TES         1
439500040615     O               70      STA(34)            132
439600960402     O                       UTIME              167 '  :  :  '
439700960402     O                  01                      190 '-C O N F E R M A-'
439800040615     O          E            TES         1
439900960402     O                       STA(18)             66
440000960402     O                       STA(20)            132
440100960402     O                       STA(21)            198
440200960506     O                       DESIE               61
440300960402     O                       D34DLG             106 '  /  /    '
440400960402     O                       D34ALG             123 '  /  /    '
440500960403     O                       D34DCG             172 '  /  /    '
440600960402     O                       D34ACG             189 '  /  /    '
440700030717     C*
440800040615     O          E            TES2        1
440900960403     O                  15   STA(16)             66
441000960403     O                 N15   STA(17)            132
441100960403     O                 N18   STA(15)            198
441200960403     O                  15   DESFV               29
441300990513     O                  15   D34NFV        Z     44
441400990513     O                  15   STADFV              61 '  /  /    '
441500980403     O                 N15   TESFSA             103
441600040611     O                 N18   D34LID             198
441700040614     O                                          120 'LINEA ARRIVO :'
441800980403     O               06      D34LNA             124
441900040614     O               06      PRTLNA             141
442000040614     O                                          154 'TIPO MERCE :'
442100040614     O                       PRTVAL             163
442200030717     O*
442300040615     O          E    23      TES2        1
442400030717     O                       STA(33)            132
442500030717     O                       D34POS              87
442600030717     O                       D34NRS             110
442700960403     O*
442800040615     O          E   N14 19   TES2        1
442900960426     O                       STA(32)             66
443000960426     O                       D34CCH              29
443100960426     O                                         +  0 '-'
443200960426     O                       TESCCH            +  0
443300040615     O          E    14      TES2        1
443400960426     O                  19   STA(32)             66
443500960426     O                       STARIG             132
443600960426     O                  19   D34CCH              29
443700960426     O                                         +  0 '-'
443800960426     O                  19   TESCCH            +  0
443900960418     O                       ANM(1)              94
444000960403     O                       ANM(2)            +  3
444100960403     O                       ANM(3)            +  3
444200960403     O                       ANM(4)            +  3
444300960403     O                       ANM(5)            +  3
444400960403     O                       ANM(6)            +  3
444500960403     O                       ANM(7)            +  3
444600960403     O                       ANM(8)            +  3
444700960403     O                       ANM(9)            +  3
444800960403     O                       ANM(10)           +  3
444900040615     o*
445000040615     O          E    80      TES3        1
445100040615     O                       STA(35)             66
445200040615     o                       stpo(1)             20
445300040615     o                       stpo(2)           +  1
445400040615     o                       stpo(3)           +  1
445500040615     o                       stpo(4)           +  1
445600040615     o                       stpo(5)           +  1
445700040615     o                       stpo(6)           +  1
445800040615     o                       stpo(7)           +  1
445900040615     o                       stpo(8)           +  1
446000040615     o                       stpo(9)           +  1
446100040615     o                       stpo(10)          +  1
446200040615     o                       stpo(11)          +  1
446300040615     o                       stpo(12)          +  1
446400040615     o                       stpo(13)          +  1
446500040615     o                       stpo(14)          +  1
446600040615     o                       stpo(15)          +  1
446700040615     o                       stpo(16)          +  1
446800040615     o                       stpo(17)          +  1
446900040615     o                       stpo(18)          +  1
447000040615     o                       stpo(19)          +  1
447100040615     o                       stpo(20)          +  1
447200040615     o                       stpo(21)          +  1
447300040615     o                       stpo(22)          +  1
447400040615     o                       stpo(23)          +  1
447500040615     o                       stpo(24)          +  1
447600040615     o                       stpo(25)          +  1
447700040615     o                       stpo(26)          +  1
447800040615     o                       stpo(27)          +  1
447900040615     o                       stpo(28)          +  1
448000040615     o                       stpo(29)          +  1
448100040615     o                       stpo(30)          +  1
448200040615     O          E            TES3        2
448300911021     O                       STA(4)              66
448400911021     O                       STA(7)             132
448500911021     O                       STA(10)            198
448600040616     O**               N04 17                   198 '     '
448700040615     O          E            TES3        1
448800911018     O                       STA(5)              66
448900911018     O                       STA(8)             132
449000911021     O                       STA(11)            198
449100040616     O**               N04 17                   198 '     '
449200960422     O*
449300960423     O          E            TESIDR      3  1
449400960422     O                       STA(25)             66
449500960422     O                       STA(26)            132
449600960513     O                       STA(13)            198
449700990513     O                       STADCI              82 '  /  /    '
449800990513     O                       STADCM             166 '  /  /    '
449900960423     O*
450000960423     O          E            TESIDE      3  1
450100960423     O                       STA(30)             66
450200960423     O                       STA(31)            132
450300960513     O                       STA(13)            198
450400990513     O                       STADCI              84 '  /  /    '
450500990513     O                       STADCM             166 '  /  /    '
450600911015     O*
450700911015     O          E            DET         1
450800990512     O                  02   PRTAAS        Z
450900921015     O                  02   ANMLNP        Z   +  1
451000921015     O                  02   ANMNRS        Z   +  1
451100921015     O                  02   ANMNSP        Z   +  1
451200921015     O                  02   ANMLNA            +  1
451300960417     O                  02   PRTDSP            +  1 '  /  '
451400960417     O*
451500990121     O                  03   ANMFLS            +  3
451600921015     O                  03   ANMLNA            +  1
451700921015     O                  03   ANMNRS        Z   +  1
451800921015     O                  03   ANMSCN        Z   +  1
451900990121     O                  22   ANMSCN        Z     48
452000040609     O                  03   PRTZNC            +  1
452100040609     O                  03   anmft4            +  0
452200990121     O                  03   ANMNPS        Z   +  1
452300040616     O                       DESTAN            +  1
452400990121     O                       ANMCAA            +  1
452500921015     O                                         +  0 '-'
452600990122     O              N40   N09DESCAA            +  0
452700990122     O                     09DESCA1              95
452800950320     O                     09ARBNDC        Z   +  0
452900950320     O                     09STADDC            +  1 '  /  '
453000950320     O                     09STAPDC            +  1
453100040616     O                     20PSTAM              110
453200040616     O                     20WDFLP              107
453300040616     O                     20PRTDET             118 '  /  '
453400040616     O                     21CDOVE              110
453500040616     O                     21PRTDOV             101
453600040616     O                     21WFGS               107
453700040616     O                     21PRTDFV             118 '  /  '
453800990121     O* NUMERO C.A. SU CUI E' IL COLLO
453900040616     O                     40CCOAN              109
454000040616     O                     40DESCA1              91
454100040616     O                     40DCTFIL        Z     98
454200040616     O                     40DCTNCA        Z    106
454300040616     O                     40STADCA             118 '  /  /  '
454400040616     O                  42 40                   124 'CHIUSA'
454500040616     O                       STALID             125
454600040616     O                       STADAc            +  1 '  /  '
454700040616     O                       STAorc            +  1 '  :  '
454800040616     O                       DESFSA            +  1
454900950320     O                       STADAO            +  1 '  /  '
455000040616     O                       ANMCDU        Z   +  1
455100040616     O                       ANMNFV        Z   +  1
455200990121     O                       DESFSC            +  1
455300950320     O                       STADCH            +  1 '  /  '
455400970522     O                       CCHPRT            +  1
455500960423     O                                         +  0 '-'
455600960423     O                       DESCCH            +  0
455700040616     O                       ANMFLE            +  1
455800050503     O*  STAMPA NUMERO C.A. e/o particolarità consegna CP
455900050503     O          E            DET1        1
456000050503     O               43      decodiCP            96
456100050503     O               41      CCOAN              109
456200050503     O               41      DCTFIL        Z     98
456300050503     O               41      DCTNCA        Z    106
456400050503     O               41      STADCA             118 '  /  /  '
456500050503     O               41                         128 'CHIUSA IL'
456600050503     O               41      STACHA             134 '  /  '
456700050504     O*  STAMPA NUMERO C.A. e/o particolarità consegna CP
456800050504     O          E            DET1        0  0
456900050504     O               43      decodiCP            96
457000051221     O*  STAMPA dati mittente e destinatario
457100051221     O          E            DET2        1
457200051222     o*                                        + 30 'Mittente:'
457300051222     o                                         + 69 'Mittente:'
457400051221     o                       blprsm            +  0
457500051222     o                                         +  2 'Rif.Mitt.Num:'
457600051222     o                       prtrmn            +  0
457700051222     o                                         +  2 'Destinatario:'
457800051221     o                       blprsd            +  0
457900960422     O*
458000960422     O*  STAMPA RIGA: DETTAGLIO FILIALE CHE NON HANNO FATTO L'IDD
458100960422     O          E            DETIDD      1
458200960423     O                       FID(1)        Z
458300960422     O                                         +  0 '-'
458400960422     O                       DFI(1)            +  0
458500050831     O                       FID(2)        Z   +  2
458600960422     O                                         +  0 '-'
458700960422     O                       DFI(2)            +  0
458800050831     O                       FID(3)        Z   +  2
458900960422     O                                         +  0 '-'
459000960422     O                       DFI(3)            +  0
459100050831     O                       FID(4)        Z   +  2
459200960422     O                                         +  0 '-'
459300960422     O                       DFI(4)            +  0
459400050831     O                       FID(5)        Z   +  2
459500960422     O                                         +  0 '-'
459600960422     O                       DFI(5)            +  0
459700050831     O                       FID(6)        Z   +  2
459800960422     O                                         +  0 '-'
459900960422     O                       DFI(6)            +  0
460000050831     O                       FID(7)        Z   +  2
460100960422     O                                         +  0 '-'
460200960422     O                       DFI(7)            +  0
460300960422     O*
460400960422     O*  STAMPA RIGA: NESSUNA ANOMALIA RISCONTRATA
460500960422     O          E            NOANOM      3  3
460600960422     O                       STA(24)            132
460700960423     O*  STAMPA RIGA: TUTTE LE FILIALI HANNO FATTO L'IDD
460800960423     O          E            SIIDD       2
460900960423     O                       STA(27)             66
461000960423     O                       STA(28)            132
461100960423     O                       STA(29)            198
461200960422     O*  STAMPA IDD3
461300911025     OPRTF198X  E            TESX             02
461400960604     O                       STA(1)              66
461500040618     O                       STA(14)            132
461600040618     O                       STA(3)             198
461700040618     O                       prtpo               46
461800960926     O                       D34FGS              50
461900960926     O                       PRTFGS              71
462000960604     O                       §7ALCI             121
462100911025     O                       RSUT                20
462200960402     O                       WDTGIO             168 '  .  .    '
462300921015     O                       PAGE2         Z    198
462400960403     O          E            TESX        1
462500960402     O                       UTIME              167 '  :  :  '
462600960402     O                  01                      190 '-C O N F E R M A-'
462700921015     O          E            TESX        2
462800911025     O                       STA(4)              66
462900911025     O                       STA(7)             132
463000911025     O                       STA(10)            198
463100921016     O                                          198 '     '
463200911025     O          E            TESX        1
463300911025     O                       STA(5)              66
463400911025     O                       STA(8)             132
463500911025     O                       STA(11)            198
463600921016     O                                          198 '     '
463700911025     O*
463800911025     O          E            DETX        1
463900990512     O                  02   PRTAAS        Z
464000921015     O                  02   ANMLNP        Z   +  1
464100921015     O                  02   ANMNRS        Z   +  1
464200921015     O                  02   ANMNSP        Z   +  1
464300921015     O                  02   ANMLNA            +  1
464400960417     O                  02   PRTDSP            +  1 '  /  '
464500960417     O*
464600990121     O                  03   ANMFLS            +  3
464700921015     O                  03   ANMLNA            +  1
464800921015     O                  03   ANMNRS        Z   +  1
464900921015     O                  03   ANMSCN        Z   +  1
465000960509     O                  03   PRTZNC            +  2
465100990121     O                  03   ANMNPS        Z   +  1
465200990121     O                       DESTAN            +  1
465300990121     O                       ANMCAA            +  1
465400921015     O                                         +  0 '-'
465500990122     O                    N40DESCAA            +  0
465600040616     O                     20PSTAM              110
465700040616     O                     20WDFLP              107
465800040616     O                     20PRTDET             118 '  /  '
465900040616     O                     21CDOVE              110
466000040616     O                     21PRTDOV             101
466100040616     O                     21WFGS               107
466200040616     O                     21PRTDFV             118 '  /  '
466300990122     O* NUMERO C.A. SU CUI E' IL COLLO
466400040616     O                     40CCOAN              109
466500040616     O                     40DESCA1              91
466600040616     O                     40DCTFIL        Z     98
466700040616     O                     40DCTNCA        Z    106
466800040616     O                     40STADCA             118 '  /  /  '
466900040616     O                  42 40                   124 'CHIUSA'
467000040616     O                       ANMLID             125
467100040616     O                       STADAc            +  1 '  /  '
467200040616     O                       STAorc            +  1 '  :  '
467300040616     O                       DESFSA            +  1
467400040616     O                       STADAO            +  1 '  /  '
467500040616     O                       ANMCDU        Z   +  1
467600040616     O                       ANMNFV        Z   +  1
467700990121     O                       DESFSC            +  1
467800040616     O                       STADCH            +  1 '  /  '
467900960423     O                       ANMCCH            +  1
468000960423     O                                         +  0 '-'
468100960423     O                       DESCCH            +  0
468200040616     O                       ANMFLE            +  1
468300990122     O*  STAMPA NUMERO C.A.
468400050503     O          E            DETX1       1
468500050503     O               43      decodiCP            96
468600050503     O               41      CCOAN              109
468700050503     O               41      DCTFIL        Z     98
468800050503     O               41      DCTNCA        Z    106
468900050503     O               41      STADCA             118 '  /  /  '
469000050503     O               41                         128 'CHIUSA IL'
469100050503     O               41      STACHA             134 '  /  '
469200050504     O          E            DETX1       0  0
469300050504     O               43      decodiCP            96
469400990122     O*
469500990512**
469600040615XXXXXXXXXXXXXXXXXXXX        xxxxxxxxxxxxxxxxxx XXX-XXXXXXXXXXXXXXX
469700960926XXXXX           **   L I S T A     A N O M A L I E   **             2
469800960417    FNLR35R               XX/XX/XXXX                     PAG. XXXX  3
469900040616    S P E D I Z I O N E          S E G N A C O L L O      A      N
470000040616AN LNP SER NUMERO LNA  DATA   LNP LNA SER NUMERO ZON PIS T I P O
470100040616XX XXX XX XXXXXXX XXX XX/XX   XXX XXX XX XXXXXXX xxV XX xxxxxxxxxx
470200040616      O      M      A      L      I      A               LIV CREAZ
470300040616    C  O  D  I  C  E                                     IDD DATA
470400040616xx XXX-bolla in consegna:XXXXXXX XX/XX XXXXXXXXXXXX  X    X XX/xx
470500040616IONE  A  P  E  R  T  U  R  A    C  H  I  U  S  U  R  A         P.O  10
470600040616 ORA  FASE     DATA   FOGLIO    FASE      DATA  CAUSALE        EMI  11
470700040616xx:xx xxxxxxxx XX/XX XXX XXXXXX xxxxxxxx XX/XX XX-XXXXXXXXXXXX XXX 12
470800990513RNO PRECEDENTE, IN DATA XX/XX/XXXX) :
470900960604        **   E L A B O R A Z I O N E      " I. D. D.  X "   **     14
471000040614                                  ANOMALIE IDD DAL LIVELLO IDD : X 15
471100990513ANOMALIE DEL FOGLIO  XXXXXXXX   N.RO  XXXXXX   DEL XX/XX/XXXX      16
471200040614FASE APERTURA ANOMALIE : XXXXXXXXXXXX                              17
471300960403ANOMALIE :  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX         18
471400960604        **   E L A B O R A Z I O N E      "I. D. D."   **          19
471500960402DATA APERTURA ANOMALIA : DAL  XX/XX/XXXX   AL  XX/XX/XXXX          20
471600960402DATA CHIUSURA ANOMALIA : DAL  XX/XX/XXXX   AL  XX/XX/XXXX          21
471700960506CODICI       RICHIESTI : XXX                                       22
471800960506CODICI  NON  RICHIESTI : XXX                                       23
471900960422 *** NESSUNA  ANOMALIA  RISCONTRATA  NEI  PARAMETRI  RICHIESTI *** 24
472000960926ELENCO PUNTI OPER.DI CUI MANCA LA TRASMISSIONE ANOMALIE I.D.D. IN  25
472100990513DATA  XX/XX/XXXX     (PER I PUNTI OPER. CHE ESEGUONO L'IDD DEL GIO 26
472200960926                                                    T U T T I    I 27
472300030717    P U N T I    O P E R A T I V I    H A N N O    C O N F E R M A 28
472400030717 T O    L'  I . D . D .                                            29
472500030717ELENCO PUNTI OPER.DELL'AREA DI CUI MANCA LA CONFERMA DELL'I.D.D. I 30
472600030717N DATA  XX/XX/XXXX   (PER I PUNTI OPER. CHE ESEGUONO L'IDD DEL GIO 31
472700960426CAUSALE CHIUSURA ANOMALIA: XX-XXXXXXXXXXXXXXXXXXXXXXXXX            32
472800030717P.O. SEGNACOLLO : XXX      NUMERO SERIE : XX                       33
472900040615                **  COLLI   DI   VALORE   MANCANTI   **            34
473000040615Punti Operativi: xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx   35
473100921016**
473200960506A R R I V O
473300960506P A R T E N Z A
473400960506  N O N    I.D.D.
473500960506  I . D . D.
473600960506  I N T E R N E
473700960506  T U T T E
473800960506     SOLO APERTE
473900960506     SOLO CHIUSE
474000021008**         CMD1
474100060619OVRDBF FILE(FNANM00T)                MBR(M000000)
474200980327**         CMD3
474300060619ADDPFM FILE(FNANM00T)                MBR(M000000)
474400980327**         CMD5
474500060619CHKOBJ OBJ(FNANM00T) OBJTYPE(*FILE)  MBR(M000000)
474600060619**         OVRSED
474700060710OVRDBF FILE(FNANMSED) TOFILE(GAITRAGRU/FNANM00F )

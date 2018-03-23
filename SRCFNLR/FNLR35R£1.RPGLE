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
001600040811     F**""**FNDCT02L  IF   E           K DISK
001700040811     F**""**                                     RENAME(FNDCT000:FNDCT002)
001800960508     FFNFVV01L  IF   E           K DISK
001900960930     FFNFGV01L  IF   E           K DISK
002000000503     FFNFGW01L  IF   E           K DISK
002100960930     FFNFVA01L  IF   E           K DISK
002200070111     FFNBRV07L  IF   E           K DISK
002300140618     FFNBRV05L  IF   E           K DISK
002400140618     F                                     RENAME(FNBRV000:BRV005)
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
005900950321     FFNART27L  IF   E           K DISK
006000950320     FFNARB01L  IF   E           K DISK
006100960403     FFNBLT27L  IF   E           K DISK
006200960412     FFNBLP01L  IF   E           K DISK
006300040609     F**BTP11L  IF   E           K DISK
006400021203     F*FNAPD01L  IF   E           K DISK
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
034000151125     C* 45    - stampa se bolla chiusa con consegna anomala 7
034100040615     C* 70    - COLLI DI VALORE MANCANTI
034200040615     C* 80    - campo d34spo pieno, passati p.o. da stampare
034300040615     C* 95/95 - per allocazione membri
034400040615     C* 97    - ANOMALIA IDD
034500960404     C* 98/99 - PER I FILE DI TRASMISSIONE
034600900828     C*-----------------------------------------------------
034700911015     C                   EXSR      IMPO
034800960416     C*
034900001030     C                   SELECT
035000001030     C* SE SOLO INVIO RICHIAMO ROUTINE DI LETTURA
035100001030     C* DELLE ANOMALIE DA TRASMETTERE CON AZIONE SCHEDULATA
035200960416     C* ELABORO SOLO SE LA DATA ULTIMO IDD E' MINORE DELLA DATA ELABORA
035300001030     C     D34GES        WHENEQ    'V'
035400001030     C                   EXSR      SCHANM
035500001030     C*
035600001030    1C     *IN01         WHENEQ    *OFF
035700990512     C     §7ADID        ORLT      D34ALA
035800000317     C*
035900960416     C*
036000960405     C                   MOVEL     'S'           D34RIL
036100960404     C     RILEG         TAG
036200960404     C                   CLEAR                   D34END
036300960327     C**
036400960327     C* RILEGGO SE MI E' STATO CHIESTO DI RICARICARE
036500960327     C**
036600000317    3C     D34RIL        DOWEQ     'S'
036700960327     C                   CLEAR                   D34RIL
036800960402     C                   CLEAR                   WUSCIT
036900960327     C**
037000960326     C* LEGGO PRIMA LE ANOMALIE PARTENZA
037100960326     C**
037200000317    4C     D34PAR        IFEQ      'P'
037300960412     C     D34PAR        OREQ      ' '
037400980327     C                   MOVEL     'P'           WANOM             1
037500970829     C* SONO LE ANOMALIE CHE USANO SOLO FLE PER ESCUDERLE DAL GIRO
037600970829     C*  DELLA LETTURA PER LNP
037700970829     C                   MOVE      FLM           FLE
037800960412     C                   EXSR      PARTEN
037900970529     C*
038000970529     C* POI ELABORO LE ANOMALIE RICEVUTE PER LE QUALI SONO FILIALE DI
038100970529     C* TRANSITO O DISGUIDO (SOLO SE SONO SIMFEL)
038200970529     C* Potenzialmente questa elabor. sarebbe da fare anche per le anom
038300970529     C* arrivo ma per ora non serve perchè in arrivo riparte sempre
038400970529     C* tutto in automatico
038500980401     C* Solo se le anomalie richieste sono anche idd
038600000317    5C     D34IE         IFEQ      ' '
038700980401     C     D34IE         OREQ      'D'
038800000317     C     D34FGS        IFEQ      SIMFEL
038900970529     C                   EXSR      USACDU
039000000317     C                   END
039100000317    5C                   END
039200970829     C**
039300970829     C* POI  LEGGO  LE ANOMALIE UTILIZZANDO ANMFLE
039400970829     C*  CONTENUTE NELLA SKIERA FLR
039500970829     C**
039600970829     C                   MOVE      FLR           FLE
039700990114     C                   SETON                                        24
039800990114     C                   SETOFF                                       25
039900970829     C                   EXSR      USAFLE
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
041800960412     C                   EXSR      USAFLE
041900000317    4C                   ENDIF
042000000317    3C                   ENDDO
042100960327     C*
042200960327     C* FINE LETTURA : IN GESTIONE RICHIAMO LO STESSO IL PGM
042300000317    3C     D34F03        IFEQ      ' '
042400960327     C     D34F12        ANDEQ     ' '
042500000317    4C     D34GES        IFEQ      'I'
042600000317     C     D34GES        OREQ      'G'
042700960327     C                   CLEAR                   DSLR44
042800960327     C                   CLEAR                   D34F03
042900960327     C                   CLEAR                   D34F12
043000960327     C                   CLEAR                   D34RIL
043100960327     C                   MOVEL     'S'           D34END
043200960327     C                   MOVEL     DSLR34        D44LR
043300960521     C                   MOVEL     'G'           D44MOD
043400960327     C*
043500960327     C                   MOVEL     DSLR44        KPJBU
043600960327     C                   CALL      'FNLR44R'
043700960327     C                   PARM                    KPJBA
043800111116     C                   PARM                    dslr34
043900960402     C*
044000960402     C                   MOVEL     KPJBU         DSLR44
044100111116     C***                MOVEL     D44LR         DSLR34
044200960404     C     D34RIL        IFEQ      'S'
044300960412     C                   GOTO      RILEG                                        RILETTURA
044400960404     C                   ENDIF
044500960404     C*
044600000317    4C                   ENDIF
044700000317    3C                   ENDIF
044800040401     C**
044900040401     C* Stampa elenco anomalie 200 aperte fino a data apertura = data idd-1
045000040401     C**
045100040401     c     d34ela        ifne      *blanks
045200040401     c                   clear                   fle
045300040401     C                   MOVE      200           FLE(1)
045400040401     C                   SETOFF                                       24
045500040401     C                   SETON                                        25
045600040401     c* mi salvo la dslr34 prima di sporcarla
045700040401     c                   eval      SAV_dslr34 = dslr34
045800040401     c                   eval      wgir200='S'
045900040401     c                   eval      d34ges='S'
046000040401     c                   eval      d34ie ='I'
046100040401     c                   eval      d34anm = '200'
046200040401     c                   eval      d34inc = 'I'
046300040401     c                   eval      d34ela = *blanks
046400040405     c                   eval      wdta200=d34ala
046500070208     c* Cerco il primo giorno lavorativo
046600070208     C                   Z-ADD     D34ala        WDATA8
046700070208     C                   EXSR      RICDAT
046800070208     C                   Z-ADD     WDATAM        d34ala
046900070208     c
047000070208     c**** *iso          Movel     d34ala        wdta
047100070208     c****               subdur    1:*D          wdta
047200070208     c****               Movel     wdta          D34ala
047300040401     C                   exsr      usafle
047400040401     c                   eval      dslr34 = sav_dslr34
047500040401     c                   eval      wgir200=' '
047600040401     c                   endif
047700960422     C**
047800960422     C* STAMPA SE NESSUNA ANOMALIA RISCONTRATA
047900960422     C**
048000000317    3C     D34GES        IFEQ      'S'
048100960422     C*
048200000317    4C     WSTAMP        IFEQ      ' '
048300040615     c                   if        *inOF
048400040615     C                   EXCEPT    TES
048500040615     C  n70              EXCEPT    TES2
048600040615     C                   EXCEPT    TES3
048700040615     C                   SETOFF                                       OF
048800040615     c                   endif
048900960422     C*
049000960422     C                   EXCEPT    NOANOM
049100000317    4C                   ENDIF
049200960422     C**
049300960422     C* STAMPA ELENCO DI CHI NON HA FATTO L'IDD
049400040615     c*  se non richiamato per altre liste
049500960422     C**
049600040615     c                   if        not *in70
049700040615    4C     D34PAR        IFEQ      'P'
049800960506     C     D34IE         ANDEQ     'D'
049900960423     C     D34ELA        ORNE      ' '
050000960424     C     SIMFEL        ANDEQ     D34FGS
050100960423     C                   EXSR      NOIDD
050200000317    4C                   ENDIF
050300000317    3C                   ENDIF
050400040615    3C                   ENDIF
050500960327     C**
050600960327     C* I N V I O   I D D
050700960327     C**
050800000317    3C     *IN01         IFEQ      *ON
050900000317    4C     W7AIDD        IFEQ      'S'
051000960404     C* CONTROLLO PER TRASMETTERE LE CHIUSURE DELLE ANOMALIE  70 E 71
051100960404     C                   EXSR      CHIUAN
051200960405     C* INVIO FILE CON L'ANOMALIA OK IDD
051300970306     C                   EXSR      CAEX
051400000317     C* PER ELABORAZIONE IDD DEFINITIVA TRASMETTO I SALDI ANOMALIE
051500040927    5C***  D34FGS        IFEQ      SIMFEL
051600040927     C***  D34ALA        ORLE      FELDID
051700040927     C***                CALL      'FNLR38R'
051800040927     C***                PARM                    KPJBA
051900040927    5C***                ENDIF
052000000317    4C                   ENDIF
052100921027     C*
052200960409     C* AGGIORNO DATA ULTIMA ESECUZIONE IDD  CON DATA DEL GIORNO
052300960409     C                   MOVEL     '7A'          COD
052400960409     C                   MOVEL(P)  D34FGS        KEY
052500960409     C     KTAB          CHAIN     TABEL                              3130
052600000317    4C     *IN31         IFEQ      *OFF
052700960409     C     *IN30         ANDEQ     *OFF
052800960409     C                   MOVEL     TBLUNI        DS7AF
052900960409     C                   Z-ADD     D34ALA        §7ADID
053000960409     C                   MOVEL     DS7AF         TBLUNI
053100960409     C                   UPDATE    TABEL
053200000317    4C                   END
053300960409     C*
053400000317    3C                   ENDIF
053500001030     C                   ENDSL
053600000317     C**
053700000320     C**
053800960327     C                   CLEAR                   KPJBU
053900111116     C                   MOVEL     DSLR34        KPJBU
054000000320     C* ULTIMA DISALLOCAZIONE
054100021009     c                   eval      D60po0 = SIMfel
054200000320     C                   MOVEL     SAVFL1        D60FL1
054300000320     C                   MOVEL     SAVFL2        D60FL2
054400000320     C                   MOVEL     AM8GIO        D60DRF
054500000320     C                   MOVEL     'D'           D60AZN
054600000320     C                   MOVEL     'FNANM00T'    D60FIL
054700021009     C                   MOVEL     FNLV60DS      KPJBU
054800000320     C                   CALL      'FNLV60R'
054900000320     C                   PARM                    KPJBA
055000000320     C                   CLEAR                   SAVFL1
055100000320     C                   CLEAR                   SAVFL2
055200970603     C*
055300970603     C* RICHIAMO PGM PER CHIUDERE IN LR
055400970603     C     WCL53         IFEQ      '1'
055500970603     C                   MOVEL     'C'           D53TLA
055600970603     C                   CALL      'FNLV53R'
055700970603     C                   PARM                    DSLV53
055800970603     C                   END
055900971118     C*
056000971118     C                   MOVEL     'C'           D55TLA
056100971118     C                   CALL      'FNLV55R'
056200971118     C                   PARM                    DSLV55
056300020930     C                   CLEAR                   TIBS55DS
056400971125     C                   MOVEL     'C'           I50TLA
056500020930     C                   CALL      'TIBS55R'
056600020930     C                   PARM                    TIBS55DS
056700990122     C                   CLEAR                   DSBS02
056800990122     C                   MOVEL     'C'           T02TLA
056900990122     C                   CALL      'TIBS02R'
057000990122     C                   PARM                    KPJBA
057100990122     C                   PARM                    DSBS02
057200040614     C*
057300040614     c                   clear                   trul09ds
057400040614     C                   MOVEL     'C'           i09TLA
057500040614     C                   CALL      'TRUL09R'
057600040614     C                   PARM                    trul09ds
057700031031     C*
057800031031     C* Se aperto wrk-file WFLAN lo chiudo
057900031031     C                   IF        %open(WFLAN01L)
058000031031     C                   CLOSE     WFLAN01L
058100031031     C                   ENDIF
058200960416     C*
058300000000     C                   SETON                                        LR
058400911018     C*
058500911018     C* IMPOSTO CAMPI FISSI -----------------------------------------*
058600911018     C     IMPO          BEGSR
058700921015     C                   Z-ADD     0             PAGE2             4 0
058800921029     C                   Z-ADD     0             PAGE1             4 0
058900921029     C                   TIME                    UTIME             6 0
059000960604     C                   Z-ADD     0             SAVFL1
059100960403     C*
059200960403     C* SE HO RICHIESTO LA STAMPA APRO I FILE DI STAMPA
059300960403     C     D34GES        IFEQ      'S'
059400000317     C**
059500960403     C                   OPEN      PRTF198
059600960403     C     D34ELA        IFNE      ' '
059700960403     C                   OPEN      PRTF198X
059800960403     C                   ENDIF
059900971120     C**
060000971120     C                   SETON                                        OFOE
060100960604     C**
060200960927     C* DECODIFICO IL P.O. IN GESTIONE
060300040614     c                   if        d34fgs>0
060400960604     C     D34FGS        CHAIN     AZORG01L                           30
060500960604     C  N30              MOVEL     ORGDES        PRTFGS           20
060600040615     C* IMPOSTO LA SCRITTA IN TESTATA
060700040615     C                   MOVEL     CPO           PRTPO            18
060800040614     c                   endif
060900040615     c
061000040615     C* SE VENGO DALLA STAMPA DEI COLLI DI VALORE IMPOSTO
061100040615     C*  INDICATORE 70 E LA SCRITTA IN TESTATA
061200040615    1C                   IF        D34STV='S'
061300040615     C                   SETON                                        70
061400040615     C* SE STAMPA PER TERMINAL, IMPOSTO TERMINAL IN TESTATA
061500040615    2C                   IF        D34SPO<>*BLANKS  and d34spo<>*zeros
061600040615    3C                   IF        D34PAR='A'
061700040615     C                   MOVEL     CTERARR       PRTPO
061800040615     C                   ELSE
061900040615     C                   MOVEL     CTERPAR       PRTPO
062000040615    3C                   ENDIF
062100040615    2C                   ENDIF
062200040615    1C                   ENDIF
062300980402     C**
062400980402     C* DECODIFICO LA LINEA DI ARRIVO SE RICHIESTA
062500980402     C     D34LNA        IFNE      *ZEROS
062600980403     C     D34TRA        ANDNE     'S'
062700980402     C                   SETON                                        06
062800980402     C     D34LNA        CHAIN     AZORG01L                           30
062900040614     C  N30              MOVEL     ORGDES        PRTLNA           16
063000980402     C                   END
063100000317     C                   ENDIF
063200040614     c*
063300040614     c* Selezione p.o. segnacollo
063400040614     c                   if        d34pos>0
063500040614     C                   SETON                                        23
063600040614     C                   ENDIF
063700960327     C*
063800960412     C* CARICO IN UNA SCHIERA TUTTE LE ANOMALIE IDD E LE ANOMALIE
063900960412     C*  DA LEGGERE CON ANMFLE
064000960326     C                   MOVEL     '7C'          COD
064100960412     C                   Z-ADD     0             Y                 3 0
064200961017     C                   Z-ADD     0             T                 3 0
064300970829     C                   Z-ADD     0             U                 3 0
064400970829     C                   Z-ADD     0             V                 3 0
064500960326     C     KTAB2         SETLL     TABEL
064600960409     C     KTAB2         READE(N)  TABEL                                  30
064700960416    1C     *IN30         DOWEQ     *OFF
064800960326     C*
064900960416    2C     TBLFLG        IFEQ      ' '
065000960326     C                   MOVEL     TBLUNI        DS7C
065100960506     C*
065200960412     C* ANOMALIE CHE CONSIDERANO ANMFLE
065300961017     C* §7CUEA='S' --> CONSIDERANO SOLO ANMFLE
065400961017     C* §7CUEA='E' --> CONSIDERANO SIA ANMFLE CHE ANMLNA
065500960416    3C     §7CUEA        IFEQ      'S'
065600960412     C                   ADD       1             Y
065700961017     C                   ADD       1             T
065800961017     C                   MOVEL     TBLKEY        FLO(Y)
065900961023     C                   MOVEL     TBLKEY        FLT(T)
066000961017   X3C                   ELSE
066100961017    4C     §7CUEA        IFEQ      'E'
066200961017     C                   ADD       1             T
066300961023     C                   MOVEL     TBLKEY        FLT(T)
066400961017    4C                   ENDIF
066500960416    3C                   ENDIF
066600970829     C** PARTENZA
066700970829    3C     §7CUEP        IFEQ      'S'
066800970829     C                   ADD       1             U
066900970829     C                   ADD       1             V
067000970829     C                   MOVEL     TBLKEY        FLM(U)
067100970829     C                   MOVEL     TBLKEY        FLR(V)
067200970829   X3C                   ELSE
067300970829    4C     §7CUEP        IFEQ      'E'
067400970829     C                   ADD       1             V
067500970829     C                   MOVEL     TBLKEY        FLR(V)
067600970829    4C                   ENDIF
067700970829    3C                   ENDIF
067800960416    2C                   ENDIF
067900960326     C*
068000960409     C     KTAB2         READE(N)  TABEL                                  30
068100000317    1C                   ENDDO
068200921015     C*
068300960327     C* GIRO DATA FOGLIO SE C'E'
068400960327     C                   MOVE      D34DFV        G02INV
068500960327     C                   MOVEL     '3'           G02ERR
068600971126     C                   CALL      'XSRDA8'
068700960327     C                   PARM                    WLBDAT
068800990513     C                   MOVEL     G02DAT        STADFV            8 0
068900960327     C*
069000960326     C* SE NON IMMESSA DAL AL  = ALLA DATA DAL
069100960326     C     D34ALA        IFEQ      0
069200960403     C                   MOVEL     D34DLA        D34ALA                         DT APERTURA
069300960326     C                   ENDIF
069400960403     C     D34ACA        IFEQ      0
069500960403     C                   MOVEL     D34DCA        D34ACA                         DT CHIUSURA
069600960403     C                   ENDIF
069700960403     C*
069800960403     C* 14 ON - RICHIESTA ALMENO UNA ANOMALIA
069900960403     C     D34ANM        IFNE      *BLANKS
070000960403     C                   SETON                                        14
070100960403     C     D34INC        IFEQ      'I'
070200960403     C                   MOVEL     STA(22)       STARIG           66
070300960403     C                   ELSE
070400960403     C                   MOVEL     STA(23)       STARIG           66
070500960403     C                   ENDIF
070600960403     C                   ENDIF
070700921015     C*
070800921029     C* 15 ON  - IMMESSO NUMERO FOGLIO
070900960326     C     D34NFV        IFGT      0
071000921015     C                   SETON                                        15
071100960403     C                   ENDIF
071200921029     C*
071300921029     C* 16 ON  - IMMESSA FASE APERTURA
071400960326     C     D34FAS        IFNE      ' '
071500921015     C                   SETON                                        16
071600960403     C                   MOVEL     '7D'          COD
071700960403     C                   MOVEL(P)  D34FAS        KEY
071800960409     C     KTAB          CHAIN(N)  TABEL                              32
071900980403     C  N32              MOVEL     TBLUNI        TESFSA           12
072000960403     C  N32              MOVEL     TBLUNI        DESFV             8
072100960403     C                   ENDIF
072200040611     C**                           ì
072300960426     C* 19 ON  - IMMESSA CAUSALE CHIUSURA
072400960426     C     D34CCH        IFNE      ' '
072500960426     C                   SETON                                        19
072600960426     C                   MOVEL     '7E'          COD
072700960426     C                   MOVEL(P)  D34CCH        KEY
072800960426     C     KTAB          CHAIN(N)  TABEL                              32
072900960426     C  N32              MOVEL     TBLUNI        TESCCH           25
073000960426     C                   ENDIF
073100960403     C*
073200960403     C* RICHIESTA DELLE ANOMALIE SOLO APERTE O SOLO CHIUSE
073300960403     C     D34APR        IFEQ      'S'
073400960506     C                   MOVEL     DAN(7)        K51(3)
073500960426     C                   ENDIF
073600960426     C     D34APR        IFEQ      'C'
073700960506     C                   MOVEL     DAN(8)        K51(3)
073800960403     C                   ENDIF
073900040611     c                   if        d34bva='V'
074000040611     C                   MOVEL     vvalore       PRTVAL            8
074100040611     C                   ENDIF
074200921029     C**
074300921029     C* 13 ON  - RICHIESTA I.D.D.
074400921029     C**
074500000317    1C     D34ELA        IFNE      ' '
074600921029     C                   SETON                                        13
074700960403     C* CHAIN A VARIABILI ARRIVI PER VEDERE A QUALE LIVELLO SI CHIUDE
074800960403     C* L'IDD
074900960403     C                   MOVEL     '7A'          COD
075000960403     C                   MOVEL     '2       '    KEY
075100960409     C     KTAB          CHAIN(N)  TABEL                              30
075200960403     C  N30              MOVEL     TBLUNI        DS7A2
075300921029     C*
075400921029     C* 01 ON  - ELABORAZIONE DEFINITIVA I.D.D.
075500000317    2C     D34ELA        IFEQ      'S'
075600921029     C                   SETON                                        01
075700960419     C*
075800960419     C* SE NON SONO SIMFEL VEDO A CHE DATA HA LANCIATO IL 1 LIVELLO
075900000317    3C     D34FGS        IFNE      SIMFEL
076000960419     C                   MOVEL     '7A'          COD
076100960419     C                   MOVEL(P)  SIMFEL        KEY
076200960419     C     KTAB          CHAIN(N)  TABEL                              31
076300960419     C   31              CLEAR                   DS7AF
076400960419     C  N31              MOVEL     TBLUNI        DS7AF
076500960419     C                   MOVE      §7ADID        FELDID
076600000317    3C                   ENDIF
076700921029     C* CHAIN A VARIABILI ARRIVI PER VEDERE SE DEVO INVIARE IDD O NO
076800921029     C                   MOVEL     '7A'          COD
076900921029     C                   MOVEL     '1       '    KEY
077000960409     C     KTAB          CHAIN(N)  TABEL                              30
077100990902     C  N30              MOVEL     TBLUNI        DS7A
077200990902     C   30              CLEAR                   DS7A
077300960417     C*
077400960417     C                   MOVEL     §7AIDD        W7AIDD            1
077500000317     C**
077600000317     C* SE DATA DI ELABORAZIONE <= ALLA DATA IN TABELLA, ESCO SENZA
077700000317     C*  ESEGUIRE IL PGM
077800000317     C                   MOVEL     '7A'          COD
077900000317     C                   MOVEL(P)  D34FGS        KEY
078000000317     C     KTAB          CHAIN(N)  TABEL                              31
078100000317     C   31              CLEAR                   DS7AF
078200000317     C  N31              MOVEL     TBLUNI        DS7AF
078300000317    2C                   ENDIF
078400000317    1C                   ENDIF
078500000317     C**
078600000317     C* D34GES='V' SOLO INVIO, IMPOSTO "S" IN W7AIDD
078700000317     C     D34GES        IFEQ      'V'
078800000317     C                   MOVEL     'S'           W7AIDD
078900000317     C                   ENDIF
079000921029     C*
079100000317    1C     D34GES        IFEQ      'V'
079200000317     C     D34ELA        OREQ      'S'
079300921029     C* SE DEVO INVIARE: CHKOBJ E ADDPF PER TUTTI I MEMBRI
079400000317    2C     W7AIDD        IFEQ      'S'
079500921029     C*
079600021008     C                   MOVE      ')'           VAR               8
079700060619     C                   Z-ADD     49            LUNG             15 5
079800921029     C* MEMBRO RICEZIONE
079900021008     c                   reset                   DSmembro
080000021008     c                   eval      MbrPOP = SIMfel
080100021008     c                   eval      MbrPOA = D34fgs
080200040615     c                   movel(p)  DSmembro      wMBR             10
080300000317    2C                   END
080400000317    1C                   END
080500960506     C**
080600960506     C*  ANOMALIE PARTENZA ARRIVO
080700960506     C**
080800960506     C     D34PAR        IFEQ      'P'
080900960506     C                   MOVEL     DAN(2)        K51(1)
081000960506     C                   ENDIF
081100960506     C     D34PAR        IFEQ      'A'
081200960506     C                   MOVEL     DAN(1)        K51(1)
081300960506     C                   ENDIF
081400921029     C**
081500960506     C* 17 OFF - RICHIESTO SPECIFICO TIPO DI ANOMALIE  INT/IDD/ NON IDD
081600921029     C**
081700960326    1C     D34IE         IFNE      ' '
081800921016     C                   SETON                                        17
081900921016     C* ANOMALIE INTERNE
082000960506     C     D34IE         IFEQ      'I'
082100960506     C                   MOVEL     DAN(5)        K51(2)
082200960327     C                   MOVEL     'I'           EL(1)
082300960403     C                   SETON                                        18
082400960506     C                   ELSE
082500960506     C* ANOMALIE IDD
082600960506    3C     D34IE         IFEQ      'D'
082700960506     C                   MOVEL     DAN(4)        K51(2)
082800960506   X3C                   ELSE
082900960506     C                   MOVEL     DAN(3)        K51(2)
083000960506    3C                   ENDIF
083100960506     C**
083200960506    3C     D34PAR        IFEQ      'P'
083300040116     C****               MOVEL     'R'           EL(1)
083400040116     C                   MOVEL     'E'           EL(1)
083500960506     C                   ELSE
083600960506     C                   MOVEL     'E'           EL(1)
083700960506    3C                   ENDIF
083800960506    2C                   ENDIF
083900960506     C*
084000960506   X1C                   ELSE
084100960506     C                   MOVEL     DAN(6)        K51(2)
084200960326    1C                   ENDIF
084300960506     C                   MOVEA     K51           DESIE            50
084400961018     C***
084500961018     C* CARICO £1
084600961018     C***
084700961104     C                   CLEAR                   DSUL06
084800961018     C                   MOVE      '£1'          D06COD
084900961018     C                   MOVEL     *BLANKS       D06TLA
085000961104     C                   MOVEL     SIMFEL        D06KEY
085100961018     C                   MOVEL     DSUL06        KPJBU
085200961018     C                   CALL      'TRUL06R'
085300961018     C                   PARM                    KPJBA
085400961018     C                   MOVEL     KPJBU         DSUL06
085500961018     C                   MOVEA     LIN           L1
085600960326     C*
085700960927     C* CALCOLO DATA -1 SE RICHIESTE IN PARTENZA LE ANOMALIE IDD
085800960927    1C     D34IE         IFEQ      'D'
085900990512     C     D34DCA        ANDGT     0
086000960927     C*
086100960927    2C     D34PAR        IFEQ      'P'
086200960927     C     D34PAR        OREQ      ' '
086300960927     C* SE IDD CHIUSE O CON CAUSALE PR
086400960927    3C     D34APR        IFEQ      'C'
086500960927     C     D34CCH        OREQ      'PR'
086600960927     C                   SETON                                        05
086700960927     C*
086800971126     C                   Z-ADD     D34DCA        WDATA8
086900960927     C                   EXSR      RICDAT
087000990512     C                   Z-ADD     WDATAM        W08DM1            8 0
087100960927     C*
087200971126    4C     D34ACA        IFGT      0
087300971126     C                   Z-ADD     D34ACA        WDATA8
087400960927     C                   EXSR      RICDAT
087500990512     C                   Z-ADD     WDATAM        W08AM1            8 0
087600960927    4C                   ENDIF
087700960927    3C                   ENDIF
087800960927    2C                   ENDIF
087900960927    1C                   ENDIF
088000960927     C*
088100960927     C* DETERMINO IN UNA SCHIERA I P.O. CHE FANNO L'IDD IL GIORNO DOPO
088200960927    1C     D34PAR        IFEQ      'P'
088300960927     C     D34PAR        OREQ      ' '
088400960927    2C     D34IE         IFNE      'I'
088500960927     C                   Z-ADD     1             L
088600960927     C     *LOVAL        SETLL     AZORG01L
088700960927     C                   READ      AZORG01L                               30
088800960927     C*
088900960927    3C     *IN30         DOWEQ     *OFF
089000960927     C* ESCLUDO LE ANNULLATE
089100960927    4C     ORGFVA        IFEQ      ' '
089200960927     C* SOLO FILIALE E AGENZIA
089300960927    5C     ORGFAG        IFEQ      'F'
089400960927     C     ORGFAG        OREQ      'A'
089500960927     C                   MOVEL     ORGDE8        OG148
089600960927     C**
089700960927    6C     §OGEID        IFNE      ' '
089800960927     C                   Z-ADD     ORGFIL        FM1(L)                         GIORNO PREC
089900960927     C                   ADD       1             L
090000960927    6C                   ENDIF
090100960927    5C                   ENDIF
090200960927    4C                   ENDIF
090300960927     C*
090400960927     C                   READ      AZORG01L                               30
090500960927    3C                   ENDDO
090600960927     C*
090700960927    2C                   ENDIF
090800960927    1C                   ENDIF
090900971126     C***
091000040614     C* CARICO LA L6 DEI P.O. IN GESTIONE  --> p.o. da elaborare solo se
091100040614     c*        schiera p.o. passata vuota
091200971126     C***
091300040614    0c                   if        d34spo=*blanks or d34spo=*zeros
091400971126     C                   CLEAR                   DSUL06
091500971126     C                   MOVE      '£6'          D06COD
091600971126     C                   MOVEL     'L'           D06TLA
091700971126     C                   MOVEL     D34FGS        D06KEY
091800971126     C                   MOVEL     DSUL06        KPJBU
091900971126     C                   CALL      'TRUL06R'
092000971126     C                   PARM                    KPJBA
092100971126     C                   MOVEL     KPJBU         DSUL06
092200971126     C                   MOVEA     LIN           L6
092300971126     C*
092400971126     C* VEDO SE IL P.O. E' UN TERMINAL DI PARTENZA E SE HA DEI P.O.
092500050906     C* ESTERI O D.P.D. o la 101 resi --> LI CARICO
092600971126     C                   CLEAR                   DSLV55
092700971126     C                   MOVEL     'P'           D55TPT
092800971126     C                   MOVEL     D34FGS        D55LIN
092900980403     C                   MOVEL     AM8GIO        D55DRF
093000971126     C                   CALL      'FNLV55R'
093100971126     C                   PARM                    DSLV55
093200971126     C     D55ERR        IFNE      ' '
093300971126     C                   MOVEL     D55LIN        D55TFP
093400971126     C                   ENDIF
093500971126     C                   MOVEL     D55TFP        FGSTFP
093600971126     C*§
093700971126    1C     D34FGS        IFEQ      D55TFP
093800971126     C                   Z-ADD     1             L
093900971126     C                   CLEAR                   W0030
094000971126     C     W0030         LOOKUP    L6(L)                                  30
094100971126     C*
094200971126     C     *LOVAL        SETLL     AZORG01L
094300971126     C                   READ      AZORG01L                               30
094400971126     C*
094500971126    2C     *IN30         DOWEQ     *OFF
094600991217    3C     ORGFAG        IFEQ      'F'
094700991217     C     ORGFAG        OREQ      'A'
094800971126     C* P.O. NON ANNULLATO
094900991217    4C     ORGFVA        IFEQ      ' '
095000991217     C                   MOVEL     ORGDE3        OG143
095100991217     C*
095200020221    5C     §OGNTW        IFEQ      'EEX'                                        ESTERA
095300020221     C     §OGNTW        OREQ      'EUP'
095400020221     C     §OGNTW        OREQ      'FED'
095500020221     C     §OGNTW        OREQ      'DPD'                                        DPD
095600050906     c* anche la 101 resi la tratto come estero: caricata col terminal
095700050906     C     orgfil        OREQ      101                                          DPD
095800971126     C*
095900971126     C* VEDO SE HA COME TERMINAL DI PARTENZA LA FIL.GESTIONE
096000971126     C                   CLEAR                   DSLV55
096100971126     C                   MOVEL     'P'           D55TPT
096200971126     C                   MOVEL     ORGFIL        D55LIN
096300980403     C                   MOVEL     AM8GIO        D55DRF
096400971126     C                   CALL      'FNLV55R'
096500971126     C                   PARM                    DSLV55
096600991217    6C     D55ERR        IFNE      ' '
096700971126     C                   MOVEL     D55LIN        D55TFP
096800991217    6C                   ENDIF
096900991217    6C     D55TFP        IFEQ      D34FGS
097000971126     C                   Z-ADD     ORGFIL        L6(L)
097100971126     C                   ADD       1             L
097200991217    6C                   ENDIF
097300991217    5C                   ENDIF
097400971126    4C                   ENDIF
097500971126    3C                   ENDIF
097600971126     C*
097700971126     C                   READ      AZORG01L                               30
097800971126    2C                   ENDDO
097900971126     C*
098000971126    1C                   ENDIF
098100040614   x0c                   else
098200040617     c                   z-add     1             xx
098300040617     c                   z-add     1             yy                3 0
098400040617     c                   do        30            xx
098500040617     c                   if        spo(xx)>*zeros
098600040617     c* imposto schiera di elaborazione
098700040617     c                   movel     spo(xx)       l6(yy)
098800040617     c* imposto schiera di stampa alfanumerica
098900040617     c                   movel     spo(xx)       stpo(yy)
099000040617     c                   add       1             yy
099100040615     c                   seton                                        80
099200040617     c                   endif
099300040615     c                   enddo
099400040617     c
099500040614     c                   endif
099600960927     C*
099700911018     C                   ENDSR
099800960412     C*
099900960412     C* LEGGO LE ANOMALIE IN PARTENZA --------------------------------*
100000960412     C     PARTEN        BEGSR
100100960412     C                   MOVEL     'N'           WFLE              1
100200960412     C*
100300960412     C* IMPOSTO LE ANOMALIE CHE DEVO LEGGERE
100400960412     C*
100500960412    2C     D34IE         IFEQ      ' '
100600960412     C                   CLEAR                   EL
100700040116     C**                 MOVEL     'R'           EL(1)
100800040116     C                   MOVEL     'E'           EL(1)
100900960412     C                   MOVEL     'I'           EL(2)
101000960412    2C                   ENDIF
101100960412     C**
101200960412     C                   Z-ADD     1             L                 3 0
101300960412    2C     L6(L)         DOWGT     0
101400960412     C     WUSCIT        ANDEQ     ' '
101500960412     C**
101600960412     C                   MOVEL     L6(L)         KFIL
101700960412     C**
101800960412     C                   Z-ADD     1             E                 3 0
101900960412    3C     EL(E)         DOWNE     ' '
102000960412     C     WUSCIT        ANDEQ     ' '
102100960412     C                   MOVEL     EL(E)         KAIE
102200970217     C                   Z-SUB     1             KLNA
102300960412     C                   SETOFF                                       30
102400960412     C**
102500960412    4C     *IN30         DOWEQ     *OFF
102600960412     C     WUSCIT        ANDEQ     ' '
102700990512     C     KANM7         SETGT     FNANM07L
102800990512     C     KANM71        READE     FNANM07L                               30
102900960412     C   30              MOVEL     999           KLNA7
103000960412     C  N30              MOVEL     ANMLNA        KLNA7
103100960412     C*
103200960412     C                   CLEAR                   KLNP
103300990512     C     KANM8         SETGT     FNANM08L
103400990512     C     KANM9         READE     FNANM08L                               30
103500960412     C   30              MOVEL     999           KLNA8
103600960412     C  N30              MOVEL     ANMLNA        KLNA8
103700960412     C*
103800960412    5C     KLNA7         IFNE      999
103900960412     C     KLNA8         ORNE      999
104000960412     C                   SETOFF                                       30
104100960412     C** TROVATA LA LNA
104200960412    6C     KLNA7         IFLT      KLNA8
104300960412     C                   MOVEL     KLNA7         KLNA
104400960412     C                   ELSE
104500960412     C                   MOVEL     KLNA8         KLNA
104600960412    6C                   ENDIF
104700960927     C*
104800960927     C* SE STO STAMPANDO LE RICEVUTE, VEDO SE STAMPARE LE LOCALI
104900040116     C********           EXSR      LOCALI
105000960412     C**
105100040116     C**** WUSCIT        IFEQ      ' '
105200960927     C                   EXSR      LEGPAR
105300040116     C****               ENDIF
105400960412     C**
105500960927   X5C                   ELSE
105600960412     C                   SETON                                        30
105700990518     C                   Z-ADD     999           KLNA
105800990518     C*
105900990518     C* SE STO STAMPANDO LE RICEVUTE, VEDO SE STAMPARE LE LOCALI
106000040116     C*****              EXSR      LOCALI
106100960412    5C                   ENDIF
106200960412    4C                   ENDDO
106300960412     C*
106400960412     C                   ADD       1             E
106500960412    3C                   ENDDO
106600960412     C*
106700960412     C                   ADD       1             L
106800960412    2C                   ENDDO
106900960412     C                   ENDSR
107000960927     C*
107100960927     C* LETTURA ANOMALIE PER LA PARTENZA------------------------------*
107200960927     C     LEGPAR        BEGSR
107300960927    6C                   SELECT
107400960927     C* SOLO ANOMALIE APERTE
107500961218     C*  SOLTANTO SE NON STO LEGGENDO LE LOCALI PERCHE' ALTRIMENTI
107600961218     C*  NON RIUSCIREI A LEGGERE LE ANOMALIE 70 E 71 CHE SONO
107700961218     C*  CHIUSE VIRUTALMENTE MA NON DI FATTO (MANCA SOLO AL CASUALE CH)
107800040116     C**   D34APR        WHENEQ    'S'
107900040116     C**   *IN07         ANDEQ     *OFF
108000040116     C**                 CLEAR                   KDCH
108100040116     C**   KANM72        SETLL     FNANM07L
108200040116     C**   KANM72        READE     FNANM07L                               31
108300960927     C* SOLO ANOMALIE CHIUSE
108400960927     C     D34APR        WHENEQ    'C'
108500960927     C                   Z-ADD     1             KDCH
108600990512     C     KANM72        SETLL     FNANM07L
108700990512     C     KANM7         READE     FNANM07L                               31
108800960927   X6C                   OTHER
108900990512     C     KANM7         SETLL     FNANM07L
109000990512     C     KANM7         READE     FNANM07L                               31
109100960927    6C                   ENDSL
109200960927     C**
109300960927     C                   SETON                                        27
109400960927     C                   SETOFF                                       252829
109500990114     C                   SETOFF                                       2624
109600960927    6C     *IN31         DOWEQ     *OFF
109700960927     C**
109800960927     C                   EXSR      ELASTA                                        STAMPA
109900960927     C** RILEGGO DA CAPO
110000960927    7C     WUSCIT        IFEQ      ' '
110100960927     C*
110200040116    8C**   D34APR        IFEQ      'S'
110300040116     C**   *IN07         ANDEQ     *OFF
110400040116     C**   KANM72        READE     FNANM07L                               31     ANOM APERTE
110500040116   X8C**                 ELSE
110600990512     C     KANM7         READE     FNANM07L                               31     TUTTE O CHI
110700040116    8C**                 ENDIF
110800960927    7C                   ENDIF
110900960927    6C                   ENDDO
111000960927     C*
111100960927     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
111200960927   6AC     WUSCIT        IFEQ      ' '
111300960927     C                   CLEAR                   KLNP
111400960927    6C                   SELECT
111500960927     C* SOLO ANOMALIE APERTE
111600040116     C**   D34APR        WHENEQ    'S'
111700040116     C**   *IN07         ANDEQ     *OFF
111800040116     C**                 CLEAR                   KDCH
111900040116     C**   KANM82        SETLL     FNANM08L
112000040116     C**   KANM82        READE     FNANM08L                               31
112100960927     C* SOLO ANOMALIE CHIUSE
112200960927     C     D34APR        WHENEQ    'C'
112300960927     C                   Z-ADD     1             KDCH
112400990512     C     KANM82        SETLL     FNANM08L
112500990512     C     KANM8         READE     FNANM08L                               31
112600960927   X6C                   OTHER
112700990512     C     KANM8         SETLL     FNANM08L
112800990512     C     KANM8         READE     FNANM08L                               31
112900960927    6C                   ENDSL
113000960927     C**
113100960927     C                   SETON                                        28
113200960927     C                   SETOFF                                       252729
113300990114     C                   SETOFF                                       2624
113400960927    6C     *IN31         DOWEQ     *OFF
113500960927     C**
113600960927     C                   EXSR      ELASTA                                        STAMPA
113700960927     C**
113800960927    7C     WUSCIT        IFEQ      ' '
113900960927     C*
114000040116    8C**   D34APR        IFEQ      'S'
114100040116     C**   *IN07         ANDEQ     *OFF
114200040116     C**   KANM82        READE     FNANM08L                               31     ANOM APERTE
114300040116   X8C**                 ELSE
114400990512     C     KANM8         READE     FNANM08L                               31     TUTTE O CHI
114500040116    8C**                 ENDIF
114600960927    7C                   ENDIF
114700960927     C*
114800960927    6C                   ENDDO
114900960927   6AC                   ENDIF
115000960927     C                   ENDSR
115100960412     C*
115200960412     C* LEGGO LE ANOMALIE IN ARRIVO ----------------------------------*
115300960412     C     ARRIVO        BEGSR
115400960412     C                   MOVEL     'N'           WFLE              1
115500960412     C*
115600960412     C* IMPOSTO LE ANOMALIE CHE DEVO LEGGERE
115700960412    2C     D34IE         IFEQ      ' '
115800960412     C                   CLEAR                   EL
115900960412     C                   MOVEL     'E'           EL(1)
116000960412     C                   MOVEL     'I'           EL(2)
116100960412    2C                   ENDIF
116200960412     C**
116300960412     C                   Z-ADD     1             L                 3 0
116400960412    2C     L6(L)         DOWGT     0
116500960412     C     WUSCIT        ANDEQ     ' '
116600960412     C**
116700960412     C                   MOVEL     L6(L)         KFIL
116800960412     C**
116900960412     C                   Z-ADD     1             E                 3 0
117000960412    3C     EL(E)         DOWNE     ' '
117100960412     C     WUSCIT        ANDEQ     ' '
117200960412     C                   MOVEL     EL(E)         KAIE
117300960412     C                   CLEAR                   KLNP
117400960412     C                   SETOFF                                       30
117500960412     C**
117600960412    4C     *IN30         DOWEQ     *OFF
117700960412     C     WUSCIT        ANDEQ     ' '
117800960412     C                   CLEAR                   KFLS
117900990512     C     KANM9         SETGT     FNANM09L
118000990512     C     KANM71        READE     FNANM09L                               30
118100960412     C   30              MOVEL     999           KLNP9
118200960412     C  N30              MOVEL     ANMLNP        KLNP9
118300960412     C*
118400960412     C                   MOVEL     KLNP          KFLS
118500960412     C                   CLEAR                   KLNP
118600990512     C     KANM94        SETGT     FNANM09L
118700990512     C     KANM9         READE     FNANM09L                               30
118800960412     C   30              MOVEL     999           KLNP92
118900960412     C  N30              MOVEL     ANMFLS        KLNP92
119000960412     C                   CLEAR                   KFLS
119100960412     C*
119200960412    5C     KLNP9         IFNE      999
119300960412     C     KLNP92        ORNE      999
119400960412     C                   SETOFF                                       30
119500960412     C** TROVATA LA LNP
119600960412     C     KLNP9         IFLT      KLNP92
119700960412     C                   MOVEL     KLNP9         KLNP
119800960412     C                   ELSE
119900960412     C                   MOVEL     KLNP92        KLNP
120000960412     C                   ENDIF
120100960412     C**
120200990512     C     KANM9         SETLL     FNANM09L
120300990512     C     KANM9         READE     FNANM09L                               31
120400960412     C**
120500960412     C                   SETON                                        29
120600960412     C                   SETOFF                                       252728
120700990114     C                   SETOFF                                       2624
120800960412    6C     *IN31         DOWEQ     *OFF
120900960412     C**
121000960412     C                   EXSR      ELASTA                                        STAMPA
121100960412     C**
121200960412     C     WUSCIT        IFEQ      ' '
121300990512     C     KANM9         READE     FNANM09L                               31     TUTTE O CHI
121400960412     C                   ENDIF
121500960412    6C                   ENDDO
121600960412     C*
121700960412     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
121800960412   6AC     WUSCIT        IFEQ      ' '
121900960412     C                   MOVEL     KLNP          KFLS
122000960412     C                   CLEAR                   KLNP
122100960412    6C                   SELECT
122200960419     C* SOLO ANOMALIE APERTE (SE NO CONFERMA IDD)
122300960412     C     D34APR        WHENEQ    'S'
122400960419     C     D34ELA        ANDNE     'S'
122500960412     C                   CLEAR                   KDCH
122600990512     C     KANM93        SETLL     FNANM09L
122700990512     C     KANM93        READE     FNANM09L                               31
122800960419     C* SOLO ANOMALIE CHIUSE  (SE NO CONFERMA IDD)
122900960412     C     D34APR        WHENEQ    'C'
123000960419     C     D34ELA        ANDNE     'S'
123100960412     C                   Z-ADD     1             KDCH
123200990512     C     KANM93        SETLL     FNANM09L
123300990512     C     KANM94        READE     FNANM09L                               31
123400960412   X6C                   OTHER
123500990512     C     KANM94        SETLL     FNANM09L
123600990512     C     KANM94        READE     FNANM09L                               31
123700960412    6C                   ENDSL
123800960412     C**
123900960412    6C     *IN31         DOWEQ     *OFF
124000960412     C**
124100960412     C                   EXSR      ELASTA                                        STAMPA
124200960412     C**
124300960412    7C     WUSCIT        IFEQ      ' '
124400960412     C*
124500960412    8C     D34APR        IFEQ      'S'
124600960423     C     D34ELA        ANDNE     'S'
124700990512     C     KANM93        READE     FNANM09L                               31     ANOM APERTE
124800960412   X8C                   ELSE
124900990512     C     KANM94        READE     FNANM09L                               31     TUTTE O CHI
125000960412    8C                   ENDIF
125100960412    7C                   ENDIF
125200960412     C*
125300960412    6C                   ENDDO
125400960412   6AC                   ENDIF
125500960412     C**
125600960412     C* REIMPOSTO IL CAMPO DELLA LINEA DI PARTENZA
125700960412     C                   MOVEL     KFLS          KLNP
125800960412     C                   CLEAR                   KFLS
125900960412     C                   ELSE
126000960412     C                   SETON                                        30
126100960412    5C                   ENDIF
126200960412    4C                   ENDDO
126300960412     C*
126400960412     C                   ADD       1             E
126500960412    3C                   ENDDO
126600960412     C*
126700960412     C                   ADD       1             L
126800960412    2C                   ENDDO
126900960412     C                   ENDSR
127000960412     C*
127100960412     C* LEGGO LE ANOMALIE CON ANMFLE ---------------------------------*
127200960412     C     USAFLE        BEGSR
127300960412     C                   MOVEL     'S'           WFLE              1
127400960412     C                   Z-ADD     1             Y
127500960412     C**
127600960412    1C     FLE(Y)        DOWGT     0
127700960412     C     WUSCIT        ANDEQ     ' '
127800960412     C                   MOVEL     FLE(Y)        KCAA
127900960412     C                   CLEAR                   KFIL
128000960412     C                   SETOFF                                       30
128100960523     C*
128200960508     C* VERIFICO SE L'ANOMALIA E' SECONDO I PARAMETRI RICHIESTI
128300960508     C                   Z-ADD     KCAA          ANMCAA
128400960508     C                   EXSR      LEGCAA
128500960523     C*
128600960508     C* RICHIESTE LE ANOMALIE INTERNE
128700960508     C     D34IE         IFEQ      'I'
128800960508     C     §7CAIE        ANDNE     'I'
128900960508     C                   SETON                                        30
129000960508     C                   ENDIF
129100960523     C**
129200960523     C* RICHIESTE LE ANOMALIE NON IDD
129300960523     C     D34IE         IFEQ      'N'
129400960523     C     §7CIDD        ANDEQ     'S'
129500060428     C     d34ie         oreq      'N'
129600060428     C     §7CAIE        ANDNE     'E'
129700960523     C                   SETON                                        30
129800960523     C                   ENDIF
129900960508     C**
130000960508     C* RICHIESTE LE ANOMALIE IDD
130100960523    1C     D34IE         IFEQ      'D'
130200960508     C     §7CIDD        ANDNE     'S'
130300960523     C*  SE E' LA CONFERMA DELL'IDD LE DEVO ELABORARE COMUNQUE
130400960523    2C     D34ELA        IFNE      'S'
130500960508     C                   SETON                                        30
130600960523   X2C                   ELSE
130700960523    3C     §7CAIE        IFNE      'E'                                          ESTERNA NO I
130800960523     C                   SETON                                        30
130900960523    3C                   ENDIF
131000960523    2C                   ENDIF
131100960523    1C                   ENDIF
131200960412     C**
131300960417    2C     *IN30         DOWEQ     *OFF
131400990512     C     KANM5         SETGT     FNANM10L
131500990512     C     KCAA          READE     FNANM10L                               30
131600960412    3C     *IN30         IFEQ      *OFF
131700960412     C                   MOVEL     ANMLNA        KFIL
131800960412     C                   CLEAR                   KLNP
131900960412     C                   CLEAR                   KFLS
132000960412     C                   SETOFF                                       31
132100960412     C*
132200960412    4C     *IN31         DOWEQ     *OFF
132300960412     C     WUSCIT        ANDEQ     ' '
132400990512     C     KANM51        SETGT     FNANM10L
132500990512     C     KANM5         READE     FNANM10L                               31
132600960412     C  N31              MOVEL     ANMLNP        KLNP9
132700960412     C   31              MOVEL     999           KLNP9
132800960412     C*
132900960412     C                   MOVEL     KLNP          KFLS
133000960412     C                   CLEAR                   KLNP
133100990512     C     KANM52        SETGT     FNANM10L
133200990512     C     KANM51        READE     FNANM10L                               31
133300960412     C  N31              MOVEL     ANMFLS        KLNP92
133400960412     C   31              MOVEL     999           KLNP92
133500960412     C*
133600960412    5C     KLNP9         IFNE      999
133700960416     C     KLNP92        ORNE      999
133800960412     C** TROVATA LA LNP
133900960412    6C     KLNP9         IFLT      KLNP92
134000960412     C                   MOVEL     KLNP9         KLNP
134100960412     C                   ELSE
134200960412     C                   MOVEL     KLNP92        KLNP
134300960412    6C                   ENDIF
134400960412     C**
134500990512     C     KANM51        SETLL     FNANM10L
134600990512     C     KANM51        READE     FNANM10L                               31
134700960412     C**
134800960412     C                   SETOFF                                       272829
134900970529     C                   SETOFF                                       26
135000960412    6C     *IN31         DOWEQ     *OFF
135100960412     C**
135200960412     C                   EXSR      ELASTA                                        STAMPA
135300960412     C**
135400960412    7C     WUSCIT        IFEQ      ' '
135500990512     C     KANM51        READE     FNANM10L                               31     TUTTE O CHI
135600960412    7C                   ENDIF
135700960412    6C                   ENDDO
135800960412     C*
135900960412     C* NUOVO CICLO LEGGENDO CON LNP = 0 E FLS = LNP LETTA
136000960412    6C     WUSCIT        IFEQ      ' '
136100960412     C                   MOVEL     KLNP          KFLS
136200960412     C                   CLEAR                   KLNP
136300990512     C     KANM52        SETLL     FNANM10L
136400990512     C     KANM52        READE     FNANM10L                               31
136500960412     C**
136600960412    7C     *IN31         DOWEQ     *OFF
136700960412     C**
136800960412     C                   EXSR      ELASTA                                        STAMPA
136900960412     C**
137000960412    8C     WUSCIT        IFEQ      ' '
137100960412     C*
137200990512     C     KANM52        READE     FNANM10L                               31     TUTTE O CHI
137300960412    8C                   ENDIF
137400960412     C*
137500960412    7C                   ENDDO
137600960412    6C                   ENDIF
137700960412     C**
137800960412     C* REIMPOSTO IL CAMPO DELLA LINEA DI PARTENZA
137900960412     C                   MOVEL     KFLS          KLNP
138000960412     C                   CLEAR                   KFLS
138100960412     C                   SETOFF                                       31
138200960412   X5C                   ELSE
138300960412     C                   SETON                                        31
138400960412    5C                   ENDIF
138500960412    4C                   ENDDO
138600960412    3C                   ENDIF
138700960412    2C                   ENDDO
138800960412     C*
138900960412     C                   ADD       1             Y
139000960412    1C                   ENDDO
139100960412     C*
139200960412     C                   ENDSR
139300970529     C*
139400040614     C* LEGGO LE ANOMALIE CON ANMCDU solo in partenza ----------------*
139500970529     C     USACDU        BEGSR
139600970529     C                   MOVEL     'N'           WFLE              1
139700040615     C                   MOVEL     'S'           Wcdu              1
139800970529     C                   SETOFF                                       252728
139900990114     C                   SETOFF                                       2924
140000970529     C                   SETON                                        26
140100970529     C* LEGGO TUTTE LE ANOMALIE RICEVUTE CON CDU=SIMFEL SCARTANDO
140200040616     C* QUELLE CON LNP O FLS PRESENTI IN £1(lo scato avviene nella
140300040616     c*        exsr CONFR)
140400970529     C****
140500970529     C* 1°CICLO: LEGGO CON LNP > 0 E <> £1
140600970529     C****
140700040116     C                   MOVEL     'E'           KAIE
140800970529     C                   Z-ADD     SIMFEL        KCDU
140900970529     C                   Z-ADD     1             ANMLNP
141000040616     c
141100970529    0C     WUSCIT        IFEQ      *BLANKS
141200990512     C     KANM3         SETLL     FNANM03L
141300990512     C     KANM3A        READE     FNANM03L                               30
141400970529    1C     *IN30         DOWEQ     *OFF
141500040615     c                   eval      wsetgt='N'
141600040615     c
141700040429     C                   EXSR      ELASTA
141800040615     c*
141900040615     c                   if        wsetgt='S'
142000040615     C     KANM3         SETGT     FNANM03L
142100040429     c                   endif
142200040615     c
142300970529    2C     WUSCIT        IFEQ      *BLANKS
142400990512     C     KANM3A        READE     FNANM03L                               30
142500970529    2C                   END
142600970529    1C                   ENDDO
142700970529     C****
142800970529     C* 2°CICLO: LEGGO CON LNP = 0
142900970529     C*                 ED FLS > 0 E <> £1
143000970529     C****
143100970529   1AC     WUSCIT        IFEQ      *BLANKS
143200970529     C                   MOVE      *ZEROS        KLNP
143300970529     C                   Z-ADD     1             ANMFLS
143400990512     C     KANM3B        SETLL     FNANM03L
143500990512     C     KANM3D        READE     FNANM03L                               30
143600970529    1C     *IN30         DOWEQ     *OFF
143700040616     c                   eval      wsetgt='N'
143800040616     c
143900040429     C                   EXSR      ELASTA
144000040616     c
144100040616     C* Fls in £1 --> Scarto e mi posiziono su nuova Fls
144200040616    2c                   if        wsetgt='S'
144300990512     C     KANM3B        SETGT     FNANM03L
144400970529    2C                   END
144500040616    c
144600970529    2C     WUSCIT        IFEQ      *BLANKS
144700990512     C     KANM3D        READE     FNANM03L                               30
144800040616    2C                   END
144900970529    1C                   ENDDO
145000970529   1AC                   END
145100970529    0C                   END
145200040615     C                   MOVEL     'N'           Wcdu              1
145300970529     C                   ENDSR
145400960327     C*
145500960523     C* ELABORO I DATI DELLE ANOMALIE PER LA STAMPA -----------------*
145600960327     C     ELASTA        BEGSR
145700960327     C*
145800960327     C* CONFRONTO SELEZIONI FATTE
145900960327     C                   EXSR      CONFR
146000960327     C**
146100960327    1C     *IN10         IFEQ      *ON
146200960327     C**
146300960327     C* SE ELABORAZIONE DEFINITIVA IDD, INVIO ANCHE LE ANOMALIE
146400960705     C*  CHE CI SONO DA TRASMETTERE
146500960327    2C     *IN01         IFEQ      *ON
146600960419     C     *IN12         ANDEQ     *ON
146700960327    3C     ANMFT1        IFEQ      ' '
146800960412     C     ANMFL1        ANDGT     0
146900960327     C     ANMFT2        OREQ      ' '
147000960412     C     ANMFL2        ANDGT     0
147100960705     C                   CLEAR                   WTRM
147200960827     C* SERIE > 0
147300960827    4C     ANMNRS        IFGT      *ZEROS
147400960827     C**SE ANOMALIA 55 ED ESISTE BOLLA PARTENZA NON LA TRASMETTO E
147500960827     C* LA CHIUDO. CHIUDO ANCHE LA 200
147600960827     C* (SE BOLLA DA ME CONFERMABILE E CONFERMABILE ANCHE DA ALTRI AS)
147700960827     C**STESSA COSA SE ANOMALIA 50 (SENZA CHIUDERE LA 200)
147800960827     C     ANMCAA        IFEQ      50
147900960827     C     ANMCAA        OREQ      55
148000990409     C     ANMCAA        OREQ      56
148100960827     C                   EXSR      A50A55
148200960827     C                   ENDIF
148300960827    4C                   ENDIF
148400960827     C*
148500960705    4C     WTRM          IFEQ      ' '
148600990114     C   24
148700990512     COR 25ANMN10        CHAIN     FNANM000                           33
148800000306     C   26ANMNR3        CHAIN     FNANM000                           33
148900000306     C   27ANMNR7        CHAIN     FNANM000                           33
149000990512     C   28ANMNR8        CHAIN     FNANM000                           33
149100990512     C   29ANMNR9        CHAIN     FNANM000                           33
149200960404     C*
149300960327     C                   EXSR      ELAB
149400960705    4C                   ENDIF
149500960327    3C                   ENDIF
149600960327    2C                   ENDIF
149700960327     C**
149800960327   X1C                   ELSE
149900960327     C* 10 OFF - RECORD SELEZIONABILE
150000960327     C                   CLEAR                   WUSCIT
150100980327     C*** TRASFERIMENTO DA FILIALE A FILIALE
150200980327     C**
150300040615     C**   D34TRA        IFEQ      'S'
150400040615     C**                 EXSR      TRASFE
150500040615     C**                 END
150600960327     C**
150700960327     C** G E S T I O N E
150800000317    2C     D34GES        IFEQ      'I'
150900000317     C     D34GES        OREQ      'G'
151000960327     C* RICHIAMO IL PGM FNLR44R PASSANDOGLI IL RECORD DELL'ANOMALIA
151100960327     C*  IL NUMERO RELATIVO DI RECORD
151200960327     C*  LA DS DEI PARAMETRI RICHIESTI
151300960515     C* LO RICHIAMO SOLO SE IL SOGGETTO ANOMALIA E' <> DA "E"
151400040116    3C**   ANMSAN        IFNE      'E'
151500040116     C**   ANMAIE        ANDEQ     'E'
151600040116     C**   ANMAIE        ORNE      'E'
151700040116    c
151800040116    3C     ANMSAN        IFNE      'E'
151900040116     C     wanom         ANDEQ     'A'
152000040116     C     ANMAIE        ORNE      'E'
152100040116     C     wanom         orne      'A'
152200960327     C                   CLEAR                   DSLR44
152300960515    4C     *IN27         IFEQ      *ON
152400960401     C     *IN28         OREQ      *ON
152500970529     C     *IN26         OREQ      *ON
152600990114     C     *IN24         OREQ      *ON
152700990114     C   24              Z-ADD     ANMN10        D44NRR
152800970529     C   26              Z-ADD     ANMNR3        D44NRR
152900960327     C   27              Z-ADD     ANMNR7        D44NRR
153000960327     C   28              Z-ADD     ANMNR8        D44NRR
153100960401     C                   MOVEL     'P'           D34APA
153200960515   X4C                   ELSE
153300960327     C   29              Z-ADD     ANMNR9        D44NRR
153400960417     C   25              Z-ADD     ANMN10        D44NRR
153500960401     C                   MOVEL     'A'           D34APA
153600960515    4C                   ENDIF
153700960327     C                   CLEAR                   D34F03
153800960327     C                   CLEAR                   D34F12
153900960327     C                   CLEAR                   D34RIL
154000960327     C                   MOVEL     DSLR34        D44LR
154100960327     C                   MOVEL     DSANM         D44ANM
154200960521     C                   MOVEL     'G'           D44MOD
154300960327     C*
154400960327     C                   MOVEL     DSLR44        KPJBU
154500960327     C                   CALL      'FNLR44R'
154600960327     C                   PARM                    KPJBA
154700111116     C                   PARM                    dslr34
154800960327     C                   MOVEL     KPJBU         DSLR44
154900111116     C***                MOVEL     D44LR         DSLR34
155000960327     C* SE PREMUTO F3 F12 O RILETTURA
155100960515    4C     D34F03        IFEQ      'S'
155200960327     C     D34F12        OREQ      'S'
155300960327     C     D34RIL        OREQ      'S'
155400960327     C                   MOVEL     'S'           WUSCIT            1
155500960327     C                   SETON                                        3031
155600960515    4C                   ENDIF
155700960515    3C                   ENDIF
155800960327     C**
155900960327   X2C                   ELSE
156000960327     C** S T A M P A
156100960327     C*
156200000317    3C                   SELECT
156300000317     C* ELABORAZIONE IDD
156400000317     C     D34ELA        WHENNE    ' '
156500960405     C  N11              EXSR      STAMPA
156600960327     C*
156700960327     C* ELABORAZIONE DEFINITIVA AGGIORNO E SPEDISCO
156800960405     C   01              EXSR      ELAB
156900960327     C**
157000000317     C     D34GES        WHENEQ    'S'
157100960327     C* NON E' STAMPA IDD --> STAMPO TUTTO
157200960327     C                   EXSR      STAMPA
157300000317     C*
157400000317    3C                   ENDSL
157500960327    2C                   ENDIF
157600960327    1C                   ENDIF
157700960327     C*
157800960327     C                   ENDSR
157900921015     C*
158000921015     C* CONFRONTO RICHIESTA FATTA A VIDEO ---------------------------*
158100921015     C     CONFR         BEGSR
158200960508     C                   SETOFF                                         2097
158300960419     C                   SETOFF                                       101112
158400960509     C                   CLEAR                   PRTZNC
158500960927     C                   CLEAR                   PRTDET
158600960927     C                   CLEAR                   WDDET
158700960927     C                   CLEAR                   WDFLP
158800960927     C                   CLEAR                   PSTAM
158900980327     C* PULISCO IL CAMPO DI CANCELLAZIONE RECORD IN CASO DI TRASFERIMENTO
159000980327     C* A FILIALE
159100040615     C**                 CLEAR                   NODEL
159200040615     C**                 CLEAR                   WRTF
159300030717     C**
159400030717     C* SE RICHIESTO ESEGUO LA SELEZIONE X P.O. SEGNACOLLO E NUMERO SERIE
159500030717     C                   IF        D34POS  > *ZEROS
159600030717     C                   IF        D34POS <> ANMFLS
159700030717     C                   SETON                                        10
159800030717     C                   GOTO      ENDCON
159900030717     C                   ENDIF
160000040614     c
160100030717     C                   IF        D34NRS <> 99
160200030717     C                   IF        D34NRS <> ANMNRS
160300030717     C                   SETON                                        10
160400030717     C                   GOTO      ENDCON
160500030717     C                   ENDIF
160600030717     C                   ENDIF
160700040614     C                   ENDIF
160800040609     c
160900040609     c* se richiesto, faccio vedere solo anomalie di colli di valore
161000040609     c                   if        D34bva='V'  and
161100040609     c                             anmft4<>'V'
161200040609     C                   SETON                                        10
161300040609     C                   GOTO      ENDCON
161400040609     C                   ENDIF
161500000317     C**
161600980402     C* CONTROLLO CHE LA LINEA DI ARRIVO SIA UGUALE A QUELLA RICHIESTA A VIDEO
161700980403     C* SE NON SONO IN UNA FASE DI TRASFERIMENTO
161800980403     C     D34TRA        IFNE      'S'
161900980402     C     D34LNA        IFNE      *ZEROS
162000980402     C     D34LNA        ANDNE     ANMLNA
162100980402     C                   SETON                                        10
162200980402     C                   GOTO      ENDCON
162300980402     C                   ENDIF
162400980403     C                   ENDIF
162500960424     C*
162600960424     C* SE STO LEGGENDO UNA ANOMALIA RICEVUTA, NON LA STAMPO
162700960424     C*  SE IL SEGNACOLLO NEL FRATTEMPO E' STATO CONSEGNATO
162800960524     C*  (NON COME IDD3 DANNI CODICE 5)
162900970127     C*  (NON SE DATA APERTURA ANOMALIA > DATA CONSEGNA)
163000040116    1C***  ANMAIE        IFEQ      'R'
163100040116     C     ANMSCN        ifgt      0
163200040223     C     wanom         andeq     'P'
163300111115     c* In gestione/int non lo faccio --> devo vedere le anomalie come sono
163400111115     c*  per poterle annullare
163500111115     c     d34ges        andeq     'S'
163600960424     C*
163700960424     C     KART          CHAIN     FNBLT27L                           32
163800960424    2C     *IN32         IFEQ      *OFF
163900960424     C*
164000960424    3C     BLTDCM        IFGT      0
164100960927     C     BLTDET        ORGT      0
164200960524     C**
164300960524     C     KBLP          CHAIN     FNBLP01L                           32
164400960524     C**
164500970127    4C     BLTDCM        IFGT      0
164600970127     C     BLPCCA        ANDNE     '5'
164700151125     C     BLPCCA        ANDNE     '7'
164800151125     C     ANMDAO        ANDLE     BLTDCM
164900960424     C* SE HO CHIESTO SOLO LE APERTE ESCO
165000960524    5C     D34APR        IFEQ      'S'
165100960424     C                   SETON                                        10
165200960424     C                   GOTO      ENDCON
165300960524   X5C                   ELSE
165400960424     C* ALTRIMENTI DICO CHE E' CHIUSA COME CONSEGNATA
165500960524    6C     ANMDCH        IFEQ      0
165600960424     C                   MOVE      BLTDCM        ANMDCH
165700960524    6C                   ENDIF
165800960524     C                   MOVEL     'CO'          ANMCCH
165900960524    5C                   ENDIF
166000960424    4C                   ENDIF
166100960424    3C                   ENDIF
166200960424     C*
166300960424     C* SE ANOMALIA 10 NON STAMPO SE LA SPEDIZIONE C'E' ED E' TRASMESSA
166400960424    3C     ANMCAA        IFEQ      10
166500960424     C     KBLP          CHAIN     FNBLP01L                           32
166600960509     C  N32              MOVEL     BLPZNC        PRTZNC            2
166700151125    4C  N32BLPFT1        IFeq      'T'
166800960424     C* SE HO CHIESTO SOLO LE APERTE ESCO
166900960424    5C     D34APR        IFEQ      'S'
167000960424     C                   SETON                                        10
167100960424     C                   GOTO      ENDCON
167200960424   X5C                   ELSE
167300960424    6C     ANMDCH        IFEQ      0
167400960424     C                   Z-ADD     BLPMGS        ANMDCH
167500990512     C                   MOVEL     BLPAAS        ANMDCH
167600960424    6C                   ENDIF
167700960424    5C                   ENDIF
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
170600970604   4AC     BRVNFV        IFNE      ANMNFV
170700970603     C                   CLEAR                   DSLV53
170800970604     C                   MOVEL     'G'           D53TFO
170900970603     C                   Z-ADD     SIMFEL        D53FEL
171000970603     C                   Z-ADD     BRVNFV        D53NFV
171100020926     C                   Z-ADD     BRVfgs        D53FGS
171200970603     C                   CALL      'FNLV53R'
171300970603     C                   PARM                    DSLV53
171400970603     C                   MOVEL     '1'           WCL53             1
171500980424     C* QUESTO CONTROLLO LO IGNORO SE SONO IN TRASFERIMENTO
171600980424    5C     D34TRA        IFNE      'S'
171700980424     C     D53ERR        ANDEQ     *BLANKS
171800970604     C     D53DFV        ANDGT     W53DFV
171900970603     C                   SETON                                        10
172000970603     C                   GOTO      ENDCON
172100970603    5C                   END
172200970604   4AC                   END
172300140618     C     KBRV3A        READE     FNBRV05L                               32
172400970603    4C                   ENDDO
172500970604   3AC                   ENDIF
172600970603     C*
172700960927     C                   EXSR      DISPAR
172800960927    3C                   ENDIF
172900960927     C**
173000960424     C* MEMORIZZO IL NUMERO DI SPEDIZIONE SE C'E'
173100960424     C                   MOVE      BLTAAS        ANMAAS
173200960424     C                   MOVE      BLTLNP        ANMLNP
173300960424     C                   MOVE      BLTNSP        ANMNSP
173400960424    2C                   ENDIF
173500960424    1C                   ENDIF
173600040617     C
173700040617     c* Per stampa colli di valore in arrivo, escludo anomalie se non
173800040617     c*  crerate dal p.o. gestione  SE STAMPA DEL TERMINAL
173900040617     c                   if        *in70 and wanom='A' AND ANMCAA=005 AND
174000040617     C                             *IN80
174100040617     c                   if        anmfle<>d34fgs
174200040617     C                   SETON                                        10
174300040617     C                   GOTO      ENDCON
174400040617     c                   endif
174500040617     c                   endif
174600960506     C* CODICE ANOMALIA
174700960508     C                   EXSR      LEGCAA
174800040615     c
174900040615     c* In USACDU escludo sempre le anomalie non IDD (in partenza sempre
175000040615     c*           lette per lnp)
175100040615     c                   if        wcdu='S' and §7caie='E' and §7cidd=' '
175200040615     C                   SETON                                        10
175300040615     C                   GOTO      ENDCON
175400040615    4C                   ENDIF
175500960419     C**
175600960506     C**
175700960419     C* 1) SE CONTROLLO NON PASSATO NON FACCIO NULLA
175800960419     C*
175900960416     C* SE DEVO USARE FLE, VERIFICO SE E' IN L6
176000961230    1C     WFLE          IFEQ      'S'
176100960416     C     ANMFLE        LOOKUP    L6                                     32
176200960416     C  N32              SETON                                            10
176300960417     C  N32              GOTO      ENDCON
176400961230     C*
176500970829    2C     WANOM         IFEQ      'A'
176600961017     C* VERIFICO ANCHE CHE NON SIA ANOMALIA CHE UTILIZZA
176700961017     C* SIA LNA CHE FLE CON LNA=FLE
176800970829    3C     §7CUEA        IFEQ      'E'
176900970829    4C     ANMLNA        IFEQ      ANMFLE
177000961017     C                   SETON                                        10
177100961017     C                   GOTO      ENDCON
177200970829    4C                   ENDIF
177300040413     C* SE E' IN L6  MA LO E' ANCHE LA LNA
177400961230     C*  ED E' UN TIPO ANOMALIA CHE VIENE STAMPATO  PER ENTRANBI
177500961230     C*  ESCLUDO PERCHE' L'HO GIA' STAMPATA
177600961230     C     ANMLNA        LOOKUP    L6                                     32
177700961230     C   32              SETON                                            10
177800961230     C   32              GOTO      ENDCON
177900970829    3C                   ENDIF
178000970829     C*
178100970829   X2C                   ELSE
178200970829     C* FLE UTILIZZATO IN PARTENZA
178300970829    3C     §7CUEP        IFEQ      'E'
178400970829     C* PRENDO O LNP O FLS
178500970829    4C     ANMLNP        IFGT      0
178600971125     C                   Z-ADD     ANMLNP        W0030             3 0
178700970829     C                   ELSE
178800970829     C                   Z-ADD     ANMFLS        W0030
178900970829    4C                   ENDIF
179000970829     C     W0030         LOOKUP    L1                                     32
179100970829     C   32              SETON                                            10
179200970829     C   32              GOTO      ENDCON
179300970829    3C                   ENDIF
179400970829     C**
179500970829    2C                   ENDIF
179600961230    1C                   ENDIF
179700980327     C* IN CASO DI TRASFERIMENTO DI FILIALE
179800040615    1C**   WFLE          IFNE      'S'
179900040615     C**   D34TRA        ANDEQ     'S'
180000040615    2C**   §7CUEA        IFEQ      'E'
180100980327     C* ARRIVO
180200040615     C**   WANOM         ANDEQ     'A'
180300040615     C**   ANMFLE        LOOKUP    L6                                     32
180400040615     C**N32ANMFLE        LOOKUP    L1                                     34
180500040615     C**N32
180600040615     C*N 34              MOVEL     'S'           NODEL             1
180700040615    2C**                 END
180800980327     C* PARTENZA
180900040615    2C**   §7CUEP        IFEQ      'E'
181000040615     C**   WANOM         ANDEQ     'P'
181100040615     C**   ANMFLE        LOOKUP    L6                                     32
181200040615     C**N32ANMFLE        LOOKUP    L1                                     34
181300040615     C**N32
181400040615     C*N 34              MOVEL     'S'           NODEL
181500040615    2C**                 END
181600980327     C*
181700040615    1C**                 END
181800960419     C**
181900960419     C* 2) SE CONTROLLO NON PASSATO NON FACCIO NULLA
182000960419     C**
182100960419     C* DATA APERTURA ANOMALIA DAL/AL
182200960419    1C     D34DLA        IFGT      0
182300960419    1C     D34ALA        ORGT      0
182400990512    2C     ANMDAO        IFLT      D34DLA
182500990512     C     ANMDAO        ORGT      D34ALA
182600960419     C                   SETON                                        10
182700960419     C                   GOTO      ENDCON
182800960419    2C                   ENDIF
182900960419    1C                   ENDIF
183000040615     c**
183100040615     c* 2a) Il controllo che faceva in USACDU lo faccio qui dopo aver
183200040615     c*     controllato la data
183300040615    1c                   if        wcdu='S'
183400040615     c
183500040615     c* cerco alla data anmdao se d34fgs è terminal di anmlnp
183600040615     c                   eval      kcaeepa='P'
183700040615    2c                   if        anmlnp>0
183800040615     c                   eval      kcaetfp=anmlnp
183900040615     c                   else
184000040615     c                   eval      kcaetfp=anmfls
184100040615    2c                   endif
184200040615     c                   eval      kcaetfe=d34fgs
184300040615     c                   eval      kcaedde=anmdao
184400040615     c     kcae          setll     azcae03l
184500040615     c     kcae2         readpe    azcae03l
184600040616     c
184700040616    2c                   dow       not %eof(azcae03l) and caeatb<>*blanks
184800040616     c     kcae2         readpe    azcae03l
184900040616    2c                   enddo
185000040616     c
185100040616    2c                   if        not %eof(azcae03l)
185200040615     c* se decorrente alla data anomalia --> p.o. della £1 di d34fgs escludo
185300040615    3c                   if        caedsc>=anmdao
185400040615     c                   seton                                        10
185500040615     c* Se Compreso tutto il periodo richiesto a video, escludo tutta la
185600040615     c*  lnp e quindi faccio SETGT in usacdu
185700040615    4c                   if        caedde<=d34dla  and
185800040615     c                             caedsc>=d34ala
185900040615     c                   eval      wsetgt='S'
186000040615    4c                   endif
186100040615     c
186200040615    3c                   endif
186300040615    2c                   endif
186400040615     c   10              goto      endcon
186500040615     c*
186600040615     c* Escludo anche dalla stampa anomalie con anmlna 2 livello di
186700040616     c*  d34fgs per non stampare anomalie di arrivo e  non di transito
186800040615    2c                   if        anmlna=d34fgs
186900040615     C                   SETON                                        10
187000040615     C                   GOTO      ENDCON
187100040616   x2c                   else
187200040616     c
187300040616     c                   clear                   wanomok           1
187400040616     c* Controllo il terminal di arrivo per sta anomalia
187500040616     c                   eval      kcaeepa='A'
187600040616    3c                   if        anmlnp>0
187700040616     c                   eval      kcaetfp=anmlnp
187800040616     c                   else
187900040616     c                   eval      kcaetfp=anmfls
188000040616    3c                   endif
188100040616     c                   eval      kcaetfa=anmlna
188200040616     c                   eval      kcaedde=anmdao
188300040616     c
188400040616     c                   EXSR      CercaCAE
188500040616     c
188600040616     c* Se non ho trovato eccezione terminal, cerco terminal normale
188700040616    3c                   if        wanomok=' '
188800040616     c                   clear                   kcaetfp
188900040616     c                   EXSR      CercaCAE
189000040616    3c                   endif
189100040616    c
189200040616     c* anomalia da non stampare
189300040616    3c                   if        wanomok='N'
189400040616     C                   SETON                                        10
189500040616     C                   GOTO      ENDCON
189600040616    3c                   endif
189700040616     c
189800040616    2c                   endif
189900040615    1c                   endif
190000960419     C**
190100960419     C* 3) SE CONTROLLO NON PASSATO NON FACCIO NULLA
190200960419     C**
190300960419     C* SE NON STO ELABORANDO PER FLE ESCLUDO LE ANOMALIE FLE
190400970829     C     WFLE          IFEQ      'N'
190500960419     C     ANMCAA        LOOKUP    FLE                                    10
190600960419     C   10              GOTO      ENDCON
190700960419     C                   ENDIF
190800960419     C**
190900960419     C* 4) SE E COMUNQUE ANOMALIE ESTERNA ANCHE SE NO IDD LA TRASMETTO
191000960419     C**
191100960327     C* SE HO RICHIESTO LE ESTERNE O LE RICEVUTE, VEDO SE
191200960327     C*  SONO QUELLE IDD O NO
191300960327     C     D34IE         IFNE      'I'
191400960327     C                   MOVEL     ANMCAA        W003A             3
191500960506     C     §7CIDD        IFEQ      'S'
191600960506     C                   SETON                                        97
191700960506     C                   ENDIF
191800960404     C* 97 ON - E' ANOMALIA IDD
191900960404    2C     *IN97         IFEQ      *ON
192000960506    3C     D34IE         IFEQ      'N'
192100960327     C                   SETON                                        10
192200960327     C                   GOTO      ENDCON
192300960327    3C                   ENDIF
192400960327     C*
192500960327   X2C                   ELSE
192600960404     C* 97 OFF  - NO IDD
192700960506    3C     D34IE         IFEQ      'D'
192800960327     C                   SETON                                        10
192900960419     C* 12 ON SE COMUNQUE ANOMALIA ESTERNA
193000040116     C   01aNMAIE        IFEQ      'E'
193100960419     C                   SETON                                        12
193200960419    3C                   ENDIF
193300960419     C*
193400960327     C                   GOTO      ENDCON
193500960327    3C                   ENDIF
193600960327    2C                   ENDIF
193700960327    1C                   ENDIF
193800960419     C**
193900960419     C* 5) SE ANOMALIA IDD CHIUSA TRASMETTO SENZA STAMPARE
194000960419     C**
194100960419     C* STAMPA SOLO ANOMALIE APERTE
194200961217     C* PER LE ANOMALIE LOCALI 70 E 71 FACCIO UN GIRO A PARTE
194300040116    0C**   *IN07         IFEQ      *OFF
194400040116     C**   *IN07         OREQ      *ON
194500040116     C**   ANMCAA        ANDNE     70
194600040116     C**   ANMCAA        ANDNE     71
194700040116     c
194800040116     c                   if        wanom='A'  or
194900040116     c                             wanom='P' and anmcaa<>70 and anmcaa<>71
195000961217     C*
195100960419    1C     D34APR        IFEQ      'S'
195200960419     C     ANMDCH        ANDNE     0
195300960419     C                   SETON                                        10
195400960419     C   01ANMAIE        IFEQ      'E'
195500960419     C                   SETON                                        12
195600960419     C                   ENDIF
195700960419     C                   GOTO      ENDCON
195800960419    1C                   ENDIF
195900960419     C* SOLO ANOMALIE CHIUSE
196000960419    1C     D34APR        IFEQ      'C'
196100960419     C     ANMDCH        ANDEQ     0
196200960419     C                   SETON                                        10
196300960419     C                   GOTO      ENDCON
196400960419    1C                   ENDIF
196500961217     C*
196600961217   X0C                   ELSE
196700961217     C* NON TESTO LA DATA DI CHIUSURA MA LA CASUALE
196800961217    1C     D34APR        IFEQ      'S'
196900961217    2C     ANMCCH        IFNE      '  '
197000961217     C                   SETON                                        10
197100961217   X2C                   ELSE
197200961217     C*
197300961217     C* PRIMA DI STAMPARLA VERIFICO SE CE N'E' UN'ALTRA COM DATA >
197400961217     C*                    MI SALVO LA DATA
197500990512     C                   MOVEL     ANMDAO        W0080             8 0
197600990512     C                   MOVEL     ANMAAS        KAAS2
197700961217     C                   MOVEL     ANMLNP        KLNP2
197800961217     C                   MOVEL     ANMNRS        KNRS2
197900961217     C                   MOVEL     ANMNSP        KNSP2
198000961217     C                   Z-ADD     ANMCAA        KCAA
198100961217     C*
198200961217     C* LEGGO IL COLLO FINTANTO CHE TROVO DELLE ANOMALIE INSERITE
198300961217     C*  NEL CASO STAMPERO' QUELLA
198400990512     C     KANM1         SETLL     FNANM01L
198500990512     C     KANM1         READE     FNANM01L                               31
198600961217    3C     *IN31         DOWEQ     *OFF
198700990512     C     ANMDAO        ANDLE     W0080
198800990512     C     KANM1         READE     FNANM01L                               31
198900961217    3C                   ENDDO
199000961217     C*
199100961217    3C     *IN31         IFEQ      *ON
199200990512     C   27ANMNR7        CHAIN(N)  FNANM000                           33
199300990512     C   28ANMNR8        CHAIN(N)  FNANM000                           33
199400961217     C*
199500961217     C                   CLEAR                   ANMDCH
199600961217     C                   CLEAR                   ANMFSC
199700961217   X3C                   ELSE
199800961217     C                   SETON                                        10
199900961217     C                   GOTO      ENDCON
200000961217    3C                   ENDIF
200100961217    2C                   ENDIF
200200961217    1C                   ENDIF
200300961217     C*
200400961217    1C     D34APR        IFEQ      'C'
200500961217     C     ANMCCH        ANDEQ     '  '
200600961217     C                   SETON                                        10
200700961217     C                   GOTO      ENDCON
200800961217    1C                   ENDIF
200900961217     C*
201000961217    0C                   ENDIF
201100960419     C**
201200960426     C* 7) TUTTI GLI ALTRI CONFRONTI, TRANNE IL LIVELLO IDD NON FANNO
201300960419     C*    NE STAMPARE NE TRASMETTERE
201400960419     C**
201500960327     C* DATA CHIUSURA ANOMALIA DAL/AL
201600960927     C* PER LE ANOMALIE NON IDD3
201700960927     C     *IN05         IFEQ      *ON
201800960927     C     ANMCCH        ANDEQ     'PR'
201900960927     C     ANMLNA        LOOKUP    FM1                                    32
202000960927     C                   ENDIF
202100960927     C*
202200960927    0C     *IN05         IFEQ      *OFF
202300960927     C     ANMCCH        ORNE      'PR'
202400960927     C     *IN32         OREQ      *OFF
202500960927     C*
202600960327    1C     D34DCA        IFGT      0
202700960419     C     D34ACA        ORGT      0
202800990512    2C     ANMDCH        IFLT      D34DCA
202900990512     C     ANMDCH        ORGT      D34ACA
203000960327     C                   SETON                                        10
203100960327     C                   GOTO      ENDCON
203200960327    2C                   ENDIF
203300960327    1C                   ENDIF
203400960927     C*
203500960927   X0C                   ELSE
203600990512    1C     ANMDCH        IFLT      W08DM1
203700990512     C     ANMDCH        ORGT      W08AM1
203800960927     C                   SETON                                        10
203900960927     C                   GOTO      ENDCON
204000960927    1C                   ENDIF
204100960927    0C                   ENDIF
204200921015     C*
204300921015     C* STAMPA FASE SPECIFICA
204400960327    1C     D34FAS        IFNE      ' '
204500960326     C     ANMFSA        ANDNE     D34FAS
204600921015     C                   SETON                                        10
204700921015     C                   GOTO      ENDCON
204800960327    1C                   ENDIF
204900960426     C*
205000960506     C* STAMPA CAUSALE CHIUSURA SPECIFICA
205100960426    1C     D34CCH        IFNE      ' '
205200960426     C     ANMCCH        ANDNE     D34CCH
205300960426     C                   SETON                                        10
205400960426     C                   GOTO      ENDCON
205500960426    1C                   ENDIF
205600961108     C*
205700040116     C* in partenza escludo le anomalie appena create (livello 0 o 2
205800040616     c*  per anom 15) solo per le anomalie IDD e se non prevista la
205900040616     c*  stampa dalla DS D34SL0
206000040116     C** 07ANMLID        IFEQ      0
206100040616     c                   if        d34sl0=' '
206200040119     c                   if        anmlid=§7clid  and §7cidd='S' and
206300040119     c                             wanom='P' and §7cinv=' '
206400961108     C                   SETON                                        10
206500961108     C                   GOTO      ENDCON
206600961108    1C                   ENDIF
206700040616    1C                   ENDIF
206800960419     C*
206900960506     C* STAMPA CODICI ANOMALIE RICHIESTE
207000960327    1C     D34ANM        IFNE      *BLANKS
207100921015     C                   MOVEL     ANMCAA        ALF3              3
207200921015     C     ALF3          LOOKUP    ANM                                    31
207300960327    2C     D34INC        IFEQ      'I'
207400921015     C  N31              SETON                                        10
207500921015     C  N31              GOTO      ENDCON
207600921015     C*
207700960327   X2C                   ELSE
207800921015     C*
207900921015     C   31              SETON                                        10
208000921015     C   31              GOTO      ENDCON
208100960327    2C                   END
208200960402     C*
208300960402     C                   ELSE
208400960402     C*
208500960402     C* SE NON HO RICHIESTO UNO SPECIFICO CODICE ANOMALIA
208600960402     C*  GESTISCO SOLO QUELLI STAMPABILI
208700960402     C     §7CSTA        IFNE      'S'
208800960402     C                   SETON                                        10
208900960402     C                   GOTO      ENDCON
209000960506    2C                   ENDIF
209100960506    1C                   ENDIF
209200921015     C*
209300921015     C* NUMERO FOGLIO VIAGGIO
209400960327    1C     D34NFV        IFNE      0
209500960326     C     ANMNFV        ANDNE     D34NFV
209600921015     C                   SETON                                        10
209700921015     C                   GOTO      ENDCON
209800960327    1C                   ENDIF
209900960506     C*
210000960506     C* SE ANOMALIA INTERNA VEDO SE RICHIESTO PART O ARR
210100960506    1C     ANMAIE        IFEQ      'I'
210200960506     C     D34PAR        ANDNE     ' '
210300960506     C*
210400960506     C* RICHIESTE LE ANOMALIE PARTENZA MA QUESTA E' ANOMALIA ARRIVO
210500960506    2C     D34PAR        IFEQ      'P'
210600960506     C     §7CPAR        ANDEQ     'A'
210700960506     C                   SETON                                        10
210800960506    2C                   ENDIF
210900960506     C* VICEVERSA
211000960506    2C     D34PAR        IFEQ      'A'
211100960506     C     §7CPAR        ANDEQ     'P'
211200960506     C                   SETON                                        10
211300960506    2C                   ENDIF
211400960506     C*
211500960506     C   10              GOTO      ENDCON
211600960506    1C                   ENDIF
211700040615     c
211800980327     C* IN CASO DI TRASFERIMENTO DI FILIALE
211900040615    1C**   D34TRA        IFEQ      'S'
212000980327     C* ANOMALIA ESTERNA
212100040615     C**   ANMAIE        ANDEQ     'E'
212200980327     C* ARRIVO
212300040615    2C**   WANOM         IFEQ      'A'
212400980327     C* PRENDO O LNP O FLS
212500040615    3C**   ANMLNP        IFGT      0
212600040615     C**                 Z-ADD     ANMLNP        W0030             3 0
212700040615     C**                 ELSE
212800040615     C**                 Z-ADD     ANMFLS        W0030
212900040615    3C**                 ENDIF
213000980327     C*
213100040615     C**   W0030         LOOKUP    L6                                     32
213200040615     C**N32W0030         LOOKUP    L1                                     34
213300040116     C**N32
213400040116     C** 34              MOVEL     'R'           WRTF
213500040116     C* N32
213600040116     C*N 34              MOVEL     'E'           WRTT
213700040615     C* N32
213800040615     C*N 34              MOVEL     'U'           NODEL
213900040615    2C**                 ENDIF
214000980327     C* PARTENZE
214100040615    2C**   WANOM         IFEQ      'P'
214200040615     C**   ANMLNA        LOOKUP    L6                                     32
214300040615     C**N32ANMLNA        LOOKUP    L1                                     34
214400040116     C**N32
214500040116     C*N 34              MOVEL     'R'           WRTT              1
214600040615     C**N32
214700040615     C*N 34              MOVEL     'E'           WRTF              1
214800040615     C**N32
214900040615     C*N 34              MOVEL     'U'           NODEL
215000040615    2C**                 ENDIF
215100980327     C*
215200040615    1C**                 ENDIF
215300960403     C* DAL LIVELLO IDD
215400980327     C     *IN97         IFEQ      *ON
215500980327     C     *IN01         ANDEQ     *ON
215600980327     C     D34TRA        OREQ      'S'
215700000317     C     D34GES        OREQ      'V'
215800990114     C   24
215900990512     COR 25ANMN10        CHAIN     FNANM000                           33
216000000306     C   26ANMNR3        CHAIN     FNANM000                           33
216100000306     C   27ANMNR7        CHAIN     FNANM000                           33
216200990512     C   28ANMNR8        CHAIN     FNANM000                           33
216300990512     C   29ANMNR9        CHAIN     FNANM000                           33
216400960403     C                   ENDIF
216500960403     C*
216600960404    1C   97D34ELA        IFNE      ' '
216700960403     C                   ADD       1             ANMLID
216800960403     C                   ENDIF
216900960403     C**
217000960405     C   97ANMLID        IFLT      D34LID
217100960405     C* SE NON E' ELABORAZIONE DEFINITIVA IDD 10 ON
217200960405     C     D34ELA        IFNE      'S'
217300960403     C                   SETON                                        10
217400960417     C                   GOTO      ENDCON
217500960405     C                   ELSE
217600960419     C* ALTRIMENTI ACCENDO L' 11  PERCHE' DEVO AGGIORNARE SENZA STAMPAR
217700960405     C                   SETON                                        11
217800960405     C                   ENDIF
217900960405     C*
218000960403    2C                   ENDIF
218100960412     C*
218200960327     C     ENDCON        ENDSR
218300040616     c     CercaCAE      begsr
218400040616     c     kcae1         setll     azcae01l
218500040616     c     kcae3         readpe    azcae01l
218600040616    3c                   dow       not %eof(azcae01l) and caeatb<>*blanks
218700040616     c     kcae3         readpe    azcae01l
218800040616    3c                   enddo
218900040616     c
219000040616    3c                   if        not %eof(azcae01l)
219100040616    4c                   if        caedde<=anmdao   and caedsc>=anmdao
219200040616     c* Trovata eccezione terminal --> se il terminal  d34fgs esco
219300040616    5c                   if        caetfe=d34fgs
219400040616     c* Anomalia da non stampare
219500040616     c                   eval      wanomok='N'
219600040616     c                   else
219700040616     c* Anomalia da     stampare
219800040616     c                   eval      wanomok='S'
219900040616    5c                   endif
220000040616    4c                   endif
220100040616    3c                   endif
220200040616     c                   ENDSR
220300960927     C*
220400960927     C* CONTROLLO SE C'E' DISGUIDO IN LETTURA ANOMALIE PARTENZA ------*
220500960930     C     DISPAR        BEGSR
220600971124     C* VEDO SE E' PRESENTE UNA ANOMALIA DI DISGUIDO
220700990409     C                   SETOFF                                       20
220800990409     C                   DO        2
220900990409     C                   Z-ADD     56            KCAA
221000990409     C     *IN20         IFEQ      *OFF
221100990512     C     KANM2         CHAIN     FNANM02L                           32
221200960927    1C     *IN32         DOWEQ     *OFF
221300971126    2C     AN2DAO        IFGT      WDDET
221400960927     C                   SETON                                        20
221500960927     C                   MOVEL     CDISG         PSTAM
221600960927     C                   MOVEL     AN2FLE        WDFLP
221700971126     C                   Z-ADD     AN2DAO        WDDET
221800960927    2C                   ENDIF
221900990512     C     KANM2         READE     FNANM02L                               32
222000960927    1C                   ENDDO
222100990409     C*
222200990409     C                   Z-ADD     55            KCAA
222300990409    2C                   ENDIF
222400990409    1C                   ENDDO
222500960927     C*
222600960927     C* SE NON TROVATA CONTROLLO BLTFLP
222700960927    1C     *IN20         IFEQ      *OFF
222800960927     C     BLTDET        ANDGT     0
222900960927     C**
223000960927     C* CHAIN IL FV
223100020807     C     K02FGV01      CHAIN     FNFGV01L                           32
223200960927    4C     *IN32         IFEQ      *OFF
223300020807     C     K02FGV01      CHAIN     FNFGW01L                           32
223400000503    5C     *IN32         IFEQ      *ON
223500000503     C     FGWATB        ORNE      ' '
223600000503     C                   CLEAR                   FGWFF3
223700000503     C                   CLEAR                   FGWFF4
223800000503     C                   CLEAR                   FGWFL3
223900000503     C                   CLEAR                   FGWFL4
224000000503    5C                   ENDIF
224100960927     C                   MOVEL     BLTFLP        W003A             3
224200960930     C                   MOVEL     ANMLNA        W003AA            3
224300961001     C                   Z-ADD     0             XX                3 0
224400961001     C                   SETON                                        32
224500961001     C*
224600961001    5C     *IN32         DOWEQ     *ON
224700961001     C     PSTAM         ANDEQ     *BLANKS
224800961001     C                   ADD       1             XX
224900960927     C     W003A         LOOKUP    FLP(XX)                                32
225000960927     C**
225100961001    6C     *IN32         IFEQ      *ON
225200960930     C     FFV(XX)       ANDEQ     W003AA
225300960930     C                   MOVEL     CTRANS        PSTAM            18
225400961001    6C                   ENDIF
225500961001    5C                   ENDDO
225600961001     C**
225700961001    5C     PSTAM         IFEQ      *BLANKS
225800960927     C                   MOVEL     CDISG         PSTAM
225900960927    5C                   ENDIF
226000960927   X4C                   ELSE
226100960927     C                   MOVEL     CDISG         PSTAM
226200960927    4C                   ENDIF
226300960927     C                   SETON                                        20
226400960927     C                   MOVEL     BLTFLP        WDFLP
226500960927     C                   MOVEL     BLTDET        WDDET
226600960927    1C                   ENDIF
226700960927     C                   ENDSR
226800960508     C*
226900960508     C* CHAIN SULLE TABELLE DELL'ANOMALIA ---------------------------*
227000960508     C     LEGCAA        BEGSR
227100960508     C     ANMCAA        IFNE      SAVCAA
227200960508     C                   MOVEL     '7C'          COD
227300960508     C                   MOVEL(P)  ANMCAA        KEY
227400960508     C     KTAB          CHAIN(N)  TABEL                              31
227500960508     C  N31              MOVEL     TBLUNI        DS7C
227600960508     C   31              CLEAR                   DS7C
227700960508     C                   MOVEL     ANMCAA        SAVCAA
227800960508     C                   ENDIF
227900960508     C                   ENDSR
228000911015     C*
228100960827     C* CHIUDO ANOMALIE DI DISGUIDO O ANOMALIE 50 SE ESISTE BLP ------*
228200960827     C     A50A55        BEGSR
228300021025     C                   SETOFF                                       32
228400021025     C     KART          CHAIN     FNBLT27L                           39
228500021025     C  N39BLTLNP        LOOKUP    L1                                     32
228600021025    2C     *IN32         IFEQ      *ON
228700960705     C* TROVATA BOLLA PARTENZA:
228800990409     C* CHIUDO ANOMALIA 55 O 56
228900960705     C                   MOVE      'N'           WTRM              1
229000960705     C                   CLEAR                   DSLR33
229100960705     C                   MOVE      ANMDAO        D33DCH
229200960705     C                   MOVEL     'M'           D33FSC
229300960705     C                   MOVEL     'AN'          D33CCH
229400960705     C                   MOVEL     'S'           D33MAC
229500990114     C   24
229600990114     COR 25              MOVE      ANMN10        D33NRR
229700000306     C   26              MOVE      ANMNR3        D33NRR
229800000306     C   27              MOVE      ANMNR7        D33NRR
229900960709     C   28              MOVE      ANMNR8        D33NRR
230000960709     C   29              MOVE      ANMNR9        D33NRR
230100960705     C                   CALL      'FNLR33R'
230200960705     C                   PARM                    DSLR33
230300960827     C* SE ANOMALIA 55:
230400960827    3C     ANMCAA        IFEQ      55
230500990409    3C     ANMCAA        OREQ      56
230600960705     C* CHIUDO ANOMALIA 200
230700960709     C                   MOVE      KCAA          WCAA
230800960705     C                   Z-ADD     200           KCAA
230900990512     C     KANM2         CHAIN     FNANM02L                           32
231000960705     C     *IN32         IFEQ      *OFF
231100960705     C                   CLEAR                   DSLR33
231200960705     C                   MOVE      AN2DAO        D33DCH
231300960705     C                   MOVEL     'AN'          D33CCH
231400960705     C                   MOVE      ANMNR2        D33NRR
231500960705     C                   CALL      'FNLR33R'
231600960705     C                   PARM                    DSLR33
231700960705     C                   END
231800960709     C                   MOVE      WCAA          KCAA
231900960827    3C                   END
232000960705    2C                   END
232100960705     C                   ENDSR
232200960705     C*
232300911021     C* STAMPO ANOMALIE ---------------------------------------------*
232400900621     C     STAMPA        BEGSR
232500151125     c                   setoff                                       434445
232600151125     c                   clear                   wdcm
232700960405     C* PER IDD
232800911025     C* SE DIVENTA 3 LA CHIUDO CON CAUSALE PR = PRATICA
232900960405    1C   97D34ELA        IFNE      ' '
233000960403    2C     ANMLID        IFEQ      §7ALCI
233100990512     C                   MOVEL     D34ALA        ANMDCH
233200911025     C                   MOVEL     'PR'          ANMCCH
233300921027     C                   MOVEL     'D'           ANMFSC
233400960327    2C                   ENDIF
233500960327    1C                   ENDIF
233600960514     C*
233700960514     C* SE IL SOGGETTO ANOMALIA E' "E" --> NON STAMPO L'ANOMALIA
233800960515     C*                                    SE IDD IN ARRIVO
233900040116    c
234000040116    3C     ANMSAN        IFNE      'E'
234100040116     C     wanom         ANDEQ     'A'
234200040116     C     ANMAIE        ORNE      'E'
234300040116     C     wanom         orne      'A'
234400911025     C*
234500911021     C* NUMERO SPEDIZIONE
234600911021     C     ANMNSP        COMP      0                                  0202
234700911021     C* NUMERO SEGNACOLLO
234800970314     C     ANMFLS        COMP      0                                  0303
234900970314     C     ANMCAA        COMP      300                                    22
235000960417     C**
235100960417     C* SE C'E' IL NUMERO DI SPEDIZIONE, VADO A PRENDERE LA DATA
235200960514    2C     ANMNSP        IFGT      0
235300960417     C                   SETOFF                                       33
235400960417     C                   CLEAR                   G02INV
235500960417     C*
235600960417     C     KANM          CHAIN     FNBLP01L                           33
235700960514    4C     *IN33         IFEQ      *OFF
235800960509     C                   MOVEL     BLPZNC        PRTZNC
235900960417     C                   Z-ADD     BLPMGS        G02INV
236000971126     C                   MOVEL     BLPAAS        G02INV
236100050503     c* Se c'e' particolarità consegna CP, la stampo
236200050503     c                   if        blpgma='CP'
236300050503     c                   seton                                        43
236400050503     c                   endif
236500151125     c* Se consegna anomala 7 stampo
236600151125     c                   if        blpcca='7' and blpdcm>0
236700151125     c                   seton                                        45
236800151125     c                   movel     blpdcm        wdcm              8 0
236900151125     c                   endif
237000151125     c
237100051221     c* Se richiesto stampo dati mittente e destinatario
237200051221     c                   if        d34med = 'S' and d34ela = ' '
237300051221     c                   seton                                        44
237400051221     c                   movel     blprsm        prtrsm           32
237500051222     c* rmn : lo stampo eliminando gli spazi a sinistra non significativi
237600051222     c                   move      blprmn        prtrmn           15
237700051222     c     ' 0'          check     prtrmn        N                 2 0
237800051222     c                   eval      prtrmn = %subst(prtrmn:N)
237900051221     c                   endif
238000050503     c
238100960514    4C                   ENDIF
238200960417     C*
238300961018     C                   CLEAR                   PRTDSP
238400960514    3C     G02INV        IFGT      0
238500960417     C                   MOVEL     3             G02ERR
238600971126     C                   CALL      'XSRDA8'
238700960417     C                   PARM                    WLBDAT
238800960417     C                   MOVEL     G02DAT        PRTDSP            4 0
238900960514    3C                   ENDIF
239000151125     c*
239100151125    3c                   if        wdcm>0  and *in45
239200151125     c                   eval      g02inv=wdcm
239300151125     C                   MOVEL     3             G02ERR
239400151125     C                   CALL      'XSRDA8'
239500151125     C                   PARM                    WLBDAT
239600151125     C                   MOVEL     G02DAT        PRTDcm            4 0
239700151125    3C                   ENDIF
239800151125    2C                   ENDIF
239900960508     C*
240000960508     C* PER L'ANOMALIA 005 VERIFICO SE IL COLLO E' ANDATO IN DISGUIDO
240100040116     C***  ANMAIE        IFNE      'R'
240200040116     C     ANMCAA        ifeq      005
240300960508     C                   EXSR      ANM005
240400960514    2C                   ENDIF
240500960927     C*
240600960508     C* SE HO TROVATO IL DISGUIDO LO STAMPO
240700960514    2C     *IN20         IFEQ      *ON
240800960508     C                   MOVE      WDDET         G02INV
240900960508     C                   MOVEL     3             G02ERR
241000971126     C                   CALL      'XSRDA8'
241100960508     C                   PARM                    WLBDAT
241200960508     C                   MOVEL     G02DAT        PRTDET            4 0
241300960514    2C                   ENDIF
241400911021     C* TIPO ANOMALIA
241500960514    2C     ANMTAN        IFNE      SAVTAN
241600911021     C                   MOVEL     '7F'          COD
241700960327     C                   MOVEL(P)  ANMTAN        KEY
241800960409     C     KTAB          CHAIN(N)  TABEL                              31
241900040616     C   31              MOVEL     *BLANKS       tbluni
242000040616     c* Vedo semettere o meno il tipo di anomalia (Idd/int/no idd)
242100040616     c                   if        *in17
242200040616     C                   MOVEL     TBLUNI        DESTAN           12
242300040616     c                   else
242400040616     c                   movel     tbluni        w008a             8
242500040616     c                   endif
242600921015     C*
242700921015     C                   MOVEL     ANMTAN        SAVTAN            1
242800960514    2C                   END
242900040616     c*
243000040616     c                   if        not *in17
243100040616     c                   clear                   destan
243200040616     C* ANOMALIA
243300040616    2C     ANMAIE        IFEQ      'I'
243400040616     C                   MOVEL     'INT'         DESAIE            3
243500040616   X2C                   ELSE
243600040616     C**
243700040616    3C     §7CIDD        IFEQ      'S'
243800040616     C                   MOVEL     'IDD'         DESAIE                         IDD
243900040616     C                   ELSE
244000040616     C                   MOVEL     'NON'         DESAIE                         NON IDD
244100040616    3C                   ENDIF
244200040616    2C                   ENDIF
244300040616     c                   movel     desaie        destan
244400040616     c                   move      w008a         destan
244500040616     c                   endif
244600960506     C*
244700960506     C* DESCRIZIONE CODICE ANOMALIA
244800960506     C                   MOVEL     §7CDES        DESCAA           25
244900960704     C*
245000960705     C* SE ANOMALIA 10-MANCA RECORD BOLLA ARRIVO O 138-COLLO GIA' CONS.
245100960705     C* CONTROLLO SE COLLO SPARATO IN INVENTARIO O IN CONSEGNA
245200960704     C                   SETOFF                                       21
245300960704     C     ANMCAA        IFEQ      10
245400960705     C     ANMCAA        OREQ      138
245500960704     C     ANMLNA        LOOKUP    L6                                     32
245600960704     C     *IN32         IFEQ      *ON
245700960704     C                   EXSR      RCRSPU
245800960704     C                   END
245900960704     C                   END
246000960402     C*
246100950320     C* SE BOLLA IN CONSEGNA REPERISCO NR/DATA DISTINTA E PADRONCINO
246200950320     C                   SETOFF                                       09
246300960514    2C     ANMCAA        IFEQ      135
246400950321     C     KART          CHAIN     FNART27L                           31
246500960514    3C     *IN31         IFEQ      *OFF
246600950320     C     KARB          CHAIN     FNARB01L                           31
246700960514    4C     *IN31         IFEQ      *OFF
246800960509     C                   MOVEL     ARBZNC        PRTZNC
246900960509     C*
247000960514    5C     ARBNDC        IFNE      0
247100950320     C                   SETON                                        09
247200950320     C*
247300950320     C                   MOVEL     DESCAA        DESCA1           18
247400950320     C                   MOVE      ':'           DESCA1
247500950320     C* Data distinta
247600950320     C                   MOVE      ARBDDC        G02INV
247700950320     C                   MOVEL     3             G02ERR
247800971126     C                   CALL      'XSRDA8'
247900950320     C                   PARM                    WLBDAT
248000950320     C                   MOVEL     G02DAT        STADDC            4 0
248100950320     C* Padroncino
248200950320     C                   MOVE      *BLANKS       STAPDC           12
248300021203     C*    ARBPDC        CHAIN     FNAPD01L                           32
248400021205     C                   move      arbpdc        kpdpdr
248500021203     C                   move      'A'           kpdtip
248600021203     C     kfapd         chain     fiapd01L                           32
248700960327     C  N32              MOVEL     APDRSC        STAPDC
248800960514    5C                   END
248900960514    4C                   END
249000960514    3C                   END
249100960514    2C                   END
249200990121     C**
249300990121     C** NUMERO CA SE C'E'
249400990122     C                   SETOFF                                       404142
249500990121     C     KART          CHAIN     FNDCD02L                           32
249600990121    2C     *IN32         IFEQ      *OFF
249700990121     C     DCDATB        ANDEQ     ' '
249800990121     C     KDCT          CHAIN     FNDCT01L                           32
249900050221      * se dctdt2 > 0 la c.a. non è valida x la filiale
250000050221     c                   If        Not *In32 and dctdt2 > 0
250100050221     c                   Eval      *In32 = *On
250200050221     c                   EndIf
250300990121     C* CONTROLLO LA CAUSALE DI CHIUSURA SE C'E'
250400990121    3C  N32DCTDCH        IFGT      0
250500990121     C     DCTCCH        ANDNE     ' '
250600990121     C                   CLEAR                   DSBS02
250700990121     C                   MOVEL     'C'           T02MOD
250800990121     C                   MOVEL     KNSIF         T02SIF
250900990121     C                   MOVEL     'CCH'         T02COD
251000990121     C                   MOVEL     DCTCCH        T02KE1
251100990121     C                   CALL      'TIBS02R'
251200990121     C                   PARM                    KPJBA
251300990121     C                   PARM                    DSBS02
251400990121    4C     T02ERR        IFEQ      ' '
251500990121     C                   MOVEL     T02UNI        DCCH
251600990121     C                   ELSE
251700020905     ***C                   MOVEL     'S'           §CHCRA
251800020905     C                   MOVEL     'S'           §CCHCRAN
251900990121    4C                   ENDIF
252000990121    3C                   ENDIF
252100990121     C**
252200990121     C** STAMPO SE CHIUSA C.A. SENZA CAUSALE
252300990122    3C  N32DCTCCH        IFEQ      *BLANKS
252400990121     C     DCTDCH        ANDGT     0
252500990121     C** STAMPO SE CHIUSA C.A. CON CAUSALE PER CUI CREARE ANOMALIA
252600990121     C*  (SIGNIFICA CHE E' CA REALE)
252700020905     ***C     §CHCRA        OREQ      'S'
252800020905     C     §CCHCRAN      OREQ      'S'
252900990121     C     DCTDCH        ANDGT     0
253000990122     C* STAMPO SE C.A. APERTA E DETT C.A. APERTO
253100990122     C     DCTDCH        OREQ      0
253200990122     C     DCDDCH        ANDEQ     0
253300990122     C* DECODIFICA DANNI
253400990122     C                   EXSR      DECDAN
253500990122    3C                   ENDIF
253600990121    2C                   ENDIF
253700050503     c
253800050503     C* VERIFICO PER CP SE STAMPARE decodifica corta o lunga
253900050503     c                   clear                   decodicp         23
254000050503     c                   if        *in43
254100050503     c                   if        *in41
254200050503     c                   eval      decodicp=CCPcort
254300050503     c                   else
254400050503     c                   eval      decodicp=CCPlung
254500050503     c                   endif
254600050503     c                   endif
254700990122     C**
254800990122     C* SE NON E' SU C.A. PER LA 151 STAMPO LA 1 C.A. CHIUSA CHE TROVO
254900990122    2C     *IN32         IFEQ      *ON
255000990122     C     ANMNSP        ANDGT     0
255100990122     C     ANMCAA        ANDEQ     151
255200990122     C                   CLEAR                   WTRDAN
255300990122     C**
255400040811     C**""**     KANM          CHAIN     FNDCT02L                           32
255500040811    3C**""**     *IN32         DOWEQ     *OFF
255600040811    4C**""**     DCTATB        IFNE      ' '
255700040811     C**""**     DCTDCH        OREQ      0
255800040811     C**""**     KANM          READE     FNDCT02L                               32
255900040811     C**""**                   ELSE
256000040811     C**""**                   SETON                                        32
256100040811     C**""**                   MOVEL     'S'           WTRDAN            1
256200040811    4C**""**                   ENDIF
256300040811    3C**""**                   ENDDO
256400990122     C**
256500040811      * Sostituisco la lettura del file FNDTC02 con
256600040811      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul e chiuse
256700040811      * legate alla spedizione che passo
256800040811      *
256900040811     c                   clear                   fidn12ds
257000040811     c                   eval      i12aas = anmaas
257100040811     c                   eval      i12lnp = anmlnp
257200040811     c                   eval      i12nrs = anmnrs
257300040811     c                   eval      i12nsp = anmnsp
257400040811     c                   eval      i12fel = 'E'
257500040811     c                   eval      i12fan = 'E'
257600040811      *
257700040811     c                   call      'FIDN12R'
257800040811     c                   parm                    fidn12ds
257900040811      *
258000040811      * se non ci sono errori
258100040811     c                   if        o12err = ' '
258200040811      * se numero di CA trovate maggiore di zero
258300040811     c                   if        o12nca > 0
258400040811      * prendo solo la prima CA chiusa
258500040811     c                   z-add     *zeros        jj                2 0
258600040811     c     1             do        o12nca        jj
258700040811     c                   if        skdch(jj) > 0
258800040811     c                   movel     'S'           wtrdan            1
258900040811     c                   movea     skkey(jj)     dskey
259000040811     C                   Z-ADD     dsfil         dctfil
259100040811     C                   Z-ADD     dsnca         dctnca
259200040811     C                   Z-ADD     skdca(jj)     dctdca
259300040811     C                   Z-ADD     skdch(jj)     dctdch
259400040811     c                   z-add     o12nca        jj
259500040811     c                   endif
259600040811     C                   ENDDO
259700040811      *
259800040811     c                   endif
259900040811     c                   endif
260000040811     C*
260100990122     C** TROVATA LA C.A. CHIUSA: STAMPO I DATI
260200990122     C     WTRDAN        IFEQ      'S'
260300990122     C                   EXSR      DECDAN
260400990122    3C                   ENDIF
260500990122    2C                   ENDIF
260600911021     C* FASE APERTURA
260700960514    2C     ANMFSA        IFNE      SAVFSA
260800911021     C                   MOVEL     '7D'          COD
260900960327     C                   MOVEL(P)  ANMFSA        KEY
261000960409     C     KTAB          CHAIN(N)  TABEL                              32
261100040616     C  N32              MOVEL     TBLUNI        DESFSA            8
261200960327     C   32              MOVEL     *BLANKS       DESFSA
261300921015     C*
261400921015     C                   MOVEL     ANMFSA        SAVFSA            1
261500960514    2C                   END
261600911021     C* DATA APERTURA
261700971126     C                   CLEAR                   G02INV
261800911021     C                   MOVE      ANMDAO        G02INV
261900911018     C                   MOVEL     3             G02ERR
262000971126     C                   CALL      'XSRDA8'
262100911018     C                   PARM                    WLBDAT
262200950320     C                   MOVEL     G02DAT        STADAO            4 0
262300911021     C* FASE CHIUSURA
262400960514    2C     ANMFSC        IFNE      SAVFSC
262500911021     C                   MOVEL     '7D'          COD
262600960327     C                   MOVEL(P)  ANMFSC        KEY
262700960409     C     KTAB          CHAIN(N)  TABEL                              32
262800040616     C  N32              MOVEL     TBLUNI        DESFSC            8
262900960327     C   32              MOVEL     *BLANKS       DESFSC
263000921015     C*
263100921015     C                   MOVEL     ANMFSC        SAVFSC            1
263200960514    2C                   END
263300960506     C*
263400960508     C** SE RICEVUTA STAMPO ANMFLE
263500040116    2C**   ANMAIE        IFEQ      'R'
263600040116     C**                 SETON                                        04
263700040116     C**                 ELSE
263800040116     C**                 SETOFF                                       04
263900040116    2C**                 ENDIF
264000040116     c* lo faccio se sto leggendo le anomalie partenza
264100040616    2C**   wanom         IFEQ      'P'
264200040616     C**                 SETON                                        04
264300040616     C**                 ELSE
264400040616     C**                 SETOFF                                       04
264500040616    2C**                 ENDIF
264600911021     C* DATA CHIUSURA
264700960514    2C     ANMDCH        IFGT      0
264800971126     C                   CLEAR                   G02INV
264900911021     C                   MOVE      ANMDCH        G02INV
265000911021     C                   MOVEL     3             G02ERR
265100971126     C                   CALL      'XSRDA8'
265200911021     C                   PARM                    WLBDAT
265300950320     C                   MOVEL     G02DAT        STADCH            4 0
265400960417     C                   ELSE
265500960417     C                   CLEAR                   STADCH
265600960514    2C                   ENDIF
265700911021     C* CAUSALE CHIUSURA
265800960514    2C     ANMCCH        IFNE      SAVCCH
265900970523     C     ANMCCH        IFNE      'RA'
266000911021     C                   MOVEL     '7E'          COD
266100960417     C                   MOVEL(P)  ANMCCH        KEY
266200960409     C     KTAB          CHAIN(N)  TABEL                              31
266300960417     C  N31              MOVEL     TBLUNI        DESCCH           12
266400911021     C   31              MOVEL     *BLANKS       DESCCH
266500970523     C                   ELSE
266600970523     C                   CLEAR                   DESCCH
266700970523     C                   END
266800921015     C*
266900921015     C                   MOVEL     ANMCCH        SAVCCH            2
267000960514    2C                   END
267100970523     C*
267200970523     C     ANMCCH        IFEQ      'RA'
267300970523     C                   CLEAR                   CCHPRT
267400970523     C                   ELSE
267500970523     C                   MOVE      ANMCCH        CCHPRT
267600970523     C                   END
267700040616     C* DATA/ora creazione anomalia
267800040616     c                   clear                   stadac
267900040616     c                   clear                   staorc
268000040616     c                   if        anmflo<>*blanks
268100040616     c                   movel     anmflo        danmflo
268200040616     C                   CLEAR                   G02INV
268300040616     C                   MOVE      §anmdac       G02INV
268400040616     C                   MOVEL     3             G02ERR
268500040616     C                   CALL      'XSRDA8'
268600040616     C                   PARM                    WLBDAT
268700040616     C                   MOVEL     G02DAT        STADAc            4 0
268800040616     c                   move      §anmorc       staorc            4 0
268900040616     c                   endif
269000970523     C*
269100960327     C**
269200960403     C* SOLO SE ANOMALIA ESTERNE IDD STAMPO IL LIVELLO IDD
269300960404     C   97              MOVEL     ANMLID        STALID            1
269400960404     C  N97              CLEAR                   STALID
269500960509     C**
269600960509     C* SE NON L'HO ANCORA TROVATA PRENDO LA ZONA DALLE SPUNTE
269700960514    2C     PRTZNC        IFEQ      '  '
269800070111     C     KART          SETLL     FNBRV07L
269900070111     C     KART          READE     FNBRV07L                               32
270000960514    3C     *IN32         DOWEQ     *OFF
270100960509     C     PRTZNC        ANDEQ     '  '
270200960514    4C     BRVZNC        IFGT      0
270300960509     C                   MOVEL     BRVZNC        PRTZNC
270400960509     C                   ELSE
270500070111     C     KART          READE     FNBRV07L                               32
270600960514    4C                   ENDIF
270700960514    3C                   ENDDO
270800960514    2C                   ENDIF
270900990512     C* ANNO A 2
271000990512     C   02              MOVE      ANMAAS        PRTAAS            2 0
271100911025     C*
271200900622     C* STAMPO DETTAGLIO
271300921015     C*
271400921015     C* STAMPA TESTATA
271500960514    2C     D34ELA        IFNE      ' '
271600960403     C     ANMLID        ANDLT     §7ALCI
271700960327     C     D34ELA        OREQ      ' '
271800960327     C*
271900040615     c                   if        *inOF
272000040615     C                   EXCEPT    TES
272100040615     C  n70              EXCEPT    TES2
272200040615     C                   EXCEPT    TES3
272300040615     C                   SETOFF                                       OF
272400040615     c                   endif
272500921015     C*
272600921015     C                   EXCEPT    DET
272700050503     c   41
272800050503     cor 43              except    det1
272900051221     c   44              except    det2
273000151125     c   45              except    det3
273100050503     c
273200031007     C   55              EXSR      WRIDET
273300960422     C                   MOVEL     '1'           WSTAMP            1
273400921015     C*
273500960514   X2C                   ELSE
273600921015     C*
273700921015     C   OE              EXCEPT    TESX
273800921015     C   OE              SETOFF                                       OE
273900921015     C*
274000911025     C                   EXCEPT    DETX
274100050503     c   41
274200050503     Cor 43              EXCEPT    DETX1
274300031007     C   55              EXSR      WRIDET
274400960514    2C                   ENDIF
274500900622     C*
274600040616     C**N17              SETOFF                                       04
274700040401     c* Scrivo file per report di sede
274800040401     c     wgir200       ifeq      'S'
274900040407     c     *in01         andeq     *on
275000040401     c                   exsr      srant
275100040401     c                   endif
275200960514    1C                   ENDIF
275300900622     C                   ENDSR
275400990122     C*
275500990122     C* DECODIFICA DATI RECORD C.A. ----------------------------------*
275600990122     C     DECDAN        BEGSR
275700990122     C* SE 09 20 O 21 STAMPO SU 2 RIGA ALTRIMENTI SULLA PRIMA
275800990122    1C     *IN09         IFEQ      *OFF
275900990122     C     *IN20         ANDEQ     *OFF
276000990122     C     *IN21         ANDEQ     *OFF
276100990122     C                   SETON                                        40
276200990122     C                   MOVEL     DESCAA        DESCA1           18
276300990122     C                   MOVE      ':'           DESCA1
276400990122     C                   ELSE
276500990122     C                   SETON                                        41
276600990122    1C                   ENDIF
276700990122     C**
276800990122     C* Data C.A.
276900990122     C                   MOVE      DCTDCA        G02INV
277000990122     C                   MOVEL     3             G02ERR
277100990122     C                   CALL      'XSRDA8'
277200990122     C                   PARM                    WLBDAT
277300990122     C                   MOVEL     G02DAT        STADCA            6 0
277400990122     C                   Z-ADD     G02DAT        W0020             2 0
277500990122     C                   MOVE      W0020         STADCA
277600990122     C* Data CHIUSURA C.A.
277700990122    1C     DCTDCH        IFGT      0
277800990122     C                   MOVE      DCTDCH        G02INV
277900990122     C                   MOVEL     3             G02ERR
278000990122     C                   CALL      'XSRDA8'
278100990122     C                   PARM                    WLBDAT
278200990122     C                   MOVEL     G02DAT        STACHA            4 0
278300990122     C                   SETON                                        42
278400990122     C**
278500990122    1C                   ENDIF
278600990122     C                   ENDSR
278700960508     C*
278800960508     C* CONTROLLO PER ANOMALIA 005 SE E' COLLO IN DISGUIDO ----------*
278900960508     C     ANM005        BEGSR
279000960508     C* PROVO A CHAINARE FNART
279100960508     C     KART          CHAIN     FNART27L                           32
279200960927    1C     *IN32         IFEQ      *OFF
279300960927     C     ARTDET        ANDGT     0
279400960508     C                   SETON                                        20
279500960508     C                   MOVEL     ARTFLP        WDFLP
279600960508     C                   MOVEL     ARTDET        WDDET
279700960807     C*
279800960927     C                   Z-ADD     1             KNPG
279900960927     C* CERCO LA SPUNTA CON FGS=ARTFLP E DATA= ARTDET
280000140618     C     KBRV3         SETLL     FNBRV05L
280100140618     C     KBRV3         READE     FNBRV05L                               32
280200960927    2C     *IN32         DOWEQ     *OFF
280300960509     C* ZONA
280400960927    3C     BRVZNC        IFGT      0
280500960509     C                   MOVEL     BRVZNC        PRTZNC
280600960927    3C                   ENDIF
280700960927     C*
280800000201     C                   Z-ADD     BRVNFV        WNFV
280900960927     C     KFVA          CHAIN     FNFVA01L                           32
281000960927    3C     *IN32         IFEQ      *OFF
281100040617     C     FVADFV        ANDEQ     ARTDUT
281200960927     C                   SETON                                        32
281300960927    4C     BRVATR        IFEQ      'T'
281400960927     C                   MOVEL     CTRANS        PSTAM
281500960927     C                   ELSE
281600960927     C                   MOVEL     CDISG         PSTAM
281700960927    4C                   ENDIF
281800960927   X3C                   ELSE
281900960927     C                   SETOFF                                       32
282000960927    3C                   ENDIF
282100960927     C**
282200140618     C  N32KBRV3         READE     FNBRV05L                               32
282300960927    2C                   ENDDO
282400960927     C**
282500960927     C* SE NON TROVATO METTO DISGUIDO
282600960927     C     PSTAM         IFEQ      *BLANKS
282700960927     C                   MOVEL     CDISG         PSTAM
282800960927     C                   ENDIF
282900960927    1C                   ENDIF
283000960508     C                   ENDSR
283100911021     C*
283200960704     C* RICERCA SPUNTA INVENTARIO O CONSEGNA PER ANOMALIA 10 --------*
283300960704     C     RCRSPU        BEGSR
283400960704     C                   CLEAR                   WNPG
283500960704     C                   CLEAR                   WSPG
283600960704     C                   CLEAR                   WDFV
283700960704     C                   CLEAR                   WFGS
283800960704     C* CERCO SPUNTA INVENTARIO E CONSEGNA E PRENDO QUELLA PIU' RECENTE
283900960704    1C                   DO        2             II                1 0
284000960704    2C     II            IFEQ      1
284100960704     C                   Z-ADD     3             KNPG
284200960704     C                   ELSE
284300960704     C                   Z-ADD     4             KNPG
284400960704    2C                   END
284500140618     C     KBRV          SETLL     FNBRV05L
284600140618     C     KBRV          READE     FNBRV05L                               32
284700960704    2C     *IN32         DOWEQ     *OFF
284800000201     C                   Z-ADD     BRVNFV        WNFV
284900960704     C     KFVV          CHAIN     FNFVV01L                           32
285000960704    3C     *IN32         IFEQ      *OFF
285100960704     C     FVVDFV        ANDGT     WDFV
285200960704     C                   MOVE      FVVNPG        WNPG
285300960704     C                   MOVE      FVVSPG        WSPG
285400960704     C                   MOVE      FVVDFV        WDFV
285500960704     C                   MOVE      FVVFGS        WFGS
285600960704    3C                   END
285700140618     C     KBRV          READE     FNBRV05L                               32
285800960704    2C                   ENDDO
285900960704    1C                   ENDDO
286000960704    1C     WDFV          IFGT      0
286100960704     C                   SETON                                        21
286200960704     C* ROVESCIO DATA SPUNTA
286300960704     C                   CLEAR                   WLBDAT
286400960704     C                   MOVE      WDFV          G02INV
286500960704     C                   MOVEL     3             G02ERR
286600971126     C                   CALL      'XSRDA8'
286700960704     C                   PARM                    WLBDAT
286800960704     C                   MOVEL     G02DAT        PRTDFV            4 0
286900960704     C* IMPOSTO CAMPO PER STAMPA DECODIFICA CATEGORIA/SOTTOCATEGORIA
287000960704    2C     WNPG          IFEQ      3
287100960704     C                   MOVEL     'IM'          W003A             3
287200960704     C                   MOVE      WSPG          W003A
287300960704     C                   MOVEL(P)  W003A         PRTDOV            5
287400960704     C                   ELSE
287500960704     C                   MOVEL(P)  'CONS.'       PRTDOV
287600960704    2C                   END
287700960704    1C                   END
287800960704     C                   ENDSR
287900911021     C* INVIO ANOMALIE ----------------------------------------------*
288000911021     C     ELAB          BEGSR
288100990512     C                   MOVEL     'FNANM00T'    D60FIL
288200971126     C                   Z-ADD     AM8GIO        D60DRF
288300971128     C                   MOVEL(P)  '*SHRUPD'     D60VNC
288400921027     C* FLAG TRASMISSIONE
288500960604    1C     ANMFL1        IFNE      0
288600921027     C                   MOVEL     'T'           ANMFT1
288700000824     C                   MOVE      AM8GIO        ANMDTR
288800960604    1C                   END
288900960604    1C     ANMFL2        IFNE      0
289000921027     C                   MOVEL     'T'           ANMFT2
289100000824     C                   MOVE      AM8GIO        ANMDTR
289200960604    1C                   END
289300921027     C* SI INVIO IDD
289400960523    1C     W7AIDD        IFEQ      'S'
289500960604     C* DISALLOCAZIONE OGGETTI
289600021009     c                   eval      D60po0 = SIMfel
289700961218     C                   CLEAR                   D60FL1
289800961218     C                   CLEAR                   D60FL2
289900960604     C*
290000961218    2C  N95SAVFL1        IFNE      ANMFL1
290100961218     C                   MOVEL     SAVFL1        D60FL1
290200960604    2C                   ENDIF
290300961218    2C  N96SAVFL2        IFNE      ANMFL2
290400961218     C                   MOVEL     SAVFL2        D60FL2
290500960604    2C                   ENDIF
290600960604     C*
290700960604    2C     D60FL1        IFGT      0
290800960604     C     D60FL2        ORGT      0
290900990512     C   99              CLOSE     FNANM00T
291000960604     C   99              SETOFF                                       99
291100960604     C                   MOVEL     'D'           D60AZN
291200021009     C                   MOVEL     FNLV60DS      KPJBU
291300960604     C                   CALL      'FNLV60R'
291400960604     C                   PARM                    KPJBA
291500960604    2C                   ENDIF
291600960604     C* ALLOCAZIONE OGGETTI
291700021009     c                   eval      D60po0 = SIMfel
291800961218     C                   CLEAR                   D60FL1
291900961218     C                   CLEAR                   D60FL2
292000961218     C                   SETOFF                                       9596
292100961218     C* ANMFL1 >0 E IN L1 --> 95 ON
292200120217     c                   if        d34ges<>'V'
292300961218     C     ANMFL1        IFGT      0
292400961218     C     ANMFL1        LOOKUP    L1                                     95
292500961218     C                   ENDIF
292600961218     C* ANMFL2 >0 E IN L1 --> 96 ON
292700961218     C     ANMFL2        IFGT      0
292800961218     C     ANMFL2        LOOKUP    L1                                     96
292900961218     C                   ENDIF
293000120217     c
293100120217     C                   ENDIF
293200960604     C*
293300961218    2C  N95ANMFL1        IFNE      SAVFL1
293400961218     C                   MOVEL     ANMFL1        D60FL1
293500960604    2C                   ENDIF
293600961218    2C  N96ANMFL2        IFNE      SAVFL2
293700961218     C                   MOVEL     ANMFL2        D60FL2
293800960604    2C                   ENDIF
293900960604     C*
294000960604    2C     D60FL1        IFGT      0
294100960604     C     D60FL2        ORGT      0
294200960604     C                   MOVEL     'A'           D60AZN
294300021009     C                   MOVEL     FNLV60DS      KPJBU
294400960604     C                   CALL      'FNLV60R'
294500960604     C                   PARM                    KPJBA
294600021009     C                   MOVEL     KPJBU         FNLV60DS
294700960604     C                   MOVEL     D60ERR        WERR
294800961023     C     D60FL1        IFGT      0
294900960604     C                   MOVEL     D60MB1        WMB1
295000961023     C                   ENDIF
295100961023     C     D60FL2        IFGT      0
295200960604     C                   MOVEL     D60MB2        WMB2
295300961023     C                   ENDIF
295400021009     C     WMB2          IFEQ      *BLANKS
295500040223     C**** D60FL2        OREQ      *zeros
295600961023     C                   CLEAR                   ANMFL2
295700961023     C                   ENDIF
295800960604    2C                   ENDIF
295900960604     C**
296000960604     C* NON CI SONO ERRORI --> SCRIVO E AGGIORNO
296100961219    2C     WERR          IFNE      'S'
296200960604     C                   MOVEL     ANMFL1        SAVFL1
296300960604     C                   MOVEL     ANMFL2        SAVFL2
296400961218    3C  N95SAVFL1        IFGT      0
296500021008     ***C                   MOVEL     WMB1          VAR
296600021009     c                   movel     wMB1          DSmembro
296700021008     c                   movel     DSmembro      VAR
296800950115     C*
296900960604     C     *IN99         IFEQ      *OFF
297000990512     C                   MOVE      VAR           CMD1                           FNANM00T
297100911021     C                   MOVEL     *BLANKS       COMMAN           80
297200911021     C                   MOVEA     CMD1(1)       COMMAN
297300960604     C                   CALL      'QCMDEXC'                              98
297400911021     C                   PARM                    COMMAN
297500911021     C                   PARM                    LUNG
297600960604     C*
297700960604     C   98              MOVEL     'S'           WERR
297800911021     C**
297900911021     C** SCRIVO FILE INVIO
298000911021     C**
298100990512     C  N98              OPEN      FNANM00T
298200960604     C                   ENDIF
298300960604     C*
298400990512     C  N98              WRITE     FNANM0TT                                      INVIO
298500960604     C  N98              SETON                                        99
298600960604    3C                   ENDIF
298700960604    2C                   ENDIF
298800960604     C**
298900960604     C* NON CI SONO ERRORI --> SCRIVO E AGGIORNO
299000961219    2C  N96WERR          IFNE      'S'
299100961023     C     SAVFL2        ANDGT     0
299200961023     C     WMB2          ANDNE     WMB1
299300990512     C                   CLOSE     FNANM00T
299400960604     C                   SETOFF                                       99
299500960604     C**
299600021009     c                   movel     wMB2          DSmembro
299700021008     c                   movel     DSmembro      VAR
299800960604     C*
299900990512     C                   MOVE      VAR           CMD1                           FNANM00T
300000960604     C                   MOVEL     *BLANKS       COMMAN           80
300100960604     C                   MOVEA     CMD1(1)       COMMAN
300200960604     C                   CALL      'QCMDEXC'                              98
300300960604     C                   PARM                    COMMAN
300400960604     C                   PARM                    LUNG
300500960604     C*
300600960604     C   98              MOVEL     'S'           WERR
300700960604     C**
300800960604     C** SCRIVO FILE INVIO
300900960604     C**
301000990512     C  N98              OPEN      FNANM00T
301100990512     C  N98              WRITE     FNANM0TT                                      INVIO
301200990512     C  N98              CLOSE     FNANM00T
301300970306    2C                   ENDIF
301400970306     C** PER REPORT DI SEDE
301500970306    3C     ANMCAA        IFEQ      55
301600990409    3C     ANMCAA        OREQ      56
301700040504     c* la lnp disguido è quella della bolla, se c'e'
301800040504     c                   if        anmlnp>0
301900040504     c                   movel     anmlnp        wlnp
302000040504     c                   else
302100040504     C                   MOVEL     ANMFLS        wLNP
302200040504     c                   endif
302300040504     c
302400970306     C     KA55          CHAIN     FNANT02L                           39
302500970306     C*
302600970306    4C     *IN39         IFEQ      *ON
302700970306     C                   CLEAR                   FNANT000
302800970306     C                   MOVEL     ANMFLE        ANTFOA
302900040504     C****               MOVEL     ANMFLS        ANTLNP
303000040504     C                   MOVEL     wlnp          ANTLNP
303100990512     C                   MOVEL     ANMDAO        ANTDAO
303200970306     C                   MOVEL     ANMCAA        ANTCAA
303300970306     C                   Z-ADD     1             ANTTOT
303400990504     C*
303500990504     C                   CLEAR                   DSLV55
303600990504     C                   MOVEL     'P'           D55TPT
303700040504     C**                 MOVEL     anmfls        D55LIN
303800040504     C                   MOVEL     wlnp          D55LIN
303900990512     C                   MOVEL     ANMDAO        D55DRF
304000990504     C                   CALL      'FNLV55R'
304100990504     C                   PARM                    DSLV55
304200000317     C* PER LE ANOMALIE 55 E 56 ANTLNA CONTIENE IL TERMINAL DI PARTENZA
304300990504     C*  DELLE BOLLE
304400990504     C                   MOVEL     D55TFP        ANTLNA
304500040927     C                   Z-ADD     AM8GIO        antdt1
304600970306     C                   WRITE     FNANT000
304700970306   X4C                   ELSE
304800970306     C                   ADD       1             ANTTOT
304900040927     C****               CLEAR                   ANTFT1
305000040927     C****               CLEAR                   ANTFT2
305100040927     C                   Z-ADD     AM8GIO        antdt1
305200970306     C                   UPDATE    FNANT000
305300970306    4C                   ENDIF
305400970306    3C                   ENDIF
305500960523    1C                   ENDIF
305600911021     C*
305700960404     C* LE ANOMALIE 70 E 71 LE CHIUDO
305800960404     C* CON CAUSALE
305900000317    1C   01ANMCAA        IFEQ      70
306000960404     C     ANMCAA        OREQ      71
306100960523    2C     ANMDCH        IFEQ      0
306200990512     C                   MOVEL     D34ALA        ANMDCH
306300960404     C                   MOVEL     'D'           ANMFSC
306400970305     C*
306500970305    3C     ANMCAA        IFEQ      71
306600970306     C     KA71          CHAIN     FNANT02L                           39
306700970305     C*
306800970305    4C     *IN39         IFEQ      *ON
306900970305     C                   CLEAR                   FNANT000
307000970306     C                   MOVEL     ANMFLE        ANTFOA
307100970306     C                   MOVEL     ANMLNP        ANTLNP
307200990512     C                   MOVEL     ANMDAO        ANTDAO
307300970305     C                   MOVEL     ANMCAA        ANTCAA
307400970305     C                   Z-ADD     1             ANTTOT
307500040927     c                   z-add     am8gio        antdt1
307600970305     C                   WRITE     FNANT000
307700970305   X4C                   ELSE
307800970305     C                   ADD       1             ANTTOT
307900040927     c                   z-add     am8gio        antdt1
308000970305     C                   UPDATE    FNANT000
308100970305    4C                   ENDIF
308200970305    3C                   ENDIF
308300970305     C**
308400960523    2C                   ENDIF
308500960523    1C                   ENDIF
308600970523     C*
308700970523     C* SE CAUSALE CHIUSURA = 'RA' LA ABBLENCO
308800970523     C* ('RA' --> RIPRISTINO DI ANOMALIA 10 CHIUSA)
308900970523     C     ANMCCH        IFEQ      'RA'
309000970523     C                   CLEAR                   ANMCCH
309100970523     C                   END
309200960404     C*
309300000317     C* SE NON CI SONO STATI ERRORI DI TRASMISSIONE UPDATO
309400961219     C     WERR          IFNE      'S'
309500001102     C     D34GES        IFEQ      'V'
309600001102     C                   UPDATE    FNANM011
309700001102     C                   ELSE
309800990512     C                   UPDATE    FNANM000                                      AGGIORNO
309900001102     C                   ENDIF
310000961023     C                   ELSE
310100001102     C     D34GES        IFEQ      'V'
310200001102     C                   UNLOCK    FNANM11L
310300001102     C                   ELSE
310400990512     C                   UNLOCK    FNANM00F
310500001102     C                   ENDIF
310600960604    1C                   ENDIF
310700911021     C                   ENDSR
310800040401     C* SCRITTURA FILE PER REPORT DI SEDE ---------------------------*
310900040401     C     SRANT         BEGSR
311000040401     C** PER REPORT DI SEDE
311100040504     c                   if        anmlnp>0
311200040504     c                   movel     anmlnp        wlnp
311300040504     c                   else
311400040504     C                   MOVEL     ANMFLS        wLNP
311500040504     c                   endif
311600040405     C     KA200         CHAIN     FNANT02L                           39
311700040401     C*
311800040401    4C     *IN39         IFEQ      *ON
311900040401     C                   CLEAR                   FNANT000
312000040401     C                   MOVEL     ANMFLE        ANTFOA
312100040504     C                   MOVEL     wlnp          ANTLNP
312200040405     C* Come data anomalia metto data idd per poter conteggiare le anomalie
312300040405     c* lette tutte nello stesso report controllo qualità
312400040405     C                   MOVEL     WDTA200       ANTDAO
312500040401     C                   MOVEL     ANMCAA        ANTCAA
312600040401     C                   Z-ADD     1             ANTTOT
312700040401     C*
312800040401     C                   CLEAR                   DSLV55
312900040401     C                   MOVEL     'P'           D55TPT
313000040401     C                   MOVEL     ANMFLe        D55LIN
313100040401     C                   MOVEL     ANMDAO        D55DRF
313200040401     C                   CALL      'FNLV55R'
313300040401     C                   PARM                    DSLV55
313400040401     C* PER l'anomalia 200 ANTLNA CONTIENE IL TERMINAL DI PARTENZA
313500040401     C*  del p.o. che ha generato il disguido
313600040401     C                   MOVEL     D55TFP        ANTLNA
313700040927     c                   z-add     am8gio        antdt1
313800040401     C                   WRITE     FNANT000
313900040401     C                   ELSE
314000040401     C                   ADD       1             ANTTOT
314100040927     c                   z-add     am8gio        antdt1
314200040401     C                   UPDATE    FNANT000
314300040401     C                   ENDIF
314400040401     c                   endsr
314500911021     C*
314600960404     C* CONTROLLO PER TRASMETTERE LA CHIUSURA ANOMALIE 70 E 71 ------*
314700960404     C     CHIUAN        BEGSR
314800960417     C** MANCA BAM
314900960417     C                   Z-ADD     71            KCAA
315000960404     C                   Z-ADD     1             L
315100960404     C*
315200960417    1C     L6(L)         DOWGT     0
315300960404     C                   Z-ADD     L6(L)         KFIL
315400990512     C     KANM5         SETLL     FNANM10L
315500990512     C     KANM5         READE     FNANM10L                               30
315600960417    2C     *IN30         DOWEQ     *OFF
315700960417     C                   EXSR      BAM
315800990512     C     KANM5         READE     FNANM10L                               30
315900960417    2C                   ENDDO
316000960404     C**
316100960404     C                   ADD       1             L
316200960417    1C                   ENDDO
316300960417     C** ECCEDE BAM
316400960417     C                   Z-ADD     70            KCAA
316500021021     C                   Z-ADD     1             L
316600021021    1C     L6(L)         DOWGT     0
316700021021     C                   Z-ADD     L6(L)         KFIL
316800021021     C     Kanm20        SETLL     FNANM20L
316900021021     C     Kanm20        READE     FNANM20L                               30
317000960417    2C     *IN30         DOWEQ     *OFF
317100960417     C* SE ANMFLE E' PRESENTE IL L6 --> ELABORO
317200021021     c*  tolto il controllo perchè anmfle già in key
317300021021     C**   ANMFLE        LOOKUP    L6                                     31
317400021021     C**   *IN31         IFEQ      *ON
317500960417     C                   EXSR      BAM
317600021021     C**                 ENDIF
317700960417     C*
317800021021     C     Kanm20        READE     FNANM20L                               30
317900960417    2C                   ENDDO
318000021021     C**
318100021021     C                   ADD       1             L
318200021021    2C                   ENDDO
318300960404     C*
318400960404     C                   ENDSR
318500960417     C*
318600960417     C* CONTROLLO LE ANOMALIE 70 E 71 PER CHIUDERLE -----------------*
318700960417     C     BAM           BEGSR
318800960417     C                   MOVEL     ANMAAS        KAAS2
318900960417     C                   MOVEL     ANMLNP        KLNP2
319000960417     C                   MOVEL     ANMNRS        KNRS2
319100960417     C                   MOVEL     ANMNSP        KNSP2
319200960417     C*
319300960417     C* LEGGO IL COLLO FINTANTO CHE TROVO DELLE ANOMALIE INSERITE
319400990512     C     KANM1         SETLL     FNANM01L
319500990512     C     KANM1         READE     FNANM01L                               31
319600960417    4C     *IN31         DOWEQ     *OFF
319700990512     C     KANM1         READE     FNANM01L                               31
319800960417    4C                   ENDDO
319900960417     C*
320000960417     C* LETTA L'ULTIMA, VEDO CHE DATA CHIUSURA HA
320100960417    4C     ANMCCH        IFEQ      '  '
320200970424     C     ANMDCH        ANDGT     0
320300990512     C     ANMDCH        ANDLT     D34ALA
320400960417     C* RICHAINO CON NUMERO RELATIVO DI RECORD
320500990512     C     ANMNR1        CHAIN     FNANM00F                           31
320600960417     C* TRASMETTO IN PARTENZA LA CHIUSURA DELLA ANOMALIA
320700960417     C                   MOVEL     'OB'          ANMCCH
320800990512     C                   MOVEL     D34ALA        ANMDCH
320900960417     C                   EXSR      ELAB
321000960417    4C                   ENDIF
321100960417     C                   ENDSR
321200960422     C*
321300960423     C* STAMPA DI CHI NON HA FATTO L'IDD ----------------------------*
321400960423     C     NOIDD         BEGSR
321500960422     C                   SETON                                        08
321600040615     C                   SETOff                                       09
321700960423     C                   CLEAR                   WMANID
321800960423     C                   Z-ADD     95            KCAA
321900960422     C* SE IMMESSA LA DATA AL CERCO NELLA DATA AL
322000960422     C*  ALTRIMENTI CERCO LA DATA IDD PIU' ALTA CHE HO
322100990512    1C     D34ALA        IFEQ      0
322200990512     C     D34ACA        ANDEQ     0
322300990512     C     KCAA          SETGT     FNANM04L
322400990512     C     KCAA          READPE    FNANM04L                               30
322500960422    2C     *IN30         DOWEQ     *OFF
322600050504     C**   ANMAIE        ANDEQ     'E'
322700050504    2C**   *IN30         OREQ      *OFF
322800990512     C     ANMDAO        ANDGE     AM8GIO
322900990512     C     KCAA          READPE    FNANM04L                               30
323000960422    2C                   ENDDO
323100960422     C*
323200971126     C     *IN30         IFEQ      *OFF
323300971126     C                   Z-ADD     ANMDAO        WDATA8            8 0
323400971126     C                   ENDIF
323500971126     C**
323600960422   X1C                   ELSE
323700971126     C     D34ALA        IFGT      0
323800971126     C                   MOVEL     D34ALA        WDATA8
323900961016     C                   ELSE
324000971126     C                   MOVEL     D34ACA        WDATA8
324100961016     C                   ENDIF
324200961016     C*
324300960422     C                   SETOFF                                       30
324400960422    1C                   ENDIF
324500960510     C*
324600960926     C* PER I P.O. CHE FANNO L'IDD IL GIORNO DOPO
324700960510     C*  CERCO LA DATA PRECEDENTE
324800960510     C  N30              EXSR      RICDAT
324900990512     C  N30              MOVEL     GI8INV        WDATAM            8 0
325000960510     C*
325100960422     C* 30 OFF - TROVATA
325200960422    1C     *IN30         IFEQ      *OFF
325300960422     C                   Z-ADD     0             L
325400990518     C* GIRO LA DATA ELABORAZIONE DEL GIORNO
325500971126     C                   MOVE      WDATA8        G02INV
325600960422     C                   MOVEL     '3'           G02ERR
325700971126     C                   CALL      'XSRDA8'
325800960422     C                   PARM                    WLBDAT
325900990513     C                   MOVEL     G02DAT        STADCI            8 0
326000960510     C*
326100971126     C                   CLEAR                   G02INV
326200960510     C                   MOVE      WDATAM        G02INV
326300960510     C                   MOVEL     '3'           G02ERR
326400971126     C                   CALL      'XSRDA8'
326500960510     C                   PARM                    WLBDAT
326600990513     C                   MOVEL     G02DAT        STADCM            8 0
326700960422     C*
326800960422     C     *LOVAL        SETLL     AZORG01L
326900960423     C                   READ      AZORG01L                               30
327000960422     C*
327100960422    2C     *IN30         DOWEQ     *OFF
327200960423    C**
327300960510     C* ESCLUDO LE ANNULLATE
327400960422    3C     ORGFVA        IFEQ      ' '
327500960423     C**
327600960422     C* SOLO FILIALE E AGENZIA
327700960422    4C     ORGFAG        IFEQ      'F'
327800960422     C     ORGFAG        OREQ      'A'
327900960423     C**
328000960926     C* ESCLUDO I P.O. CHE NON DEVONO FARE IDD
328100960422     C                   MOVEL     ORGDE8        OG148
328200050831     C                   MOVEL     ORGDE3        OG143
328300960422    5C     §OGNID        IFEQ      ' '
328400971124     C* CERCO IL TERMINAL DI PARTENZA
328500971124     C* SE STO ELABORANDO L'IDD E NON SONO IL TERMINAL DI PARTENZA
328600971124   5AC     D34ELA        IFNE      ' '
328700971124     C     ORGFIL        ANDNE     D34FGS
328800971124     C**
328900971124     C                   CLEAR                   DSLV55
329000971124     C                   MOVEL     'P'           D55TPT
329100971124     C                   MOVEL     ORGFIL        D55LIN
329200980403     C                   Z-ADD     AM8GIO        D55DRF
329300971124     C                   CALL      'FNLV55R'
329400971124     C                   PARM                    DSLV55
329500971124   5BC     D55ERR        IFNE      ' '
329600971124     C                   Z-ADD     SIMFEL        D55TFP
329700971124   5BC                   ENDIF
329800971124   5AC                   ENDIF
329900960510     C**
330000960509     C* SE ANOMALIE IDD --> ESCLUDO SE NON SONO DELL'AREA
330100960423   5AC     D34ELA        IFEQ      ' '
330200971124     C     ORGFIL        ORNE      D34FGS
330300971124     C     D55TFP        ANDEQ     D34FGS
330400960422     C*
330500960422     C                   MOVEL     ORGFIL        KLNA
330600960510     C* IMPOSTO LA DATA
330700960510     C     §OGEID        IFEQ      ' '
330800971126     C                   Z-ADD     WDATA8        KDAO                           DEL GIORNO
330900960510     C                   ELSE
331000960510     C                   Z-ADD     WDATAM        KDAO                           GIORNO PREC
331100960510     C                   ENDIF
331200960510     C*
331300990512     C     KANM4         SETLL     FNANM04L                               31
331400960422    6C     *IN31         IFEQ      *OFF
331500960423     C                   MOVEL     '1'           WMANID            1
331600960422     C*
331700960422     C* NON TROVATA STAMPO ERRORE
331800960422    7C     L             IFEQ      7
331900960422     C                   EXSR      ERRIDD
332000960422    7C                   ENDIF
332100050830     c
332200050830     C                   ADD       1             L
332300050830     C                   Z-ADD     ORGFIL        FID(L)
332400050831     C                   MOVEL     ORGDES        DFI(L)
332500050831     c                   clear                   dslv55
332600050830     c* Verifico se il £6
332700050830     c                   if        §ogntw<>'EEX' and §ogntw<>'DPD' and
332800050830     c                             §ogntw<>'FED' and §ogntw<>'EUP'
332900050830     C                   MOVEL     '6'           D55TPT
333000050831     c                   else
333100050831     c                   movel     'P'           D55tpt
333200050831     c                   endif
333300050830     C                   MOVEL     ORGFIL        D55LIN
333400050830     C                   Z-ADD     kdao          D55DRF
333500050830     C                   CALL      'FNLV55R'
333600050830     C                   PARM                    DSLV55
333700960422     C**
333800050831     c                   if        §ogntw<>'EEX' and §ogntw<>'DPD' and
333900050906     c                             §ogntw<>'FED' and §ogntw<>'EUP' and
334000050906     c                             orgfil<>101
334100050906     c
334200050830     c                   if        d55tfa=orgfil
334300050831     c* Imposto la decodifica del p.o. se non è in gestione ad altri
334400050831     C***                MOVEL     ORGDES        DFI(L)
334500050830     c                   else
334600050831     c* Imposto il p.o. gestione
334700050831     c****               eval      DFI(L)=CGestito
334800050831     c                   movel     d55tfa        w003a             3
334900050831     c                   eval      %subst(DFI(L):8:12)=CGESTITO
335000050831     c                   eval      %subst(DFI(L):20:3)=w003a
335100050830     c                   endif
335200050830     c
335300050830     c                   else
335400050906     c* Imposto il terminal di partenza del P.o estero o della 101
335500050906     c*  come gestione
335600050831     c***                eval      DFI(L)=CGestito
335700050831     c                   movel     d55tfp        w003a             3
335800050831     c                   eval      %subst(DFI(L):8:12)=CGESTITO
335900050831     c                   eval      %subst(DFI(L):20:3)=w003a
336000050830     c                   endif
336100960422     C*
336200960422    6C                   ENDIF
336300960422     C*
336400960423   5AC                   ENDIF
336500960423    5C                   ENDIF
336600960422    4C                   ENDIF
336700960422    3C                   ENDIF
336800960422     C*
336900960423     C                   READ      AZORG01L                               30
337000960422    2C                   ENDDO
337100960422     C*
337200960422     C* SE NE RIMANGONO FINISCO DI STAMPARE
337300960423    2C     L             IFGT      0
337400960422     C                   EXSR      ERRIDD
337500960423   X2C                   ELSE
337600960423     C* SE CI SONO TUTTI LO DICO
337700960423    3C     WMANID        IFEQ      ' '
337800960423     C*
337900960423    4C     *INOF         IFEQ      *ON
338000960423     C     *IN09         OREQ      *OFF
338100040615     c                   if        *inof
338200040615     C                   EXCEPT    TES
338300040615     C  n70              EXCEPT    TES2
338400040615     c                   endif
338500960423     C*
338600960423    5C     D34ELA        IFEQ      ' '
338700960423     C                   EXCEPT    TESIDR
338800960423     C                   ELSE
338900960423     C                   EXCEPT    TESIDE
339000960423    5C                   ENDIF
339100960423     C                   SETON                                        09
339200960423     C                   SETOFF                                       OF
339300960423    4C                   ENDIF
339400960423     C**
339500960423     C* TUTTI HANNO ESEGUITO L'IDD
339600960423     C                   EXCEPT    SIIDD
339700960423    3C                   ENDIF
339800960423     C*
339900960423    2C                   ENDIF
340000960422     C*
340100960422    1C                   ENDIF
340200960422     C*
340300960422     C                   SETOFF                                       08
340400960422     C                   ENDSR
340500960510     C*
340600960926     C* RICERCA DATA IDD-1 X I P.O. CHE LO FANNO IL GG DOPO----------*
340700990512     C     RICDAT        BEGSR
340800960510     C* UTILIZZO IL CALENDARIO
340900971126     C                   MOVE      WDATA8        G02INV
341000960510     C                   MOVEL     '3'           G02ERR
341100971126     C                   CALL      'XSRDA8'
341200960510     C                   PARM                    WLBDAT
341300960510     C*
341400960510     C                   MOVEL     'S'           WFEST             1
341500960510     C                   CLEAR                   KTFP
341600960510     C* CERCO IL PRIMO GIORNO LAVORATIVO PRECEDENTE
341700960510     C     WFEST         DOWEQ     'S'
341800960510     C                   SUB       1             G02TGI
341900990512     C                   Z-ADD     G02TGI        GI8TGI
342000990518     C                   CALL      'XSRGI8'
342100960510     C                   PARM                    WGIDAT
342200990512     C                   Z-ADD     GI8INV        WDATAM
342300960510     C*
342400960510     C                   CLEAR                   WFEST
342500960510     C     KCLN          CHAIN     AZCLN01L                           31
342600960510    1C     *IN31         IFEQ      *OFF
342700960510     C     POM(WGG)      ANDNE     *BLANKS
342800960510     C                   MOVEL     '1Y'          COD
342900960510     C                   MOVEL(P)  POM(WGG)      KEY
343000960510     C     KTAB          CHAIN     TABEL                              31
343100960510    2C     *IN31         IFEQ      *OFF
343200960510     C                   MOVEL     TBLUNI        DS1Y
343300960510    3C     §1YFEI        IFEQ      'S'
343400960510     C                   MOVEL     'S'           WFEST
343500960510    3C                   ENDIF
343600960510    2C                   ENDIF
343700960510    1C                   ENDIF
343800960510     C                   ENDDO
343900960510     C                   ENDSR
344000960422     C*
344100960422     C* STAMPA ERRORE NO IDD ----------------------------------------*
344200960422     C     ERRIDD        BEGSR
344300960422     C* SE OF ACCESO LA PRIMA VOLTA STAMPO LA TESTATA COMPLETA
344400960422    1C     *INOF         IFEQ      *ON
344500960423     C     *IN09         OREQ      *OFF
344600040615     c                   if        *inOF
344700040615     C                   EXCEPT    TES
344800040615     C  n70              EXCEPT    TES2
344900040615     c                   endif
345000960423     C*
345100960423     C     D34ELA        IFEQ      ' '
345200960423     C                   EXCEPT    TESIDR
345300960423     C                   ELSE
345400960423     C                   EXCEPT    TESIDE
345500960423     C                   ENDIF
345600960423     C                   SETON                                        09
345700960423     C                   SETOFF                                       OF
345800960422    1C                   ENDIF
345900960422     C*
346000960422     C                   EXCEPT    DETIDD
346100960422     C*
346200960422     C                   CLEAR                   FID
346300960422     C                   CLEAR                   DFI
346400960422     C                   CLEAR                   L
346500960422     C*
346600960422     C                   ENDSR
346700960404     C*
346800911021     C* INVIO MEMBRI TRASMISSIONE -----------------------------------*
346900911021     C     CAEX          BEGSR
347000050804     C* CREO ANOMALIA OK IDD SUL SISTEMA per tutti i p.o. della £6
347100000317     C     D34ELA        IFEQ      'S'
347200060619     c* apro il file anomalie di sede per scriverci l'anomalia 95
347300060619     c                   EXSR      opensed
347400060619     c
347500050804     C                   Z-ADD     1             LL                3 0
347600050804    2C     L6(LL)        DOWGT     0
347700050804     c
347800050804     C                   Z-ADD     L6(LL)        anmlna
347900050804     C                   Z-ADD     FGSTFP        D55TFP
348000911021     C                   Z-ADD     95            ANMCAA
348100960404     C                   EXSR      CRTANM
348200990512     C                   WRITE     FNANM000
348300060619     c                   if        %open(fnanmsed)
348400060619     C                   WRITE     FNANM0sd
348500060619     c                   endif
348600050804     c
348700050804     c                   add       1             LL
348800050804     c                   enddo
348900050804     c
349000060711    1C                   ENDIF
349100060711     c
349200060711     C* Trasmissione membri  solo se chi esegue è il terminal o se
349300060711     c*  un 2 livello che lo fa dopo il terminal
349400060711    0C     D34FGS        IFEQ      SIMFEL
349500060711     C     D34ALA        ORLE      FELDID
349600911021     C*
349700921029     C     *LOVAL        SETLL     AZORG
349800911021     C                   READ      AZORG                                  30
349900911021     C*
350000960604    1C     *IN30         DOWEQ     *OFF
350100971124    2C     ORGFAG        IFEQ      'F'
350200971124     C     ORGFAG        OREQ      'A'
350300971124    3C     ORGFVA        IFEQ      ' '
350400971124     C* PRENDO IL TERMINAL DI PARTENZA
350500971124     C                   CLEAR                   DSLV55
350600971124     C                   MOVEL     'P'           D55TPT
350700971124     C                   MOVEL     ORGFIL        D55LIN
350800980403     C                   MOVEL     AM8GIO        D55DRF
350900971124     C                   CALL      'FNLV55R'
351000971124     C                   PARM                    DSLV55
351100990517     C* SE NON TROVATO TERMINAL DI PARTENZA NON INVIO OK IDD
351200990517     C* (P.O. NON ANCORA OPERATIVO MA GIA' PRESENTE IN ORGANIGRAMMA)
351300990517    4C     D55ERR        IFEQ      *BLANKS
351400990517    4C     ORGFIL        ANDEQ     D55TFP
351500990331     C     ORGFIL        ANDNE     SIMFEL
351600120217     c
351700120217    4C     D55ERR        orEQ      *BLANKS
351800120217     C     ORGFIL        ANDEQ     D55TFP
351900120217     C     ORGFIL        ANDeq     SIMFEL
352000120217     c     d34ges        andeq     'V'
352100060711     c
352200990610     C* LA SEDE NON HA TERMINAL MA BISOGNA INVIARLA LO STESSO
352300060711     c* La sede non la invio più scrivo subito fnanm00f di sede
352400960409     C* SOLO PER FILIALI O AGENZIE
352500990512     C   99              CLOSE     FNANM00T
352600960604     C   99              SETOFF                                       99
352700960604     C* DISALLOCAZIONE
352800021009     c                   eval      D60po0 = SIMfel
352900960604     C                   MOVEL     SAVFL1        D60FL1
353000960604     C                   MOVEL     SAVFL2        D60FL2
353100971126     C                   MOVEL     AM8GIO        D60DRF
353200960604     C                   MOVEL     'D'           D60AZN
353300990512     C                   MOVEL     'FNANM00T'    D60FIL
353400021009     C                   MOVEL     FNLV60DS      KPJBU
353500960604     C                   CALL      'FNLV60R'
353600960604     C                   PARM                    KPJBA
353700960604     C                   CLEAR                   SAVFL1
353800960604     C                   CLEAR                   SAVFL2
353900960604     C** ALLOCAZIONE
354000021009     c                   eval      D60po0 = SIMfel
354100021008     c                   reset                   DSmembro
354200021008     c                   eval      MbrPOP = SIMfel
354300021008     c                   eval      MbrPOA = D55tfp
354400021008     c                   movel     DSmembro      VAR
354500960604     C                   CLEAR                   D60FL2
354600971124     C                   MOVEL     D55TFP        D60FL1
354700060711     C                   MOVEL     'X'           D60AZN
354800971126     C                   MOVEL     AM8GIO        D60DRF
354900971128     C                   MOVEL(P)  '*EXCL  '     D60VNC
355000021009     C                   MOVEL     FNLV60DS      KPJBU
355100960604     C                   CALL      'FNLV60R'
355200960604     C                   PARM                    KPJBA
355300021009     C                   MOVEL     KPJBU         FNLV60DS
355400960604     C**
355500960604     C* NON TROVATI ERRORI DI ALLOCAZIONE
355600971124    5C     D60ERR        IFEQ      'N'
355700971124     C                   MOVEL     D55TFP        SAVFL1
355800021009     c                   movel     D60mb1        DSmembro
355900021008     c                   movel     DSmembro      VAR
356000021009     c                   movel     D60mb1        wMBR
356100000317     C**
356200911025     C* PRENDO NUMERO RECORD
356300990512     C                   MOVEL     'FNANM00T'    PGM              10
356400911021     C                   EXSR      NUMREC
356500960405     C*
356600060711     C*
356700971124    6C     NRR           IFGT      0
356800060711     c* controllo spostato sopra
356900060711    7C***  D34FGS        IFEQ      SIMFEL
357000060711     C***  D34ALA        ORLE      FELDID
357100911021     C                   MOVEL     ORGFEL        §DST              3
357200971125     C*
357300020930     C                   CLEAR                   TIBS55DS
357400971125     C                   MOVEL     SIMFEL        I50PPO
357500971125     C                   MOVEL     ORGFIL        I50APO
357600020930     C                   CALL      'TIBS55R'
357700020930     C                   PARM                    TIBS55DS
357800971125     C*
357900971125     C                   MOVEL     O50PLA        CMZLBD
358000971125     C                   MOVEL     O50ALA        CMZLBA
358100971125     C                   MOVEL     O50ASI        CMZSYF
358200911021     C*
358300960405     C                   EXSR      X25ANM
358400060711    7C****               ENDIF
358500971124    6C                   ENDIF
358600960604    5C                   ENDIF
358700960604    4C                   ENDIF
358800960604    3C                   END
358900960604    2C                   END
359000911021     C*
359100911021     C                   READ      AZORG                                  30
359200960604    1C                   ENDDO
359300060711    0C                   ENDif
359400911021     C                   ENDSR
359500060619     C*
359600060619     C* open file anomalie di sede ----------------------------------*
359700060619     C     OPENSED       BEGSR
359800060619     C                   MOVEL     *BLANKS       COMMAN           80
359900060619     C                   MOVEL     OVRSED(1)     COMMAN
360000060706     C                   CALL      'QCMDEXC'                            98
360100060619     C                   PARM                    COMMAN
360200060619     C                   PARM                    LUNG
360300060619     C                   OPEN(E)   FNANMSED
360400060619     c* provo in gaitragrps
360500060619     C                   if        %error
360600060710     c                   eval      %subst(comman:38:11)='PS/FNANM00F'
360700060706     C                   CALL      'QCMDEXC'                            98
360800060619     C                   PARM                    COMMAN
360900060619     C                   PARM                    LUNG
361000060619     C                   OPEN(E)   FNANMSED
361100060619     c                   endif
361200060619     c
361300060619     C                   ENDSR
361400911021     C*
361500911021     C* CREO ANOMALIE ESEGUITO IDD ----------------------------------*
361600911021     C     CRTANM        BEGSR
361700911021     C                   MOVEL     *BLANKS       ANMATB
361800950922     C                   CLEAR                   ANMCDU
361900911021     C                   Z-ADD     0             ANMFLS
362000911021     C                   Z-ADD     0             ANMNRS
362100911021     C                   Z-ADD     0             ANMSCN
362200911021     C                   Z-ADD     0             ANMAAS
362300960404     C                   Z-ADD     0             ANMLNP
362400911021     C                   Z-ADD     0             ANMNSP
362500911021     C                   MOVEL     '7C'          COD
362600960404     C                   MOVEL(P)  ANMCAA        KEY
362700960409     C     KTAB          CHAIN(N)  TABEL                              31
362800911021     C  N31              MOVEL     TBLUNI        DS7C
362900911021     C   31              MOVEL     *BLANKS       DS7C
363000911021     C                   MOVEL     §7CTAN        ANMTAN
363100911021     C                   MOVEL     §7CSAN        ANMSAN
363200921027     C                   MOVEL     'D'           ANMFSA
363300990512     C                   MOVEL     D34ALA        ANMDAO
363400911021     C                   MOVEL     §7CAIE        ANMAIE
363500911025     C                   MOVEL     ' '           ANMFT1
363600911025     C                   MOVEL     *ZEROS        ANMFL1
363700000824     C                   MOVEL     *ZEROS        ANMDTR
363800911021     C* CHIUSURA AUTOMATICA
363900911021     C     §7CCHA        IFEQ      'S'
364000921027     C                   MOVEL     'D'           ANMFSC
364100990512     C                   MOVEL     D34ALA        ANMDCH
364200911021     C                   END
364300960509     C* DA INVIARE
364400911021     C     §7CAIE        IFEQ      'E'
364500971124     C                   MOVEL     D55TFP        ANMFL1
364600911025     C                   MOVEL     'T'           ANMFT1
364700000824     C                   MOVE      AM8GIO        ANMDTR
364800911021     C                   END
364900911021     C                   Z-ADD     0             ANMFL2
365000911021     C                   MOVEL     *BLANKS       ANMFT2
365100911021     C                   MOVEL     *BLANKS       ANMCCH
365200911021     C                   Z-ADD     0             ANMNPS
365300911021     C                   Z-ADD     0             ANMNFV
365400911021     C                   MOVEL     SIMFEL        ANMFLE
365500911021     C                   MOVEL     §7CLID        ANMLID
365600990512     C                   CLEAR                   ANMDUE
365700990512     C                   CLEAR                   ANMNPG
365800990512     C                   CLEAR                   ANMFGS
365900990512     C                   CLEAR                   ANMFLO
366000040601     c* data ora di creazione
366100040601     c                   z-add     *date         wdata             6 0
366200040601     c                   time                    wora              6 0
366300040607     c                   movel     wora          wora4             4 0
366400040601     c                   movel     wdata         anmflo
366500040607     c                   move      wora4         anmflo
366600040601     c
366700990512     C                   CLEAR                   ANMFT3
366800990512     C                   CLEAR                   ANMFL3
366900990512     C                   CLEAR                   ANMFT4
367000040601     C                   z-add     999           ANMFL4
367100911021     C*
367200911021     C                   ENDSR
367300911021     C*
367400990512     C** SOTTOMETTE TRASMISSIONE FNANM00T ---------------------------*
367500911021     C     X25ANM        BEGSR
367600911021     C                   MOVE      *BLANK        CMZPRR                          PRE-REQ.
367700911021     C                   MOVE      'S'           §CLR              1             CLEAR
367800911021     C                   MOVEL     §DST          CMZDST                          DESTINAT.
367900990512     C                   MOVEL     'FNANM00T'    CMZFLD                          DA: >>>
368000911021     C                   MOVEL     *BLANK        CMZMBD
368100021008     C                   MOVEL     wMBR          CMZMBD
368200990512     C                   MOVEL     'FNANM00R'    CMZFLA                          A: >>>
368300911021     C                   MOVEL     *BLANK        CMZMBA
368400021008     C                   MOVEL     wMBR          CMZMBA
368500911021     C                   MOVE      '0000'        CMZORI                          ORA
368600911021     C                   MOVE      '0000000'     CMZDTI                          DATA
368700040615     C**   D34TRA        IFEQ      'S'
368800040615     C**                 MOVE      'T'           CMZURG
368900040615     C**                 ELSE
369000911115     C                   MOVE      'N'           CMZURG                          ATTESA
369100040615     C**                 END
369200911021     C                   MOVE      'A'           CMZADD                          IN ADD
369300911021     C                   MOVE      §CLR          CMZCLR                          CLEAR
369400911021     C                   MOVE      'S'           CMZFTR                          DA TRANS.
369500911021     C                   MOVE      *BLANK        CMZERR                          1= ERR.
369600040615     C**   D34TRA        IFEQ      'S'
369700040615     C**                 MOVE      'FF20'        CMZAZN                          AZIONE
369800040615     C**                 ELSE
369900990518     C                   MOVE      'LR16'        CMZAZN                          AZIONE
370000040615     C**                 END
370100911021     C                   MOVE      NRR           CMZNRR                          N.REC.
370200020930     C                   CALL      'TIBS53C'
370300971125     C                   PARM                    O50LCL
370400971125     C                   PARM                    CMZDST
370500911021     C                   PARM                    CMZFLD
370600911021     C                   PARM                    CMZMBD
370700911021     C                   PARM                    CMZLBD
370800911021     C                   PARM                    CMZFLA
370900911021     C                   PARM                    CMZMBA
371000911021     C                   PARM                    CMZLBA
371100911021     C                   PARM                    CMZORI
371200911021     C                   PARM                    CMZDTI
371300911021     C                   PARM                    CMZURG
371400911021     C                   PARM                    CMZADD
371500911021     C                   PARM                    CMZCLR
371600911021     C                   PARM                    CMZFTR
371700911021     C                   PARM                    CMZERR
371800911021     C                   PARM                    CMZAZN
371900911021     C                   PARM                    CMZSYF
372000911021     C                   PARM                    CMZNRR
372100911021     C                   PARM                    CMZPRR
372200971125     C                   PARM                    W236            236
372300911021     C                   ENDSR
372400911021     C*
372500911021     C* PRELEVO NUMERO RECORD DAL MEMBRO DEL FILE --------------------*
372600911021     C     NUMREC        BEGSR
372700021008     ***C                   MOVEL     'M   '        VAR2              4
372800021008     ***C                   MOVE      D55TFP        VAR2
372900021008     ***C                   MOVEL     VAR2          ALF10
373000021008     c                   reset                   DSmembro
373100021008     c                   eval      MbrPOP = SIMfel
373200021008     c                   eval      MbrPOA = D55tfp
373300021008     c                   movel     DSmembro      ALF10
373400911021     C                   Z-ADD     0             NRR              10 0
373500921027     C                   CALL      'TRUL51C'
373600921027     C                   PARM                    KNSIF
373700921027     C                   PARM                    PGM
373800921027     C                   PARM                    ALF10            10
373900911021     C                   PARM                    NRR
374000911021     C                   ENDSR
374100980327     C*-------------------------------------------------------------------
374200980327     C* TRASFERIMENTO DA FILIALE A FILIALE
374300040615     C**   TRASFE        BEGSR
374400980327     C* SCRIVO IL RECORD NEL T
374500040615     C**   NODEL         IFEQ      'U'
374600040116     C******             MOVEL     WRTT          ANMAIE
374700980327     C*
374800040615     C**                 MOVEL     'T'           ANMFT1
374900040615     C**                 MOVE      AM8GIO        ANMDTR
375000040615     C**   ANMFL2        IFNE      *ZEROS
375100040615     C**                 MOVEL     'T'           ANMFT2
375200040615     C**                 END
375300040615     C**                 END
375400980327     C*
375500040615     C**   ANMCAA        IFEQ      70
375600040615     C*+   ANMCAA        OREQ      71
375700040116     C**   ANMAIE        IFEQ      'R'
375800040615     c**   wanom         ifeq      'P'
375900040615     C**   ANMCCH        ANDEQ     '  '
376000040615     C**                 CLEAR                   ANMDCH
376100040615     C**                 ENDIF
376200040615     C**                 ENDIF
376300980327     C*
376400040615     C**                 WRITE     FNANM0TT
376500040615     C**                 ADD       1             RECAN             5 0
376600980327     C*
376700980327     C* SE NON SONO IN FASE DECISIVA
376800040615     C**   D34AGG        IFEQ      'AGG'
376900040615     C**   NODEL         IFEQ      'U'
377000040615     C**                 MOVEL     WRTF          ANMAIE
377100040615     C**                 MOVEL     'T'           ANMFT1
377200040615     C**                 MOVE      AM8GIO        ANMDTR
377300040615     C**   ANMFL2        IFNE      *ZEROS
377400040615     C**                 MOVEL     'T'           ANMFT2
377500040615     C**                 END
377600040615     C**   ANMCAA        IFEQ      70
377700040615     C**   ANMCAA        OREQ      71
377800040116     C**   ANMAIE        IFEQ      'R'
377900040615     c**   wanom         ifeq      'P'
378000040615     C**   ANMCCH        ANDEQ     '  '
378100040615     C**                 CLEAR                   ANMDCH
378200040615     C**                 ENDIF
378300040615     C**                 ENDIF
378400040615     C**                 UPDATE    FNANM000
378500040615     C**                 END
378600980327     C*
378700040615     C**   NODEL         IFEQ      ' '
378800040615     C**                 DELETE    FNANM000
378900040615     C**                 END
379000980327     C*
379100040615     C**   NODEL         IFEQ      'S'
379200040615     C**                 UNLOCK    FNANM00F
379300040615     C**                 END
379400980327     C*
379500040615     C**                 END
379600980327     C*
379700040615     C**                 ENDSR
379800980327     C**-------------------------------------------------------------**
379900980327     C** ESEGUE CALL QCAEXEC SUL MEMBRO                              **
380000980327     C**-------------------------------------------------------------**
380100040615     C**   CAEX2         BEGSR
380200040615     C**                 MOVE      ')'           VAR
380300040615     C**                 Z-ADD     48            LUNG             15 5
380400040615     c**                 clear                   DSmembro
380500040615     c**                 eval      MbrM   = 'M'
380600040615     c**                 eval      MbrPOP = SIMfel
380700040615     c**                 eval      MbrPOA = D34fgs
380800040615     c**                 movel     DSmembro      VAR
380900980327     C** FILIALE DI PARTENZA PER APERTURA MEMBRO
381000980327     C** CONTROLLO SE ESISTE IL MEMBRO
381100040615     C**                 SETOFF                                       20        FNANM00T
381200040615     C**                 MOVE      VAR           CMD5
381300040615     C**                 MOVEL     *BLANKS       COMMAN           80
381400040615     C**                 MOVEA     CMD5(1)       COMMAN
381500040615     C**                 CALL      'QCMDEXC'                            20
381600040615     C**                 PARM                    COMMAN
381700040615     C**                 PARM                    LUNG
381800980327     C** CONTROLLO SE ESISTE IL MEMBRO
381900040615     C** 20              DO
382000040615     C**                 MOVE      VAR           CMD3
382100040615     C**                 MOVEL     *BLANKS       COMMAN
382200040615     C**                 MOVEA     CMD3(1)       COMMAN
382300040615     C**                 CALL      'QCMDEXC'
382400040615     C**                 PARM                    COMMAN
382500040615     C**                 PARM                    LUNG
382600040615     C**                 END
382700980327     C** CONTROLLO SE ESISTE IL MEMBRO
382800980327     C** ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
382900040615     C**                 MOVE      VAR           CMD1
383000040615     C**                 MOVEL     *BLANKS       COMMAN
383100040615     C**                 MOVEA     CMD1(1)       COMMAN
383200040615     C**                 CALL      'QCMDEXC'
383300040615     C**                 PARM                    COMMAN
383400040615     C**                 PARM                    LUNG
383500980327     C*
383600040615     C**                 OPEN      FNANM00T
383700040615     C**                 ENDSR
383800001030     C*-------------------------------------------------------------------
383900001030     C* TRASMISSIONE SCHEDULATA ANOMALIE (ESTERNE NON TRASMESSE)
384000001030     C     SCHANM        BEGSR
384100001102     C* N.B. LEGGO SOLO LE ANOMALIE ESTERNE VISTO CHE PER IL MOMENTO
384200001102     C*      LE AZIONI CHE PREVEDONO L'INVIO SCHEDULATO SONO TUTTE
384300001102     C*      ANOMALIE ESTERNE
384400001102     C                   MOVEL     'E'           KAIE
384500001030     C                   MOVEL     '7C'          COD
384600001030     C     KTAB2         SETLL     TABEL
384700001030     C     KTAB2         READE     TABEL                                  30
384800001030     C     *IN30         DOWEQ     *OFF
384900001030     C                   MOVEL     TBLUNI        DS7C
385000001030     C     §7CINV        IFEQ      'S'
385100001102     C                   MOVEL     TBLKEY        KCAA
385200001030     C     KAN11         SETLL     FNANM11L
385300001030     C     KAN11         READE     FNANM11L                               30
385400001030     C     *IN30         DOWEQ     *OFF
385500001030     C                   EXSR      ELAB
385600001030     C     KAN11         READE     FNANM11L                               30
385700001030     C                   ENDDO
385800001030     C                   ENDIF
385900001030     C     KTAB2         READE     TABEL                                  30
386000001030     C                   ENDDO
386100001030     C                   EXSR      CAEX
386200041020     c
386300041020     c* Leggo le anomalie 145 146 per le quali devo scrivere fnant
386400041020     c* sbr   spostata dal pgm fnlr76r che viene eliminato
386500041020     C                   MOVEL     'I'           KAIE
386600041020     c                   z-add     145           kcaa
386700041020     c                   do        2
386800041020     c     kan11         setll     fnanm11l
386900041020     C     KAN11         READE     FNANM11L
387000041020     c                   dow       not %eof(fnanm11l)
387100041020     c                   if        anmcch<>'AN'
387200041020     c                   EXSR      SCRANT
387300041020     c                   update    fnanm011
387400041020     C                   ENDIF
387500041020     c
387600041020     C     KAN11         READE     FNANM11L
387700041020     c                   enddo
387800041020     c                   z-add     146           kcaa
387900041020     c                   enddo
388000001030     C                   ENDSR
388100031007     C*-------------------------------------------------------------------
388200031007     C* WRIDET - ROUTINE SCRITTURA WRK-FILE WFLAN00F SU STAMPA FORMATO "DET"
388300031007     C*-------------------------------------------------------------------
388400031007     C     WRIDET        BEGSR
388500031007     C*
388600031007     C* Inizializzo il buffer del formato record
388700031007     C                   CLEAR                   WFLAN000
388800031007     C*
388900031031     C                   EVAL      WLANFGS = D34FGS
389000031007     C                   EVAL      WLANPOA = D34PAR
389100031007     C                   EVAL      WLANAAS = ANMAAS
389200031007     C                   EVAL      WLANLNP = ANMLNP
389300031007     C                   EVAL      WLANNRS = ANMNRS
389400031007     C                   EVAL      WLANNSP = ANMNSP
389500031007     C                   EVAL      WLANLNA = ANMLNA
389600031007     C                   MOVE      PRTDSP        WLANDSP
389700031007     C                   EVAL      WLANFLS = ANMFLS
389800031007     C                   EVAL      WLANSCN = ANMSCN
389900031007     C                   MOVE      PRTZNC        WLANZNC
390000031007     C                   EVAL      WLANNPS = ANMNPS
390100031007     C                   EVAL      WLANAIE = DESAIE
390200031007     C                   EVAL      WLANDEA = DESTAN
390300031007     C                   EVAL      WLANCAA = ANMCAA
390400031007     C                   EVAL      WLANDEC = DESCAA
390500031007     C                   EVAL      WLANNDC = ARBNDC
390600031007     C                   MOVE      STADDC        WLANDDC
390700031007     C                   EVAL      WLANPDC = STAPDC
390800031007     C* Se già valorizzato il campo PSTAM compondo la descrizione
390900031007     C                   IF        PSTAM <> *blanks
391000031007     C                   EVAL      WLANCO1 = PSTAM
391100031007     C                   MOVE      WDFLP         WDFLPALFA         3
391200031007     C                   EVAL      %subst(WLANCO1:13:3) = WDFLPALFA
391300031007     C                   MOVE      PRTDET        WPRTDET           4
391400031007     C                   EVAL      WLANCO1 = %trim(WLANCO1) + ' ' +
391500031007     C                                       %subst(WPRTDET:1:2)  + '/' +
391600031007     C                                       %subst(WPRTDET:3:2)
391700031007     C***                EVAL      WLANFLP = WDFLP
391800031007     C***                MOVE      PRTDET        WLANDET
391900031007     C                   ENDIF
392000031007     C*
392100031007     C* Se valorizzato il campo WFGS compondo la descrizione
392200031007     C                   IF        WFGS <> *zeros
392300031007     C                   EVAL      WLANCO2 = CDOVE
392400031007     C                   MOVE      WFGS          WDFLPALFA
392500031007     C                   EVAL      %subst(WLANCO2:13:3) = WDFLPALFA
392600031007     C                   EVAL      %subst(WLANCO2:5:5) = PRTDOV
392700031007     C                   MOVE      PRTDFV        WPRTDET
392800031007     C                   EVAL      WLANCO2 = %trim(WLANCO2) + ' ' +
392900031007     C                                       %subst(WPRTDET:1:2)  + '/' +
393000031007     C                                       %subst(WPRTDET:3:2)
393100031007     C***                EVAL      WLANDOV = PRTDOV
393200031007     C***                EVAL      WLANFGS = WFGS
393300031007     C***                MOVE      PRTDFV        WLANDFV
393400031007     C                   ENDIF
393500031007     C*
393600031007     C* Se valorizzato il numero C.A. compongo la descrizione
393700031007     C                   IF        DCTNCA > *zeros
393800040616     C                   EVAL      WLANCO3 = CCOAN1
393900031007     C                   MOVE      DCTFIL        WDFLPALFA
394000031007     C                   EVAL      %subst(WLANCO3:8:3) = WDFLPALFA
394100031007     C                   MOVE      DCTNCA        WNCAALFA          7
394200031007     C                   EVAL      %subst(WLANCO3:12:7) = WNCAALFA
394300031007     C                   MOVE      STADCA        WDCAALFA          6
394400031007     C                   EVAL      WLANCO3 = %trim(WLANCO3) + ' ' +
394500031007     C                                       %subst(WDCAALFA:1:2)  + '/' +
394600031007     C                                       %subst(WDCAALFA:3:2)  + '/' +
394700031007     C                                       %subst(WDCAALFA:5:2)
394800031007     C***                EVAL      WLANFIL = DCTFIL
394900031007     C***                EVAL      WLANNCA = DCTNCA
395000031007     C***                MOVE      DCTDCA        WLANDCA
395100031007     C                   ENDIF
395200031007     C*
395300031007     C                   EVAL      WLANLID = ANMLID
395400031007     C                   EVAL      WLANDFA = DESFSA
395500031007     C                   MOVE      STADAO        WLANDAA
395600031007     C                   EVAL      WLANCDU = ANMCDU
395700031007     C                   EVAL      WLANNFV = ANMNFV
395800031007     C                   EVAL      WLANDFC = DESFSC
395900031007     C                   MOVE      STADCH        WLANDAC
396000031007     C                   MOVE      CCHPRT        WLANCCH
396100031007     C                   EVAL      WLANDCC = DESCCH
396200031007     C                   EVAL      WLANFLE = ANMFLE
396300031007     C                   EVAL      WLANCHA = DCTDCH
396400031007     C*
396500031007     C* SCarico il buffer sul file
396600031007     C                   WRITE     WFLAN000
396700031007     C*
396800031007     C                   ENDSR
396900031007     C*-------------------------------------------------------------------
397000031007     C* DELWF  - ROUTINE DI CANCELLAZIONE WRK-FILE WFLAN00F X LANCIO CORRENTE
397100031007     C*-------------------------------------------------------------------
397200031007     C     DELWF         BEGSR
397300031007     C*
397400031007     C     KLAN01        SETLL     WFLAN01L
397500031007     C                   IF        %equal(WFLAN01L)
397600031007     C     KLAN01        READE     WFLAN01L
397700031007     C                   DOW       not %eof(WFLAN01L)
397800031007     C                   DELETE    WFLAN01L
397900031007     C     KLAN01        READE     WFLAN01L
398000031007     C                   ENDDO
398100031007     C                   ENDIF
398200031007     C*
398300031007     C                   ENDSR
398400960327     C****************************************************************
398500960327     C     *INZSR        BEGSR
398600960327     C*
398700960327     C     *ENTRY        PLIST
398800960327     C                   PARM                    KPJBA
398900960327     C                   MOVEL     KPJBU         DSLR34
399000960327     C                   Z-ADD     1             CODUT
399100960327     C                   CALL      'X§PARUT'
399200020905     C                   PARM                    UT§DSE0F
399300960327     C                   MOVEL     RAGUT         RSUT             20
399400960327     C                   MOVEL     REC80         CNCR80
399500031007     C*
399600031007     C* Verifico subito se per il lancio richiesto è prevista anche la creazione del wrk-file
399700031009     C* Lista Anomalie (WFLAN00F)
399800111201     C                   IF        DSLR34 <> *blanks  and                       * NO x trasmisisone
399900111201     c                             parsch<>'V'
400000031009     C***                IF        D34IE = 'D'                                  * solo anomalie IDD
400100031007     C* Imposto a NO l'indicatore d condizionamento creazoine wrk-file
400200031007     C                   SETOFF                                       55
400300031007     C* Verifico se x il cliente richiesto nel lancio è prevista la creazine del wrk-file
400400031007     C                   EVAL      WD34POS = D34POS
400500031007     C                   EVAL      WD34NRS = D34NRS
400600031007     C                   EVAL      TBECOD = 'LAN'
400700031007     C                   EVAL      TBEKE1 = DSKEYLAN
400800031007     C     KTBE01P       CHAIN     TNTBE01L
400900031007     C                   IF        %found(TNTBE01L)
401000031007     C* Se trovo il cliente in tabella, verifico i flag x le abilitazioni elaborazione
401100031007     C* anomalie in partenza e/o in arrivo
401200031007     C                   EVAL      DLAN = TBEUNI
401300031007     C                   IF        (§LANPAR = 'S' AND D34PAR = 'P') OR
401400031007     C                             (§LANARR = 'S' AND D34PAR = 'A')
401500031007     C                   SETON                                        55
401600031031     C                   OPEN      WFLAN01L
401700031007     C                   EXSR      DELWF
401800031007     C                   ENDIF
401900031007     C                   ENDIF
402000031007     C***                ENDIF
402100031009     C                   ENDIF
402200960327     C*-----------------------------------------------------
402300960327     C     KTAB          KLIST
402400960327     C                   KFLD                    CODUT
402500960327     C                   KFLD                    COD               2
402600960327     C                   KFLD                    KEY               8
402700031007     C     KTBE01P       KLIST                                                  * Parziale
402800031007     C                   KFLD                    TBECOD
402900031007     C                   KFLD                    TBEKE1
403000031007     C     KLAN01        KLIST                                                  * Completa
403100031031     C                   KFLD                    D34FGS
403200031007     C                   KFLD                    D34POS
403300031007     C                   KFLD                    D34NRS
403400960327     C     KTAB2         KLIST
403500960327     C                   KFLD                    CODUT
403600960327     C                   KFLD                    COD               2
403700960927     C     KFVA          KLIST
403800960927     C                   KFLD                    BRVFGS
403900000201     C                   KFLD                    WNFV
404000960510     C     KCLN          KLIST
404100960510     C                   KFLD                    KTFP
404200960510     C                   KFLD                    KLNA
404300990512     C                   KFLD                    WAA
404400990512     C                   KFLD                    WMM
404500960417     C     KANM          KLIST
404600990512     C                   KFLD                    ANMAAS
404700960417     C                   KFLD                    ANMLNP
404800960417     C                   KFLD                    ANMNRS
404900960417     C                   KFLD                    ANMNSP
405000020614     C     KBTP          KLIST
405100020614     C                   KFLD                    SIMFEL
405200020614     C                   KFLD                    ANMAAS
405300020614     C                   KFLD                    ANMLNP
405400020614     C                   KFLD                    ANMNRS
405500020614     C                   KFLD                    ANMNSP
405600960405     C     KANM1         KLIST
405700960405     C                   KFLD                    KAAS2
405800960405     C                   KFLD                    KLNP2
405900960405     C                   KFLD                    KNRS2
406000960405     C                   KFLD                    KNSP2
406100960404     C                   KFLD                    KCAA
406200960509     C     KANM2         KLIST
406300960509     C                   KFLD                    ANMFLS
406400960509     C                   KFLD                    ANMLNA
406500960509     C                   KFLD                    ANMNRS
406600960509     C                   KFLD                    ANMSCN
406700960509     C                   KFLD                    KCAA
406800021021     C     KANM20        KLIST
406900021021     C                   KFLD                    KFIL
407000021021     C                   KFLD                    KCAA
407100970529     C     KANM3         KLIST
407200970529     C                   KFLD                    KAIE
407300970529     C                   KFLD                    KCDU
407400970529     C                   KFLD                    ANMLNP
407500970529     C     KANM3A        KLIST
407600970529     C                   KFLD                    KAIE
407700970529     C                   KFLD                    KCDU
407800970529     C     KANM3B        KLIST
407900970529     C                   KFLD                    KAIE
408000970529     C                   KFLD                    KCDU
408100970529     C                   KFLD                    KLNP
408200970529     C                   KFLD                    ANMFLS
408300970529     C     KANM3D        KLIST
408400970529     C                   KFLD                    KAIE
408500970529     C                   KFLD                    KCDU
408600970529     C                   KFLD                    KLNP
408700960422     C     KANM4         KLIST
408800960422     C                   KFLD                    KCAA
408900960422     C                   KFLD                    KDAO
409000960423     C                   KFLD                    KLNA
409100960404     C     KANM5         KLIST
409200960404     C                   KFLD                    KCAA
409300960404     C                   KFLD                    KFIL
409400960412     C     KANM51        KLIST
409500960412     C                   KFLD                    KCAA
409600960412     C                   KFLD                    KFIL
409700960412     C                   KFLD                    KLNP
409800960412     C     KANM52        KLIST
409900960412     C                   KFLD                    KCAA
410000960412     C                   KFLD                    KFIL
410100960412     C                   KFLD                    KLNP
410200960412     C                   KFLD                    KFLS
410300960327     C     KANM7         KLIST
410400960327     C                   KFLD                    KAIE
410500960327     C                   KFLD                    KFIL
410600960327     C                   KFLD                    KLNA
410700960327     C     KANM71        KLIST
410800960327     C                   KFLD                    KAIE
410900960327     C                   KFLD                    KFIL
411000960327     C     KANM72        KLIST
411100960327     C                   KFLD                    KAIE
411200960327     C                   KFLD                    KFIL
411300960327     C                   KFLD                    KLNA
411400960327     C                   KFLD                    KDCH
411500960327     C     KANM8         KLIST
411600960327     C                   KFLD                    KAIE
411700960327     C                   KFLD                    KFIL
411800960327     C                   KFLD                    KLNP
411900960327     C                   KFLD                    KLNA
412000960327     C     KANM82        KLIST
412100960327     C                   KFLD                    KAIE
412200960327     C                   KFLD                    KFIL
412300960327     C                   KFLD                    KLNP
412400960327     C                   KFLD                    KLNA
412500960327     C                   KFLD                    KDCH
412600960327     C     KANM9         KLIST
412700960327     C                   KFLD                    KAIE
412800960327     C                   KFLD                    KFIL
412900960327     C                   KFLD                    KLNP
413000960327     C     KANM93        KLIST
413100960327     C                   KFLD                    KAIE
413200960327     C                   KFLD                    KFIL
413300960327     C                   KFLD                    KLNP
413400960327     C                   KFLD                    KFLS
413500960327     C                   KFLD                    KDCH
413600960327     C     KANM94        KLIST
413700960327     C                   KFLD                    KAIE
413800960327     C                   KFLD                    KFIL
413900960327     C                   KFLD                    KLNP
414000960327     C                   KFLD                    KFLS
414100001102     C     KAN11         KLIST
414200001102     C                   KFLD                    KAIE
414300001102     C                   KFLD                    KCAA
414400960327     C     KART          KLIST
414500960327     C                   KFLD                    ANMFLS
414600960327     C                   KFLD                    ANMLNA
414700960327     C                   KFLD                    ANMNRS
414800960327     C                   KFLD                    ANMSCN
414900960327     C     KARB          KLIST
415000960327     C                   KFLD                    ARTAAS
415100960327     C                   KFLD                    ARTLNP
415200960327     C                   KFLD                    ARTNRS
415300960327     C                   KFLD                    ARTNSP
415400960412     C     KBLP          KLIST
415500960412     C                   KFLD                    BLTAAS
415600960412     C                   KFLD                    BLTLNP
415700960412     C                   KFLD                    BLTNRS
415800960412     C                   KFLD                    BLTNSP
415900960508     C     KFVV          KLIST
416000960508     C                   KFLD                    BRVNPG
416100000201     C                   KFLD                    WNFV
416200960508     C                   KFLD                    BRVFGS
416300960704     C     KBRV          KLIST
416400960704     C                   KFLD                    KNPG
416500960704     C                   KFLD                    ANMFLS
416600960704     C                   KFLD                    ANMLNA
416700960704     C                   KFLD                    ANMNRS
416800960704     C                   KFLD                    ANMSCN
416900960927     C     KBRV3         KLIST
417000960927     C                   KFLD                    KNPG
417100960927     C                   KFLD                    ANMFLS
417200960927     C                   KFLD                    ANMLNA
417300960927     C                   KFLD                    ANMNRS
417400960927     C                   KFLD                    ANMSCN
417500960927     C                   KFLD                    ARTFLP
417600970604     C     KBRV3A        KLIST
417700970604     C                   KFLD                    KNPG
417800970604     C                   KFLD                    ANMFLS
417900970604     C                   KFLD                    ANMLNA
418000970604     C                   KFLD                    ANMNRS
418100970604     C                   KFLD                    ANMSCN
418200970604     C                   KFLD                    SIMFEL
418300970306     C     KA71          KLIST
418400970306     C                   KFLD                    ANMFLE
418500990512     C                   KFLD                    ANMDAO
418600970305     C                   KFLD                    ANMCAA
418700970306     C                   KFLD                    ANMLNP
418800970306     C     KA55          KLIST
418900970306     C                   KFLD                    ANMFLE
419000990512     C                   KFLD                    ANMDAO
419100970306     C                   KFLD                    ANMCAA
419200040504     C                   KFLD                    wlnp
419300040405     C     KA200         KLIST
419400040405     C                   KFLD                    ANMFLE
419500040405     C                   KFLD                    wdta200
419600040405     C                   KFLD                    ANMCAA
419700040504     C                   KFLD                    wlnp
419800041020     C     KANT          KLIST
419900041020     C                   KFLD                    KFOA
420000041020     C                   KFLD                    ANMDAO
420100041020     C                   KFLD                    ANMCAA                                     NE
420200041020     C                   KFLD                    KANLNP                                     NE
420300041020     C                   KFLD                    ANMLNA                                     NE
420400990122     C     KDCT          KLIST
420500990122     C                   KFLD                    DCDAAC
420600990122     C                   KFLD                    DCDFIL
420700990122     C                   KFLD                    DCDNCA
420800020807     C     K02FGV01      klist
420900020807     C                   kfld                    BLTnfv
421000020926     C                   kfld                    SIMfel
421100021203
421200021203     C     Kfapd         klist
421300021203     C                   kfld                    kpdtip
421400021203     C                   kfld                    kpdpdr
421500040615
421600040615     C     Kcae          klist
421700040615     C                   kfld                    kcaeepa
421800040615     C                   kfld                    kcaetfp
421900040615     C                   kfld                    kcaetfe
422000040615     C                   kfld                    kcaedde
422100040615     C     Kcae2         klist
422200040615     C                   kfld                    kcaeepa
422300040615     C                   kfld                    kcaetfp
422400040615     C                   kfld                    kcaetfe
422500040616     C     Kcae1         klist
422600040616     C                   kfld                    kcaeepa
422700040616     C                   kfld                    kcaetfp
422800040616     C                   kfld                    kcaetfa
422900040616     C                   kfld                    kcaedde
423000040616     C     Kcae3         klist
423100040616     C                   kfld                    kcaeepa
423200040616     C                   kfld                    kcaetfp
423300040616     C                   kfld                    kcaetfa
423400020905     ***C* DEFINIZIONE CAMPI
423500020905     ***C     *LIKE         DEFINE    FVVFGS        FGSTFP
423600020905     ***C     *LIKE         DEFINE    FVVFGS        WLNA
423700020905     ***C     *LIKE         DEFINE    D60ERR        WERR
423800020905     ***C     *LIKE         DEFINE    D60MB1        WMB1
423900020905     ***C     *LIKE         DEFINE    D60MB2        WMB2
424000020905     ***C     *LIKE         DEFINE    ANMFL1        SAVFL1
424100020905     ***C     *LIKE         DEFINE    ANMFL2        SAVFL2
424200020905     ***C     *LIKE         DEFINE    CLNTFP        KTFP
424300020905     ***C     *LIKE         DEFINE    CLNANN        KANN
424400020905     ***C     *LIKE         DEFINE    CLNMES        KMES
424500020905     ***C     *LIKE         DEFINE    §7ADID        FELDID
424600020905     ***C     *LIKE         DEFINE    ANMDCH        KDCH
424700020905     ***C     *LIKE         DEFINE    ANMLNP        KFIL
424800020905     ***C     *LIKE         DEFINE    ANMLNP        KLNP
424900020905     ***C     *LIKE         DEFINE    ANMLNP        KLNP9
425000020905     ***C     *LIKE         DEFINE    ANMLNP        KLNP92
425100020905     ***C     *LIKE         DEFINE    ANMLNP        KFLS
425200020905     ***C     *LIKE         DEFINE    ANMLNA        KLNA
425300020905     ***C     *LIKE         DEFINE    ANMLNA        SAVLNA
425400020905     ***C     *LIKE         DEFINE    ANMLNA        KLNA7
425500020905     ***C     *LIKE         DEFINE    ANMLNA        KLNA8
425600020905     ***C     *LIKE         DEFINE    ANMAIE        KAIE
425700020905     ***C     *LIKE         DEFINE    ANMCAA        KCAA
425800020905     ***C     *LIKE         DEFINE    ANMCAA        SAVCAA
425900020905     ***C     *LIKE         DEFINE    ANMCAA        WCAA
426000020905     ***C     *LIKE         DEFINE    ANMAAS        KAAS2
426100020905     ***C     *LIKE         DEFINE    ANMLNP        KLNP2
426200020905     ***C     *LIKE         DEFINE    ANMNRS        KNRS2
426300020905     ***C     *LIKE         DEFINE    ANMNSP        KNSP2
426400020905     ***C     *LIKE         DEFINE    ANMDAO        KDAO
426500020905     ***C     *LIKE         DEFINE    D53DFV        W53DFV
426600020905     ***C     *LIKE         DEFINE    ANMNFV        SAVNFV
426700020905     ***C     *LIKE         DEFINE    ANMCCH        CCHPRT
426800020905     ***C     *LIKE         DEFINE    ANMCDU        KCDU
426900020905     ***C     *LIKE         DEFINE    ARTDET        WDDET
427000020905     ***C     *LIKE         DEFINE    ARTFLP        WDFLP
427100020905     ***C     *LIKE         DEFINE    BRVNPG        KNPG
427200020905     ***C     *LIKE         DEFINE    FVVNFV        WNFV
427300020905     ***C     *LIKE         DEFINE    FVVNPG        WNPG
427400020905     ***C     *LIKE         DEFINE    FVVSPG        WSPG
427500020905     ***C     *LIKE         DEFINE    FVVDFV        WDFV
427600020905     ***C     *LIKE         DEFINE    FVVFGS        WFGS
427700020905     ***C     *LIKE         DEFINE    TBLKEY        WMAX
427800960417     C*
427900960402     C* UDATE IN GGMMAAAA
428000960417     C                   TIME                    W0140            14 0
428100960402     C                   MOVE      W0140         WDTGIO            8 0
428200960422     C*
428300960422     C* PRENDO DATA IN AAMMGG
428400960422     C                   MOVEL     WDTGIO        G02DAT
428500960422     C                   MOVEL     0             G02ERR
428600971126     C                   CALL      'XSRDA8'
428700960422     C                   PARM                    WLBDAT
428800971126     C                   MOVE      G02INV        AM8GIO            8 0
428900980327     C* SE RICHIESTO TRASFERIMENTO APRO I MEMBRI
429000040615     C**   D34TRA        IFEQ      'S'
429100040615     C**                 EXSR      CAEX2
429200040615     C**                 END
429300001102     C* SE AZIONE SCHEDULATA IMPOSTO D34GES E D34FGS
429400001102     C     PARSCH        IFEQ      'V'
429500001102     C                   CLEAR                   DSLR34
429600001102     C                   MOVEL     'V'           D34GES
429700001102     C                   Z-ADD     SIMFEL        D34FGS
429800001102     C                   ENDIF
429900960327     C                   ENDSR
430000041020     C**************************************************************************
430100041020     C* SCRITTURA DEL FILE FNANT00F
430200041020     C**************************************************************************
430300041020     C     SCRANT        BEGSR
430400041020     C**
430500041020     C* PER L'ANOMALIA 145  FOA = ANMFLE
430600041020     C* PER L'ANOMALIA 146  FOA  = TERMINAL DI ARRIVO
430700041020    2C     ANMCAA        IFEQ      145
430800041020     C                   Z-ADD     ANMFLE        KFOA
430900041020   X2C                   ELSE
431000041020     C*
431100041020     C                   CLEAR                   KFOA
431200041020     C* PRENDO FLS SE LNP =0
431300041020     C*
431400041020     C                   CLEAR                   DSLV55
431500041020     C                   MOVEL     'A'           D55TPT
431600041020     C                   Z-ADD     ANMLNA        D55LIN
431700041020     C                   Z-ADD     ANMDAO        D55DRF
431800041020    3C     ANMLNP        IFGT      0
431900041020     C                   Z-ADD     ANMLNP        d55lnp
432000041020     C                   ELSE
432100041020     C                   Z-ADD     ANMFLS        d55lnp
432200041020    3C                   ENDIF
432300041020     C                   CALL      'FNLV55R'
432400041020     C                   PARM                    DSLV55
432500041020     C                   Z-ADD     D55TFA        KFOA
432600041020    2C                   ENDIF
432700041020     C*
432800041020     C     KANT          CHAIN     FNANT02L                           39
432900041020     C*
433000041020    2C     *IN39         IFEQ      *ON
433100041020     C                   CLEAR                   FNANT000
433200041020     C                   MOVEL     KFOA          ANTFOA
433300041020     C                   MOVEL     ANMLNA        ANTLNA
433400041020     C                   MOVEL     ANMDAO        ANTDAO
433500041020     C                   MOVEL     ANMCAA        ANTCAA
433600041020     C                   Z-ADD     1             ANTTOT
433700041020     c                   z-add     *date         antdt1
433800041020     C                   WRITE     FNANT000
433900041020   X2C                   ELSE
434000041020     C                   ADD       1             ANTTOT
434100041020     C                   CLEAR                   ANTFT1
434200041020     C                   CLEAR                   ANTFT2
434300041020     c                   z-add     *date         antdt1
434400041020     C                   UPDATE    FNANT000
434500041020    2C                   ENDIF
434600041020     C*
434700041020     C                   MOVEL     'T'           ANMFT1
434800041020     C                   ENDSR
434900911021     C*
435000960422     O* TESTATA IDD
435100911021     OPRTF198   E            TES              02
435200960604     O                       STA(1)              66
435300960604     O              N13      STA(2)             132
435400040614     o               13      STA(19)            132
435500921015     O                       STA(3)             198
435600900626     O                       RSUT                20
435700040615     O                       prtpo               46
435800040615     O                       D34FGS              50
435900960926     O                       PRTFGS              71
436000960402     O                       WDTGIO             168 '  .  .    '
436100921029     O                       PAGE1         Z    198
436200921015     O          E            TES         1
436300040615     O               70      STA(34)            132
436400960402     O                       UTIME              167 '  :  :  '
436500960402     O                  01                      190 '-C O N F E R M A-'
436600040615     O          E            TES         1
436700960402     O                       STA(18)             66
436800960402     O                       STA(20)            132
436900960402     O                       STA(21)            198
437000960506     O                       DESIE               61
437100960402     O                       D34DLG             106 '  /  /    '
437200960402     O                       D34ALG             123 '  /  /    '
437300960403     O                       D34DCG             172 '  /  /    '
437400960402     O                       D34ACG             189 '  /  /    '
437500030717     C*
437600040615     O          E            TES2        1
437700960403     O                  15   STA(16)             66
437800960403     O                 N15   STA(17)            132
437900960403     O                 N18   STA(15)            198
438000960403     O                  15   DESFV               29
438100990513     O                  15   D34NFV        Z     44
438200990513     O                  15   STADFV              61 '  /  /    '
438300980403     O                 N15   TESFSA             103
438400040611     O                 N18   D34LID             198
438500040614     O                                          120 'LINEA ARRIVO :'
438600980403     O               06      D34LNA             124
438700040614     O               06      PRTLNA             141
438800040614     O                                          154 'TIPO MERCE :'
438900040614     O                       PRTVAL             163
439000030717     O*
439100040615     O          E    23      TES2        1
439200030717     O                       STA(33)            132
439300030717     O                       D34POS              87
439400030717     O                       D34NRS             110
439500960403     O*
439600040615     O          E   N14 19   TES2        1
439700960426     O                       STA(32)             66
439800960426     O                       D34CCH              29
439900960426     O                                         +  0 '-'
440000960426     O                       TESCCH            +  0
440100040615     O          E    14      TES2        1
440200960426     O                  19   STA(32)             66
440300960426     O                       STARIG             132
440400960426     O                  19   D34CCH              29
440500960426     O                                         +  0 '-'
440600960426     O                  19   TESCCH            +  0
440700960418     O                       ANM(1)              94
440800960403     O                       ANM(2)            +  3
440900960403     O                       ANM(3)            +  3
441000960403     O                       ANM(4)            +  3
441100960403     O                       ANM(5)            +  3
441200960403     O                       ANM(6)            +  3
441300960403     O                       ANM(7)            +  3
441400960403     O                       ANM(8)            +  3
441500960403     O                       ANM(9)            +  3
441600960403     O                       ANM(10)           +  3
441700040615     o*
441800040615     O          E    80      TES3        1
441900040615     O                       STA(35)             66
442000040615     o                       stpo(1)             20
442100040615     o                       stpo(2)           +  1
442200040615     o                       stpo(3)           +  1
442300040615     o                       stpo(4)           +  1
442400040615     o                       stpo(5)           +  1
442500040615     o                       stpo(6)           +  1
442600040615     o                       stpo(7)           +  1
442700040615     o                       stpo(8)           +  1
442800040615     o                       stpo(9)           +  1
442900040615     o                       stpo(10)          +  1
443000040615     o                       stpo(11)          +  1
443100040615     o                       stpo(12)          +  1
443200040615     o                       stpo(13)          +  1
443300040615     o                       stpo(14)          +  1
443400040615     o                       stpo(15)          +  1
443500040615     o                       stpo(16)          +  1
443600040615     o                       stpo(17)          +  1
443700040615     o                       stpo(18)          +  1
443800040615     o                       stpo(19)          +  1
443900040615     o                       stpo(20)          +  1
444000040615     o                       stpo(21)          +  1
444100040615     o                       stpo(22)          +  1
444200040615     o                       stpo(23)          +  1
444300040615     o                       stpo(24)          +  1
444400040615     o                       stpo(25)          +  1
444500040615     o                       stpo(26)          +  1
444600040615     o                       stpo(27)          +  1
444700040615     o                       stpo(28)          +  1
444800040615     o                       stpo(29)          +  1
444900040615     o                       stpo(30)          +  1
445000040615     O          E            TES3        2
445100911021     O                       STA(4)              66
445200911021     O                       STA(7)             132
445300911021     O                       STA(10)            198
445400040616     O**               N04 17                   198 '     '
445500040615     O          E            TES3        1
445600911018     O                       STA(5)              66
445700911018     O                       STA(8)             132
445800911021     O                       STA(11)            198
445900040616     O**               N04 17                   198 '     '
446000960422     O*
446100960423     O          E            TESIDR      3  1
446200960422     O                       STA(25)             66
446300960422     O                       STA(26)            132
446400960513     O                       STA(13)            198
446500990513     O                       STADCI              82 '  /  /    '
446600990513     O                       STADCM             166 '  /  /    '
446700960423     O*
446800960423     O          E            TESIDE      3  1
446900960423     O                       STA(30)             66
447000960423     O                       STA(31)            132
447100960513     O                       STA(13)            198
447200990513     O                       STADCI              84 '  /  /    '
447300990513     O                       STADCM             166 '  /  /    '
447400911015     O*
447500911015     O          E            DET         1
447600990512     O                  02   PRTAAS        Z
447700921015     O                  02   ANMLNP        Z   +  1
447800921015     O                  02   ANMNRS        Z   +  1
447900921015     O                  02   ANMNSP        Z   +  1
448000921015     O                  02   ANMLNA            +  1
448100960417     O                  02   PRTDSP            +  1 '  /  '
448200960417     O*
448300990121     O                  03   ANMFLS            +  3
448400921015     O                  03   ANMLNA            +  1
448500921015     O                  03   ANMNRS        Z   +  1
448600921015     O                  03   ANMSCN        Z   +  1
448700990121     O                  22   ANMSCN        Z     48
448800040609     O                  03   PRTZNC            +  1
448900040609     O                  03   anmft4            +  0
449000990121     O                  03   ANMNPS        Z   +  1
449100040616     O                       DESTAN            +  1
449200990121     O                       ANMCAA            +  1
449300921015     O                                         +  0 '-'
449400990122     O              N40   N09DESCAA            +  0
449500990122     O                     09DESCA1              95
449600950320     O                     09ARBNDC        Z   +  0
449700950320     O                     09STADDC            +  1 '  /  '
449800950320     O                     09STAPDC            +  1
449900040616     O                     20PSTAM              110
450000040616     O                     20WDFLP              107
450100040616     O                     20PRTDET             118 '  /  '
450200040616     O                     21CDOVE              110
450300040616     O                     21PRTDOV             101
450400040616     O                     21WFGS               107
450500040616     O                     21PRTDFV             118 '  /  '
450600990121     O* NUMERO C.A. SU CUI E' IL COLLO
450700040616     O                     40CCOAN              109
450800040616     O                     40DESCA1              91
450900040616     O                     40DCTFIL        Z     98
451000040616     O                     40DCTNCA        Z    106
451100040616     O                     40STADCA             118 '  /  /  '
451200040616     O                  42 40                   124 'CHIUSA'
451300040616     O                       STALID             125
451400040616     O                       STADAc            +  1 '  /  '
451500040616     O                       STAorc            +  1 '  :  '
451600040616     O                       DESFSA            +  1
451700950320     O                       STADAO            +  1 '  /  '
451800040616     O                       ANMCDU        Z   +  1
451900040616     O                       ANMNFV        Z   +  1
452000990121     O                       DESFSC            +  1
452100950320     O                       STADCH            +  1 '  /  '
452200970522     O                       CCHPRT            +  1
452300960423     O                                         +  0 '-'
452400960423     O                       DESCCH            +  0
452500040616     O                       ANMFLE            +  1
452600050503     O*  STAMPA NUMERO C.A. e/o particolarità consegna CP
452700050503     O          E            DET1        1
452800050503     O               43      decodiCP            96
452900050503     O               41      CCOAN              109
453000050503     O               41      DCTFIL        Z     98
453100050503     O               41      DCTNCA        Z    106
453200050503     O               41      STADCA             118 '  /  /  '
453300050503     O               41                         128 'CHIUSA IL'
453400050503     O               41      STACHA             134 '  /  '
453500050504     O*  STAMPA NUMERO C.A. e/o particolarità consegna CP
453600050504     O          E            DET1        0  0
453700050504     O               43      decodiCP            96
453800051221     O*  STAMPA dati mittente e destinatario
453900051221     O          E            DET2        1
454000051222     o*                                        + 30 'Mittente:'
454100051222     o                                         + 69 'Mittente:'
454200051221     o                       blprsm            +  0
454300051222     o                                         +  2 'Rif.Mitt.Num:'
454400051222     o                       prtrmn            +  0
454500051222     o                                         +  2 'Destinatario:'
454600051221     o                       blprsd            +  0
454700151125     O*  STAMPA consegna anomala 7
454800151125     O          E            DET3        1
454900151125     o                                         + 30 'Bolla chiusa per '
455000151125     o                                         +  0 'merce mai affidata il'
455100151125     o                       prtdcm            +  2 '  /  '
455200960422     O*
455300960422     O*  STAMPA RIGA: DETTAGLIO FILIALE CHE NON HANNO FATTO L'IDD
455400960422     O          E            DETIDD      1
455500960423     O                       FID(1)        Z
455600960422     O                                         +  0 '-'
455700960422     O                       DFI(1)            +  0
455800050831     O                       FID(2)        Z   +  2
455900960422     O                                         +  0 '-'
456000960422     O                       DFI(2)            +  0
456100050831     O                       FID(3)        Z   +  2
456200960422     O                                         +  0 '-'
456300960422     O                       DFI(3)            +  0
456400050831     O                       FID(4)        Z   +  2
456500960422     O                                         +  0 '-'
456600960422     O                       DFI(4)            +  0
456700050831     O                       FID(5)        Z   +  2
456800960422     O                                         +  0 '-'
456900960422     O                       DFI(5)            +  0
457000050831     O                       FID(6)        Z   +  2
457100960422     O                                         +  0 '-'
457200960422     O                       DFI(6)            +  0
457300050831     O                       FID(7)        Z   +  2
457400960422     O                                         +  0 '-'
457500960422     O                       DFI(7)            +  0
457600960422     O*
457700960422     O*  STAMPA RIGA: NESSUNA ANOMALIA RISCONTRATA
457800960422     O          E            NOANOM      3  3
457900960422     O                       STA(24)            132
458000960423     O*  STAMPA RIGA: TUTTE LE FILIALI HANNO FATTO L'IDD
458100960423     O          E            SIIDD       2
458200960423     O                       STA(27)             66
458300960423     O                       STA(28)            132
458400960423     O                       STA(29)            198
458500960422     O*  STAMPA IDD3
458600911025     OPRTF198X  E            TESX             02
458700960604     O                       STA(1)              66
458800040618     O                       STA(14)            132
458900040618     O                       STA(3)             198
459000040618     O                       prtpo               46
459100960926     O                       D34FGS              50
459200960926     O                       PRTFGS              71
459300960604     O                       §7ALCI             121
459400911025     O                       RSUT                20
459500960402     O                       WDTGIO             168 '  .  .    '
459600921015     O                       PAGE2         Z    198
459700960403     O          E            TESX        1
459800960402     O                       UTIME              167 '  :  :  '
459900960402     O                  01                      190 '-C O N F E R M A-'
460000921015     O          E            TESX        2
460100911025     O                       STA(4)              66
460200911025     O                       STA(7)             132
460300911025     O                       STA(10)            198
460400921016     O                                          198 '     '
460500911025     O          E            TESX        1
460600911025     O                       STA(5)              66
460700911025     O                       STA(8)             132
460800911025     O                       STA(11)            198
460900921016     O                                          198 '     '
461000911025     O*
461100911025     O          E            DETX        1
461200990512     O                  02   PRTAAS        Z
461300921015     O                  02   ANMLNP        Z   +  1
461400921015     O                  02   ANMNRS        Z   +  1
461500921015     O                  02   ANMNSP        Z   +  1
461600921015     O                  02   ANMLNA            +  1
461700960417     O                  02   PRTDSP            +  1 '  /  '
461800960417     O*
461900990121     O                  03   ANMFLS            +  3
462000921015     O                  03   ANMLNA            +  1
462100921015     O                  03   ANMNRS        Z   +  1
462200921015     O                  03   ANMSCN        Z   +  1
462300130423     O                  03   PRTZNC            +  1
462400130423     O                  03   anmft4            +  0
462500990121     O                  03   ANMNPS        Z   +  1
462600990121     O                       DESTAN            +  1
462700990121     O                       ANMCAA            +  1
462800921015     O                                         +  0 '-'
462900990122     O                    N40DESCAA            +  0
463000040616     O                     20PSTAM              110
463100040616     O                     20WDFLP              107
463200040616     O                     20PRTDET             118 '  /  '
463300040616     O                     21CDOVE              110
463400040616     O                     21PRTDOV             101
463500040616     O                     21WFGS               107
463600040616     O                     21PRTDFV             118 '  /  '
463700990122     O* NUMERO C.A. SU CUI E' IL COLLO
463800040616     O                     40CCOAN              109
463900040616     O                     40DESCA1              91
464000040616     O                     40DCTFIL        Z     98
464100040616     O                     40DCTNCA        Z    106
464200040616     O                     40STADCA             118 '  /  /  '
464300040616     O                  42 40                   124 'CHIUSA'
464400040616     O                       ANMLID             125
464500040616     O                       STADAc            +  1 '  /  '
464600040616     O                       STAorc            +  1 '  :  '
464700040616     O                       DESFSA            +  1
464800040616     O                       STADAO            +  1 '  /  '
464900040616     O                       ANMCDU        Z   +  1
465000040616     O                       ANMNFV        Z   +  1
465100990121     O                       DESFSC            +  1
465200040616     O                       STADCH            +  1 '  /  '
465300960423     O                       ANMCCH            +  1
465400960423     O                                         +  0 '-'
465500960423     O                       DESCCH            +  0
465600040616     O                       ANMFLE            +  1
465700990122     O*  STAMPA NUMERO C.A.
465800050503     O          E            DETX1       1
465900050503     O               43      decodiCP            96
466000050503     O               41      CCOAN              109
466100050503     O               41      DCTFIL        Z     98
466200050503     O               41      DCTNCA        Z    106
466300050503     O               41      STADCA             118 '  /  /  '
466400050503     O               41                         128 'CHIUSA IL'
466500050503     O               41      STACHA             134 '  /  '
466600050504     O          E            DETX1       0  0
466700050504     O               43      decodiCP            96
466800990122     O*
466900990512**
467000040615XXXXXXXXXXXXXXXXXXXX        xxxxxxxxxxxxxxxxxx XXX-XXXXXXXXXXXXXXX
467100960926XXXXX           **   L I S T A     A N O M A L I E   **             2
467200960417    FNLR35R               XX/XX/XXXX                     PAG. XXXX  3
467300040616    S P E D I Z I O N E          S E G N A C O L L O      A      N
467400040616AN LNP SER NUMERO LNA  DATA   LNP LNA SER NUMERO ZON PIS T I P O
467500040616XX XXX XX XXXXXXX XXX XX/XX   XXX XXX XX XXXXXXX xxV XX xxxxxxxxxx
467600040616      O      M      A      L      I      A               LIV CREAZ
467700040616    C  O  D  I  C  E                                     IDD DATA
467800040616xx XXX-bolla in consegna:XXXXXXX XX/XX XXXXXXXXXXXX  X    X XX/xx
467900040616IONE  A  P  E  R  T  U  R  A    C  H  I  U  S  U  R  A         P.O  10
468000040616 ORA  FASE     DATA   FOGLIO    FASE      DATA  CAUSALE        EMI  11
468100040616xx:xx xxxxxxxx XX/XX XXX XXXXXX xxxxxxxx XX/XX XX-XXXXXXXXXXXX XXX 12
468200990513RNO PRECEDENTE, IN DATA XX/XX/XXXX) :
468300960604        **   E L A B O R A Z I O N E      " I. D. D.  X "   **     14
468400040614                                  ANOMALIE IDD DAL LIVELLO IDD : X 15
468500990513ANOMALIE DEL FOGLIO  XXXXXXXX   N.RO  XXXXXX   DEL XX/XX/XXXX      16
468600040614FASE APERTURA ANOMALIE : XXXXXXXXXXXX                              17
468700960403ANOMALIE :  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX         18
468800960604        **   E L A B O R A Z I O N E      "I. D. D."   **          19
468900960402DATA APERTURA ANOMALIA : DAL  XX/XX/XXXX   AL  XX/XX/XXXX          20
469000960402DATA CHIUSURA ANOMALIA : DAL  XX/XX/XXXX   AL  XX/XX/XXXX          21
469100960506CODICI       RICHIESTI : XXX                                       22
469200960506CODICI  NON  RICHIESTI : XXX                                       23
469300960422 *** NESSUNA  ANOMALIA  RISCONTRATA  NEI  PARAMETRI  RICHIESTI *** 24
469400960926ELENCO PUNTI OPER.DI CUI MANCA LA TRASMISSIONE ANOMALIE I.D.D. IN  25
469500990513DATA  XX/XX/XXXX     (PER I PUNTI OPER. CHE ESEGUONO L'IDD DEL GIO 26
469600960926                                                    T U T T I    I 27
469700030717    P U N T I    O P E R A T I V I    H A N N O    C O N F E R M A 28
469800030717 T O    L'  I . D . D .                                            29
469900030717ELENCO PUNTI OPER.DELL'AREA DI CUI MANCA LA CONFERMA DELL'I.D.D. I 30
470000030717N DATA  XX/XX/XXXX   (PER I PUNTI OPER. CHE ESEGUONO L'IDD DEL GIO 31
470100960426CAUSALE CHIUSURA ANOMALIA: XX-XXXXXXXXXXXXXXXXXXXXXXXXX            32
470200030717P.O. SEGNACOLLO : XXX      NUMERO SERIE : XX                       33
470300040615                **  COLLI   DI   VALORE   MANCANTI   **            34
470400040615Punti Operativi: xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx xxx   35
470500921016**
470600960506A R R I V O
470700960506P A R T E N Z A
470800960506  N O N    I.D.D.
470900960506  I . D . D.
471000960506  I N T E R N E
471100960506  T U T T E
471200960506     SOLO APERTE
471300960506     SOLO CHIUSE
471400021008**         CMD1
471500060619OVRDBF FILE(FNANM00T)                MBR(M000000)
471600980327**         CMD3
471700060619ADDPFM FILE(FNANM00T)                MBR(M000000)
471800980327**         CMD5
471900060619CHKOBJ OBJ(FNANM00T) OBJTYPE(*FILE)  MBR(M000000)
472000060619**         OVRSED
472100060710OVRDBF FILE(FNANMSED) TOFILE(GAITRAGRU/FNANM00F )
